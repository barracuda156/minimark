-- minimark: RTF reader (-f rtf, T5.1).  Parses the RTF that TextEdit,
-- Word and our own -t rtf writer produce, back into the shared Doc AST.
--
-- Pipeline: raw bytes -> tokens -> a group-scoped state machine that
-- emits Blocks.  RTF is 7-bit ASCII with escapes (\'xx raw bytes, \uN
-- Unicode, control words), so the input is read through a BINARY handle
-- (one Char = one byte) and any high bytes are decoded by OUR cp1252 /
-- MacRoman tables here, never by MicroHs's UTF-8 transducer, which would
-- hard-error on a stray 0x92 curly quote.
--
-- Scope (per roadmap T5.1): bold/italic/strike run styling scoped by
-- group nesting; paragraphs on \par; headings from \sN style refs when
-- the stylesheet names them "heading N" (else a font-size heuristic);
-- destination groups we cannot render (fonttbl, stylesheet content,
-- info, pict, *-prefixed custom destinations, field instructions) are
-- skipped; lists fall back to \pntext bullet markers.  Tables, nested
-- numbering and embedded objects degrade to plain paragraphs.  Graceful
-- degradation is a hard rule: malformed input never crashes.
module MiniMark.RtfReader(parseRtf) where

import Data.List(isPrefixOf)
import MiniMark.AST
import MiniMark.RtfEncoding(cp1252, macRoman)

--------------------------------------------------------------------------
-- Tokenizer

-- One token.  Text and decoded bytes both become Char output; keeping
-- TByte separate lets \uN's skip-count consume following bytes/text
-- uniformly (a skipped \'xx counts as one).
data Tok
  = TCtrl String (Maybe Int)   -- \word or \word123 or \word-1  (arg parsed)
  | TSym  Char                 -- \{ \} \\ and other control symbols
  | TByte Int                  -- \'xx  -> raw byte value 0..255
  | TUni  Int                  -- \uN   -> Unicode code point (may be signed)
  | TOpen                      -- {
  | TClose                     -- }
  | TChar Char                 -- a literal character (already one byte)

-- Tokenize the whole input.  Bare CR/LF between control words are RTF
-- whitespace and dropped; a backslash-escaped newline is a hard line
-- break (rare, but Word emits it), handled as \line at parse time.
tokenize :: String -> [Tok]
tokenize [] = []
tokenize (c:cs) = case c of
  '{'  -> TOpen  : tokenize cs
  '}'  -> TClose : tokenize cs
  '\\' -> ctrl cs
  '\r' -> tokenize cs
  '\n' -> tokenize cs
  '\t' -> TChar '\t' : tokenize cs
  -- A raw byte >= 0x80 in the content (some producers, and all MacRoman
  -- docs, embed accented text unescaped) is a codepage byte, not a
  -- Unicode char: route it through TByte so the active table decodes it.
  _ | fromEnum c >= 0x80 -> TByte (fromEnum c) : tokenize cs
    | otherwise          -> TChar c : tokenize cs

-- After a backslash.  Either a control WORD (letters, optional numeric
-- arg, one optional trailing space consumed) or a control SYMBOL (a
-- single non-letter).  \'xx and \uN are special-cased.
ctrl :: String -> [Tok]
ctrl [] = []                                   -- trailing backslash: drop
ctrl (c:cs)
  | c == '\'' =
      let (hx, rest) = splitAt 2 cs
      in TByte (hexByte hx) : tokenize rest
  | isLetter c =
      let (word, rest0) = span isLetter (c:cs)
          (marg, rest1) = spanArg rest0
          rest2         = dropOneSpace rest1
      in case word of
           "u"  -> TUni (maybe 0 id marg) : tokenize rest2
           _    -> TCtrl word marg : tokenize rest2
  | otherwise =
      -- control symbol: the char itself is the token, no arg, no space swallow
      TSym c : tokenize cs

-- A control word's optional integer argument: optional '-' then digits.
spanArg :: String -> (Maybe Int, String)
spanArg s = case s of
  '-':ds -> case span isDigit ds of
              (n@(_:_), r) -> (Just (negate (readNat n)), r)
              _            -> (Nothing, s)
  _      -> case span isDigit s of
              (n@(_:_), r) -> (Just (readNat n), r)
              _            -> (Nothing, s)

-- Exactly one space after a control word/arg is a delimiter and is
-- swallowed; any further space is literal text.
dropOneSpace :: String -> String
dropOneSpace (' ':r) = r
dropOneSpace r       = r

isLetter :: Char -> Bool
isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

readNat :: String -> Int
readNat = foldl (\n c -> n * 10 + (fromEnum c - fromEnum '0')) 0

hexByte :: String -> Int
hexByte = foldl (\n c -> n * 16 + hexVal c) 0
  where
    hexVal c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise            = 0

--------------------------------------------------------------------------
-- Parser state

-- Character-formatting flags, snapshotted on '{' and restored on '}'
-- (RTF scopes formatting to the group).  `dest` names the current
-- destination so we can drop content we can't render; `skip` is \uN's
-- pending byte-skip count.
data St = St
  { bold   :: Bool
  , ital   :: Bool
  , strk   :: Bool
  , mono   :: Bool          -- current font is monospaced -> code span
  , fsize  :: Int           -- \fsN font size in half-points (24 = 12pt body)
  , dest   :: Dest
  , uc     :: Int           -- \ucN Unicode fallback skip count (default 1)
  , skipN  :: Int           -- fallback chars still to discard after a \uN
  , enc    :: Enc           -- byte -> Unicode table in force
  , monoFs :: [Int]         -- font ids the fonttbl marked monospaced
  }

data Dest = DBody | DSkip | DStyleSheet | DField
  deriving (Eq)

data Enc = EAnsi | EMac
  deriving (Eq)

st0 :: [Int] -> St
st0 mf = St False False False False 24 DBody 1 0 EAnsi mf

-- Accumulator threaded through the token walk.  curRun/curText/curFmt
-- together build the current paragraph: curText is the pending literal
-- run under curFmt (bold/ital/strk/mono), flushed to an Inline when the
-- format changes or the paragraph ends.
data Acc = Acc
  { blocks  :: [Block] -> [Block]   -- finished blocks, as a difference list
  , curRun  :: [Inline] -> [Inline] -- inlines of the paragraph in progress
  , curText :: String -> String     -- pending literal chars (flushed to a Str)
  , curFmt  :: Fmt                   -- format the pending chars were typed in
  , curSty  :: Maybe Int            -- \sN seen for the current paragraph
  , curFs   :: Int                  -- font size active at paragraph's text
  , styMap  :: [(Int, String)]      -- \sN -> style name (from stylesheet)
  }

-- The four run attributes we can represent, snapshotted per literal run.
data Fmt = Fmt Bool Bool Bool Bool  -- bold ital strk mono
  deriving (Eq)

fmtOf :: St -> Fmt
fmtOf s = Fmt (bold s) (ital s) (strk s) (mono s)

acc0 :: [(Int, String)] -> Acc
acc0 sm = Acc id id id (Fmt False False False False) Nothing 0 sm

--------------------------------------------------------------------------
-- Entry point

-- Reader contract: whole RTF input (as raw bytes in a String) -> Doc.
-- Two passes: first harvest the stylesheet's \sN->name map so heading
-- detection can consult it, then the real walk.  A leading/trailing junk
-- brace mismatch is tolerated (walk stops cleanly at end of tokens).
parseRtf :: String -> Doc
parseRtf input =
  let toks  = tokenize input
      sm    = harvestStyles toks
      mf    = harvestMonoFonts toks
      acc   = walk (st0 mf) (acc0 sm) toks
      acc'  = endPara acc                    -- flush the final paragraph
  in Doc emptyMeta (blocks acc' [])

--------------------------------------------------------------------------
-- Stylesheet harvest (first pass)
--
-- {\stylesheet {\s1 ... Heading 1;}{\s2 ... heading 2;} ...}.  Each
-- entry is a group starting with a \sN control word; its display name is
-- the text after the last control word, up to the ';'.  We only need the
-- \sN -> name association; scoped brace tracking keeps us inside the
-- stylesheet destination.

harvestStyles :: [Tok] -> [(Int, String)]
harvestStyles toks = case findStyleSheet toks of
  Nothing   -> []
  Just body -> collectEntries body

-- Find the token stream inside the {\stylesheet ...} group, balanced.
findStyleSheet :: [Tok] -> Maybe [Tok]
findStyleSheet [] = Nothing
findStyleSheet (TOpen : TCtrl "stylesheet" _ : rest) =
  Just (fst (splitGroup 0 rest))
findStyleSheet (_ : rest) = findStyleSheet rest

-- Split a token list at the '}' that closes the current group (depth 0
-- = the group we're already inside).  Returns (inside, afterClose).
splitGroup :: Int -> [Tok] -> ([Tok], [Tok])
splitGroup _ [] = ([], [])
splitGroup d (t:ts) = case t of
  TOpen  -> let (a, b) = splitGroup (d + 1) ts in (t:a, b)
  TClose -> if d == 0 then ([], ts)
                      else let (a, b) = splitGroup (d - 1) ts in (t:a, b)
  _      -> let (a, b) = splitGroup d ts in (t:a, b)

-- Each stylesheet entry is a top-level sub-group; scan them.
collectEntries :: [Tok] -> [(Int, String)]
collectEntries [] = []
collectEntries (TOpen : rest) =
  let (inside, after) = splitGroup 0 rest
  in case styleEntry inside of
       Just kv -> kv : collectEntries after
       Nothing -> collectEntries after
collectEntries (_ : rest) = collectEntries rest

-- One entry's group body: find the \sN number, then the trailing name.
styleEntry :: [Tok] -> Maybe (Int, String)
styleEntry ts = do
  n <- styleNum ts
  Just (n, styleName ts)
  where
    styleNum []                       = Nothing
    styleNum (TCtrl "s" (Just k) : _) = Just k
    styleNum (_ : r)                  = styleNum r

-- The visible name is the run of literal chars up to ';', after any
-- control words / nested groups have been dropped.  Good enough for the
-- "heading N" test.
styleName :: [Tok] -> String
styleName = takeWhile (/= ';') . dropControls
  where
    dropControls [] = []
    dropControls (t:ts) = case t of
      TChar c -> c : dropControls ts
      TByte b -> decodeByte EAnsi b ++ dropControls ts
      _       -> dropControls ts

--------------------------------------------------------------------------
-- Main walk (second pass)

walk :: St -> Acc -> [Tok] -> Acc
walk _  a [] = a
walk s a (t:ts)
  -- Inside a skipped destination, drop everything but keep the brace
  -- structure balanced (groups were split out, so plain tokens vanish).
  | dropping = case t of
      TOpen  -> let (_, after) = splitGroup 0 ts in walk s a after
      _      -> walk s a ts
  -- \uN Unicode fallback: the next `skipN` "characters" (a literal char,
  -- a decoded byte, or a nested {group} each count as one) are the ANSI
  -- approximation we discard because we already emitted the real cp.
  | skipN s > 0 = case t of
      TOpen  -> let (_, after) = splitGroup 0 ts
                in walk s{skipN = skipN s - 1} a after
      TClose -> walk s a ts     -- stray close: don't consume a skip slot
      -- A control word during the skip window still acts (e.g. \uc, \b)
      -- and does NOT consume a skip slot; only literal chars/bytes/groups
      -- are the fallback we discard.
      TCtrl w marg -> let (s', a') = control s a w marg in walk s' a' ts
      _      -> walk s{skipN = skipN s - 1} a ts
  | otherwise = case t of
      TOpen  ->
        -- Recurse into the group with a copy of the state; formatting set
        -- inside does not leak out, but emitted blocks/text do (threaded
        -- through the returned Acc).
        let (inside, after) = splitGroup 0 ts
            a' = walk s a inside
        in walk s a' after
      TClose -> walk s a ts          -- unmatched close: ignore, keep going
      TCtrl w marg -> let (s', a') = control s a w marg in walk s' a' ts
      TSym c       -> let (s', a') = symbol s a c in walk s' a' ts
      TByte b      -> walk s (feedByte s a b) ts
      TUni cp      -> walk (s{skipN = uc s}) (feedUni s a cp) ts
      TChar c      -> walk s (feedChar s a c) ts
  where dropping = dest s == DSkip || dest s == DStyleSheet || dest s == DField

--------------------------------------------------------------------------
-- Control words

control :: St -> Acc -> String -> Maybe Int -> (St, Acc)
control s a w marg = case w of
  -- destinations we drop wholesale
  _ | w `elem` skipDests -> (s{dest = DSkip}, a)
  "stylesheet"           -> (s{dest = DStyleSheet}, a)
  "fldinst"              -> (s{dest = DField}, a)

  -- paragraph / line structure
  "par"      -> (s, endPara a)
  "pard"     -> (s, a{curSty = Nothing})   -- reset paragraph props
  "line"     -> (s, pushInline s a (Str "\n"))
  "tab"      -> (s, feedChar s a '\t')
  "sect"     -> (s, endPara a)
  "page"     -> (s, endPara a)

  -- list bullet fallback (\pntext group holds the marker; we already
  -- render its chars as text, so nothing extra needed — but \bullet is
  -- a control word in some producers)
  "bullet"   -> (s, feedChar s a '\x2022')
  "emdash"   -> (s, feedChar s a '\x2014')
  "endash"   -> (s, feedChar s a '\x2013')
  "lquote"   -> (s, feedChar s a '\x2018')
  "rquote"   -> (s, feedChar s a '\x2019')
  "ldblquote"-> (s, feedChar s a '\x201C')
  "rdblquote"-> (s, feedChar s a '\x201D')
  "enspace"  -> (s, feedChar s a ' ')
  "emspace"  -> (s, feedChar s a ' ')

  -- character formatting (arg 0 turns the attribute OFF, RTF convention)
  "b"        -> (s{bold = onOff marg}, a)
  "i"        -> (s{ital = onOff marg}, a)
  "strike"   -> (s{strk = onOff marg}, a)
  "ul"       -> (s, a)                 -- underline: no AST node, ignore
  "ulnone"   -> (s, a)
  "plain"    -> (s{bold = False, ital = False, strk = False, mono = False}, a)

  -- font selection: mono iff the fonttbl marked this font id \fmodern
  -- (harvested in the first pass).  This is how our own writer's \f1
  -- code spans and TextEdit's monospaced runs round-trip.
  "f"        -> (s{mono = maybe False (`elem` monoFs s) marg}, a)

  -- font size (half-points): drives the heading heuristic when the
  -- paragraph has no named style.
  "fs"       -> (s{fsize = maybe 24 id marg}, a)

  -- style ref: remember it for heading detection at \par time
  "s"        -> (s, a{curSty = marg})

  -- encoding switches
  "mac"      -> (s{enc = EMac}, a)
  "ansi"     -> (s{enc = EAnsi}, a)
  "pc"       -> (s{enc = EAnsi}, a)     -- cp437 unsupported; nearest-safe
  "pca"      -> (s{enc = EAnsi}, a)
  "uc"       -> (s{uc = maybe 1 id marg}, a)
  "ansicpg"  -> (s, a)                  -- codepage number: cp1252 assumed

  -- swallow a \* -prefixed unknown destination's marker word if it slips
  -- through (handled as TSym '*' normally)
  _          -> (s, a)                  -- unknown control word: ignore
  where onOff m = m /= Just 0

-- Destinations whose entire content we discard.  \*\destination custom
-- ones are caught by the TSym '*' handler flipping dest to DSkip.
-- NOT skipped: \pntext / \pntxtb (list bullet markers — we let their
-- chars flow into the paragraph, the writer-parity list fallback) and
-- \fldrslt (a field's rendered result, e.g. a hyperlink's display text).
skipDests :: [String]
skipDests =
  [ "fonttbl", "colortbl", "info", "pict", "object", "header", "footer"
  , "headerl", "headerr", "footerl", "footerr", "footnote", "annotation"
  , "themedata", "colorschememapping", "latentstyles", "datastore"
  , "generator", "xmlnstbl", "listtable", "listoverridetable", "rsidtbl"
  , "bkmkstart", "bkmkend", "nonshppict", "shppict", "revtbl"
  , "wgrffmtfilter", "template", "pgptbl", "protusertbl", "sn", "sv"
  ]

--------------------------------------------------------------------------
-- Control symbols and character feed

-- Control symbols after a backslash: \{ \} \\ are literal braces/slash,
-- \~ non-breaking space, \- optional hyphen (drop), \_ non-breaking
-- hyphen, \* marks an unknown destination -> skip its group.  We don't
-- have St here for \* (it needs to set dest=DSkip); handle \* in walk.
symbol :: St -> Acc -> Char -> (St, Acc)
symbol s a c = case c of
  '{'  -> (s, pushChar s a '{')
  '}'  -> (s, pushChar s a '}')
  '\\' -> (s, pushChar s a '\\')
  '~'  -> (s, pushChar s a '\x00A0')     -- non-breaking space
  '_'  -> (s, pushChar s a '\x2011')     -- non-breaking hyphen
  '-'  -> (s, a)                         -- optional hyphen: drop
  '*'  -> (s{dest = DSkip}, a)           -- \*\dest -> ignore whole group
  '\n' -> (s, pushInline s a (Str "\n")) -- escaped newline = line break
  '\r' -> (s, pushInline s a (Str "\n"))
  _    -> (s, a)                         -- other symbols: ignore

-- Feed one literal character into the pending run, tracking font size
-- for the heading heuristic (record the largest size any visible char
-- in the paragraph was typed at, ignoring whitespace-only runs).
feedChar :: St -> Acc -> Char -> Acc
feedChar s a c = pushChar s a c

-- \'xx raw byte -> decode via the active cp1252/MacRoman table, then
-- feed the resulting Char(s).  A byte inside \uN's skip window is eaten
-- (handled by feedUni's skip, which decrements uc-count via pushChar? —
-- no: skip is tracked here).  We keep it simple: no pending-skip state
-- machine across tokens; \uN already emits its char and RTF's fallback
-- bytes follow, so we DON'T skip them here — see feedUni.
feedByte :: St -> Acc -> Int -> Acc
feedByte s a b = foldl (pushChar s) a (decodeByte (enc s) b)

-- \uN Unicode code point.  N may be a signed 16-bit value (negative for
-- code points >= 0x8000); normalize to a real code point.  The uc-count
-- fallback characters that follow are ansi approximations we skip.  We
-- approximate skipping by dropping the next `uc` literal chars: track a
-- pending counter in Acc? To stay stateless-per-token we instead rely on
-- producers using \ucN with the exact count — implemented via feedUni
-- setting a skip that feedChar/feedByte honor.  Simplest correct form:
-- carry the skip in St is cleaner, but St is immutable per return here,
-- so we handle the common \uc1 case by having walk consult it — see the
-- pending field.  For robustness we DECODE the code point and let the
-- ANSI fallback char through only when uc=0.
feedUni :: St -> Acc -> Int -> Acc
feedUni s a n =
  let cp = if n < 0 then n + 0x10000 else n
      ch = if cp >= 0 && cp <= 0x10FFFF then toEnum cp else '\xFFFD'
  in pushChar s a ch

-- Push one char into the pending literal run.  If the active format
-- differs from the run's format, flush the run first so each Str gets a
-- single consistent style wrapper.
pushChar :: St -> Acc -> Char -> Acc
pushChar s a c =
  let a1 = if fmtOf s == curFmt a then a else (flushText a){curFmt = fmtOf s}
      fs = if c == ' ' || c == '\t' then curFs a1 else max (curFs a1) (fsize s)
  in a1{curText = curText a1 . (c :), curFs = fs}

-- Flush pending literal chars into the paragraph run as a styled Inline.
flushText :: Acc -> Acc
flushText a =
  let t = curText a ""
  in if null t
       then a
       else a{ curText = id
             , curRun  = curRun a . (styleInline (curFmt a) t :) }

-- Wrap a literal string in the emphasis nodes its format calls for.
-- Order (innermost first): CodeSpan is exclusive; otherwise nest
-- Strong / Emph / Strike so a bold-italic-struck run round-trips.
styleInline :: Fmt -> String -> Inline
styleInline (Fmt b i k m) t
  | m         = CodeSpan t
  | otherwise = wrap b Strong (wrap i Emph (wrap k Strike (Str t)))
  where
    wrap True  f x = f [x]
    wrap False _ x = x

-- Push an already-built Inline (e.g. a hard line break) after flushing
-- any pending literal chars so ordering is preserved.
pushInline :: St -> Acc -> Inline -> Acc
pushInline _ a inl =
  let a1 = flushText a
  in a1{curRun = curRun a1 . (inl :)}

--------------------------------------------------------------------------
-- Paragraph flush + heading detection

-- End the current paragraph: flush pending text, decide Heading vs Para
-- (or drop an all-blank paragraph), append the block, and reset the
-- paragraph-scoped accumulator fields.
endPara :: Acc -> Acc
endPara a0 =
  let a  = flushText a0
      is = curRun a []
  in if allBlank is
       then a{ curRun = id, curText = id, curSty = Nothing, curFs = 0 }
       else let blk = case headingLevel a of
                        Just n  -> Heading n (trimInlines is)
                        Nothing -> Para is
            in a{ blocks  = blocks a . (blk :)
                , curRun  = id
                , curText = id
                , curSty  = Nothing
                , curFs   = 0 }

-- Heading level for the just-finished paragraph.  First try the named
-- style (\sN resolved through the stylesheet to "heading N" / "Heading
-- N"); then fall back to a font-size heuristic tuned to our own writer
-- (H1 \fs48, H2 \fs36, H3+ \fs28; body \fs24) and to common word-
-- processor heading sizes.  Documented heuristic: a paragraph whose text
-- is set >= 26 half-points (13pt) and is short is treated as a heading.
headingLevel :: Acc -> Maybe Int
headingLevel a =
  case curSty a >>= \n -> lookup n (styMap a) of
    Just name | Just lvl <- headingName name -> Just lvl
    _ -> sizeHeading (curFs a)

-- "Heading 1", "heading2", "Title" -> level.  Title maps to H1.
headingName :: String -> Maybe Int
headingName raw =
  let s = dropWhile (== ' ') raw
      low = map toLowerA s
  in if "heading" `isPrefixOf` low
       then case readNat1 (dropWhile (== ' ') (drop 7 low)) of
              Just n | n >= 1 && n <= 6 -> Just n
              _                         -> Just 1
       else if "title" `isPrefixOf` low then Just 1
       else if "subtitle" `isPrefixOf` low then Just 2
       else Nothing

-- Font-size fallback (half-points).  Mirrors the -t rtf writer's sizes
-- so writer->reader round-trips, plus a general "bigger than body ==
-- heading" rule.  Body text (<= 25 half-pt, i.e. <=12.5pt) is never a
-- heading.  Returns Nothing for body so caller keeps a Para.
sizeHeading :: Int -> Maybe Int
sizeHeading fs
  | fs >= 44  = Just 1     -- ~22pt+  (writer H1 = 48)
  | fs >= 32  = Just 2     -- ~16pt+  (writer H2 = 36)
  | fs >= 26  = Just 3     -- ~13pt+  (writer H3+ = 28)
  | otherwise = Nothing

-- A paragraph is blank if it has no non-whitespace text.
allBlank :: [Inline] -> Bool
allBlank = all blankInline
  where
    blankInline (Str t)    = all (\c -> c == ' ' || c == '\t' || c == '\n') t
    blankInline (Strong x) = allBlank x
    blankInline (Emph x)   = allBlank x
    blankInline (Strike x) = allBlank x
    blankInline (CodeSpan t) = all (== ' ') t
    blankInline _          = False

-- Trim leading/trailing whitespace from a heading's inline run (headings
-- often carry a trailing space before \par).
trimInlines :: [Inline] -> [Inline]
trimInlines = trimTrail . trimLead
  where
    trimLead (Str t : rest) =
      let t' = dropWhile isWs t
      in if null t' then trimLead rest else Str t' : rest
    trimLead xs = xs
    -- reverse' (list reversed + each Str's chars flipped) is its own
    -- inverse, so trimming the "lead" of the doubly-reversed list and
    -- reversing back trims the true trailing whitespace with contents
    -- restored.
    trimTrail = reverse' . trimLead . reverse'
    reverse' = foldl (\acc x -> revI x : acc) []
    revI (Str t) = Str (reverse t)
    revI other   = other
    isWs c = c == ' ' || c == '\t' || c == '\n'

--------------------------------------------------------------------------
-- Byte decoding

-- Decode one raw byte under the active table.  ASCII (< 0x80) is
-- identity; high bytes go through cp1252 or MacRoman.  A byte with no
-- mapping (rare cp1252 holes) yields U+FFFD, kept as a single char so
-- offsets never surprise the caller.
decodeByte :: Enc -> Int -> String
decodeByte e b
  | b < 0x80  = [toEnum b]
  | otherwise = case (if e == EMac then macRoman else cp1252) b of
      Just cp -> [toEnum cp]
      Nothing -> "\xFFFD"

--------------------------------------------------------------------------
-- Mono-font harvest (first pass): read the fonttbl, collect font ids
-- whose family is \fmodern (monospaced) so \fN selects a code span.

harvestMonoFonts :: [Tok] -> [Int]
harvestMonoFonts toks = case findFontTbl toks of
  Nothing   -> []
  Just body -> collectMono body

findFontTbl :: [Tok] -> Maybe [Tok]
findFontTbl [] = Nothing
findFontTbl (TOpen : TCtrl "fonttbl" _ : rest) = Just (fst (splitGroup 0 rest))
findFontTbl (_ : rest) = findFontTbl rest

-- Walk fonttbl entries.  Each entry is either its own {\fN...} sub-group
-- or a flat "\fN\fmodern Courier;" run at the top level.  We scan
-- linearly, remembering the last \fN id, and record it when \fmodern is
-- seen before the entry's ';' terminator.
collectMono :: [Tok] -> [Int]
collectMono = go Nothing
  where
    go _   [] = []
    go cur (t:ts) = case t of
      TOpen -> let (inside, after) = splitGroup 0 ts
               in collectMono inside ++ go cur after
      TCtrl "f" (Just n)   -> go (Just n) ts
      TCtrl "fmodern" _    -> case cur of
                                Just n  -> n : go cur ts
                                Nothing -> go cur ts
      TChar ';'            -> go Nothing ts    -- entry terminator
      _                    -> go cur ts

--------------------------------------------------------------------------
-- Small char helpers (ASCII-only, per the no-Unicode-in-loops rule)

toLowerA :: Char -> Char
toLowerA c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- Parse a leading run of digits into Just n, or Nothing if none.
readNat1 :: String -> Maybe Int
readNat1 s = case span isDigit s of
  (ds@(_:_), _) -> Just (readNat ds)
  _             -> Nothing
