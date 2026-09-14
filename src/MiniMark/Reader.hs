-- minimark: Markdown reader.  Line-oriented block parser plus a
-- recursive-descent inline parser.  Deliberately dependency-free:
-- runs on MicroHs base and ports verbatim to Idris2 later.
module MiniMark.Reader(
  MdOpts(..), defMdOpts,
  parseDocument, parseDocumentWith, parseDoc, parseInlines
) where

import MiniMark.CharClass(isSp, isDig, isAlphaA, isAlnumA)
import Data.List(isPrefixOf)
import MiniMark.AST
import MiniMark.TexMath(parseMath)

-- Link/image reference definitions, collected by the pre-pass (T1.2) and
-- threaded through block/inline parsing so [text][label] can resolve.
-- Label keys are already ASCII-lowercased (see foldLabel).
type Defs = [(String, (String, String))]   -- label -> (url, title)

-- Markdown dialect switches.  Pandoc spells these as "+extension"
-- suffixes on the format name (markdown+hard_line_breaks); this is the
-- parsed form, and Readers.parseFromSpec owns the spelling.
data MdOpts = MdOpts
  { moHardBreaks :: Bool   -- every newline inside a paragraph is a break
  }

defMdOpts :: MdOpts
defMdOpts = MdOpts False

-- What the block parser threads down: the reference definitions the
-- pre-pass collected, plus the dialect switches.  Inline parsing only
-- ever needs the definitions, so it keeps taking a bare Defs.
data Ctx = Ctx
  { cDefs :: Defs
  , cOpts :: MdOpts
  }

-- Reader-contract entry point (see Readers.hs): whole input -> Doc.
parseDocument :: String -> Doc
parseDocument = parseDocumentWith defMdOpts

-- Metadata comes from a YAML front-matter block or, failing that, a
-- pandoc percent title block -- checked in that order because "---" on
-- line 1 is unambiguous while '%' is not.
parseDocumentWith :: MdOpts -> String -> Doc
parseDocumentWith o s =
  let ls = map (expandTabs . dropCR) (lines s)
  in case ls of
       "---":rest -> case extractFrontMatter rest of
         Just (meta, body) -> Doc meta (parseLines o body)
         Nothing           -> fromTitleBlock o ls
       _ -> fromTitleBlock o ls
  where dropCR l = [c | c <- l, c /= '\r']

fromTitleBlock :: MdOpts -> [String] -> Doc
fromTitleBlock o ls = case extractTitleBlock ls of
  Just (meta, body) -> Doc meta (parseLines o body)
  Nothing           -> Doc emptyMeta (parseLines o ls)

-- Pandoc's percent title block (its pandoc_title_block extension): a
-- document whose first line starts with '%' opens with up to three
-- '%'-introduced fields -- title, author(s), date -- each continuable
-- on following indented lines.  Every man page in the wild carries its
-- .TH arguments this way, so the title field is additionally split as
-- "NGS(1) NGS User Manual" -> name / section / manual header.
extractTitleBlock :: [String] -> Maybe (Meta, [String])
extractTitleBlock ls0 = case ls0 of
  (l:_) | "%" `isPrefixOf` l ->
    let (fields, body) = takeFields (3 :: Int) ls0
    in Just (metaOfFields fields, body)
  _ -> Nothing
  where
    takeFields n (l:ls)
      | n > 0 && "%" `isPrefixOf` l =
          let (cont, rest) = span isCont ls
              (fs, body)   = takeFields (n - 1) rest
          in (trim (unwords (trim (drop 1 l) : map trim cont)) : fs, body)
    takeFields _ ls = ([], ls)
    -- A field continues onto the next line only while that line is
    -- indented; '%' or column 0 starts the next field or the body.
    isCont l = not (isBlank l) && isSp (head l)
    metaOfFields fs =
      let fld i = case drop i fs of
                    (v:_) | not (null v) -> Just v
                    _                    -> Nothing
          (nm, sec, man) = splitManTitle (maybe "" id (fld 0))
      in Meta (if null nm then Nothing else Just nm) (fld 1) (fld 2) sec man

-- "NGS(1) NGS User Manual" -> ("NGS", Just "1", Just "NGS User Manual").
-- A title not shaped that way stays whole and carries no section.
splitManTitle :: String -> (String, Maybe String, Maybe String)
splitManTitle t = case break (== '(') t of
  (nm, _:r) | not (null (trim nm)) -> case break (== ')') r of
    (sec, _:rest)
      | not (null sec) && all (\c -> not (isSp c) && c /= '(') sec ->
          (trim nm, Just sec, nonNull (trim rest))
    _ -> (trim t, Nothing, Nothing)
  _ -> (trim t, Nothing, Nothing)
  where nonNull x = if null x then Nothing else Just x

-- Front matter (T1.4): input starts with "---" on line 1; scan for a
-- closing "---" or "..." line. Between them, collect top-level
-- title:/author:/date: by exact lowercase line-prefix (YAML is
-- case-sensitive; we honor that); value = rest of line trimmed, one
-- matching pair of "/' quotes stripped. Everything else in the block
-- is ignored. No closing fence found -> not front matter (Nothing):
-- caller falls back to today's parse ("---" as HRule).
extractFrontMatter :: [String] -> Maybe (Meta, [String])
extractFrontMatter ls =
  case break (\l -> trim l == "---" || trim l == "...") ls of
    (_, []) -> Nothing
    (block, _closeLine:body) ->
      Just (foldl applyLine emptyMeta block, body)
  where
    applyLine m l
      | Just v <- stripPfx "title:" l  = m{mTitle = Just (unquote v)}
      | Just v <- stripPfx "author:" l = m{mAuthor = Just (unquote v)}
      | Just v <- stripPfx "date:" l   = m{mDate = Just (unquote v)}
      | otherwise = m
    stripPfx pfx l = if pfx `isPrefixOf` l then Just (trim (drop (length pfx) l)) else Nothing
    unquote v = case v of
      '"':r | not (null r) && last r == '"'  -> init r
      '\'':r | not (null r) && last r == '\'' -> init r
      _ -> v

parseDoc :: String -> [Block]
parseDoc s = parseLines defMdOpts (map (expandTabs . dropCR) (lines s))
  where dropCR l = [c | c <- l, c /= '\r']

-- Shared by parseDoc and parseDocument (post front-matter): lines
-- already tab-expanded and CR-stripped.
parseLines :: MdOpts -> [String] -> [Block]
parseLines o ls =
  let (defs, ls') = stripDefs ls
  in parseBlocks (Ctx defs o) ls'

expandTabs :: String -> String
expandTabs = concatMap (\c -> if c == '\t' then "    " else [c])

isBlank :: String -> Bool
isBlank = all isSp

trim :: String -> String
trim = dropWhile isSp . dropWhileEnd isSp

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x:xs) []

indentOf :: String -> Int
indentOf = length . takeWhile (== ' ')

--------------------------------------------------------------------------
-- Link/image reference definitions: a line-oriented pre-pass shared by
-- link defs here and footnote defs (T1.3, not yet implemented). Runs
-- before parseBlocks; skips fenced-code and $$ regions (a def-looking
-- line inside a fence is content, not a definition) and never looks
-- inside list-item continuation lines (kept indent-blind on purpose).

-- ASCII-only case fold for label matching (rule: no Data.Char Unicode
-- predicates in parsing loops). Non-ASCII label chars compare verbatim.
foldLabel :: String -> String
foldLabel = map lo
  where lo c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- `[label]: url` optionally followed by "title"/'title'/(title), at
-- 0-3 spaces indent, optionally after '>' quote markers.
defLine :: String -> Maybe (String, (String, String))
defLine l0 =
  let l = dropQuote (dropIndent3 l0)
  in case l of
       '[':r -> case matchBracket 0 "" r of
         Just (lbl, ':':r2) | not (null lbl) ->
           let (url, title) = splitTitle (trim r2)
           in if null url then Nothing else Just (foldLabel lbl, (url, title))
         _ -> Nothing
       _ -> Nothing
  where
    dropQuote s = case dropWhile (== ' ') s of
      '>':s' -> dropWhile (== ' ') s'
      _       -> s

-- Strip matched definition lines from the input, skipping fenced-code
-- and $$ regions (their contents pass through untouched) and list-item
-- continuation lines (left alone even if they look like a def). First
-- definition of a label wins, so defs accumulate in document order.
stripDefs :: [String] -> (Defs, [String])
stripDefs ls0 = go [] ls0
  where
    go seen [] = (seen, [])
    go seen (l:ls)
      | Just _ <- fenceOpen l =
          let (body, rest) = break fenceClose ls
              closeLine = case rest of (c:_) -> [c]; [] -> []
              afterClose = drop 1 rest
              (defs, rest') = go seen afterClose
          in (defs, l : body ++ closeLine ++ rest')
      | dmathOpen l && not (dmathOneLiner l) =
          let (body, rest) = break dmathClose ls
              closeLine = case rest of (c:_) -> [c]; [] -> []
              afterClose = drop 1 rest
              (defs, rest') = go seen afterClose
          in (defs, l : body ++ closeLine ++ rest')
      | Just _ <- listMarker l =
          -- Lists are left entirely alone: def-collection is indent-blind
          -- and must not reach into item continuation lines.
          let (items, rest) = span (not . isBlank) ls
              (defs, rest') = go seen rest
          in (defs, l : items ++ rest')
      | Just kv@(k, _) <- defLine l =
          let seen' = if any ((== k) . fst) seen then seen else seen ++ [kv]
          in go seen' ls
      | otherwise =
          let (defs, rest) = go seen ls in (defs, l : rest)

dmathOneLiner :: String -> Bool
dmathOneLiner l =
  let inner = drop 2 (trim l)
  in length inner >= 2 && "$$" `isPrefixOf` reverse inner

dmathClose :: String -> Bool
dmathClose x = "$$" `isPrefixOf` reverse (trim x) || trim x == "$$"

--------------------------------------------------------------------------
-- Block structure

parseBlocks :: Ctx -> [String] -> [Block]
parseBlocks _ [] = []
parseBlocks ctx (l:ls)
  | isBlank l                = parseBlocks ctx ls
  | indentOf l >= 4          = indentedCode ctx (l:ls)
  | Just lang <- fenceOpen l = fencedCode ctx lang ls
  | dmathOpen l              = displayMath ctx (l:ls)
  | Just (n,t) <- heading l  = Heading n (parseInlines' (cDefs ctx) t) : parseBlocks ctx ls
  | hruleLine l              = HRule : parseBlocks ctx ls
  | quoteLine l              = quoteBlock ctx (l:ls)
  | tableStart (l:ls)        = tableBlock ctx (l:ls)
  | Just _ <- listMarker l   = listBlock ctx (l:ls)
  | otherwise                = paraBlock ctx (l:ls)

-- Indented code block (CommonMark 4.4): four or more spaces at a block
-- boundary, four of them stripped.  Tested BEFORE every other rule
-- because the others are indent-blind in ways that would misread code:
-- hruleLine filters all spaces out, dmathOpen and tableStart trim, so
-- an indented "---", "$$" or "|" line would come back as markup.
--
-- It cannot interrupt a paragraph, which needs no code here: paraBlock
-- consumes its own continuation lines before parseBlocks sees them,
-- and that is exactly CommonMark's rule.  Inside a list item it works
-- on the item's own indentation, collectItems having already stripped
-- the marker width.
indentedCode :: Ctx -> [String] -> [Block]
indentedCode ctx ls =
  let (body, rest) = spanCode ls
  in CodeBlock "" (map (drop 4) body) : parseBlocks ctx rest
  where
    spanCode [] = ([], [])
    spanCode (x:xs)
      | indentOf x >= 4 = let (b, r) = spanCode xs in (x : b, r)
      | isBlank x =
          -- A blank run belongs to the block only when code resumes
          -- after it; a trailing run ends the block and stays outside.
          case spanCode xs of
            (b, r) | not (null b) -> (x : b, r)
            _                     -> ([], x:xs)
      | otherwise = ([], x:xs)

-- ``` or ~~~ fences, up to 3 leading spaces, optional language word
fenceOpen :: String -> Maybe String
fenceOpen l =
  let l' = dropIndent3 l in
  case span (== '`') l' of
    (bt, rest) | length bt >= 3 -> Just (trim rest)
    _ -> case span (== '~') l' of
           (td, rest) | length td >= 3 -> Just (trim rest)
           _ -> Nothing

fenceClose :: String -> Bool
fenceClose l =
  let l' = trim l
  in (length l' >= 3) && (all (== '`') l' || all (== '~') l')

dropIndent3 :: String -> String
dropIndent3 l = let (sp, r) = span (== ' ') l
                in if length sp <= 3 then r else l

fencedCode :: Ctx -> String -> [String] -> [Block]
fencedCode ctx lang ls =
  let (body, rest) = break fenceClose ls
  in CodeBlock lang body : parseBlocks ctx (drop 1 rest)

-- $$ ... $$ display math (single- or multi-line)
dmathOpen :: String -> Bool
dmathOpen l = "$$" `isPrefixOf` trim l

displayMath :: Ctx -> [String] -> [Block]
displayMath ctx (l:ls) =
  let t = trim l
      inner = drop 2 t
  in if length inner >= 2 && "$$" `isPrefixOf` reverse inner
       -- one-liner:  $$ e = mc^2 $$
       then let raw = trim (take (length inner - 2) inner)
            in DisplayMath raw (parseMath raw) : parseBlocks ctx ls
       else let (body, rest) = break (\x -> "$$" `isPrefixOf` reverse (trim x)
                                            || trim x == "$$") ls
                lastRaw = case rest of
                            (r:_) -> let t' = trim r
                                     in [take (length t' - 2) t' | t' /= "$$"]
                            []    -> []
                raw = trim (unwords (filter (not . null) (inner : body ++ lastRaw)))
            in DisplayMath raw (parseMath raw) : parseBlocks ctx (drop 1 rest)
displayMath _ [] = []

heading :: String -> Maybe (Int, String)
heading l =
  let (hs, rest) = span (== '#') (dropIndent3 l)
      n = length hs
  in if n >= 1 && n <= 6 && (null rest || " " `isPrefixOf` rest)
       then Just (n, trim (dropWhileEnd (== '#') (trim rest)))
       else Nothing

-- ***, ---, ___ (spaces allowed between), >= 3 marks
hruleLine :: String -> Bool
hruleLine l =
  let cs = filter (not . isSp) l
  in length cs >= 3 && not (null cs)
     && head cs `elem` "-*_" && all (== head cs) cs

quoteLine :: String -> Bool
quoteLine l = ">" `isPrefixOf` dropIndent3 l

quoteBlock :: Ctx -> [String] -> [Block]
quoteBlock ctx ls =
  let (qs, rest) = span (\x -> quoteLine x || (not (isBlank x) && lazyOK x)) ls
      lazyOK x = not (startsNewBlock [x])   -- lazy continuation lines
      strip x = case dropIndent3 x of
                  '>':' ':r -> r
                  '>':r     -> r
                  r         -> r
  in Quote (parseBlocks ctx (map strip qs)) : parseBlocks ctx rest

startsNewBlock :: [String] -> Bool
startsNewBlock []      = True
startsNewBlock (l:ls') =
     isBlank l || hruleLine l
  || maybe False (const True) (fenceOpen l)
  || maybe False (const True) (heading l)
  || quoteLine l
  || maybe False (const True) (listMarker l)
  || dmathOpen l
  || tableStart (l:ls')

--------------------------------------------------------------------------
-- Lists

-- Returns (isOrdered, startNum, markerWidth incl. trailing spaces)
listMarker :: String -> Maybe (Bool, Int, Int)
listMarker l =
  let ind = indentOf l
      l'  = drop ind l
  in if ind > 3 then Nothing else
     case l' of
       (c:' ':_) | c `elem` "-*+" -> Just (False, 1, ind + 2)
       _ -> case span isDig l' of
              (ds@(_:_), p:' ':_) | p == '.' || p == ')'
                  -> Just (True, readInt ds, ind + length ds + 2)
              _   -> Nothing

readInt :: String -> Int
readInt = foldl (\a c -> a * 10 + (fromEnum c - fromEnum '0')) 0

listBlock :: Ctx -> [String] -> [Block]
listBlock ctx ls@(l:_) =
  case listMarker l of
    Nothing -> paraBlock ctx ls
    Just (ord, start, _) ->
      let (items, rest) = collectItems ord ls
          mkItem (checked, lns) = ListItem checked (parseBlocks ctx lns)
          blocks = map mkItem items
      in (if ord then OrderedList start blocks else BulletList blocks)
         : parseBlocks ctx rest
listBlock _ [] = []

-- `[ ] `/`[x] `/`[X] ` at the start of an item's first line -> checkbox.
checkbox :: String -> Maybe (Bool, String)
checkbox l = case l of
  '[':' ':']':' ':r -> Just (False, r)
  '[':c:']':' ':r | c `elem` "xX" -> Just (True, r)
  _ -> Nothing

-- Split consecutive lines into items of a list of the given family,
-- each tagged with its checkbox state (marker width grows by 4 when
-- a checkbox is present, so continuation lines indent past it).
collectItems :: Bool -> [String] -> ([(Maybe Bool, [String])], [String])
collectItems ord (l:ls) =
  case listMarker l of
    Just (o, _, w0) | o == ord ->
      let raw = drop w0 l
          (checked, content, w) = case checkbox raw of
            Just (c, r) -> (Just c, r, w0 + 4)
            Nothing     -> (Nothing, raw, w0)
          (cont, rest) = itemCont w ls
          (moreItems, rest') = collectItems ord rest
      in ((checked, content : cont) : moreItems, rest')
    _ -> ([], l:ls)
collectItems _ [] = ([], [])

-- Continuation lines of one item: indented >= w, or blank lines that are
-- followed by further indented content.
itemCont :: Int -> [String] -> ([String], [String])
itemCont w (l:ls)
  | isBlank l =
      case ls of
        (n:_) | not (isBlank n) && indentOf n >= w ->
          let (cs, rest) = itemCont w ls in ("" : cs, rest)
        _ -> ([], l:ls)
  | indentOf l >= w =
      let (cs, rest) = itemCont w ls in (drop w l : cs, rest)
  | otherwise = ([], l:ls)
itemCont _ [] = ([], [])

--------------------------------------------------------------------------
-- Pipe tables

tableStart :: [String] -> Bool
tableStart (h:s:_) = '|' `elem` h && tableSep s
tableStart _       = False

tableSep :: String -> Bool
tableSep l =
  let t = trim l
  in not (null t) && '-' `elem` t && all (`elem` "|-: ") t && '|' `elem` t

tableBlock :: Ctx -> [String] -> [Block]
tableBlock ctx (h:s:ls) =
  let (rows, rest) = span (\x -> '|' `elem` x && not (isBlank x)) ls
      aligns = map alignOf (splitCells s)
      hdr = map (parseInlines' (cDefs ctx)) (splitCells h)
      body = map (map (parseInlines' (cDefs ctx)) . padTo (length hdr) . splitCells) rows
  in Table (padAligns (length hdr) aligns) hdr body : parseBlocks ctx rest
tableBlock ctx ls = paraBlock ctx ls

padTo :: Int -> [String] -> [String]
padTo n xs = take n (xs ++ repeat "")

padAligns :: Int -> [Align] -> [Align]
padAligns n xs = take n (xs ++ repeat ALeft)

alignOf :: String -> Align
alignOf cell =
  let t = trim cell
      l = ":" `isPrefixOf` t
      r = ":" `isPrefixOf` reverse t
  in if l && r then ACenter else if r then ARight else ALeft

-- Split on unescaped '|', dropping the outer empty cells produced by
-- leading/trailing pipes.
splitCells :: String -> [String]
splitCells l = dropOuter (go (trim l) "")
  where
    go [] acc = [reverse acc]
    go ('\\':'|':r) acc = go r ('|':'\\':acc)
    go ('|':r) acc = reverse acc : go r ""
    go (c:r) acc = go r (c:acc)
    dropOuter cs =
      let cs1 = case cs of ("":t) -> t; _ -> cs
      in case reverse cs1 of
           ("":t) -> map trim (reverse t)
           _      -> map trim cs1

--------------------------------------------------------------------------
-- Paragraphs

paraBlock :: Ctx -> [String] -> [Block]
paraBlock ctx ls =
  let (ps, rest) = breakPara ls
  in Para (parseInlines' (cDefs ctx) (joinPara (moHardBreaks (cOpts ctx)) ps))
     : parseBlocks ctx rest

-- Join a paragraph's physical lines into the single string the inline
-- parser sees.  A '\n' in that string means a hard break and nothing
-- else -- Str stays newline-free (see AST), inl turns it into
-- LineBreak -- while a soft break is a plain space, as before.
--
-- A line breaks hard when it ends in two or more spaces or in an
-- unescaped backslash (CommonMark 6.7), and under hard_line_breaks
-- every line does.  The trailing spaces or the backslash are consumed
-- by the break and never reach the output.  The last line has no break
-- to carry, so it keeps a trailing backslash as the literal character
-- CommonMark says it is.
joinPara :: Bool -> [String] -> String
joinPara hard = go
  where
    go []       = ""
    go [l]      = trim l
    go (l:rest) = let (body, brk) = lineBreak l
                  in trim body ++ [if brk then '\n' else ' '] ++ go rest
    lineBreak l =
      let t   = dropWhileEnd isSp l
          nsp = length l - length t
          bs  = length (takeWhile (== '\\') (reverse t))
      in if nsp >= 2 then (t, True)
         else if odd bs then (take (length t - 1) t, True)
         else (t, hard)

breakPara :: [String] -> ([String], [String])
breakPara [] = ([], [])
breakPara ls@(l:rest)
  | startsNewBlock ls = ([], ls)
  | otherwise = let (ps, r) = breakPara rest in (l:ps, r)

--------------------------------------------------------------------------
-- Inlines.  `prev` is the preceding character (for word-boundary rules).

-- Public entry point: no reference definitions in scope (table cells,
-- headings and callers outside this module all go through parseBlocks
-- instead, which threads the real Defs collected by the pre-pass).
parseInlines :: String -> [Inline]
parseInlines = parseInlines' []

parseInlines' :: Defs -> String -> [Inline]
parseInlines' defs = inl defs ' '

inl :: Defs -> Char -> String -> [Inline]
inl _ _ [] = []
inl defs prev s = case s of
  -- joinPara puts a '\n' in only where it means a hard break, and this
  -- is the one place that consumes it: no Str ever holds a newline.
  '\n':r -> LineBreak : inl defs ' ' r
  '\\':c:r | c `elem` "\\`*_{}[]()#+-.!|$<>~" -> prepend c (inl defs c r)
  '`':_ -> codeSpan defs prev s
  '$':r -> mathSpan defs prev r s
  '*':'*':r -> delim defs prev "**" Strong r s
  '*':r     -> delim defs prev "*"  Emph   r s
  '_':'_':r | not (isAlnumA prev) -> delim defs prev "__" Strong r s
  '_':r     | not (isAlnumA prev) -> delim defs prev "_"  Emph   r s
  '~':'~':r -> delim defs prev "~~" Strike r s
  '!':'[':r -> imageSpan defs prev r s
  '[':r -> linkSpan defs prev r s
  '<':r | httpish r -> autoAngle defs r s
  c:r | c == 'h' && httpish s && wordStart prev -> bareUrl defs s
      | otherwise -> prepend c (inl defs c r)
  where
    wordStart p = isSp p || p `elem` "(["

httpish :: String -> Bool
httpish t = "http://" `isPrefixOf` t || "https://" `isPrefixOf` t

-- Raw spans captured as text (code, math) flatten an embedded hard
-- break back to a space rather than carrying a newline into the AST.
unNl :: Char -> Char
unNl c = if c == '\n' then ' ' else c

prepend :: Char -> [Inline] -> [Inline]
prepend c (Str t : is) = Str (c:t) : is
prepend c is           = Str [c] : is

-- `code`, with multi-backtick fences
codeSpan :: Defs -> Char -> String -> [Inline]
codeSpan defs _ s =
  let (bt, rest) = span (== '`') s
      n = length bt
  in case findRun n rest of
       -- A hard break inside a code span is a space (CommonMark 6.1),
       -- which also keeps the newline-free promise for CodeSpan text.
       Just (inside, after) -> CodeSpan (map unNl inside) : inl defs '`' after
       Nothing -> prepend '`' (inl defs '`' (drop 1 s))

-- find a run of exactly n backticks
findRun :: Int -> String -> Maybe (String, String)
findRun n = go ""
  where
    go _   [] = Nothing
    go acc t@('`':_) =
      let (bt, r) = span (== '`') t
      in if length bt == n then Just (reverse acc, r)
         else go (reverse bt ++ acc) r
    go acc (c:r) = go (c:acc) r

-- $math$ with pandoc-style guards: no space right after opening $,
-- no space right before closing $, closing $ not followed by a digit.
mathSpan :: Defs -> Char -> String -> String -> [Inline]
mathSpan defs _ r orig =
  case r of
    (c0:_) | not (isSp c0) && c0 /= '$' ->
      case scanMath "" r of
        Just (raw0, after) ->
          let raw = map unNl raw0
          in MathI raw (parseMath raw) : inl defs '$' after
        Nothing -> prepend '$' (inl defs '$' r)
    _ -> prepend '$' (inl defs '$' r)
  where
    scanMath acc ('\\':c:t) = scanMath (c:'\\':acc) t
    scanMath acc ('$':t)
      | not (null acc) && not (isSp (head acc))
        && (null t || not (isDig (head t))) = Just (reverse acc, t)
      | otherwise = Nothing
    scanMath acc (c:t) = scanMath (c:acc) t
    scanMath _ [] = Nothing
    _unused = orig

-- Emphasis delimiters with nesting-aware close search.
delim :: Defs -> Char -> String -> ([Inline] -> Inline) -> String -> String -> [Inline]
delim defs _ d ctor r orig =
  case findClose d r of
    Just (inside, after) | not (null (trim inside)) ->
      ctor (parseInlines' defs inside) : inl defs (last d) after
    _ -> prepend (head d) (inl defs (head d) (drop 1 orig))

findClose :: String -> String -> Maybe (String, String)
findClose d = go ""
  where
    go _ [] = Nothing
    go acc ('\\':c:t) = go (c:'\\':acc) t
    go acc t@('`':_) =                      -- skip code spans wholesale
      let (bt, r1) = span (== '`') t
      in case findRun (length bt) r1 of
           Just (inside, r2) -> go (reverse (bt ++ inside ++ bt) ++ acc) r2
           Nothing -> go (reverse bt ++ acc) r1
    go acc t@('*':'*':r)
      | d == "**" = Just (reverse acc, r)
      | d == "*" =                           -- skip nested strong
          case findClose "**" r of
            Just (inside, r2) -> go (reverse ("**" ++ inside ++ "**") ++ acc) r2
            Nothing -> go ('*':'*':acc) r
      | otherwise = go ('*':'*':acc) r
      where _ = t
    go acc ('~':'~':r)
      | d == "~~" = Just (reverse acc, r)
      | otherwise = go ('~':'~':acc) r
    go acc (c:t)
      | [c] == d = Just (reverse acc, t)
      | c == head d && [c] == "_" && d == "__" =
          case t of ('_':r2) -> Just (reverse acc, r2)
                    _        -> go (c:acc) t
      | otherwise = go (c:acc) t

-- [text](url), [text](url "title"), [text][label], [label][], [label]
linkSpan :: Defs -> Char -> String -> String -> [Inline]
linkSpan defs _ r orig =
  case matchBracket 0 "" r of
    Just (txt, r2) -> resolveTail defs orig (\u t -> Link (parseInlines' defs txt) u t) txt r2
    Nothing -> fallback defs orig
  where
    fallback ds o = prepend (head o) (inl ds (head o) (drop 1 o))

-- ![alt](url), ![alt](url "title"), ![alt][label], ![label][], ![label]
imageSpan :: Defs -> Char -> String -> String -> [Inline]
imageSpan defs _ r orig =
  case matchBracket 0 "" r of
    Just (txt, r2) -> resolveTail defs orig (\u t -> Image (parseInlines' defs txt) u t) txt r2
    Nothing -> fallback
  where
    fallback = prepend (head orig) (inl defs (head orig) (drop 1 orig))

-- Shared tail resolution after the `[...]` bracket: try inline (url [title]),
-- then reference forms [label][], [label], falling back to literal text
-- when nothing resolves (graceful degradation).
resolveTail :: Defs -> String -> (String -> String -> Inline) -> String -> String -> [Inline]
resolveTail defs orig mk txt r2 = case r2 of
  '(':r3 -> case matchParen 0 "" r3 of
    Just (inside, r4) -> let (url, title) = splitTitle inside
                          in mk url title : inl defs ')' r4
    Nothing -> fallback
  '[':r3 -> case matchBracket 0 "" r3 of
    Just ("", r4) -> refOr txt r4          -- collapsed [label][]
    Just (lbl, r4) -> refOr lbl r4         -- full [text][label]
    Nothing -> shortcut
  _ -> shortcut
  where
    fallback = prepend (head orig) (inl defs (head orig) (drop 1 orig))
    shortcut = refOr txt r2
    refOr lbl rest = case lookup (foldLabel lbl) defs of
      Just (url, title) -> mk url title : inl defs ']' rest
      Nothing -> fallback

matchBracket :: Int -> String -> String -> Maybe (String, String)
matchBracket _ _ [] = Nothing
matchBracket n acc ('\\':c:t) = matchBracket n (c:'\\':acc) t
matchBracket n acc ('[':t) = matchBracket (n+1) ('[':acc) t
matchBracket n acc (']':t)
  | n == 0 = Just (reverse acc, t)
  | otherwise = matchBracket (n-1) (']':acc) t
matchBracket n acc (c:t) = matchBracket n (c:acc) t

matchParen :: Int -> String -> String -> Maybe (String, String)
matchParen _ _ [] = Nothing
matchParen n acc ('(':t) = matchParen (n+1) ('(':acc) t
matchParen n acc (')':t)
  | n == 0 = Just (reverse acc, t)
  | otherwise = matchParen (n-1) (')':acc) t
matchParen n acc (c:t) = matchParen n (c:acc) t

-- Split "url" / "url "title"" / "url 'title'" / "url (title)" — the
-- title is the last whitespace-separated quoted/paren group, if any.
-- A group NOT preceded by a space belongs to the url: wikipedia-style
-- ".../Haskell_(programming_language)" urls must survive whole.
splitTitle :: String -> (String, String)
splitTitle inside =
  let t = trim inside
  in case reverse t of
       '"':rest -> case break (== '"') rest of
         (rtitle, '"':rurl@(' ':_)) -> (trim (reverse rurl), reverse rtitle)
         _ -> (t, "")
       '\'':rest -> case break (== '\'') rest of
         (rtitle, '\'':rurl@(' ':_)) -> (trim (reverse rurl), reverse rtitle)
         _ -> (t, "")
       ')':rest -> case break (== '(') rest of
         (rtitle, '(':rurl@(' ':_)) -> (trim (reverse rurl), reverse rtitle)
         _ -> (t, "")
       _ -> (t, "")

-- <http://...>
autoAngle :: Defs -> String -> String -> [Inline]
autoAngle defs r orig =
  case break (== '>') r of
    (url, '>':after) -> Link [Str url] url "" : inl defs '>' after
    _ -> prepend '<' (inl defs '<' (drop 1 orig))

-- bare http(s)://... url, trailing punctuation trimmed
bareUrl :: Defs -> String -> [Inline]
bareUrl defs s =
  let (u0, _) = break (\c -> isSp c || c `elem` "<>\"") s
      trailPunct = ".,;:!?)]}"
      u = dropWhileEnd (`elem` trailPunct) u0
      rest = drop (length u) s
  in Link [Str u] u "" : inl defs (last ('x':u)) rest
