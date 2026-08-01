-- minimark: XML mini-parser (T4.2) — the substrate for the abw/odt/
-- docx/idml/Pages readers (Phase 5).
--
-- SAX-ish flat event stream, no DOM: every consumer folds `[XmlEvent]`
-- into a `Doc` by walking a fixed-prefix vocabulary (`w:p`, `text:h`,
-- `sf:p`, ...), so namespace resolution buys nothing and is not done —
-- prefixes stay literal in element/attribute names.  `parseXml` is
-- TOTAL: any bytes in, some (possibly empty) event list out, never a
-- crash, never an error channel.  Malformed input degrades by silent
-- truncation (see the caps below), matching MiniMark.Zip/Inflate.
--
-- Parse bytes, decode late: all XML structure characters are ASCII and
-- UTF-8 continuation bytes are >= 0x80 by construction, so a byte-at-a-
-- time scan can never split a multibyte sequence; `decodeUtf8` (lenient,
-- already in MiniMark.Bytes) is applied only to finished slices (a
-- name, an attribute value, a text run).  The driver is a guarded
-- unfold over a plain `Int` offset — never thread a growing accumulator
-- through the per-byte loop (the shape that overflowed the RTF reader's
-- stack, see docs/minimark-rtf-stackoverflow-analysis.md).  `balanced`
-- is the one place with a (tail-recursive, reversed-accumulator) depth
-- counter; the parser itself has none because its output is flat.
--
-- Full spec: docs/minimark-xml-parser-spec.md.
module MiniMark.Xml(
  XmlEvent(..),
  parseXml, xmlLooksUtf16,
  balanced, textUnder, attr
) where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)

import MiniMark.Bytes(byteAt, decodeUtf8)

data XmlEvent
  = XStart String [(String, String)]
  | XEnd String
  | XText String
  deriving (Eq)

-- UTF-16 BOM or a bare '<'/NUL pairing at the very start.  Callers must
-- check this before parseXml — the byte loop below assumes UTF-8 (or
-- ASCII-compatible) input and would otherwise emit NUL-riddled garbage.
xmlLooksUtf16 :: ByteString -> Bool
xmlLooksUtf16 bs =
  n >= 2 &&
  ( (b 0 == 0xFF && b 1 == 0xFE)
  || (b 0 == 0xFE && b 1 == 0xFF)
  || (b 0 == 0 && b 1 == 0x3C)
  || (b 0 == 0x3C && b 1 == 0) )
  where
    n = BS.length bs
    b = byteAt bs

maxNameLen :: Int
maxNameLen = 1024

maxAttrLen :: Int
maxAttrLen = 1024 * 1024

refLookahead :: Int
refLookahead = 32

-- Name start/continuation bytes (lenient superset of the XML spec:
-- also accepts any byte >= 0x80, i.e. any UTF-8 lead/continuation
-- byte, without decoding it here — decode happens once, on the whole
-- slice, in the caller).
isNameStart :: Int -> Bool
isNameStart b =
  (b >= 0x41 && b <= 0x5A) || (b >= 0x61 && b <= 0x7A)
  || b == 0x5F || b == 0x3A || b >= 0x80

isNameCont :: Int -> Bool
isNameCont b =
  isNameStart b || (b >= 0x30 && b <= 0x39) || b == 0x2D || b == 0x2E

isWs :: Int -> Bool
isWs b = b == 0x20 || b == 0x09 || b == 0x0D || b == 0x0A

parseXml :: ByteString -> [XmlEvent]
parseXml bs0 = go (skipBom 0)
  where
    n = BS.length bs0
    b = byteAt bs0

    skipBom i
      | i + 2 < n && b i == 0xEF && b (i + 1) == 0xBB && b (i + 2) == 0xBF
        = i + 3
      | otherwise = i

    -- Guarded unfold: never build a growing list argument here.
    go i
      | i >= n = []
      | b i == 0x3C = lt (i + 1)
      | otherwise = text i

    -- Just past a '<'.
    lt i
      | i >= n = []                          -- stray '<' at EOF: drop it
      | b i == 0x2F = endTag (i + 1)          -- </
      | b i == 0x21 = bang (i + 1)            -- <!
      | b i == 0x3F = pi_ (i + 1)             -- <?
      | isNameStart (b i) = startTag i
      | otherwise = skipJunkTag (i + 1)       -- '<' followed by garbage

    -- Unrecognized '<X...': consume to the next '>' and drop it (never
    -- emit a bogus event, never loop on the same byte).
    skipJunkTag i = go (skipToGt i)
    skipToGt i
      | i >= n = n
      | b i == 0x3E = i + 1
      | otherwise = skipToGt (i + 1)

    -- ---------------- character data ----------------
    text i =
      let e = textEnd i
      in if e == i then go e   -- shouldn't happen (text i implies b i /= '<'), guard anyway
                   else XText (resolveRefs (BS.take (e - i) (BS.drop i bs0))) : go e
    textEnd i
      | i >= n = i
      | b i == 0x3C = i
      | otherwise = textEnd (i + 1)

    -- ---------------- end tag: </name ...> ----------------
    endTag i =
      let ne = nameEnd i
          nm = sliceName i ne
          after = skipToGt ne
      in XEnd nm : go after

    -- ---------------- comment / PI / doctype / bogus bang ----------------
    bang i
      | matchAt i "--" = comment (i + 2)
      | matchAt i "[CDATA[" = cdata (i + 7)
      | matchAt i "DOCTYPE" = doctype (i + 7)
      | otherwise = go (skipToGt i)

    matchAt i s = i + length s <= n && all (\(k, c) -> b (i + k) == fromEnum c) (zip [0..] s)

    comment i = go (skipComment i)
    skipComment i
      | i + 2 >= n = n
      | b i == 0x2D && b (i + 1) == 0x2D && b (i + 2) == 0x3E = i + 3
      | otherwise = skipComment (i + 1)

    pi_ i = go (skipPi i)
    skipPi i
      | i + 1 >= n = n
      | b i == 0x3F && b (i + 1) == 0x3E = i + 2
      | otherwise = skipPi (i + 1)

    -- DOCTYPE ... [ ... ] ... >  — track one level of internal subset.
    doctype i = go (skipDoctype i)
    skipDoctype i
      | i >= n = n
      | b i == 0x5B = skipDoctype (skipToRBracket (i + 1))
      | b i == 0x3E = i + 1
      | otherwise = skipDoctype (i + 1)
    skipToRBracket i
      | i >= n = n
      | b i == 0x5D = i + 1
      | otherwise = skipToRBracket (i + 1)

    -- CDATA: emitted as XText verbatim (CR/LF normalized only), no
    -- reference resolution.  Unterminated: rest of input is the text.
    cdata i =
      let e = findCdataEnd i
      in if e < 0
           then [XText (normalizeNl (decodeUtf8 (BS.drop i bs0)))]
           else XText (normalizeNl (decodeUtf8 (BS.take (e - i) (BS.drop i bs0))))
                : go (e + 3)
    findCdataEnd i
      | i + 2 >= n = -1
      | b i == 0x5D && b (i + 1) == 0x5D && b (i + 2) == 0x3E = i
      | otherwise = findCdataEnd (i + 1)

    -- ---------------- start tag: <name attr="v" ...> or .../> ----------
    startTag i =
      let ne = nameEnd i
          nm = sliceName i ne
      in attrs nm [] ne

    -- attrs walks whitespace-separated "name=value" pairs, tolerating
    -- junk, until '>' or "/>" or EOF.  Bounded accumulator (attribute
    -- count per tag is small) — a reversed list is fine here.
    attrs nm acc i
      | i >= n = XStart nm (reverse acc) : []                 -- EOF mid-tag: emit what we have, no matching close synthesized
      | isWs (b i) = attrs nm acc (skipWs i)
      | b i == 0x2F && i + 1 < n && b (i + 1) == 0x3E =        -- />
          XStart nm (reverse acc) : XEnd nm : go (i + 2)
      | b i == 0x2F =                                         -- '/' not followed by '>': treat as junk, skip it
          attrs nm acc (i + 1)
      | b i == 0x3E =                                         -- >
          XStart nm (reverse acc) : go (i + 1)
      | isNameStart (b i) =
          let ae = attrNameEnd (i + 1)
              anm = sliceName i ae
          in attrValue nm acc anm ae
      | otherwise = attrs nm acc (i + 1)                      -- junk byte: skip

    skipWs i | i < n && isWs (b i) = skipWs (i + 1)
             | otherwise = i

    attrNameEnd i | i < n && isNameCont (b i) = attrNameEnd (i + 1)
                  | otherwise = i

    -- After an attribute name: optional ws, '=', optional ws, then a
    -- quoted or bare value.  No '=' at all: value is "".
    attrValue nm acc anm i0 =
      let i1 = skipWs i0
      in if i1 < n && b i1 == 0x3D
           then let i2 = skipWs (i1 + 1)
                in if i2 < n && (b i2 == 0x22 || b i2 == 0x27)
                     then
                       let q = b i2
                           ve = findQuote q (i2 + 1)
                           raw = BS.take (min maxAttrLen (ve - (i2 + 1))) (BS.drop (i2 + 1) bs0)
                           v = resolveAttrValue raw
                           after = if ve >= n then n else ve + 1
                       in attrs nm ((anm, v) : acc) after
                     else
                       let ve = bareValEnd i2
                           raw = BS.take (min maxAttrLen (ve - i2)) (BS.drop i2 bs0)
                           v = resolveAttrValue raw
                       in attrs nm ((anm, v) : acc) ve
           else attrs nm ((anm, "") : acc) i1

    findQuote q i
      | i >= n = n
      | b i == q = i
      | otherwise = findQuote q (i + 1)

    bareValEnd i
      | i >= n = i
      | isWs (b i) = i
      | b i == 0x3E = i
      | b i == 0x2F && i + 1 < n && b (i + 1) == 0x3E = i
      | otherwise = bareValEnd (i + 1)

    -- ---------------- shared name/slice helpers ----------------
    nameEnd i | i < n && isNameCont (b i) = nameEnd (i + 1)
              | otherwise = i
    sliceName i e =
      let cap = min e (i + maxNameLen)
      in decodeUtf8 (BS.take (cap - i) (BS.drop i bs0))

    -- Reference resolution over an already-extracted ByteString slice
    -- (a text run).  Decodes to String directly so entity/char refs can
    -- substitute mid-stream without a second pass.  CR/LF normalize to
    -- LF only.
    resolveRefs raw = normalizeNl (resolveOn (decodeUtf8 raw))

    -- Attribute values additionally normalize CR/LF/TAB to a single
    -- space each (XML attribute-value normalization), applied BEFORE
    -- entity resolution so a numeric ref can still produce a literal
    -- newline/tab in the value if the author really wants one.
    resolveAttrValue raw = resolveOn (attrSpace (decodeUtf8 raw))
    attrSpace s = map (\c -> if c == '\r' || c == '\n' || c == '\t' then ' ' else c) s

-- ---------------- reference resolution over decoded String ----------

-- CRLF and lone CR normalize to LF (both text and attribute values,
-- applied post-decode since it's char-level, not byte-level, work).
normalizeNl :: String -> String
normalizeNl ('\r' : '\n' : cs) = '\n' : normalizeNl cs
normalizeNl ('\r' : cs)        = '\n' : normalizeNl cs
normalizeNl (c : cs)           = c : normalizeNl cs
normalizeNl []                 = []

resolveOn :: String -> String
resolveOn [] = []
resolveOn ('&' : cs) = case tryRef cs of
  Just (ch, rest) -> ch : resolveOn rest
  Nothing         -> '&' : resolveOn cs
resolveOn (c : cs) = c : resolveOn cs

-- Look for a terminating ';' within refLookahead bytes; on success
-- resolve to one Char and return the remainder past ';'.  On any
-- failure (no ';' in range, unknown named entity, bad numeric ref)
-- fall back per the spec: unknown named entities pass through as
-- literal text (INCLUDING the & and the ;), everything else literal
-- '&' with scanning resumed at the next char.
tryRef :: String -> Maybe (Char, String)
tryRef cs =
  case break (== ';') (take refLookahead cs) of
    (body, ';' : _) | length body < refLookahead ->
      let rest = drop (length body + 1) cs
      in case resolveNamedOrNumeric body of
           Just ch -> Just (ch, rest)
           Nothing -> Nothing   -- unknown named entity: literal passthrough (handled by caller keeping '&')
    _ -> Nothing

resolveNamedOrNumeric :: String -> Maybe Char
resolveNamedOrNumeric body = case body of
  "lt"   -> Just '<'
  "gt"   -> Just '>'
  "amp"  -> Just '&'
  "apos" -> Just '\''
  "quot" -> Just '"'
  ('#' : 'x' : hexBody) -> numericChar 16 hexBody
  ('#' : 'X' : hexBody) -> numericChar 16 hexBody
  ('#' : decBody)       -> numericChar 10 decBody
  _ -> Nothing  -- unknown named entity: signal "not resolved" (passthrough happens in tryRef/resolveOn)

numericChar :: Int -> String -> Maybe Char
numericChar _ [] = Just '\xFFFD'
numericChar base digits
  | not (all (isValidDigit base) digits) = Just '\xFFFD'
  | otherwise =
      let v = foldl (\acc d -> min 0x110000 (acc * base + digitVal d)) 0 digits
      in Just (codeToChar v)
  where
    isValidDigit 16 c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    isValidDigit _  c = c >= '0' && c <= '9'
    digitVal c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | otherwise            = fromEnum c - fromEnum 'A' + 10

codeToChar :: Int -> Char
codeToChar v
  | v == 0 = '\xFFFD'
  | v > 0x10FFFF = '\xFFFD'
  | v >= 0xD800 && v <= 0xDFFF = '\xFFFD'
  | otherwise = toEnum v

-- ---------------- consumer helpers ----------------

-- Depth-counted, name-blind: any XStart is +1, any XEnd is -1, so
-- mismatched tags still terminate.  The matching XEnd is dropped from
-- the "inside" half.  EOF before balance: (everything, []).
balanced :: [XmlEvent] -> ([XmlEvent], [XmlEvent])
balanced es = go es 0 []
  where
    go [] _ acc = (reverse acc, [])
    go (e : rest) d acc = case e of
      XStart _ _ -> go rest (d + 1) (e : acc)
      XEnd _
        | d == 0 -> (reverse acc, rest)
        | otherwise -> go rest (d - 1) (e : acc)
      XText _ -> go rest d (e : acc)

textUnder :: [XmlEvent] -> String
textUnder = concatMap f
  where
    f (XText s) = s
    f _         = ""

attr :: String -> [(String, String)] -> Maybe String
attr k as = case [v | (k', v) <- as, k' == k] of
  (v : _) -> Just v
  []      -> Nothing
