-- minimark: AbiWord reader (-f abw, T5.6).  Plain UTF-8 AWML XML, no
-- container — the easiest non-markdown reader, so it lands first to
-- prove MiniMark.Xml (T4.2) on a real mapping before odt/docx.
--
-- Mirrors MiniMark.Abw (the writer) in reverse: <p style="Heading N">
-- -> Heading, <c props="..."> CSS-ish strings -> Strong/Emph/Strike/
-- CodeSpan (props splitter is local, matching the writer's comment
-- that it shares with nothing), <a xlink:href> -> Link, attach-grid
-- <table>/<cell> -> Table.  The writer's list/code/quote/hrule
-- degradations (literal marker prefixes, one <p> per code line, extra
-- left-margin props, a dashed-line paragraph) are NOT losslessly
-- invertible from AWML alone — a real AbiWord document has no marker
-- to say "this Normal paragraph used to be a list item" — so they read
-- back as plain paragraphs, exactly the graceful-degradation rule
-- every reader in this codebase already follows for constructs it
-- cannot recover structurally.
--
-- Malformed/unexpected AWML never crashes: MiniMark.Xml is already
-- total (truncates on EOF, never errors), and every event walk here
-- degrades unknown elements to their text content or drops them.
module MiniMark.AbwReader(parseAbw) where

import Data.ByteString(ByteString)
import MiniMark.AST
import MiniMark.Xml(XmlEvent(..), parseXml, balanced, attr)

parseAbw :: ByteString -> Doc
parseAbw bs =
  let evs = parseXml bs
      section = findSection evs
  in Doc emptyMeta (blocks section)

-- Find the <section> element's content (first one; AbiWord documents
-- have exactly one for our purposes).  No <section> found (empty/
-- malformed input): empty document, never an error.
findSection :: [XmlEvent] -> [XmlEvent]
findSection [] = []
findSection (XStart "section" _ : rest) = fst (balanced rest)
findSection (_ : rest) = findSection rest

--------------------------------------------------------------------------
-- Blocks

blocks :: [XmlEvent] -> [Block]
blocks [] = []
blocks (XStart "p" as : rest) =
  let (inner, after) = balanced rest
  in pblock as inner : blocks after
blocks (XStart "table" _ : rest) =
  let (inner, after) = balanced rest
      (hdr, body) = tableRows inner
  in Table [] hdr body : blocks after
blocks (XStart _ _ : rest) =
  -- Unknown container: skip its content, keep walking siblings.
  blocks (snd (balanced rest))
blocks (_ : rest) = blocks rest

-- One <p style="..."> becomes Heading N or Para, per the writer's
-- style-name convention.  A block that is entirely a single <c
-- props="font-family:Courier New">...</c> line with no other styling
-- reads back as Para, not CodeBlock — see module comment: the writer's
-- one-paragraph-per-line degradation for code blocks has no marker to
-- undo, so it round-trips as prose (the code TEXT survives, its
-- block-level identity does not).
pblock :: [(String, String)] -> [XmlEvent] -> Block
pblock as inner = case attr "style" as of
  Just "Heading 1" -> Heading 1 is
  Just "Heading 2" -> Heading 2 is
  Just "Heading 3" -> Heading 3 is
  _                -> Para is
  where is = inlines inner

-- Rows from a flat <cell> list, grouped by top-attach and ordered by
-- left-attach.  Any cell missing/malformed attach props sorts as row 0,
-- col 0 rather than crashing or being dropped (graceful degradation).
tableRows :: [XmlEvent] -> ([[Inline]], [[[Inline]]])
tableRows evs = case grouped of
  []           -> ([], [])
  (hdr : body) -> (hdr, body)
  where
    cells = collectCells evs
    grouped = groupRows (sortCells cells)

collectCells :: [XmlEvent] -> [(Int, Int, [Inline])]
collectCells [] = []
collectCells (XStart "cell" as : rest) =
  let (inner, after) = balanced rest
      ps = propsOf as
  in (propInt "top-attach" ps, propInt "left-attach" ps, cellInlines inner)
     : collectCells after
collectCells (_ : rest) = collectCells rest

-- A cell's content is one or more <p> paragraphs (the writer emits
-- exactly one); pull inline content out of each <p>, ignoring any
-- whitespace-only XText sitting between <cell> and <p> tags (pretty-
-- printing artifacts, not document content).  Multiple paragraphs
-- (not produced by our own writer, but plausible from a hand-authored
-- .abw) join with a line break.
cellInlines :: [XmlEvent] -> [Inline]
cellInlines evs = case [is | is <- map paraOf (paras evs), not (null is)] of
  []    -> []
  [one] -> one
  many  -> intercalateInlines LineBreak many
  where
    paraOf (XStart "p" _, content) = inlines content
    paraOf _ = []
    paras [] = []
    paras (XStart "p" as : rs) =
      let (c, r) = balanced rs in (XStart "p" as, c) : paras r
    paras (_ : rs) = paras rs

intercalateInlines :: Inline -> [[Inline]] -> [Inline]
intercalateInlines sep = go
  where
    go []       = []
    go [x]      = x
    go (x : xs) = x ++ [sep] ++ go xs

-- Attach-grid coordinates live INSIDE the props string
-- ("top-attach:0; left-attach:1; ..."), not as their own XML
-- attributes, so look them up among the already-split "key:value"
-- prop fragments.
propInt :: String -> [String] -> Int
propInt k ps = case [drop 1 v | p <- ps, let (k', v) = break (== ':') p, k' == k] of
  (v : _) -> parseIntDefault 0 v
  []      -> 0

parseIntDefault :: Int -> String -> Int
parseIntDefault dflt s = case s of
  ('-' : ds) | not (null ds) && all isDigitA ds -> negate (foldl step 0 ds)
  ds         | not (null ds) && all isDigitA ds -> foldl step 0 ds
  _ -> dflt
  where
    step acc c = acc * 10 + (fromEnum c - fromEnum '0')
    isDigitA c = c >= '0' && c <= '9'

-- Stable sort by (row, col) without pulling in Data.List.sortBy's
-- comparator plumbing — insertion sort is fine, tables are small.
sortCells :: [(Int, Int, [Inline])] -> [(Int, Int, [Inline])]
sortCells = foldr ins []
  where
    ins c [] = [c]
    ins c@(r, k, _) (d@(r', k', _) : ds)
      | (r, k) <= (r', k') = c : d : ds
      | otherwise          = d : ins c ds

groupRows :: [(Int, Int, [Inline])] -> [[[Inline]]]
groupRows [] = []
groupRows cs@((r0, _, _) : _) =
  map (\(_, _, is) -> is) here : groupRows rest
  where (here, rest) = span (\(r, _, _) -> r == r0) cs

--------------------------------------------------------------------------
-- Inlines

inlines :: [XmlEvent] -> [Inline]
inlines [] = []
inlines (XText t : rest) = Str t : inlines rest
inlines (XStart "br" _ : rest) = LineBreak : inlines rest
inlines (XStart "c" as : rest) =
  let (inner, after) = balanced rest
  in wrapProps (propsOf as) (inlines inner) ++ inlines after
inlines (XStart "a" as : rest) =
  let (inner, after) = balanced rest
      url = maybe "" id (attr "xlink:href" as)
  in Link (inlines inner) url "" : inlines after
inlines (XStart _ _ : rest) =
  -- Unknown inline element: keep its text, drop the wrapper.
  let (inner, after) = balanced rest
  in inlines inner ++ inlines after
inlines (_ : rest) = inlines rest

-- Split "font-weight:bold; font-style:italic; ..." on "; " into
-- individual "key:value" props, local to this reader like the writer's
-- own splitter comment says.  Unrecognized/empty props are ignored
-- (graceful degradation: styling we don't model is simply dropped,
-- never a crash).
propsOf :: [(String, String)] -> [String]
propsOf as = case attr "props" as of
  Nothing -> []
  Just s  -> splitProps s

splitProps :: String -> [String]
splitProps s = case break (== ';') s of
  (p, ';' : rest) -> trim p : splitProps (dropWhile (== ' ') rest)
  (p, [])         -> if null (trim p) then [] else [trim p]
  _               -> []
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- Wrap already-built inline content in the Strong/Emph/Strike/CodeSpan
-- constructors implied by a <c>'s props, one layer per recognized prop,
-- nesting order doesn't matter (each is a distinct wrapper).  CodeSpan
-- only applies cleanly to flat text; if the run is pure Str content
-- (the common case — writer never nests <c> inside CodeSpan runs),
-- collapse it back to one CodeSpan, else keep the Courier styling as
-- CodeSpan around the flattened text (still lossless enough: content
-- survives, per the degradation rule).
wrapProps :: [String] -> [Inline] -> [Inline]
wrapProps [] is = is
wrapProps (p : ps) is = case p of
  "font-weight:bold"              -> [Strong (wrapProps ps is)]
  "font-style:italic"             -> [Emph (wrapProps ps is)]
  "text-decoration:line-through"  -> [Strike (wrapProps ps is)]
  "font-family:Courier New"       -> [CodeSpan (flatText (wrapProps ps is))]
  _                                -> wrapProps ps is

flatText :: [Inline] -> String
flatText = concatMap f
  where
    f (Str t)       = t
    f LineBreak     = " "
    f (Emph is)     = flatText is
    f (Strong is)   = flatText is
    f (Strike is)   = flatText is
    f (CodeSpan t)  = t
    f (Link t _ _)  = flatText t
    f (Image t _ _) = flatText t
    f (MathI raw _) = raw
