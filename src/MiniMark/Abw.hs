-- minimark: AbiWord writer (-t abw).  Emits a .abw file — plain UTF-8
-- AWML XML, no container, no compression (.zabw is the gzipped variant
-- and waits for the T4.1 zlib FFI).  AbiWord 2.4.x runs natively on the
-- ppc box, so this writer plus the future abw reader (T5.6) is a
-- round-trip demo format.  Modeled on Rtf.hs (header, metaBlock, block/
-- inlines/flatText, esc).  Math renders via MathRender's bmp-level
-- renderMath (already Unicode text), escaped like prose.
module MiniMark.Abw(renderAbw) where

import Data.List(intercalate)
import MiniMark.AST
import MiniMark.MathRender(GlyphLevel(..), renderMath)

renderAbw :: Doc -> String
renderAbw (Doc m bs) =
  header ++ metaBlock m ++ concatMap (block 0) bs ++ footer

-- Prolog + AWML root, one <section> wrapping every block.  Newline after
-- each block element keeps goldens diffable (XML ignores it).
header :: String
header = concat
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  , "<abiword xmlns=\"http://www.abisource.com/awml.dtd\""
  , " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
  , " template=\"false\" fileformat=\"1.1\">\n"
  , "<section>\n"
  ]

footer :: String
footer = "</section>\n</abiword>\n"

-- Title block (bold Heading-1-style <p>) + byline (author · date,
-- italic), only when Meta carries something — mirrors Rtf.hs's shape.
metaBlock :: Meta -> String
metaBlock (Meta Nothing Nothing Nothing _ _) = ""
metaBlock (Meta mt ma md _ _) = concat
  [ maybe "" (\t -> para "Heading 1" "" (cspan "font-weight:bold" (esc t))) mt
  , maybe "" (para "Normal" "font-style:italic" . esc) byline
  ]
  where
    byline = case (ma, md) of
      (Nothing, Nothing) -> Nothing
      (Just a, Nothing)  -> Just a
      (Nothing, Just d)  -> Just d
      (Just a, Just d)   -> Just (a ++ " \x00B7 " ++ d)

--------------------------------------------------------------------------
-- Blocks.  The Int is the quote-nesting depth: each Quote level appends
-- another 0.5in of left margin to its paragraphs (AbiWord has no block
-- quote element, so we indent every contained <p>).

block :: Int -> Block -> String
block q b = case b of
  Heading n is -> para (headStyle n) (indentProp q) (inlines is)
  Para is      -> para "Normal" (indentProp q) (inlines is)
  -- No multi-line pre block in AbiWord: one Plain Text paragraph per
  -- source line, content in a Courier run, preserves the line breaks.
  CodeBlock _ lns ->
    concatMap (\l -> para "Plain Text" (indentProp q)
                          (cspan "font-family:Courier New" (esc l))) lns'
    where lns' = if null lns then [""] else lns
  BulletList items -> concatMap (listItem q "\x2022 ") items
  OrderedList start items ->
    concat (zipWith (\n it -> listItem q (show n ++ ". ") it)
                    [start ..] items)
  -- Deeper quotes add 0.5in each; thread the depth down.
  Quote bs' -> concatMap (block (q + 1)) bs'
  HRule -> para "Normal" (indentProp q) (replicate 40 '-')
  Table aligns hdr rows -> table q aligns hdr rows
  DisplayMath raw es ->
    para "Normal" (joinProps ["text-align:center", indentProp q])
         (mathText raw es)

headStyle :: Int -> String
headStyle 1 = "Heading 1"
headStyle 2 = "Heading 2"
headStyle _ = "Heading 3"

-- 0.5in of left margin per quote level, "" at depth 0.
indentProp :: Int -> String
indentProp q
  | q <= 0    = ""
  | otherwise = "margin-left:" ++ showHalf q ++ "in"
  where
    -- q*0.5 as a minimal decimal: even -> whole, odd -> N.5
    showHalf n = if even n then show (n `div` 2)
                           else show (n `div` 2) ++ ".5"

--------------------------------------------------------------------------
-- Lists.  Like the RTF writer we do NOT use AbiWord's <l> list
-- machinery — each item is a Normal paragraph, indented, with a literal
-- marker prefix; checkbox items get an ascii [ ]/[x] prefix (a text
-- prefix, not a glyph tier).

listItem :: Int -> String -> ListItem -> String
listItem q mark (ListItem mb bs) =
  para "Normal" (joinProps ["margin-left:0.35in", indentProp q])
       (esc (mark ++ checkPfx mb) ++ itemBody bs)

checkPfx :: Maybe Bool -> String
checkPfx Nothing = ""
checkPfx (Just checked) = if checked then "[x] " else "[ ] "

-- The item body's inline content.  A single-paragraph item (the common
-- case) contributes just its inlines; richer items flatten their blocks'
-- inline content inline (no nested <p> inside a list paragraph).
itemBody :: [Block] -> String
itemBody bs = case bs of
  [Para is] -> inlines is
  _         -> intercalate " " (map blockInline bs)

-- Inline-only rendering of a block, for list-item continuation content
-- that can't become its own paragraph.
blockInline :: Block -> String
blockInline b = case b of
  Para is        -> inlines is
  Heading _ is   -> inlines is
  CodeBlock _ ls -> cspan "font-family:Courier New" (esc (intercalate " " ls))
  DisplayMath raw es -> mathText raw es
  _              -> ""

--------------------------------------------------------------------------
-- Tables.  Attach-grid model: one <cell> per cell with 0-based grid
-- lines, each holding one <p>; header-row runs are bold.

table :: Int -> [Align] -> [[Inline]] -> [[[Inline]]] -> String
table _ _ hdr rows =
  "<table>\n" ++ concat (zipWith trow [0 ..] (hdr : rows)) ++ "</table>\n"
  where
    trow r cells =
      concat (zipWith (tcell r) [0 ..] cells)
      where
        isHdr = r == (0 :: Int)
        tcell row col is =
          "<cell props=\"top-attach:" ++ show row
          ++ "; bot-attach:" ++ show (row + 1)
          ++ "; left-attach:" ++ show (col :: Int)
          ++ "; right-attach:" ++ show (col + 1) ++ "\">\n"
          ++ para "Normal" ""
                  (if isHdr then cspan "font-weight:bold" (inlines is)
                            else inlines is)
          ++ "</cell>\n"

--------------------------------------------------------------------------
-- Inlines.  Nesting MERGES props threaded down a prop list (like Ansi.hs
-- threads Style); unstyled text goes bare (no <c>).

inlines :: [Inline] -> String
inlines = concatMap (inline [])

-- The prop list accumulates ancestor styling; a Str at the leaf emits one
-- <c props="..."> (or bare, if no props are active).
inline :: [String] -> Inline -> String
inline ps il = case il of
  Str t        -> emit ps (esc t)
  LineBreak    -> "<br/>"
  Emph is      -> concatMap (inline (add "font-style:italic" ps)) is
  Strong is    -> concatMap (inline (add "font-weight:bold" ps)) is
  Strike is    -> concatMap (inline (add "text-decoration:line-through" ps)) is
  CodeSpan t   -> emit (add "font-family:Courier New" ps) (esc t)
  -- Link wraps the (possibly styled) text in <a xlink:href>.
  Link txt url _ ->
    "<a xlink:href=\"" ++ esc url ++ "\">"
    ++ concatMap (inline ps) txt ++ "</a>"
  Image alt url _ ->
    emit ps (esc ("[image: " ++ flatText alt ++ "] (" ++ url ++ ")"))
  MathI raw es -> emit ps (mathText raw es)
  where add p qs = if p `elem` qs then qs else qs ++ [p]

-- Emit already-escaped run text under the active props: bare when empty.
emit :: [String] -> String -> String
emit [] t = t
emit ps t = cspan (joinProps ps) t

--------------------------------------------------------------------------
-- Building blocks

-- A <p style=..> element, optional props= attribute, given inner XML.
para :: String -> String -> String -> String
para sty props inner =
  "<p style=\"" ++ esc sty ++ "\""
  ++ (if null props then "" else " props=\"" ++ esc props ++ "\"")
  ++ ">" ++ inner ++ "</p>\n"

-- A <c props=..> run wrapping already-escaped content.
cspan :: String -> String -> String
cspan props inner = "<c props=\"" ++ esc props ++ "\">" ++ inner ++ "</c>"

-- Join non-empty prop fragments with "; " (AbiWord's CSS-ish separator).
joinProps :: [String] -> String
joinProps = intercalate "; " . filter (not . null)

mathText :: String -> [MExpr] -> String
mathText _ es = esc (renderMath GBmp es)

flatText :: [Inline] -> String
flatText = concatMap f
  where
    f (Str t) = t
    f LineBreak = " "
    f (Emph is) = flatText is
    f (Strong is) = flatText is
    f (Strike is) = flatText is
    f (CodeSpan t) = t
    f (Link t _ _) = flatText t
    f (Image t _ _) = flatText t
    f (MathI raw _) = raw

-- XML escaping, applied wherever text or an attribute value lands.
-- Non-ASCII passes through RAW: the file is UTF-8, do NOT entity-encode.
esc :: String -> String
esc = concatMap e
  where
    e '&' = "&amp;"
    e '<' = "&lt;"
    e '>' = "&gt;"
    e '"' = "&quot;"
    e c   = [c]
