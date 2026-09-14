-- minimark: RTF writer (-t rtf).  Math renders via MathRender's
-- bmp-level renderMath (already Unicode text), escaped through the
-- same \uN? path as prose.  Target: opens in TextEdit on 10.5.
module MiniMark.Rtf(renderRtf) where

import Data.List(intercalate)
import MiniMark.AST
import MiniMark.MathRender(GlyphLevel(..), renderMath)

renderRtf :: Doc -> String
renderRtf (Doc m bs) =
  header ++ metaBlock m ++ concatMap block bs ++ "}\n"

-- {\rtf1\ansi\deff0 + fonttbl (f0 Helvetica sans, f1 Courier mono) +
-- colortbl matching the ANSI 16-color palette (Ansi.hs's Color set,
-- same RGB triples as colRgb so `--color=true` term output and RTF
-- agree). \viewkind4 = normal page-layout view (TextEdit default).
header :: String
header = concat
  [ "{\\rtf1\\ansi\\ansicpg1252\\deff0\\viewkind4\n"
  , "{\\fonttbl{\\f0\\fswiss Helvetica;}{\\f1\\fmodern Courier;}}\n"
  , "{\\colortbl;", colortbl, "}\n"
  , "\\uc1\\f0\\fs24\n"
  ]

-- Entries are 1-indexed after the implicit entry 0 ("automatic" color,
-- the leading ";"); order: blue, cyan, green, yellow, magenta, gray,
-- red, black, white. \cf6 (metaBlock's byline) is gray.
colortbl :: String
colortbl = concatMap rgb
  [ (95,175,255), (0,190,190), (95,195,95), (215,175,0), (200,120,245)
  , (150,150,150), (220,95,95), (0,0,0), (235,235,235) ]
  where rgb (r,g,b) = "\\red" ++ show r ++ "\\green" ++ show g
                      ++ "\\blue" ++ show b ++ ";"

-- Title bold \fs48, byline dim-ish (gray color) \fs20, blank \par
-- before body (design note's term/plain shape, adapted to RTF sizes).
metaBlock :: Meta -> String
metaBlock (Meta Nothing Nothing Nothing _ _) = ""
metaBlock (Meta mt ma md _ _) = concat
  [ maybe "" (\t -> "\\fs48\\b " ++ esc t ++ "\\b0\\fs24\\par\n") mt
  , maybe "" (\b -> "\\fs20\\cf6 " ++ esc b ++ "\\cf0\\fs24\\par\n") byline
  , "\\par\n"
  ]
  where
    byline = case (ma, md) of
      (Nothing, Nothing) -> Nothing
      (Just a, Nothing)  -> Just a
      (Nothing, Just d)  -> Just d
      (Just a, Just d)   -> Just (a ++ " \x00B7 " ++ d)

-- Heading sizes: H1/H2/H3+ = \fs48/\fs36/\fs28 (half-points), body
-- \fs24; each heading restores \fs24 after itself so later paragraphs
-- aren't affected (RTF font/size state is not block-scoped without
-- explicit groups, so this writer resets rather than groups).
block :: Block -> String
block b = case b of
  Heading n is ->
    "\\fs" ++ show (headSize n) ++ "\\b " ++ inlines is ++ "\\b0\\fs24\\par\n"
  Para is -> inlines is ++ "\\par\n"
  CodeBlock _ lns ->
    "\\f1 " ++ intercalate "\\line\n" (map esc lns) ++ "\\f0\\par\n"
  BulletList items -> concatMap (item "\\'b7") items
  OrderedList start items ->
    concat (zipWith (\n it -> item (show n ++ ".") it) [start ..] items)
  Quote bs' ->
    "\\li720\n" ++ concatMap block bs' ++ "\\li0\n"
  HRule -> "\\pard\\brdrb\\brdrs\\brdrw10\\brsp20 \\par\\pard\n"
  Table aligns hdr rows -> table aligns hdr rows
  DisplayMath raw es -> "\\qc " ++ mathText raw es ++ "\\par\\ql\n"

headSize :: Int -> Int
headSize 1 = 48
headSize 2 = 36
headSize _ = 28

item :: String -> ListItem -> String
item mark (ListItem mb bs) =
  "\\li360 " ++ mark ++ "\\tab " ++ checkPfx mb ++ itemBody bs ++ "\\li0\n"

checkPfx :: Maybe Bool -> String
checkPfx Nothing = ""
checkPfx (Just checked) = if checked then "[x] " else "[ ] "

itemBody :: [Block] -> String
itemBody bs = case bs of
  [Para is] -> inlines is ++ "\\par\n"
  _         -> concatMap block bs

-- tbl-free RTF table: \trowd/\cellx per column, \intbl cells, \row end.
-- Flat equal column widths over the usable US-Letter width (8.5in −
-- 2×1in margins = 6.5in = 9360 twips; RTF defaults to Letter when no
-- \paperw is given, and \cellx positions are absolute right edges —
-- dividing 1440 here would make the WHOLE table one inch wide).
table :: [Align] -> [[Inline]] -> [[[Inline]]] -> String
table aligns hdr rows =
  concatMap (trow True) [hdr] ++ concatMap (trow False) rows
  where
    ncol = length hdr
    colw = 9360 `div` max 1 ncol
    cellx = concatMap (\i -> "\\cellx" ++ show ((i + 1) * colw)) [0 .. ncol - 1]
    trow bold cells =
      "\\trowd" ++ cellx ++ "\n"
      ++ concat (zipWith (cell bold) (aligns ++ repeat ALeft) cells)
      ++ "\\row\n"
    cell bold al is =
      "\\intbl" ++ alCmd al ++ (if bold then "\\b " else " ")
      ++ inlines is ++ (if bold then "\\b0" else "") ++ "\\cell "
    alCmd ALeft   = "\\ql"
    alCmd ACenter = "\\qc"
    alCmd ARight  = "\\qr"

mathText :: String -> [MExpr] -> String
mathText _ es = esc (renderMath GBmp es)

inlines :: [Inline] -> String
inlines = concatMap f
  where
    f (Str t) = esc t
    f LineBreak = "\\line\n"
    f (Emph is) = "\\i " ++ inlines is ++ "\\i0 "
    f (Strong is) = "\\b " ++ inlines is ++ "\\b0 "
    f (Strike is) = "\\strike " ++ inlines is ++ "\\strike0 "
    f (CodeSpan t) = "\\f1 " ++ esc t ++ "\\f0 "
    f (Link txt url _) = inlines txt ++ " (" ++ esc url ++ ")"
    f (Image alt url _) = "[image: " ++ esc (flatText alt) ++ "] (" ++ esc url ++ ")"
    f (MathI raw es) = mathText raw es

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

-- Escaping: '\', '{', '}' backslash-escaped; non-ASCII -> \uN? where N
-- is the SIGNED 16-bit decimal code point (cp >= 0x8000 -> cp -
-- 0x10000), astral code points (> 0xFFFF) emit a UTF-16 surrogate
-- pair as two \uN? words. \ucN (skip-count for the '?' fallback byte
-- per \u) is set once in the header (\uc1) and never changes.
esc :: String -> String
esc = concatMap e
  where
    e '\\' = "\\\\"
    e '{'  = "\\{"
    e '}'  = "\\}"
    e '\n' = "\\line\n"
    e c
      | n <= 127  = [c]
      | n <= 0xFFFF = u16 n
      | otherwise = let n' = n - 0x10000
                        hi = 0xD800 + (n' `div` 0x400)
                        lo = 0xDC00 + (n' `mod` 0x400)
                    in u16 hi ++ u16 lo
      where n = fromEnum c

u16 :: Int -> String
u16 n = "\\u" ++ show signed ++ "?"
  where signed = if n >= 0x8000 then n - 0x10000 else n
