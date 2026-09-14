-- minimark: man/roff writer (-t man).  Math arrives already
-- Unicode-rendered (bmp level, MathRender's renderMath) and is escaped
-- through the same path as prose.
module MiniMark.Man(ManOpts(..), renderMan) where

import Data.List(intercalate)
import MiniMark.AST
import MiniMark.MathRender(GlyphLevel(..), renderMath)

-- Name and section are resolved by the caller (Main.hs's manOpts:
-- --title flag > title block > input filename) so precedence lives in
-- one place.  Date and manual header come straight off the Meta the
-- title block produced.
data ManOpts = ManOpts
  { mnName    :: String
  , mnSection :: String
  }

renderMan :: ManOpts -> Doc -> String
renderMan o (Doc m bs) =
  th o m ++ concatMap block bs

-- .TH NAME SECTION DATE FOOTER HEADER -- pandoc's argument order, and
-- what groff's man macros expect: FOOTER is the left-hand page footer
-- (we have nothing for it) and HEADER the centred manual title, e.g.
--   .TH "NGS" "1" "2015" "" "NGS User Manual"
-- Empty trailing arguments are dropped rather than emitted as "" runs.
-- The name is upper-cased per man(7) convention.
th :: ManOpts -> Meta -> String
th o m = ".TH " ++ unwords (map thArg (dropTrailing args)) ++ "\n"
  where
    args = [ upperAscii (mnName o)
           , mnSection o
           , maybe "" id (mDate m)
           , ""
           , maybe "" id (mManual m)
           ]
    dropTrailing = reverse . dropWhile null . reverse
    thArg t = "\"" ++ concatMap thEsc (esc t) ++ "\""
    thEsc '"' = "\\(dq"
    thEsc c   = [c]
    upperAscii = map upC
    upC c | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
          | otherwise            = c

block :: Block -> String
block b = case b of
  Heading n is
    | n == 1    -> ".SH " ++ quoted (inlines is) ++ "\n"
    | n == 2    -> ".SS " ++ quoted (inlines is) ++ "\n"
    | otherwise -> ".PP\n\\fB" ++ inlines is ++ "\\fR\n"
  Para is -> ".PP\n" ++ breakLines (inlines is) ++ "\n"
  CodeBlock _ lns ->
    ".\\\" groff 1.22+: .EX/.EE; falls back to .nf/.fi on older groff\n"
    ++ ".EX\n" ++ intercalate "\n" (map (textLines . escCode) lns) ++ "\n.EE\n"
  BulletList items -> concatMap bulletItem items
  OrderedList start items ->
    concat (zipWith orderedItem [start ..] items)
  Quote bs' ->
    ".RS\n" ++ concatMap block bs' ++ ".RE\n"
  HRule -> ".PP\n\\l'\\n(.lu'\n"
  Table aligns hdr rows -> table aligns hdr rows
  DisplayMath raw es -> ".PP\n" ++ textLines (mathText raw es) ++ "\n"

bulletItem :: ListItem -> String
bulletItem (ListItem mb bs) =
  ".IP \\(bu 2\n" ++ checkPfx mb ++ itemBody bs

orderedItem :: Int -> ListItem -> String
orderedItem n (ListItem mb bs) =
  ".IP " ++ show n ++ ". 4\n" ++ checkPfx mb ++ itemBody bs

checkPfx :: Maybe Bool -> String
checkPfx Nothing = ""
checkPfx (Just checked) = (if checked then "[x] " else "[ ] ")

itemBody :: [Block] -> String
itemBody bs = case bs of
  [Para is] -> textLines (inlines is) ++ "\n"
  _         -> concatMap block bs

table :: [Align] -> [[Inline]] -> [[[Inline]]] -> String
table aligns hdr rows =
  ".TS\n" ++ "allbox;\n" ++ colspec ++ ".\n"
  ++ trow hdr ++ concatMap trow rows
  ++ ".TE\n"
  where
    ncol = length hdr
    colspec = unwords (take ncol (map alSpec (aligns ++ repeat ALeft))) ++ "\n"
    alSpec ALeft   = "l"
    alSpec ACenter = "c"
    alSpec ARight  = "r"
    trow cells = intercalate "\t" (map inlines cells) ++ "\n"

-- Text lines: roff sees blank lines as vertical space, so a paragraph
-- with internal blank content never happens (block-level newlines are
-- already stripped by the parser); this just applies the leading-dot
-- escape per physical line.
textLines :: String -> String
textLines = intercalate "\n" . map leadEsc . splitLines

splitLines :: String -> [String]
splitLines s = case break (== '\n') s of
  (l, [])     -> [l]
  (l, _:rest) -> l : splitLines rest

-- Paragraph text carrying hard LineBreaks (rendered as '\n' by inlines):
-- each break becomes an explicit .br request so roff's fill mode does not
-- reflow across it.  An empty segment (a blank source line between
-- sections) becomes a .br on an otherwise empty output line, which roff
-- renders as a blank line.  Break-free paragraphs collapse to textLines.
breakLines :: String -> String
breakLines s = intercalate "\n" (interBr (map leadEsc (splitLines s)))
  where
    interBr []     = []
    interBr (l:ls) = l : concatMap (\x -> [".br", x]) ls

-- Rule 2: a line that would start with '.' or '\'' gets \& prefixed
-- (the zero-width character escape) so roff doesn't read it as a macro.
leadEsc :: String -> String
leadEsc l@(c:_) | c == '.' || c == '\'' = "\\&" ++ l
leadEsc l = l

mathText :: String -> [MExpr] -> String
mathText _ es = esc (renderMath GBmp es)

inlines :: [Inline] -> String
inlines = concatMap f
  where
    f (Str t) = esc t
    f LineBreak = "\n"   -- physical newline; breakLines turns it into .br
    f (Emph is) = "\\fI" ++ inlines is ++ "\\fR"
    f (Strong is) = "\\fB" ++ inlines is ++ "\\fR"
    f (Strike is) = inlines is
    f (CodeSpan t) = "\\f(CW" ++ escCode t ++ "\\fR"
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

quoted :: String -> String
quoted t = "\"" ++ concatMap qc t ++ "\""
  where
    qc '"' = "\\(dq"
    qc c   = [c]

-- Escaping order (rule 1 first, always): backslash before anything
-- that would introduce one, then non-ASCII -> \[uXXXX].  Leading-dot
-- escaping is line-level (textLines/leadEsc), not per-char.
esc :: String -> String
esc = concatMap e
  where
    e '\\' = "\\[rs]"
    e c | fromEnum c > 127 = uEsc c
        | otherwise        = [c]

-- CodeSpan/code blocks only: '-' -> \- (exact ASCII minus, for
-- copy-paste) in addition to the usual escaping; prose keeps plain '-'.
escCode :: String -> String
escCode = concatMap e
  where
    e '\\' = "\\[rs]"
    e '-'  = "\\-"
    e c | fromEnum c > 127 = uEsc c
        | otherwise        = [c]

-- groff_char(7) form: \[uXXXX], uppercase hex, 4+ digits.
uEsc :: Char -> String
uEsc c = "\\[u" ++ pad (hex (fromEnum c)) ++ "]"
  where
    pad h = replicate (max 0 (4 - length h)) '0' ++ h
    hex 0 = "0"
    hex n = go n ""
    go 0 acc = acc
    go n acc = go (n `div` 16) (hexDigit (n `mod` 16) : acc)
    hexDigit d
      | d < 10    = toEnum (fromEnum '0' + d)
      | otherwise = toEnum (fromEnum 'A' + d - 10)
