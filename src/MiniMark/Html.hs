-- minimark: HTML writer.  Math becomes Unicode text in styled spans —
-- no MathML, no JS — so output renders in period browsers
-- (Safari 4/5 on 10.5/10.6, TenFourFox) as well as modern ones.
module MiniMark.Html(HtmlOpts(..), renderHtml) where

import Data.List(intercalate)
import MiniMark.AST
import MiniMark.MathRender

data HtmlOpts = HtmlOpts
  { hoStandalone :: Bool
  , hoGlyphs     :: GlyphLevel
  , hoAscii      :: Bool
  , hoTitle      :: String
  }

-- Meta is ignored until T1.4 adds the header-block rendering.
renderHtml :: HtmlOpts -> Doc -> String
renderHtml o (Doc _ bs) =
  let body = concatMap (block o) bs
  in if hoStandalone o
       then htmlHeader (hoTitle o) ++ body ++ "</body>\n</html>\n"
       else body

htmlHeader :: String -> String
htmlHeader title = concat
  [ "<!DOCTYPE html>\n<html>\n<head>\n"
  , "<meta charset=\"utf-8\">\n"
  , "<title>", esc title, "</title>\n"
  , "<style>\n"
  , "body { font-family: Georgia, serif; max-width: 42em;\n"
  , "       margin: 2em auto; padding: 0 1em; line-height: 1.5; }\n"
  , "code { font-family: Menlo, Monaco, monospace; font-size: 0.92em;\n"
  , "       background: #f2f2f2; padding: 0 0.15em; }\n"
  , "pre { background: #f6f6f6; border: 1px solid #ddd; padding: 0.8em;\n"
  , "      overflow-x: auto; }\n"
  , "pre code { background: none; padding: 0; }\n"
  , "blockquote { border-left: 3px solid #9c9; margin-left: 0;\n"
  , "             padding-left: 1em; color: #444; }\n"
  , "table { border-collapse: collapse; }\n"
  , "th, td { border: 1px solid #bbb; padding: 0.25em 0.6em; }\n"
  , "th { background: #eee; }\n"
  , ".math { white-space: nowrap; }\n"
  , ".math.display { display: block; text-align: center; margin: 1em 0;\n"
  , "                white-space: normal; }\n"
  , "hr { border: none; border-top: 1px solid #bbb; }\n"
  , "</style>\n</head>\n<body>\n"
  ]

esc :: String -> String
esc = concatMap e
  where
    e '&' = "&amp;"
    e '<' = "&lt;"
    e '>' = "&gt;"
    e '"' = "&quot;"
    e c   = [c]

block :: HtmlOpts -> Block -> String
block o b = case b of
  Heading n is ->
    let t = show n
    in "<h" ++ t ++ ">" ++ inlines o is ++ "</h" ++ t ++ ">\n"
  Para is -> "<p>" ++ inlines o is ++ "</p>\n"
  CodeBlock lang lns ->
    let cls = if null lang then "" else " class=\"language-" ++ esc lang ++ "\""
    in "<pre><code" ++ cls ++ ">"
       ++ esc (unlines lns) ++ "</code></pre>\n"
  BulletList items ->
    "<ul>\n" ++ concatMap (li o) items ++ "</ul>\n"
  OrderedList start items ->
    let st = if start == 1 then "" else " start=\"" ++ show start ++ "\""
    in "<ol" ++ st ++ ">\n" ++ concatMap (li o) items ++ "</ol>\n"
  Quote bs' -> "<blockquote>\n" ++ concatMap (block o) bs' ++ "</blockquote>\n"
  HRule -> "<hr>\n"
  Table aligns hdr rows -> table o aligns hdr rows
  DisplayMath raw es ->
    "<div class=\"math display\">" ++ esc (mathText o raw es) ++ "</div>\n"

li :: HtmlOpts -> ListItem -> String
li o (ListItem _ bs) = "<li>" ++ tight ++ "</li>\n"
  where
    -- single-paragraph items render without <p> wrapper
    tight = case bs of
      [Para is] -> inlines o is
      _         -> "\n" ++ concatMap (block o) bs

table :: HtmlOpts -> [Align] -> [[Inline]] -> [[[Inline]]] -> String
table o aligns hdr rows = concat
  [ "<table>\n<thead>\n<tr>"
  , concat (zipWith (cell "th") aligns hdr)
  , "</tr>\n</thead>\n<tbody>\n"
  , concatMap trow rows
  , "</tbody>\n</table>\n"
  ]
  where
    trow cells = "<tr>" ++ concat (zipWith (cell "td") (aligns ++ repeat ALeft) cells) ++ "</tr>\n"
    cell tag al is =
      let alattr = case al of
            ALeft   -> ""
            ACenter -> " style=\"text-align:center\""
            ARight  -> " style=\"text-align:right\""
      in "<" ++ tag ++ alattr ++ ">" ++ inlines o is ++ "</" ++ tag ++ ">"

inlines :: HtmlOpts -> [Inline] -> String
inlines o = concatMap f
  where
    f (Str t) = esc t
    f (Emph is) = "<em>" ++ inlines o is ++ "</em>"
    f (Strong is) = "<strong>" ++ inlines o is ++ "</strong>"
    f (CodeSpan t) = "<code>" ++ esc t ++ "</code>"
    f (Link txt url) =
      "<a href=\"" ++ esc url ++ "\">" ++ inlines o txt ++ "</a>"
    f (MathI raw es) =
      "<span class=\"math\">" ++ esc (mathText o raw es) ++ "</span>"

mathText :: HtmlOpts -> String -> [MExpr] -> String
mathText o raw es
  | hoAscii o = "$" ++ raw ++ "$"
  | otherwise = renderMath (hoGlyphs o) es
