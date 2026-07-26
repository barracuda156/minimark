-- minimark: math AST -> Unicode text, shared by the ANSI and HTML writers.
-- GlyphLevel picks how adventurous the codepoints are:
--   GFull : plane-1 math alphabets, full super/subscript letters
--   GBmp  : BMP only — letterlike fallbacks (R -> \x211D), safe scripts
-- (--glyphs=ascii never reaches here: writers emit the raw TeX instead.)
--
-- Two entry points (T0.3):
--   renderMathSpans : tagged spans, for writers that style parts of the
--     math (dim script fallback, SGR bold/italic at bmp, HTML sup/sub).
--   renderMath      : collapsed String, byte-identical to the pre-span
--     output.  Writers that don't interpret spans use this.
--
-- Span semantics:
--   SpPlain      text is final output.
--   SpScript pos text is the script's rendered content WITHOUT sub/sup
--                char-mapping or the _/^ fallback marker.  Consumers
--                apply mapScript, or scriptFallback when it fails —
--                collapseSpans does exactly that.  Nested structure
--                inside a script is flattened (scripts are leaves).
--   SpStyled st  text already ran the (level, style) char map; the tag
--                lets a writer add SGR styling where that map is the
--                identity (\mathbf / \mathit at GBmp).
--   SpColor nm   text is final output; nm is the \textcolor color name
--                verbatim, for writers that can colorize (ANSI SGR).
--                collapseSpans drops the tag, so non-color writers
--                render the content uncolored.  Like SpStyled, nested
--                structure is flattened (color spans are leaves).
module MiniMark.MathRender(
  GlyphLevel(..), MathSpan(..), ScriptPos(..),
  renderMath, renderMathSpans, collapseSpans,
  mapScript, scriptFallback
) where

import MiniMark.AST
import MiniMark.Symbols

data GlyphLevel = GFull | GBmp
  deriving (Eq)

data ScriptPos = PSub | PSup
  deriving (Eq)

data MathSpan = SpPlain | SpScript ScriptPos | SpStyled MStyle
              | SpColor String
  deriving (Eq)

renderMath :: GlyphLevel -> [MExpr] -> String
renderMath lvl = collapseSpans lvl . renderMathSpans lvl

collapseSpans :: GlyphLevel -> [(MathSpan, String)] -> String
collapseSpans lvl = concatMap col
  where
    col (SpScript pos, t) = case mapScript lvl pos t of
                              Just t' -> t'
                              Nothing -> scriptFallback pos t
    col (_, t) = t

-- Translate script content to Unicode sub/superscript chars, all or
-- nothing (spaces pass through).
mapScript :: GlyphLevel -> ScriptPos -> String -> Maybe String
mapScript lvl pos = mapAll (scriptMapOf lvl pos)

-- The marker form used when mapScript fails: _x, ^(n+1), ...
scriptFallback :: ScriptPos -> String -> String
scriptFallback pos t =
  (case pos of PSub -> "_"; PSup -> "^") ++ parenLong t

renderMathSpans :: GlyphLevel -> [MExpr] -> [(MathSpan, String)]
renderMathSpans lvl = coalesce . concatMap (rm lvl)

-- Merge adjacent same-tag spans.  Script spans are exempt: their
-- content feeds mapScript per span, and merging would couple the
-- all-or-nothing translation of independent scripts.
coalesce :: [(MathSpan, String)] -> [(MathSpan, String)]
coalesce ((t1, a) : (t2, b) : r)
  | t1 == t2 && mergeable t1 = coalesce ((t1, a ++ b) : r)
  where
    mergeable (SpScript _) = False
    mergeable _            = True
coalesce (x : r) = x : coalesce r
coalesce []      = []

rm :: GlyphLevel -> MExpr -> [(MathSpan, String)]
rm _   (MChar c)      = [(SpPlain, [c])]
rm _   (MText t)      = [(SpPlain, t)]
rm lvl (MGroup es)    = renderMathSpans lvl es
rm lvl (MStyle st es) =
  [(SpStyled st, mapStyle lvl st (renderMath lvl es))]
rm lvl (MColor nm es) =
  [(SpColor nm, renderMath lvl es)]
rm lvl (MScript b sub sup) =
     rm lvl b
  ++ maybe [] (script PSub) sub
  ++ maybe [] (script PSup) sup
  where
    script pos es = [(SpScript pos, renderMath lvl es)]
rm lvl (MFrac a b) =
     parenSpans lvl (renderMathSpans lvl a)
  ++ [(SpPlain, "/")]
  ++ parenSpans lvl (renderMathSpans lvl b)
rm lvl (MSqrt a) =
  (SpPlain, "\x221A") : parenSpans lvl (renderMathSpans lvl a)
rm _   (MUnknown cmd) = [(SpPlain, '\\' : cmd)]

-- Parenthesize when the COLLAPSED text is longer than one glyph — the
-- decision must match what renderMath produces, not the raw span text
-- (script fallbacks add markers and parens of their own).
parenSpans :: GlyphLevel -> [(MathSpan, String)] -> [(MathSpan, String)]
parenSpans lvl sps =
  if dlen (collapseSpans lvl sps) <= 1
    then sps
    else (SpPlain, "(") : sps ++ [(SpPlain, ")")]

parenLong :: String -> String
parenLong t = if dlen t <= 1 then t else "(" ++ t ++ ")"

-- length ignoring combining marks
dlen :: String -> Int
dlen = length . filter (not . combining)
  where combining c = (c >= '\x0300' && c <= '\x036F')
                   || (c >= '\x20D0' && c <= '\x20FF')

scriptMapOf :: GlyphLevel -> ScriptPos -> [(Char, Char)]
scriptMapOf GFull PSup = supMapFull
scriptMapOf GBmp  PSup = supMapSafe
scriptMapOf GFull PSub = subMapFull
scriptMapOf GBmp  PSub = subMapSafe

-- translate the whole string or fail (spaces pass through)
mapAll :: [(Char, Char)] -> String -> Maybe String
mapAll mp = go
  where
    go [] = Just []
    go (c:cs) | c == ' ' = fmap (c:) (go cs)
              | otherwise = case lookup c mp of
                              Just c' -> fmap (c':) (go cs)
                              Nothing -> Nothing

mapStyle :: GlyphLevel -> MStyle -> String -> String
mapStyle lvl st = map tr
  where
    mp = styleMap lvl st
    tr c = case lookup c mp of
             Just c' -> c'
             Nothing -> c

styleMap :: GlyphLevel -> MStyle -> [(Char, Char)]
styleMap GFull SBold = styleBold
styleMap GFull SItal = styleItal
styleMap GFull SCal  = styleScript
styleMap GFull SFrak = styleFrak
styleMap GFull SBb   = styleBb
styleMap GBmp  SCal  = styleScriptBmp
styleMap GBmp  SFrak = styleFrakBmp
styleMap GBmp  SBb   = styleBbBmp
styleMap _     _     = []
