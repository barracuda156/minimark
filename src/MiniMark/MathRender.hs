-- minimark: math AST -> Unicode text, shared by the ANSI and HTML writers.
-- GlyphLevel picks how adventurous the codepoints are:
--   GFull : plane-1 math alphabets, full super/subscript letters
--   GBmp  : BMP only — letterlike fallbacks (R -> \x211D), safe scripts
-- (--glyphs=ascii never reaches here: writers emit the raw TeX instead.)
module MiniMark.MathRender(GlyphLevel(..), renderMath) where

import MiniMark.AST
import MiniMark.Symbols

data GlyphLevel = GFull | GBmp
  deriving (Eq)

renderMath :: GlyphLevel -> [MExpr] -> String
renderMath lvl = concatMap (rm lvl)

rm :: GlyphLevel -> MExpr -> String
rm _   (MChar c)     = [c]
rm _   (MText t)     = t
rm lvl (MGroup es)   = renderMath lvl es
rm lvl (MStyle st es) = mapStyle lvl st (renderMath lvl es)
rm lvl (MScript b sub sup) =
     rm lvl b
  ++ maybe "" (scriptTxt (subMapOf lvl) "_") sub
  ++ maybe "" (scriptTxt (supMapOf lvl) "^") sup
  where
    scriptTxt mp pfx es =
      let t = renderMath lvl es
      in case mapAll mp t of
           Just t' -> t'
           Nothing -> pfx ++ parenLong t
rm lvl (MFrac a b) =
  let ta = renderMath lvl a
      tb = renderMath lvl b
  in parenLong ta ++ "/" ++ parenLong tb
rm lvl (MSqrt a) = "\x221A" ++ parenLong (renderMath lvl a)
rm _   (MUnknown cmd) = '\\' : cmd

parenLong :: String -> String
parenLong t = if dlen t <= 1 then t else "(" ++ t ++ ")"

-- length ignoring combining marks
dlen :: String -> Int
dlen = length . filter (not . combining)
  where combining c = (c >= '\x0300' && c <= '\x036F')
                   || (c >= '\x20D0' && c <= '\x20FF')

supMapOf :: GlyphLevel -> [(Char, Char)]
supMapOf GFull = supMapFull
supMapOf GBmp  = supMapSafe

subMapOf :: GlyphLevel -> [(Char, Char)]
subMapOf GFull = subMapFull
subMapOf GBmp  = subMapSafe

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
