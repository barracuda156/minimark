-- minimark: ANSI terminal writer.
-- Color modes: MNone (no escapes, doubles as the plain writer),
-- M16 (classic SGR 30-37, safe in Terminal.app on 10.5/10.6),
-- MTrue (24-bit SGR 38;2, for contour / mlterm on ppc).
module MiniMark.Ansi(ColorMode(..), AnsiOpts(..), renderAnsi) where

import MiniMark.CharClass(isSp)
import Data.List(intercalate)
import MiniMark.AST
import MiniMark.MathRender

data ColorMode = MNone | M16 | MTrue
  deriving (Eq)

data AnsiOpts = AnsiOpts
  { aoColor  :: ColorMode
  , aoGlyphs :: GlyphLevel
  , aoWidth  :: Int
  , aoItalic :: Bool        -- SGR 3 for emphasis (else underline)
  , aoAscii  :: Bool        -- math as raw TeX
  }

--------------------------------------------------------------------------
-- Styles

data Color = CBlue | CCyan | CGreen | CYellow | CMagenta | CGray
  deriving (Eq)

data Style = Style
  { sBold, sItal, sUnder, sDim :: Bool
  , sFg :: Maybe Color
  }

plainS :: Style
plainS = Style False False False False Nothing

isPlain :: Style -> Bool
isPlain (Style b i u d f) = not b && not i && not u && not d
                            && (case f of Nothing -> True; _ -> False)

col16 :: Color -> Int
col16 CBlue = 34
col16 CCyan = 36
col16 CGreen = 32
col16 CYellow = 33
col16 CMagenta = 35
col16 CGray = 37

colRgb :: Color -> (Int, Int, Int)
colRgb CBlue    = (95, 175, 255)
colRgb CCyan    = (0, 190, 190)
colRgb CGreen   = (95, 195, 95)
colRgb CYellow  = (215, 175, 0)
colRgb CMagenta = (200, 120, 245)
colRgb CGray    = (150, 150, 150)

sgr :: AnsiOpts -> Style -> String
sgr o st
  | aoColor o == MNone || isPlain st = ""
  | otherwise = "\ESC[" ++ intercalate ";" codes ++ "m"
  where
    codes = concat
      [ ["1" | sBold st]
      , ["2" | sDim st]
      , ["3" | sItal st]
      , ["4" | sUnder st]
      , fg (sFg st)
      ]
    fg Nothing = []
    fg (Just c) = case aoColor o of
      MTrue -> let (r,g,b) = colRgb c
               in ["38", "2", show r, show g, show b]
      _     -> [show (col16 c)]

emit :: AnsiOpts -> Style -> String -> String
emit o st t =
  let open = sgr o st
  in if null open then t else open ++ t ++ "\ESC[0m"

--------------------------------------------------------------------------
-- Character display width (approximate wcwidth)

charW :: Char -> Int
charW c
  | combining = 0
  | wide      = 2
  | otherwise = 1
  where
    combining = (c >= '\x0300' && c <= '\x036F')
             || (c >= '\x1DC0' && c <= '\x1DFF')
             || (c >= '\x20D0' && c <= '\x20FF')
             || (c >= '\xFE20' && c <= '\xFE2F')
    wide = (c >= '\x1100' && c <= '\x115F')
        || (c >= '\x2E80' && c <= '\xA4CF')
        || (c >= '\xAC00' && c <= '\xD7A3')
        || (c >= '\xF900' && c <= '\xFAFF')
        || (c >= '\xFF00' && c <= '\xFF60')
        || (c >= '\xFFE0' && c <= '\xFFE6')
        || (c >= '\x1F300' && c <= '\x1FAFF')
        || c `elem` "\x231A\x231B\x2614\x2615\x267F\x2693\x26A1\x26AA\x26AB\
                    \\x26BD\x26BE\x26C4\x26C5\x26CE\x26D4\x26EA\x26F2\x26F3\
                    \\x26F5\x26FA\x26FD\x2705\x270A\x270B\x2728\x274C\x274E\
                    \\x2753\x2754\x2755\x2757\x2795\x2796\x2797\x27B0\x27BF\
                    \\x2B1B\x2B1C\x2B50\x2B55"

strW :: String -> Int
strW = sum . map charW

--------------------------------------------------------------------------
-- Styled spans

type Span = (Style, String)

spanW :: [Span] -> Int
spanW = sum . map (strW . snd)

spanText :: [Span] -> String
spanText = concatMap snd

renderSpans :: AnsiOpts -> [Span] -> String
renderSpans o = concatMap (\(st, t) -> emit o st t)

inlineSpans :: AnsiOpts -> Style -> [Inline] -> [Span]
inlineSpans o st = concatMap f
  where
    f (Str t) = [(st, t)]
    f (Emph is)
      | aoItalic o = inlineSpans o st{sItal = True} is
      | otherwise  = inlineSpans o st{sUnder = True} is
    f (Strong is) = inlineSpans o st{sBold = True} is
    f (CodeSpan t) = [(st{sFg = Just CCyan}, t)]
    f (Link txt url)
      | flatText txt == url = [(linkS, url)]
      | otherwise = inlineSpans o linkS txt
                    ++ [(st{sDim = True}, " (" ++ url ++ ")")]
      where linkS = st{sUnder = True, sFg = Just CBlue}
    f (MathI raw es) = [(st, mathText o raw es)]

mathText :: AnsiOpts -> String -> [MExpr] -> String
mathText o raw es
  | aoAscii o = "$" ++ raw ++ "$"
  | otherwise = renderMath (aoGlyphs o) es

flatText :: [Inline] -> String
flatText = concatMap f
  where
    f (Str t) = t
    f (Emph is) = flatText is
    f (Strong is) = flatText is
    f (CodeSpan t) = t
    f (Link t _) = flatText t
    f (MathI raw _) = raw

--------------------------------------------------------------------------
-- Word wrapping of styled spans

wrapSpans :: Int -> [Span] -> [[Span]]
wrapSpans w spans =
  let ws = toWords spans
  in if null ws then [[]] else fill ws
  where
    fill [] = []
    fill (x:xs) = go x (spanW x) xs
      where
        go cur _ [] = [cur]
        go cur cw (y:ys)
          | cw + 1 + spanW y <= w = go (cur ++ [(plainS, " ")] ++ y) (cw + 1 + spanW y) ys
          | otherwise = cur : fill (y:ys)

-- split spans into space-free words
toWords :: [Span] -> [[Span]]
toWords = filter (not . null) . go []
  where
    go cur [] = [reverse cur]
    go cur ((st, t):rest) = splitT cur st t rest
    splitT cur st t rest =
      case break isSp t of
        (word, [])       -> go (addFrag st word cur) rest
        (word, _:t')     -> reverse (addFrag st word cur)
                            : splitT [] st (dropWhile isSp t') rest
    addFrag _  "" cur = cur
    addFrag st w  cur = (st, w) : cur

--------------------------------------------------------------------------
-- Blocks

-- Meta is ignored until T1.4 adds the title-block rendering.
renderAnsi :: AnsiOpts -> Doc -> String
renderAnsi o (Doc _ bs) =
  unlines (intercalate [""] (filter (not . null) (map (blockLines o 0) bs)))

indentLines :: Int -> [String] -> [String]
indentLines n = map (\l -> if null l then l else replicate n ' ' ++ l)

blockLines :: AnsiOpts -> Int -> Block -> [String]
blockLines o depth b = case b of

  Heading n is ->
    let st = headingStyle n
        spans = inlineSpans o st is
        txt = renderSpans o spans
        w = spanW spans
    in if n <= 2
         then [txt, emit o st (replicate (min w (aoWidth o))
                                 (if n == 1 then '\x2550' else '\x2500'))]
         else [txt]

  Para is ->
    map (renderSpans o) (wrapSpans (aoWidth o) (inlineSpans o plainS is))

  CodeBlock lang lns ->
    let cw = maximum (1 : map strW lns)
        top = "\x250C\x2500" ++ langLbl
              ++ replicate (cw + 1 - strW langLbl) '\x2500' ++ "\x2510"
        langLbl = if null lang then "" else " " ++ lang ++ " "
        bot = "\x2514" ++ replicate (cw + 2) '\x2500' ++ "\x2518"
        frame t = emit o frameS t
        frameS = plainS{sDim = True}
        row l = frame "\x2502 " ++ l ++ replicate (cw - strW l) ' '
                ++ frame " \x2502"
    in [frame' top] ++ map row lns ++ [frame' bot]
    where frame' t = emit o plainS{sDim = True} t

  Quote bs ->
    let inner = intercalate [""] (map (blockLines o' depth) bs)
        o' = o{aoWidth = aoWidth o - 2}
        bar = emit o plainS{sFg = Just CGreen} "\x2502 "
    in map (bar ++) inner

  BulletList items ->
    let bullet = [bulletChar depth]
        o' = o{aoWidth = aoWidth o - 2}
    in concatMap (item o' (emit o plainS{sFg = Just CYellow} bullet ++ " ") 2 depth) items

  OrderedList start items ->
    let nums = map show [start .. start + length items - 1]
        nw = maximum (map length nums)
        o' = o{aoWidth = aoWidth o - (nw + 2)}
        mk n = let lbl = replicate (nw - length n) ' ' ++ n ++ "."
               in emit o plainS{sFg = Just CYellow} lbl ++ " "
    in concat (zipWith (\n it -> item o' (mk n) (nw + 2) depth it) nums items)

  HRule ->
    [emit o plainS{sDim = True} (replicate (aoWidth o) '\x2500')]

  Table aligns hdr rows ->
    tableLines o aligns hdr rows

  DisplayMath raw es ->
    let t = if aoAscii o then raw else renderMath (aoGlyphs o) es
    in indentLines 4 [t]

-- one list item: label on first line, hanging indent after.
-- Tight rendering: no blank separators inside an item.
item :: AnsiOpts -> String -> Int -> Int -> ListItem -> [String]
item o lbl ind depth (ListItem _ bs) =
  case concatMap (blockLines o (depth + 1)) bs of
    []     -> [lbl]
    (l:ls) -> (lbl ++ l) : indentLines ind ls

bulletChar :: Int -> Char
bulletChar d = case d `mod` 3 of
  0 -> '\x2022'   -- bullet
  1 -> '\x25E6'   -- white bullet
  _ -> '\x25AA'   -- small square

headingStyle :: Int -> Style
headingStyle 1 = plainS{sBold = True, sFg = Just CMagenta}
headingStyle 2 = plainS{sBold = True, sFg = Just CBlue}
headingStyle 3 = plainS{sBold = True, sFg = Just CCyan}
headingStyle _ = plainS{sBold = True, sDim = True}

--------------------------------------------------------------------------
-- Tables

tableLines :: AnsiOpts -> [Align] -> [[Inline]] -> [[[Inline]]] -> [String]
tableLines o aligns hdr rows =
  let hs = map (inlineSpans o plainS{sBold = True}) hdr
      rs = map (map (inlineSpans o plainS)) rows
      ncol = length hdr
      widths = map colWidth [0 .. ncol - 1]
      colWidth i = maximum (1 : map (cellW i) (hs : rs))
      cellW i cells = if i < length cells then spanW (cells !! i) else 0
      border l m r =
        emit o plainS{sDim = True}
          (l ++ intercalate m (map (\w -> replicate (w + 2) '\x2500') widths) ++ r)
      row cells =
        let cell i =
              let sp = if i < length cells then cells !! i else []
                  w = widths !! i
                  pad = w - spanW sp
                  al = if i < length aligns then aligns !! i else ALeft
                  (lp, rp) = padFor al pad
              in " " ++ replicate lp ' ' ++ renderSpans o sp
                 ++ replicate rp ' ' ++ " "
            v = emit o plainS{sDim = True} "\x2502"
        in v ++ intercalate v (map cell [0 .. ncol - 1]) ++ v
  in [ border "\x250C" "\x252C" "\x2510"
     , row hs
     , border "\x251C" "\x253C" "\x2524" ]
     ++ map row rs
     ++ [ border "\x2514" "\x2534" "\x2518" ]

padFor :: Align -> Int -> (Int, Int)
padFor ALeft   p = (0, p)
padFor ARight  p = (p, 0)
padFor ACenter p = (p `div` 2, p - p `div` 2)
