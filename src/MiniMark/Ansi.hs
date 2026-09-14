-- minimark: ANSI terminal writer.
-- Color modes: MNone (no escapes, doubles as the plain writer),
-- M16 (classic SGR 30-37, safe in Terminal.app on 10.5/10.6),
-- MTrue (24-bit SGR 38;2, for contour / mlterm on ppc).
module MiniMark.Ansi(ColorMode(..), LinkMode(..), AnsiOpts(..), renderAnsi) where

import MiniMark.CharClass(isSp)
import Data.List(intercalate)
import MiniMark.AST
import MiniMark.MathRender

data ColorMode = MNone | M16 | MTrue
  deriving (Eq)

-- OSC 8 hyperlinks (T2.3): Off is byte-identical to pre-T2.3 output;
-- Osc8 wraps link text and suppresses the dim "(url)" suffix.
data LinkMode = LinksOff | LinksOsc8
  deriving (Eq)

data AnsiOpts = AnsiOpts
  { aoColor  :: ColorMode
  , aoGlyphs :: GlyphLevel
  , aoWidth  :: Int
  , aoItalic :: Bool        -- SGR 3 for emphasis (else underline)
  , aoAscii  :: Bool        -- math as raw TeX
  , aoLinks  :: LinkMode
  }

--------------------------------------------------------------------------
-- Styles

data Color = CBlue | CCyan | CGreen | CYellow | CMagenta | CGray
           | CRed | CBlack | CWhite
  deriving (Eq)

data Style = Style
  { sBold, sItal, sUnder, sDim, sStrike :: Bool
  , sFg :: Maybe Color
  , sLink :: Maybe String   -- OSC 8 target URL (T2.3)
  }

plainS :: Style
plainS = Style False False False False False Nothing Nothing

isPlain :: Style -> Bool
isPlain (Style b i u d k f l) = not b && not i && not u && not d && not k
                              && (case f of Nothing -> True; _ -> False)
                              && (case l of Nothing -> True; _ -> False)

col16 :: Color -> Int
col16 CBlue = 34
col16 CCyan = 36
col16 CGreen = 32
col16 CYellow = 33
col16 CMagenta = 35
col16 CGray = 37
col16 CRed = 31
col16 CBlack = 30
col16 CWhite = 37

colRgb :: Color -> (Int, Int, Int)
colRgb CBlue    = (95, 175, 255)
colRgb CCyan    = (0, 190, 190)
colRgb CGreen   = (95, 195, 95)
colRgb CYellow  = (215, 175, 0)
colRgb CMagenta = (200, 120, 245)
colRgb CGray    = (150, 150, 150)
colRgb CRed     = (220, 95, 95)
colRgb CBlack   = (0, 0, 0)
colRgb CWhite   = (235, 235, 235)

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
      , ["9" | sStrike st]
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
      styled = if null open then t else open ++ t ++ "\ESC[0m"
  in case sLink st of
       Just url -> osc8Open url ++ styled ++ osc8Close
       Nothing  -> styled

-- OSC 8 ; ; URL ST ... OSC 8 ; ; ST — ST (String Terminator, ESC \), not
-- BEL: BEL-terminated OSC 8 confuses some terminals' other OSC handlers.
osc8Open :: String -> String
osc8Open url = "\ESC]8;;" ++ url ++ "\ESC\\"

osc8Close :: String
osc8Close = "\ESC]8;;\ESC\\"

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

-- Split an inline run at every hard LineBreak into segments (the breaks
-- themselves dropped).  n breaks -> n+1 segments; a leading/trailing or
-- doubled break yields an empty segment, which the Para renderer turns
-- into a blank line.  Breaks nested inside Emph/Strong are left alone
-- (rare; they render as a space via inlineSpans' LineBreak case).
splitBreaks :: [Inline] -> [[Inline]]
splitBreaks = foldr step [[]]
  where
    step LineBreak acc      = [] : acc
    step x (seg:rest)       = (x:seg) : rest
    step x []               = [[x]]     -- unreachable (seed is [[]])

inlineSpans :: AnsiOpts -> Style -> [Inline] -> [Span]
inlineSpans o st = concatMap f
  where
    f (Str t) = [(st, t)]
    f LineBreak = [(st, " ")]   -- only reached inside a styled wrapper
    f (Emph is)
      | aoItalic o = inlineSpans o st{sItal = True} is
      | otherwise  = inlineSpans o st{sUnder = True} is
    f (Strong is) = inlineSpans o st{sBold = True} is
    f (Strike is)
      | aoColor o == MNone = [(st, "~~")] ++ inlineSpans o st is ++ [(st, "~~")]
      | aoItalic o = inlineSpans o st{sStrike = True} is
      | otherwise  = inlineSpans o st{sDim = True} is
    f (CodeSpan t) = [(st{sFg = Just CCyan}, t)]
    f (Link txt url _)
      | aoLinks o == LinksOsc8 = inlineSpans o linkS{sLink = Just url} txt
      | flatText txt == url = [(linkS, url)]
      | otherwise = inlineSpans o linkS txt
                    ++ [(st{sDim = True}, " (" ++ url ++ ")")]
      where linkS = st{sUnder = True, sFg = Just CBlue}
    f (Image alt url _) =
      [(st{sDim = True}, "[image: " ++ flatText alt ++ "]")]
      ++ [(st{sDim = True}, " (" ++ url ++ ")")]
    f (MathI raw es) = mathSpans o st raw es

-- Math as styled spans (T1.5): SpColor tags from \textcolor become an
-- SGR foreground; every other span collapses exactly as renderMath
-- would, so output without color escapes is byte-identical to the
-- collapsed form.  Unknown color names render uncolored.
mathSpans :: AnsiOpts -> Style -> String -> [MExpr] -> [Span]
mathSpans o st raw es
  | aoAscii o = [(st, "$" ++ raw ++ "$")]
  | otherwise = map conv (renderMathSpans lvl es)
  where
    lvl = aoGlyphs o
    conv (SpColor nm, t) = case texColor nm of
                             Just c  -> (st{sFg = Just c}, t)
                             Nothing -> (st, t)
    conv (sp, t)         = (st, collapseSpans lvl [(sp, t)])

-- The 8 LaTeX base color names (the \textcolor set minimark maps).
texColor :: String -> Maybe Color
texColor nm = lookup nm
  [ ("red", CRed), ("green", CGreen), ("blue", CBlue), ("cyan", CCyan)
  , ("magenta", CMagenta), ("yellow", CYellow)
  , ("black", CBlack), ("white", CWhite) ]

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

renderAnsi :: AnsiOpts -> Doc -> String
renderAnsi o (Doc m bs) =
  unlines (intercalate [""] (filter (not . null) (metaLines o m : map (blockLines o 0) bs)))

-- Title block (term/plain, design note §1): if any field set, title
-- bold (H1 heading style, no underline bar), byline "author · date"
-- dim (only fields present), blank line, then body.
metaLines :: AnsiOpts -> Meta -> [String]
metaLines o (Meta Nothing Nothing Nothing _ _) = []
metaLines o (Meta mt ma md _ _) =
  [emit o (headingStyle 1) t | Just t <- [mt]]
  ++ [emit o plainS{sDim = True} b | Just b <- [byline]]
  where
    byline = case (ma, md) of
      (Nothing, Nothing) -> Nothing
      (Just a, Nothing)  -> Just a
      (Nothing, Just d)  -> Just d
      (Just a, Just d)   -> Just (a ++ " \x00B7 " ++ d)

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
    -- A paragraph may carry hard line breaks (LineBreak, from the RTF
    -- reader's \line / escaped-newline).  Each break splits the run into a
    -- segment that wraps independently; consecutive breaks (blank source
    -- lines) leave empty segments -> blank output lines.  A break-free
    -- paragraph is one segment and reflows exactly as before.
    concatMap (\seg -> case wrapSpans (aoWidth o) (inlineSpans o plainS seg) of
                         [[]] -> [""]
                         wls  -> map (renderSpans o) wls)
              (splitBreaks is)

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
        lbl it = emit o plainS{sFg = Just CYellow} bullet ++ " " ++ checkPfx o it
        -- wrap width shrinks by the full label incl. checkbox, per item
        oFor it = o{aoWidth = aoWidth o - (2 + checkW o it)}
    in concatMap (\it -> item (oFor it) (lbl it) (2 + checkW o it) depth it) items

  OrderedList start items ->
    let nums = map show [start .. start + length items - 1]
        nw = maximum (map length nums)
        mk n it = let lbl = replicate (nw - length n) ' ' ++ n ++ "."
                  in emit o plainS{sFg = Just CYellow} lbl ++ " " ++ checkPfx o it
        oFor it = o{aoWidth = aoWidth o - (nw + 2 + checkW o it)}
    in concat (zipWith (\n it -> item (oFor it) (mk n it) (nw + 2 + checkW o it) depth it) nums items)

  HRule ->
    [emit o plainS{sDim = True} (replicate (aoWidth o) '\x2500')]

  Table aligns hdr rows ->
    tableLines o aligns hdr rows

  DisplayMath raw es ->
    let t = if aoAscii o then raw
            else renderSpans o (mathSpans o plainS raw es)
    in indentLines 4 [t]

-- one list item: label on first line, hanging indent after.
-- Tight rendering: no blank separators inside an item.
item :: AnsiOpts -> String -> Int -> Int -> ListItem -> [String]
item o lbl ind depth (ListItem _ bs) =
  case concatMap (blockLines o (depth + 1)) bs of
    []     -> [lbl]
    (l:ls) -> (lbl ++ l) : indentLines ind ls

-- Checkbox prefix for a list item (task lists, T1.1): "" when no
-- checkbox. ☐/☑ (Unicode 1.1, safe at every glyph tier) unless
-- --glyphs=ascii, which falls back to [ ]/[x].
checkPfx :: AnsiOpts -> ListItem -> String
checkPfx o (ListItem mb _) = case mb of
  Nothing -> ""
  Just checked
    | aoAscii o -> (if checked then "[x]" else "[ ]") ++ " "
    | otherwise -> emit o plainS{sFg = Just CYellow}
                     [if checked then '\x2611' else '\x2610'] ++ " "

checkW :: AnsiOpts -> ListItem -> Int
checkW o (ListItem mb _) = case mb of
  Nothing -> 0
  Just _ | aoAscii o -> 4
         | otherwise -> 2

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
