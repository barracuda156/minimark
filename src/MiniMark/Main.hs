-- minimark — terminal Markdown viewer / mini-pandoc with math support.
-- MicroHs implementation for macOS PowerPC (10.5/10.6) and anywhere else
-- a C compiler exists.
module MiniMark.Main(main) where

import Data.List(isPrefixOf)
import System.Environment(getArgs, lookupEnv)
import System.Exit(exitSuccess, exitWith, ExitCode(ExitFailure))
import System.IO

import MiniMark.AST
import MiniMark.Reader(parseDoc)
import MiniMark.MathRender(GlyphLevel(..))
import MiniMark.Ansi(ColorMode(..), AnsiOpts(..), renderAnsi)
import MiniMark.Html(HtmlOpts(..), renderHtml)
import MiniMark.Latex(LatexOpts(..), renderLatex)

version :: String
version = "minimark 0.1.0"

usage :: String
usage = unlines
  [ version ++ " — terminal Markdown viewer / converter with TeX math"
  , ""
  , "usage: minimark [OPTIONS] [FILE ...]        (stdin when no FILE)"
  , ""
  , "  -t FORMAT        term (default) | html | latex | plain"
  , "  -o FILE          write output to FILE"
  , "  -s, --standalone full document (html/latex)"
  , "  --color=MODE     auto (default) | none | 16 | true"
  , "  --glyphs=LEVEL   bmp (default) | full | ascii"
  , "                   full: plane-1 math alphabets (needs good fonts)"
  , "                   bmp:  BMP-only, safe on 10.5/10.6 system fonts"
  , "                   ascii: math shown as raw TeX"
  , "  --ascii          shorthand for --glyphs=ascii"
  , "  --width=N        wrap width (default: $COLUMNS or 80)"
  , "  --italics=B      on | off (default: on for --color=true, else off)"
  , "  --title=T        document title (html -s)"
  , "  -h, --help       this text"
  , "  --version        version"
  ]

data Opts = Opts
  { oFmt        :: String
  , oOut        :: Maybe String
  , oColor      :: String
  , oGlyphs     :: String
  , oWidth      :: Maybe Int
  , oItalics    :: Maybe Bool
  , oStandalone :: Bool
  , oTitle      :: String
  , oFiles      :: [String]
  }

defOpts :: Opts
defOpts = Opts "term" Nothing "auto" "bmp" Nothing Nothing False "" []

main :: IO ()
main = do
  args <- getArgs
  opts <- parseArgs defOpts args
  txt <- case oFiles opts of
           [] -> getContents
           fs -> fmap concat (mapM readF fs)
  let doc = parseDoc txt
  out <- render opts doc
  case oOut opts of
    Nothing -> putStr out
    Just f  -> writeFile f out
  where
    readF "-" = getContents
    readF f   = readFile f

parseArgs :: Opts -> [String] -> IO Opts
parseArgs o [] = return o{oFiles = reverse (oFiles o)}
parseArgs o (a:as) = case a of
  "-h"           -> putStr usage >> exitSuccess
  "--help"       -> putStr usage >> exitSuccess
  "--version"    -> putStrLn version >> exitSuccess
  "-t"           -> withVal as (\v r -> parseArgs o{oFmt = v} r)
  "-o"           -> withVal as (\v r -> parseArgs o{oOut = Just v} r)
  "-s"           -> parseArgs o{oStandalone = True} as
  "--standalone" -> parseArgs o{oStandalone = True} as
  "--ascii"      -> parseArgs o{oGlyphs = "ascii"} as
  _ | Just v <- eqOpt "--color" a   -> parseArgs o{oColor = v} as
    | Just v <- eqOpt "--glyphs" a  -> parseArgs o{oGlyphs = v} as
    | Just v <- eqOpt "--width" a   -> parseArgs o{oWidth = Just (readInt v)} as
    | Just v <- eqOpt "--title" a   -> parseArgs o{oTitle = v} as
    | Just v <- eqOpt "--italics" a -> parseArgs o{oItalics = Just (v == "on")} as
    | Just v <- eqOpt "-t" a        -> parseArgs o{oFmt = v} as
    | "-" `isPrefixOf` a && a /= "-" -> do
        hPutStrLn stderr ("minimark: unknown option " ++ a)
        hPutStrLn stderr "try: minimark --help"
        exitWith (ExitFailure 2)
    | otherwise -> parseArgs o{oFiles = a : oFiles o} as
  where
    withVal (v:rest) k = k v rest
    withVal [] _ = do
      hPutStrLn stderr ("minimark: option " ++ a ++ " needs a value")
      exitWith (ExitFailure 2)

eqOpt :: String -> String -> Maybe String
eqOpt name arg =
  let pfx = name ++ "="
  in if pfx `isPrefixOf` arg then Just (drop (length pfx) arg) else Nothing

readInt :: String -> Int
readInt s = case span (\c -> c >= '0' && c <= '9') s of
  (ds@(_:_), _) -> foldl (\n c -> n * 10 + (fromEnum c - fromEnum '0')) 0 ds
  _ -> 0

render :: Opts -> [Block] -> IO String
render o doc = case oFmt o of
  f | f `elem` ["term", "ansi"] -> do
        cm <- colorMode (oColor o)
        w <- widthOf (oWidth o)
        let ital = case oItalics o of
                     Just b  -> b
                     Nothing -> cm == MTrue
        return (renderAnsi (AnsiOpts cm (glyphs o) w ital (ascii o)) doc)
    | f == "plain" -> do
        w <- widthOf (oWidth o)
        return (renderAnsi (AnsiOpts MNone (glyphs o) w False (ascii o)) doc)
    | f == "html" ->
        return (renderHtml (HtmlOpts (oStandalone o) (glyphs o) (ascii o)
                                     (titleOf o)) doc)
    | f `elem` ["latex", "tex"] ->
        return (renderLatex (LatexOpts (oStandalone o)) doc)
    | otherwise -> do
        hPutStrLn stderr ("minimark: unknown format " ++ f)
        exitWith (ExitFailure 2)

titleOf :: Opts -> String
titleOf o = if null (oTitle o)
              then (case oFiles o of (f:_) -> f; [] -> "minimark")
              else oTitle o

ascii :: Opts -> Bool
ascii o = oGlyphs o == "ascii"

glyphs :: Opts -> GlyphLevel
glyphs o = if oGlyphs o == "full" then GFull else GBmp

colorMode :: String -> IO ColorMode
colorMode s = case s of
  "none" -> return MNone
  "16"   -> return M16
  "true" -> return MTrue
  _      -> do   -- auto
    ct <- lookupEnv "COLORTERM"
    case ct of
      Just v | v `elem` ["truecolor", "24bit"] -> return MTrue
      _ -> return M16

widthOf :: Maybe Int -> IO Int
widthOf (Just w) = return (max 20 w)
widthOf Nothing = do
  c <- lookupEnv "COLUMNS"
  case c of
    Just v | n <- readInt v, n >= 20 -> return n
    _ -> return 80
