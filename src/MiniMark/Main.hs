-- minimark — terminal Markdown viewer / mini-pandoc with math support.
-- MicroHs implementation for macOS PowerPC (10.5/10.6) and anywhere else
-- a C compiler exists.
module MiniMark.Main(main) where

import Data.List(isPrefixOf)
import System.Environment(getArgs, lookupEnv)
import System.Exit(exitSuccess)
import System.IO

import MiniMark.AST
import MiniMark.Readers(parseFormat, readDocFile, readDocStdin)
import MiniMark.MathRender(GlyphLevel(..))
import MiniMark.Ansi(ColorMode(..), LinkMode(..), AnsiOpts(..), renderAnsi)
import MiniMark.Html(HtmlOpts(..), renderHtml)
import MiniMark.Latex(LatexOpts(..), renderLatex)
import MiniMark.Man(renderMan)
import MiniMark.Rtf(renderRtf)

version :: String
version = "minimark 0.1.0"

usage :: String
usage = unlines
  [ version ++ " — terminal Markdown viewer / converter with TeX math"
  , ""
  , "usage: minimark [OPTIONS] [FILE ...]        (stdin when no FILE)"
  , ""
  , "  -t FORMAT        term (default) | html | latex | man | rtf | plain"
  , "  -f FORMAT        input: markdown (default) | rtf | odt | docx | idml"
  , "                   auto-detected from magic bytes / extension;"
  , "                   markdown and rtf readers are implemented so far"
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
  , "  --links=MODE     osc8 | off (default: off) — term only; osc8 emits"
  , "                   clickable OSC 8 hyperlinks instead of \"text (url)\""
  , "  --title=T        document title (html -s)"
  , "  -h, --help       this text"
  , "  --version        version"
  ]

data Opts = Opts
  { oFmt        :: String
  , oFrom       :: Maybe String
  , oOut        :: Maybe String
  , oColor      :: String
  , oGlyphs     :: String
  , oWidth      :: Maybe Int
  , oItalics    :: Maybe Bool
  , oStandalone :: Bool
  , oTitle      :: String
  , oLinks      :: String
  , oFiles      :: [String]
  }

defOpts :: Opts
defOpts = Opts "term" Nothing Nothing "auto" "bmp" Nothing Nothing False "" "off" []

main :: IO ()
main = do
  args <- getArgs
  opts <- parseArgs defOpts args
  ffmt <- case oFrom opts of
            Nothing -> return Nothing
            Just s -> case parseFormat s of
              Just fm -> return (Just fm)
              Nothing -> die ("unknown input format " ++ s
                              ++ " (markdown, rtf, odt, docx, idml)")
  eds <- case oFiles opts of
           [] -> fmap (: []) (readDocStdin ffmt)
           fs -> mapM (readF ffmt) fs
  doc <- either die return (combineDocs eds)
  out <- render opts doc
  case oOut opts of
    Nothing -> putStr out
    Just f  -> writeFile f out
  where
    readF fm "-" = readDocStdin fm
    readF fm f   = readDocFile fm f

-- Error exits go through libc exit() directly: the MicroHs runtime
-- special-cases only ExitSuccess, an ExitFailure throw surfaces as
-- "uncaught exception" noise with status 1 (eval.c).  exit() flushes
-- C stdio but not MicroHs's own handle buffers — hFlush first.
foreign import ccall "exit" cExit :: Int -> IO ()

die :: String -> IO a
die msg = do
  hPutStrLn stderr ("minimark: " ++ msg)
  hFlush stdout
  hFlush stderr
  cExit 2
  error "unreachable"

-- Blocks concatenate in input order; the first input's meta wins
-- (byte-neutral while readers only produce empty metas — see Readers).
combineDocs :: [Either String Doc] -> Either String Doc
combineDocs eds = case sequence eds of
  Left err -> Left err
  Right ds -> Right (Doc (firstMeta ds) (concat [bs | Doc _ bs <- ds]))
  where
    firstMeta (Doc m _ : _) = m
    firstMeta []            = emptyMeta

parseArgs :: Opts -> [String] -> IO Opts
parseArgs o [] = return o{oFiles = reverse (oFiles o)}
parseArgs o (a:as) = case a of
  "-h"           -> putStr usage >> exitSuccess
  "--help"       -> putStr usage >> exitSuccess
  "--version"    -> putStrLn version >> exitSuccess
  "-t"           -> withVal as (\v r -> parseArgs o{oFmt = v} r)
  "-f"           -> withVal as (\v r -> parseArgs o{oFrom = Just v} r)
  "-o"           -> withVal as (\v r -> parseArgs o{oOut = Just v} r)
  "-s"           -> parseArgs o{oStandalone = True} as
  "--standalone" -> parseArgs o{oStandalone = True} as
  "--ascii"      -> parseArgs o{oGlyphs = "ascii"} as
  _ | Just v <- eqOpt "--color" a   -> parseArgs o{oColor = v} as
    | Just v <- eqOpt "--glyphs" a  -> parseArgs o{oGlyphs = v} as
    | Just v <- eqOpt "--width" a   -> parseArgs o{oWidth = Just (readInt v)} as
    | Just v <- eqOpt "--title" a   -> parseArgs o{oTitle = v} as
    | Just v <- eqOpt "--italics" a -> parseArgs o{oItalics = Just (v == "on")} as
    | Just v <- eqOpt "--links" a   -> parseArgs o{oLinks = v} as
    | Just v <- eqOpt "-t" a        -> parseArgs o{oFmt = v} as
    | Just v <- eqOpt "-f" a        -> parseArgs o{oFrom = Just v} as
    | Just v <- eqOpt "--from" a    -> parseArgs o{oFrom = Just v} as
    | "-" `isPrefixOf` a && a /= "-" ->
        die ("unknown option " ++ a ++ "\ntry: minimark --help")
    | otherwise -> parseArgs o{oFiles = a : oFiles o} as
  where
    withVal (v:rest) k = k v rest
    withVal [] _ = die ("option " ++ a ++ " needs a value")

eqOpt :: String -> String -> Maybe String
eqOpt name arg =
  let pfx = name ++ "="
  in if pfx `isPrefixOf` arg then Just (drop (length pfx) arg) else Nothing

readInt :: String -> Int
readInt s = case span (\c -> c >= '0' && c <= '9') s of
  (ds@(_:_), _) -> foldl (\n c -> n * 10 + (fromEnum c - fromEnum '0')) 0 ds
  _ -> 0

render :: Opts -> Doc -> IO String
render o doc = case oFmt o of
  f | f `elem` ["term", "ansi"] -> do
        cm <- colorMode (oColor o)
        w <- widthOf (oWidth o)
        let ital = case oItalics o of
                     Just b  -> b
                     Nothing -> cm == MTrue
        return (renderAnsi (AnsiOpts cm (glyphs o) w ital (ascii o) (linkMode o)) doc)
    | f == "plain" -> do
        w <- widthOf (oWidth o)
        return (renderAnsi (AnsiOpts MNone (glyphs o) w False (ascii o) LinksOff) doc)
    | f == "html" ->
        return (renderHtml (HtmlOpts (oStandalone o) (glyphs o) (ascii o)
                                     (titleOf o doc)) doc)
    | f `elem` ["latex", "tex"] ->
        return (renderLatex (LatexOpts (oStandalone o)) doc)
    | f == "man" ->
        return (renderMan (titleOf o doc) doc)
    | f == "rtf" ->
        return (renderRtf doc)
    | otherwise -> die ("unknown format " ++ f)

-- <title> precedence: --title flag > mTitle (front matter) > first filename.
titleOf :: Opts -> Doc -> String
titleOf o (Doc m _)
  | not (null (oTitle o)) = oTitle o
  | Just t <- mTitle m    = t
  | otherwise = case oFiles o of (f:_) -> f; [] -> "minimark"

ascii :: Opts -> Bool
ascii o = oGlyphs o == "ascii"

glyphs :: Opts -> GlyphLevel
glyphs o = if oGlyphs o == "full" then GFull else GBmp

linkMode :: Opts -> LinkMode
linkMode o = if oLinks o == "osc8" then LinksOsc8 else LinksOff

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
