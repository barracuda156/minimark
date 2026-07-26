-- minimark: input-format detection and reader dispatch (T5.0).
--
-- The reader contract — every reader turns one input into a Doc:
--   text formats   : String -> Doc         (UTF-8-decoded contents)
--   binary formats : FilePath -> IO Doc    (reader reads bytes itself,
--                                           e.g. via the zlib FFI, T4.1)
-- Binary input must NEVER go through the text path: MicroHs's UTF-8
-- decoder hard-errors on invalid bytes (getb_utf8 in the runtime).
-- So dispatch sniffs magic bytes through a binary handle first, and
-- refuses what it cannot parse yet before any text read happens.
--
-- Detection order: -f flag > magic bytes > file extension > markdown.
-- Magic outranks extension so a misnamed binary file gets a clean
-- error instead of a decoder crash.
--
-- Stdin is text-only by construction: the MicroHs runtime attaches the
-- UTF-8 transducer to stdin at startup, and zip containers need a
-- named seekable file anyway.  Binary formats on stdin are refused.
--
-- Multiple inputs: each file is read with its own detected format;
-- blocks concatenate, the FIRST input's Meta wins (predictable, and
-- byte-neutral while only markdown produces empty metas).
module MiniMark.Readers(
  Format(..), formatName, parseFormat,
  readDocFile, readDocStdin
) where

import System.IO
import System.IO.Base(openBinaryFile)
import MiniMark.AST
import MiniMark.Reader(parseDocument)
import MiniMark.RtfReader(parseRtf)

data Format = FMarkdown | FRtf | FOdt | FDocx | FIdml
  deriving (Eq)

formatName :: Format -> String
formatName FMarkdown = "markdown"
formatName FRtf      = "rtf"
formatName FOdt      = "odt"
formatName FDocx     = "docx"
formatName FIdml     = "idml"

-- Accepted -f / --from values.
parseFormat :: String -> Maybe Format
parseFormat s = case s of
  "markdown" -> Just FMarkdown
  "md"       -> Just FMarkdown
  "rtf"      -> Just FRtf
  "odt"      -> Just FOdt
  "fodt"     -> Just FOdt
  "docx"     -> Just FDocx
  "idml"     -> Just FIdml
  _          -> Nothing

-- Read one named input.  Left = user-facing refusal (reader not
-- implemented, unusable container); IO errors (missing file) propagate
-- like they always have.
readDocFile :: Maybe Format -> FilePath -> IO (Either String Doc)
readDocFile mfmt f = do
  efmt <- case mfmt of
            Just fmt -> return (Right fmt)
            Nothing  -> sniffFile f
  case efmt of
    Right FMarkdown -> do
      txt <- readFile f
      return (Right (parseDocument txt))
    Right FRtf -> do
      -- RTF is 7-bit ASCII + \'xx / \uN escapes, but may carry raw
      -- cp1252/MacRoman high bytes; read it through a binary handle so
      -- OUR tables decode them, not MicroHs's UTF-8 transducer (which
      -- would hard-error on a stray 0x92).  One Char = one byte here.
      bytes <- readBinaryFile f
      return (Right (parseRtf bytes))
    Right fmt -> return (Left (f ++ ": " ++ formatName fmt
                               ++ " reader not implemented yet"))
    Left err -> return (Left (f ++ ": " ++ err))

readDocStdin :: Maybe Format -> IO (Either String Doc)
readDocStdin mfmt = case mfmt of
  Just FMarkdown -> md
  Nothing        -> md
  -- RTF is a text format (7-bit ASCII + \'xx escapes), so it can come
  -- through stdin's UTF-8 transducer as long as it carries no raw high
  -- bytes.  Word/TextEdit escape non-ASCII as \'xx or \uN anyway; a raw
  -- high byte on stdin would still crash the transducer, so pipe a file
  -- through `-f rtf FILE` for MacRoman-heavy vintage docs.
  Just FRtf -> do
      txt <- getContents
      return (Right (parseRtf txt))
  Just fmt -> return (Left ("stdin: " ++ formatName fmt
                            ++ " input needs a named file"))
  where
    md = do
      txt <- getContents
      return (Right (parseDocument txt))

-- Read a whole file as raw bytes-as-Chars through a binary handle.
-- hGetContents is lazy; force the entire string (seqList) before the
-- close so no byte is read after hClose.  Used by the RTF reader, whose
-- input may contain cp1252/MacRoman high bytes.
readBinaryFile :: FilePath -> IO String
readBinaryFile f = do
  h <- openBinaryFile f ReadMode
  s <- hGetContents h
  seqList s (hClose h)
  return s

--------------------------------------------------------------------------
-- Detection

-- First bytes as raw Chars (binary handle: one Char = one byte).
-- hGetContents is lazy per char; force the prefix, then close — a
-- MicroHs handle tolerates hClose after a partial lazy read.
sniffFile :: FilePath -> IO (Either String Format)
sniffFile f = do
  h <- openBinaryFile f ReadMode
  s <- hGetContents h
  let pfx = take 8 s
  seqList pfx (hClose h)
  return (classify (extOf f) pfx)

seqList :: [a] -> IO b -> IO b
seqList []       act = act
seqList (x : xs) act = x `seq` seqList xs act

classify :: String -> String -> Either String Format
classify ext pfx
  | zipMagic = case ext of
      "docx" -> Right FDocx
      "odt"  -> Right FOdt
      "idml" -> Right FIdml
      _      -> Left "zip container with unrecognized extension \
                     \(expected .docx, .odt or .idml)"
  | take 5 pfx == "{\\rtf" = Right FRtf
  | take 4 pfx == "\xD0\xCF\x11\xE0" =
      Left "OLE2 container - legacy binary Word (.doc) is not \
           \supported; save as .rtf or .docx"
  | otherwise = Right (extFormat ext)
  where
    zipMagic = take 2 pfx == "PK" &&
               case drop 2 pfx of
                 (a:b:_) -> (a, b) `elem` [('\x03','\x04'),
                                           ('\x05','\x06'),
                                           ('\x07','\x08')]
                 _       -> False

-- Extension fallback for inputs without magic (fodt is flat XML; an
-- .rtf without the {\rtf brace is misnamed, but trust the name and
-- let the future rtf reader complain).  Everything else: markdown,
-- exactly as before T5.0.
extFormat :: String -> Format
extFormat ext = case ext of
  "rtf"  -> FRtf
  "fodt" -> FOdt
  _      -> FMarkdown

-- lowercase extension of the last path component, "" if none
extOf :: FilePath -> String
extOf = map lowerA . go ""
  where
    go ext []       = ext
    go _   ('/':cs) = go "" cs
    go _   ('.':cs) = go cs cs
    go ext (_:cs)   = go ext cs
    lowerA c = if c >= 'A' && c <= 'Z'
                 then toEnum (fromEnum c + 32)
                 else c
