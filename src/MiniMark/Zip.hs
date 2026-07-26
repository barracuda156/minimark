-- minimark: zip container walker (T4.1) — the substrate for the
-- docx/odt/idml readers (Phase 5) and for `--zip=` container plumbing.
--
-- Reads the end-of-central-directory record, walks the central
-- directory, extracts members by local-header offset.  Only methods 0
-- (stored) and 8 (deflate, via the mm_zlib shim) exist in the wild for
-- the office formats we target; everything else is refused by name.
-- Member sizes and offsets come from the CENTRAL directory, never the
-- local header — local size fields are legitimately zero when the
-- writer streamed with a data descriptor (general-purpose bit 3).
--
-- All multi-byte fields are little-endian by spec; reads go through
-- MiniMark.Bytes, so a BE ppc build parses identically.  Out of scope,
-- refused with clean errors: zip64, multi-disk archives, encrypted
-- members.  Malformed input never crashes — every offset is
-- bounds-checked before use.
module MiniMark.Zip(
  ZipEntry(..), zipEntries, zipFind, zipExtract
) where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)

import MiniMark.Bytes(byteAt, le16, le32, decodeUtf8)
import MiniMark.Inflate(inflateRaw, crcCheck, maxMember)

data ZipEntry = ZipEntry
  { zeName     :: String
  , zeMethod   :: Int      -- 0 = stored, 8 = deflate
  , zeFlags    :: Int      -- general-purpose bits (bit 0 = encrypted)
  , zeCSize    :: Int
  , zeUSize    :: Int
  , zeLocalOff :: Int
  , zeCrcOff   :: Int      -- archive offset of the entry's CRC-32 field
  }

-- End of central directory: 22 bytes + trailing comment (<= 64K).
-- Scan back from the no-comment position; a hit must account for the
-- rest of the file as its comment, which rejects stray signatures
-- inside member data.
findEocd :: ByteString -> Maybe Int
findEocd bs = go (n - 22)
  where
    n = BS.length bs
    lo = max 0 (n - 22 - 65535)
    go o | o < lo = Nothing
         | byteAt bs o == 0x50 && byteAt bs (o + 1) == 0x4B
           && byteAt bs (o + 2) == 5 && byteAt bs (o + 3) == 6
           && o + 22 + le16 bs (o + 20) == n = Just o
         | otherwise = go (o - 1)

zipEntries :: ByteString -> Either String [ZipEntry]
zipEntries bs
  | BS.length bs < 22 = Left "not a zip archive (too small)"
  | otherwise =
      case findEocd bs of
        Nothing -> Left "not a zip archive (no end-of-central-directory)"
        Just eo
          | le16 bs (eo + 4) /= 0 || le16 bs (eo + 6) /= 0 ->
              Left "multi-disk zip not supported"
          | le16 bs (eo + 10) == 0xFFFF ->
              Left "zip64 not supported"
          | otherwise -> do
              cdOff <- le32 bs (eo + 16)
              cdSize <- le32 bs (eo + 12)
              if cdOff + cdSize > BS.length bs
                then Left "central directory out of bounds"
                else walk cdOff (le16 bs (eo + 10))
  where
    walk _ 0 = Right []
    walk p k
      | p + 46 > BS.length bs = Left "truncated central directory"
      | byteAt bs p /= 0x50 || byteAt bs (p + 1) /= 0x4B
        || byteAt bs (p + 2) /= 1 || byteAt bs (p + 3) /= 2 =
          Left "bad central directory entry"
      | otherwise = do
          csize <- le32 bs (p + 20)
          usize <- le32 bs (p + 24)
          off   <- le32 bs (p + 42)
          let nameLen = le16 bs (p + 28)
              skip = nameLen + le16 bs (p + 30) + le16 bs (p + 32)
          if p + 46 + nameLen > BS.length bs
            then Left "truncated central directory"
            else do
              let e = ZipEntry
                        { zeName = decodeUtf8
                            (BS.take nameLen (BS.drop (p + 46) bs))
                        , zeMethod = le16 bs (p + 10)
                        , zeFlags = le16 bs (p + 8)
                        , zeCSize = csize
                        , zeUSize = usize
                        , zeLocalOff = off
                        , zeCrcOff = p + 16
                        }
              rest <- walk (p + 46 + skip) (k - 1)
              Right (e : rest)

zipFind :: [ZipEntry] -> String -> Maybe ZipEntry
zipFind es nm = case [e | e <- es, zeName e == nm] of
  (e : _) -> Just e
  []      -> Nothing

zipExtract :: ByteString -> ZipEntry -> IO (Either String ByteString)
zipExtract bs e
  | odd (zeFlags e) = err "member is encrypted"
  | zeUSize e > maxMember =
      err ("member larger than the "
           ++ show (maxMember `div` (1024 * 1024)) ++ "MB cap")
  | lo + 30 > n = err "local header out of bounds"
  | byteAt bs lo /= 0x50 || byteAt bs (lo + 1) /= 0x4B
    || byteAt bs (lo + 2) /= 3 || byteAt bs (lo + 3) /= 4 =
      err "bad local header"
  | otherwise =
      let dataOff = lo + 30 + le16 bs (lo + 26) + le16 bs (lo + 28)
      in if dataOff + zeCSize e > n
           then err "member data out of bounds"
           else extract (BS.take (zeCSize e) (BS.drop dataOff bs))
  where
    n = BS.length bs
    lo = zeLocalOff e
    err = return . Left
    -- Both methods end in a CRC-32 check against the central-directory
    -- field: deflate streams can survive byte flips and still inflate
    -- to the declared length, so size agreement alone proves nothing.
    extract raw = case zeMethod e of
      0 | zeCSize e == zeUSize e -> checked (return (Right raw))
        | otherwise -> err "stored member with mismatched sizes"
      8 -> checked (inflateRaw raw (zeUSize e))
      m -> err ("unsupported compression method " ++ show m
                ++ " (only stored and deflated)")
    checked act = do
      r <- act
      case r of
        Left e' -> err e'
        Right out -> do
          ok <- crcCheck bs (zeCrcOff e) out
          if ok then return (Right out)
                else err "CRC mismatch (corrupt member)"
