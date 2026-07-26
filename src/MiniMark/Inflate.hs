-- minimark: zlib inflate FFI + gzip unwrapping (T4.1).
--
-- The C side is one flat call (cbits/mm_zlib.c): init/inflate/end over
-- caller-sized buffers.  No z_stream crosses the FFI — struct layout
-- differs between 32-bit BE ppc and 64-bit LE x86 (see the shim's
-- comment and docs/minimark-roadmap.md T4.1).
--
-- gzip is handled here in Haskell: parse the RFC 1952 header, size the
-- output from ISIZE (last 4 bytes, LE), feed the deflate body to the
-- same shim — no second C entry point.  Single-member files only
-- (.abw.gz / .zabw / index.xml.gz are all single-member).
module MiniMark.Inflate(inflateRaw, gunzip, crcCheck, maxMember) where

import Data.Bits((.&.))
import Data.Word(Word8)
import Foreign.Ptr(Ptr, castPtr)
import Foreign.ForeignPtr(mallocForeignPtrBytes, withForeignPtr)
import qualified Data.ByteString as BS
import Data.ByteString(ByteString)

import MiniMark.Bytes(byteAt, le16, le32)

foreign import ccall "mm_zlib.h mm_inflate_raw"
  c_inflate :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO Int

foreign import ccall "mm_zlib.h mm_crc32_check"
  c_crc32_check :: Ptr Word8 -> Int -> Int -> Int -> Int -> Int -> IO Int

-- Does the data match the CRC-32 whose four bytes (LSB first, as laid
-- out in zip central directories and gzip trailers) sit at OFFSET in
-- HDR?  Computed and compared in C — a CRC is a full 32-bit value that
-- a 32-bit ppc Int cannot hold without sign games (which is also why
-- le32 cannot read the field).
crcCheck :: ByteString -> Int -> ByteString -> IO Bool
crcCheck hdr off dat =
  BS.useAsCStringLen dat $ \ (p, len) -> do
    r <- c_crc32_check (castPtr p) len
           (byteAt hdr off) (byteAt hdr (off + 1))
           (byteAt hdr (off + 2)) (byteAt hdr (off + 3))
    return (r /= 0)

-- Decompressed-size cap for any container member: keeps a corrupt or
-- hostile size field from asking a 1GB G4 for an absurd allocation.
maxMember :: Int
maxMember = 64 * 1024 * 1024

-- Inflate a raw deflate stream whose uncompressed size is known
-- exactly (zip and gzip both record it).  Producing any other size is
-- an error: a mismatch means the size field and the stream disagree,
-- and downstream parsers should not see the result.
inflateRaw :: ByteString -> Int -> IO (Either String ByteString)
inflateRaw src usize
  | usize <= 0 = return (Right BS.empty)
  | usize > maxMember =
      return (Left ("member larger than the "
                    ++ show (maxMember `div` (1024 * 1024)) ++ "MB cap"))
  | otherwise =
      BS.useAsCStringLen src $ \ (sp, slen) -> do
        fp <- mallocForeignPtrBytes usize
        withForeignPtr fp $ \ dp -> do
          n <- c_inflate (castPtr sp) slen dp usize
          if n == usize
            then fmap Right (BS.packCStringLen (castPtr dp, n))
            else return (Left (if n < 0
                                 then zerr n
                                 else "inflated size mismatch (got "
                                      ++ show n ++ ", header says "
                                      ++ show usize ++ ")"))

zerr :: Int -> String
zerr n = case n of
  -3   -> "corrupt deflate data"
  -4   -> "zlib out of memory"
  -5   -> "deflate stream does not fit its declared size"
  -100 -> "zlib init failed"
  -101 -> "truncated deflate stream"
  _    -> "zlib error " ++ show n

-- RFC 1952.  Header: 1F 8B, CM, FLG, MTIME(4), XFL, OS = 10 bytes,
-- then optional FEXTRA/FNAME/FCOMMENT/FHCRC fields per FLG bits.
-- Trailer: CRC32(4) + ISIZE(4) — ISIZE sizes the output buffer.
gunzip :: ByteString -> IO (Either String ByteString)
gunzip bs
  | n < 18 = err "truncated gzip file"
  | byteAt bs 0 /= 0x1F || byteAt bs 1 /= 0x8B = err "not a gzip file"
  | byteAt bs 2 /= 8 = err "unsupported gzip compression method"
  | flg >= 0x20 = err "bad gzip flags (reserved bits set)"
  | otherwise =
      case hdrEnd of
        Nothing -> err "truncated gzip header"
        Just h
          | h > n - 8 -> err "truncated gzip file"
          | otherwise ->
              case le32 bs (n - 4) of
                Left e -> err ("gzip size field: " ++ e)
                Right isize -> do
                  r <- inflateRaw (BS.take (n - 8 - h) (BS.drop h bs)) isize
                  case r of
                    Left e -> err e
                    Right out -> do
                      ok <- crcCheck bs (n - 8) out
                      if ok then return (Right out)
                            else err "CRC mismatch (corrupt gzip data)"
  where
    n = BS.length bs
    flg = byteAt bs 3
    err = return . Left
    hdrEnd = pure 10 >>= fextra >>= fname >>= fcomment >>= fhcrc
    fextra o | flg .&. 4 == 0 = Just o
             | o + 2 > n = Nothing
             | otherwise = bounded (o + 2 + le16 bs o)
    fname o | flg .&. 8 == 0 = Just o
            | otherwise = skipz o
    fcomment o | flg .&. 16 == 0 = Just o
               | otherwise = skipz o
    fhcrc o | flg .&. 2 == 0 = Just o
            | otherwise = bounded (o + 2)
    bounded o = if o <= n then Just o else Nothing
    skipz o | o >= n = Nothing
            | byteAt bs o == 0 = Just (o + 1)
            | otherwise = skipz (o + 1)
