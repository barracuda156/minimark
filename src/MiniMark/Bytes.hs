-- minimark: byte-level helpers over Data.ByteString (T4.1).
--
-- Zip and gzip fields are little-endian by spec, so multi-byte reads
-- assemble byte-at-a-time with plain arithmetic — endian-safe on the
-- BE ppc targets by construction, no Data.Bits needed.
--
-- 32-bit fields go through le32, which REFUSES values needing more
-- than 31 bits: a 32-bit ppc Int cannot represent them, and in
-- practice every such value is a zip64 sentinel (0xFFFFFFFF) or a
-- >=2GB member, neither of which we support.  Refusing in this one
-- place makes container behavior identical on 32- and 64-bit builds.
module MiniMark.Bytes(
  byteAt, le16, le32,
  decodeUtf8
) where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)

-- Byte at offset as Int (0..255).  Callers bounds-check first; this
-- module never sees an out-of-range offset from container code.
byteAt :: ByteString -> Int -> Int
byteAt bs i = fromIntegral (BS.index bs i)

le16 :: ByteString -> Int -> Int
le16 bs o = byteAt bs o + 256 * byteAt bs (o + 1)

le32 :: ByteString -> Int -> Either String Int
le32 bs o =
  let b3 = byteAt bs (o + 3)
  in if b3 >= 0x80
       then Left "32-bit field out of range (zip64 or >=2GB — not supported)"
       else Right (byteAt bs o
                   + 256 * (byteAt bs (o + 1)
                   + 256 * (byteAt bs (o + 2)
                   + 256 * b3)))

-- Lenient UTF-8 decode: invalid sequences, overlong forms and
-- surrogates become U+FFFD and the decoder resynchronizes on the next
-- byte — container members and zip names must never crash the run.
-- ASCII (the common case for XML members) takes the first guard only.
decodeUtf8 :: ByteString -> String
decodeUtf8 bs = go 0
  where
    n = BS.length bs
    go i
      | i >= n = []
      | b < 0x80 = toEnum b : go (i + 1)
      | b >= 0xC2 && b <= 0xDF && cont 1 =
          toEnum ((b - 0xC0) * 64 + c 1) : go (i + 2)
      | b >= 0xE0 && b <= 0xEF && cont 1 && cont 2 =
          let v = (b - 0xE0) * 4096 + c 1 * 64 + c 2
          in (if v < 0x800 || (v >= 0xD800 && v <= 0xDFFF)
                then bad else toEnum v) : go (i + 3)
      | b >= 0xF0 && b <= 0xF4 && cont 1 && cont 2 && cont 3 =
          let v = (b - 0xF0) * 262144 + c 1 * 4096 + c 2 * 64 + c 3
          in (if v < 0x10000 || v > 0x10FFFF
                then bad else toEnum v) : go (i + 4)
      | otherwise = bad : go (i + 1)
      where
        b = byteAt bs i
        cont k = i + k < n && byteAt bs (i + k) >= 0x80
                           && byteAt bs (i + k) < 0xC0
        c k = byteAt bs (i + k) - 0x80
        bad = '\xFFFD'
