/* minimark zlib shim (T4.1).
 *
 * One flat call instead of binding z_stream through the FFI: the struct
 * layout differs between 32-bit BE ppc and 64-bit LE x86, and
 * field-offset arithmetic on the Haskell side is exactly the kind of
 * silent breakage this project cannot afford.  Buffers are sized by the
 * caller (zip and gzip headers both carry the uncompressed size), so no
 * growth loop is needed.
 *
 * windowBits = -15 selects a RAW deflate stream (no zlib/gzip header) -
 * the encoding of zip method-8 members and of the gzip body after its
 * header, so this single entry point serves every container we read.
 *
 * Returns the number of bytes written to dst (== the caller's expected
 * size on success), or a negative code: zlib's own errors pass through
 * (Z_DATA_ERROR -3, Z_MEM_ERROR -4, Z_BUF_ERROR -5), -100 means
 * inflateInit2 failed, -101 means the stream ran out of input before
 * the final block (truncated data).
 */
#include <string.h>
#include <zlib.h>
#include "mm_zlib.h"

int mm_inflate_raw(void *src, int srcLen, void *dst, int dstCap)
{
  z_stream zs;
  int ret;
  memset(&zs, 0, sizeof zs);
  zs.next_in = (Bytef *)src;
  zs.avail_in = (uInt)srcLen;
  zs.next_out = (Bytef *)dst;
  zs.avail_out = (uInt)dstCap;
  if (inflateInit2(&zs, -15) != Z_OK)
    return -100;
  ret = inflate(&zs, Z_FINISH);
  inflateEnd(&zs);
  if (ret != Z_STREAM_END)
    return ret < 0 ? ret : -101;
  return (int)zs.total_out;
}

/* CRC-32 verification, computed AND compared here: a CRC is a full
 * 32-bit value, and the Haskell side's Int is 32-bit on the ppc
 * targets, where high-bit CRCs would go negative — keeping the
 * comparison in C sidesteps every sign/width trap.  The expected value
 * arrives as the four bytes of the header field, LSB first, exactly as
 * they sit in the file.  Returns 1 on match, 0 on mismatch.
 *
 * This is why corrupt-but-still-decodable deflate data cannot slip
 * through: fixed-huffman streams can survive byte flips and still
 * inflate to the declared length, so the length check alone is not
 * enough — the CRC is the integrity check the container format
 * intends.
 */
int mm_crc32_check(void *buf, int len, int c0, int c1, int c2, int c3)
{
  uLong want = (uLong)(c0 & 0xFF)
             | ((uLong)(c1 & 0xFF) << 8)
             | ((uLong)(c2 & 0xFF) << 16)
             | ((uLong)(c3 & 0xFF) << 24);
  uLong got = crc32(crc32(0L, Z_NULL, 0), (const Bytef *)buf, (uInt)len);
  return got == want;
}
