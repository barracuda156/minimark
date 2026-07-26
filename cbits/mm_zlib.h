/* minimark zlib shim (T4.1).  See mm_zlib.c. */
int mm_inflate_raw(void *src, int srcLen, void *dst, int dstCap);
int mm_crc32_check(void *buf, int len, int c0, int c1, int c2, int c3);
