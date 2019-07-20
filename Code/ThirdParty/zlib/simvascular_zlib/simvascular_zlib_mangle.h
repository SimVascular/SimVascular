#ifndef simvascular_zlib_mangle_h
#define simvascular_zlib_mangle_h
/*

This header file mangles all symbols exported from the zlib library.
It is included in all files while building the zlib library.  Due to
namespace pollution, no zlib headers should be included in .h files in
cm.

The following command was used to obtain the symbol list:

nm libcmzlib.so |grep " [TRD] "

This is the way to recreate the whole list:

nm libcmzlib.so |grep " [TRD] " | awk '{ print "#define "$3" cv_zlib_"$3 }'

REMOVE the "_init" and "_fini" entries.

*/

#define adler32 cv_zlib_adler32
#define adler32_combine cv_zlib_adler32_combine
#define compress cv_zlib_compress
#define compress2 cv_zlib_compress2
#define compressBound cv_zlib_compressBound
#define crc32 cv_zlib_crc32
#define crc32_combine cv_zlib_crc32_combine
#define get_crc_table cv_zlib_get_crc_table
#define deflate cv_zlib_deflate
#define deflateBound cv_zlib_deflateBound
#define deflateCopy cv_zlib_deflateCopy
#define deflateEnd cv_zlib_deflateEnd
#define deflateInit2_ cv_zlib_deflateInit2_
#define deflateInit_ cv_zlib_deflateInit_
#define deflateParams cv_zlib_deflateParams
#define deflatePrime cv_zlib_deflatePrime
#define deflateReset cv_zlib_deflateReset
#define deflateSetDictionary cv_zlib_deflateSetDictionary
#define deflateSetHeader cv_zlib_deflateSetHeader
#define deflateTune cv_zlib_deflateTune
#define deflate_copyright cv_zlib_deflate_copyright
#define gzclearerr cv_zlib_gzclearerr
#define gzclose cv_zlib_gzclose
#define gzdirect cv_zlib_gzdirect
#define gzdopen cv_zlib_gzdopen
#define gzeof cv_zlib_gzeof
#define gzerror cv_zlib_gzerror
#define gzflush cv_zlib_gzflush
#define gzgetc cv_zlib_gzgetc
#define gzgets cv_zlib_gzgets
#define gzopen cv_zlib_gzopen
#define gzprintf cv_zlib_gzprintf
#define gzputc cv_zlib_gzputc
#define gzputs cv_zlib_gzputs
#define gzread cv_zlib_gzread
#define gzrewind cv_zlib_gzrewind
#define gzseek cv_zlib_gzseek
#define gzsetparams cv_zlib_gzsetparams
#define gztell cv_zlib_gztell
#define gzungetc cv_zlib_gzungetc
#define gzwrite cv_zlib_gzwrite
#define inflate_fast cv_zlib_inflate_fast
#define inflate cv_zlib_inflate
#define inflateCopy cv_zlib_inflateCopy
#define inflateEnd cv_zlib_inflateEnd
#define inflateGetHeader cv_zlib_inflateGetHeader
#define inflateInit2_ cv_zlib_inflateInit2_
#define inflateInit_ cv_zlib_inflateInit_
#define inflatePrime cv_zlib_inflatePrime
#define inflateReset cv_zlib_inflateReset
#define inflateSetDictionary cv_zlib_inflateSetDictionary
#define inflateSync cv_zlib_inflateSync
#define inflateSyncPoint cv_zlib_inflateSyncPoint
#define inflate_copyright cv_zlib_inflate_copyright
#define inflate_table cv_zlib_inflate_table
#define _dist_code cv_zlib__dist_code
#define _length_code cv_zlib__length_code
#define _tr_align cv_zlib__tr_align
#define _tr_flush_block cv_zlib__tr_flush_block
#define _tr_init cv_zlib__tr_init
#define _tr_stored_block cv_zlib__tr_stored_block
#define _tr_tally cv_zlib__tr_tally
#define uncompress cv_zlib_uncompress
#define zError cv_zlib_zError
#define z_errmsg cv_zlib_z_errmsg
#define zcalloc cv_zlib_zcalloc
#define zcfree cv_zlib_zcfree
#define zlibCompileFlags cv_zlib_zlibCompileFlags
#define zlibVersion cv_zlib_zlibVersion

#endif
