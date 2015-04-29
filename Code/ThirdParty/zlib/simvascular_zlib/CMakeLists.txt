PROJECT(SimVascularZLIB)

INCLUDE_DIRECTORIES(BEFORE ${SimVascularZLIB_SOURCE_DIR})

# source files for zlib
SET(SRCS
	adler32.c  compress.c  crc32.c  deflate.c  gzio.c   inffast.c
	inflate.c  inftrees.c  trees.c  uncompr.c  zutil.c
	)

IF(WIN32)
	IF(BUILD_SHARED_LIBS)
		SET(ZLIB_DLL 1)
	ENDIF(BUILD_SHARED_LIBS)
ENDIF(WIN32)

add_library(lib_simvascular_zlib ${SRCS})