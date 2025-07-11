#/bin/bash  -f

REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

export CC=REPLACEME_CC
export CXX=REPLACEME_CXX

rm -Rf REPLACEME_SV_TOP_BIN_DIR_QT
mkdir -p REPLACEME_SV_TOP_BIN_DIR_QT
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_QT

cd ../REPLACEME_SV_QT_DIR

./configure -opensource -confirm-license -release -qt-zlib -qt-libpng -qt-libjpeg -qt-freetype -qt-pcre -xcb -silent --extprefix=REPLACEME_SV_TOP_BIN_DIR_QT

REPLACEME_SV_MAKE_CMD REPLACEME_SV_MAKE_BUILD_PARAMETERS
REPLACEME_SV_MAKE_CMD install

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT


