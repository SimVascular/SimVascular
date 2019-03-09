#/bin/bash  -f

REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

export CC=REPLACEME_CC
export CXX=REPLACEME_CXX

rm -Rf  REPLACEME_SV_TOP_BIN_DIR_PYTHON
mkdir -p REPLACEME_SV_TOP_BIN_DIR_PYTHON
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_PYTHON

cd ../REPLACEME_SV_PYTHON_DIR
./configure --prefix=REPLACEME_SV_TOP_BIN_DIR_PYTHON --with-threads --enable-shared
make clean
make release
make install
make clean
cd ../BuildHelpers

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT
