#
# python
#

REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

export CC=REPLACEME_CC
export CXX=REPLACEME_CXX

# pick up openssl from homebrew install location
export LDFLAGS="/usr/local/opt/openssl/lib/libssl.a /usr/local/opt/openssl/lib/libcrypto.a"

# not sure if we need to add this or can just use system after
# specifying LDFLAGS above
#   -DUSE_SYSTEM_OpenSSL=OFF \
#   -DOPENSSL_ROOT_DIR:PATH=/usr/local/opt/openssl \
#   -DOPENSSL_INCLUDE_DIR:PATH=/usr/local/opt/openssl/include \
#   -DOPENSSL_CRYPTO_LIBRARY:FILEPATH=/usr/local/opt/openssl/lib/libssl.a \
#   -DOPENSSL_SSL_LIBRARY:FILEPATH=/usr/local/opt/openssl/lib/libcrypto.a \
#   -DOPENSSL_LIBRARIES="/usr/local/opt/openssl/lib/libssl.a"  \
#

#   -DOPENSSL_ROOT_DIR=/usr/local/opt/openssl \
#   -DOPENSSL_INCLUDE_DIR=/usr/local/opt/openssl/include \
#   -DOPENSSL_LIBRARIES="/usr/local/opt/openssl/lib/libssl.a"  \
   
rm -Rf REPLACEME_SV_TOP_BIN_DIR_PYTHON
mkdir -p REPLACEME_SV_TOP_BIN_DIR_PYTHON
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_PYTHON

rm -Rf REPLACEME_SV_TOP_BLD_DIR_PYTHON
mkdir -p REPLACEME_SV_TOP_BLD_DIR_PYTHON
cd REPLACEME_SV_TOP_BLD_DIR_PYTHON

REPLACEME_SV_CMAKE_CMD -G REPLACEME_SV_CMAKE_GENERATOR \
   -DBUILD_TESTING=OFF \
   -DCMAKE_MACOSX_RPATH=1 \
   -DCMAKE_OSX_SYSROOT=/ \
   -DCMAKE_OSX_SDK=REPLACEME_SV_OS_VER_NO \
   -DCMAKE_OSX_DEPLOYMENT_TARGET=REPLACEME_SV_OS_VER_NO \
   -DBUILD_LIBPYTHON_SHARED=ON \
   -DENABLE_SSL=ON \
   -DBUILTIN_SSL=ON \
   -DUSE_SYSTEM_OpenSSL=ON \
   -DOPENSSL_ROOT_DIR=/usr/local/opt/openssl \
   -DBUILTIN_HASHLIB=ON \
   -DENABLE_CTYPES=ON \
   -DBUILTIN_CTYPES=ON \
   -DCMAKE_INSTALL_PREFIX=REPLACEME_SV_TOP_BIN_DIR_PYTHON \
   -DCMAKE_BUILD_TYPE=REPLACEME_SV_CMAKE_BUILD_TYPE \
   -DPYTHON_VERSION=REPLACEME_SV_PYTHON_FULL_VERSION \
REPLACEME_SV_TOP_SRC_DIR_PYTHON

REPLACEME_SV_MAKE_CMD

REPLACEME_SV_MAKE_CMD install

chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_PYTHON/lib/REPLACEME_SV_PYTHON_LIB_NAME
install_name_tool -id REPLACEME_SV_PYTHON_LIB_NAME REPLACEME_SV_TOP_BIN_DIR_PYTHON/lib/REPLACEME_SV_PYTHON_LIB_NAME

# create a wrapper script for python executable

echo "#!/bin/bash -f" > REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "export PYTHONHOME=REPLACEME_SV_TOP_BIN_DIR_PYTHON" >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "export PYTHONPATH=REPLACEME_SV_TOP_BIN_DIR_PYTHON/lib/python2.7/lib-dynload:REPLACEME_SV_TOP_BIN_DIR_PYTHON/lib:REPLACEME_SV_TOP_BIN_DIR_PYTHON/lib/python2.7:REPLACEME_SV_TOP_BIN_DIR_PYTHON/lib/python2.7/site-packages" >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper

echo "if [ \"\$#\" -eq 0 ]; then" >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "  REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python2.7 " >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "elif [ \"\$#\" -eq 1 ]; then" >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "  REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python2.7 \"\$1\" " >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "elif [ \"\$#\" -eq 2 ]; then" >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "  REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python2.7 \"\$1\" \"\$2\" " >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "elif [ \"\$#\" -eq 3 ]; then" >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "  REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python2.7 \"\$1\" \"\$2\" \"\$3\" " >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "else" >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "  REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python2.7 \"\$1\" \"\$2\" \"\$3\" \"\${@:4}\" " >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
echo "fi" >> REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python-wrapper

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT
