### install latest version of CMake for Ubuntu
if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
  wget http://simvascular.stanford.edu/downloads/public/open_source/linux/cmake/cmake-3.6.1-Linux-x86_64.sh
  chmod a+rx ./cmake-3.6.1-Linux-x86_64.sh
  sudo mkdir -p /usr/local/package/cmake-3.6.1
  sudo ./cmake-3.6.1-Linux-x86_64.sh --prefix=/usr/local/package/cmake-3.6.1 --skip-license
  sudo ln -s /usr/local/package/cmake-3.6.1/bin/ccmake    /usr/local/bin/ccmake
  sudo ln -s /usr/local/package/cmake-3.6.1/bin/cmake     /usr/local/bin/cmake
  sudo ln -s /usr/local/package/cmake-3.6.1/bin/cmake-gui /usr/local/bin/cmake-gui
  sudo ln -s /usr/local/package/cmake-3.6.1/bin/cpack     /usr/local/bin/cpack
  sudo ln -s /usr/local/package/cmake-3.6.1/bin/ctest     /usr/local/bin/ctest
fi

#compilers
if [[ "$TRAVIS_OS_NAME" == "linux" ]]
then
  export CC="gcc"
  export CXX="g++"
  export Qt5_DIR="/opt/Qt5.4.2/5.4/gcc_64/lib/cmake/Qt5"
elif [[ "$TRAVIS_OS_NAME" == "osx" ]]
then
  export CC="clang"
  export CXX="clang++"
  export Qt5_DIR="/usr/local/package/Qt5.4.2/5.4/clang_64/lib/cmake/Qt5"
fi

#cmake
export REPLACEME_SV_CMAKE_CMD="cmake"
export REPLACEME_SV_CMAKE_GENERATOR="Unix Makefiles"
export REPLACEME_SV_CMAKE_BUILD_TYPE="RelWithDebInfo"
export REPLACEME_SV_MAKE_CMD="make -j8"

MAKE="make --jobs=$NUM_THREADS --keep-going"

# Get externals
mkdir -p $SV_EXTERNALS_BUILD_DIR
pushd $SV_EXTERNALS_BUILD_DIR
"$REPLACEME_SV_CMAKE_CMD" \
  -G "$REPLACEME_SV_CMAKE_GENERATOR" \
  -Qt5_DIR=$Qt5_DIR \
 ../
$MAKE
popd

