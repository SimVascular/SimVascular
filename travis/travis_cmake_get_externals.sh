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
  export Qt5_DIR="/opt/Qt5.4.2/5.4/clang_64/lib/cmake/Qt5"
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

