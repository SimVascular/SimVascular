rm -Rf /cygdrive/c/svdwnld
mkdir -p /cygdrive/c/svdwnld

export REPLACEME_SV_CL_COMPILER="C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/bin/amd64/cl.exe"
export REPLACEME_SV_IFORT_COMPILER="C:/Program Files (x86)/Intel/Composer XE 2013 SP1/bin/intel64/ifort.exe"
export REPLACEME_SV_CMAKE_CMD="/cygdrive/c/Program Files/CMake/bin/cmake.exe"
export REPLACEME_SV_CMAKE_BUILD_TYPE="RelWithDebInfo"
export REPLACEME_SV_CMAKE_GENERATOR="Visual Studio 12 2013 Win64"
export REPLACEME_SV_TOP_SRC_DIR_SV=`pwd`/../../Code
export REPLACEME_SV_TOP_SRC_DIR_SV=`cygpath -m $REPLACEME_SV_TOP_SRC_DIR_SV`

echo "TOP_SRC_DIR $REPLACEME_SV_TOP_SRC_DIR_SV"

pushd /cygdrive/c/svdwnld

#Qt5
export Qt5_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5"

export EXTERNALS_DIR="C:/svdwnld/ext"

mkdir -p $EXTERNALS_DIR
chmod -R 777 $EXTERNALS_DIR

"$REPLACEME_SV_CMAKE_CMD" \
\
    --debug-output \
\
   -G "$REPLACEME_SV_CMAKE_GENERATOR" \
\
   -DCMAKE_BUILD_TYPE="$REPLACEME_SV_CMAKE_BUILD_TYPE" \
   -DBUILD_SHARED_LIBS=ON \
   -DBUILD_TESTING=OFF \
\
   -DSV_USE_FREETYPE=ON \
   -DSV_USE_GDCM=ON \
   -DSV_USE_ITK=ON \
   -DSV_USE_MPICH2=OFF \
   -DSV_USE_OpenCASCADE=ON \
   -DSV_USE_PYTHON=ON \
   -DSV_USE_MMG=ON \
   -DSV_USE_MITK=ON \
   -DSV_USE_QT_GUI=ON \
\
   -DSV_USE_SYSTEM_FREETYPE=ON \
   -DSV_USE_SYSTEM_GDCM=ON \
   -DSV_USE_SYSTEM_ITK=ON \
   -DSV_USE_SYSTEM_PYTHON=ON \
   -DSV_USE_SYSTEM_OpenCASCADE=ON \
   -DSV_USE_SYSTEM_TCL=ON \
   -DSV_USE_SYSTEM_VTK=ON \
   -DSV_USE_SYSTEM_MMG=ON \
   -DSV_USE_SYSTEM_MITK=ON \
\
   -DSV_USE_GDCM_SHARED=ON \
   -DSV_USE_FREETYPE_SHARED=ON \
   -DSV_USE_ITK_SHARED=ON \
   -DSV_USE_OpenCASCADE_SHARED=ON \
   -DSV_USE_TCL_SHARED=ON \
   -DSV_USE_VTK_SHARED=ON \
   -DSV_USE_MITK_SHARED=ON \
\
   -DSV_DOWNLOAD_EXTERNALS=ON \
   -DSV_EXTERNALS_USE_TOPLEVEL_DIR=ON \
   -DSV_EXTERNALS_TOPLEVEL_DIR="$EXTERNALS_DIR" \
   -Qt5_DIR=$Qt5_DIR \
\
 "$REPLACEME_SV_TOP_SRC_DIR_SV" >& stdout-cmake-config.txt

$REPLACEME_SV_MAKE_CMD >& stdout-compile.txt

popd
