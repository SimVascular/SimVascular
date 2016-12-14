rm -Rf ../../Code/NoFlowSolverUseExternalsBuild
mkdir -p ../../Code/NoFlowSolverUseExternalsBuild
pushd ../../Code/NoFlowSolverUseExternalsBuild

#compilers
export CC="gcc"
export CXX="g++"

#cmake
export REPLACEME_SV_CMAKE_CMD="cmake"
export REPLACEME_SV_CMAKE_GENERATOR="Unix Makefiles"
export REPLACEME_SV_CMAKE_BUILD_TYPE="RelWithDebInfo"
export REPLACEME_SV_MAKE_CMD="make -j8"
export REPLACEME_SV_TOP_SRC_DIR_SV="../"

#Qt5
export Qt5_DIR="/opt/Qt5.4.2/5.4/gcc_64/lib/cmake/Qt5"
export EXTERNALS_DIR="/usr/local/sv/ext"

#sudo mkdir -p $EXTERNALS_DIR
#sudo chmod -R 777 $EXTERNALS_DIR

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
   -DSV_USE_MITK_CONFIG:BOOL=ON \
\
   -DSV_USE_GDCM_SHARED=ON \
   -DSV_USE_FREETYPE_SHARED=ON \
   -DSV_USE_ITK_SHARED=ON \
   -DSV_USE_OpenCASCADE_SHARED=ON \
   -DSV_USE_TCL_SHARED=ON \
   -DSV_USE_VTK_SHARED=ON \
   -DSV_USE_MITK_SHARED=ON \
\
   -DSV_EXTERNALS_USE_TOPLEVEL_DIR=ON \
   -DSV_EXTERNALS_TOPLEVEL_DIR="$EXTERNALS_DIR" \
   -Qt5_DIR=$Qt5_DIR \
   -DMITK_DIR:PATH="/usr/local/sv/ext/build/gnu-4.8/x64/mitk-2016.03/MITK-build" \
\
  -DFREETYPE_DIR:PATH="/usr/local/sv/ext/bin/gnu-4.8/x64/freetype-2.6.3" \
  -DFREETYPE_LIBRARY:FILEPATH="/usr/local/sv/ext/bin/gnu-4.8/x64/freetype-2.6.3/lib/libfreetype.so" \
  -DFREETYPE_INCLUDE_DIR_freetype2:PATH="/usr/local/sv/ext/bin/gnu-4.8/x64/freetype-2.6.3/include" \
  -DFREETYPE_INCLUDE_DIR_ft2build:PATH="/usr/local/sv/ext/bin/gnu-4.8/x64/freetype-2.6.3/include" \
\
 "$REPLACEME_SV_TOP_SRC_DIR_SV" >& stdout-cmake-config.txt

#$REPLACEME_SV_MAKE_CMD >& stdout-compile.txt

popd
