rm -Rf ../../Code/WorkbenchBuild
mkdir -p ../../Code/WorkbenchBuild
pushd ../../Code/WorkbenchBuild

#compilers
export CC="clang"
export CXX="clang++"

#cmake
export REPLACEME_SV_CMAKE_CMD="cmake"
export REPLACEME_SV_CMAKE_GENERATOR="Unix Makefiles"
export REPLACEME_SV_CMAKE_BUILD_TYPE="RelWithDebInfo"
export REPLACEME_SV_MAKE_CMD="make -j8"
export REPLACEME_SV_TOP_SRC_DIR_SV="../"

#externals
export REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR="/usr/local/sv/ext"
#Qt5
export Qt5_DIR="/usr/local/package/Qt5.4.2/5.4/clang_64/lib/cmake/Qt5"

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
   -DSV_USE_TETGEN_ADAPTOR=ON \
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
\
   -DVTK_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/bin/clang-7.0/x64/vtk-6.2.0/lib/cmake/vtk-6.2" \
   -DITK_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/bin/clang-7.0/x64/itk-4.7.1/lib/cmake/ITK-4.7" \
   -DGDCM_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/bin/clang-7.0/x64/gdcm-2.6.1/lib/gdcm-2.6" \
   -DMMG_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/bin/clang-7.0/x64/mmg-5.1.0" \
   -DOpenCASCADE_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/bin/clang-7.0/x64/opencascade-7.0.0/lib/cmake/opencascade" \
   -DTCL_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/bin/clang-7.0/x64/tcltk-8.6.4" \
   -DPYTHON_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/bin/clang-7.0/x64/python-2.7.11" \
   -DFREETYPE_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/bin/clang-7.0/x64/freetype-2.6.3" \
   -DMITK_DIR="/Users/adamupdegrove/Documents/Software/MITK-11-21-2016/Build/install" \
\
   -Qt5_DIR=$Qt5_DIR \
\
 "$REPLACEME_SV_TOP_SRC_DIR_SV" >& stdout-cmake-config.txt

$REPLACEME_SV_MAKE_CMD >& stdout-compile.txt

$REPLACEME_SV_CPACK_CMD >& stdout-cpack.txt

popd

