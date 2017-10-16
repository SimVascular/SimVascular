
rm -Rf /cygdrive/c/svtest
mkdir -p /cygdrive/c/svtest

export REPLACEME_SV_CL_COMPILER="C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/bin/amd64/cl.exe"
export REPLACEME_SV_IFORT_COMPILER="C:/Program Files (x86)/Intel/Composer XE 2013 SP1/bin/intel64/ifort.exe"
export REPLACEME_SV_CMAKE_CMD="/cygdrive/c/Program Files/CMake/bin/cmake.exe"
export REPLACEME_SV_CMAKE_BUILD_TYPE="RelWithDebInfo"
export REPLACEME_SV_CMAKE_GENERATOR="Visual Studio 12 2013 Win64"
export REPLACEME_SV_TOP_SRC_DIR_SV=`pwd`/../Code
export REPLACEME_SV_TOP_SRC_DIR_SV=`cygpath -m $REPLACEME_SV_TOP_SRC_DIR_SV`
export REPLACEME_SV_EXTERN_OPEN_BIN_DIR=`cygpath -m ../BuildWithMake/ext/bin`
#export REPLACEME_SV_EXTERN_OPEN_BIN_DIR=C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64

pushd /cygdrive/c/svtest

"$REPLACEME_SV_CMAKE_CMD" \
\
    --debug-output \
\
   -G "$REPLACEME_SV_CMAKE_GENERATOR" \
\
   -DCMAKE_BUILD_TYPE="$REPLACEME_SV_CMAKE_BUILD_TYPE" \
   -DCMAKE_INSTALL_PREFIX=`cygpath -m $PWD`/installed \
\
   -DBUILD_SHARED_LIBS:BOOL=ON \
   -DBUILD_TESTING=OFF \
\
   -DSV_EXTERN_LICENSED_BIN_DIR="C:/cygwin64/usr/local/sv/licensed" \
   -DSV_EXTERN_OPEN_BIN_DIR="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR" \
\
   -DSV_USE_SOLVERIO=ON \
\
   -DSV_USE_TETGEN_ADAPTOR=ON \
   -DSV_USE_FREETYPE=ON \
   -DSV_USE_FREETYPE_SHARED:BOOL=OFF \
   -DSV_USE_GDCM=ON \
   -DSV_USE_ITK=ON \
   -DSV_USE_MPICH2=OFF \
   -DSV_USE_OpenCASCADE=ON \
   -DSV_USE_PYTHON=ON \
   -DSV_USE_QT=ON \
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
   -DSV_USE_MITK_CONFIG:BOOL=OFF \
\
   -DSV_USE_GDCM_SHARED=ON \
   -DSV_USE_FREETYPE_SHARED=ON \
   -DSV_USE_ITK_SHARED=ON \
   -DSV_USE_OpenCASCADE_SHARED=ON \
   -DSV_USE_TCL_SHARED=ON \
   -DSV_USE_VTK_SHARED=ON \
   -DSV_USE_MITK_SHARED=ON \
\
   -DSV_USE_SOLVERIO=ON \
\
   -DGDCM_DIR:PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/gdcm-2.6.1/lib/gdcm-2.6" \
   -DITK_DIR:PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/itk-4.7.1/lib/cmake/ITK-4.7" \
   -DOpenCASCADE_DIR:PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/opencascade-7.0.0/cmake" \
   -DQT_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5" \
   -DQt5_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5" \
   -DQt5Core_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5Core" \
   -DQt5Gui_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5Gui" \
   -DQt5Network_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5Network" \
   -DQt5Widgets_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5Widgets" \
   -DQt5WebKit_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5WebKit" \
   -DQt5WebKitWidgets_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5WebKitWidgets" \
   -DQt5OpenGL_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5OpenGL" \
   -DQt5Sql_DIR="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/lib/cmake/Qt5Sql" \
   -DVTK_DIR:PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/vtk-6.2.0/lib/cmake/vtk-6.2" \
   -DVTK_PYTHON_SITE_PACKAGES="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/vtk-6.2.0/lib/python2.7/site-packages" \
   -DMITK_DIR:PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/mitk-2016.03" \
\
   -DGDCM_DLL_PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/gdcm-2.6.1/bin" \
   -DITK_DLL_PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/itk-4.7.1/bin" \
   -DOpenCASCADE_DLL_PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/opencascade-7.0.0/bin" \
   -DQT_DLL_PATH:PATH="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/bin" \
   -DQT_PLUGIN_PATH:PATH="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/plugins" \
   -DTCL_DLL_PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/tcltk-8.6.4/bin" \
   -DVTK_DLL_PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/vtk-6.2.0/bin" \
\
  -DFREETYPE_DIR:PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/freetype-2.6.3" \
  -DFREETYPE_LIBRARY:FILEPATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/freetype-2.6.3/lib/freetype.lib" \
  -DFREETYPE_INCLUDE_DIR_freetype2:PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/freetype-2.6.3/include" \
  -DFREETYPE_INCLUDE_DIR_ft2build:PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/freetype-2.6.3/include" \
\
-DTCL_INCLUDE_PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/tcltk-8.6.4/include" \
-DTCL_LIBRARY="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/tcltk-8.6.4/lib/tcl86t.lib" \
-DTCL_TCLSH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/tcltk-8.6.4/bin/tclsh86t.exe" \
-DTK_INCLUDE_PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/tcltk-8.6.4/include" \
-DTK_LIBRARY="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/tcltk-8.6.4/lib/tk86t.lib" \
-DTK_WISH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/tcltk-8.6.4/bin/wish86t.exe" \
\
-DPYTHON_DEBUG_LIBRARY:FILEPATH="" \
-DPYTHON_INCLUDE_DIR:PATH=$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/python-2.7.11/include \
-DPYTHON_LIBRARY:FILEPATH=$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/python-2.7.11/libs/python27.lib \
-DPYTHON_LIBRARY_DEBUG="" \
-DPYTHON_SITE_PACKAGES="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/python-2.7.11/Lib/site-packages" \
-DPYTHON_CORE_PACKAGES="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/python-2.7.11/Lib" \
-DPYTHON_DLL_PATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/python-2.7.11/Bin" \
-DPYTHON_EXECUTABLE:FILEPATH="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/python-2.7.11/Bin/python.exe" \
\
-DMMG_DIR="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/mmg-5.1.0" \
-DMMG_INCLUDE_DIR="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/mmg-5.1.0/include" \
-DMMG_LIBRARY="$REPLACEME_SV_EXTERN_OPEN_BIN_DIR/mmg-5.1.0/lib/mmg.lib" \
\
 "$REPLACEME_SV_TOP_SRC_DIR_SV" >& stdout-cmake-config.txt

popd
