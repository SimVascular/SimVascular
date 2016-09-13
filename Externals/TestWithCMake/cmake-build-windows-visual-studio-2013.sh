
rm -Rf ../../bld
mkdir -p ../../bld
pushd ../../bld

export REPLACEME_SV_CL_COMPILER="C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/bin/amd64/cl.exe"
export REPLACEME_SV_IFORT_COMPILER="C:/Program Files (x86)/Intel/Composer XE 2013 SP1/bin/intel64/ifort.exe"
export REPLACEME_SV_CMAKE_CMD="/cygdrive/c/Program Files/CMake/bin/cmake.exe"
export REPLACEME_SV_CMAKE_BUILD_TYPE="RelWithDebInfo"
export REPLACEME_SV_CMAKE_GENERATOR="Visual Studio 12 2013 Win64"
export REPLACEME_SV_TOP_SRC_DIR_SV=`pwd`/../Code
export REPLACEME_SV_TOP_SRC_DIR_SV=`cygpath -m $REPLACEME_SV_TOP_SRC_DIR_SV`

"$REPLACEME_SV_CMAKE_CMD" \
\
    --debug-output \
\
   -G "$REPLACEME_SV_CMAKE_GENERATOR" \
\
   -DCMAKE_BUILD_TYPE="$REPLACEME_SV_CMAKE_BUILD_TYPE" \
   -DCMAKE_INSTALL_PREFIX=`cygpath -m $PWD`/installed \
\
   -DSV_SUPERBUILD=OFF \
\
   -DSV_EXTERN_LICENSED_BIN_DIR="C:/cygwin64/usr/local/sv/licensed" \
   -DSV_EXTERN_OPEN_BIN_DIR="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64" \
\
   -DSV_THREEDSOLVER_USE_VTK=ON \
   -DSV_USE_TET_ADAPTOR=ON \
   -DSV_USE_THREEDSOLVER=ON \
\
   -DSV_USE_FREETYPE=ON \
   -DSV_USE_FREETYPE_SHARED:BOOL=OFF \
   -DSV_USE_GDCM=ON \
   -DSV_USE_ITK=ON \
   -DSV_USE_MPICH2=OFF \
   -DSV_USE_MSMPI=ON \
   -DSV_USE_OpenCASCADE=ON \
   -DSV_USE_PYTHON=ON \
   -DSV_USE_QT=ON \
   -DSV_USE_MMG=ON \
\
   -DSV_USE_SYSTEM_FREETYPE=ON \
   -DSV_USE_SYSTEM_GDCM=ON \
   -DSV_USE_SYSTEM_ITK=ON \
   -DSV_USE_SYSTEM_PYTHON=ON \
   -DSV_USE_SYSTEM_OpenCASCADE=ON \
   -DSV_USE_SYSTEM_QT=ON \
   -DSV_USE_SYSTEM_TCL=ON \
   -DSV_USE_SYSTEM_VTK=ON \
   -DSV_USE_SYSTEM_MMG=ON \
\
   -DGDCM_USE_SHARED=ON \
   -DFREETYPE_USE_SHARED=OFF \
   -DITK_USE_SHARED=ON \
   -DOpenCASCADE_USE_SHARED=ON \
   -DQT_USE_SHARED=ON \
   -DTCL_USE_SHARED=ON \
   -DVTK_USE_SHARED=ON \
\
   -DGDCM_DIR="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/gdcm-2.6.1/lib/gdcm-2.6" \
   -DITK_DIR="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/itk-4.7.1/lib/cmake/ITK-4.7" \
   -DOpenCASCADE_DIR="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/opencascade-7.0.0/lib" \
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
   -DVTK_DIR="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/vtk-6.2.0/lib/cmake/vtk-6.2" \
   -DVTK_PYTHON_SITE_PACKAGES="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/vtk-6.2.0/lib/python2.7/site-packages" \
\
   -DGDCM_DLL_PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/gdcm-2.6.1/bin" \
   -DITK_DLL_PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/itk-4.7.1/bin" \
   -DOpenCASCADE_DLL_PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/opencascade-7.0.0/bin" \
   -DQT_DLL_PATH="C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl/bin" \
   -DTCL_DLL_PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/tcltk-8.6.4/bin" \
   -DVTK_DLL_PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/vtk-6.2.0/bin" \
\
  -DFREETYPE_DIR:PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/freetype-2.6.3" \
  -DFREETYPE_LIBRARY:FILEPATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/freetype-2.6.3/lib/freetype.lib" \
  -DFREETYPE_INCLUDE_DIR_freetype2:PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/freetype-2.6.3/include" \
  -DFREETYPE_INCLUDE_DIR_ft2build:PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/freetype-2.6.3/include" \
\
-DTCL_INCLUDE_PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/tcltk-8.6.4/include" \
-DTCL_LIBRARY="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/tcltk-8.6.4/lib/tcl86t.lib" \
-DTCL_TCLSH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/tcltk-8.6.4/bin/tclsh86t.exe" \
-DTK_INCLUDE_PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/tcltk-8.6.4/include" \
-DTK_LIBRARY="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/tcltk-8.6.4/lib/tk86t.lib" \
-DTK_WISH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/tcltk-8.6.4/bin/wish86t.exe" \
\
-DPYTHON_DEBUG_LIBRARY="" \
-DPYTHON_INCLUDE_DIR=C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/python-2.7.11/include \
-DPYTHON_LIBRARY=C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/python-2.7.11/libs/python27.lib \
-DPYTHON_LIBRARY_DEBUG="" \
-DPYTHON_SITE_PACKAGES="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/python-2.7.11/Lib/site-packages" \
-DPYTHON_CORE_PACKAGES="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/python-2.7.11/Lib" \
-DPYTHON_DLL_PATH="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/python-2.7.11/Bin" \
\
-DMMG_DIR="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/mmg-5.1.0" \
-DMMG_INCLUDE_DIR="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/mmg-5.1.0/include" \
-DMMG_LIBRARY="C:/cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/mmg-5.1.0/lib/mmg.lib" \
\
-DMPIEXEC="C:/Program Files/Microsoft MPI/Bin/mpiexec.exe" \
-DMPIEXEC_MAX_NUMPROCS=2 \
-DMPIEXEC_NUMPROC_FLAG="-np" \
\
-DMPI_CXX_COMPILER="$REPLACEME_SV_CL_COMPILER" \
-DMPI_CXX_COMPILE_FLAGS="" \
-DMPI_CXX_INCLUDE_PATH="$REPLACEME_SV_TOP_SRC_DIR_SV/ThirdParty/msmpi/Include" \
-DMPI_CXX_LIBRARIES="$REPLACEME_SV_TOP_SRC_DIR_SV/ThirdParty/msmpi/Lib/x64/msmpi.lib" \
-DMPI_CXX_LINK_FLAGS="" \
\
-DMPI_C_COMPILER="$REPLACEME_SV_CL_COMPILER" \
-DMPI_C_COMPILE_FLAGS="" \
-DMPI_C_INCLUDE_PATH="$REPLACEME_SV_TOP_SRC_DIR_SV/ThirdParty/msmpi/Include" \
-DMPI_C_LIBRARIES="$REPLACEME_SV_TOP_SRC_DIR_SV/ThirdParty/msmpi/Lib/x64/msmpi.lib" \
-DMPI_C_LINK_FLAGS="" \
\
-DMPI_Fortran_COMPILER="$REPLACEME_SV_IFORT_COMPILER" \
-DMPI_Fortran_COMPILE_FLAGS="-fpp /MD /Zi /O2 /W0 /4L132 /heap-arrays:256" \
-DMPI_Fortran_INCLUDE_PATH="$REPLACEME_SV_TOP_SRC_DIR_SV/ThirdParty/msmpi/Include" \
-DMPI_Fortran_LIBRARIES="$REPLACEME_SV_TOP_SRC_DIR_SV/ThirdParty/msmpi/Lib/x64/msmpifmc.lib" \
-DMPI_Fortran_LINK_FLAGS=/DEBUG \
\
-DMPI_LIBRARY="$REPLACEME_SV_TOP_SRC_DIR_SV/ThirdParty/msmpi/Lib/x64/msmpi.lib" \
-DMPI_fmpich2_LIBRARY="" \
-DMPI_EXTRA_LIBRARY="" \
\
 "$REPLACEME_SV_TOP_SRC_DIR_SV" >& stdout-cmake-config.txt
 
popd
