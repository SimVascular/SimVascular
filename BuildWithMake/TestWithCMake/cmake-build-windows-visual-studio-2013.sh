
rm -Rf ../../bld
mkdir -p ../../bld
pushd ../../bld

export REPLACEME_SV_CL_COMPILER="C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/bin/amd64/cl.exe"
export REPLACEME_SV_IFORT_COMPILER="C:/Program Files (x86)/Intel/Composer XE 2013 SP1/bin/intel64/ifort.exe"
export REPLACEME_SV_CMAKE_CMD="/cygdrive/c/Program Files (x86)/CMake/bin/cmake.exe"
export REPLACEME_SV_CMAKE_BUILD_TYPE="RelWithDebInfo"
export REPLACEME_SV_CMAKE_GENERATOR="Visual Studio 12 2013 Win64"
export REPLACEME_SV_TOP_SRC_DIR_SV=`pwd`/../Code
export REPLACEME_SV_TOP_SRC_DIR_SV=`cygpath -m $REPLACEME_SV_TOP_SRC_DIR_SV`

"$REPLACEME_SV_CMAKE_CMD" \
\
   -G "$REPLACEME_SV_CMAKE_GENERATOR" \
\
   -DCMAKE_BUILD_TYPE="$REPLACEME_SV_CMAKE_BUILD_TYPE" \
   -DCMAKE_INSTALL_PREFIX=`cygpath -m $PWD`/installed \
\
   -DSV_SUPERBUILD=OFF \
\
   -DSV_EXTERN_LICENSED_BIN_DIR="C:/cygwin64/SV16/licensed" \
   -DSV_EXTERN_OPEN_BIN_DIR="C:/cygwin64/SV16/bin/vs12.5/x64" \
\
   -DSV_THREEDSOLVER_USE_VTK=ON \
   -DSV_USE_TET_ADAPTOR=ON \
   -DSV_USE_THREEDSOLVER=ON \
\
   -DSV_USE_FREETYPE=ON \
   -DSV_USE_GDCM=ON \
   -DSV_USE_ITK=ON \
   -DSV_USE_MPICH2=OFF \
   -DSV_USE_MSMPI=ON \
   -DSV_USE_OPENCASCADE=ON \
   -DSV_USE_PYTHON=ON \
   -DSV_USE_QT=ON \
\
   -DSV_USE_SYSTEM_FREETYPE=ON \
   -DSV_USE_SYSTEM_GDCM=ON \
   -DSV_USE_SYSTEM_ITK=ON \
   -DSV_USE_SYSTEM_PYTHON=ON \
   -DSV_USE_SYSTEM_OPENCASCADE=ON \
   -DSV_USE_SYSTEM_QT=ON \
   -DSV_USE_SYSTEM_TCL=ON \
   -DSV_USE_SYSTEM_VTK=ON \
\
   -DGDCM_SHARED_LIBRARIES=ON \
   -DFREETYPE_SHARED_LIBRARIES=ON \
   -DITK_SHARED_LIBRARIES=ON \
   -DOPENCASCADE_SHARED_LIBRARIES=ON \
   -DQT_SHARED_LIBRARIES=ON \
   -DTCL_SHARED_LIBRARIES=ON \
   -DVTK_SHARED_LIBRARIES=ON \
\
   -DGDCM_DIR="C:/cygwin64/SV16/bin/vs12.5/x64/gdcm-2.6.1/lib/gdcm-2.6" \
   -DITK_DIR="C:/cygwin64/SV16/bin/vs12.5/x64/itk-4.7.1/lib/cmake/ITK-4.7" \
   -DQT_DIR="C:/OpenSource/Qt/5.4/msvc2013_64_opengl/lib/cmake/Qt5" \
   -DVTK_DIR="C:/cygwin64/SV16/bin/vs12.5/x64/vtk-6.2.0/lib/cmake/vtk-6.2" \
   -DVTK_PYTHON_SITE_PACKAGES="C:/cygwin64/SV16/bin/vs12.5/x64/vtk-6.2.0/lib/python2.7/site-packages" \
\
   -DGDCM_DLL_PATH="C:/cygwin64/SV16/bin/vs12.5/x64/gdcm-2.6.1/bin" \
   -DITK_DLL_PATH="C:/cygwin64/SV16/bin/vs12.5/x64/itk-4.7.1/bin" \
   -DOPENCASCADE_DLL_PATH="C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/bin" \
   -DQT_DLL_PATH="C:/OpenSource/Qt/5.4/msvc2013_64_opengl/bin" \
   -DTCL_DLL_PATH="C:/cygwin64/SV16/bin/vs12.5/x64/tcltk-8.6.4/bin" \
   -DVTK_DLL_PATH="C:/cygwin64/SV16/bin/vs12.5/x64/vtk-6.2.0/bin" \
\
  -DFREETYPE_LIBRARY="C:/cygwin64/SV16/bin/vs12.5/x64/freetype-2.6.3/lib/freetype.lib" \
  -DFREETYPE_INCLUDE_DIR_freetype2="C:/cygwin64/SV16/bin/vs12.5/x64/freetype-2.6.3/include" \
  -DFREETYPE_INCLUDE_DIR_ft2build="C:/cygwin64/SV16/bin/vs12.5/x64/freetype-2.6.3/include" \
\
  -DOPENCASCADE_DIR="C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib" \
  -DOPENCASCADE_INCLUDE_DIR="C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/inc" \
O -DOPENCASCADE_LIBRARIES="C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/FWOSPlugin.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKBinTObj.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKBinXCAF.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKFeat.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKFillet.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKIVtk.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKMeshVS.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKOffset.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKOpenGl.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKSTL.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKStd.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKVRML.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKXDEIGES.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKXDESTEP.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKXMesh.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKXmlTObj.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKXmlXCAF.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKBin.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKStdL.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKIGES.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKSTEP.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKTObj.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKXCAF.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKXml.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKBinL.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKBool.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKSTEPAttr.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKSTEP209.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKVCAF.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKXmlL.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKSTEPBase.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKCAF.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKV3d.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKXSBase.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKBO.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKLCAF.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKMesh.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKHLR.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKService.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKPrim.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKCDF.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKShHealing.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKTopAlgo.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKGeomAlgo.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKBRep.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKGeomBase.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKG3d.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKG2d.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKMath.lib;C:/cygwin64/SV16/bin/vs12.5/x64/opencascade-7.0.0/lib/TKernel.lib" \
\
-DTCL_INCLUDE_PATH="C:/cygwin64/SV16/bin/vs12.5/x64/tcltk-8.6.4/include" \
-DTCL_LIBRARY="C:/cygwin64/SV16/bin/vs12.5/x64/tcltk-8.6.4/lib/tcl86t.lib" \
-DTCL_TCLSH="C:/cygwin64/SV16/bin/vs12.5/x64/tcltk-8.6.4/bin/tclsh86t.exe" \
-DTK_INCLUDE_PATH="C:/cygwin64/SV16/bin/vs12.5/x64/tcltk-8.6.4/include" \
-DTK_LIBRARY="C:/cygwin64/SV16/bin/vs12.5/x64/tcltk-8.6.4/lib/tk86t.lib" \
-DTK_WISH="C:/cygwin64/SV16/bin/vs12.5/x64/tcltk-8.6.4/bin/wish86t.exe" \
\
-DPYTHON_DEBUG_LIBRARY="" \
-DPYTHON_INCLUDE_DIR=C:/OpenSource/Python-2.7/include \
-DPYTHON_LIBRARY=C:/OpenSource/Python-2.7/libs/python27.lib \
-DPYTHON_LIBRARY_DEBUG="" \
-DPYTHON_SITE_PACKAGES="C:/OpenSource/Python-2.7/Lib/site-packages" \
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
