rm -Rf ../../Code/WorkbenchDistBuild
mkdir -p ../../Code/WorkbenchDistBuild
pushd ../../Code/WorkbenchDistBuild

#compilers
export CC="clang"
export CXX="clang++"
export MPICC="/opt/local/bin/mpicc"
export MPICXX="/opt/local/bin/mpicxx"
export MPIF90="/opt/local/bin/mpif90"
export MPIEXEC="/opt/local/bin/mpiexec"

#mpi paths and libs
export MPI_INCLUDE_PATH="/opt/local/include/mpich-gcc48"
export LIB_MPI="/opt/local/lib/mpich-gcc48/libmpi.dylib"
export LIB_PMPI="/opt/local/lib/mpich-gcc48/libpmpi.dylib"
export LIB_MPICXX="/opt/local/lib/mpich-gcc48/libmpicxx.dylib"
export LIB_MPIFORT="/opt/local/lib/mpich-gcc48/libmpifort.dylib"
export MPI_LINK_FLAGS="-Wl -headerpad_max_install_names -flat_namespace -commons,use_dylibs"

#cmake
export REPLACEME_SV_CMAKE_CMD="cmake"
export REPLACEME_SV_CMAKE_GENERATOR="Unix Makefiles"
export REPLACEME_SV_CMAKE_BUILD_TYPE="RelWithDebInfo"
export REPLACEME_SV_MAKE_CMD="make -j8"
export REPLACEME_SV_CPACK_CMD="cpack"
export REPLACEME_SV_TOP_SRC_DIR_SV="../"

#externals
export REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR="/Users/adamupdegrove/Documents/Software/sv_externals"
#Qt5
export Qt5_DIR="/usr/local/package/Qt5.4.2/5.4/clang_64/lib/cmake/Qt5"

"$REPLACEME_SV_CMAKE_CMD" \
\
   -G "$REPLACEME_SV_CMAKE_GENERATOR" \
\
   -DCMAKE_BUILD_TYPE="$REPLACEME_SV_CMAKE_BUILD_TYPE" \
   -DBUILD_SHARED_LIBS=ON \
   -DBUILD_TESTING=OFF \
\
   -DSV_ENABLE_DISTRIBUTION=ON \
\
   -DSV_USE_TETGEN_ADAPTOR=ON \
   -DSV_USE_THREEDSOLVER=OFF \
   -DSV_THREEDSOLVER_USE_VTK=ON \
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
\
   -DVTK_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/vtk-6.2.0/lib/cmake/vtk-6.2" \
   -DITK_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/itk-4.7.1/lib/cmake/ITK-4.7" \
   -DGDCM_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/gdcm-2.6.1/lib/gdcm-2.6" \
   -DMMG_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/mmg-5.1.0" \
   -DOpenCASCADE_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/opencascade-7.0.0/lib/cmake/opencascade" \
   -DTCL_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/tcltk-8.6.4" \
   -DPYTHON_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/python-2.7.11" \
   -DFREETYPE_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/freetype-2.6.3" \
   -DMITK_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR/mitk-2016.03" \
\
   -Qt5_DIR=$Qt5_DIR \
\
  -DMPIEXEC="$MPIEXEC" \
  -DMPIEXEC_MAX_NUMPROCS=4 \
  -DMPIEXEC_NUMPROC_FLAG="-np" \
\
  -DMPI_CXX_COMPILER="$MPICXX" \
  -DMPI_CXX_COMPILE_FLAGS="" \
  -DMPI_CXX_INCLUDE_PATH="$MPI_INCLUDE_PATH" \
  -DMPI_CXX_LIBRARIES="$LIB_MPICXX;$LIB_MPI;$LIB_PMPI" \
  -DMPI_CXX_LINK_FLAGS="$MPI_LINK_FLAGS" \
\
  -DMPI_C_COMPILER="$MPICC" \
  -DMPI_C_COMPILE_FLAGS="" \
  -DMPI_C_INCLUDE_PATH="$MPI_INCLUDE_PATH" \
  -DMPI_C_LIBRARIES="$LIB_MPI;$LIB_PMPI" \
  -DMPI_C_LINK_FLAGS="$MPI_LINK_FLAGS" \
\
  -DMPI_Fortran_COMPILER="$MPIF90" \
  -DMPI_Fortran_COMPILE_FLAGS="" \
  -DMPI_Fortran_INCLUDE_PATH="$MPI_INCLUDE_PATH" \
  -DMPI_Fortran_LIBRARIES="$LIB_MPIFORT;$LIB_MPI;$LIB_PMPI" \
  -DMPI_Fortran_LINK_FLAGS="$MPI_LINK_FLAGS" \
\
  -DMPI_LIBRARY="$LIB_MPICXX" \
  -DMPI_fmpich2_LIBRARY="" \
  -DMPI_EXTRA_LIBRARY="" \
\
\
 "$REPLACEME_SV_TOP_SRC_DIR_SV" >& stdout-cmake-config.txt

#$REPLACEME_SV_MAKE_CMD >& stdout-compile.txt

#$REPLACEME_SV_CPACK_CMD >& stdout-cpack.txt

popd
