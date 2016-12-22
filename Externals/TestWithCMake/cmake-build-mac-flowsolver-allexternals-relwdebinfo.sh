rm -Rf ../../Code/FlowSolverExternalsBuild
mkdir -p ../../Code/FlowSolverExternalsBuild
pushd ../../Code/FlowSolverExternalsBuild

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
   -DSV_USE_TETGEN_ADAPTOR=ON \
   -DSV_USE_THREEDSOLVER=ON \
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
   -DSV_EXTERNALS_USE_TOPLEVEL_DIR=ON \
   -DSV_EXTERNALS_TOPLEVEL_DIR="$REPLACEME_SV_EXTERNALS_TOPLEVEL_DIR" \
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

$REPLACEME_SV_MAKE_CMD >& stdout-compile.txt

popd
