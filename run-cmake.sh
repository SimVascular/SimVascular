#!/bin/bash

# run-cmake --externals-path=SV_EXTERNALS  --ml-data-path=SV_ML_DATA

export LC_TYPE=C
export LANG=C
export CXX=/usr/bin/g++

SV_EXTERNALS=""
SV_ML_DATA=""

for i in "$@"; do
  case "$i" in
    --externals-path=*)
      SV_EXTERNALS="${i#*=}"
      ;;
    --ml-data-path=*)
      SV_ML_DATA="${i#*=}"
      ;;
  esac
done

if [ -z "$SV_EXTERNALS" ]; then
  echo "**** ERROR: No --externals-path argument was given."
  exit
fi

if [ -z "$SV_ML_DATA" ]; then
  echo "**** ERROR: No --ml-data-path argument was given."
  exit
fi

#GDCM_VERSION=3.0.10
#GDCM_MAJOR_VERSION=${GDCM_VERSION%.*}
#GDCM_INSTALL_DIR=$SV_EXTERNALS/install/gdcm
#GDCM_CMAKE_DIR=$GDCM_INSTALL_DIR/lib/gdcm-$GDCM_MAJOR_VERSION
#GDCM_INCLUDE_DIR=$GDCM_INSTALL_DIR/include/gdcm-$GDCM_MAJOR_VER
#GDCM_LIB_DIR=$GDCM_INSTALL_DIR/lib
#echo "GDCM_LIB_DIR: ${GDCM_LIB_DIR}"

#HDF5_INSTALL_DIR=$SV_EXTERNALS/install/hdf5
#HDF5_LIB_DIR=$HDF5_INSTALL_DIR/lib

#export Python_SV_EXTERNALS_DIR=${SV_EXTERNALS}/install/python/

# Need to set path to Python shared library to run
# Python interpreter. 
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$SV_EXTERNALS/install/python/lib:

cmake \
  -DSV_ENABLE_DISTRIBUTION:BOOL=ON \
  -DSV_EXTERNALS_DIR:PATH=${SV_EXTERNALS}/install \
  -DSV_FREETYPE_DIR:PATH=${SV_EXTERNALS}/install/freetype \
  -DSV_GDCM_DIR:PATH=${SV_EXTERNALS}/install/gdcm \
  -DSV_HDF5_DIR:PATH=${SV_EXTERNALS}/install/hdf5 \
  -DSV_ITK_DIR:PATH=${SV_EXTERNALS}/install/itk \
  -DSV_MITK_DIR:PATH=${SV_EXTERNALS}/install/mitk \
  -DSV_ML_DIR:PATH=${SV_ML_DATA} \
  -DSV_MMG_DIR:PATH=${SV_EXTERNALS}/install/mmg \
  -DSV_OpenCASCADE_DIR:PATH=${SV_EXTERNALS}/install/opencascade \
  -DSV_Qt6_DIR:PATH=${SV_EXTERNALS}/install/qt6 \
  -DSV_PYTHON_DIR:PATH=${SV_EXTERNALS}/install/python \
  -DSV_TINYXML2_DIR:PATH=${SV_EXTERNALS}/install/tinyxml2 \
  -DSV_VTK_DIR:PATH=${SV_EXTERNALS}/install/vtk \
  -DPython3_INCLUDE_DIRS:PATH=${SV_EXTERNALS}/install/python/include/python3.11 \
  -DPython3_LIBRARY_DIRS:PATH=${SV_EXTERNALS}/install/python/lib/ \
  -DPython_SV_EXTERNALS_DIR:PATH=${SV_EXTERNALS}/install/python \
  -DPython_DIR:PATH=${SV_EXTERNALS}/install/python \
  -DPYTHON_DIR:PATH=${SV_EXTERNALS}/install/python \
  -DPYTHON_EXECUTABLE:PATH=${SV_EXTERNALS}/install/python/bin/python3 \
  -DPython3_EXECUTABLE:PATH=${SV_EXTERNALS}/install/python/bin/python3 \
  -DCMAKE_PREFIX_PATH:PATH=${SV_EXTERNALS}/install/python \
  ..

make -j6

echo "----------------------------------------------------"
echo "-------------------- make finished -----------------"
echo "----------------------------------------------------"

