#!/bin/bash

export LC_TYPE=C
export LANG=C

ROOT=/Users/parkerda/software/ktbolt/svExternals

GDCM_VERSION=3.0.10
GDCM_MAJOR_VERSION=${GDCM_VERSION%.*}
GDCM_INSTALL_DIR=$ROOT/install/gdcm
GDCM_CMAKE_DIR=$GDCM_INSTALL_DIR/lib/gdcm-$GDCM_MAJOR_VERSION
GDCM_INCLUDE_DIR=$GDCM_INSTALL_DIR/include/gdcm-$GDCM_MAJOR_VER
GDCM_LIB_DIR=$GDCM_INSTALL_DIR/lib
echo "GDCM_LIB_DIR: ${GDCM_LIB_DIR}"

HDF5_INSTALL_DIR=$ROOT/install/hdf5
HDF5_LIB_DIR=$HDF5_INSTALL_DIR/lib

export Python_ROOT_DIR=${ROOT}/install/python/

cmake \
  -DSV_EXTERNALS_DIR:PATH=${ROOT}/install \
  -DSV_FREETYPE_DIR:PATH=${ROOT}/install/freetype \
  -DSV_GDCM_DIR:PATH=${ROOT}/install/gdcm \
  -DSV_HDF5_DIR:PATH=${ROOT}/install/hdf5 \
  -DSV_ITK_DIR:PATH=${ROOT}/install/itk \
  -DSV_MITK_DIR:PATH=${ROOT}/install/mitk \
  -DSV_MMG_DIR:PATH=${ROOT}/install/mmg \
  -DSV_OpenCASCADE_DIR:PATH=${ROOT}/install/opencascade \
  -DSV_Qt6_DIR:PATH=${ROOT}/install/qt6 \
  -DSV_PYTHON_DIR:PATH=${ROOT}/install/python \
  -DSV_VTK_DIR:PATH=${ROOT}/install/vtk \
  -DPython_INCLUDE_DIRS:PATH=${ROOT}/install/python/include/python3.13 \
  -DPython_LIBRARY_DIRS:PATH=${ROOT}/install/python/lib/ \
  -DCMAKE_PREFIX_PATH:PATH=${ROOT}/install/python \
  -DPython_ROOT_DIR:PATH=${ROOT}/install/python \
  -DPython_DIR:PATH=${ROOT}/install/python \
  -DPYTHON_EXECUTABLE:PATH=${ROOT}/install/python/bin/python3 \
  ..

make

echo "----------------------------------------------------"
echo "-------------------- make finished -----------------"
echo "----------------------------------------------------"

