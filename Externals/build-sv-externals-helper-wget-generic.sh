export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/src/originals/

mkdir Originals
pushd Originals

#  tcl/tk 8.6
if [[ $SV_SUPER_OPTIONS == *WGET_TCL* ]]; then
  echo "WGET_TCL"
  mkdir -p tcltk
  pushd tcltk
  wget $PARENT_URL/tcltk/tcl8.5.18-src.tar.gz
  wget $PARENT_URL/tcltk/tcl8.6.4-src.tar.gz
  wget $PARENT_URL/tcltk/tcllib-1.17.tar.gz
  wget $PARENT_URL/tcltk/tk8.5.18-src.tar.gz
  wget $PARENT_URL/tcltk/tk8.6.4-src.tar.gz
  wget $PARENT_URL/tcltk/tklib-0.6.tar.tgz
  popd
fi

# python 2.7
if [[ $SV_SUPER_OPTIONS == *WGET_PYTHON* ]]; then
  echo "WGET_PYTHON"
  mkdir -p python
  pushd python
  wget $PARENT_URL/python/get-pip.py
  wget $PARENT_URL/python/Python-2.7.11.tgz
  wget $PARENT_URL/python/python-2.7.11-cmakebuild.tar.gz
  popd
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *WGET_NUMPY* ]]; then
  echo "WGET_NUMPY"
  mkdir -p numpy
  pushd numpy
  wget $PARENT_URL/numpy/numpy-1.11.1.tar.gz
  popd
fi

# freetype
if [[ $SV_SUPER_OPTIONS == *WGET_FREETYPE* ]]; then
  echo "WGET_FREETYPE"
  mkdir -p freetype
  pushd freetype
  wget $PARENT_URL/freetype/freetype-2.5.5.tar.gz
  wget $PARENT_URL/freetype/ft263.zip
  popd
fi

# gdcm
if [[ $SV_SUPER_OPTIONS == *WGET_GDCM* ]]; then
  echo "WGET_GDCM"
  mkdir -p gdcm
  pushd gdcm
  wget $PARENT_URL/gdcm/gdcm-2.6.1.tar.gz
  popd
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *WGET_VTK* ]]; then
  echo "WGET_VTK"
  mkdir -p vtk
  pushd vtk
  wget $PARENT_URL/vtk/VTK-6.2.0.tar.gz
  wget $PARENT_URL/vtk/VTK-6.3.0.tar.gz
  popd
fi

# itk
if [[ $SV_SUPER_OPTIONS == *WGET_ITK* ]]; then
  echo "WGET_ITK"
  mkdir -p itk
  pushd itk
  wget $PARENT_URL/itk/InsightToolkit-4.7.1.tar.gz
  popd
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *WGET_OPENCASCADE* ]]; then
  echo "WGET_OPENCASCADE"
  mkdir -p opencascade
  pushd opencascade
  wget $PARENT_URL/opencascade/opencascade-7.0.0.tgz
  wget $PARENT_URL/opencascade/Release_Notes_7.0.0.pdf
  popd
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *WGET_MMG* ]]; then
  echo "WGET_MMG"
  mkdir -p mmg
  pushd mmg
  wget $PARENT_URL/mmg/mmg-5.1.0.tar.gz
  popd
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *WGET_MITK* ]]; then
  echo "WGET_MITK"
  mkdir -p mitk
  pushd mitk
  wget $PARENT_URL/mitk/mitk-v2016.03.0.tar.gz
  popd
fi

popd
