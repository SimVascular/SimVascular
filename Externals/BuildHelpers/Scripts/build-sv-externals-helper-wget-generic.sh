export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/src/originals/

mkdir Originals
pushd Originals

#  tcl/tk 8.6
if [[ $SV_SUPER_OPTIONS == *WGET_TCL* ]]; then
  echo "WGET_TCL"
  rm -Rf tcltk
  mkdir -p tcltk
  pushd tcltk
  wget $PARENT_URL/tcltk/tcl8.6.4-src.tar.gz
  wget $PARENT_URL/tcltk/tcllib-1.17.tar.gz
  wget $PARENT_URL/tcltk/tk8.6.4-src.tar.gz
  wget $PARENT_URL/tcltk/tklib-0.6.tar.tgz
  popd
fi

# python 2.7
if [[ $SV_SUPER_OPTIONS == *WGET_PYTHON* ]]; then
  echo "WGET_PYTHON"
  rm -Rf python
  mkdir -p python
  pushd python
  wget $PARENT_URL/python/get-pip.py
  wget $PARENT_URL/python/Python-2.7.13.tgz
  wget $PARENT_URL/python/python-2.7.13-cmakebuild.tar.gz
  popd
fi

# swig
if [[ $SV_SUPER_OPTIONS == *WGET_SWIG* ]]; then
  echo "WGET_SWIG"
  rm -Rf swig
  mkdir -p swig
  pushd swig
  wget $PARENT_URL/swig/swig-3.0.12.tar.gz
  popd
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *WGET_NUMPY* ]]; then
  echo "WGET_NUMPY"
  rm -Rf numpy
  mkdir -p numpy
  pushd numpy
  wget $PARENT_URL/numpy/numpy-1.11.1.tar.gz
  popd
fi

# qt
if [[ $SV_SUPER_OPTIONS == *WGET_QT* ]]; then
  echo "WGET_QT"
  rm -Rf qt
  mkdir -p qt
  pushd qt
  wget $PARENT_URL/qt/qt-everywhere-opensource-src-5.4.2.tar.gz
  popd
fi

# freetype
if [[ $SV_SUPER_OPTIONS == *WGET_FREETYPE* ]]; then
  echo "WGET_FREETYPE"
  rm -Rf freetype
  mkdir -p freetype
  pushd freetype
  wget $PARENT_URL/freetype/freetype-2.5.5.tar.gz
  wget $PARENT_URL/freetype/ft263.zip
  popd
fi

# gdcm
if [[ $SV_SUPER_OPTIONS == *WGET_GDCM* ]]; then
  echo "WGET_GDCM"
  rm -Rf gdcm
  mkdir -p gdcm
  pushd gdcm
  wget $PARENT_URL/gdcm/gdcm-2.6.1.tar.gz
  popd
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *WGET_VTK* ]]; then
  echo "WGET_VTK"
  rm -Rf vtk
  mkdir -p vtk
  pushd vtk
  wget $PARENT_URL/vtk/VTK-6.2.0.tar.gz
  popd
fi

# itk
if [[ $SV_SUPER_OPTIONS == *WGET_ITK* ]]; then
  echo "WGET_ITK"
  rm -Rf itk
  mkdir -p itk
  pushd itk
  wget $PARENT_URL/itk/InsightToolkit-4.7.1.tar.gz
  popd
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *WGET_OPENCASCADE* ]]; then
  echo "WGET_OPENCASCADE"
  rm -Rf opencascade
  mkdir -p opencascade
  pushd opencascade
  wget $PARENT_URL/opencascade/opencascade-7.0.0.tgz
  wget $PARENT_URL/opencascade/Release_Notes_7.0.0.pdf
  popd
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *WGET_MMG* ]]; then
  echo "WGET_MMG"
  rm -Rf mmg
  mkdir -p mmg
  pushd mmg
  wget $PARENT_URL/mmg/mmg-5.1.0.tar.gz
  popd
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *WGET_MITK* ]]; then
  echo "WGET_MITK"
  rm -Rf mitk
  mkdir -p mitk
  pushd mitk
  wget $PARENT_URL/mitk/mitk-v2016.03.0.tar.gz
  popd
fi

popd
