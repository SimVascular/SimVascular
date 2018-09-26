#
#  tcl/tk
#

if [[ $SV_SUPER_OPTIONS == *UNTAR_TCL* ]]; then
  echo "UNTAR_TCL"

  rm -Rf ../tcl-8.6.4
  rm -Rf ../tk-8.6.4
  rm -Rf ../tcllib-1.17
  rm -Rf ../tklib-0.6

  #  untar tcl/tk
  tar xvf Originals/tcltk/tcl8.6.4-src.tar.gz
  tar xvf Originals/tcltk/tk8.6.4-src.tar.gz
  tar xvf Originals/tcltk/tcllib-1.17.tar.gz
  tar xvf Originals/tcltk/tklib-0.6.tar.tgz

  #
  # move and rename tcl/tk
  #

  mv tcl8.6.4 ../tcl-8.6.4
  mv tk8.6.4 ../tk-8.6.4
  mv tcllib-1.17 ../tcllib-1.17
  mv tklib-0.6 ../tklib-0.6

fi

#  python
if [[ $SV_SUPER_OPTIONS == *UNTAR_PYTHON* ]]; then
  echo "UNTAR_PYTHON"
  rm -Rf ../python-3.5.5
  tar xvf Originals/python/python-cmake-buildsystem-2018-05-28.tar.gz
  mv  python-cmake-buildsystem-2018-05-28 ../python-3.5.5
  tar xvf Originals/python/Python-3.5.5.tgz
  mv Python-3.5.5 ../python-3.5.5
fi

# swig
if [[ $SV_SUPER_OPTIONS == *UNTAR_SWIG* ]]; then
    echo "UNTAR_SWIG"
  rm -Rf ../swig-3.0.12
  tar xvf Originals/swig/swig-3.0.12.tar.gz
  mv swig-3.0.12 ..
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *UNTAR_NUMPY* ]]; then
    echo "UNTAR_NUMPY"
  rm -Rf ../numpy-1.14.3
  tar xvf Originals/numpy/numpy-1.14.3.tar.gz
  mv numpy-1.14.3 ..
fi

# tinyxml2
if [[ $SV_SUPER_OPTIONS == *UNTAR_TINYXML2* ]]; then
    echo "UNTAR_TINYXML2"
  rm -Rf ../tinyxml2-6.2.0
  tar xvf Originals/tinyxml2/tinyxml2-6.2.0.tar.gz
  mv tinyxml2-6.2.0 ..
fi

# qt
if [[ $SV_SUPER_OPTIONS == *UNTAR_QT* ]]; then
    echo "UNTAR_SWIG"
  rm -Rf ../qt-5.6.0
  tar xvf Originals/qt/qt-everywhere-opensource-src-5.6.3.tar.gz
  mv qt-everywhere-opensource-src-5.6.3 ../qt-5.6.3
fi

# freetype2
if [[ $SV_SUPER_OPTIONS == *UNTAR_FREETYPE* ]]; then
  echo "UNTAR_FREETYPE"
  rm -Rf ../freetype-2.6.3
  unzip Originals/freetype/ft263.zip
  mv freetype-2.6.3 ..
fi

#  gdcm
if [[ $SV_SUPER_OPTIONS == *UNTAR_GDCM* ]]; then
  echo "UNTAR_GDCM"
  rm -Rf ../gdcm-2.6.3
  tar xvf Originals/gdcm/gdcm-2.6.3.tar.gz
  mv gdcm-2.6.3 ..
  pushd ../gdcm-2.6.3
  patch -p1 < ../BuildHelpers/Patches/2018.05/patch-gdcm-2.6.3-macos.patch
  popd
fi

#  hdf5
if [[ $SV_SUPER_OPTIONS == *UNTAR_HDF5* ]]; then
  echo "UNTAR_HDF5"
  rm -Rf ../hdf5-1.10.1
  tar xvf Originals/hdf5/hdf5-1.10.1.tar.gz
  mv hdf5-1.10.1 ..
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *UNTAR_VTK* ]]; then
  echo "UNTAR_VTK"
  rm -Rf ../vtk-8.1.1
  tar xvf Originals/vtk/VTK-8.1.1.tar.gz
  mv VTK-8.1.1 ../vtk-8.1.1
  pushd ../vtk-8.1.1
  patch -p1 < ../BuildHelpers/Patches/2018.05/patch-vtk-8.1.1-windows.patch
  popd
fi

# itk
if [[ $SV_SUPER_OPTIONS == *UNTAR_ITK* ]]; then
  echo "UNTAR_ITK"
  rm -Rf ../itk-4.13.0
  tar xvf Originals/itk/InsightToolkit-4.13.0.tar.gz
  mv InsightToolkit-4.13.0 ../itk-4.13.0
### do we still need this???  source Patches/patch-source-itk-4.12.2.sh
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *UNTAR_OPENCASCADE* ]]; then
  echo "UNTAR_OPENCASCADE"
  rm -Rf ../opencascade-7.3.0
  tar xvf Originals/opencascade/opencascade-7.3.0.tgz
  mv opencascade-7.3.0 ..
  pushd ../opencascade-7.3.0
  patch -p1 < ../BuildHelpers/Patches/2018.05/patch-opencascade-vtk-greater-8.0.patch
  popd
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *UNTAR_MMG* ]]; then
  echo "UNTAR_MMG"
  rm -Rf ../mmg-5.3.9
  tar xvf Originals/mmg/mmg-5.3.9.tar.gz
  mv mmg-5.3.9 ..
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *UNTAR_MITK* ]]; then
  echo "UNTAR_MITK"
  rm -Rf ../mitk-2018.04
  tar xvf Originals/mitk/mitk-2018-04-alpha.tar.gz
  mv mitk-2018-04-alpha ../mitk-2018.04
  pushd ../mitk-2018.04
  patch -p1 < ../BuildHelpers/Patches/2018.05/patch-mitk-2018.04.patch
  patch -p1 < ../BuildHelpers/Patches/2018.05/patch-mitk-2018.04-boost-windows.patch
  popd
fi
