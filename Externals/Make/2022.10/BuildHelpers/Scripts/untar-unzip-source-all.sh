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
  tar xf Originals/tcltk/tcl8.6.4-src.tar.gz
  tar xf Originals/tcltk/tk8.6.4-src.tar.gz
  tar xf Originals/tcltk/tcllib-1.17.tar.gz
  tar xf Originals/tcltk/tklib-0.6.tar.tgz

  #
  # move and rename tcl/tk
  #

  mv tcl8.6.4 ../tcl-8.6.4
  mv tk8.6.4 ../tk-8.6.4
  mv tcllib-1.17 ../tcllib-1.17
  mv tklib-0.6 ../tklib-0.6

  pushd ../tk-8.6.4
  patch -p1 < ../BuildHelpers/Patches/2019.06/patch-tk-8.6.4.patch
  popd

fi

#  python
if [[ $SV_SUPER_OPTIONS == *UNTAR_PYTHON* ]]; then
  echo "UNTAR_PYTHON"
  rm -Rf ../python-3.9.10
  tar xf Originals/python/python-3.9.10-cmakebuild.tar.gz
  mv  python-cmake-buildsystem ../python-3.9.10
  tar xf Originals/python/Python-3.9.10.tgz
  mv Python-3.9.10 ../python-3.9.10

  # pushd ../python-3.9.10/Python-3.9.10
  # patch -p1 < ../../BuildHelpers/Patches/2019.06/patch-python-3.5.5-gnu8.patch
  # popd

fi

# swig
if [[ $SV_SUPER_OPTIONS == *UNTAR_SWIG* ]]; then
    echo "UNTAR_SWIG"
  rm -Rf ../swig-3.0.12
  tar xf Originals/swig/swig-3.0.12.tar.gz
  mv swig-3.0.12 ..
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *UNTAR_NUMPY* ]]; then
    echo "UNTAR_NUMPY"
  rm -Rf ../numpy-1.14.3
  tar xf Originals/numpy/numpy-1.14.3.tar.gz
  mv numpy-1.14.3 ..
fi

# tinyxml2
if [[ $SV_SUPER_OPTIONS == *UNTAR_TINYXML2* ]]; then
    echo "UNTAR_TINYXML2"
  rm -Rf ../tinyxml2-6.2.0
  tar xf Originals/tinyxml2/tinyxml2-6.2.0.tar.gz
  mv tinyxml2-6.2.0 ..
fi

# qt
if [[ $SV_SUPER_OPTIONS == *UNTAR_QT* ]]; then
    echo "UNTAR_QT"
  rm -Rf ../qt-5.11.3
  tar xf Originals/qt/qt-everywhere-src-5.11.3.tar.xz
  mv qt-everywhere-src-5.11.3 ../qt-5.11.3
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
  tar xf Originals/gdcm/gdcm-2.6.3.tar.gz
  mv gdcm-2.6.3 ..
  pushd ../gdcm-2.6.3
  patch -p1 < ../BuildHelpers/Patches/2019.06/patch-gdcm-2.6.3-macos.patch
  popd
fi

#  hdf5
if [[ $SV_SUPER_OPTIONS == *UNTAR_HDF5* ]]; then
  echo "UNTAR_HDF5"
  rm -Rf ../hdf5-1.12.2
  tar xf Originals/hdf5/hdf5-1.12.2.tar.gz
  mv hdf5-1.12.2 ..
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *UNTAR_VTK* ]]; then
  echo "UNTAR_VTK"
  rm -Rf ../vtk-9.2.0
  tar xf Originals/vtk/VTK-9.2.0.tar.gz
  mv VTK-9.2.0 ../vtk-9.2.0
  pushd ../vtk-9.2.0
  # patch -p1 < ../BuildHelpers/Patches/2019.06/patch-vtk-8.1.1-windows.patch
  # patch -p1 < ../BuildHelpers/Patches/2019.06/patch-vtk-8.1.1-tk-windows.patch
  popd
fi

# itk
if [[ $SV_SUPER_OPTIONS == *UNTAR_ITK* ]]; then
  echo "UNTAR_ITK"
  rm -Rf ../itk-5.2.1
  tar xf Originals/itk/InsightToolkit-5.2.1.tar.gz
  mv InsightToolkit-5.2.1 ../itk-5.2.1
### do we still need this???  source Patches/patch-source-itk-4.12.2.sh
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *UNTAR_OPENCASCADE* ]]; then
  echo "UNTAR_OPENCASCADE"
  rm -Rf ../opencascade-7.6.0
  tar xf Originals/opencascade/opencascade-7.6.0.tar
  mv opencascade-7.6.0 ..
  # pushd ../opencascade-7.6.0
  # patch -p1 < ../BuildHelpers/Patches/2019.06/patch-opencascade-vtk-greater-8.0.patch
  # patch -p1 < ../BuildHelpers/Patches/2019.06/patch-opencascade-7.3.0-macos.patch
  # popd
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *UNTAR_MMG* ]]; then
  echo "UNTAR_MMG"
  rm -Rf ../mmg-5.3.9
  tar xf Originals/mmg/mmg-5.3.9.tar.gz
  mv mmg-5.3.9 ..
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *UNTAR_MITK* ]]; then
  echo "UNTAR_MITK"
  rm -Rf ../mitk-2022.09.27
  tar xf Originals/mitk/mitk-2022.09.27.tar.gz
  mv mitk ../mitk-2022.09.27
  pushd ../mitk-2022.09.27
  patch -p1 < ../BuildHelpers/Patches/2022.09/patch-mitk-2022.09.27.patch
  popd
fi
