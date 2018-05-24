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
  rm -Rf ../python-2.7.13
  tar xvf Originals/python/python-2.7.13-cmakebuild.tar.gz
  mv python-2.7.13-cmakebuild ../python-2.7.13
  tar xvf Originals/python/Python-2.7.13.tgz
  mv Python-2.7.13 ../python-2.7.13
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
  rm -Rf ../numpy-1.11.1
  tar xvf Originals/numpy/numpy-1.11.1.tar.gz
  mv numpy-1.11.1 ..
fi

# qt
if [[ $SV_SUPER_OPTIONS == *UNTAR_QT* ]]; then
    echo "UNTAR_SWIG"
  rm -Rf ../qt-5.4.2
  tar xvf Originals/qt/qt-everywhere-opensource-src-5.4.2.tar.gz
  mv qt-everywhere-opensource-src-5.4.2 ../qt-5.4.2
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
  rm -Rf ../gdcm-2.6.1
  tar xvf Originals/gdcm/gdcm-2.6.1.tar.gz
  mv gdcm-2.6.1 ..
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *UNTAR_VTK* ]]; then
  echo "UNTAR_VTK"
  rm -Rf ../vtk-6.2.0
  tar xvf Originals/vtk/VTK-6.2.0.tar.gz
  mv VTK-6.2.0 ../vtk-6.2.0
  source Patches/patch-source-vtk-6.2.sh
fi

# itk
if [[ $SV_SUPER_OPTIONS == *UNTAR_ITK* ]]; then
  echo "UNTAR_ITK"
  rm -Rf ../itk-4.7.1
  tar xvf Originals/itk/InsightToolkit-4.7.1.tar.gz
  mv InsightToolkit-4.7.1 ../itk-4.7.1
  source Patches/patch-source-itk-4.7.1.sh
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *UNTAR_OPENCASCADE* ]]; then
  echo "UNTAR_OPENCASCADE"
  rm -Rf ../opencascade-7.0.0
  tar xvf Originals/opencascade/opencascade-7.0.0.tgz
  mv opencascade-7.0.0 ..
  source Patches/patch-source-opencascade-7.0.0.sh
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *UNTAR_MMG* ]]; then
  echo "UNTAR_MMG"
  rm -Rf ../mmg-5.1.0
  tar xvf Originals/mmg/mmg-5.1.0.tar.gz
  mv mmg-5.1.0 ..
  source Patches/patch-source-mmg-5.1.0.sh
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *UNTAR_MITK* ]]; then
  echo "UNTAR_MITK"
  rm -Rf ../mitk-2016.03
  tar xvf Originals/mitk/mitk-v2016.03.0.tar.gz
  #cd ..
  mv mitk-2016.03 ..
  source Patches/patch-source-mitk-2016.03.sh
fi
