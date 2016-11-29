#
#  tcl/tk
#

if [[ $SV_SUPER_OPTIONS == *BUILD_TCL* ]]; then
  echo "BUILD_TCL"

  #  untar tcl/tk
  tar xvf Originals/tcltk/tcl8.5.18-src.tar.gz
  tar xvf Originals/tcltk/tk8.5.18-src.tar.gz  
  tar xvf Originals/tcltk/tcl8.6.4-src.tar.gz
  tar xvf Originals/tcltk/tk8.6.4-src.tar.gz
  tar xvf Originals/tcltk/tcllib-1.17.tar.gz
  tar xvf Originals/tcltk/tklib-0.6.tar.tgz

  #
  # move and rename tcl/tk
  #

  mv tcl8.5.18 ../tcl-8.5.18
  mv tk8.5.18 ../tk-8.5.18
  mv tcl8.6.4 ../tcl-8.6.4
  mv tk8.6.4 ../tk-8.6.4
  mv tcllib-1.17 ../tcllib-1.17
  mv tklib-0.6 ../tklib-0.6

  source Patches/patch-source-tcltk-8.5.sh

fi

#  python
if [[ $SV_SUPER_OPTIONS == *BUILD_PYTHON* ]]; then
  echo "BUILD_PYTHON"
  rm -Rf ../python-2.7.11
  tar xvf Originals/python/python-2.7.11-cmakebuild.tar.gz
  mv python-2.7.11-cmakebuild ../python-2.7.11
  tar xvf Originals/python/Python-2.7.11.tgz
  mv Python-2.7.11 ../python-2.7.11
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *BUILD_NUMPY* ]]; then
  echo "BUILD_NUMPY"
  tar xvf Originals/numpy/numpy-1.11.1.tar.gz
  mv numpy-1.11.1 ..
fi

# freetype2
if [[ $SV_SUPER_OPTIONS == *BUILD_FREETYPE* ]]; then
  echo "BUILD_FREETYPE"
  unzip Originals/freetype/ft263.zip
  mv freetype-2.6.3 ..
fi

#  gdcm
if [[ $SV_SUPER_OPTIONS == *BUILD_GDCM* ]]; then
  echo "BUILD_GDCM"
  tar xvf Originals/gdcm/gdcm-2.6.1.tar.gz
  mv gdcm-2.6.1 ..
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *BUILD_VTK* ]]; then
  echo "BUILD_VTK"
  tar xvf Originals/vtk/VTK-6.2.0.tar.gz
  mv VTK-6.2.0 ../vtk-6.2.0
  tar xvf Originals/vtk/VTK-6.3.0.tar.gz
  mv VTK-6.3.0 ../vtk-6.3.0
  source Patches/patch-source-vtk-6.2.sh
  source Patches/patch-source-vtk-6.3.sh
fi

# itk
if [[ $SV_SUPER_OPTIONS == *BUILD_ITK* ]]; then
  echo "BUILD_ITK"
  tar xvf Originals/itk/InsightToolkit-4.7.1.tar.gz
  mv InsightToolkit-4.7.1 ../itk-4.7.1
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *BUILD_OPENCASCADE* ]]; then
  echo "BUILD_OPENCASCADE"
  tar xvf Originals/opencascade/opencascade-7.0.0.tgz
  mv opencascade-7.0.0 ..
  source Patches/patch-source-opencascade-7.0.0.sh
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *BUILD_MMG* ]]; then
  echo "BUILD_MMG"
  tar xvf Originals/mmg/mmg-5.1.0.tar.gz
  mv mmg-5.1.0 ..
  source Patches/patch-source-mmg-5.1.0.sh
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *BUILD_MITK* ]]; then
  echo "BUILD_MITK"
  tar xvf Originals/mitk/mitk-v2016.03.0.tar.gz
  #cd ..
  mv mitk-2016.03 ..
  source Patches/patch-source-mitk-2016.03.sh
fi
