export PARENT_URL=http://simvascular.stanford.edu/downloads/public/simvascular/externals/2019.06/src/originals/

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
  wget $PARENT_URL/python/Python-3.5.5.tgz
  wget $PARENT_URL/python/python-cmake-buildsystem-2018-05-28.tar.gz
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
  wget $PARENT_URL/numpy/numpy-1.14.3.tar.gz
  popd
fi

# tinyxml2
if [[ $SV_SUPER_OPTIONS == *WGET_TINYXML2* ]]; then
  echo "WGET_TINYXML2"
  rm -Rf tinyxml2
  mkdir -p tinyxml2
  pushd tinyxml2
  wget $PARENT_URL/tinyxml2/tinyxml2-6.2.0.tar.gz
  popd
fi

# qt
if [[ $SV_SUPER_OPTIONS == *WGET_QT* ]]; then
  echo "WGET_QT"
  rm -Rf qt
  mkdir -p qt
  pushd qt
  wget $PARENT_URL/qt/qt-everywhere-opensource-src-5.11.3.tar.gz
  popd
fi
if [[ $SV_SUPER_OPTIONS == *WGET_BIN_QT* ]]; then
  echo "WGET_QT"
  rm -Rf qt
  mkdir -p qt
  pushd qt
  if [[ $SV_EXTERN_OS == "linux" ]]; then
     osid=$(lsb_release -si)
     case "$osid" in

       'Ubuntu')
          wget $PARENT_URL/qt/qt-opensource-linux-x64-5.11.3.run
          chmod a+rx ./qt-opensource-linux-x64-5.11.3.run	
	  ;;

      'CentOS')
          wget $PARENT_URL/qt/qt-opensource-linux-x64-5.11.3.run
          chmod a+rx ./qt-opensource-linux-x64-5.11.3.run
	  ;;

      'AmazonAMI')
          wget $PARENT_URL/qt/qt-opensource-centos-x64-5.11.3.tar.gz
          chmod a+rx ./qt-opensource-centos-x64-5.11.3.tar.gz
          ;;

      'Amazon')
          wget $PARENT_URL/qt/qt-opensource-centos-x64-5.11.3.tar.gz
          chmod a+rx ./qt-opensource-centos-x64-5.11.3.tar.gz
          ;;

      *)	 
	echo "Error!  Invalid Linux Version!"
	exit
	;;
      esac
  fi
  if [[ $SV_EXTERN_OS == "mac_osx" ]]; then
    wget $PARENT_URL/qt/qt-opensource-mac_osx-x64-5.11.3.tar.gz
    chmod a+rx ./qt-opensource-mac_osx-x64-5.11.3.tar.gz
  fi
  if [[ $SV_EXTERN_OS == "windows" ]]; then
    wget $PARENT_URL/qt/qt-windows-release-x64-5.11.3.tar.gz
    chmod a+rx ./qt-windows-release-x64-5.11.3.tar.gz
  fi
  popd
fi

# freetype
if [[ $SV_SUPER_OPTIONS == *WGET_FREETYPE* ]]; then
  echo "WGET_FREETYPE"
  rm -Rf freetype
  mkdir -p freetype
  pushd freetype
  #wget $PARENT_URL/freetype/freetype-2.5.5.tar.gz
  wget $PARENT_URL/freetype/ft263.zip
  popd
fi

# gdcm
if [[ $SV_SUPER_OPTIONS == *WGET_GDCM* ]]; then
  echo "WGET_GDCM"
  rm -Rf gdcm
  mkdir -p gdcm
  pushd gdcm
  wget $PARENT_URL/gdcm/gdcm-2.6.3.tar.gz
  popd
fi

# hdf5
if [[ $SV_SUPER_OPTIONS == *WGET_HDF5* ]]; then
  echo "WGET_HDF5"
  rm -Rf hdf5
  mkdir -p hdf5
  pushd hdf5
  #  wget $PARENT_URL/hdf5/CMake-hdf5-1.10.1.zip
  wget $PARENT_URL/hdf5/hdf5-1.10.1.tar.gz
  popd
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *WGET_VTK* ]]; then
  echo "WGET_VTK"
  rm -Rf vtk
  mkdir -p vtk
  pushd vtk
  wget $PARENT_URL/vtk/VTK-8.1.1.tar.gz
  popd
fi

# itk
if [[ $SV_SUPER_OPTIONS == *WGET_ITK* ]]; then
  echo "WGET_ITK"
  rm -Rf itk
  mkdir -p itk
  pushd itk
  wget $PARENT_URL/itk/InsightToolkit-4.13.2.tar.gz
  popd
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *WGET_OPENCASCADE* ]]; then
  echo "WGET_OPENCASCADE"
  rm -Rf opencascade
  mkdir -p opencascade
  pushd opencascade
  wget $PARENT_URL/opencascade/opencascade-7.3.0.tgz
#  wget $PARENT_URL/opencascade/Release_Notes_7.3.0.pdf
  popd
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *WGET_MMG* ]]; then
  echo "WGET_MMG"
  rm -Rf mmg
  mkdir -p mmg
  pushd mmg
  wget $PARENT_URL/mmg/mmg-5.3.9.tar.gz
  popd
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *WGET_MITK* ]]; then
  echo "WGET_MITK"
  rm -Rf mitk
  mkdir -p mitk
  pushd mitk
  wget $PARENT_URL/mitk/mitk-2018.04.2.tar.gz
  popd
fi

popd
