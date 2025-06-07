#
#  Create directories
#

mkdir -p tmp
mkdir -p tar_output
mkdir -p zip_output

#
#  Build everything, but can manually override for advanced users
#
#  e.g. to build only MITK:
#   export SV_SUPER_OPTIONS="UNTAR_UNZIP_ALL WGET_MITK UNTAR_MITK BUILD_MITK ARCHIVE_MITK ZIP_MITK"
#

if [ -z "$SV_SUPER_OPTIONS" ]; then
   echo "NOTE: SV_SUPER_OPTIONS defaulting to all possible options."
   rm -Rf tar_output
   mkdir -p tar_output
   rm -Rf zip_output
   mkdir -p zip_output
   SV_SUPER_OPTIONS=""
   SV_SUPER_OPTIONS="WGET_TCL         UNTAR_TCL         BUILD_TCL         ARCHIVE_TCL         ZIP_TCL         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_PYTHON      UNTAR_PYTHON      BUILD_PYTHON      ARCHIVE_PYTHON      ZIP_PYTHON      $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_SWIG        UNTAR_SWIG        BUILD_SWIG        ARCHIVE_SWIG        ZIP_SWIG        $SV_SUPER_OPTIONS"
# numpy is now pip installed during postprocessing script!  Our version is incompatible with tensorflow
#   SV_SUPER_OPTIONS="WGET_NUMPY       UNTAR_NUMPY       BUILD_NUMPY       ARCHIVE_NUMPY       ZIP_NUMPY       $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_TINYXML2    UNTAR_TINYXML2    BUILD_TINYXML2    ARCHIVE_TINYXML2    ZIP_TINYXML2    $SV_SUPER_OPTIONS"
   #   SV_SUPER_OPTIONS="WGET_QT          UNTAR_QT          BUILD_QT          ARCHIVE_QT          ZIP_QT          $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_BIN_QT          INSTALL_BIN_QT   ARCHIVE_QT          ZIP_QT          $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_FREETYPE    UNTAR_FREETYPE    BUILD_FREETYPE    ARCHIVE_FREETYPE    ZIP_FREETYPE    $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_GDCM        UNTAR_GDCM        BUILD_GDCM        ARCHIVE_GDCM        ZIP_GDCM        $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_HDF5        UNTAR_HDF5        BUILD_HDF5        ARCHIVE_HDF5        ZIP_HDF5        $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_VTK         UNTAR_VTK         BUILD_VTK         ARCHIVE_VTK         ZIP_VTK         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_ITK         UNTAR_ITK         BUILD_ITK         ARCHIVE_ITK         ZIP_ITK         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_OPENCASCADE UNTAR_OPENCASCADE BUILD_OPENCASCADE ARCHIVE_OPENCASCADE ZIP_OPENCASCADE $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_MMG         UNTAR_MMG         BUILD_MMG         ARCHIVE_MMG         ZIP_MMG         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_MITK        UNTAR_MITK        BUILD_MITK        ARCHIVE_MITK        ZIP_MITK        $SV_SUPER_OPTIONS"
   export SV_SUPER_OPTIONS
fi

echo "SV_SUPER_OPTIONS for build: $SV_SUPER_OPTIONS"

export CC=/usr/bin/gcc-8
export CXX=/usr/bin/g++-8
export PATH=/home/luca/tmp/cmake/build/bin/:$PATH

#
# wget all source code
#

# source Scripts/build-sv-externals-helper-wget-generic.sh

#
# unpack all of the source code
#

# source Scripts/untar-unzip-source-all.sh

#
# must have primary destination build dir for subst commands
#

sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/create-toplevel-build-dir.sh > tmp/create-toplevel-build-dir.sh
chmod a+rx ./tmp/create-toplevel-build-dir.sh

#
# make build scripts
#

#  tcl/tk 8.6
if [[ $SV_SUPER_OPTIONS == *BUILD_TCL* ]]; then
  echo "CREATE_BUILD_SCRIPT_TCL"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/tcl-linux-generic.sh > tmp/compile.make.tcl.gcc.sh
  chmod a+rx ./tmp/compile.make.tcl.gcc.sh
fi

## python
if [[ $SV_SUPER_OPTIONS == *BUILD_PYTHON* ]]; then
  echo "CREATE_BUILD_SCRIPT_PYTHON"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-cmake-python-linux.sh > tmp/compile.cmake.python.gcc.sh
  chmod a+rx ./tmp/compile.cmake.python.gcc.sh
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/post-install-python-linux.sh > tmp/post-install-python-linux.sh
  chmod a+rx ./tmp/post-install-python-linux.sh
fi

# swig
if [[ $SV_SUPER_OPTIONS == *BUILD_SWIG* ]]; then
  echo "CREATE_BUILD_SCRIPT_SWIG"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-make-swig-generic.sh > tmp/compile.make.swig.gcc.sh
  chmod a+rx ./tmp/compile.make.swig.gcc.sh
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *BUILD_NUMPY* ]]; then
  echo "CREATE_BUILD_SCRIPT_NUMPY"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-python-numpy-linux.sh > tmp/compile.python.numpy-linux.sh
  chmod a+rx ./tmp/compile.python.numpy-linux.sh
fi

# tinyxml2
if [[ $SV_SUPER_OPTIONS == *BUILD_TINYXML2* ]]; then
  echo "CREATE_BUILD_SCRIPT_TINYXML2"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-cmake-tinyxml2-generic.sh > tmp/compile.cmake.tinyxml2.gcc.sh
  chmod a+rx ./tmp/compile.cmake.tinyxml2.gcc.sh
fi

# qt
if [[ $SV_SUPER_OPTIONS == *BUILD_QT* ]]; then
  echo "CREATE_BUILD_SCRIPT_QT"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-make-qt-generic.sh > tmp/compile.make.qt.gcc.sh
  chmod a+rx ./tmp/compile.make.qt.gcc.sh
fi

# qt
if [[ $SV_SUPER_OPTIONS == *INSTALL_BIN_QT* ]]; then
  echo "CREATE_INSTALL_SCRIPT_QT"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/install-qt-linux.sh > tmp/install.qt.gcc.sh
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/qt.installer-noninteractive.qs> tmp/qt.installer-noninteractive.qs
  chmod a+rx ./tmp/install.qt.gcc.sh
  chmod a+rx ./tmp/qt.installer-noninteractive.qs
fi

# freetype
if [[ $SV_SUPER_OPTIONS == *BUILD_FREETYPE* ]]; then
  echo "CREATE_BUILD_SCRIPT_FREETYPE"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-cmake-freetype-generic.sh > tmp/compile.cmake.freetype.gcc.sh
  chmod a+rx ./tmp/compile.cmake.freetype.gcc.sh
fi

# gdcm
if [[ $SV_SUPER_OPTIONS == *BUILD_GDCM* ]]; then
  echo "CREATE_BUILD_SCRIPT_GDCM"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-cmake-gdcm-generic.sh > tmp/compile.cmake.gdcm.gcc.sh
  chmod a+rx ./tmp/compile.cmake.gdcm.gcc.sh
fi

# hdf5
if [[ $SV_SUPER_OPTIONS == *BUILD_HDF5* ]]; then
  echo "CREATE_BUILD_SCRIPT_HDF5"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-cmake-hdf5-generic.sh > tmp/compile.cmake.hdf5.gcc.sh
  chmod a+rx ./tmp/compile.cmake.hdf5.gcc.sh
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *BUILD_VTK* ]]; then
  echo "CREATE_BUILD_SCRIPT_VTK"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-cmake-vtk-generic.sh > tmp/compile.cmake.vtk.gcc.sh
  chmod a+rx ./tmp/compile.cmake.vtk.gcc.sh
fi

# itk
if [[ $SV_SUPER_OPTIONS == *BUILD_ITK* ]]; then
  echo "CREATE_BUILD_SCRIPT_ITK"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-cmake-itk-generic.sh > tmp/compile.cmake.itk.gcc.sh
  chmod a+rx ./tmp/compile.cmake.itk.gcc.sh
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *BUILD_OPENCASCADE* ]]; then
  echo "CREATE_BUILD_SCRIPT_OPENCASCADE"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-cmake-opencascade-generic.sh > tmp/compile.cmake.opencascade.gcc.sh
  chmod a+rx ./tmp/compile.cmake.opencascade.gcc.sh
#  sed -f CompileScripts/sed-script-x64_cygwin-options-gcc.sh CompileScripts/post-install-opencascade-linux.sh > tmp/post-install-opencascade-linux.sh
#  chmod a+rx ./tmp/post-install-opencascade-linux.sh
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *BUILD_MMG* ]]; then
  echo "CREATE_BUILD_SCRIPT_MMG"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-cmake-mmg-generic.sh > tmp/compile.cmake.mmg.gcc.sh
  chmod a+rx ./tmp/compile.cmake.mmg.gcc.sh
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *BUILD_MITK* ]]; then
  echo "CREATE_BUILD_SCRIPT_MITK"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/compile-cmake-mitk-linux.sh > tmp/compile.cmake.mitk.gcc.sh
  chmod a+rx ./tmp/compile.cmake.mitk.gcc.sh
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh CompileScripts/post-install-mitk-linux.sh > tmp/post-install-mitk-linux.sh
  chmod a+rx ./tmp/post-install-mitk-linux.sh
fi

# create script to create tar files
if [[ $SV_SUPER_OPTIONS == *ARCHIVE_* ]]; then
  echo "CREATE_BUILD_SCRIPT_TAR_FILES_ALL"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh Scripts/create-archives-generic.sh > tmp/create-archives-all.gcc.sh
  chmod a+rx ./tmp/create-archives-all.gcc.sh
fi

# create script to create zip files
if [[ $SV_SUPER_OPTIONS == *ZIP_* ]]; then
  echo "CREATE_BUILD_SCRIPT_ZIP_FILES_ALL"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh Scripts/tar-to-zip-all.sh > tmp/tar-to-zip-all.gcc.sh
  chmod a+rx ./tmp/tar-to-zip-all.gcc.sh
fi

# should probably do this on a case-by-case basis inside of cmake builders
echo "CREATE_POST_PROCESS_ALL_CMAKE_CONFIG"
  sed -f CompileScripts/sed-script-x64_${SV_EXTERN_LINUX_VERSION}-options-gcc.sh Scripts/replace-explicit-paths-in-config-cmake.tcl > tmp/replace-explicit-paths-in-config-cmake.tcl
  chmod a+rx ./tmp/replace-explicit-paths-in-config-cmake.tcl

#
# make sure toplevel build directory exists
#

./tmp/create-toplevel-build-dir.sh >& ./tmp/stdout.create-toplevel-build-dir.txt

#
# run installers
#

# # qt interactive installer
# if [[ $SV_SUPER_OPTIONS == *INSTALL_BIN_QT* ]]; then
#   echo "INSTALL_BIN_QT"
#   time ./tmp/install.qt.gcc.sh >& ./tmp/stdout.install.qt.txt
# fi

# #
# # compile code
# #

# #  tcl/tk 8.6
# if [[ $SV_SUPER_OPTIONS == *BUILD_TCL* ]]; then
#   echo "BUILD_TCL"
#   ./tmp/compile.make.tcl.gcc.sh >& ./tmp/stdout.tcl.txt
#   tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
# fi

## python
if [[ $SV_SUPER_OPTIONS == *BUILD_PYTHON* ]]; then
  echo "BUILD_PYTHON"
  ./tmp/compile.cmake.python.gcc.sh >& ./tmp/stdout.python.gcc.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

export PYPATH=/usr/local/sv/ext/2022.10/release/gl2/bin/gnu/7.5/x64/python-3.9.10/
export PATH=$PYPATH/share/python3.9:$PYPATH/bin/:$PYPATH/include/:$PYPATH/lib/:$PATH

#  swig
if [[ $SV_SUPER_OPTIONS == *BUILD_SWIG* ]]; then
  echo "BUILD_SWIG"
  time ./tmp/compile.make.swig.gcc.sh >& ./tmp/stdout.swig.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *BUILD_NUMPY* ]]; then
  echo "BUILD_NUMPY"
  ./tmp/compile.python.numpy-linux.sh >& ./tmp/stdout.numpy.python.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

## python
## want to run post install after numpy is built
if [[ $SV_SUPER_OPTIONS == *BUILD_PYTHON* ]]; then
  echo "BUILD_PYTHON post-install"
  ./tmp/post-install-python-linux.sh >& ./tmp/stdout.post-install-python-linux.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# tinyxml2
if [[ $SV_SUPER_OPTIONS == *BUILD_TINYXML2* ]]; then
  echo "BUILD_TINYXML2"
  ./tmp/compile.cmake.tinyxml2.gcc.sh >& ./tmp/stdout.tinyxml2.gcc.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

#  qt
if [[ $SV_SUPER_OPTIONS == *BUILD_QT* ]]; then
  echo "BUILD_QT"
  time ./tmp/compile.make.qt.gcc.sh >& ./tmp/stdout.qt.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# freetype
if [[ $SV_SUPER_OPTIONS == *BUILD_FREETYPE* ]]; then
  echo "BUILD_FREETYPE"
  ./tmp/compile.cmake.freetype.gcc.sh >& ./tmp/stdout.freetype.gcc.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# gdcm
if [[ $SV_SUPER_OPTIONS == *BUILD_GDCM* ]]; then
  echo "BUILD_GDCM"
  ./tmp/compile.cmake.gdcm.gcc.sh >& ./tmp/stdout.gdcm.gcc.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# hdf5
if [[ $SV_SUPER_OPTIONS == *BUILD_HDF5* ]]; then
  echo "BUILD_HDF5"
  ./tmp/compile.cmake.hdf5.gcc.sh >& ./tmp/stdout.hdf5.gcc.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *BUILD_VTK* ]]; then
  echo "BUILD_VTK"
  ./tmp/compile.cmake.vtk.gcc.sh >& ./tmp/stdout.vtk.gcc.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# itk
if [[ $SV_SUPER_OPTIONS == *BUILD_ITK* ]]; then
  echo "BUILD_ITK"
  ./tmp/compile.cmake.itk.gcc.sh >& ./tmp/stdout.itk.gcc.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# # problem with https://tracker.dev.opencascade.org/view.php?id=32163
# # opencascade
if [[ $SV_SUPER_OPTIONS == *BUILD_OPENCASCADE* ]]; then
  echo "BUILD_OPENCASCADE"
  ./tmp/compile.cmake.opencascade.gcc.sh >& ./tmp/stdout.opencascade.gcc.txt
  #  ./tmp/post-install-opencascade-linux.sh >& ./tmp/stdout.post-install-linux.opencascade.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *BUILD_MMG* ]]; then
  echo "BUILD_MMG"
  ./tmp/compile.cmake.mmg.gcc.sh >& ./tmp/stdout.mmg.gcc.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/gdcm-2.6.3/lib/gdcm-2.6:$PATH
# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/gdcm-2.6.3/lib:$PATH
# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/hdf5-1.10.1/cmake/:$PATH
# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/vtk-8.1.1/lib/cmake/vtk-8.1/:$PATH
# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/vtk-8.1.1/lib/:$PATH
# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/itk-4.13.2/lib/cmake/ITK-4.13/:$PATH
# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/itk-4.13.2/lib/:$PATH
# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/gdcm-2.6.3/include/gdcm-2.6:$PATH
# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/tinyxml2-6.2.0/lib/cmake/:$PATH
# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/tinyxml2-6.2.0/lib/:$PATH
# export PATH=/usr/local/sv/ext/2019.06/release/gl2/bin/gnu/7.5/x64/tinyxml2-6.2.0/include/:$PATH

# mitk
if [[ $SV_SUPER_OPTIONS == *BUILD_MITK* ]]; then
  echo "BUILD_MITK"
  ./tmp/compile.cmake.mitk.gcc.sh >& ./tmp/stdout.mitk.gcc.txt
  ./tmp/post-install-mitk-linux.sh >& ./tmp/stdout.post-install-mitk-linux.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

#
# check generated cmake configs for hardcorded paths
#
tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl

#
# create tar files for distrution
#

source ./tmp/create-archives-all.gcc.sh >& ./tmp/stdout.create-archives-all.gcc.txt

source ./tmp/tar-to-zip-all.gcc.sh >& ./tmp/stdout.tar-to-zip-all.gcc.txt

