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
   SV_SUPER_OPTIONS="UNTAR_UNZIP_ALL"
   SV_SUPER_OPTIONS="WGET_TCL         UNTAR_TCL         BUILD_TCL         ARCHIVE_TCL         ZIP_TCL         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_PYTHON      UNTAR_PYTHON      BUILD_PYTHON      ARCHIVE_PYTHON      ZIP_PYTHON      $SV_SUPER_OPTIONS"
#   SV_SUPER_OPTIONS="WGET_SWIG        UNTAR_SWIG        BUILD_SWIG        ARCHIVE_SWIG        ZIP_SWIG        $SV_SUPER_OPTIONS"
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

#
# wget all source code
#

source Scripts/build-sv-externals-helper-wget-generic.sh

#
# unpack all of the source code
#

source Scripts/untar-unzip-source-all.sh

#
# must have primary destination build dir for subst commands
#

sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/create-toplevel-build-dir.sh > tmp/create-toplevel-build-dir.sh
chmod a+rx ./tmp/create-toplevel-build-dir.sh

#
# make build scripts
#

#  tcl/tk 8.6
if [[ $SV_SUPER_OPTIONS == *BUILD_TCL* ]]; then
  echo "CREATE_BUILD_SCRIPT_TCL"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-make-tcl-windows.sh > tmp/compile.make.tcl.cl.sh
  chmod a+rx ./tmp/compile.make.tcl.cl.sh
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-tcl-msvc.bat > tmp/compile.tcl.msvc.bat
  chmod a+rx ./tmp/compile.tcl.msvc.bat
fi

## python
if [[ $SV_SUPER_OPTIONS == *BUILD_PYTHON* ]]; then
  echo "CREATE_BUILD_SCRIPT_PYTHON"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-python-windows.sh > tmp/compile.cmake.python.cl.sh
  chmod a+rx ./tmp/compile.cmake.python.cl.sh
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/post-install-python-windows.sh > tmp/post-install-python-windows.sh
  chmod a+rx ./tmp/post-install-python-windows.sh
fi

# swig
if [[ $SV_SUPER_OPTIONS == *BUILD_SWIG* ]]; then
  echo "CREATE_BUILD_SCRIPT_SWIG"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-make-swig-generic.sh > tmp/compile.make.swig.cl.sh
  chmod a+rx ./tmp/compile.make.swig.cl.sh
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *BUILD_NUMPY* ]]; then
  echo "CREATE_BUILD_SCRIPT_NUMPY"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-python-numpy-windows.sh > tmp/compile.msvc.numpy.cl.sh
  chmod a+rx ./tmp/compile.msvc.numpy.cl.sh
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-python-numpy-msvc.bat > tmp/compile.msvc.numpy.bat
  chmod a+rx ./tmp/compile.msvc.numpy.bat
fi

# tinyxml2
if [[ $SV_SUPER_OPTIONS == *BUILD_TINYXML2* ]]; then
  echo "CREATE_BUILD_SCRIPT_TINYXML2"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-tinyxml2-generic.sh > tmp/compile.cmake.tinyxml2.cl.sh
  chmod a+rx ./tmp/compile.cmake.tinyxml2.cl.sh
fi

# qt
if [[ $SV_SUPER_OPTIONS == *BUILD_QT* ]]; then
  echo "CREATE_BUILD_SCRIPT_QT"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-qt-windows.sh > tmp/compile.qt.msvc.sh
  chmod a+rx ./tmp/compile.qt.msvc.sh
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-qt-msvc-env.bat > tmp/compile.qt.msvc.env.bat
  chmod a+rx ./tmp/compile.qt.msvc.env.bat
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-qt-msvc-configure.bat > tmp/compile.qt.msvc.configure.bat
  chmod a+rx ./tmp/compile.qt.msvc.configure.bat
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-qt-msvc-nmake.bat > tmp/compile.qt.msvc.nmake.bat
  chmod a+rx ./tmp/compile.qt.msvc.nmake.bat
fi

# qt
if [[ $SV_SUPER_OPTIONS == *INSTALL_BIN_QT* ]]; then
  echo "CREATE_INSTALL_SCRIPT_QT"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/install-qt-windows.sh > tmp/install.qt.windows.sh
  chmod a+rx ./tmp/install.qt.windows.sh
fi

# freetype
if [[ $SV_SUPER_OPTIONS == *BUILD_FREETYPE* ]]; then
  echo "CREATE_BUILD_SCRIPT_FREETYPE"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-freetype-generic.sh > tmp/compile.cmake.freetype.cl.sh
  chmod a+rx ./tmp/compile.cmake.freetype.cl.sh
fi

# gdcm
if [[ $SV_SUPER_OPTIONS == *BUILD_GDCM* ]]; then
  echo "CREATE_BUILD_SCRIPT_GDCM"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-gdcm-generic.sh > tmp/compile.cmake.gdcm.cl.sh
  chmod a+rx ./tmp/compile.cmake.gdcm.cl.sh
fi

# hdf5
if [[ $SV_SUPER_OPTIONS == *BUILD_HDF5* ]]; then
  echo "CREATE_BUILD_SCRIPT_HDF5"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-hdf5-generic.sh > tmp/compile.cmake.hdf5.cl.sh
  chmod a+rx ./tmp/compile.cmake.hdf5.cl.sh
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *BUILD_VTK* ]]; then
  echo "CREATE_BUILD_SCRIPT_VTK"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-vtk-generic.sh > tmp/compile.cmake.vtk.cl.sh
  chmod a+rx ./tmp/compile.cmake.vtk.cl.sh
fi

# itk
if [[ $SV_SUPER_OPTIONS == *BUILD_ITK* ]]; then
  echo "CREATE_BUILD_SCRIPT_ITK"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-itk-generic.sh > tmp/compile.cmake.itk.cl.sh
  chmod a+rx ./tmp/compile.cmake.itk.cl.sh
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *BUILD_OPENCASCADE* ]]; then
  echo "CREATE_BUILD_SCRIPT_OPENCASCADE"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-opencascade-generic.sh > tmp/compile.cmake.opencascade.cl.sh
  chmod a+rx ./tmp/compile.cmake.opencascade.cl.sh
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/post-install-opencascade-windows.sh > tmp/post-install-opencascade-windows.sh
  chmod a+rx ./tmp/post-install-opencascade-windows.sh
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *BUILD_MMG* ]]; then
  echo "CREATE_BUILD_SCRIPT_MMG"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-mmg-generic.sh > tmp/compile.cmake.mmg.cl.sh
  chmod a+rx ./tmp/compile.cmake.mmg.cl.sh
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *BUILD_MITK* ]]; then
  echo "CREATE_BUILD_SCRIPT_MITK"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-mitk-generic.sh > tmp/compile.cmake.mitk.cl.sh
  chmod a+rx ./tmp/compile.cmake.mitk.cl.sh
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/post-install-mitk-windows.sh > tmp/post-install-mitk-windows.sh
  chmod a+rx ./tmp/post-install-mitk-windows.sh
fi

# create script to create tar files
if [[ $SV_SUPER_OPTIONS == *ARCHIVE_* ]]; then
  echo "CREATE_BUILD_SCRIPT_TAR_FILES_ALL"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh Scripts/create-archives-generic.sh > tmp/create-archives-windows.cl.sh
  chmod a+rx ./tmp/create-archives-windows.cl.sh
fi

# create script to create zip files
if [[ $SV_SUPER_OPTIONS == *ZIP_* ]]; then
  echo "CREATE_BUILD_SCRIPT_ZIP_FILES_ALL"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh Scripts/tar-to-zip-all.sh > tmp/tar-to-zip-all.windows.cl.sh
  chmod a+rx ./tmp/tar-to-zip-all.windows.cl.sh
fi

# should probably do this on a case-by-case basis inside of cmake builders
echo "CREATE_POST_PROCESS_ALL_CMAKE_CONFIG"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh Scripts/replace-explicit-paths-in-config-cmake.tcl > tmp/replace-explicit-paths-in-config-cmake.tcl
  chmod a+rx ./tmp/replace-explicit-paths-in-config-cmake.tcl

#
# make sure toplevel build directory exists
#

./tmp/create-toplevel-build-dir.sh >& ./tmp/stdout.create-toplevel-build-dir.txt

#
# run installers
#

# qt interactive installer
if [[ $SV_SUPER_OPTIONS == *INSTALL_BIN_QT* ]]; then
  echo "INSTALL_BIN_QT"
  time ./tmp/install.qt.windows.sh >& ./tmp/stdout.install.qt.txt
fi

#
# compile code
#

#  tcl/tk 8.6
if [[ $SV_SUPER_OPTIONS == *BUILD_TCL* ]]; then
  echo "BUILD_TCL"
  ./tmp/compile.make.tcl.cl.sh >& ./tmp/stdout.tcl.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

## python
if [[ $SV_SUPER_OPTIONS == *BUILD_PYTHON* ]]; then
  echo "BUILD_PYTHON"
  ./tmp/compile.cmake.python.cl.sh >& ./tmp/stdout.python.cl.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

#  swig
if [[ $SV_SUPER_OPTIONS == *BUILD_SWIG* ]]; then
  echo "BUILD_SWIG"
  time ./tmp/compile.make.swig.cl.sh >& ./tmp/stdout.swig.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *BUILD_NUMPY* ]]; then
  echo "BUILD_NUMPY"
  ./tmp/compile.msvc.numpy.cl.sh >& ./tmp/stdout.msvc.numpy.cl.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

## python
## want to run post install after numpy is built
if [[ $SV_SUPER_OPTIONS == *BUILD_PYTHON* ]]; then
  echo "BUILD_PYTHON post-install"
  ./tmp/post-install-python-windows.sh >& ./tmp/stdout.post-install-python-windows.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# tinyxml2
if [[ $SV_SUPER_OPTIONS == *BUILD_TINYXML2* ]]; then
  echo "BUILD_TINYXML2"
  ./tmp/compile.cmake.tinyxml2.cl.sh >& ./tmp/stdout.tinyxml2.cl.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

#  qt
if [[ $SV_SUPER_OPTIONS == *BUILD_QT* ]]; then
  echo "BUILD_QT"
  time ./tmp/compile.qt.msvc.sh >& ./tmp/stdout.qt.msvc.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# freetype
if [[ $SV_SUPER_OPTIONS == *BUILD_FREETYPE* ]]; then
  echo "BUILD_FREETYPE"
  ./tmp/compile.cmake.freetype.cl.sh >& ./tmp/stdout.freetype.cl.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# gdcm
if [[ $SV_SUPER_OPTIONS == *BUILD_GDCM* ]]; then
  echo "BUILD_GDCM"
  ./tmp/compile.cmake.gdcm.cl.sh >& ./tmp/stdout.gdcm.cl.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl  
fi

# hdf5
if [[ $SV_SUPER_OPTIONS == *BUILD_HDF5* ]]; then
  echo "BUILD_HDF5"
  ./tmp/compile.cmake.hdf5.cl.sh >& ./tmp/stdout.hdf5.cl.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *BUILD_VTK* ]]; then
  echo "BUILD_VTK"
  ./tmp/compile.cmake.vtk.cl.sh >& ./tmp/stdout.vtk.cl.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# itk
if [[ $SV_SUPER_OPTIONS == *BUILD_ITK* ]]; then
  echo "BUILD_ITK"
  ./tmp/compile.cmake.itk.cl.sh >& ./tmp/stdout.itk.cl.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *BUILD_OPENCASCADE* ]]; then
  echo "BUILD_OPENCASCADE"
  ./tmp/compile.cmake.opencascade.cl.sh >& ./tmp/stdout.opencascade.cl.txt
  ./tmp/post-install-opencascade-windows.sh >& ./tmp/stdout.post-install-windows.opencascade.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *BUILD_MMG* ]]; then
  echo "BUILD_MMG"
  ./tmp/compile.cmake.mmg.cl.sh >& ./tmp/stdout.mmg.cl.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *BUILD_MITK* ]]; then
  echo "BUILD_MITK"
  ./tmp/compile.cmake.mitk.cl.sh >& ./tmp/stdout.mitk.cl.txt
  ./tmp/post-install-mitk-windows.sh >& ./tmp/stdout.post-install-windows.mitk.txt
  tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl
fi

#
# check generated cmake configs for hardcorded paths
#
tclsh ./tmp/replace-explicit-paths-in-config-cmake.tcl

#
# create tar files for distrution
#

source ./tmp/create-archives-windows.cl.sh >& ./tmp/stdout.create-archives-windows.cl.txt

source ./tmp/tar-to-zip-all.windows.cl.sh >& ./tmp/stdout.tar-to-zip-all.windows.cl.txt

