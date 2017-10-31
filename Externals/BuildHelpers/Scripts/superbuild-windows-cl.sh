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
   SV_SUPER_OPTIONS="WGET_NUMPY       UNTAR_NUMPY       BUILD_NUMPY       ARCHIVE_NUMPY       ZIP_NUMPY       $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_FREETYPE    UNTAR_FREETYPE    BUILD_FREETYPE    ARCHIVE_FREETYPE    ZIP_FREETYPE    $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_GDCM        UNTAR_GDCM        BUILD_GDCM        ARCHIVE_GDCM        ZIP_GDCM        $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_VTK         UNTAR_VTK         BUILD_VTK         ARCHIVE_VTK         ZIP_VTK         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_ITK         UNTAR_ITK         BUILD_ITK         ARCHIVE_ITK         ZIP_ITK         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_OPENCASCADE UNTAR_OPENCASCADE BUILD_OPENCASCADE ARCHIVE_OPENCASCADE ZIP_OPENCASCADE $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_MMG         UNTAR_MMG         BUILD_MMG         ARCHIVE_MMG         ZIP_MMG         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_MITK        UNTAR_MITK        BUILD_MITK        ARCHIVE_MITK        ZIP_MITK        $SV_SUPER_OPTIONS"
   export SV_SUPER_OPTIONS
fi

#
# wget all source code
#

source Scripts/build-sv-externals-helper-wget-generic.sh

#
# unpack all of the source code
#

source Scripts/untar-unzip-source-all.sh

#
# make build scripts
#

#  tcl/tk 8.6
if [[ $SV_SUPER_OPTIONS == *BUILD_TCL* ]]; then
  echo "CREATE_BUILD_SCRIPT_TCL"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/tcl-windows-generic.sh > tmp/compile.make.tcl.cl.sh
  chmod a+rx ./tmp/compile.make.tcl.cl.sh
fi

## python 2.7
if [[ $SV_SUPER_OPTIONS == *BUILD_PYTHON* ]]; then
  echo "CREATE_BUILD_SCRIPT_PYTHON"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-python-windows.sh > tmp/compile.cmake.python.cl.sh
  chmod a+rx ./tmp/compile.cmake.python.cl.sh
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *BUILD_NUMPY* ]]; then
  echo "CREATE_BUILD_SCRIPT_NUMPY"
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-python-numpy-windows.sh > tmp/compile.msvc.numpy.cl.sh
  chmod a+rx ./tmp/compile.msvc.numpy.cl.sh
  sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-python-numpy-msvc.bat > tmp/compile.msvc.numpy.bat
  chmod a+rx ./tmp/compile.msvc.numpy.bat
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

#
# compile code
#

#  tcl/tk 8.6
if [[ $SV_SUPER_OPTIONS == *BUILD_TCL* ]]; then
  echo "BUILD_TCL"
  ./tmp/compile.make.tcl.cl.sh >& ./tmp/stdout.tcl.txt
fi

## python 2.7
if [[ $SV_SUPER_OPTIONS == *BUILD_PYTHON* ]]; then
  echo "BUILD_PYTHON"
  ./tmp/compile.cmake.python.cl.sh >& ./tmp/stdout.python.cl.txt
fi

# numpy
if [[ $SV_SUPER_OPTIONS == *BUILD_NUMPY* ]]; then
  echo "BUILD_NUMPY"
  ./tmp/compile.msvc.numpy.cl.sh >& ./tmp/stdout.msvc.numpy.cl.txt
fi

# freetype
if [[ $SV_SUPER_OPTIONS == *BUILD_FREETYPE* ]]; then
  echo "BUILD_FREETYPE"
  ./tmp/compile.cmake.freetype.cl.sh >& ./tmp/stdout.freetype.cl.txt
fi

# gdcm
if [[ $SV_SUPER_OPTIONS == *BUILD_GDCM* ]]; then
  echo "BUILD_GDCM"
  ./tmp/compile.cmake.gdcm.cl.sh >& ./tmp/stdout.gdcm.cl.txt
fi

# vtk
if [[ $SV_SUPER_OPTIONS == *BUILD_VTK* ]]; then
  echo "BUILD_VTK"
  ./tmp/compile.cmake.vtk.cl.sh >& ./tmp/stdout.vtk.cl.txt
fi

# itk
if [[ $SV_SUPER_OPTIONS == *BUILD_ITK* ]]; then
  echo "BUILD_ITK"
  ./tmp/compile.cmake.itk.cl.sh >& ./tmp/stdout.itk.cl.txt
fi

# opencascade
if [[ $SV_SUPER_OPTIONS == *BUILD_OPENCASCADE* ]]; then
  echo "BUILD_OPENCASCADE"
  ./tmp/compile.cmake.opencascade.cl.sh >& ./tmp/stdout.opencascade.cl.txt
fi

# mmg
if [[ $SV_SUPER_OPTIONS == *BUILD_MMG* ]]; then
  echo "BUILD_MMG"
  ./tmp/compile.cmake.mmg.cl.sh >& ./tmp/stdout.mmg.cl.txt
fi

# mitk
if [[ $SV_SUPER_OPTIONS == *BUILD_MITK* ]]; then
  echo "BUILD_MITK"
  ./tmp/compile.cmake.mitk.cl.sh >& ./tmp/stdout.mitk.cl.txt
  ./tmp/post-install-mitk-windows.sh >& ./tmp/stdout.post-install-windows.mitk.txt
fi

#
# create tar files for distrution
#

source ./tmp/create-archives-windows.cl.sh >& ./tmp/stdout.create-archives-windows.cl.txt

source ./tmp/tar-to-zip-all.windows.cl.sh >& ./tmp/stdout.tar-to-zip-all.windows.cl.txt

