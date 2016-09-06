#
# unpack all of the source code
#

source Scripts/untar-unzip-source-all.sh
mkdir -p tmp

#
# make build scripts
#

#  tcl/tk 8.6
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/tcl-windows-generic.sh > tmp/compile.make.tcl.cl.sh
chmod a+rx ./tmp/compile.make.tcl.cl.sh

## python 2.7
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-python-windows.sh > tmp/compile.cmake.python.cl.sh
chmod a+rx ./tmp/compile.cmake.python.cl.sh

# freetype
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-freetype-generic.sh > tmp/compile.cmake.freetype.cl.sh
chmod a+rx ./tmp/compile.cmake.freetype.cl.sh

# gdcm
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-gdcm-generic.sh > tmp/compile.cmake.gdcm.cl.sh
chmod a+rx ./tmp/compile.cmake.gdcm.cl.sh

# vtk
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-vtk-generic.sh > tmp/compile.cmake.vtk.cl.sh
chmod a+rx ./tmp/compile.cmake.vtk.cl.sh

# itk
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-itk-generic.sh > tmp/compile.cmake.itk.cl.sh
chmod a+rx ./tmp/compile.cmake.itk.cl.sh

# opencascade
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-opencascade-generic.sh > tmp/compile.cmake.opencascade.cl.sh
chmod a+rx ./tmp/compile.cmake.opencascade.cl.sh

# mmg
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-mmg-generic.sh > tmp/compile.cmake.mmg.cl.sh
chmod a+rx ./tmp/compile.cmake.mmg.cl.sh

# numpy
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-python-numpy-windows.sh > tmp/compile.msvc.numpy.cl.sh
chmod a+rx ./tmp/compile.msvc.numpy.cl.sh
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-python-numpy-msvc.bat > tmp/compile.msvc.numpy.bat
chmod a+rx ./tmp/compile.msvc.numpy.bat

# mitk
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/compile-cmake-mitk-generic.sh > tmp/compile.cmake.mitk.cl.sh
chmod a+rx ./tmp/compile.cmake.mitk.cl.sh
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh CompileScripts/post-install-mitk-windows.sh > tmp/post-install-mitk-windows.sh
chmod a+rx ./tmp/post-install-mitk-windows.sh

# create script to create tar files
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh Scripts/create-archives-generic.sh > tmp/create-archives-windows.cl.sh
chmod a+rx ./tmp/create-archives-windows.cl.sh

# create script to create zip files
sed -f CompileScripts/sed-script-x64_cygwin-options-cl.sh Scripts/tar-to-zip-all.sh > tmp/tar-to-zip-all.windows.cl.sh
chmod a+rx ./tmp/tar-to-zip-all.windows.cl.sh

#
# compile code
#

#  tcl/tk 8.6
./tmp/compile.make.tcl.cl.sh >& ./tmp/stdout.tcl.txt

## python 2.7
./tmp/compile.cmake.python.cl.sh >& ./tmp/stdout.python.cl.txt

# freetype
./tmp/compile.cmake.freetype.cl.sh >& ./tmp/stdout.freetype.cl.txt

# gdcm
./tmp/compile.cmake.gdcm.cl.sh >& ./tmp/stdout.gdcm.cl.txt

# vtk
./tmp/compile.cmake.vtk.cl.sh >& ./tmp/stdout.vtk.cl.txt

# itk
./tmp/compile.cmake.itk.cl.sh >& ./tmp/stdout.itk.cl.txt

# opencascade
./tmp/compile.cmake.opencascade.cl.sh >& ./tmp/stdout.opencascade.cl.txt

# mmg
./tmp/compile.cmake.mmg.cl.sh >& ./tmp/stdout.mmg.cl.txt

# numpy
./tmp/compile.msvc.numpy.cl.sh >& ./tmp/stdout.msvc.numpy.cl.txt

# mitk
./tmp/compile.cmake.mitk.cl.sh >& ./tmp/stdout.mitk.cl.txt
./tmp/post-install-mitk-windows.sh >& ./tmp/stdout.post-install-windows.mitk.txt

#
# create tar files for distrution
#

./tmp/create-archives-windows.cl.sh >& ./tmp/stdout.create-archives-windows.cl.txt

./tmp/tar-to-zip-all.windows.cl.sh >& ./tmp/stdout.tar-to-zip-all.windows.cl.txt
