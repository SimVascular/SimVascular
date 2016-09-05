#
# unpack all of the source code
#

source Scripts/untar-unzip-source-all.sh
mkdir -p tmp

#
# make build scripts
#


#  tcl/tk 8.6
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/tcl-linux-generic.sh > tmp/compile.make.tcl.gcc.sh
chmod a+rx ./tmp/compile.make.tcl.gcc.sh

# python 2.7
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/compile-cmake-python-linux.sh > tmp/compile.cmake.python.gcc.sh
chmod a+rx ./tmp/compile.cmake.python.gcc.sh

# numpy
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/compile-python-numpy-linux.sh > tmp/compile.python.numpy-linux.sh
chmod a+rx ./tmp/compile.python.numpy-linux.sh

# freetype
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/compile-cmake-freetype-generic.sh > tmp/compile.cmake.freetype.gcc.sh
chmod a+rx ./tmp/compile.cmake.freetype.gcc.sh

# gdcm
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/compile-cmake-gdcm-generic.sh > tmp/compile.cmake.gdcm.gcc.sh
chmod a+rx ./tmp/compile.cmake.gdcm.gcc.sh

# vtk
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/compile-cmake-vtk-generic.sh > tmp/compile.cmake.vtk.gcc.sh
chmod a+rx ./tmp/compile.cmake.vtk.gcc.sh

# itk
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/compile-cmake-itk-generic.sh > tmp/compile.cmake.itk.gcc.sh
chmod a+rx ./tmp/compile.cmake.itk.gcc.sh

# opencascade
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/compile-cmake-opencascade-generic.sh > tmp/compile.cmake.opencascade.gcc.sh
chmod a+rx ./tmp/compile.cmake.opencascade.gcc.sh

# mmg
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/compile-cmake-mmg-generic.sh > tmp/compile.cmake.mmg.gcc.sh
chmod a+rx ./tmp/compile.cmake.mmg.gcc.sh

# mitk
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/compile-cmake-mitk-linux.sh > tmp/compile.cmake.mitk.gcc.sh
chmod a+rx ./tmp/compile.cmake.mitk.gcc.sh
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh CompileScripts/post-install-mitk-linux.sh > tmp/post-install-mitk-linux.sh
chmod a+rx ./tmp/post-install-mitk-linux.sh

# create script to create tar files
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh Scripts/create-archives-all.sh > tmp/create-archives-all.gcc.sh
chmod a+rx ./tmp/create-archives-all.gcc.sh

# create script to create zip files
sed -f CompileScripts/sed-script-x64_linux-options-gcc.sh Scripts/tar-to-zip-all.sh > tmp/tar-to-zip-all.gcc.sh
chmod a+rx ./tmp/tar-to-zip-all.gcc.sh

#
# compile code
#

#  tcl/tk 8.6
./tmp/compile.make.tcl.gcc.sh >& ./tmp/stdout.tcl.txt

# python 2.7
./tmp/compile.cmake.python.gcc.sh >& ./tmp/stdout.python.gcc.txt

# numpy
./tmp/compile.python.numpy-linux.sh >& ./tmp/stdout.numpy.python.txt

# freetype
./tmp/compile.cmake.freetype.gcc.sh >& ./tmp/stdout.freetype.gcc.txt

# gdcm
./tmp/compile.cmake.gdcm.gcc.sh >& ./tmp/stdout.gdcm.gcc.txt

# vtk
./tmp/compile.cmake.vtk.gcc.sh >& ./tmp/stdout.vtk.gcc.txt

# itk
./tmp/compile.cmake.itk.gcc.sh >& ./tmp/stdout.itk.gcc.txt

# opencascade
./tmp/compile.cmake.opencascade.gcc.sh >& ./tmp/stdout.opencascade.gcc.txt

# mmg
./tmp/compile.cmake.mmg.gcc.sh >& ./tmp/stdout.mmg.gcc.txt

# mitk
./tmp/compile.cmake.mitk.gcc.sh >& ./tmp/stdout.mitk.gcc.txt
./tmp/post-install-mitk-linux.sh >& ./tmp/stdout.post-install-mitk-linux.txt

#
# create tar files for distrution
#

./tmp/create-archives-all.gcc.sh >& ./tmp/stdout.create-archives-all.gcc.txt

./tmp/tar-to-zip-all.gcc.sh >& ./tmp/stdout.tar-to-zip-all.gcc.txt
