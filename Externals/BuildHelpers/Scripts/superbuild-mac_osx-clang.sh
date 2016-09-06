#
# unpack all of the source code
#

source Scripts/untar-unzip-source-all.sh
mkdir -p tmp

#
# make build scripts
#


#  tcl/tk 8.6
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/tcl-mac_osx-generic.sh > tmp/compile.make.tcl.clang.sh
chmod a+rx ./tmp/compile.make.tcl.clang.sh

# python 2.7
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/compile-cmake-python-mac_osx.sh > tmp/compile.cmake.python.clang.sh
chmod a+rx ./tmp/compile.cmake.python.clang.sh

# numpy
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/compile-python-numpy-mac_osx.sh > tmp/compile.python.numpy-mac_osx.sh
chmod a+rx ./tmp/compile.python.numpy-mac_osx.sh

# freetype
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/compile-cmake-freetype-generic.sh > tmp/compile.cmake.freetype.clang.sh
chmod a+rx ./tmp/compile.cmake.freetype.clang.sh

# gdcm
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/compile-cmake-gdcm-generic.sh > tmp/compile.cmake.gdcm.clang.sh
chmod a+rx ./tmp/compile.cmake.gdcm.clang.sh

# vtk
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/compile-cmake-vtk-generic.sh > tmp/compile.cmake.vtk.clang.sh
chmod a+rx ./tmp/compile.cmake.vtk.clang.sh

# itk
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/compile-cmake-itk-generic.sh > tmp/compile.cmake.itk.clang.sh
chmod a+rx ./tmp/compile.cmake.itk.clang.sh

# opencascade
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/compile-cmake-opencascade-generic.sh > tmp/compile.cmake.opencascade.clang.sh
chmod a+rx ./tmp/compile.cmake.opencascade.clang.sh

# mmg
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/compile-cmake-mmg-generic.sh > tmp/compile.cmake.mmg.clang.sh
chmod a+rx ./tmp/compile.cmake.mmg.clang.sh

# mitk
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/compile-cmake-mitk-generic.sh > tmp/compile.cmake.mitk.clang.sh
chmod a+rx ./tmp/compile.cmake.mitk.clang.sh
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh CompileScripts/post-install-mitk-mac_osx.sh > tmp/post-install-mitk-mac_osx.sh
chmod a+rx ./tmp/post-install-mitk-mac_osx.sh

# create script to create tar files
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh Scripts/create-archives-mac_osx.sh > tmp/create-archives-mac_osx.clang.sh
chmod a+rx ./tmp/create-archives-mac_osx.clang.sh

# create script to create zip files
sed -f CompileScripts/sed-script-x64_mac_osx-options-clang.sh Scripts/tar-to-zip-all.sh > tmp/tar-to-zip-all.clang.sh
chmod a+rx ./tmp/tar-to-zip-all.clang.sh

#
# compile code
#

#  tcl/tk 8.6
./tmp/compile.make.tcl.clang.sh >& ./tmp/stdout.tcl.txt

# python 2.7
./tmp/compile.cmake.python.clang.sh >& ./tmp/stdout.python.clang.txt

# numpy
./tmp/compile.python.numpy-mac_osx.sh >& ./tmp/stdout.numpy.python.txt

# mmg
./tmp/compile.cmake.mmg.clang.sh >& ./tmp/stdout.mmg.clang.txt

# freetype
./tmp/compile.cmake.freetype.clang.sh >& ./tmp/stdout.freetype.clang.txt

# gdcm
./tmp/compile.cmake.gdcm.clang.sh >& ./tmp/stdout.gdcm.clang.txt

# vtk
./tmp/compile.cmake.vtk.clang.sh >& ./tmp/stdout.vtk.clang.txt

# itk
./tmp/compile.cmake.itk.clang.sh >& ./tmp/stdout.itk.clang.txt

# opencascade
./tmp/compile.cmake.opencascade.clang.sh >& ./tmp/stdout.opencascade.clang.txt

# mitk
./tmp/compile.cmake.mitk.clang.sh >& ./tmp/stdout.mitk.clang.txt
./tmp/post-install-mitk-mac_osx.sh >& ./tmp/stdout.post-install-mac_osx.mitk.txt

#
# create tar files for distrution
#

./tmp/create-archives-mac_osx.clang.sh >& ./tmp/stdout.create-archives-mac_osx.clang.txt

./tmp/tar-to-zip-all.clang.sh >& ./tmp/stdout.tar-to-zip-all.clang.txt
