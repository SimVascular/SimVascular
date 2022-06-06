call "C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\VC\Auxiliary\Build\vcvars64.bat"

sh ./quick-build-windows.sh

echo "release-make"

cd Release

make msi-bundle