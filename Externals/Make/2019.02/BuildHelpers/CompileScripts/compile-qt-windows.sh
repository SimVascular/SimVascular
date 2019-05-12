rm -Rf REPLACEME_SV_TOP_BLD_DIR_QT
cp -Rf REPLACEME_SV_TOP_SRC_DIR_QT REPLACEME_SV_TOP_BLD_DIR_QT
cp tmp/compile.qt.msvc.env.bat REPLACEME_SV_TOP_BLD_DIR_QT
cp tmp/compile.qt.msvc.configure.bat REPLACEME_SV_TOP_BLD_DIR_QT
cp tmp/compile.qt.msvc.nmake.bat REPLACEME_SV_TOP_BLD_DIR_QT
pushd REPLACEME_SV_TOP_BLD_DIR_QT
touch qtbase/.gitignore
cmd /C compile.qt.msvc.configure.bat
cmd /C compile.qt.msvc.nmake.bat
popd
