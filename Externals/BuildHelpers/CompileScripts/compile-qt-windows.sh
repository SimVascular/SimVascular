rm -Rf REPLACEME_SV_TOP_BLD_DIR_QT
cp -Rf REPLACEME_SV_TOP_SRC_DIR_QT REPLACEME_SV_TOP_BLD_DIR_QT
cp tmp/compile-msvc-qt.bat REPLACEME_SV_TOP_BLD_DIR_QT
pushd REPLACEME_SV_TOP_BLD_DIR_QT
touch qtbase/.gitignore
cmd /C compile-msvc-qt.bat
popd
