#/bin/bash  -f

sudo rm -Rf REPLACEME_SV_TOP_BIN_DIR_QT
mkdir -p REPLACEME_SV_TOP_BIN_DIR_QT
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_QT

tar xzf ./Originals/qt/qt-opensource-mac_osx-x64-REPLACEME_SV_QT_VERSION.tar.gz
mv REPLACEME_SV_QT_VERSION REPLACEME_SV_TOP_BIN_DIR_QT
