#/bin/bash  -f

sudo rm -Rf REPLACEME_SV_TOP_BIN_DIR_QT
mkdir -p REPLACEME_SV_TOP_BIN_DIR_QT
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_QT

tar xzf ./Originals/qt/qt-opensource-mac_osx-x64-5.6.3.tar.gz
mv 5.6.3 REPLACEME_SV_TOP_BIN_DIR_QT
