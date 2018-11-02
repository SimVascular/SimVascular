#/bin/bash  -f

rm -Rf REPLACEME_SV_TOP_BIN_DIR_QT
mkdir -p REPLACEME_SV_TOP_BIN_DIR_QT
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_QT

tar xzf ./Originals/qt/qt-windows-release-x64-5.6.3.tar.gz
mv REPLACEME_SV_QT_DIR/* REPLACEME_SV_TOP_BIN_DIR_QT
rmdir REPLACEME_SV_QT_DIR
