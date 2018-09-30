#/bin/bash  -f

sudo rm -Rf REPLACEME_SV_TOP_BIN_DIR_QT
mkdir -p REPLACEME_SV_TOP_BIN_DIR_QT
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_QT

echo ""
echo "*** install in ****"
echo ""
echo "REPLACEME_SV_TOP_BIN_DIR_QT"
echo ""
echo "*******************"
echo ""

sudo ./Originals/qt/qt-opensource-linux-x64-5.6.3.run --script tmp/qt.installer-noninteractive.qs
sudo chown -R $USER REPLACEME_SV_TOP_BIN_DIR_QT
sudo chgrp -R $USER REPLACEME_SV_TOP_BIN_DIR_QT
