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

osid=$(lsb_release -si)
case "$osid" in

  'Ubuntu')
     sudo nmcli networking off
     sudo ./Originals/qt/qt-opensource-linux-x64-REPLACEME_SV_QT_VERSION.run --script tmp/qt.installer-noninteractive.qs
     sudo nmcli networking on
     ;;

  'CentOS')
     sudo ./Originals/qt/qt-opensource-linux-x64-REPLACEME_SV_QT_VERSION.run --script tmp/qt.installer-noninteractive.qs
     ;;

  'AmazonAMI')
     tar xzf ./Originals/qt/qt-opensource-centos-x64-REPLACEME_SV_QT_VERSION.tar.gz
     sudo rm -Rf REPLACEME_SV_TOP_BIN_DIR_QT
     mv qt-REPLACEME_SV_QT_VERSION REPLACEME_SV_TOP_BIN_DIR_QT 
     ;;

  'Amazon')
     tar xzf ./Originals/qt/qt-opensource-centos-x64-REPLACEME_SV_QT_VERSION.tar.gz
     sudo rm -Rf REPLACEME_SV_TOP_BIN_DIR_QT
     mv qt-REPLACEME_SV_QT_VERSION REPLACEME_SV_TOP_BIN_DIR_QT  
     ;;

  *)	 
    echo "Error!  Invalid Linux Version!"
    exit
    ;;
esac

sudo chown -R $USER REPLACEME_SV_TOP_BIN_DIR_QT
sudo chgrp -R $USER REPLACEME_SV_TOP_BIN_DIR_QT
