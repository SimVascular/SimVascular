#/bin/bash  -f

sudo rm -Rf REPLACEME_SV_TOP_BIN_DIR_QT
mkdir -p REPLACEME_SV_TOP_BIN_DIR_QT
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_QT

wget http://simvascular.stanford.edu/downloads/public/simvascular/externals/2019.02/linux/centos/7.6/gnu/6.3/x64/release/2019.03.16/centos.7.6.gnu.6.3.x64.release.2019.03.16.qt.5.6.3.tar.gz

tar xzf ./centos.7.6.gnu.6.3.x64.release.2019.03.16.qt.5.6.3.tar.gz

mv ./qt-5.6.3/* REPLACEME_SV_TOP_BIN_DIR_QT

sudo chown -R $USER REPLACEME_SV_TOP_BIN_DIR_QT
sudo chgrp -R $USER REPLACEME_SV_TOP_BIN_DIR_QT

