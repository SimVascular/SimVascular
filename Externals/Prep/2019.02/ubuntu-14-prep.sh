# generic
sudo apt-get -y update

#some helpers
sudo apt-get -y install git
sudo apt-get -y install emacs
sudo apt-get -y install dos2unix

# needed for some makefile stuff
sudo apt-get -y install tcl

### compilers
sudo apt-get -y install g++
sudo apt-get -y install gfortran

### used by some of the SV cmake code
sudo apt-get -y install lsb-core

### cmake tools  (note: we need newer version of cmake installed below!)
#sudo apt-get -y install cmake
#sudo apt-get -y install cmake-qt-gui

### for flowsolver
sudo apt-get -y install libmpich2-dev

### for vtk
sudo apt-get -y install libglu1-mesa-dev
sudo apt-get -y install libxt-dev
sudo apt-get -y install libxi-dev

### for qt/mitk
sudo apt-get -y install libicu-dev

### to link against qt pre-built
sudo apt-get install libglew-dev

### for qt
sudo apt-get -y install libfontconfig1-dev
sudo apt-get -y install libfreetype6-dev
sudo apt-get -y install libx11-dev
sudo apt-get -y install libxext-dev
sudo apt-get -y install libxfixes-dev
sudo apt-get -y install libxi-dev
sudo apt-get -y install libxrender-dev
sudo apt-get -y install libxcb1-dev
sudo apt-get -y install libx11-xcb-dev
sudo apt-get -y install libxcb-glx0-dev

# qt webengine
sudo apt-get -y install libegl1-mesa-dev
sudo apt-get -y install libdbus-1-dev
sudo apt-get -y install libfontconfig1-dev
sudo apt-get -y install libdrm-dev
sudo apt-get -y install libxcomposite-dev
sudo apt-get -y install libxcursor-dev
sudo apt-get -y install libxrandr-dev
sudo apt-get -y install libxi-dev
### lib xss is for xscrnsaver in pkg-config
sudo apt-get -y install libxss-dev
sudo apt-get -y install libxtst-dev
sudo apt-get -y install libpci-dev
# qt webengine required, but not listed on website
sudo apt-get -y install glib-2.0-dev
sudo apt-get -y install re2c
# http://wiki.qt.io/QtWebEngine/How_to_Try
#  libminizip-dev not found on ubuntu 14, maybe 16?
sudo apt-get -y install bison build-essential gperf flex ruby python libasound2-dev libbz2-dev libcap-dev \
libcups2-dev libdrm-dev libegl1-mesa-dev libgcrypt11-dev libnss3-dev libpci-dev libpulse-dev libudev-dev \
libxtst-dev gyp ninja-build
sudo apt-get -y install libssl-dev libxcursor-dev libxcomposite-dev libxdamage-dev libxrandr-dev \
libfontconfig1-dev libxss-dev libsrtp0-dev libwebp-dev libjsoncpp-dev libopus-dev  \
libavutil-dev libavformat-dev libavcodec-dev libevent-dev

### mitk
sudo apt-get -y install libxmu-dev
sudo apt-get -y install libxi-dev
sudo apt-get -y install libtiff4-dev
sudo apt-get -y install libwrap0-dev

### python
sudo apt-get -y install libssl-dev

# for swig-3.0.12
sudo apt-get -y install libpcre3-dev

# qt webkit build
sudo apt-get -y install ruby2.0
sudo apt-get -y install gperf
sudo apt-get -y install bison
sudo apt-get -y install flex
sudo apt-get -y install libxrender-dev


### gdcm/mitk
#sudo apt-get -y install swig3.0

# optional: mitk
sudo apt-get -y install doxygen

### install Qt
#wget http://simvascular.stanford.edu/downloads/public/open_source/linux/qt/5.4/qt-opensource-linux-x64-5.4.2.run
#chmod a+rx ./qt-opensource-linux-x64-5.4.2.run
#sudo ./qt-opensource-linux-x64-5.4.2.run --script ./ubuntu-qt-installer-noninteractive.qs

### install latest version of CMake
wget http://simvascular.stanford.edu/downloads/public/open_source/linux/cmake/cmake-3.10.3-Linux-x86_64.sh
chmod a+rx ./cmake-3.10.3-Linux-x86_64.sh
sudo mkdir -p /usr/local/package/cmake-3.10.3
sudo ./cmake-3.10.3-Linux-x86_64.sh --prefix=/usr/local/package/cmake-3.10.3 --skip-license
sudo ln -s /usr/local/package/cmake-3.10.3/bin/ccmake    /usr/local/bin/ccmake
sudo ln -s /usr/local/package/cmake-3.10.3/bin/cmake     /usr/local/bin/cmake
sudo ln -s /usr/local/package/cmake-3.10.3/bin/cmake-gui /usr/local/bin/cmake-gui
sudo ln -s /usr/local/package/cmake-3.10.3/bin/cpack     /usr/local/bin/cpack
sudo ln -s /usr/local/package/cmake-3.10.3/bin/ctest     /usr/local/bin/ctest

### MITK requires a newer version of gcc
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt-get update
sudo apt-get install gcc-4.9 g++-4.9
## Make sure to explicitly specify these compilers when configuring MITK:
#  CMAKE_C_COMPILER:FILEPATH=/usr/bin/gcc-4.9
#  CMAKE_CXX_COMPILER:FILEPATH=/usr/bin/g++-4.9

