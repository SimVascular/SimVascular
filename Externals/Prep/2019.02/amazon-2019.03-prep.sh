# generic
sudo yum -y update

# 2019.03
sudo yum -y install gcc
sudo yum -y install gcc-c++

# needed for some makefile stuff
sudo yum -y install tcl

### to link against qt pre-built
sudo yum -y install libGLEW
sudo yum -y install glew
sudo yum -y install glew-devel

# for qt
sudo yum -y install ruby
sudo yum -y install gperf
sudo yum -y install icu
sudo yum -y install libicu-devel
sudo yum -y install libxcb
sudo yum -y install libxcb-devel
sudo yum -y install xcb-util
sudo yum -y install xcb-util-devel

# for mitk
sudo yum -y install tcp_wrappers-devel
sudo yum -y install tcp_wrappers-libs
sudo yum -y install libtiff-devel

# for swig-3.0.12
sudo yum -y install pcre-devel

#some helpers
#sudo yum -y install git
sudo yum -y install emacs
sudo yum -y install dos2unix
#sudo yum -y install xterm
sudo apt-get -y install bbe

### used by some of the SV cmake code
sudo yum -y install system-lsb-core

### cmake tools  (note: we need newer version of cmake installed below!)
#sudo yum -y install cmake

### for flowsolver
#sudo yum -y install libmpich2-dev

### for vtk
sudo yum -y install libGLU-devel
sudo yum -y install libXt-devel
sudo yum -y install libXi-devel

### for qt/mitk
 sudo yum -y install libicu-devel

### mitk
sudo yum -y install libXmu-devel
sudo yum -y install libXi-devel

### python
sudo yum -y install openssl-devel
sudo yum -y install sqlite-devel
sudo yum -y install ncurses-devel

### gdcm/mitk
#sudo yum -y install swig3.0

# optional: mitk
sudo yum -y install doxygen

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

### install Qt
#echo "Must install Qt into /opt/Qt5.6.3!"
#echo "Must instal Qt + WebView + WebEngine + Qt Scripts!"
#wget http://simvascular.stanford.edu/downloads/public/open_source/linux/qt/5.6/qt-opensource-linux-x64-5.6.3.run
#chmod a+rx ./qt-opensource-linux-x64-5.6.3.run
#sudo ./qt-opensource-linux-x64-5.6.3.run

