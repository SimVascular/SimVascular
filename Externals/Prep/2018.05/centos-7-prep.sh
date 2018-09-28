# generic
sudo yum -y update

# need Developer Toolset since gcc 4.8 is too old for Qt
sudo yum -y install centos-release-scl
sudo yum -y install devtoolset-6
sudo yum -y install rh-git29

# needed for some makefile stuff
sudo yum -y install tcl

### to link against qt pre-built
sudo yum -y install libglew
sudo yum -y install libglew-devel

# for qt
sudo yum -y install rh-ruby23
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

### used by some of the SV cmake code
sudo yum -y install lsb-core

### cmake tools  (note: we need newer version of cmake installed below!)
#sudo yum -y install cmake

### for flowsolver
#sudo yum -y install libmpich2-dev

### for vtk
sudo yum -y install libglu1-mesa-dev
sudo yum -y install libxt-dev
sudo yum -y install libxi-dev

### for qt/mitk
 sudo yum -y install libicu-dev

### mitk
sudo yum -y install libXmu-dev
sudo yum -y install libXi-dev

### python
sudo yum -y install libssl-dev

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

# do these need to be done every time? Maybe doesn't belong in prep script?
scl enable devtoolset-6 bash
scl enable rh-git29 bash
scl enable rh-ruby23 bash

