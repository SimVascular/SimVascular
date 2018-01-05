# generic
sudo yum -y update

# need Developer Toolset since gcc 4.4 is too old for Qt
sudo yum -y install centos-release-scl
sudo yum -y install devtoolset-4
sudo yum -y install rh-git29

# needed for some makefile stuff
sudo yum -y install tcl

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

scl enable devtoolset-4 bash
scl enable rh-git29 bash
scl enable rh-ruby23 bash

# for swig-3.0.12
sudo yum -y install pcre-devel

#some helpers
sudo yum -y install git
sudo yum -y install emacs
sudo yum -y install dos2unix
sudo yum -y install xterm

### used by some of the SV cmake code
sudo yum -y install lsb-core

### cmake tools  (note: we need newer version of cmake installed below!)
#sudo yum -y install cmake

### for flowsolver
sudo yum -y install libmpich2-dev

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
sudo yum -y install swig3.0

# optional: mitk
sudo yum -y install doxygen

### install latest version of CMake
wget http://simvascular.stanford.edu/downloads/public/open_source/linux/cmake/cmake-3.6.1-Linux-x86_64.sh
chmod a+rx ./cmake-3.6.1-Linux-x86_64.sh
sudo mkdir -p /usr/local/package/cmake-3.6.1
sudo ./cmake-3.6.1-Linux-x86_64.sh --prefix=/usr/local/package/cmake-3.6.1 --skip-license
sudo ln -s /usr/local/package/cmake-3.6.1/bin/ccmake    /usr/local/bin/ccmake
sudo ln -s /usr/local/package/cmake-3.6.1/bin/cmake     /usr/local/bin/cmake
sudo ln -s /usr/local/package/cmake-3.6.1/bin/cmake-gui /usr/local/bin/cmake-gui
sudo ln -s /usr/local/package/cmake-3.6.1/bin/cpack     /usr/local/bin/cpack
sudo ln -s /usr/local/package/cmake-3.6.1/bin/ctest     /usr/local/bin/ctest
