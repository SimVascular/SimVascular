# generic
sudo apt-get update

#some helpers
sudo apt-get install git
sudo apt-get install emacs
sudo apt-get install dos2unix

### compilers
sudo apt-get install g++
sudo apt-get install gfortran

### cmake tools  (note: we need newer version of cmake installed below!)
#sudo apt-get install cmake
#sudo apt-get install cmake-qt-gui

### for flowsolver
sudo apt-get install libmpich2-dev

### for vtk
sudo apt-get install libglu1-mesa-dev
sudo apt-get install libxt-dev
sudo apt-get install libxi-dev

### for qt/mitk
 sudo apt-get install libicu-dev

### mitk
sudo apt-get install libXmu-dev
sudo apt-get install libXi-dev
sudo apt-get install libtiff4-dev
sudo apt-get install libwrap0-dev

### python
sudo apt-get install libssl-dev

### gdcm/mitk
sudo apt-get install swig3.0

# optional: mitk
sudo apt-get install doxygen

### install Qt
wget http://simvascular.stanford.edu/downloads/public/open_source/linux/qt/5.4/qt-opensource-linux-x64-5.4.2.run
chmod a+rx ./qt-opensource-linux-x64-5.4.2.run
sudo ./qt-opensource-linux-x64-5.4.2.run

### install latest version of CMake
wget http://simvascular.stanford.edu/downloads/public/open_source/linux/cmake/cmake-3.6.1-Linux-x86_64.sh
chmod a+rx ./cmake-3.6.1-Linux-x86_64.sh
sudo mkdir -p /usr/local/package
sudo ./cmake-3.6.1-Linux-x86_64.sh --prefix=/usr/local/package
sudo ln -s /usr/local/package/cmake-3.6.1-Linux-x86_64/bin/ccmake    /usr/local/bin/ccmake
sudo ln -s /usr/local/package/cmake-3.6.1-Linux-x86_64/bin/cmake     /usr/local/bin/cmake
sudo ln -s /usr/local/package/cmake-3.6.1-Linux-x86_64/bin/cmake-gui /usr/local/bin/cmake-gui
sudo ln -s /usr/local/package/cmake-3.6.1-Linux-x86_64/bin/cpack     /usr/local/bin/cpack
sudo ln -s /usr/local/package/cmake-3.6.1-Linux-x86_64/bin/ctest     /usr/local/bin/ctest
