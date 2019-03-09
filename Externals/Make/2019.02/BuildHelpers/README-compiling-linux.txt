#-----------------------------------------------------
#      Compiling Instructions for SV16 on Linux
#                       Revised 2016-09-02
#-----------------------------------------------------

#--------
#Overview
#--------

#Building SimVascular Externals

#---------------
#Software Needed
#---------------

# These scripts are tested on Ubuntu 14.04.  You need to have the
# following packages installed:
#
# % sudo apt-get install libXmu-dev     (mitk)
# % sudo apt-get install libXi-dev      (mitk)
# % sudo apt-get install libtiff4-dev   (mitk)
# % sudo apt-get install libwrap0-dev   (mitk)
# % sudo apt-get install libssl-dev     (python)
# % sudo apt-get install swig3.0        (gdcm, mitk)
#
# optional:
#
# % sudo apt-get install doxygen        (mitk)
#
# You need at least CMake 3.2.  This is not provided via
# apt-get on Ubuntu 14.04, so you should download and install
# from cmake.org.
#
# Scripts are set up for the following default path
# where you prepend "/usr/local/package" to the
# default installation paths, i.e.:
#
# /usr/local/package/cmake-3.6.1-Linux-x86_64
#
# % cd /usr/local/bin
# % sudo ln -s ../package/cmake-3.6.1-Linux-x86_64/bin/ccmake .
# % sudo ln -s ../package/cmake-3.6.1-Linux-x86_64/bin/cmake .
# % sudo ln -s ../package/cmake-3.6.1-Linux-x86_64/bin/cmake-gui .
# % sudo ln -s ../package/cmake-3.6.1-Linux-x86_64/bin/cpack .
# % sudo ln -s ../package/cmake-3.6.1-Linux-x86_64/bin/ctest .

#
# You need to install Qt 5.4 from the Qt company installers.
#
# Qt: /opt/Qt5.4.2
#

#  NOTE: the installers for tcllib and tklib are interactive.
#        simply click "install" button when the windows pop up.

cd /usr/local/sv/ext/src/BuildHelpers/
Scripts/superbuild-linux-gcc.sh
