------------------------------------------------------------------------
            Compiling Instructions for SimVascular on Linux
                       Revised 2016-11-29
------------------------------------------------------------------------

--------
Overview
--------

By default, SimVascular is configured to build on Windows using
makefiles.  You must override the deafult options to build on linux.

Our base test configuration for linux is:

Ubuntu 14.04 64-bit desktop (w/ patches)
Intel 7 processor
gcc/g++/gfortran version 4.8.4

-----------
Major Steps
-----------

1. Ubuntu prerequisities
------------------------
The following packages are required to build simvascular

### compilers
% sudo apt-get install g++
% sudo apt-get install gfortran

### cmake tools
% sudo apt-get install cmake
% sudo apt-get install cmake-qt-gui

### for flowsolver
% sudo apt-get install libmpich2-dev

### for vtk
% sudo apt-get install libglu1-mesa-dev
% sudo apt-get install libxt-dev

### used by SV to read DICOM headers
% sudo apt-get install dcmtk
% sudo apt-get install libgdcm-tools


#
# default subversion for (ubuntu 14.04) is 1.8.8
#

% sudo apt-get install subversion
% sudo apt-get install git

# python
% sudo apt-get install python2.7-dev
% sudo apt-get install python-numpy
% sudo apt-get install python-scipy

#
# install qt
#

% sudo ./qt-opensource-linux-x64-5.4.2.run

leads to /opt/Qt5.4.2

% sudo apt-get install libicu-dev

#
# some optional helpers
#
% sudo apt-get install dos2unix
% sudo apt-get install emacs

2.  Install of build external open source packages
--------------------------------------------------

For downloading pre-built binaries see the quick-build-linux.sh script:

% ./quick-build-linux.sh

To build your own version:

% cd ../Externals
% source build-sv-exeternals-linux.sh

3. Checkout SV source code
--------------------------
% git clone https://github.com/SimVascular/SimVascular.git simvascular

4. Override options
-------------------
Override defaults with:

  * cluster_overrides.mk
  * global_overrides.mk
  * site_overrides.mk
  * pk_overrides.mk

See include.mk for all options.

5. Copy meshsim license file
----------------------------
If you are building with MeshSim, copy the license file
into the appropriate directory.
% cp license.dat simvascular/Licenses/MeshSim/license.dat

6. Build
--------
% cd simvascular/BuildWithMake
% make

7. Running developer version
----------------------------
% cd simvascular/BuildWithMake
% ./sv

8. Build release
-----------------
% cd simvascular/BuildWithMake/Release
% make

9. Installing a distribution
----------------------------
sudo tar --directory /usr/local -xvzpsf simvascular-linux-x64.*.tar.gz
sudo /usr/local/package/simvascular/xxxxxxxx/post-install.sh
sudo /usr/local/package/simvascular/xxxxxxxx/post-solver-install.sh

% sudo apt-get install gcc-multilib
% sudo apt-get install ia32-libs

10. Optional
------------

#
#  if you have a current nvidia graphics card
#

% sudo apt-get install nvidia-current
