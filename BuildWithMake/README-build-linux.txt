------------------------------------------------------------------------
            Compiling Instructions for SimVascular on Linux
                       Revised 2016-05-13
------------------------------------------------------------------------

--------
Overview
--------

By default, SimVascular is configured to build on linux using
makefiles.  Our base test configuration for linux is:

Ubuntu 14.04 64-bit desktop (w/ patches)
Intel 7 processor

gcc/g++/gfortran version 4.8.4

and/or

ifort/icpc/icc intel compilers verison 13.0 (2013.1.117)

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

sudo ./qt-opensource-linux-x64-5.4.2.run

leads to /opt/Qt5.4.2

sudo apt-get install libicu-dev

#
#  choose your version of subversion (1.6, 1.7, 1.8)
#


%
#
# some optional helpers
#
% sudo apt-get install dos2unix
% sudo apt-get install emacs

2.  /sv_extern
--------------
Download necessary external packages from simtk.org:

% svn co https://simtk.org/svn/sv_private/trunk sv_private_trunk
% svn co https://simtk.org/svn/sv_thirdparty/trunk/linux sv_thirdparty_linux
% svn co https://simtk.org/svn/sv_thirdparty/trunk/src sv_thirdparty_src

untar all files into "/sv_extern", e.g. using bash:

% cd sv_thirdparty_linux
% for f in *.tar.gz; do tar -xf $f -C /;done

or using tclsh:

% cd sv_thirdparty_linux
% tclsh
tclsh% foreach fn [glob *.tar.gz] {
tclsh%   tar --directory / -xvzf $fn
tclsh% }
tclsh% exit

repeat for "sv_thirdparty_src" and "sv_private_trunk".

3. Checkout SV source code
--------------------------
% svn co https://simtk.org/svn/sv_reorg/trunk/ReleaseCandidate1 simvascular

4. Override options
-------------------
Override defaults with:

  * cluster_overrides.mk
  * global_overrides.mk
  * site_overrides.mk
  * pk_overrides.mk

Building with gnu compilers and normal /sv_extern should
build "out of the box" without required overrides.

See include.mk for all options.  The most common:

COMPILER_VERSION = intel_13.0  (build with intel instead of gcc)
EXCLUDE_ALL_BUT_THREEDSOLVER = 1 (build flow solver only)

# build with binary distribution of svls
SV_USE_BINARY_SVLS = 1
SV_USE_DUMMY_SVLS = 0

5. Copy meshsim license file
----------------------------
% cp license.dat simvascular/Licenses/MeshSim/license.dat

6. Build
--------
% cd simvascular/Code
% make

7. Running developer version
----------------------------
% cd simvascular/trunk/Code
% ./mysim

8. Build release
-----------------
% cd simvascular/trunk/Release
% make

9. Installing a distribution
----------------------------
sudo tar --directory /usr/local -xvzpsf simvascular-linux-x64.*.tar.gz
sudo /usr/local/package/simvascular/xxxxxxxx/post-install.sh
sudo /usr/local/package/simvascular/xxxxxxxx/post-solver-install.sh

% sudo apt-get install gcc-multilib
% sudo apt-get install ia32-libs

% sudo mkdir /usr/local/SV16
% wget "http://simvascular.stanford.edu/downloads/public/simvascular/externals/linux/ubuntu/14.04/latest/linux.gcc-4.8.x64.everything.tar.gz"
% sudo tar --directory /usr/local/SV16 -xvzf ./linux.gcc-4.8.x64.everything.tar.gz

% git clone https://

#
#  if you have a current nvidia graphics card
#

% sudo apt-get install nvidia-current
