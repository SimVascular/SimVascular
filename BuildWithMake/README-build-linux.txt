------------------------------------------------------------------------
            Compiling Instructions for SimVascular on Linux
                       Revised 2017-11-14
------------------------------------------------------------------------

--------
Overview
--------

By default, SimVascular is configured to build on Windows using
makefiles.  You must override the deafult options to build on linux.

Our base test configuration for linux is:

Ubuntu 14.04 64-bit desktop (w/ patches)
Intel 7/9 processor
gcc/g++ version 4.8.4

-----------
Major Steps
-----------

1. Ubuntu prerequisities
------------------------

See:

Externals/ubuntu-14-prep.sh

for system packages that required to build simvascular.

2.  Install of build external open source packages
--------------------------------------------------

For downloading pre-built binaries see the quick-build-linux.sh script:

% ./quick-build-ubuntu-14.sh

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
