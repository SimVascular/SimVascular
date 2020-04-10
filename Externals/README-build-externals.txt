****************************************************************
                      Building Externals
                          2019-06-14
****************************************************************

--> The current externals build is "2019.06"  <--

Notes:

1. It may be possible to build externals on some platforms
   using CMake, but the "make" scripts are the preferred method.
   The CMake build hasn't been tested since 2018.05 externals.

2. Windows can only be built using Cygwin and make

3. The make scripts require sudo, which can timeout on some
   linux installations if you require a password for sudo.

   Try:

   % sudo visudo

   and either set:

   Defaults    timestamp_timeout=-1

   or on centos do something like:
  
   ## Same thing without a password
   %wheel	ALL=(ALL)	NOPASSWD: ALL

   or on ubuntu something like (where USER is your desired account):

   $USER ALL=NOPASSWD: ALL

4. You must prep your system before building externals.  Run the
   appropriate script, e.g.:

   % cd Prep/2019.06
   % ./centos-7-prep.sh

   you only need to run prep once even if you rebuild multiple
   times (as long as the script doesn't get updated!).

5. On CentOS 7, you must also use the updated toolchain:

   scl enable devtoolset-6 bash
   scl enable rh-git29 bash
   scl enable rh-ruby23 bash

%cat /etc/profile.d/enabledevtoolset-6.sh 
#!/bin/bash
source scl_source enable devtoolset-6

%cat /etc/profile.d/enablerh-git29.sh 
#!/bin/bash
source scl_source enable rh-git29

%cat /etc/profile.d/enablerh-ruby23.sh 
#!/bin/bash
source scl_source enable rh-ruby23
 
   CentOS may require you to specify:

   % xhost +

   and disable internet connectivity to run the Qt installer,
   but you need internet access to build MITK

6. Build them all, e.g.:

   % cd Make/2019.06
   % source ./build-sv-externals-linux.sh

7. Check out ~/.bashrc and make sure that you don't:

   * alias cp, rm, etc. to require interactive user response
   * /usr/local/bin must precede /usr/bin in your path

8. Running Centos in virtualbox requires:

   export MESA_GL_VERSION_OVERRIDE=3.2

9. Building Qt from source doesn't work!  You must use
   the binary download.
