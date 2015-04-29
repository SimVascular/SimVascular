------------------------------------------------------------------------
            Compiling Instructions for SimVascular with CMake
                       Revised 2014-2-13
------------------------------------------------------------------------

--------
Overview
--------

Test Configurations
-------------------

Linux:
Ubuntu 13.04 64-bit desktop (w/ patches)
Intel processor

GNU compilers
gcc/g++/gfortran version 4.6.3
and/or Intel Compilers
ifort/icpc/icc intel compilers version 13.0 (2013.1.117)

Windows:
Visual Studio 2010 Service Pack 1 (for x64)
If you want to compiler the 3-D flow solver, you also need the
    Intel Fortran compiler (ifort)
MinGW gfortran not supported yet. (As of Apr 4 2014)

OS X:
Version 10.8.5 (Mountain Lion) and 10.9 (Mavericks)

XCode 4.6 or 5.0 (with 4.6.3 previously installed)
i686-apple-darwin11-gcc-4.2.1 (GCC) 4.2.1 (Apple Inc. build 5666)
OR
MacPorts gcc-46 installed
gcc (MacPorts gcc46 4.6.4_3) 4.6.4

Currently, clang 5.0 is not supported (As of March 6, 2014)

-----------
Major Steps
-----------

1) Install CMake 2.8.12 or later
	***Linux***
	apt-get install cmake
	***OS X and Windows***
	Visit http://www.cmake.org/ for details

2) (Highly) Recommended: Install CMake curses gui or cmake qt gui
	***Linux*** 
	apt-get cmake-curses-qui
	apt-get cmake-gui
	***OS X and Windows***
	Visit http://www.cmake.org/ for details


3) Install required packages (you may already have a number of these)
	***Linux***	
	apt-get install build-essential
	apt-get install tcl8.5 tcl8.5-dev libtcl8.5
	apt-get install tk8.5 tk8.5-dev libtk8.5
	apt-get install g++
	apt-get install gfortran
       ### for flowsolver
	apt-get install libmpich2-dev
       ### for vtk
	apt-get install libglu1-mesa-dev	
	apt-get install libgl1-mesa-dev
	apt-get install libxt-dev
       ### Libs
	apt-get install gcc-multilib
	apt-get install build-essential
	apt-get install glib-2.0
	
	***OS X***
	If you have a clang based gcc then you'll need to install macports compilers
	Install macports from https://www.macports.org/
	sudo port install gcc46
	sudo port install mpich-gcc46
	sudo port select --set gcc mp-gcc46
	
	If you have a GCC based compilers (preferably 4.2.1)
	You'll just need MPICH
	sudo port install mpich
	sudo port select --set mpi mpich-gcc46-fortran

	Also you need to install glib
	sudo port install glib-2.0
	
	***Windows***
	You will need Microsoft Visual Studio 2010 Win x64 
	and Intel Visual Fortran installed. 
	In addition if you want to use the flowsolver you need to install 
	mpich2-1.4.1p1-win-x86-64. This package is available on our SVN.
	
	Note (Developers): are encouraged to test out multiple compilers when
	when testing.  A guide on installing and managing multiple compilers is
	at the end of this README.


4) Download necessary external packages from simtk.org:

	Licensed/private components:
	svn co https://simtk.org/svn/sv_private/trunk sv_private_trunk

	(optional) If you need/want to rebuild external packages from source:
	svn co https://simtk.org/svn/sv_private/src sv_thirdparty_src
	
	***Linux***
	svn co https://simtk.org/svn/sv_thirdparty/trunk/linux sv_thirdparty_linux

	***OS X***
	svn co https://simtk.org/svn/sv_thirdparty/trunk/macos sv_thirdparty_osx
	NOTE: You will need to build VTK from source. VTK source can be obtained online, 
	or from svn co of src tarballs via above src link. 
	
	***Windows***
	svn co https://simtk.org/svn/sv_thirdparty/trunk/ sv_thirdparty_windows

    
    4a) Extract these packages*

	* If you use the precompiled sv_extern VTK or ITK, you must untar your
	externals in "/sv_extern" directory! The build group (linux-gnu-x64, 
	linux-intel x64, etc) for VTK and ITK must match but you may use 
	different build groups for the other binaries. The other binaries 
	do not need to match. These VTK/ITK builds assume your GL libraries are
	in /usr/lib/x86_64-linux-gnu/.  You may need to create links to these 
	directories.

	If you wish to use your own build of VTK and ITK follow the settings in
	the build ITK/VTK section. OS X users will need to build their own copy
	of VTK. See instructions at the end of the readme. 

	Note for windows users:
	If you want to use the precompiled VTK and ITK libraries, you will need
	to place them at C:\Cygwin\sv_extern\... even though cygwin is not
	required.  If you compile them yourself, you may place them where ever
	you want.
		
	
5) Generate build using CMake
	5a) Open CMake and point the source directory to <simvascular source>/
	Code.  
	Then set the Build directory to any location not in your simvascular 
	source directory, i.e Source: ~/SimVasuclar/Code, Build: ~/SimVascularBin.
	In-source builds are not supported at this time.
	
	5b) Hit configure

	5c) 
	    - Set SVEXTERN_DIR to the location of your sv_extern directory.
		
	    For Windows and Linux users:
	    - If you didn't install tcl/tk use the following settings:
		TCL_INCLUDE_PATH <SVEXTERN_DIR>bin/<BUILDGROUP>/tcltk-8.5.11/include/
		TCL_LIBRARY <SVEXTERN_DIR>bin/<BUILDGROUP>/tcltk-8.5.11/lib/libtcl8.5.so
		TCL_SH <SVEXTERN_DIR>bin/<BUILDGROUP>/tcltk-8.5.11/bin/tclsh8.5
		TK_INCLUDE_PATH <SVEXTERN_DIR>bin/<BUILDGROUP>/tcltk-8.5.11/include/
		TK_LIBRARY <SVEXTERN_DIR>bin/<BUILDGROUP>/tcltk-8.5.11/lib/libtk8.5.so


		Where <SVEXTERN> is the toplevel of your sv_extern directory e.g. /sv_extern, 
		and <BUILDGROUP> is the path to your selected binary e.g. linux/intel_13.0/x64 		
		Note: You may wish to double check that are using the correct version on Tcl/tk. If
		you use our precompiled library, you must use our tcltk libraries. If you compile 
		your own libraries, make sure you use the same Tcl/Tk libraries for all parties.
	For Mac Users:
		Many of the third-party libraries binaries are not currently available.  You will
		need to build VTK and ITK yourself. And you must you the OS X versions of Tcl/Tk
		You may need to check 'Advanced' for these to appear. 

		
	5d) Select your desired options in the GUI, hit configure. Continue to hit 
	configure until no options change (or in curses-gui continue till the generate
	option appears. Once configured, hit generate.  
	

6) Compile the project using whichever generator you specified to CMake
	
	Note to Windows users: When compiling simvascular in Visual Studio, you must set the 
	Build type to 'RelWithDebInfo', or recompile VTK with different settings. (Debug, Release, 
	etc)

	
8) Enjoy!



	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			Using your own ITK/VTK Builds
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	On some systems particular versions of vtk\itk with the needed settings 
	will not build correctly with gcc4.4 or gcc4.8. It is recommended that 
	you obtain and install gcc-4.6. Here is a guide to get you started.

	You may get these from source or svn co https://simtk.org/svn/sv_private/src
	
	Compiling VTK/ITK using cmake follows the same general steps as outlined in 
	Step 5 above for compiling simvascular using cmake.

	The following settings are required when building VTK on your own:

	BUILD_SHARED_LIBS=OFF
	BUILD_TESTING=OFF
	VTK_Group_Tk=ON
	VTK_WRAP_TCL=ON

	Note: Make sure c/c++ compiler flags set appropriately during configuration. 

	Note: Different versions of VTK have slightly different CMake Variable names.
	It is important to build VTK with static libraries. You need to wrap Tcl and 
	Tk. Lastly, Rendering Tk is also important (this may be in VTK_Group_Rendering,
	depending on your version)
	We recommend using VTK 6.0.0. SimVascular does not compile using VTK 6.1.0 yet.
	
	ITK Settings  (Optional)

	ITK will need these settings if building on your own:
	BUILD_SHARED_LIBS OFF
	BUILD_TESTING OFF
	Module_ITKVTKGlue ON
	Module_ITKReview ON
	 
	We recommend using ITK 4.5.0.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				Linux: Multiple Compilers
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	Some versions of the needed software do not compile correctly using certain 
	versions of gcc/g++.  We use gcc-4.6 to compile many of the external libraries.
	It is easy to obtain gcc-4.6 without overwriting your current version using 
	update-alternatives.  Below is an example of how to install both gcc-4.6 and 
	gcc-4.8 on a ubuntu system.

	apt-get install c++-4.6 c++-4.8 g++-4.6 

	update-alternatives --remove-all gcc 
	update-alternatives --remove-all g++ 

	update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.6 10 
	update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 20 

	update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.6 10 
	update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.8 20 

	update-alternatives --install /usr/bin/cc cc /usr/bin/gcc 30 	
	update-alternatives --set cc /usr/bin/gcc 

	update-alternatives --install /usr/bin/c++ c++ /usr/bin/g++ 30 
	update-alternatives --set c++ /usr/bin/g++ 

	then use update-alternatives to switch compilers (as root): 
	update-alternatives --config gcc 
	update-alternatives --config g++

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
				OS X: Installing MacPorts, gcc46, MPI
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	
	If you get the Macports versions of GNU compilers you'll need the matching 
	MPI installation.
	
	Install macports from https://www.macports.org/
	sudo port install gcc46
	sudo port install mpich-gcc46
	sudo port select --set gcc mp-gcc46
	sudo port select --set mpi mpich-gcc46-fortran

	
	
	
	
	
	




 
