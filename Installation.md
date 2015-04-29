----------------------------------------------------------------------------
#            Compiling Instructions for SimVascular with CMake
######                     Revised 2014-10-12
----------------------------------------------------------------------------
##
----------------------------------------------------------------------------
#							Overview
----------------------------------------------------------------------------
Put some general good test stuff here.

----------------------------------------------------------------------------
#				Tested Configurations and System Requirements
----------------------------------------------------------------------------

#### Linux
- Ubuntu 13.04 64-bit desktop (w/ patches)
- Intel processor
- C++ (required) fortran (optional) compilers


Compilers currently supported:

- gcc/g++/gfortran version 4.6.3 (others will most likely work too)
- ifort/icpc/icc intel compilers verison 13.0 (2013.1.117)
    
#### Microsoft Windows
- Windows 7 and Windows 8
- C++ (required) fortran (optional) compilers


Compiler environments currently supported:

- Visual Studio 2010 Service Pack 1 (for x64)
- Intel Visual Fortran Fortran compiler (ifort)

*We currently **only support MS Visual Stuidio 2010**, later versions are **not
** supported at this time. MinGW gfortran is not supported but we plan to add 
this functionality in future releases.*

#### Apple OS X
- Version 10.8.5 (Mountain Lion) and 10.9 (Mavericks)
- XCode 4.6 or 5.0 (with 4.6.3 previously installed)

*XCode's tools and libraries are required but compiling with XCode is currently 
**unsupported.***


Compilers currently supported:

- i686-apple-darwin11-gcc-4.2.1 (GCC) 4.2.1 (Apple Inc. build 5666)
- MacPorts with gcc (4.6.4)

Use `gcc --version` to see which compiler you are using.
    
*Clang 5.0+ is*  **not** *supported*


----------------------------------------------------------------------------
# 							Building SimVascular
----------------------------------------------------------------------------

This readme gives an overview of compiling SimVascular using CMake.  We also 
support standard MakeFiles.  For details on using MakeFiles see .



## What libraries you need to build SimVascular
----------------------------------------------------------------------------
#### Linux
	
You will need the following packages, available from the APT repository:

```bash
sudo apt-get cmake-curses-qui
sudo apt-get cmake-gui

### Build Tools (Fortran is optional)
sudo apt-get install gcc-multilib build-essential g++ gfortran
	
### Tcl/Tk
sudo apt-get install tcl8.5 tcl8.5-dev tcl8.5-lib
sudo apt-get install tk8.5 tk8.5-dev tk8.5-lib
	
### For flowsolver
sudo apt-get install libmpich2-dev
	
### For VTK   
sudo apt-get install libglu1-mesa-dev libxt-dev libgl1-mesa-dev

##Optional (plugin libraries)
sudo apt-get install glib2.0-dev
```


#### Apple OS X
To install CMake on OS X visit http://www.cmake.org/ for details

If you have clang 5.0, you will also need to install MacPorts. MacPorts can be
downloaded at: https://www.macports.org/. The following libraries and tool
should be installed using MacPorts:

```bash
	### Build Tools 
	sudo port install gcc46
	sudo port install mpich-gcc46

	### For flowsolver only (Optional)
	port install mpich

	## For Plugins (Optional)
	sudo port install glib2-devel
	sudo port install pkgconfig
```

#### Windows
You need to install: 
 * CMake, Visit http://www.cmake.org/ for details
 * Microsoft Visual Studio 2010 Win (with x64 compilers)
 * Intel Visual Fortran installed. 
 * MPICH2, available at: mpich2-1.4.1p1-win-x86-64.msi


## Steps for compiling
----------------------------------------------------------------------------

By default SimVascular will build using a superbuild mode that downloads and
configures many of the libraries automatically.  If you wish to perform this
step manually, turn off SimVascular_SUPERBUILD from the CMake menu.

1. Open CMake and point the source directory to [simvascular source]/Code.  
Then set the Build directory to any location not in your simvascular source 
directory. *In source builds are not supported at this time.* For example:

````
  Source: /home/[username]/SimVascular/Code
  Build: /home/[username]/SimVascularBin.
````
**Note to Windows Users:** *There is a bug in MSVC10 that causes errors if the compile paths are too long,  It is reccomended that you place your source directoy so that the path to it is short (C:/Code/SimVascular is known to work).*

2. Select your desired options in the GUI, hit configure. Continue to hit 
configure till no options change (or in curses-gui continue till the generate 
option apears.  Once configured, hit generate.  
	

Now you can compile the project using whichever generator you specificed to 
CMake (i.e. make or MSVC build all).  Alternatively, you make use CMake to call 
the command for you.

On all systems (from the command prompt or terminal) this command is: 
`cmake --build /path/to/build-directory --config RelWithDebInfo`


## Running SimVascular
----------------------------------------------------------------------------

To open the executables simply the corresponding scripts:

|  Executable    |  Unix Script  |  Windows Script  |
| -------------- | ------------- |  --------------- |
| SimVascular    |  mysim        | mysim.bat        |
| Flow Solver    |  mysolver     | mysolver.bat     |
| Adaptor        |  myadaptor    | myadaptor.bat    |
| Post Solver    |  mypost       | mypost.bat       |
