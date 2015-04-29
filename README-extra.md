
--------------------------------------------------------------------------
## Using your own ITK/VTK Builds
--------------------------------------------------------------------------

#### VTK
On some systems particular verisions of vtk\itk with the needed settings 
will not build correctly with gcc4.4 or gcc4.8. It is reccomended that 
you obtain and install gcc-4.6.  There is a helpful guide in this readme 
to get you started.

You may get these from source or use the SV_thirdparty_svn

The following settings are required when building VTK on your own:

* BUILD_SHARED_LIBS=OFF
* BUILD_TESTING=OFF
* VTK_Group_Tk=ON
* VTK_WRAP_TCL=ON

Note: Different versions of VTK have slightly different CMake Variable names.
It is important to build VTK with static libraries, You need to wrap Tcl and 
Tk. Lastly, Rendering Tk is also important (this may be in VTK_Group_Rendering,
depending on your verison)
We reccomend using VTK 6.0.0.  SimVascular does not compile using VTK 6.1.0 yet.

#### ITK (Optional) 

ITK will need these settings if building on your own:
* BUILD_SHARED_LIBS OFF
* BUILD_TESTING OFF
* ITKVTKGlue ON
* ITK_Review ON

You also must ensure that the VTK_DIR from above. We reccomend using ITK 4.5.0

--------------------------------------------------------------------------
Linux: Multiple Compilers
--------------------------------------------------------------------------

Some versions of the needed software do not compile correctly using certain 
versions of gcc/g++.  We use gcc-4.6 to compile many of the external libraries.
It is easy to obtain gcc-4.6 without overwriting your current version using 
update-alternatives.  Below is an example of how to install both gcc-4.6 and 
gcc-4.8 on a ubuntu system.

````bash
apt-get install c++-4.6 c++-4.8 

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

update-alternatives --config gcc 
update-alternatives --config g++
````


--------------------------------------------------------------------------
OS X: Installing MacPorts, gcc46, MPI
--------------------------------------------------------------------------

If you get the Macports versions of GNU compilers you'll need the matching 
MPI installation.

Install macports from https://www.macports.org/
sudo port install gcc46
sudo port install mpich-gcc46
