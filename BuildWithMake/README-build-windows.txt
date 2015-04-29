----------------------------------------------------------
compiling instructions for SimVascular (version 4/26/2007)
----------------------------------------------------------

Note: These instructions are unnecessary if you only want to use the 
      SimVascular software and not make modifications to the C++ code. 
      If you simply want to use SimVascular, or you will only make minor 
      changes to the interface through Tcl code, please download the 
      precompiled version for end users.

      Read over this ENTIRE document before following the instructions!

--------
Overview
--------

1.  Get initial software
2.  Download buildtools
3.  Install cygwin
4.  Set up Cygwin to call the MSVC++ compiler
5.  Checkout required external open source projects
6.  Obtain commercial libraries
7.  Checkout SimVascular source code
8.  Compile code
9.  Add license files
10. Run executables
11. Known limitations and final comments

-------
Details
-------

1. To compile SimVascular, you need the following required software:

a. TortoiseSVN (www.tortoisesvn.net). Download and install this software.

   We recommend TortoiseSVN instead of using subversion within cygwin. In our
   tests, we had random errors when using subversion within cygwin.
   
b. One of the following Microsoft Visual Studio C++ Compilers:
    - Visual Studio 2003 Service Pack 1 (13.10.6030 for 80x86)
    - Visual Studio 2005 Service Pack 1 (14.00.50727.762 for 80x86)
    - Visual Studio 2005 Service Pack 1 (14.00.50727.762 for x64)

c.  If you want to compiler the 3-D flow solver, you also need the
    Intel Fortran compiler:
     -Version 9.1    Build 20060519Z Package ID: W_FC_C_9.1.025 (32-bit)


2. Download the buildtools

   NOTE:  If you have a recent complete installation of Cygwin, you may be 
          able to skip part or all of steps 2, 3, and 4.  
          Please read over the steps and see if you need to change
          your environment.  If you are uncertain, follow the instructions
          as given in each step.

   Open a Windows Explorer window, right mouse click on your desired hard
    drive target.
   Choose "SVN Checkout..."
   Enter URL of repository --> https:/simtk.org/svn/vasc_buildtools/trunk
   Checkout directory should be C:/cygwin/simtk.org/buildtools where C:/ is
   your target hard drive.
   Push the OK button.


3. Install Cygwin

   Go into C:/cygwin/simtk.org/buildtools/cygwin/install and click setup.exe.
   
    *** Note that you should install Cygwin in C:/cygwin or other
        simple path WITHOUT spaces. ***
        
   When asked "Choose a Download Source" choose "Install from Local Directory."
   Root Directory --> Use C:/cygwin and click next
   When asked "Select Local Package Directory", browse to 
     C:/cygwin/simtk.org/buildtools/cygwin/install
   When you click Next, there will be a delay while it looks at the local 
     packages.
   When asked "Select Packages" click Next.
   Wait while cygwin installs. (May take up to 30 minutes for full install.)


4. Make sure you can access the Microsoft compiler from the cygwin prompt.

    If you have installed the microsoft compilers in the default directories 
    on C:/, you may be able to use the files found in cygwin_helpers 
    in buildtools to help you run the CL compiler from the command line.  
    From the cygwin prompt:

    % cd /simtk.org/buildtools/cygwin_helpers
    % cp usr_local_etc/* /usr/local/etc
    % cp bashrc ~/.bashrc
    % cp bash_profile ~/.bash_profile

    Edit ~/.bashrc by commenting / uncommenting the line corresponding to 
    your preferred compilers (only one can be selected at a time, 
    default is VS2003 32-bit).

    Exit (close) the cygwin shell and reopen.  Now, when you type:

    % which CL
 
    and 

    % which link

    the path returned should be to the desired microsoft compiler.


5.  SimVascular requires several open source packages.  The source code 
    and precompiled binaries for Windows XP 32-bit (VS2003 and VS2005) and 
    Windows XP 64-bit (VS2005) are available for download from www.simtk.org.

    Within the Windows Explorer window, right mouse click on C:/simtk.org.
    Choose "SVN Checkout..."
    Enter URL of repository --> https://simtk.org/svn/cardio_external/trunk
    Enter Checkout directory --> C:\cygwin\simtk.org\external_open_source


6.  SimVascular requires several commercial libraries.

    **For non-Stanford users, you must contact UGS and Simmetrix to obtain
    licenses for the Parasolid (solid modeling) and MeshSim (meshing)
    components required by SimVascular.

    For users associated with the Taylor lab, the precompiled binaries for
    Windows XP 32-bit (VS2003 and VS2005) and Windows XP 64-bit (VS2005)
    are available for download from www.simtk.org.
    
    Do the following:

    Within the Windows Explorer window, right mouse click on C:/simtk.org.
    Choose "SVN Checkout..."
    Enter URL of repository --> https://simtk.org/svn/cardio_licensed/trunk
    Enter Checkout directory --> C:\cygwin\simtk.org\licensed_software

7.  Check out the SimVascular source code from simtk.org:

    Within the Windows Explorer window, right mouse click on C:/simtk.org.
    Choose "SVN Checkout..."
    Enter URL of repository --> https://simtk.org/svn/simvascular/trunk
    Enter Checkout directory --> C:\cygwin\simtk.org\simvascular

8.  Compile the code after specifying your platform and compiler by editing
    the appropriate files.  In general, you may need to modify
    simvascular/Code/include.mk or create a new file 
    simvascular/Code/global_overrides.mk that contains your installation 
    dependent path.
    
    The benefit to using global_overrides.mk when you are only changing the
    platform and top-level paths is that since global_overrides.mk is excluded
    from version control, you can easily update include.mk.

    NOTE: The default compiler is currently VS2003 in include.mk.

    VS2003:

    % mkdir -p /simtk.org
    % cd /simtk.org

    now you are ready to compile:

    % cd simvascular/Code
    % make

    VS2005 (32-bit):

    % mkdir -p /simtk.org
    % cd /simtk.org

    create a file simvascular/Code/global_overrides.mk with the following 
    four lines of content (must be unix style file with NO carriage returns!):

COMPILER_VERSION = vs8
OPEN_SOFTWARE_BINARIES_TOPLEVEL = C:/cygwin/simtk.org/external_open_source/binaries/x86-$(COMPILER_VERSION)
OPEN_SOFTWARE_SOURCES_TOPLEVEL = C:/cygwin/simtk.org/external_open_source/sources
LICENSED_SOFTWARE_TOPLEVEL = C:/cygwin/simtk.org/licensed_software

    now you are ready to compile:

    % cd simvascular/Code
    % make

    VS2005 (64-bit):

    % mkdir -p /simtk.org
    % cd /simtk.org

    create a file simvascular/Code/global_overrides.mk with the following 
    five lines of content (must be unix style file with NO carriage returns!):

COMPILER_VERSION = vs8
CLUSTER = x64_cygwin
OPEN_SOFTWARE_BINARIES_TOPLEVEL = C:/cygwin/simtk.org/external_open_source/binaries/x64
OPEN_SOFTWARE_SOURCES_TOPLEVEL = C:/cygwin/simtk.org/external_open_source/sources
LICENSED_SOFTWARE_TOPLEVEL = C:/cygwin/simtk.org/licensed_software

    now you are ready to compile:

    % cd simvascular/Code
    % make

9.  Add license files

    You need to add license files for MeshSim and LesLib before you can
    run your executables.  Follow the instructions found in README's under
    the "Licenses" subdirectories.
 
10. Running your new executables:

    Scripts were automatically generated by thge build process to run 
    simvascular, solver, and adaptor.  If these are in your PATH, they
    will be accessible from the Cygwin prompt by typing the appropriate name.
    One possibility is to copy the scripts to "/usr/local/bin":

    % cp mysim myadaptor mysolver /usr/local/bin

    Now from the command prompt in any directory you can type:

    % mysim

    To run your copy of SimVascular.  To run the 3-D solver from the command
    line, do the following:

    % cd <some test directory containing solver.inp geombc.dat restart.dat.1>
    % solver

11. Known limitations and final comments:

    NOTE:  Currently only VS2003 32-bit & VS2005 64-bit libraries are 
           complete for external_open_source in Alpha release.

    NOTE:  Currently only the solver can be compiled on IA64 linux (polonius).
           You must specify the following line:
  
           EXCLUDE_ALL_BUT_THREEDSOLVER=1

           in Code/global_overrides.mk if you want to build on ia64_linux
           (i.e. Polonius).

    NOTE:  Currently on x86, LesLib requires the /MT flag when building
           the solver.  This restriction will be removed in the future,
           but the Makefiles currently handle this automatically.

    NOTE:  Although it has only undergone limited testing, you can
           build an optimized version of simvascular by issuing:
 
           % make MAKE_OPTIMIZED=1

           above.

    NOTE:  You must have a valid leslib license for your computer to 
           run the solver.

    NOTE:  The automagically generated wrapper script "solver" for x86
           assumes you only want to run locally one processor.  You must
           modify this script by hand if you want to use more processors.
