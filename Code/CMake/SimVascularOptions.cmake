# Initial SV Options 
#-----------------------------------------------------------------------------
# Developer flag (Output extra info during configure)
option(SV_DEVELOPER_OUTPUT "This is a developer mode to print extra messages during configure" OFF)
mark_as_superbuild(SV_DEVELOPER_OUTPUT)

set(SV_BUILD_TYPE "CMAKE" CACHE STRING "Designate CMAKE build" FORCE)
set_property(CACHE SV_BUILD_TYPE PROPERTY STRINGS CMAKE)
mark_as_advanced(SV_BUILD_TYPE)
mark_as_superbuild(SV_BUILD_TYPE)

#-----------------------------------------------------------------------------
# Distribution
option(SV_ENABLE_DISTRIBUTION "Distribute" OFF)
mark_as_superbuild(SV_ENABLE_DISTRIBUTION)
#-----------------------------------------------------------------------------
# Superbuild Option
option(SV_SUPERBUILD
	"Build ${PROJECT_NAME} and the projects it depends on.
	This must be turn on if your wish to download any packages." OFF)
mark_as_superbuild(SV_SUPERBUILD)

set(SV_SUPERBUILD_LIBS_DIR "${svlibs_OPEN_URLBASE}" CACHE TYPE PATH)
mark_as_superbuild(SV_SUPERBUILD_LIBS_DIR)

option(BUILD_SHARED_LIBS "Build ${PROJECT_NAME} as shared libraries." OFF)
mark_as_superbuild(VARS BUILD_SHARED_LIBS ALL_PROJECTS)

set(SV_LIBRARY_TYPE "STATIC" CACHE STRING "Options are STATIC or SHARED" FORCE)
set_property(CACHE SV_LIBRARY_TYPE PROPERTY STRINGS STATIC SHARED)
mark_as_superbuild(SV_LIBRARY_TYPE)

#-----------------------------------------------------------------------------
# SimVascular Build options
option(SV_ONLY_BUILD_FLOWSOLVER "Option ONLY build the 3D Solver (No GUI)" OFF)
mark_as_superbuild(SV_ONLY_BUILD_FLOWSOLVER)

option(SV_SUPPRESS_WARNINGS "Option to suppress all compiler warnings while compiling" ON)
mark_as_superbuild(SV_SUPPRESS_WARNINGS)

#-----------------------------------------------------------------------------
# General Library Options
option(SV_USE_MPICH2 "Use MPICH2" ON)
mark_as_superbuild(SV_USE_MPICH2)

option(SV_USE_DUMMY_MPICH2 "Use Dummy MPICH2" OFF)
mark_as_superbuild(SV_USE_DUMMY_MPICH2)

option(SV_USE_MSMPI "Use MSMPI" OFF)
mark_as_superbuild(SV_USE_MSMPI)

#-----------------------------------------------------------------------------
# Externals
set(SV_EXTERNALS_TOPLEVEL_DIR "${CMAKE_BINARY_DIR}/Externals" CACHE PATH "Externals toplevel directory")
mark_as_superbuild(SV_EXTERNALS_TOPLEVEL_DIR:PATH)

option(SV_EXTERNALS_USE_TOPLEVEL_DIR "If ON, SV_EXTERNALS_TOPLEVEL_DIR will be used as location for external packages" OFF)
mark_as_superbuild(SV_EXTERNALS_USE_TOPLEVEL_DIR)

#-----------------------------------------------------------------------------
# External download options
option(SV_DOWNLOAD_EXTERNALS "Option to download all externals from a remote location" OFF)
set(SV_EXTERNALS_DOWNLOAD_URL "http://simvascular.stanford.edu/downloads/public/simvascular/externals/mac_osx/10.10/latest/mac_osx.clang-7.0.x64.everything.tar.gz")

#-----------------------------------------------------------------------------
# ThirdParty
option(SV_USE_ZLIB "Use ZLib" ON)
mark_as_superbuild(SV_USE_ZLIB)

option(SV_USE_VMTK "Enable VMTK Plugin" ON)
mark_as_superbuild(SV_USE_VMTK)

option(SV_USE_TETGEN "Enable Tetgen Meshing Plugin" ON)
mark_as_superbuild(SV_USE_TETGEN)

option(SV_USE_PYTHON "Use Tcl Python" OFF)
mark_as_superbuild(SV_USE_PYTHON)

#-----------------------------------------------------------------------------
# Adaptor Options
# Adaptor converts objects between the different solid models.
option(SV_USE_MESHSIM_ADAPTOR "Build the adapter (Requires Fortran and MeshSim)" OFF)
mark_as_superbuild(SV_USE_MESHSIM_ADAPTOR)

#-----------------------------------------------------------------------------
# Commercial Software Options: Solid Models - Parasolid
option(SV_USE_PARASOLID "Parasolid" OFF)
mark_as_superbuild(SV_USE_PARASOLID)

#-----------------------------------------------------------------------------
# Commercial Software Options: Meshing - MeshSim
option(SV_USE_MESHSIM "Use MeshSim commercial libraries.  Requires licenese" OFF)
mark_as_superbuild(SV_USE_MESHSIM)

#-----------------------------------------------------------------------------
# Solver Build Options (Modules)
#-----------------------------------------------------------------------------
option(SV_USE_THREEDSOLVER "Option to build flowsolver modules (requires Fortran)" OFF)
mark_as_superbuild(SV_USE_THREEDSOLVER)
option(SV_USE_THREEDSOLVER_SHARED_LIBRARIES "Option to build flowsolver libs as shared" OFF)
mark_as_superbuild(SV_USE_THREEDSOLVER_SHARED_LIBRARIES)

#-----------------------------------------------------------------------------
# Remaining optional dependencies
#-----------------------------------------------------------------------------
# Enable Interl Runtime libs if we need or want them
option(SV_USE_INTELRUNTIME "Add Intel Runtime Libraries (these may be needed by some libraries)" OFF)
mark_as_superbuild(SV_USE_INTELRUNTIME)

#-----------------------------------------------------------------------------
# Enable Testing
option(BUILD_TESTING "Build ${PROJECT_NAME} testing" OFF)
mark_as_superbuild(VARS BUILD_TESTING)

#-----------------------------------------------------------------------------
# Thirdparty shared libs
option(SV_USE_THIRDPARTY_SHARED_LIBRARIES "Option to build the thirdparty libs as shared" OFF)
mark_as_superbuild(SV_USE_THIRDPARTY_SHARED_LIBRARIES)

#-----------------------------------------------------------------------------
# Thirdparty shared libs
option(SV_USE_MODULES_SHARED_LIBRARIES "Option to build the thirdparty libs as shared" ON)
mark_as_superbuild(SV_USE_MODULES_SHARED_LIBRARIES)

#-----------------------------------------------------------------------------
# Option to build qt GUI
option(SV_USE_QT_GUI "Option to build the SimVascular QT GUI" OFF)
mark_as_superbuild(SV_USE_QT_GUI)

option(SV_USE_QT "Option to build the SimVascular QT" OFF)
mark_as_superbuild(SV_USE_QT)

option(SV_NO_PYTHONQT_ALL "Option to use PythonQt_all" ON)
mark_as_superbuild(SV_NO_PYTHONQT_ALL)

#-----------------------------------------------------------------------------
# Custom CTK
option(SV_USE_CUSTOM_CTK "Option to build a custom CTK" OFF)
mark_as_superbuild(SV_USE_CUSTOM_CTK)

#-----------------------------------------------------------------------------
# Custom SimpleITK
option(SV_USE_CUSTOM_SimpleITK "Option to build a custom SimpleITK" OFF)
mark_as_superbuild(SV_USE_CUSTOM_SimpleITK)
