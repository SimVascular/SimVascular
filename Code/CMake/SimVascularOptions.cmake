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
	This must be turn on if your wish to download any packages." ON)

set(SV_SUPERBUILD_LIBS_DIR "${svlibs_OPEN_URLBASE}" CACHE TYPE PATH)
mark_as_superbuild(SV_SUPERBUILD_LIBS_DIR)

option(SV_BUILD_SHARED_LIBS "Build ${PROJECT_NAME} as shared libraries." OFF)
mark_as_superbuild(VARS SV_BUILD_SHARED_LIBS ALL_PROJECTS)

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
option(SV_USE_ZLIB "Use ZLib" ON)
mark_as_superbuild(SV_USE_ZLIB)

option(SV_USE_MPICH2 "Use MPICH2" ON)
mark_as_superbuild(SV_USE_MPICH2)

option(SV_USE_DUMMY_MPICH2 "Use Dummy MPICH2" OFF)
mark_as_superbuild(SV_USE_DUMMY_MPICH2)

option(SV_USE_MSMPI "Use MSMPI" OFF)
mark_as_superbuild(SV_USE_MSMPI)

#-----------------------------------------------------------------------------
# General Library Options
option(SV_USE_GLIB "Use GLIB Library" OFF)
mark_as_superbuild(SV_USE_GLIB)

option(SV_USE_GTS "Use GTS Library" OFF)
mark_as_superbuild(SV_USE_GTS)

#-----------------------------------------------------------------------------
# Plugins
option(SV_USE_GDCM "Enable GDCM" ON)
mark_as_superbuild(SV_USE_GDCM)

option(SV_USE_ITK "Enable ITK Plugin" ON)
mark_as_superbuild(SV_USE_ITK)

option(ITK_SHARED_LIBRARIES "Buiild vtk libraries as shared libs" OFF)
mark_as_superbuild(ITK_SHARED_LIBRARIES)

option(SV_USE_VMTK "Enable VMTK Plugin" ON)
mark_as_superbuild(SV_USE_VMTK)

option(SV_USE_TETGEN "Enable Tetgen Meshing Plugin" ON)
mark_as_superbuild(SV_USE_TETGEN)

option(VTK_SHARED_LIBRARIES "Buiild vtk libraries as shared libs" OFF)
mark_as_superbuild(VTK_SHARED_LIBRARIES)

option(SV_USE_PYTHON "Use Tcl Python" OFF)
mark_as_superbuild(SV_USE_PYTHON)

#-----------------------------------------------------------------------------
# Adaptor Options
# Adaptor converts objects between the different solid models.
option(SV_USE_MESHSIM_ADAPTOR "Build the adapter (Requires Fortran and MeshSim)" OFF)
mark_as_superbuild(SV_USE_MESHSIM_ADAPTOR)

#-----------------------------------------------------------------------------
# Open Source Software Options: Solid Models - OPENCASCADE
option(SV_USE_OPENCASCADE "OPENCASCADE" OFF)
mark_as_superbuild(SV_USE_OPENCASCADE)

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

#-----------------------------------------------------------------------------
# Remaining optional dependencies
#-----------------------------------------------------------------------------
# Enable Interl Runtime libs if we need or want them
option(SV_USE_INTELRUNTIME "Add Intel Runtime Libraries (these may be needed by some libraries)" OFF)
mark_as_superbuild(SV_USE_INTELRUNTIME)

#-----------------------------------------------------------------------------
# Enable Testing
option(SV_BUILD_TESTING "Build ${PROJECT_NAME} testing" OFF)
mark_as_superbuild(VARS BUILD_TESTING)
