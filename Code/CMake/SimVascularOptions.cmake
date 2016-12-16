# Initial SV Options
#-----------------------------------------------------------------------------
# Developer flag (Output extra info during configure)
option(SV_DEVELOPER_OUTPUT "This is a developer mode to print extra messages during configure" OFF)

set(SV_BUILD_TYPE "CMAKE" CACHE STRING "Designate CMAKE build" FORCE)
set_property(CACHE SV_BUILD_TYPE PROPERTY STRINGS CMAKE)
mark_as_advanced(SV_BUILD_TYPE)

#-----------------------------------------------------------------------------
# Distribution
option(SV_ENABLE_DISTRIBUTION "Distribute" OFF)

#-----------------------------------------------------------------------------
# Libs options
option(BUILD_SHARED_LIBS "Build ${PROJECT_NAME} as shared libraries." OFF)

set(SV_LIBRARY_TYPE "STATIC" CACHE STRING "Options are STATIC or SHARED" FORCE)
set_property(CACHE SV_LIBRARY_TYPE PROPERTY STRINGS STATIC SHARED)

#-----------------------------------------------------------------------------
option(SV_SUPPRESS_WARNINGS "Option to suppress all compiler warnings while compiling" ON)

#-----------------------------------------------------------------------------
# General Library Options
option(SV_USE_MPICH2 "Use MPICH2" ON)

option(SV_USE_DUMMY_MPICH2 "Use Dummy MPICH2" OFF)

option(SV_USE_MSMPI "Use MSMPI" OFF)

#-----------------------------------------------------------------------------
# Externals
set(SV_EXTERNALS_TOPLEVEL_DIR "${CMAKE_BINARY_DIR}/Externals" CACHE PATH "Externals toplevel directory")

option(SV_EXTERNALS_USE_TOPLEVEL_DIR "If ON, SV_EXTERNALS_TOPLEVEL_DIR will be used as location for external packages" OFF)

#-----------------------------------------------------------------------------
# External download options
option(SV_DOWNLOAD_EXTERNALS "Option to download all externals from a remote location" OFF)

#-----------------------------------------------------------------------------
# ThirdParty
option(SV_USE_ZLIB "Use ZLib" ON)

option(SV_USE_VMTK "Enable VMTK Plugin" ON)

option(SV_USE_TETGEN "Enable Tetgen Meshing Plugin" ON)

option(SV_USE_PYTHON "Use Tcl Python" OFF)

#-----------------------------------------------------------------------------
# Adaptor Options
# Adaptor converts objects between the different solid models.
option(SV_USE_MESHSIM_ADAPTOR "Build the adapter (Requires Fortran and MeshSim)" OFF)

#-----------------------------------------------------------------------------
# Commercial Software Options: Solid Models - Parasolid
option(SV_USE_PARASOLID "Parasolid" OFF)

#-----------------------------------------------------------------------------
# Commercial Software Options: Meshing - MeshSim
option(SV_USE_MESHSIM "Use MeshSim commercial libraries.  Requires licenese" OFF)

#-----------------------------------------------------------------------------
# Solver Build Options (Modules)
#-----------------------------------------------------------------------------
option(SV_USE_THREEDSOLVER "Option to build flowsolver modules (requires Fortran)" OFF)
option(SV_USE_THREEDSOLVER_SHARED_LIBRARIES "Option to build flowsolver libs as shared" OFF)

#-----------------------------------------------------------------------------
# Remaining optional dependencies
#-----------------------------------------------------------------------------
# Enable Interl Runtime libs if we need or want them
option(SV_USE_INTELRUNTIME "Add Intel Runtime Libraries (these may be needed by some libraries)" OFF)

#-----------------------------------------------------------------------------
# Enable Testing
option(BUILD_TESTING "Build ${PROJECT_NAME} testing" OFF)

#-----------------------------------------------------------------------------
# Thirdparty shared libs
option(SV_USE_THIRDPARTY_SHARED_LIBRARIES "Option to build the thirdparty libs as shared" OFF)

#-----------------------------------------------------------------------------
# Thirdparty shared libs
option(SV_USE_MODULES_SHARED_LIBRARIES "Option to build the thirdparty libs as shared" ON)

#-----------------------------------------------------------------------------
# Option to build qt GUI
option(SV_USE_QT_GUI "Option to build the SimVascular QT GUI" OFF)

option(SV_USE_QT "Option to build the SimVascular QT" OFF)

option(SV_NO_PYTHONQT_ALL "Option to use PythonQt_all" ON)

option(SV_USE_MITK_CONFIG "Option to use MITKConfig.cmake" OFF)

#-----------------------------------------------------------------------------
# Custom CTK
option(SV_USE_CUSTOM_CTK "Option to build a custom CTK" OFF)

#-----------------------------------------------------------------------------
# Custom SimpleITK
option(SV_USE_CUSTOM_SimpleITK "Option to build a custom SimpleITK" OFF)
