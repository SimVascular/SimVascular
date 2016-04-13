#-----------------------------------------------------------------------------
# Setup SimVascular Install options and directories
#-----------------------------------------------------------------------------
#option(SV_SUPERBUILD_INSTALL 
  #  "Enabling this option will install automatically SimVascular in Superbuild Mode" OFF)
option(SV_ENABLE_DISTRIBUTION 
  "Enalble Distribution Targets (CPack)" OFF)
mark_as_advanced(SV_ENABLE_DISTRIBUTION)
mark_as_superbuild(SV_ENABLE_DISTRIBUTION)

if(SV_ENABLE_DISTRIBUTION)
  #set(SV_INSTALL_SYSTEM_LIBS OFF CACHE "This is for distribution only, it enables installing certain system libraries.")
  #mark_as_advanced(SV_INSTALL_SYSTEM_LIBS)
  mark_as_superbuild(SV_INSTALL_SYSTEM_LIBS:BOOL)
  if(WIN32)
  endif()
endif()

#if(NOT SUPERBUILD_INSTALL_PREFIX)
#  SET(SUPERBUILD_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/install/)
#endif()
#mark_as_superbuild(SUPERBUILD_INSTALL_PREFIX:PATH)

if(NOT SV_INSTALL_ROOT_DIR)
  set(SV_INSTALL_ROOT_DIR simvascular)
endif()
mark_as_superbuild(SV_INSTALL_ROOT_DIR:PATH)
if(NOT WIN32)
  if(NOT CMAKE_INSTALL_PREFIX MATCHES "${SV_INSTALL_ROOT_DIR}")
    set(CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX}/${SV_INSTALL_ROOT_DIR})
  endif()
endif()
if(NOT SV_INSTALL_HOME_DIR)
  set(SV_INSTALL_HOME_DIR ".")
endif()
mark_as_superbuild(SV_INSTALL_HOME_DIR:PATH)

if(NOT SV_INSTALL_TIMESTAMP_DIR)
  set(SV_INSTALL_TIMESTAMP_DIR "timestamp")
endif()
mark_as_superbuild(SV_INSTALL_TIMESTAMP_DIR:PATH)

if(NOT SV_INSTALL_SCRIPT_DIR)
  set(SV_INSTALL_SCRIPT_DIR ".")
endif()
mark_as_superbuild(SV_INSTALL_SCRIPT_DIR:PATH)

if(NOT SV_INSTALL_RUNTIME_DIR)
  # CHANGE FOR EXECUTABLE RENAME REMOVE
  set(SV_INSTALL_RUNTIME_DIR "Bin")
endif()
mark_as_superbuild(SV_INSTALL_RUNTIME_DIR:PATH)

if(NOT SV_INSTALL_LIBRARY_DIR)
  set(SV_INSTALL_LIBRARY_DIR Lib)
endif()
mark_as_superbuild(SV_INSTALL_LIBRARY_DIR:PATH)

if(NOT SV_INSTALL_ARCHIVE_DIR)
  set(SV_INSTALL_ARCHIVE_DIR Lib)
endif()
mark_as_superbuild(SV_INSTALL_ARCHIVE_DIR:PATH)

if(NOT SV_INSTALL_INCLUDE_DIR)
  set(SV_INSTALL_INCLUDE_DIR include/)
endif()
mark_as_superbuild(SV_INSTALL_INCLUDE_DIR:PATH)

if(NOT SV_INSTALL_DATA_DIR)
  set(SV_INSTALL_DATA_DIR data/)
endif()
mark_as_superbuild(SV_INSTALL_DATA_DIR:PATH)

if(NOT SV_INSTALL_DOC_DIR)
 set(SV_INSTALL_DOC_DIR doc/)
endif()
mark_as_superbuild(SV_INSTALL_DOC_DIR:PATH)

if(NOT SV_INSTALL_DOXYGEN_DIR)
  set(SV_INSTALL_DOXYGEN_DIR ${SV_INSTALL_DOC_DIR}/doxygen)
endif()
mark_as_superbuild(SV_INSTALL_DOXYGEN_DIR:PATH)

if(NOT SV_INSTALL_TCL_CODE_DIR)
  #this refers to simvascular TCL code, not tcltk itself
  set(SV_INSTALL_TCL_CODE_DIR Tcl)
endif()
mark_as_superbuild(SV_INSTALL_TCL_CODE_DIR:PATH)
#-----------------------------------------------------------------------------
# Third Party install locations
#-----------------------------------------------------------------------------
if(NOT SV_INSTALL_EXTERNALS_RUNTIME_DIR)
  set(SV_INSTALL_EXTERNALS_RUNTIME_DIR lib)
endif()
mark_as_superbuild(SV_INSTALL_EXTERNALS_RUNTIME_DIR:PATH)

if(NOT SV_INSTALL_MPI_RUNTIME_DIR)
  if(WIN32)
    set(SV_INSTALL_MPI_RUNTIME_DIR lib)
  else()
    set(SV_INSTALL_MPI_RUNTIME_DIR lib)
  endif()
endif()
mark_as_superbuild(SV_INSTALL_MPI_RUNTIME_DIR:PATH)

if(NOT SV_INSTALL_MPI_LIBRARY_DIR)
  set(SV_INSTALL_MPI_LIBRARY_DIR lib)
endif()
mark_as_superbuild(SV_INSTALL_MPI_LIBRARY_DIR:PATH)

if(NOT SV_INSTALL_MPI_EXE_DIR)
  set(SV_INSTALL_MPI_EXE_DIR ".")
endif()
mark_as_superbuild(SV_INSTALL_MPI_EXE_DIR:PATH)

if(NOT SV_INSTALL_EXTERNAL_EXE_DIR)
  set(SV_INSTALL_EXTERNAL_EXE_DIR ".")
endif()
mark_as_superbuild(SV_INSTALL_EXTERNAL_EXE_DIR:PATH)

#-----------------------------------------------------------------------------
# Third Party install locations
#-----------------------------------------------------------------------------
# VTK 
if(NOT SV_INSTALL_VTK_RUNTIME_DIR)
  set(SV_INSTALL_VTK_RUNTIME_DIR vtk/bin)
endif()
mark_as_superbuild(SV_INSTALL_VTK_RUNTIME_DIR:PATH)

if(NOT SV_INSTALL_VTK_LIBRARY_DIR)
  set(SV_INSTALL_VTK_LIBRARY_DIR vtk/lib)
endif()
mark_as_superbuild(SV_INSTALL_VTK_LIBRARY_DIR:PATH)

if(NOT SV_INSTALL_VTK_ARCHIVE_DIR)
  set(SV_INSTALL_VTK_ARCHIVE_DIR vtk/lib)
endif()
mark_as_superbuild(SV_INSTALL_VTK_ARCHIVE_DIR:PATH)

if(NOT SV_INSTALL_VTK_INCLUDE_DIR)
  set(SV_INSTALL_VTK_INCLUDE_DIR vtk/include)
endif()
mark_as_superbuild(SV_INSTALL_VTK_INCLUDE_DIR:PATH)

if(NOT SV_INSTALL_VTK_TCL_DIR)
  set(SV_INSTALL_VTK_TCL_DIR vtk/lib/tcltk/modules)
endif()
mark_as_superbuild(SV_INSTALL_VTK_TCL_DIR:PATH)

set(SV_INSTALL_BUILD_SHARED_LIBS ON)
mark_as_superbuild(SV_INSTALL_BUILD_SHARED_LIBS)


#-----------------------------------------------------------------------------
# ITK 
#
if(NOT SV_INSTALL_ITK_RUNTIME_DIR)
  set(SV_INSTALL_ITK_RUNTIME_DIR itk/bin)
endif()
mark_as_superbuild(SV_INSTALL_ITK_RUNTIME_DIR:PATH)
if(NOT SV_INSTALL_ITK_LIBRARY_DIR)
  set(SV_INSTALL_ITK_LIBRARY_DIR itk/lib)
endif()
mark_as_superbuild(SV_INSTALL_ITK_LIBRARY_DIR:PATH)
if(NOT SV_INSTALL_ITK_ARCHIVE_DIR)
  set(SV_INSTALL_ITK_ARCHIVE_DIR itk/lib)
endif()
mark_as_superbuild(SV_INSTALL_ITK_ARCHIVE_DIR:PATH)

if(NOT SV_INSTALL_ITK_INCLUDE_DIR)
  set(SV_INSTALL_ITK_INCLUDE_DIR itk/include)
endif()
mark_as_superbuild(SV_INSTALL_ITK_INCLUDE_DIR:PATH)

#-----------------------------------------------------------------------------
# OPENCASCADE 
#
if(NOT OPENCASCADE_INSTALL_DIR)
  set(OPENCASCADE_INSTALL_DIR ${SV_INSTALL_ROOT_DIR}/opencascade)
endif()

if(NOT SV_INSTALL_OPENCASCADE_LIBRARY_DIR)
  set(SV_INSTALL_OPENCASCADE_LIBRARY_DIR opencascade/lib)
endif()
mark_as_superbuild(SV_INSTALL_OPENCASCADE_LIBRARY_DIR:PATH)

if(NOT SV_INSTALL_OPENCASCADE_INCLUDE_DIR)
  set(SV_INSTALL_OPENCASCADE_INCLUDE_DIR opencascade/include)
endif()
mark_as_superbuild(SV_INSTALL_OPENCASCADE_INCLUDE_DIR:PATH)

#-----------------------------------------------------------------------------
# TCL 
#
if(NOT SV_INSTALL_TCL_TOP_DIR)
  set(SV_INSTALL_TCL_TOP_DIR "TclTk")
endif()
mark_as_superbuild(SV_INSTALL_TCL_TOP_DIR:PATH)

if(NOT SV_INSTALL_TCL_RUNTIME_DIR)
  set(SV_INSTALL_TCL_RUNTIME_DIR ".")
endif()
mark_as_superbuild(SV_INSTALL_TCL_RUNTIME_DIR:PATH)

if(NOT SV_INSTALL_TCL_LIBRARY_DIR)
  set(SV_INSTALL_TCL_LIBRARY_DIR ${SV_INSTALL_TCL_TOP_DIR}/lib)
endif()
mark_as_superbuild(SV_INSTALL_TCL_LIBRARY_DIR:PATH)

if(NOT SV_INSTALL_TCL_ARCHIVE_DIR)
  set(SV_INSTALL_TCL_ARCHIVE_DIR ${SV_INSTALL_TCL_TOP_DIR}/lib)
endif()
mark_as_superbuild(SV_INSTALL_TCL_ARCHIVE_DIR:PATH)

if(NOT SV_INSTALL_TCL_INCLUDE_DIR)
  set(SV_INSTALL_TCL_INCLUDE_DIR ${SV_INSTALL_TCL_TOP_DIR}/include)
endif()
mark_as_superbuild(SV_INSTALL_TCL_INCLUDE_DIR:PATH)


if(NOT SV_INSTALL_PARASOLID_RUNTIME_DIR)
  if(WIN32)
    set(SV_INSTALL_PARASOLID_RUNTIME_DIR ".")
  else()
    set(SV_INSTALL_PARASOLID_RUNTIME_DIR ".")
  endif()
endif()
mark_as_superbuild(SV_INSTALL_PARASOLID_RUNTIME_DIR:PATH)

if(NOT SV_INSTALL_PARASOLID_SCHEMA_DIR)
  set(SV_INSTALL_PARASOLID_SCHEMA_DIR schema)
endif()
mark_as_superbuild(SV_INSTALL_PARASOLID_SCHEMA_DIR:PATH)

if(NOT SV_INSTALL_RUNTIME_DIR)
 if(WIN32)
  set(SV_INSTALL_RUNTIME_DIR ".")
else()
  set(SV_INSTALL_RUNTIME_DIR ".")
endif()
endif()
mark_as_superbuild(SV_INSTALL_RUNTIME_DIR:PATH)

#install(CODE "set(ENV{PATH} \"${CMAKE_INSTALL_PREFIX}\")")


#if(NOT SV_INSTALL_EXPORT_NAME)
#  set(SV_INSTALL_EXPORT_NAME SVTargets)
#endif()
# if(NOT SV_INSTALL_PACKAGE_DIR)
#   set(SV_INSTALL_PACKAGE_DIR "lib/cmake/simvascular-${SV_MAJOR_VERSION}.${SV_MINOR_VERSION}")
# endif()

getListOfVarsPrefix("SV_INSTALL" _VARLIST)
foreach(_var ${_VARLIST})
  string(REPLACE "SV_INSTALL" "SV_INSTALL_NATIVE" _var_native ${_var})
  if(${_var})
    file(TO_NATIVE_PATH ${${_var}} ${_var_native})
    dev_message("${_var_native} ${${_var_native}}")
  else()
    dev_message("NO ${_var}")
  endif()
endforeach()

if(SV_DEVELOPER_OUTPUT)
  getListOfVarsPrefix("SV_INSTALL" _VARLIST)
  list(INSERT _VARLIST 0 CMAKE_INSTALL_PREFIX)
  print_vars(_VARLIST)
endif()


#-----------------------------------------------------------------------------
# Setup Output directories for compiling
#-----------------------------------------------------------------------------
#
if(NOT DEFINED OUTBIN_DIR OR NOT DEFINED OUTLIB_DIR)
  set(OUTBIN_DIR "${SV_BINARY_DIR}/Bin")
  set(OUTLIB_DIR ${SV_BINARY_DIR}/Lib)
endif()
mark_as_superbuild(OUTBIN_DIR:PATH)
mark_as_superbuild(OUTLIB_DIR:PATH)

if(NOT DEFINED SV_DEVELOPER_SCRIPT_DIR)
  set(SV_DEVELOPER_SCRIPT_DIR "${SV_BINARY_DIR}")
endif()
mark_as_superbuild(SV_DEVELOPER_SCRIPT_DIR:PATH)

if(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${OUTBIN_DIR}")
endif()
mark_as_superbuild(CMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH)

if(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${OUTLIB_DIR}")
endif()
mark_as_superbuild(CMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH)

if(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${OUTLIB_DIR}")
endif()
mark_as_superbuild(CMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH)

mark_as_advanced(CMAKE_RUNTIME_OUTPUT_DIRECTORY
  CMAKE_LIBRARY_OUTPUT_DIRECTORY
  CMAKE_ARCHIVE_OUTPUT_DIRECTORY)

if(SV_DEVELOPER_OUTPUT)
  set(_VARLIST OUTBIN_DIR
    OUTLIB_DIR
    SCRIPT_DIR)
  print_vars(_VARLIST)
endif()





