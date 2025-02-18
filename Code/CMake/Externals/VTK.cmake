# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

set(proj VTK)

set(msg "[Code/CMake/Externals/VTK.cmake]")
message(STATUS "${msg} =============== Code/CMake/Externals    VTK.cmake ===============")
message(STATUS "${msg} proj: ${proj}")
message(STATUS "${msg} SV_USE_VTK: ${SV_USE_VTK}")
message(STATUS "${msg} SV_VTK_DIR: ${SV_VTK_DIR}")
message(STATUS "${msg} VTK_VERISON: ${${proj}_VERSION}")
message(STATUS "${msg} SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR: ${SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR}")
message(STATUS "${msg} PYTHON_LIBRARY: ${PYTHON_LIBRARY}")

# VTK must come before ITK because ITK calls find_package(VTK) without any
# extra specifications and picks up a random VTK which ruins everything. We
# resolve by finding VTK first, and then setting temp variables that are reset
# after ITK is found
# If using toplevel dir, foce VTK_DIR to be the SV_VTK_DIR set by the
# simvascular_add_new_external macro
#
if(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)
  set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/vtk-${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION} CACHE PATH "Force ${proj} dir to externals" FORCE)
  message(STATUS "${msg} VTK_DIR: ${VTK_DIR}")

  if(WIN32)
    set(${proj}_DLL_PATH "${SV_${proj}_DIR}/bin" CACHE PATH "Force VTK DLL Path")
  endif()
endif()

# Find VTK, specific components
#
# [NOTE:DaveP] Commenting out vtkWrappingTcl and vtkRenderingTk causes
# build to fail
#
# Undefined symbols for architecture x86_64:
#   "vtkInteractionStyle_AutoInit_Destruct()", referenced from:
#      vtkRenderingCore_AutoInit::~vtkRenderingCore_AutoInit() in sv_vtk_utils.cxx.o
#     vtkRenderingCore_AutoInit::~vtkRenderingCore_AutoInit() in sv_cgeom.cxx.o
#    vtkRenderingCore_AutoInit::~vtkRenderingCore_AutoInit() in sv_Math.cxx.o
#
# We need to build VTK withour TCL bindings I guess.
#
simvascular_external(${proj}
  COMPONENTS
  FiltersFlowPaths
  # WrappingTcl
  # RenderingTk
  RenderingLabel
  CommonDataModel
  CommonCore
  ChartsCore
  CommonExecutionModel
  FiltersCore
  FiltersTexture
  FiltersVerdict
  IOGeometry
  IOLegacy
  IOPLY
  IOXML
  ImagingStencil
  ParallelCore
  tiff
  ${${proj}_PYTHON_MODULES}
  NO_DEFAULT_PATH
  SHARED_LIB ${SV_USE_${proj}_SHARED}
  VERSION ${${proj}_VERSION}
  REQUIRED
  )

message(STATUS "${msg} VTK_DIR: ${VTK_DIR}")

# No longer needed
# Include cmake file provided by VTK to define libs and include dirs
# include(${${proj}_USE_FILE})

# We have to manually add vtkCommonCoreTCL to the list of libraries
# set(${proj}_LIBRARIES ${${proj}_LIBRARIES} vtkCommonCoreTCL)
#
set(vtkCommonCoreTCL_LIBRARIES "")

if(NOT SV_INSTALL_${proj}_TCL_DIR)
  set(SV_INSTALL_${proj}_TCL_DIR ${SV_EXT_${proj}_BIN_DIR}/lib/tcltk/vtk-${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION})
endif()

# Set SV_VTK_DIR to the toplevel VTK if it exists
# [davep] already set.
#simvascular_get_external_path_from_include_dir(${proj})

# Finally add VTK_DIR to the front of prefix path so that the correct vtk
# is found by itk
set(CMAKE_PREFIX_PATH "${${proj}_DIR}" "${CMAKE_PREFIX_PATH}")

# We must replace any occurences of tcl with the specified tcl. This can also
# cause problems if it is not the same tcl used to compile vtk.
#
# [davep] what's this all about ? 
#
#  PYTHON_LIBRARY is not set so comment out.
#
if(SV_USE_PYTHON)
  #get_filename_component(tmp_replace_python_lib_name ${PYTHON_LIBRARY} NAME)
endif()

if(SV_USE_FREETYPE)
  get_filename_component(tmp_replace_freetype_lib_name ${FREETYPE_LIBRARY} NAME)
endif()

get_cmake_property(_variableNames VARIABLES)
list(FILTER _variableNames INCLUDE REGEX "vtk")
list(FILTER _variableNames INCLUDE REGEX "LIBRARIES")

# [davep] what's this all about ? 
#
foreach (_variableName ${_variableNames})
  message("${msg} begin variable ${_variableName}=${${_variableName}}")
  string(FIND ${_variableName} "_" myloc)
  string(SUBSTRING ${_variableName} 0 ${myloc} modname)
#   math(EXPR myloc "$myloc - 1")
#message("modname ${_variableName} ${modname}")

   if(SV_USE_PYTHON)
     #simvascular_list_find_and_replace(${_variableName} "${tmp_replace_python_lib_name}" ${PYTHON_LIBRARY})
     #simvascular_property_list_find_and_replace(${modname} INTERFACE_LINK_LIBRARIES "${tmp_replace_python_lib_name}" ${PYTHON_LIBRARY})
   endif()

   if(SV_USE_FREETYPE)
     #simvascular_list_find_and_replace(${_variableName} "${tmp_replace_freetype_lib_name}" ${FREETYPE_LIBRARY})
     #simvascular_property_list_find_and_replace(${modname} INTERFACE_LINK_LIBRARIES "${tmp_replace_freetype_lib_name}" ${FREETYPE_LIBRARY})
   endif()

endforeach()

# If using python, we must replace any replace any occurences of python
# with the python specified. Note: This can cause issues if the python
# specified is not the same version as the one used to build vtk

# [davep] what's this all about ? 

#[[
if(SV_USE_PYTHON)
  get_filename_component(tmp_replace_python_lib_name ${PYTHON_LIBRARY} NAME)
  #message("replace python name ${tmp_replace_python_lib_name}")
  simvascular_list_find_and_replace(${proj}_LIBRARIES "${tmp_replace_python_lib_name}" ${PYTHON_LIBRARY})
  #simvascular_property_list_find_and_replace(WrappingPythonCore INTERFACE_LINK_LIBRARIES "${tmp_replace_python_lib_name}" ${PYTHON_LIBRARY})
endif()

if(SV_USE_FREETYPE)
  get_filename_component(tmp_replace_freetype_lib_name ${FREETYPE_LIBRARY} NAME)
  #message("replace freetype name ${tmp_replace_freetype_lib_name}")
  #message("proj_libs ${${proj}_LIBRARIES}")
  simvascular_list_find_and_replace(${proj}_LIBRARIES "${tmp_replace_freetype_lib_name}" ${FREETYPE_LIBRARY})
  #message("proj_libs_modified ${${proj}_LIBRARIES}")
endif()

#]]

message(STATUS "${msg} ----- Done Code/CMake/Externals    VTK.cmake ")

