# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved.
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

#-----------------------------------------------------------------------------
# VTK
set(proj VTK)
# VTK must come before ITK because ITK calls find_package(VTK) without any
# extra specifications and picks up a random VTK which ruins everything. We
# resolve by finding VTK first, and then setting temp variables that are reset
# after ITK is found
# If using toplevel dir, foce VTK_DIR to be the SV_VTK_DIR set by the
# simvascular_add_new_external macro
if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
  set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/vtk-${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION} CACHE PATH "Force ${proj} dir to externals" FORCE)
endif()
# Find VTK, specific components
simvascular_external(${proj}
  COMPONENTS
  vtkFiltersFlowPaths
  vtkWrappingTcl
  vtkRenderingTk
  vtkCommonDataModel
  vtkCommonCore
  vtkChartsCore
  vtkCommonExecutionModel
  vtkFiltersCore
  vtkFiltersVerdict
  vtkIOLegacy
  vtkIOPLY
  vtkIOXML
  vtkImagingStencil
  vtktiff
  ${${proj}_PYTHON_MODULES}
  NO_DEFAULT_PATH
  SHARED_LIB ${SV_USE_${proj}_SHARED}
  VERSION ${${proj}_VERSION})

# Include cmake file provided by VTK to define libs and include dirs
include(${${proj}_USE_FILE})

# If using python, we must replace any replace any occurences of python
# with the python specified. Note: This can cause issues if the python
# specified is not the same version as the one used to build vtk
if(SV_USE_PYTHON)
  simvascular_list_find_and_replace(${proj}_LIBRARIES "^.*libpython.*$" ${PYTHON_LIBRARY})
  simvascular_property_list_find_and_replace(vtkWrappingPythonCore INTERFACE_LINK_LIBRARIES "^.*libpython.*$" ${PYTHON_LIBRARY})
endif()
# We must replace any occurences of tcl with the specified tcl. This can also
# cause problems if it is not the same tcl used to compile vtk. We also have
# to manually add vtkCommonCoreTCL to the list of libraries
simvascular_property_list_find_and_replace(vtkCommonCoreTCL INTERFACE_LINK_LIBRARIES "^.*libtcl.*$" ${TCL_LIBRARY})
set(${proj}_LIBRARIES ${${proj}_LIBRARIES} vtkCommonCoreTCL)
if(NOT SV_INSTALL_${proj}_TCL_DIR)
  set(SV_INSTALL_${proj}_TCL_DIR ${SV_EXT_${proj}_BIN_DIR}/lib/tcltk/vtk-${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION})
endif()
# Set SV_VTK_DIR to the toplevel VTK if it exists
simvascular_get_external_path_from_include_dir(${proj})
# Finally add VTK_DIR to the front of prefix path so that the correct vtk
# is found by itk
set(CMAKE_PREFIX_PATH "${${proj}_DIR}" "${CMAKE_PREFIX_PATH}")
#-----------------------------------------------------------------------------
