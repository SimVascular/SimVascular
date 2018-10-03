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

#------------------------------------------------------------------------------
# Lib/module name
set(lib ${SV_LIB_VTKSVPARAMETERIZATION_NAME})
set(module vtkSVParameterization)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Core SRCS and HDRS
set(SRCS
  vtkSVPlanarMapper.cxx
  vtkSVBoundaryMapper.cxx
  vtkSVSquareBoundaryMapper.cxx
  vtkSVSuperSquareBoundaryMapper.cxx
  vtkSVPointSetBoundaryMapper.cxx
  vtkSVParameterizeSurfaceOnPolycube.cxx
  vtkSVNewParameterizeSurfaceOnPolycube.cxx
  vtkSVParameterizeVolumeOnPolycube.cxx
  vtkSVSphericalMapper.cxx
  )

set(HDRS
  vtkSVPlanarMapper.h
  vtkSVBoundaryMapper.h
  vtkSVSquareBoundaryMapper.h
  vtkSVSuperSquareBoundaryMapper.h
  vtkSVPointSetBoundaryMapper.h
  vtkSVParameterizeSurfaceOnPolycube.h
  vtkSVNewParameterizeSurfaceOnPolycube.h
  vtkSVParameterizeVolumeOnPolycube.h
  vtkSVSphericalMapper.h
  )
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Build lib either as vtk module or as a regular lib
if(VTKSV_BUILD_LIBS_AS_VTK_MODULES)
  if(NOT "${lib}" STREQUAL "${module}")
    message(FATAL_ERROR "Error: lib and module name should be equal; Lib is ${lib} and Module is ${module}")
  endif()

  # Parameterization vtk module
  vtksv_add_module(${lib}
    SRCS ${SRCS}
    HDRS ${HDRS}
    PACKAGE_DEPENDS ${VTK_LIBRARIES})

else()

  # Regular lib
  add_library(${lib} ${SV_LIBRARY_TYPE} ${SRCS})

  target_link_libraries(${lib}
    ${VTK_LIBRARIES}
    ${SV_LIB_VTKSVCOMMON_NAME}
    ${SV_LIB_VTKSVIO_NAME}
    ${SV_LIB_VTKSVMISC_NAME}
    ${SV_LIB_VTKSVNURBS_NAME}
    ${SV_LIB_VTKSVGEOMETRY_NAME}
    )

  # Set up for exports
  string(TOUPPER ${module} EXPORT_NAME)
  set(_export_macro_names
    EXPORT_MACRO_NAME ${EXPORT_NAME}_EXPORT
    NO_EXPORT_MACRO_NAME ${EXPORT_NAME}_NO_EXPORT
    DEPRECATED_MACRO_NAME ${EXPORT_NAME}_DEPRECATED
    NO_DEPRECATED_MACRO_NAME ${EXPORT_NAME}_NO_DEPRECATED
    )

  # Generate export headers
  generate_export_header(${lib}
    ${_export_macro_names}
    EXPORT_FILE_NAME ${module}Module.h)

  # Install of libs and headers
  if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
      RUNTIME DESTINATION ${VTKSV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
      LIBRARY DESTINATION ${VTKSV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
      ARCHIVE DESTINATION ${VTKSV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
      )
  endif()
  if(SV_INSTALL_HEADERS)
    install(FILES ${HDRS} ${CMAKE_CURRENT_BINARY_DIR}/${module}Module.h
      DESTINATION ${VTKSV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
      )
  endif()

endif()
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Create additional executables
if(VTKSV_BUILD_EXES)
  set(EXE_LIST PlanarMapper SuperSquarePlanarMapper)
  if(VTKSV_BUILD_MISC)
    set(EXE_LIST ${EXE_LIST} SphericalMapper)
  endif()
  foreach(exe ${EXE_LIST})
    add_executable(vtkSV${exe} ${exe}.cxx)
    target_link_libraries(vtkSV${exe} ${VTK_LIBRARIES} ${SV_LIB_VTKSVPARAMETERIZATION_NAME} ${EXTRA_LIBRARIES} ${SV_LIB_VTKSVCOMMON_NAME})
    install(TARGETS vtkSV${exe}
      RUNTIME DESTINATION ${VTKSV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables)
  endforeach()
endif()
#------------------------------------------------------------------------------
