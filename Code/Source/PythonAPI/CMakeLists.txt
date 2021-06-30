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

#-----------------------------------------------------------------------------
# Segmentation
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_PYTHON_API_NAME})
set(export_directive python_api)
set(export_file_name svPythonAPIExports.h)

list(APPEND CORELIBS ${lib})

include_directories(${PYTHON_INCLUDE_PATH} ${PYTHON_INCLUDE_DIR} ${PYTHON_INCLUDE_DIRS})
include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH} ${TINYXML_INCLUDE_DIR})

list (APPEND CXXSRCS 
  Imaging_PyModule.cxx
  Segmentation_PyModule.cxx
  Geometry_PyModule.cxx
  Math_PyModule.cxx
  Meshing_PyModule.cxx
  MeshUtils_PyModule.cxx
  PathPlanning_PyModule.cxx
  PyUtils.cxx
  Modeling_PyModule.cxx
  Simulation_PyModule.cxx
  Vmtk_PyModule.cxx
)

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS})

# hack to define US_MODULE_NAME because we are including sv4gui_ContourGroupIO.h
set_property(TARGET ${lib} PROPERTY US_MODULE_NAME sv4guiModulePythonAPI)
set_property(TARGET ${lib} APPEND PROPERTY COMPILE_DEFINITIONS US_MODULE_NAME=sv4guiModulePythonAPI)

target_link_libraries(${lib} ${PYTHON_LIBRARY} ${org_sv_pythondatanodes} )

target_link_libraries(${lib}
  ${OpenCASCADE_LIBRARIES}
  ${SV_LIB_COMMON_NAME} 
  ${SV_LIB_GEOM_NAME} 
  ${SV_LIB_GLOBALS_NAME} 
  ${SV_LIB_ITK_LSET_NAME} 
  ${SV_LIB_OpenCASCADE_SOLID_NAME}
  ${SV_LIB_PATH_NAME}
  ${SV_LIB_POLYDATA_SOLID_NAME}
  ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  ${SV_LIB_SEGMENTATION_NAME}
  ${SV_LIB_TETGEN_MESH_NAME}
  ${SV_LIB_UTILS_NAME}
  ${SV_LIB_TETGEN_ADAPTOR_NAME}

  org_sv_gui_qt_romsimulation

  ${SV_LIB_MODULE_MESH_NAME}
  ${SV_LIB_MODULE_MODEL_NAME}
  ${SV_LIB_MODULE_MODEL_OCCT_NAME}
  ${SV_LIB_MODULE_PATH_NAME}
  ${SV_LIB_MODULE_SEGMENTATION_NAME}
  ${TCL_LIBRARY} 
  ${TK_LIBRARY}
  ${VTK_LIBRARIES} 
  ${SV_LIB_MODULE_COMMON_NAME}
)

if(SV_USE_TINYXML)
  target_link_libraries(${lib} ${TINYXML_LIBRARY})
endif()

# Set up for exports
string(TOUPPER ${export_directive} EXPORT_NAME)
set(_export_macro_names
  EXPORT_MACRO_NAME SV_EXPORT_${EXPORT_NAME}
  NO_EXPORT_MACRO_NAME SV_EXPORT_${EXPORT_NAME}_NO_EXPORT
  DEPRECATED_MACRO_NAME SV_EXPORT_${EXPORT_NAME}_DEPRECATED
  NO_DEPRECATED_MACRO_NAME SV_EXPORT_${EXPORT_NAME}_NO_DEPRECATED
  )

# Generate export headers
generate_export_header(${lib}
  ${_export_macro_names}
  EXPORT_FILE_NAME ${export_file_name})

if(SV_INSTALL_LIBS)
	install(TARGETS ${lib}
	  RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
		LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
		ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
		)
endif()
if(SV_INSTALL_HEADERS)
  install(FILES ${HDRS} ${CMAKE_CURRENT_BINARY_DIR}/${export_file_name}
	DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
	)
endif()

