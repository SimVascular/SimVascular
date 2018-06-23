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
# Image
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_IMAGE_NAME})
set(export_directive image)
set(export_file_name svImageExports.h)

list(APPEND CORELIBS ${lib})
if(SV_USE_PYTHON)
	include_directories(${PYTHON_INCLUDE_PATH}
	  ${PYTHON_INCLUDE_DIR}
	  ${PYTHON_INCLUDE_DIRS})
endif()

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})

set(CXXSRCS sv2_image.cxx sv2_read_header.cxx
  sv2_decode.cxx sv2_calc_correction_eqn.cxx
  sv2_img_threshold.cxx sv2_DistanceMap.cxx
  sv2_mask_image_in_place.cxx
  )
set(HDRS sv2_image.h sv2_img_offsets.h
  sv2_decode.h sv2_calc_correction_eqn.h
  sv2_img_threshold.h sv2_DistanceMap.h
  sv2_mask_image_in_place.h
  )

if(SV_USE_PYTHON)
  list(APPEND CXXSRCS sv2_image_init_py.cxx)
endif()

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} sv2_image_init.cxx )

if(SV_USE_PYTHON)
  target_link_libraries(${lib} ${PYTHON_LIBRARY})
endif()

target_link_libraries(${lib}
  ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
  ${SV_LIB_GLOBALS_NAME} ${SV_LIB_GEOM_NAME})

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
