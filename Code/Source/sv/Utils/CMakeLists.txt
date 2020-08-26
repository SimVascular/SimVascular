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
# Utils
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_UTILS_NAME})
set(export_directive utils)
set(export_file_name svUtilsExports.h)

list(APPEND CORELIBS ${lib})


SET(CXXSRCS sv_misc_utils.cxx sv_vtk_utils.cxx
  sv_cgeom.cxx
  sv_Math.cxx sv_arg.cxx
  sv_FactoryRegistrar.cxx
  )
SET(HDRS sv_misc_utils.h sv_vtk_utils.h
  sv_cgeom.h
  sv_Math.h sv_arg.h sv_FactoryRegistrar.h
  )

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} sv_utils_init.cxx sv_math_init.cxx)

target_link_libraries(${lib}
	${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
	${SV_LIB_GLOBALS_NAME}
  )

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

