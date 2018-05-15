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
# Legacy Level Sets
#-----------------------------------------------------------------------------

set(lib ${SV_LIB_LSET_NAME})
set(export_directive lset)
set(export_file_name svLSetExports.h)

list(APPEND CORELIBS ${lib})

set(CXXSRCS sv2_LevelSet.cxx sv2_LevelSetNode.cxx
  sv2_LevelSetStructuredGrid.cxx sv2_LevelSetDenseGrid.cxx
  sv2_LevelSetSparseGrid.cxx sv2_LevelSetVelocity.cxx
  sv2_LevelSetVelocityKGI.cxx sv2_LevelSetVelocityConstant.cxx
  sv2_LevelSetVelocityThreshold.cxx sv2_LevelSetVelocityPotential.cxx
  sv2_LevelSetVelocityExponentialDecay.cxx
  sv2_LevelSetVelocitySmooth.cxx sv2_LevelSetVelocityImage.cxx
  sv2_IntArrayList.cxx sv2_StateArray.cxx sv2_Timer.cxx
  )
set(HDRS sv2_LevelSet.h sv2_LevelSetNode.h
  sv2_LevelSetStructuredGrid.h sv2_LevelSetDenseGrid.h
  sv2_LevelSetSparseGrid.h sv2_LevelSetVelocity.h
  sv2_LevelSetVelocityKGI.h sv2_LevelSetVelocityConstant.h
  sv2_LevelSetVelocityThreshold.h sv2_LevelSetVelocityPotential.h
  sv2_LevelSetVelocityExponentialDecay.h sv2_LevelSetVelocitySmooth.h
  sv2_LevelSetVelocityImage.h
  sv2_IntArrayList.h sv2_StateArray.h sv2_Timer.h
  )

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} sv2_LsetCore_init.cxx sv2_LsetV_init.cxx sv2_Lset_init.cxx)

target_link_libraries(${lib}
  ${ITK_LIBRARIES} ${VTK_LIBRARIES}
  ${TCL_LIBRARY} ${TK_LIBRARY}
  ${SV_LIB_GLOBALS_NAME} ${SV_LIB_IMAGE_NAME})

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
