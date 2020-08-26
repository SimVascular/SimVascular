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
# Plugin - ITK Level Sets
#-----------------------------------------------------------------------------
set(lib ${SV_LIB_ITK_LSET_NAME})
set(export_directive segitk)
set(export_file_name svSegITKExports.h)

list(APPEND PLUGINLIBS ${lib})
include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH} ${TINYXML_INCLUDE_DIR})

set(CXXSRCS  sv3_ITKUtils.cxx sv3_ITKLevelSet.cxx)
set(HDRS sv3_ITKLevelSet.h sv3_ITKLevelSetBase.h
        sv3_VascularPhaseOneLevelSetFunction.hxx
	sv3_VascularPhaseOneLevelSetImageFilter.hxx
	sv3_VascularPhaseTwoLevelSetFunction.hxx
	sv3_VascularPhaseTwoLevelSetImageFilter.hxx
	sv3_VascularLevelSetObserver.h
        sv3_ITKLset_ConnectVTKITK.h sv3_ITKLset_ITK_Macros.h
        sv3_ITKLset_Macros.h sv3_ITKLset_TCL_Macros.h
        sv3_ITKLset_VTK_Macros.h sv3_ITKLset_ImgInfo.h
  )

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} sv3_ITKUtils_init.cxx sv3_ITKLset2d_init.cxx sv3_ITKLset3d_init.cxx sv3_ITKLset_init.cxx )

target_link_libraries(${lib}
 ${SV_LIB_REPOSITORY_NAME}
 ${ITK_LIBRARIES} ${VTK_LIBRARIES})

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
