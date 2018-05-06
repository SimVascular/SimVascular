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
if(SV_USE_PYTHON)
	include_directories(${PYTHON_INCLUDE_PATH}
		    ${PYTHON_INCLUDE_DIR}
		    ${PYTHON_INCLUDE_DIRS})
endif()

set(CXXSRCS  cvITKUtils.cxx cvITKLevelSet.cxx)
set(HDRS cvITKLevelSet.h cvITKLevelSetBase.h
        cv_VascularPhaseOneLevelSetFunction.hxx
	cv_VascularPhaseOneLevelSetImageFilter.hxx
	cv_VascularPhaseTwoLevelSetFunction.hxx
	cv_VascularPhaseTwoLevelSetImageFilter.hxx
	cv_VascularLevelSetObserver.h
        cv_ITKLset_ConnectVTKITK.h cv_ITKLset_ITK_Macros.h
        cv_ITKLset_Macros.h cv_ITKLset_TCL_Macros.h
        cv_ITKLset_VTK_Macros.h cv_ITKLset_ImgInfo.h
  )

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} cv_ITKUtils_init.cxx cv_ITKLset2d_init.cxx cv_ITKLset3d_init.cxx cv_ITKLset_init.cxx 
cv_ITKUtils_init_py.cxx cv_ITKLset2d_init_py.cxx cv_ITKLset3d_init_py.cxx cv_ITKLset_init_py.cxx)

if(SV_USE_PYTHON)
  target_link_libraries(${lib} ${PYTHON_LIBRARY})
endif()

target_link_libraries(${lib}
 ${SV_LIB_REPOSITORY_NAME}
 ${ITK_LIBRARIES} ${VTK_LIBRARIES})

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