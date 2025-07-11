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

project(org_sv_gui_qt_datamanager)

set(CMAKE_CXX_STANDARD 17)

set(lib org_sv_gui_qt_datamanager)

if(SV_USE_TINYXML)
  include_directories(${TINYXML_INCLUDE_DIR})
endif()

if (SV_USE_MITK_CONFIG)
  mitk_create_plugin(
    EXPORT_DIRECTIVE SV_QT_DATAMANAGER
    EXPORTED_INCLUDE_SUFFIXES src
    MODULE_DEPENDS MitkQtWidgetsExt ${SV_LIB_MODULE_PATH_NAME} ${SV_LIB_MODULE_SEGMENTATION_NAME} ${SV_LIB_MODULE_MODEL_NAME} ${SV_LIB_MODULE_MESH_NAME} ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  )
else()
  simvascular_create_plugin(
    EXPORT_DIRECTIVE SV_QT_DATAMANAGER
    MODULE_DEPENDS ${MITK_LIBRARIES} ${SV_LIB_MODULE_PATH_NAME} ${SV_LIB_MODULE_SEGMENTATION_NAME} ${SV_LIB_MODULE_MODEL_NAME} ${SV_LIB_MODULE_MESH_NAME} ${SV_LIB_MODULE_PROJECTMANAGEMENT_NAME}
  )
endif()

if(SV_INSTALL_LIBS)
    install(TARGETS ${lib}
        RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR}/plugins COMPONENT CoreExecutables
        LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR}/plugins COMPONENT CoreLibraries
        ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR}/plugins COMPONENT CoreLibraries
    )
endif()
