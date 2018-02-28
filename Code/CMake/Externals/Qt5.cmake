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
# Qt
set(proj Qt5)
if(SV_USE_${proj})

  if(SV_USE_QT_GUI)

    # If using toplevel dir, foce Qt_DIR to be the SV_Qt_DIR set by the
    # simvascular_add_new_external macro
    if(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)
      set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt5 CACHE PATH "Force ${proj} dir to externals" FORCE)
      if(WIN32)
        set(${proj}_DLL_PATH "${SV_${proj}_DIR}/bin" CACHE PATH "Force Qt DLL Path")
      endif()
    endif()

    set(SV_${proj}_COMPONENTS
      Concurrent
      Core
      Designer
      Gui
      Help
      OpenGL
      PrintSupport
      Script
      Sql
      Svg
      WebKitWidgets
      WebKit
      Widgets
      Xml
      XmlPatterns
      UiTools)

    # Find ITK
    simvascular_external(${proj}
      SHARED_LIB ${SV_USE_${proj}_SHARED}
      VERSION ${${proj}_VERSION}
      COMPONENTS ${SV_${proj}_COMPONENTS}
      REQUIRED
      )

    # Get toplevel Qt dir from location of config file
    if(Qt5_DIR)
      get_filename_component(_Qt5_DIR "${Qt5_DIR}/../../../" ABSOLUTE)
      list(FIND CMAKE_PREFIX_PATH "${_Qt5_DIR}" _result)
      if(_result LESS 0)
        set(CMAKE_PREFIX_PATH "${_Qt5_DIR};${CMAKE_PREFIX_PATH}" CACHE PATH "" FORCE)
      endif()
      set(QT_PLUGIN_PATH "${_Qt5_DIR}/plugins")
    endif()
    # Need to set include dirs and libraries of Qt from individual components
    if(NOT SV_USE_MITK_CONFIG)
      set(QT_LIBRARIES "")
      set(QT_INCLUDE_DIRS "")
      foreach(comp ${SV_Qt5_COMPONENTS})
        if(Qt5${comp}_LIBRARIES)
          set(QT_LIBRARIES ${QT_LIBRARIES} ${Qt5${comp}_LIBRARIES})
        endif()
        if(Qt5${comp}_INCLUDE_DIRS)
          set(QT_INCLUDE_DIRS ${QT_INCLUDE_DIRS} ${Qt5${comp}_INCLUDE_DIRS})
        endif()
      endforeach()
      include_directories(${QT_INCLUDE_DIRS})

    endif()

  endif()

endif()

#-----------------------------------------------------------------------------
