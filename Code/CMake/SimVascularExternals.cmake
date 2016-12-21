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
# QT
# First process Qt which is required to build the qt gui but is very different
# then simvascular's other dependencies. The other externals can be built
# as part of the externals packages of simvascular. Qt is always installed as
# pre-built libs and bins
if(SV_USE_QT_GUI)
  # Qt5 modules to load
  set(SV_QT5_COMPONENTS
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
  # Prefix path helper if needed
  set(CMAKE_PREFIX_PATH "${CMAKE_PREFIX_PATH}" CACHE PATH "")
  # Find Qt
  simvascular_external(Qt5 COMPONENTS ${SV_QT5_COMPONENTS} REQUIRED)
  # Get toplevel Qt dir from location of config file
  if(Qt5_DIR)
    get_filename_component(_Qt5_DIR "${Qt5_DIR}/../../../" ABSOLUTE)
    list(FIND CMAKE_PREFIX_PATH "${_Qt5_DIR}" _result)
    if(_result LESS 0)
      set(CMAKE_PREFIX_PATH "${_Qt5_DIR};${CMAKE_PREFIX_PATH}" CACHE PATH "" FORCE)
    endif()
  endif()
  # Need to set include dirs and libraries of Qt from individual components
  if(NOT SV_USE_MITK_CONFIG)
    set(QT_LIBRARIES "")
    set(QT_INCLUDE_DIRS "")
    foreach(comp ${SV_QT5_COMPONENTS})
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
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Process each external in the order they were added to SV_EXTERNALS_LIST
# using simvascular_add_new_external in SimVascularOptions.cmake
foreach(proj ${SV_EXTERNALS_LIST})
  if(SV_USE_${proj})
    if(EXISTS "${SV_SOURCE_DIR}/CMake/Externals/${proj}.cmake")
      include("${SV_SOURCE_DIR}/CMake/Externals/${proj}.cmake")
    endif()
    # Install
    if(SV_USE_${proj}_SHARED AND SV_EXTERNALS_USE_TOPLEVEL_DIR)
      simvascular_install_external(${proj})
    endif()
  endif()
endforeach()
#-----------------------------------------------------------------------------
