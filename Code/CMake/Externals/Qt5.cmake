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
  if(SV_EXTERNALS_USE_PREBUILT_QT)
    set(Qt5_DIR ${SV_EXTERNALS_PREBUILT_QT_PATH} CACHE PATH "Force ${proj} dir to prebuilt Qt" FORCE)
    set(SV_Qt5_DIR ${SV_EXTERNALS_PREBUILT_QT_PATH} CACHE PATH "Force ${proj} dir to prebuilt Qt" FORCE)
  endif()

  
  if(SV_USE_QT_GUI)
    message("Externals/Qt5.cmake Qt5_DIR initial value: ${Qt5_DIR}")
    # If using toplevel dir, foce Qt_DIR to be the SV_Qt_DIR set by the
    # simvascular_add_new_external macro
    if(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)
      if(WIN32)
        if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.01")
           set(${proj}_DIR "${SV_${proj}_DIR}/msvc2013_64_opengl/lib/cmake/Qt5" CACHE PATH "Force ${proj} dir to externals" FORCE)
        elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.05")
           set(${proj}_DIR "${SV_${proj}_DIR}/${Qt5_VERSION}/msvc2015_64/lib/cmake/Qt5" CACHE PATH "Force ${proj} dir to externals" FORCE)
	elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.02")
           set(${proj}_DIR "${SV_${proj}_DIR}/${Qt5_VERSION}/msvc2015_64/lib/cmake/Qt5" CACHE PATH "Force ${proj} dir to externals" FORCE)
	elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2019.06")
           set(${proj}_DIR "${SV_${proj}_DIR}/${Qt5_VERSION}/msvc2017_64/lib/cmake/Qt5" CACHE PATH "Force ${proj} dir to externals" FORCE)
	else()
	   message(FATAL_ERROR "Invalid SV_EXTERNALS_VERSION_NUMBER ${SV_EXTERNALS_VERSION_NUMBER}")
        endif()
      elseif(LINUX)
        if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.01")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt5 CACHE PATH "Force ${proj} dir to externals" FORCE)
        elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.05")
          set(${proj}_DIR ${SV_${proj}_DIR}/${Qt5_VERSION}/gcc_64/lib/cmake/Qt5 CACHE PATH "Force ${proj} dir to externals" FORCE)
	elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.02")
          set(${proj}_DIR ${SV_${proj}_DIR}/${Qt5_VERSION}/gcc_64/lib/cmake/Qt5 CACHE PATH "Force ${proj} dir to externals" FORCE)
	elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2019.06")
          set(${proj}_DIR ${SV_${proj}_DIR}/${Qt5_VERSION}/gcc_64/lib/cmake/Qt5 CACHE PATH "Force ${proj} dir to externals" FORCE)
        else()
	   message(FATAL_ERROR "Invalid SV_EXTERNALS_VERSION_NUMBER ${SV_EXTERNALS_VERSION_NUMBER}")
        endif()
      elseif(APPLE)
        if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.01")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt5 CACHE PATH "Force ${proj} dir to externals" FORCE)
        elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.05")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt5 CACHE PATH "Force ${proj} dir to externals" FORCE)        
	elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.02")
          set(${proj}_DIR ${SV_${proj}_DIR}/${Qt5_VERSION}/clang_64/lib/cmake/Qt5 CACHE PATH "Force ${proj} dir to externals" FORCE)
	elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2019.06")
          set(${proj}_DIR ${SV_${proj}_DIR}/${Qt5_VERSION}/clang_64/lib/cmake/Qt5 CACHE PATH "Force ${proj} dir to externals" FORCE)
        else()
	   message(FATAL_ERROR "Invalid SV_EXTERNALS_VERSION_NUMBER ${SV_EXTERNALS_VERSION_NUMBER}")
        endif()
      else()
        message(FATAL_ERROR "Unknown Platform for Qt5.cmake script")
      endif()
      if(SV_EXTERNALS_USE_PREBUILT_QT)
        set(Qt5_DIR ${SV_EXTERNALS_PREBUILT_QT_PATH} CACHE PATH "Force ${proj} dir to prebuilt Qt" FORCE)
      endif()
      if(WIN32)
        get_filename_component(_win32_qt5_top_path "${${proj}_DIR}/../../../" ABSOLUTE)
        set(${proj}_DLL_PATH "${_win32_qt5_top_path}/bin" CACHE PATH "Force Qt DLL Path" FORCE)
      endif()
    endif()
    
    message("Externals/Qt5.cmake Qt5_DIR final value: ${Qt5_DIR}")
    
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
      Widgets
      Xml
      XmlPatterns
      UiTools)

    if(${proj}_VERSION VERSION_EQUAL "5.4.2")
      list(APPEND SV_${proj}_COMPONENTS
        WebKitWidgets
        WebKit
        )
    elseif(${proj}_VERSION VERSION_EQUAL "5.6.3")
      list(APPEND SV_${proj}_COMPONENTS
        WebEngineCore
        WebEngineWidgets
        WebView
        )
    elseif(${proj}_VERSION VERSION_EQUAL "5.11.3")
      list(APPEND SV_${proj}_COMPONENTS
        WebEngineCore
        WebEngineWidgets
        WebView
        )
    endif()

    # Find Qt
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
      if(WIN32)
        set(${proj}_DLL_PATH "${_Qt5_DIR}/bin" CACHE PATH "Force Qt DLL Path" FORCE)
      endif()
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
