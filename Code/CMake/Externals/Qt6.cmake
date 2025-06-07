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

# Note that the project name must match the SV_*_DIR variables
# names used to set the external package direcvtory.

set(proj Qt6)

set(msg, "[Code/CMake/Externals/Qt6.cmake] ")
message(STATUS "=============== Code/CMake/Externals    Qt6.cmake ===============")
message(STATUS "${msg} proj: ${proj}")
message(STATUS "${msg} SV_USE_Qt6: ${SV_USE_Qt6}")
message(STATUS "${msg} SV_Qt6_DIR: ${SV_Qt6_DIR}")
message(STATUS "${msg} Qt6_DIR: ${Qt6_DIR}") 
message(STATUS "${msg} SV_EXTERNALS_USE_PREBUILT_QT: ${SV_EXTERNALS_USE_PREBUILT_QT}") 
message(STATUS "${msg} SV_EXTERNALS_PREBUILT_QT_PATH: ${SV_EXTERNALS_PREBUILT_QT_PATH}") 
message(STATUS "${msg} SV_USE_MITK_CONFIG: ${SV_USE_MITK_CONFIG}") 
#message(FATAL_ERROR "${msg} SV_USE_MITK_CONFIG: ${SV_USE_MITK_CONFIG}") 

if(SV_USE_${proj})
  message(STATUS "${msg} SV_USE_${proj} true")

  if(SV_EXTERNALS_USE_PREBUILT_QT)
    set(Qt6_DIR ${SV_EXTERNALS_PREBUILT_QT_PATH} CACHE PATH "Force ${proj} dir to prebuilt Qt" FORCE)
    set(SV_Qt6_DIR ${SV_EXTERNALS_PREBUILT_QT_PATH} CACHE PATH "Force ${proj} dir to prebuilt Qt" FORCE)
  endif()

  message(STATUS "${msg} Qt6_DIR: ${Qt6_DIR}") 
  message(STATUS "${msg} SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR: ${SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR}") 

  # If using toplevel dir, foce Qt_DIR to be the SV_Qt_DIR set by the
  # simvascular_add_new_external macro
  #
  if(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)

    if(WIN32)
      if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.01")
        set(${proj}_DIR "${SV_${proj}_DIR}/msvc2013_64_opengl/lib/cmake/Qt6" CACHE 
            PATH "Force ${proj} dir to externals" FORCE)
      elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.05")
        set(${proj}_DIR "${SV_${proj}_DIR}/${Qt6_VERSION}/msvc2015_64/lib/cmake/Qt6" CACHE 
            PATH "Force ${proj} dir to externals" FORCE)
      elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.02")
        set(${proj}_DIR "${SV_${proj}_DIR}/${Qt6_VERSION}/msvc2015_64/lib/cmake/Qt6" CACHE 
            PATH "Force ${proj} dir to externals" FORCE)
      elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2019.06")
        set(${proj}_DIR "${SV_${proj}_DIR}/${Qt6_VERSION}/msvc2017_64/lib/cmake/Qt6" CACHE 
            PATH "Force ${proj} dir to externals" FORCE)
      else()
        message(FATAL_ERROR "Invalid SV_EXTERNALS_VERSION_NUMBER ${SV_EXTERNALS_VERSION_NUMBER}")
      endif()

    elseif(LINUX)
      if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.01")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt6 CACHE PATH "Force ${proj} dir to externals" FORCE)
      elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.05")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt6 CACHE PATH "Force ${proj} dir to externals" FORCE)
      elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.02")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt6 CACHE PATH "Force ${proj} dir to externals" FORCE)
      elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2019.06")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt6 CACHE PATH "Force ${proj} dir to externals" FORCE)
      else()
	   message(FATAL_ERROR "Invalid SV_EXTERNALS_VERSION_NUMBER ${SV_EXTERNALS_VERSION_NUMBER}")
      endif()

    elseif(APPLE)
      if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.01")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt6 CACHE PATH "Force ${proj} dir to externals" FORCE)
      elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.05")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt6 CACHE PATH "Force ${proj} dir to externals" FORCE)        
      elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.02")
          set(${proj}_DIR ${SV_${proj}_DIR}/${Qt6_VERSION}/clang_64/lib/cmake/Qt6 CACHE PATH "Force ${proj} dir to externals" FORCE)
      elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2019.06")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/Qt6 CACHE PATH "Force ${proj} dir to externals" FORCE)
          message(STATUS "${msg} >>> Set Qt6_DIR from ${SV_${proj}_DIR}/lib/cmake/Qt6") 
          message(STATUS "${msg}     Qt6_DIR: ${Qt6_DIR}") 
      else()
	   message(FATAL_ERROR "Invalid SV_EXTERNALS_VERSION_NUMBER ${SV_EXTERNALS_VERSION_NUMBER}")
      endif()

    else()
      message(FATAL_ERROR "Unknown Platform for Qt6.cmake script")
    endif()

    if(SV_EXTERNALS_USE_PREBUILT_QT)
      set(Qt6_DIR ${SV_EXTERNALS_PREBUILT_QT_PATH} CACHE PATH "Force ${proj} dir to prebuilt Qt" FORCE)
    endif()

    if(WIN32)
      get_filename_component(_win32_qt5_top_path "${${proj}_DIR}/../../../" ABSOLUTE)
      set(${proj}_DLL_PATH "${_win32_qt5_top_path}/bin" CACHE PATH "Force Qt DLL Path" FORCE)
    endif()

    message(STATUS "${msg} Qt6_DIR: ${Qt6_DIR}") 
    message(STATUS "${msg} Qt6 version: ${proj}_VERSION")
    
    set(SV_${proj}_COMPONENTS
      Concurrent
      Core
      Designer
      Gui
      Help
      OpenGL
      PrintSupport
      #dp Script
      Sql
      Svg
      Widgets
      Xml
      #dp XmlPatterns
      UiTools
      Core5Compat
    )

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
    else()
      list(APPEND SV_${proj}_COMPONENTS
        WebEngineCore
        WebEngineWidgets
        WebView
        )
    endif()

    # Find Qt
    #
    message(STATUS "${msg} Find Qt6 using simvascular_external ...") 
    message(STATUS "${msg} Qt6 version: ${${proj}_VERSION}") 

    # This is needed to find WebEngine (?).
    set(QT_HOST_PATH "${SV_Qt6_DIR}")
    message(STATUS "${msg} QT_HOST_PATH: ${QT_HOST_PATH}")

    simvascular_external(${proj}
      SHARED_LIB ${SV_USE_${proj}_SHARED}
      VERSION ${${proj}_VERSION}
      COMPONENTS ${SV_${proj}_COMPONENTS}
      REQUIRED
      )

    message(STATUS "${msg} Qt6_DIR: ${Qt6_DIR}")
    message(STATUS "${msg} QT_INCLUDE_DIRS: ${QT_INCLUDE_DIRS}")

    # Get toplevel Qt dir from location of config file
    #
    if(Qt6_DIR)
      get_filename_component(_Qt6_DIR "${Qt6_DIR}/../../../" ABSOLUTE)
      list(FIND CMAKE_PREFIX_PATH "${_Qt6_DIR}" _result)

      if(_result LESS 0)
        set(CMAKE_PREFIX_PATH "${_Qt6_DIR};${CMAKE_PREFIX_PATH}" CACHE PATH "" FORCE)
      endif()

      set(QT_PLUGIN_PATH "${_Qt6_DIR}/plugins")
  

      if(WIN32)
        set(${proj}_DLL_PATH "${_Qt6_DIR}/bin" CACHE PATH "Force Qt DLL Path" FORCE)
      endif()
    endif()

    #target_link_libraries(SV Qt6::Core Qt6::Gui Qt6::Widgets )

    # Need to set include dirs and libraries of Qt from individual components
    #
    if(NOT SV_USE_MITK_CONFIG)
      set(QT_LIBRARIES "")
      set(QT_INCLUDE_DIRS "")

      foreach(comp ${SV_Qt6_COMPONENTS})
        if(Qt6${comp}_LIBRARIES)
          set(QT_LIBRARIES ${QT_LIBRARIES} ${Qt6${comp}_LIBRARIES})
        endif()
        if(Qt6${comp}_INCLUDE_DIRS)
          set(QT_INCLUDE_DIRS ${QT_INCLUDE_DIRS} ${Qt6${comp}_INCLUDE_DIRS})
        endif()
      endforeach()

      set(QT_LIBRARIES ${QT_LIBRARIES} Qt6::Core5Compat) 
      include_directories(${QT_INCLUDE_DIRS})

    endif()

  endif()

endif()

message(STATUS "----- Done Code/CMake/Externals Qt6.cmake")
#message(FATAL_ERROR "----- Done Code/CMake/Externals Qt6.cmake")

#-----------------------------------------------------------------------------
