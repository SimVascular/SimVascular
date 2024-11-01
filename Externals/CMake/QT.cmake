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
#
#-----------------------------------------------------------------------------
# QT
set(proj QT)

message(STATUS "[QT.cmake] ")
message(STATUS "[QT.cmake] -------------------------------------------------------------------------------------")
message(STATUS "[QT.cmake] +++++                               QT.cmake                                         ")
message(STATUS "[QT.cmake] -------------------------------------------------------------------------------------")
message(STATUS "[QT.cmake] proj: ${proj}")
message(STATUS "[QT.cmake] SV_QT_DIR: ${SV_QT_DIR}")

# Dependencies
set(${proj}_DEPENDENCIES "")

# Git info
set(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL "" CACHE STRING "Manual specification of ${proj}, can be web address or local path to tar file")
mark_as_advanced(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)

if(NOT SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_ORIGINALS_URL}/qt/qt-everywhere-opensource-src-${SV_EXTERNALS_${proj}_VERSION}.tar.gz")
else()
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL}")
endif()

set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
  -opensource
  -confirm-license
  -release
  -qt-zlib
  -qt-libpng
  -qt-libjpeg
  -qt-freetype
  -qt-pcre
  -prefix ${SV_EXTERNALS_${proj}_BIN_DIR}
  )

if(APPLE)
  if (SV_EXTERNALS_MAC_PACKAGE_MANAGER STREQUAL HOMEBREW)
    set(OPENSSL_ROOT "/usr/local/opt/openssl")
  elseif(SV_EXTERNALS_MAC_PACKAGE_MANAGER STREQUAL MACPORTS)
    set(OPENSSL_ROOT "/opt/local")
  endif()

  set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
    ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    -sdk macosx${SV_OSX_MAJOR_VERSION}.${SV_OSX_MINOR_VERSION}
    )
  if(SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "5.4.2")
  set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
    ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    -skip webengine
    -openssl
    -openssl-linked
    -I${OPENSSL_ROOT}/include
    -L${OPENSSL_ROOT}/lib
    -lssl
    )
  endif()

endif()

if(LINUX)
  set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
    ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    -qt-xcb
    )
endif()

#Patch for lalr.cpp
if("${COMPILER_VERSION}" STREQUAL "Clang")
  if(SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "5.4.2")
    set(SV_EXTERNALS_${proj}_CUSTOM_PATCH COMMAND patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.01/patch-qt-5.4.2-clang.patch)
  elseif(SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "5.6.0")
    set(SV_EXTERNALS_${proj}_CUSTOM_PATCH COMMAND patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.03/patch-qt-5.6.0-clang.patch)
  elseif(SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "5.6.3")
    set(SV_EXTERNALS_${proj}_CUSTOM_PATCH COMMAND patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.03/patch-qt-5.6.3-clang.patch)
  else()
    set(SV_EXTERNALS_${proj}_CUSTOM_PATCH "")
  endif()

  if ((NOT ("${CMAKE_CXX_COMPILER_VERSION}" LESS "8.0")) AND SV_EXTERNALS_${proj}_VERSION VERSION_LESS "5.6.3")
    set(SV_EXTERNALS_${proj}_CUSTOM_PATCH ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
      COMMAND patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.03/patch-qt-5.6.3-less-clang-8.0-greater.patch)
  elseif((NOT ("${CMAKE_CXX_COMPILER_VERSION}" LESS "8.0")) AND SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "5.6.3")
    set(SV_EXTERNALS_${proj}_CUSTOM_PATCH ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
      COMMAND patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.03/patch-qt-5.6.3-clang-8.0-greater.patch)
  endif()
else()
  set(SV_EXTERNALS_${proj}_CUSTOM_PATCH "")
endif()

# Post install script
if(APPLE AND SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "5.4.2")
  set(SV_EXTERNALS_${proj}_INSTALL_SCRIPT install-qt-mac_osx.sh)
  configure_file(${SV_EXTERNALS_CMAKE_DIR}/Install/${SV_EXTERNALS_${proj}_INSTALL_SCRIPT}.in "${SV_EXTERNALS_${proj}_BIN_DIR}/${SV_EXTERNALS_${proj}_INSTALL_SCRIPT}" @ONLY)
  set(SV_EXTERNALS_${proj}_CUSTOM_INSTALL make install
    COMMAND ${SV_EXTERNALS_${proj}_BIN_DIR}/${SV_EXTERNALS_${proj}_INSTALL_SCRIPT})
else()
  set(SV_EXTERNALS_${proj}_CUSTOM_INSTALL make install)
endif()


message(STATUS "[QT.cmake] Set Qt components ...")

# QT externals dirs also needed
  #Find QT!

set(SV_QT_COMPONENTS
    Concurrent
    Core
    Designer
    Gui
    Help
    OpenGL
    PrintSupport
    #Script
    Sql
    Svg
    Widgets
    WebEngineWidgets
    WebEngineView
    Xml
    XmlPatterns
    UiTools
    )

if(SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "5.4.2")
  list(APPEND SV_EXTERNALS_${proj}_COMPONENTS
    WebKitWidgets
    WebKit
    )
elseif(SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "5.6.3")
  list(APPEND SV_EXTERNALS_${proj}_COMPONENTS
    WebEngineCore
    WebEngineWidgets
    WebView
    )
endif()

message(STATUS "[QT.cmake] Add external project  ...")
set(Qt6_DIR /Users/parkerda/software/qt/qt-src/install)

# Add external project
if(SV_QT_DIR STREQUAL "system")
#if(SV_EXTERNALS_USE_PREBUILT_${proj})
  message(STATUS "[QT.cmake] +++++ Use prebuilt Qt ")

  # Find package

  if(SV_QT_DIR STREQUAL "system")
    message(STATUS "[QT.cmake] Use system Qt 6")
    find_package(Qt6 COMPONENTS Core CoreTools Gui Widgets REQUIRED)
  else()
    message(STATUS "[QT.cmake] Use custom install Qt 6")
    find_package(Qt6 COMPONENTS Core CoreTools Gui Widgets REQUIRED PATHS ${SV_QT_DIR} NO_DEFAULT_PATH)
  endif()

  if(Qt6_FOUND)
    message(STATUS "[QT.cmake] Qt 6 found")
  else()
    message(FATAL_ERROR "[QT.cmake] Qt 6 not found")
  endif()

  #find_package(Qt6 COMPONENTS ${SV_EXTERNALS_QT5_COMPONENTS} REQUIRED PATHS ${SV_QT_DIR})
  #find_package(Qt6 COMPONENTS ${SV_EXTERNALS_QT5_COMPONENTS} REQUIRED)

  message(STATUS "[QT.cmake] Qt6_DIR: ${Qt6_DIR}")
  message(STATUS "[QT.cmake] Qt6Widgets_INCLUDE_DIRS: ${Qt6Widgets_INCLUDE_DIRS}")
  message(STATUS "[QT.cmake] QT_MAKE_EXECUTABLE: ${QT_MAKE_EXECUTABLE}")

  # Create empty qt to satisfy dependencies
  #ExternalProject_Add(${proj}
    #PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}-empty
    #SOURCE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}-empty
    #BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}-empty
    #DEPENDS ${${proj}_DEPENDENCIES}
    #DOWNLOAD_COMMAND ""
    #CONFIGURE_COMMAND ""
    #BUILD_COMMAND ""
    #INSTALL_COMMAND ""
    #)
  message(STATUS "[QT.cmake] ----- Done Use prebuilt Qt ")
  message(STATUS "[QT.cmake] ")

elseif(SV_EXTERNALS_DOWNLOAD_${proj})
  message(STATUS "[QT.cmake] Use downloaded Qt ")
  message(STATUS "[QT.cmake] +++++ ExternalProject_Add ${proj} ...")
  message(STATUS "[QT.cmake] URL ${SV_EXTERNALS_${proj}_BINARIES_URL}")
  message(STATUS "[QT.cmake] PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}") 
  message(STATUS "[QT.cmake] SOURCE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}")
  message(STATUS "[QT.cmake] BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}") 
  message(STATUS "[QT.cmake] DEPENDS ${${proj}_DEPENDENCIES}") 

  #ExternalProject_Add(${proj}
    #URL ${SV_EXTERNALS_${proj}_BINARIES_URL}
    #PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    #SOURCE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}
    #BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
    #DEPENDS ${${proj}_DEPENDENCIES}
    #CONFIGURE_COMMAND ""
    #BUILD_COMMAND ""
    #INSTALL_COMMAND ""
    #UPDATE_COMMAND ""
    #)

  message(STATUS "[QT.cmake] ----- Done ExternalProject_Add ${proj}")

else()

  message(FATAL_ERROR "[QT.cmake] Unknown Qt 6 source")

  #BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR} We have to do an in source build so that qt cmake files populate the private headers

  #ExternalProject_Add(${proj}
    #URL ${SV_EXTERNALS_${proj}_SOURCE_URL}
    #PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    #SOURCE_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
    #BINARY_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
    #DEPENDS ${${proj}_DEPENDENCIES}
    #PATCH_COMMAND ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
    #CONFIGURE_COMMAND ${SV_EXTERNALS_${proj}_SRC_DIR}/configure ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    #INSTALL_COMMAND ${SV_EXTERNALS_${proj}_CUSTOM_INSTALL}
    #UPDATE_COMMAND ""
    #)

endif()

# QT variables needed later on

if(SV_EXTERNALS_USE_PREBUILT_${proj})
  set(SV_EXTERNALS_${proj}_QMAKE_EXECUTABLE ${QT_MAKE_EXECUTABLE})
  get_filename_component(SV_EXTERNALS_${proj}_TOPLEVEL_CMAKE_DIR ${Qt6_DIR} DIRECTORY)
  set(SV_EXTERNALS_${proj}_CMAKE_DIR ${Qt6_DIR})
else()
  set(SV_EXTERNALS_${proj}_QMAKE_EXECUTABLE ${SV_EXTERNALS_${proj}_BIN_DIR}/bin/qmake)
  set(SV_EXTERNALS_${proj}_TOPLEVEL_CMAKE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/lib/cmake)
  set(SV_EXTERNALS_${proj}_CMAKE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/lib/cmake/Qt6)
endif()

message(STATUS "[QT.cmake] ----- Done QT.cmake -----")
message(STATUS "[QT.cmake] ")

#-----------------------------------------------------------------------------


