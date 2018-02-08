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
# Qt
set(proj Qt)

# Dependencies
set(${proj}_DEPENDENCIES "")

# Git info
set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_ORIGINALS_URL}/qt/qt-everywhere-opensource-src-${SV_EXTERNALS_${proj}_VERSION}.tar.gz" CACHE STRING "Location of ${proj}, can be web address or local path")
mark_as_advanced(SV_EXTERNALS_${proj}_SOURCE_URL)

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
  -openssl
  )

if(APPLE)
  if (SV_EXTERNALS_MAC_PACKAGE_MANAGER STREQUAL HOMEBREW)
    set(OPENSSL_ROOT "/usr/local/opt/openssl")
  elseif(SV_EXTERNALS_MAC_PACKAGE_MANAGER STREQUAL MACPORTS)
    set(OPENSSL_ROOT "/opt/local")
  endif()

  set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
    ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    -openssl
    -openssl-linked
    -I${OPENSSL_ROOT}/include
    -L${OPENSSL_ROOT}/lib
    -lssl
    )

endif()

if(LINUX)
  set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
    ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    -qt-xcb
    )
endif()

#Patch for lalr.cpp
if("${COMPILER_VERSION}" STREQUAL "Clang")
  set(SV_EXTERNALS_${proj}_CUSTOM_PATCH patch -N -p1 -i ${SV_EXTERNALS_CMAKE_DIR}/Patch/patch-qt-5.4.2-clang-7.0.patch)
else()
  set(SV_EXTERNALS_${proj}_CUSTOM_PATCH "")
endif()

# Add external project
if(SV_EXTERNALS_DOWNLOAD_${proj})
  ExternalProject_Add(${proj}
    URL ${SV_EXTERNALS_${proj}_BINARIES_URL}
    PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    SOURCE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}
    BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
    DEPENDS ${${proj}_DEPENDENCIES}
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    UPDATE_COMMAND ""
    )
else()
  ExternalProject_Add(${proj}
    URL ${SV_EXTERNALS_${proj}_SOURCE_URL}
    PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    SOURCE_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
    BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
    DEPENDS ${${proj}_DEPENDENCIES}
    PATCH_COMMAND ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
    CONFIGURE_COMMAND ${SV_EXTERNALS_${proj}_SRC_DIR}/configure ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    UPDATE_COMMAND ""
    )
endif()

# Qt variables needed later on
set(SV_EXTERNALS_${proj}_QMAKE_EXECUTABLE ${SV_EXTERNALS_${proj}_BIN_DIR}/bin/qmake)
set(SV_EXTERNALS_${proj}_TOPLEVEL_CMAKE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/lib/cmake)
set(SV_EXTERNALS_${proj}_CMAKE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/lib/cmake/Qt5)

# Qt externals dirs also needed
  #Find Qt!
set(SV_EXTERNALS_Qt5_COMPONENTS
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
#-----------------------------------------------------------------------------


