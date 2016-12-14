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
# TCL
set(proj TCL)

# Dependencies
set(${proj}_DEPENDENCIES "")

# Source URL
set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_STANFORD_URL}/tcltk/tcl${SV_EXTERNALS_${proj}_VERSION}-src.tar.gz" CACHE STRING "Location of ${proj}, can be web address or local path")
mark_as_advanced(SV_EXTERNALS_${proj}_SOURCE_URL)

# Configure options
set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
  --prefix=${SV_EXTERNALS_${proj}_BIN_DIR}
  --enable-threads)
if(SV_EXTERNALS_BUILD_${proj}_SHARED)
  set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
    ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    --enable-shared)
endif()

# Platform specific additions
if(APPLE)
  set(SV_EXTERNALS_${proj}_URL_EXTENSION unix)
  set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
    ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    --enable-corefoundation)
elseif(LINUX)
  set(SV_EXTERNALS_${proj}_URL_EXTENSION unix)
  set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
    ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS})
else()
  set(SV_EXTERNALS_${proj}_URL_EXTENSION win)
  set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
    ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    --enable-64bit)
endif()

# TCL variables needed later on
if(SV_EXTERNALS_BUILD_${proj}_SHARED)
  set(${proj}_LIBRARY_NAME libtcl${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}${CMAKE_SHARED_LIBRARY_SUFFIX})
else()
  set(${proj}_LIBRARY_NAME libtcl${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

set(SV_EXTERNALS_TCLSH_EXECUTABLE ${SV_EXTERNALS_${proj}_BIN_DIR}/bin/tclsh${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION})
set(SV_EXTERNALS_${proj}_INCLUDE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/include)
set(SV_EXTERNALS_${proj}_LIBRARY ${SV_EXTERNALS_${proj}_BIN_DIR}/lib/${${proj}_LIBRARY_NAME})
get_filename_component(SV_EXTERNALS_${proj}_LIBRARY_DIR ${SV_EXTERNALS_${proj}_LIBRARY} DIRECTORY)

# Special install rules
if(APPLE)
  set(SV_EXTERNALS_${proj}_CUSTOM_INSTALL make install
    COMMAND chmod -R u+w,a+rx ${SV_EXTERNALS_${proj}_LIBRARY_DIR}
    COMMAND install_name_tool -id @rpath/${${proj}_LIBRARY_NAME} ${SV_EXTERNALS_${proj}_LIBRARY}
    COMMAND install_name_tool -change ${SV_EXTERNALS_${proj}_LIBRARY} ${${proj}_LIBRARY_NAME} ${SV_EXTERNALS_TCLSH_EXECUTABLE})
else()
  set(SV_EXTERNALS_${proj}_CUSTOM_INSTALL make install)
endif()


# Add external project
ExternalProject_Add(${proj}
  URL ${SV_EXTERNALS_${proj}_SOURCE_URL}
  PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
  SOURCE_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
  BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
  DEPENDS ${${proj}_DEPENDENCIES}
  CONFIGURE_COMMAND CC=${CMAKE_C_COMPILER} ${SV_EXTERNALS_${proj}_SRC_DIR}/${SV_EXTERNALS_${proj}_URL_EXTENSION}/configure -C ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
  UPDATE_COMMAND ""
  )

