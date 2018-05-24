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
# SWIG
set(proj SWIG)

# Dependencies
set(${proj}_DEPENDENCIES "")
if(SV_EXTERNALS_ENABLE_TCL)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "TCL")
endif()
if(SV_EXTERNALS_ENABLE_TCLLIB)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "TCLLIB")
endif()
if(SV_EXTERNALS_ENABLE_TK)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "TK")
endif()
if(SV_EXTERNALS_ENABLE_TKLIB)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "TKLIB")
endif()
if(SV_EXTERNALS_ENABLE_PYTHON)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "PYTHON")
endif()
if(SV_EXTERNALS_ENABLE_PIP)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "PIP")
endif()
if(SV_EXTERNALS_ENABLE_NUMPY)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "NUMPY")
endif()

# Git info
set(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL "" CACHE STRING "Manual specification of ${proj}, can be web address or local path to tar file")
mark_as_advanced(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
if(NOT SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_ORIGINALS_URL}/swig/swig-${SV_EXTERNALS_${proj}_VERSION}.tar.gz")
else()
endif()

set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS "")

if(SV_EXTERNALS_ENABLE_TCL)
  set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
    ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    --prefix=${SV_EXTERNALS_${proj}_BIN_DIR}
    --without-alllang
    --with-tclconfig=${SV_EXTERNALS_TCL_LIBRARY_DIR}
    --with-tcl=${SV_EXTERNALS_TCL_BIN_DIR}
    --with-tclincl=${SV_EXTERNALS_TCL_INCLUDE_DIR}
    --with-tcllib=${SV_EXTERNALS_TCL_LIBRARY_DIR}
    )
endif()
if(SV_EXTERNALS_ENABLE_PYTHON)
  if(SV_EXTERNALS_PYTHON_MAJOR_VERSION VERSION_EQUAL "2")
    set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
      ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
      --with-python=${SV_EXTERNALS_PYTHON_EXECUTABLE}
      )
    if(APPLE)
      set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
        ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
        PYCONFIG=${SV_EXTERNALS_PYTHON_CONFIG_SCRIPT}
        )
    endif()
  elseif(SV_EXTERNALS_PYTHON_MAJOR_VERSION VERSION_EQUAL "3")
    set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
      ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
      --with-python3=${SV_EXTERNALS_PYTHON_EXECUTABLE}
      )
    if(APPLE)
      set(SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS
        ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
        PY3CONFIG=${SV_EXTERNALS_PYTHON_CONFIG_SCRIPT}
        )
    endif()
  endif()
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
    CONFIGURE_COMMAND ${SV_EXTERNALS_${proj}_SRC_DIR}/configure ${SV_EXTERNALS_${proj}_CONFIGURE_OPTIONS}
    UPDATE_COMMAND ""
    )
endif()

# SWIG variables to be used later
set(SV_EXTERNALS_${proj}_EXECUTABLE ${SV_EXTERNALS_${proj}_BIN_DIR}/bin/swig)
