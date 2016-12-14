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
# sv_externals_add_new_external
# \brief Create new external and set variables with default values based on inputs
macro(sv_externals_add_new_external proj version use shared dirname)
  option(SV_EXTERNALS_BUILD_${proj} "Enable ${proj} Plugin" ${use})
  option(SV_EXTERNALS_BUILD_${proj}_SHARED "Build ${proj} libraries as shared libs" ${shared})

# Version
  set(SV_EXTERNALS_${proj}_VERSION "${version}" CACHE TYPE STRING)
  mark_as_advanced(SV_EXTERNALS_${proj}_VERSION)
  sv_externals_get_major_minor_version(${SV_EXTERNALS_${proj}_VERSION} SV_EXTERNALS_${proj}_MAJOR_VERSION SV_EXTERNALS_${proj}_MINOR_VERSION)

# Platform
if(APPLE)
  set(SV_EXTERNALS_PLATFORM_DIR "mac_osx")
elseif(LINUX)
  set(SV_EXTERNALS_PLATFORM_DIR "linux")
elseif(WIN64)
  set(SV_EXTERNALS_PLATFORM_DIR "windows")
else()
  set(SV_EXTERNALS_PLATFORM_DIR "unsupported")
endif()

# Src, bin, build, prefic dirs
  set(${proj}_VERSION_DIR ${dirname}-${SV_EXTERNALS_${proj}_VERSION})
  set(SV_EXTERNALS_${proj}_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_SRC_DIR}/${${proj}_VERSION_DIR})
  if(NOT "${ARGN}" STREQUAL "")
    set(SV_EXTERNALS_${proj}_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_BIN_DIR}/${ARGN}-${SV_EXTERNALS_${proj}_VERSION})
  else()
    set(SV_EXTERNALS_${proj}_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_BIN_DIR}/${${proj}_VERSION_DIR})
  endif()
  set(SV_EXTERNALS_${proj}_BLD_DIR ${SV_EXTERNALS_TOPLEVEL_BLD_DIR}/${${proj}_VERSION_DIR})
  set(SV_EXTERNALS_${proj}_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_PFX_DIR}/${${proj}_VERSION_DIR})

# Install dirs
  if(NOT SV_EXTERNALS_INSTALL_${proj}_RUNTIME_DIR)
    set(SV_EXTERNALS_INSTALL_${proj}_RUNTIME_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/bin)
  endif()

  if(NOT SV_EXTERNALS_INSTALL_${proj}_LIBRARY_DIR)
    set(SV_EXTERNALS_INSTALL_${proj}_LIBRARY_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/lib)
  endif()

  if(NOT SV_EXTERNALS_INSTALL_${proj}_ARCHIVE_DIR)
    set(SV_EXTERNALS_INSTALL_${proj}_ARCHIVE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/lib)
  endif()

  if(NOT SV_EXTERNALS_INSTALL_${proj}_INCLUDE_DIR)
    set(SV_EXTERNALS_INSTALL_${proj}_INCLUDE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/include)
  endif()

# Add to externals list
  if(SV_EXTERNALS_BUILD_${proj})
    list(APPEND SV_EXTERNALS_LIST ${proj})
  endif()
  if(SV_EXTERNALS_LIST)
    list(REMOVE_DUPLICATES SV_EXTERNALS_LIST)
  endif()

  # Add install step for each external
  sv_externals_today(TODAY)
  set(SV_EXTERNALS_${proj}_TAR_INSTALL_NAME ${SV_EXTERNALS_PLATFORM_DIR}.${SV_EXTERNALS_COMPILER_DIR}.${SV_EXTERNALS_ARCH_DIR}.${${proj}_VERSION_DIR}-BUILD${TODAY})
  if(EXISTS "${SV_EXTERNALS_TAR_INSTALL_DIR}")
    install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E tar -czvf ${SV_EXTERNALS_TAR_INSTALL_DIR}/${SV_EXTERNALS_${proj}_TAR_INSTALL_NAME}.tar.gz ${SV_EXTERNALS_${proj}_BIN_DIR}
      WORKING_DIRECTORY ${SV_EXTERNALS_TOPLEVEL_BIN_DIR})")
  endif()
endmacro()

#-----------------------------------------------------------------------------
# sv_externals_get_major_minor_version
# \brief Return major and minor version of version string
macro(sv_externals_get_major_minor_version version major_version minor_version)
  string(REPLACE "." ";" version_list ${version})
  list(GET version_list 0 ${major_version})
  list(GET version_list 1 ${minor_version})
endmacro()

#-----------------------------------------------------------------------------
# Gets the date and returns it
macro(sv_externals_today RESULT)
  if(WIN32)
    execute_process(COMMAND "cmd" " /C date /T" OUTPUT_VARIABLE ${RESULT})
    string(REGEX REPLACE "(..)/(..)/(....).*" "\\3-\\2-\\1" ${RESULT}
      ${${RESULT}})
  elseif(UNIX)
    execute_process(COMMAND "date" "+%d/%m/%Y" OUTPUT_VARIABLE ${RESULT})
    string(REGEX REPLACE "(..)/(..)/(....).*" "\\3-\\2-\\1" ${RESULT}
      ${${RESULT}})
  else(WIN32)
    message(SEND_ERROR "date not implemented")
    set(${RESULT} 000000)
  endif(WIN32)
endmacro()

