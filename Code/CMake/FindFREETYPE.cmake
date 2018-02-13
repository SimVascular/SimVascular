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

# - Locate FreeType library
# This module defines
#  FREETYPE_LIBRARIES, the library to link against
#  FREETYPE_FOUND, if false, do not try to link to FREETYPE
#  FREETYPE_INCLUDE_DIRS, where to find headers.
#  This is the concatenation of the paths:
#  FREETYPE_INCLUDE_DIR_ft2build
#  FREETYPE_INCLUDE_DIR_freetype2
#
# $FREETYPE_DIR is an environment variable that would
# correspond to the ./configure --prefix=$FREETYPE_DIR
# used in building FREETYPE.

# Created by Eric Wing.
# Modifications by Alexander Neundorf.
# This file has been renamed to "FindFreetype.cmake" instead of the correct
# "FindFreeType.cmake" in order to be compatible with the one from KDE4, Alex.

# Ugh, FreeType seems to use some #include trickery which
# makes this harder than it should be. It looks like they
# put ft2build.h in a common/easier-to-find location which
# then contains a #include to a more specific header in a
# more specific location (#include <freetype/config/ftheader.h>).
# Then from there, they need to set a bunch of #define's
# so you can do something like:
# #include FT_FREETYPE_H
# Unfortunately, using CMake's mechanisms like INCLUDE_DIRECTORIES()
# wants explicit full paths and this trickery doesn't work too well.
# I'm going to attempt to cut out the middleman and hope
# everything still works.

set(proj FREETYPE)

include(FindPackageHandleStandardArgs)
include(CMakeFindFrameworks)

if(NOT ${proj}_DIR)
  set(${proj}_DIR "${proj}_DIR-NOTFOUND" CACHE PATH "Path of toplevel ${proj} dir. Specify this if ${proj} cannot be found.")
  message(FATAL_ERROR "${proj}_DIR was not specified. Set ${proj}_DIR to the toplevel ${proj} dir that contains bin, lib, include")
endif()

set(${proj}_POSSIBLE_INCLUDE_PATHS
  "${${proj}_DIR}/*"
  "${${proj}_DIR}/include/*"
  "${${proj}_DIR}/include/*/*"
  )

unset(${proj}_INCLUDE_DIR_ft2build CACHE)
find_path(${proj}_INCLUDE_DIR_ft2build
  NAMES
  ft2build.h
  PATHS
  ${${proj}_POSSIBLE_INCLUDE_PATHS}
  NO_DEFAULT_PATH
)
mark_as_advanced(${proj}_INCLUDE_DIR_ft2build)

unset(${proj}_INCLUDE_DIR_freetype2 CACHE)
find_path(${proj}_INCLUDE_DIR_freetype2
  NAMES
  config/ftheader.h
  PATHS
  ${${proj}_POSSIBLE_INCLUDE_PATHS}
  NO_DEFAULT_PATH
)
mark_as_advanced(${proj}_INCLUDE_DIR_freetype2)

set(${proj}_POSSIBLE_LIB_PATHS
  "${${proj}_DIR}/*"
  "${${proj}_DIR}/lib/*"
  )

unset(${proj}_LIBRARY CACHE)
find_library(${proj}_LIBRARY
  NAMES
  freetype
  freetype.${${proj}_VERSION}
  freetyped
  freetype.${${proj}_VERSION}d
  PATHS
  ${${proj}_POSSIBLE_LIB_PATHS}
  NO_DEFAULT_PATH
)

# set the user variables
if(${proj}_INCLUDE_DIR_ft2build AND ${proj}_INCLUDE_DIR_freetype2)
  set(${proj}_INCLUDE_DIRS "${${proj}_INCLUDE_DIR_ft2build};${${proj}_INCLUDE_DIR_freetype2}")
endif()

set(${proj}_LIBRARIES "${${proj}_LIBRARY}")

# handle the QUIETLY and REQUIRED arguments and set FREETYPE_FOUND to TRUE if
# all listed variables are TRUE
find_package_handle_standard_args(${proj}
  FOUND_VAR ${proj}_FOUND
	REQUIRED_VARS ${proj}_LIBRARY ${proj}_INCLUDE_DIRS
  VERSION_VAR ${proj}_VERSION_STRING
	FAIL_MESSAGE "Could NOT find ${proj}")

get_filename_component(${proj}_LIBRARY_DIR ${${proj}_LIBRARY} DIRECTORY)

mark_as_advanced(
  ${proj}_INCLUDE_DIRS
  ${proj}_LIBRARY
  ${proj}_LIBRARY_DIR
  )
