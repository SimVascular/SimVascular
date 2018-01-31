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
find_path(FREETYPE_INCLUDE_DIR_ft2build ft2build.h
  PATHS
  ${FREETYPE_DIR}
  ${FREETYPE_DIR}/include
  ${FREETYPE_DIR}/include/freetype2
  ${FREETYPE_DIR}/include/freetype2/freetype
  $ENV{FREETYPE_DIR}
  $ENV{FREETYPE_DIR}/include
  $ENV{FREETYPE_DIR}/include/freetype2
  $ENV{FREETYPE_DIR}/include/freetype2/freetype
  NO_DEFAULT_PATH
  PATH_SUFFIXES include
)


find_path(FREETYPE_INCLUDE_DIR_ft2build ft2build.h
  PATHS
  /usr/local/X11R6/include
  /usr/local/X11/include
  /usr/X11/include
  /sw/include
  /opt/local/include
  /opt/X11/include
  /usr/freeware/include
  PATH_SUFFIXES freetype2
)

find_path(FREETYPE_INCLUDE_DIR_freetype2 config/ftheader.h
  PATHS
  ${FREETYPE_DIR}
  ${FREETYPE_DIR}/include
  ${FREETYPE_DIR}/include/freetype2
  ${FREETYPE_DIR}/include/freetype2/freetype
  $ENV{FREETYPE_DIR}
  $ENV{FREETYPE_DIR}/include
  $ENV{FREETYPE_DIR}/include/freetype2
  $ENV{FREETYPE_DIR}/include/freetype2/freetype
  NO_DEFAULT_PATH
)

find_path(FREETYPE_INCLUDE_DIR_freetype2 config/ftheader.h
  PATHS
  ${FREETYPE_DIR}/include
  /usr/local/X11R6/include
  /usr/local/X11/include
  /usr/X11/include
  /sw/include
  /opt/local/include
  /usr/freeware/include
  PATH_SUFFIXES freetype2
)

find_library(FREETYPE_LIBRARY
  NAMES freetype libfreetype freetype219 freetyped
  PATHS
  ${FREETYPE_DIR}
  $ENV{FREETYPE_DIR}
  NO_DEFAULT_PATH
  PATH_SUFFIXES lib64 lib
)

find_library(FREETYPE_LIBRARY
  NAMES freetype libfreetype freetype219
  PATHS
  /usr/local/X11R6
  /usr/local/X11
  /usr/X11
  /sw
  /usr/freeware
  NO_DEFAULT_PATH
  PATH_SUFFIXES lib64 lib
)

# set the user variables
if(FREETYPE_INCLUDE_DIR_ft2build AND FREETYPE_INCLUDE_DIR_freetype2)
  set(FREETYPE_INCLUDE_DIRS "${FREETYPE_INCLUDE_DIR_ft2build};${FREETYPE_INCLUDE_DIR_freetype2}")
endif(FREETYPE_INCLUDE_DIR_ft2build AND FREETYPE_INCLUDE_DIR_freetype2)
set(FREETYPE_LIBRARIES "${FREETYPE_LIBRARY}")

set(FREETYPE_DIR ${FREETYPE_DIR} CACHE PATH "Path to top level libraries.  Specify this if FREETYPE cannot be found.")
# handle the QUIETLY and REQUIRED arguments and set FREETYPE_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FREETYPE
  FOUND_VAR FREETYPE_FOUND
	REQUIRED_VARS FREETYPE_LIBRARY FREETYPE_INCLUDE_DIRS
	VERSION_VAR FREETYPE_VERSION
	FAIL_MESSAGE "Could NOT find FREETYPE:")

get_filename_component(FREETYPE_LIBRARY_DIR ${FREETYPE_LIBRARY} DIRECTORY)


#MARK_AS_ADVANCED(FREETYPE_LIBRARY FREETYPE_INCLUDE_DIR_freetype2 FREETYPE_INCLUDE_DIR_ft2build)
get_filename_component(FREETYPE_LIBRARY_DIR ${FREETYPE_LIBRARY} DIRECTORY)
link_directories(${FREETYPE_LIBRARY_DIR})
