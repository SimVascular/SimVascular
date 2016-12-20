## =============================================================================
##  This file is part of the mmg software package for the tetrahedral
##  mesh modification.
##  Copyright (c) Inria - IMB (Université de Bordeaux) - LJLL (UPMC), 2004- .
##
##  mmg is free software: you can redistribute it and/or modify it
##  under the terms of the GNU Lesser General Public License as published
##  by the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  mmg is distributed in the hope that it will be useful, but WITHOUT
##  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
##  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
##  License for more details.
##
##  You should have received a copy of the GNU Lesser General Public
##  License and of the GNU General Public License along with mmg (in
##  files COPYING.LESSER and COPYING). If not, see
##  <http://www.gnu.org/licenses/>. Please read their terms carefully and
##  use this copy of the mmg distribution only if you accept them.
## =============================================================================
#
# This package define the MMG_INCLUDE_DIR and the MMG_LIBRARY variables.  To
# link with the mmg library using CMake add the following lines to your
# CMakeLists.txt:
#
# INCLUDE(FindMmg.cmake)
#
# INCLUDE_DIRECTORIES(${MMG_INCLUDE_DIR})
#
# TARGET_LINK_LIBRARIES( ${YOUR_TARGET} ${MMG_LIBRARY})


if((NOT WIN32) AND (NOT WIN64))
  set( MMG_INCLUDE_DIR MMG_INCLUDE_DIR-NOTFOUND )
  set( MMG_LIBRARY MMG_LIBRARY-NOTFOUND )
endif()

find_path(MMG_INCLUDE_DIR
  NAMES mmg/libmmg.h
  HINTS ${MMG_INCLUDE_DIR}
  $ENV{MMG_INCLUDE_DIR}
  $ENV{HOME}/include/
  ${MMG_DIR}/include/
  ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_MMG_BIN_DIR}/include
  ${MMG_DIR}/simvascular/include/
  $ENV{MMG_DIR}/include/
  PATH_SUFFIXES
  DOC "Directory of mmg Headers")

# Check for mmg library
find_library(MMG_LIBRARY
  NAMES mmg mmg${MMG_LIB_SUFFIX}
  HINTS ${MMG_LIBRARY}
  $ENV{MMG_LIBRARY}
  $ENV{HOME}/lib
  ${MMG_DIR}/lib
  ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_MMG_BIN_DIR}/lib
  ${MMG_DIR}/simvascular/lib
  $ENV{MMG_DIR}/lib
  DOC "The mmg library"
  )

set(MMG_DIR "${MMG_DIR}" CACHE PATH "Path to top level libraries.  Specify this if MMG cannot be found.")
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MMG
	FOUND_VAR MMG_FOUND
	REQUIRED_VARS MMG_INCLUDE_DIR MMG_LIBRARY
	FAIL_MESSAGE "Could NOT find MMG")

