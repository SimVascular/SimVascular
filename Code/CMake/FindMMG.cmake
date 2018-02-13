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

set(proj MMG)

include(FindPackageHandleStandardArgs)
include(CMakeFindFrameworks)

if(NOT ${proj}_DIR)
  set(${proj}_DIR "${proj}_DIR-NOTFOUND" CACHE PATH "Path of toplevel ${proj} dir. Specify this if ${proj} cannot be found.")
  message(FATAL_ERROR "${proj}_DIR was not specified. Set ${proj}_DIR to the toplevel ${proj} dir that contains bin, lib, include")
endif()

set(${proj}_POSSIBLE_INCLUDE_PATHS
  "${${proj}_DIR}/*"
  "${${proj}_DIR}/include/*"
  )

unset(${proj}_INCLUDE_DIR CACHE)
find_path(${proj}_INCLUDE_DIR
  NAMES
  mmg/libmmg.h
  PATHS
  ${${proj}_POSSIBLE_INCLUDE_PATHS}
  NO_DEFAULT_PATH
  )

set(${proj}_POSSIBLE_LIB_PATHS
  "${${proj}_DIR}/*"
  "${${proj}_DIR}/lib/*"
  )

# Check for mmg library
unset(${proj}_LIBRARY CACHE)
find_library(${proj}_LIBRARY
  NAMES
  mmg
  PATHS
  ${${proj}_POSSIBLE_LIB_PATHS}
  )

find_package_handle_standard_args(${proj}
	FOUND_VAR ${proj}_FOUND
	REQUIRED_VARS ${proj}_INCLUDE_DIR ${proj}_LIBRARY
	FAIL_MESSAGE "Could NOT find ${proj}")

mark_as_advanced(
  ${proj}_INCLUDE_DIR
  ${proj}_LIBRARY
  )
