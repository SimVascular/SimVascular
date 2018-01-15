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

set(SVLS_DEFINITIONS "")
set(SVLS_NEEDED_LIBS svLS)

if(WIN32 OR (CYGWIN AND NOT IS64))
	#no svls for x32
endif()

if(LINUX AND IS64)
	#no SVLS for linux
endif()

if(IS64 AND (WIN32 OR CYGWIN))
unset(SVLS_INCLUDE_DIR)
	set(SVLS_PATH_PREFIX "svls-2013.08.10/win/x64")
	set(SVLS_FULL_PATH "${LicensedLibs_Bin_Directory}${SVLS_PATH_PREFIX}")
	set(SVLS_LIB_DIR "${SVLS_FULL_PATH}" CACHE TYPE PATH)
	set(SVLS_POSSIBLE_INCLUDE_DIR "${SVLS_FULL_PATH}" CACHE TYPE PATH)
endif()

find_path(SVLS_INCLUDE_DIR svLS.h HINTS ${SVLS_POSSIBLE_INCLUDE_DIR})

GENLIBS(SVLS_LIBRARY "${SVLS_NEEDED_LIBS}" "SVLS" "${SVLS_LIB_DIR}")
set(SVLS_LIBRARY ${SVLS_svLS_LIBRARY})

include_directories(${SVLS_INCLUDE_DIR})
link_directories(${SVLS_LIB_DIR})
add_definitions(${SVLS_DEFINITIONS})


