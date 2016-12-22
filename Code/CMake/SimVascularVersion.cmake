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

#-----------------------------------------------------------------------------
# These are SimVascular build and version that are presented to the user to modify
set(SV_RELEASE_TYPE "Release" CACHE STRING "This specificies which install dir and GUIDs to use, is also used in header files")
set_property(CACHE SV_RELEASE_TYPE PROPERTY STRINGS Release Beta)
mark_as_advanced(SV_RELEASE_TYPE)

set(SV_VERSION "simvascular")
if(SV_RELEASE_TYPE MATCHES "^Beta$")
	set(SV_VERSION "simvascular-beta")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Timestamp
string(TIMESTAMP DATE_IMESTAMP %y%m%d)
math(EXPR SV_VERSION_TIMESTAMP "${DATE_IMESTAMP}-140000")
string(TIMESTAMP SV_RELEASE_TIMESTAMP %y%m%d%H%M%S)
set(SV_PLATFORM ${ARCH})
set(SV_MAJOR_VERSION 2)
set(SV_MINOR_VERSION 0)
set(SV_PATCH_VERSION ${SV_VERSION_TIMESTAMP})
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Other helpful sv variables
set(SV_RELEASE_BUILD 0)
set(SV_MAJOR_VER_NO ${SV_MAJOR_VERSION})
set(SV_FULL_VER_NO
	"${SV_MAJOR_VERSION}.${SV_MINOR_VERSION}")
set(SV_FULL_VERSION
	"${SV_MAJOR_VERSION}.${SV_MINOR_VERSION}.${SV_PATCH_VERSION}")
set(SV_REGISTRY_TOPLEVEL "SV")

message(STATUS "SimVascular Version: ${SV_VERSION} ${SV_MAJOR_VERSION}.${SV_MINOR_VERSION}.${SV_PATCH_VERSION}")
#-----------------------------------------------------------------------------
