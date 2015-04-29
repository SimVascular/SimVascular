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

set(SV_EXTERN_POSSIBLE_DIRS ${USER_HOME_DIR}/sv_extern /sv_extern C:/cygwin/sv_extern/
	/c/cygwin/sv_extern/ c:/sv_extern/)

if(LINUX)
	set(SV_EXTERN_DEFAULT "/sv_extern")
	set(OpenLibs_Bin_REL_PATH "bin/linux/intel_13.0/x64" CACHE PATH "sv extern open lib directory")
	set(LicensedLibs_Bin_REL_PATH "/licensed" CACHE PATH "sv extern licensed lib directory")
endif()

if(CYGWIN AND NOT IS64)
	set(SV_EXTERN_DEFAULT "/sv_extern")
	set(OpenLibs_Bin_REL_PATH "bin/win/vs10sp1/x86")
	set(LicensedLibs_Bin_REL_PATH "/licensed/")
endif()

if(CYGWIN AND IS64)
	set(SV_EXTERN_DEFAULT /sv_extern/)
	set(OpenLibs_Bin_REL_PATH "/sv_extern/bin/win/vs10sp1/x64")
	set(LicensedLibs_Bin_REL_PATH "/licensed/")
endif()

if(WIN32 AND IS64 AND NOT CYGWIN)
	set(SV_EXTERN_DEFAULT "C:/cygwin/sv_extern")
	set(OpenLibs_Bin_REL_PATH "bin/win/vs10sp1/x64/")
	set(LicensedLibs_Bin_REL_PATH "/licensed/")
endif()

if(APPLE)
	set(SV_EXTERN_DEFAULT "${USER_HOME_DIR}/sv_extern")
	set(OpenLibs_Bin_REL_PATH "bin/macos/gnu/x64/")
	set(LicensedLibs_Bin_REL_PATH "/licensed/")
endif()

set(SimVascular_SV_EXTERN_OpenLibs_BIN_DIR 
	${SV_EXTERN_DEFAULT}/${OpenLibs_Bin_REL_PATH} CACHE PATH "Location of")
mark_as_superbuild(SimVascular_SV_EXTERN_OpenLibs_BIN_DIR)

set(SimVascular_SV_EXTERN_LicensedLibs_BIN_DIR 
	${SV_EXTERN_DEFAULT}/${LicensedLibs_Bin_REL_PATH} CACHE PATH "Location of")
mark_as_superbuild(SimVascular_SV_EXTERN_LicensedLibs_BIN_DIR)
