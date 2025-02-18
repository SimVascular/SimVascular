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

set(proj FREETYPE)

set(msg "[Code/CMake/Externals/FREETYPE.cmake]")
message(STATUS "${msg} =============== Code/CMake/Externals    FREETYPE.cmake ===============")
message(STATUS "${msg} proj: ${proj}")
message(STATUS "${msg} SV_USE_FREETYPE: ${SV_USE_FREETYPE}")
message(STATUS "${msg} SV_FREETYPE_DIR: ${SV_FREETYPE_DIR}")
message(STATUS "${msg} FREETYPE_VERISON: ${${proj}_VERSION}")
message(STATUS "${msg} SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR: ${SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR}")

if(SV_USE_${proj})

  # If using toplevel dir, force FREETYPE_DIR to be the SV_FREETYPE_DIR set by the
  # simvascular_add_new_external macro
  #
  # [davep] always set this ?
  #
  #if(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)
  #  set(${proj}_DIR ${SV_${proj}_DIR} CACHE PATH "Force ${proj} dir to externals" FORCE)
  #endif()

  set(${proj}_DIR ${SV_${proj}_DIR} CACHE PATH "Force ${proj} dir to externals" FORCE)

  # Find Freetype
  simvascular_external(${proj}
    SHARED_LIB ${SV_USE_${proj}_SHARED}
    VERSION ${${proj}_VERSION}
    REQUIRED
    )

  # Set SV_FREETYPE_DIR to the directory that was found to contain FREETYPE
  #set(SV_${proj}_DIR ${${proj}_DIR})

  link_directories(${${proj}_LIBRARY_DIR})

endif()
#-----------------------------------------------------------------------------

