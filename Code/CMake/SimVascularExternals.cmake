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

set(msg "[Code/CMake/SimVascularExternals.cmake] ") 
message(STATUS "${msg} ")
message(STATUS "${msg} ----------------------------------------------------------------------")
message(STATUS "${msg} +++++                     SimVascularExternals.cmake                  ")
message(STATUS "${msg} ----------------------------------------------------------------------")
message(STATUS "${msg} SV_SOURCE_DIR: ${SV_SOURCE_DIR}")
message(STATUS "${msg} SV_EXTERNALS_LIST: ${SV_EXTERNALS_LIST}")

# Process each external in the order they were added to SV_EXTERNALS_LIST
# using simvascular_add_new_external in SimVascularOptions.cmake
#
message(STATUS "${msg} Process externals ") 
message(STATUS "${msg} SV_EXTERNALS_LIST: ${SV_EXTERNALS_LIST}")

foreach(proj ${SV_EXTERNALS_LIST})
  set(msg "[Code/CMake/SimVascularExternals.cmake] ") 
  message(STATUS "${msg} -------------------------------------------------------- ")
  message(STATUS "${msg} foreach proj : ${proj} ")
  message(STATUS "${msg} Use proj: ${SV_USE_${proj}} ")

  if(SV_USE_${proj})

    if(EXISTS "${SV_SOURCE_DIR}/CMake/Externals/${proj}.cmake")
      message(STATUS "${msg} Include ${SV_SOURCE_DIR}/CMake/Externals/${proj}.cmake") 
      include("${SV_SOURCE_DIR}/CMake/Externals/${proj}.cmake")
    else()
      message(STATUS "${msg} **** There is no file ${SV_SOURCE_DIR}/CMake/Externals/${proj}.cmake") 
    endif()

    # Install
    if(SV_USE_${proj}_SHARED AND SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)
      simvascular_install_external(${proj})
    endif()
  endif()
endforeach()

