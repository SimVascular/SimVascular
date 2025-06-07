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

set(proj HDF5)

set(msg "[Code/CMake/Externals/HDF5.cmake] ")      
message(STATUS "${msg} =============== Code/CMake/Externals    HDF5.cmake ===============")
message(STATUS "${msg} proj: ${proj}")
message(STATUS "${msg} SV_HDF5_DIR: ${SV_HDF5_DIR}")
message(STATUS "${msg} HDF5_DIR: ${HDF5_DIR}")
message(STATUS "${msg} SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR: ${SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR}")

if(SV_USE_${proj})

  # If using toplevel dir, foce HDF5_DIR to be the SV_HDF5_DIR set by the
  # simvascular_add_new_external macro
  #
  if(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)
      if(WIN32)
        set(${proj}_DIR ${SV_${proj}_DIR}/cmake CACHE PATH "Force ${proj} dir to externals" FORCE)
      else()
        set(${proj}_DIR ${SV_${proj}_DIR}/cmake CACHE PATH "Force ${proj} dir to externals" FORCE)
      endif()
      if(WIN32)
        set(${proj}_DLL_PATH "${SV_${proj}_DIR}/bin" CACHE PATH "Force HDF5 DLL Path" FORCE)
      endif()
  endif()

  # For itk which manually sets HDF5 dir and we need to override
  set(CMAKE_PREFIX_PATH "${HDF5_DIR};${CMAKE_PREFIX_PATH}" CACHE PATH "" FORCE)
  message(STATUS "${msg} Find HDF5 ... ")

  # Find HDF5
  simvascular_external(${proj}
    SHARED_LIB ${SV_USE_${proj}_SHARED}
    VERSION ${${proj}_VERSION}
    REQUIRED
    )
  message(STATUS "${msg} --- Done find HDF5 ")
  message(STATUS "${msg} HDF5_DIR: ${HDF5_DIR}")

  # Set SV_HDF5_DIR to the directory that was found to contain HDF5
  #if(NOT LINUX)
  #  set(SV_${proj}_DIR ${${proj}_DIR})
  #endif()

endif()

message(STATUS "${msg} ----- Done Code/CMake/Externals    HDF5.cmake ")
