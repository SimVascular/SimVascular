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

set(proj GDCM)

set(msg "[Code/CMake/Externals/GDCM.cmake]")
message(STATUS "${msg} =============== Code/CMake/Externals    GDCM.cmake ===============")
message(STATUS "${msg} proj: ${proj}")
message(STATUS "${msg} SV_USE_GDCM: ${SV_USE_GDCM}")
message(STATUS "${msg} SV_GDCM_DIR: ${SV_GDCM_DIR}")
message(STATUS "${msg} GDCM_VERISON: ${${proj}_VERSION}")
message(STATUS "${msg} SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR: ${SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR}")
message(STATUS "${msg} PYTHON_LIBRARY: ${PYTHON_LIBRARY}")

if(SV_USE_${proj})

  # If using toplevel dir, foce GDCM_DIR to be the SV_GDCM_DIR set by the
  # simvascular_add_new_external macro
  if(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)
    set(${proj}_DIR ${SV_${proj}_DIR}/lib/gdcm-${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION} CACHE PATH "Force GDCM dir to externals" FORCE)
    if(WIN32)
      set(${proj}_DLL_PATH "${SV_${proj}_DIR}/bin" CACHE PATH "Force GDCM DLL Path")
    endif()
  endif()

  # Find GDCM
  simvascular_external(${proj}
    SHARED_LIB ${SV_USE_${proj}_SHARED}
    VERSION ${${proj}_VERSION}
    REQUIRED
    )

  # Include cmake file provided by GDCM to define libs and include dirs
  include(${${proj}_USE_FILE})

  # Set SV_GDCM_DIR to the toplevel GDCM if it exists
  simvascular_get_external_path_from_include_dir(${proj})
endif()
#-----------------------------------------------------------------------------
