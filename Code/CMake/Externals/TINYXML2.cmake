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

#-----------------------------------------------------------------------------
# TINYXML2
set(proj TINYXML2)

if(SV_USE_${proj})
  # If using toplevel dir, TINYXML2_DIR to be the SV_TINYXML2_DIR set by the
  # simvascular_add_new_external macro
  if(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)
      if(APPLE)
        if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.01")
          set(${proj}_DIR ${SV_${proj}_DIR}/share/lib/cmake/tinyxml2 CACHE PATH "Force ${proj} dir to externals" FORCE)
        elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2018.05")
          set(${proj}_DIR ${SV_${proj}_DIR}/share/lib/cmake/tinyxml2 CACHE PATH "Force ${proj} dir to externals" FORCE)
	elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.02")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/tinyxml2 CACHE PATH "Force ${proj} dir to externals" FORCE)
	elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2019.06")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/tinyxml2 CACHE PATH "Force ${proj} dir to externals" FORCE)
  elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_GREATER_EQUAL "2022.10")
          set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/tinyxml2 CACHE PATH "Force ${proj} dir to externals" FORCE)
        else()
	  message(FATAL_ERROR "Invalid SV_EXTERNALS_VERSION_NUMBER ${SV_EXTERNALS_VERSION_NUMBER}")
        endif()
      else()
        set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/tinyxml2 CACHE PATH "Force ${proj} dir to externals" FORCE)
      endif()
      if(WIN32)
        set(${proj}_DLL_PATH "${SV_${proj}_DIR}/bin" CACHE PATH "Force Tinyxml2 DLL Path" FORCE)
      endif()
  endif()

  set(tinyxml2_DIR ${${proj}_DIR})

  # No version in tinyxml2 config files, leave version out
  ## Find TINYXML2
  #simvascular_external(${proj}
  #  SHARED_LIB ${SV_USE_${proj}_SHARED}
  #  VERSION ${${proj}_VERSION}
  #  REQUIRED
  #  )

  set(tinyxml2_INCLUDE_DIR "${SV_${proj}_DIR}/include/")

  # Find TINYXML2
  simvascular_external(tinyxml2
    SHARED_LIB ${SV_USE_${proj}_SHARED}
    REQUIRED
    )

  # Set SV_HDF5_DIR to the directory that was found to contain HDF5
  set(SV_${proj}_DIR ${${proj}_DIR})

endif()
#-----------------------------------------------------------------------------
