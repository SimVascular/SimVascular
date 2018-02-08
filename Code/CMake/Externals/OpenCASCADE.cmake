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
# OpenCASCADE
set(proj OpenCASCADE)
if(SV_USE_${proj})

  # If using toplevel dir, foce OpenCASCADE_DIR to be the SV_OpenCASCADE_DIR set by the
  # simvascular_add_new_external macro
  if(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)
    if(WIN32)
      set(${proj}_DIR ${SV_${proj}_DIR}/cmake CACHE PATH "Force ${proj} dir to externals" FORCE)
      set(${proj}_DLL_PATH "${SV_${proj}_DIR}/bin" CACHE PATH "Force OpenCASCADE DLL Path")
    else()
      set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/opencascade CACHE PATH "Force ${proj} dir to externals" FORCE)
    endif()
  endif()

  if(NOT ${proj}_DIR)
    set(${proj}_DIR "${proj}_DIR-NOTFOUND" CACHE PATH "Path of toplevel ${proj} dir. Specify this if ${proj} cannot be found.")
    message(FATAL_ERROR "${proj}_DIR was not specified. Set ${proj}_DIR to the build or bin directory containing OpenCASCADEConfig.cmake")
  endif()

  # Find OpenCASCADE
  simvascular_external(${proj}
    SHARED_LIB ${SV_USE_${proj}_SHARED}
    VERSION ${${proj}_VERSION}
    REQUIRED
    )

  # Set SV_OpenCASCADE_DIR to the toplevel OpenCASCADE if it exists
  simvascular_get_external_path_from_include_dir(${proj})

endif()
#-----------------------------------------------------------------------------
