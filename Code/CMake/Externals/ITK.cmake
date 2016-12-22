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
# ITK
set(proj ITK)
if(SV_USE_${proj})
  # ITK resets the vtk dir and variables (very annoying), must set temp vars
  # vtk dir to reset at the end
  set(TEMP_VTK_DIR ${VTK_DIR})
  set(TEMP_VTK_LIBRARIES ${VTK_LIBRARIES})
  # If using toplevel dir, foce ITK_DIR to be the SV_ITK_DIR set by the
  # simvascular_add_new_external macro
  if(SV_EXTERNALS_USE_TOPLEVEL_DIR)
    set(${proj}_DIR ${SV_${proj}_DIR}/lib/cmake/${proj}-${${proj}_MAJOR_VERSION}.${ITK_MINOR_VERSION} CACHE PATH "Force ${proj} dir to externals" FORCE)
  endif()
  # Find ITK
  simvascular_external(${proj} SHARED_LIB ${SV_USE_${proj}_SHARED} VERSION ${${proj}_VERSION})

  # Include cmake file provided by ITK to define libs and include dirs
  include(${${proj}_USE_FILE})

  # Set SV_ITK_DIR to the toplevel ITK if it exists
  simvascular_get_external_path_from_include_dir(${proj})
  # Reset VTK vars
  set(VTK_DIR ${TEMP_VTK_DIR} CACHE PATH "Must reset VTK dir after processing ${proj}" FORCE)
  set(VTK_LIBRARIES ${TEMP_VTK_LIBRARIES})
endif()
#-----------------------------------------------------------------------------
