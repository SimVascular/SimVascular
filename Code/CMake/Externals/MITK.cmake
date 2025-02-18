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

set(proj MITK)

set(msg "[Code/CMake/Externals/MITK.cmake] ")
message(STATUS "${msg} =============== Code/CMake/Externals    MITK.cmake ===============")
message(STATUS "${msg} proj: ${proj}")
message(STATUS "${msg} SV_MITK_DIR: ${SV_MITK_DIR}")
message(STATUS "${msg} MITK_DIR: ${MITK_DIR}")
message(STATUS "${msg} SV_USE_MITK_CONFIG: ${SV_USE_MITK_CONFIG}")
message(STATUS "${msg} SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR: ${SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR}")

if(SV_USE_${proj})

  # If using toplevel dir, foce MITK_DIR to be the SV_MITK_DIR set by the
  # simvascular_add_new_external macro
  #
  if(SV_EXTERNALS_USE_TOPLEVEL_BIN_DIR)
    set(${proj}_DIR ${SV_${proj}_DIR} CACHE PATH "Force ${proj} dir to externals" FORCE)
  endif()
  message(STATUS "${msg} MITK_DIR: ${MITK_DIR}")

  if(SV_USE_${proj}_CONFIG)

    # ITK resets the vtk dir and variables (very annoying), must set temp vars
    # vtk dir to reset at the end
    set(TEMP_VTK_DIR ${VTK_DIR})
    set(TEMP_VTK_LIBRARIES ${VTK_LIBRARIES})

    find_package(${proj} NO_MODULE)

    set(${proj}_LIBRARIES
      MitkCore
      MitkAppUtil
      MitkQtWidgets
      MitkQtWidgetsExt
      MitkMapperExt
      MitkImageDenoising
      MitkSegmentationUI
      MitkSegmentation
      MitkSceneSerialization)

    # Reset VTK vars
    set(VTK_DIR ${TEMP_VTK_DIR} CACHE PATH "Must reset VTK dir after processing ${proj}" FORCE)
    set(VTK_LIBRARIES ${TEMP_VTK_LIBRARIES})

  else()

    message(STATUS "${msg} Don't use MITK config")

    message(STATUS "${msg} Find MITK ... ")

    simvascular_external(${proj}
      SHARED_LIB ${SV_USE_${proj}_SHARED}
      VERSION ${${proj}_VERSION}
      REQUIRED
      )

    message(STATUS "${msg} ----- Done find MITK ")

    # Copy of necessary resource files are in SV CMake dir. usResourceCompiler
    # is in mitk bin directory
    find_package(CppMicroServices)

    # Set SV_MITK_DIR to the toplevel MITK if it exists
    get_filename_component(SV_${proj}_DIR ${${proj}_INCLUDE_DIR} DIRECTORY)

  endif()

endif()

message(STATUS "${msg} ----- Done Code/CMake/Externals    MITK.cmake ")
