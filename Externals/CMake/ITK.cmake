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
#
#-----------------------------------------------------------------------------
# ITK
set(proj ITK)

# Dependencies
set(${proj}_DEPENDENCIES "VTK")
if(${SV_EXTERNALS_BUILD_GDCM})
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "GDCM")
endif()

# Git info
set(SV_EXTERNALS_${proj}_GIT_URL "${SV_EXTERNALS_GIT_URL}/ITK.git" CACHE STRING "Location of ${proj}, can be web address or local path")
mark_as_advanced(SV_EXTERNALS_${proj}_GIT_URL)
set(SV_EXTERNALS_${proj}_GIT_TAG "v${SV_EXTERNALS_${proj}_VERSION}" CACHE STRING "Tag for ${proj}")
mark_as_advanced(SV_EXTERNALS_${proj}_GIT_TAG)

#If using Qt
if(SV_EXTERNALS_USE_QT)
  #MINGW specific flags
  if(MINGW)
    list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
      -DCMAKE_USE_WIN32_THREADS:BOOL=ON
      -DCMAKE_USE_PTHREADS:BOOL=OFF
      )
  endif()
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DUSE_WRAP_ITK:BOOL=OFF
    -DModule_ITKOpenJPEG:BOOL=ON
    )
endif()

#If using GDCM
if(SV_EXTERNALS_BUILD_GDCM)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DGDCM_DIR:PATH=${SV_EXTERNALS_GDCM_CMAKE_DIR}
    )
endif()

# Add external project
ExternalProject_Add(${proj}
  GIT_REPOSITORY ${SV_EXTERNALS_${proj}_GIT_URL}
  GIT_TAG ${SV_EXTERNALS_${proj}_GIT_TAG}
  PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
  SOURCE_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
  BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
  DEPENDS ${${proj}_DEPENDENCIES}
  UPDATE_COMMAND ""
  CMAKE_CACHE_ARGS
    -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
    -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
    -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
    -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DCMAKE_MACOSX_RPATH:BOOL=ON
    -DBUILD_SHARED_LIBS:BOOL=${SV_EXTERNALS_BUILD_${proj}_SHARED}
    -DBUILD_EXAMPLES:BOOL=OFF
    -DBUILD_TESTING:BOOL=OFF
    -DITK_USE_SYSTEM_GDCM:BOOL=${SV_EXTERNALS_BUILD_GDCM}
    -DITK_WRAP_PYTHON:BOOL=OFF
    -DITK_LEGACY_SILENT:BOOL=OFF
    -DModule_ITKReview:BOOL=ON
    -DModule_ITKVtkGlue:BOOL=ON
    -DVTK_DIR:PATH=${SV_EXTERNALS_VTK_CMAKE_DIR}
    -DCMAKE_INSTALL_PREFIX:STRING=${SV_EXTERNALS_${proj}_BIN_DIR}
    ${SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS}
  )

# ITK variables needed later on
set(SV_EXTERNALS_${proj}_CMAKE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/lib/cmake/ITK-${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION})
set(SV_EXTERNALS_${proj}_INCLUDE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/include)
