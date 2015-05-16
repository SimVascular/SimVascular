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
set(proj ITK)

set(${proj}_DEPENDENCIES VTK)

ExternalProject_Include_Dependencies(${proj}
  PROJECT_VAR proj
  DEPENDS_VAR ${proj}_DEPENDENCIES
  EP_ARGS_VAR ${proj}_EXTERNAL_PROJECT_ARGS
  USE_SYSTEM_VAR ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj}
  )


# Sanity checks
if(DEFINED ITK_DIR AND NOT EXISTS ${ITK_DIR})
  message(FATAL_ERROR "ITK_DIR variable is defined but corresponds to non-existing directory")
endif()


if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})


  set(revision_tag "v4.5.0")
  set(location_args GIT_REPOSITORY "${git_protocol}://itk.org/ITK.git"
    GIT_TAG ${revision_tag})
  if(WIN32)
    set(${proj}_OUTPUT_DIR ${CMAKE_BINARY_DIR}/kw/${proj} 
       CACHE PATH "On windows, there is a bug with ITK source code directory path length, you can change this path to avoid it")
    set(${proj}_OUTPUT_BIN_DIR ${CMAKE_BINARY_DIR}/kw/${proj}-b  
      CACHE PATH "On windows, there is a bug with ITK source code directory path length, you can change this path to avoid it")
  else()
    set(${proj}_OUTPUT_DIR ${CMAKE_BINARY_DIR}/kw/${proj})
    set(${proj}_OUTPUT_BIN_DIR ${CMAKE_BINARY_DIR}/kw/${proj}-b)
  endif()
  ExternalProject_Add(${proj}
   ${location_args}
   PREFIX ${${proj}_OUTPUT_DIR}-p
   SOURCE_DIR ${${proj}_OUTPUT_DIR}
   BINARY_DIR ${${proj}_OUTPUT_BIN_DIR}
   UPDATE_COMMAND ""
   CMAKE_CACHE_ARGS
   -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
   -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
   -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
   -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
   -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
   -DCMAKE_THREAD_LIBS:STRING=-lpthread
   -DBUILD_EXAMPLES:BOOL=OFF
   -DBUILD_SHARED_LIBS:BOOL=OFF
   -DBUILD_TESTING:BOOL=OFF
   -DModule_ITKReview:BOOL=ON
   -DModule_ITKVtkGlue:BOOL=ON
   -DVTK_DIR:PATH=${VTK_DIR}
   -DCMAKE_INSTALL_PREFIX:STRING=${SIMVASCULAR_INSTALL_ROOT_DIR}
   -DITK_INSTALL_RUNTIME_DIR:PATH=${SIMVASCULAR_INSTALL_ITK_RUNTIME_DIR}
   -DITK_INSTALL_LIBRARY_DIR:PATH=${SIMVASCULAR_INSTALL_ITK_LIBRARY_DIR}
   -DITK_INSTALL_ARCHIVE_DIR:PATH=${SIMVASCULAR_INSTALL_ITK_ARCHIVE_DIR}
   -DITK_INSTALL_INCLUDE_DIR:PATH=${SIMVASCULAR_INSTALL_ITK_INCLUDE_DIR}
   INSTALL_COMMAND ""
   DEPENDS
   ${${proj}_DEPENDENCIES}
   )
set(${proj}_SOURCE_DIR ${${proj}_OUTPUT_DIR})
set(${proj}_DIR ${${proj}_OUTPUT_BIN_DIR})

else()
  ExternalProject_Add_Empty(${proj} DEPENDS ${${proj}_DEPENDENCIES})
endif()
if(SIMVASCULAR_INSTALL_EXTERNALS)
  ExternalProject_Install_CMake(${proj})
endif()
mark_as_superbuild(${proj}_SOURCE_DIR:PATH)

mark_as_superbuild(
  VARS ${proj}_DIR:PATH
  LABELS "FIND_PACKAGE"
  )



