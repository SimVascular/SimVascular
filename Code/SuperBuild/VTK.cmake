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
set(proj VTK)
if(${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})
  set(${proj}_DEPENDENCIES "")
  if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_TCL)
    message(WARNING  "You have specified ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj} but not ${CMAKE_PROJECT_NAME}_USE_SYSTEM_TCL. This is not reccomended, as the libraries may not match, so it may lead to namespace pollution.")
  endif()
else()
  set(${proj}_DEPENDENCIES "TCL")
  if(${${CMAKE_PROJECT_NAME}_USE_PYTHON})
    set(${proj}_DEPENDENCIES ${${proj}_DEPENDENCIES} "PYTHON")
  endif()
endif()

if(SV_USE_QT_GUI)
    if(WIN32)
      option(VTK_USE_SYSTEM_FREETYPE OFF)
    else(WIN32)
      option(VTK_USE_SYSTEM_FREETYPE ON)
    endif(WIN32)
endif()


ExternalProject_Include_Dependencies(${proj} 
  PROJECT_VAR proj
  EP_ARGS_VAR ${proj}_EP_ARGS 
  DEPENDS_VAR ${proj}_DEPENDENCIES)

if(APPLE)
  if(NOT DEFINED TK_INTERNAL_PATH)
    STRING(REGEX REPLACE "/Headers" "/PrivateHeaders" _TK_INTERNAL_PATH ${TK_INCLUDE_PATH})
    set(TK_INTERNAL_PATH "${_TK_INTERNAL_PATH}" CACHE STRING "The path to the Tk internal headers (tkInt.h)")
    set(VTK_TK_INTENAL_PATH_DEFINE  "-DTK_INTERNAL_PATH:PATH=${TK_INTERNAL_PATH}")
  endif()
  list(APPEND EXTERNAL_PROJECT_OPTIONAL_ARGS
    -DTK_INTERNAL_PATH:STRING=${TK_INTERNAL_PATH}
    -DVTK_USE_CARBON:BOOL=OFF
    -DVTK_USE_COCOA:BOOL=ON # Default to Cocoa, VTK/CMakeLists.txt will enable Carbon and disable cocoa if needed
    -DVTK_USE_X:BOOL=OFF
    )
endif()

if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})

  if(WIN32)
    set(${proj}_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_PFX_DIR} 
      CACHE PATH "On windows, there is a bug with VTK source code directory path length, you can change this path to avoid it")
    set(${proj}_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_SRC_DIR} 
      CACHE PATH "On windows, there is a bug with VTK source code directory path length, you can change this path to avoid it")
    set(${proj}_BLD_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BLD_DIR}  
      CACHE PATH "On windows, there is a bug with VTK source code directory path length, you can change this path to avoid it")
    set(${proj}_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR}  
      CACHE PATH "On windows, there is a bug with VTK source code directory path length, you can change this path to avoid it")
  else()
    set(${proj}_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_PFX_DIR})
    set(${proj}_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_SRC_DIR})
    set(${proj}_BLD_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BLD_DIR})
    set(${proj}_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR})
  endif()
  if(WIN32 AND NOT TK_INTERNAL_PATH)
    set(TK_INTERNAL_PATH ${TCL_INCLUDE_PATH}/internals/tk8.6)
    set(VTK_TK_INTENAL_PATH_DEFINE  "-DTK_INTERNAL_PATH:PATH=${TK_INTERNAL_PATH}")
  endif()
  if(WIN32 AND NOT TK_XLIB_PATH)
    set(TK_XLIB_PATH ${TCL_INCLUDE_PATH})
    set(VTK_TK_XLIB_PATH_DEFINE  "-DTK_XLIB_PATH:PATH=${TK_XLIB_PATH}")
  endif()

  set(revision_tag "simvascular-patch-6.2")

  set(additional_cmake_args )
  if(SV_USE_QT_GUI)
      if(MINGW)
        set(additional_cmake_args
            -DCMAKE_USE_WIN32_THREADS:BOOL=ON
            -DCMAKE_USE_PTHREADS:BOOL=OFF
            -DVTK_USE_VIDEO4WINDOWS:BOOL=OFF # no header files provided by MinGW
            )
      endif()
    
      if(WIN32)
        # see http://bugs.mitk.org/show_bug.cgi?id=17858
        list(APPEND additional_cmake_args
             -DVTK_DO_NOT_DEFINE_OSTREAM_SLL:BOOL=ON
             -DVTK_DO_NOT_DEFINE_OSTREAM_ULL:BOOL=ON
            )
      endif()

    list(APPEND additional_cmake_args
        -DVTK_QT_VERSION:STRING=5
        "-DCMAKE_PREFIX_PATH:PATH=${CMAKE_PREFIX_PATH}"
        -DQT_QMAKE_EXECUTABLE:FILEPATH=${QT_QMAKE_EXECUTABLE}
        -DModule_vtkGUISupportQt:BOOL=ON
        -DModule_vtkGUISupportQtWebkit:BOOL=ON
        -DModule_vtkGUISupportQtSQL:BOOL=ON
        -DModule_vtkRenderingQt:BOOL=ON
        #-DModule_vtkFiltersParallelStatistics:BOOL=ON
        -DVTK_Group_Qt:BOOL=ON
     )

      list(APPEND additional_cmake_args
          -DVTK_WINDOWS_PYTHON_DEBUGGABLE:BOOL=OFF
          )

    list(APPEND additional_cmake_args
        -DModule_vtkTestingRendering:BOOL=ON
        -DVTK_MAKE_INSTANTIATORS:BOOL=ON
        -DVTK_USE_SYSTEM_FREETYPE:BOOL=${VTK_USE_SYSTEM_FREETYPE}
     )
     
   #set(revision_tag "simvascular-patch-6.2b")
  endif()

  ExternalProject_Add(${proj}
   ${${proj}_EP_ARGS}
   GIT_REPOSITORY "https://github.com/SimVascular/VTK.git"
   PREFIX ${${proj}_PFX_DIR}
   SOURCE_DIR ${${proj}_SRC_DIR}
   BINARY_DIR ${${proj}_BLD_DIR}
   GIT_TAG ${revision_tag}
   UPDATE_COMMAND ""
   CMAKE_CACHE_ARGS
   -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
   -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
   -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
   -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
   -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
   -DCMAKE_THREAD_LIBS:STRING=-lpthread
   -DCMAKE_MACOSX_RPATH:INTERNAL=1
   -DBUILD_SHARED_LIBS:BOOL=${SV_USE_${proj}_SHARED}
   -DBUILD_TESTING:BOOL=OFF
   -DVTK_WRAP_TCL:BOOL=ON
   -DVTK_WRAP_PYTHON:BOOL=${SV_USE_PYTHON}
   -DVTK_Group_Tk:BOOL=ON
   -DTCL_INCLUDE_PATH:PATH=${TCL_INCLUDE_PATH}
   -DTCL_LIBRARY:FILEPATH=${TCL_LIBRARY}
   -DTCL_TCLSH:FILEPATH=${TCL_TCLSH}
   -DTK_INCLUDE_PATH:PATH=${TK_INCLUDE_PATH}
   ${VTK_TK_XLIB_PATH_DEFINE}
   ${VTK_TK_INTENAL_PATH_DEFINE}
   -DTK_LIBRARY:FILEPATH=${TK_LIBRARY}
   -DPYTHON_EXECUTABLE:FILEPATH=${PYTHON_EXECUTABLE}
   -DPYTHON_INCLUDE_DIR:PATH=${PYTHON_INCLUDE_PATH}
   -DPYTHON_LIBRARIES:FILEPATH=${PYTHON_LIBRARIES}
   -DPYTHON_LIBRARY:FILEPATH=${PYTHON_LIBRARIES}
   -DBUILD_EXAMPLES:BOOL=OFF
   -DCMAKE_INSTALL_PREFIX:STRING=${${proj}_BIN_DIR}
   ${additional_cmake_args}
   )

set(${proj}_SOURCE_DIR ${${proj}_SRC_DIR})

set(SV_${proj}_DIR ${${proj}_BIN_DIR})
set(${proj}_DIR ${${proj}_BIN_DIR}/lib/cmake/vtk-${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION})
mark_as_superbuild(${proj}_DIR})

else()
  # Sanity checks
  if((DEFINED SV_VTK_DIR AND NOT EXISTS ${SV_VTK_DIR})
    AND (DEFINED VTK_DIR AND NOT EXISTS ${VTK_DIR}))
    message(FATAL_ERROR "SV_VTK_DIR and VTK_DIR variable is defined but corresponds to non-existing directory")
  endif()
  ExternalProject_Add_Empty(${proj} DEPENDS ${${proj}_DEPENDENCIES})
endif()
if(SV_INSTALL_EXTERNALS)
  ExternalProject_Install_CMake(${proj})
endif()
mark_as_superbuild(${proj}_SOURCE_DIR:PATH)

mark_as_superbuild(
  VARS SV_${proj}_DIR:PATH
  LABELS "FIND_PACKAGE"
  )
