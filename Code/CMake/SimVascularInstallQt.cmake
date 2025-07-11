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

# SimVascularInstallQt.cmake
#
# Add Qt directories and files that will be included
# in an installer using the CMake 'install' command.
#
# Much of the complexity of here is caused by adding
# files needed by QtWebEngineProcess.
#
if(Qt6_DIR)
  get_filename_component(_Qt6_DIR "${Qt6_DIR}/../../../" ABSOLUTE)

  if(${CMAKE_PROJECT_NAME}_ENABLE_DISTRIBUTION)
    set(LIB_DESTINATION "${SV_EXTERNALS_INSTALL_PREFIX}")
  else()
    set(LIB_DESTINATION "${SV_EXTERNALS_INSTALL_PREFIX}/Qt6")
  endif()

  # If we are creating an installer using the CMake cpack command.
  #
  if(${CMAKE_PROJECT_NAME}_ENABLE_DISTRIBUTION)

    if(EXISTS ${_Qt6_DIR}/lib)
      install(DIRECTORY ${_Qt6_DIR}/lib DESTINATION ${LIB_DESTINATION} COMPONENT ExternalLibraries)
    endif()

    if(EXISTS ${_Qt6_DIR}/bin)
      install(DIRECTORY ${_Qt6_DIR}/bin DESTINATION ${LIB_DESTINATION} COMPONENT ExternalExecutables)
    endif()

    # For Ubuntu we need to manually set several files to install.
    #
    if (LINUX) 
      # Copty the QtWebEngineProcess executable to the installer libexec directory.
      #
      if(EXISTS ${_Qt6_DIR}/libexec)
        install(PROGRAMS ${_Qt6_DIR}/libexec/QtWebEngineProcess
          DESTINATION ${LIB_DESTINATION}/bin 
         COMPONENT ExternalExecutables)
        message(STATUS "${qt_install_msg} qt libexec exists")
      endif()

      # Create the svExternals/resources installer directory 
      # needed by QtWebEngine.
      #
      if(EXISTS ${_Qt6_DIR}/resources)
        install(DIRECTORY ${_Qt6_DIR}/resources
            DESTINATION ${LIB_DESTINATION}/bin COMPONENT ExternalExecutables)
      endif()
 
      # Create the svExternals/translations installer directory 
      # needed by QtWebEngine.
      #
      if(EXISTS ${_Qt6_DIR}/translations)
        install(DIRECTORY ${_Qt6_DIR}/translations
            DESTINATION ${LIB_DESTINATION}/bin COMPONENT ExternalExecutables)
      endif()
    endif()

    if(EXISTS ${_Qt6_DIR}/plugins)
      simvascular_get_subdirs(_Qt6_SUBDIRS "${_Qt6_DIR}/plugins")

      foreach(subdir ${_Qt6_SUBDIRS})
        install(DIRECTORY "${_Qt6_DIR}/plugins/${subdir}" DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT ExternalLibraries)
      endforeach()
    endif()

  endif()
endif()

