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

include( CTest )
include(${CMAKE_CURRENT_SOURCE_DIR}/SimVascularTestMacros.cmake)

option(SV_RUN_GUI_TESTS "Option to run GUI testing when performing test" ON)
set(SV_TEST_DIR "" CACHE PATH "Path to SV automated test files")
set(SV_TEST_SAVEOUT_DIR "${OUTBIN_DIR}/Testing/" CACHE PATH "Path to SV automated test files" FORCE)

#TODO: Change this!
if(UNIX)
  set(SV_TEST_EXE "${SV_DEVELOPER_SCRIPT_DIR}/sv")
endif()
if(WIN32)
  set(SV_TEST_EXE "${SV_DEVELOPER_SCRIPT_DIR}/sv.bat")
endif()
file(TO_NATIVE_PATH "${FLOWSOLVER_CONFIG_PATH}" FLOWSOLVER_CONFIG_NATIVE_PATH)
if(SV_DEV_OUTPUT)
  getListOfVarsPrefix("SV_SCRIPT_NAME" _VARLIST)
  list(INSERT _VARLIST 0 CMAKE_INSTALL_PREFIX)
  print_vars(_VARLIST)
endif()

if(SV_TEST_DIR STREQUAL "" OR NOT EXISTS ${SV_TEST_DIR}/common/executable_names.tcl.in)
  message(FATAL_ERROR "You must set Variable SV_TEST_DIR to location of simvasculars automated tests!")
endif()

if(SV_DEV_OUTPUT)
  getListOfVarsPrefix("SV_TEST" _VARLIST)
  print_vars(_VARLIST)
endif()

if(SV_RUN_GUI_TESTS)
  add_test_return(StartUpTest ${SV_TEST_EXE} "${SV_TEST_DIR}/startup/startup.tcl -tcl")
endif()
