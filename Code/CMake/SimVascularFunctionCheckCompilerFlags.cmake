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

#
# Helper macro allowing to check if the given flags are supported
# by the underlying build tool
#
# If the flag(s) is/are supported, they will be appended to the string identified by RESULT_VAR
#
# Usage:
#   SimVascularFunctionCheckCompilerFlags(FLAGS_TO_CHECK VALID_FLAGS_VAR)
#
# The above example uses the C++ compiler to check the flags. To individually check with
# the C and C++ compiler, use:
#
#   SimVascularFunctionCheckCompilerFlags2(FLAGS_TO_CHECK VALID_C_FLAGS_VAR VALID_CXX_FLAGS_VAR)
#
# Example:
#
#   set(myflags)
#   SimVascularFunctionCheckCompilerFlags("-fprofile-arcs" myflags)
#   message(1-myflags:${myflags})
#   SimVascularFunctionCheckCompilerFlags("-fauto-bugfix" myflags)
#   message(2-myflags:${myflags})
#   SimVascularFunctionCheckCompilerFlags("-Wall" myflags)
#   message(1-myflags:${myflags})
#
#   The output will be:
#    1-myflags: -fprofile-arcs
#    2-myflags: -fprofile-arcs
#    3-myflags: -fprofile-arcs -Wall

include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)

#-----------------------------------------------------------------------------
# Function taken froom mitk to check and see if compiler flag works
function(SimVascularFunctionCheckCompilerFlags CXX_FLAG_TO_TEST RESULT_VAR)

  if(CXX_FLAG_TO_TEST STREQUAL "")
    message(FATAL_ERROR "CXX_FLAG_TO_TEST shouldn't be empty")
  endif()

  # Internally, the macro CMAKE_CXX_ACCEPTS_FLAG calls TRY_COMPILE. To avoid
  # the cost of compiling the test each time the project is configured, the variable set by
  # the macro is added to the cache so that following invocation of the macro with
  # the same variable name skip the compilation step.
  # For that same reason, the SimVascularFunctionCheckCompilerFlags function appends a unique suffix to
  # the HAS_FLAG variable. This suffix is created using a 'clean version' of the flag to test.
  string(REGEX REPLACE "[, \\$\\+\\*\\{\\}\\(\\)\\#]" "" suffix ${CXX_FLAG_TO_TEST})
  CHECK_CXX_COMPILER_FLAG(${CXX_FLAG_TO_TEST} HAS_FLAG_${suffix})

  if(HAS_FLAG_${suffix})
    set(${RESULT_VAR} "${${RESULT_VAR}} ${CXX_FLAG_TO_TEST}" PARENT_SCOPE)
  endif()

endfunction()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
function(SimVascularFunctionCheckCAndCXXCompilerFlags FLAG_TO_TEST C_RESULT_VAR CXX_RESULT_VAR)

  if(FLAG_TO_TEST STREQUAL "")
    message(FATAL_ERROR "FLAG_TO_TEST shouldn't be empty")
  endif()

  # Save the contents of CXX_RESULT_VAR temporarily.
  # This is needed of ${CXX_RESULT_VAR} is one of the CMAKE_<LANG>_FLAGS_* variables.
  set(_saved_c_result_var ${${C_RESULT_VAR}})
  set(_saved_cxx_result_var ${${CXX_RESULT_VAR}})

  # Clear all flags. If not, existing flags triggering warnings might lead to
  # false-negatives when checking for certain compiler flags.
  set(CMAKE_C_FLAGS )
  set(CMAKE_C_FLAGS_DEBUG )
  set(CMAKE_C_FLAGS_MINSIZEREL )
  set(CMAKE_C_FLAGS_RELEASE )
  set(CMAKE_C_FLAGS_RELWITHDEBINFO )
  set(CMAKE_CXX_FLAGS )
  set(CMAKE_CXX_FLAGS_DEBUG )
  set(CMAKE_CXX_FLAGS_MINSIZEREL )
  set(CMAKE_CXX_FLAGS_RELEASE )
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO )

  # Internally, the macro CMAKE_CXX_COMPILER_FLAG calls TRY_COMPILE. To avoid
  # the cost of compiling the test each time the project is configured, the variable set by
  # the macro is added to the cache so that following invocation of the macro with
  # the same variable name skip the compilation step.
  # For that same reason, the SimVascularFunctionCheckCompilerFlags2 function appends a unique suffix to
  # the HAS_CXX_FLAG and HAS_C_FLAG variable. This suffix is created using a 'clean version' of the
  # flag to test. The value of HAS_C(XX)_FLAG_${suffix} additonally needs to be a valid
  # pre-processor token because CHECK_CXX_COMPILER_FLAG adds it as a definition to the compiler
  # arguments. An invalid token triggers compiler warnings, which in case of the "-Werror" flag
  # leads to false-negative checks.
  string(REGEX REPLACE "[/-]" "_" suffix ${FLAG_TO_TEST})
  string(REGEX REPLACE "[, \\$\\+\\*\\{\\}\\(\\)\\#]" "" suffix ${suffix})

  # workaround for gcc's strange behaviour on -Wno-... options in combination with -Werror
  # we test the flag without the "no-" prefix because that is more reliable
  string(REGEX REPLACE "^-Wno-" "-W" FLAG_TO_TEST_FIXED ${FLAG_TO_TEST})

  CHECK_CXX_COMPILER_FLAG(${FLAG_TO_TEST_FIXED} HAS_CXX_FLAG_${suffix})

  if(HAS_CXX_FLAG_${suffix})
    set(${CXX_RESULT_VAR} "${_saved_cxx_result_var} ${FLAG_TO_TEST}" PARENT_SCOPE)
  endif()

  CHECK_C_COMPILER_FLAG(${FLAG_TO_TEST_FIXED} HAS_C_FLAG_${suffix})

  if(HAS_C_FLAG_${suffix})
    set(${C_RESULT_VAR} "${_saved_c_result_var} ${FLAG_TO_TEST}" PARENT_SCOPE)
  endif()

endfunction()
#-----------------------------------------------------------------------------
