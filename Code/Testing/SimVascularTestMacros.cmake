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

macro(add_test_return name test_cmd test_args)
message(STATUS "Adding test: ${name}")
  add_test("${name}"
   "${CMAKE_COMMAND}" -Dtest_cmd=${test_cmd} -Dtest_args=${test_args} -P ${CMAKE_SOURCE_DIR}/Testing/returnTestScript.cmake)
endmacro()
macro(add_test_compare_files name test_cmd test_args output_blessed output_test)
  message(STATUS "Adding test: ${name}")
  add_test(NAME "${name}"
   COMMAND 
   "${CMAKE_COMMAND}" 
   -Dtest_cmd="${test_cmd}" 
   -Dtest_args=${test_args} 
   -Doutput_blessed="${output_blessed}" 
   -Doutput_test="${output_test}" 
   -P ${CMAKE_SOURCE_DIR}/Testing/compareFileTestScript.cmake
   )
endmacro()

macro(add_test_residualSimple name test_cmd test_args output_test threshold)
  message(STATUS "Adding test: ${name}")

  add_test(NAME "${name}"
   COMMAND 
   "${CMAKE_COMMAND}"
   -Dtest_cmd="${test_cmd}" 
   -Dtest_args=${test_args} 
   -Doutput_test=${output_test}
   -Dthreshold=${threshold}
   -P ${CMAKE_SOURCE_DIR}/Testing/residualTestScriptSimple.cmake
   )
  #message(STATUS "\"${CMAKE_COMMAND}\" -Dtest_cmd=\"${test_cmd}\" -Dtest_args=${test_args} -Doutput_test=\"${output_test}\" -Dthreshold=\"${threshold}\" -P ${CMAKE_SOURCE_DIR}/Testing/residuleTestScript.cmake")
endmacro()

macro(add_test_residual)
  set(options)
  set(oneValueArgs NAME 
    SIM_CMD TCL_FILE 
    WORKING_DIRECTORY
    POST_CMD THRESHOLD 
    OUTPUT_FILE BLESSED_FILE RESIDUAL_FILE)
  set(multiValueArgs )

  unset(add_test_residual_NAME)
  unset(add_test_residual_SIM_CMD)
  unset(add_test_residual_POST_CMD)
  unset(add_test_residual_TCL_FILE)
  unset(add_test_residual_WORKING_DIRECTORY)
  unset(add_test_residual_THRESHOLD)
  unset(add_test_residual_BLESSED_FILE)
  unset(add_test_residual_OUTPUT_FILE)
  unset(add_test_residual_RESIDUAL_FILE)


  CMAKE_PARSE_ARGUMENTS("add_test_residual" 
    "${options}"
    "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  message(STATUS "Adding test: ${add_test_residual_NAME}")

  add_test(NAME "${add_test_residual_NAME}"
   COMMAND 
   "${CMAKE_COMMAND}" -Wno-dev
   -Dsim_cmd=${add_test_residual_SIM_CMD}
   -Dpost_cmd=${add_test_residual_POST_CMD}
   -Dtest_tcl_file=${add_test_residual_TCL_FILE} 
   -Doutput_file=${add_test_residual_OUTPUT_FILE}
   -Dblessed_file=${add_test_residual_BLESSED_FILE}
   -Dresidual_file=${add_test_residual_RESIDUAL_FILE}
   -Dthreshold=${add_test_residual_THRESHOLD}
   -Dpost_mode=-res
   -Dsave_out_name=${SimVascular_TEST_SAVEOUT_DIR}/${add_test_residual_NAME}_${add_test_residual_RESIDUAL_FILE}
   -P ${CMAKE_SOURCE_DIR}/Testing/residualTestScript.cmake
   )
endmacro()

macro(add_test_append_compare)
  set(options)
  set(oneValueArgs NAME 
    SIM_CMD TCL_FILE 
    WORKING_DIRECTORY
    POST_CMD THRESHOLD 
    OUTPUT_FILE BLESSED_FILE RESIDUAL_FILE
    TEST_DEPENDS)
  set(multiValueArgs )

  unset(add_test_append_compare_NAME)
  unset(add_test_append_compare_POST_CMD)
  unset(add_test_append_compare_THRESHOLD)
  unset(add_test_append_compare_BLESSED_FILE)
  unset(add_test_append_compare_OUTPUT_FILE)
  unset(add_test_append_compare_RESIDUAL_FILE)
  unset(add_test_append_compare_TEST_DEPENDS)


  CMAKE_PARSE_ARGUMENTS("add_test_append_compare" 
    "${options}"
    "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  message(STATUS "Adding test: ${add_test_append_compare_NAME}")

  add_test(NAME "${add_test_append_compare_NAME}"
   COMMAND
   "${CMAKE_COMMAND}" -Wno-dev
   -Dpost_cmd=${add_test_append_compare_POST_CMD}
   -Doutput_file=${add_test_append_compare_OUTPUT_FILE}
   -Dblessed_file=${add_test_append_compare_BLESSED_FILE}
   -Dresidual_file=${add_test_append_compare_RESIDUAL_FILE}
   -Dthreshold=${add_test_append_compare_THRESHOLD}
   -Dpost_mode=-compare
   -Dsave_out_name=${SimVascular_TEST_SAVEOUT_DIR}/${add_test_append_compare_NAME}_${add_test_append_compare_RESIDUAL_FILE}
   -P ${CMAKE_SOURCE_DIR}/Testing/residualTestScriptAppendCompare.cmake
   )

  set_property(TEST "${add_test_append_compare_NAME}" APPEND PROPERTY DEPENDS "${add_test_append_compare_TEST_DEPENDS}")
  
 endmacro()