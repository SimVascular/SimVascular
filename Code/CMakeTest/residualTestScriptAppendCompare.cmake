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
# some argument checking:
# test_cmd is the command to run with all its arguments

cmake_policy(SET CMP0007 NEW)

if( NOT post_cmd )
 message( FATAL_ERROR "Variable post_cmd not defined" )
endif()

if( NOT post_mode )
 message( FATAL_ERROR "Variable post_mode not defined" )
endif()

if( NOT output_file )
 message( FATAL_ERROR "Variable output_file not defined" )
endif()

if( NOT blessed_file )
 message( FATAL_ERROR "Variable blessed_file not defined" )
endif()

if( NOT residual_file )
 message( FATAL_ERROR "Variable residual_file not defined" )
endif()

if( NOT threshold )
 message( FATAL_ERROR "Variable threshold not defined" )
endif()
message(STATUS "")
message(STATUS "post_cmd ${post_cmd}")
message(STATUS "output_file ${output_file}")
message(STATUS "blessed_file ${blessed_file}")
message(STATUS "residual_file ${residual_file}")
message(STATUS "threshold ${threshold}")
message(STATUS "")

message(STATUS "Preparing to run postsolver in mode ${post_mode}")
file(REMOVE ${residual_file})
exec_program(${post_cmd} ARGS "${post_mode} ${output_file} ${blessed_file} ${threshold}" )

message(STATUS "")
file (STRINGS ${residual_file} file_string)
list(GET file_string 0 test_successful)
string(TOUPPER "${test_successful}" test_successful)
message(STATUS "Output variable: test_successful ${test_successful}!")
file(READ ${residual_file} text)
if(NOT (test_successful MATCHES "TRUE"))
 message( SEND_ERROR "Test did not pass!\n${residual_file}:\n${text}\n" )
else()
    message( STATUS "Test Passed!\n${residual_file}:\n${text}\n")
endif()

if( save_out_name )
 GET_FILENAME_COMPONENT(save_dir ${save_out_name} PATH)
 file(MAKE_DIRECTORY ${save_dir})
 message(STATUS "Saving output in ${save_dir}")
 file(RENAME ${residual_file} ${save_out_name})
endif()