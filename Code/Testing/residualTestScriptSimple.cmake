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
if( NOT test_cmd )
 message( FATAL_ERROR "Variable test_cmd not defined" )
endif( NOT test_cmd )

if( NOT test_args )
 message( FATAL_ERROR "Variable test_args not defined" )
endif( NOT test_args )

# output_blessed contains the name of the "blessed" output file
if( NOT threshold )
 message( FATAL_ERROR "Variable threshold not defined" )
endif( NOT threshold )

# output_test contains the name of the output file the test_cmd will produce
if( NOT output_test )
 message( FATAL_ERROR "Variable output_test not defined" )
endif( NOT output_test )

message(STATUS "test_cmd: ${test_cmd}")
message(STATUS "output_test: ${output_test}")
file(REMOVE ${output_test})
exec_program(${test_cmd} ARGS ${test_args} )

file (STRINGS ${output_test} RESIDUAL)
message(STATUS "Threshold ${threshold}, Residual: ${RESIDUAL}")
string(COMPARE GREATER ${RESIDUAL} ${threshold} test_not_successful)

if( test_not_successful )
 message( SEND_ERROR "${output_test} does not match ${output_blessed}!" )
else()
 message(STATUS "TEST Passed!")
endif( test_not_successful )