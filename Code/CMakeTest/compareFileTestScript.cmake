# some argument checking:
# test_cmd is the command to run with all its arguments
if( NOT test_cmd )
	message( FATAL_ERROR "Variable test_cmd not defined" )
endif( NOT test_cmd )

if( NOT test_args )
	message( FATAL_ERROR "Variable test_args not defined" )
endif( NOT test_args )

# output_blessed contains the name of the "blessed" output file
if( NOT output_blessed )
	message( FATAL_ERROR "Variable output_blessed not defined" )
endif( NOT output_blessed )

# output_test contains the name of the output file the test_cmd will produce
if( NOT output_test )
	message( FATAL_ERROR "Variable output_test not defined" )
endif( NOT output_test )

message(STATUS "test_cmd: ${test_cmd}")
message(STATUS "output_blessed: ${output_blessed}")
message(STATUS "output_test: ${output_test}")

file(REMOVE ${output_test})
exec_program(${test_cmd} ARGS ${test_args} )
exec_program(${CMAKE_COMMAND} ARGS -E compare_files ${output_blessed} ${output_test}
	OUTPUT_VARIABLE test_not_successful
	)
message(STATUS "${test_not_successful}")
if( test_not_successful )
	message( SEND_ERROR "${output_test} does not match ${output_blessed}! and ${output_test} does not exist" )
else()
	message(STATUS "TEST Passed!")
endif( test_not_successful )