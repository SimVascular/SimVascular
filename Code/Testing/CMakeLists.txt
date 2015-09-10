
include( CTest )
include(${CMAKE_CURRENT_SOURCE_DIR}/SimVascularTestMacros.cmake)

option(SimVascular_RUN_GUI_TESTS "Option to run GUI testing when performing test" ON)
set(SimVascular_TEST_DIR "" CACHE PATH "Path to SimVascular automated test files")
set(SimVascular_TEST_SAVEOUT_DIR "${OUTBIN_DIR}/Testing/" CACHE PATH "Path to SimVascular automated test files" FORCE)
mark_as_superbuild(SimVascular_TEST_DIR)
mark_as_superbuild(SimVascular_RUN_GUI_TESTS)

#TODO: Change this!
if(UNIX)
  set(SimVascular_TEST_EXE "${SIMVASCULAR_DEVELOPER_SCRIPT_DIR}/mysim")
  set(POSTSOLVER_TEST_EXE "${SIMVASCULAR_DEVELOPER_SCRIPT_DIR}/mypost")
  set(SOLVER_TEST_EXE "${SIMVASCULAR_DEVELOPER_SCRIPT_DIR}/mysolver")
endif()
if(WIN32)
  set(SimVascular_TEST_EXE "${SIMVASCULAR_DEVELOPER_SCRIPT_DIR}/mysim.bat")
  set(POSTSOLVER_TEST_EXE "${SIMVASCULAR_DEVELOPER_SCRIPT_DIR}/mypost.bat")
  set(SOLVER_TEST_EXE "${SIMVASCULAR_DEVELOPER_SCRIPT_DIR}/mysolver.bat")
endif()
file(TO_NATIVE_PATH "${FLOWSOLVER_CONFIG_PATH}" FLOWSOLVER_CONFIG_NATIVE_PATH)
if(SimVascular_DEV_OUTPUT)
  getListOfVarsPrefix("SIMVASCULAR_SCRIPT_NAME" _VARLIST)
  list(INSERT _VARLIST 0 CMAKE_INSTALL_PREFIX)
  print_vars(_VARLIST)
endif()

if(SimVascular_TEST_DIR STREQUAL "" OR NOT EXISTS ${SimVascular_TEST_DIR}/common/executable_names.tcl.in)
  message(FATAL_ERROR "You must set Variable SimVascular_TEST_DIR to location of simvasculars automated tests!")
endif()

if(SimVascular_DEV_OUTPUT)
  getListOfVarsPrefix("SIMVASCULAR_TEST" _VARLIST)
  print_vars(_VARLIST)
endif()

if(SimVascular_RUN_GUI_TESTS)
  add_test_return(StartUpTest ${SimVascular_TEST_EXE} "${SimVascular_TEST_DIR}/startup/startup.tcl" )
endif()

if(NOT SimVascular_USE_DUMMY_MPICH2 AND MPIEXEC)
  set(solver_cmd "${MPIEXEC} -n 1 ${SOLVER_TEST_EXE}")
else()
  set(solver_cmd "${SOLVER_TEST_EXE}")
endif()
add_test(NAME SolverSimpleTest
  COMMAND  ${solver_cmd}
  WORKING_DIRECTORY ${SimVascular_TEST_DIR}/solver_basic)
set_tests_properties(SolverSimpleTest
    PROPERTIES ENVIRONMENT FLOWSOLVER_CONFIG=${FLOWSOLVER_CONFIG_NATIVE_PATH})
set_tests_properties(SolverSimpleTest PROPERTIES FAIL_REGULAR_EXPRESSION "Input error;Input exception;BAD TERMINATION")
if(WIN32)
  file(COPY ${FLOWSOLVER_CONFIG_FILE} DESTINATION ${SimVascular_TEST_DIR}/solver_basic)
endif()
