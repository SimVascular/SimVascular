
include( CTest )
include(${CMAKE_CURRENT_SOURCE_DIR}/SimVascularTestMacros.cmake)

option(SV_RUN_GUI_TESTS "Option to run GUI testing when performing test" ON)
set(SV_TEST_DIR "" CACHE PATH "Path to SV automated test files")
set(SV_TEST_SAVEOUT_DIR "${OUTBIN_DIR}/Testing/" CACHE PATH "Path to SV automated test files" FORCE)

#TODO: Change this!
if(UNIX)
  set(SV_TEST_EXE "${SV_DEVELOPER_SCRIPT_DIR}/mysim")
endif()
if(WIN32)
  set(SV_TEST_EXE "${SV_DEVELOPER_SCRIPT_DIR}/mysim.bat")
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
  add_test_return(StartUpTest ${SV_TEST_EXE} "${SV_TEST_DIR}/startup/startup.tcl" )
endif()
