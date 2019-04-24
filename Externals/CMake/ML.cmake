set(proj ML)

message(WARNING "*****************ML CMAKE********************")

# Dependencies
set(${proj}_DEPENDENCIES "PYTHON" "PIP")

include(ExternalProject)

set(ML_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_SRC_DIR}/${proj}-${SV_EXTERNALS_ML_VERSION})
set(ML_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_BIN_DIR}/${proj}-${SV_EXTERNALS_ML_VERSION})

#file(MAKE_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/python_packages)

ExternalProject_Add("seg_regression"
  SOURCE_DIR        ${ML_BIN_DIR}/seg_regression
  GIT_REPOSITORY    "https://github.com/gmaher/seg_regression.git"
  GIT_TAG           "master"
  UPDATE_COMMAND    ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ""
  TEST_COMMAND      ""
  INSTALL_COMMAND   ""
)

ExternalProject_Add("networks"
  URL               http://simvascular.stanford.edu/downloads/public/machine_learning/networks.tar
  DOWNLOAD_DIR      ${ML_SRC_DIR}
  SOURCE_DIR        ${ML_BIN_DIR}/seg_regression/results
  UPDATE_COMMAND    ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ""
  INSTALL_COMMAND   ""
  DEPENDS "seg_regression"
)

ExternalProject_Add("python_packages"
  DOWNLOAD_COMMAND  ""
  BUILD_COMMAND     ""
  UPDATE_COMMAND    ""
  DEPENDS "seg_regression" ${${proj}_DEPENDENCIES}
  CONFIGURE_COMMAND ${SV_EXTERNALS_PYTHON_EXECUTABLE} -m pip install -r ${ML_BIN_DIR}/seg_regression/requirements.txt
  INSTALL_COMMAND   ""
)
