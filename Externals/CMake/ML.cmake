set(proj ml)

# Dependencies
include(ExternalProject)

set(ML_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_SRC_DIR}/${proj}-${SV_EXTERNALS_ML_VERSION})
set(ML_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_PFX_DIR}/${proj}-${SV_EXTERNALS_ML_VERSION})
set(ML_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_BIN_DIR}/${proj}-${SV_EXTERNALS_ML_VERSION})

ExternalProject_Add("${proj}_networks"
  URL               http://simvascular.stanford.edu/downloads/public/machine_learning/networks.tar
  PREFIX            ${ML_PFX_DIR}
  DOWNLOAD_DIR      ${ML_SRC_DIR}
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/../Python/site-packages/sv_ml/results
  UPDATE_COMMAND    ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ""
  INSTALL_COMMAND   ""
)
