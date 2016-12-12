#-----------------------------------------------------------------------------
# Toplevel directories for src, bin, build or externals
set(SV_EXTERNALS_TOPLEVEL_SRC_DIR "${CMAKE_BINARY_DIR}/Externals/src"
  CACHE PATH "Directory where source files for externals will be put")
set(SV_EXTERNALS_TOPLEVEL_BIN_DIR "${CMAKE_BINARY_DIR}/Externals/bin"
  CACHE PATH "Directory where install files for externals will be put")
set(SV_EXTERNALS_TOPLEVEL_BLD_DIR "${CMAKE_BINARY_DIR}/Externals/build"
  CACHE PATH "Directory where build files for externals will be put")
set(SV_EXTERNALS_TOPLEVEL_PFX_DIR "${CMAKE_BINARY_DIR}/Externals/prefix"
  CACHE PATH "Directory where prefix files for externals will be put")

set(SV_EXTERNALS_STANFORD_URL "http://simvascular.stanford.edu/downloads/public/simvascular/externals/src/originals" CACHE STRING "URL with source downloads for externals")
set(SV_EXTERNALS_GIT_URL "http://github.com/SimVascular" CACHE STRING "Git URL for SimVascular")

#-----------------------------------------------------------------------------
# Qt
# First external is not built by the project but is required if building
# MITK
option(SV_EXTERNALS_USE_QT "Enable QT Plugin" OFF)

# Add externals with default values of version, build_with, shared, dirname,
# and optional install dir name. Order matters; put independent packages first
#-----------------------------------------------------------------------------
# TCL
sv_externals_add_new_external(TCL 8.6.4 ON ON tcl tcltk)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TK
sv_externals_add_new_external(TK 8.6.4 ON ON tk tcltk)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#PYTHON
sv_externals_add_new_external(PYTHON 2.7.11 ON ON python)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#PIP
sv_externals_add_new_external(PIP 0.0.0 ON ON pip)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#NUMPY
sv_externals_add_new_external(NUMPY 1.11.1 ON ON numpy)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#FREETYPE
sv_externals_add_new_external(FREETYPE 2.6.3 ON ON freetype)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# MMG
sv_externals_add_new_external(MMG 5.1.0 ON OFF mmg)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# VTK
sv_externals_add_new_external(VTK 6.2.0 ON ON vtk)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# GDCM
sv_externals_add_new_external(GDCM 2.6.1 ON ON gdcm)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# ITK
sv_externals_add_new_external(ITK 4.7.1 ON ON itk)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# OpenCASCADE
sv_externals_add_new_external(OpenCASCADE 7.0.0 OFF ON opencascade)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# MITK
sv_externals_add_new_external(MITK 2016.03 OFF ON mitk)
#-----------------------------------------------------------------------------
