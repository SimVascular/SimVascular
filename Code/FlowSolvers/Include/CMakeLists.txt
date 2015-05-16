# Set Coronary Flag
if(ThreeDSolver_USE_CORONARY)
  SET(VER_CORONARY 1)
else()
  SET(VER_CORONARY 0)
endif()
# Set Closed Loop Flag
if(ThreeDSolver_USE_CLOSEDLOOP)
  SET(VER_CLOSEDLOOP 1)
else()
  SET(VER_CLOSEDLOOP 0)
endif()
# Set Variable wall Flag
if(ThreeDSolver_USE_VARWALL)
  SET(VER_VARWALL 1)
else()
  SET(VER_VARWALL 0)
endif()
# Set Use VTK Flag
if(ThreeDSolver_USE_VTK)
  SET(VER_USE_VTK 1)
else()
  SET(VER_USE_VTK 0)
endif()

# Write include file
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cvFlowsolverOptions.h.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cvFlowsolverOptions.h"
)