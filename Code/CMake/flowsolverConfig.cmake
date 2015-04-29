# FLOWSOLVER CONFIGURATION FILE

# ===========================
# MACROS TO EXTRACT FROM LIST
# ===========================
MACRO(CAR var)
  SET(${var} ${ARGV1})
ENDMACRO(CAR)
MACRO(CDR var junk)
  SET(${var} ${ARGN})
ENDMACRO(CDR)
MACRO(LIST_INDEX var index)
  SET(list . ${ARGN})
  FOREACH(i RANGE 1 ${index})
    CDR(list ${list})
  ENDFOREACH(i)
  CAR(${var} ${list})
ENDMACRO(LIST_INDEX)
# ===========================


# Check if version file exists
#SET(VER_FILE ${SimVascular_SOURCE_DIR}/Modules/ThreeDSolver/version.h)
#if(EXISTS ${VER_FILE})
#  MESSAGE(STATUS "--- FLOWSOLVER VERSION FILE WAS FOUND.") 
#  # Define version variables
#  FILE (STRINGS "${VER_FILE}" VER_CONTENT)
#  SEPARATE_ARGUMENTS(VER_CONTENT)
#  # Get name of variables
#  LIST_INDEX(CORONARY_NAME 2 ${VER_CONTENT})
#  LIST_INDEX(CLOSEDLOOP_NAME 5 ${VER_CONTENT})
#  LIST_INDEX(VARWALL_NAME 8 ${VER_CONTENT})
#  LIST_INDEX(USE_VTK_NAME 11 ${VER_CONTENT})
#  if((CORONARY_NAME STREQUAL "VER_CORONARY")AND
#    (CLOSEDLOOP_NAME STREQUAL "VER_CLOSEDLOOP")AND
#    (VARWALL_NAME STREQUAL "VER_VARWALL")AND
#    (USE_VTK_NAME STREQUAL "VER_USE_VTK"))
#    # Get The Variables in fixed Order
#    LIST_INDEX(VER_CORONARY 3 ${VER_CONTENT})
#    LIST_INDEX(VER_CLOSEDLOOP 6 ${VER_CONTENT})
#    LIST_INDEX(VER_VARWALL 9 ${VER_CONTENT})
#    LIST_INDEX(VER_USE_VTK 12 ${VER_CONTENT})
#    # Show Included features
#    if(VER_CORONARY EQUAL 1)
#      MESSAGE(STATUS "Including coronary bc feature")
#    endif()
#    if(VER_CLOSEDLOOP EQUAL 1)
#      MESSAGE(STATUS "Including closed loop bc feature")
#    endif()
#    if(VER_VARWALL EQUAL 1)
#      MESSAGE(STATUS "Including variable wall feature")
#    endif()
#    if(VER_USE_VTK EQUAL 1)
#      MESSAGE(STATUS "Using VTK")
#    endif()
#  else()
#    MESSAGE("INVALID FLOWSOLVER VERSION FILE FORMAT.") 
#    MESSAGE(STATUS "Compiling baseline version")
#    SET (VER_CORONARY 0)
#    SET (VER_CLOSEDLOOP 0)
#    SET (VER_VARWALL 0)
#    SET (VER_USE_VTK 0)
#  endif()
#else()
#  MESSAGE(STATUS "--- FLOWSOLVER VERSION FILE WAS NOT FOUND.") 
#  MESSAGE(STATUS "Compiling baseline version")
#  SET (VER_CORONARY 0)
#  SET (VER_CLOSEDLOOP 0)
#  SET (VER_VARWALL 0)
#  SET (VER_USE_VTK 0)
#endif()
#MESSAGE(STATUS "--- END FLOWSOLVER VERSION.") 


