#
#  NOTE:  Due to cmake bugs, currently only devenv works on Windows.
#         Below are the code for the sed script to try the different
#         generator / build system options for windows.
#

#
# NOTE: Doxygen parameters don't seem to be passed downstream!
#

#s+REPLACEME_SV_DOXYGEN_EXECUTABLE+"C:/Program Files/doxygen/bin/doyxgen.exe"+g
#s+REPLACEME_SV_DOXYGEN_VERSION+1.8.11+g
#s+REPLACEME_SV_DOXYGEN_DOT_EXECUTABLE++g

# note: nmake currently doesn't work for mitk
#s+REPLACEME_SV_CMAKE_GENERATOR+"NMake Makefiles"+g
#s+REPLACEME_SV_MAKE_CMD+nmake+g
#s+REPLACEME_SV_MAKE_BUILD_PARAMETERS++g
#s+REPLACEME_SV_MAKE_INSTALL_PARAMETERS+install+g

# note: ninja currently doesn't work for mitk
#s+REPLACEME_SV_CMAKE_GENERATOR+"Ninja"+g
#s+REPLACEME_SV_MAKE_CMD+ninja+g
#s+REPLACEME_SV_MAKE_BUILD_PARAMETERS++g
#s+REPLACEME_SV_MAKE_INSTALL_PARAMETERS+install+g

# note: jom currently doesn't work for mitk
#s+REPLACEME_SV_CMAKE_GENERATOR+"NMake Makefiles JOM"+g
#s+REPLACEME_SV_MAKE_CMD+jom+g
#s+REPLACEME_SV_MAKE_BUILD_PARAMETERS++g
#s+REPLACEME_SV_MAKE_INSTALL_PARAMETERS+install+g

# note: msbuild currently doesn't work for mitk
#s+REPLACEME_SV_CMAKE_GENERATOR+"Visual Studio 12 2013 Win64"+g
#s+REPLACEME_SV_MAKE_CMD+MsBuild.exe+g
#s+REPLACEME_SV_MAKE_BUILD_PARAMETERS+"REPLACEME_SV_MAKE_SLN_FILENAME_PARAMETERS /m:8 /p:Configuration=REPLACEME_SV_CMAKE_BUILD_TYPE /t:ALL_BUILD"+g
#s+REPLACEME_SV_MAKE_INSTALL_PARAMETERS+"REPLACEME_SV_MAKE_SLN_FILENAME_PARAMETERS /m:8 /p:Configuration=REPLACEME_SV_CMAKE_BUILD_TYPE /t:INSTALL"+g
