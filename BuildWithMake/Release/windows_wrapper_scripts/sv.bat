@ECHO OFF
PATH=%~dp0;%~dp0/mitk/bin/SV_CMAKE_BUILD_TYPE;%~dp0/mitk/bin/plugins/SV_CMAKE_BUILD_TYPE;%~dp0/mitk/bin;%~dp0/plugins;%PATH%
cd "%~dp0"
"%~dp0/simvascular-bin.exe" %*
