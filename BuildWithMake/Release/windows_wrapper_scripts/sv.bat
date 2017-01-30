@ECHO OFF
PATH=%PATH%;%~dp0/mitk/bin/RelWithDebInfo;%~dp0\mitk\bin
"%~dp0\simvascular-bin.exe" %*

