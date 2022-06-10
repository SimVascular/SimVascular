REM This file is used to setup the environment variables and start the build in
REM the GitHub Workflow

REM Call vcvars64.bat to setup environment variables like CL
call "C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\VC\Auxiliary\Build\vcvars64.bat"

REM Build SimVascular
sh ./quick-build-windows.sh

REM Make release executable BuildWithMake/SimVascular-VERSION-Windows-64bit-bundle.exe
cd Release
make all