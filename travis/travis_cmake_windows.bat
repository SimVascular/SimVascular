REM Copyright (c) Stanford University, The Regents of the University of
REM               California, and others.
REM 
REM All Rights Reserved.
REM 
REM See Copyright-SimVascular.txt for additional details.
REM 
REM Permission is hereby granted, free of charge, to any person obtaining
REM a copy of this software and associated documentation files (the
REM "Software"), to deal in the Software without restriction, including
REM without limitation the rights to use, copy, modify, merge, publish,
REM distribute, sublicense, and/or sell copies of the Software, and to
REM permit persons to whom the Software is furnished to do so, subject
REM to the following conditions:
REM 
REM The above copyright notice and this permission notice shall be included
REM in all copies or substantial portions of the Software.
REM 
REM THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
REM IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
REM TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
REM PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
REM OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
REM EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
REM PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
REM PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
REM LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
REM NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
REM SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

REM @echo off
REM script for travis build using MSVC 2017

if [%1]==[] goto hardcode
if [%2]==[] goto hardcode
  set CWD=%1
  set SV_EXTERNALS_VERSION_NUMBER=%2
goto :buildit

:hardcode
  set CWD=C:/travis
  set SV_EXTERNALS_VERSION_NUMBER=2019.06
goto :buildit

:buildit
  echo SV_EXTERNALS_VERSION_NUMBER=%SV_EXTERNALS_VERSION_NUMBER%
  echo CWD=%CWD%

  cd %CWD%

  REM for debugging
  dir

  set SV_EXTERNALS_USE_PREBUILT_QT=false
  REM hardcode a dummy path to system Qt
  set SV_EXTERNALS_PREBUILT_QT_PATH=""
  
  set SV_CMAKE_BUILD_TYPE="Release"
  set SV_CMAKE_CONFIGURATION_TYPES="Release"
  set SV_CMAKE_CMD="C:/Program Files/CMake/bin/cmake.exe"
  set SV_CMAKE_GENERATOR="Visual Studio 15 2017 Win64"
  set SV_CMAKE_GENERATOR_PLATFORM="x64"
  set SV_CMAKE_VS_PLATFORM_TOOLSET="v141"

cd %CWD%

%SV_CMAKE_CMD% ^
 ^
   -G %SV_CMAKE_GENERATOR% ^
   -DCMAKE_BUILD_TYPE=%SV_CMAKE_BUILD_TYPE% ^
   -DCMAKE_CONFIGURATION_TYPES=%SV_CMAKE_CONFIGURATION_TYPES% ^
   -DCMAKE_VS_PLATFORM_TOOLSET=%SV_CMAKE_VS_PLATFORM_TOOLSET% ^
   -DSV_USE_SV4_GUI:BOOL=OFF ^
   -DCMAKE_CXX_FLAGS="/MD /MP /Od /EHsc" ^
   -DCMAKE_C_FLAGS="/MD /MP /Od" ^
   -DCMAKE_CXX_FLAGS_RELEASE="/MD /MP /Od /EHsc" ^
   -DCMAKE_C_FLAGS_RELEASE="/MD /MP /Od" ^
   -DBUILD_SHARED_LIBS=ON ^
   -DBUILD_TESTING=OFF ^
   -DSV_USE_FREETYPE=ON ^
   -DSV_USE_GDCM=ON ^
   -DSV_USE_ITK=ON ^
   -DSV_USE_MPICH2=OFF ^
   -DSV_USE_OpenCASCADE=ON ^
   -DSV_USE_PYTHON=ON ^
   -DSV_USE_MMG=ON ^
   -DSV_USE_MITK=ON ^
   -DSV_USE_Qt5=ON ^
   -DSV_USE_QT=ON ^
   -DSV_DOWNLOAD_EXTERNALS=ON ^
   -DSV_EXTERNALS_USE_TOPLEVEL_BIN_DIR=ON ^
   -DSV_EXTERNALS_VERSION_NUMBER:STRING=%SV_EXTERNALS_VERSION_NUMBER% ^
   -DSV_EXTERNALS_USE_PREBUILT_QT:BOOL=%SV_EXTERNALS_USE_PREBUILT_QT% ^
 %CWD%

%SV_CMAKE_CMD% --build %CWD% -j 2 --config Release
