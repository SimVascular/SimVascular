@echo off

C:
chdir C:\cygwin64\bin

REM set the correct path and visual studio path if you have Intel Fortran compiler
REM call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2019.4.245\windows\bin\compilervars.bat" intel64 vs2017

REM Add in MSVC 2017 C++ x64 compiler (not needed if you include ifort above)
call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise\VC\Auxiliary\Build\vcvars64.bat"

REM use startxwin if you use x server, otherwise just use next line
REM bash --login
bash --login