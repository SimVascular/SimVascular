# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

file(WRITE  "${TCL_STARTUP_CONFIG_FILE}"
"global SV_FULL_VER_NO
global SV_MAJOR_VER_NO
global SV_PLATFORM
global SV_MINOR_VERSION
global SV_PATCH_VERSION
global SV_VERSION
global SV_NO_RENDERER
global SV_USE_PYTHON
global SV_SHARED_BUILD
global SV_BUILD_TYPE
global VTK_DIR
set SV_FULL_VER_NO \"${SV_FULL_VER_NO}\"
set SV_MAJOR_VER_NO \"${SV_MAJOR_VER_NO}\"
set SV_PLATFORM    \"${SV_PLATFORM}\"
set SV_MINOR_VERSION    \"${SV_MINOR_VERSION}\"
set SV_PATCH_VERSION    \"${SV_PATCH_VERSION}\"
set SV_VERSION     \"${SV_VERSION}\"
set SV_NO_RENDERER \"${SV_NO_RENDERER}\"
set SV_SHARED_BUILD \"${BUILD_SHARED_LIBS}\"
set SV_BUILD_TYPE \"${SV_BUILD_TYPE}\"
set SV_USE_PYTHON \"${SV_USE_PYTHON}\"
set VTK_DIR \"${SV_VTK_DIR}\"\n")

file(WRITE ${TCL_SPLASH_CONFIG_FILE} 
"if {$SV_RELEASE_BUILD == 1} {
	set {gOptions(simvascular_version)} \"VERSION ${SV_FULL_VER_NO}.${SV_VERSION_TIMESTAMP}\"
} else {
set {gOptions(simvascular_version)} \"VERSION ${SV_FULL_VER_NO} (developers build)\"
}\n")

# file(WRITE ${TCL_EXTERNAL_CONFIG_FILE} 
# "set bindir $env(SV_HOME)/Bin
file(WRITE ${TCL_EXTERNAL_CONFIG_FILE}
"if {$SV_RELEASE_BUILD == 1} {
	set bindir $env(SV_HOME)/${SV_INSTALL_RUNTIME_DIR}
	set homedir $env(SV_HOME)	
	set execext {${CMAKE_EXECUTABLE_SUFFIX}}
	set execbinext {${CMAKE_EXECUTABLE_SUFFIX}}
	set gExternalPrograms(mpiexec) [file join  $homedir mpiexec${CMAKE_EXECUTABLE_SUFFIX}]
} else {
	set bindir $env(SV_HOME)/Bin
	set homedir $env(SV_HOME)
	set execext {${CMAKE_EXECUTABLE_SUFFIX}}
	set execbinext {${CMAKE_EXECUTABLE_SUFFIX}}
	set gExternalPrograms(mpiexec) {${MPIEXEC}}
}
set gExternalPrograms(cvpresolver) [file join $bindir ${SV_PRESOLVER_EXE}$execbinext]
set gExternalPrograms(cvpostsolver) [file join $bindir ${SV_POSTSOLVER_EXE}$execbinext]
set gExternalPrograms(cvflowsolver) [file join $bindir ${SV_FLOWSOLVER_EXE}$execbinext]
set gExternalPrograms(cvadaptor) [file join $bindir ${SV_MESHSIM_ADAPTOR_EXE}$execbinext]
set gExternalPrograms(cvtetadaptor) [file join $bindir ${SV_TETGEN_ADAPTOR_EXE}$execbinext]
set gExternalPrograms(gdcmdump) gdcmdump
set gExternalPrograms(dicom2) [file join $homedir dicom2${CMAKE_EXECUTABLE_SUFFIX}]
")

if(WIN32)
file(APPEND ${TCL_EXTERNAL_CONFIG_FILE} 
"set gExternalPrograms(dcmodify) [file join $homedir dcmodify${CMAKE_EXECUTABLE_SUFFIX}]
set gExternalPrograms(dcmdump) [file join $homedir dcmdump${CMAKE_EXECUTABLE_SUFFIX}]
")
else()
file(APPEND ${TCL_EXTERNAL_CONFIG_FILE} 
"set gExternalPrograms(dcmodify) [file join dcmodify${CMAKE_EXECUTABLE_SUFFIX}]
set gExternalPrograms(dcmdump) [file join dcmdump${CMAKE_EXECUTABLE_SUFFIX}]
")
endif()

