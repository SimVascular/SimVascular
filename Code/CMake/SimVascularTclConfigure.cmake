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
"set SIMVASCULAR_FULL_VER_NO \"${SIMVASCULAR_FULL_VER_NO}\"
set SIMVASCULAR_MAJOR_VER_NO \"${SIMVASCULAR_MAJOR_VER_NO}\"
set SIMVASCULAR_PLATFORM    \"${SIMVASCULAR_PLATFORM}\"
set SIMVASCULAR_MINOR_VERSION    \"${SIMVASCULAR_MINOR_VERSION}\"
set SIMVASCULAR_PATCH_VERSION    \"${SIMVASCULAR_PATCH_VERSION}\"
set SIMVASCULAR_VERSION     \"${SIMVASCULAR_VERSION}\"\n")

file(WRITE ${TCL_SPLASH_CONFIG_FILE} 
"if {$SIMVASCULAR_RELEASE_BUILD == 1} {
	set {gOptions(simvascular_version)} \"VERSION ${SIMVASCULAR_FULL_VER_NO}.${SIMVASCULAR_VERSION_TIMESTAMP}\"
} else {
set {gOptions(simvascular_version)} \"VERSION ${SIMVASCULAR_FULL_VER_NO} (developers build)\"
}\n")

# file(WRITE ${TCL_EXTERNAL_CONFIG_FILE} 
# "set bindir $env(SIMVASCULAR_HOME)/Bin
file(WRITE ${TCL_EXTERNAL_CONFIG_FILE}
"if {$SIMVASCULAR_RELEASE_BUILD == 1} {
	set bindir $env(SIMVASCULAR_HOME)/${SIMVASCULAR_INSTALL_RUNTIME_DIR}
	set homedir $env(SIMVASCULAR_HOME)	
	set execext {${CMAKE_EXECUTABLE_SUFFIX}}
	set execbinext {${CMAKE_EXECUTABLE_SUFFIX}}
	set gExternalPrograms(mpiexec) [file join  $homedir mpiexec${CMAKE_EXECUTABLE_SUFFIX}]
} else {
	set bindir $env(SIMVASCULAR_HOME)/Bin
	set homedir $env(SIMVASCULAR_HOME)
	set execext {${CMAKE_EXECUTABLE_SUFFIX}}
	set execbinext {${CMAKE_EXECUTABLE_SUFFIX}}
	set gExternalPrograms(mpiexec) {${MPIEXEC}}
}
set gExternalPrograms(cvpresolver) [file join $bindir ${PRESOLVER_EXE}$execbinext]
set gExternalPrograms(cvpostsolver) [file join $bindir ${POSTSOLVER_EXE}$execbinext]
set gExternalPrograms(cvflowsolver) [file join $bindir ${FLOWSOLVER_EXE}$execbinext]
set gExternalPrograms(cvadaptor) [file join $bindir ${ADAPTOR_EXE}$execbinext]
set gExternalPrograms(cvtetadaptor) [file join $bindir ${TET_ADAPTOR_EXE}$execbinext]
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

