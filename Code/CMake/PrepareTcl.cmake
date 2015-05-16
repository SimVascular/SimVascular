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


file(REMOVE_RECURSE ${TEMP_DIR}/Tcl ${TEMP_DIR}/UnTcl)
	file(COPY ${SimVascular_SOURCE_TCL_DIR} DESTINATION ${TEMP_DIR})
	file(RENAME ${TEMP_DIR}/Tcl ${TEMP_DIR}/UnTcl)
	file(MAKE_DIRECTORY ${TEMP_DIR}/Tcl/SimVascular_2.0)
	
	set(TOP_TCL_DIRS Common Visualization SimVascular_2.0)

	foreach(dir ${TOP_TCL_DIRS})
		set(this_dir ${TEMP_DIR}/UnTcl/${dir}/)
		string(LENGTH ${this_dir} begin)
		file(GLOB temp_list ${this_dir}*)
		glob_dirs(SUBDIRS ${temp_list})

		foreach(subdir ${SUBDIRS})
			file(GLOB files ${subdir}/*.tcl)
			string(SUBSTRING ${subdir} ${begin} -1 tclfile)
			set(tclfile "${tclfile}-code.tcl")
			dev_message("Generating ${tclfile}")
			#message("${tclfile}\n${files}")
			combine_files(temp ${files})
			file(WRITE ${TEMP_DIR}/Tcl/SimVascular_2.0/${tclfile} "${temp}")
		endforeach()
	endforeach()

	file(COPY ${TEMP_DIR}/UnTcl/SimVascular_2.0/simvascular_startup.tcl 
		DESTINATION ${TEMP_DIR}/Tcl/SimVascular_2.0/)
	file(COPY ${TEMP_DIR}/UnTcl/SimVascular_2.0/simvascular_vtk_init.tcl 
		DESTINATION ${TEMP_DIR}/Tcl/SimVascular_2.0/)
	file(COPY ${TEMP_DIR}/UnTcl/SimVascular_2.0/simvascular_logo.jpg 
		DESTINATION ${TEMP_DIR}/Tcl/SimVascular_2.0/)
		file(COPY ${TEMP_DIR}/UnTcl/SimVascular_2.0/splash.gif 
		DESTINATION ${TEMP_DIR}/Tcl/SimVascular_2.0/)
	file(COPY ${TEMP_DIR}/UnTcl/SimVascular_2.0/simvascular.rc 
		DESTINATION ${TEMP_DIR}/Tcl/SimVascular_2.0/)
	file(COPY ${TEMP_DIR}/UnTcl/External 
		DESTINATION ${TEMP_DIR}/Tcl/)
	file(COPY ${TEMP_DIR}/UnTcl/Help 
		DESTINATION ${TEMP_DIR}/Tcl/)
	
	file(REMOVE_RECURSE ${TEMP_DIR}/UnTcl)

	exec_program(${TCL_TCLSH} 
		ARGS 
		${SimVascular_SOURCE_DIR}/../Distribution/create_tclIndex.tcl 
		${TEMP_DIR}/Tcl/SimVascular_2.0/
		OUTPUT_VARIABLE TCLOUT
		)