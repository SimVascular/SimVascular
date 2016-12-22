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


file(REMOVE_RECURSE ${TEMP_DIR}/Python ${TEMP_DIR}/UnPython)
file(COPY ${SV_SOURCE_PYTHON_DIR} DESTINATION ${TEMP_DIR})
file(RENAME ${TEMP_DIR}/Python ${TEMP_DIR}/UnPython)
file(MAKE_DIRECTORY ${TEMP_DIR}/Python/io)
file(MAKE_DIRECTORY ${TEMP_DIR}/Python/model)
file(MAKE_DIRECTORY ${TEMP_DIR}/Python/model/occt)

set(TOP_PYTHON_DIRS io model)

#foreach(dir ${TOP_PYTHON_DIRS})
#	set(this_dir ${TEMP_DIR}/UnPython/${dir}/)
#	string(LENGTH ${this_dir} begin)
#	file(GLOB temp_list ${this_dir}*)
#	glob_dirs(SUBDIRS ${temp_list})

#	foreach(subdir ${SUBDIRS})
#		file(GLOB files ${subdir}/*.py)
#		string(SUBSTRING ${subdir} ${begin} -1 pyfile)
#		set(pyfile "${pyfile}-code.py")
#		dev_message("Generating ${pyfile}")
#		#message("${pyfile}\n${files}")
#		combine_files(temp ${files})
#		file(WRITE ${TEMP_DIR}/Python/SimVascular_2.0/${pyfile} "${temp}")
#	endforeach()
#endforeach()

file(COPY ${TEMP_DIR}/UnPython/io/process_groups.py
	DESTINATION ${TEMP_DIR}/Python/io/)
file(COPY ${TEMP_DIR}/UnPython/io/__init__.py
	DESTINATION ${TEMP_DIR}/Python/io/)
file(COPY ${TEMP_DIR}/UnPython/model/occt/nurbs_lofting.py
	DESTINATION ${TEMP_DIR}/Python/model/occt/)
file(COPY ${TEMP_DIR}/UnPython/model/occt/__init__.py
	DESTINATION ${TEMP_DIR}/Python/model/occt/__init__.py)
file(REMOVE_RECURSE ${TEMP_DIR}/UnPython)

