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

if(NOT TEMP_DIR)
set(TEMP_DIR ${SimVascular_BINARY_DIR}/tmp)
file(MAKE_DIRECTORY ${TEMP_DIR})
endif()
mark_as_superbuild(TEMP_DIR)

get_filename_component(SimVascular_SOURCE_HOME ${SimVascular_SOURCE_DIR}/../ ABSOLUTE)
dev_message("SimVascular Source Home: ${SimVascular_SOURCE_HOME}")
if(NOT SimVascular_BINARY_HOME)
	set(SimVascular_BINARY_HOME ${SimVascular_BINARY_DIR})
endif()

set(SimVascular_HOME ${SimVascular_BINARY_HOME})
set(SimVascular_DISTRIBUTION_DIR ${SimVascular_SOURCE_HOME}/Distribution)
set(SimVascular_BINARY_DISTRIBUTION_DIR ${SimVascular_BINARY_HOME}/Distribution)

mark_as_superbuild(VARS
	SimVascular_SOURCE_HOME:PATH
	SimVascular_BINARY_HOME:PATH
	SimVascular_HOME:PATH
	SimVascular_SOURCE_TCL_DIR:PATH
	SimVascular_BINARY_TCL_DIR:PATH
	SimVascular_TCL:PATH
	SimVascular_DISTRIBUTION_DIR:PATH
	)