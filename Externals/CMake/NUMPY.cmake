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
#
#-----------------------------------------------------------------------------
# PYTHON
set(proj NUMPY)

# Dependencies
set(${proj}_DEPENDENCIES "PYTHON" "PIP")

# Source URL
set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_STANFORD_URL}/numpy/numpy-1.11.1.tar.gz" CACHE STRING "Location of ${proj}, can be web address or local path")
mark_as_advanced(SV_EXTERNALS_${proj}_SOURCE_URL)

# Configure options
set(SV_EXTERNALS_${proj}_INSTALL_OPTIONS
  Cython --install-option=--no-cython-compile)

set(SV_EXTERNALS_${proj}_INSTALL_COMMANDS ${SV_EXTERNALS_PYTHON_EXECUTABLE} ${SV_EXTERNALS_${proj}_SRC_DIR}/setup.py install --prefix ${SV_EXTERNALS_PYTHON_BIN_DIR} --old-and-unmanageable
  COMMAND ${SV_EXTERNALS_PIP_EXECUTABLE} install datetime)

# Add external project
ExternalProject_Add(${proj}
  URL ${SV_EXTERNALS_${proj}_SOURCE_URL}
  PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
  SOURCE_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
  BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
  DEPENDS ${${proj}_DEPENDENCIES}
  CONFIGURE_COMMAND ${SV_EXTERNALS_PIP_EXECUTABLE} install ${SV_EXTERNALS_${proj}_INSTALL_OPTIONS}
  BUILD_COMMAND ${SV_EXTERNALS_PYTHON_EXECUTABLE} ${SV_EXTERNALS_${proj}_SRC_DIR}/setup.py build
  INSTALL_COMMAND ${SV_EXTERNALS_${proj}_INSTALL_COMMANDS}
  UPDATE_COMMAND ""
  )

# NUMPY variables needed later on
set(SV_EXTERNALS_${proj}_SITE_DIR ${SV_EXTERNALS_PYTHON_SITE_DIR}/numpy)
