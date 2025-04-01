# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
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
set(proj ml)

#set(SV_ML_DIR /Users/parkerda/tmp/ml-1.0.0)

set(msg "[Externals/CMake/ML.cmake] ")
message(STATUS "${msg} ")
message(STATUS "${msg} -------------------------------------------------------------------------------------")
message(STATUS "${msg} +++++                               ML.cmake                                         ")
message(STATUS "${msg} -------------------------------------------------------------------------------------")
message(STATUS "${msg} proj: ${proj}")
message(STATUS "${msg} SV_ML_DIR: ${SV_ML_DIR}")

# Dependencies
include(ExternalProject)

set(ML_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_SRC_DIR}/${proj}-${SV_EXTERNALS_ML_VERSION})
set(ML_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_PFX_DIR}/${proj}-${SV_EXTERNALS_ML_VERSION})
set(ML_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_BIN_DIR}/${proj}-${SV_EXTERNALS_ML_VERSION})

message(STATUS "${msg} ")
message(STATUS "${msg} ML_SRC_DIR: ${ML_SRC_DIR}")
message(STATUS "${msg} ML_PFX_DIR: ${ML_PFX_DIR}")
message(STATUS "${msg} ML_BIN_DIR: ${ML_BIN_DIR}")

#file(COPY ${SV_ML_DIR}${CMAKE_SOURCE_DIR}/../Python/site-packages/sv_ml/results

ExternalProject_Add("${proj}_networks"
  URL               
  PREFIX            ${ML_PFX_DIR}
  DOWNLOAD_DIR      ${SV_ML_DIR}
  #DOWNLOAD_DIR      ${ML_SRC_DIR}
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/../Python/site-packages/sv_ml/results
  UPDATE_COMMAND    ""
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ""
  INSTALL_COMMAND   ""
)

