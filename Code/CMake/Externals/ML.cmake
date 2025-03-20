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

# This is used to copy the machine learning weights to the 
# SimVascular/Python/site-packages/sv_ml/results directory.

set(proj ML)

set(SV_ML_FILE_NAME "networks.tar")

set(msg "[Code/CMake/Externals/ML.cmake]")
message(STATUS "${msg} =============== Code/CMake/Externals    ML.cmake ===============")
message(STATUS "${msg} proj: ${proj}")
message(STATUS "${msg} SV_USE_DIR: ${SV_USE_DIR}")
message(STATUS "${msg} SV_ML_DIR: ${SV_ML_DIR}")
message(STATUS "${msg} SV_ML_FILE_NAME: ${SV_ML_FILE_NAME}")

set(SOURCE_FILE "${SV_ML_DIR}/${SV_ML_FILE_NAME}")
set(DESTINATION_DIR "${CMAKE_SOURCE_DIR}/../Python/site-packages/sv_ml/")
message(STATUS "${msg} SOURCE_FILE: ${SOURCE_FILE}")
message(STATUS "${msg} DESTINATION_DIR: ${DESTINATION_DIR}")

file(COPY "${SOURCE_FILE}" DESTINATION "${DESTINATION_DIR}")

execute_process(
    COMMAND tar -xzf "${DESTINATION_DIR}/${SV_ML_FILE_NAME}" -C "${DESTINATION_DIR}"
    RESULT_VARIABLE TAR_RESULT
    OUTPUT_VARIABLE TAR_OUTPUT
    ERROR_VARIABLE TAR_ERROR
)

if(NOT ${TAR_RESULT} EQUAL 0)
    message(STATUS "Tar extraction failed with error code: ${TAR_RESULT}")
    message(STATUS "Tar output: ${TAR_OUTPUT}")
    message(STATUS "Tar error: ${TAR_ERROR}")
else()
    message(STATUS "Successfully extracted to ${DESTINATION_DIR}")
endif()


