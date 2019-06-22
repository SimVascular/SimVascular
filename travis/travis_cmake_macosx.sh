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

export CC="clang"
export CXX="clang++"
export SV_CMAKE_BUILD_TYPE="Release"

#cmake
export SV_CMAKE_CMD="/usr/local/bin/cmake"
export SV_CMAKE_GENERATOR="Unix Makefiles"
export SV_MAKE_CMD="make -j8"

MAKE="make --jobs=$NUM_THREADS --keep-going"

if [[ "$SV_EXTERNALS_VERSION_NUMBER" == "2018.05" ]]
then
  export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/Qt5.6.3/5.6.3/gcc_64/lib:$SV_EXTERNALS_BIN_DIR/qt-5.6.3/5.6.3/gcc_64/lib"
elif [[ "$SV_EXTERNALS_VERSION_NUMBER" == "2019.02" ]]
then
  export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/Qt5.6.3/5.6.3/gcc_64/lib:$SV_EXTERNALS_BIN_DIR/qt-5.6.3/5.6.3/gcc_64/lib"
elif [[ "$SV_EXTERNALS_VERSION_NUMBER" == "2019.06" ]]
then
  export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/Qt5.11.3/5.11.3/gcc_64/lib:$SV_EXTERNALS_BIN_DIR/qt-5.11.3/5.11.3/gcc_64/lib"
fi

mkdir -p $BUILD_DIR

#
# build
#

pushd $BUILD_DIR

"$SV_CMAKE_CMD" \
\
   -G "$SV_CMAKE_GENERATOR" \
\
   -DCMAKE_BUILD_TYPE="$SV_CMAKE_BUILD_TYPE" \
   -DSV_EXTERNALS_VERSION_NUMBER:STRING=${SV_EXTERNALS_VERSION_NUMBER} \
   -DSV_EXTERNALS_USE_PREBUILT_QT:BOOL=${SV_EXTERNALS_USE_PREBUILT_QT} \
   -DSV_EXTERNALS_PREBUILT_QT_PATH:PATH=${SV_EXTERNALS_PREBUILT_QT_PATH} \
   -DQt5_DIR:PATH=${SV_EXTERNALS_PREBUILT_QT_PATH} \
\
 "$SV_CODE_DIR"

$MAKE

popd
