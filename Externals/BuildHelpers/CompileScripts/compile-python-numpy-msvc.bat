
call "C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/vcvarsall.bat" x64

REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python.exe C:/cygwin64/usr/local/sv/ext/src/BuildHelpers/Originals/python/get-pip.py
REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/pip2.7.exe install Cython --install-option="--no-cython-compile"

set BLAS=None
set LAPACK=None
set ATLAS=None

REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python.exe setup.py build
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python.exe setup.py install --prefix C:\\cygwin64/usr/local/sv/ext/bin/msvc-12.5/x64/python-2.7.11
