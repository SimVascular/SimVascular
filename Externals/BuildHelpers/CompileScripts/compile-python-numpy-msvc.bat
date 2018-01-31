
call "C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/vcvarsall.bat" x64

REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python.exe REPLACEME_SV_TOPLEVEL_SRCDIR/BuildHelpers/Originals/python/get-pip.py
REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/pip2.7.exe install Cython --install-option="--no-cython-compile"

set BLAS=None
set LAPACK=None
set ATLAS=None

REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python.exe setup.py build
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python.exe setup.py install --prefix REPLACEME_SV_SPECIAL_TOP_BIN_DIR_PYTHON
