# don't need the compilers, but need the S: drive!
REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

GCP=cp
GDIRNAME=dirname
GBASENAME=basename
GMKDIR=mkdir
GMV=mv

$GCP -fl REPLACEME_SV_TOP_BIN_DIR_PYTHON/REPLACEME_SV_PYTHON_EXECUTABLE  REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/svpython

# need pip to install things
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python REPLACEME_SV_TOPLEVEL_SRCDIR/BuildHelpers/Originals/python/get-pip.py

# needed to compile numpy if desired
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install Cython --install-option="--no-cython-compile"

#must install torando-5.1.1 since tornado-6.0.1 breaks jupyter!!
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install tornado==5.1.1
#install numpy
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install numpy
#install jupyter
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install jupyter
#install tensorflow
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install tensorflow

$GMKDIR REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-alt

for f in REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/*; do
    shortf="${f##*/}"
    echo "File -> $f"
    sed -e "s+REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python+svpython+g" $f > REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-alt/$shortf
done

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT
