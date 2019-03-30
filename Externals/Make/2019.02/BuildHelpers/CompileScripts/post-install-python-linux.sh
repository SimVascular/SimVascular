# don't need the compilers, but need the S: drive!
REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

GCP=cp
GDIRNAME=dirname
GBASENAME=basename
GMKDIR=mkdir
GMV=mv
GRM=rm

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

$GMKDIR REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate

for f in REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/*; do
    shortf="${f##*/}"
    echo "File -> $f"
    sed -e "s+REPLACEME_SV_TOP_BIN_DIR_PYTHON+/usr/local/sv/svpython+g" $f > REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate/$shortf
done

# sed destroys the actual python executable, need to overwrite with original
$GRM -f REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate/python
$GRM -f REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate/python3*
$GCP -f REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate/ 
$GCP -df REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python3* REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate/

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT
