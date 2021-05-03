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

# purge if a previous installation of python existed
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip cache purge

# needed to compile numpy if desired
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install Cython --install-option="--no-cython-compile"

#must install torando-5.1.1 since tornado-6.0.1 breaks jupyter!!
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install tornado==5.1.1
#install numpy
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install numpy==1.14.3
#install jupyter
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install jupyter==1.0.0
#install tensorflow
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install tensorflow==1.14.0
#install pwlf for 1-D python code
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install pwlf==2.0.4
#install scipy for 1-D python code
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install scipy==1.6.0
#install pydoe for 1-D python code
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install pydoe==0.3.8
#install pyyaml needed for ML
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/pip install pyyaml==3.12

$GMKDIR REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate

for f in REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/*; do
    shortf="${f##*/}"
    echo "File -> $f"
    sed -e "s+REPLACEME_SV_TOP_BIN_DIR_PYTHON+/usr/local/sv/svpython+g;s+#!/usr/local/sv/svpython/bin/python+#!/usr/local/sv/svpython/bin/svpython+g" $f > REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate/$shortf
done

# sed destroys the actual python executable, need to overwrite with original
$GRM -f REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate/python
$GRM -f REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate/python3*
$GCP -f REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate/ 
$GCP -df REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python3* REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin-relocate/

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT
