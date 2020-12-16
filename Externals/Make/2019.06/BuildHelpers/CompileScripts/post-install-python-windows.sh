# don't need the compilers, but need the S: drive!
REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

GCP=cp
GDIRNAME=dirname
GBASENAME=basename
GMKDIR=mkdir
GMV=mv

$GCP -fl REPLACEME_SV_TOP_BIN_DIR_PYTHON/REPLACEME_SV_PYTHON_EXECUTABLE  REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/svpython.exe
$GCP -fl REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/*  REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts

# need pip to install things
REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/python.exe REPLACEME_SV_TOPLEVEL_SRCDIR/BuildHelpers/Originals/python/get-pip.py

# needed to compile numpy if desired
REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/pip.exe install Cython --install-option="--no-cython-compile"

#must install torando-5.1.1 since tornado-6.0.1 breaks jupyter!!
REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/pip.exe install tornado==5.1.1
#install numpy
REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/pip.exe install numpy==1.14.3
#install jupyter
REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/pip.exe install jupyter
#install tensorflow
REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/pip.exe install tensorflow==1.4.0

###tmppythonpath=`echo REPLACEME_SV_TOP_BIN_DIR_PYTHON/REPLACEME_SV_PYTHON_EXECUTABLE | sed s+/+\\\\\\\\\\\\\\\\+g`
###tmppythonpathlower=`echo $tmppythonpath | sed -e 's/\(.*\)/\L\1/'`
###
###for f in REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/*.exe; do
###    echo "File -> $f"
###    sed -e "s/$tmppythonpath/svpython.exe/g" $f > $f.tmp
###    sed -e "s/$tmppythonpathlower/svpython.exe/g" $f.tmp > $f.tmp.tmp
###    rm $f.tmp
###    mv $f.tmp.tmp $f
###done

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT
