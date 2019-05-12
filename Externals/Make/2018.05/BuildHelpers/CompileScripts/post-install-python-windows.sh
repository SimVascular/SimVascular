# don't need the compilers, but need the S: drive!
REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

GCP=cp
GDIRNAME=dirname
GBASENAME=basename
GMKDIR=mkdir
GMV=mv

$GCP -fl REPLACEME_SV_TOP_BIN_DIR_PYTHON/REPLACEME_SV_PYTHON_EXECUTABLE  REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/svpython.exe

#install jupyter
REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/pip.exe install jupyter

tmppythonpath=`echo REPLACEME_SV_TOP_BIN_DIR_PYTHON/REPLACEME_SV_PYTHON_EXECUTABLE | sed s+/+\\\\\\\\+g`
tmppythonpathlower=`echo $tmppythonpath | sed -e 's/\(.*\)/\L\1/'`

for f in REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/*.exe; do
    echo "File -> $f"
    sed -e "s/$tmppythonpath/svpython.exe/g" $f > $f.tmp
    sed -e "s/$tmppythonpathlower/svpython.exe/g" $f > $f.tmp.tmp
    rm $f.tmp
    mv $f.tmp.tmp $f
done

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT
