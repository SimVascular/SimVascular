# don't need the compilers, but need the S: drive!
REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

GCP=cp
GDIRNAME=dirname
GBASENAME=basename
GMKDIR=mkdir
GMV=mv

# paths
#REPLACEME_SV_TOP_BIN_DIR_PYTHON/REPLACEME_SV_PYTHON_EXECUTABLE

$GCP -fl REPLACEME_SV_TOP_BIN_DIR_PYTHON/REPLACEME_SV_PYTHON_EXECUTABLE  REPLACEME_SV_TOP_BIN_DIR_PYTHON/bin/svpython.exe

for f in REPLACEME_SV_TOP_BIN_DIR_PYTHON/Scripts/*.exe; do
    echo "File -> $f"
    bbe -e "s/c:\\\\cygwin64\\\\usr\\\\local\\\\sv\\\\ext\\\\2018.05\\\\release\\\\bin\\\\msvc\\\\19.0\\\\x64\\\\release\\\\python-3.5.5\\\\bin\\\\python.exe/svpython.exe/g" $f > $f.tmp
    mv $f.tmp $f
done

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT
