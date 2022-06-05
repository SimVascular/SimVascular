#
# alias cd=cd_func

"C:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise\VC\Auxiliary\Build\vcvars64.bat"
which CL

if hash CL 2>/dev/null; then
    clfullpath=`which CL`
    clparentdir="$(dirname "$clfullpath")"
    export PATH=$clparentdir::$PATH
fi
if hash vsstrace.exe 2>/dev/null; then
    rcfullpath=`which vsstrace.exe`
    rcparentdir="$(dirname "$rcfullpath")"
    export PATH=$rcparentdir:$PATH
fi
