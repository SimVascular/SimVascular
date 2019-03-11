# don't need the compilers, but need the S: drive!
REPLACEME_SV_SPECIAL_COMPILER_SCRIPT

GCP=cp
GDIRNAME=dirname
GBASENAME=basename
GMKDIR=mkdir
GMV=mv

# paths

export OPENCASCADE_SRCDIR=REPLACEME_SV_TOP_SRC_DIR_OPENCASCADE
export OPENCASCADE_BINDIR=REPLACEME_SV_TOP_BIN_DIR_OPENCASCADE
export OPENCASCADE_BLDDIR=REPLACEME_SV_TOP_BLD_DIR_OPENCASCADE

# need build type for windows
export OPENCASCADE_BLDTYPE=REPLACEME_SV_CMAKE_BUILD_TYPE

# primary directories to install into

$GCP -Rfl $OPENCASCADE_BINDIR/bini $OPENCASCADE_BINDIR/bin
$GCP -Rfl $OPENCASCADE_BINDIR/libi $OPENCASCADE_BINDIR/lib

REPLACEME_SV_SPECIAL_COMPILER_END_SCRIPT
