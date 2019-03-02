EXTERNALS_TOP=/usr/local/sv/ext/2019.02/release/gl2
EXTERNALS_BUILD_TOP=/cygdrive/c/sv18gl2

SV_EXTERN_OS=windows

#
#  must have write permissions to dirs!
#

#
# initial setup
#

echo "Deleting previous build dir ($EXTERNALS_BUILD_TOP)"
rm -Rf $EXTERNALS_BUILD_TOP
mkdir -p $EXTERNALS_BUILD_TOP

echo "Deleting previous src+bin dir ($EXTERNALS_TOP)"
rm -Rf $EXTERNALS_TOP

mkdir -p /usr/local/sv
chmod a+rwx /usr/local/sv

mkdir -p $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/src
cp -Rf BuildHelpers $EXTERNALS_TOP/src
cp -Rf ../../Patches $EXTERNALS_TOP/src/BuildHelpers

#
# let's do it
#

pushd $EXTERNALS_TOP/src/BuildHelpers

echo "Starting build in ($EXTERNALS_TOP)..."
echo "  note: see logs in stdout.superbuild.txt, tmp/stdout*, etc."
echo "  note: see individual build logs as well"
source Scripts/superbuild-windows-cl.sh >& stdout.superbuild.txt

popd
