EXTERNALS_TOP=/usr/local/sv/ext
EXTERNALS_BUILD_TOP=/cygdrive/c/sv

#
# initial setup
#

echo "Deleting previous build dir ($EXTERNALS_BUILD_TOP)"
rm -Rf $EXTERNALS_BUILD_TOP
mkdir $EXTERNALS_BUILD_TOP

echo "Deleting previous src+bin dir ($EXTERNALS_TOP)"
rm -Rf $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/src
cp -Rf BuildHelpers $EXTERNALS_TOP/src

#
# wget the original src files
#

source build-sv-externals-helper-wget-generic.sh
mv Originals $EXTERNALS_TOP/src/BuildHelpers

#
# let's do it
#

pushd $EXTERNALS_TOP/src/BuildHelpers

echo "Starting build in ($EXTERNALS_TOP)..."
echo "  note: see logs in stdout.superbuild.txt, tmp/stdout*, etc."
echo "  note: see individual build logs as well"
source Scripts/superbuild-windows-cl.sh >& stdout.superbuild.txt

popd


