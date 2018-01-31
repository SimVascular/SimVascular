EXTERNALS_TOP=/usr/local/sv/ext
EXTERNALS_BUILD_TOP=$EXTERNALS_TOP/build

#
#  must have write permissions to dirs!
#

sudo mkdir -p /usr/local/sv
sudo chmod a+rwx /usr/local/sv

#
# initial setup
#

echo "Deleting previous build dir ($EXTERNALS_BUILD_TOP)"
rm -Rf $EXTERNALS_BUILD_TOP
mkdir -p $EXTERNALS_BUILD_TOP

echo "Deleting previous src+bin dir ($EXTERNALS_TOP)"
rm -Rf $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/src
cp -Rf BuildHelpers $EXTERNALS_TOP/src

#
# let's do it
#

pushd $EXTERNALS_TOP/src/BuildHelpers

echo "Starting build in ($EXTERNALS_TOP)..."
echo "  note: see logs in stdout.superbuild.txt, tmp/stdout*, etc."
source Scripts/superbuild-mac_osx-clang.sh >& stdout.superbuild.txt

popd


