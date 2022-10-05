#EXTERNALS_TOP=/usr/local/sv/ext/2019.06/release/gl1
EXTERNALS_TOP=/usr/local/sv/ext/2019.06/release/gl2
EXTERNALS_BUILD_TOP=$EXTERNALS_TOP/build

SV_EXTERN_OS=mac_osx

#
#  must have write permissions to dirs!
#

sudo mkdir -p /usr/local/sv
sudo chmod a+rwx /usr/local/sv

#
# initial setup
#

echo "Deleting previous build dir ($EXTERNALS_BUILD_TOP)"
sudo rm -Rf $EXTERNALS_BUILD_TOP
sudo mkdir -p $EXTERNALS_BUILD_TOP

echo "Deleting previous src+bin dir ($EXTERNALS_TOP)"
sudo rm -Rf $EXTERNALS_TOP

sudo mkdir -p /usr/local/sv
sudo chmod a+rwx /usr/local/sv
sudo chown -R $USER /usr/local/sv

sudo mkdir -p $EXTERNALS_TOP
sudo chown -R $USER $EXTERNALS_TOP
sudo chgrp -R $USER $EXTERNALS_TOP
mkdir -p $EXTERNALS_TOP/src
 
cp -Rf BuildHelpers $EXTERNALS_TOP/src
cp -Rf ../../Patches $EXTERNALS_TOP/src/BuildHelpers

#
# let's do it
#

pushd $EXTERNALS_TOP/src/BuildHelpers

echo "Starting build in ($EXTERNALS_TOP)..."
echo "  note: see logs in stdout.superbuild.txt, tmp/stdout*, etc."
source Scripts/superbuild-mac_osx-clang.sh >& stdout.superbuild.txt

popd


