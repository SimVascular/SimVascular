EXTERNALS_TOP=/usr/local/sv/ext
EXTERNALS_BUILD_TOP=$EXTERNALS_TOP/build

#
#  Build everything, but can manually override for advanced users
#

if [ -z "$SV_SUPER_OPTIONS" ]; then
   echo "NOTE: SV_SUPER_OPTIONS defaulting to all possible options."
   rm -Rf tar_output
   mkdir -p tar_output
   rm -Rf zip_output
   mkdir -p zip_output
   SV_SUPER_OPTIONS="UNTAR_UNZIP_ALL TAR_EVERYTHING ZIP_EVERYTHING"
   SV_SUPER_OPTIONS="WGET_TCL WGET_PYTHON WGET_NUMPY WGET_FREETYPE WGET_GDCM WGET_VTK WGET_ITK WGET_OPENCASCADE WGET_MMG WGET_MITK $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="UNTAR_TCL UNTAR_PYTHON UNTAR_NUMPY UNTAR_FREETYPE UNTAR_GDCM UNTAR_VTK UNTAR_ITK UNTAR_OPENCASCADE UNTAR_MMG UNTAR_MITK $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="BUILD_TCL BUILD_PYTHON BUILD_NUMPY BUILD_FREETYPE BUILD_GDCM BUILD_VTK BUILD_ITK BUILD_OPENCASCADE BUILD_MMG BUILD_MITK $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="ARCHIVE_TCL ARCHIVE_PYTHON ARCHIVE_NUMPY ARCHIVE_FREETYPE ARCHIVE_GDCM ARCHIVE_VTK ARCHIVE_ITK ARCHIVE_OPENCASCADE ARCHIVE_MMG ARCHIVE_MITK $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="ZIP_TCL ZIP_PYTHON ZIP_NUMPY ZIP_FREETYPE ZIP_GDCM ZIP_VTK ZIP_ITK ZIP_OPENCASCADE ZIP_MMG ZIP_MITK $SV_SUPER_OPTIONS"
fi

#
#  must have write permissions to dirs!
#

sudo mkdir -p /usr/local/package
sudo chmod a+rwx /usr/local/package

sudo mkdir -p /usr/local/sv
sudo chmod a+rwx /usr/local/sv

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
source Scripts/superbuild-linux-gcc.sh >& stdout.superbuild.txt

popd


