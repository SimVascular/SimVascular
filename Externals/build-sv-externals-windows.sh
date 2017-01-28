EXTERNALS_TOP=/usr/local/sv/ext
EXTERNALS_BUILD_TOP=/cygdrive/c/sv

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
   SV_SUPER_OPTIONS="WGET_TCL         UNTAR_TCL         BUILD_TCL         ARCHIVE_TCL         ZIP_TCL         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_PYTHON      UNTAR_PYTHON      BUILD_PYTHON      ARCHIVE_PYTHON      ZIP_PYTHON      $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_NUMPY       UNTAR_NUMPY       BUILD_NUMPY       ARCHIVE_NUMPY       ZIP_NUMPY       $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_FREETYPE    UNTAR_FREETYPE    BUILD_FREETYPE    ARCHIVE_FREETYPE    ZIP_FREETYPE    $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_GDCM        UNTAR_GDCM        BUILD_GDCM        ARCHIVE_GDCM        ZIP_GDCM        $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_VTK         UNTAR_VTK         BUILD_VTK         ARCHIVE_VTK         ZIP_VTK         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_ITK         UNTAR_ITK         BUILD_ITK         ARCHIVE_ITK         ZIP_ITK         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_OPENCASCADE UNTAR_OPENCASCADE BUILD_OPENCASCADE ARCHIVE_OPENCASCADE ZIP_OPENCASCADE $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_MMG         UNTAR_MMG         BUILD_MMG         ARCHIVE_MMG         ZIP_MMG         $SV_SUPER_OPTIONS"
   SV_SUPER_OPTIONS="WGET_MITK        UNTAR_MITK        BUILD_MITK        ARCHIVE_MITK        ZIP_MITK        $SV_SUPER_OPTIONS"
fi

#
#  must have write permissions to dirs!
#

mkdir -p /usr/local/package
chmod a+rwx /usr/local/package

mkdir -p /usr/local/sv
chmod a+rwx /usr/local/sv

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


