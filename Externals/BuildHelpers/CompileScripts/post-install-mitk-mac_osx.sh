GCP=/opt/local/libexec/gnubin/cp
GDIRNAME=/opt/local/libexec/gnubin/dirname
GBASENAME=/opt/local/libexec/gnubin/basename
GMKDIR=/opt/local/libexec/gnubin/mkdir

# paths

export MITK_SRCDIR=REPLACEME_SV_TOP_SRC_DIR_MITK
export MITK_BINDIR=REPLACEME_SV_TOP_BIN_DIR_MITK
export MITK_BLDDIR=REPLACEME_SV_TOP_BLD_DIR_MITK

# build type not used on linux
export MITK_BLDTYPE=

# primary directories to install into

$GMKDIR -p $MITK_BINDIR/bin
$GMKDIR -p $MITK_BINDIR/bin/plugins
$GMKDIR -p $MITK_BINDIR/lib
$GMKDIR -p $MITK_BINDIR/liball
$GMKDIR -p $MITK_BINDIR/include

# copy qRestAPI from CTK-build

$GMKDIR -p $MITK_BINDIR/include/qRestAPI
$GCP -f $MITK_BLDDIR/ep/src/CTK-build/qRestAPI/*.h $MITK_BINDIR/include/qRestAPI
$GCP -f $MITK_BLDDIR/ep/src/CTK-build/qRestAPI-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXqRestAPI.REPLACEME_SV_LIB_FILE_EXTENSION $MITK_BINDIR/lib
$GCP -f -d $MITK_BLDDIR/ep/src/CTK-build/qRestAPI-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXqRestAPI.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/bin
$GCP -f -d $MITK_BLDDIR/ep/src/CTK-build/qRestAPI-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXqRestAPI.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/lib

# copy PythonQt from CTK-build

$GMKDIR -p $MITK_BINDIR/include/PythonQt
$GCP -f $MITK_BLDDIR/ep/src/CTK-build/PythonQt/src/*.h $MITK_BINDIR/include/PythonQt
$GCP -f $MITK_BLDDIR/ep/src/CTK-build/PythonQt/src/gui/*.h $MITK_BINDIR/include/PythonQt
$GCP -f $MITK_BLDDIR/ep/src/CTK-build/PythonQt-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXPythonQt.REPLACEME_SV_LIB_FILE_EXTENSION $MITK_BINDIR/lib
$GCP -f -d $MITK_BLDDIR/ep/src/CTK-build/PythonQt-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXPythonQt.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/bin
$GCP -f -d $MITK_BLDDIR/ep/src/CTK-build/PythonQt-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXPythonQt.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/lib

# CTK

# Note: this doesn't seem to work anymore!  Manually copying below instead (sigh)
# need to call cmake install for CTK-build sln manually first before copy!
# can probably update cmake file here (update cmake files)

#$GCP -R ep/include/ctk-0.1 $MITK_BINDIR/include
#$GCP ep/lib/ctk-0.1/*.lib $MITK_BINDIR/lib
#$GCP ep/lib/ctk-0.1/plugins/*.lib $MITK_BINDIR/lib
#$GCP ep/lib/ctk-0.1/*.dll $MITK_BINDIR/bin
#$GCP ep/lib/ctk-0.1/designer/*.dll $MITK_BINDIR/bin
#$GCP ep/lib/ctk-0.1/plugins/*.dll $MITK_BINDIR/bin

$GMKDIR -p $MITK_BINDIR/include/ctk

$GCP -f $MITK_BLDDIR/ep/src/CTK/Libs/Core/*.h $MITK_BINDIR/include/ctk
$GCP -f $MITK_BLDDIR/ep/src/CTK/Libs/Core/*.tpp $MITK_BINDIR/include/ctk
$GCP -f $MITK_BLDDIR/ep/src/CTK/Libs/Scripting/Python/Core/*.h $MITK_BINDIR/include/ctk
$GCP -f $MITK_BLDDIR/ep/src/CTK/Libs/Scripting/Python/Widgets/*.h $MITK_BINDIR/include/ctk
$GCP -f $MITK_BLDDIR/ep/src/CTK/Libs/Visualization/VTK/Core/*.h $MITK_BINDIR/include/ctk
$GCP -f $MITK_BLDDIR/ep/src/CTK/Libs/Widgets/*.h $MITK_BINDIR/include/ctk

$GCP -f -d $MITK_BLDDIR/ep/src/CTK-build/CTK-build/bin/$MITK_BLDTYPE/*.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/bin
$GCP -f -d $MITK_BLDDIR/ep/src/CTK-build/CTK-build/bin/$MITK_BLDTYPE/*.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/lib
$GCP -f $MITK_BLDDIR/ep/src/CTK-build/CTK-build/bin/$MITK_BLDTYPE/*.REPLACEME_SV_LIB_FILE_EXTENSION $MITK_BINDIR/lib

# mitk files

$GCP -f -d $MITK_BLDDIR/MITK-build/bin/$MITK_BLDTYPE/*.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/bin
$GCP -f -d $MITK_BLDDIR/MITK-build/lib/$MITK_BLDTYPE/*.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/lib
$GCP -f $MITK_BLDDIR/MITK-build/lib/$MITK_BLDTYPE/*.REPLACEME_SV_LIB_FILE_EXTENSION $MITK_BINDIR/lib

$GMKDIR -p $MITK_BINDIR/include/mitk
$GMKDIR -p $MITK_BINDIR/include/mitk/configs
$GMKDIR -p $MITK_BINDIR/include/mitk/exports
$GMKDIR -p $MITK_BINDIR/include/mitk/ui_files
$GMKDIR -p $MITK_BINDIR/include/mitk/Modules

$GCP $MITK_BLDDIR/MITK-build/*.h $MITK_BINDIR/include/mitk

for i in $($GDIRNAME $MITK_SRCDIR/Modules/*/include); do
    $GMKDIR -p $MITK_BINDIR/include/mitk/$($GBASENAME $i)
    $GCP -R $i/include $MITK_BINDIR/include/mitk/$($GBASENAME $i)
done

for i in $($GDIRNAME $MITK_SRCDIR/Modules/*/include); do
    $GCP $MITK_BLDDIR/MITK-build/Modules/$($GBASENAME $i)/ui_*.h $MITK_BINDIR/include/mitk/$($GBASENAME $i)
done

for i in $($GDIRNAME $MITK_SRCDIR/Modules/*/*/include); do
    $GMKDIR -p $MITK_BINDIR/include/mitk/$($GBASENAME $($GDIRNAME $i))/$($GBASENAME $i)
    $GCP -R $i/include $MITK_BINDIR/include/mitk/$($GBASENAME $($GDIRNAME $i))/$($GBASENAME $i)
done


for i in $(find $MITK_BLDDIR -name "*Exports.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -f $i $MITK_BINDIR/include/mitk/exports
done

for i in $(find $MITK_BLDDIR/MITK-build/Modules -name "*Export.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -f $i $MITK_BINDIR/include/mitk/exports
done

for i in $(find $MITK_BLDDIR/ep/src/CTK-build -name "*Export.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -f $i $MITK_BINDIR/include/mitk/exports
done

for i in $(find $MITK_BLDDIR/MITK-build/Modules -name "ui_*.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -f $i $MITK_BINDIR/include/mitk/ui_files
done

for i in $(find $MITK_BLDDIR/MITK-build -name "*Config.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -f $i $MITK_BINDIR/include/mitk/configs
done

           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/ContourModel/DataManagement
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/module
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/service
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/util
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/ImageDenoising
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/LegacyGL
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/Multilabel
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/Overlays
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/Segmentation/Algorithms
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/Segmentation/Controllers
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/Segmentation/Interactions
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/SegmentationUI/Qmitk
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/SurfaceInterpolation
	   
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/ContourModel
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/ImageDenoising
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/LegacyGL
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/Multilabel
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/Overlays
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/QtWidgets
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/Segmentation
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/Segmentation/Interactions
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/SegmentationUI
           $GMKDIR -p $MITK_BINDIR/include/mitk/Modules/SurfaceInterpolation
           $GMKDIR -p $MITK_BINDIR/include/mitk/Utilities/mbilog

	   $GCP -f $MITK_SRCDIR/Modules/ContourModel/DataManagement/*.h $MITK_BINDIR/include/mitk/Modules/ContourModel/DataManagement
           $GCP -f $MITK_SRCDIR/Modules/CppMicroServices/core/src/module/*.h $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/module
           $GCP -f $MITK_SRCDIR/Modules/CppMicroServices/core/src/service/*.h $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/service
           $GCP -f $MITK_SRCDIR/Modules/CppMicroServices/core/src/util/*.h $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/util
	   $GCP -f $MITK_SRCDIR/Modules/CppMicroServices/core/src/module/*.tpp $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/module
           $GCP -f $MITK_SRCDIR/Modules/CppMicroServices/core/src/service/*.tpp $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/service
           $GCP -f $MITK_SRCDIR/Modules/CppMicroServices/core/src/util/*.tpp $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/util
           $GCP -f $MITK_SRCDIR/Modules/ImageDenoising/*.h $MITK_BINDIR/include/mitk/Modules/ImageDenoising
	   $GCP -f $MITK_SRCDIR/Modules/ImageDenoising/*.txx $MITK_BINDIR/include/mitk/Modules/ImageDenoising
           $GCP -f $MITK_SRCDIR/Modules/LegacyGL/*.h $MITK_BINDIR/include/mitk/Modules/LegacyGL
           $GCP -f $MITK_SRCDIR/Modules/Multilabel/*.h $MITK_BINDIR/include/mitk/Modules/Multilabel
           $GCP -f $MITK_SRCDIR/Modules/Overlays/*.h $MITK_BINDIR/include/mitk/Modules/Overlays
           $GCP -f $MITK_SRCDIR/Modules/Segmentation/Algorithms/*.h $MITK_BINDIR/include/mitk/Modules/Segmentation/Algorithms
           $GCP -f $MITK_SRCDIR/Modules/Segmentation/Controllers/*.h $MITK_BINDIR/include/mitk/Modules/Segmentation/Controllers
           $GCP -f $MITK_SRCDIR/Modules/Segmentation/Interactions/*.h $MITK_BINDIR/include/mitk/Modules/Segmentation/Interactions
           $GCP -f $MITK_SRCDIR/Modules/SegmentationUI/Qmitk/*.h $MITK_BINDIR/include/mitk/Modules/SegmentationUI/Qmitk
           $GCP -f $MITK_SRCDIR/Modules/SurfaceInterpolation/*.h $MITK_BINDIR/include/mitk/Modules/SurfaceInterpolation
           $GCP -f $MITK_SRCDIR/Utilities/mbilog/*.h $MITK_BINDIR/include/mitk/Utilities/mbilog
	   
           $GCP -f $MITK_BLDDIR/MITK-build/Modules/ContourModel/*.h $MITK_BINDIR/include/mitk/Modules/ContourModel
           $GCP -f $MITK_BLDDIR/MITK-build/Modules/ImageDenoising/*.h $MITK_BINDIR/include/mitk/Modules/ImageDenoising
           $GCP -f $MITK_BLDDIR/MITK-build/Modules/LegacyGL/*.h $MITK_BINDIR/include/mitk/Modules/LegacyGL
           $GCP -f $MITK_BLDDIR/MITK-build/Modules/Multilabel/*.h $MITK_BINDIR/include/mitk/Modules/Multilabel
           $GCP -f $MITK_BLDDIR/MITK-build/Modules/Overlays/*.h $MITK_BINDIR/include/mitk/Modules/Overlays
           $GCP -f $MITK_BLDDIR/MITK-build/Modules/QtWidgets/*.h $MITK_BINDIR/include/mitk/Modules/QtWidgets
           $GCP -f $MITK_BLDDIR/MITK-build/Modules/Segmentation/*.h $MITK_BINDIR/include/mitk/Modules/Segmentation
           $GCP -f $MITK_BLDDIR/MITK-build/Modules/Segmentation/Interactions/*.h $MITK_BINDIR/include/mitk/Modules/Segmentation/Interactions
           $GCP -f $MITK_BLDDIR/MITK-build/Modules/SegmentationUI/*.h $MITK_BINDIR/include/mitk/Modules/SegmentationUI
           $GCP -f $MITK_BLDDIR/MITK-build/Modules/SurfaceInterpolation/*.h $MITK_BINDIR/include/mitk/Modules/SurfaceInterpolation

# copy executable
$GCP -fR $MITK_BLDDIR/MITK-build/bin/MitkWorkbench* $MITK_BINDIR/bin
$GCP -f $MITK_BLDDIR/MITK-build/bin/usResourceCompiler* $MITK_BINDIR/bin
$GCP -f $MITK_BLDDIR/MITK-build/bin/MitkPluginGenerator* $MITK_BINDIR/bin

for i in $(find $MITK_BLDDIR -name "*.REPLACEME_SV_SO_FILE_EXTENSION*"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -f -d $i $MITK_BINDIR/liball
done

for i in $(find $MITK_BLDDIR/MITK-build/lib/plugins -name "*.REPLACEME_SV_SO_FILE_EXTENSION*"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -f $i $MITK_BINDIR/bin/plugins
done

# create a wrapper script for python executable

echo "#!/bin/sh -f" > REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "export LD_LIBRARY_PATH=REPLACEME_SV_TOP_BIN_DIR_MITK/lib:\$LD_LIBRARY_PATH" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "export LD_LIBRARY_PATH=REPLACEME_SV_TOP_BIN_DIR_MITK/liball:\$LD_LIBRARY_PATH" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "export LD_LIBRARY_PATH=REPLACEME_SV_TOP_BIN_DIR_MITK/bin:\$LD_LIBRARY_PATH" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "export PYTHONHOME=REPLACEME_SV_TOP_BIN_DIR_PYTHON" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "export PYTHONPATH=REPLACEME_SV_TOP_BIN_DIR_PYTHON/lib/python2.7/lib-dynload:REPLACEME_SV_TOP_BIN_DIR_PYTHON/lib:REPLACEME_SV_TOP_BIN_DIR_PYTHON/lib/python2.7:REPLACEME_SV_TOP_BIN_DIR_PYTHON/lib/python2.7/site-packages" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "if [ \"\$#\" -gt 0 ]" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "then" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "  REPLACEME_SV_TOP_BIN_DIR_MITK/bin/MitkWorkbench \"\$1\" \"\$2\" \"\$3\" \"\$4\" \"\$5\" " >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "else" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "  REPLACEME_SV_TOP_BIN_DIR_MITK/bin/MitkWorkbench" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "fi" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
chmod u+w,a+rx REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
