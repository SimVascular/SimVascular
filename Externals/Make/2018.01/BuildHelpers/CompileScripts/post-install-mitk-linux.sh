GCP=cp
GDIRNAME=dirname
GBASENAME=basename
GMKDIR=mkdir
GRM=rm

# paths

export MITK_SRCDIR=REPLACEME_SV_TOP_SRC_DIR_MITK
export MITK_BINDIR=REPLACEME_SV_TOP_BIN_DIR_MITK
export MITK_BLDDIR=REPLACEME_SV_TOP_BLD_DIR_MITK

# build type not used on linux
export MITK_BLDTYPE=

# primary directories to install into

$GMKDIR -p $MITK_BINDIR/bin
$GMKDIR -p $MITK_BINDIR/lib
$GMKDIR -p $MITK_BINDIR/lib/plugins
$GMKDIR -p $MITK_BINDIR/include

$GCP -Rfl $MITK_BLDDIR/MITK-build/bin $MITK_BINDIR
$GCP -Rfl $MITK_BLDDIR/MITK-build/lib $MITK_BINDIR
$GCP -Rfl $MITK_BLDDIR/ep/bin $MITK_BINDIR
$GCP -Rfl $MITK_BLDDIR/ep/lib $MITK_BINDIR
$GCP -Rfl $MITK_BLDDIR/ep/include $MITK_BINDIR
$GCP -Rfl $MITK_BLDDIR/ep/share $MITK_BINDIR
$GCP -Rfl $MITK_BLDDIR/ep/plugins $MITK_BINDIR
$GCP -Rfl $MITK_BLDDIR/ep/src/CTK-build/CMakeExternals/Install/include $MITK_BINDIR
$GCP -Rfl $MITK_BLDDIR/ep/src/CTK-build/CMakeExternals/Install/lib $MITK_BINDIR
$GCP -Rfl $MITK_BLDDIR/ep/src/CTK-build/qRestAPI-build/*.REPLACEME_SV_SO_FILE_EXTENSION $MITK_BINDIR/lib
$GCP -Rfl $MITK_BLDDIR/ep/src/CTK-build/qRestAPI-build/*.h $MITK_BINDIR/include
$GCP -Rfl $MITK_BLDDIR/ep/src/CTK-build/CTK-build/bin/* $MITK_BINDIR/bin
$GCP -fl -d $MITK_BLDDIR/ep/src/CTK-build/CTK-build/bin/$MITK_BLDTYPE/*CTK*.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/lib
$GCP -fl -d $MITK_BLDDIR/ep/src/CTK-build/CTK-build/bin/$MITK_BLDTYPE/liborg*.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/lib/plugins


# copy qRestAPI from CTK-build

$GMKDIR -p $MITK_BINDIR/include/qRestAPI
$GCP -fl $MITK_BLDDIR/ep/src/CTK-build/qRestAPI/*.h $MITK_BINDIR/include/qRestAPI
$GCP -fl $MITK_BLDDIR/ep/src/CTK-build/qRestAPI-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXqRestAPI.REPLACEME_SV_LIB_FILE_EXTENSION $MITK_BINDIR/lib
$GCP -fl -d $MITK_BLDDIR/ep/src/CTK-build/qRestAPI-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXqRestAPI.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/bin
$GCP -fl -d $MITK_BLDDIR/ep/src/CTK-build/qRestAPI-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXqRestAPI.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/lib

# copy PythonQt from CTK-build

$GMKDIR -p $MITK_BINDIR/include/PythonQt
$GCP -fl $MITK_BLDDIR/ep/src/CTK-build/PythonQt/src/*.h $MITK_BINDIR/include/PythonQt
$GCP -fl $MITK_BLDDIR/ep/src/CTK-build/PythonQt/src/gui/*.h $MITK_BINDIR/include/PythonQt
$GCP -fl $MITK_BLDDIR/ep/src/CTK-build/PythonQt-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXPythonQt.REPLACEME_SV_LIB_FILE_EXTENSION $MITK_BINDIR/lib
$GCP -fl -d $MITK_BLDDIR/ep/src/CTK-build/PythonQt-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXPythonQt.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/bin
$GCP -fl -d $MITK_BLDDIR/ep/src/CTK-build/PythonQt-build/$MITK_BLDTYPE/REPLACEME_SV_LIB_FILE_PREFIXPythonQt.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/lib

# remove libs from $MITK_BINDIR/bin
$GRM -Rf  $MITK_BINDIR/bin/*.REPLACEME_SV_SO_FILE_EXTENSION*
$GRM -Rf  $MITK_BINDIR/bin/plugins

# CTK

$GMKDIR -p $MITK_BINDIR/include/ctk

$GCP -fl $MITK_BLDDIR/ep/src/CTK/Libs/Core/*.h $MITK_BINDIR/include/ctk
$GCP -fl $MITK_BLDDIR/ep/src/CTK/Libs/Core/*.tpp $MITK_BINDIR/include/ctk
$GCP -fl $MITK_BLDDIR/ep/src/CTK/Libs/Scripting/Python/Core/*.h $MITK_BINDIR/include/ctk
$GCP -fl $MITK_BLDDIR/ep/src/CTK/Libs/Scripting/Python/Widgets/*.h $MITK_BINDIR/include/ctk
$GCP -fl $MITK_BLDDIR/ep/src/CTK/Libs/Visualization/VTK/Core/*.h $MITK_BINDIR/include/ctk
$GCP -fl $MITK_BLDDIR/ep/src/CTK/Libs/Widgets/*.h $MITK_BINDIR/include/ctk

# $GCP -fl -d $MITK_BLDDIR/ep/src/CTK-build/CTK-build/bin/$MITK_BLDTYPE/*.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/bin
# $GCP -fl -d $MITK_BLDDIR/ep/src/CTK-build/CTK-build/bin/$MITK_BLDTYPE/*.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/lib
$GCP -fl $MITK_BLDDIR/ep/src/CTK-build/CTK-build/bin/$MITK_BLDTYPE/*.REPLACEME_SV_LIB_FILE_EXTENSION $MITK_BINDIR/lib

# copying more than needed here, but not sure how many of the subdirectories are required.
$GCP -Rf  $MITK_BLDDIR/ep/src/CTK/Libs/PluginFramework $MITK_BINDIR/include/ctk

for i in $(find $MITK_BLDDIR/ep/src/CTK-build -name "*Export.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -fl $i $MITK_BINDIR/include/ctk
done

#$GCP -fl $MITK_BLDDIR/MITK-build/lib/plugins/$MITK_BLDTYPE/* $MITK_BINDIR/lib/plugins

# mitk files

#$GCP -fl -d $MITK_BLDDIR/MITK-build/bin/$MITK_BLDTYPE/*.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/bin
#$GCP -fl -d $MITK_BLDDIR/MITK-build/lib/$MITK_BLDTYPE/*.REPLACEME_SV_SO_FILE_EXTENSION* $MITK_BINDIR/lib
#$GCP -fl $MITK_BLDDIR/MITK-build/lib/$MITK_BLDTYPE/*.REPLACEME_SV_LIB_FILE_EXTENSION $MITK_BINDIR/lib

$GMKDIR -p $MITK_BINDIR/include/mitk
$GMKDIR -p $MITK_BINDIR/include/mitk/configs
$GMKDIR -p $MITK_BINDIR/include/mitk/exports
$GMKDIR -p $MITK_BINDIR/include/mitk/ui_files
$GMKDIR -p $MITK_BINDIR/include/mitk/Modules

$GCP $MITK_BLDDIR/MITK-build/*.h $MITK_BINDIR/include/mitk

#
#  plugins
#

# currently require the following plugins:
#
# org.blueberry.core.runtime  (nested)
# org.blueberry.ui.qt (nested)
# org.mitk.core.services
# org.mitk.gui.common
# org.mitk.gui.qt.common
# org.mitk.gui.qt.common.legacy
# org.mitk.gui.qt.datamanager

for i in $MITK_SRCDIR/Plugins/org.mitk.*/src; do
    $GMKDIR -p $MITK_BINDIR/include/mitk/plugins/$($GBASENAME $($GDIRNAME $i))
    $GCP -R $i/*.h $MITK_BINDIR/include/mitk/plugins/$($GBASENAME $($GDIRNAME $i))
done

for i in $MITK_SRCDIR/Plugins/org.mitk.*/src/*; do
    if [ -d $i ];then \
      $GMKDIR -p $MITK_BINDIR/include/mitk/plugins/$($GBASENAME $($GDIRNAME $($GDIRNAME $i)))/$($GBASENAME $i); \
      $GCP -R $i/*.h $MITK_BINDIR/include/mitk/plugins/$($GBASENAME $($GDIRNAME $($GDIRNAME $i)))/$($GBASENAME $i); \
    fi
done

for i in $MITK_SRCDIR/Plugins/org.blueberry.*/src; do
    $GMKDIR -p $MITK_BINDIR/include/mitk/plugins/$($GBASENAME $($GDIRNAME $i))
    $GCP -R $i/*.h $MITK_BINDIR/include/mitk/plugins/$($GBASENAME $($GDIRNAME $i))
done

for i in $MITK_SRCDIR/Plugins/org.blueberry.*/src/*; do
    if [ -d $i ];then \
      $GMKDIR -p $MITK_BINDIR/include/mitk/plugins/$($GBASENAME $($GDIRNAME $($GDIRNAME $i)))/$($GBASENAME $i); \
      $GCP -R $i/*.h $MITK_BINDIR/include/mitk/plugins/$($GBASENAME $($GDIRNAME $($GDIRNAME $i)))/$($GBASENAME $i); \
    fi
done

for i in $(find $MITK_BLDDIR/MITK-build/Plugins -name "*Export.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -fl $i $MITK_BINDIR/include/mitk/exports
done

#
# everything else
#

for i in $MITK_SRCDIR/Modules/*/include; do
    $GMKDIR -p $MITK_BINDIR/include/mitk/$($GBASENAME $($GDIRNAME $i))
    $GCP -R $i $MITK_BINDIR/include/mitk/$($GBASENAME $($GDIRNAME $i))
done

for i in $MITK_SRCDIR/Modules/*/include; do
    $GCP $MITK_BLDDIR/MITK-build/Modules/$($GBASENAME $($GDIRNAME $i))/ui_*.h $MITK_BINDIR/include/mitk/$($GBASENAME $($GDIRNAME $i))
done

for i in $MITK_SRCDIR/Modules/*/*/include; do
    $GMKDIR -p $MITK_BINDIR/include/mitk/$($GBASENAME $($GDIRNAME $($GDIRNAME $i)))/$($GBASENAME $($GDIRNAME $i))
    $GCP -R $i $MITK_BINDIR/include/mitk/$($GBASENAME $($GDIRNAME $($GDIRNAME $i)))/$($GBASENAME $($GDIRNAME $i))
done


for i in $(find $MITK_BLDDIR -name "*Exports.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -fl $i $MITK_BINDIR/include/mitk/exports
done

for i in $(find $MITK_BLDDIR/MITK-build/Modules -name "*Export.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -fl $i $MITK_BINDIR/include/mitk/exports
done

for i in $(find $MITK_BLDDIR/MITK-build/Modules -name "ui_*.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -fl $i $MITK_BINDIR/include/mitk/ui_files
done

for i in $(find $MITK_BLDDIR/MITK-build -name "*Config.h"); do
    echo "$i  $($GBASENAME $i)"
    $GCP -fl $i $MITK_BINDIR/include/mitk/configs
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

	   $GCP -fl $MITK_SRCDIR/Modules/ContourModel/DataManagement/*.h $MITK_BINDIR/include/mitk/Modules/ContourModel/DataManagement
           $GCP -fl $MITK_SRCDIR/Modules/CppMicroServices/core/src/module/*.h $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/module
           $GCP -fl $MITK_SRCDIR/Modules/CppMicroServices/core/src/service/*.h $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/service
           $GCP -fl $MITK_SRCDIR/Modules/CppMicroServices/core/src/util/*.h $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/util
	   $GCP -fl $MITK_SRCDIR/Modules/CppMicroServices/core/src/module/*.tpp $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/module
           $GCP -fl $MITK_SRCDIR/Modules/CppMicroServices/core/src/service/*.tpp $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/service
           $GCP -fl $MITK_SRCDIR/Modules/CppMicroServices/core/src/util/*.tpp $MITK_BINDIR/include/mitk/Modules/CppMicroServices/core/src/util
           $GCP -fl $MITK_SRCDIR/Modules/ImageDenoising/*.h $MITK_BINDIR/include/mitk/Modules/ImageDenoising
	   $GCP -fl $MITK_SRCDIR/Modules/ImageDenoising/*.txx $MITK_BINDIR/include/mitk/Modules/ImageDenoising
           $GCP -fl $MITK_SRCDIR/Modules/LegacyGL/*.h $MITK_BINDIR/include/mitk/Modules/LegacyGL
           $GCP -fl $MITK_SRCDIR/Modules/Multilabel/*.h $MITK_BINDIR/include/mitk/Modules/Multilabel
           $GCP -fl $MITK_SRCDIR/Modules/Overlays/*.h $MITK_BINDIR/include/mitk/Modules/Overlays
           $GCP -fl $MITK_SRCDIR/Modules/Segmentation/Algorithms/*.h $MITK_BINDIR/include/mitk/Modules/Segmentation/Algorithms
           $GCP -fl $MITK_SRCDIR/Modules/Segmentation/Controllers/*.h $MITK_BINDIR/include/mitk/Modules/Segmentation/Controllers
           $GCP -fl $MITK_SRCDIR/Modules/Segmentation/Interactions/*.h $MITK_BINDIR/include/mitk/Modules/Segmentation/Interactions
           $GCP -fl $MITK_SRCDIR/Modules/SegmentationUI/Qmitk/*.h $MITK_BINDIR/include/mitk/Modules/SegmentationUI/Qmitk
           $GCP -fl $MITK_SRCDIR/Modules/SurfaceInterpolation/*.h $MITK_BINDIR/include/mitk/Modules/SurfaceInterpolation
           $GCP -fl $MITK_SRCDIR/Utilities/mbilog/*.h $MITK_BINDIR/include/mitk/Utilities/mbilog

           $GCP -fl $MITK_BLDDIR/MITK-build/Modules/ContourModel/*.h $MITK_BINDIR/include/mitk/Modules/ContourModel
           $GCP -fl $MITK_BLDDIR/MITK-build/Modules/ImageDenoising/*.h $MITK_BINDIR/include/mitk/Modules/ImageDenoising
           $GCP -fl $MITK_BLDDIR/MITK-build/Modules/LegacyGL/*.h $MITK_BINDIR/include/mitk/Modules/LegacyGL
           $GCP -fl $MITK_BLDDIR/MITK-build/Modules/Multilabel/*.h $MITK_BINDIR/include/mitk/Modules/Multilabel
           $GCP -fl $MITK_BLDDIR/MITK-build/Modules/Overlays/*.h $MITK_BINDIR/include/mitk/Modules/Overlays
           $GCP -fl $MITK_BLDDIR/MITK-build/Modules/QtWidgets/*.h $MITK_BINDIR/include/mitk/Modules/QtWidgets
           $GCP -fl $MITK_BLDDIR/MITK-build/Modules/Segmentation/*.h $MITK_BINDIR/include/mitk/Modules/Segmentation
           $GCP -fl $MITK_BLDDIR/MITK-build/Modules/Segmentation/Interactions/*.h $MITK_BINDIR/include/mitk/Modules/Segmentation/Interactions
           $GCP -fl $MITK_BLDDIR/MITK-build/Modules/SegmentationUI/*.h $MITK_BINDIR/include/mitk/Modules/SegmentationUI
           $GCP -fl $MITK_BLDDIR/MITK-build/Modules/SurfaceInterpolation/*.h $MITK_BINDIR/include/mitk/Modules/SurfaceInterpolation

# copy executable
$GCP -fl $MITK_BLDDIR/MITK-build/bin/MitkWorkbench* $MITK_BINDIR/bin
$GCP -fl $MITK_BLDDIR/MITK-build/bin/usResourceCompiler* $MITK_BINDIR/bin
$GCP -fl $MITK_BLDDIR/MITK-build/bin/MitkPluginGenerator* $MITK_BINDIR/bin

# for i in $(find $MITK_BLDDIR/MITK-build/lib/plugins -name "*.REPLACEME_SV_SO_FILE_EXTENSION*"); do
#     echo "$i  $($GBASENAME $i)"
#     $GCP -fl $i $MITK_BINDIR/bin/plugins
# done

# create a wrapper script for python executable

echo "#!/bin/sh -f" > REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
echo "export LD_LIBRARY_PATH=REPLACEME_SV_TOP_BIN_DIR_MITK/lib:\$LD_LIBRARY_PATH" >> REPLACEME_SV_TOP_BIN_DIR_MITK/bin/workbench-wrapper
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
