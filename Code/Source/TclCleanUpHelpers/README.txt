tclsh check_files_and_rename_sv3gui.tcl
mkdir -p backup
mv Plugins backup
mv Modules backup
rm -Rf ../Plugins
rm -Rf ../Modules
mv sv3gui/Modules ..
mv sv3gui/Plugins ..
tclsh change_sv_classes_to_sv3gui_classes.tcl
