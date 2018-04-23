tclsh check_files_and_rename_sv4gui.tcl
mkdir -p backup
mv Plugins backup
mv Modules backup
rm -Rf ../Plugins
rm -Rf ../Modules
mv sv4gui/Modules ..
mv sv4gui/Plugins ..
tclsh change_sv_classes_to_sv4gui_classes.tcl
