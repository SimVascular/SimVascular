.xfEdit.frame1.helpMenu config \
  -menu {.xfEdit.frame1.helpMenu.m} \
  -text {Help} \
  -underline {0}

if {"[info commands XFDestroy]" != ""} {
  catch "XFDestroy .xfEdit.frame1.helpMenu.m"
} {
  catch "destroy .xfEdit.frame1.helpMenu.m"
}
menu .xfEdit.frame1.helpMenu.m
.xfEdit.frame1.helpMenu.m add command \
  -command {XFProcHelpAbout} \
  -label {About}
.xfEdit.frame1.helpMenu.m add command \
  -command {XFProcHelpHelp HELPXF} \
  -label {XF}
.xfEdit.frame1.helpMenu.m add command \
  -command {XFProcHelpTutorial} \
  -label {Tutorial}


.xfEdit.frame1.more3Menu config \
  -text {}

.xfEdit.frame1.more2Menu config \
  -text {}

.xfEdit.frame1.more1Menu config \
  -text {}

.xfEdit.frame1.optionsMenu config \
  -menu {.xfEdit.frame1.optionsMenu.m} \
  -text {Options} \
  -underline {0}

if {"[info commands XFDestroy]" != ""} {
  catch "XFDestroy .xfEdit.frame1.optionsMenu.m"
} {
  catch "destroy .xfEdit.frame1.optionsMenu.m"
}
menu .xfEdit.frame1.optionsMenu.m
.xfEdit.frame1.optionsMenu.m add command \
  -command {
           global xfFile
           XFProcMiscAppDefaults $xfFile(appdef)} \
  -label {XF Application defaults} \
  -underline {0}
.xfEdit.frame1.optionsMenu.m add separator 
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsBindings} \
  -label {Bindings} \
  -underline {0}
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsGeneral} \
  -label {General options} \
  -underline {0}
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsIconBar} \
  -label {Iconbar configuration} \
  -underline {1}
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsInterpreter} \
  -label {Interpreter options} \
  -underline {0}
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsMenuBar} \
  -label {Menubar configuration} \
  -underline {0}
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsPathFile} \
  -label {Path/file names} \
  -underline {0}
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsSource} \
  -label {Source options} \
  -underline {0}
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsWindow} \
  -label {Window options} \
  -underline {0}
.xfEdit.frame1.optionsMenu.m add separator 
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsSaveModuleList} \
  -label {Save module list} \
  -underline {7}
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsSaveOptions} \
  -label {Save options} \
  -underline {1}
.xfEdit.frame1.optionsMenu.m add command \
  -command {XFProcOptionsSavePositions} \
  -label {Save window positions} \
  -underline {9}


.xfEdit.frame1.miscMenu config \
  -menu {.xfEdit.frame1.miscMenu.m} \
  -text {Misc} \
  -underline {0}

if {"[info commands XFDestroy]" != ""} {
  catch "XFDestroy .xfEdit.frame1.miscMenu.m"
} {
  catch "destroy .xfEdit.frame1.miscMenu.m"
}
menu .xfEdit.frame1.miscMenu.m
.xfEdit.frame1.miscMenu.m add command \
  -command {
           global xfConf
           global xfPath
           XFProcMiscAppDefaults $xfConf(programPath)/[file rootname $xfConf(programName)].ad} \
  -label {Application defaults} \
  -underline {0}
.xfEdit.frame1.miscMenu.m add command \
  -command {XFProcMiscHardcopy} \
  -label {Hardcopy} \
  -underline {0}
.xfEdit.frame1.miscMenu.m add separator 
.xfEdit.frame1.miscMenu.m add command \
  -command {XFProcMiscAliases} \
  -label {Aliases} \
  -underline {1}
.xfEdit.frame1.miscMenu.m add command \
  -command {XFProcMiscModules} \
  -label {Module structure} \
  -underline {0}
.xfEdit.frame1.miscMenu.m add command \
  -command {XFProcMiscImages} \
  -label {Images} \
  -underline {0}
.xfEdit.frame1.miscMenu.m add separator 
.xfEdit.frame1.miscMenu.m add command \
  -command {XFProcMiscTestProgram} \
  -label {Test program} \
  -underline {0}


.xfEdit.frame1.infoMenu config \
  -menu {.xfEdit.frame1.infoMenu.m} \
  -text {Programming} \
  -underline {0}

if {"[info commands XFDestroy]" != ""} {
  catch "XFDestroy .xfEdit.frame1.infoMenu.m"
} {
  catch "destroy .xfEdit.frame1.infoMenu.m"
}
menu .xfEdit.frame1.infoMenu.m
.xfEdit.frame1.infoMenu.m add command \
  -command {XFProcProgGlobals} \
  -label {Global variables} \
  -underline {0}
.xfEdit.frame1.infoMenu.m add command \
  -command {XFProcProgProcs} \
  -label {Procedures} \
  -underline {0}
.xfEdit.frame1.infoMenu.m add command \
  -command {XFProcProgCommands} \
  -label {Commands} \
  -underline {0}
.xfEdit.frame1.infoMenu.m add command \
  -command {XFProcProgWidgetTree} \
  -label {Widget tree} \
  -underline {0}
.xfEdit.frame1.infoMenu.m add separator 
.xfEdit.frame1.infoMenu.m add command \
  -command {XFProcProgShowScript} \
  -label {Show script} \
  -underline {7}
.xfEdit.frame1.infoMenu.m add command \
  -command {XFProcProgEditScript} \
  -label {Edit script} \
  -underline {2}
.xfEdit.frame1.infoMenu.m add command \
  -command {XFProcProgStartupSrc} \
  -label {Startup source} \
  -underline {0}
.xfEdit.frame1.infoMenu.m add command \
  -command {XFProcProgEndSrc} \
  -label {End source} \
  -underline {0}
.xfEdit.frame1.infoMenu.m add separator 
.xfEdit.frame1.infoMenu.m add command \
  -command {XFProcProgErrors} \
  -label {Error status} \
  -underline {8}


.xfEdit.frame1.editMenu config \
  -menu {.xfEdit.frame1.editMenu.m} \
  -text {Edit} \
  -underline {0}

if {"[info commands XFDestroy]" != ""} {
  catch "XFDestroy .xfEdit.frame1.editMenu.m"
} {
  catch "destroy .xfEdit.frame1.editMenu.m"
}
menu .xfEdit.frame1.editMenu.m
.xfEdit.frame1.editMenu.m add command \
  -command {
           global xfStatus
           XFProcEditPaste $xfStatus(path)} \
  -label {Paste} \
  -underline {0}
.xfEdit.frame1.editMenu.m add command \
  -command {
           global xfStatus
           XFProcEditCopy $xfStatus(path)} \
  -label {Copy} \
  -underline {0}
.xfEdit.frame1.editMenu.m add command \
  -command {
           global xfStatus
           XFProcEditCut $xfStatus(path)} \
  -label {Cut} \
  -underline {1}
.xfEdit.frame1.editMenu.m add command \
  -command {
           global xfStatus
           XFProcEditMakeAProc $xfStatus(path)} \
  -label {Make a procedure} \
  -underline {1}
.xfEdit.frame1.editMenu.m add command \
  -command {
           global xfStatus
           XFProcEditDelete $xfStatus(path)} \
  -label {Delete} \
  -underline {0}
.xfEdit.frame1.editMenu.m add separator 
.xfEdit.frame1.editMenu.m add command \
  -command {XFProcEditClearCut} \
  -label {Clear Cutbuffer} \
  -underline {4}
.xfEdit.frame1.editMenu.m add command \
  -command {XFProcEditLoadCut} \
  -label {Load Cutbuffer} \
  -underline {0}
.xfEdit.frame1.editMenu.m add command \
  -command {XFProcEditSaveCut} \
  -label {Save Cutbuffer} \
  -underline {0}
.xfEdit.frame1.editMenu.m add command \
  -command {XFProcEditSaveCutAsTemplate cb} \
  -label {Save Template (cut buffer)} \
  -underline {5}
.xfEdit.frame1.editMenu.m add separator 
.xfEdit.frame1.editMenu.m add command \
  -command {XFProcEditShowCut script} \
  -label {Show Cutbuffer (script)} \
  -underline {1}
.xfEdit.frame1.editMenu.m add command \
  -command {XFProcEditShowCut tree} \
  -label {Show Cutbuffer (tree)} \
  -underline {2}


.xfEdit.frame1.configMenu config \
  -menu {.xfEdit.frame1.configMenu.m} \
  -text {Configuration} \
  -underline {0}

if {"[info commands XFDestroy]" != ""} {
  catch "XFDestroy .xfEdit.frame1.configMenu.m"
} {
  catch "destroy .xfEdit.frame1.configMenu.m"
}
menu .xfEdit.frame1.configMenu.m
.xfEdit.frame1.configMenu.m add command \
  -command {XFProcConfParametersDefault} \
  -label {Parameters} \
  -underline {0}
.xfEdit.frame1.configMenu.m add separator 
.xfEdit.frame1.configMenu.m add command \
  -command {XFProcConfPacking} \
  -label {Packing} \
  -underline {2}
.xfEdit.frame1.configMenu.m add command \
  -command {XFProcConfPlacing} \
  -label {Placing} \
  -underline {2}
.xfEdit.frame1.configMenu.m add command \
  -command {XFProcConfLayout} \
  -label {Layout} \
  -underline {0}
.xfEdit.frame1.configMenu.m add separator 
.xfEdit.frame1.configMenu.m add command \
  -command {XFProcConfBinding} \
  -label {Binding} \
  -underline {0}
.xfEdit.frame1.configMenu.m add separator 
.xfEdit.frame1.configMenu.m add command \
  -command {XFProcConfParametersSmall} \
  -label {Parameters (small)} \
  -underline {2}
.xfEdit.frame1.configMenu.m add command \
  -command {XFProcConfParametersSpecial} \
  -label {Parameters (special)} \
  -underline {12}
.xfEdit.frame1.configMenu.m add command \
  -command {XFProcConfParametersGeneral} \
  -label {Parameters (general)} \
  -underline {12}
.xfEdit.frame1.configMenu.m add command \
  -command {XFProcConfParametersGroups} \
  -label {Parameters (widget groups)} \
  -underline {12}


.xfEdit.frame1.fileMenu config \
  -menu {.xfEdit.frame1.fileMenu.m} \
  -text {File} \
  -underline {0}

if {"[info commands XFDestroy]" != ""} {
  catch "XFDestroy .xfEdit.frame1.fileMenu.m"
} {
  catch "destroy .xfEdit.frame1.fileMenu.m"
}
menu .xfEdit.frame1.fileMenu.m
.xfEdit.frame1.fileMenu.m add command \
  -command {XFProcFileNew} \
  -label {New} \
  -underline {0}
.xfEdit.frame1.fileMenu.m add command \
  -command {XFProcFileLoad} \
  -label {Load...} \
  -underline {0}
.xfEdit.frame1.fileMenu.m add command \
  -command {XFProcFileInsert} \
  -label {Insert...} \
  -underline {0}
.xfEdit.frame1.fileMenu.m add separator 
.xfEdit.frame1.fileMenu.m add command \
  -command {XFProcFileSave} \
  -label {Save} \
  -underline {0}
.xfEdit.frame1.fileMenu.m add command \
  -command {XFProcFileSaveAs} \
  -label {Save as...} \
  -underline {5}
.xfEdit.frame1.fileMenu.m add separator 
.xfEdit.frame1.fileMenu.m add command \
  -command {XFProcFileEnterTCL} \
  -label {Enter TCL code} \
  -underline {0}
.xfEdit.frame1.fileMenu.m add separator 
.xfEdit.frame1.fileMenu.m add command \
  -command {XFProcFileQuit} \
  -label {Quit} \
  -underline {0}





