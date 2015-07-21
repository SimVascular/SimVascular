# Program: xf
# Description: the main editing window
#
# $Header: xfedit.tcl[2.3] Wed Mar 10 12:05:44 1993 garfield@garfield frozen $

##########
# Procedure: XFEdit
# Description: show main editing window
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFEdit {} {
  global xfBind
  global xfConf
  global xfFile
  global xfPath
  global xfStatus
  global xfMisc
  global xfWrongName

  # build widget structure
  XFTmpltToplevel .xfEdit 450x480 {XF}
  wm maxsize .xfEdit 1600 1200 
  XFTmpltFrame .xfEdit.frame1

  XFTmpltFrame .xfEdit.frame10

  XFTmpltFrame .xfEdit.frame2 0

  XFTmpltFrame .xfEdit.frame3 0

  XFTmpltFrame .xfEdit.frame4 0
  
  XFTmpltFrame .xfEdit.frame5

  XFTmpltFrame .xfEdit.frame9 0

  if {!$xfConf(menuBarHidden)} {
    # file menu
    menubutton .xfEdit.frame1.fileMenu \
      -borderwidth 1 \
      -text {}

    # config menu
    menubutton .xfEdit.frame1.configMenu \
      -borderwidth 1 \
      -text {}

    # edit menu
    menubutton .xfEdit.frame1.editMenu \
      -borderwidth 1 \
      -text {}

    # info menu
    menubutton .xfEdit.frame1.infoMenu \
      -borderwidth 1 \
      -text {}

    # misc menu
    menubutton .xfEdit.frame1.miscMenu \
      -borderwidth 1 \
      -text {}

    # options menu
    menubutton .xfEdit.frame1.optionsMenu \
      -borderwidth 1 \
      -text {}

    # add1 menu
    menubutton .xfEdit.frame1.more1Menu \
      -borderwidth 1 \
      -text {}

    # add2 menu
    menubutton .xfEdit.frame1.more2Menu \
      -borderwidth 1 \
      -text {}

    # add3 menu
    menubutton .xfEdit.frame1.more3Menu \
      -borderwidth 1 \
      -text {}

    # help menu
    menubutton .xfEdit.frame1.helpMenu \
      -borderwidth 1 \
      -text {}

    XFMenuBarInit $xfFile(menu) $xfPath(src)/xfdefmenubar.tcl
  }

  if {!$xfConf(iconBarHidden)} {
    XFIconBarInit $xfFile(iconbar) $xfPath(src)/xfdeficonbar.tcl $xfPath(icons)

    XFIconBarShow edit .xfEdit.frame10 $xfConf(iconBar)
  }

  if {!$xfConf(editListsHidden)} {
    # object lists
    XFTmpltListbox .xfEdit.frame2 standard
    .xfEdit.frame2.standard.standard configure \
      -borderwidth 1 \
      -background gray90 \
      -width 12 -height 30

    label .xfEdit.frame2.standard.standardMess \
      -borderwidth 1 \
      -relief raised \
      -text {Standard}

    XFTmpltListbox .xfEdit.frame2 additional
    .xfEdit.frame2.additional.additional configure \
      -borderwidth 1 \
      -background gray90 \
      -width 17 -height 30

    label .xfEdit.frame2.additional.additionalMess \
      -borderwidth 1 \
      -relief raised \
      -text {Additional}

    XFTmpltListbox .xfEdit.frame2 template
    .xfEdit.frame2.template.template configure \
      -borderwidth 1 \
      -background gray90 \
      -width 17 -height 30

    label .xfEdit.frame2.template.templateDir \
      -borderwidth 1 \
      -relief raised

    # type line
    label .xfEdit.frame9.message2 \
      -borderwidth 1 \
      -anchor w \
      -relief raised \
      -text "Current widget type:"
  
    label .xfEdit.frame9.currentType \
      -borderwidth 1 \
      -anchor w \
      -relief raised
  }

  if {!$xfConf(statusHidden)} {
    # status line
    label .xfEdit.frame3.status \
      -borderwidth 1 \
      -anchor w \
      -relief raised
  
    label .xfEdit.frame3.cutPaste \
      -borderwidth 1 \
      -anchor w \
      -relief raised

    # set status line
    set xfStatus(foreground) \
      [lindex [.xfEdit.frame3.status configure -foreground] 4]
    XFEditSetStatus {}

    # set cut buffer status
    XFEditSetCutInfo  
  }

  if {!$xfConf(pathNameHidden)} {
    # path line
    menubutton .xfEdit.frame4.showWindows \
	-pady 5 \
      -borderwidth 1 \
      -menu .xfEdit.frame4.showWindows.m \
      -relief raised \
      -text "Current widget path:"
    menu .xfEdit.frame4.showWindows.m -tearoff 0

    XFTmpltFrame .xfEdit.frame4.path
  
    # set path line
    XFEditSetPath .

    # set window displaying
    XFEditSetShowWindows
  }
  
  if {!$xfConf(editListsHidden)} {
    # the add buttons
    button .xfEdit.frame5.addDefault \
      -borderwidth 1 \
      -text {Add with defaults} \
      -command {XFProcConfAddCurrentItem add}

    scale .xfEdit.frame5.addNumber \
      -borderwidth 1 \
      -from 1 \
      -to 50 \
      -width 11 \
      -sliderlength 20 \
      -showvalue false \
      -orient horizontal \
      -label "Insert 1 item" \
      -command "XFEditSetAddNumber"

    button .xfEdit.frame5.addConfigure \
      -borderwidth 1 \
      -text {Configure and add} \
      -command {XFProcConfAddCurrentItem config}
  }

  if {[XFMiscIsDir $xfPath(elements)]} {
    foreach xfName [lsort [glob -nocomplain $xfPath(elements)/*]] {
      set xfCounter [file tail $xfName]
      if {[XFMiscIsFile $xfPath(elements)/$xfCounter] &&
          "$xfCounter" != "Version" && "$xfCounter" != "Makefile" &&
          "$xfCounter" != "Shapefile" && "$xfCounter" != "Dependencies"} {
        if {"$xfCounter" == "CheckButton" ||
            "$xfCounter" == "RadioButton"} {
          continue
        }
        lappend xfStatus(elementList) $xfCounter
        if {!$xfConf(editListsHidden)} {
          .xfEdit.frame2.standard.standard insert end $xfCounter
        }
      }
    }
  }

  if {!$xfConf(editListsHidden)} {
    # set default type
    XFEditSetType Frame 0
    .xfEdit.frame2.standard.standard select anchor 4
    .xfEdit.frame2.standard.standard select set 4
  }

  # internal selection buffer (not mapped)
  entry .xfEdit.curSelected -exportselection true
  .xfEdit.curSelected delete 0 end
  .xfEdit.curSelected insert 0 .
  .xfEdit.curSelected select from 0
  .xfEdit.curSelected select to end

  foreach xfPathElement [split $xfPath(additionals) $xfMisc(separator)] {
    if {[XFMiscIsDir $xfPathElement]} {
      foreach xfName [lsort [glob -nocomplain $xfPathElement/*]] {
        set xfCounter [file tail $xfName]
        if {[XFMiscIsFile $xfPathElement/$xfCounter] &&
            "$xfCounter" != "Version" && "$xfCounter" != "Makefile" &&
            "$xfCounter" != "Shapefile" && "$xfCounter" != "Dependencies" &&
            ("[info commands $xfCounter]" == "" ||
             "[info commands [string tolower $xfCounter]]" == "" ||
             ([lsearch $xfWrongName "$xfCounter*"] != -1 &&
              "[info commands [lindex [lindex $xfWrongName [lsearch $xfWrongName $xfCounter*]] 1]]" != ""))} {
          set xfTmpFileList($xfCounter) ""
        }
      }
    }
  }
  if {[info exists xfTmpFileList]} {
    foreach xfCounter [lsort [array names xfTmpFileList]] {
      lappend xfStatus(additionalList) $xfCounter
      if {!$xfConf(editListsHidden)} {
        .xfEdit.frame2.additional.additional insert end $xfCounter
      }
    }
  }

  if {!$xfConf(editListsHidden) && !$xfConf(statusHidden)} {
    set xfStatus(tmpltPath) ""
    .xfEdit.frame2.template.templateDir configure \
      -text "Templates"
    XFEditReadTemplates $xfStatus(tmpltPath)
  }

  if {!$xfConf(editListsHidden)} {
    # bindings
    bind .xfEdit.frame2.standard.standard $xfBind(select1) {
      XFBindSelectOne %W %y
      .xfEdit.frame2.additional.additional selection clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 0
      XFProcConfAddCurrentItem add}
    bind .xfEdit.frame2.standard.standard $xfBind(select2) {
      global xfConf
      XFBindSelectOne %W %y
      .xfEdit.frame2.additional.additional selection clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 0
      set xfTmpValue $xfConf(getWidgetName)
      set xfConf(getWidgetName) 0
      XFProcConfAddCurrentItem add
      set xfConf(getWidgetName) $xfTmpValue}
    bind .xfEdit.frame2.standard.standard $xfBind(select3) {
      XFBindSelectOne %W %y
      .xfEdit.frame2.additional.additional selection clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 0
      XFProcConfAddCurrentItem config}
    bind .xfEdit.frame2.standard.standard <ButtonPress-1> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.additional.additional selection clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 0}
    bind .xfEdit.frame2.standard.standard <Button1-Motion> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.additional.additional selection clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 0} 
    bind .xfEdit.frame2.standard.standard <Shift-ButtonPress-1> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.additional.additional selection clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 0}
    bind .xfEdit.frame2.standard.standard <Shift-Button1-Motion> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.additional.additional selection clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 0}

    bind .xfEdit.frame2.additional.additional $xfBind(select1) {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 1
      XFProcConfAddCurrentItem add}
    bind .xfEdit.frame2.additional.additional $xfBind(select3) {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 1
      XFProcConfAddCurrentItem config}
    bind .xfEdit.frame2.additional.additional <ButtonPress-1> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 1}
    bind .xfEdit.frame2.additional.additional <Button1-Motion> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 1}
    bind .xfEdit.frame2.additional.additional <Shift-ButtonPress-1> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 1}
    bind .xfEdit.frame2.additional.additional <Shift-Button1-Motion> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.template.template selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 1}

    bind .xfEdit.frame2.template.template $xfBind(select1) {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.additional.additional selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 2
      XFProcConfAddCurrentItem add}
    bind .xfEdit.frame2.template.template $xfBind(select3) {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.additional.additional selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 2
      XFProcConfAddCurrentItem add}
    bind .xfEdit.frame2.template.template <ButtonPress-1> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.additional.additional selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 2}
    bind .xfEdit.frame2.template.template <Button1-Motion> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.additional.additional selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 2}
    bind .xfEdit.frame2.template.template <Shift-ButtonPress-1> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.additional.additional selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 2}
    bind .xfEdit.frame2.template.template <Shift-Button1-Motion> {
      XFBindSelectOne %W %y
      .xfEdit.frame2.standard.standard select clear 0 end
      .xfEdit.frame2.additional.additional selection clear 0 end
      XFEditSetType [%W get [%W nearest %y]] 2}
  }

  # packing
  if {!$xfConf(menuBarHidden)} {
    pack append .xfEdit.frame1 \
                .xfEdit.frame1.fileMenu {left} \
                .xfEdit.frame1.configMenu {left} \
                .xfEdit.frame1.editMenu {left} \
                .xfEdit.frame1.infoMenu {left} \
                .xfEdit.frame1.miscMenu {left} \
                .xfEdit.frame1.optionsMenu {left} \
                .xfEdit.frame1.helpMenu {right}
  }
  if {!$xfConf(editListsHidden)} {
    pack before .xfEdit.frame2.standard.vscroll \
                .xfEdit.frame2.standard.standardMess {top fillx}
    pack before .xfEdit.frame2.additional.vscroll \
                .xfEdit.frame2.additional.additionalMess {top fillx}
    pack before .xfEdit.frame2.template.vscroll \
                .xfEdit.frame2.template.templateDir {top fillx}
    pack append .xfEdit.frame2 \
                .xfEdit.frame2.standard {left fill} \
                .xfEdit.frame2.additional {left fill} \
                .xfEdit.frame2.template {left fill expand}
    pack append .xfEdit.frame5 \
                .xfEdit.frame5.addDefault {left expand padx 10 pady 10} \
                .xfEdit.frame5.addNumber {left expand padx 10 pady 10} \
                .xfEdit.frame5.addConfigure {left expand padx 10 pady 10}
    pack append .xfEdit.frame9 \
                .xfEdit.frame9.message2 {left} \
                .xfEdit.frame9.currentType {left expand fill}
  }
  if {!$xfConf(statusHidden)} {
    pack append .xfEdit.frame3 \
                .xfEdit.frame3.cutPaste {right} \
                .xfEdit.frame3.status {left expand fill}
  }
  if {!$xfConf(pathNameHidden)} {
    pack append .xfEdit.frame4 \
                .xfEdit.frame4.showWindows {left} \
                .xfEdit.frame4.path {left expand fill}
  }
  pack append .xfEdit \
              .xfEdit.frame1 {top fill} \
              .xfEdit.frame10 {top fill} \
              .xfEdit.frame3 {top fill} \
              .xfEdit.frame4 {top fill} \
              .xfEdit.frame5 {bottom fill} \
              .xfEdit.frame9 {bottom fill} \
              .xfEdit.frame2 {top expand fill}
}

##########
# Procedure: XFEditInsertTmplt
# Description: select and insert a template when double clicked
# Arguments: xfName - the name of the selected template
# Returns: none
# Sideeffects: none
##########
proc XFEditInsertTmplt {xfName} {
  global xfPath
  global xfStatus
  global xfMisc

  set xfTmpEntry $xfName
  # one directory up
  if {"$xfTmpEntry" == "../"} {
    set xfTmpEntry [string range $xfStatus(tmpltPath) 0 \
          [expr [string length $xfStatus(tmpltPath)]-2]]
    if {[string last "/" $xfTmpEntry] <= 0} {
      set xfStatus(tmpltPath) ""
    } {
      set xfStatus(tmpltPath) [string range $xfTmpEntry 0 \
            [expr [string last "/" $xfTmpEntry]-1]]
    }
    if {"$xfStatus(tmpltPath)" == ""} {
      .xfEdit.frame2.template.templateDir configure -text \
        "Templates"
    } {
      .xfEdit.frame2.template.templateDir configure -text \
        .../[file tail $xfStatus(tmpltPath)]
    }
    XFEditReadTemplates $xfStatus(tmpltPath)
  } {
    set xfIsDir 0
    set xfIsExec 0
    if {"[string index $xfTmpEntry \
          [expr [string length $xfTmpEntry]-1]]" == "/" ||
        "[string index $xfTmpEntry \
          [expr [string length $xfTmpEntry]-1]]" == "@"} {
      set xfFileName [string range $xfTmpEntry 0 \
            [expr [string length $xfTmpEntry]-2]]
      foreach xfPathElement [split $xfPath(templates) $xfMisc(separator)] {
        if {[XFMiscIsDir $xfPathElement/$xfStatus(tmpltPath)/$xfFileName]} {
          set xfIsDir 1
          break
        }
      }
      if {!$xfIsDir} {
        set xfFileName $xfTmpEntry
      }
    } {
      if {"[string index $xfTmpEntry \
            [expr [string length $xfTmpEntry]-1]]" == "*"} {
        set xfFileName [string range $xfTmpEntry 0 \
              [expr [string length $xfTmpEntry]-2]]
        foreach xfPathElement [split $xfPath(templates) $xfMisc(separator)] {
          if {[file executable $xfPathElement/$xfStatus(tmpltPath)/$xfFileName]} {
            set xfIsExec 1
            break
          }
        }
        if {!$xfIsEcec} {
          set xfFileName $xfTmpEntry
        }
      } {
        set xfFileName $xfTmpEntry
        foreach xfPathElement [split $xfPath(templates) $xfMisc(separator)] {
          if {[file exists $xfPathElement/$xfStatus(tmpltPath)/$xfFileName.t]} {
            break
          }
        }
      }
    }
    if {$xfIsDir} {
      if {"$xfStatus(tmpltPath)" == "" ||
          "$xfStatus(tmpltPath)" == "/"} {
        append xfStatus(tmpltPath) $xfFileName
      } {
        append xfStatus(tmpltPath) / $xfFileName
      }
      .xfEdit.frame2.template.templateDir configure \
        -text .../$xfFileName
      XFEditReadTemplates $xfStatus(tmpltPath)
    } {
      XFEditSetStatus "Inserting template $xfFileName..."
      XFPasteFile $xfStatus(path) $xfPathElement/$xfStatus(tmpltPath)/$xfFileName.t
      XFEditSetStatus "Inserting template $xfFileName...done"
    }
  }
}

##########
# Procedure: XFEditMakeDestroy
# Description: create a DestroyWindow procedure
# Arguments: xfW - the widget to destroy
# Returns: none
# Sideeffects: a new procedure named DestroyWindow... is created
##########
proc XFEditMakeDestroy {xfW} {

  catch "proc DestroyWindow$xfW {} {# xf ignore me 7
    if {\"\[info procs XFEdit\]\" != \"\"} {
      if {\"\[info commands $xfW\]\" != \"\"} {
        global xfShowWindow$xfW
        set xfShowWindow$xfW 0
        XFEditSetPath .
        after 2 \"XFSaveAsProc $xfW; XFEditSetShowWindows\"
      }
    } {
      catch \"destroy $xfW\"
      update
    }
  }"
}

##########
# Procedure: XFEditReadTemplates
# Description: read the current template directory
# Arguments: xfCurrentPath - the current template directory
# Returns: none
# Sideeffects: none
##########
proc XFEditReadTemplates {xfCurrentPath} {
  global xfPath
  global xfStatus
  global xfMisc

  XFMiscClearList .xfEdit.frame2.template.template
  # add a ..
  if {"$xfCurrentPath" != "" && "$xfCurrentPath" != "/"} {
    .xfEdit.frame2.template.template insert end "../"
  }
  # walk through directory list
  foreach xfPathElement [split $xfPath(templates) $xfMisc(separator)] {
    if {[file exists $xfPathElement/$xfCurrentPath]} {
      foreach xfCounter [Ls -F $xfPathElement/$xfCurrentPath] {
        if {("[string index $xfCounter \
               [expr [string length $xfCounter]-1]]" == "/" ||
             "[string index $xfCounter \
               [expr [string length $xfCounter]-1]]" == "@") &&
            [XFMiscIsDir $xfPathElement/$xfCurrentPath/[string range \
              $xfCounter 0 [expr [string length $xfCounter]-2]]]} {
          if {"AtFS/" != "$xfCounter" && "RCS/" != "$xfCounter" &&
              "AtFS@" != "$xfCounter" && "RCS@" != "$xfCounter"} {
            set tmpFileList($xfCounter) ""
          }
        } {
          if {[string match "*.t" $xfCounter] ||
              [string match "*.t@" $xfCounter]} {
            set tmpFileList([file rootname $xfCounter]) ""
          }
        }
      }
    }
  }
  if {[info exists tmpFileList]} {
    foreach xfCounter [lsort [array names tmpFileList]] {
      .xfEdit.frame2.template.template insert end $xfCounter
    }
  }
}

##########
# Procedure: XFEditSetAddNumber
# Description: set the number of widgets to insert
# Arguments: xfNewValue - the new position of the scale
# Returns: none
# Sideeffects: none
##########
proc XFEditSetAddNumber {xfNewValue} {

  if {$xfNewValue == 1} {
    .xfEdit.frame5.addNumber configure -label \
      "Insert 1 item"
  } {
    .xfEdit.frame5.addNumber configure -label \
      "Insert $xfNewValue items"
  }
}

##########
# Procedure: XFEditSetCutInfo
# Description: set the cutbuffer info field
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFEditSetCutInfo {} {
  global xfStatus

  if {"[info commands .xfEdit.frame3.cutPaste]" == ""} {
    return
  }
  if {$xfStatus(cutBuffer)} {
    .xfEdit.frame3.cutPaste configure \
      -text "not empty"
  } {
    .xfEdit.frame3.cutPaste configure \
      -text "    empty"
  }
}

##########
# Procedure: XFEditSetPath
# Description: set the window path
# Arguments: xfW - the new path
# Returns: none
# Sideeffects: none
##########
proc XFEditSetPath {xfW} {
  global xfConf
  global xfMisc
  global xfStatus

  set xfCurrentItem ""
  set xfItemCounter 1
  set xfOldPosition 1
  set xfPathLength [string length $xfW]
  set xfPosition 1

  if {"[info commands .xfEdit.frame4.path]" == ""} {
    if {![string match ".xf*" $xfW] &&
        ![string match "xf*" [winfo name $xfW]]} {
      set xfStatus(path) $xfW
    }
    return
  }

  foreach xfCounter [winfo children .xfEdit.frame4.path] {
    destroy $xfCounter
  }

  # the root
  menubutton .xfEdit.frame4.path.pathMenu0 \
    -borderwidth 0 \
    -text {.} \
    -padx 0 \
    -pady 0 \
    -menu ".xfEdit.frame4.path.pathMenu0.m"

  menu .xfEdit.frame4.path.pathMenu0.m -tearoff 0
  .xfEdit.frame4.path.pathMenu0.m add command \
    -label "." \
    -command "XFEditSetPath .; XFMiscFlash ."

  foreach xfCounter [winfo children .] {
    if {![string match ".xf*" $xfCounter] &&
        ![string match "xf*" [winfo name $xfCounter]]} {
      .xfEdit.frame4.path.pathMenu0.m add command \
        -label "[string range $xfCounter 1 \
                  [expr [string length $xfCounter]-1]]" \
        -command "XFEditSetPath $xfCounter; XFMiscFlash $xfCounter"
    }
  }

  if {$xfPosition == $xfPathLength} {
    set xfStatus(path) $xfW
  }
  while {$xfPosition < $xfPathLength} {
    while {$xfPosition < $xfPathLength} {
      set xfCurrent [string index $xfW $xfPosition]
      if {[string match $xfCurrent "."] &&
          ![string match $xfCurrent "\*"]} {
        break
      }
      incr xfPosition 1
    }
    if {![string match ".xf*" $xfStatus(path)]} {
      set xfStatus(path) [string range $xfW 0 [expr $xfPosition-1]]
      set xfCurrentItem [string range $xfW $xfOldPosition [expr $xfPosition-1]]
      if {![string match "xf*" $xfCurrentItem]} {
        button .xfEdit.frame4.path.pathLabel$xfItemCounter \
          -borderwidth 0 \
          -relief flat \
          -pady 0 \
          -padx 0 \
          -highlightthickness 0 \
          -text "$xfCurrentItem" \
          -command "
            XFEditSetPath $xfStatus(path)
            XFMiscFlash $xfStatus(path)"

        menubutton .xfEdit.frame4.path.pathMenu$xfItemCounter \
          -borderwidth 0 \
          -text {.} \
          -padx 0 \
          -pady 0 \
          -menu ".xfEdit.frame4.path.pathMenu$xfItemCounter.m"

        menu .xfEdit.frame4.path.pathMenu$xfItemCounter.m -tearoff 0
        foreach xfChildren [winfo children $xfStatus(path)] {
          if {![string match "xf*" [winfo name $xfChildren]]} {
            .xfEdit.frame4.path.pathMenu$xfItemCounter.m add command \
              -label "[string range $xfChildren [expr $xfPosition+1] \
                        [expr [string length $xfChildren]-1]]" \
              -command "XFEditSetPath $xfChildren; XFMiscFlash $xfChildren"
          }
        }
        incr xfItemCounter
      }
    }
    incr xfPosition
    set xfOldPosition $xfPosition
  }

  set xfCounter 0
  while {$xfCounter < $xfItemCounter} {
    if {$xfCounter > 0} {
      pack append .xfEdit.frame4.path \
        .xfEdit.frame4.path.pathLabel$xfCounter {left}
    }
    pack append .xfEdit.frame4.path \
      .xfEdit.frame4.path.pathMenu$xfCounter {left}
    incr xfCounter 1
  }
  
  if {$xfConf(scanTree) && "[info commands .xfParameters]" != ""} {
    XFMiscReadTree . .xfParameters.frame2.widgets.widgets all 1"
  }
  if {$xfConf(scanTree) && "[info commands .xfGroups]" != ""} {
    XFMiscReadTree . .xfGroups.frame2.widgets.widgets all 1"
  }
  if {$xfConf(scanTree) && "[info commands .xfInfoWidgetTree]" != ""} {
    XFInfoWidgetTreeRead $xfMisc(widgetTreeRoot) 10 10
  }
}

##########
# Procedure: XFEditSetShowWindows
# Description: set the window displaying menu
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFEditSetShowWindows {} {

  if {"[info commands .xfEdit.frame4.showWindows]" == ""} {
    return
  }
  # clear menu
  XFMiscDeleteMenuChilds .xfEdit.frame4.showWindows.m

  # clear the window procedures, and variables
  foreach xfCounter [info globals xfShowWindow.*] {
    global $xfCounter
    if {[set $xfCounter]} {
      unset $xfCounter
      if {"[info proc ShowWindow.[string range $xfCounter 13 end]]" != ""} {
        rename ShowWindow.[string range $xfCounter 13 end] {}
      }
    }
    if {"[info proc DestroyWindow.[string range $xfCounter 13 end]]" != ""} {
      rename DestroyWindow.[string range $xfCounter 13 end] {}
    }
  }

  # append all displayed toplevels
  foreach xfCounter [winfo children .] {
    if {"[winfo class $xfCounter]" == "Toplevel"} {
      if {![string match ".xf*" $xfCounter] &&
          ![string match "xf*" [winfo name $xfCounter]]} {
        global xfShowWindow$xfCounter
        set xfShowWindow$xfCounter 1
        catch "proc ShowWindow$xfCounter {args} {# xf ignore me 7}"
        .xfEdit.frame4.showWindows.m add checkbutton \
          -label "[string range $xfCounter 1 \
                    [expr [string length $xfCounter]-1]]" \
          -offvalue 0 \
          -onvalue 1 \
          -variable xfShowWindow$xfCounter \
          -command "
            after 2 \"catch \{XFMiscToplevelRemove $xfCounter\}\""
        XFEditMakeDestroy $xfCounter
      }
    }
  }

  # append all hidden toplevels
  foreach xfCounter [info globals xfShowWindow.*] {
    global $xfCounter
    if {![set $xfCounter]} {
      set $xfCounter 0
      .xfEdit.frame4.showWindows.m add checkbutton \
        -label "[string range $xfCounter 13 \
                  [expr [string length $xfCounter]-1]]" \
        -offvalue 0 \
        -onvalue 1 \
        -variable $xfCounter \
        -command "
          after 2 \"catch \{XFMiscToplevelShow [string range $xfCounter 13 \
            [expr [string length $xfCounter]-1]]\}\""
      XFEditMakeDestroy .[string range $xfCounter 13 \
                          [expr [string length $xfCounter]-1]]
    }
  }
}

##########
# Procedure: XFEditSetStatus
# Description: set the status line
# Arguments: xfNewStatus - new contents of the status line
# Returns: none
# Sideeffects: none
##########
proc XFEditSetStatus {xfNewStatus} {
  global xfConf
  global xfStatus

  if {"[info commands .xfEdit.frame3.status]" != ""} {
    if {$xfStatus(hasColor)} {
      if {[regexp {\.\.\.$} $xfNewStatus] ||
          [regexp {\.\.\.in progress$} $xfNewStatus]} {
        .xfEdit.frame3.status configure \
          -foreground $xfConf(flash)
      } {
        .xfEdit.frame3.status configure \
          -foreground $xfStatus(foreground)
      }
    }
    .xfEdit.frame3.status configure \
      -text $xfNewStatus
  }
  update
  flush stdout
  flush stderr
}

##########
# Procedure: XFEditSetType
# Description: set the type field
# Arguments: xfType - the new type
#            xfListNum - the current list
# Returns: none
# Sideeffects: none
##########
proc XFEditSetType {xfNewType xfListNum} {
  global xfStatus

  set xfStatus(itemList) $xfListNum
  set xfStatus(type) $xfNewType

  if {"[info commands .xfEdit.frame9.currentType]" == ""} {
    return
  }
  .xfEdit.frame9.currentType configure \
    -text $xfNewType
  update
}

# eof

