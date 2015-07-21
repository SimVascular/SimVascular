# Program: xf
# Description: set parameters for widget groups
#
# $Header: xfgroups.tcl[2.4] Wed Mar 10 12:05:59 1993 garfield@garfield frozen $

##########
# Procedure: XFGroups
# Description: set parameters for widget groups
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFGroups {} {
  global xfBind
  global xfConf
  global xfStatus

  # build widget structure
  XFTmpltToplevel .xfGroups 520x420 \
   "XF parameters for groups"

  label .xfGroups.message1 \
    -relief raised \
    -text "Set parameters for widget groups:"

  XFTmpltFrame .xfGroups.frame1 0

  button .xfGroups.frame1.ok \
    -text {OK} \
    -command {destroy .xfGroups}

  button .xfGroups.frame1.rescan \
    -text {Rescan} \
    -command {XFMiscReadTree . .xfGroups.frame2.widgets.widgets all sym}

  checkbutton .xfGroups.frame1.rescanperm \
    -offvalue 0 \
    -onvalue 1 \
    -text {Rescan permanently} \
    -variable xfConf(scanTree)

  XFTmpltFrame .xfGroups.frame4 0

  button .xfGroups.frame4.set \
    -text {Apply} \
    -command {
      set xfResource [.xfGroups.resource.resource get]
      set xfValue [string trim [XFMiscGetText .xfGroups.value.value]]
      if {"$xfResource" != ""} {
        set xfCounter 0
        set xfLast [.xfGroups.frame2.selection.selection size]
        if {"$xfLast" == "none"} {
          set xfLast -1
        }
        while {$xfCounter < $xfLast} {
          XFMiscSetResource \
            [.xfGroups.frame2.selection.selection get $xfCounter] \
            $xfResource $xfValue
          incr xfCounter 1
        }
      } {
        XFProcError "No resource name specified"
      }}

  button .xfGroups.frame4.settree \
    -text {Apply to subtree} \
    -command {
      set xfResource [.xfGroups.resource.resource get]
      set xfValue [string trim [XFMiscGetText .xfGroups.value.value]]
      if {"$xfResource" != ""} {
        set xfCounter 0
        set xfLast [.xfGroups.frame2.selection.selection size]
        if {"$xfLast" == "none"} {
          set xfLast -1
        }
        while {$xfCounter < $xfLast} {
          XFMiscSetResourceToTree \
            [.xfGroups.frame2.selection.selection get $xfCounter] \
              $xfResource $xfValue
          incr xfCounter 1
        }
      } {
        XFProcError "No resource name specified"
      }}

  button .xfGroups.frame4.scan \
    -text {Search} \
    -command {XFGroupsScan}

  button .xfGroups.frame4.clear \
    -text {Clear selection} \
    -command {XFMiscClearList .xfGroups.frame2.selection.selection}

  checkbutton .xfGroups.frame4.clearperm \
    -text {Clear permanently} \
    -offvalue 0 \
    -onvalue 1 \
    -variable xfStatus(clearSelection)

  XFTmpltFrame .xfGroups.frame2 0

  XFTmpltListbox .xfGroups.frame2 widgets
  .xfGroups.frame2.widgets.widgets configure \
    -width 16 -height 30

  label .xfGroups.frame2.widgets.widgetsMess \
    -relief raised \
    -text {Widgets:}

  XFTmpltListbox .xfGroups.frame2 selection
  .xfGroups.frame2.selection.selection configure \
    -width 16 -height 30

  label .xfGroups.frame2.selection.selectionMess \
    -relief raised \
    -text {Selection:}

  XFTmpltListbox .xfGroups.frame2 resources
  .xfGroups.frame2.resources.resources configure \
    -width 16 -height 30

  XFTmpltFrame .xfGroups.frame3 0

  button .xfGroups.frame3.color \
    -text {Color} \
    -command "XFProcColorBox \"\[.xfGroups.resource.resource get\]\" .xfGroups.value.value"

  button .xfGroups.frame3.cursor \
    -text {Cursor} \
    -command "XFProcCursorBox \"\[.xfGroups.resource.resource get\]\" .xfGroups.value.value"

  button .xfGroups.frame3.font \
    -text {Font} \
    -command "XFProcFontBox \"\[.xfGroups.resource.resource get\]\" .xfGroups.value.value"

  button .xfGroups.frame3.pixmap \
    -text {Pixmap} \
    -command "XFProcFSBoxPixmap .xfGroups.value.value"

  button .xfGroups.frame3.file \
    -text {File} \
    -command "XFProcFSBoxFile .xfGroups.value.value"

  label .xfGroups.frame2.resources.resourcesMess \
    -relief raised \
    -text {Resources:}

  XFTmpltLabledEntry .xfGroups resource "Resource:"

  XFTmpltLabledEntry .xfGroups match "Match (shell style):"

  XFTmpltLabledEntry .xfGroups regexp "Regular expression:"

  label .xfGroups.message2 \
    -relief raised \
    -text {Value:}

  XFTmpltText .xfGroups value
  XFMiscSetTextHeight \
    ".xfGroups.value.value value" 4

  menu .xfGroups.frame2.widgets.widgets.m
       .xfGroups.frame2.widgets.widgets.m add command \
         -label {Parameters} \
         -command {
           XFProcConfParametersDefault \
             [lindex [.xfGroups.frame2.widgets.widgets get \
               [.xfGroups.frame2.widgets.widgets curselect]] 1]}
       .xfGroups.frame2.widgets.widgets.m add separator
       .xfGroups.frame2.widgets.widgets.m add command \
         -label {Packing} \
         -command {
           XFProcConfPacking \
             [lindex [.xfGroups.frame2.widgets.widgets get \
               [.xfGroups.frame2.widgets.widgets curselect]] 1]}
       .xfGroups.frame2.widgets.widgets.m add command \
         -label {Placing} \
         -command {
           XFProcConfPlacing \
             [lindex [.xfGroups.frame2.widgets.widgets get \
               [.xfGroups.frame2.widgets.widgets curselect]] 1]}
       .xfGroups.frame2.widgets.widgets.m add separator
       .xfGroups.frame2.widgets.widgets.m add command \
         -label {Binding} \
         -command {
           XFProcConfBinding \
             [lindex [.xfGroups.frame2.widgets.widgets get \
               [.xfGroups.frame2.widgets.widgets curselect]] 1]}
       .xfGroups.frame2.widgets.widgets.m add separator
       .xfGroups.frame2.widgets.widgets.m add command \
         -label {Parameters (small)} \
         -command {
           XFProcConfParametersSmall \
             [lindex [.xfGroups.frame2.widgets.widgets get \
               [.xfGroups.frame2.widgets.widgets curselect]] 1]}
       .xfGroups.frame2.widgets.widgets.m add command \
         -label {Parameters (special)} \
         -command {
           XFProcConfParametersSpecial \
             [lindex [.xfGroups.frame2.widgets.widgets get \
               [.xfGroups.frame2.widgets.widgets curselect]] 1]}
       .xfGroups.frame2.widgets.widgets.m add separator
       .xfGroups.frame2.widgets.widgets.m add command \
         -label {Paste} \
         -command {
           XFProcEditPaste [lindex [.xfGroups.frame2.widgets.widgets get \
                         [.xfGroups.frame2.widgets.widgets curselect]] 1]}
       .xfGroups.frame2.widgets.widgets.m add command \
         -label {Copy} \
         -command {
           XFProcEditCopy [lindex [.xfGroups.frame2.widgets.widgets get \
                        [.xfGroups.frame2.widgets.widgets curselect]] 1]}
       .xfGroups.frame2.widgets.widgets.m add command \
         -label {Cut} \
         -command {
           XFProcEditCut [lindex [.xfGroups.frame2.widgets.widgets get \
                       [.xfGroups.frame2.widgets.widgets curselect]] 1]}
       .xfGroups.frame2.widgets.widgets.m add command \
         -label {Delete} \
         -command {
           XFProcEditDelete [lindex [.xfGroups.frame2.widgets.widgets get \
                          [.xfGroups.frame2.widgets.widgets curselect]] 1]}

  XFMiscReadTree . .xfGroups.frame2.widgets.widgets all sym

  .xfGroups.frame2.resources.resources insert end \
    "activebackground"
  .xfGroups.frame2.resources.resources insert end \
    "activeborderwidth"
  .xfGroups.frame2.resources.resources insert end \
    "activeforeground"
  .xfGroups.frame2.resources.resources insert end \
    "anchor"
  .xfGroups.frame2.resources.resources insert end \
    "background"
  .xfGroups.frame2.resources.resources insert end \
    "bitmap"
  .xfGroups.frame2.resources.resources insert end \
    "borderwidth"
  .xfGroups.frame2.resources.resources insert end \
    "cursor"
  .xfGroups.frame2.resources.resources insert end \
    "disabledforeground"
  .xfGroups.frame2.resources.resources insert end \
    "exportselection"
  .xfGroups.frame2.resources.resources insert end \
    "font"
  .xfGroups.frame2.resources.resources insert end \
    "foreground"
  .xfGroups.frame2.resources.resources insert end \
    "width"
  .xfGroups.frame2.resources.resources insert end \
    "height"
  .xfGroups.frame2.resources.resources insert end \
    "insertbackground"
  .xfGroups.frame2.resources.resources insert end \
    "insertborderwidth"
  .xfGroups.frame2.resources.resources insert end \
    "insertofftime"
  .xfGroups.frame2.resources.resources insert end \
    "insertontime"
  .xfGroups.frame2.resources.resources insert end \
    "insertwidth"
  if {$xfConf(kanji)} {
    .xfGroups.frame2.resources.resources insert end \
      "kanjifont"
  }
  .xfGroups.frame2.resources.resources insert end \
    "label"
  .xfGroups.frame2.resources.resources insert end \
    "orient"
  .xfGroups.frame2.resources.resources insert end \
    "padx"
  .xfGroups.frame2.resources.resources insert end \
    "pady"
  .xfGroups.frame2.resources.resources insert end \
    "relief"
  .xfGroups.frame2.resources.resources insert end \
    "repeatdelay"
  .xfGroups.frame2.resources.resources insert end \
    "repeatinterval"
  .xfGroups.frame2.resources.resources insert end \
    "scrollcommand"
  .xfGroups.frame2.resources.resources insert end \
    "selectbackground"
  .xfGroups.frame2.resources.resources insert end \
    "selectborderwidth"
  .xfGroups.frame2.resources.resources insert end \
    "selectforeground"
  .xfGroups.frame2.resources.resources insert end \
    "setgrid"
  .xfGroups.frame2.resources.resources insert end \
    "state"
  .xfGroups.frame2.resources.resources insert end \
    "text"
  .xfGroups.frame2.resources.resources insert end \
    "textvariable"
  .xfGroups.frame2.resources.resources insert end \
    "underline"
  .xfGroups.frame2.resources.resources insert end \
    "wrap"
  .xfGroups.frame2.resources.resources insert end \
    "xscrollcommand"
  .xfGroups.frame2.resources.resources insert end \
    "yscrollcommand"

  # bindings
  bind .xfGroups.frame2.widgets.widgets "<ButtonPress-$xfBind(popup)>" {
    XFBindSelectOne %W %y
    XFGroupsSelectWidget %W %y
    tk_popup .xfGroups.frame2.widgets.widgets.m %X %Y}
  bind .xfGroups.frame2.widgets.widgets $xfBind(select1) {
    XFGroupsSelectWidget %W %y}
  bind .xfGroups.frame2.widgets.widgets $xfBind(configure) {
    XFBindSelectOne %W %y
    set xfCurSelect [lindex [%W get [%W curselect]] 1]
    XFGroupsSelectWidget %W %y
    XFMiscFlash $xfCurSelect
    XFProcConfParametersDefault $xfCurSelect}
  bind .xfGroups.frame2.widgets.widgets $xfBind(select) {
    XFBindSelectOne %W %y
    set xfCurSelect [lindex [%W get [%W curselect]] 1]
    XFEditSetPath $xfCurSelect
    XFMiscFlash $xfCurSelect
    XFGroupsSelectWidget %W %y}
  bind .xfGroups.frame2.widgets.widgets $xfBind(showName) {
    XFBindSelectOne %W %y
    set xfCurSelect [lindex [%W get [%W curselect]] 1]
    set xfCurSelect2 [%W get [%W curselect]]
    XFEditSetPath $xfCurSelect
    .xfEdit.curSelected delete 0 end
    if {[llength $xfCurSelect2] == 2} {
      .xfEdit.curSelected insert 0 [lindex $xfCurSelect2 1]
    } {
      .xfEdit.curSelected insert 0 "\[SymbolicName \{[lrange $xfCurSelect2 3 end]\}\]"
    }
    .xfEdit.curSelected select anchor 0
    .xfEdit.curSelected select set 0 end
    XFGroupsSelectWidget %W %y}
  bind .xfGroups.frame2.widgets.widgets <ButtonPress-1> {
    XFGroupsSelectWidget %W %y}
  bind .xfGroups.frame2.widgets.widgets <Button1-Motion> {
    XFGroupsSelectWidget %W %y}
  bind .xfGroups.frame2.widgets.widgets <Shift-ButtonPress-1> {
    XFGroupsSelectWidget %W %y}
  bind .xfGroups.frame2.widgets.widgets <Shift-Button1-Motion> {
    XFGroupsSelectWidget %W %y}

  bind .xfGroups.frame2.selection.selection $xfBind(select1) {
    XFBindSelectOne %W %y
    if {[%W nearest %y] >= 0} {
      %W delete [%W nearest %y]}}
  bind .xfGroups.frame2.selection.selection <ButtonPress-1> {
    XFBindSelectOne %W %y
    if {[%W nearest %y] >= 0} {
      %W delete [%W nearest %y]}}
  bind .xfGroups.frame2.selection.selection <Button1-Motion> {
    XFBindSelectOne %W %y
    if {[%W nearest %y] >= 0} {
      %W delete [%W nearest %y]}}
  bind .xfGroups.frame2.selection.selection <Shift-ButtonPress-1> {
    XFBindSelectOne %W %y
    if {[%W nearest %y] >= 0} {
      %W delete [%W nearest %y]}}
  bind .xfGroups.frame2.selection.selection <Shift-Button1-Motion> {
    XFBindSelectOne %W %y
    if {[%W nearest %y] >= 0} {
      %W delete [%W nearest %y]}}

  bind .xfGroups.frame2.resources.resources <ButtonPress-1> {
    XFBindSelectOneIntoEntry %W %y .xfGroups.resource.resource}
  bind .xfGroups.frame2.resources.resources <Button1-Motion> {
    XFBindSelectOneIntoEntry %W %y .xfGroups.resource.resource}
  bind .xfGroups.frame2.resources.resources <Shift-ButtonPress-1> {
    XFBindSelectOneIntoEntry %W %y .xfGroups.resource.resource}
  bind .xfGroups.frame2.resources.resources <Shift-Button1-Motion> {
    XFBindSelectOneIntoEntry %W %y .xfGroups.resource.resource}

  bind .xfGroups.regexp.regexp <Return> {
    XFGroupsScan}

  bind .xfGroups.match.match <Return> {
    XFGroupsScan}

  # packing
  pack before .xfGroups.frame2.widgets.vscroll \
              .xfGroups.frame2.widgets.widgetsMess {top fillx}
  pack before .xfGroups.frame2.selection.vscroll \
              .xfGroups.frame2.selection.selectionMess {top fillx}
  pack before .xfGroups.frame2.resources.vscroll \
              .xfGroups.frame2.resources.resourcesMess {top fillx}
  pack append .xfGroups.frame1 \
              .xfGroups.frame1.ok {left fill expand} \
              .xfGroups.frame1.rescan {left fill expand} \
              .xfGroups.frame1.rescanperm {left fill expand}
  pack append .xfGroups.frame4 \
              .xfGroups.frame4.set {left fill expand} \
              .xfGroups.frame4.settree {left fill expand} \
              .xfGroups.frame4.scan {left fill expand} \
              .xfGroups.frame4.clear {left fill expand} \
              .xfGroups.frame4.clearperm {left fill expand}
  pack append .xfGroups.frame2 \
              .xfGroups.frame2.resources {right fill} \
              .xfGroups.frame2.selection {right fill} \
              .xfGroups.frame2.widgets {left fill expand}
  pack append .xfGroups.frame3 \
              .xfGroups.frame3.color {left fill expand} \
              .xfGroups.frame3.cursor {left fill expand} \
              .xfGroups.frame3.font {left fill expand} \
              .xfGroups.frame3.pixmap {left fill expand} \
              .xfGroups.frame3.file {left fill expand}
  pack append .xfGroups \
              .xfGroups.frame1 {bottom fill} \
              .xfGroups.frame4 {bottom fill} \
              .xfGroups.frame3 {bottom fill} \
              .xfGroups.message1 {top fillx} \
              .xfGroups.value {bottom fill} \
              .xfGroups.message2 {bottom fillx} \
              .xfGroups.resource {bottom fill} \
              .xfGroups.regexp {bottom fill} \
              .xfGroups.match {bottom fill} \
              .xfGroups.frame2 {top fill expand}
}

##########
# Procedure: XFGroupsScan
# Description: search for widgets that match the current regexp
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFGroupsScan {} {
  global xfStatus

  if {$xfStatus(clearSelection)} {
    XFMiscClearList .xfGroups.frame2.selection.selection
  }
  set xfMatched 0
  set xfRegexp [.xfGroups.regexp.regexp get]
  set xfMatch [.xfGroups.match.match get]
  if {"$xfRegexp" != "" || "$xfMatch" != ""} {
    if {"$xfRegexp" != "" && ![catch "regexp \{$xfRegexp\} ." xfResult]} {
      if {$xfResult == 1} {
        set xfMatched 1
      }
    }
    if {"$xfMatch" != "" && ![catch "string match \{$xfMatch\} ." xfResult]} {
      if {$xfResult == 1} {
        set xfMatched 1
      }
    }
    if {$xfMatched} {
      if {$xfResult == 1} {
        set xfInsert 1
        set xfCounter 0
        set xfLast [.xfGroups.frame2.selection.selection size]
        if {"$xfLast" == "none"} {
          set xfLast -1
        }
        while {$xfCounter < $xfLast} {
          if {"." == "[.xfGroups.frame2.selection.selection get $xfCounter]"} {
            set xfInsert 0
            break
          }
          incr xfCounter 1
        }
        if {$xfInsert} {
          .xfGroups.frame2.selection.selection insert end .
        }
      }
    }
    XFGroupsScanTree . $xfRegexp $xfMatch
  }
}

##########
# Procedure: XFGroupsScanTree
# Description: read the complete widget tree and enter matching widgets
# Arguments: xfW - the current widget
#            xfRegexp - the regular expression
#            xfMatch - the match pattern
# Returns: none
# Sideeffects: none
##########
proc XFGroupsScanTree {xfW xfRegexp xfMatch} {

  foreach xfCounter2 [winfo children $xfW] {
    set xfMatched 0
    if {![string match ".xf*" $xfCounter2] &&
        ![string match "xf*" [winfo name $xfCounter2]]} {
      if {"$xfRegexp" != "" &&
          ![catch "regexp \{$xfRegexp\} $xfCounter2" xfResult]} {
        if {$xfResult == 1} {
          set xfMatched 1
        }
      }
      if {"$xfMatch" != "" &&
          ![catch "string match \{$xfMatch\} $xfCounter2" xfResult]} {
        if {$xfResult == 1} {
          set xfMatched 1
        }
      }
      if {$xfMatched} {
        set xfInsert 1
        set xfCounter 0
        set xfLast [.xfGroups.frame2.selection.selection size]
        if {"$xfLast" == "none"} {
          set xfLast -1
        }
        while {$xfCounter < $xfLast} {
          if {"$xfCounter2" == "[.xfGroups.frame2.selection.selection get $xfCounter]"} {
            set xfInsert 0
            break
          }
          incr xfCounter 1
        }
        if {$xfInsert} {
          .xfGroups.frame2.selection.selection insert end \
            $xfCounter2
        }
      }
      XFGroupsScanTree $xfCounter2 $xfRegexp $xfMatch
    }
  }
}

##########
# Procedure: XFGroupsSelectWidget
# Description: select the widget
# Arguments: xfW - the list
#            xfY - the y position in the list
# Returns: none
# Sideeffects: none
##########
proc XFGroupsSelectWidget {xfW xfY} {

  set xfNearest [$xfW nearest $xfY]
  if {$xfNearest >= 0} {
    $xfW select anchor $xfNearest
    $xfW select set $xfNearest
    set xfTmpVal [lindex [$xfW get $xfNearest] 1]
    set xfInsert 1
    set xfCounter 0
    set xfLast [.xfGroups.frame2.selection.selection size]
    if {"$xfLast" == "none"} {
      set xfLast -1
    }
    while {$xfCounter < $xfLast} {
      if {"$xfTmpVal" == "[.xfGroups.frame2.selection.selection get $xfCounter]"} {
        set xfInsert 0
        break
      }
      incr xfCounter 1
    }
    if {$xfInsert} {
      .xfGroups.frame2.selection.selection insert end $xfTmpVal
    }
  }
}

# eof

