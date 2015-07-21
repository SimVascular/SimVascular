# Program: xf
# Description: info routine for widget tree displaying
#
# $Header: xfinfoTree.tcl[2.4] Wed Mar 10 12:06:22 1993 garfield@garfield frozen $

##########
# Procedure: XFInfoWidgetTree
# Description: show the widget tree in a canvas
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFInfoWidgetTree {} {
  global xfBind
  global xfConf
  global xfMisc

  set xfMisc(widgetTreeCurrent) ""

  # build widget structure
  XFTmpltToplevel .xfInfoWidgetTree 450x400 {XF widget tree}

  XFTmpltFrame .xfInfoWidgetTree.frame0 0

  button .xfInfoWidgetTree.frame0.ok \
    -text {OK} \
    -command {destroy .xfInfoWidgetTree}

  button .xfInfoWidgetTree.frame0.print \
    -text {Print to (./xfWidgetTree)} \
    -command {
        set xfBBox [.xfInfoWidgetTree.frame1.canvas2 bbox all]
        .xfInfoWidgetTree.frame1.canvas2 postscript -file ./xfWidgetTree -x 0 -y 0 -width [lindex $xfBBox 2] -height [lindex $xfBBox 3]}

  checkbutton .xfInfoWidgetTree.frame0.rescanperm \
    -offvalue 0 \
    -onvalue 1 \
    -text {Rescan permanently} \
    -variable xfConf(scanTree)

  button .xfInfoWidgetTree.frame0.rescan \
    -text {Rescan} \
    -command {XFInfoWidgetTreeRead $xfMisc(widgetTreeRoot) 10 10}

  XFTmpltFrame .xfInfoWidgetTree.frame1 0

  scrollbar .xfInfoWidgetTree.frame1.scrollbar3 \
    -command {.xfInfoWidgetTree.frame1.canvas2 xview} \
    -orient {horizontal} \
    -relief {raised}

  scrollbar .xfInfoWidgetTree.frame1.scrollbar1 \
    -command {.xfInfoWidgetTree.frame1.canvas2 yview} \
    -relief {raised}

  canvas .xfInfoWidgetTree.frame1.canvas2 \
    -confine {true} \
    -height {207} \
    -relief {raised} \
    -scrollregion {0c 0c 20c 20c} \
    -width {295} \
    -xscrollcommand {.xfInfoWidgetTree.frame1.scrollbar3 set} \
    -yscrollcommand {.xfInfoWidgetTree.frame1.scrollbar1 set}

  menu .xfInfoWidgetTree.frame1.canvas2.m
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Parameters} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcConfParametersDefault $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add separator
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Packing} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcConfPacking $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Placing} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcConfPlacing $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add separator
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Binding} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcConfBinding $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add separator
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Parameters (small)} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcConfParametersSmall $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Parameters (special)} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcConfParametersSpecial $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Parameters (general)} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcConfParametersGeneral $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add separator
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Paste} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcEditPaste $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Copy} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcEditCopy $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Cut} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcEditCut $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Delete} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             XFProcEditDelete $xfMisc(widgetTreeCurrent)}}
       .xfInfoWidgetTree.frame1.canvas2.m add separator
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Hide subtree} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             if {[lsearch $xfMisc(widgetTreeHidden) $xfMisc(widgetTreeCurrent)] == -1} {
               lappend xfMisc(widgetTreeHidden) $xfMisc(widgetTreeCurrent)
               XFInfoWidgetTreeRead $xfMisc(widgetTreeRoot) 10 10}}}
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Show subtree} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             set xfTmpPos [lsearch $xfMisc(widgetTreeHidden) $xfMisc(widgetTreeCurrent)]
             if {$xfTmpPos != -1} {
               set xfMisc(widgetTreeHidden) [lreplace $xfMisc(widgetTreeHidden) $xfTmpPos $xfTmpPos]
               XFInfoWidgetTreeRead $xfMisc(widgetTreeRoot) 10 10}}}
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Select as root} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             set xfMisc(widgetTreeRoot) $xfMisc(widgetTreeCurrent)
             XFInfoWidgetTreeRead $xfMisc(widgetTreeRoot) 10 10}}
       .xfInfoWidgetTree.frame1.canvas2.m add command \
         -label {Select . as root} \
         -command {
           global xfMisc
           if {"$xfMisc(widgetTreeCurrent)" != ""} {
             set xfMisc(widgetTreeRoot) .
             XFInfoWidgetTreeRead $xfMisc(widgetTreeRoot) 10 10}}

  XFInfoWidgetTreeRead $xfMisc(widgetTreeRoot) 10 10

  # bindings
  bind .xfInfoWidgetTree.frame1.canvas2 "<ButtonPress-1>" {
    set xfMisc(widgetTreeCurrent) [lindex [lindex [.xfInfoWidgetTree.frame1.canvas2 itemconfig [.xfInfoWidgetTree.frame1.canvas2 find closest [%W canvasx %x] [%W canvasy %y]] -tag] 4] 0]
    tk_popup .xfInfoWidgetTree.frame1.canvas2.m %X %Y}
  bind .xfInfoWidgetTree.frame1.canvas2 $xfBind(configure) {
    set xfW [lindex [lindex [.xfInfoWidgetTree.frame1.canvas2 itemconfig [.xfInfoWidgetTree.frame1.canvas2 find closest [%W canvasx %x] [%W canvasy %y]] -tag] 4] 0]
    if {"$xfW" != ""} {
      XFMiscFlash $xfW
      XFProcConfParametersDefault $xfW}}
  bind .xfInfoWidgetTree.frame1.canvas2 $xfBind(select) {
    set xfW [lindex [lindex [.xfInfoWidgetTree.frame1.canvas2 itemconfig [.xfInfoWidgetTree.frame1.canvas2 find closest [%W canvasx %x] [%W canvasy %y]] -tag] 4] 0]
    if {"$xfW" != ""} {
      XFMiscFlash $xfW
      XFEditSetPath $xfW}}
  bind .xfInfoWidgetTree.frame1.canvas2 $xfBind(showName) {
    set xfW [lindex [lindex [.xfInfoWidgetTree.frame1.canvas2 itemconfig [.xfInfoWidgetTree.frame1.canvas2 find closest [%W canvasx %x] [%W canvasy %y]] -tag] 4] 0]
    if {"$xfW" != ""} {
      .xfEdit.curSelected delete 0 end
      .xfEdit.curSelected insert 0 $xfW
      .xfEdit.curSelected select anchor 0
      .xfEdit.curSelected select set 0 end}}

  # packing
  pack append .xfInfoWidgetTree.frame0 \
              .xfInfoWidgetTree.frame0.ok {left fill expand} \
              .xfInfoWidgetTree.frame0.print {left fill expand} \
              .xfInfoWidgetTree.frame0.rescanperm {left fill expand} \
              .xfInfoWidgetTree.frame0.rescan {left fill expand}
  pack append .xfInfoWidgetTree.frame1 \
              .xfInfoWidgetTree.frame1.scrollbar1 "$xfConf(scrollSide) filly" \
              .xfInfoWidgetTree.frame1.canvas2 {top expand fill} \
              .xfInfoWidgetTree.frame1.scrollbar3 {top fillx} 
  pack append .xfInfoWidgetTree \
              .xfInfoWidgetTree.frame0 {bottom fill} \
              .xfInfoWidgetTree.frame1 {bottom fill expand}
}

##########
# Procedure: XFInfoWidgetTreeRead
# Description: update the widget tree in a canvas
# Arguments: xfW - the current widget
#            xfX - current X offset
#            xfY - current Y offset
# Returns: none
# Sideeffects: none
##########
proc XFInfoWidgetTreeRead {xfW xfX xfY} {
  global symbolicName
  global xfMisc
  global xfPath

  if {"$xfW" == "$xfMisc(widgetTreeRoot)"} {
    set xfMisc(widgetTreeY) 10
    foreach xfCounter [.xfInfoWidgetTree.frame1.canvas2 find all] {
      .xfInfoWidgetTree.frame1.canvas2 delete $xfCounter
    }
  }
  
  if {("[info commands $xfW]" == "" && "$xfW" != ".") ||
      [string match ".xf*" $xfW] ||
      ([string match "xf*" [winfo name $xfW]] && "$xfW" != ".")} {
    return
  }
  if {[file exists $xfPath(lib)/icons/[winfo class $xfW]]} {
    set item [.xfInfoWidgetTree.frame1.canvas2 create bitmap $xfX $xfMisc(widgetTreeY) -bitmap @$xfPath(lib)/icons/[winfo class $xfW] -anchor nw -tag $xfW]
  } {
    if {[file exists $xfPath(lib)/icons/Unknown]} {
      set item [.xfInfoWidgetTree.frame1.canvas2 create bitmap $xfX $xfMisc(widgetTreeY) -bitmap @$xfPath(lib)/icons/Unknown -anchor nw -tag $xfW]
    } {
      set item [.xfInfoWidgetTree.frame1.canvas2 create rectangle $xfX $xfMisc(widgetTreeY) [expr $xfX+40] [expr $xfMisc(widgetTreeY)+40] -tag $xfW]
    }
  }

  set xfCounter3 ""
  foreach xfCounter2 [array names symbolicName] {
    set xfArrayName ""
    append xfArrayName symbolicName ( $xfCounter2 )
    if {"$xfW" == "[set $xfArrayName]"} {
      set xfCounter3 $xfCounter2
      break
    }
  }
  if {"$xfCounter3" != ""} {
    set itemText [.xfInfoWidgetTree.frame1.canvas2 create text [expr $xfX+50] [expr $xfMisc(widgetTreeY)+30] -text "[winfo name $xfW] = $xfCounter3" -anchor sw -tag $xfW]
  } {
    set itemText [.xfInfoWidgetTree.frame1.canvas2 create text [expr $xfX+50] [expr $xfMisc(widgetTreeY)+30] -text [winfo name $xfW] -anchor sw -tag $xfW]
  }

  if {"$xfW" != "$xfMisc(widgetTreeRoot)"} {
    set itemLine [.xfInfoWidgetTree.frame1.canvas2 create line $xfX [expr $xfMisc(widgetTreeY)+20] [expr $xfX-30] [expr $xfMisc(widgetTreeY)+20] [expr $xfX-30] [expr $xfY-10]]
  }

  incr xfMisc(widgetTreeY) 50
  set xfY $xfMisc(widgetTreeY)
  if {[lsearch $xfMisc(widgetTreeHidden) $xfW] != -1} {
    set itemLine [.xfInfoWidgetTree.frame1.canvas2 create line [expr $xfX+20] [expr $xfMisc(widgetTreeY)-5] [expr $xfX+20] [expr $xfY-10]]
  } {
    foreach xfCounter [winfo children $xfW] {
      XFInfoWidgetTreeRead $xfCounter [expr $xfX+50] $xfY
    }
  }

  if {"$xfW" == "$xfMisc(widgetTreeRoot)"} {
    set xfBBox [.xfInfoWidgetTree.frame1.canvas2 bbox all]
    .xfInfoWidgetTree.frame1.canvas2 config -scrollregion "0c 0c [expr [lindex $xfBBox 2]+20] [expr [lindex $xfBBox 3]+20]"
  }
}

# eof

