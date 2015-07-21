# Program: xf
# Description: specify packing for widget
#
# $Header: xfpacking.tcl[2.4] Wed Mar 10 12:07:31 1993 garfield@garfield frozen $

##########
# Procedure: XFPacking
# Description: specify packing for widget
# Arguments: xfW - the widget we configure
#            xfType - the type of configuration (add, config)
#            xfClass - the class we currently configure
#                      (if master changes this does not neccessary
#                       reflect the class of the new master...)
#            xfLeader - the leading window
# Returns: none
# Sideeffects: none
##########
proc XFPacking {xfW xfType xfClass {xfLeader ""}} {
  global xfBind
  global xfMisc
  global xfStatus

  set xfMisc(${xfClass},expand) 0
  set xfMisc(${xfClass},frame) c
  set xfMisc(${xfClass},fillX) 0
  set xfMisc(${xfClass},fillY) 0
  set xfMisc(${xfClass},side) top
  set xfMisc(${xfClass},curPacking) ""
  set xfMisc(${xfClass},savePackChilds) ""
  set xfMisc(${xfClass},savePacking) ""

  set xfStatus(firstPacking) 1
  XFEditSetStatus "Calling packing for $xfClass..."

  # build widget structure
  XFTmpltToplevel .xfPacking$xfClass 540x600 \
   "XF packing:[XFMiscPathTail $xfW]" $xfLeader

  XFTmpltFrame .xfPacking$xfClass.frame1 0

  button .xfPacking$xfClass.frame1.ok \
    -text {OK}

  button .xfPacking$xfClass.frame1.apply \
    -text {Apply}

  checkbutton .xfPacking$xfClass.frame1.applyperm \
    -text {Apply permanently} \
    -variable xfConf(applyPacking) \
    -onvalue 1 \
    -offvalue 0

  button .xfPacking$xfClass.frame1.undo \
    -text {Undo}

  button .xfPacking$xfClass.frame1.cancel \
    -text {Cancel}

  label .xfPacking$xfClass.message1 \
    -anchor c \
    -relief raised

  XFTmpltFrame .xfPacking$xfClass.additional 0

  button .xfPacking$xfClass.additional.parameters \
    -text {Parameters}

  button .xfPacking$xfClass.additional.chldparameters \
    -text {Child parameters}

  button .xfPacking$xfClass.additional.binding \
    -text {Binding}

  button .xfPacking$xfClass.additional.chldbinding \
    -text {Child binding}

  XFTmpltFrame .xfPacking$xfClass.functions 0

  button .xfPacking$xfClass.functions.parent \
    -text {Select parent}

  button .xfPacking$xfClass.functions.selchild \
    -text {Select child}

  button .xfPacking$xfClass.functions.rescanmaster \
    -text {Rescan widgets}

  button .xfPacking$xfClass.functions.child \
    -text {Pack child}

  button .xfPacking$xfClass.functions.unchild \
    -text {Unpack child}

  XFTmpltFrame .xfPacking$xfClass.frame2

  XFTmpltFrame .xfPacking$xfClass.frame2.side 0

  XFTmpltFrame .xfPacking$xfClass.frame2.expand 0

  XFTmpltFrame .xfPacking$xfClass.frame2.fill 0

  XFTmpltFrame .xfPacking$xfClass.frame2.frame 0

  XFTmpltFrame .xfPacking$xfClass.frame2.frame.f1 0

  XFTmpltFrame .xfPacking$xfClass.frame2.frame.f2 0

  XFTmpltFrame .xfPacking$xfClass.frame2.frame.f3 0

  XFTmpltFrame .xfPacking$xfClass.frame2.chldframe 0

  XFTmpltFrame .xfPacking$xfClass.frame2.children 0

  label .xfPacking$xfClass.frame2.side.message2 \
    -text {Side:}

  radiobutton .xfPacking$xfClass.frame2.side.top \
    -anchor c \
    -text {Top} \
    -value top \
    -variable xfMisc(${xfClass},side)

  radiobutton .xfPacking$xfClass.frame2.side.bottom \
    -anchor c \
    -text {Bottom} \
    -value bottom \
    -variable xfMisc(${xfClass},side)

  radiobutton .xfPacking$xfClass.frame2.side.left \
    -anchor c \
    -text {Left} \
    -value left \
    -variable xfMisc(${xfClass},side)

  radiobutton .xfPacking$xfClass.frame2.side.right \
    -anchor c \
    -text {Right} \
    -value right \
    -variable xfMisc(${xfClass},side)
  .xfPacking$xfClass.frame2.side.top select

  label .xfPacking$xfClass.frame2.chldframe.message3 \
    -text {Children frame:}

  checkbutton .xfPacking$xfClass.frame2.expand.expand \
    -text {Expand widget} \
    -variable xfMisc(${xfClass},expand) \
    -onvalue 1 \
    -offvalue 0

  label .xfPacking$xfClass.frame2.fill.message4 \
    -text {Fill:}

  checkbutton .xfPacking$xfClass.frame2.fill.fillx \
    -text {X} \
    -onvalue 1 \
    -offvalue 0 \
    -variable xfMisc(${xfClass},fillX)

  checkbutton .xfPacking$xfClass.frame2.fill.filly \
    -text {Y} \
    -onvalue 1 \
    -offvalue 0 \
    -variable xfMisc(${xfClass},fillY)

  label .xfPacking$xfClass.frame2.frame.message5 \
    -text {Frame:}

  radiobutton .xfPacking$xfClass.frame2.frame.f1.nw \
    -anchor w \
    -value nw \
    -text {NW} \
    -variable xfMisc(${xfClass},frame)

  radiobutton .xfPacking$xfClass.frame2.frame.f1.w \
    -anchor w \
    -value w \
    -text {W} \
    -variable xfMisc(${xfClass},frame)

  radiobutton .xfPacking$xfClass.frame2.frame.f1.sw \
    -anchor w \
    -value sw \
    -text {SW} \
    -variable xfMisc(${xfClass},frame)

  radiobutton .xfPacking$xfClass.frame2.frame.f2.n \
    -anchor w \
    -value n \
    -text {N} \
    -variable xfMisc(${xfClass},frame)

  radiobutton .xfPacking$xfClass.frame2.frame.f2.c \
    -anchor w \
    -value center \
    -text {C} \
    -variable xfMisc(${xfClass},frame)

  radiobutton .xfPacking$xfClass.frame2.frame.f2.s \
    -anchor w \
    -value s \
    -text {S} \
    -variable xfMisc(${xfClass},frame)

  radiobutton .xfPacking$xfClass.frame2.frame.f3.ne \
    -anchor w \
    -value ne \
    -text {NE} \
    -variable xfMisc(${xfClass},frame)

  radiobutton .xfPacking$xfClass.frame2.frame.f3.e \
    -anchor w \
    -value e \
    -text {E} \
    -variable xfMisc(${xfClass},frame)

  radiobutton .xfPacking$xfClass.frame2.frame.f3.se \
    -anchor w \
    -value se \
    -text {SE} \
    -variable xfMisc(${xfClass},frame)
  .xfPacking$xfClass.frame2.frame.f2.c select

  XFTmpltFrame .xfPacking$xfClass.frame2.chldframe.pad 0

  label .xfPacking$xfClass.frame2.chldframe.pad.message1 \
    -text "Pad:"

  XFTmpltFrame .xfPacking$xfClass.frame2.chldframe.pad.pad1 0

  XFTmpltFrame .xfPacking$xfClass.frame2.chldframe.pad.pad2 0

  scale .xfPacking$xfClass.frame2.chldframe.pad.pad1.pad1 \
    -from 0 \
    -label "X" \
    -orient horizontal \
    -relief sunken \
    -sliderlength 15 \
    -to 300 \
    -width 8

  scale .xfPacking$xfClass.frame2.chldframe.pad.pad2.pad2 \
    -from 0 \
    -label "Y" \
    -orient horizontal \
    -relief sunken \
    -sliderlength 15 \
    -to 300 \
    -width 8

  XFTmpltFrame .xfPacking$xfClass.frame2.chldframe.ipad 0

  label .xfPacking$xfClass.frame2.chldframe.ipad.message1 \
    -text "IPad:"

  XFTmpltFrame .xfPacking$xfClass.frame2.chldframe.ipad.pad1 0

  XFTmpltFrame .xfPacking$xfClass.frame2.chldframe.ipad.pad2 0

  scale .xfPacking$xfClass.frame2.chldframe.ipad.pad1.pad1 \
    -from 0 \
    -label "X" \
    -orient horizontal \
    -relief sunken \
    -sliderlength 15 \
    -to 300 \
    -width 8

  scale .xfPacking$xfClass.frame2.chldframe.ipad.pad2.pad2 \
    -from 0 \
    -label "Y" \
    -orient horizontal \
    -relief sunken \
    -sliderlength 15 \
    -to 300 \
    -width 8

  XFTmpltListbox .xfPacking$xfClass.frame2.children widgets

  label .xfPacking$xfClass.frame2.children.widgets.message6 \
    -text {Widgets:}
  pack before .xfPacking$xfClass.frame2.children.widgets.vscroll \
              .xfPacking$xfClass.frame2.children.widgets.message6 {top fillx}  

  XFTmpltListbox .xfPacking$xfClass.frame2.children childs

  label .xfPacking$xfClass.frame2.children.childs.message6 \
    -text {Children:}
  pack before .xfPacking$xfClass.frame2.children.childs.vscroll \
              .xfPacking$xfClass.frame2.children.childs.message6 {top fillx}

  scale .xfPacking$xfClass.frame2.children.childs.mover \
    -orient vertical \
    -width 8 \
    -sliderlength 15 \
    -from 0
  pack before .xfPacking$xfClass.frame2.children.childs.vscroll \
              .xfPacking$xfClass.frame2.children.childs.mover {right filly}
  
  XFMiscReadTree $xfW .xfPacking$xfClass.frame2.children.widgets.widgets all

  XFPackingMakeMaster $xfW $xfType $xfClass

  .xfPacking$xfClass.functions.parent configure \
    -command "
      global xfMisc
      if {\"\$xfMisc(packingMaster,$xfClass)\" != \".\"} {
        set xfLength \[.xfPacking$xfClass.frame2.children.widgets.widgets size\]
        set xfCounter 0
        set xfCurSelected 0
        while {\$xfCounter < \$xfLength} {
          if {\"\[winfo parent \$xfMisc(packingMaster,$xfClass)\]\" == \
              \"\[lindex \[.xfPacking$xfClass.frame2.children.widgets.widgets get \$xfCounter\] 1\]\"} {
            set xfCurSelected \$xfCounter
            break
          }
          incr xfCounter 1
        }
        .xfPacking$xfClass.frame2.children.widgets.widgets \
          selection clear 0 end
        .xfPacking$xfClass.frame2.children.widgets.widgets \
          selection set \$xfCurSelected
        if {\$xfCurSelected >= 0} {
          XFPackingSetPacking \$xfMisc(packingMaster,$xfClass) $xfClass
          XFPackingMakeMaster \[winfo parent \$xfMisc(packingMaster,$xfClass)\] \"$xfType\" $xfClass
        }
      }"

  .xfPacking$xfClass.functions.selchild configure \
    -command "
      global xfMisc
      set xfCurSelect \[.xfPacking$xfClass.frame2.children.childs.childs curselection\]
      if {\"\$xfCurSelect\" != \"\"} {
        set xfCurSelect \[lindex \[.xfPacking$xfClass.frame2.children.childs.childs get \$xfCurSelect\] 1\]
        set xfLength \[.xfPacking$xfClass.frame2.children.widgets.widgets size\]
        set xfCounter 0
        set xfCurSelected 0
        while {\$xfCounter < \$xfLength} {
          if {\"\$xfCurSelect\" == \
              \"\[lindex \[.xfPacking$xfClass.frame2.children.widgets.widgets get \$xfCounter\] 1\]\"} {
            set xfCurSelected \$xfCounter
            break
          }
          incr xfCounter 1
        }
        .xfPacking$xfClass.frame2.children.widgets.widgets \
          selection clear 0 end
        .xfPacking$xfClass.frame2.children.widgets.widgets \
          selection set \$xfCurSelected
        if {\$xfCurSelected >= 0} {
          XFPackingSetPacking \$xfMisc(packingMaster,$xfClass) $xfClass
          XFPackingMakeMaster \$xfCurSelect \"$xfType\" $xfClass
        }
      }"

  # bindings
  bind .xfPacking$xfClass.frame2.children.widgets.widgets $xfBind(select1) "
    global xfMisc
    XFBindSelectOne %W %y
    set xfCurSelected \
      \[.xfPacking$xfClass.frame2.children.widgets.widgets curselection\]
    if {\$xfCurSelected >= 0} {
      XFPackingSetPacking \$xfMisc(packingMaster,$xfClass) $xfClass
      XFPackingMakeMaster \[lindex \[.xfPacking$xfClass.frame2.children.widgets.widgets get \$xfCurSelected\] 1\] \"$xfType\" $xfClass
    }"
  bind .xfPacking$xfClass.frame2.children.childs.childs <ButtonPress-1> "
    XFBindSelectOne %W %y"
  bind .xfPacking$xfClass.frame2.children.widgets.widgets <Button1-Motion> "
    XFBindSelectOne %W %y"
  bind .xfPacking$xfClass.frame2.children.widgets.widgets <Shift-ButtonPress-1> "
    XFBindSelectOne %W %y"
  bind .xfPacking$xfClass.frame2.children.widgets.widgets <Shift-Button1-Motion> "
    XFBindSelectOne %W %y"

  bind .xfPacking$xfClass.frame2.children.childs.childs <ButtonPress-1> "
    XFBindSelectOne %W %y
    XFPackingGetPacking $xfW $xfClass"
  bind .xfPacking$xfClass.frame2.children.childs.childs <Button1-Motion> "
    NoFunction"
  bind .xfPacking$xfClass.frame2.children.childs.childs <Shift-ButtonPress-1> "
    XFBindSelectOne %W %y
    XFPackingGetPacking $xfW $xfClass"
  bind .xfPacking$xfClass.frame2.children.childs.childs <Shift-Button1-Motion> "
    NoFunction"
  
  # packing
  pack append .xfPacking$xfClass.frame1 \
              .xfPacking$xfClass.frame1.ok {left fill expand} \
              .xfPacking$xfClass.frame1.apply {left fill expand} \
              .xfPacking$xfClass.frame1.applyperm {left fill expand} \
              .xfPacking$xfClass.frame1.undo {left fill expand} \
              .xfPacking$xfClass.frame1.cancel {left fill expand}
  pack append .xfPacking$xfClass.functions \
              .xfPacking$xfClass.functions.parent {left fill expand} \
              .xfPacking$xfClass.functions.selchild {left fill expand} \
              .xfPacking$xfClass.functions.rescanmaster {left fill expand} \
              .xfPacking$xfClass.functions.child {left fill expand} \
              .xfPacking$xfClass.functions.unchild {left fill expand}
  pack append .xfPacking$xfClass.additional \
              .xfPacking$xfClass.additional.parameters {left fill expand} \
              .xfPacking$xfClass.additional.chldparameters {left fill expand} \
              .xfPacking$xfClass.additional.binding {left fill expand} \
              .xfPacking$xfClass.additional.chldbinding {left fill expand}
  pack append .xfPacking$xfClass.frame2.side \
              .xfPacking$xfClass.frame2.side.message2 {left} \
              .xfPacking$xfClass.frame2.side.top {left} \
              .xfPacking$xfClass.frame2.side.bottom {left} \
              .xfPacking$xfClass.frame2.side.left {left} \
              .xfPacking$xfClass.frame2.side.right {left}
  pack append .xfPacking$xfClass.frame2.chldframe.pad.pad1 \
              .xfPacking$xfClass.frame2.chldframe.pad.pad1.pad1 {left fillx expand}
  pack append .xfPacking$xfClass.frame2.chldframe.pad.pad2 \
              .xfPacking$xfClass.frame2.chldframe.pad.pad2.pad2 {left fillx expand}
  pack append .xfPacking$xfClass.frame2.chldframe.pad \
              .xfPacking$xfClass.frame2.chldframe.pad.message1 {left} \
              .xfPacking$xfClass.frame2.chldframe.pad.pad1 {left fillx expand} \
              .xfPacking$xfClass.frame2.chldframe.pad.pad2 {left fillx expand}
  pack append .xfPacking$xfClass.frame2.chldframe.ipad.pad1 \
              .xfPacking$xfClass.frame2.chldframe.ipad.pad1.pad1 {left fillx expand}
  pack append .xfPacking$xfClass.frame2.chldframe.ipad.pad2 \
              .xfPacking$xfClass.frame2.chldframe.ipad.pad2.pad2 {left fillx expand}
  pack append .xfPacking$xfClass.frame2.chldframe.ipad \
              .xfPacking$xfClass.frame2.chldframe.ipad.message1 {left} \
              .xfPacking$xfClass.frame2.chldframe.ipad.pad1 {left fillx expand} \
              .xfPacking$xfClass.frame2.chldframe.ipad.pad2 {left fillx expand}
  pack append .xfPacking$xfClass.frame2.chldframe \
              .xfPacking$xfClass.frame2.chldframe.message3 {top frame center} \
              .xfPacking$xfClass.frame2.chldframe.pad {top fillx} \
              .xfPacking$xfClass.frame2.chldframe.ipad {top fillx}
  pack append .xfPacking$xfClass.frame2.expand \
              .xfPacking$xfClass.frame2.expand.expand {bottom pady 6}
  pack append .xfPacking$xfClass.frame2.fill \
              .xfPacking$xfClass.frame2.fill.message4 {left} \
              .xfPacking$xfClass.frame2.fill.fillx {left} \
              .xfPacking$xfClass.frame2.fill.filly {left}
  pack append .xfPacking$xfClass.frame2.frame.f1 \
              .xfPacking$xfClass.frame2.frame.f1.nw {top fillx} \
              .xfPacking$xfClass.frame2.frame.f1.w {top fillx} \
              .xfPacking$xfClass.frame2.frame.f1.sw {top fillx}
  pack append .xfPacking$xfClass.frame2.frame.f2 \
              .xfPacking$xfClass.frame2.frame.f2.n {top fillx} \
              .xfPacking$xfClass.frame2.frame.f2.c {top fillx} \
              .xfPacking$xfClass.frame2.frame.f2.s {top fillx}
  pack append .xfPacking$xfClass.frame2.frame.f3 \
              .xfPacking$xfClass.frame2.frame.f3.ne {top fillx} \
              .xfPacking$xfClass.frame2.frame.f3.e {top fillx} \
              .xfPacking$xfClass.frame2.frame.f3.se {top fillx}
  pack append .xfPacking$xfClass.frame2.frame \
              .xfPacking$xfClass.frame2.frame.message5 {top frame center} \
              .xfPacking$xfClass.frame2.frame.f1 {left} \
              .xfPacking$xfClass.frame2.frame.f2 {left} \
              .xfPacking$xfClass.frame2.frame.f3 {left}
  pack append .xfPacking$xfClass.frame2.children \
              .xfPacking$xfClass.frame2.children.widgets {left fill expand} \
              .xfPacking$xfClass.frame2.children.childs {left fill expand}
  pack append .xfPacking$xfClass.frame2 \
              .xfPacking$xfClass.frame2.side {top pady 6} \
              .xfPacking$xfClass.frame2.expand {top fillx pady 6} \
              .xfPacking$xfClass.frame2.fill {top pady 6} \
              .xfPacking$xfClass.frame2.frame {top pady 6} \
              .xfPacking$xfClass.frame2.chldframe {top fillx pady 6} \
              .xfPacking$xfClass.frame2.children {top fill pady 6 expand}
  pack append .xfPacking$xfClass \
              .xfPacking$xfClass.frame1 {bottom fill} \
              .xfPacking$xfClass.additional {bottom fill} \
              .xfPacking$xfClass.functions {bottom fill} \
              .xfPacking$xfClass.message1 {top fillx} \
              .xfPacking$xfClass.frame2 {top fill expand}
  update
  XFEditSetStatus "Calling packing for $xfClass...done"
}

##########
# Procedure: XFPackingGetPacking
# Description: get packing for currently selected widget
# Arguments: xfW - the widget
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPackingGetPacking {xfW xfClass} {
  global xfMisc

  set xfCurSelected [.xfPacking$xfClass.frame2.children.childs.childs curselection]
  if {$xfCurSelected >= 0} {
    set xfGotSide 0
    set xfGotExpand 0
    set xfGotFillX 0
    set xfGotFillY 0
    set xfGotPadX 0
    set xfGotPadY 0
    set xfGotFrame 0
    set xfNextIs ""
    set xfPacking [lindex $xfMisc($xfClass,curPacking) $xfCurSelected]

    .xfPacking$xfClass.frame2.side.top configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.side.bottom configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.side.left configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.side.right configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.chldframe.pad.pad1.pad1 configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.chldframe.pad.pad2.pad2 configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.chldframe.ipad.pad1.pad1 configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.chldframe.ipad.pad2.pad2 configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.expand.expand configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.fill.fillx configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.fill.filly configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.frame.f1.nw configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.frame.f1.w configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.frame.f1.sw configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.frame.f2.n configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.frame.f2.c configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.frame.f2.s configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.frame.f3.ne configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.frame.f3.e configure \
      -command "NoFunction"

    .xfPacking$xfClass.frame2.frame.f3.se configure \
      -command "NoFunction"

    # pack info is of the form:
    # -in . -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 \
    #     -padx 0 -pady 0 -side left
    # First set the side.
    set len [llength $xfPacking]
    for {set i 0} {$i < $len} {incr i} {
      set field [lindex $xfPacking $i]
      case $field in {
	{-side} {
	  incr i
	  set value [lindex $xfPacking $i]
	  case $value in {
	    {top} {
	      .xfPacking$xfClass.frame2.side.top select
	      set xfGotSide 1
	    }
	    {bottom} {
	      .xfPacking$xfClass.frame2.side.bottom select
	      set xfGotSide 1
	    }
	    {left} {
	      .xfPacking$xfClass.frame2.side.left select
	      set xfGotSide 1
	    }
	    {right} {
	      .xfPacking$xfClass.frame2.side.right select
	      set xfGotSide 1
	    }
	  }
	}
	# Next set anchor.
	{-anchor} {
	  incr i
	  set value [lindex $xfPacking $i]
	  case $value in {
	    {nw} {
	      .xfPacking$xfClass.frame2.frame.f1.nw select
	    }
	    {w} {
	      .xfPacking$xfClass.frame2.frame.f1.w select
	    }
	    {sw} {
	      .xfPacking$xfClass.frame2.frame.f1.sw select
	    }
	    {n} {
	      .xfPacking$xfClass.frame2.frame.f2.n select
	    }
	    {c*} {
	      .xfPacking$xfClass.frame2.frame.f2.c select
	    }
	    {s} {
	      .xfPacking$xfClass.frame2.frame.f2.s select
	    }
	    {ne} {
	      .xfPacking$xfClass.frame2.frame.f3.ne select
	    }
	    {e} {
	      .xfPacking$xfClass.frame2.frame.f3.e select
	    }
	    {se} {
	      .xfPacking$xfClass.frame2.frame.f3.se select
	    }
	  }
	}
	# Next set fill.
	{-fill} {
	  incr i
	  set value [lindex $xfPacking $i]
	  case $value in {
	    {x} {
	      .xfPacking$xfClass.frame2.fill.fillx select
	      set xfGotFillX 1
	    }
	    {y} {
	      .xfPacking$xfClass.frame2.fill.filly select
	      set xfGotFillY 1
	    }
	    {both} {
	      .xfPacking$xfClass.frame2.fill.fillx select
	      .xfPacking$xfClass.frame2.fill.filly select
	      set xfGotFillX 1
	      set xfGotFillY 1
	    }
	  }
	}
	# Next set expand.
	{-expand} {
	  incr i
	  set value [lindex $xfPacking $i]
	  case $value in {
	    {1} {
	      .xfPacking$xfClass.frame2.expand.expand select
	      set xfGotExpand 1
	    }
	  } 
	}

	# Now set ipadx
	{-padx} {
	  incr i
	  set value [lindex $xfPacking $i]
	  catch ".xfPacking$xfClass.frame2.chldframe.pad.pad1.pad1 set $value"
	}

	{-ipadx} {
	  incr i
	  set value [lindex $xfPacking $i]
	  catch ".xfPacking$xfClass.frame2.chldframe.ipad.pad1.pad1 set $value"
	}
	{-pady} {
	  incr i
	  set value [lindex $xfPacking $i]
	  catch ".xfPacking$xfClass.frame2.chldframe.pad.pad2.pad2 set $value"
	}
	{-ipady} {
	  incr i
	  set value [lindex $xfPacking $i]
	  catch ".xfPacking$xfClass.frame2.chldframe.ipad.pad2.pad2 set $value"
	}
      }
    }

 
    if {$xfGotSide == 0} {
      .xfPacking$xfClass.frame2.side.top select
    }
    if {$xfGotExpand == 0} {
      .xfPacking$xfClass.frame2.expand.expand deselect
    }
    if {$xfGotFillX == 0} {
      .xfPacking$xfClass.frame2.fill.fillx deselect
    }
    if {$xfGotFillY == 0} {
      .xfPacking$xfClass.frame2.fill.filly deselect
    }
    if {$xfGotPadX == 0} {
      .xfPacking$xfClass.frame2.chldframe.pad.pad1.pad1 set 0
    }
    if {$xfGotPadY == 0} {
      .xfPacking$xfClass.frame2.chldframe.pad.pad2.pad2 set 0
    }
    XFMiscFlash [lindex [.xfPacking$xfClass.frame2.children.childs.childs get $xfCurSelected] 1]

    .xfPacking$xfClass.frame2.side.top configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.side.bottom configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.side.left configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.side.right configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.chldframe.pad.pad1.pad1 configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.chldframe.pad.pad2.pad2 configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.chldframe.ipad.pad1.pad1 configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.chldframe.ipad.pad2.pad2 configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.expand.expand configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.fill.fillx configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.fill.filly configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.frame.f1.nw configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.frame.f1.w configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.frame.f1.sw configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.frame.f2.n configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.frame.f2.c configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.frame.f2.s configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.frame.f3.ne configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.frame.f3.e configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"

    .xfPacking$xfClass.frame2.frame.f3.se configure \
      -command "XFPackingUpdatePacking $xfW $xfClass 1"
  }
}

##########
# Procedure: XFPackingMakeChild
# Description: make current selection a packed widget
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPackingMakeChild {xfW xfClass} {
  global xfMisc

  set xfCurSelected [.xfPacking$xfClass.frame2.children.widgets.widgets curselection]
  if {$xfCurSelected >= 0} {
    set xfCurrent [lindex [.xfPacking$xfClass.frame2.children.widgets.widgets get $xfCurSelected] 1]
    set xfTmpVal [string trimright [string range $xfW 0 [string length [winfo parent $xfCurrent]]] .]
    if {"$xfTmpVal" == ""} {
      set xfTmpVal .
    }
    set xfMisc($xfClass,curPacking) \
      [lappend $xfMisc($xfClass,curPacking) top]
    if {"$xfCurrent" != "" && "[winfo parent $xfCurrent]" == "$xfTmpVal" &&
        "$xfW" != "$xfCurrent"} {
      catch "place forget $xfCurrent"
      pack append $xfW $xfCurrent {top}
    }
  }
}

##########
# Procedure: XFPackingMakeMaster
# Description: make current selection new master
# Arguments: xfW - the widget we configure
#            xfType - the type of configuration (add, config)
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPackingMakeMaster {xfW xfType xfClass} {
  global xfMisc

  set xfMisc(packingMaster,$xfClass) $xfW
  wm title .xfPacking$xfClass "XF packing:[XFMiscPathTail $xfW]"

  .xfPacking$xfClass.frame1.ok configure \
    -command "
      XFPackingSetPacking $xfW $xfClass
      destroy .xfPacking$xfClass"

  .xfPacking$xfClass.frame1.apply configure \
    -command "XFPackingSetPacking $xfW $xfClass"

  .xfPacking$xfClass.frame1.applyperm configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame1.undo configure \
    -command "XFPackingUndoPacking $xfW $xfClass"

  .xfPacking$xfClass.frame1.cancel configure \
    -command "
      XFPackingUndoPacking $xfW $xfClass
      destroy .xfPacking$xfClass"

  .xfPacking$xfClass.message1 configure \
    -text "Packing:$xfW"

  .xfPacking$xfClass.additional.parameters configure \
    -command "XFProcConfParametersSmall $xfW .xfPacking$xfClass"

  .xfPacking$xfClass.additional.chldparameters configure \
    -command "
      set xfCurSelected \
        \[.xfPacking$xfClass.frame2.children.childs.childs curselection\]
      if {\$xfCurSelected >= 0} {
        set xfChld \[lindex \[.xfPacking$xfClass.frame2.children.childs.childs get \$xfCurSelected\] 1\]
        XFProcConfParametersSmall \$xfChld .xfPacking$xfClass
      }"

  .xfPacking$xfClass.additional.binding configure \
    -command "XFProcConfBinding $xfW .xfPacking$xfClass"

  .xfPacking$xfClass.additional.chldbinding configure \
    -command "
      set xfCurSelected \
        \[.xfPacking$xfClass.frame2.children.childs.childs curselection\]
      if {\$xfCurSelected >= 0} {
        set xfChld \[lindex \[.xfPacking$xfClass.frame2.children.childs.childs get \$xfCurSelected\] 1\]
        XFProcConfBinding \$xfChld .xfPacking$xfClass
      }"

  .xfPacking$xfClass.functions.rescanmaster configure \
    -command "
      XFMiscReadTree $xfW .xfPacking$xfClass.frame2.children.widgets.widgets all
      XFPackingReadPacking $xfW $xfClass"

  .xfPacking$xfClass.functions.child configure \
    -command "
      XFPackingMakeChild $xfW $xfClass
      XFPackingReadPacking $xfW $xfClass"

  .xfPacking$xfClass.functions.unchild configure \
    -command "
      XFPackingRemoveChild $xfW $xfClass
      XFPackingReadPacking $xfW $xfClass"

  .xfPacking$xfClass.frame2.side.top configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.side.bottom configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.side.left configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.side.right configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.chldframe.pad.pad1.pad1 configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.chldframe.pad.pad2.pad2 configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.expand.expand configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.fill.fillx configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.fill.filly configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.frame.f1.nw configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.frame.f1.w configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.frame.f1.sw configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.frame.f2.n configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.frame.f2.c configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.frame.f2.s configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.frame.f3.ne configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.frame.f3.e configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.frame.f3.se configure \
    -command "XFPackingUpdatePacking $xfW $xfClass 1"

  .xfPacking$xfClass.frame2.children.childs.mover configure \
    -command "
      XFPackingRepositionChild $xfClass
      XFPackingUpdatePacking $xfW $xfClass 1"

  bind .xfPacking$xfClass.frame2.children.childs.childs <Shift-ButtonPress-1> "
    XFBindSelectOne %W %y
    XFPackingGetPacking $xfW $xfClass"

  bind .xfPacking$xfClass.frame2.children.childs.childs <ButtonPress-1> "
    XFBindSelectOne %W %y
    XFPackingGetPacking $xfW $xfClass"
  bind .xfPacking$xfClass.frame2.children.childs.childs <Shift-ButtonPress-1> "
    XFBindSelectOne %W %y
    XFPackingGetPacking $xfW $xfClass"

	
  # read new packing into xfMisc(current) variables
  XFPackingReadPacking $xfW $xfClass

  # save the current packing before editing for restore.
  set xfMisc(${xfClass},savePackChilds) ""
  set xfMisc(${xfClass},savePacking) ""

  # save the children.
  set xfPackList [pack slave $xfW]
  foreach xfCounter $xfPackList {
    lappend xfMisc(${xfClass},savePackChilds) $xfCounter
  }
  # save the children pack information.
  foreach xfCounter $xfPackList {
    set xfPackData [lrange [pack info $xfCounter] 0 end]
    lappend xfMisc(${xfClass},savePacking) $xfPackData
  }
}

##########
# Procedure: XFPackingReadPacking
# Description: read packing for the current widget.  Updates the
#             xfMisc(
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPackingReadPacking {xfW xfClass} {
  global xfMisc
  global xfStatus

  XFMiscClearList .xfPacking$xfClass.frame2.children.childs.childs
  set xfMisc($xfClass,curPacking) ""

  foreach xfCounter [pack slave $xfW] {
    .xfPacking$xfClass.frame2.children.childs.childs insert end "<[winfo class $xfCounter]> $xfCounter"
    set xfPackData [lrange [pack info $xfCounter] 0 end]
    lappend xfMisc(${xfClass},curPacking) $xfPackData
  }
  if {[llength $xfMisc($xfClass,curPacking)] > 0} {
    .xfPacking$xfClass.frame2.children.childs.childs selection clear 0 end
    .xfPacking$xfClass.frame2.children.childs.childs selection set 0
    XFPackingGetPacking $xfW $xfClass
    .xfPacking$xfClass.frame2.children.childs.mover configure \
      -to [llength $xfMisc($xfClass,curPacking)]
    set xfStatus(firstPacking) 0
    XFPackingUpdatePacking $xfW $xfClass 1
  } {
    set xfStatus(firstPacking) 0
    .xfPacking$xfClass.frame2.children.childs.mover configure \
      -to 1
  }
}

##########
# Procedure: XFPackingRemoveChild
# Description: unpack the currently selected widget
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPackingRemoveChild {xfW xfClass} {
  global xfMisc

  set xfCurSelected [.xfPacking$xfClass.frame2.children.childs.childs curselection]
  if {$xfCurSelected >= 0} {
    set xfCurrent [lindex [.xfPacking$xfClass.frame2.children.childs.childs get $xfCurSelected] 1]
    set xfTmpVal [string trimright [string range $xfW 0 [string length [winfo parent $xfCurrent]]] .]
    if {"$xfTmpVal" == ""} {
      set xfTmpVal .
    }
    set xfMisc($xfClass,curPacking) \
      [lreplace $xfMisc($xfClass,curPacking) $xfCurSelected $xfCurSelected]
    if {"$xfCurrent" != "" && "[winfo parent $xfCurrent]" == "$xfTmpVal"} {
      catch "place forget $xfCurrent"
    }
  }
}

##########
# Procedure: XFPackingRepositionChild
# Description: put currently selected child to new position
# Arguments: xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPackingRepositionChild {xfClass} {
  global xfMisc

  set xfCurSelected [.xfPacking$xfClass.frame2.children.childs.childs curselection]
  if {$xfCurSelected >= 0} {
    set xfNewPos [.xfPacking$xfClass.frame2.children.childs.mover get]
    set xfTmpBuff1 [.xfPacking$xfClass.frame2.children.childs.childs get $xfCurSelected]
    set xfTmpBuff2 [lindex $xfMisc($xfClass,curPacking) $xfCurSelected]
    .xfPacking$xfClass.frame2.children.childs.childs delete $xfCurSelected
    set xfMisc($xfClass,curPacking) \
      [lreplace $xfMisc($xfClass,curPacking) $xfCurSelected $xfCurSelected]
    .xfPacking$xfClass.frame2.children.childs.childs insert $xfNewPos $xfTmpBuff1
    set xfMisc($xfClass,curPacking) \
      [linsert $xfMisc($xfClass,curPacking) $xfNewPos $xfTmpBuff2]
    .xfPacking$xfClass.frame2.children.childs.childs selection clear 0 end
    .xfPacking$xfClass.frame2.children.childs.childs selection set $xfNewPos
  }
}
  
##########
# Procedure: XFPackingSetPacking
# Description: set packing for currently selected widget
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPackingSetPacking {xfW xfClass} {
  global xfMisc

  set xfCounter 0
  if {[catch "pack slave $xfW" xfPackList ]} {
    return
  }
  if {"$xfPackList" != ""} {
    if {[catch "pack forget $xfPackList" xfResult]} {
      XFProcError "$xfResult"
    }
  }
	
  while {$xfCounter < [llength $xfMisc($xfClass,curPacking)]} {
    set xfPackString "pack "
    append xfPackString [lindex [.xfPacking$xfClass.frame2.children.childs.childs get $xfCounter] 1]
    append xfPackString " "
    append xfPackString [lindex $xfMisc($xfClass,curPacking) $xfCounter]
    incr xfCounter 1
    if {[catch "$xfPackString" xfResult]} {
      XFProcError "$xfResult"
    }
  }
}

##########
# Procedure: XFPackingUndoPacking
# Description: undo packing for widgets
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPackingUndoPacking {xfW xfClass} {
  global xfMisc

  set xfPackList [pack slave $xfW]
  if {"$xfPackList" != ""} {
    if {[catch "pack forget $xfPackList" xfResult]} {
      XFProcError "$xfResult"
    }
  }
  if {[llength $xfMisc(${xfClass},savePackChilds)] > 0} {
    set xfCounter 0
    while {$xfCounter < [llength $xfMisc($xfClass,savePackChilds)]} {
      set xfPackString "pack "
      append xfPackString [lindex $xfMisc($xfClass,savePackChilds) $xfCounter]
      append xfPackString " "
      append xfPackString [lindex $xfMisc($xfClass,savePacking) $xfCounter]
      append xfPackString " "
      if {[catch "$xfPackString" xfResult]} {
	XFProcError "$xfResult"
      }
      incr xfCounter 1
    }
  }
}

##########
# Procedure: XFPackingUpdatePacking
# Description: update packing for widget
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
#            xfUpdatePos - update packing
#            xfParam1 - ignored parameter
# Returns: none
# Sideeffects: none
##########
proc XFPackingUpdatePacking {xfW xfClass {xfUpdatePos 1} {xfParam1 ""}} {
  global xfConf
  global xfMisc
  global xfStatus

  set xfApplyPacking [set xfConf(applyPacking)]
  set xfExpand [set xfMisc(${xfClass},expand)]
  set xfFillX [set xfMisc(${xfClass},fillX)]
  set xfFillY [set xfMisc(${xfClass},fillY)]
  set xfFrame [set xfMisc(${xfClass},frame)]
  set xfSide [set xfMisc(${xfClass},side)]

  if {$xfStatus(firstPacking)} {
    return
  }

  # Now we mimic the pack info command.
  # pack info is of the form:
  # -in . -anchor center -expand 0 -fill none -ipadx 0 -ipady 0 \
  #     -padx 0 -pady 0 -side left
	
  set xfNewPacking "-in $xfW"
  set xfCurSelected [.xfPacking$xfClass.frame2.children.childs.childs curselection]
  if {$xfCurSelected >= 0} {
    append xfNewPacking " -anchor $xfFrame"
    if {$xfExpand == 1} {
      append xfNewPacking " -expand 1"
    } else {
      append xfNewPacking " -expand 0"
    }
    if {$xfFillX == 1 && $xfFillY == 1} {
      append xfNewPacking " -fill both"
    } {
      if {$xfFillX == 1} {
        append xfNewPacking " -fill x"
      }
      if {$xfFillY == 1} {
        append xfNewPacking " -fill y"
      }
    }
    if {[.xfPacking$xfClass.frame2.chldframe.pad.pad1.pad1 get] > 0} {
      append xfNewPacking " -padx [.xfPacking$xfClass.frame2.chldframe.pad.pad1.pad1 get]"
    }
    if {[.xfPacking$xfClass.frame2.chldframe.ipad.pad1.pad1 get] > 0} {
      append xfNewPacking " -ipadx [.xfPacking$xfClass.frame2.chldframe.ipad.pad1.pad1 get]"
    }
    if {[.xfPacking$xfClass.frame2.chldframe.pad.pad2.pad2 get] > 0} {
      append xfNewPacking " -pady [.xfPacking$xfClass.frame2.chldframe.pad.pad2.pad2 get]"
    }
    if {[.xfPacking$xfClass.frame2.chldframe.ipad.pad2.pad2 get] > 0} {
      append xfNewPacking " -ipady [.xfPacking$xfClass.frame2.chldframe.ipad.pad2.pad2 get]"
    }
    append xfNewPacking " -side $xfSide"

    set xfMisc($xfClass,curPacking) \
      [lreplace $xfMisc($xfClass,curPacking) $xfCurSelected $xfCurSelected $xfNewPacking]

    if {$xfConf(applyPacking) && $xfUpdatePos} {
      XFPackingSetPacking $xfW $xfClass
    }
  }
}

# eof

