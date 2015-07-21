# Program: xf
# Description: specify placing for widget
#
# $Header: xfplacing.tcl[2.3] Wed Mar 10 12:07:40 1993 garfield@garfield frozen $

##########
# Procedure: XFPlacing
# Description: specify placing for widget
# Arguments: xfW - the widget we configure
#            xfType - the type of configuration (add, config)
#            xfClass - the class we currently configure
#                      (if master changes this does not neccessary
#                       reflect the class of the new master...)
#            xfLeader - the leading window
# Returns: none
# Sideeffects: none
##########
proc XFPlacing {xfW xfType xfClass {xfLeader ""}} {
  global xfBind
  global xfMisc
  global xfStatus

  set xfMisc(${xfClass},anchor) nw
  set xfMisc(${xfClass},border) inside
  set xfMisc(${xfClass},curPlacing) ""
  set xfMisc(${xfClass},savePlaceChilds) ""
  set xfMisc(${xfClass},savePlacing) ""

  set xfStatus(firstPlacing) 1
  XFEditSetStatus "Calling placing for $xfClass..."

  # build widget structure
  XFTmpltToplevel .xfPlacing$xfClass 500x510 \
   "XF placing:[XFMiscPathTail $xfW]" $xfLeader

  XFTmpltFrame .xfPlacing$xfClass.frame1 0

  button .xfPlacing$xfClass.frame1.ok \
    -text {OK}

  button .xfPlacing$xfClass.frame1.apply \
    -text {Apply}

  checkbutton .xfPlacing$xfClass.frame1.applyperm \
    -text {Apply permanently} \
    -variable xfConf(applyPlacing) \
    -onvalue 1 \
    -offvalue 0

  button .xfPlacing$xfClass.frame1.undo \
    -text {Undo}

  button .xfPlacing$xfClass.frame1.cancel \
    -text {Cancel}

  label .xfPlacing$xfClass.message1 \
    -relief raised \
    -anchor c

  XFTmpltFrame .xfPlacing$xfClass.additional 0

  button .xfPlacing$xfClass.additional.parameters \
    -text {Parameters}

  button .xfPlacing$xfClass.additional.chldparameters \
    -text {Child parameters}

  button .xfPlacing$xfClass.additional.binding \
    -text {Binding}

  button .xfPlacing$xfClass.additional.chldbinding \
    -text {Child binding}

  XFTmpltFrame .xfPlacing$xfClass.functions 0

  button .xfPlacing$xfClass.functions.master \
    -text {Make master}

  button .xfPlacing$xfClass.functions.rescanmaster \
    -text {Rescan widgets}

  button .xfPlacing$xfClass.functions.child \
    -text {Place child}

  button .xfPlacing$xfClass.functions.delchild \
    -text {Forget child}

  XFTmpltFrame .xfPlacing$xfClass.frame2

  XFTmpltFrame .xfPlacing$xfClass.frame2.geo 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.geo.geobutt 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.geo.geobutt.width 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.geo.geobutt.height 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.pos 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.pos.posbutt 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.pos.posbutt.x 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.pos.posbutt.y 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.anchor 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.anchor.f1 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.anchor.f2 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.anchor.f3 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.border 0

  XFTmpltFrame .xfPlacing$xfClass.frame2.children 0

  label .xfPlacing$xfClass.frame2.geo.message1 \
    -anchor e \
    -width $xfStatus(elementWidth) \
    -text {Geometry:}

  scale .xfPlacing$xfClass.frame2.geo.geo1 \
    -from 0 \
    -label "Width" \
    -orient horizontal \
    -relief sunken \
    -sliderlength 15 \
    -to 600 \
    -width 8

  scale .xfPlacing$xfClass.frame2.geo.geo2 \
    -from 0 \
    -label "Height" \
    -orient horizontal \
    -relief sunken \
    -sliderlength 15 \
    -to 600 \
    -width 8

  label .xfPlacing$xfClass.frame2.geo.geobutt.message1 \
    -anchor e \
    -width $xfStatus(elementWidth) \
    -text {}

  radiobutton .xfPlacing$xfClass.frame2.geo.geobutt.width.abs \
    -value 1 \
    -text {Absolute} \
    -variable xfMisc(layoutWidth) \
    -command "XFPlacingToggleScale $xfClass width"

  radiobutton .xfPlacing$xfClass.frame2.geo.geobutt.width.rel \
    -value 0 \
    -text {Relative} \
    -variable xfMisc(layoutWidth) \
    -command "XFPlacingToggleScale $xfClass width"

  radiobutton .xfPlacing$xfClass.frame2.geo.geobutt.height.abs \
    -value 1 \
    -text {Absolute} \
    -variable xfMisc(layoutHeight) \
    -command "XFPlacingToggleScale $xfClass height"

  radiobutton .xfPlacing$xfClass.frame2.geo.geobutt.height.rel \
    -value 0 \
    -text {Relative} \
    -variable xfMisc(layoutHeight) \
    -command "XFPlacingToggleScale $xfClass height"

  label .xfPlacing$xfClass.frame2.pos.message1 \
    -anchor e \
    -width $xfStatus(elementWidth) \
    -text {Position:}

  scale .xfPlacing$xfClass.frame2.pos.pos1 \
    -from 0 \
    -label "X" \
    -orient horizontal \
    -relief sunken \
    -sliderlength 15 \
    -to 600 \
    -width 8

  scale .xfPlacing$xfClass.frame2.pos.pos2 \
    -from 0 \
    -label "Y" \
    -orient horizontal \
    -relief sunken \
    -sliderlength 15 \
    -to 600 \
    -width 8

  label .xfPlacing$xfClass.frame2.pos.posbutt.message1 \
    -anchor e \
    -width $xfStatus(elementWidth) \
    -text {}

  radiobutton .xfPlacing$xfClass.frame2.pos.posbutt.x.abs \
    -value 1 \
    -text {Absolute} \
    -variable xfMisc(layoutX) \
    -command "XFPlacingToggleScale $xfClass x"

  radiobutton .xfPlacing$xfClass.frame2.pos.posbutt.x.rel \
    -value 0 \
    -text {Relative} \
    -variable xfMisc(layoutX) \
    -command "XFPlacingToggleScale $xfClass x"

  radiobutton .xfPlacing$xfClass.frame2.pos.posbutt.y.abs \
    -value 1 \
    -text {Absolute} \
    -variable xfMisc(layoutY) \
    -command "XFPlacingToggleScale $xfClass y"

  radiobutton .xfPlacing$xfClass.frame2.pos.posbutt.y.rel \
    -value 0 \
    -text {Relative} \
    -variable xfMisc(layoutY) \
    -command "XFPlacingToggleScale $xfClass y"

  label .xfPlacing$xfClass.frame2.anchor.message5 \
    -text {Anchor:}

  radiobutton .xfPlacing$xfClass.frame2.anchor.f1.nw \
    -anchor w \
    -value nw \
    -text {NW} \
    -variable xfMisc(${xfClass},anchor)

  radiobutton .xfPlacing$xfClass.frame2.anchor.f1.w \
    -anchor w \
    -value w \
    -text {W} \
    -variable xfMisc(${xfClass},anchor)

  radiobutton .xfPlacing$xfClass.frame2.anchor.f1.sw \
    -anchor w \
    -value sw \
    -text {SW} \
    -variable xfMisc(${xfClass},anchor)

  radiobutton .xfPlacing$xfClass.frame2.anchor.f2.n \
    -anchor w \
    -value n \
    -text {N} \
    -variable xfMisc(${xfClass},anchor)

  radiobutton .xfPlacing$xfClass.frame2.anchor.f2.c \
    -anchor w \
    -value center \
    -text {C} \
    -variable xfMisc(${xfClass},anchor)

  radiobutton .xfPlacing$xfClass.frame2.anchor.f2.s \
    -anchor w \
    -value s \
    -text {S} \
    -variable xfMisc(${xfClass},anchor)

  radiobutton .xfPlacing$xfClass.frame2.anchor.f3.ne \
    -anchor w \
    -value ne \
    -text {NE} \
    -variable xfMisc(${xfClass},anchor)

  radiobutton .xfPlacing$xfClass.frame2.anchor.f3.e \
    -anchor w \
    -value e \
    -text {E} \
    -variable xfMisc(${xfClass},anchor)

  radiobutton .xfPlacing$xfClass.frame2.anchor.f3.se \
    -anchor w \
    -value se \
    -text {SE} \
    -variable xfMisc(${xfClass},anchor)
  .xfPlacing$xfClass.frame2.anchor.f2.c select

  label .xfPlacing$xfClass.frame2.border.message2 \
    -text {Border mode:}

  radiobutton .xfPlacing$xfClass.frame2.border.inside \
    -anchor c \
    -text {Inside} \
    -value inside \
    -variable xfMisc(${xfClass},border)

  radiobutton .xfPlacing$xfClass.frame2.border.outside \
    -anchor c \
    -text {Outside} \
    -value outside \
    -variable xfMisc(${xfClass},border)

  radiobutton .xfPlacing$xfClass.frame2.border.ignore \
    -anchor c \
    -text {Ignore} \
    -value ignore \
    -variable xfMisc(${xfClass},border)

  XFTmpltListbox .xfPlacing$xfClass.frame2.children widgets

  label .xfPlacing$xfClass.frame2.children.widgets.message6 \
    -text {Masters:}
  pack before .xfPlacing$xfClass.frame2.children.widgets.vscroll \
              .xfPlacing$xfClass.frame2.children.widgets.message6 {top fillx}  

  XFTmpltListbox .xfPlacing$xfClass.frame2.children childs

  label .xfPlacing$xfClass.frame2.children.childs.message6 \
    -text {Slaves:}
  pack before .xfPlacing$xfClass.frame2.children.childs.vscroll \
              .xfPlacing$xfClass.frame2.children.childs.message6 {top fillx}

  XFMiscReadTree $xfW .xfPlacing$xfClass.frame2.children.widgets.widgets all

  XFPlacingMakeMaster $xfW $xfType $xfClass

  .xfPlacing$xfClass.functions.master configure \
    -command "
      global xfMisc
      set xfCurSelected \
        \[.xfPlacing$xfClass.frame2.children.widgets.widgets curselection\]
      if {\$xfCurSelected >= 0} {
        XFPlacingSetPlacing \$xfMisc(placingMaster,$xfClass) $xfClass
        XFPlacingMakeMaster \[lindex \[.xfPlacing$xfClass.frame2.children.widgets.widgets get \$xfCurSelected\] 1\] \"$xfType\" $xfClass
      }"

  # bindings
  bind .xfPlacing$xfClass.frame2.children.widgets.widgets $xfBind(select1) "
    global xfMisc
    XFBindSelectOne %W %y
    set xfCurSelected \
      \[.xfPlacing$xfClass.frame2.children.widgets.widgets curselection\]
    if {\$xfCurSelected >= 0} {
      XFPlacingSetPlacing \$xfMisc(placingMaster,$xfClass) $xfClass
      XFPlacingMakeMaster \[lindex \[.xfPlacing$xfClass.frame2.children.widgets.widgets get \$xfCurSelected\] 1\] \"$xfType\" $xfClass
    }"

  bind .xfPlacing$xfClass.frame2.children.widgets.widgets <ButtonPress-1> "
    XFBindSelectOne %W %y"
  bind .xfPlacing$xfClass.frame2.children.widgets.widgets <Button1-Motion> "
    XFBindSelectOne %W %y"
  bind .xfPlacing$xfClass.frame2.children.widgets.widgets <Shift-ButtonPress-1> "
    XFBindSelectOne %W %y"
  bind .xfPlacing$xfClass.frame2.children.widgets.widgets <Shift-Button1-Motion> "
    XFBindSelectOne %W %y"
  
  bind .xfPlacing$xfClass.frame2.children.childs.childs <ButtonPress-1> "
    XFBindSelectOne %W %y
    XFPlacingGetPlacing $xfW $xfClass"
  bind .xfPlacing$xfClass.frame2.children.childs.childs <Button1-Motion> "
    NoFunction"
  bind .xfPlacing$xfClass.frame2.children.childs.childs <Shift-ButtonPress-1> "
    XFBindSelectOne %W %y
    XFPlacingGetPlacing $xfW $xfClass"
  bind .xfPlacing$xfClass.frame2.children.childs.childs <Shift-Button1-Motion> "
    NoFunction"

  # packing  
  pack append .xfPlacing$xfClass.frame1 \
              .xfPlacing$xfClass.frame1.ok {left fill expand} \
              .xfPlacing$xfClass.frame1.apply {left fill expand} \
              .xfPlacing$xfClass.frame1.applyperm {left fill expand} \
              .xfPlacing$xfClass.frame1.undo {left fill expand} \
              .xfPlacing$xfClass.frame1.cancel {left fill expand}
  pack append .xfPlacing$xfClass.functions \
              .xfPlacing$xfClass.functions.master {left fill expand} \
              .xfPlacing$xfClass.functions.rescanmaster {left fill expand} \
              .xfPlacing$xfClass.functions.child {left fill expand} \
              .xfPlacing$xfClass.functions.delchild {left fill expand}
  pack append .xfPlacing$xfClass.additional \
              .xfPlacing$xfClass.additional.parameters {left fill expand} \
              .xfPlacing$xfClass.additional.chldparameters {left fill expand} \
              .xfPlacing$xfClass.additional.binding {left fill expand} \
              .xfPlacing$xfClass.additional.chldbinding {left fill expand}
  pack append .xfPlacing$xfClass.frame2.geo.geobutt.width \
              .xfPlacing$xfClass.frame2.geo.geobutt.width.abs {left fillx expand} \
              .xfPlacing$xfClass.frame2.geo.geobutt.width.rel {left fillx expand}
  pack append .xfPlacing$xfClass.frame2.geo.geobutt.height \
              .xfPlacing$xfClass.frame2.geo.geobutt.height.abs {left fillx expand} \
              .xfPlacing$xfClass.frame2.geo.geobutt.height.rel {left fillx expand}
  pack append .xfPlacing$xfClass.frame2.geo.geobutt \
              .xfPlacing$xfClass.frame2.geo.geobutt.message1 {left} \
              .xfPlacing$xfClass.frame2.geo.geobutt.width {left fillx expand} \
              .xfPlacing$xfClass.frame2.geo.geobutt.height {left fillx expand}
  pack append .xfPlacing$xfClass.frame2.geo \
              .xfPlacing$xfClass.frame2.geo.geobutt {bottom fillx} \
              .xfPlacing$xfClass.frame2.geo.message1 {left} \
              .xfPlacing$xfClass.frame2.geo.geo1 {left fillx expand} \
              .xfPlacing$xfClass.frame2.geo.geo2 {left fillx expand}
  pack append .xfPlacing$xfClass.frame2.pos.posbutt.x \
              .xfPlacing$xfClass.frame2.pos.posbutt.x.abs {left fillx expand} \
              .xfPlacing$xfClass.frame2.pos.posbutt.x.rel {left fillx expand}
  pack append .xfPlacing$xfClass.frame2.pos.posbutt.y \
              .xfPlacing$xfClass.frame2.pos.posbutt.y.abs {left fillx expand} \
              .xfPlacing$xfClass.frame2.pos.posbutt.y.rel {left fillx expand}
  pack append .xfPlacing$xfClass.frame2.pos.posbutt \
              .xfPlacing$xfClass.frame2.pos.posbutt.message1 {left} \
              .xfPlacing$xfClass.frame2.pos.posbutt.x {left fillx expand} \
              .xfPlacing$xfClass.frame2.pos.posbutt.y {left fillx expand}
  pack append .xfPlacing$xfClass.frame2.pos \
              .xfPlacing$xfClass.frame2.pos.posbutt {bottom fillx} \
              .xfPlacing$xfClass.frame2.pos.message1 {left} \
              .xfPlacing$xfClass.frame2.pos.pos1 {left fillx expand} \
              .xfPlacing$xfClass.frame2.pos.pos2 {left fillx expand}
  pack append .xfPlacing$xfClass.frame2.border \
              .xfPlacing$xfClass.frame2.border.message2 {left} \
              .xfPlacing$xfClass.frame2.border.inside {left} \
              .xfPlacing$xfClass.frame2.border.outside {left} \
              .xfPlacing$xfClass.frame2.border.ignore {left}
  pack append .xfPlacing$xfClass.frame2.anchor.f1 \
              .xfPlacing$xfClass.frame2.anchor.f1.nw {top fillx} \
              .xfPlacing$xfClass.frame2.anchor.f1.w {top fillx} \
              .xfPlacing$xfClass.frame2.anchor.f1.sw {top fillx}
  pack append .xfPlacing$xfClass.frame2.anchor.f2 \
              .xfPlacing$xfClass.frame2.anchor.f2.n {top fillx} \
              .xfPlacing$xfClass.frame2.anchor.f2.c {top fillx} \
              .xfPlacing$xfClass.frame2.anchor.f2.s {top fillx}
  pack append .xfPlacing$xfClass.frame2.anchor.f3 \
              .xfPlacing$xfClass.frame2.anchor.f3.ne {top fillx} \
              .xfPlacing$xfClass.frame2.anchor.f3.e {top fillx} \
              .xfPlacing$xfClass.frame2.anchor.f3.se {top fillx}
  pack append .xfPlacing$xfClass.frame2.anchor \
              .xfPlacing$xfClass.frame2.anchor.message5 {top frame center} \
              .xfPlacing$xfClass.frame2.anchor.f1 {left} \
              .xfPlacing$xfClass.frame2.anchor.f2 {left} \
              .xfPlacing$xfClass.frame2.anchor.f3 {left}
  pack append .xfPlacing$xfClass.frame2.children \
              .xfPlacing$xfClass.frame2.children.widgets {left fill expand} \
              .xfPlacing$xfClass.frame2.children.childs {left fill expand}
  pack append .xfPlacing$xfClass.frame2 \
              .xfPlacing$xfClass.frame2.geo {top fillx pady 6} \
              .xfPlacing$xfClass.frame2.pos {top fillx pady 6} \
              .xfPlacing$xfClass.frame2.anchor {top pady 6} \
              .xfPlacing$xfClass.frame2.border {top pady 6} \
              .xfPlacing$xfClass.frame2.children {top fill pady 6 expand}
  pack append .xfPlacing$xfClass \
              .xfPlacing$xfClass.frame1 {bottom fill} \
              .xfPlacing$xfClass.additional {bottom fill} \
              .xfPlacing$xfClass.functions {bottom fill} \
              .xfPlacing$xfClass.message1 {top fillx} \
              .xfPlacing$xfClass.frame2 {top fill expand}
  XFEditSetStatus "Calling placing for $xfClass...done"
}

##########
# Procedure: XFPlacingGetPlacing
# Description: get placing for currently selected widget
# Arguments: xfW - the widget
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPlacingGetPlacing {xfW xfClass} {
  global xfMisc

  set xfCurSelected [.xfPlacing$xfClass.frame2.children.childs.childs curselection]
  if {$xfCurSelected >= 0} {
    set xfGotIn 0
    set xfGotX 0
    set xfGotY 0
    set xfGotRelX 0
    set xfGotRelY 0
    set xfGotAnchor 0
    set xfGotWidth 0
    set xfGotHeight 0
    set xfGotRelWidth 0
    set xfGotRelHeight 0
    set xfGotBorder 0
    set xfNextIs ""
    set xfPlacing [lindex $xfMisc($xfClass,curPlacing) $xfCurSelected]

    .xfPlacing$xfClass.frame2.geo.geo1 configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.geo.geo2 configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.pos.pos1 configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.pos.pos2 configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.anchor.f1.nw configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.anchor.f1.w configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.anchor.f1.sw configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.anchor.f2.n configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.anchor.f2.c configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.anchor.f2.s configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.anchor.f3.ne configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.anchor.f3.e configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.anchor.f3.se configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.border.inside configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.border.outside configure \
      -command "NoFunction"

    .xfPlacing$xfClass.frame2.border.ignore configure \
      -command "NoFunction"

    foreach xfCounter $xfPlacing {
      case $xfCounter in {
        {-in} {
          set xfNextIs in
          set xfGotIn 1
        }
        {-x} {
          set xfNextIs x
          set xfGotX 1
        }
        {-y} {
          set xfNextIs y
          set xfGotY 1
        }
        {-relx} {
          set xfNextIs relx
          set xfGotRelX 1
        }
        {-rely} {
          set xfNextIs rely
          set xfGotRelY 1
        }
        {-anchor} {
          set xfNextIs anchor
          set xfGotAnchor 1
        }
        {-width} {
          set xfNextIs width
          set xfGotWidth 1
        }
        {-height} {
          set xfNextIs height
          set xfGotHeight 1
        }
        {-relwidth} {
          set xfNextIs relwidth
          set xfGotRelWidth 1
        }
        {-relheight} {
          set xfNextIs relheight
          set xfGotRelHeight 1
        }
        {-bordermode} {
          set xfNextIs border
          set xfGotBorder 1
        }
        {default} {
	  if {"$xfCounter" == ""} {
	    set xfNextIs ""
	  }
          if {"$xfNextIs" == "in"} {
            set xfNextIs ""
          }
          if {"$xfNextIs" == "x"} {
            set xfMisc(layoutX) 1
            catch ".xfPlacing$xfClass.frame2.pos.pos1 set \
                     [lindex [split $xfCounter .] 0]"
            set xfNextIs ""
          }
          if {"$xfNextIs" == "y"} {
            set xfMisc(layoutY) 1
            catch ".xfPlacing$xfClass.frame2.pos.pos2 set \
                     [lindex [split $xfCounter .] 0]"
            set xfNextIs ""
          }
          if {"$xfNextIs" == "relx"} {
            set xfMisc(layoutX) 0
            catch ".xfPlacing$xfClass.frame2.pos.pos1 set \
                     [lindex [split [expr $xfCounter*100] .] 0]"
            set xfNextIs ""
          }
          if {"$xfNextIs" == "rely"} {
            set xfMisc(layoutY) 0
            catch ".xfPlacing$xfClass.frame2.pos.pos2 set \
                     [lindex [split [expr $xfCounter*100] .] 0]"
            set xfNextIs ""
          }
          if {"$xfNextIs" == "width"} {
            set xfMisc(layoutWidth) 1
            catch ".xfPlacing$xfClass.frame2.geo.geo1 set \
                     [lindex [split $xfCounter .] 0]"
            set xfNextIs ""
          }
          if {"$xfNextIs" == "height"} {
            set xfMisc(layoutHeight) 1
            catch ".xfPlacing$xfClass.frame2.geo.geo2 set \
                     [lindex [split $xfCounter .] 0]"
            set xfNextIs ""
          }
          if {"$xfNextIs" == "relwidth"} {
            set xfMisc(layoutWidth) 0
            catch ".xfPlacing$xfClass.frame2.geo.geo1 set \
                     [lindex [split [expr $xfCounter*100] .] 0]"
            set xfNextIs ""
          }
          if {"$xfNextIs" == "relheight"} {
            set xfMisc(layoutHeight) 0
            catch ".xfPlacing$xfClass.frame2.geo.geo2 set \
                     [lindex [split [expr $xfCounter*100] .] 0]"
            set xfNextIs ""
          }
          if {"$xfNextIs" == "border"} {
            case [string tolower $xfCounter] in {
              {inside} {
                .xfPlacing$xfClass.frame2.border.inside select
              }
              {outside} {
                .xfPlacing$xfClass.frame2.border.outside select
              }
              {ignore} {
                .xfPlacing$xfClass.frame2.border.ignore select
              }
            }
            set xfNextIs ""
          }
          if {"$xfNextIs" == "anchor"} {
            case [string tolower $xfCounter] in {
              {nw} {
                .xfPlacing$xfClass.frame2.anchor.f1.nw select
              }
              {w} {
                .xfPlacing$xfClass.frame2.anchor.f1.w select
              }
              {sw} {
                .xfPlacing$xfClass.frame2.anchor.f1.sw select
              }
              {n} {
                .xfPlacing$xfClass.frame2.anchor.f2.n select
              }
              {center} {
                .xfPlacing$xfClass.frame2.anchor.f2.c select
              }
              {s} {
                .xfPlacing$xfClass.frame2.anchor.f2.s select
              }
              {ne} {
                .xfPlacing$xfClass.frame2.anchor.f3.ne select
              }
              {e} {
                .xfPlacing$xfClass.frame2.anchor.f3.e select
              }
              {se} {
                .xfPlacing$xfClass.frame2.anchor.f3.se select
              }
            }
            set xfNextIs ""
          }
        }
      }
    }
    if {$xfGotAnchor == 0} {
      .xfPlacing$xfClass.frame2.anchor.f1.nw select
    }
    if {$xfGotBorder == 0} {
      .xfPlacing$xfClass.frame2.border.inside select
    }
    if {$xfGotX == 0 && $xfGotRelX == 0} {
      set xfMisc(layoutX) 1
      .xfPlacing$xfClass.frame2.pos.pos1 set 0
    }
    if {$xfGotY == 0 && $xfGotRelY == 0} {
      set xfMisc(layoutY) 1
      .xfPlacing$xfClass.frame2.pos.pos2 set 0
    }
    if {$xfGotWidth == 0 && $xfGotRelWidth == 0} {
      set xfMisc(layoutWidth) 1
      .xfPlacing$xfClass.frame2.geo.geo1 set 0
    }
    if {$xfGotHeight == 0 && $xfGotRelHeight == 0} {
      set xfMisc(layoutHeight) 1
      .xfPlacing$xfClass.frame2.geo.geo2 set 0
    }
    XFMiscFlash [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected]

    .xfPlacing$xfClass.frame2.geo.geo1 configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.geo.geo2 configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.pos.pos1 configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.pos.pos2 configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.anchor.f1.nw configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.anchor.f1.w configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.anchor.f1.sw configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.anchor.f2.n configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.anchor.f2.c configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.anchor.f2.s configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.anchor.f3.ne configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.anchor.f3.e configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.anchor.f3.se configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.border.inside configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.border.outside configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

    .xfPlacing$xfClass.frame2.border.ignore configure \
      -command "XFPlacingUpdatePlacing $xfW $xfClass 1"
  }
}

##########
# Procedure: XFPlacingMakeChild
# Description: make current selection child of master
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPlacingMakeChild {xfW xfClass} {

  set xfCurSelected [.xfPlacing$xfClass.frame2.children.widgets.widgets curselection]
  if {$xfCurSelected >= 0} {
    set xfCurrent [lindex [.xfPlacing$xfClass.frame2.children.widgets.widgets get $xfCurSelected] 1]
    if {"$xfCurrent" == ""} {
      return
    }
    if {"$xfCurrent" == "."} {
      return
    }
    if {"[winfo parent $xfCurrent]" != \
        "[string trimright [string range $xfW 0 [string length [winfo parent $xfCurrent]]] .]" &&
        "[winfo parent $xfCurrent]" != "."} {
      return
    }
    if {"[winfo parent $xfCurrent]" == "."} {
      if {"$xfW" != "."} {
        return
      }
    }
    set xfPackList [pack slave [winfo parent $xfCurrent]]
    set xfElementType "window"
    foreach xfCounter $xfPackList {
      if {"$xfElementType" == "window"} {
        if {"$xfCurrent" == "$xfCounter"} {
          catch "pack unpack $xfCurrent"
        }
        set xfElementType "options"
      } {
        set xfElementType "window"
      }
    }
    if {"[place info $xfCurrent]" != ""} {
      catch "place forget $xfCurrent"
    }
    place $xfCurrent -in $xfW
    XFPlacingReadPlacing $xfW $xfClass
  }
}

##########
# Procedure: XFPlacingMakeMaster
# Description: make current selection new master
# Arguments: xfW - the widget we configure
#            xfType - the type of configuration (add, config)
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPlacingMakeMaster {xfW xfType xfClass} {
  global xfMisc

  set xfMisc(placingMaster,$xfClass) $xfW
  wm title .xfPlacing$xfClass "XF placing:[XFMiscPathTail $xfW]"

  .xfPlacing$xfClass.frame1.ok configure \
    -command "
      XFPlacingSetPlacing $xfW $xfClass
      destroy .xfPlacing$xfClass"

  .xfPlacing$xfClass.frame1.apply configure \
    -command "XFPlacingSetPlacing $xfW $xfClass"

  .xfPlacing$xfClass.frame1.applyperm configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame1.undo configure \
    -command "XFPlacingUndoPlacing $xfW $xfClass"

  .xfPlacing$xfClass.frame1.cancel configure \
    -command "
      XFPlacingUndoPlacing $xfW $xfClass
      destroy .xfPlacing$xfClass"

  .xfPlacing$xfClass.message1 configure \
    -text "Placing:$xfW"

  .xfPlacing$xfClass.additional.parameters configure \
    -command "XFProcConfParametersSmall $xfW .xfPlacing$xfClass"

  .xfPlacing$xfClass.additional.chldparameters configure \
    -command "
      set xfCurSelected \
        \[.xfPlacing$xfClass.frame2.children.childs.childs curselection\]
      if {\$xfCurSelected >= 0} {
        set xfChld \[.xfPlacing$xfClass.frame2.children.childs.childs get \$xfCurSelected\]
        XFProcConfParametersSmall \$xfChld .xfPlacing$xfClass
      }"

  .xfPlacing$xfClass.additional.binding configure \
    -command "XFProcConfBinding $xfW .xfPlacing$xfClass"

  .xfPlacing$xfClass.additional.chldbinding configure \
    -command "
      set xfCurSelected \
        \[.xfPlacing$xfClass.frame2.children.childs.childs curselection\]
      if {\$xfCurSelected >= 0} {
        set xfChld \[.xfPlacing$xfClass.frame2.children.childs.childs get \$xfCurSelected\]
        XFProcConfBinding \$xfChld .xfPlacing$xfClass
      }"

  .xfPlacing$xfClass.functions.rescanmaster configure \
    -command "
      XFMiscReadTree $xfW .xfPlacing$xfClass.frame2.children.widgets.widgets"

  .xfPlacing$xfClass.functions.child configure \
    -command "XFPlacingMakeChild $xfW $xfClass"

  .xfPlacing$xfClass.functions.delchild configure \
    -command "XFPlacingMakeRemove $xfW $xfClass"

  .xfPlacing$xfClass.frame2.geo.geo1 configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.geo.geo2 configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.pos.pos1 configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.pos.pos2 configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.anchor.f1.nw configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.anchor.f1.w configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.anchor.f1.sw configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.anchor.f2.n configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.anchor.f2.c configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.anchor.f2.s configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.anchor.f3.ne configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.anchor.f3.e configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.anchor.f3.se configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.border.inside configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.border.outside configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  .xfPlacing$xfClass.frame2.border.ignore configure \
    -command "XFPlacingUpdatePlacing $xfW $xfClass 1"

  bind .xfPlacing$xfClass.frame2.children.childs.childs <ButtonPress-1> "
    XFBindSelectOne %W %y
    XFPlacingGetPlacing $xfW $xfClass"
  bind .xfPlacing$xfClass.frame2.children.childs.childs <Shift-ButtonPress-1> "
    XFBindSelectOne %W %y
    XFPlacingGetPlacing $xfW $xfClass"

  XFPlacingReadPlacing $xfW $xfClass

  set xfMisc($xfClass,savePlaceChilds) ""
  set xfMisc($xfClass,savePlacing) ""

  set xfPlaceList [place slaves $xfW]
  foreach xfCounter $xfPlaceList {
    lappend xfMisc($xfClass,savePlaceChilds) $xfCounter
    lappend xfMisc($xfClass,savePlacing) [place info $xfCounter]
  }
}

##########
# Procedure: XFPlacingMakeRemove
# Description: remove current selection from master
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPlacingMakeRemove {xfW xfClass} {

  set xfCurSelected [.xfPlacing$xfClass.frame2.children.childs.childs curselection]
  if {$xfCurSelected >= 0} {
    place forget [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected]
    XFPlacingReadPlacing $xfW $xfClass
  }
}

##########
# Procedure: XFPlacingReadPlacing
# Description: read placing
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPlacingReadPlacing {xfW xfClass} {
  global xfMisc

  XFMiscClearList .xfPlacing$xfClass.frame2.children.childs.childs
  set xfMisc($xfClass,curPlacing) ""

  set xfPlaceList [place slaves $xfW]
  foreach xfCounter $xfPlaceList {
    if {![string match "xf*" [winfo name $xfCounter]]} {
      .xfPlacing$xfClass.frame2.children.childs.childs insert end "$xfCounter"
      lappend xfMisc($xfClass,curPlacing) [place info $xfCounter]
    }
  }
  if {[llength $xfMisc($xfClass,curPlacing)] > 0} {
    .xfPlacing$xfClass.frame2.children.childs.childs select anchor 0
    .xfPlacing$xfClass.frame2.children.childs.childs select set 0
    XFPlacingGetPlacing $xfW $xfClass
    set xfMisc(firstPlacing) 0
    XFPlacingUpdatePlacing $xfW $xfClass 1
  } {
    set xfMisc(firstPlacing) 0
  }
}

##########
# Procedure: XFPlacingSetPlacing
# Description: set placing for currently selected widget
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPlacingSetPlacing {xfW xfClass} {
  global xfMisc

  set xfCounter 0
  while {$xfCounter < [llength $xfMisc($xfClass,curPlacing)]} {
    set xfPlaceString "place "
    append xfPlaceString [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCounter]
    append xfPlaceString " "
    append xfPlaceString [lindex $xfMisc($xfClass,curPlacing) $xfCounter]
    if {[catch "$xfPlaceString" xfResult]} {
      XFProcError "$xfResult"
    }
    incr xfCounter 1
  }
}

##########
# Procedure: XFPlacingToggleScale
# Description: change the scale from abs to rel and vice versa
# Arguments: xfClass - the current edited class
#            xfScale - the scale we toggle
# Returns: none
# Sideeffects: none
##########
proc XFPlacingToggleScale {xfClass xfScale} {
  global xfMisc

  set xfCurSelected [.xfPlacing$xfClass.frame2.children.childs.childs curselection]
  if {$xfCurSelected >= 0} {
    if {"$xfScale" == "x"} {
      if {$xfMisc(layoutX)} {
        if {[lindex [.xfPlacing$xfClass.frame2.pos.pos1 config -to] 4] == 100} {
          set xfNewPos [expr ([.xfPlacing$xfClass.frame2.pos.pos1 get]*[winfo width $xfMisc(placingMaster,$xfClass)])/100]
          place [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected] -x $xfNewPos
          .xfPlacing$xfClass.frame2.pos.pos1 config -to 600
          .xfPlacing$xfClass.frame2.pos.pos1 set \
	    [lindex [split $xfNewPos .] 0]
        }
      } {
        if {[lindex [.xfPlacing$xfClass.frame2.pos.pos1 config -to] 4] == 600} {
          set xfNewPos [expr ([.xfPlacing$xfClass.frame2.pos.pos1 get]*100)/[winfo width $xfMisc(placingMaster,$xfClass)]]
          place [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected] -relx [expr $xfNewPos/100]
          .xfPlacing$xfClass.frame2.pos.pos1 config -to 100
          .xfPlacing$xfClass.frame2.pos.pos1 set \
            [lindex [split $xfNewPos .] 0]
        }
      }
    } {
      if {"$xfScale" == "y"} {
        if {$xfMisc(layoutY)} {
          if {[lindex [.xfPlacing$xfClass.frame2.pos.pos2 config -to] 4] == 100} {
            set xfNewPos [expr ([.xfPlacing$xfClass.frame2.pos.pos2 get]*[winfo height $xfMisc(placingMaster,$xfClass)])/100]
            place [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected] -y $xfNewPos
            .xfPlacing$xfClass.frame2.pos.pos2 config -to 600
            .xfPlacing$xfClass.frame2.pos.pos2 set \
              [lindex [split $xfNewPos .] 0]
          }
        } {
          if {[lindex [.xfPlacing$xfClass.frame2.pos.pos2 config -to] 4] == 600} {
            set xfNewPos [expr ([.xfPlacing$xfClass.frame2.pos.pos2 get]*100)/[winfo height $xfMisc(placingMaster,$xfClass)]]
            place [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected] -rely [expr $xfNewPos/100]
            .xfPlacing$xfClass.frame2.pos.pos2 config -to 100
            .xfPlacing$xfClass.frame2.pos.pos2 set \
              [lindex [split $xfNewPos .] 0]
          }
        }
      } {
        if {"$xfScale" == "width"} {
          if {$xfMisc(layoutWidth)} {
            if {[lindex [.xfPlacing$xfClass.frame2.geo.geo1 config -to] 4] == 100} {
              set xfNewPos [expr ([.xfPlacing$xfClass.frame2.geo.geo1 get]*[winfo width $xfMisc(placingMaster,$xfClass)])/100]
              place [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected] -width $xfNewPos
              .xfPlacing$xfClass.frame2.geo.geo1 config -to 600
              .xfPlacing$xfClass.frame2.geo.geo1 set \
                [lindex [split $xfNewPos .] 0]
            }
          } {
            if {[lindex [.xfPlacing$xfClass.frame2.geo.geo1 config -to] 4] == 600} {
              set xfNewPos [expr ([.xfPlacing$xfClass.frame2.geo.geo1 get]*100)/[winfo width $xfMisc(placingMaster,$xfClass)]]
              place [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected] -relwidth [expr $xfNewPos/100]
              .xfPlacing$xfClass.frame2.geo.geo1 config -to 100
              .xfPlacing$xfClass.frame2.geo.geo1 set \
                [lindex [split $xfNewPos .] 0]
            }
          }
        } {
          if {$xfMisc(layoutHeight)} {
            if {[lindex [.xfPlacing$xfClass.frame2.geo.geo2 config -to] 4] == 100} {
              set xfNewPos [expr ([.xfPlacing$xfClass.frame2.geo.geo2 get]*[winfo height $xfMisc(placingMaster,$xfClass)])/100]
              place [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected] -height $xfNewPos
              .xfPlacing$xfClass.frame2.geo.geo2 config -to 600
              .xfPlacing$xfClass.frame2.geo.geo2 set \
                [lindex [split $xfNewPos .] 0]
            }
          } {
            if {[lindex [.xfPlacing$xfClass.frame2.geo.geo2 config -to] 4] == 600} {
              set xfNewPos [expr ([.xfPlacing$xfClass.frame2.geo.geo2 get]*100)/[winfo height $xfMisc(placingMaster,$xfClass)]]
              place [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected] -relheight [expr $xfNewPos/100]
              .xfPlacing$xfClass.frame2.geo.geo2 config -to 100
              .xfPlacing$xfClass.frame2.geo.geo2 set \
                [lindex [split $xfNewPos .] 0]
            }
          }
        }
      }
    }
  }
}

##########
# Procedure: XFPlacingUndoPlacing
# Description: undo placing for widgets
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
# Returns: none
# Sideeffects: none
##########
proc XFPlacingUndoPlacing {xfW xfClass} {
  global xfMisc

  if {[llength $xfMisc($xfClass,savePlaceChilds)] > 0} {
    set xfCounter 0
    while {$xfCounter < [llength $xfMisc($xfClass,savePlaceChilds)]} {
      set xfPlaceString "place "
      append xfPlaceString [lindex $xfMisc($xfClass,savePlaceChilds) $xfCounter]
      append xfPlaceString " "
      append xfPlaceString [lindex $xfMisc($xfClass,savePlacing) $xfCounter]
      if {[catch "$xfPlaceString" xfResult]} {
        XFProgError "$xfResult"
      }
      incr xfCounter 1
    }
  }
}

##########
# Procedure: XFPlacingUpdatePlacing
# Description: update placing for widget
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
#            xfUpdatePos - update the widget position
#            xfParam1 - ignored parameter
# Returns: none
# Sideeffects: none
##########
proc XFPlacingUpdatePlacing {xfW xfClass {xfUpdatePos 1} {xfParam1 ""}} {
  global xfConf
  global xfMisc

  set xfApplyPlacing [set xfConf(applyPlacing)]
  set xfAnchor [set xfMisc(${xfClass},anchor)]
  set xfBorder [set xfMisc(${xfClass},border)]

  if {$xfMisc(firstPlacing)} {
    return
  }

  set xfCurSelected [.xfPlacing$xfClass.frame2.children.childs.childs curselection]
  if {$xfCurSelected >= 0} {
    set xfNewPlacing ""
    append xfNewPlacing " -anchor $xfAnchor"
    if {"$xfBorder" != "inside"} {
      append xfNewPlacing " -bordermode $xfBorder"
    }
    if {[.xfPlacing$xfClass.frame2.geo.geo1 get] > 0} {
      if {$xfMisc(layoutWidth)} {
        append xfNewPlacing " -width [.xfPlacing$xfClass.frame2.geo.geo1 get]"
      } {
        append xfNewPlacing " -relwidth [expr [.xfPlacing$xfClass.frame2.geo.geo1 get]/100.0]"
      }
    }
    if {[.xfPlacing$xfClass.frame2.geo.geo2 get] > 0} {
      if {$xfMisc(layoutHeight)} {
        append xfNewPlacing " -height [.xfPlacing$xfClass.frame2.geo.geo2 get]"
      } {
        append xfNewPlacing " -relheight [expr [.xfPlacing$xfClass.frame2.geo.geo2 get]/100.0]"
      }
    }
    if {[.xfPlacing$xfClass.frame2.pos.pos1 get] > 0} {
      if {$xfMisc(layoutX)} {
        append xfNewPlacing " -x [.xfPlacing$xfClass.frame2.pos.pos1 get]"
      } {
        append xfNewPlacing " -relx [expr [.xfPlacing$xfClass.frame2.pos.pos1 get]/100.0]"
      }
    }
    if {[.xfPlacing$xfClass.frame2.pos.pos2 get] > 0} {
      if {$xfMisc(layoutY)} {
        append xfNewPlacing " -y [.xfPlacing$xfClass.frame2.pos.pos2 get]"
      } {
        append xfNewPlacing " -rely [expr [.xfPlacing$xfClass.frame2.pos.pos2 get]/100.0]"
      }
    }
    if {[.xfPlacing$xfClass.frame2.pos.pos1 get] == 0} {
      append xfNewPlacing " -x 0"
    }
    if {[.xfPlacing$xfClass.frame2.pos.pos2 get] == 0} {
      append xfNewPlacing " -y 0"
    }
    set xfMisc($xfClass,curPlacing) \
      [lreplace $xfMisc($xfClass,curPlacing) $xfCurSelected $xfCurSelected $xfNewPlacing]

    if {$xfConf(applyPlacing) && $xfUpdatePos} {
      set xfPlaceString "place "
      append xfPlaceString \
        [.xfPlacing$xfClass.frame2.children.childs.childs get $xfCurSelected] \
          " -in " $xfW
      append xfPlaceString $xfNewPlacing
      if {[catch "$xfPlaceString" xfResult]} {
        XFProcError "$xfResult"
      }
    }
  }
}

# eof

