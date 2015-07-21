# Program: xf
# Description: the option dialog for bindings
#
# $Header: xfoptBind.tcl[2.3] Wed Mar 10 12:07:10 1993 garfield@garfield frozen $

##########
# Procedure: XFOptionsBind
# Description: allow the editing of the bindings
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsBind {} {
  global xfBind

  # build widget structure
  XFTmpltToplevel .xfOptionsBind 400x260 {XF bindings}

  XFTmpltFrame .xfOptionsBind.frame1 0

  XFTmpltFrame .xfOptionsBind.frame2

  XFTmpltFrame .xfOptionsBind.frame3

  button .xfOptionsBind.frame1.ok \
    -text {OK} \
    -command {
      XFOptionsBindSet
      destroy .xfOptionsBind}

  button .xfOptionsBind.frame1.save \
    -text {Save + OK} \
    -command {
      XFOptionsBindSet
      XFProcOptionsSaveOptions
      destroy .xfOptionsBind}

  button .xfOptionsBind.frame1.cancel \
    -text {Cancel} \
    -command {destroy .xfOptionsBind}

  label .xfOptionsBind.frame2.message2 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Call configuration:}

  label .xfOptionsBind.frame2.message3 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Select current widget:}

  label .xfOptionsBind.frame2.message4 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Primary select:}

  label .xfOptionsBind.frame2.message5 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Secondary select:}

  label .xfOptionsBind.frame2.message6 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Tertiary select:}

  label .xfOptionsBind.frame2.message7 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Show widget name:}

  label .xfOptionsBind.frame2.message8 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Remove widget name:}

  label .xfOptionsBind.frame2.message9 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Begin widget moving:}

  label .xfOptionsBind.frame2.message10 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Move widget:}

  label .xfOptionsBind.frame2.message11 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {End widget moving:}

  label .xfOptionsBind.frame2.message12 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Popup menu (mouse nr.):}

  entry .xfOptionsBind.frame3.config \
    -relief sunken
  .xfOptionsBind.frame3.config insert 0 $xfBind(configure)

  entry .xfOptionsBind.frame3.select \
    -relief sunken
  .xfOptionsBind.frame3.select insert 0 $xfBind(select)

  entry .xfOptionsBind.frame3.select1 \
    -relief sunken
  .xfOptionsBind.frame3.select1 insert 0 $xfBind(select1)

  entry .xfOptionsBind.frame3.select2 \
    -relief sunken
  .xfOptionsBind.frame3.select2 insert 0 $xfBind(select2)

  entry .xfOptionsBind.frame3.select3 \
    -relief sunken
  .xfOptionsBind.frame3.select3 insert 0 $xfBind(select3)

  entry .xfOptionsBind.frame3.showname \
    -relief sunken
  .xfOptionsBind.frame3.showname insert 0 $xfBind(showName)

  entry .xfOptionsBind.frame3.removename \
    -relief sunken
  .xfOptionsBind.frame3.removename insert 0 $xfBind(removeName)

  entry .xfOptionsBind.frame3.placing \
    -relief sunken
  .xfOptionsBind.frame3.placing insert 0 $xfBind(placing)

  entry .xfOptionsBind.frame3.placingmot \
    -relief sunken
  .xfOptionsBind.frame3.placingmot insert 0 $xfBind(placingMotion)

  entry .xfOptionsBind.frame3.placingrel \
    -relief sunken
  .xfOptionsBind.frame3.placingrel insert 0 $xfBind(placingRelease)

  entry .xfOptionsBind.frame3.popup \
    -relief sunken
  .xfOptionsBind.frame3.popup insert 0 $xfBind(popup)

  # packing
  pack append .xfOptionsBind.frame1 \
              .xfOptionsBind.frame1.ok {left fill expand} \
              .xfOptionsBind.frame1.save {left fill expand} \
              .xfOptionsBind.frame1.cancel {left fill expand}
  pack append .xfOptionsBind.frame2 \
              .xfOptionsBind.frame2.message2 {top fillx} \
              .xfOptionsBind.frame2.message3 {top fillx} \
              .xfOptionsBind.frame2.message4 {top fillx} \
              .xfOptionsBind.frame2.message5 {top fillx} \
              .xfOptionsBind.frame2.message6 {top fillx} \
              .xfOptionsBind.frame2.message7 {top fillx} \
              .xfOptionsBind.frame2.message8 {top fillx} \
              .xfOptionsBind.frame2.message9 {top fillx} \
              .xfOptionsBind.frame2.message10 {top fillx} \
              .xfOptionsBind.frame2.message11 {top fillx} \
              .xfOptionsBind.frame2.message12 {top fillx}
  pack append .xfOptionsBind.frame3 \
              .xfOptionsBind.frame3.config {top fillx pady 4} \
              .xfOptionsBind.frame3.select {top fillx pady 4} \
              .xfOptionsBind.frame3.select1 {top fillx pady 4} \
              .xfOptionsBind.frame3.select2 {top fillx pady 4} \
              .xfOptionsBind.frame3.select3 {top fillx pady 4} \
              .xfOptionsBind.frame3.showname {top fillx pady 4} \
              .xfOptionsBind.frame3.removename {top fillx pady 4} \
              .xfOptionsBind.frame3.placing {top fillx pady 4} \
              .xfOptionsBind.frame3.placingmot {top fillx pady 4} \
              .xfOptionsBind.frame3.placingrel {top fillx pady 4} \
              .xfOptionsBind.frame3.popup {top fillx pady 4}
  pack append .xfOptionsBind \
              .xfOptionsBind.frame1 {bottom fill} \
              .xfOptionsBind.frame2 {left filly} \
              .xfOptionsBind.frame3 {left fill expand}

  XFBindFormConnect .xfOptionsBind.frame3
}

##########
# Procedure: XFOptionsBindSet
# Description: set the new bindings
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsBindSet {} {
  global xfBind

  XFMiscRemoveBindWidgetTree . all    
  if {"[string index [.xfOptionsBind.frame3.config get] 0]" == "<" &&
      "[string index [.xfOptionsBind.frame3.config get] \
        [expr [string length [.xfOptionsBind.frame3.config get]]-1]]" == ">"} {
    set xfBind(configure) [.xfOptionsBind.frame3.config get]
  }
  if {"[string index [.xfOptionsBind.frame3.placing get] 0]" == "<" &&
      "[string index [.xfOptionsBind.frame3.placing get] \
        [expr [string length [.xfOptionsBind.frame3.placing get]]-1]]" == ">"} {
    set xfBind(placing) [.xfOptionsBind.frame3.placing get]
  }
  if {"[string index [.xfOptionsBind.frame3.placingmot get] 0]" == "<" &&
      "[string index [.xfOptionsBind.frame3.placingmot get] \
        [expr [string length [.xfOptionsBind.frame3.placingmot get]]-1]]" == ">"} {
    set xfBind(placingMotion) [.xfOptionsBind.frame3.placingmot get]
  }
  if {"[string index [.xfOptionsBind.frame3.placingrel get] 0]" == "<" &&
      "[string index [.xfOptionsBind.frame3.placingrel get] \
        [expr [string length [.xfOptionsBind.frame3.placingrel get]]-1]]" == ">"} {
    set xfBind(placingRelease) [.xfOptionsBind.frame3.placingrel get]
  }
  if {"[string index [.xfOptionsBind.frame3.select get] 0]" == "<" &&
      "[string index [.xfOptionsBind.frame3.select get] \
        [expr [string length [.xfOptionsBind.frame3.select get]]-1]]" == ">"} {
    set xfBind(select) [.xfOptionsBind.frame3.select get]
  }
  if {"[string index [.xfOptionsBind.frame3.select1 get] 0]" == "<"&&
      "[string index [.xfOptionsBind.frame3.select1 get] \
        [expr [string length [.xfOptionsBind.frame3.select1 get]]-1]]" == ">"} {
    set xfBind(select1) [.xfOptionsBind.frame3.select1 get]
  }
  if {"[string index [.xfOptionsBind.frame3.select2 get] 0]" == "<" &&
      "[string index [.xfOptionsBind.frame3.select2 get] \
        [expr [string length [.xfOptionsBind.frame3.select2 get]]-1]]" == ">"} {
    set xfBind(select2) [.xfOptionsBind.frame3.select2 get]
  }
  if {"[string index [.xfOptionsBind.frame3.select3 get] 0]" == "<" &&
      "[string index [.xfOptionsBind.frame3.select3 get] \
        [expr [string length [.xfOptionsBind.frame3.select3 get]]-1]]" == ">"} {
    set xfBind(select3) [.xfOptionsBind.frame3.select3 get]
  }
  if {"[string index [.xfOptionsBind.frame3.showname get] 0]" == "<" &&
      "[string index [.xfOptionsBind.frame3.showname get] \
        [expr [string length [.xfOptionsBind.frame3.showname get]]-1]]" == ">"} {
    set xfBind(showName) [.xfOptionsBind.frame3.showname get]
  }
  if {"[string index [.xfOptionsBind.frame3.removename get] 0]" == "<" &&
      "[string index [.xfOptionsBind.frame3.removename get] \
          [expr [string length [.xfOptionsBind.frame3.removename get]]-1]]" == ">"} {
    set xfBind(removeName) [.xfOptionsBind.frame3.removename get]
  }
  if {[string length [.xfOptionsBind.frame3.popup get]] == 1} {
    set xfBindPopup [.xfOptionsBind.frame3.popup get]
  } {
    set xfBindPopup 3
  }
  XFMiscBindWidgetTree .
}

# eof

