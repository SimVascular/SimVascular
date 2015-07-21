# Program: xf
# Description: set parameters for one widget
#
# $Header: xfparams.tcl[2.3] Wed Mar 10 12:07:36 1993 garfield@garfield frozen $

##########
# Procedure: XFParameters
# Description: set parameters for one widget
# Arguments: xfW - the widget to configure
#            xfLeader - the leading window
# Returns: none
# Sideeffects: none
##########
proc XFParameters {xfW xfLeader} {
  global xfBind
  global xfStatus

  set xfStatus(currentConfigWidget) $xfW

  # build widget structure
  XFTmpltToplevel .xfParameters 400x400 \
   "XF general parameters" $xfLeader

  button .xfParameters.message1 \
    -command {
      global xfStatus
      XFMiscFlash $xfStatus(currentConfigWidget)} \
    -relief raised \
    -text "Set parameters for: $xfStatus(currentConfigWidget)"

  XFTmpltFrame .xfParameters.frame1 0

  button .xfParameters.frame1.ok \
    -text {OK} \
    -command {
      XFParametersSet 1
      destroy .xfParameters}

  button .xfParameters.frame1.rescan \
    -text {Rescan} \
    -command {
      global xfStatus
      XFMiscReadTree . .xfParameters.frame2.widgets.widgets all sym
      set xfCounter 0
      while {$xfCounter < [.xfParameters.frame2.widgets.widgets size]} {
        if {"$xfStatus(currentConfigWidget)" == "[lindex [.xfParameters.frame2.widgets.widgets get $xfCounter] 1]"} {
          .xfParameters.frame2.widgets.widgets select anchor $xfCounter
          .xfParameters.frame2.widgets.widgets select set $xfCounter
          break
        }
        incr xfCounter
      }
      XFParametersGetResources $xfStatus(currentConfigWidget)}

  checkbutton .xfParameters.frame1.rescanperm \
    -offvalue 0 \
    -onvalue 1 \
    -text {Rescan permanently} \
    -variable xfConf(scanTree)

  button .xfParameters.frame1.cancel \
    -text {Cancel} \
    -command {
      global xfMisc
      global xfStatus
      foreach xfCounter $xfMisc(generalParamsSave) {
        XFMiscSetResource \
          $xfStatus(currentConfigWidget) [lindex $xfCounter 0] \
            [lindex $xfCounter 1]
      }
      destroy .xfParameters}

  XFTmpltFrame .xfParameters.frame3 0

  button .xfParameters.frame3.apply \
    -text {Apply} \
    -command "XFParametersSetAllResources"

  button .xfParameters.frame3.applytree \
    -text {Apply to subtree} \
    -command "XFParametersSetAllResources 1"

  checkbutton .xfParameters.frame3.applyperm \
    -text {Apply permanently} \
    -variable xfConf(applyParameters) \
    -onvalue 1 \
    -offvalue 0 \
    -command "XFParametersSet 0"

  button .xfParameters.frame3.undo \
    -text {Undo} \
    -command {
      global xfMisc
      global xfStatus
      foreach xfCounter $xfMisc(generalParamsSave) {
        XFMiscSetResource \
          $xfStatus(currentConfigWidget) [lindex $xfCounter 0] \
            [lindex $xfCounter 1]
      }
      set xfMisc(generalParams) $xfMisc(generalParamsSave)
      XFParametersSetResource [.xfParameters.frame2.resources.resources curselect]}

  XFTmpltFrame .xfParameters.frame5 0

  button .xfParameters.frame5.color \
    -text {Color} \
    -command "XFProcColorBox \"\[.xfParameters.resource.resource get\]\" .xfParameters.value.value"

  button .xfParameters.frame5.cursor \
    -text {Cursor} \
    -command "XFProcCursorBox \"\[.xfParameters.resource.resource get\]\" .xfParameters.value.value"

  button .xfParameters.frame5.font \
    -text {Font} \
    -command "XFProcFontBox \"\[.xfParameters.resource.resource get\]\" .xfParameters.value.value"

  button .xfParameters.frame5.pixmap \
    -text {Pixmap} \
    -command "XFProcFSBoxPixmap .xfParameters.value.value"

  button .xfParameters.frame5.file \
    -text {File} \
    -command "XFProcFSBoxFile .xfParameters.value.value"

  XFTmpltFrame .xfParameters.frame2 0

  XFTmpltListbox .xfParameters.frame2 widgets
  .xfParameters.frame2.widgets.widgets configure \
    -width 16 -height 30

  label .xfParameters.frame2.widgets.widgetsMess \
    -relief raised \
    -text {Widgets:}

  XFTmpltListbox .xfParameters.frame2 resources
  .xfParameters.frame2.resources.resources configure \
    -width 25 -height 30

  label .xfParameters.frame2.resources.resourcesMess \
    -relief raised \
    -text {Resources:}

  XFTmpltLabledEntry .xfParameters symname "Symbolic name:"

  button .xfParameters.symname.setsymname \
    -text "Set symbolic name" \
    -command {
      global xfStatus
      XFMiscSetSymbolicName $xfStatus(currentConfigWidget) \
        [.xfParameters.symname.symname get]
      XFMiscReadTree . .xfParameters.frame2.widgets.widgets all sym
      set xfCounter 0
      while {$xfCounter < [.xfParameters.frame2.widgets.widgets size]} {
        if {"$xfStatus(currentConfigWidget)" == "[lindex [.xfParameters.frame2.widgets.widgets get $xfCounter] 1]"} {
          .xfParameters.frame2.widgets.widgets select anchor $xfCounter
          .xfParameters.frame2.widgets.widgets select set $xfCounter
          break
        }
        incr xfCounter
      }
      XFParametersGetResources $xfStatus(currentConfigWidget)}

  XFTmpltFrame .xfParameters.class 0

  label .xfParameters.class.message1 \
    -relief raised \
    -text {Class:}

  label .xfParameters.class.class \
    -anchor w \
    -relief raised \
    -text "[winfo class $xfStatus(currentConfigWidget)]"

  XFTmpltLabledEntry .xfParameters resource "Resource:"

  label .xfParameters.message2 \
    -relief raised \
    -text {Value:}

  XFTmpltText .xfParameters value
  XFMiscSetTextHeight \
    ".xfParameters.value.value value" 4

  XFMiscReadTree . .xfParameters.frame2.widgets.widgets all sym
  set xfCounter 0
  while {$xfCounter < [.xfParameters.frame2.widgets.widgets size]} {
    if {"$xfStatus(currentConfigWidget)" == "[lindex [.xfParameters.frame2.widgets.widgets get $xfCounter] 1]"} {
      .xfParameters.frame2.widgets.widgets select anchor $xfCounter
      .xfParameters.frame2.widgets.widgets select set $xfCounter
      break
    }
    incr xfCounter
  }

  menu .xfParameters.frame2.widgets.widgets.m
       .xfParameters.frame2.widgets.widgets.m add command \
         -label {Parameters} \
         -command {
           XFProcConfParametersDefault \
             [lindex [.xfParameters.frame2.widgets.widgets get \
               [.xfParameters.frame2.widgets.widgets curselect]] 1]}
       .xfParameters.frame2.widgets.widgets.m add separator
       .xfParameters.frame2.widgets.widgets.m add command \
         -label {Packing} \
         -command {
           XFProcConfPacking \
             [lindex [.xfParameters.frame2.widgets.widgets get \
               [.xfParameters.frame2.widgets.widgets curselect]] 1]}
       .xfParameters.frame2.widgets.widgets.m add command \
         -label {Placing} \
         -command {
           XFProcConfPlacing \
             [lindex [.xfParameters.frame2.widgets.widgets get \
               [.xfParameters.frame2.widgets.widgets curselect]] 1]}
       .xfParameters.frame2.widgets.widgets.m add separator
       .xfParameters.frame2.widgets.widgets.m add command \
         -label {Binding} \
         -command {
           XFProcConfBinding \
             [lindex [.xfParameters.frame2.widgets.widgets get \
               [.xfParameters.frame2.widgets.widgets curselect]] 1]}
       .xfParameters.frame2.widgets.widgets.m add separator
       .xfParameters.frame2.widgets.widgets.m add command \
         -label {Parameters (small)} \
         -command {
           XFProcConfParametersSmall \
             [lindex [.xfParameters.frame2.widgets.widgets get \
               [.xfParameters.frame2.widgets.widgets curselect]] 1]}
       .xfParameters.frame2.widgets.widgets.m add command \
         -label {Parameters (special)} \
         -command {
           XFProcConfParametersSpecial \
             [lindex [.xfParameters.frame2.widgets.widgets get \
               [.xfParameters.frame2.widgets.widgets curselect]] 1]}
       .xfParameters.frame2.widgets.widgets.m add separator
       .xfParameters.frame2.widgets.widgets.m add command \
         -label {Paste} \
         -command {
           XFProcEditPaste [lindex [.xfParameters.frame2.widgets.widgets get \
                         [.xfParameters.frame2.widgets.widgets curselect]] 1]}
       .xfParameters.frame2.widgets.widgets.m add command \
         -label {Copy} \
         -command {
           XFProcEditCopy [lindex [.xfParameters.frame2.widgets.widgets get \
                        [.xfParameters.frame2.widgets.widgets curselect]] 1]}
       .xfParameters.frame2.widgets.widgets.m add command \
         -label {Cut} \
         -command {
           XFProcEditCut [lindex [.xfParameters.frame2.widgets.widgets get \
                       [.xfParameters.frame2.widgets.widgets curselect]] 1]}
       .xfParameters.frame2.widgets.widgets.m add command \
         -label {Delete} \
         -command {
           XFProcEditDelete [lindex [.xfParameters.frame2.widgets.widgets get \
                          [.xfParameters.frame2.widgets.widgets curselect]] 1]}

  XFParametersGetResources $xfStatus(currentConfigWidget)

  # bindings
  bind .xfParameters.frame2.widgets.widgets "<ButtonPress-$xfBind(popup)>" {
    XFBindSelectOne %W %y
    XFParametersSelectWidget %W %y
    tk_popup .xfParameters.frame2.widgets.widgets.m %X %Y}
  bind .xfParameters.frame2.widgets.widgets $xfBind(select1) {
    XFBindSelectOne %W %y
    set xfCurSelect [lindex [%W get [%W curselect]] 1]
    XFEditSetPath $xfCurSelect
    XFMiscFlash $xfCurSelect
    XFParametersSelectWidget %W %y}
  bind .xfParameters.frame2.widgets.widgets $xfBind(configure) {
    XFBindSelectOne %W %y
    set xfCurSelect [lindex [%W get [%W curselect]] 1]
    XFParametersSelectWidget %W %y
    XFMiscFlash $xfCurSelect
    XFProcConfParametersDefault $xfCurSelect}
  bind .xfParameters.frame2.widgets.widgets $xfBind(select) {
    XFBindSelectOne %W %y
    set xfCurSelect [lindex [%W get [%W curselect]] 1]
    XFEditSetPath $xfCurSelect
    XFMiscFlash $xfCurSelect
    XFParametersSelectWidget %W %y}
  bind .xfParameters.frame2.widgets.widgets $xfBind(showName) {
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
    XFParametersSelectWidget %W %y}
  bind .xfParameters.frame2.widgets.widgets <ButtonPress-1> {
    XFParametersSelectWidget %W %y}
  bind .xfParameters.frame2.widgets.widgets <Button1-Motion> {
    XFParametersSelectWidget %W %y}
  bind .xfParameters.frame2.widgets.widgets <Shift-ButtonPress-1> {
    XFParametersSelectWidget %W %y}
  bind .xfParameters.frame2.widgets.widgets <Shift-Button1-Motion> {
    XFParametersSelectWidget %W %y}

  bind .xfParameters.frame2.resources.resources <ButtonPress-1> {
    XFParametersSelectResource %W %y}
  bind .xfParameters.frame2.resources.resources <Button1-Motion> {
    XFParametersSelectResource %W %y}
  bind .xfParameters.frame2.resources.resources <Shift-ButtonPress-1> {
    XFParametersSelectResource %W %y}
  bind .xfParameters.frame2.resources.resources <Shift-Button1-Motion> {
    XFParametersSelectResource %W %y}

  bind .xfParameters.symname.symname <Return> {
    global xfStatus
    XFMiscSetSymbolicName $xfStatus(currentConfigWidget) \
      [.xfParameters.symname.symname get]
    XFMiscReadTree . .xfParameters.frame2.widgets.widgets all sym
    set xfCounter 0
    while {$xfCounter < [.xfParameters.frame2.widgets.widgets size]} {
      if {"$xfStatus(currentConfigWidget)" == "[lindex [.xfParameters.frame2.widgets.widgets get $xfCounter] 1]"} {
        .xfParameters.frame2.widgets.widgets select anchor $xfCounter
        .xfParameters.frame2.widgets.widgets select set $xfCounter
        break
      }
      incr xfCounter
    }
    XFParametersGetResources $xfStatus(currentConfigWidget)}

  # packing
  pack after .xfParameters.symname.symname \
             .xfParameters.symname.setsymname {left fillx}
  pack before .xfParameters.frame2.widgets.vscroll \
              .xfParameters.frame2.widgets.widgetsMess {top fillx}
  pack before .xfParameters.frame2.resources.vscroll \
              .xfParameters.frame2.resources.resourcesMess {top fillx}
  pack append .xfParameters.frame1 \
              .xfParameters.frame1.ok {left fill expand} \
              .xfParameters.frame1.rescan {left fill expand} \
              .xfParameters.frame1.rescanperm {left fill expand} \
              .xfParameters.frame1.cancel {left fill expand}
  pack append .xfParameters.frame3 \
              .xfParameters.frame3.apply {left fill expand} \
              .xfParameters.frame3.applytree {left fill expand} \
              .xfParameters.frame3.applyperm {left fill expand} \
              .xfParameters.frame3.undo {left fill expand}
  pack append .xfParameters.frame2 \
              .xfParameters.frame2.resources {right fill} \
              .xfParameters.frame2.widgets {left fill expand}
  pack append .xfParameters.frame5 \
              .xfParameters.frame5.color {left fill expand} \
              .xfParameters.frame5.cursor {left fill expand} \
              .xfParameters.frame5.font {left fill expand} \
              .xfParameters.frame5.pixmap {left fill expand} \
              .xfParameters.frame5.file {left fill expand}
  pack append .xfParameters.class \
              .xfParameters.class.message1 {left fill} \
              .xfParameters.class.class {left fill expand}
  pack append .xfParameters \
              .xfParameters.frame1 {bottom fill} \
              .xfParameters.frame3 {bottom fill} \
              .xfParameters.frame5 {bottom fill} \
              .xfParameters.message1 {top fillx} \
              .xfParameters.value {bottom fill} \
              .xfParameters.message2 {bottom fillx} \
              .xfParameters.resource {bottom fill} \
              .xfParameters.symname {bottom fill} \
              .xfParameters.class {bottom fill} \
              .xfParameters.frame2 {top fill expand}
}

##########
# Procedure: XFParametersGetResources
# Description: retrieve the allowed resources for the widget
# Arguments: xfW - the widget we configure
# Returns: none
# Sideeffects: none
##########
proc XFParametersGetResources {xfW} {
  global symbolicName
  global xfConf
  global xfMisc

  set xfMisc(generalParams) ""
  set xfMisc(generalParamsSave) ""
  XFMiscClearList .xfParameters.frame2.resources.resources

  .xfParameters.class.class config -text [winfo class $xfW]
  .xfParameters.symname.symname delete 0 end
  foreach xfCounter [array names symbolicName] {
    set xfArrayName ""
    append xfArrayName symbolicName ( $xfCounter )
    if {"$xfW" == "[set $xfArrayName]"} {
      .xfParameters.symname.symname insert 0 $xfCounter
    }
  }

  if {"$xfW" == "."} {
    return
  }

  set xfTmpParams [lsort [$xfW config]]
  set xfTmpIndex [lsearch $xfTmpParams *kanji*]
  if {!$xfConf(kanji) &&  $xfTmpIndex >= 0} {
      set xfTmpParams [lreplace $xfTmpParams $xfTmpIndex $xfTmpIndex]
  }
  foreach xfCounter $xfTmpParams {
    if {[llength $xfCounter] == 5} {
      # cant set the class variable because its read only
      # so don't even display
      if {[lindex $xfCounter 0] == "-class"} {
          #puts "skipping -class"
          continue
      }
      set xfTmpVal [string range [lindex $xfCounter 0] 1 end]
      lappend xfMisc(generalParams) [list $xfTmpVal [lindex $xfCounter 4]]
      lappend xfMisc(generalParamsSave) [list $xfTmpVal [lindex $xfCounter 4]]
      .xfParameters.frame2.resources.resources insert end $xfTmpVal
    }
  }
  .xfParameters.frame2.resources.resources select anchor 0
  .xfParameters.frame2.resources.resources select set 0
  XFParametersSetResource 0
}

##########
# Procedure: XFParametersSelectWidget
# Description: select the widget
# Arguments: xfW - the list
#            xfY - the y position in the list
# Returns: none
# Sideeffects: none
##########
proc XFParametersSelectWidget {xfW xfY} {
  global xfMisc
  global xfStatus 

  set xfNearest [$xfW nearest $xfY]
  if {$xfNearest >= 0} {
    if {"$xfMisc(generalParams)" != "$xfMisc(generalParamsSave)"} {
      if {[XFProcYesNo "Set changed parameters for current widget ?"]} {
        foreach xfCounter $xfMisc(generalParams) {
          XFMiscSetResource \
            $xfStatus(currentConfigWidget) [lindex $xfCounter 0] \
              [lindex $xfCounter 1]
        }
      } {
        foreach xfCounter $xfMisc(generalParamsSave) {
          XFMiscSetResource \
            $xfStatus(currentConfigWidget) [lindex $xfCounter 0] \
              [lindex $xfCounter 1]
        }
      }
    }
    $xfW select anchor $xfNearest
    $xfW select set $xfNearest
    set xfStatus(currentConfigWidget) [lindex [$xfW get $xfNearest] 1]
    .xfParameters.message1 config \
      -text "Set parameters for: $xfStatus(currentConfigWidget)"
    XFParametersGetResources $xfStatus(currentConfigWidget)
  }
}

##########
# Procedure: XFParametersSelectResource
# Description: select the resource
# Arguments: xfW - the list
#            xfY - the y position in the list
# Returns: none
# Sideeffects: none
##########
proc XFParametersSelectResource {xfW xfY} {

  set xfNearest [$xfW nearest $xfY]
  if {$xfNearest >= 0} {
    $xfW select anchor $xfNearest
    $xfW select set $xfNearest
    XFParametersSet 1
    XFParametersSetResource $xfNearest
  }
}

##########
# Procedure: XFParametersSet
# Description: set the resource values
# Arguments: xfType - force setting of parameters
# Returns: none
# Sideeffects: none
##########
proc XFParametersSet {xfType} {
  global xfConf
  global xfMisc
  global xfStatus

  set xfTmpRes [.xfParameters.resource.resource get]
  set xfTmpValue [XFMiscGetText .xfParameters.value.value]
  set xfIndex [lsearch $xfMisc(generalParams) ${xfTmpRes}*]
  if {$xfIndex != -1} {
    set xfMisc(generalParams) [lreplace $xfMisc(generalParams) $xfIndex $xfIndex [list $xfTmpRes $xfTmpValue]]
    if {$xfType == 0 || !$xfConf(applyParameters)} {
      return
    }
    XFMiscSetSymbolicName $xfStatus(currentConfigWidget) \
      [.xfParameters.symname.symname get]
    catch "$xfStatus(currentConfigWidget) conf -$xfTmpRes \"$xfTmpValue\""
  }
}

##########
# Procedure: XFParametersSetAllResources
# Description: set all resource
# Arguments: {type} - the type of parameter setting
# Returns: none
# Sideeffects: none
##########
proc XFParametersSetAllResources {{type 0}} {
  global xfConf
  global xfMisc
  global xfStatus

  set xfTmpRes [.xfParameters.resource.resource get]
  set xfTmpValue [XFMiscGetText .xfParameters.value.value]
  set xfIndex [lsearch $xfMisc(generalParams) ${xfTmpRes}*]
  XFMiscSetSymbolicName $xfStatus(currentConfigWidget) \
    [.xfParameters.symname.symname get]
  if {$type == 1} {
      XFMiscSetResourceToTree \
        $xfStatus(currentConfigWidget) $xfTmpRes [string trim $xfTmpValue]
  } {
    set xfMisc(generalParams) [lreplace $xfMisc(generalParams) $xfIndex $xfIndex [list $xfTmpRes $xfTmpValue]]
    foreach xfCounter $xfMisc(generalParams) {
      XFMiscSetResource \
        $xfStatus(currentConfigWidget) [lindex $xfCounter 0] \
          [string trim [lindex $xfCounter 1]]
    }
  }
}

##########
# Procedure: XFParametersSetResource
# Description: set the resource
# Arguments: xfIndex - the index in the list
# Returns: none
# Sideeffects: none
##########
proc XFParametersSetResource {xfIndex} {
  global xfMisc

  .xfParameters.resource.resource delete 0 end
  .xfParameters.resource.resource insert end \
    [.xfParameters.frame2.resources.resources get $xfIndex]
  .xfParameters.value.value delete 1.0 end
  .xfParameters.value.value insert 1.0 \
    [lindex [lindex $xfMisc(generalParams) $xfIndex] 1]
}

# eof

