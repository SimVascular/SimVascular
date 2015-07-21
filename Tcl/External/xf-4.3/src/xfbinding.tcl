# Program: xf
# Description: specify event bindings for widget
#
# $Header: xfbinding.tcl[2.3] Wed Mar 10 12:05:26 1993 garfield@garfield frozen $

##########
# Procedure: XFBinding
# Description: specify event bindings for widget
# Arguments: xfW - the widget we configure
#            xfType - the type of configuration (add, config)
#            xfClass - the class we currently configure
#            xfLeader - the leading window
#            xfCanvas - contains the item of the canvas
# Returns: none
# Sideeffects: none
##########
proc XFBinding {xfW xfType xfClass xfLeader {xfCanvas ""}} {
  global xfBind
  global xfConf
  global xfMisc

  set xfMisc($xfClass,saveEvent) ""
  set xfMisc($xfClass,saveBind) ""
  set xfMisc($xfClass,curEvent) ""
  set xfMisc($xfClass,curBind) ""
  XFEditSetStatus "Calling binding for $xfClass..."

  # build widget structure
  XFTmpltToplevel .xfBinding$xfClass 420x430 \
   "XF binding:[XFMiscPathTail $xfW]" $xfLeader

  XFTmpltFrame .xfBinding$xfClass.frame1 0

  button .xfBinding$xfClass.frame1.ok \
    -text {OK} \
    -command "
      XFBindingTestInsert $xfW $xfClass \"XFBindingSetBinding $xfW $xfClass $xfCanvas; destroy .xfBinding$xfClass\" $xfCanvas"

  button .xfBinding$xfClass.frame1.apply \
    -text {Apply} \
    -command "XFBindingSetBinding $xfW $xfClass $xfCanvas"

  checkbutton .xfBinding$xfClass.frame1.applyperm \
    -text {Apply permanently} \
    -variable xfConf(applyBinding) \
    -onvalue 1 \
    -offvalue 0 \
    -command "XFBindingSetBinding $xfW $xfClass $xfCanvas"

  button .xfBinding$xfClass.frame1.cancel \
    -text {Cancel} \
    -command "
      XFBindingUndoBinding $xfW $xfClass $xfCanvas
      destroy .xfBinding$xfClass"

  XFTmpltFrame .xfBinding$xfClass.additional 0

  button .xfBinding$xfClass.additional.parameters \
    -text {Small} \
    -command "XFProcConfParametersSmall $xfW .xfBinding$xfClass"

  button .xfBinding$xfClass.additional.parametersspcl \
    -text {Special} \
    -command "XFProcConfParametersSpecial $xfW .xfBinding$xfClass"

  button .xfBinding$xfClass.additional.parametersgnrl \
    -text {General} \
    -command "XFProcConfParametersGeneral $xfW .xfBinding$xfClass"

  button .xfBinding$xfClass.additional.packing \
    -text {Geometry} \
    -command "XFProcConfGeometryDefault $xfW .xfBinding$xfClass"

  if {"$xfCanvas" == ""} {
    button .xfBinding$xfClass.message1 \
      -anchor c \
      -command "XFMiscFlash $xfW" \
      -relief raised \
      -text "Binding:$xfW"
  } {
    label .xfBinding$xfClass.message1 \
      -anchor c \
      -relief raised \
      -text "Binding:$xfW Item: $xfCanvas"
  }

  label .xfBinding$xfClass.message3 \
    -anchor c \
    -relief raised \
    -text "Current Events:"

  XFTmpltFrame .xfBinding$xfClass.frame4 0

  label .xfBinding$xfClass.frame4.insertarea \
    -anchor c \
    -relief raised \
    -text "Press key to insert keysym!"

  XFTmpltFrame .xfBinding$xfClass.frame2 0

  button .xfBinding$xfClass.frame2.insert \
    -text {Insert event} \
    -command "XFBindingEnterEvent $xfW $xfClass $xfCanvas"

  button .xfBinding$xfClass.frame2.delete \
    -text {Delete event} \
    -command "XFBindingDeleteEvent $xfW $xfClass $xfCanvas"

  XFTmpltFrame .xfBinding$xfClass.frame3

  # modifier menu
  menubutton .xfBinding$xfClass.frame3.modMenu \
    -text {Modifier} \
    -underline 0 \
    -menu ".xfBinding$xfClass.frame3.modMenu.m"

  menu .xfBinding$xfClass.frame3.modMenu.m
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {Any} \
         -command "XFBindingInsertEvent Any $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {B1} \
         -command "XFBindingInsertEvent B1 $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {B2} \
         -command "XFBindingInsertEvent B2 $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {B3} \
         -command "XFBindingInsertEvent B3 $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {Control} \
         -command "XFBindingInsertEvent Control $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {Double} \
         -command "XFBindingInsertEvent Double $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {Lock} \
         -command "XFBindingInsertEvent Lock $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {M1} \
         -command "XFBindingInsertEvent M1 $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {M2} \
         -command "XFBindingInsertEvent M2 $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {M3} \
         -command "XFBindingInsertEvent M3 $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {Shift} \
         -command "XFBindingInsertEvent Shift $xfClass"
       .xfBinding$xfClass.frame3.modMenu.m add command \
         -label {Triple} \
         -command "XFBindingInsertEvent Triple $xfClass"

  # type menu
  menubutton .xfBinding$xfClass.frame3.mouseTypeMenu \
    -text {Mouse Type} \
    -underline 1 \
    -menu ".xfBinding$xfClass.frame3.mouseTypeMenu.m"

  menu .xfBinding$xfClass.frame3.mouseTypeMenu.m
       .xfBinding$xfClass.frame3.mouseTypeMenu.m add cascade \
         -label {ButtonPress} \
         -accelerator {=>} \
         -menu .xfBinding$xfClass.frame3.mouseTypeMenu.m.press
       .xfBinding$xfClass.frame3.mouseTypeMenu.m add cascade \
         -label {ButtonRelease} \
         -accelerator {=>} \
         -menu .xfBinding$xfClass.frame3.mouseTypeMenu.m.release
       .xfBinding$xfClass.frame3.mouseTypeMenu.m add command \
         -label {Motion} \
         -command "XFBindingInsertEvent Motion $xfClass"

  menu .xfBinding$xfClass.frame3.mouseTypeMenu.m.press
       .xfBinding$xfClass.frame3.mouseTypeMenu.m.press add command \
         -label {Any} \
         -command "XFBindingInsertEvent Button $xfClass"
       .xfBinding$xfClass.frame3.mouseTypeMenu.m.press add command \
         -label {Button 1} \
         -command "XFBindingInsertEvent Button-1 $xfClass"
       .xfBinding$xfClass.frame3.mouseTypeMenu.m.press add command \
         -label {Button 2} \
         -command "XFBindingInsertEvent Button-2 $xfClass"
       .xfBinding$xfClass.frame3.mouseTypeMenu.m.press add command \
         -label {Button 3} \
         -command "XFBindingInsertEvent Button-3 $xfClass"

  menu .xfBinding$xfClass.frame3.mouseTypeMenu.m.release
       .xfBinding$xfClass.frame3.mouseTypeMenu.m.release add command \
         -label {Any} \
         -command "XFBindingInsertEvent ButtonRelease $xfClass"
       .xfBinding$xfClass.frame3.mouseTypeMenu.m.release add command \
         -label {Button 1} \
         -command "XFBindingInsertEvent ButtonRelease-1 $xfClass"
       .xfBinding$xfClass.frame3.mouseTypeMenu.m.release add command \
         -label {Button 2} \
         -command "XFBindingInsertEvent ButtonRelease-2 $xfClass"
       .xfBinding$xfClass.frame3.mouseTypeMenu.m.release add command \
         -label {Button 3} \
         -command "XFBindingInsertEvent ButtonRelease-3 $xfClass"

  # type menu
  menubutton .xfBinding$xfClass.frame3.keyTypeMenu \
    -text {Key Type} \
    -underline 0 \
    -menu ".xfBinding$xfClass.frame3.keyTypeMenu.m"

  menu .xfBinding$xfClass.frame3.keyTypeMenu.m
       .xfBinding$xfClass.frame3.keyTypeMenu.m add command \
         -label {KeyMap} \
         -command "XFBindingInsertEvent KeyMap $xfClass"
       .xfBinding$xfClass.frame3.keyTypeMenu.m add command \
         -label {KeyPress} \
         -command "XFBindingInsertEvent KeyPress $xfClass"
       .xfBinding$xfClass.frame3.keyTypeMenu.m add command \
         -label {KeyRelease} \
         -command "XFBindingInsertEvent KeyRelease $xfClass"

  # type menu
  menubutton .xfBinding$xfClass.frame3.windowTypeMenu \
    -text {Window Type} \
    -underline 0 \
    -menu ".xfBinding$xfClass.frame3.windowTypeMenu.m"

  menu .xfBinding$xfClass.frame3.windowTypeMenu.m
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Circulate} \
         -command "XFBindingInsertEvent Circulate $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {CirculateRequest} \
         -command "XFBindingInsertEvent CirculateRequest $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Colormap} \
         -command "XFBindingInsertEvent Colormap $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Configure} \
         -command "XFBindingInsertEvent Configure $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {ConfigureRequest} \
         -command "XFBindingInsertEvent ConfigureRequest $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Destroy} \
         -command "XFBindingInsertEvent Destroy $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Enter} \
         -command "XFBindingInsertEvent Enter $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Expose} \
         -command "XFBindingInsertEvent Expose $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {FocusIn} \
         -command "XFBindingInsertEvent FocusIn $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {FocusOut} \
         -command "XFBindingInsertEvent FocusOut $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Gravity} \
         -command "XFBindingInsertEvent Gravity $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Leave} \
         -command "XFBindingInsertEvent Leave $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Map} \
         -command "XFBindingInsertEvent Map $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {MapRequest} \
         -command "XFBindingInsertEvent MapRequest $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Property} \
         -command "XFBindingInsertEvent Property $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Reparent} \
         -command "XFBindingInsertEvent Reparent $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {ResizeRequest} \
         -command "XFBindingInsertEvent ResizeRequest $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Unmap} \
         -command "XFBindingInsertEvent Unmap $xfClass"
       .xfBinding$xfClass.frame3.windowTypeMenu.m add command \
         -label {Visibility} \
         -command "XFBindingInsertEvent Visibility $xfClass"

  XFTmpltListbox .xfBinding$xfClass events
  .xfBinding$xfClass.events.events configure \
    -width 7 -height 7

  XFTmpltLabledEntry .xfBinding$xfClass current "Current event:"

  label .xfBinding$xfClass.message2 \
    -anchor c \
    -relief raised \
    -text "The actions of the current event are:"

  XFTmpltText .xfBinding$xfClass value

  if {"$xfCanvas" != ""} {
    .xfBinding$xfClass.additional.parameters configure \
      -state disabled

    .xfBinding$xfClass.additional.parametersspcl configure \
      -state disabled

    .xfBinding$xfClass.additional.parametersgnrl configure \
      -state disabled

    .xfBinding$xfClass.additional.packing configure \
      -state disabled
  }

  # bindings
  bind .xfBinding$xfClass.value.value $xfBind(configure) "
    XFInfoCommands .xfBinding$xfClass.value.value"

  bind .xfBinding$xfClass.current.current $xfBind(configure) \
    "XFProcKeysymBox Keysym .xfBinding$xfClass.current.current"

  bind .xfBinding$xfClass.current.current <Return> "
    XFBindingEnterEvent $xfW $xfClass $xfCanvas"

  bind .xfBinding$xfClass.frame4.insertarea <Enter> "
    global xfMisc
    set xfMisc(savedBindFocus) \[focus\]
    focus .xfBinding$xfClass.frame4.insertarea"
  bind .xfBinding$xfClass.frame4.insertarea <Leave> "
    global xfMisc
    focus \$xfMisc(savedBindFocus)
# 03/16/98 - Dennis LaBelle - added next line to ensure toplevel window stays on top after mouse leaves widget
    raise .xfBinding$xfClass"
  bind .xfBinding$xfClass.frame4.insertarea <Any-KeyPress> "
    XFBindingInsertEvent \"Key-%K\" $xfClass"

  bind .xfBinding$xfClass.events.events <ButtonPress-1> "
    XFBindingSelectEvent $xfW $xfClass %W %y $xfCanvas"
  bind .xfBinding$xfClass.events.events <Button1-Motion> "
    XFBindingSelectEvent $xfW $xfClass %W %y $xfCanvas"
  bind .xfBinding$xfClass.events.events <Shift-ButtonPress-1> "
    XFBindingSelectEvent $xfW $xfClass %W %y $xfCanvas"
  bind .xfBinding$xfClass.events.events <Shift-Button1-Motion> "
    XFBindingSelectEvent $xfW $xfClass %W %y $xfCanvas"

  if {"$xfCanvas" == ""} {
    set xfBindList [lsort [bind $xfW]]
    foreach xfCounter $xfBindList {
      if {[XFMiscCorrectLevel bindshow [bind $xfW $xfCounter]]} {
        .xfBinding$xfClass.events.events insert end $xfCounter
      }
      lappend xfMisc($xfClass,curEvent) $xfCounter
      lappend xfMisc($xfClass,curBind) [bind $xfW $xfCounter]
      lappend xfMisc($xfClass,saveEvent) $xfCounter
      lappend xfMisc($xfClass,saveBind) [bind $xfW $xfCounter]
    }
  } {
    set xfBindList [lsort [$xfW bind $xfCanvas]]
    foreach xfCounter $xfBindList {
      if {[XFMiscCorrectLevel bindshow [$xfW bind $xfCanvas $xfCounter]]} {
        .xfBinding$xfClass.events.events insert end $xfCounter
      }
      lappend xfMisc($xfClass,curEvent) $xfCounter
      lappend xfMisc($xfClass,curBind) [$xfW bind $xfCanvas $xfCounter]
      lappend xfMisc($xfClass,saveEvent) $xfCounter
      lappend xfMisc($xfClass,saveBind) [$xfW bind $xfCanvas $xfCounter]
    }
  }

  # packing
  pack append .xfBinding$xfClass.frame1 \
              .xfBinding$xfClass.frame1.ok {left fill expand} \
              .xfBinding$xfClass.frame1.apply {left fill expand} \
              .xfBinding$xfClass.frame1.applyperm {left fill expand} \
              .xfBinding$xfClass.frame1.cancel {left fill expand}
  pack append .xfBinding$xfClass.additional \
              .xfBinding$xfClass.additional.parameters {left fill expand} \
              .xfBinding$xfClass.additional.parametersspcl {left fill expand} \
              .xfBinding$xfClass.additional.parametersgnrl {left fill expand} \
              .xfBinding$xfClass.additional.packing {left fill expand}
  pack append .xfBinding$xfClass.frame2 \
              .xfBinding$xfClass.frame2.insert {left fill expand} \
              .xfBinding$xfClass.frame2.delete {left fill expand}
  pack append .xfBinding$xfClass.frame3 \
              .xfBinding$xfClass.frame3.modMenu {left fill} \
              .xfBinding$xfClass.frame3.mouseTypeMenu {left fill} \
              .xfBinding$xfClass.frame3.keyTypeMenu {left fill} \
              .xfBinding$xfClass.frame3.windowTypeMenu {left fill}
  pack append .xfBinding$xfClass.frame4 \
              .xfBinding$xfClass.frame4.insertarea {left fill expand}
  pack append .xfBinding$xfClass \
              .xfBinding$xfClass.frame1 {bottom fill} \
              .xfBinding$xfClass.additional {bottom fill} \
              .xfBinding$xfClass.frame3 {top fill} \
              .xfBinding$xfClass.message1 {top fill} \
              .xfBinding$xfClass.message3 {top fill} \
              .xfBinding$xfClass.events {top fill} \
              .xfBinding$xfClass.current {top frame center fillx} \
              .xfBinding$xfClass.frame4 {top fillx} \
              .xfBinding$xfClass.frame2 {top fillx} \
              .xfBinding$xfClass.message2 {top fill} \
              .xfBinding$xfClass.value {top fill expand}
  XFEditSetStatus "Calling binding for $xfClass...done"
}

##########
# Procedure: XFBindingDeleteEvent
# Description: delete new event in list
# Arguments: xfW - the widget we configure
#            xfClass - the class we edit
#            xfCanvas - contains the item of the canvas 
# Returns: none
# Sideeffects: none
##########
proc XFBindingDeleteEvent {xfW xfClass {xfCanvas ""}} {
  global xfConf
  global xfMisc

  set xfCurrent [.xfBinding$xfClass.current.current get]
  set xfCounter 0
  while {$xfCounter < [llength $xfMisc($xfClass,curEvent)]} {
    if {"$xfCurrent" == "[lindex $xfMisc($xfClass,curEvent) $xfCounter]"} {
      if {[XFMiscCorrectLevel bindshow [lindex $xfMisc($xfClass,curBind) $xfCounter]]} {
        set xfMisc($xfClass,curEvent) \
          [lreplace $xfMisc($xfClass,curEvent) $xfCounter $xfCounter]
        set xfMisc($xfClass,curBind) \
          [lreplace $xfMisc($xfClass,curBind) $xfCounter $xfCounter]
      } {
        XFProcError "Hidden bindings cannot be removed!"
      }
    }
    incr xfCounter 1
  }
  set xfCounter 0
  while {$xfCounter < [.xfBinding$xfClass.events.events size]} {
    if {"$xfCurrent" == "[.xfBinding$xfClass.events.events get $xfCounter]"} {
      .xfBinding$xfClass.events.events delete $xfCounter
    }
    incr xfCounter 1
  }

  XFMiscSetText .xfBinding$xfClass.value.value ""
  
  if {$xfConf(applyBinding)} {
    XFBindingSetBinding $xfW $xfClass $xfCanvas
  }
}

##########
# Procedure: XFBindingEnterEvent
# Description: insert new event in list
# Arguments: xfW - the widget we configure
#            xfClass - the class we edit
#            xfCanvas - contains the item of the canvas 
# Returns: none
# Sideeffects: none
##########
proc XFBindingEnterEvent {xfW xfClass {xfCanvas ""}} {
  global xfConf
  global xfMisc

  set xfInserted 0
  set xfCurrent [.xfBinding$xfClass.current.current get]
  set xfCounter 0
  if {"$xfCurrent" != ""} {
    while {$xfCounter < [llength $xfMisc($xfClass,curEvent)]} {
      if {"$xfCurrent" == "[lindex $xfMisc($xfClass,curEvent) $xfCounter]"} {
        if {[XFMiscCorrectLevel bindshow [lindex $xfMisc($xfClass,curBind) $xfCounter]]} {
          set xfMisc($xfClass,curBind) \
            [lreplace $xfMisc($xfClass,curBind) $xfCounter $xfCounter \
              [XFMiscGetText .xfBinding$xfClass.value.value]]
        } {
          XFProcError "Hidden bindings cannot be changed!"
        }
        set xfInserted 1
      }
      incr xfCounter 1
    }
    if {!$xfInserted} {
      if {[XFMiscCorrectLevel bindshow [XFMiscGetText .xfBinding$xfClass.value.value]]} {
        .xfBinding$xfClass.events.events insert end \
          [.xfBinding$xfClass.current.current get]
      }
      lappend xfMisc($xfClass,curEvent) \
        [.xfBinding$xfClass.current.current get]
      lappend xfMisc($xfClass,curBind) \
        [XFMiscGetText .xfBinding$xfClass.value.value]
    }

    if {$xfConf(applyBinding)} {
      XFBindingSetBinding $xfW $xfClass $xfCanvas
    }
  }
}

##########
# Procedure: XFBindingInsertEvent
# Description: insert event into event string
# Arguments: xfEvent - the event to insert
#            xfClass - the class we edit
# Returns: none
# Sideeffects: none
##########
proc XFBindingInsertEvent {xfEvent xfClass} {

  set xfCurrentEvent [.xfBinding$xfClass.current.current get]
  set xfCurrentEvent [string trimleft $xfCurrentEvent "<"] 
  set xfCurrentEvent [string trimright $xfCurrentEvent ">"]
  .xfBinding$xfClass.current.current delete 0 end
  if {"$xfCurrentEvent" == ""} {
    .xfBinding$xfClass.current.current insert 0 "<$xfEvent>"
  } {
    .xfBinding$xfClass.current.current insert 0 "<$xfCurrentEvent-$xfEvent>"
  }
}

##########
# Procedure: XFBindingSelectEvent
# Description: select the current event
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
#            xfCanvas - contains the item of the canvas 
# Returns: none
# Sideeffects: none
##########
proc XFBindingSelectEvent {xfW xfClass xfList xfY {xfCanvas ""}} {
  global xfMisc

  XFBindingTestInsert $xfW $xfClass {} $xfCanvas
  XFBindSelectOneIntoEntry $xfList $xfY .xfBinding$xfClass.current.current
  set xfIndex [lsearch $xfMisc($xfClass,curEvent) \
    [.xfBinding$xfClass.events.events get [$xfList nearest $xfY]]]
  if {$xfIndex != -1} {
    XFMiscSetText .xfBinding$xfClass.value.value \
      [lindex $xfMisc($xfClass,curBind) $xfIndex]
  }
}

##########
# Procedure: XFBindingSetBinding
# Description: set binding for currently selected widget
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
#            xfCanvas - contains the item of the canvas 
# Returns: none
# Sideeffects: none
##########
proc XFBindingSetBinding {xfW xfClass {xfCanvas ""}} {
  global xfConf
  global xfMisc

  if {"$xfCanvas" == ""} {
    set xfBindList [bind $xfW]
    foreach xfCounter $xfBindList {
      catch "bind $xfW $xfCounter \"\""
    }
  } {
    set xfBindList [$xfW bind $xfCanvas]
    foreach xfCounter $xfBindList {
      catch "$xfW bind $xfCanvas $xfCounter \"\""
    }
  }

  set xfCounter 0
  while {$xfCounter < [llength $xfMisc($xfClass,curEvent)]} {
    if {"$xfCanvas" == ""} {
      if {$xfConf(encloseBinding)} {
        if {[catch "bind $xfW [lindex $xfMisc($xfClass,curEvent) $xfCounter] {[string trim [lindex $xfMisc($xfClass,curBind) $xfCounter]]}" xfResult]} {
          XFProcError "$xfResult"
        }
      } {
        if {[catch "bind $xfW [lindex $xfMisc($xfClass,curEvent) $xfCounter] \"[string trim [lindex $xfMisc($xfClass,curBind) $xfCounter]]\"" xfResult]} {
          XFProcError "$xfResult"
        }
      }
    } {
      if {$xfConf(encloseBinding)} {
        if {[catch "$xfW bind $xfCanvas [lindex $xfMisc($xfClass,curEvent) $xfCounter] {[string trim [lindex $xfMisc($xfClass,curBind) $xfCounter]]}" xfResult]} {
          XFProcError "$xfResult"
        }
      } {
        if {[catch "$xfW bind $xfCanvas [lindex $xfMisc($xfClass,curEvent) $xfCounter] \"[string trim [lindex $xfMisc($xfClass,curBind) $xfCounter]]\"" xfResult]} {
          XFProcError "$xfResult"
        }
      }
    }
    incr xfCounter 1
  }
  update
}

##########
# Procedure: XFBindingTestInsert
# Description: check if there are unsaved changes
# Arguments: xfW - the widget we configure
#            xfClass - the class
#            xfCommand - optional command string
#            xfCanvas - contains the item of the canvas 
# Returns: none
# Sideeffects: none
##########
proc XFBindingTestInsert {xfW xfClass {xfCommand ""} {xfCanvas ""}} {
  global xfMisc

  set xfCurrent [.xfBinding$xfClass.current.current get]
  set xfCounter 0
  set xfFound 0
  while {$xfCounter < [llength $xfMisc($xfClass,curEvent)]} {
    if {"$xfCurrent" == "[lindex $xfMisc($xfClass,curEvent) $xfCounter]"} {
      set xfFound 1
      if {"[XFMiscGetText .xfBinding$xfClass.value.value]" != "[lindex $xfMisc($xfClass,curBind) $xfCounter]"} {
        set xfTmpParam [XFMiscGetText .xfBinding$xfClass.value.value]
        if {[XFProcYesNo "Insert the currently modified event ?"]} {
          XFBindingEnterEvent $xfW $xfClass $xfCanvas
        }
        eval $xfCommand
        return
      }
    }
    incr xfCounter 1
  }
  if {$xfFound == 0} {
    if {"[XFMiscGetText .xfBinding$xfClass.value.value]" != "" &&
        "$xfCurrent" != ""} {
      set xfTmpParam [XFMiscGetText .xfBinding$xfClass.value.value]
      if {[XFProcYesNo "Insert the currently edited event ?"]} {
        XFBindingEnterEvent $xfW $xfClass $xfCanvas
      }
      eval $xfCommand
      return
    }
  }
  eval $xfCommand
}

##########
# Procedure: XFBindingUndoBinding
# Description: undo binding for widgets
# Arguments: xfW - the widget we configure
#            xfClass - the class we currently configure
#            xfCanvas - contains the item of the canvas 
# Returns: none
# Sideeffects: none
##########
proc XFBindingUndoBinding {xfW xfClass {xfCanvas ""}} {
  global xfConf
  global xfMisc

  if {"$xfCanvas" == ""} {
    set xfBindList [bind $xfW]
    foreach xfCounter $xfBindList {
      catch "bind $xfW $xfCounter \"\""
    }
  } {
    set xfBindList [$xfW bind $xfCanvas]
    foreach xfCounter $xfBindList {
      catch "$xfW bind $xfCanvas $xfCounter \"\""
    }
  }

  set xfCounter 0
  while {$xfCounter < [llength $xfMisc($xfClass,saveEvent)]} {
    if {"$xfCanvas" == ""} {
      if {$xfConf(encloseBinding)} {
        if {[catch "bind $xfW [lindex $xfMisc($xfClass,saveEvent) $xfCounter] {[string trim [lindex $xfMisc($xfClass,saveBind) $xfCounter]]}" xfResult]} {
          XFProcError "$xfResult"
        }
      } {
        if {[catch "bind $xfW [lindex $xfMisc($xfClass,saveEvent) $xfCounter] \"[string trim [lindex $xfMisc($xfClass,saveBind) $xfCounter]]\"" xfResult]} {
          XFProcError "$xfResult"
        }
      }
    } {
      if {$xfConf(encloseBinding)} {
        if {[catch "$xfW bind $xfCanvas [lindex $xfMisc($xfClass,saveEvent) $xfCounter] {[string trim [lindex $xfMisc($xfClass,saveBind) $xfCounter]]}" xfResult]} {
          XFProcError "$xfResult"
        }
      } {
        if {[catch "$xfW bind $xfCanvas [lindex $xfMisc($xfClass,saveEvent) $xfCounter] \"[string trim [lindex $xfMisc($xfClass,saveBind) $xfCounter]]\"" xfResult]} {
          XFProcError "$xfResult"
        }
      }
    }
    incr xfCounter 1
  }
}

# eof

