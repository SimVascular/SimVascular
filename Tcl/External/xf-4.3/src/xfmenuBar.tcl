# XFNoParsing
# Program: template
# Description: configure menus
#
# $Header: xfmenuBar.tcl[2.3] Wed Mar 10 12:06:51 1993 garfield@garfield frozen $

global xfMenuBar
set xfMenuBar(activeBackground) ""
set xfMenuBar(activeForeground) ""
set xfMenuBar(background) ""
set xfMenuBar(font) ""
set xfMenuBar(foreground) ""
set xfMenuBar(scrollActiveForeground) ""
set xfMenuBar(scrollBackground) ""
set xfMenuBar(scrollForeground) ""
set xfMenuBar(menuType) "<command> "
set xfMenuBar(state) normal

proc XFMenuBarInit {xfMenuBarUserFile xfMenuBarFile} {# xf ignore me 5
##########
# Procedure: XFMenuBarInit
# Description: initialize the configuration of menubuttons and menus
# Arguments: xfMenuBarUserFile - the user specific loadfile
#            xfMenuBarFile - the default loadfile
# Returns: none
# Sideeffects: none
##########

  global xfMenuBar

  set xfMenuBar(initialized) 1
  set xfMenuBar(file) $xfMenuBarFile
  set xfMenuBar(userFile) $xfMenuBarUserFile
  if {[file exists $xfMenuBar(userFile)]} {
    if {[catch "source \"$xfMenuBar(userFile)\"" xfMenuBarResult]} {
      puts stderr $xfMenuBarResult
    }
  } {
    if {[file exists $xfMenuBar(file)]} {
      if {[catch "source \"$xfMenuBar(file)\"" xfMenuBarResult]} {
        puts stderr $xfMenuBarResult
      }
    }
  }
}

proc XFMenuBarConf {xfMenuBarConfig} {# xf ignore me 5
##########
# Procedure: XFMenuBarConf
# Description: configure the menubuttons and menus of
#              the given pathname
# Arguments: xfMenuBarConfig - the widget pathname we configure
# Returns: none
# Sideeffects: none
##########
# 
# global xfMenuBar(activeBackground) - active background color
# global xfMenuBar(activeForeground) - active foreground color
# global xfMenuBar(background) - background color
# global xfMenuBar(font) - text font
# global xfMenuBar(foreground) - foreground color
# global xfMenuBar(scrollActiveForeground) - scrollbar active background color
# global xfMenuBar(scrollBackground) - scrollbar background color
# global xfMenuBar(scrollForeground) - scrollbar foreground color
# global xfMenuBar(scrollSide) - side where scrollbar is located

  global xfMenuBar

  if {![info exists xfMenuBar(initialized)]} {
    return
  }
  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$xfMenuBar(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfMenuBar(activeBackground)\" "
  }
  if {"$xfMenuBar(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfMenuBar(activeForeground)\" "
  }
  if {"$xfMenuBar(background)" != ""} {
    append tmpButtonOpt "-background \"$xfMenuBar(background)\" "
    append tmpFrameOpt "-background \"$xfMenuBar(background)\" "
    append tmpMessageOpt "-background \"$xfMenuBar(background)\" "
    append tmpScaleOpt "-background \"$xfMenuBar(background)\" "
  }
  if {"$xfMenuBar(font)" != ""} {
    append tmpButtonOpt "-font \"$xfMenuBar(font)\" "
    append tmpMessageOpt "-font \"$xfMenuBar(font)\" "
  }
  if {"$xfMenuBar(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfMenuBar(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfMenuBar(foreground)\" "
    append tmpScaleOpt "-foreground \"$xfMenuBar(foreground)\" "
  }
  if {"$xfMenuBar(scrollActiveForeground)" != ""} {
    append tmpScaleOpt "-activeforeground \"$xfMenuBar(scrollActiveForeground)\" "
    append tmpScrollOpt "-activeforeground \"$xfMenuBar(scrollActiveForeground)\" "
  }
  if {"$xfMenuBar(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$xfMenuBar(scrollBackground)\" "
  }
  if {"$xfMenuBar(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$xfMenuBar(scrollForeground)\" "
  }

  XFTmpltToplevel .xfMenuBar 530x425 "XF menubar configuration"

  frame .xfMenuBar.frame1 \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1 config $tmpFrameOpt"
 
  frame .xfMenuBar.frame1.frame2 \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2 config $tmpFrameOpt"
 
  frame .xfMenuBar.frame1.frame2.frame4 \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame4 config $tmpFrameOpt"
 
  frame .xfMenuBar.frame1.frame2.frame5 \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame5 config $tmpFrameOpt"
 
  frame .xfMenuBar.frame1.frame2.frame6 \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame6 config $tmpFrameOpt"
 
  frame .xfMenuBar.frame1.frame2.frame8 \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame8 config $tmpFrameOpt"
 
  frame .xfMenuBar.frame1.frame7 \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame7 config $tmpFrameOpt"
 
  frame .xfMenuBar.frame1.frame3 \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame3 config $tmpFrameOpt"
 
  scrollbar .xfMenuBar.frame1.frame2.frame5.vscroll \
    -highlightthickness 0 \
    -relief raised \
    -command ".xfMenuBar.frame1.frame2.frame5.buttons yview"
  catch ".xfMenuBar.frame1.frame2.frame5.vscroll config $tmpScrollOpt"

  scrollbar .xfMenuBar.frame1.frame2.frame5.hscroll \
    -highlightthickness 0 \
    -orient horiz \
    -relief raised \
    -command ".xfMenuBar.frame1.frame2.frame5.buttons xview"
  catch ".xfMenuBar.frame1.frame2.frame5.hscroll config $tmpScrollOpt"

  listbox .xfMenuBar.frame1.frame2.frame5.buttons \
    -highlightthickness 0 \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".xfMenuBar.frame1.frame2.frame5.hscroll set" \
    -yscrollcommand ".xfMenuBar.frame1.frame2.frame5.vscroll set"
  catch ".xfMenuBar.frame1.frame2.frame5.buttons config $tmpMessageOpt"

  scrollbar .xfMenuBar.frame1.frame2.frame6.vscroll \
    -highlightthickness 0 \
    -relief raised \
    -command ".xfMenuBar.frame1.frame2.frame6.menus yview"
  catch ".xfMenuBar.frame1.frame2.frame6.vscroll config $tmpScrollOpt"

  scrollbar .xfMenuBar.frame1.frame2.frame6.hscroll \
    -highlightthickness 0 \
    -orient horiz \
    -relief raised \
    -command ".xfMenuBar.frame1.frame2.frame6.menus xview"
  catch ".xfMenuBar.frame1.frame2.frame6.hscroll config $tmpScrollOpt"

  listbox .xfMenuBar.frame1.frame2.frame6.menus \
    -highlightthickness 0 \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".xfMenuBar.frame1.frame2.frame6.hscroll set" \
    -yscrollcommand ".xfMenuBar.frame1.frame2.frame6.vscroll set"
  catch ".xfMenuBar.frame1.frame2.frame6.menus config $tmpMessageOpt"

  scrollbar .xfMenuBar.frame1.frame2.frame8.vscroll \
    -highlightthickness 0 \
    -relief raised \
    -command ".xfMenuBar.frame1.frame2.frame8.menu yview"
  catch ".xfMenuBar.frame1.frame2.frame8.vscroll config $tmpScrollOpt"

  scrollbar .xfMenuBar.frame1.frame2.frame8.hscroll \
    -highlightthickness 0 \
    -orient horiz \
    -relief raised \
    -command ".xfMenuBar.frame1.frame2.frame8.menu xview"
  catch ".xfMenuBar.frame1.frame2.frame8.hscroll config $tmpScrollOpt"

  listbox .xfMenuBar.frame1.frame2.frame8.menu \
    -highlightthickness 0 \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".xfMenuBar.frame1.frame2.frame8.hscroll set" \
    -yscrollcommand ".xfMenuBar.frame1.frame2.frame8.vscroll set"
  catch ".xfMenuBar.frame1.frame2.frame8.menu config $tmpMessageOpt"

  scale .xfMenuBar.frame1.frame2.frame8.mover \
    -highlightthickness 0 \
    -orient vertical \
    -width 8 \
    -relief raised \
    -sliderlength 15 \
    -from 0 \
    -command "XFMenuBarReposition"
  catch ".xfMenuBar.frame1.frame2.frame8.mover config $tmpScaleOpt"

  button .xfMenuBar.frame1.frame7.insert \
    -text "Append" \
    -command "XFMenuBarInsert \"$xfMenuBarConfig\""
  catch ".xfMenuBar.frame1.frame7.insert config $tmpButtonOpt"

  button .xfMenuBar.frame1.frame7.modify \
    -text "Modify" \
    -command "XFMenuBarModify \"$xfMenuBarConfig\""
  catch ".xfMenuBar.frame1.frame7.modify config $tmpButtonOpt"

  button .xfMenuBar.frame1.frame7.modifymenu \
    -text "Modify menu" \
    -command {
      set curSelected [.xfMenuBar.frame1.frame2.frame6.menus curselection]
      if {$curSelected >= 0} {
        place forget .xfMenuBar.frame1.frame2.frame5
        place forget .xfMenuBar.frame1.frame2.frame6
        place .xfMenuBar.frame1.frame2.frame8 \
          -in .xfMenuBar.frame1.frame2 \
          -relx 0.5 \
          -rely 0 \
          -relheight 1.0 \
          -relwidth 0.5
        pack unpack .xfMenuBar.frame1.frame7.modifymenu
        pack append .xfMenuBar.frame1.frame7 \
                    .xfMenuBar.frame1.frame7.insert {left fill expand} \
                    .xfMenuBar.frame1.frame7.modify {left fill expand} \
                    .xfMenuBar.frame1.frame7.modifyback {left fill expand} \
                    .xfMenuBar.frame1.frame7.delete {left fill expand}
        update idletask
        XFMenuBarSetItems command
        XFMenuBarReadMenu \
          [.xfMenuBar.frame1.frame2.frame6.menus get $curSelected]
        case [lindex [.xfMenuBar.frame1.frame2.frame4.items.message1 config -text] 4] in {
          {Cascadebutton} {
            .xfMenuBar.frame1.frame2.frame4.items.items.m invoke 0
          }
          {Checkbutton} {
            .xfMenuBar.frame1.frame2.frame4.items.items.m invoke 1
          }
          {Radiobutton} {
            .xfMenuBar.frame1.frame2.frame4.items.items.m invoke 3
          }
          {Separator} {
            .xfMenuBar.frame1.frame2.frame4.items.items.m invoke 4
          }
          {default} {
            .xfMenuBar.frame1.frame2.frame4.items.items.m invoke 2
          }
        }
        XFMenuBarSetItem
      }}
  catch ".xfMenuBar.frame1.frame7.modifymenu config $tmpButtonOpt"

  button .xfMenuBar.frame1.frame7.modifyback \
    -text "Back to main" \
    -command {
      place forget .xfMenuBar.frame1.frame2.frame8
      if {[.xfMenuBar.frame1.frame2.frame5.buttons size] > 0} {
        place .xfMenuBar.frame1.frame2.frame5 \
          -in .xfMenuBar.frame1.frame2 \
          -relx 0.5 \
          -rely 0 \
          -relheight 0.5 \
          -relwidth 0.5
        place .xfMenuBar.frame1.frame2.frame6 \
          -in .xfMenuBar.frame1.frame2 \
          -relx 0.5 \
          -rely 0.5 \
          -relheight 0.5 \
          -relwidth 0.5
      } {
        place .xfMenuBar.frame1.frame2.frame6 \
          -in .xfMenuBar.frame1.frame2 \
          -relx 0.5 \
          -rely 0 \
          -relheight 1 \
          -relwidth 0.5
      }
      update idletask
      pack unpack .xfMenuBar.frame1.frame7.modifyback
      pack append .xfMenuBar.frame1.frame7 \
                  .xfMenuBar.frame1.frame7.insert {left fill expand} \
                  .xfMenuBar.frame1.frame7.modify {left fill expand} \
                  .xfMenuBar.frame1.frame7.modifymenu {left fill expand} \
                  .xfMenuBar.frame1.frame7.delete {left fill expand}
      XFMenuBarSetItem}
  catch ".xfMenuBar.frame1.frame7.modifyback config $tmpButtonOpt"

  button .xfMenuBar.frame1.frame7.delete \
    -text "Remove" \
    -command "
      if {\[.xfMenuBar.frame1.frame2.frame6.menus size\] > 0} {
        XFMenuBarDelete \"$xfMenuBarConfig\"
      }"
  catch ".xfMenuBar.frame1.frame7.delete config $tmpButtonOpt"

  button .xfMenuBar.frame1.frame3.ok \
    -text "OK" \
    -command "
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfMenuBar}
      } {
        catch {destroy .xfMenuBar}
      }"
  catch ".xfMenuBar.frame1.frame3.ok config $tmpButtonOpt"

  button .xfMenuBar.frame1.frame3.save \
    -text "Save" \
    -command "XFMenuBarSave \"$xfMenuBarConfig\""
  catch ".xfMenuBar.frame1.frame3.save config $tmpButtonOpt"

  frame .xfMenuBar.frame1.frame2.frame4.items \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame4.items config $tmpFrameOpt"
 
  menubutton .xfMenuBar.frame1.frame2.frame4.items.items \
    -text "Menu entry:" \
    -menu ".xfMenuBar.frame1.frame2.frame4.items.items.m"
  catch ".xfMenuBar.frame1.frame2.frame4.items.items config $tmpButtonOpt"

  menu .xfMenuBar.frame1.frame2.frame4.items.items.m
  catch ".xfMenuBar.frame1.frame2.frame4.items.items.m config $tmpButtonOpt"

  .xfMenuBar.frame1.frame2.frame4.items.items.m \
    add radiobutton \
      -label "Cascadebutton" \
      -variable xfMenuBar(menuType) \
      -value "<cascade> " \
      -command "XFMenuBarSetItems cascade"
  .xfMenuBar.frame1.frame2.frame4.items.items.m \
    add radiobutton \
      -label "Checkbutton" \
      -variable xfMenuBar(menuType) \
      -value "<checkbutton> " \
      -command "XFMenuBarSetItems check"
  .xfMenuBar.frame1.frame2.frame4.items.items.m \
    add radiobutton \
      -label "Commandbutton" \
      -variable xfMenuBar(menuType) \
      -value "<command> " \
      -command "XFMenuBarSetItems command"
  .xfMenuBar.frame1.frame2.frame4.items.items.m \
    add radiobutton \
      -label "Radiobutton" \
      -variable xfMenuBar(menuType) \
      -value "<radiobutton> " \
      -command "XFMenuBarSetItems radio"
  .xfMenuBar.frame1.frame2.frame4.items.items.m \
    add radiobutton \
      -label "Separator" \
      -variable xfMenuBar(menuType) \
      -value "<separator> " \
      -command "XFMenuBarSetItems separator"
  
  label .xfMenuBar.frame1.frame2.frame4.items.message1 \
    -anchor w \
    -text "Commandbutton"
  catch ".xfMenuBar.frame1.frame2.frame4.items.message1 config $tmpMessageOpt"
  
  XFMenuBarEntry accelerator "Accelerator:"
  XFMenuBarEntry actbg "Active background:"
  XFMenuBarScale actborder "Active borderwidth:" "pixels" 40
  XFMenuBarEntry actfg "Active foreground:"
  XFMenuBarEntry bg "Background:"
  XFMenuBarEntry bitmap "Bitmap:"
  XFMenuBarScale border "Borderwidth:" "pixels" 40
  XFMenuBarEntry font "Font:"
  XFMenuBarEntry fg "Foreground:"
  XFMenuBarEntry label "Label:"
  XFMenuBarEntry menu "Menu:"
  XFMenuBarEntry name "Menu name:"
  XFMenuBarEntry offvalue "Offvalue:"
  XFMenuBarEntry onvalue "Onvalue:"
  XFMenuBarScaleDouble size "Size:" "Width" "Height" 300 300

  frame .xfMenuBar.frame1.frame2.frame4.state \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame4.state config $tmpFrameOpt"
 
  label .xfMenuBar.frame1.frame2.frame4.state.message1 \
    -anchor w \
    -relief raised \
    -text "State:"
  catch ".xfMenuBar.frame1.frame2.frame4.state.message1 config $tmpMessageOpt"

  radiobutton .xfMenuBar.frame1.frame2.frame4.state.normal \
    -value normal \
    -text "Normal" \
    -variable xfMenuBar(state)
  catch ".xfMenuBar.frame1.frame2.frame4.state.normal config $tmpButtonOpt"

  radiobutton .xfMenuBar.frame1.frame2.frame4.state.active \
    -value active \
    -text "Active" \
    -variable xfMenuBar(state)
  catch ".xfMenuBar.frame1.frame2.frame4.state.active config $tmpButtonOpt"

  radiobutton .xfMenuBar.frame1.frame2.frame4.state.disabled \
    -value disabled \
    -text "Disabled" \
    -variable xfMenuBar(state)
  catch ".xfMenuBar.frame1.frame2.frame4.state.disabled config $tmpButtonOpt"

  pack append .xfMenuBar.frame1.frame2.frame4.state \
              .xfMenuBar.frame1.frame2.frame4.state.message1 {left} \
              .xfMenuBar.frame1.frame2.frame4.state.normal {left fillx expand} \
              .xfMenuBar.frame1.frame2.frame4.state.active {left fillx expand} \
              .xfMenuBar.frame1.frame2.frame4.state.disabled {left fillx expand}

  XFMenuBarEntry textvar "Text variable:"

  XFMenuBarScale underline "Underline:" "" 40
  .xfMenuBar.frame1.frame2.frame4.underline.underline config \
    -from -1

  XFMenuBarEntry value "Value:"
  XFMenuBarEntry variable "Variable:"

  label .xfMenuBar.frame1.frame2.frame4.message1 \
    -anchor c \
    -relief raised \
    -text "Command:"
  catch ".xfMenuBar.frame1.frame2.frame4.message1 config $tmpMessageOpt"
  
  frame .xfMenuBar.frame1.frame2.frame4.command \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame4.command config $tmpFrameOpt"

  text .xfMenuBar.frame1.frame2.frame4.command.command \
    -highlightthickness 0 \
    -relief raised \
    -wrap none \
    -borderwidth 2 \
    -yscrollcommand ".xfMenuBar.frame1.frame2.frame4.command.vscroll set"
  catch ".xfMenuBar.frame1.frame2.frame4.command.command config $tmpMessageOpt"

  scrollbar .xfMenuBar.frame1.frame2.frame4.command.vscroll \
    -highlightthickness 0 \
    -relief raised \
    -command ".xfMenuBar.frame1.frame2.frame4.command.command yview"
  catch ".xfMenuBar.frame1.frame2.frame4.command.vscroll config $tmpScrollOpt"

  pack append .xfMenuBar.frame1.frame2.frame4.command \
              .xfMenuBar.frame1.frame2.frame4.command.vscroll {left filly} \
              .xfMenuBar.frame1.frame2.frame4.command.command {left fill expand}

  # bindings
  bind .xfMenuBar.frame1.frame2.frame5.buttons <ButtonPress-1> {
   XFMenuBarSelect1 %W %y}
  bind .xfMenuBar.frame1.frame2.frame5.buttons <Button1-Motion> {
   XFMenuBarSelect1 %W %y}
  bind .xfMenuBar.frame1.frame2.frame5.buttons <Shift-ButtonPress-1> {
   XFMenuBarSelect1 %W %y}
  bind .xfMenuBar.frame1.frame2.frame5.buttons <Shift-Button1-Motion> {
   XFMenuBarSelect1 %W %y}

  bind .xfMenuBar.frame1.frame2.frame6.menus <ButtonPress-1> {
   XFMenuBarSelect1 %W %y}
  bind .xfMenuBar.frame1.frame2.frame6.menus <Button1-Motion> {
   XFMenuBarSelect1 %W %y}
  bind .xfMenuBar.frame1.frame2.frame6.menus <Shift-ButtonPress-1> {
   XFMenuBarSelect1 %W %y}
  bind .xfMenuBar.frame1.frame2.frame6.menus <Shift-Button1-Motion> {
   XFMenuBarSelect1 %W %y}

  bind .xfMenuBar.frame1.frame2.frame8.menu <ButtonPress-1> {
   XFMenuBarSelect2 %W %y}
  bind .xfMenuBar.frame1.frame2.frame8.menu <Button1-Motion> {
   XFMenuBarSelect2 %W %y}
  bind .xfMenuBar.frame1.frame2.frame8.menu <Shift-ButtonPress-1> {
   XFMenuBarSelect2 %W %y}
  bind .xfMenuBar.frame1.frame2.frame8.menu <Shift-Button1-Motion> {
   XFMenuBarSelect2 %W %y}

  bind .xfMenuBar.frame1.frame2.frame4.actbg.actbg <Double-ButtonPress-3> {
   global xfMenuBar
   if {"[info commands XFColorBox]" != ""} {
     set xfMenuBarResult [XFColorBox $xfMenuBar(colorFile) "Active background:"]
     if {"$xfMenuBarResult" != ""} {
       .xfMenuBar.frame1.frame2.frame4.actbg.actbg delete 0 end
       .xfMenuBar.frame1.frame2.frame4.actbg.actbg insert end $xfMenuBarResult
     }
   }}

  bind .xfMenuBar.frame1.frame2.frame4.actfg.actfg <Double-ButtonPress-3> {
   global xfMenuBar
   if {"[info commands XFColorBox]" != ""} {
     set xfMenuBarResult [XFColorBox $xfMenuBar(colorFile) "Active foreground:"]
     if {"$xfMenuBarResult" != ""} {
       .xfMenuBar.frame1.frame2.frame4.actfg.actfg delete 0 end
       .xfMenuBar.frame1.frame2.frame4.actfg.actfg insert end $xfMenuBarResult
     }
   }}

  bind .xfMenuBar.frame1.frame2.frame4.bg.bg <Double-ButtonPress-3> {
   global xfMenuBar
   if {"[info commands XFColorBox]" != ""} {
     set xfMenuBarResult [XFColorBox $xfMenuBar(colorFile) "Background:"]
     if {"$xfMenuBarResult" != ""} {
       .xfMenuBar.frame1.frame2.frame4.bg.bg delete 0 end
       .xfMenuBar.frame1.frame2.frame4.bg.bg insert end $xfMenuBarResult
     }
   }}

  bind .xfMenuBar.frame1.frame2.frame4.bitmap.bitmap <Double-ButtonPress-3> {
   if {"[info commands FSBox]" != ""} {
     set xfMenuBarResult [FSBox]
     if {"$xfMenuBarResult" != ""} {
       .xfMenuBar.frame1.frame2.frame4.bitmap.bitmap delete 0 end
       .xfMenuBar.frame1.frame2.frame4.bitmap.bitmap insert end $xfMenuBarResult
     }
   }}

  bind .xfMenuBar.frame1.frame2.frame4.font.font <Double-ButtonPress-3> {
   global xfMenuBar
   if {"[info commands XFFontBox]" != ""} {
     set xfMenuBarResult [XFFontBox $xfMenuBar(fontFile)]
     if {"$xfMenuBarResult" != ""} {
       .xfMenuBar.frame1.frame2.frame4.font.font delete 0 end
       .xfMenuBar.frame1.frame2.frame4.font.font insert end $xfMenuBarResult
     }
   }}

  bind .xfMenuBar.frame1.frame2.frame4.fg.fg <Double-ButtonPress-3> {
   global xfMenuBar
   if {"[info commands XFColorBox]" != ""} {
     set xfMenuBarResult [XFColorBox $xfMenuBar(colorFile) "Foreground:"]
     if {"$xfMenuBarResult" != ""} {
       .xfMenuBar.frame1.frame2.frame4.fg.fg delete 0 end
       .xfMenuBar.frame1.frame2.frame4.fg.fg insert end $xfMenuBarResult
     }
   }}

  XFMenuBarReadMenus $xfMenuBarConfig

  # packing
  pack append .xfMenuBar.frame1.frame7 \
              .xfMenuBar.frame1.frame7.insert {left fill expand} \
              .xfMenuBar.frame1.frame7.modify {left fill expand} \
              .xfMenuBar.frame1.frame7.modifymenu {left fill expand} \
              .xfMenuBar.frame1.frame7.delete {left fill expand}
  pack append .xfMenuBar.frame1.frame3 \
              .xfMenuBar.frame1.frame3.ok {left fill expand} \
              .xfMenuBar.frame1.frame3.save {left fill expand}
  pack append .xfMenuBar.frame1.frame2.frame4.items \
              .xfMenuBar.frame1.frame2.frame4.items.items {left fill} \
              .xfMenuBar.frame1.frame2.frame4.items.message1 {left fill expand}
  pack append .xfMenuBar.frame1.frame2.frame4 \
              .xfMenuBar.frame1.frame2.frame4.actbg {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.actfg {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.bg {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.bitmap {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.border {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.font {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.fg {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.label {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.menu {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.size {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.textvar {top fillx} \
              .xfMenuBar.frame1.frame2.frame4.underline {top fillx}
  pack append .xfMenuBar.frame1.frame2.frame5 \
              .xfMenuBar.frame1.frame2.frame5.vscroll "$xfMenuBar(scrollSide) filly" \
              .xfMenuBar.frame1.frame2.frame5.hscroll {bottom fillx} \
              .xfMenuBar.frame1.frame2.frame5.buttons {left fill expand}
  pack append .xfMenuBar.frame1.frame2.frame6 \
              .xfMenuBar.frame1.frame2.frame6.vscroll "$xfMenuBar(scrollSide) filly" \
              .xfMenuBar.frame1.frame2.frame6.hscroll {bottom fillx} \
              .xfMenuBar.frame1.frame2.frame6.menus {left fill expand}
  pack append .xfMenuBar.frame1.frame2.frame8 \
              .xfMenuBar.frame1.frame2.frame8.mover {right filly} \
              .xfMenuBar.frame1.frame2.frame8.vscroll "$xfMenuBar(scrollSide) filly" \
              .xfMenuBar.frame1.frame2.frame8.hscroll {bottom fillx} \
              .xfMenuBar.frame1.frame2.frame8.menu {left fill expand}

  place .xfMenuBar.frame1.frame2.frame4 \
    -in .xfMenuBar.frame1.frame2 \
    -relx 0 \
    -rely 0 \
    -relheight 1.0 \
    -relwidth 0.5

  if {[.xfMenuBar.frame1.frame2.frame5.buttons size] > 0} {
    .xfMenuBar.frame1.frame7.insert config \
      -state disabled
    .xfMenuBar.frame1.frame7.modifymenu config \
      -state disabled
    .xfMenuBar.frame1.frame7.delete config \
      -state disabled

    place .xfMenuBar.frame1.frame2.frame5 \
      -in .xfMenuBar.frame1.frame2 \
      -relx 0.5 \
      -rely 0 \
      -relheight 0.5 \
      -relwidth 0.5

    place .xfMenuBar.frame1.frame2.frame6 \
      -in .xfMenuBar.frame1.frame2 \
      -relx 0.5 \
      -rely 0.5 \
      -relheight 0.5 \
      -relwidth 0.5
    XFMenuBarSetItems menubutton
  } {
    .xfMenuBar.frame1.frame7.insert config \
      -state normal
    .xfMenuBar.frame1.frame7.modifymenu config \
      -state normal
    .xfMenuBar.frame1.frame7.delete config \
      -state normal

    place .xfMenuBar.frame1.frame2.frame6 \
      -in .xfMenuBar.frame1.frame2 \
      -relx 0.5 \
      -rely 0 \
      -relheight 1 \
      -relwidth 0.5
    XFMenuBarSetItems menu
  }

  pack append .xfMenuBar.frame1 \
              .xfMenuBar.frame1.frame3 {bottom fillx} \
              .xfMenuBar.frame1.frame7 {bottom fillx} \
              .xfMenuBar.frame1.frame2 {top fill expand}
  pack append .xfMenuBar \
              .xfMenuBar.frame1 {top fill expand}

  update idletask
  XFMenuBarSetItem
}

##########
# Procedure: XFMenuBarEntry
# Description: build labled entry window
# Arguments: xfMenuBarName - the name of the widget we want to build
#            xfMenuBarLabel - the label
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarEntry {xfMenuBarName xfMenuBarLabel} {# xf ignore me 6
  global xfMenuBar

  set tmpFrameOpt ""
  set tmpMessageOpt ""
  if {"$xfMenuBar(background)" != ""} {
    append tmpFrameOpt "-background \"$xfMenuBar(background)\" "
    append tmpMessageOpt "-background \"$xfMenuBar(background)\" "
  }
  if {"$xfMenuBar(font)" != ""} {
    append tmpMessageOpt "-font \"$xfMenuBar(font)\" "
  }
  if {"$xfMenuBar(foreground)" != ""} {
    append tmpMessageOpt "-foreground \"$xfMenuBar(foreground)\" "
  }

  # build widgets
  frame .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName config $tmpFrameOpt"

  label .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.label$xfMenuBarName \
    -relief raised \
    -text "$xfMenuBarLabel"
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.label$xfMenuBarName config $tmpMessageOpt"

  entry .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.$xfMenuBarName \
    -highlightthickness 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.$xfMenuBarName config $tmpMessageOpt"

  # packing of the subwidgets
  pack append .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName \
              .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.label$xfMenuBarName {left filly} \
              .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.$xfMenuBarName {left fill expand}
}

##########
# Procedure: XFMenuBarDelete
# Description: delete a menu, or a menu item
# Arguments: xfMenuBarConfig - the menubar we configure
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarDelete {xfMenuBarConfig} {# xf ignore me 6

  set curSelected \
    [.xfMenuBar.frame1.frame2.frame6.menus curselection]
  if {[winfo ismapped .xfMenuBar.frame1.frame2.frame6] && $curSelected >= 0} {
    if {"[info commands XFDestroy]" != ""} {
      catch "XFDestroy [.xfMenuBar.frame1.frame2.frame6.menus get $curSelected]"
    } {
      catch "destroy [.xfMenuBar.frame1.frame2.frame6.menus get $curSelected]"
    }
    XFMenuBarReadMenus $xfMenuBarConfig
  } {
    set curSelected \
      [.xfMenuBar.frame1.frame2.frame8.menu curselection]
    if {[winfo ismapped .xfMenuBar.frame1.frame2.frame8] && $curSelected >= 0} {
      [.xfMenuBar.frame1.frame2.frame6.menus get [.xfMenuBar.frame1.frame2.frame6.menus curselection]] delete $curSelected
      XFMenuBarReadMenu \
        [.xfMenuBar.frame1.frame2.frame6.menus get [.xfMenuBar.frame1.frame2.frame6.menus curselection]]
    }
  }
}

##########
# Procedure: XFMenuBarEntryType
# Description: determine the type of a menu item
# Arguments: xfMenuBarW - the widget
#            xfMenuBarPosition - index of item in menu
# Returns: item type
# Sideeffects: none
##########
proc XFMenuBarEntryType {xfMenuBarW xfMenuBarPosition} {# xf ignore me 6

  if {![catch "$xfMenuBarW entryconfig $xfMenuBarPosition -menu"]} {
    return "cascade"
  } {
    if {![catch "$xfMenuBarW entryconfig $xfMenuBarPosition -value"]} {
      return "radiobutton"
    } {
      if {![catch "$xfMenuBarW entryconfig $xfMenuBarPosition -onvalue"]} {
        return "checkbutton"
      } {
        if {![catch "$xfMenuBarW entryconfig $xfMenuBarPosition -command"]} {
          return "command"
        } {
          return "separator"
        }
      }
    }
  }
}

##########
# Procedure: XFMenuBarInsert
# Description: insert a menu, or a menu item
# Arguments: xfMenuBarConfig - the menubar configure
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarInsert {xfMenuBarConfig} {# xf ignore me 6
  global xfMenuBar

  if {[winfo ismapped .xfMenuBar.frame1.frame2.frame6]} {
    if {"[.xfMenuBar.frame1.frame2.frame4.name.name get]" != ""} {
      if {"[info commands XFDestroy]" != ""} {
        catch "XFDestroy [.xfMenuBar.frame1.frame2.frame4.name.name get]"
      } {
        catch "destroy [.xfMenuBar.frame1.frame2.frame4.name.name get]"
      }
      if {"[info commands [.xfMenuBar.frame1.frame2.frame4.menu.menu get]]" == ""} {
        if {[catch "menu [.xfMenuBar.frame1.frame2.frame4.menu.menu get]" xfMenuBarResult]} {
          puts stderr "$xfMenuBarResult"
        } {
          .xfMenuBar.frame1.frame2.frame6.menus insert end \
            "[.xfMenuBar.frame1.frame2.frame4.name.name get]"
          .xfMenuBar.frame1.frame2.frame6.menus select anchor \
            [.xfMenuBar.frame1.frame2.frame6.menus size]
          .xfMenuBar.frame1.frame2.frame6.menus select set \
            [.xfMenuBar.frame1.frame2.frame6.menus size]
          XFMenuBarModify $xfMenuBarConfig
          XFMenuBarReadMenus $xfMenuBarConfig
        }
      }
    }
  } {
    set currType \
      [string trim [string trim $xfMenuBar(menuType)] "<>"]
    if {[winfo ismapped .xfMenuBar.frame1.frame2.frame8]} {
      if {[catch "[.xfMenuBar.frame1.frame2.frame6.menus get [.xfMenuBar.frame1.frame2.frame6.menus curselection]] add $currType" xfMenuBarResult]} {
        puts stderr $xfMenuBarResult
      } {
        .xfMenuBar.frame1.frame2.frame8.menu insert end \
          "$xfMenuBar(menuType) [.xfMenuBar.frame1.frame2.frame4.name.name get]"
        .xfMenuBar.frame1.frame2.frame8.menu select anchor \
          [.xfMenuBar.frame1.frame2.frame8.menu size]
        .xfMenuBar.frame1.frame2.frame8.menu select set \
          [.xfMenuBar.frame1.frame2.frame8.menu size]
        XFMenuBarModify $xfMenuBarConfig
        XFMenuBarReadMenu \
          [.xfMenuBar.frame1.frame2.frame6.menus get [.xfMenuBar.frame1.frame2.frame6.menus curselection]]
      }
    }
  }
}

##########
# Procedure: XFMenuBarInsertMenus
# Description: read all child menus
# Arguments: xfMenuBarW - the current menu
# Returns: none
# Sideeffects: listbox gets filled
##########
proc XFMenuBarInsertMenus {xfMenuBarW} {# xf ignore me 6

  if {"[winfo class $xfMenuBarW]" == "Menu"} {
    .xfMenuBar.frame1.frame2.frame6.menus insert end $xfMenuBarW
  }

  set last [$xfMenuBarW index last]
  if {"$last" == "none"} {
    set last -1
  }
  for {set counter 0} {$counter <= $last} {incr counter 1} {
    if {"[XFMenuBarEntryType $xfMenuBarW $counter]" == "cascade"} {
      if {"[lindex [$xfMenuBarW entryconfig $counter -menu] 4]" != ""} {
        if {"[info commands [lindex [$xfMenuBarW entryconfig $counter -menu] 4]]" == ""} {
          $xfMenuBarW entryconfig $counter -menu ""
        }
      }
    }
  }
  foreach counter [lsort [winfo children $xfMenuBarW]] {
    if {"[winfo class $counter]" == "Menu"} {
      XFMenuBarInsertMenus $counter
    }
  }
}

##########
# Procedure: XFMenuBarModify
# Description: modify a menubutton, menu, or a menu item
# Arguments: xfMenuBarConfig - the menubar we configure
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarModify {xfMenuBarConfig} {# xf ignore me 6
  global xfMenuBar

  set curSelected \
    [.xfMenuBar.frame1.frame2.frame5.buttons curselection]
  if {[winfo ismapped .xfMenuBar.frame1.frame2.frame5] && $curSelected >= 0} {
    set xfMenuBarList .xfMenuBar.frame1.frame2.frame5.buttons
    set xfMenuBarW [$xfMenuBarList get $curSelected]
    # insert values
    set optionString "config"
    if {"[.xfMenuBar.frame1.frame2.frame4.actbg.actbg get]" != ""} {
      append optionString \
        " -activebackground \{[.xfMenuBar.frame1.frame2.frame4.actbg.actbg get]\}"
    }
    if {"[.xfMenuBar.frame1.frame2.frame4.actfg.actfg get]" != ""} {
      append optionString \
        " -activeforeground \{[.xfMenuBar.frame1.frame2.frame4.actfg.actfg get]\}"
    }
    if {"[.xfMenuBar.frame1.frame2.frame4.bg.bg get]" != ""} {
      append optionString \
        " -background \{[.xfMenuBar.frame1.frame2.frame4.bg.bg get]\}"
    }
    if {"[.xfMenuBar.frame1.frame2.frame4.bitmap.bitmap get]" != ""} {
      if {"[string index [.xfMenuBar.frame1.frame2.frame4.bitmap.bitmap get] 0]" == "@"} {
        append optionString \
          " -bitmap \{[.xfMenuBar.frame1.frame2.frame4.bitmap.bitmap get]\}"
      } {
        append optionString \
          " -bitmap \{@[.xfMenuBar.frame1.frame2.frame4.bitmap.bitmap get]\}"
      }
    } {
      append optionString \
        " -bitmap \{\}"
    }
    append optionString \
      " -borderwidth [.xfMenuBar.frame1.frame2.frame4.border.border get]"
    if {"[.xfMenuBar.frame1.frame2.frame4.font.font get]" != ""} {
      append optionString \
        " -font \{[.xfMenuBar.frame1.frame2.frame4.font.font get]\}"
    }
    if {"[.xfMenuBar.frame1.frame2.frame4.fg.fg get]" != ""} {
      append optionString \
        " -foreground \{[.xfMenuBar.frame1.frame2.frame4.fg.fg get]\}"
    }
    append optionString \
      " -menu \{[.xfMenuBar.frame1.frame2.frame4.menu.menu get]\}"
    append optionString \
      " -text \{[.xfMenuBar.frame1.frame2.frame4.label.label get]\}"
    append optionString \
      " -textvariable \{[.xfMenuBar.frame1.frame2.frame4.textvar.textvar get]\}"
    append optionString \
      " -underline \{[.xfMenuBar.frame1.frame2.frame4.underline.underline get]\}"
    if {[.xfMenuBar.frame1.frame2.frame4.size.size1.size1 get] > 0 &&
	[.xfMenuBar.frame1.frame2.frame4.size.size2.size2 get] > 0} {
      append optionString \
        " -width \{[.xfMenuBar.frame1.frame2.frame4.size.size1.size1 get]\}"
      append optionString \
        " -height \{[.xfMenuBar.frame1.frame2.frame4.size.size2.size2 get]\}"
    }
    if {[catch "$xfMenuBarW $optionString" xfMenuBarResult]} {
      puts stderr $xfMenuBarResult
    }

    if {"[info commands [.xfMenuBar.frame1.frame2.frame4.menu.menu get]]" == ""} {
      if {[catch "menu [.xfMenuBar.frame1.frame2.frame4.menu.menu get]" xfMenuBarResult]} {
        puts stderr "$xfMenuBarResult"
      } {
        .xfMenuBar.frame1.frame2.frame6.menus insert end \
          "[.xfMenuBar.frame1.frame2.frame4.name.name get]"
        .xfMenuBar.frame1.frame2.frame6.menus select anchor \
          [.xfMenuBar.frame1.frame2.frame6.menus size]
        .xfMenuBar.frame1.frame2.frame6.menus select set \
          [.xfMenuBar.frame1.frame2.frame6.menus size]
        XFMenuBarModify $xfMenuBarConfig
        XFMenuBarReadMenus $xfMenuBarConfig
      }
    }
  } {
    set curSelected \
      [.xfMenuBar.frame1.frame2.frame6.menus curselection]
    if {[winfo ismapped .xfMenuBar.frame1.frame2.frame6] && $curSelected >= 0} {
      set xfMenuBarList .xfMenuBar.frame1.frame2.frame6.menus
      set xfMenuBarW [$xfMenuBarList get $curSelected]
      # insert values
      set optionString "config"
      if {"[.xfMenuBar.frame1.frame2.frame4.actbg.actbg get]" != ""} {
        append optionString \
          " -activebackground \{[.xfMenuBar.frame1.frame2.frame4.actbg.actbg get]\}"
      }
      append optionString \
        " -activeborderwidth \{[.xfMenuBar.frame1.frame2.frame4.actborder.actborder get]\}"
      if {"[.xfMenuBar.frame1.frame2.frame4.actfg.actfg get]" != ""} {
        append optionString \
          " -activeforeground \{[.xfMenuBar.frame1.frame2.frame4.actfg.actfg get]\}"
      }
      if {"[.xfMenuBar.frame1.frame2.frame4.bg.bg get]" != ""} {
        append optionString \
          " -background \{[.xfMenuBar.frame1.frame2.frame4.bg.bg get]\}"
      }
      append optionString \
        " -borderwidth \{[.xfMenuBar.frame1.frame2.frame4.border.border get]\}"
      if {"[.xfMenuBar.frame1.frame2.frame4.font.font get]" != ""} {
        append optionString \
          " -font \{[.xfMenuBar.frame1.frame2.frame4.font.font get]\}"
      }
      if {"[.xfMenuBar.frame1.frame2.frame4.fg.fg get]" != ""} {
        append optionString \
          " -foreground \{[.xfMenuBar.frame1.frame2.frame4.fg.fg get]\}"
      }
      if {[catch "$xfMenuBarW $optionString" xfMenuBarResult]} {
        puts stderr $xfMenuBarResult
      }
    } {
      set curSelected \
        [.xfMenuBar.frame1.frame2.frame8.menu curselection]
      if {[winfo ismapped .xfMenuBar.frame1.frame2.frame8] &&
          $curSelected >= 0} {
        set xfMenuBarList .xfMenuBar.frame1.frame2.frame6.menus
        set xfMenuBarW \
          [$xfMenuBarList get [.xfMenuBar.frame1.frame2.frame6.menus curselection]]
        # insert values
        if {![string match <separator* [.xfMenuBar.frame1.frame2.frame8.menu get $curSelected]]} {
          set optionString "entryconfig $curSelected"
          if {"[.xfMenuBar.frame1.frame2.frame4.actbg.actbg get]" != ""} {
            append optionString \
              " -activebackground \{[.xfMenuBar.frame1.frame2.frame4.actbg.actbg get]\}"
          }
          append optionString \
            " -accelerator \{[.xfMenuBar.frame1.frame2.frame4.accelerator.accelerator get]\}"
          if {"[.xfMenuBar.frame1.frame2.frame4.bg.bg get]" != ""} {
            append optionString \
              " -background \{[.xfMenuBar.frame1.frame2.frame4.bg.bg get]\}"
          }
          if {"[.xfMenuBar.frame1.frame2.frame4.bitmap.bitmap get]" != ""} {
            if {"[string index [.xfMenuBar.frame1.frame2.frame4.bitmap.bitmap get] 0]" == "@"} {
              append optionString \
                " -bitmap \{[.xfMenuBar.frame1.frame2.frame4.bitmap.bitmap get]\}"
            } {
              append optionString \
                " -bitmap \{@[.xfMenuBar.frame1.frame2.frame4.bitmap.bitmap get]\}"
            }
          } {
            append optionString \
              " -bitmap \{\}"
          }
          append optionString \
            " -command \{[.xfMenuBar.frame1.frame2.frame4.command.command get 1.0 end]\}"
          if {"[.xfMenuBar.frame1.frame2.frame4.font.font get]" != ""} {
            append optionString \
              " -font \{[.xfMenuBar.frame1.frame2.frame4.font.font get]\}"
          }
          append optionString \
            " -label \{[.xfMenuBar.frame1.frame2.frame4.label.label get]\}"
          append optionString \
            " -state \{$xfMenuBar(state)\}"
          append optionString \
            " -underline \{[.xfMenuBar.frame1.frame2.frame4.underline.underline get]\}"
          case [.xfMenuBar.frame1.frame2.frame8.menu get $curSelected] in {
            {<cascade*} {
              append optionString \
                " -menu \{[.xfMenuBar.frame1.frame2.frame4.menu.menu get]\}"
            }
            {<radiobutton*} {
              append optionString \
                " -value \{[.xfMenuBar.frame1.frame2.frame4.value.value get]\}"
              if {"[.xfMenuBar.frame1.frame2.frame4.variable.variable get]" != ""} {
                append optionString \
                  " -variable \{[.xfMenuBar.frame1.frame2.frame4.variable.variable get]\}"
              }
            }
            {<checkbutton*} {
              append optionString \
                " -offvalue \{[.xfMenuBar.frame1.frame2.frame4.offvalue.offvalue get]\}"
              append optionString \
                " -onvalue \{[.xfMenuBar.frame1.frame2.frame4.onvalue.onvalue get]\}"
              if {"[.xfMenuBar.frame1.frame2.frame4.variable.variable get]" != ""} {
                append optionString \
                  " -variable \{[.xfMenuBar.frame1.frame2.frame4.variable.variable get]\}"
              }
            }
          }
          if {[catch "$xfMenuBarW $optionString" xfMenuBarResult]} {
            puts stderr $xfMenuBarResult
          }
        }
      }
    }
  }
}

##########
# Procedure: XFMenuBarReadMenu
# Description: read the items of the current menu
# Arguments: xfMenuBarW - the name of the menu we want to edit
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarReadMenu {xfMenuBarW} {# xf ignore me 6

  if {[.xfMenuBar.frame1.frame2.frame8.menu size] > 0} {
    .xfMenuBar.frame1.frame2.frame8.menu delete 0 end
  }
  set last [$xfMenuBarW index last]
  if {"$last" == "none"} {
    set last -1
  }
  for {set counter 0} {$counter <= $last} {incr counter 1} {
    if {"[XFMenuBarEntryType $xfMenuBarW $counter]" == "separator"} {
      .xfMenuBar.frame1.frame2.frame8.menu insert end \
        "<[XFMenuBarEntryType $xfMenuBarW $counter]>"
    } {
      .xfMenuBar.frame1.frame2.frame8.menu insert end \
        "<[XFMenuBarEntryType $xfMenuBarW $counter]> [lindex [$xfMenuBarW entryconfigure $counter -label] 4]"
    }
  }

  if {[.xfMenuBar.frame1.frame2.frame8.menu size] > 0} {
    .xfMenuBar.frame1.frame2.frame8.menu select anchor 0
    .xfMenuBar.frame1.frame2.frame8.menu select set 0
    XFMenuBarSetItem
    .xfMenuBar.frame1.frame2.frame8.mover configure \
        -to [.xfMenuBar.frame1.frame2.frame8.menu size]
  } {
    .xfMenuBar.frame1.frame2.frame8.mover configure \
        -to 1
  }
}

##########
# Procedure: XFMenuBarReadMenus
# Description: read all menubuttons and menus
# Arguments: xfMenuBarConfig - the menubar we configure
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarReadMenus {xfMenuBarConfig} {# xf ignore me 6

  if {[.xfMenuBar.frame1.frame2.frame5.buttons size] > 0} {
    .xfMenuBar.frame1.frame2.frame5.buttons delete 0 end
  }
  if {[.xfMenuBar.frame1.frame2.frame6.menus size] > 0} {
    .xfMenuBar.frame1.frame2.frame6.menus delete 0 end
  }
  if {![catch "winfo children $xfMenuBarConfig" xfMenuBarResult]} {
    foreach xfMenuBarChild [lsort [split $xfMenuBarResult]] {
      if {"[winfo class $xfMenuBarChild]" == "Menubutton"} {
        if {"[lindex [$xfMenuBarChild config -menu] 4]" != ""} {
          if {"[info commands [lindex [$xfMenuBarChild config -menu] 4]]" == ""} {
            $xfMenuBarChild config -menu ""
          }
        }
        .xfMenuBar.frame1.frame2.frame5.buttons insert end $xfMenuBarChild
        foreach xfMenuBarMenuChild [lsort [winfo children $xfMenuBarChild]] {
          XFMenuBarInsertMenus $xfMenuBarMenuChild
        }
      }
      if {"[winfo class $xfMenuBarChild]" == "Menu"} {
        XFMenuBarInsertMenus $xfMenuBarChild
      }
    }
  }
  if {[.xfMenuBar.frame1.frame2.frame5.buttons size] > 0} {
    .xfMenuBar.frame1.frame2.frame5.buttons select anchor 0
    .xfMenuBar.frame1.frame2.frame5.buttons select set 0
    XFMenuBarSetItem
  } {
    .xfMenuBar.frame1.frame2.frame6.menus select anchor 0
    .xfMenuBar.frame1.frame2.frame6.menus select set 0
    XFMenuBarSetItem
  }
}

##########
# Procedure: XFMenuBarReposition
# Description: reposition a menu item
# Arguments: xfMenuBarPos - the new position of the item
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarReposition {xfMenuBarPos} {# xf ignore me 6

  set curSelected \
    [.xfMenuBar.frame1.frame2.frame8.menu curselection]
  set xfMenuBarW \
    [.xfMenuBar.frame1.frame2.frame6.menus get [.xfMenuBar.frame1.frame2.frame6.menus curselection]]
  if {$curSelected >= 0} {
    set xfMenuBarLast [$xfMenuBarW index last]
    if {"$xfMenuBarLast" == "none"} {
      set xfMenuBarLast -1
    }
    set counter 0
    set newMenu ""
    set currentItem "[XFMenuBarEntryType $xfMenuBarW $curSelected] "
    foreach optCounter [$xfMenuBarW entryconfig $curSelected] {
      if {[llength $optCounter] == 5} {
        if {"[lindex $optCounter 3]" != "[lindex $optCounter 4]"} {
          append currentItem " [lindex $optCounter 0] \{[lindex $optCounter 4]\}"
        }
      }
    }
    set counter 0
    while {$counter <= $xfMenuBarLast} {
      if {$xfMenuBarPos > $curSelected &&
          $counter == [expr $xfMenuBarPos+1]} {
        lappend newMenu $currentItem
      }
      if {$xfMenuBarPos <= $curSelected &&
          $counter == $xfMenuBarPos} {
        lappend newMenu $currentItem
      }
      if {$counter == $curSelected} {
        incr counter
        continue
      }
      set newItem "[XFMenuBarEntryType $xfMenuBarW $counter] "
      foreach optCounter [$xfMenuBarW entryconfig $counter] {
        if {[llength $optCounter] == 5} {
          if {"[lindex $optCounter 3]" != "[lindex $optCounter 4]"} {
            append newItem " [lindex $optCounter 0] \{[lindex $optCounter 4]\}"
          }
        }
      }
      lappend newMenu $newItem
      incr counter
    }
    if {$xfMenuBarPos > $curSelected} {
      lappend newMenu $currentItem
    }
    set counter 0
    while {$counter <= $xfMenuBarLast} {
      $xfMenuBarW delete 0
      incr counter
    }
    set counter 0
    while {$counter <= $xfMenuBarLast} {
      if {[catch "$xfMenuBarW add [lindex $newMenu $counter]" xfMenuBarRes]} {
        puts stderr $xfMenuBarRes
      }
      incr counter
    }
    XFMenuBarReadMenu $xfMenuBarW
    .xfMenuBar.frame1.frame2.frame8.menu select anchor $xfMenuBarPos
    .xfMenuBar.frame1.frame2.frame8.menu select set $xfMenuBarPos
    XFMenuBarSetItem
  }
}

##########
# Procedure: XFMenuBarSave
# Description: save the current definition
# Arguments: xfMenuBarConfig - the menubar we configure
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarSave {xfMenuBarConfig} {# xf ignore me 6
  global xfMenuBar

  if {![catch "open $xfMenuBar(userFile) w" outFile]} {
    foreach widgets [winfo children $xfMenuBarConfig] {
      if {"[winfo class $widgets]" == "Menu"} {
        XFMenuBarSaveMenu $outFile $widgets
      }
      if {"[winfo class $widgets]" == "Menubutton"} {
        puts $outFile "$widgets config" nonewline
        foreach optCounter [$widgets config] {
          if {[llength $optCounter] == 5} {
            if {"[lindex $optCounter 3]" != "[lindex $optCounter 4]"} {
              puts $outFile "\\\n  [lindex $optCounter 0] \{[lindex $optCounter 4]\}" nonewline
            }
          }
        }
        puts $outFile ""
        puts $outFile ""
        foreach children [winfo children $widgets] {
          if {"[winfo class $children]" == "Menu"} {
            XFMenuBarSaveMenu $outFile $children
          }
        }
      }
    }
    puts $outFile "# eof"
    close $outFile
  } {
    puts stderr $outFile
  }
}

##########
# Procedure: XFMenuBarSaveMenu
# Description: save the current definition
# Arguments: outFile - the output descriptor
#            xfMenuBarW - the menu widget
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarSaveMenu {outFile xfMenuBarW} {# xf ignore me 6

  puts $outFile "if {\"\[info commands XFDestroy\]\" != \"\"} {"
  puts $outFile "  catch \"XFDestroy $xfMenuBarW\""
  puts $outFile "} {"
  puts $outFile "  catch \"destroy $xfMenuBarW\""
  puts $outFile "}"
  puts $outFile "menu $xfMenuBarW" nonewline
  foreach optCounter [$xfMenuBarW config] {
    if {[llength $optCounter] == 5} {
      if {"[lindex $optCounter 3]" != "[lindex $optCounter 4]"} {
        puts $outFile "\\\n  [lindex $optCounter 0] \{[lindex $optCounter 4]\}" nonewline
      }
    }
  }
  puts $outFile ""

  set menuLast [$xfMenuBarW index last]
  if {"$menuLast" == "none"} {
    set menuLast -1
  }
  set counter 0
  while {$counter <= $menuLast} {
    puts $outFile "$xfMenuBarW add [XFMenuBarEntryType $xfMenuBarW $counter] " nonewline
    foreach optCounter [$xfMenuBarW entryconfig $counter] {
      if {[llength $optCounter] == 5} {
        if {"[lindex $optCounter 3]" != "[lindex $optCounter 4]"} {
          puts $outFile "\\\n  [lindex $optCounter 0] \{[lindex $optCounter 4]\}" nonewline
        }
      }
    }
    puts $outFile ""
    incr counter
  }
  puts $outFile ""
  puts $outFile ""

  foreach children [winfo children $xfMenuBarW] {
    if {"[winfo class $children]" == "Menu"} {
      XFMenuBarSaveMenu $outFile $children
    }
  }
}

##########
# Procedure: XFMenuBarScale
# Description: build labled scale
# Arguments: xfMenuBarName - the name of the widget we want to build
#            xfMenuBarLabel1 - the label left from scale
#            xfMenuBarLabel2 - the label of the scale
#            xfMenuBarTo - the maximal value of the scale
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarScale {xfMenuBarName xfMenuBarLabel1 xfMenuBarLabel2 xfMenuBarTo} {# xf ignore me 6
  global xfMenuBar

  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  if {"$xfMenuBar(background)" != ""} {
    append tmpFrameOpt "-background \"$xfMenuBar(background)\" "
    append tmpMessageOpt "-background \"$xfMenuBar(background)\" "
    append tmpScaleOpt "-background \"$xfMenuBar(background)\" "
  }
  if {"$xfMenuBar(font)" != ""} {
    append tmpMessageOpt "-font \"$xfMenuBar(font)\" "
  }
  if {"$xfMenuBar(foreground)" != ""} {
    append tmpMessageOpt "-foreground \"$xfMenuBar(foreground)\" "
    append tmpScaleOpt "-foreground \"$xfMenuBar(foreground)\" "
  }
  if {"$xfMenuBar(scrollActiveForeground)" != ""} {
    append tmpScaleOpt "-activeforeground \"$xfMenuBar(scrollActiveForeground)\" "
  }

  # build widgets
  frame .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName config $tmpFrameOpt"

  label .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.message1 \
    -relief raised \
    -text "$xfMenuBarLabel1"
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.message1 config $tmpMessageOpt"

  scale .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.$xfMenuBarName \
    -highlightthickness 0 \
    -from 0 \
    -label "$xfMenuBarLabel2" \
    -orient horizontal \
    -relief raised \
    -sliderlength 15 \
    -to $xfMenuBarTo \
    -width 8
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.$xfMenuBarName config $tmpScaleOpt"

  # packing of the subwidgets
  pack append .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName \
              .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.message1 {left filly} \
              .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.$xfMenuBarName {left fillx expand}
}

##########
# Procedure: XFMenuBarScaleDouble
# Description: build scale frame with two scales
# Arguments: xfMenuBarName - the name of the scales
#            xfMenuBarLabel1 - the label left from the scales
#            xfMenuBarLabel2 - the label of the 1st scale
#            xfMenuBarLabel3 - the label of the 2nd scale
#            xfMenuBarTo1 - the maximal value of the 1st scale
#            xfMenuBarTo2 - the maximal value of the 2nd scale
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarScaleDouble {xfMenuBarName xfMenuBarLabel1 xfMenuBarLabel2 xfMenuBarLabel3 xfMenuBarTo1 xfMenuBarTo2} {# xf ignore me 6
  global xfMenuBar

  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  if {"$xfMenuBar(background)" != ""} {
    append tmpFrameOpt "-background \"$xfMenuBar(background)\" "
    append tmpMessageOpt "-background \"$xfMenuBar(background)\" "
    append tmpScaleOpt "-background \"$xfMenuBar(background)\" "
  }
  if {"$xfMenuBar(font)" != ""} {
    append tmpMessageOpt "-font \"$xfMenuBar(font)\" "
  }
  if {"$xfMenuBar(foreground)" != ""} {
    append tmpMessageOpt "-foreground \"$xfMenuBar(foreground)\" "
    append tmpScaleOpt "-foreground \"$xfMenuBar(foreground)\" "
  }
  if {"$xfMenuBar(scrollActiveForeground)" != ""} {
    append tmpScaleOpt "-activeforeground \"$xfMenuBar(scrollActiveForeground)\" "
  }

  # build widgets
  frame .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName config $tmpFrameOpt"

  frame .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}1 \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}1 config $tmpFrameOpt"

  frame .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}2 \
    -borderwidth 0 \
    -relief raised
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}2 config $tmpFrameOpt"

  label .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.message1 \
    -relief raised \
    -text "$xfMenuBarLabel1"
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.message1 config $tmpMessageOpt"

  scale .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}1.${xfMenuBarName}1 \
    -highlightthickness 0 \
    -from 0 \
    -label "$xfMenuBarLabel2" \
    -orient horizontal \
    -relief raised \
    -sliderlength 15 \
    -to $xfMenuBarTo1 \
    -width 8
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}1.${xfMenuBarName}1 config $tmpScaleOpt"

  scale .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}2.${xfMenuBarName}2 \
    -highlightthickness 0 \
    -from 0 \
    -label "$xfMenuBarLabel3" \
    -orient horizontal \
    -relief raised \
    -sliderlength 15 \
    -to $xfMenuBarTo2 \
    -width 8
  catch ".xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}2.${xfMenuBarName}2 config $tmpScaleOpt"

  # packing of the subwidgets
  pack append .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}1 \
              .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}1.${xfMenuBarName}1 {left fillx expand}
  pack append .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}2 \
              .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}2.${xfMenuBarName}2 {left fillx expand}
  pack append .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName \
              .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.message1 {left filly} \
              .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}1 {left fillx expand} \
              .xfMenuBar.frame1.frame2.frame4.$xfMenuBarName.${xfMenuBarName}2 {left fillx expand}
}

##########
# Procedure: XFMenuBarSelect1
# Description: select a specified menubutton or menu
# Arguments: xfMenuBarW - the listbox
#            xfMenuBarY - the y coordinate in listbox
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarSelect1 {xfMenuBarW xfMenuBarY} {# xf ignore me 6

  .xfMenuBar.frame1.frame2.frame5.buttons select clear 0 end
  .xfMenuBar.frame1.frame2.frame6.menus select clear 0 end

  set xfMenuBarNearest [$xfMenuBarW nearest $xfMenuBarY]
  if {$xfMenuBarNearest >= 0} {
    $xfMenuBarW select anchor $xfMenuBarNearest
    $xfMenuBarW select set $xfMenuBarNearest
    XFMenuBarSetItem
    if {"$xfMenuBarW" == ".xfMenuBar.frame1.frame2.frame5.buttons"} {
      .xfMenuBar.frame1.frame7.insert config \
        -state disabled
      .xfMenuBar.frame1.frame7.modifymenu config \
        -state disabled
      .xfMenuBar.frame1.frame7.delete config \
        -state disabled
    } {
      .xfMenuBar.frame1.frame7.insert config \
        -state normal
      .xfMenuBar.frame1.frame7.modifymenu config \
        -state normal
      .xfMenuBar.frame1.frame7.delete config \
        -state normal
    }
  }
}

##########
# Procedure: XFMenuBarSelect2
# Description: select a specified menu item
# Arguments: xfMenuBarW - the listbox
#            xfMenuBarY - the y coordinate in listbox
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarSelect2 {xfMenuBarW xfMenuBarY} {# xf ignore me 6

  set xfMenuBarNearest [$xfMenuBarW nearest $xfMenuBarY]
  if {$xfMenuBarNearest >= 0} {
    $xfMenuBarW select anchor $xfMenuBarNearest
    $xfMenuBarW select set $xfMenuBarNearest
    XFMenuBarSetItem
  }
}

##########
# Procedure: XFMenuBarSetItem
# Description: set item fields to values from currently selected item
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarSetItem {} {# xf ignore me 6
  global xfMenuBar

  set xfMenuBarList ""
  set curSelected \
    [.xfMenuBar.frame1.frame2.frame5.buttons curselection]
  if {[winfo ismapped .xfMenuBar.frame1.frame2.frame5] && $curSelected >= 0} {
    set xfMenuBarList .xfMenuBar.frame1.frame2.frame5.buttons
    XFMenuBarSetItems menubutton
    set xfMenuBarW [$xfMenuBarList get $curSelected]
    # insert values
    .xfMenuBar.frame1.frame2.frame4.actbg.actbg \
      insert 0 [lindex [$xfMenuBarW config -activebackground] 4]
    .xfMenuBar.frame1.frame2.frame4.actfg.actfg \
      insert 0 [lindex [$xfMenuBarW config -activeforeground] 4]
    .xfMenuBar.frame1.frame2.frame4.bg.bg \
      insert 0 [lindex [$xfMenuBarW config -background] 4]
    .xfMenuBar.frame1.frame2.frame4.bitmap.bitmap \
      insert 0 [lindex [$xfMenuBarW config -bitmap] 4]
    if {"[lindex [$xfMenuBarW config -borderwidth] 4]" != ""} {
      .xfMenuBar.frame1.frame2.frame4.border.border \
        set [lindex [$xfMenuBarW config -borderwidth] 4]
    }
    .xfMenuBar.frame1.frame2.frame4.font.font \
      insert 0 [lindex [$xfMenuBarW config -font] 4]
    .xfMenuBar.frame1.frame2.frame4.fg.fg \
      insert 0 [lindex [$xfMenuBarW config -foreground] 4]
    if {"[lindex [$xfMenuBarW config -height] 4]" != ""} {
      .xfMenuBar.frame1.frame2.frame4.size.size2.size2 \
        set [lindex [$xfMenuBarW config -height] 4]
    }
    .xfMenuBar.frame1.frame2.frame4.label.label \
      insert 0 [lindex [$xfMenuBarW config -text] 4]
    if {"[lindex [$xfMenuBarW config -menu] 4]" != ""} {
      .xfMenuBar.frame1.frame2.frame4.menu.menu \
        delete 0 end
    }
    .xfMenuBar.frame1.frame2.frame4.menu.menu \
      insert 0 [lindex [$xfMenuBarW config -menu] 4]
    .xfMenuBar.frame1.frame2.frame4.textvar.textvar \
      insert 0 [lindex [$xfMenuBarW config -textvariable] 4]
    if {"[lindex [$xfMenuBarW config -underline] 4]" != ""} {
      .xfMenuBar.frame1.frame2.frame4.underline.underline \
        set [lindex [$xfMenuBarW config -underline] 4]
    }
    if {"[lindex [$xfMenuBarW config -width] 4]" != ""} {
      .xfMenuBar.frame1.frame2.frame4.size.size1.size1 \
        set [lindex [$xfMenuBarW config -width] 4]
    }
  } {
    set curSelected \
      [.xfMenuBar.frame1.frame2.frame6.menus curselection]
    if {[winfo ismapped .xfMenuBar.frame1.frame2.frame6] && $curSelected >= 0} {
      set xfMenuBarList .xfMenuBar.frame1.frame2.frame6.menus
      XFMenuBarSetItems menu
      set xfMenuBarW [$xfMenuBarList get $curSelected]
      # insert values
      .xfMenuBar.frame1.frame2.frame4.name.name \
        insert 0 [.xfMenuBar.frame1.frame2.frame6.menus get $curSelected]
      .xfMenuBar.frame1.frame2.frame4.actbg.actbg \
        insert 0 [lindex [$xfMenuBarW config -activebackground] 4]
      if {"[lindex [$xfMenuBarW config -activeborderwidth] 4]" != ""} {
        .xfMenuBar.frame1.frame2.frame4.actborder.actborder \
          set [lindex [$xfMenuBarW config -activeborderwidth] 4]
      }
      .xfMenuBar.frame1.frame2.frame4.actfg.actfg \
        insert 0 [lindex [$xfMenuBarW config -activeforeground] 4]
      .xfMenuBar.frame1.frame2.frame4.bg.bg \
        insert 0 [lindex [$xfMenuBarW config -background] 4]
      if {"[lindex [$xfMenuBarW config -borderwidth] 4]" != ""} {
        .xfMenuBar.frame1.frame2.frame4.border.border \
          set [lindex [$xfMenuBarW config -borderwidth] 4]
      }
      .xfMenuBar.frame1.frame2.frame4.font.font \
        insert 0 [lindex [$xfMenuBarW config -font] 4]
      .xfMenuBar.frame1.frame2.frame4.fg.fg \
        insert 0 [lindex [$xfMenuBarW config -foreground] 4]
    } {
      set curSelected \
        [.xfMenuBar.frame1.frame2.frame8.menu curselection]
      if {[winfo ismapped .xfMenuBar.frame1.frame2.frame8] &&
          $curSelected >= 0} {
        set xfMenuBarList .xfMenuBar.frame1.frame2.frame8.menu
        case [.xfMenuBar.frame1.frame2.frame8.menu get $curSelected] in {
          {<cascade*} {
            set xfMenuBar(menuType) "<cascade> "
            .xfMenuBar.frame1.frame2.frame4.items.items.m \
              entryconfigure 0 -state active
            XFMenuBarSetItems cascade
          }
          {<check*} {
            set xfMenuBar(menuType) "<checkbutton> "
            .xfMenuBar.frame1.frame2.frame4.items.items.m \
              entryconfigure 1 -state active
            XFMenuBarSetItems check
          }
          {<radio*} {
            set xfMenuBar(menuType) "<radiobutton> "
            .xfMenuBar.frame1.frame2.frame4.items.items.m \
              entryconfigure 3 -state active
            XFMenuBarSetItems radio
          }
          {<separator*} {
            set xfMenuBar(menuType) "<separator> "
            .xfMenuBar.frame1.frame2.frame4.items.items.m \
              entryconfigure 4 -state active
            XFMenuBarSetItems separator
          }
          {default} {
            set xfMenuBar(menuType) "<command> "
            .xfMenuBar.frame1.frame2.frame4.items.items.m \
              entryconfigure 2 -state active
            XFMenuBarSetItems command
          }
        }
        # insert values
        if {![string match <separator* [.xfMenuBar.frame1.frame2.frame8.menu get $curSelected]]} {
          set xfMenuBarW [.xfMenuBar.frame1.frame2.frame6.menus get [.xfMenuBar.frame1.frame2.frame6.menus curselection]]
          .xfMenuBar.frame1.frame2.frame4.accelerator.accelerator \
            insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -accelerator] 4]
          .xfMenuBar.frame1.frame2.frame4.actbg.actbg \
            insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -activebackground] 4]
          .xfMenuBar.frame1.frame2.frame4.bg.bg \
            insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -background] 4]
          .xfMenuBar.frame1.frame2.frame4.bitmap.bitmap \
            insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -bitmap] 4]
          .xfMenuBar.frame1.frame2.frame4.command.command delete 1.0 end
          .xfMenuBar.frame1.frame2.frame4.command.command insert 1.0 \
            [lindex [$xfMenuBarW entryconfig $curSelected -command] 4]
          .xfMenuBar.frame1.frame2.frame4.font.font \
            insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -font] 4]
          .xfMenuBar.frame1.frame2.frame4.label.label \
            insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -label] 4]
          set xfMenuBar(state) \
            [lindex [$xfMenuBarW entryconfig $curSelected -state] 4]
          case $xfMenuBar(state) in {
            {active} {
              .xfMenuBar.frame1.frame2.frame4.state.active select
            }
            {disabled} {
              .xfMenuBar.frame1.frame2.frame4.state.disabled select
            }
            {default} {
              .xfMenuBar.frame1.frame2.frame4.state.normal select
            }
          }
          if {"[lindex [$xfMenuBarW entryconfig $curSelected -underline] 4]" != ""} {
            .xfMenuBar.frame1.frame2.frame4.underline.underline \
              set [lindex [$xfMenuBarW entryconfig $curSelected -underline] 4]
          }
          case [.xfMenuBar.frame1.frame2.frame8.menu get $curSelected] in {
            {<cascade*} {
              if {"[lindex [$xfMenuBarW entryconfig $curSelected -menu] 4]" != ""} {
                .xfMenuBar.frame1.frame2.frame4.menu.menu \
                  delete 0 end
              }
              .xfMenuBar.frame1.frame2.frame4.menu.menu \
                insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -menu] 4]
            }
            {<radiobutton*} {
              .xfMenuBar.frame1.frame2.frame4.value.value \
                insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -value] 4]
              .xfMenuBar.frame1.frame2.frame4.variable.variable \
                insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -variable] 4]
            }
            {<checkbutton*} {
              .xfMenuBar.frame1.frame2.frame4.offvalue.offvalue \
                insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -offvalue] 4]
              .xfMenuBar.frame1.frame2.frame4.onvalue.onvalue \
                insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -onvalue] 4]
              .xfMenuBar.frame1.frame2.frame4.variable.variable \
                insert 0 [lindex [$xfMenuBarW entryconfig $curSelected -variable] 4]
            }
          }
        }
      }
    }
  }
}

##########
# Procedure: XFMenuBarSetItems
# Description: set item fields for currently selected widget
# Arguments: xfMenuBarType - the type we want to edit
# Returns: none
# Sideeffects: none
##########
proc XFMenuBarSetItems {xfMenuBarType} {# xf ignore me 6

  # remove widgets
  pack unpack .xfMenuBar.frame1.frame2.frame4.items
  pack unpack .xfMenuBar.frame1.frame2.frame4.accelerator
  pack unpack .xfMenuBar.frame1.frame2.frame4.actbg
  pack unpack .xfMenuBar.frame1.frame2.frame4.actborder
  pack unpack .xfMenuBar.frame1.frame2.frame4.actfg
  pack unpack .xfMenuBar.frame1.frame2.frame4.bg
  pack unpack .xfMenuBar.frame1.frame2.frame4.bitmap
  pack unpack .xfMenuBar.frame1.frame2.frame4.border
  pack unpack .xfMenuBar.frame1.frame2.frame4.message1
  pack unpack .xfMenuBar.frame1.frame2.frame4.command
  pack unpack .xfMenuBar.frame1.frame2.frame4.font
  pack unpack .xfMenuBar.frame1.frame2.frame4.fg
  pack unpack .xfMenuBar.frame1.frame2.frame4.label
  pack unpack .xfMenuBar.frame1.frame2.frame4.menu
  pack unpack .xfMenuBar.frame1.frame2.frame4.name
  pack unpack .xfMenuBar.frame1.frame2.frame4.offvalue
  pack unpack .xfMenuBar.frame1.frame2.frame4.onvalue
  pack unpack .xfMenuBar.frame1.frame2.frame4.size
  pack unpack .xfMenuBar.frame1.frame2.frame4.state
  pack unpack .xfMenuBar.frame1.frame2.frame4.textvar
  pack unpack .xfMenuBar.frame1.frame2.frame4.underline
  pack unpack .xfMenuBar.frame1.frame2.frame4.value
  pack unpack .xfMenuBar.frame1.frame2.frame4.variable

  # clear entry fields
  .xfMenuBar.frame1.frame2.frame4.accelerator.accelerator \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.actbg.actbg \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.actborder.actborder \
    set 0
  .xfMenuBar.frame1.frame2.frame4.actfg.actfg \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.bg.bg \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.bitmap.bitmap \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.border.border \
    set 0
  .xfMenuBar.frame1.frame2.frame4.command.command \
    delete 1.0 end
  .xfMenuBar.frame1.frame2.frame4.font.font \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.fg.fg \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.size.size2.size2 \
    set 0
  .xfMenuBar.frame1.frame2.frame4.label.label \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.menu.menu \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.name.name \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.offvalue.offvalue \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.onvalue.onvalue \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.state.normal \
    select
  .xfMenuBar.frame1.frame2.frame4.textvar.textvar \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.underline.underline \
    set -1
  .xfMenuBar.frame1.frame2.frame4.value.value \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.variable.variable \
    delete 0 end
  .xfMenuBar.frame1.frame2.frame4.size.size1.size1 \
    set 0

  case $xfMenuBarType in {
    {menubutton} {
      pack append .xfMenuBar.frame1.frame2.frame4 \
                  .xfMenuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.actfg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bitmap {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.border {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.font {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.fg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.label {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.menu {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.size {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.textvar {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.underline {top fillx}
    }
    {menu} {
      pack append .xfMenuBar.frame1.frame2.frame4 \
                  .xfMenuBar.frame1.frame2.frame4.name {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.actborder {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.actfg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.border {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.font {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.fg {top fillx}
    }
    {cascade} {
      .xfMenuBar.frame1.frame2.frame4.items.message1 configure \
        -text Cascadebutton

      pack append .xfMenuBar.frame1.frame2.frame4 \
                  .xfMenuBar.frame1.frame2.frame4.items {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.label {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.accelerator {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bitmap {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.font {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.menu {top fill} \
                  .xfMenuBar.frame1.frame2.frame4.state {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.underline {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.message1 {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.command {top fill}
    }
    {check} {
      .xfMenuBar.frame1.frame2.frame4.items.message1 configure \
        -text Checkbutton

      pack append .xfMenuBar.frame1.frame2.frame4 \
                  .xfMenuBar.frame1.frame2.frame4.items {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.label {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.accelerator {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bitmap {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.font {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.offvalue {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.onvalue {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.state {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.underline {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.variable {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.message1 {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.command {top fill}
    }
    {radio} {
      .xfMenuBar.frame1.frame2.frame4.items.message1 configure \
        -text Radiobutton

      pack append .xfMenuBar.frame1.frame2.frame4 \
                  .xfMenuBar.frame1.frame2.frame4.items {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.label {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.accelerator {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bitmap {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.font {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.state {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.underline {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.value {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.variable {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.message1 {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.command {top fill}
    }
    {separator} {
      .xfMenuBar.frame1.frame2.frame4.items.message1 configure \
        -text Separator

      pack append .xfMenuBar.frame1.frame2.frame4 \
                  .xfMenuBar.frame1.frame2.frame4.items {top fillx}
    }
    {default} {
      .xfMenuBar.frame1.frame2.frame4.items.message1 configure \
        -text Commandbutton

      pack append .xfMenuBar.frame1.frame2.frame4 \
                  .xfMenuBar.frame1.frame2.frame4.items {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.label {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.accelerator {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bg {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.bitmap {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.font {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.state {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.underline {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.message1 {top fillx} \
                  .xfMenuBar.frame1.frame2.frame4.command {top fill}
    }
  }
}

# eof

