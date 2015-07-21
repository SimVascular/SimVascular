# XFNoParsing
# Program: template
# Description: configure menus
#
# $Header: MenuBar.t[2.3] Wed Mar 10 12:03:30 1993 garfield@garfield frozen $

global menuBar
set menuBar(activeBackground) ""
set menuBar(activeForeground) ""
set menuBar(background) ""
set menuBar(font) ""
set menuBar(foreground) ""
set menuBar(scrollActiveForeground) ""
set menuBar(scrollBackground) ""
set menuBar(scrollForeground) ""
set menuBar(scrollSide) left
set menuBar(colorFile) "/usr/local/lib/tk/xf/lib/Colors"
set menuBar(fontFile) "/usr/local/lib/tk/xf/lib/Fonts"
set menuBar(file) ""
set menuBar(menuType) "<command> "
set menuBar(state) normal
set menuBar(userFile) ""

proc MenuBarInit {menuBarUserFile menuBarFile} {# xf ignore me 5
##########
# Procedure: MenuBarInit
# Description: initialize the configuration of menubuttons and menus
# Arguments: menuBarUserFile - the user specific loadfile
#            menuBarFile - the default loadfile
# Returns: none
# Sideeffects: none
##########

  global menuBar

  set menuBar(initialized) 1
  set menuBar(file) $menuBarFile
  set menuBar(userFile) $menuBarUserFile
  if {[file exists $menuBar(userFile)]} {
    if {[catch "source \"$menuBar(userFile)\"" menuBarResult]} {
      puts stderr $menuBarResult
    }
  } {
    if {[file exists $menuBar(file)]} {
      if {[catch "source \"$menuBar(file)\"" menuBarResult]} {
        puts stderr $menuBarResult
      }
    }
  }
}

proc MenuBarConf {menuBarConfig} {# xf ignore me 5
##########
# Procedure: MenuBarConf
# Description: configure the menubuttons and menus of
#              the given pathname
# Arguments: menuBarConfig - the widget pathname we configure
# Returns: none
# Sideeffects: none
##########
# 
# global menuBar(activeBackground) - active background color
# global menuBar(activeForeground) - active foreground color
# global menuBar(background) - background color
# global menuBar(font) - text font
# global menuBar(foreground) - foreground color
# global menuBar(scrollActiveForeground) - scrollbar active background color
# global menuBar(scrollBackground) - scrollbar background color
# global menuBar(scrollForeground) - scrollbar foreground color
# global menuBar(scrollSide) - side where scrollbar is located

  global menuBar

  if {![info exists menuBar(initialized)]} {
    return
  }
  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$menuBar(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$menuBar(activeBackground)\" "
  }
  if {"$menuBar(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$menuBar(activeForeground)\" "
  }
  if {"$menuBar(background)" != ""} {
    append tmpButtonOpt "-background \"$menuBar(background)\" "
    append tmpFrameOpt "-background \"$menuBar(background)\" "
    append tmpMessageOpt "-background \"$menuBar(background)\" "
    append tmpScaleOpt "-background \"$menuBar(background)\" "
  }
  if {"$menuBar(font)" != ""} {
    append tmpButtonOpt "-font \"$menuBar(font)\" "
    append tmpMessageOpt "-font \"$menuBar(font)\" "
  }
  if {"$menuBar(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$menuBar(foreground)\" "
    append tmpMessageOpt "-foreground \"$menuBar(foreground)\" "
    append tmpScaleOpt "-foreground \"$menuBar(foreground)\" "
  }
  if {"$menuBar(scrollActiveForeground)" != ""} {
    append tmpScaleOpt "-activeforeground \"$menuBar(scrollActiveForeground)\" "
    append tmpScrollOpt "-activeforeground \"$menuBar(scrollActiveForeground)\" "
  }
  if {"$menuBar(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$menuBar(scrollBackground)\" "
  }
  if {"$menuBar(scrollForeground)" != ""} {
    append tmpScaleOpt "-sliderforeground \"$menuBar(scrollForeground)\" "
    append tmpScrollOpt "-foreground \"$menuBar(scrollForeground)\" "
  }

  # start build of toplevel
  if {"[info commands XFDestroy]" != ""} {
    catch {XFDestroy .menuBar}
  } {
    catch {destroy .menuBar}
  }
  toplevel .menuBar \
    -borderwidth 0
  catch ".menuBar config $tmpFrameOpt"
  wm geometry .menuBar 530x400
  wm title .menuBar {Menu configuration}
  wm maxsize .menuBar 1000 1000
  wm minsize .menuBar 100 100
  # end build of toplevel

  frame .menuBar.frame1 \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1 config $tmpFrameOpt"
 
  frame .menuBar.frame1.frame2 \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2 config $tmpFrameOpt"
 
  frame .menuBar.frame1.frame2.frame4 \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame4 config $tmpFrameOpt"
 
  frame .menuBar.frame1.frame2.frame5 \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame5 config $tmpFrameOpt"
 
  frame .menuBar.frame1.frame2.frame6 \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame6 config $tmpFrameOpt"
 
  frame .menuBar.frame1.frame2.frame8 \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame8 config $tmpFrameOpt"
 
  frame .menuBar.frame1.frame7 \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame7 config $tmpFrameOpt"
 
  frame .menuBar.frame1.frame3 \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame3 config $tmpFrameOpt"
 
  scrollbar .menuBar.frame1.frame2.frame5.vscroll \
    -relief raised \
    -command ".menuBar.frame1.frame2.frame5.buttons yview"
  catch ".menuBar.frame1.frame2.frame5.vscroll config $tmpScrollOpt"

  scrollbar .menuBar.frame1.frame2.frame5.hscroll \
    -orient horiz \
    -relief raised \
    -command ".menuBar.frame1.frame2.frame5.buttons xview"
  catch ".menuBar.frame1.frame2.frame5.hscroll config $tmpScrollOpt"

  listbox .menuBar.frame1.frame2.frame5.buttons \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".menuBar.frame1.frame2.frame5.hscroll set" \
    -yscrollcommand ".menuBar.frame1.frame2.frame5.vscroll set"
  catch ".menuBar.frame1.frame2.frame5.buttons config $tmpMessageOpt"

  scrollbar .menuBar.frame1.frame2.frame6.vscroll \
    -relief raised \
    -command ".menuBar.frame1.frame2.frame6.menus yview"
  catch ".menuBar.frame1.frame2.frame6.vscroll config $tmpScrollOpt"

  scrollbar .menuBar.frame1.frame2.frame6.hscroll \
    -orient horiz \
    -relief raised \
    -command ".menuBar.frame1.frame2.frame6.menus xview"
  catch ".menuBar.frame1.frame2.frame6.hscroll config $tmpScrollOpt"

  listbox .menuBar.frame1.frame2.frame6.menus \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".menuBar.frame1.frame2.frame6.hscroll set" \
    -yscrollcommand ".menuBar.frame1.frame2.frame6.vscroll set"
  catch ".menuBar.frame1.frame2.frame6.menus config $tmpMessageOpt"

  scrollbar .menuBar.frame1.frame2.frame8.vscroll \
    -relief raised \
    -command ".menuBar.frame1.frame2.frame8.menu yview"
  catch ".menuBar.frame1.frame2.frame8.vscroll config $tmpScrollOpt"

  scrollbar .menuBar.frame1.frame2.frame8.hscroll \
    -orient horiz \
    -relief raised \
    -command ".menuBar.frame1.frame2.frame8.menu xview"
  catch ".menuBar.frame1.frame2.frame8.hscroll config $tmpScrollOpt"

  listbox .menuBar.frame1.frame2.frame8.menu \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".menuBar.frame1.frame2.frame8.hscroll set" \
    -yscrollcommand ".menuBar.frame1.frame2.frame8.vscroll set"
  catch ".menuBar.frame1.frame2.frame8.menu config $tmpMessageOpt"

  scale .menuBar.frame1.frame2.frame8.mover \
    -orient vertical \
    -width 8 \
    -relief raised \
    -sliderlength 15 \
    -from 0 \
    -command "MenuBarReposition"
  catch ".menuBar.frame1.frame2.frame8.mover config $tmpScaleOpt"

  button .menuBar.frame1.frame7.insert \
    -text "Append" \
    -command "MenuBarInsert \"$menuBarConfig\""
  catch ".menuBar.frame1.frame7.insert config $tmpButtonOpt"

  button .menuBar.frame1.frame7.modify \
    -text "Modify" \
    -command "MenuBarModify \"$menuBarConfig\""
  catch ".menuBar.frame1.frame7.modify config $tmpButtonOpt"

  button .menuBar.frame1.frame7.modifymenu \
    -text "Modify menu" \
    -command {
      set curSelected [.menuBar.frame1.frame2.frame6.menus curselection]
      if {$curSelected >= 0} {
        place forget .menuBar.frame1.frame2.frame5
        place forget .menuBar.frame1.frame2.frame6
        place .menuBar.frame1.frame2.frame8 \
          -in .menuBar.frame1.frame2 \
          -relx 0.5 \
          -rely 0 \
          -relheight 1.0 \
          -relwidth 0.5
        pack unpack .menuBar.frame1.frame7.modifymenu
        pack append .menuBar.frame1.frame7 \
                    .menuBar.frame1.frame7.insert {left fill expand} \
                    .menuBar.frame1.frame7.modify {left fill expand} \
                    .menuBar.frame1.frame7.modifyback {left fill expand} \
                    .menuBar.frame1.frame7.delete {left fill expand}
        update idletask
        MenuBarSetItems command
        MenuBarReadMenu \
          [.menuBar.frame1.frame2.frame6.menus get $curSelected]
        case [lindex [.menuBar.frame1.frame2.frame4.items.message1 config -text] 4] in {
          {Cascadebutton} {
            .menuBar.frame1.frame2.frame4.items.items.m invoke 0
          }
          {Checkbutton} {
            .menuBar.frame1.frame2.frame4.items.items.m invoke 1
          }
          {Radiobutton} {
            .menuBar.frame1.frame2.frame4.items.items.m invoke 3
          }
          {Separator} {
            .menuBar.frame1.frame2.frame4.items.items.m invoke 4
          }
          {default} {
            .menuBar.frame1.frame2.frame4.items.items.m invoke 2
          }
        }
        MenuBarSetItem
      }}
  catch ".menuBar.frame1.frame7.modifymenu config $tmpButtonOpt"

  button .menuBar.frame1.frame7.modifyback \
    -text "Back to main" \
    -command {
      place forget .menuBar.frame1.frame2.frame8
      if {[.menuBar.frame1.frame2.frame5.buttons size] > 0} {
        place .menuBar.frame1.frame2.frame5 \
          -in .menuBar.frame1.frame2 \
          -relx 0.5 \
          -rely 0 \
          -relheight 0.5 \
          -relwidth 0.5
        place .menuBar.frame1.frame2.frame6 \
          -in .menuBar.frame1.frame2 \
          -relx 0.5 \
          -rely 0.5 \
          -relheight 0.5 \
          -relwidth 0.5
      } {
        place .menuBar.frame1.frame2.frame6 \
          -in .menuBar.frame1.frame2 \
          -relx 0.5 \
          -rely 0 \
          -relheight 1 \
          -relwidth 0.5
      }
      update idletask
      pack unpack .menuBar.frame1.frame7.modifyback
      pack append .menuBar.frame1.frame7 \
                  .menuBar.frame1.frame7.insert {left fill expand} \
                  .menuBar.frame1.frame7.modify {left fill expand} \
                  .menuBar.frame1.frame7.modifymenu {left fill expand} \
                  .menuBar.frame1.frame7.delete {left fill expand}
      MenuBarSetItem}
  catch ".menuBar.frame1.frame7.modifyback config $tmpButtonOpt"

  button .menuBar.frame1.frame7.delete \
    -text "Remove" \
    -command "
      if {\[.menuBar.frame1.frame2.frame6.menus size\] > 0} {
        MenuBarDelete \"$menuBarConfig\"
      }"
  catch ".menuBar.frame1.frame7.delete config $tmpButtonOpt"

  button .menuBar.frame1.frame3.ok \
    -text "OK" \
    -command "
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .menuBar}
      } {
        catch {destroy .menuBar}
      }"
  catch ".menuBar.frame1.frame3.ok config $tmpButtonOpt"

  button .menuBar.frame1.frame3.save \
    -text "Save" \
    -command "MenuBarSave \"$menuBarConfig\""
  catch ".menuBar.frame1.frame3.save config $tmpButtonOpt"

  frame .menuBar.frame1.frame2.frame4.items \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame4.items config $tmpFrameOpt"
 
  menubutton .menuBar.frame1.frame2.frame4.items.items \
    -text "Menu entry:" \
    -menu ".menuBar.frame1.frame2.frame4.items.items.m"
  catch ".menuBar.frame1.frame2.frame4.items.items config $tmpButtonOpt"

  menu .menuBar.frame1.frame2.frame4.items.items.m
  catch ".menuBar.frame1.frame2.frame4.items.items.m config $tmpButtonOpt"

  .menuBar.frame1.frame2.frame4.items.items.m \
    add radiobutton \
      -label "Cascadebutton" \
      -variable menuBar(menuType) \
      -value "<cascade> " \
      -command "MenuBarSetItems cascade"
  .menuBar.frame1.frame2.frame4.items.items.m \
    add radiobutton \
      -label "Checkbutton" \
      -variable menuBar(menuType) \
      -value "<checkbutton> " \
      -command "MenuBarSetItems check"
  .menuBar.frame1.frame2.frame4.items.items.m \
    add radiobutton \
      -label "Commandbutton" \
      -variable menuBar(menuType) \
      -value "<command> " \
      -command "MenuBarSetItems command"
  .menuBar.frame1.frame2.frame4.items.items.m \
    add radiobutton \
      -label "Radiobutton" \
      -variable menuBar(menuType) \
      -value "<radiobutton> " \
      -command "MenuBarSetItems radio"
  .menuBar.frame1.frame2.frame4.items.items.m \
    add radiobutton \
      -label "Separator" \
      -variable menuBar(menuType) \
      -value "<separator> " \
      -command "MenuBarSetItems separator"
  
  label .menuBar.frame1.frame2.frame4.items.message1 \
    -anchor w \
    -text "Commandbutton"
  catch ".menuBar.frame1.frame2.frame4.items.message1 config $tmpMessageOpt"
  
  MenuBarEntry accelerator "Accelerator:"
  MenuBarEntry actbg "Active background:"
  MenuBarScale actborder "Active borderwidth:" "pixels" 40
  MenuBarEntry actfg "Active foreground:"
  MenuBarEntry bg "Background:"
  MenuBarEntry bitmap "Bitmap:"
  MenuBarScale border "Borderwidth:" "pixels" 40
  MenuBarEntry font "Font:"
  MenuBarEntry fg "Foreground:"
  MenuBarEntry label "Label:"
  MenuBarEntry menu "Menu:"
  MenuBarEntry name "Menu name:"
  MenuBarEntry offvalue "Offvalue:"
  MenuBarEntry onvalue "Onvalue:"
  MenuBarScaleDouble size "Size:" "Width" "Height" 300 300

  frame .menuBar.frame1.frame2.frame4.state \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame4.state config $tmpFrameOpt"
 
  label .menuBar.frame1.frame2.frame4.state.message1 \
    -anchor w \
    -relief raised \
    -text "State:"
  catch ".menuBar.frame1.frame2.frame4.state.message1 config $tmpMessageOpt"

  radiobutton .menuBar.frame1.frame2.frame4.state.normal \
    -text "Normal" \
    -variable menuBar(state)
  catch ".menuBar.frame1.frame2.frame4.state.normal config $tmpButtonOpt"

  radiobutton .menuBar.frame1.frame2.frame4.state.active \
    -text "Active" \
    -variable menuBar(state)
  catch ".menuBar.frame1.frame2.frame4.state.active config $tmpButtonOpt"

  radiobutton .menuBar.frame1.frame2.frame4.state.disabled \
    -text "Disabled" \
    -variable menuBar(state)
  catch ".menuBar.frame1.frame2.frame4.state.disabled config $tmpButtonOpt"

  pack append .menuBar.frame1.frame2.frame4.state \
              .menuBar.frame1.frame2.frame4.state.message1 {left} \
              .menuBar.frame1.frame2.frame4.state.normal {left fillx expand} \
              .menuBar.frame1.frame2.frame4.state.active {left fillx expand} \
              .menuBar.frame1.frame2.frame4.state.disabled {left fillx expand}

  MenuBarEntry textvar "Text variable:"

  MenuBarScale underline "Underline:" "" 40
  .menuBar.frame1.frame2.frame4.underline.underline config \
    -from -1

  MenuBarEntry value "Value:"
  MenuBarEntry variable "Variable:"

  label .menuBar.frame1.frame2.frame4.message1 \
    -anchor c \
    -relief raised \
    -text "Command:"
  catch ".menuBar.frame1.frame2.frame4.message1 config $tmpMessageOpt"
  
  frame .menuBar.frame1.frame2.frame4.command \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame4.command config $tmpFrameOpt"

  text .menuBar.frame1.frame2.frame4.command.command \
    -relief raised \
    -wrap none \
    -borderwidth 2 \
    -yscrollcommand ".menuBar.frame1.frame2.frame4.command.vscroll set"
  catch ".menuBar.frame1.frame2.frame4.command.command config $tmpMessageOpt"

  scrollbar .menuBar.frame1.frame2.frame4.command.vscroll \
    -relief raised \
    -command ".menuBar.frame1.frame2.frame4.command.command yview"
  catch ".menuBar.frame1.frame2.frame4.command.vscroll config $tmpScrollOpt"

  pack append .menuBar.frame1.frame2.frame4.command \
              .menuBar.frame1.frame2.frame4.command.vscroll {left filly} \
              .menuBar.frame1.frame2.frame4.command.command {left fill expand}

  # bindings
  bind .menuBar.frame1.frame2.frame5.buttons <ButtonPress-1> {
   MenuBarSelect1 %W %y}
  bind .menuBar.frame1.frame2.frame5.buttons <Button1-Motion> {
   MenuBarSelect1 %W %y}
  bind .menuBar.frame1.frame2.frame5.buttons <Shift-ButtonPress-1> {
   MenuBarSelect1 %W %y}
  bind .menuBar.frame1.frame2.frame5.buttons <Shift-Button1-Motion> {
   MenuBarSelect1 %W %y}

  bind .menuBar.frame1.frame2.frame6.menus <ButtonPress-1> {
   MenuBarSelect1 %W %y}
  bind .menuBar.frame1.frame2.frame6.menus <Button1-Motion> {
   MenuBarSelect1 %W %y}
  bind .menuBar.frame1.frame2.frame6.menus <Shift-ButtonPress-1> {
   MenuBarSelect1 %W %y}
  bind .menuBar.frame1.frame2.frame6.menus <Shift-Button1-Motion> {
   MenuBarSelect1 %W %y}

  bind .menuBar.frame1.frame2.frame8.menu <ButtonPress-1> {
   MenuBarSelect2 %W %y}
  bind .menuBar.frame1.frame2.frame8.menu <Button1-Motion> {
   MenuBarSelect2 %W %y}
  bind .menuBar.frame1.frame2.frame8.menu <Shift-ButtonPress-1> {
   MenuBarSelect2 %W %y}
  bind .menuBar.frame1.frame2.frame8.menu <Shift-Button1-Motion> {
   MenuBarSelect2 %W %y}

  bind .menuBar.frame1.frame2.frame4.actbg.actbg <Double-ButtonPress-3> {
   global menuBar
   if {"[info commands ColorBox]" != ""} {
     set menuBarResult [ColorBox $menuBar(colorFile) "Active background:"]
     if {"$menuBarResult" != ""} {
       .menuBar.frame1.frame2.frame4.actbg.actbg delete 0 end
       .menuBar.frame1.frame2.frame4.actbg.actbg insert end $menuBarResult
     }
   }}

  bind .menuBar.frame1.frame2.frame4.actfg.actfg <Double-ButtonPress-3> {
   global menuBar
   if {"[info commands ColorBox]" != ""} {
     set menuBarResult [ColorBox $menuBar(colorFile) "Active foreground:"]
     if {"$menuBarResult" != ""} {
       .menuBar.frame1.frame2.frame4.actfg.actfg delete 0 end
       .menuBar.frame1.frame2.frame4.actfg.actfg insert end $menuBarResult
     }
   }}

  bind .menuBar.frame1.frame2.frame4.bg.bg <Double-ButtonPress-3> {
   global menuBar
   if {"[info commands ColorBox]" != ""} {
     set menuBarResult [ColorBox $menuBar(colorFile) "Background:"]
     if {"$menuBarResult" != ""} {
       .menuBar.frame1.frame2.frame4.bg.bg delete 0 end
       .menuBar.frame1.frame2.frame4.bg.bg insert end $menuBarResult
     }
   }}

  bind .menuBar.frame1.frame2.frame4.bitmap.bitmap <Double-ButtonPress-3> {
   if {"[info commands FSBox]" != ""} {
     set menuBarResult [FSBox]
     if {"$menuBarResult" != ""} {
       .menuBar.frame1.frame2.frame4.bitmap.bitmap delete 0 end
       .menuBar.frame1.frame2.frame4.bitmap.bitmap insert end $menuBarResult
     }
   }}

  bind .menuBar.frame1.frame2.frame4.font.font <Double-ButtonPress-3> {
   global menuBar
   if {"[info commands FontBox]" != ""} {
     set menuBarResult [FontBox $menuBar(fontFile)]
     if {"$menuBarResult" != ""} {
       .menuBar.frame1.frame2.frame4.font.font delete 0 end
       .menuBar.frame1.frame2.frame4.font.font insert end $menuBarResult
     }
   }}

  bind .menuBar.frame1.frame2.frame4.fg.fg <Double-ButtonPress-3> {
   global menuBar
   if {"[info commands ColorBox]" != ""} {
     set menuBarResult [ColorBox $menuBar(colorFile) "Foreground:"]
     if {"$menuBarResult" != ""} {
       .menuBar.frame1.frame2.frame4.fg.fg delete 0 end
       .menuBar.frame1.frame2.frame4.fg.fg insert end $menuBarResult
     }
   }}

  MenuBarReadMenus $menuBarConfig

  # packing
  pack append .menuBar.frame1.frame7 \
              .menuBar.frame1.frame7.insert {left fill expand} \
              .menuBar.frame1.frame7.modify {left fill expand} \
              .menuBar.frame1.frame7.modifymenu {left fill expand} \
              .menuBar.frame1.frame7.delete {left fill expand}
  pack append .menuBar.frame1.frame3 \
              .menuBar.frame1.frame3.ok {left fill expand} \
              .menuBar.frame1.frame3.save {left fill expand}
  pack append .menuBar.frame1.frame2.frame4.items \
              .menuBar.frame1.frame2.frame4.items.items {left fill} \
              .menuBar.frame1.frame2.frame4.items.message1 {left fill expand}
  pack append .menuBar.frame1.frame2.frame4 \
              .menuBar.frame1.frame2.frame4.actbg {top fillx} \
              .menuBar.frame1.frame2.frame4.actfg {top fillx} \
              .menuBar.frame1.frame2.frame4.bg {top fillx} \
              .menuBar.frame1.frame2.frame4.bitmap {top fillx} \
              .menuBar.frame1.frame2.frame4.border {top fillx} \
              .menuBar.frame1.frame2.frame4.font {top fillx} \
              .menuBar.frame1.frame2.frame4.fg {top fillx} \
              .menuBar.frame1.frame2.frame4.label {top fillx} \
              .menuBar.frame1.frame2.frame4.menu {top fillx} \
              .menuBar.frame1.frame2.frame4.size {top fillx} \
              .menuBar.frame1.frame2.frame4.textvar {top fillx} \
              .menuBar.frame1.frame2.frame4.underline {top fillx}
  pack append .menuBar.frame1.frame2.frame5 \
              .menuBar.frame1.frame2.frame5.vscroll "$menuBar(scrollSide) filly" \
              .menuBar.frame1.frame2.frame5.hscroll {bottom fillx} \
              .menuBar.frame1.frame2.frame5.buttons {left fill expand}
  pack append .menuBar.frame1.frame2.frame6 \
              .menuBar.frame1.frame2.frame6.vscroll "$menuBar(scrollSide) filly" \
              .menuBar.frame1.frame2.frame6.hscroll {bottom fillx} \
              .menuBar.frame1.frame2.frame6.menus {left fill expand}
  pack append .menuBar.frame1.frame2.frame8 \
              .menuBar.frame1.frame2.frame8.mover {right filly} \
              .menuBar.frame1.frame2.frame8.vscroll "$menuBar(scrollSide) filly" \
              .menuBar.frame1.frame2.frame8.hscroll {bottom fillx} \
              .menuBar.frame1.frame2.frame8.menu {left fill expand}

  place .menuBar.frame1.frame2.frame4 \
    -in .menuBar.frame1.frame2 \
    -relx 0 \
    -rely 0 \
    -relheight 1.0 \
    -relwidth 0.5

  if {[.menuBar.frame1.frame2.frame5.buttons size] > 0} {
    .menuBar.frame1.frame7.insert config \
      -state disabled
    .menuBar.frame1.frame7.modifymenu config \
      -state disabled
    .menuBar.frame1.frame7.delete config \
      -state disabled

    place .menuBar.frame1.frame2.frame5 \
      -in .menuBar.frame1.frame2 \
      -relx 0.5 \
      -rely 0 \
      -relheight 0.5 \
      -relwidth 0.5

    place .menuBar.frame1.frame2.frame6 \
      -in .menuBar.frame1.frame2 \
      -relx 0.5 \
      -rely 0.5 \
      -relheight 0.5 \
      -relwidth 0.5
    MenuBarSetItems menubutton
  } {
    .menuBar.frame1.frame7.insert config \
      -state normal
    .menuBar.frame1.frame7.modifymenu config \
      -state normal
    .menuBar.frame1.frame7.delete config \
      -state normal

    place .menuBar.frame1.frame2.frame6 \
      -in .menuBar.frame1.frame2 \
      -relx 0.5 \
      -rely 0 \
      -relheight 1 \
      -relwidth 0.5
    MenuBarSetItems menu
  }

  pack append .menuBar.frame1 \
              .menuBar.frame1.frame3 {bottom fillx} \
              .menuBar.frame1.frame7 {bottom fillx} \
              .menuBar.frame1.frame2 {top fill expand}
  pack append .menuBar \
              .menuBar.frame1 {top fill expand}

  update idletask
  MenuBarSetItem
}

##########
# Procedure: MenuBarEntry
# Description: build labled entry window
# Arguments: menuBarName - the name of the widget we want to build
#            menuBarLabel - the label
# Returns: none
# Sideeffects: none
##########
proc MenuBarEntry {menuBarName menuBarLabel} {# xf ignore me 6
  global menuBar

  set tmpFrameOpt ""
  set tmpMessageOpt ""
  if {"$menuBar(background)" != ""} {
    append tmpFrameOpt "-background \"$menuBar(background)\" "
    append tmpMessageOpt "-background \"$menuBar(background)\" "
  }
  if {"$menuBar(font)" != ""} {
    append tmpMessageOpt "-font \"$menuBar(font)\" "
  }
  if {"$menuBar(foreground)" != ""} {
    append tmpMessageOpt "-foreground \"$menuBar(foreground)\" "
  }

  # build widgets
  frame .menuBar.frame1.frame2.frame4.$menuBarName \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame4.$menuBarName config $tmpFrameOpt"

  label .menuBar.frame1.frame2.frame4.$menuBarName.label$menuBarName \
    -relief raised \
    -text "$menuBarLabel"
  catch ".menuBar.frame1.frame2.frame4.$menuBarName.label$menuBarName config $tmpMessageOpt"

  entry .menuBar.frame1.frame2.frame4.$menuBarName.$menuBarName \
    -relief raised
  catch ".menuBar.frame1.frame2.frame4.$menuBarName.$menuBarName config $tmpMessageOpt"

  # packing of the subwidgets
  pack append .menuBar.frame1.frame2.frame4.$menuBarName \
              .menuBar.frame1.frame2.frame4.$menuBarName.label$menuBarName {left filly} \
              .menuBar.frame1.frame2.frame4.$menuBarName.$menuBarName {left fill expand}
}

##########
# Procedure: MenuBarDelete
# Description: delete a menu, or a menu item
# Arguments: menuBarConfig - the menubar we configure
# Returns: none
# Sideeffects: none
##########
proc MenuBarDelete {menuBarConfig} {# xf ignore me 6

  set curSelected \
    [.menuBar.frame1.frame2.frame6.menus curselection]
  if {[winfo ismapped .menuBar.frame1.frame2.frame6] && $curSelected >= 0} {
    if {"[info commands XFDestroy]" != ""} {
      catch "XFDestroy [.menuBar.frame1.frame2.frame6.menus get $curSelected]"
    } {
      catch "destroy [.menuBar.frame1.frame2.frame6.menus get $curSelected]"
    }
    MenuBarReadMenus $menuBarConfig
  } {
    set curSelected \
      [.menuBar.frame1.frame2.frame8.menu curselection]
    if {[winfo ismapped .menuBar.frame1.frame2.frame8] && $curSelected >= 0} {
      [.menuBar.frame1.frame2.frame6.menus get [.menuBar.frame1.frame2.frame6.menus curselection]] delete $curSelected
      MenuBarReadMenu \
        [.menuBar.frame1.frame2.frame6.menus get [.menuBar.frame1.frame2.frame6.menus curselection]]
    }
  }
}

##########
# Procedure: MenuBarEntryType
# Description: determine the type of a menu item
# Arguments: menuBarW - the widget
#            menuBarPosition - index of item in menu
# Returns: item type
# Sideeffects: none
##########
proc MenuBarEntryType {menuBarW menuBarPosition} {# xf ignore me 6

  if {![catch "$menuBarW entryconfig $menuBarPosition -menu"]} {
    return "cascade"
  } {
    if {![catch "$menuBarW entryconfig $menuBarPosition -value"]} {
      return "radiobutton"
    } {
      if {![catch "$menuBarW entryconfig $menuBarPosition -onvalue"]} {
        return "checkbutton"
      } {
        if {![catch "$menuBarW entryconfig $menuBarPosition -command"]} {
          return "command"
        } {
          return "separator"
        }
      }
    }
  }
}

##########
# Procedure: MenuBarInsert
# Description: insert a menu, or a menu item
# Arguments: menuBarConfig - the menubar configure
# Returns: none
# Sideeffects: none
##########
proc MenuBarInsert {menuBarConfig} {# xf ignore me 6
  global menuBar

  if {[winfo ismapped .menuBar.frame1.frame2.frame6]} {
    if {"[.menuBar.frame1.frame2.frame4.name.name get]" != ""} {
      if {"[info commands XFDestroy]" != ""} {
        catch "XFDestroy [.menuBar.frame1.frame2.frame4.name.name get]"
      } {
        catch "destroy [.menuBar.frame1.frame2.frame4.name.name get]"
      }
      if {"[info commands [.menuBar.frame1.frame2.frame4.menu.menu get]]" == ""} {
        if {[catch "menu [.menuBar.frame1.frame2.frame4.menu.menu get]" menuBarResult]} {
          puts stderr "$menuBarResult"
        } {
          .menuBar.frame1.frame2.frame6.menus insert end \
            "[.menuBar.frame1.frame2.frame4.name.name get]"
          .menuBar.frame1.frame2.frame6.menus select clear 0 end
          .menuBar.frame1.frame2.frame6.menus select set \
            [.menuBar.frame1.frame2.frame6.menus size]
          MenuBarModify $menuBarConfig
          MenuBarReadMenus $menuBarConfig
        }
      }
    }
  } {
    set currType \
      [string trim [string trim $menuBar(menuType)] "<>"]
    if {[winfo ismapped .menuBar.frame1.frame2.frame8]} {
      if {[catch "[.menuBar.frame1.frame2.frame6.menus get [.menuBar.frame1.frame2.frame6.menus curselection]] add $currType" menuBarResult]} {
        puts stderr $menuBarResult
      } {
        .menuBar.frame1.frame2.frame8.menu insert end \
          "$menuBar(menuType) [.menuBar.frame1.frame2.frame4.name.name get]"
        .menuBar.frame1.frame2.frame8.menu select clear 0 end
        .menuBar.frame1.frame2.frame8.menu select set \
          [.menuBar.frame1.frame2.frame8.menu size]
        MenuBarModify $menuBarConfig
        MenuBarReadMenu \
          [.menuBar.frame1.frame2.frame6.menus get [.menuBar.frame1.frame2.frame6.menus curselection]]
      }
    }
  }
}

##########
# Procedure: MenuBarInsertMenus
# Description: read all child menus
# Arguments: menuBarW - the current menu
# Returns: none
# Sideeffects: listbox gets filled
##########
proc MenuBarInsertMenus {menuBarW} {# xf ignore me 6

  if {"[winfo class $menuBarW]" == "Menu"} {
    .menuBar.frame1.frame2.frame6.menus insert end $menuBarW
  }

  set last [$menuBarW index last]
  if {"$last" == "none"} {
    set last -1
  }
  for {set counter 0} {$counter <= $last} {incr counter 1} {
    if {"[MenuBarEntryType $menuBarW $counter]" == "cascade"} {
      if {"[lindex [$menuBarW entryconfig $counter -menu] 4]" != ""} {
        if {"[info commands [lindex [$menuBarW entryconfig $counter -menu] 4]]" == ""} {
          $menuBarW entryconfig $counter -menu ""
        }
      }
    }
  }
  foreach counter [lsort [winfo children $menuBarW]] {
    if {"[winfo class $counter]" == "Menu"} {
      MenuBarInsertMenus $counter
    }
  }
}

##########
# Procedure: MenuBarModify
# Description: modify a menubutton, menu, or a menu item
# Arguments: menuBarConfig - the menubar we configure
# Returns: none
# Sideeffects: none
##########
proc MenuBarModify {menuBarConfig} {# xf ignore me 6
  global menuBar

  set curSelected \
    [.menuBar.frame1.frame2.frame5.buttons curselection]
  if {[winfo ismapped .menuBar.frame1.frame2.frame5] && $curSelected >= 0} {
    set menuBarList .menuBar.frame1.frame2.frame5.buttons
    set menuBarW [$menuBarList get $curSelected]
    # insert values
    set optionString "config"
    if {"[.menuBar.frame1.frame2.frame4.actbg.actbg get]" != ""} {
      append optionString \
        " -activebackground \{[.menuBar.frame1.frame2.frame4.actbg.actbg get]\}"
    }
    if {"[.menuBar.frame1.frame2.frame4.actfg.actfg get]" != ""} {
      append optionString \
        " -activeforeground \{[.menuBar.frame1.frame2.frame4.actfg.actfg get]\}"
    }
    if {"[.menuBar.frame1.frame2.frame4.bg.bg get]" != ""} {
      append optionString \
        " -background \{[.menuBar.frame1.frame2.frame4.bg.bg get]\}"
    }
    if {"[.menuBar.frame1.frame2.frame4.bitmap.bitmap get]" != ""} {
      if {"[string index [.menuBar.frame1.frame2.frame4.bitmap.bitmap get] 0]" == "@"} {
        append optionString \
          " -bitmap \{[.menuBar.frame1.frame2.frame4.bitmap.bitmap get]\}"
      } {
        append optionString \
          " -bitmap \{@[.menuBar.frame1.frame2.frame4.bitmap.bitmap get]\}"
      }
    } {
      append optionString \
        " -bitmap \{\}"
    }
    append optionString \
      " -borderwidth [.menuBar.frame1.frame2.frame4.border.border get]"
    if {"[.menuBar.frame1.frame2.frame4.font.font get]" != ""} {
      append optionString \
        " -font \{[.menuBar.frame1.frame2.frame4.font.font get]\}"
    }
    if {"[.menuBar.frame1.frame2.frame4.fg.fg get]" != ""} {
      append optionString \
        " -foreground \{[.menuBar.frame1.frame2.frame4.fg.fg get]\}"
    }
    append optionString \
      " -menu \{[.menuBar.frame1.frame2.frame4.menu.menu get]\}"
    append optionString \
      " -text \{[.menuBar.frame1.frame2.frame4.label.label get]\}"
    append optionString \
      " -textvariable \{[.menuBar.frame1.frame2.frame4.textvar.textvar get]\}"
    append optionString \
      " -underline \{[.menuBar.frame1.frame2.frame4.underline.underline get]\}"
    if {[.menuBar.frame1.frame2.frame4.size.size1.size1 get] > 0 &&
	[.menuBar.frame1.frame2.frame4.size.size2.size2 get] > 0} {
      append optionString \
        " -width \{[.menuBar.frame1.frame2.frame4.size.size1.size1 get]\}"
      append optionString \
        " -height \{[.menuBar.frame1.frame2.frame4.size.size2.size2 get]\}"
    }
    if {[catch "$menuBarW $optionString" menuBarResult]} {
      puts stderr $menuBarResult
    }

    if {"[info commands [.menuBar.frame1.frame2.frame4.menu.menu get]]" == ""} {
      if {[catch "menu [.menuBar.frame1.frame2.frame4.menu.menu get]" menuBarResult]} {
        puts stderr "$menuBarResult"
      } {
        .menuBar.frame1.frame2.frame6.menus insert end \
          "[.menuBar.frame1.frame2.frame4.name.name get]"
        .menuBar.frame1.frame2.frame6.menus select clear 0 end
        .menuBar.frame1.frame2.frame6.menus select set \
          [.menuBar.frame1.frame2.frame6.menus size]
        MenuBarModify $menuBarConfig
        MenuBarReadMenus $menuBarConfig
      }
    }
  } {
    set curSelected \
      [.menuBar.frame1.frame2.frame6.menus curselection]
    if {[winfo ismapped .menuBar.frame1.frame2.frame6] && $curSelected >= 0} {
      set menuBarList .menuBar.frame1.frame2.frame6.menus
      set menuBarW [$menuBarList get $curSelected]
      # insert values
      set optionString "config"
      if {"[.menuBar.frame1.frame2.frame4.actbg.actbg get]" != ""} {
        append optionString \
          " -activebackground \{[.menuBar.frame1.frame2.frame4.actbg.actbg get]\}"
      }
      append optionString \
        " -activeborderwidth \{[.menuBar.frame1.frame2.frame4.actborder.actborder get]\}"
      if {"[.menuBar.frame1.frame2.frame4.actfg.actfg get]" != ""} {
        append optionString \
          " -activeforeground \{[.menuBar.frame1.frame2.frame4.actfg.actfg get]\}"
      }
      if {"[.menuBar.frame1.frame2.frame4.bg.bg get]" != ""} {
        append optionString \
          " -background \{[.menuBar.frame1.frame2.frame4.bg.bg get]\}"
      }
      append optionString \
        " -borderwidth \{[.menuBar.frame1.frame2.frame4.border.border get]\}"
      if {"[.menuBar.frame1.frame2.frame4.font.font get]" != ""} {
        append optionString \
          " -font \{[.menuBar.frame1.frame2.frame4.font.font get]\}"
      }
      if {"[.menuBar.frame1.frame2.frame4.fg.fg get]" != ""} {
        append optionString \
          " -foreground \{[.menuBar.frame1.frame2.frame4.fg.fg get]\}"
      }
      if {[catch "$menuBarW $optionString" menuBarResult]} {
        puts stderr $menuBarResult
      }
    } {
      set curSelected \
        [.menuBar.frame1.frame2.frame8.menu curselection]
      if {[winfo ismapped .menuBar.frame1.frame2.frame8] &&
          $curSelected >= 0} {
        set menuBarList .menuBar.frame1.frame2.frame6.menus
        set menuBarW \
          [$menuBarList get [.menuBar.frame1.frame2.frame6.menus curselection]]
        # insert values
        if {![string match <separator* [.menuBar.frame1.frame2.frame8.menu get $curSelected]]} {
          set optionString "entryconfig $curSelected"
          if {"[.menuBar.frame1.frame2.frame4.actbg.actbg get]" != ""} {
            append optionString \
              " -activebackground \{[.menuBar.frame1.frame2.frame4.actbg.actbg get]\}"
          }
          append optionString \
            " -accelerator \{[.menuBar.frame1.frame2.frame4.accelerator.accelerator get]\}"
          if {"[.menuBar.frame1.frame2.frame4.bg.bg get]" != ""} {
            append optionString \
              " -background \{[.menuBar.frame1.frame2.frame4.bg.bg get]\}"
          }
          if {"[.menuBar.frame1.frame2.frame4.bitmap.bitmap get]" != ""} {
            if {"[string index [.menuBar.frame1.frame2.frame4.bitmap.bitmap get] 0]" == "@"} {
              append optionString \
                " -bitmap \{[.menuBar.frame1.frame2.frame4.bitmap.bitmap get]\}"
            } {
              append optionString \
                " -bitmap \{@[.menuBar.frame1.frame2.frame4.bitmap.bitmap get]\}"
            }
          } {
            append optionString \
              " -bitmap \{\}"
          }
          append optionString \
            " -command \{[.menuBar.frame1.frame2.frame4.command.command get 1.0 end]\}"
          if {"[.menuBar.frame1.frame2.frame4.font.font get]" != ""} {
            append optionString \
              " -font \{[.menuBar.frame1.frame2.frame4.font.font get]\}"
          }
          append optionString \
            " -label \{[.menuBar.frame1.frame2.frame4.label.label get]\}"
          append optionString \
            " -state \{$menuBar(state)\}"
          append optionString \
            " -underline \{[.menuBar.frame1.frame2.frame4.underline.underline get]\}"
          case [.menuBar.frame1.frame2.frame8.menu get $curSelected] in {
            {<cascade*} {
              append optionString \
                " -menu \{[.menuBar.frame1.frame2.frame4.menu.menu get]\}"
            }
            {<radiobutton*} {
              append optionString \
                " -value \{[.menuBar.frame1.frame2.frame4.value.value get]\}"
              if {"[.menuBar.frame1.frame2.frame4.variable.variable get]" != ""} {
                append optionString \
                  " -variable \{[.menuBar.frame1.frame2.frame4.variable.variable get]\}"
              }
            }
            {<checkbutton*} {
              append optionString \
                " -offvalue \{[.menuBar.frame1.frame2.frame4.offvalue.offvalue get]\}"
              append optionString \
                " -onvalue \{[.menuBar.frame1.frame2.frame4.onvalue.onvalue get]\}"
              if {"[.menuBar.frame1.frame2.frame4.variable.variable get]" != ""} {
                append optionString \
                  " -variable \{[.menuBar.frame1.frame2.frame4.variable.variable get]\}"
              }
            }
          }
          if {[catch "$menuBarW $optionString" menuBarResult]} {
            puts stderr $menuBarResult
          }
        }
      }
    }
  }
}

##########
# Procedure: MenuBarReadMenu
# Description: read the items of the current menu
# Arguments: menuBarW - the name of the menu we want to edit
# Returns: none
# Sideeffects: none
##########
proc MenuBarReadMenu {menuBarW} {# xf ignore me 6

  if {[.menuBar.frame1.frame2.frame8.menu size] > 0} {
    .menuBar.frame1.frame2.frame8.menu delete 0 end
  }
  set last [$menuBarW index last]
  if {"$last" == "none"} {
    set last -1
  }
  for {set counter 0} {$counter <= $last} {incr counter 1} {
    if {"[MenuBarEntryType $menuBarW $counter]" == "separator"} {
      .menuBar.frame1.frame2.frame8.menu insert end \
        "<[MenuBarEntryType $menuBarW $counter]>"
    } {
      .menuBar.frame1.frame2.frame8.menu insert end \
        "<[MenuBarEntryType $menuBarW $counter]> [lindex [$menuBarW entryconfigure $counter -label] 4]"
    }
  }

  if {[.menuBar.frame1.frame2.frame8.menu size] > 0} {
    .menuBar.frame1.frame2.frame8.menu select clear 0 end
    .menuBar.frame1.frame2.frame8.menu select set 0
    MenuBarSetItem
    .menuBar.frame1.frame2.frame8.mover configure \
        -to [.menuBar.frame1.frame2.frame8.menu size]
  } {
    .menuBar.frame1.frame2.frame8.mover configure \
        -to 1
  }
}

##########
# Procedure: MenuBarReadMenus
# Description: read all menubuttons and menus
# Arguments: menuBarConfig - the menubar we configure
# Returns: none
# Sideeffects: none
##########
proc MenuBarReadMenus {menuBarConfig} {# xf ignore me 6

  if {[.menuBar.frame1.frame2.frame5.buttons size] > 0} {
    .menuBar.frame1.frame2.frame5.buttons delete 0 end
  }
  if {[.menuBar.frame1.frame2.frame6.menus size] > 0} {
    .menuBar.frame1.frame2.frame6.menus delete 0 end
  }
  if {![catch "winfo children $menuBarConfig" menuBarResult]} {
    foreach menuBarChild [lsort [split $menuBarResult]] {
      if {"[winfo class $menuBarChild]" == "Menubutton"} {
        if {"[lindex [$menuBarChild config -menu] 4]" != ""} {
          if {"[info commands [lindex [$menuBarChild config -menu] 4]]" == ""} {
            $menuBarChild config -menu ""
          }
        }
        .menuBar.frame1.frame2.frame5.buttons insert end $menuBarChild
        foreach menuBarMenuChild [lsort [winfo children $menuBarChild]] {
          MenuBarInsertMenus $menuBarMenuChild
        }
      }
      if {"[winfo class $menuBarChild]" == "Menu"} {
        MenuBarInsertMenus $menuBarChild
      }
    }
  }
  if {[.menuBar.frame1.frame2.frame5.buttons size] > 0} {
    .menuBar.frame1.frame2.frame5.buttons select clear 0 end
    .menuBar.frame1.frame2.frame5.buttons select set 0
    MenuBarSetItem
  } {
    .menuBar.frame1.frame2.frame6.menus select clear 0 end
    .menuBar.frame1.frame2.frame6.menus select set 0
    MenuBarSetItem
  }
}

##########
# Procedure: MenuBarReposition
# Description: reposition a menu item
# Arguments: menuBarPos - the new position of the item
# Returns: none
# Sideeffects: none
##########
proc MenuBarReposition {menuBarPos} {# xf ignore me 6

  set curSelected \
    [.menuBar.frame1.frame2.frame8.menu curselection]
  set menuBarW \
    [.menuBar.frame1.frame2.frame6.menus get [.menuBar.frame1.frame2.frame6.menus curselection]]
  if {$curSelected >= 0} {
    set menuBarLast [$menuBarW index last]
    if {"$menuBarLast" == "none"} {
      set menuBarLast -1
    }
    set counter 0
    set newMenu ""
    set currentItem "[MenuBarEntryType $menuBarW $curSelected] "
    foreach optCounter [$menuBarW entryconfig $curSelected] {
      if {[llength $optCounter] == 5} {
        if {"[lindex $optCounter 3]" != "[lindex $optCounter 4]"} {
          append currentItem " [lindex $optCounter 0] \{[lindex $optCounter 4]\}"
        }
      }
    }
    set counter 0
    while {$counter <= $menuBarLast} {
      if {$menuBarPos > $curSelected &&
          $counter == [expr $menuBarPos+1]} {
        lappend newMenu $currentItem
      }
      if {$menuBarPos <= $curSelected &&
          $counter == $menuBarPos} {
        lappend newMenu $currentItem
      }
      if {$counter == $curSelected} {
        incr counter
        continue
      }
      set newItem "[MenuBarEntryType $menuBarW $counter] "
      foreach optCounter [$menuBarW entryconfig $counter] {
        if {[llength $optCounter] == 5} {
          if {"[lindex $optCounter 3]" != "[lindex $optCounter 4]"} {
            append newItem " [lindex $optCounter 0] \{[lindex $optCounter 4]\}"
          }
        }
      }
      lappend newMenu $newItem
      incr counter
    }
    if {$menuBarPos > $curSelected} {
      lappend newMenu $currentItem
    }
    set counter 0
    while {$counter <= $menuBarLast} {
      $menuBarW delete 0
      incr counter
    }
    set counter 0
    while {$counter <= $menuBarLast} {
      if {[catch "$menuBarW add [lindex $newMenu $counter]" menuBarRes]} {
        puts stderr $menuBarRes
      }
      incr counter
    }
    MenuBarReadMenu $menuBarW
    .menuBar.frame1.frame2.frame8.menu select clear 0 end
    .menuBar.frame1.frame2.frame8.menu select set $menuBarPos
    MenuBarSetItem
  }
}

##########
# Procedure: MenuBarSave
# Description: save the current definition
# Arguments: menuBarConfig - the menubar we configure
# Returns: none
# Sideeffects: none
##########
proc MenuBarSave {menuBarConfig} {# xf ignore me 6
  global menuBar

  if {![catch "open $menuBar(userFile) w" outFile]} {
    foreach widgets [winfo children $menuBarConfig] {
      if {"[winfo class $widgets]" == "Menu"} {
        MenuBarSaveMenu $outFile $widgets
      }
      if {"[winfo class $widgets]" == "Menubutton"} {
        puts $outFile "$widgets config" nonewline
        foreach optCounter [$widgets config] {
          if {[llength $optCounter] == 5} {
            if {"[lindex $optCounter 3]" != "[lindex $optCounter 4]"} {
              puts $outFile " \\\n  [lindex $optCounter 0] \{[lindex $optCounter 4]\}" nonewline
            }
          }
        }
        puts $outFile ""
        puts $outFile ""
        foreach children [winfo children $widgets] {
          if {"[winfo class $children]" == "Menu"} {
            MenuBarSaveMenu $outFile $children
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
# Procedure: MenuBarSaveMenu
# Description: save the current definition
# Arguments: outFile - the output descriptor
#            menuBarW - the menu widget
# Returns: none
# Sideeffects: none
##########
proc MenuBarSaveMenu {outFile menuBarW} {# xf ignore me 6

  puts $outFile "if {\"\[info commands XFDestroy\]\" != \"\"} {"
  puts $outFile "  catch \"XFDestroy $menuBarW\""
  puts $outFile "} {"
  puts $outFile "  catch \"destroy $menuBarW\""
  puts $outFile "}"
  puts $outFile "menu $menuBarW" nonewline
  foreach optCounter [$menuBarW config] {
    if {[llength $optCounter] == 5} {
      if {"[lindex $optCounter 3]" != "[lindex $optCounter 4]"} {
        puts $outFile " \\\n  [lindex $optCounter 0] \{[lindex $optCounter 4]\}" nonewline
      }
    }
  }
  puts $outFile ""

  set menuLast [$menuBarW index last]
  if {"$menuLast" == "none"} {
    set menuLast -1
  }
  set counter 0
  while {$counter <= $menuLast} {
    puts $outFile "$menuBarW add [MenuBarEntryType $menuBarW $counter] " nonewline
    foreach optCounter [$menuBarW entryconfig $counter] {
      if {[llength $optCounter] == 5} {
        if {"[lindex $optCounter 3]" != "[lindex $optCounter 4]"} {
          puts $outFile " \\\n  [lindex $optCounter 0] \{[lindex $optCounter 4]\}" nonewline
        }
      }
    }
    puts $outFile ""
    incr counter
  }
  puts $outFile ""
  puts $outFile ""

  foreach children [winfo children $menuBarW] {
    if {"[winfo class $children]" == "Menu"} {
      MenuBarSaveMenu $outFile $children
    }
  }
}

##########
# Procedure: MenuBarScale
# Description: build labled scale
# Arguments: menuBarName - the name of the widget we want to build
#            menuBarLabel1 - the label left from scale
#            menuBarLabel2 - the label of the scale
#            menuBarTo - the maximal value of the scale
# Returns: none
# Sideeffects: none
##########
proc MenuBarScale {menuBarName menuBarLabel1 menuBarLabel2 menuBarTo} {# xf ignore me 6
  global menuBar

  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  if {"$menuBar(background)" != ""} {
    append tmpFrameOpt "-background \"$menuBar(background)\" "
    append tmpMessageOpt "-background \"$menuBar(background)\" "
    append tmpScaleOpt "-background \"$menuBar(background)\" "
  }
  if {"$menuBar(font)" != ""} {
    append tmpMessageOpt "-font \"$menuBar(font)\" "
  }
  if {"$menuBar(foreground)" != ""} {
    append tmpMessageOpt "-foreground \"$menuBar(foreground)\" "
    append tmpScaleOpt "-foreground \"$menuBar(foreground)\" "
  }
  if {"$menuBar(scrollActiveForeground)" != ""} {
    append tmpScaleOpt "-activeforeground \"$menuBar(scrollActiveForeground)\" "
  }
  if {"$menuBar(scrollForeground)" != ""} {
    append tmpScaleOpt "-sliderforeground \"$menuBar(scrollForeground)\" "
  }

  # build widgets
  frame .menuBar.frame1.frame2.frame4.$menuBarName \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame4.$menuBarName config $tmpFrameOpt"

  label .menuBar.frame1.frame2.frame4.$menuBarName.message1 \
    -relief raised \
    -text "$menuBarLabel1"
  catch ".menuBar.frame1.frame2.frame4.$menuBarName.message1 config $tmpMessageOpt"

  scale .menuBar.frame1.frame2.frame4.$menuBarName.$menuBarName \
    -from 0 \
    -label "$menuBarLabel2" \
    -orient horizontal \
    -relief raised \
    -sliderlength 15 \
    -to $menuBarTo \
    -width 8
  catch ".menuBar.frame1.frame2.frame4.$menuBarName.$menuBarName config $tmpScaleOpt"

  # packing of the subwidgets
  pack append .menuBar.frame1.frame2.frame4.$menuBarName \
              .menuBar.frame1.frame2.frame4.$menuBarName.message1 {left filly} \
              .menuBar.frame1.frame2.frame4.$menuBarName.$menuBarName {left fillx expand}
}

##########
# Procedure: MenuBarScaleDouble
# Description: build scale frame with two scales
# Arguments: menuBarName - the name of the scales
#            menuBarLabel1 - the label left from the scales
#            menuBarLabel2 - the label of the 1st scale
#            menuBarLabel3 - the label of the 2nd scale
#            menuBarTo1 - the maximal value of the 1st scale
#            menuBarTo2 - the maximal value of the 2nd scale
# Returns: none
# Sideeffects: none
##########
proc MenuBarScaleDouble {menuBarName menuBarLabel1 menuBarLabel2 menuBarLabel3 menuBarTo1 menuBarTo2} {# xf ignore me 6
  global menuBar

  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  if {"$menuBar(background)" != ""} {
    append tmpFrameOpt "-background \"$menuBar(background)\" "
    append tmpMessageOpt "-background \"$menuBar(background)\" "
    append tmpScaleOpt "-background \"$menuBar(background)\" "
  }
  if {"$menuBar(font)" != ""} {
    append tmpMessageOpt "-font \"$menuBar(font)\" "
  }
  if {"$menuBar(foreground)" != ""} {
    append tmpMessageOpt "-foreground \"$menuBar(foreground)\" "
    append tmpScaleOpt "-foreground \"$menuBar(foreground)\" "
  }
  if {"$menuBar(scrollActiveForeground)" != ""} {
    append tmpScaleOpt "-activeforeground \"$menuBar(scrollActiveForeground)\" "
  }
  if {"$menuBar(scrollForeground)" != ""} {
    append tmpScaleOpt "-sliderforeground \"$menuBar(scrollForeground)\" "
  }

  # build widgets
  frame .menuBar.frame1.frame2.frame4.$menuBarName \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame4.$menuBarName config $tmpFrameOpt"

  frame .menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}1 \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}1 config $tmpFrameOpt"

  frame .menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}2 \
    -borderwidth 0 \
    -relief raised
  catch ".menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}2 config $tmpFrameOpt"

  label .menuBar.frame1.frame2.frame4.$menuBarName.message1 \
    -relief raised \
    -text "$menuBarLabel1"
  catch ".menuBar.frame1.frame2.frame4.$menuBarName.message1 config $tmpMessageOpt"

  scale .menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}1.${menuBarName}1 \
    -from 0 \
    -label "$menuBarLabel2" \
    -orient horizontal \
    -relief raised \
    -sliderlength 15 \
    -to $menuBarTo1 \
    -width 8
  catch ".menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}1.${menuBarName}1 config $tmpScaleOpt"

  scale .menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}2.${menuBarName}2 \
    -from 0 \
    -label "$menuBarLabel3" \
    -orient horizontal \
    -relief raised \
    -sliderlength 15 \
    -to $menuBarTo2 \
    -width 8
  catch ".menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}2.${menuBarName}2 config $tmpScaleOpt"

  # packing of the subwidgets
  pack append .menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}1 \
              .menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}1.${menuBarName}1 {left fillx expand}
  pack append .menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}2 \
              .menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}2.${menuBarName}2 {left fillx expand}
  pack append .menuBar.frame1.frame2.frame4.$menuBarName \
              .menuBar.frame1.frame2.frame4.$menuBarName.message1 {left filly} \
              .menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}1 {left fillx expand} \
              .menuBar.frame1.frame2.frame4.$menuBarName.${menuBarName}2 {left fillx expand}
}

##########
# Procedure: MenuBarSelect1
# Description: select a specified menubutton or menu
# Arguments: menuBarW - the listbox
#            menuBarY - the y coordinate in listbox
# Returns: none
# Sideeffects: none
##########
proc MenuBarSelect1 {menuBarW menuBarY} {# xf ignore me 6

  .menuBar.frame1.frame2.frame5.buttons select clear
  .menuBar.frame1.frame2.frame6.menus select clear

  set menuBarNearest [$menuBarW nearest $menuBarY]
  if {$menuBarNearest >= 0} {
    $menuBarW select clear 0 end
    $menuBarW select set $menuBarNearest
    MenuBarSetItem
    if {"$menuBarW" == ".menuBar.frame1.frame2.frame5.buttons"} {
      .menuBar.frame1.frame7.insert config \
        -state disabled
      .menuBar.frame1.frame7.modifymenu config \
        -state disabled
      .menuBar.frame1.frame7.delete config \
        -state disabled
    } {
      .menuBar.frame1.frame7.insert config \
        -state normal
      .menuBar.frame1.frame7.modifymenu config \
        -state normal
      .menuBar.frame1.frame7.delete config \
        -state normal
    }
  }
}

##########
# Procedure: MenuBarSelect2
# Description: select a specified menu item
# Arguments: menuBarW - the listbox
#            menuBarY - the y coordinate in listbox
# Returns: none
# Sideeffects: none
##########
proc MenuBarSelect2 {menuBarW menuBarY} {# xf ignore me 6

  set menuBarNearest [$menuBarW nearest $menuBarY]
  if {$menuBarNearest >= 0} {
    $menuBarW select clear 0 end
    $menuBarW select set $menuBarNearest
    MenuBarSetItem
  }
}

##########
# Procedure: MenuBarSetItem
# Description: set item fields to values from currently selected item
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc MenuBarSetItem {} {# xf ignore me 6
  global menuBar

  set menuBarList ""
  set curSelected \
    [.menuBar.frame1.frame2.frame5.buttons curselection]
  if {[winfo ismapped .menuBar.frame1.frame2.frame5] && $curSelected >= 0} {
    set menuBarList .menuBar.frame1.frame2.frame5.buttons
    MenuBarSetItems menubutton
    set menuBarW [$menuBarList get $curSelected]
    # insert values
    .menuBar.frame1.frame2.frame4.actbg.actbg \
      insert 0 [lindex [$menuBarW config -activebackground] 4]
    .menuBar.frame1.frame2.frame4.actfg.actfg \
      insert 0 [lindex [$menuBarW config -activeforeground] 4]
    .menuBar.frame1.frame2.frame4.bg.bg \
      insert 0 [lindex [$menuBarW config -background] 4]
    .menuBar.frame1.frame2.frame4.bitmap.bitmap \
      insert 0 [lindex [$menuBarW config -bitmap] 4]
    if {"[lindex [$menuBarW config -borderwidth] 4]" != ""} {
      .menuBar.frame1.frame2.frame4.border.border \
        set [lindex [$menuBarW config -borderwidth] 4]
    }
    .menuBar.frame1.frame2.frame4.font.font \
      insert 0 [lindex [$menuBarW config -font] 4]
    .menuBar.frame1.frame2.frame4.fg.fg \
      insert 0 [lindex [$menuBarW config -foreground] 4]
    if {"[lindex [$menuBarW config -height] 4]" != ""} {
      .menuBar.frame1.frame2.frame4.size.size2.size2 \
        set [lindex [$menuBarW config -height] 4]
    }
    .menuBar.frame1.frame2.frame4.label.label \
      insert 0 [lindex [$menuBarW config -text] 4]
    if {"[lindex [$menuBarW config -menu] 4]" != ""} {
      .menuBar.frame1.frame2.frame4.menu.menu \
        delete 0 end
    }
    .menuBar.frame1.frame2.frame4.menu.menu \
      insert 0 [lindex [$menuBarW config -menu] 4]
    .menuBar.frame1.frame2.frame4.textvar.textvar \
      insert 0 [lindex [$menuBarW config -textvariable] 4]
    if {"[lindex [$menuBarW config -underline] 4]" != ""} {
      .menuBar.frame1.frame2.frame4.underline.underline \
        set [lindex [$menuBarW config -underline] 4]
    }
    if {"[lindex [$menuBarW config -width] 4]" != ""} {
      .menuBar.frame1.frame2.frame4.size.size1.size1 \
        set [lindex [$menuBarW config -width] 4]
    }
  } {
    set curSelected \
      [.menuBar.frame1.frame2.frame6.menus curselection]
    if {[winfo ismapped .menuBar.frame1.frame2.frame6] && $curSelected >= 0} {
      set menuBarList .menuBar.frame1.frame2.frame6.menus
      MenuBarSetItems menu
      set menuBarW [$menuBarList get $curSelected]
      # insert values
      .menuBar.frame1.frame2.frame4.name.name \
        insert 0 [.menuBar.frame1.frame2.frame6.menus get $curSelected]
      .menuBar.frame1.frame2.frame4.actbg.actbg \
        insert 0 [lindex [$menuBarW config -activebackground] 4]
      if {"[lindex [$menuBarW config -activeborderwidth] 4]" != ""} {
        .menuBar.frame1.frame2.frame4.actborder.actborder \
          set [lindex [$menuBarW config -activeborderwidth] 4]
      }
      .menuBar.frame1.frame2.frame4.actfg.actfg \
        insert 0 [lindex [$menuBarW config -activeforeground] 4]
      .menuBar.frame1.frame2.frame4.bg.bg \
        insert 0 [lindex [$menuBarW config -background] 4]
      if {"[lindex [$menuBarW config -borderwidth] 4]" != ""} {
        .menuBar.frame1.frame2.frame4.border.border \
          set [lindex [$menuBarW config -borderwidth] 4]
      }
      .menuBar.frame1.frame2.frame4.font.font \
        insert 0 [lindex [$menuBarW config -font] 4]
      .menuBar.frame1.frame2.frame4.fg.fg \
        insert 0 [lindex [$menuBarW config -foreground] 4]
    } {
      set curSelected \
        [.menuBar.frame1.frame2.frame8.menu curselection]
      if {[winfo ismapped .menuBar.frame1.frame2.frame8] &&
          $curSelected >= 0} {
        set menuBarList .menuBar.frame1.frame2.frame8.menu
        case [.menuBar.frame1.frame2.frame8.menu get $curSelected] in {
          {<cascade*} {
            set menuBar(menuType) "<cascade> "
            .menuBar.frame1.frame2.frame4.items.items.m \
              entryconfigure 0 -state active
            MenuBarSetItems cascade
          }
          {<check*} {
            set menuBar(menuType) "<checkbutton> "
            .menuBar.frame1.frame2.frame4.items.items.m \
              entryconfigure 1 -state active
            MenuBarSetItems check
          }
          {<radio*} {
            set menuBar(menuType) "<radiobutton> "
            .menuBar.frame1.frame2.frame4.items.items.m \
              entryconfigure 3 -state active
            MenuBarSetItems radio
          }
          {<separator*} {
            set menuBar(menuType) "<separator> "
            .menuBar.frame1.frame2.frame4.items.items.m \
              entryconfigure 4 -state active
            MenuBarSetItems separator
          }
          {default} {
            set menuBar(menuType) "<command> "
            .menuBar.frame1.frame2.frame4.items.items.m \
              entryconfigure 2 -state active
            MenuBarSetItems command
          }
        }
        # insert values
        if {![string match <separator* [.menuBar.frame1.frame2.frame8.menu get $curSelected]]} {
          set menuBarW [.menuBar.frame1.frame2.frame6.menus get [.menuBar.frame1.frame2.frame6.menus curselection]]
          .menuBar.frame1.frame2.frame4.accelerator.accelerator \
            insert 0 [lindex [$menuBarW entryconfig $curSelected -accelerator] 4]
          .menuBar.frame1.frame2.frame4.actbg.actbg \
            insert 0 [lindex [$menuBarW entryconfig $curSelected -activebackground] 4]
          .menuBar.frame1.frame2.frame4.bg.bg \
            insert 0 [lindex [$menuBarW entryconfig $curSelected -background] 4]
          .menuBar.frame1.frame2.frame4.bitmap.bitmap \
            insert 0 [lindex [$menuBarW entryconfig $curSelected -bitmap] 4]
          .menuBar.frame1.frame2.frame4.command.command delete 1.0 end
          .menuBar.frame1.frame2.frame4.command.command insert 1.0 \
            [lindex [$menuBarW entryconfig $curSelected -command] 4]
          .menuBar.frame1.frame2.frame4.font.font \
            insert 0 [lindex [$menuBarW entryconfig $curSelected -font] 4]
          .menuBar.frame1.frame2.frame4.label.label \
            insert 0 [lindex [$menuBarW entryconfig $curSelected -label] 4]
          set menuBar(state) \
            [lindex [$menuBarW entryconfig $curSelected -state] 4]
          case $menuBar(state) in {
            {active} {
              .menuBar.frame1.frame2.frame4.state.active select
            }
            {disabled} {
              .menuBar.frame1.frame2.frame4.state.disabled select
            }
            {default} {
              .menuBar.frame1.frame2.frame4.state.normal select
            }
          }
          if {"[lindex [$menuBarW entryconfig $curSelected -underline] 4]" != ""} {
            .menuBar.frame1.frame2.frame4.underline.underline \
              set [lindex [$menuBarW entryconfig $curSelected -underline] 4]
          }
          case [.menuBar.frame1.frame2.frame8.menu get $curSelected] in {
            {<cascade*} {
              if {"[lindex [$menuBarW entryconfig $curSelected -menu] 4]" != ""} {
                .menuBar.frame1.frame2.frame4.menu.menu \
                  delete 0 end
              }
              .menuBar.frame1.frame2.frame4.menu.menu \
                insert 0 [lindex [$menuBarW entryconfig $curSelected -menu] 4]
            }
            {<radiobutton*} {
              .menuBar.frame1.frame2.frame4.value.value \
                insert 0 [lindex [$menuBarW entryconfig $curSelected -value] 4]
              .menuBar.frame1.frame2.frame4.variable.variable \
                insert 0 [lindex [$menuBarW entryconfig $curSelected -variable] 4]
            }
            {<checkbutton*} {
              .menuBar.frame1.frame2.frame4.offvalue.offvalue \
                insert 0 [lindex [$menuBarW entryconfig $curSelected -offvalue] 4]
              .menuBar.frame1.frame2.frame4.onvalue.onvalue \
                insert 0 [lindex [$menuBarW entryconfig $curSelected -onvalue] 4]
              .menuBar.frame1.frame2.frame4.variable.variable \
                insert 0 [lindex [$menuBarW entryconfig $curSelected -variable] 4]
            }
          }
        }
      }
    }
  }
}

##########
# Procedure: MenuBarSetItems
# Description: set item fields for currently selected widget
# Arguments: menuBarType - the type we want to edit
# Returns: none
# Sideeffects: none
##########
proc MenuBarSetItems {menuBarType} {# xf ignore me 6

  # remove widgets
  pack unpack .menuBar.frame1.frame2.frame4.items
  pack unpack .menuBar.frame1.frame2.frame4.accelerator
  pack unpack .menuBar.frame1.frame2.frame4.actbg
  pack unpack .menuBar.frame1.frame2.frame4.actborder
  pack unpack .menuBar.frame1.frame2.frame4.actfg
  pack unpack .menuBar.frame1.frame2.frame4.bg
  pack unpack .menuBar.frame1.frame2.frame4.bitmap
  pack unpack .menuBar.frame1.frame2.frame4.border
  pack unpack .menuBar.frame1.frame2.frame4.message1
  pack unpack .menuBar.frame1.frame2.frame4.command
  pack unpack .menuBar.frame1.frame2.frame4.font
  pack unpack .menuBar.frame1.frame2.frame4.fg
  pack unpack .menuBar.frame1.frame2.frame4.label
  pack unpack .menuBar.frame1.frame2.frame4.menu
  pack unpack .menuBar.frame1.frame2.frame4.name
  pack unpack .menuBar.frame1.frame2.frame4.offvalue
  pack unpack .menuBar.frame1.frame2.frame4.onvalue
  pack unpack .menuBar.frame1.frame2.frame4.size
  pack unpack .menuBar.frame1.frame2.frame4.state
  pack unpack .menuBar.frame1.frame2.frame4.textvar
  pack unpack .menuBar.frame1.frame2.frame4.underline
  pack unpack .menuBar.frame1.frame2.frame4.value
  pack unpack .menuBar.frame1.frame2.frame4.variable

  # clear entry fields
  .menuBar.frame1.frame2.frame4.accelerator.accelerator \
    delete 0 end
  .menuBar.frame1.frame2.frame4.actbg.actbg \
    delete 0 end
  .menuBar.frame1.frame2.frame4.actborder.actborder \
    set 0
  .menuBar.frame1.frame2.frame4.actfg.actfg \
    delete 0 end
  .menuBar.frame1.frame2.frame4.bg.bg \
    delete 0 end
  .menuBar.frame1.frame2.frame4.bitmap.bitmap \
    delete 0 end
  .menuBar.frame1.frame2.frame4.border.border \
    set 0
  .menuBar.frame1.frame2.frame4.command.command \
    delete 1.0 end
  .menuBar.frame1.frame2.frame4.font.font \
    delete 0 end
  .menuBar.frame1.frame2.frame4.fg.fg \
    delete 0 end
  .menuBar.frame1.frame2.frame4.size.size2.size2 \
    set 0
  .menuBar.frame1.frame2.frame4.label.label \
    delete 0 end
  .menuBar.frame1.frame2.frame4.menu.menu \
    delete 0 end
  .menuBar.frame1.frame2.frame4.name.name \
    delete 0 end
  .menuBar.frame1.frame2.frame4.offvalue.offvalue \
    delete 0 end
  .menuBar.frame1.frame2.frame4.onvalue.onvalue \
    delete 0 end
  .menuBar.frame1.frame2.frame4.state.normal \
    select
  .menuBar.frame1.frame2.frame4.textvar.textvar \
    delete 0 end
  .menuBar.frame1.frame2.frame4.underline.underline \
    set -1
  .menuBar.frame1.frame2.frame4.value.value \
    delete 0 end
  .menuBar.frame1.frame2.frame4.variable.variable \
    delete 0 end
  .menuBar.frame1.frame2.frame4.size.size1.size1 \
    set 0

  case $menuBarType in {
    {menubutton} {
      pack append .menuBar.frame1.frame2.frame4 \
                  .menuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .menuBar.frame1.frame2.frame4.actfg {top fillx} \
                  .menuBar.frame1.frame2.frame4.bg {top fillx} \
                  .menuBar.frame1.frame2.frame4.bitmap {top fillx} \
                  .menuBar.frame1.frame2.frame4.border {top fillx} \
                  .menuBar.frame1.frame2.frame4.font {top fillx} \
                  .menuBar.frame1.frame2.frame4.fg {top fillx} \
                  .menuBar.frame1.frame2.frame4.label {top fillx} \
                  .menuBar.frame1.frame2.frame4.menu {top fillx} \
                  .menuBar.frame1.frame2.frame4.size {top fillx} \
                  .menuBar.frame1.frame2.frame4.textvar {top fillx} \
                  .menuBar.frame1.frame2.frame4.underline {top fillx}
    }
    {menu} {
      pack append .menuBar.frame1.frame2.frame4 \
                  .menuBar.frame1.frame2.frame4.name {top fillx} \
                  .menuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .menuBar.frame1.frame2.frame4.actborder {top fillx} \
                  .menuBar.frame1.frame2.frame4.actfg {top fillx} \
                  .menuBar.frame1.frame2.frame4.bg {top fillx} \
                  .menuBar.frame1.frame2.frame4.border {top fillx} \
                  .menuBar.frame1.frame2.frame4.font {top fillx} \
                  .menuBar.frame1.frame2.frame4.fg {top fillx}
    }
    {cascade} {
      .menuBar.frame1.frame2.frame4.items.message1 configure \
        -text Cascadebutton

      pack append .menuBar.frame1.frame2.frame4 \
                  .menuBar.frame1.frame2.frame4.items {top fillx} \
                  .menuBar.frame1.frame2.frame4.label {top fillx} \
                  .menuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .menuBar.frame1.frame2.frame4.accelerator {top fillx} \
                  .menuBar.frame1.frame2.frame4.bg {top fillx} \
                  .menuBar.frame1.frame2.frame4.bitmap {top fillx} \
                  .menuBar.frame1.frame2.frame4.font {top fillx} \
                  .menuBar.frame1.frame2.frame4.menu {top fill} \
                  .menuBar.frame1.frame2.frame4.state {top fillx} \
                  .menuBar.frame1.frame2.frame4.underline {top fillx} \
                  .menuBar.frame1.frame2.frame4.message1 {top fillx} \
                  .menuBar.frame1.frame2.frame4.command {top fill}
    }
    {check} {
      .menuBar.frame1.frame2.frame4.items.message1 configure \
        -text Checkbutton

      pack append .menuBar.frame1.frame2.frame4 \
                  .menuBar.frame1.frame2.frame4.items {top fillx} \
                  .menuBar.frame1.frame2.frame4.label {top fillx} \
                  .menuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .menuBar.frame1.frame2.frame4.accelerator {top fillx} \
                  .menuBar.frame1.frame2.frame4.bg {top fillx} \
                  .menuBar.frame1.frame2.frame4.bitmap {top fillx} \
                  .menuBar.frame1.frame2.frame4.font {top fillx} \
                  .menuBar.frame1.frame2.frame4.offvalue {top fillx} \
                  .menuBar.frame1.frame2.frame4.onvalue {top fillx} \
                  .menuBar.frame1.frame2.frame4.state {top fillx} \
                  .menuBar.frame1.frame2.frame4.underline {top fillx} \
                  .menuBar.frame1.frame2.frame4.variable {top fillx} \
                  .menuBar.frame1.frame2.frame4.message1 {top fillx} \
                  .menuBar.frame1.frame2.frame4.command {top fill}
    }
    {radio} {
      .menuBar.frame1.frame2.frame4.items.message1 configure \
        -text Radiobutton

      pack append .menuBar.frame1.frame2.frame4 \
                  .menuBar.frame1.frame2.frame4.items {top fillx} \
                  .menuBar.frame1.frame2.frame4.label {top fillx} \
                  .menuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .menuBar.frame1.frame2.frame4.accelerator {top fillx} \
                  .menuBar.frame1.frame2.frame4.bg {top fillx} \
                  .menuBar.frame1.frame2.frame4.bitmap {top fillx} \
                  .menuBar.frame1.frame2.frame4.font {top fillx} \
                  .menuBar.frame1.frame2.frame4.state {top fillx} \
                  .menuBar.frame1.frame2.frame4.underline {top fillx} \
                  .menuBar.frame1.frame2.frame4.value {top fillx} \
                  .menuBar.frame1.frame2.frame4.variable {top fillx} \
                  .menuBar.frame1.frame2.frame4.message1 {top fillx} \
                  .menuBar.frame1.frame2.frame4.command {top fill}
    }
    {separator} {
      .menuBar.frame1.frame2.frame4.items.message1 configure \
        -text Separator

      pack append .menuBar.frame1.frame2.frame4 \
                  .menuBar.frame1.frame2.frame4.items {top fillx}
    }
    {default} {
      .menuBar.frame1.frame2.frame4.items.message1 configure \
        -text Commandbutton

      pack append .menuBar.frame1.frame2.frame4 \
                  .menuBar.frame1.frame2.frame4.items {top fillx} \
                  .menuBar.frame1.frame2.frame4.label {top fillx} \
                  .menuBar.frame1.frame2.frame4.actbg {top fillx} \
                  .menuBar.frame1.frame2.frame4.accelerator {top fillx} \
                  .menuBar.frame1.frame2.frame4.bg {top fillx} \
                  .menuBar.frame1.frame2.frame4.bitmap {top fillx} \
                  .menuBar.frame1.frame2.frame4.font {top fillx} \
                  .menuBar.frame1.frame2.frame4.state {top fillx} \
                  .menuBar.frame1.frame2.frame4.underline {top fillx} \
                  .menuBar.frame1.frame2.frame4.message1 {top fillx} \
                  .menuBar.frame1.frame2.frame4.command {top fill}
    }
  }
}

# eof

