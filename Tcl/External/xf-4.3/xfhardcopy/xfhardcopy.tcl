#! /bin/sh
# The next line is executed by /bin/sh, but not Tcl \
  exec wish $0 ${1+"$@"}

# procedure to show window ShowWindow.top0
proc ShowWindow.top0 { args} {
# xf ignore me 7

  # build widget .top0
  if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy .top0"
  } {
    catch "destroy .top0"
  }
  toplevel .top0 

  # Window manager configurations
  wm positionfrom .top0 ""
  wm sizefrom .top0 ""
  wm geometry .top0 450x95
  wm maxsize .top0 1000 1000
  wm minsize .top0 10 10
  wm title .top0 {xfhardcopy}


  # build widget .top0.frame
  frame .top0.frame     -borderwidth {2}    -relief {raised}

  # build widget .top0.frame.frame
  frame .top0.frame.frame 

  # build widget .top0.frame.frame.scrollbar1
  scrollbar .top0.frame.frame.scrollbar1     -command {.top0.frame.frame.entry2 xview}    -orient {horizontal}    -width {11}

  # build widget .top0.frame.frame.entry2 - MAC modified this 4-14-96
  entry .top0.frame.frame.entry2     -relief {sunken}    -xscrollcommand {.top0.frame.frame.scrollbar1 set}

  # pack widget .top0.frame.frame
  pack append .top0.frame.frame     .top0.frame.frame.entry2 {top frame center expand fill}     .top0.frame.frame.scrollbar1 {top frame center fillx} 

  # build widget .top0.frame.label1
  label .top0.frame.label1     -text {Name:}

  # pack widget .top0.frame
  pack append .top0.frame     .top0.frame.label1 {left frame center filly}     .top0.frame.frame {top frame center fillx} 

  # build widget .top0.frame4
  frame .top0.frame4     -borderwidth {2}    -relief {raised}

  # build widget .top0.frame4.frame
  frame .top0.frame4.frame 

  # build widget .top0.frame4.frame.scrollbar1
  scrollbar .top0.frame4.frame.scrollbar1     -command {.top0.frame4.frame.entry2 xview}    -orient {horizontal}    -width {11}

  # build widget .top0.frame4.frame.entry2 - MAC modified this 4-14-96
  entry .top0.frame4.frame.entry2     -relief {sunken}    -xscrollcommand {.top0.frame4.frame.scrollbar1 set}

  # pack widget .top0.frame4.frame
  pack append .top0.frame4.frame     .top0.frame4.frame.entry2 {top frame center expand fill}     .top0.frame4.frame.scrollbar1 {top frame center fillx} 

  # build widget .top0.frame4.label1
  label .top0.frame4.label1     -text {Command:}

  # pack widget .top0.frame4
  pack append .top0.frame4     .top0.frame4.label1 {left frame center filly}     .top0.frame4.frame {top frame center fillx} 

  # build widget .top0.frame5
  frame .top0.frame5     -relief {raised}

  # build widget .top0.frame5.button6
  button .top0.frame5.button6     -command {HardcopyOk}    -text {Ok}

  # build widget .top0.frame5.button7
  button .top0.frame5.button7     -command {HardcopyAdd}    -text {Add}

  # build widget .top0.frame5.button8
  button .top0.frame5.button8     -command {HardcopyDelete}    -text {Delete}

  # build widget .top0.frame5.button9
  button .top0.frame5.button9     -command {HardcopySave}    -text {Save}

  # pack widget .top0.frame5
  pack append .top0.frame5     .top0.frame5.button6 {left frame center expand fill}     .top0.frame5.button7 {left frame center expand fill}     .top0.frame5.button8 {left frame center expand fill}     .top0.frame5.button9 {left frame center expand fill} 

  # pack widget .top0
  pack append .top0     .top0.frame {top frame center fill}     .top0.frame4 {top frame center fill}     .top0.frame5 {top frame center expand fill} 

  if {"[info procs XFEdit]" != ""} {
    XFEditSetShowWindows
    XFMiscBindWidgetTree .top0
  }

  .top0.frame.frame.entry2 insert end {}
  .top0.frame4.frame.entry2 insert end {}
}

proc DestroyWindow.top0 {} {# xf ignore me 7
  if {"[info procs XFEdit]" != ""} {
    if {"[info commands .top0]" != ""} {
      global xfShowWindow.top0
      set xfShowWindow.top0 0
      XFEditSetPath .
      after 200 "XFSaveAsProc .top0; XFEditSetShowWindows"
    }
  } {
    catch "destroy .top0"
    update
  }
}


# procedure to show window .
proc ShowWindow. {args} {# xf ignore me 7

  # Window manager configurations
  wm positionfrom . user
  wm sizefrom . ""
  wm geometry . 400x600
  wm maxsize . 1024 1024
  wm minsize . 0 0
  wm title . {xfhardcopy to: ./xfHardCopy}


  # build widget .frame
  frame .frame 

  # build widget .frame.scrollbar2
  scrollbar .frame.scrollbar2 \
    -command {.frame.listbox1 yview} \
    -relief {raised}

  # build widget .frame.scrollbar3
  scrollbar .frame.scrollbar3 \
    -command {.frame.listbox1 xview} \
    -orient {horizontal} \
    -relief {raised}

  # build widget .frame.listbox1; (removed) -geometry {10x8}
  listbox .frame.listbox1 \
    -exportselection {0} \
    -relief {raised} \
    -xscrollcommand {.frame.scrollbar3 set} \
    -yscrollcommand {.frame.scrollbar2 set}
  # bindings
  bind .frame.listbox1 <B1-Motion> {SelectCommand %W %y}
  bind .frame.listbox1 <Button-1> {SelectCommand %W %y}
  bind .frame.listbox1 <Double-Button-1> {SelectCommand %W %y; Hardcopy}
  bind .frame.listbox1 <Shift-B1-Motion> {SelectCommand %W %y}
  bind .frame.listbox1 <Shift-Button-1> {SelectCommand %W %y}

  # pack widget .frame
  pack append .frame \
    .frame.scrollbar2 {left frame center filly} \
    .frame.listbox1 {top frame center expand fill} \
    .frame.scrollbar3 {bottom frame center fillx} 

  # build widget .frame0
  frame .frame0 \
    -relief {raised}

  # build widget .frame0.label1
  label .frame0.label1 \
    -relief {raised} \
    -text {Status:}

  # build widget .frame0.label2
  label .frame0.label2 \
    -anchor {w} \
    -relief {raised} \
    -text {Rescanning widget tree for xf...done}

  # pack widget .frame0
  pack append .frame0 \
    .frame0.label1 {left frame center fillx} \
    .frame0.label2 {left frame center expand fillx} 

  # build widget .frame1
  frame .frame1 \
    -borderwidth {2} \
    -relief {raised}

  # build widget .frame1.menubutton3
  menubutton .frame1.menubutton3 \
    -menu {.frame1.menubutton3.m} \
    -text {File} \
    -underline {0}

  # build widget .frame1.menubutton3.m
  menu .frame1.menubutton3.m 
  .frame1.menubutton3.m add command \
    -command {Hardcopy} \
    -label {Hardcopy} \
    -underline {0}
  .frame1.menubutton3.m add command \
    -command {HardcopyTo} \
    -label {Hardcopy to ...} \
    -underline {9}
  .frame1.menubutton3.m add separator
  .frame1.menubutton3.m add command \
    -command {RescanApplications
RescanWidgets} \
    -label {Rescan} \
    -underline {0}
  .frame1.menubutton3.m add separator
  .frame1.menubutton3.m add command \
    -command {HandleHardcopies} \
    -label {Modify hardcopy command} \
    -underline {0}
  .frame1.menubutton3.m add separator
  .frame1.menubutton3.m add command \
    -command {QuitProgram} \
    -label {Quit} \
    -underline {0}

  # pack widget .frame1
  pack append .frame1 \
    .frame1.menubutton3 {left frame center} 

  # build widget .frame2
  frame .frame2 \
    -relief {raised}

  # build widget .frame2.frame
  frame .frame2.frame 

  # build widget .frame2.frame.scrollbar2
  scrollbar .frame2.frame.scrollbar2 \
    -command {.frame2.frame.listbox1 yview} \
    -relief {raised}

  # build widget .frame2.frame.scrollbar3
  scrollbar .frame2.frame.scrollbar3 \
    -command {.frame2.frame.listbox1 xview} \
    -orient {horizontal} \
    -relief {raised}

  # build widget .frame2.frame.label6
  label .frame2.frame.label6 \
    -relief {raised} \
    -text {Applications:}

  # build widget .frame2.frame.listbox1; (removed) -geometry {10x2}
  listbox .frame2.frame.listbox1 \
    -exportselection {0} \
    -relief {raised} \
    -xscrollcommand {.frame2.frame.scrollbar3 set} \
    -yscrollcommand {.frame2.frame.scrollbar2 set}
  # bindings
  bind .frame2.frame.listbox1 <B1-Motion> {SelectApplication %W %y}
  bind .frame2.frame.listbox1 <Button-1> {SelectApplication %W %y}
  bind .frame2.frame.listbox1 <Shift-B1-Motion> {SelectApplication %W %y}
  bind .frame2.frame.listbox1 <Shift-Button-1> {SelectApplication %W %y}

  # pack widget .frame2.frame
  pack append .frame2.frame \
    .frame2.frame.label6 {top frame center fillx} \
    .frame2.frame.scrollbar2 {left frame center filly} \
    .frame2.frame.listbox1 {top frame center expand fill} \
    .frame2.frame.scrollbar3 {bottom frame center fillx} 

  # build widget .frame2.frame4
  frame .frame2.frame4 

  # build widget .frame2.frame4.scrollbar2
  scrollbar .frame2.frame4.scrollbar2 \
    -command {.frame2.frame4.listbox1 yview} \
    -relief {raised}

  # build widget .frame2.frame4.scrollbar3
  scrollbar .frame2.frame4.scrollbar3 \
    -command {.frame2.frame4.listbox1 xview} \
    -orient {horizontal} \
    -relief {raised}

  # build widget .frame2.frame4.label5
  label .frame2.frame4.label5 \
    -relief {raised} \
    -text {Widget structure:}

  # build widget .frame2.frame4.listbox1; (removed) -geometry {10x2}
  listbox .frame2.frame4.listbox1 \
    -exportselection {0} \
    -relief {raised} \
    -xscrollcommand {.frame2.frame4.scrollbar3 set} \
    -yscrollcommand {.frame2.frame4.scrollbar2 set}
  # bindings
  bind .frame2.frame4.listbox1 <B1-Motion> {SelectWidget %W %y}
  bind .frame2.frame4.listbox1 <Button-1> {SelectWidget %W %y}
  bind .frame2.frame4.listbox1 <Double-Button-1> {ChangeWidget %W %y}
  bind .frame2.frame4.listbox1 <Shift-B1-Motion> {SelectWidget %W %y}
  bind .frame2.frame4.listbox1 <Shift-Button-1> {SelectWidget %W %y}

  # pack widget .frame2.frame4
  pack append .frame2.frame4 \
    .frame2.frame4.label5 {top frame center fillx} \
    .frame2.frame4.scrollbar2 {left frame center filly} \
    .frame2.frame4.listbox1 {top frame center expand fill} \
    .frame2.frame4.scrollbar3 {bottom frame center fillx} 

  # pack widget .frame2
  pack append .frame2 \
    .frame2.frame {left frame center expand fill} \
    .frame2.frame4 {left frame center expand fill} 

  # build widget .label7
  label .label7 \
    -relief {raised} \
    -text {Hardcopy command:}

  # pack widget .
  pack append . \
    .frame1 {top frame center fill} \
    .frame0 {top frame center fillx} \
    .frame2 {top frame center expand fill} \
    .label7 {top frame center fillx} \
    .frame {top frame center fill} 

  if {"[info procs XFEdit]" != ""} {
    XFEditSetShowWindows
    XFMiscBindWidgetTree .xfFSBox
  }

  tk_menuBar .frame1 .frame1.menubutton3

  .frame.listbox1 insert end {TCL Postscript color}
  .frame.listbox1 insert end {TCL Postscript gray}
  .frame.listbox1 insert end {TCL Postscript mono}
  .frame.listbox1 insert end {Color hardcopy to PS (click, no border)}
  .frame.listbox1 insert end {Color hardcopy to PS (id, no border)}
  .frame.listbox1 insert end {Color hardcopy to XPM3 (click, no border)}
  .frame.listbox1 insert end {Color hardcopy to XPM3 (id, no border)}
  .frame.listbox1 insert end {Floyd-Steinberg tp PS (click, no border)}
  .frame.listbox1 insert end {Floyd-Steinberg tp PS (id, no border)}
  .frame.listbox1 insert end {Floyd-Steinberg tp XPM3 (click, no border)}
  .frame.listbox1 insert end {Floyd-Steinberg tp XPM3 (id, no border)}
  .frame.listbox1 insert end {Hardcopy to PS (click, border, no dither)}
  .frame.listbox1 insert end {Hardcopy to PS (click, no border, no dither)}
  .frame.listbox1 insert end {Hardcopy to PS (id, border, no dither)}
  .frame.listbox1 insert end {Hardcopy to PS (id, no border, no dither)}
  .frame.listbox1 insert end {Mono Hardcopy to PS (click, no border)}
  .frame.listbox1 insert end {Mono Hardcopy to PS (id, no border)}
  .frame.listbox1 insert end {Mono Hardcopy to XPM3 (click, no border)}
  .frame.listbox1 insert end {Mono Hardcopy to XPM3 (id, no border)}
  .frame.listbox1 insert end {Mono Hardcopy to bitmap (click, no border)}
  .frame.listbox1 insert end {Mono Hardcopy to bitmap (id, no border)}
  .frame.listbox1 insert end {XWD}
  .frame2.frame.listbox1 insert end {xf}
  .frame2.frame4.listbox1 insert end {. : Toplevel}
  .frame2.frame4.listbox1 insert end {.label7 : Label}
  .frame2.frame4.listbox1 insert end {.frame2 : Frame}
  .frame2.frame4.listbox1 insert end {.frame1 : Frame}
  .frame2.frame4.listbox1 insert end {.frame0 : Frame}
  .frame2.frame4.listbox1 insert end {.frame : Frame}
  .frame2.frame4.listbox1 insert end {.xfLoading : Toplevel}


}


# Procedure: AlertBox
proc AlertBox { {alertBoxMessage "Alert message"} {alertBoxCommand ""} {alertBoxGeometry "350x150"} {alertBoxTitle "Alert box"} args} {
# xf ignore me 5
##########
# Procedure: AlertBox
# Description: show alert box
# Arguments: {alertBoxMessage} - the text to display
#            {alertBoxCommand} - the command to call after ok
#            {alertBoxGeometry} - the geometry for the window
#            {alertBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          AlertBoxFile - to open and read a file automatically
#          AlertBoxFd - to read from an already opened filedescriptor
##########
#
# global alertBox(activeBackground) - active background color
# global alertBox(activeForeground) - active foreground color
# global alertBox(after) - destroy alert box after n seconds
# global alertBox(anchor) - anchor for message box
# global alertBox(background) - background color
# global alertBox(font) - message font
# global alertBox(foreground) - foreground color
# global alertBox(justify) - justify for message box
# global alertBox(toplevelName) - the toplevel name

  global alertBox

  # show alert box
  if {[llength $args] > 0} {
    eval AlertBoxInternal "\{$alertBoxMessage\}" "\{$alertBoxCommand\}" "\{$alertBoxGeometry\}" "\{$alertBoxTitle\}" $args
  } {
    AlertBoxInternal $alertBoxMessage $alertBoxCommand $alertBoxGeometry $alertBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $alertBox(toplevelName)
    tkwait window $alertBox(toplevelName)

    return $alertBox(button)
  }
}


# Procedure: AlertBoxInternal
proc AlertBoxInternal { alertBoxMessage alertBoxCommand alertBoxGeometry alertBoxTitle args} {
# xf ignore me 6
  global alertBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  if {"$alertBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$alertBox(activeBackground)\" "
  }
  if {"$alertBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$alertBox(activeForeground)\" "
  }
  if {"$alertBox(background)" != ""} {
    append tmpButtonOpt "-background \"$alertBox(background)\" "
    append tmpFrameOpt "-background \"$alertBox(background)\" "
    append tmpMessageOpt "-background \"$alertBox(background)\" "
  }
  if {"$alertBox(font)" != ""} {
    append tmpButtonOpt "-font \"$alertBox(font)\" "
    append tmpMessageOpt "-font \"$alertBox(font)\" "
  }
  if {"$alertBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$alertBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$alertBox(foreground)\" "
  }

  # start build of toplevel
  if {"[info commands XFDestroy]" != ""} {
    catch {XFDestroy $alertBox(toplevelName)}
  } {
    catch {destroy $alertBox(toplevelName)}
  }
  toplevel $alertBox(toplevelName)     -borderwidth 0
  catch "$alertBox(toplevelName) config $tmpFrameOpt"
  if {[catch "wm geometry $alertBox(toplevelName) $alertBoxGeometry"]} {
    wm geometry $alertBox(toplevelName) 350x150
  }
  wm title $alertBox(toplevelName) $alertBoxTitle
  wm maxsize $alertBox(toplevelName) 1000 1000
  wm minsize $alertBox(toplevelName) 100 100
  # end build of toplevel

  message $alertBox(toplevelName).message1     -anchor "$alertBox(anchor)"     -justify "$alertBox(justify)"     -relief raised     -text "$alertBoxMessage"
  catch "$alertBox(toplevelName).message1 config $tmpMessageOpt"

  set xfTmpWidth     [string range $alertBoxGeometry 0 [expr [string first x $alertBoxGeometry]-1]]
  if {"$xfTmpWidth" != ""} {
    # set message size
    catch "$alertBox(toplevelName).message1 configure       -width [expr $xfTmpWidth-10]"
  } {
    $alertBox(toplevelName).message1 configure       -aspect 1500
  }

  frame $alertBox(toplevelName).frame1     -borderwidth 0     -relief raised
  catch "$alertBox(toplevelName).frame1 config $tmpFrameOpt"

  set alertBoxCounter 0
  set buttonNum [llength $args]
  if {$buttonNum > 0} {
    while {$alertBoxCounter < $buttonNum} {
      button $alertBox(toplevelName).frame1.button$alertBoxCounter         -text "[lindex $args $alertBoxCounter]"         -command "
          global alertBox
          set alertBox(button) $alertBoxCounter
          if {\"\[info commands XFDestroy\]\" != \"\"} {
            catch {XFDestroy $alertBox(toplevelName)}
          } {
            catch {destroy $alertBox(toplevelName)}
          }"
      catch "$alertBox(toplevelName).frame1.button$alertBoxCounter config $tmpButtonOpt"

      pack append $alertBox(toplevelName).frame1                   $alertBox(toplevelName).frame1.button$alertBoxCounter {left fillx expand}

      incr alertBoxCounter
    }
  } {
    button $alertBox(toplevelName).frame1.button0       -text "OK"       -command "
        global alertBox
        set alertBox(button) 0
        if {\"\[info commands XFDestroy\]\" != \"\"} {
          catch {XFDestroy $alertBox(toplevelName)}
        } {
          catch {destroy $alertBox(toplevelName)}
        }
        $alertBoxCommand"
    catch "$alertBox(toplevelName).frame1.button0 config $tmpButtonOpt"

    pack append $alertBox(toplevelName).frame1                 $alertBox(toplevelName).frame1.button0 {left fillx expand}
  }

  # packing
  pack append $alertBox(toplevelName)               $alertBox(toplevelName).frame1 {bottom fill}               $alertBox(toplevelName).message1 {top fill expand}

  if {$alertBox(after) != 0} {
    after [expr $alertBox(after)*1000]       "catch \"$alertBox(toplevelName).frame1.button0 invoke\""
  }
}


# Procedure: Alias
proc Alias { args} {
# xf ignore me 7
##########
# Procedure: Alias
# Description: establish an alias for a procedure
# Arguments: args - no argument means that a list of all aliases
#                   is returned. Otherwise the first parameter is
#                   the alias name, and the second parameter is
#                   the procedure that is aliased.
# Returns: nothing, the command that is bound to the alias or a
#          list of all aliases - command pairs. 
# Sideeffects: internalAliasList is updated, and the alias
#              proc is inserted
##########
  global internalAliasList

  if {[llength $args] == 0} {
    return $internalAliasList
  } {
    if {[llength $args] == 1} {
      set xfTmpIndex [lsearch $internalAliasList "[lindex $args 0] *"]
      if {$xfTmpIndex != -1} {
        return [lindex [lindex $internalAliasList $xfTmpIndex] 1]
      }
    } {
      if {[llength $args] == 2} {
        eval "proc [lindex $args 0] {args} {#xf ignore me 4
return \[eval \"[lindex $args 1] \$args\"\]}"
        set xfTmpIndex [lsearch $internalAliasList "[lindex $args 0] *"]
        if {$xfTmpIndex != -1} {
          set internalAliasList [lreplace $internalAliasList $xfTmpIndex $xfTmpIndex "[lindex $args 0] [lindex $args 1]"]
        } {
          lappend internalAliasList "[lindex $args 0] [lindex $args 1]"
        }
      } {
        error "Alias: wrong number or args: $args"
      }
    }
  }
}


# Procedure: ChangeWidget
proc ChangeWidget { w y} {
  global currentWidget

  set nearest [$w nearest $y]
  $w selection clear 0 end
  $w selection set $nearest
  set currentWidget [lindex [split [[SymbolicName Widgets] get $nearest] :] 0]
  RescanWidgets
}


# Procedure: ClearList
proc ClearList { listWidget} {
  # Description: clear listbox widget
  # Arguments: listWidget - the widget to clear
  # Returns: none

  if {[$listWidget size] > 0} {
    $listWidget delete 0 end
  }
}


# Procedure: FSBox
proc FSBox { {fsBoxMessage "Select file:"} {fsBoxFileName ""} {fsBoxActionOk ""} {fsBoxActionCancel ""}} {
# xf ignore me 5
##########
# Procedure: FSBox
# Description: show file selector box
# Arguments: fsBoxMessage - the text to display
#            fsBoxFileName - a file name that should be selected
#            fsBoxActionOk - the action that should be performed on ok
#            fsBoxActionCancel - the action that should be performed on cancel
# Returns: the filename that was selected, or nothing
# Sideeffects: none
##########
# 
# global fsBox(activeBackground) - active background color
# global fsBox(activeForeground) - active foreground color
# global fsBox(background) - background color
# global fsBox(font) - text font
# global fsBox(foreground) - foreground color
# global fsBox(extensions) - scan directory for extensions
# global fsBox(scrollActiveForeground) - scrollbar active background color
# global fsBox(scrollBackground) - scrollbar background color
# global fsBox(scrollForeground) - scrollbar foreground color
# global fsBox(scrollSide) - side where scrollbar is located

  global fsBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$fsBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$fsBox(activeBackground)\" "
  }
  if {"$fsBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$fsBox(activeForeground)\" "
  }
  if {"$fsBox(background)" != ""} {
    append tmpButtonOpt "-background \"$fsBox(background)\" "
    append tmpFrameOpt "-background \"$fsBox(background)\" "
    append tmpMessageOpt "-background \"$fsBox(background)\" "
  }
  if {"$fsBox(font)" != ""} {
    append tmpButtonOpt "-font \"$fsBox(font)\" "
    append tmpMessageOpt "-font \"$fsBox(font)\" "
  }
  if {"$fsBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$fsBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$fsBox(foreground)\" "
  }
  if {"$fsBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$fsBox(scrollActiveForeground)\" "
  }
  if {"$fsBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$fsBox(scrollBackground)\" "
  }
  if {"$fsBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$fsBox(scrollForeground)\" "
  }

  if {[file exists [file tail $fsBoxFileName]] &&
      [IsAFile [file tail $fsBoxFileName]]} {
    set fsBox(name) [file tail $fsBoxFileName]
  } {
    set fsBox(name) ""
  }
  if {[file exists $fsBoxFileName] && [IsADir $fsBoxFileName]} {
    set fsBox(path) $fsBoxFileName
  } {
    if {"[file rootname $fsBoxFileName]" != "."} {
      set fsBox(path) [file rootname $fsBoxFileName]
    }
  }
  if {$fsBox(showPixmap)} {
    set fsBox(path) [string trimleft $fsBox(path) @]
  }
  if {"$fsBox(path)" != "" && [file exists $fsBox(path)] &&
      [IsADir $fsBox(path)]} {
    set fsBox(internalPath) $fsBox(path)
  } {
    if {"$fsBox(internalPath)" == "" ||
        ![file exists $fsBox(internalPath)]} {
      set fsBox(internalPath) [pwd]
    }
  }
  # build widget structure

  # start build of toplevel
  if {"[info commands XFDestroy]" != ""} {
    catch {XFDestroy .fsBox}
  } {
    catch {destroy .fsBox}
  }
  toplevel .fsBox     -borderwidth 0
  catch ".fsBox config $tmpFrameOpt"
  wm geometry .fsBox 350x300 
  wm title .fsBox {File select box}
  wm maxsize .fsBox 1000 1000
  wm minsize .fsBox 100 100
  # end build of toplevel

  label .fsBox.message1     -anchor c     -relief raised     -text "$fsBoxMessage"
  catch ".fsBox.message1 config $tmpMessageOpt"

  frame .fsBox.frame1     -borderwidth 0     -relief raised
  catch ".fsBox.frame1 config $tmpFrameOpt"

  button .fsBox.frame1.ok     -text "OK"     -command "
      global fsBox
      set fsBox(name) \[.fsBox.file.file get\]
      if {$fsBox(showPixmap)} {
        set fsBox(path) @\[.fsBox.path.path get\]
      } {
        set fsBox(path) \[.fsBox.path.path get\]
      }
      set fsBox(internalPath) \[.fsBox.path.path get\]
      $fsBoxActionOk
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .fsBox}
      } {
        catch {destroy .fsBox}
      }"
  catch ".fsBox.frame1.ok config $tmpButtonOpt"

  button .fsBox.frame1.rescan     -text "Rescan"     -command {
      global fsBox
      FSBoxFSShow [.fsBox.path.path get]         [.fsBox.pattern.pattern get] $fsBox(all)}
  catch ".fsBox.frame1.rescan config $tmpButtonOpt"

  button .fsBox.frame1.cancel     -text "Cancel"     -command "
      global fsBox
      set fsBox(name) {}
      set fsBox(path) {}
      $fsBoxActionCancel
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .fsBox}
      } {
        catch {destroy .fsBox}
      }"
  catch ".fsBox.frame1.cancel config $tmpButtonOpt"

  if {$fsBox(showPixmap)} {
    frame .fsBox.frame2       -borderwidth 0       -relief raised
    catch ".fsBox.frame2 config $tmpFrameOpt"

    scrollbar .fsBox.frame2.scrollbar3       -command {.fsBox.frame2.canvas2 xview}       -orient {horizontal}       -relief {raised}
    catch ".fsBox.frame2.scrollbar3 config $tmpScrollOpt"

    scrollbar .fsBox.frame2.scrollbar1       -command {.fsBox.frame2.canvas2 yview}       -relief {raised}
    catch ".fsBox.frame2.scrollbar1 config $tmpScrollOpt"

    canvas .fsBox.frame2.canvas2       -confine {true}       -relief {raised}       -scrollregion {0c 0c 20c 20c}       -width {100}       -xscrollcommand {.fsBox.frame2.scrollbar3 set}       -yscrollcommand {.fsBox.frame2.scrollbar1 set}
    catch ".fsBox.frame2.canvas2 config $tmpFrameOpt"

    .fsBox.frame2.canvas2 addtag currentBitmap withtag [.fsBox.frame2.canvas2 create bitmap 5 5 -anchor nw]
  }

  frame .fsBox.path     -borderwidth 0     -relief raised
  catch ".fsBox.path config $tmpFrameOpt"

  frame .fsBox.path.paths     -borderwidth 2     -relief raised
  catch ".fsBox.path.paths config $tmpFrameOpt"

  menubutton .fsBox.path.paths.paths     -borderwidth 0     -menu ".fsBox.path.paths.paths.menu"     -relief flat     -text "Pathname:"
  catch ".fsBox.path.paths.paths config $tmpButtonOpt"

  menu .fsBox.path.paths.paths.menu
  catch ".fsBox.path.paths.paths.menu config $tmpButtonOpt"

  .fsBox.path.paths.paths.menu add command      -label "[string trimright $fsBox(internalPath) {/@}]"      -command "
       global fsBox
       FSBoxFSShow \[.fsBox.path.path get\]          \[.fsBox.pattern.pattern get\] \$fsBox(all)
       .fsBox.path.path delete 0 end
       .fsBox.path.path insert 0 [string trimright $fsBox(internalPath) {/@}]"

  entry .fsBox.path.path     -relief raised
  catch ".fsBox.path.path config $tmpMessageOpt"

  if {![IsADir $fsBox(internalPath)]} {
    set $fsBox(internalPath) [pwd]
  }
  .fsBox.path.path insert 0 $fsBox(internalPath)

  frame .fsBox.pattern     -borderwidth 0     -relief raised
  catch ".fsBox.pattern config $tmpFrameOpt"

  frame .fsBox.pattern.patterns     -borderwidth 2     -relief raised
  catch ".fsBox.pattern.patterns config $tmpFrameOpt"

  menubutton .fsBox.pattern.patterns.patterns     -borderwidth 0     -menu ".fsBox.pattern.patterns.patterns.menu"     -relief flat     -text "Selection pattern:"
  catch ".fsBox.pattern.patterns.patterns config $tmpButtonOpt"

  menu .fsBox.pattern.patterns.patterns.menu
  catch ".fsBox.pattern.patterns.patterns.menu config $tmpButtonOpt"

  .fsBox.pattern.patterns.patterns.menu add checkbutton     -label "Scan extensions"     -variable fsBoxExtensions     -command {
      global fsBox
      FSBoxFSShow [.fsBox.path.path get]         [.fsBox.pattern.pattern get] $fsBox(all)}

  entry .fsBox.pattern.pattern     -relief raised
  catch ".fsBox.pattern.pattern config $tmpMessageOpt"

  .fsBox.pattern.pattern insert 0 $fsBox(pattern)
  
  frame .fsBox.files     -borderwidth 0     -relief raised
  catch ".fsBox.files config $tmpFrameOpt"

  scrollbar .fsBox.files.vscroll     -relief raised     -command ".fsBox.files.files yview"
  catch ".fsBox.files.vscroll config $tmpScrollOpt"

  scrollbar .fsBox.files.hscroll     -orient horiz     -relief raised     -command ".fsBox.files.files xview"
  catch ".fsBox.files.hscroll config $tmpScrollOpt"

  listbox .fsBox.files.files     -exportselection false     -relief raised     -xscrollcommand ".fsBox.files.hscroll set"     -yscrollcommand ".fsBox.files.vscroll set"
  catch ".fsBox.files.files config $tmpMessageOpt"

  frame .fsBox.file     -borderwidth 0     -relief raised
  catch ".fsBox.file config $tmpFrameOpt"

  label .fsBox.file.labelfile     -relief raised     -text "Filename:"
  catch ".fsBox.file.labelfile config $tmpMessageOpt"

  entry .fsBox.file.file     -relief raised
  catch ".fsBox.file.file config $tmpMessageOpt"

  .fsBox.file.file delete 0 end
  .fsBox.file.file insert 0 $fsBox(name)
  
  checkbutton .fsBox.pattern.all     -offvalue 0     -onvalue 1     -text "Show all files"     -variable fsBox(all)     -command {
      global fsBox
      FSBoxFSShow [.fsBox.path.path get]         [.fsBox.pattern.pattern get] $fsBox(all)}
  catch ".fsBox.pattern.all config $tmpButtonOpt"

  FSBoxFSShow $fsBox(internalPath) $fsBox(pattern) $fsBox(all)

  # bindings
  bind .fsBox.files.files <Double-Button-1> "
    FSBoxFSFileSelectDouble %W $fsBox(showPixmap) \{$fsBoxActionOk\} %y"
  bind .fsBox.files.files <ButtonPress-1> "
    FSBoxFSFileSelect %W $fsBox(showPixmap) %y"
  bind .fsBox.files.files <Button1-Motion> "
    FSBoxFSFileSelect %W $fsBox(showPixmap) %y"
  bind .fsBox.files.files <Shift-Button1-Motion> "
    FSBoxFSFileSelect %W $fsBox(showPixmap) %y"
  bind .fsBox.files.files <Shift-ButtonPress-1> "
    FSBoxFSFileSelect %W $fsBox(showPixmap) %y"

  bind .fsBox.path.path <Tab> {
    FSBoxFSNameComplete path}
  bind .fsBox.path.path <Return> {
    global fsBox
    FSBoxFSShow [.fsBox.path.path get]       [.fsBox.pattern.pattern get] $fsBox(all)
    FSBoxFSInsertPath
    .fsBox.file.file icursor end
    focus .fsBox.file.file}
  catch "bind .fsBox.path.path <Up> {}"
  bind .fsBox.path.path <Down> {
    .fsBox.file.file icursor end
    focus .fsBox.file.file}

  bind .fsBox.file.file <Tab> {
    FSBoxFSNameComplete file}
  bind .fsBox.file.file <Return> "
    global fsBox
    set fsBox(name) \[.fsBox.file.file get\]
    if {$fsBox(showPixmap)} {
      set fsBox(path) @\[.fsBox.path.path get\]
    } {
      set fsBox(path) \[.fsBox.path.path get\]
    }
    set fsBox(internalPath) \[.fsBox.path.path get\]
    $fsBoxActionOk
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .fsBox}
    } {
      catch {destroy .fsBox}
    }"
  bind .fsBox.file.file <Up> {
    .fsBox.path.path icursor end
    focus .fsBox.path.path}
  bind .fsBox.file.file <Down> {
    .fsBox.pattern.pattern icursor end
    focus .fsBox.pattern.pattern}

  bind .fsBox.pattern.pattern <Return> {
    global fsBox
    FSBoxFSShow [.fsBox.path.path get]       [.fsBox.pattern.pattern get] $fsBox(all)}
  bind .fsBox.pattern.pattern <Up> {
    .fsBox.file.file icursor end
    focus .fsBox.file.file}
  catch "bind .fsBox.pattern.pattern <Down> {}"

  # packing
  pack append .fsBox.files               .fsBox.files.vscroll "$fsBox(scrollSide) filly"               .fsBox.files.hscroll {bottom fillx}               .fsBox.files.files {left fill expand}
  pack append .fsBox.file               .fsBox.file.labelfile {left}               .fsBox.file.file {left fill expand}
  pack append .fsBox.frame1               .fsBox.frame1.ok {left fill expand}               .fsBox.frame1.rescan {left fill expand}               .fsBox.frame1.cancel {left fill expand}
  pack append .fsBox.path.paths               .fsBox.path.paths.paths {left}
  pack append .fsBox.pattern.patterns               .fsBox.pattern.patterns.patterns {left}
  pack append .fsBox.path               .fsBox.path.paths {left}               .fsBox.path.path {left fill expand}
  pack append .fsBox.pattern               .fsBox.pattern.patterns {left}               .fsBox.pattern.all {right fill}               .fsBox.pattern.pattern {left fill expand}
  if {$fsBox(showPixmap)} {
    pack append .fsBox.frame2                 .fsBox.frame2.scrollbar1 {left filly}                 .fsBox.frame2.canvas2 {top expand fill}                 .fsBox.frame2.scrollbar3 {top fillx} 

    pack append .fsBox                 .fsBox.message1 {top fill}                 .fsBox.frame1 {bottom fill}                 .fsBox.pattern {bottom fill}                 .fsBox.file {bottom fill}                 .fsBox.path {bottom fill}                 .fsBox.frame2 {right fill}                 .fsBox.files {left fill expand}
  } {
    pack append .fsBox                 .fsBox.message1 {top fill}                 .fsBox.frame1 {bottom fill}                 .fsBox.pattern {bottom fill}                 .fsBox.file {bottom fill}                 .fsBox.path {bottom fill}                 .fsBox.files {left fill expand}
  }

  if {"$fsBoxActionOk" == "" && "$fsBoxActionCancel" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .fsBox
    tkwait window .fsBox

    if {"[string trim $fsBox(path)]" != "" ||
        "[string trim $fsBox(name)]" != ""} {
      if {"[string trimleft [string trim $fsBox(name)] /]" == ""} {
        return [string trimright [string trim $fsBox(path)] /]
      } {
        return [string trimright [string trim $fsBox(path)] /]/[string trimleft [string trim $fsBox(name)] /]
      }
    }
  }
}


# Procedure: FSBoxBindSelectOne
proc FSBoxBindSelectOne { fsBoxW fsBoxY} {
# xf ignore me 6

  set fsBoxNearest [$fsBoxW nearest $fsBoxY]
  if {$fsBoxNearest >= 0} {
  $fsBoxW selection clear 0 end
  $fsBoxW selection set $fsBoxNearest
  }
}


# Procedure: FSBoxFSFileSelect
proc FSBoxFSFileSelect { fsBoxW fsBoxShowPixmap fsBoxY} {
# xf ignore me 6
  global fsBox

  FSBoxBindSelectOne $fsBoxW $fsBoxY
  set fsBoxNearest [$fsBoxW nearest $fsBoxY]
  if {$fsBoxNearest >= 0} {
    set fsBoxTmpEntry [$fsBoxW get $fsBoxNearest]
    if {"[string index $fsBoxTmpEntry           [expr [string length $fsBoxTmpEntry]-1]]" == "/" ||
        "[string index $fsBoxTmpEntry           [expr [string length $fsBoxTmpEntry]-1]]" == "@"} {
      set fsBoxFileName [string range $fsBoxTmpEntry 0             [expr [string length $fsBoxTmpEntry]-2]]
      if {![IsADir [string trimright $fsBox(internalPath)/$fsBoxFileName @]] &&
          ![IsASymlink [string trimright $fsBox(internalPath)/$fsBoxFileName @]]} {
        set fsBoxFileName $fsBoxTmpEntry
      }
    } {
      if {"[string index $fsBoxTmpEntry             [expr [string length $fsBoxTmpEntry]-1]]" == "*"} {
        set fsBoxFileName [string range $fsBoxTmpEntry 0           [expr [string length $fsBoxTmpEntry]-2]]
        if {![file executable $fsBox(internalPath)/$fsBoxFileName]} {
          set fsBoxFileName $fsBoxTmpEntry
        }
      } {
        set fsBoxFileName $fsBoxTmpEntry
      }
    }
    if {![IsADir [string trimright $fsBox(internalPath)/$fsBoxFileName @]]} {
      set fsBox(name) $fsBoxFileName
      .fsBox.file.file delete 0 end
      .fsBox.file.file insert 0 $fsBox(name)
      if {$fsBoxShowPixmap} {
        catch ".fsBox.frame2.canvas2 itemconfigure currentBitmap -bitmap \"@$fsBox(internalPath)/$fsBox(name)\""
      }
    }
  }
}


# Procedure: FSBoxFSFileSelectDouble
proc FSBoxFSFileSelectDouble { fsBoxW fsBoxShowPixmap fsBoxAction fsBoxY} {
# xf ignore me 6
  global fsBox

  FSBoxBindSelectOne $fsBoxW $fsBoxY
  set fsBoxNearest [$fsBoxW nearest $fsBoxY]
  if {$fsBoxNearest >= 0} {
    set fsBoxTmpEntry [$fsBoxW get $fsBoxNearest]
    if {"$fsBoxTmpEntry" == "../"} {
      set fsBoxTmpEntry [string trimright [string trim $fsBox(internalPath)] "@/"]
      if {"$fsBoxTmpEntry" == ""} {
        return
      }
      FSBoxFSShow [file dirname $fsBoxTmpEntry]         [.fsBox.pattern.pattern get] $fsBox(all)
      .fsBox.path.path delete 0 end
      .fsBox.path.path insert 0 $fsBox(internalPath)
    } {
      if {"[string index $fsBoxTmpEntry             [expr [string length $fsBoxTmpEntry]-1]]" == "/" ||
          "[string index $fsBoxTmpEntry             [expr [string length $fsBoxTmpEntry]-1]]" == "@"} {
        set fsBoxFileName [string range $fsBoxTmpEntry 0               [expr [string length $fsBoxTmpEntry]-2]]
        if {![IsADir [string trimright $fsBox(internalPath)/$fsBoxFileName @]] &&
            ![IsASymlink [string trimright $fsBox(internalPath)/$fsBoxFileName @]]} {
          set fsBoxFileName $fsBoxTmpEntry
        }
      } {
        if {"[string index $fsBoxTmpEntry               [expr [string length $fsBoxTmpEntry]-1]]" == "*"} {
          set fsBoxFileName [string range $fsBoxTmpEntry 0                 [expr [string length $fsBoxTmpEntry]-2]]
          if {![file executable $fsBox(internalPath)/$fsBoxFileName]} {
            set fsBoxFileName $fsBoxTmpEntry
          }
        } {
          set fsBoxFileName $fsBoxTmpEntry
        }
      }
      if {[IsADir [string trimright $fsBox(internalPath)/$fsBoxFileName @]]} {
        set fsBox(internalPath) "[string trimright $fsBox(internalPath) {/@}]/$fsBoxFileName"
        FSBoxFSShow $fsBox(internalPath)           [.fsBox.pattern.pattern get] $fsBox(all)
        .fsBox.path.path delete 0 end
        .fsBox.path.path insert 0 $fsBox(internalPath)
      } {
        set fsBox(name) $fsBoxFileName
        if {$fsBoxShowPixmap} {
          set fsBox(path) @$fsBox(internalPath)
        } {
          set fsBox(path) $fsBox(internalPath)
        }
        if {"$fsBoxAction" != ""} {
          eval "global fsBox; $fsBoxAction"
        }
        if {"[info commands XFDestroy]" != ""} {
          catch {XFDestroy .fsBox}
        } {
          catch {destroy .fsBox}
        }
      }
    }
  }
}


# Procedure: FSBoxFSInsertPath
proc FSBoxFSInsertPath {} {
# xf ignore me 6
  global fsBox

  set fsBoxLast [.fsBox.path.paths.paths.menu index last]
  set fsBoxNewEntry [string trimright [.fsBox.path.path get] "/@"]
  for {set fsBoxCounter 0} {$fsBoxCounter <= $fsBoxLast} {incr fsBoxCounter 1} {
    if {"$fsBoxNewEntry" ==           "[lindex [.fsBox.path.paths.paths.menu entryconfigure                     $fsBoxCounter -label] 4]"} {
      return
    }
  }
  if {$fsBoxLast < 9} {
    .fsBox.path.paths.paths.menu add command       -label "$fsBoxNewEntry"       -command "
        global fsBox
        FSBoxFSShow $fsBoxNewEntry           \[.fsBox.pattern.pattern get\] \$fsBox(all)
        .fsBox.path.path delete 0 end
        .fsBox.path.path insert 0 $fsBoxNewEntry"
  } {
    for {set fsBoxCounter 0} {$fsBoxCounter < $fsBoxLast} {incr fsBoxCounter 1} {
      .fsBox.path.paths.paths.menu entryconfigure         $fsBoxCounter -label           [lindex [.fsBox.path.paths.paths.menu entryconfigure             [expr $fsBoxCounter+1] -label] 4]
      .fsBox.path.paths.paths.menu entryconfigure $fsBoxCounter         -command "
          global fsBox
          FSBoxFSShow [lindex [.fsBox.path.paths.paths.menu entryconfigure             [expr $fsBoxCounter+1] -label] 4]             \[.fsBox.pattern.pattern get\] \$fsBox(all)
          .fsBox.path.path delete 0 end
          .fsBox.path.path insert 0 [lindex             [.fsBox.path.paths.paths.menu entryconfigure               [expr $fsBoxCounter+1] -label] 4]"
    }
    .fsBox.path.paths.paths.menu entryconfigure $fsBoxLast       -label "$fsBoxNewEntry"
    .fsBox.path.paths.paths.menu entryconfigure $fsBoxCounter       -command "
        global fsBox
        FSBoxFSShow \[.fsBox.path.path get\]           \[.fsBox.pattern.pattern get\] \$fsBox(all)
        .fsBox.path.path delete 0 end
        .fsBox.path.path insert 0 $fsBoxNewEntry"
  }
}


# Procedure: FSBoxFSNameComplete
proc FSBoxFSNameComplete { fsBoxType} {
# xf ignore me 6
  global fsBox

  set fsBoxNewFile ""
  if {"$fsBoxType" == "path"} {
    set fsBoxDirName [file dirname [.fsBox.path.path get]]
    set fsBoxFileName [file tail [.fsBox.path.path get]]
  } {
    set fsBoxDirName [file dirname [.fsBox.path.path get]/]
    set fsBoxFileName [file tail [.fsBox.file.file get]]
  }

  set fsBoxNewFile ""
  if {[IsADir [string trimright $fsBoxDirName @]]} {
    catch "glob -nocomplain $fsBoxDirName/${fsBoxFileName}*" fsBoxResult
    foreach fsBoxCounter $fsBoxResult {
      if {"$fsBoxNewFile" == ""} {
        set fsBoxNewFile [file tail $fsBoxCounter]
      } {
        if {"[string index [file tail $fsBoxCounter] 0]" !=
            "[string index $fsBoxNewFile 0]"} {
          set fsBoxNewFile ""
          break
        }
        set fsBoxCounter1 0
        set fsBoxTmpFile1 $fsBoxNewFile
        set fsBoxTmpFile2 [file tail $fsBoxCounter]
        set fsBoxLength1 [string length $fsBoxTmpFile1]
        set fsBoxLength2 [string length $fsBoxTmpFile2]
        set fsBoxNewFile ""
        if {$fsBoxLength1 > $fsBoxLength2} {
          set fsBoxLength1 $fsBoxLength2
        }
        while {$fsBoxCounter1 < $fsBoxLength1} {
          if {"[string index $fsBoxTmpFile1 $fsBoxCounter1]" ==                 "[string index $fsBoxTmpFile2 $fsBoxCounter1]"} {
            append fsBoxNewFile [string index $fsBoxTmpFile1 $fsBoxCounter1]
          } {
            break
          }
          incr fsBoxCounter1 1
        }
      }
    }
  }
  if {"$fsBoxNewFile" != ""} {
    if {[IsADir [string trimright $fsBoxDirName/$fsBoxNewFile @]] ||
        ![IsAFile [string trimright $fsBoxDirName/$fsBoxNewFile @]]} {
      if {[IsADir [string trimright $fsBoxDirName/$fsBoxNewFile @]]} {
        if {"$fsBoxDirName" == "/"} {
          .fsBox.path.path delete 0 end
          .fsBox.path.path insert 0 "/[string trimright [string trim $fsBoxNewFile /] @]/"
        } {
          .fsBox.path.path delete 0 end
          .fsBox.path.path insert 0 "[string trimright $fsBoxDirName /]/[string trimright [string trim $fsBoxNewFile /] @]/"
        }
        FSBoxFSShow [.fsBox.path.path get]           [.fsBox.pattern.pattern get] $fsBox(all)
        FSBoxFSInsertPath
      } {
        .fsBox.path.path delete 0 end
        .fsBox.path.path insert 0 "[string trimright $fsBoxDirName /]/[string trimright [string trim $fsBoxNewFile /] @]"
      }
    } {
      .fsBox.path.path delete 0 end
      .fsBox.path.path insert 0 "[string trimright $fsBoxDirName {@/}]/"
      .fsBox.file.file delete 0 end
      .fsBox.file.file insert 0 $fsBoxNewFile
      .fsBox.file.file icursor end
      focus .fsBox.file.file
    }
  }
}


# Procedure: FSBoxFSShow
proc FSBoxFSShow { fsBoxPath fsBoxPattern fsBoxAll} {
# xf ignore me 6
  global fsBox

  set tmpButtonOpt ""
  if {"$fsBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$fsBox(activeBackground)\" "
  }
  if {"$fsBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$fsBox(activeForeground)\" "
  }
  if {"$fsBox(background)" != ""} {
    append tmpButtonOpt "-background \"$fsBox(background)\" "
  }
  if {"$fsBox(font)" != ""} {
    append tmpButtonOpt "-font \"$fsBox(font)\" "
  }
  if {"$fsBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$fsBox(foreground)\" "
  }

  set fsBox(pattern) $fsBoxPattern
  if {[file exists $fsBoxPath] && [file readable $fsBoxPath] &&
      [IsADir $fsBoxPath]} {
    set fsBox(internalPath) $fsBoxPath
  } {
    if {[file exists $fsBoxPath] && [file readable $fsBoxPath] &&
        [IsAFile $fsBoxPath]} {
      set fsBox(internalPath) [file dirname $fsBoxPath]
      .fsBox.file.file delete 0 end
      .fsBox.file.file insert 0 [file tail $fsBoxPath]
      set fsBoxPath $fsBox(internalPath)
    } {
      while {"$fsBoxPath" != "" && "$fsBoxPath" != "/" &&
             ![file isdirectory $fsBoxPath]} {
        set fsBox(internalPath) [file dirname $fsBoxPath]
         set fsBoxPath $fsBox(internalPath)
      }
    }
  }
  if {"$fsBoxPath" == ""} {
    set fsBoxPath "/"
    set fsBox(internalPath) "/"
  }
  .fsBox.path.path delete 0 end
  .fsBox.path.path insert 0 $fsBox(internalPath)

  if {[.fsBox.files.files size] > 0} {
    .fsBox.files.files delete 0 end
  }
  if {$fsBoxAll} {
    if {[catch "Ls -F -a $fsBoxPath" fsBoxResult]} {
      puts stderr "$fsBoxResult"
    }
  } {
    if {[catch "Ls -F $fsBoxPath" fsBoxResult]} {
      puts stderr "$fsBoxResult"
    }
  }
  set fsBoxElementList [lsort $fsBoxResult]

  foreach fsBoxCounter [winfo children .fsBox.pattern.patterns.patterns] {
    if {[string length [info commands XFDestroy]] > 0} {
      catch {XFDestroy $fsBoxCounter}
    } {
      catch {destroy $fsBoxCounter}
    }
  }
  menu .fsBox.pattern.patterns.patterns.menu
  catch ".fsBox.pattern.patterns.patterns.menu config $tmpButtonOpt"

  if {$fsBox(extensions)} {
    .fsBox.pattern.patterns.patterns.menu add command       -label "*"       -command {
        global fsBox
        set fsBox(pattern) "*"
        .fsBox.pattern.pattern delete 0 end
        .fsBox.pattern.pattern insert 0 $fsBox(pattern)
        FSBoxFSShow [.fsBox.path.path get] $fsBox(pattern)           $fsBox(all)}
  }

  if {"$fsBoxPath" != "/"} {
    .fsBox.files.files insert end "../"
  }
  foreach fsBoxCounter $fsBoxElementList {
    if {[string match $fsBoxPattern $fsBoxCounter] ||
        [IsADir [string trimright $fsBoxPath/$fsBoxCounter "/@"]]} {
      if {"$fsBoxCounter" != "../" &&
          "$fsBoxCounter" != "./"} {
        .fsBox.files.files insert end $fsBoxCounter
      }
    }

    if {$fsBox(extensions)} {
      catch "file rootname $fsBoxCounter" fsBoxRootName
      catch "file extension $fsBoxCounter" fsBoxExtension
      set fsBoxExtension [string trimright $fsBoxExtension "/*@"]
      if {"$fsBoxExtension" != "" && "$fsBoxRootName" != ""} {
        set fsBoxInsert 1
        set fsBoxLast [.fsBox.pattern.patterns.patterns.menu index last]
        for {set fsBoxCounter1 0} {$fsBoxCounter1 <= $fsBoxLast} {incr fsBoxCounter1 1} {
          if {"*$fsBoxExtension" ==                 "[lindex [.fsBox.pattern.patterns.patterns.menu entryconfigure                         $fsBoxCounter1 -label] 4]"} {
            set fsBoxInsert 0
          }
        }
	if {$fsBoxInsert} {
          .fsBox.pattern.patterns.patterns.menu add command             -label "*$fsBoxExtension"             -command "
              global fsBox
              set fsBox(pattern) \"*$fsBoxExtension\"
              .fsBox.pattern.pattern delete 0 end
              .fsBox.pattern.pattern insert 0 \$fsBox(pattern)
              FSBoxFSShow \[.fsBox.path.path get\] \$fsBox(pattern)                 \$fsBox(all)"
        }
      }
    }
  }
  if {$fsBox(extensions)} {
    .fsBox.pattern.patterns.patterns.menu add separator
  }
  if {$fsBox(extensions) || 
      "[.fsBox.pattern.patterns.patterns.menu index last]" == "none"} {
    .fsBox.pattern.patterns.patterns.menu add checkbutton       -label "Scan extensions"       -variable "fsBox(extensions)"       -command {
        global fsBox
        FSBoxFSShow [.fsBox.path.path get]           [.fsBox.pattern.pattern get] $fsBox(all)}
  }
}


# Procedure: GetSelection
if {"[info procs GetSelection]" == ""} {
proc GetSelection {} {
# xf ignore me 7
##########
# Procedure: GetSelection
# Description: get current selection
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  # the save way
  set xfSelection ""
  catch "selection get" xfSelection
  if {"$xfSelection" == "selection doesn't exist or form \"STRING\" not defined"} {
    return ""
  } {
    return $xfSelection
  }
}
}


# Procedure: HandleHardcopies
proc HandleHardcopies {} {
  global hardcopy

  ShowWindow.top0
  set currentSelect [[SymbolicName Commands] curselection]
  if {"$currentSelect" != ""} {
    set current [[SymbolicName Commands] get $currentSelect]
    [SymbolicName CommandName] delete 0 end
    [SymbolicName CommandValue] delete 0 end
    [SymbolicName CommandName] insert 0 $current
    if {"$current" != "TCL Postscript mono" &&
        "$current" != "TCL Postscript gray" &&
        "$current" != "TCL Postscript color"} {
      [SymbolicName CommandValue] insert 0 [set hardcopy($current)]
    }
  }
}


# Procedure: Hardcopy
proc Hardcopy {} {
  global currentApp
  global hardcopy
  global outputFile

  SetStatus "Making harcopy...in progress"
  set currentCommands [[SymbolicName Commands] curselection]
  set currentWidgets [[SymbolicName Widgets] curselection]
  if {"$currentCommands" != "" &&
      "$currentWidgets" != ""} {
    if {"[[SymbolicName Commands] get $currentCommands]" == "TCL Postscript mono" ||
        "[[SymbolicName Commands] get $currentCommands]" == "TCL Postscript gray" ||
        "[[SymbolicName Commands] get $currentCommands]" == "TCL Postscript color"} {
      set xfWidget [lindex [split [[SymbolicName Widgets] get $currentWidgets] :] 0]

      if {"$xfWidget" != ""} {
        HardcopyPS $xfWidget
      }
    } {
      set xfCommand [set hardcopy([[SymbolicName Commands] get $currentCommands])]
      set xfWidget [lindex [split [[SymbolicName Widgets] get $currentWidgets] :] 0]

      if {"$xfCommand" != "" &&
          "$xfWidget" != ""} {
        if {![catch "winfo ismapped $xfWidget"]} {
      	  if {[catch "send $currentApp \"winfo id $xfWidget\"" xfId]} {
            SetStatus "Making harcopy...aborted"
	    return
	  }
	  if {[catch "send $currentApp \"winfo rootx $xfWidget\"" xfRootX]} {
            SetStatus "Making harcopy...aborted"
	    return
	  }
	  if {[catch "send $currentApp \"winfo rooty $xfWidget\"" xfRootY]} {
            SetStatus "Making harcopy...aborted"
	    return
	  }
	  if {[catch "send $currentApp \"winfo x $xfWidget\"" xfX]} {
            SetStatus "Making harcopy...aborted"
	    return
	  }
	  if {[catch "send $currentApp \"winfo y $xfWidget\"" xfY]} {
            SetStatus "Making harcopy...aborted"
	    return
	  }
	  if {[catch "send $currentApp \"winfo width $xfWidget\"" xfWidth]} {
            SetStatus "Making harcopy...aborted"
	    return
	  }
	  if {[catch "send $currentApp \"winfo height $xfWidget\"" xfHeight]} {
            SetStatus "Making harcopy...aborted"
	    return
	  }
          if {[regsub -all {\$rootx} $xfCommand $xfRootX xfCommandString]} {
            set xfCommand $xfCommandString
          }
          if {[regsub -all {\$rooty} $xfCommand $xfRootY xfCommandString]} {
            set xfCommand $xfCommandString
          }
          if {[regsub -all {\$x} $xfCommand $xfX xfCommandString]} {
            set xfCommand $xfCommandString
          }
          if {[regsub -all {\$y} $xfCommand $xfY xfCommandString]} {
            set xfCommand $xfCommandString
          }
          if {[regsub -all {\$width} $xfCommand $xfWidth xfCommandString]} {
            set xfCommand $xfCommandString
          }
          if {[regsub -all {\$height} $xfCommand $xfHeight xfCommandString]} {
            set xfCommand $xfCommandString
          }
          if {[regsub -all {\$id} $xfCommand $xfId xfCommandString]} {
            set xfCommand $xfCommandString
          }
          if {[regsub -all {\$widget} $xfCommand $xfWidget xfCommandString]} {
            set xfCommand $xfCommandString
          }
          if {[regsub -all {\$outputFile} $xfCommand $outputFile xfCommandString]} {
            set xfCommand $xfCommandString
          }
          if {[catch "exec $xfCommand" xfResult]} {
            AlertBox "$xfResult"
          }
        }
      }
    }
  }
  SetStatus "Making harcopy...done"
}


# Procedure: HardcopyAdd
proc HardcopyAdd {} {
  global hardcopy

  set currentName [[SymbolicName CommandName] get]
  if {"$currentName" != "" &&
      "$currentName" != "TCL Postscript mono" &&
      "$currentName" != "TCL Postscript gray" &&
      "$currentName" != "TCL Postscript color"} {
    set hardcopy($currentName) [[SymbolicName CommandValue] get]
    SetHardcopyCommands
  }
}


# Procedure: HardcopyDelete
proc HardcopyDelete {} {
  global hardcopy

  set currentName [[SymbolicName CommandName] get]
  if {"$currentName" != "" &&
      "$currentName" != "TCL Postscript mono" &&
      "$currentName" != "TCL Postscript gray" &&
      "$currentName" != "TCL Postscript color"} {
    catch "unset \{hardcopy([[SymbolicName CommandName] get])\}"
    SetHardcopyCommands
  }
}


# Procedure: HardcopyOk
proc HardcopyOk {} {

  DestroyWindow.top0
}


# Procedure: HardcopyPS
proc HardcopyPS { w} {
  global currentApp
  global outputFile

  set currentCommands [[SymbolicName Commands] get [[SymbolicName Commands] curselection]]
  set currentWidgets [lindex [[SymbolicName Widgets] get [[SymbolicName Widgets] curselection]] 0]
  if {"$currentCommands" != "" &&
      "$currentWidgets" != ""} {
    if {[catch "send $currentApp \"winfo class $w\"" class]} {
      return
    }
    if {"$class" != "Canvas"} {
      AlertBox "Postscript hardcopy only from canvas!"
      return
    }
    if {"$currentCommands" == "TCL Postscript mono"} {
      set xfSendCommand "$currentWidgets postscript -colormode mono -file $outputFile"
    } {
      if {"$currentCommands" == "TCL Postscript gray"} {
        set xfSendCommand "$currentWidgets postscript -colormode gray -file $outputFile"
      } {
        set xfSendCommand "$currentWidgets postscript -colormode color -file $outputFile"
      }
    }
    if {[catch "send $currentApp $xfSendCommand" result]} {
      AlertBox $result
    }
  }
}


# Procedure: HardcopySave
proc HardcopySave {} {
  global hardcopy

  SetStatus "Saving harcopy commands..."
  if {[catch "open ~/.xfhardcopy w" outFile]} {
    AlertBox "$outFile"
  } {
    puts $outFile "global \{hardcopy\}"
    foreach counter [array names hardcopy] {
      if {"$counter" != "" &&
        "$counter" != "TCL Postscript mono" &&
        "$counter" != "TCL Postscript gray" &&
        "$counter" != "TCL Postscript color"} {
        puts $outFile "set \{hardcopy($counter)\} \{[set hardcopy($counter)]\}"
      }
    }
    close $outFile
  }
  SetStatus "Saving harcopy commands...done"
}


# Procedure: HardcopyTo
proc HardcopyTo {} {
  global outputFile

  SetStatus "Making harcopy..."
  set selFile [FSBox]
  if {"$selFile" != ""} {
    set outputFile $selFile
    wm title . "xfhardcopy to: $outputFile"
    Hardcopy
  } {
    SetStatus "Making harcopy...aborted"
  }
}


# Procedure: InitSetStatus
proc InitSetStatus {} {
  global hasColor
  global savedForeground

  set hasColor [regexp -nocase {color} [winfo screenvisual .]]
  set savedForeground [lindex [[SymbolicName Status] configure -foreground] 4]
}


# Procedure: IsADir
proc IsADir { pathName} {
# xf ignore me 5
##########
# Procedure: IsADir
# Description: check if name is a directory (including symbolic links)
# Arguments: pathName - the path to check
# Returns: 1 if its a directory, otherwise 0
# Sideeffects: none
##########

  if {[file isdirectory $pathName]} {
    return 1
  } {
    catch "file type $pathName" fileType
    if {"$fileType" == "link"} {
      if {[catch "file readlink $pathName" linkName]} {
        return 0
      }
      catch "file type $linkName" fileType
      while {"$fileType" == "link"} {
        if {[catch "file readlink $linkName" linkName]} {
          return 0
        }
        catch "file type $linkName" fileType
      }
      return [file isdirectory $linkName]
    }
  }
  return 0
}


# Procedure: IsAFile
proc IsAFile { fileName} {
# xf ignore me 5
##########
# Procedure: IsAFile
# Description: check if filename is a file (including symbolic links)
# Arguments: fileName - the filename to check
# Returns: 1 if its a file, otherwise 0
# Sideeffects: none
##########

  if {[file isfile $fileName]} {
    return 1
  } {
    catch "file type $fileName" fileType
    if {"$fileType" == "link"} {
      if {[catch "file readlink $fileName" linkName]} {
        return 0
      }
      catch "file type $linkName" fileType
      while {"$fileType" == "link"} {
        if {[catch "file readlink $linkName" linkName]} {
          return 0
        }
        catch "file type $linkName" fileType
      }
      return [file isfile $linkName]
    }
  }
  return 0
}


# Procedure: IsASymlink
proc IsASymlink { fileName} {
# xf ignore me 5
##########
# Procedure: IsASymlink
# Description: check if filename is a symbolic link
# Arguments: fileName - the path/filename to check
# Returns: none
# Sideeffects: none
##########

  catch "file type $fileName" fileType
  if {"$fileType" == "link"} {
    return 1
  }
  return 0
}


proc Ls {args} {
global tcl_platform
if {$tcl_platform(platform) == "unix"} {eval exec ls $args} {
   set last [lindex $args end]
   if {[lsearch $last -* ] >= 0 || $last == "" }\
      {set path "*"}\
      {set path $last/*}
   set lst [glob $path]
   set lst1 ""
   if {[lsearch -exact $args "-F"] >= 0} then {set car "/"} else {set car ""}
   foreach f $lst {
      if {[file isdirectory $f]} \
         {set f [file tail $f]$car}\
         {set f [file tail $f]}
      lappend lst1 $f
   }
   return [lsort $lst1]
}
}

# Procedure: NoFunction
if {"[info procs NoFunction]" == ""} {
proc NoFunction { args} {
# xf ignore me 7
##########
# Procedure: NoFunction
# Description: do nothing (especially with scales and scrollbars)
# Arguments: args - a number of ignored parameters
# Returns: none
# Sideeffects: none
##########
}
}


# Procedure: QuitProgram
proc QuitProgram {} {

  catch "destroy ."
  catch "exit 0"
}


# Procedure: RescanApplications
proc RescanApplications {} {
  global currentApp

  SetStatus "Rescanning Tk applications..."
  set counter1 0
  ClearList [SymbolicName Applications]
  foreach counter2 [winfo interps] {
    if {![catch "send $counter2 \"winfo children .\""]} {
      [SymbolicName Applications] insert end $counter2
      if {"$currentApp" == "$counter2"} {
        [SymbolicName Applications] selection clear 0 end
        [SymbolicName Applications] selection set $counter1
      }
      incr counter1 1
    }
  }
  if {"$currentApp" == ""} {
    if {[[SymbolicName Applications] size] > 0} {
      set currentApp [[SymbolicName Applications] get 0]
       [SymbolicName Applications] selection clear 0 end
       [SymbolicName Applications] selection set 0
    }
  }
  SetStatus "Rescanning Tk applications...done"
}


# Procedure: RescanWidgets
proc RescanWidgets {} {
  global currentApp
  global currentWidget

  SetStatus "Rescanning widget tree for $currentApp..."
  set counter1 0
  ClearList [SymbolicName Widgets]
  if {"$currentApp" != ""} {
    [SymbolicName Widgets] insert end ". : Toplevel"
    if {![catch "send $currentApp \"winfo children $currentWidget\"" result]} {
      foreach counter2 $result {
        catch "send $currentApp \"winfo class $counter2\"" result
        [SymbolicName Widgets] insert end "$counter2 : $result"
        if {"$currentWidget" == "$counter2"} {
          [SymbolicName Widgets] selection clear 0 end
          [SymbolicName Widgets] selection set $counter1
        }
        incr counter1 1
      }
    }
  }
  if {"$currentWidget" == "."} {
     [SymbolicName Widgets] selection clear 0 end
     [SymbolicName Widgets] selection set 0
  }
  SetStatus "Rescanning widget tree for $currentApp...done"
}


# Procedure: SN
if {"[info procs SN]" == ""} {
proc SN { {xfName ""}} {
# xf ignore me 7
##########
# Procedure: SN
# Description: map a symbolic name to the widget path
# Arguments: xfName
# Returns: the symbolic name
# Sideeffects: none
##########

  SymbolicName $xfName
}
}


# Procedure: SelectApplication
proc SelectApplication { w y} {
  global currentApp

  set nearest [$w nearest $y]
  $w selection clear 0 end
  $w selection set $nearest
  set currentApp [[SymbolicName Applications] get $nearest]
  RescanWidgets
}


# Procedure: SelectCommand
proc SelectCommand { w y} {

  set nearest [$w nearest $y]
  $w selection clear 0 end
  $w selection set $nearest
}


# Procedure: SelectWidget
proc SelectWidget { w y} {
  global currentWidget

  set nearest [$w nearest $y]
  $w selection clear 0 end
  $w selection set $nearest
}


# Procedure: SetHardcopyCommands
proc SetHardcopyCommands {} {
  global hardcopy

  ClearList [SymbolicName Commands]
  [SymbolicName Commands] insert end "TCL Postscript color"
  [SymbolicName Commands] insert end "TCL Postscript gray"
  [SymbolicName Commands] insert end "TCL Postscript mono"
  foreach counter [lsort [array names hardcopy]] {
    [SymbolicName Commands] insert end $counter
  }
   [SymbolicName Commands] selection clear 0 end
   [SymbolicName Commands] selection set 0
}


# Procedure: SetStatus
proc SetStatus { statusMessage} {
  global hasColor
  global savedForeground

  if {$hasColor == 1} {
    if {[regexp {\.\.\.$} $statusMessage] ||
        [regexp {\.\.\.in progress$} $statusMessage]} {
      [SymbolicName Status] configure -foreground "hotpink"
    } {
      [SymbolicName Status] configure -foreground $savedForeground
    }
  }
  [SymbolicName Status] configure -text $statusMessage
  update
}


# Procedure: SymbolicName
if {"[info procs SymbolicName]" == ""} {
proc SymbolicName { {xfName ""}} {
# xf ignore me 7
##########
# Procedure: SymbolicName
# Description: map a symbolic name to the widget path
# Arguments: xfName
# Returns: the symbolic name
# Sideeffects: none
##########

  global symbolicName

  if {"$xfName" != ""} {
    set xfArrayName ""
    append xfArrayName symbolicName ( $xfName )
    if {![catch "set \"$xfArrayName\"" xfValue]} {
      return $xfValue
    } {
      if {"[info commands XFProcError]" != ""} {
        XFProcError "Unknown symbolic name:\n$xfName"
      } {
        puts stderr "XF error: unknown symbolic name:\n$xfName"
      }
    }
  }
  return ""
}
}


# Procedure: Unalias
proc Unalias { aliasName} {
# xf ignore me 7
##########
# Procedure: Unalias
# Description: remove an alias for a procedure
# Arguments: aliasName - the alias name to remove
# Returns: none
# Sideeffects: internalAliasList is updated, and the alias
#              proc is removed
##########
  global internalAliasList

  set xfIndex [lsearch $internalAliasList "$aliasName *"]
  if {$xfIndex != -1} {
    rename $aliasName ""
    set internalAliasList [lreplace $internalAliasList $xfIndex $xfIndex]
  }
}



# module load procedure
proc XFLocalIncludeModule {{moduleName ""}} {
  global env
  global xfLoadInfo
  global xfLoadPath
  global xfStatus

  foreach p [split $xfLoadPath :] {
    if {[file exists "$p/$moduleName"]} {
      if {![file readable "$p/$moduleName"]} {
        puts stderr "Cannot read $p/$moduleName (permission denied)"
        continue
      }
      if {$xfLoadInfo} {
        puts stdout "Loading $p/$moduleName..."
      }
      source "$p/$moduleName"
      return 1
    }
    # first see if we have a load command
    if {[info exists env(XF_VERSION_SHOW)]} {
      set xfCommand $env(XF_VERSION_SHOW)
      regsub -all {\$xfFileName} $xfCommand $p/$moduleName xfCommand
      if {$xfLoadInfo} {
        puts stdout "Loading $p/$moduleName...($xfCommand)"
      }
      if {[catch "$xfCommand" contents]} {
        continue
      } {
        eval $contents
        return 1
      }
    }
    # are we able to load versions from wish ?
    if {[catch "afbind $p/$moduleName" aso]} {
      # try to use xf version load command
      global xfVersion
      if {[info exists xfVersion(showDefault)]} {
        set xfCommand $xfVersion(showDefault)
      } {
	# our last hope
        set xfCommand "vcat -q $p/$moduleName"
      }
      regsub -all {\$xfFileName} $xfCommand $p/$moduleName xfCommand
      if {$xfLoadInfo} {
        puts stdout "Loading $p/$moduleName...($xfCommand)"
      }
      if {[catch "$xfCommand" contents]} {
        continue
      } {
        eval $contents
        return 1
      }
    } {
      # yes we can load versions directly
      if {[catch "$aso open r" inFile]} {
        puts stderr "Cannot open $p/[$aso attr af_bound] (permission denied)"
        continue
      }
      if {$xfLoadInfo} {
        puts stdout "Loading $p/[$aso attr af_bound]..."
      }
      if {[catch "read \{$inFile\}" contents]} {
        puts stderr "Cannot read $p/[$aso attr af_bound] (permission denied)"
        close $inFile
        continue
      }
      close $inFile
      eval $contents
      return 1
    }
  }
  puts stderr "Cannot load module $moduleName -- check your xf load path"
  catch "destroy ."
  catch "exit 0"
}

# application parsing procedure
proc XFLocalParseAppDefs {xfAppDefFile} {
  global xfAppDefaults

  # basically from: Michael Moore
  if {[file exists $xfAppDefFile] &&
      [file readable $xfAppDefFile] &&
      "[file type $xfAppDefFile]" == "link"} {
    catch "file type $xfAppDefFile" xfType
    while {"$xfType" == "link"} {
      if {[catch "file readlink $xfAppDefFile" xfAppDefFile]} {
        return
      }
      catch "file type $xfAppDefFile" xfType
    }
  }
  if {!("$xfAppDefFile" != "" &&
        [file exists $xfAppDefFile] &&
        [file readable $xfAppDefFile] &&
        "[file type $xfAppDefFile]" == "file")} {
    return
  }
  if {![catch "open $xfAppDefFile r" xfResult]} {
    while {[gets $xfResult line] != -1} {
      # backup indicates how far to backup.  It applies to the
      # situation where a resource name ends in . and when it
      # ends in *.  In the second case you want to keep the *
      # in the widget name for pattern matching, but you want
      # to get rid of the . if it is the end of the name. 
      set backup -2  
      set line [string trim $line]
      if {[string index $line 0] == "#" || "$line" == ""} {
        # skip comments and empty lines
        continue
      }
      set list [split $line ":"]
      set resource [string trim [lindex $list 0]]
      set i [string last "." $resource]
      set j [string last "*" $resource]
      if {$j > $i} { 
        set i $j
        set backup -1
      }
      incr i
      set name [string range $resource $i end]
      incr i $backup
      set widname [string range $resource 0 $i]
      set value [string trim [lindex $list 1]]
      if {"$widname" != "" && "$widname" != "*"} {
        # insert the widget and resourcename to the application
        # defaults list.
        set xfAppDefaults($widname:[string tolower $name]) $value
      }
    }
  }
}

# application loading procedure
proc XFLocalLoadAppDefs {xfClasses {xfPriority "startupFile"} {xfAppDefFile ""}} {
  global env

  if {"$xfAppDefFile" == ""} {
    set xfFileList ""
    if {[info exists env(XUSERFILESEARCHPATH)]} {
      append xfFileList [split $env(XUSERFILESEARCHPATH) :]
    }
    if {[info exists env(XAPPLRESDIR)]} {
      append xfFileList [split $env(XAPPLRESDIR) :]
    }
    if {[info exists env(XFILESEARCHPATH)]} {
      append xfFileList [split $env(XFILESEARCHPATH) :]
    }
    append xfFileList " /usr/lib/X11/app-defaults"
    append xfFileList " /usr/X11/lib/X11/app-defaults"

    foreach xfCounter1 $xfClasses {
      foreach xfCounter2 $xfFileList {
        set xfPathName $xfCounter2
        if {[regsub -all "%N" "$xfPathName" "$xfCounter1" xfResult]} {
          set xfPathName $xfResult
        }
        if {[regsub -all "%T" "$xfPathName" "app-defaults" xfResult]} {
          set xfPathName $xfResult
        }
        if {[regsub -all "%S" "$xfPathName" "" xfResult]} {
          set xfPathName $xfResult
        }
        if {[regsub -all "%C" "$xfPathName" "" xfResult]} {
          set xfPathName $xfResult
        }
        if {[file exists $xfPathName] &&
            [file readable $xfPathName] &&
            ("[file type $xfPathName]" == "file" ||
             "[file type $xfPathName]" == "link")} {
          catch "option readfile $xfPathName $xfPriority"
          if {"[info commands XFParseAppDefs]" != ""} {
            XFParseAppDefs $xfPathName
          } {
            if {"[info commands XFLocalParseAppDefs]" != ""} {
              XFLocalParseAppDefs $xfPathName
            }
          }
        } {
          if {[file exists $xfCounter2/$xfCounter1] &&
              [file readable $xfCounter2/$xfCounter1] &&
              ("[file type $xfCounter2/$xfCounter1]" == "file" ||
               "[file type $xfCounter2/$xfCounter1]" == "link")} {
            catch "option readfile $xfCounter2/$xfCounter1 $xfPriority"
            if {"[info commands XFParseAppDefs]" != ""} {
              XFParseAppDefs $xfCounter2/$xfCounter1
            } {
              if {"[info commands XFLocalParseAppDefs]" != ""} {
                XFLocalParseAppDefs $xfCounter2/$xfCounter1
              }
            }
          }
        }
      }
    }
  } {
    # load a specific application defaults file
    if {[file exists $xfAppDefFile] &&
        [file readable $xfAppDefFile] &&
        ("[file type $xfAppDefFile]" == "file" ||
         "[file type $xfAppDefFile]" == "link")} {
      catch "option readfile $xfAppDefFile $xfPriority"
      if {"[info commands XFParseAppDefs]" != ""} {
        XFParseAppDefs $xfAppDefFile
      } {
        if {"[info commands XFLocalParseAppDefs]" != ""} {
          XFLocalParseAppDefs $xfAppDefFile
        }
      }
    }
  }
}

# application setting procedure
proc XFLocalSetAppDefs {{xfWidgetPath "."}} {
  global xfAppDefaults

  if {![info exists xfAppDefaults]} {
    return
  }
  foreach xfCounter [array names xfAppDefaults] {
    if {[string match "${xfWidgetPath}*" $xfCounter]} {
      set widname [string range $xfCounter 0 [expr [string first : $xfCounter]-1]]
      set name [string range $xfCounter [expr [string first : $xfCounter]+1] end]
      # Now lets see how many tcl commands match the name
      # pattern specified.
      set widlist [info command $widname]
      if {"$widlist" != ""} {
        foreach widget $widlist {
          # make sure this command is a widget.
          if {![catch "winfo id $widget"]} {
            catch "$widget configure -[string tolower $name] $xfAppDefaults($xfCounter)" 
          }
        }
      }
    }
  }
}



# end source
proc EndSrc {} {
  global currentApp
  global currentWidget
  global outputFile

  InitSetStatus
  if {[file exists ~/.xfhardcopy] &&
      [file readable ~/.xfhardcopy] &&
      "[file type ~/.xfhardcopy]" == "file"} {
    catch "source ~/.xfhardcopy"
  }
  set currentApp ""
  set currentWidget "."
  set outputFile "./xfHardCopy"
  wm title . "xfhardcopy to: $outputFile"
  SetHardcopyCommands
  RescanApplications
  RescanWidgets
}

# initialize global variables
global {alertBox}
set {alertBox(activeBackground)} {}
set {alertBox(activeForeground)} {}
set {alertBox(after)} {0}
set {alertBox(anchor)} {nw}
set {alertBox(background)} {}
set {alertBox(button)} {0}
set {alertBox(colormodel)} {}
set {alertBox(font)} {}
set {alertBox(foreground)} {}
set {alertBox(justify)} {center}
set {alertBox(toplevelName)} {.alertBox}
global {currentApp}
set {currentApp} {xf}
global {currentWidget}
set {currentWidget} {.}
global {fsBox}
set {fsBox(activeBackground)} {}
set {fsBox(activeForeground)} {}
set {fsBox(all)} {0}
set {fsBox(background)} {}
set {fsBox(button)} {0}
set {fsBox(extensions)} {0}
set {fsBox(fileName)} {}
set {fsBox(font)} {}
set {fsBox(foreground)} {}
set {fsBox(internalPath)} {/}
set {fsBox(name)} {}
set {fsBox(path)} {/}
set {fsBox(pattern)} {*}
set {fsBox(scrollActiveForeground)} {}
set {fsBox(scrollBackground)} {}
set {fsBox(scrollForeground)} {}
set {fsBox(scrollSide)} {left}
set {fsBox(showPixmap)} {0}
global {hardcopy}
set {hardcopy(Color hardcopy to PS (click, no border))} {xgrabsc --bell -comp -cps -nobdrs -click -o $outputFile}
set {hardcopy(Color hardcopy to PS (id, no border))} {xgrabsc --bell -comp -cps -nobdrs -id $id -o $outputFile}
set {hardcopy(Color hardcopy to XPM3 (click, no border))} {xgrabsc --bell -bm3 -nobdrs -click -o $outputFile}
set {hardcopy(Color hardcopy to XPM3 (id, no border))} {xgrabsc --bell -bm3 -nobdrs -id $id -o $outputFile}
set {hardcopy(Floyd-Steinberg tp PS (click, no border))} {xgrabsc --bell -comp -eps -dither -nobdrs -click -o $outputFile}
set {hardcopy(Floyd-Steinberg tp PS (id, no border))} {xgrabsc --bell -comp -eps -dither -nobdrs -id $id -o $outputFile}
set {hardcopy(Floyd-Steinberg tp XPM3 (click, no border))} {xgrabsc --bell -bm3 -dither -nobdrs -click -o $outputFile}
set {hardcopy(Floyd-Steinberg tp XPM3 (id, no border))} {xgrabsc --bell -bm3 -dither -nobdrs -id $id -o $outputFile}
set {hardcopy(Hardcopy to PS (click, border, no dither))} {xgrabsc --bell -comp -eps -nodither -click -o $outputFile}
set {hardcopy(Hardcopy to PS (click, no border, no dither))} {xgrabsc --bell -comp -eps -nodither -nobdrs -click -o $outputFile}
set {hardcopy(Hardcopy to PS (id, border, no dither))} {xgrabsc --bell -comp -eps -nodither -id $id -o $outputFile}
set {hardcopy(Hardcopy to PS (id, no border, no dither))} {xgrabsc --bell -comp -eps -nodither -nobdrs -id $id -o $outputFile}
set {hardcopy(Mono Hardcopy to PS (click, no border))} {xgrabsc --bell -comp -eps -mdither -nobdrs -click -o $outputFile}
set {hardcopy(Mono Hardcopy to PS (id, no border))} {xgrabsc --bell -comp -eps -mdither -nobdrs -id $id -o $outputFile}
set {hardcopy(Mono Hardcopy to XPM3 (click, no border))} {xgrabsc --bell -bm3 -mdither -nobdrs -click -o $outputFile}
set {hardcopy(Mono Hardcopy to XPM3 (id, no border))} {xgrabsc --bell -bm3 -mdither -nobdrs -id $id -o $outputFile}
set {hardcopy(Mono Hardcopy to bitmap (click, no border))} {xgrabsc --bell -bm -mdither -nobdrs -click -o $outputFile}
set {hardcopy(Mono Hardcopy to bitmap (id, no border))} {xgrabsc --bell -bm -mdither -nobdrs -id $id -o $outputFile}
set {hardcopy(XWD)} {xwd -nobdrs -out $outputFile}
global {hasColor}
set {hasColor} {1}
global {outputFile}
set {outputFile} {./xfHardCopy}
global {savedForeground}
set {savedForeground} {Black}

# please don't modify the following
# variables. They are needed by xf.
global {autoLoadList}
set {autoLoadList(main.tcl)} {0}
global {internalAliasList}
set {internalAliasList} {}
global {moduleList}
set {moduleList(alertBox.tcl)} { AlertBox AlertBoxInternal}
set {moduleList(extrnl.tcl)} { Alias ClearList GetSelection NoFunction SN SymbolicName Unalias}
set {moduleList(fnctns.tcl)} { ChangeWidget HandleHardcopies Hardcopy HardcopyPS HardcopyTo HardcopyAdd HardcopyOk HardcopySave InitSetStatus QuitProgram RescanApplications RescanWidgets SelectApplication SelectWidget SetStatus SetHardcopyCommands HardcopyDelete SelectCommand}
set {moduleList(fsBox.tcl)} { FSBox FSBoxBindSelectOne FSBoxFSFileSelect FSBoxFSFileSelectDouble FSBoxFSInsertPath FSBoxFSNameComplete FSBoxFSShow IsADir IsAFile IsASymlink}
set {moduleList(interface.tcl)} { . .top0}
set {moduleList(main.tcl)} {}
global {preloadList}
set {preloadList(xfInternal)} {}
global {symbolicName}
set {symbolicName(Applications)} {.frame2.frame.listbox1}
set {symbolicName(CommandName)} {.top0.frame.frame.entry2}
set {symbolicName(CommandValue)} {.top0.frame4.frame.entry2}
set {symbolicName(Commands)} {.frame.listbox1}
set {symbolicName(HardcopyCommand)} {.frame8.frame.entry2}
set {symbolicName(Status)} {.frame0.label2}
set {symbolicName(Widgets)} {.frame2.frame4.listbox1}
set {symbolicName(root)} {.}
global {xfWmSetPosition}
set {xfWmSetPosition} {}
global {xfWmSetSize}
set {xfWmSetSize} {. .top0}
global {xfAppDefToplevels}
set {xfAppDefToplevels} {}

# display/remove toplevel windows.
ShowWindow.

global xfShowWindow.top0
set xfShowWindow.top0 0

# load default bindings.
if {[info exists env(XF_BIND_FILE)] &&
    "[info procs XFShowHelp]" == ""} {
  source $env(XF_BIND_FILE)
}

# parse and apply application defaults.
XFLocalLoadAppDefs Main
XFLocalSetAppDefs

# end source
EndSrc

# eof
#

