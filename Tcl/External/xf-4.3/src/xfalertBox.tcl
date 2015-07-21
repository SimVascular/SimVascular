# XFNoParsing
# Program: template
# Description: alert box
#
# $Header: xfalertBox.tcl[2.3] Wed Mar 10 12:05:21 1993 garfield@garfield frozen $

global xfAlertBox
set xfAlertBox(activeBackground) ""
set xfAlertBox(activeForeground) ""
set xfAlertBox(after) 0
set xfAlertBox(anchor) nw
set xfAlertBox(background) ""
set xfAlertBox(font) ""
set xfAlertBox(foreground) ""
set xfAlertBox(justify) center
set xfAlertBox(toplevelName) .xfAlertBox
set xfAlertBox(button) 0

proc XFAlertBox {{xfAlertBoxMessage {Alert message}} {xfAlertBoxCommand ""} {xfAlertBoxGeometry 350x150} {xfAlertBoxTitle "Alert box"} args} {# xf ignore me 5
##########
# Procedure: XFAlertBox
# Description: show alert box
# Arguments: {xfAlertBoxMessage} - the text to display
#            {xfAlertBoxCommand} - the command to call after ok
#            {xfAlertBoxGeometry} - the geometry for the window
#            {xfAlertBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          XFAlertBoxFile - to open and read a file automatically
#          XFAlertBoxFd - to read from an already opened filedescriptor
##########
#
# global xfAlertBox(activeBackground) - active background color
# global xfAlertBox(activeForeground) - active foreground color
# global xfAlertBox(after) - destroy alert box after n seconds
# global xfAlertBox(anchor) - anchor for message box
# global xfAlertBox(background) - background color
# global xfAlertBox(font) - message font
# global xfAlertBox(foreground) - foreground color
# global xfAlertBox(justify) - justify for message box
# global xfAlertBox(toplevelName) - the toplevel name

  global xfAlertBox

  # show alert box
  if {[llength $args] > 0} {
    eval XFAlertBoxInternal "\{$xfAlertBoxMessage\}" "\{$xfAlertBoxCommand\}" "\{$xfAlertBoxGeometry\}" "\{$xfAlertBoxTitle\}" $args
  } {
    XFAlertBoxInternal $xfAlertBoxMessage $xfAlertBoxCommand $xfAlertBoxGeometry $xfAlertBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $xfAlertBox(toplevelName)
    tkwait window $xfAlertBox(toplevelName)

    return $xfAlertBox(button)
  }
}

proc XFAlertBoxFd {{xfAlertBoxInFile ""} {xfAlertBoxCommand ""} {xfAlertBoxGeometry 350x150} {xfAlertBoxTitle "Alert box"} args} {# xf ignore me 5
##########
# Procedure: XFAlertBoxFd
# Description: show alert box containing a filedescriptor
# Arguments: {xfAlertBoxInFile} - a filedescriptor to read. The descriptor
#                               is closed after reading
#            {xfAlertBoxCommand} - the command to call after ok
#            {xfAlertBoxGeometry} - the geometry for the window
#            {xfAlertBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          XFAlertBox - to display a passed string
#          XFAlertBoxFile - to open and read a file automatically
##########
#
# global xfAlertBox(activeBackground) - active background color
# global xfAlertBox(activeForeground) - active foreground color
# global xfAlertBox(after) - destroy alert box after n seconds
# global xfAlertBox(anchor) - anchor for message box
# global xfAlertBox(background) - background color
# global xfAlertBox(font) - message font
# global xfAlertBox(foreground) - foreground color
# global xfAlertBox(justify) - justify for message box
# global xfAlertBox(toplevelName) - the toplevel name

  global xfAlertBox

  # check file existance
  if {"$xfAlertBoxInFile" == ""} {
    puts stderr "No filedescriptor specified"
    return
  }

  set xfAlertBoxMessage [read $xfAlertBoxInFile]
  close $xfAlertBoxInFile

  # show alert box
  if {[llength $args] > 0} {
    eval XFAlertBoxInternal "\{$xfAlertBoxMessage\}" "\{$xfAlertBoxCommand\}" "\{$xfAlertBoxGeometry\}" "\{$xfAlertBoxTitle\}" $args
  } {
    XFAlertBoxInternal $xfAlertBoxMessage $xfAlertBoxCommand $xfAlertBoxGeometry $xfAlertBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $xfAlertBox(toplevelName)
    tkwait window $xfAlertBox(toplevelName)

    return $xfAlertBox(button)
  }
}

proc XFAlertBoxFile {{xfAlertBoxFile ""} {xfAlertBoxCommand ""} {xfAlertBoxGeometry 350x150} {xfAlertBoxTitle "Alert box"} args} {# xf ignore me 5
##########
# Procedure: XFAlertBoxFile
# Description: show alert box containing a file
# Arguments: {xfAlertBoxFile} - filename to read
#            {xfAlertBoxCommand} - the command to call after ok
#            {xfAlertBoxGeometry} - the geometry for the window
#            {xfAlertBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          XFAlertBox - to display a passed string
#          XFAlertBoxFd - to read from an already opened filedescriptor
##########
#
# global xfAlertBox(activeBackground) - active background color
# global xfAlertBox(activeForeground) - active foreground color
# global xfAlertBox(after) - destroy alert box after n seconds
# global xfAlertBox(anchor) - anchor for message box
# global xfAlertBox(background) - background color
# global xfAlertBox(font) - message font
# global xfAlertBox(foreground) - foreground color
# global xfAlertBox(justify) - justify for message box
# global xfAlertBox(toplevelName) - the toplevel name

  global xfAlertBox

  # check file existance
  if {"$xfAlertBoxFile" == ""} {
    puts stderr "No filename specified"
    return
  }

  if {[catch "open $xfAlertBoxFile r" xfAlertBoxInFile]} {
    puts stderr "$xfAlertBoxInFile"
    return
  }

  set xfAlertBoxMessage [read $xfAlertBoxInFile]
  close $xfAlertBoxInFile

  # show alert box
  if {[llength $args] > 0} {
    eval XFAlertBoxInternal "\{$xfAlertBoxMessage\}" "\{$xfAlertBoxCommand\}" "\{$xfAlertBoxGeometry\}" "\{$xfAlertBoxTitle\}" $args
  } {
    XFAlertBoxInternal $xfAlertBoxMessage $xfAlertBoxCommand $xfAlertBoxGeometry $xfAlertBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $xfAlertBox(toplevelName)
    tkwait window $xfAlertBox(toplevelName)

    return $xfAlertBox(button)
  }
}

##########
# Procedure: XFAlertBoxInternal
# Description: show alert box internal
# Arguments: xfAlertBoxMessage - the text to display
#            xfAlertBoxCommand - the command to call after ok
#            xfAlertBoxGeometry - the geometry for the window
#            xfAlertBoxTitle - the title for the window
#            args - labels of buttons
# Returns: none
# Sideeffects: none
##########
proc XFAlertBoxInternal {xfAlertBoxMessage xfAlertBoxCommand xfAlertBoxGeometry xfAlertBoxTitle args} {# xf ignore me 6
  global xfAlertBox

  # xf error file output
  XFMiscSaveError "Alert message:
$xfAlertBoxMessage"
  # xf error file output

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  if {"$xfAlertBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfAlertBox(activeBackground)\" "
  }
  if {"$xfAlertBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfAlertBox(activeForeground)\" "
  }
  if {"$xfAlertBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfAlertBox(background)\" "
    append tmpFrameOpt "-background \"$xfAlertBox(background)\" "
    append tmpMessageOpt "-background \"$xfAlertBox(background)\" "
  }
  if {"$xfAlertBox(font)" != ""} {
    append tmpButtonOpt "-font \"$xfAlertBox(font)\" "
    append tmpMessageOpt "-font \"$xfAlertBox(font)\" "
  }
  if {"$xfAlertBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfAlertBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfAlertBox(foreground)\" "
  }

  XFTmpltToplevel "$xfAlertBox(toplevelName)" "$xfAlertBoxGeometry" "$xfAlertBoxTitle"

  message $xfAlertBox(toplevelName).message1 \
    -anchor "$xfAlertBox(anchor)" \
    -justify "$xfAlertBox(justify)" \
    -relief raised \
    -text "$xfAlertBoxMessage"
  catch "$xfAlertBox(toplevelName).message1 config $tmpMessageOpt"

  set xfTmpWidth \
    [string range $xfAlertBoxGeometry 0 [expr [string first x $xfAlertBoxGeometry]-1]]
  if {"$xfTmpWidth" != ""} {
    # set message size
    catch "$xfAlertBox(toplevelName).message1 configure \
      -width [expr $xfTmpWidth-10]"
  } {
    $xfAlertBox(toplevelName).message1 configure \
      -aspect 1500
  }

  frame $xfAlertBox(toplevelName).frame1 \
    -borderwidth 2 \
    -relief raised
  catch "$xfAlertBox(toplevelName).frame1 config $tmpFrameOpt"

  set xfAlertBoxCounter 0
  set buttonNum [llength $args]
  if {$buttonNum > 0} {
    while {$xfAlertBoxCounter < $buttonNum} {
      button $xfAlertBox(toplevelName).frame1.button$xfAlertBoxCounter \
        -text "[lindex $args $xfAlertBoxCounter]" \
        -command "
          global xfAlertBox
          set xfAlertBox(button) $xfAlertBoxCounter
          if {\"\[info commands XFDestroy\]\" != \"\"} {
            catch {XFDestroy $xfAlertBox(toplevelName)}
          } {
            catch {destroy $xfAlertBox(toplevelName)}
          }"
      catch "$xfAlertBox(toplevelName).frame1.button$xfAlertBoxCounter config $tmpButtonOpt"

      pack $xfAlertBox(toplevelName).frame1.button$xfAlertBoxCounter -side left -padx 4 -pady 4 -expand 1

      incr xfAlertBoxCounter
    }
  } {
    button $xfAlertBox(toplevelName).frame1.button0 \
      -text "OK" \
      -command "
        global xfAlertBox
        set xfAlertBox(button) 0
        if {\"\[info commands XFDestroy\]\" != \"\"} {
          catch {XFDestroy $xfAlertBox(toplevelName)}
        } {
          catch {destroy $xfAlertBox(toplevelName)}
        }
        $xfAlertBoxCommand"
    catch "$xfAlertBox(toplevelName).frame1.button0 config $tmpButtonOpt"

    pack $xfAlertBox(toplevelName).frame1.button0 -side left -padx 4 -pady 4 -expand 1
  }

  # packing
  pack $xfAlertBox(toplevelName).frame1 -side bottom -fill both
  pack $xfAlertBox(toplevelName).message1 -side top -fill both -expand 1

  if {$xfAlertBox(after) != 0} {
    after [expr $xfAlertBox(after)*1000] \
      "catch \"$xfAlertBox(toplevelName).frame1.button0 invoke\""
  }
}

# eof

