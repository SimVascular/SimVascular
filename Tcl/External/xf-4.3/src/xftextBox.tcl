# XFNoParsing
# Program: template
# Description: text box
#
# $Header: xftextBox.tcl[2.3] Wed Mar 10 12:08:26 1993 garfield@garfield frozen $

global xfTextBox
set xfTextBox(activeBackground) ""
set xfTextBox(activeForeground) ""
set xfTextBox(background) ""
set xfTextBox(font) ""
set xfTextBox(foreground) ""
set xfTextBox(scrollActiveForeground) ""
set xfTextBox(scrollBackground) ""
set xfTextBox(scrollForeground) ""
set xfTextBox(state) disabled
set xfTextBox(toplevelName) .xfTextBox
set xfTextBox(button) 0
set xfTextBox(contents) ""

proc XFTextBox {{xfTextBoxMessage {Text message}} {xfTextBoxCommand ""} {xfTextBoxGeometry 350x150} {xfTextBoxTitle "Text box"} args} {# xf ignore me 5
##########
# Procedure: XFTextBox
# Description: show text box
# Arguments: {xfTextBoxMessage} - the text to display
#            {xfTextBoxCommand} - the command to call after ok
#            {xfTextBoxGeometry} - the geometry for the window
#            {xfTextBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, or nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          XFTextBoxFile - to open and read a file automatically
#          XFTextBoxFd - to read from an already opened filedescriptor
##########
#
# global xfTextBox(activeBackground) - active background color
# global xfTextBox(activeForeground) - active foreground color
# global xfTextBox(background) - background color
# global xfTextBox(font) - text font
# global xfTextBox(foreground) - foreground color
# global xfTextBox(scrollActiveForeground) - scrollbar active background color
# global xfTextBox(scrollBackground) - scrollbar background color
# global xfTextBox(scrollForeground) - scrollbar foreground color
# global xfTextBox(scrollSide) - side where scrollbar is located

  global xfTextBox

  # show text box
  if {[llength $args] > 0} {
    eval XFTextBoxInternal "\{$xfTextBoxMessage\}" "\{$xfTextBoxCommand\}" "\{$xfTextBoxGeometry\}" "\{$xfTextBoxTitle\}" $args
  } {
    XFTextBoxInternal $xfTextBoxMessage $xfTextBoxCommand $xfTextBoxGeometry $xfTextBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $xfTextBox(toplevelName)
    tkwait window $xfTextBox(toplevelName)

    return $xfTextBox(button)
  }
}

proc XFTextBoxFd {{xfTextBoxInFile ""} {xfTextBoxCommand ""} {xfTextBoxGeometry 350x150} {xfTextBoxTitle "Text box"} args} {# xf ignore me 5
##########
# Procedure: XFTextBoxFd
# Description: show text box containing a filedescriptor
# Arguments: {xfTextBoxInFile} - a filedescriptor to read. The descriptor
#                              is closed after reading
#            {xfTextBoxCommand} - the command to call after ok
#            {xfTextBoxGeometry} - the geometry for the window
#            {xfTextBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          XFTextBox - to display a passed string
#          XFTextBoxFile - to open and read a file automatically
##########
#
# global xfTextBox(activeBackground) - active background color
# global xfTextBox(activeForeground) - active foreground color
# global xfTextBox(background) - background color
# global xfTextBox(font) - text font
# global xfTextBox(foreground) - foreground color
# global xfTextBox(scrollActiveForeground) - scrollbar active background color
# global xfTextBox(scrollBackground) - scrollbar background color
# global xfTextBox(scrollForeground) - scrollbar foreground color
# global xfTextBox(scrollSide) - side where scrollbar is located

  global xfTextBox

  # check file existance
  if {"$xfTextBoxInFile" == ""} {
    puts stderr "No filedescriptor specified"
    return
  }

  set xfTextBoxMessage [read $xfTextBoxInFile]
  close $xfTextBoxInFile

  # show text box
  if {[llength $args] > 0} {
    eval XFTextBoxInternal "\{$xfTextBoxMessage\}" "\{$xfTextBoxCommand\}" "\{$xfTextBoxGeometry\}" "\{$xfTextBoxTitle\}" $args
  } {
    XFTextBoxInternal $xfTextBoxMessage $xfTextBoxCommand $xfTextBoxGeometry $xfTextBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $xfTextBox(toplevelName)
    tkwait window $xfTextBox(toplevelName)

    return $xfTextBox(button)
  }
}

proc XFTextBoxFile {{xfTextBoxFile ""} {xfTextBoxCommand ""} {xfTextBoxGeometry 350x150} {xfTextBoxTitle "Text box"} args} {# xf ignore me 5
##########
# Procedure: XFTextBoxFile
# Description: show text box containing a file
# Arguments: {xfTextBoxFile} - filename to read
#            {xfTextBoxCommand} - the command to call after ok
#            {xfTextBoxGeometry} - the geometry for the window
#            {xfTextBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          XFTextBox - to display a passed string
#          XFTextBoxFd - to read from an already opened filedescriptor
##########
#
# global xfTextBox(activeBackground) - active background color
# global xfTextBox(activeForeground) - active foreground color
# global xfTextBox(background) - background color
# global xfTextBox(font) - text font
# global xfTextBox(foreground) - foreground color
# global xfTextBox(scrollActiveForeground) - scrollbar active background color
# global xfTextBox(scrollBackground) - scrollbar background color
# global xfTextBox(scrollForeground) - scrollbar foreground color
# global xfTextBox(scrollSide) - side where scrollbar is located

  global xfTextBox

  # check file existance
  if {"$xfTextBoxFile" == ""} {
    puts stderr "No filename specified"
    return
  }

  if {[catch "open $xfTextBoxFile r" xfTextBoxInFile]} {
    puts stderr "$xfTextBoxInFile"
    return
  }

  set xfTextBoxMessage [read $xfTextBoxInFile]
  close $xfTextBoxInFile

  # show text box
  if {[llength $args] > 0} {
    eval XFTextBoxInternal "\{$xfTextBoxMessage\}" "\{$xfTextBoxCommand\}" "\{$xfTextBoxGeometry\}" "\{$xfTextBoxTitle\}" $args
  } {
    XFTextBoxInternal $xfTextBoxMessage $xfTextBoxCommand $xfTextBoxGeometry $xfTextBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $xfTextBox(toplevelName)
    tkwait window $xfTextBox(toplevelName)

    return $xfTextBox(button)
  }
}

##########
# Procedure: XFTextBoxInternal
# Description: show text box internal
# Arguments: xfTextBoxMessage - the text to display
#            xfTextBoxCommand - the command to call after ok
#            xfTextBoxGeometry - the geometry for the window
#            xfTextBoxTitle - the title for the window
#            args - labels of buttons
# Returns: none
# Sideeffects: none
##########
proc XFTextBoxInternal {xfTextBoxMessage xfTextBoxCommand xfTextBoxGeometry xfTextBoxTitle args} {# xf ignore me 6
  global xfTextBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScrollOpt ""
  if {"$xfTextBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfTextBox(activeBackground)\" "
  }
  if {"$xfTextBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfTextBox(activeForeground)\" "
  }
  if {"$xfTextBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfTextBox(background)\" "
    append tmpFrameOpt "-background \"$xfTextBox(background)\" "
    append tmpMessageOpt "-background \"$xfTextBox(background)\" "
  }
  if {"$xfTextBox(font)" != ""} {
    append tmpButtonOpt "-font \"$xfTextBox(font)\" "
    append tmpMessageOpt "-font \"$xfTextBox(font)\" "
  }
  if {"$xfTextBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfTextBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfTextBox(foreground)\" "
  }
  if {"$xfTextBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$xfTextBox(scrollActiveForeground)\" "
  }
  if {"$xfTextBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$xfTextBox(scrollBackground)\" "
  }
  if {"$xfTextBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$xfTextBox(scrollForeground)\" "
  }

  XFTmpltToplevel "$xfTextBox(toplevelName)" "$xfTextBoxGeometry" "$xfTextBoxTitle"

  frame $xfTextBox(toplevelName).frame0 \
    -borderwidth 0 \
    -relief raised
  catch "$xfTextBox(toplevelName).frame0 config $tmpFrameOpt"

  text $xfTextBox(toplevelName).frame0.text1 \
    -relief raised \
    -wrap none \
    -highlightthickness 0 \
    -borderwidth 2 \
    -yscrollcommand "$xfTextBox(toplevelName).frame0.vscroll set" \
    -xscrollcommand "$xfTextBox(toplevelName).frame0.hscroll set"
  catch "$xfTextBox(toplevelName).frame0.text1 config $tmpMessageOpt"

  scrollbar $xfTextBox(toplevelName).frame0.vscroll \
    -relief raised \
    -highlightthickness 0 \
    -command "$xfTextBox(toplevelName).frame0.text1 yview"
  catch "$xfTextBox(toplevelName).frame0.vscroll config $tmpScrollOpt"

  scrollbar $xfTextBox(toplevelName).frame0.hscroll \
    -relief raised \
    -orient horizontal \
    -highlightthickness 0 \
    -command "$xfTextBox(toplevelName).frame0.text1 xview"
  catch "$xfTextBox(toplevelName).frame0.vscroll config $tmpScrollOpt"

  frame $xfTextBox(toplevelName).frame1 \
    -borderwidth 2 \
    -relief raised
  catch "$xfTextBox(toplevelName).frame1 config $tmpFrameOpt"

  set xfTextBoxCounter 0
  set buttonNum [llength $args]

  if {$buttonNum > 0} {
    while {$xfTextBoxCounter < $buttonNum} {
      button $xfTextBox(toplevelName).frame1.button$xfTextBoxCounter \
        -text "[lindex $args $xfTextBoxCounter]" \
        -command "
          global xfTextBox
          set xfTextBox(button) $xfTextBoxCounter
          set xfTextBox(contents) \[$xfTextBox(toplevelName).frame0.text1 get 1.0 end\]
          if {\"\[info commands XFDestroy\]\" != \"\"} {
            catch {XFDestroy $xfTextBox(toplevelName)}
          } {
            catch {destroy $xfTextBox(toplevelName)}
          }"
      catch "$xfTextBox(toplevelName).frame1.button$xfTextBoxCounter config $tmpButtonOpt"

      pack $xfTextBox(toplevelName).frame1.button$xfTextBoxCounter -side left -padx 4 -pady 4 -expand 1

      incr xfTextBoxCounter
    }
  } {
    button $xfTextBox(toplevelName).frame1.button0 \
      -text "OK" \
      -command "
        global xfTextBox
        set xfTextBox(button) 0
        set xfTextBox(contents) \[$xfTextBox(toplevelName).frame0.text1 get 1.0 end\]
        if {\"\[info commands XFDestroy\]\" != \"\"} {
          catch {XFDestroy $xfTextBox(toplevelName)}
        } {
          catch {destroy $xfTextBox(toplevelName)}
        }
        $xfTextBoxCommand"
    catch "$xfTextBox(toplevelName).frame1.button0 config $tmpButtonOpt"

    pack $xfTextBox(toplevelName).frame1.button0 -side left -padx 4 -pady 4 -expand 1
  }

  $xfTextBox(toplevelName).frame0.text1 insert end "$xfTextBoxMessage"

  $xfTextBox(toplevelName).frame0.text1 config \
    -state $xfTextBox(state)

  # packing
  pack append $xfTextBox(toplevelName).frame0 \
              $xfTextBox(toplevelName).frame0.vscroll "$xfTextBox(scrollSide) filly" \
              $xfTextBox(toplevelName).frame0.hscroll {bottom fillx} \
              $xfTextBox(toplevelName).frame0.text1 {left fill expand}
  pack append $xfTextBox(toplevelName) \
              $xfTextBox(toplevelName).frame1 {bottom fill} \
              $xfTextBox(toplevelName).frame0 {top fill expand}
}

# eof

