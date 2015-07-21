# XFNoParsing
# Program: template
# Description: read TCL commands and send them to TCL
#
# $Header: xfreadBox.tcl[2.3] Wed Mar 10 12:08:10 1993 garfield@garfield frozen $

global xfReadBox
set xfReadBox(activeBackground) ""
set xfReadBox(activeForeground) ""
set xfReadBox(background) ""
set xfReadBox(font) ""
set xfReadBox(foreground) ""
set xfReadBox(scrollActiveForeground) ""
set xfReadBox(scrollBackground) ""
set xfReadBox(scrollForeground) ""

proc XFReadBox {} {# xf ignore me 5
##########
# Procedure: XFReadBox
# Description: show a box where tcl code can be entered and
#              evaluated
# Arguments: none
# Returns: none
# Sideeffects: may be everything...
##########
#
# global xfReadBox(activeBackground) - active background color
# global xfReadBox(activeForeground) - active foreground color
# global xfReadBox(background) - background color
# global xfReadBox(font) - text font
# global xfReadBox(foreground) - foreground color
# global xfReadBox(scrollActiveForeground) - scrollbar active background color
# global xfReadBox(scrollBackground) - scrollbar background color
# global xfReadBox(scrollForeground) - scrollbar foreground color
# global xfReadBox(scrollSide) - side where scrollbar is located

  global xfReadBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScrollOpt ""
  if {"$xfReadBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfReadBox(activeBackground)\" "
  }
  if {"$xfReadBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfReadBox(activeForeground)\" "
  }
  if {"$xfReadBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfReadBox(background)\" "
    append tmpFrameOpt "-background \"$xfReadBox(background)\" "
    append tmpMessageOpt "-background \"$xfReadBox(background)\" "
  }
  if {"$xfReadBox(font)" != ""} {
    append tmpButtonOpt "-font \"$xfReadBox(font)\" "
    append tmpMessageOpt "-font \"$xfReadBox(font)\" "
  }
  if {"$xfReadBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfReadBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfReadBox(foreground)\" "
  }
  if {"$xfReadBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$xfReadBox(scrollActiveForeground)\" "
  }
  if {"$xfReadBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$xfReadBox(scrollBackground)\" "
  }
  if {"$xfReadBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$xfReadBox(scrollForeground)\" "
  }

  # build widget structure

  XFTmpltToplevel .xfReadBox 420x210 {XF read TCL commands}

  frame .xfReadBox.frame1 \
    -borderwidth 2 \
    -relief raised
  catch ".xfReadBox.frame1 config $tmpFrameOpt"
 
  button .xfReadBox.frame1.send \
    -text "Send" \
    -command {
      if {[catch "[string trim [.xfReadBox.text1.text1 get 1.0 end]]" result]} {
        if {"[info commands XFAlertBox]" != ""} {
          XFAlertBox "$result"
        } {
          puts stderr "$result"
        }
      }
      flush stderr
      flush stdout}
  catch ".xfReadBox.frame1.send config $tmpButtonOpt"

  button .xfReadBox.frame1.sendAndClear \
    -text "Send + Clear" \
    -command {
      if {[catch "[string trim [.xfReadBox.text1.text1 get 1.0 end]]" result]} {
        if {"[info commands XFAlertBox]" != ""} {
          XFAlertBox "$result"
        } {
          puts stderr "$result"
        }
      }
      flush stderr
      flush stdout
      .xfReadBox.text1.text1 delete 1.0 end}
  catch ".xfReadBox.frame1.sendAndClear config $tmpButtonOpt"

  button .xfReadBox.frame1.clear \
    -text "Clear" \
    -command {.xfReadBox.text1.text1 delete 1.0 end}
  catch ".xfReadBox.frame1.clear config $tmpButtonOpt"

  button .xfReadBox.frame1.ok \
    -text "OK" \
    -command {
      if {"[info commands XFDestroy]" != ""} {
        catch {XFDestroy .xfReadBox}
      } {
        catch {destroy .xfReadBox}
      }}
  catch ".xfReadBox.frame1.ok config $tmpButtonOpt"

  frame .xfReadBox.text1 \
    -borderwidth 0\
    -relief raised
  catch ".xfReadBox.text1 config $tmpFrameOpt"
 
  scrollbar .xfReadBox.text1.scrollbar1 \
    -command {.xfReadBox.text1.text1 yview} \
    -highlightthickness 0 \
    -relief "raised"
  catch ".xfReadBox.text1.scrollbar1 config $tmpScrollOpt"

  text .xfReadBox.text1.text1 \
    -borderwidth "2" \
    -exportselection "true" \
    -insertbackground "blue" \
    -highlightthickness 0 \
    -relief "raised" \
    -yscrollcommand ".xfReadBox.text1.scrollbar1 set"
  catch ".xfReadBox.text1.text1 config $tmpMessageOpt"

  # packing
  pack .xfReadBox.frame1.send -side left -padx 4 -pady 4 -expand 1
  pack .xfReadBox.frame1.sendAndClear -side left -padx 4 -pady 4 -expand 1
  pack .xfReadBox.frame1.clear -side left -padx 4 -pady 4 -expand 1
  pack .xfReadBox.frame1.ok -side left -padx 4 -pady 4 -expand 1
  pack .xfReadBox.text1.scrollbar1 -side $xfReadBox(scrollSide) -fill y
  pack .xfReadBox.text1.text1 -side top -expand 1 -fill both
  pack .xfReadBox.frame1 -side bottom -fill both
  pack .xfReadBox.text1 -side top -fill both -expand 1
}

# eof

