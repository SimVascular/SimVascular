# XFNoParsing
# Program: template
# Description: yes no box
#
# $Header: xfyesNoBox.tcl[2.3] Wed Mar 10 12:08:37 1993 garfield@garfield frozen $

global xfYesNoBox
set xfYesNoBox(activeBackground) ""
set xfYesNoBox(activeForeground) ""
set xfYesNoBox(anchor) n
set xfYesNoBox(background) ""
set xfYesNoBox(font) "*times-bold-r-normal*24*"
set xfYesNoBox(foreground) ""
set xfYesNoBox(justify) center
set xfYesNoBox(afterYes) 0
set xfYesNoBox(afterNo) 0
set xfYesNoBox(button) 0

proc XFYesNoBox {{xfYesNoBoxMessage {Yes/no message}} {xfYesNoBoxGeometry 350x150}} {# xf ignore me 5
##########
# Procedure: XFYesNoBox
# Description: show yesno box
# Arguments: {xfYesNoBoxMessage} - the text to display
#            {xfYesNoBoxGeometry} - the geometry for the window
# Returns: none
# Sideeffects: none
##########
#
# global xfYesNoBox(activeBackground) - active background color
# global xfYesNoBox(activeForeground) - active foreground color
# global xfYesNoBox(anchor) - anchor for message box
# global xfYesNoBox(background) - background color
# global xfYesNoBox(font) - message font
# global xfYesNoBox(foreground) - foreground color
# global xfYesNoBox(justify) - justify for message box
# global xfYesNoBox(afterNo) - destroy yes-no box after n seconds.
#                            The no button is activated
# global xfYesNoBox(afterYes) - destroy yes-no box after n seconds.
#                             The yes button is activated

  global xfYesNoBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  if {"$xfYesNoBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfYesNoBox(activeBackground)\" "
  }
  if {"$xfYesNoBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfYesNoBox(activeForeground)\" "
  }
  if {"$xfYesNoBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfYesNoBox(background)\" "
    append tmpFrameOpt "-background \"$xfYesNoBox(background)\" "
    append tmpMessageOpt "-background \"$xfYesNoBox(background)\" "
  }
  if {"$xfYesNoBox(font)" != ""} {
    append tmpButtonOpt "-font \"$xfYesNoBox(font)\" "
    append tmpMessageOpt "-font \"$xfYesNoBox(font)\" "
  }
  if {"$xfYesNoBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfYesNoBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfYesNoBox(foreground)\" "
  }

  XFTmpltToplevel .xfYesNoBox "$xfYesNoBoxGeometry" {XF yes/no box}

  message .xfYesNoBox.message1 \
    -anchor "$xfYesNoBox(anchor)" \
    -justify "$xfYesNoBox(justify)" \
    -relief raised \
    -text "$xfYesNoBoxMessage"
  catch ".xfYesNoBox.message1 config $tmpMessageOpt"

  set xfTmpWidth \
    [string range $xfYesNoBoxGeometry 0 [expr [string first x $xfYesNoBoxGeometry]-1]]
  if {"$xfTmpWidth" != ""} {
    # set message size
    catch ".xfYesNoBox.message1 configure \
      -width [expr $xfTmpWidth-10]"
  } {
    .xfYesNoBox.message1 configure \
      -aspect 1500
  }

  frame .xfYesNoBox.frame1 \
    -borderwidth 2 \
    -relief raised
  catch ".xfYesNoBox.frame1 config $tmpFrameOpt"

  button .xfYesNoBox.frame1.button0 \
    -text "Yes" \
    -command "
      global xfYesNoBox
      set xfYesNoBox(button) 1
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfYesNoBox}
      } {
        catch {destroy .xfYesNoBox}
      }"
  catch ".xfYesNoBox.frame1.button0 config $tmpButtonOpt"

  button .xfYesNoBox.frame1.button1 \
    -text "No" \
    -command "
      global xfYesNoBox
      set xfYesNoBox(button) 0
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfYesNoBox}
      } {
        catch {destroy .xfYesNoBox}
      }"
  catch ".xfYesNoBox.frame1.button1 config $tmpButtonOpt"

  pack .xfYesNoBox.frame1.button0 -side left -padx 4 -pady 4 -expand 1
  pack .xfYesNoBox.frame1.button1 -side left -padx 4 -pady 4 -expand 1

  # packing
  pack .xfYesNoBox.frame1 -side bottom -fill both
  pack .xfYesNoBox.message1 -side top -fill both -expand 1

  if {$xfYesNoBox(afterYes) != 0} {
    after [expr $xfYesNoBox(afterYes)*1000] \
      "catch \".xfYesNoBox.frame1.button0 invoke\""
  }
  if {$xfYesNoBox(afterNo) != 0} {
    after [expr $xfYesNoBox(afterNo)*1000] \
      "catch \".xfYesNoBox.frame1.button1 invoke\""
  }

  # wait for the box to be destroyed
  update idletask
  grab .xfYesNoBox
  tkwait window .xfYesNoBox

  return $xfYesNoBox(button)
}

# eof

