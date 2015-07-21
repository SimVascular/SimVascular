# XFNoParsing
# Program: template
# Description: yes no box
#
# $Header: YesNoBox.t[2.3] Wed Mar 10 12:03:47 1993 garfield@garfield frozen $

global yesNoBox
set yesNoBox(activeBackground) ""
set yesNoBox(activeForeground) ""
set yesNoBox(anchor) n
set yesNoBox(background) ""
set yesNoBox(font) "*times-bold-r-normal*24*"
set yesNoBox(foreground) ""
set yesNoBox(justify) center
set yesNoBox(afterYes) 0
set yesNoBox(afterNo) 0
set yesNoBox(button) 0

proc YesNoBox {{yesNoBoxMessage {Yes/no message}} {yesNoBoxGeometry 350x150}} {# xf ignore me 5
##########
# Procedure: YesNoBox
# Description: show yesno box
# Arguments: {yesNoBoxMessage} - the text to display
#            {yesNoBoxGeometry} - the geometry for the window
# Returns: none
# Sideeffects: none
##########
#
# global yesNoBox(activeBackground) - active background color
# global yesNoBox(activeForeground) - active foreground color
# global yesNoBox(anchor) - anchor for message box
# global yesNoBox(background) - background color
# global yesNoBox(font) - message font
# global yesNoBox(foreground) - foreground color
# global yesNoBox(justify) - justify for message box
# global yesNoBox(afterNo) - destroy yes-no box after n seconds.
#                            The no button is activated
# global yesNoBox(afterYes) - destroy yes-no box after n seconds.
#                             The yes button is activated

  global yesNoBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  if {"$yesNoBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$yesNoBox(activeBackground)\" "
  }
  if {"$yesNoBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$yesNoBox(activeForeground)\" "
  }
  if {"$yesNoBox(background)" != ""} {
    append tmpButtonOpt "-background \"$yesNoBox(background)\" "
    append tmpFrameOpt "-background \"$yesNoBox(background)\" "
    append tmpMessageOpt "-background \"$yesNoBox(background)\" "
  }
  if {"$yesNoBox(font)" != ""} {
    append tmpButtonOpt "-font \"$yesNoBox(font)\" "
    append tmpMessageOpt "-font \"$yesNoBox(font)\" "
  }
  if {"$yesNoBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$yesNoBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$yesNoBox(foreground)\" "
  }

  # start build of toplevel
  if {"[info commands XFDestroy]" != ""} {
    catch {XFDestroy .yesNoBox}
  } {
    catch {destroy .yesNoBox}
  }
  toplevel .yesNoBox \
    -borderwidth 0
  catch ".yesNoBox config $tmpFrameOpt"
  if {[catch "wm geometry .yesNoBox $yesNoBoxGeometry"]} {
    wm geometry .yesNoBox 350x150
  }
  wm title .yesNoBox {Alert box}
  wm maxsize .yesNoBox 1000 1000
  wm minsize .yesNoBox 100 100
  # end build of toplevel

  message .yesNoBox.message1 \
    -anchor "$yesNoBox(anchor)" \
    -justify "$yesNoBox(justify)" \
    -relief raised \
    -text "$yesNoBoxMessage"
  catch ".yesNoBox.message1 config $tmpMessageOpt"

  set xfTmpWidth \
    [string range $yesNoBoxGeometry 0 [expr [string first x $yesNoBoxGeometry]-1]]
  if {"$xfTmpWidth" != ""} {
    # set message size
    catch ".yesNoBox.message1 configure \
      -width [expr $xfTmpWidth-10]"
  } {
    .yesNoBox.message1 configure \
      -aspect 1500
  }

  frame .yesNoBox.frame1 \
    -borderwidth 0 \
    -relief raised
  catch ".yesNoBox.frame1 config $tmpFrameOpt"

  button .yesNoBox.frame1.button0 \
    -text "Yes" \
    -command "
      global yesNoBox
      set yesNoBox(button) 1
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .yesNoBox}
      } {
        catch {destroy .yesNoBox}
      }"
  catch ".yesNoBox.frame1.button0 config $tmpButtonOpt"

  button .yesNoBox.frame1.button1 \
    -text "No" \
    -command "
      global yesNoBox
      set yesNoBox(button) 0
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .yesNoBox}
      } {
        catch {destroy .yesNoBox}
      }"
  catch ".yesNoBox.frame1.button1 config $tmpButtonOpt"

  pack append .yesNoBox.frame1 \
              .yesNoBox.frame1.button0 {left fillx expand} \
              .yesNoBox.frame1.button1 {left fillx expand}

  # packing
  pack append .yesNoBox \
              .yesNoBox.frame1 {bottom fill} \
              .yesNoBox.message1 {top fill expand}

  if {$yesNoBox(afterYes) != 0} {
    after [expr $yesNoBox(afterYes)*1000] \
      "catch \".yesNoBox.frame1.button0 invoke\""
  }
  if {$yesNoBox(afterNo) != 0} {
    after [expr $yesNoBox(afterNo)*1000] \
      "catch \".yesNoBox.frame1.button1 invoke\""
  }

  # wait for the box to be destroyed
  update idletask
  grab .yesNoBox
  tkwait window .yesNoBox

  return $yesNoBox(button)
}

# eof

