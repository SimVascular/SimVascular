# XFNoParsing
# Program: template
# Description: read TCL commands and send them to TCL
#
# $Header: ReadBox.t[2.4] Wed Mar 10 12:03:36 1993 garfield@garfield frozen $

global readBox
set readBox(activeBackground) ""
set readBox(activeForeground) ""
set readBox(background) ""
set readBox(font) ""
set readBox(foreground) ""
set readBox(scrollActiveForeground) ""
set readBox(scrollBackground) ""
set readBox(scrollForeground) ""
set readBox(scrollSide) left

proc ReadBox {} {# xf ignore me 5
##########
# Procedure: ReadBox
# Description: show a box where tcl code can be entered and
#              evaluated
# Arguments: none
# Returns: none
# Sideeffects: may be everything...
##########
#
# global readBox(activeBackground) - active background color
# global readBox(activeForeground) - active foreground color
# global readBox(background) - background color
# global readBox(font) - text font
# global readBox(foreground) - foreground color
# global readBox(scrollActiveForeground) - scrollbar active background color
# global readBox(scrollBackground) - scrollbar background color
# global readBox(scrollForeground) - scrollbar foreground color
# global readBox(scrollSide) - side where scrollbar is located

  global readBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScrollOpt ""
  if {"$readBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$readBox(activeBackground)\" "
  }
  if {"$readBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$readBox(activeForeground)\" "
  }
  if {"$readBox(background)" != ""} {
    append tmpButtonOpt "-background \"$readBox(background)\" "
    append tmpFrameOpt "-background \"$readBox(background)\" "
    append tmpMessageOpt "-background \"$readBox(background)\" "
  }
  if {"$readBox(font)" != ""} {
    append tmpButtonOpt "-font \"$readBox(font)\" "
    append tmpMessageOpt "-font \"$readBox(font)\" "
  }
  if {"$readBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$readBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$readBox(foreground)\" "
  }
  if {"$readBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$readBox(scrollActiveForeground)\" "
  }
  if {"$readBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$readBox(scrollBackground)\" "
  }
  if {"$readBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$readBox(scrollForeground)\" "
  }

  # build widget structure

  # start build of toplevel
  if {"[info commands XFDestroy]" != ""} {
    catch {XFDestroy .readBox}
  } {
    catch {destroy .readBox}
  }
  toplevel .readBox \
    -borderwidth 0
  catch ".readBox config $tmpFrameOpt"
  wm geometry .readBox 420x300
  wm title .readBox {Read TCL commands}
  wm maxsize .readBox 1000 1000
  wm minsize .readBox 100 100
  # end build of toplevel

  button .readBox.ok \
    -text "OK" \
    -command {
      if {"[info commands XFDestroy]" != ""} {
        catch {XFDestroy .readBox}
      } {
        catch {destroy .readBox}
      }}
  catch ".readBox.ok config $tmpButtonOpt"

  frame .readBox.frame1 \
    -borderwidth 0 \
    -relief raised
  catch ".readBox.frame1 config $tmpFrameOpt"
 
  button .readBox.frame1.send \
    -text "Send" \
    -command {
      if {[catch "[string trim [.readBox.text1.text1 get 1.0 end]]" result]} {
        if {"[info commands AlertBox]" != ""} {
          AlertBox "$result"
        } {
          puts stderr "$result"
        }
      }
      flush stderr
      flush stdout}
  catch ".readBox.frame1.send config $tmpButtonOpt"

  button .readBox.frame1.sendAndClear \
    -text "Send + Clear" \
    -command {
      if {[catch "[string trim [.readBox.text1.text1 get 1.0 end]]" result]} {
        if {"[info commands AlertBox]" != ""} {
          AlertBox "$result"
        } {
          puts stderr "$result"
        }
      }
      flush stderr
      flush stdout
      .readBox.text1.text1 delete 1.0 end}
  catch ".readBox.frame1.sendAndClear config $tmpButtonOpt"

  button .readBox.frame1.clear \
    -text "Clear" \
    -command {.readBox.text1.text1 delete 1.0 end}
  catch ".readBox.frame1.clear config $tmpButtonOpt"

  frame .readBox.text1 \
    -borderwidth 0 \
    -relief raised
  catch ".readBox.text1 config $tmpFrameOpt"
 
  scrollbar .readBox.text1.scrollbar1 \
    -command {.readBox.text1.text1 yview} \
    -relief "raised"
  catch ".readBox.text1.scrollbar1 config $tmpScrollOpt"

  text .readBox.text1.text1 \
    -borderwidth "2" \
    -exportselection "true" \
    -insertbackground "blue" \
    -relief "raised" \
    -yscrollcommand ".readBox.text1.scrollbar1 set"
  catch ".readBox.text1.text1 config $tmpMessageOpt"

  # packing
  pack append .readBox.frame1 \
              .readBox.frame1.send {left fill expand} \
              .readBox.frame1.sendAndClear {left fill expand} \
              .readBox.frame1.clear {left fill expand}
  pack append .readBox.text1 \
              .readBox.text1.scrollbar1 "$readBox(scrollSide) filly" \
              .readBox.text1.text1 {top expand fill}
  pack append .readBox \
              .readBox.ok {bottom fill} \
              .readBox.frame1 {bottom fill} \
              .readBox.text1 {bottom fill expand}
}

# eof

