# XFNoParsing
# Program: template
# Description: text box
#
# $Header: TextBox.t[2.3] Wed Mar 10 12:03:38 1993 garfield@garfield frozen $

global textBox
set textBox(activeBackground) ""
set textBox(activeForeground) ""
set textBox(background) ""
set textBox(font) ""
set textBox(foreground) ""
set textBox(scrollActiveForeground) ""
set textBox(scrollBackground) ""
set textBox(scrollForeground) ""
set textBox(scrollSide) left
set textBox(state) disabled
set textBox(toplevelName) .textBox
set textBox(button) 0
set textBox(contents) ""

proc TextBox {{textBoxMessage {Text message}} {textBoxCommand ""} {textBoxGeometry 350x150} {textBoxTitle "Text box"} args} {# xf ignore me 5
##########
# Procedure: TextBox
# Description: show text box
# Arguments: {textBoxMessage} - the text to display
#            {textBoxCommand} - the command to call after ok
#            {textBoxGeometry} - the geometry for the window
#            {textBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, or nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          TextBoxFile - to open and read a file automatically
#          TextBoxFd - to read from an already opened filedescriptor
##########
#
# global textBox(activeBackground) - active background color
# global textBox(activeForeground) - active foreground color
# global textBox(background) - background color
# global textBox(font) - text font
# global textBox(foreground) - foreground color
# global textBox(scrollActiveForeground) - scrollbar active background color
# global textBox(scrollBackground) - scrollbar background color
# global textBox(scrollForeground) - scrollbar foreground color
# global textBox(scrollSide) - side where scrollbar is located

  global textBox

  # show text box
  if {[llength $args] > 0} {
    eval TextBoxInternal "\{$textBoxMessage\}" "\{$textBoxCommand\}" "\{$textBoxGeometry\}" "\{$textBoxTitle\}" $args
  } {
    TextBoxInternal $textBoxMessage $textBoxCommand $textBoxGeometry $textBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $textBox(toplevelName)
    tkwait window $textBox(toplevelName)

    return $textBox(button)
  }
}

proc TextBoxFd {{textBoxInFile ""} {textBoxCommand ""} {textBoxGeometry 350x150} {textBoxTitle "Text box"} args} {# xf ignore me 5
##########
# Procedure: TextBoxFd
# Description: show text box containing a filedescriptor
# Arguments: {textBoxInFile} - a filedescriptor to read. The descriptor
#                              is closed after reading
#            {textBoxCommand} - the command to call after ok
#            {textBoxGeometry} - the geometry for the window
#            {textBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          TextBox - to display a passed string
#          TextBoxFile - to open and read a file automatically
##########
#
# global textBox(activeBackground) - active background color
# global textBox(activeForeground) - active foreground color
# global textBox(background) - background color
# global textBox(font) - text font
# global textBox(foreground) - foreground color
# global textBox(scrollActiveForeground) - scrollbar active background color
# global textBox(scrollBackground) - scrollbar background color
# global textBox(scrollForeground) - scrollbar foreground color
# global textBox(scrollSide) - side where scrollbar is located

  global textBox

  # check file existance
  if {"$textBoxInFile" == ""} {
    puts stderr "No filedescriptor specified"
    return
  }

  set textBoxMessage [read $textBoxInFile]
  close $textBoxInFile

  # show text box
  if {[llength $args] > 0} {
    eval TextBoxInternal "\{$textBoxMessage\}" "\{$textBoxCommand\}" "\{$textBoxGeometry\}" "\{$textBoxTitle\}" $args
  } {
    TextBoxInternal $textBoxMessage $textBoxCommand $textBoxGeometry $textBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $textBox(toplevelName)
    tkwait window $textBox(toplevelName)

    return $textBox(button)
  }
}

proc TextBoxFile {{textBoxFile ""} {textBoxCommand ""} {textBoxGeometry 350x150} {textBoxTitle "Text box"} args} {# xf ignore me 5
##########
# Procedure: TextBoxFile
# Description: show text box containing a file
# Arguments: {textBoxFile} - filename to read
#            {textBoxCommand} - the command to call after ok
#            {textBoxGeometry} - the geometry for the window
#            {textBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          TextBox - to display a passed string
#          TextBoxFd - to read from an already opened filedescriptor
##########
#
# global textBox(activeBackground) - active background color
# global textBox(activeForeground) - active foreground color
# global textBox(background) - background color
# global textBox(font) - text font
# global textBox(foreground) - foreground color
# global textBox(scrollActiveForeground) - scrollbar active background color
# global textBox(scrollBackground) - scrollbar background color
# global textBox(scrollForeground) - scrollbar foreground color
# global textBox(scrollSide) - side where scrollbar is located

  global textBox

  # check file existance
  if {"$textBoxFile" == ""} {
    puts stderr "No filename specified"
    return
  }

  if {[catch "open $textBoxFile r" textBoxInFile]} {
    puts stderr "$textBoxInFile"
    return
  }

  set textBoxMessage [read $textBoxInFile]
  close $textBoxInFile

  # show text box
  if {[llength $args] > 0} {
    eval TextBoxInternal "\{$textBoxMessage\}" "\{$textBoxCommand\}" "\{$textBoxGeometry\}" "\{$textBoxTitle\}" $args
  } {
    TextBoxInternal $textBoxMessage $textBoxCommand $textBoxGeometry $textBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $textBox(toplevelName)
    tkwait window $textBox(toplevelName)

    return $textBox(button)
  }
}

##########
# Procedure: TextBoxInternal
# Description: show text box internal
# Arguments: textBoxMessage - the text to display
#            textBoxCommand - the command to call after ok
#            textBoxGeometry - the geometry for the window
#            textBoxTitle - the title for the window
#            args - labels of buttons
# Returns: none
# Sideeffects: none
##########
proc TextBoxInternal {textBoxMessage textBoxCommand textBoxGeometry textBoxTitle args} {# xf ignore me 6
  global textBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScrollOpt ""
  if {"$textBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$textBox(activeBackground)\" "
  }
  if {"$textBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$textBox(activeForeground)\" "
  }
  if {"$textBox(background)" != ""} {
    append tmpButtonOpt "-background \"$textBox(background)\" "
    append tmpFrameOpt "-background \"$textBox(background)\" "
    append tmpMessageOpt "-background \"$textBox(background)\" "
  }
  if {"$textBox(font)" != ""} {
    append tmpButtonOpt "-font \"$textBox(font)\" "
    append tmpMessageOpt "-font \"$textBox(font)\" "
  }
  if {"$textBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$textBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$textBox(foreground)\" "
  }
  if {"$textBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$textBox(scrollActiveForeground)\" "
  }
  if {"$textBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$textBox(scrollBackground)\" "
  }
  if {"$textBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$textBox(scrollForeground)\" "
  }

  # start build of toplevel
  if {"[info commands XFDestroy]" != ""} {
    catch {XFDestroy $textBox(toplevelName)}
  } {
    catch {destroy $textBox(toplevelName)}
  }
  toplevel $textBox(toplevelName) \
    -borderwidth 0
  catch "$textBox(toplevelName) config $tmpFrameOpt"
  if {[catch "wm geometry $textBox(toplevelName) $textBoxGeometry"]} {
    wm geometry $textBox(toplevelName) 350x150
  }
  wm title $textBox(toplevelName) $textBoxTitle
  wm maxsize $textBox(toplevelName) 1000 1000
  wm minsize $textBox(toplevelName) 100 100
  # end build of toplevel

  frame $textBox(toplevelName).frame0 \
    -borderwidth 0 \
    -relief raised
  catch "$textBox(toplevelName).frame0 config $tmpFrameOpt"

  text $textBox(toplevelName).frame0.text1 \
    -relief raised \
    -wrap none \
    -borderwidth 2 \
    -yscrollcommand "$textBox(toplevelName).frame0.vscroll set"
  catch "$textBox(toplevelName).frame0.text1 config $tmpMessageOpt"

  scrollbar $textBox(toplevelName).frame0.vscroll \
    -relief raised \
    -command "$textBox(toplevelName).frame0.text1 yview"
  catch "$textBox(toplevelName).frame0.vscroll config $tmpScrollOpt"

  frame $textBox(toplevelName).frame1 \
    -borderwidth 0 \
    -relief raised
  catch "$textBox(toplevelName).frame1 config $tmpFrameOpt"

  set textBoxCounter 0
  set buttonNum [llength $args]

  if {$buttonNum > 0} {
    while {$textBoxCounter < $buttonNum} {
      button $textBox(toplevelName).frame1.button$textBoxCounter \
        -text "[lindex $args $textBoxCounter]" \
        -command "
          global textBox
          set textBox(button) $textBoxCounter
          set textBox(contents) \[$textBox(toplevelName).frame0.text1 get 1.0 end\]
          if {\"\[info commands XFDestroy\]\" != \"\"} {
            catch {XFDestroy $textBox(toplevelName)}
          } {
            catch {destroy $textBox(toplevelName)}
          }"
      catch "$textBox(toplevelName).frame1.button$textBoxCounter config $tmpButtonOpt"

      pack append $textBox(toplevelName).frame1 \
                  $textBox(toplevelName).frame1.button$textBoxCounter {left fillx expand}

      incr textBoxCounter
    }
  } {
    button $textBox(toplevelName).frame1.button0 \
      -text "OK" \
      -command "
        global textBox
        set textBox(button) 0
        set textBox(contents) \[$textBox(toplevelName).frame0.text1 get 1.0 end\]
        if {\"\[info commands XFDestroy\]\" != \"\"} {
          catch {XFDestroy $textBox(toplevelName)}
        } {
          catch {destroy $textBox(toplevelName)}
        }
        $textBoxCommand"
    catch "$textBox(toplevelName).frame1.button0 config $tmpButtonOpt"

    pack append $textBox(toplevelName).frame1 \
                $textBox(toplevelName).frame1.button0 {left fillx expand}
  }

  $textBox(toplevelName).frame0.text1 insert end "$textBoxMessage"

  $textBox(toplevelName).frame0.text1 config \
    -state $textBox(state)

  # packing
  pack append $textBox(toplevelName).frame0 \
              $textBox(toplevelName).frame0.vscroll "$textBox(scrollSide) filly" \
              $textBox(toplevelName).frame0.text1 {left fill expand}
  pack append $textBox(toplevelName) \
              $textBox(toplevelName).frame1 {bottom fill} \
              $textBox(toplevelName).frame0 {top fill expand}
}

# eof

