# XFNoParsing
# Program: template
# Description: alert box
#
# $Header: AlertBoxFd.t[2.3] Wed Mar 10 12:02:34 1993 garfield@garfield frozen $

global alertBox
set alertBox(activeBackground) ""
set alertBox(activeForeground) ""
set alertBox(after) 0
set alertBox(anchor) nw
set alertBox(background) ""
set alertBox(font) ""
set alertBox(foreground) ""
set alertBox(justify) center
set alertBox(toplevelName) .alertBox
set alertBox(button) 0

proc AlertBox {{alertBoxMessage {Alert message}} {alertBoxCommand ""} {alertBoxGeometry 350x150} {alertBoxTitle "Alert box"} args} {# xf ignore me 5
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

proc AlertBoxFd {{alertBoxInFile ""} {alertBoxCommand ""} {alertBoxGeometry 350x150} {alertBoxTitle "Alert box"} args} {# xf ignore me 5
##########
# Procedure: AlertBoxFd
# Description: show alert box containing a filedescriptor
# Arguments: {alertBoxInFile} - a filedescriptor to read. The descriptor
#                               is closed after reading
#            {alertBoxCommand} - the command to call after ok
#            {alertBoxGeometry} - the geometry for the window
#            {alertBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          AlertBox - to display a passed string
#          AlertBoxFile - to open and read a file automatically
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

  # check file existance
  if {"$alertBoxInFile" == ""} {
    puts stderr "No filedescriptor specified"
    return
  }

  set alertBoxMessage [read $alertBoxInFile]
  close $alertBoxInFile

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

proc AlertBoxFile {{alertBoxFile ""} {alertBoxCommand ""} {alertBoxGeometry 350x150} {alertBoxTitle "Alert box"} args} {# xf ignore me 5
##########
# Procedure: AlertBoxFile
# Description: show alert box containing a file
# Arguments: {alertBoxFile} - filename to read
#            {alertBoxCommand} - the command to call after ok
#            {alertBoxGeometry} - the geometry for the window
#            {alertBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          AlertBox - to display a passed string
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

  # check file existance
  if {"$alertBoxFile" == ""} {
    puts stderr "No filename specified"
    return
  }

  if {[catch "open $alertBoxFile r" alertBoxInFile]} {
    puts stderr "$alertBoxInFile"
    return
  }

  set alertBoxMessage [read $alertBoxInFile]
  close $alertBoxInFile

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

##########
# Procedure: AlertBoxInternal
# Description: show alert box internal
# Arguments: alertBoxMessage - the text to display
#            alertBoxCommand - the command to call after ok
#            alertBoxGeometry - the geometry for the window
#            alertBoxTitle - the title for the window
#            args - labels of buttons
# Returns: none
# Sideeffects: none
##########
proc AlertBoxInternal {alertBoxMessage alertBoxCommand alertBoxGeometry alertBoxTitle args} {# xf ignore me 6
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
  toplevel $alertBox(toplevelName) \
    -borderwidth 0
  catch "$alertBox(toplevelName) config $tmpFrameOpt"
  if {[catch "wm geometry $alertBox(toplevelName) $alertBoxGeometry"]} {
    wm geometry $alertBox(toplevelName) 350x150
  }
  wm title $alertBox(toplevelName) $alertBoxTitle
  wm maxsize $alertBox(toplevelName) 1000 1000
  wm minsize $alertBox(toplevelName) 100 100
  # end build of toplevel

  message $alertBox(toplevelName).message1 \
    -anchor "$alertBox(anchor)" \
    -justify "$alertBox(justify)" \
    -relief raised \
    -text "$alertBoxMessage"
  catch "$alertBox(toplevelName).message1 config $tmpMessageOpt"

  set xfTmpWidth \
    [string range $alertBoxGeometry 0 [expr [string first x $alertBoxGeometry]-1]]
  if {"$xfTmpWidth" != ""} {
    # set message size
    catch "$alertBox(toplevelName).message1 configure \
      -width [expr $xfTmpWidth-10]"
  } {
    $alertBox(toplevelName).message1 configure \
      -aspect 1500
  }

  frame $alertBox(toplevelName).frame1 \
    -borderwidth 0 \
    -relief raised
  catch "$alertBox(toplevelName).frame1 config $tmpFrameOpt"

  set alertBoxCounter 0
  set buttonNum [llength $args]
  if {$buttonNum > 0} {
    while {$alertBoxCounter < $buttonNum} {
      button $alertBox(toplevelName).frame1.button$alertBoxCounter \
        -text "[lindex $args $alertBoxCounter]" \
        -command "
          global alertBox
          set alertBox(button) $alertBoxCounter
          if {\"\[info commands XFDestroy\]\" != \"\"} {
            catch {XFDestroy $alertBox(toplevelName)}
          } {
            catch {destroy $alertBox(toplevelName)}
          }"
      catch "$alertBox(toplevelName).frame1.button$alertBoxCounter config $tmpButtonOpt"

      pack append $alertBox(toplevelName).frame1 \
                  $alertBox(toplevelName).frame1.button$alertBoxCounter {left fillx expand}

      incr alertBoxCounter
    }
  } {
    button $alertBox(toplevelName).frame1.button0 \
      -text "OK" \
      -command "
        global alertBox
        set alertBox(button) 0
        if {\"\[info commands XFDestroy\]\" != \"\"} {
          catch {XFDestroy $alertBox(toplevelName)}
        } {
          catch {destroy $alertBox(toplevelName)}
        }
        $alertBoxCommand"
    catch "$alertBox(toplevelName).frame1.button0 config $tmpButtonOpt"

    pack append $alertBox(toplevelName).frame1 \
                $alertBox(toplevelName).frame1.button0 {left fillx expand}
  }

  # packing
  pack append $alertBox(toplevelName) \
              $alertBox(toplevelName).frame1 {bottom fill} \
              $alertBox(toplevelName).message1 {top fill expand}

  if {$alertBox(after) != 0} {
    after [expr $alertBox(after)*1000] \
      "catch \"$alertBox(toplevelName).frame1.button0 invoke\""
  }
}

# eof

