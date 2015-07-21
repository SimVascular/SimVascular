# XFNoParsing
# Program: template
# Description: input box
#
# $Header: InputBox.t[2.3] Wed Mar 10 12:03:12 1993 garfield@garfield frozen $

global inputBox
set inputBox(activeBackground) ""
set inputBox(activeForeground) ""
set inputBox(anchor) n
set inputBox(background) ""
set inputBox(erase) 1
set inputBox(font) ""
set inputBox(foreground) ""
set inputBox(justify) center
set inputBox(scrollActiveForeground) ""
set inputBox(scrollBackground) ""
set inputBox(scrollForeground) ""
set inputBox(scrollSide) left
set inputBox(toplevelName) .inputBox

proc InputBoxOne {{inputBoxMessage "Input box:"} {inputBoxCommandOk ""} {inputBoxCommandCancel ""} {inputBoxGeometry 350x150} {inputBoxTitle "Input box"}} {# xf ignore me 5
##########
# Procedure: InputBoxOne
# Description: show input box with one text line
# Arguments: {inputBoxMessage} - message to display
#            {inputBoxCommandOk} - the command to call after ok
#            {inputBoxCommandCancel} - the command to call after cancel
#            {inputBoxGeometry} - the geometry for the window
#            {inputBoxTitle} - the title for the window
# Returns: The entered text
# Sideeffects: none
# Notes: there exist also a function called:
#          InputBoxMulti - to enter multiline text
##########
#
# global inputBox(activeBackground) - active background color
# global inputBox(activeForeground) - active foreground color
# global inputBox(anchor) - anchor for message box
# global inputBox(background) - background color
# global inputBox(erase) - erase previous text
# global inputBox(font) - message font
# global inputBox(foreground) - foreground color
# global inputBox(justify) - justify for message box
# global inputBox(scrollActiveForeground) - scrollbar active background color
# global inputBox(scrollBackground) - scrollbar background color
# global inputBox(scrollForeground) - scrollbar foreground color
# global inputBox(scrollSide) - side where scrollbar is located
# global inputBox(toplevelName) - the toplevel name
# global inputBox(toplevelName,inputOne) - the text in the entry widget

  global inputBox

  if {$inputBox(erase)} {
    set inputBox($inputBox(toplevelName),inputOne) ""
  } {
    if {![info exists inputBox($inputBox(toplevelName),inputOne)]} {
      set inputBox($inputBox(toplevelName),inputOne) ""
    }
  }
  InputBoxInternal $inputBoxMessage $inputBoxCommandOk $inputBoxCommandCancel $inputBoxGeometry $inputBoxTitle 1

  # wait for the box to be destroyed
  update idletask
  grab $inputBox(toplevelName)
  tkwait window $inputBox(toplevelName)

  return $inputBox($inputBox(toplevelName),inputOne)
}

proc InputBoxMulti {{inputBoxMessage "Input box:"} {inputBoxCommandOk ""} {inputBoxCommandCancel ""} {inputBoxGeometry 350x150} {inputBoxTitle "Input box"}} {# xf ignore me 5
##########
# Procedure: InputBoxMulti
# Description: show input box with one text line
# Arguments: {inputBoxMessage} - message to display
#            {inputBoxCommandOk} - the command to call after ok
#            {inputBoxCommandCancel} - the command to call after cancel
#            {inputBoxGeometry} - the geometry for the window
#            {inputBoxTitle} - the title for the window
# Returns: The entered text
# Sideeffects: none
# Notes: there exist also a function called:
#          InputBoxOne - to enter one line text
##########
#
# global inputBox(activeBackground) - active background color
# global inputBox(activeForeground) - active foreground color
# global inputBox(anchor) - anchor for message box
# global inputBox(background) - background color
# global inputBox(erase) - erase previous text
# global inputBox(font) - message font
# global inputBox(foreground) - foreground color
# global inputBox(justify) - justify for message box
# global inputBox(scrollActiveForeground) - scrollbar active background color
# global inputBox(scrollBackground) - scrollbar background color
# global inputBox(scrollForeground) - scrollbar foreground color
# global inputBox(scrollSide) - side where scrollbar is located
# global inputBox(toplevelName) - the toplevel name
# global inputBox(toplevelName,inputMulti) - the text in the text widget

  global inputBox

  if {"$inputBoxGeometry" == ""} {
    set inputBoxGeometry 350x150
  }
  if {$inputBox(erase)} {
    set inputBox($inputBox(toplevelName),inputMulti) ""
  } {
    if {![info exists inputBox($inputBox(toplevelName),inputMulti)]} {
      set inputBox($inputBox(toplevelName),inputMulti) ""
    }
  }
  InputBoxInternal $inputBoxMessage $inputBoxCommandOk $inputBoxCommandCancel $inputBoxGeometry $inputBoxTitle 2

  # wait for the box to be destroyed
  update idletask
  grab $inputBox(toplevelName)
  tkwait window $inputBox(toplevelName)

  return $inputBox($inputBox(toplevelName),inputMulti)
}

##########
# Procedure: InputBoxInternal
# Description: show input box internal
# Arguments: inputBoxMessage - the text to display
#            inputBoxCommandOk - the command to call after ok
#            inputBoxCommandCancel - the command to call after cancel
#            inputBoxGeometry - the geometry for the window
#            inputBoxTitle - the title for the window
#            lineNum - number of lines
# Returns: none
# Sideeffects: none
##########
proc InputBoxInternal {inputBoxMessage inputBoxCommandOk inputBoxCommandCancel inputBoxGeometry inputBoxTitle lineNum} {# xf ignore me 6
  global inputBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$inputBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$inputBox(activeBackground)\" "
  }
  if {"$inputBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$inputBox(activeForeground)\" "
  }
  if {"$inputBox(background)" != ""} {
    append tmpButtonOpt "-background \"$inputBox(background)\" "
    append tmpFrameOpt "-background \"$inputBox(background)\" "
    append tmpMessageOpt "-background \"$inputBox(background)\" "
  }
  if {"$inputBox(font)" != ""} {
    append tmpButtonOpt "-font \"$inputBox(font)\" "
    append tmpMessageOpt "-font \"$inputBox(font)\" "
  }
  if {"$inputBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$inputBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$inputBox(foreground)\" "
  }
  if {"$inputBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$inputBox(scrollActiveForeground)\" "
  }
  if {"$inputBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$inputBox(scrollBackground)\" "
  }
  if {"$inputBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$inputBox(scrollForeground)\" "
  }

  # start build of toplevel
  if {"[info commands XFDestroy]" != ""} {
    catch {XFDestroy $inputBox(toplevelName)}
  } {
    catch {destroy $inputBox(toplevelName)}
  }
  toplevel $inputBox(toplevelName) \
    -borderwidth 0
  catch "$inputBox(toplevelName) config $tmpFrameOpt"
  if {[catch "wm geometry $inputBox(toplevelName) $inputBoxGeometry"]} {
    wm geometry $inputBox(toplevelName) 350x150
  }
  wm title $inputBox(toplevelName) $inputBoxTitle
  wm maxsize $inputBox(toplevelName) 1000 1000
  wm minsize $inputBox(toplevelName) 100 100
  # end build of toplevel

  message $inputBox(toplevelName).message1 \
    -anchor "$inputBox(anchor)" \
    -justify "$inputBox(justify)" \
    -relief raised \
    -text "$inputBoxMessage"
  catch "$inputBox(toplevelName).message1 config $tmpMessageOpt"

  set xfTmpWidth \
    [string range $inputBoxGeometry 0 [expr [string first x $inputBoxGeometry]-1]]
  if {"$xfTmpWidth" != ""} {
    # set message size
    catch "$inputBox(toplevelName).message1 configure \
      -width [expr $xfTmpWidth-10]"
  } {
    $inputBox(toplevelName).message1 configure \
      -aspect 1500
  }

  frame $inputBox(toplevelName).frame0 \
    -borderwidth 0 \
    -relief raised
  catch "$inputBox(toplevelName).frame0 config $tmpFrameOpt"

  frame $inputBox(toplevelName).frame1 \
    -borderwidth 0 \
    -relief raised
  catch "$inputBox(toplevelName).frame1 config $tmpFrameOpt"

  if {$lineNum == 1} {
    scrollbar $inputBox(toplevelName).frame1.hscroll \
      -orient "horizontal" \
      -relief raised \
      -command "$inputBox(toplevelName).frame1.input xview"
    catch "$inputBox(toplevelName).frame1.hscroll config $tmpScrollOpt"

    entry $inputBox(toplevelName).frame1.input \
      -relief raised \
      -xscrollcommand "$inputBox(toplevelName).frame1.hscroll set"
    catch "$inputBox(toplevelName).frame1.input config $tmpMessageOpt"

    $inputBox(toplevelName).frame1.input insert 0 \
      $inputBox($inputBox(toplevelName),inputOne)
    
    # bindings
    bind $inputBox(toplevelName).frame1.input <Return> "
      global inputBox
      set inputBox($inputBox(toplevelName),inputOne) \[$inputBox(toplevelName).frame1.input get\]
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy $inputBox(toplevelName)}
      } {
        catch {destroy $inputBox(toplevelName)}
      }
      $inputBoxCommandOk"
    
    # packing
    pack append $inputBox(toplevelName).frame1 \
                $inputBox(toplevelName).frame1.hscroll {bottom fill} \
                $inputBox(toplevelName).frame1.input {top fill expand}
  } {
    text $inputBox(toplevelName).frame1.input \
      -relief raised \
      -wrap none \
      -borderwidth 2 \
      -yscrollcommand "$inputBox(toplevelName).frame1.vscroll set"
    catch "$inputBox(toplevelName).frame1.input config $tmpMessageOpt"

    scrollbar $inputBox(toplevelName).frame1.vscroll \
      -relief raised \
      -command "$inputBox(toplevelName).frame1.input yview"
    catch "$inputBox(toplevelName).frame1.vscroll config $tmpScrollOpt"

    $inputBox(toplevelName).frame1.input insert 1.0 \
      $inputBox($inputBox(toplevelName),inputMulti)

    # bindings
    bind $inputBox(toplevelName).frame1.input <Control-Return> "
      global inputBox
      set inputBox($inputBox(toplevelName),inputMulti) \[$inputBox(toplevelName).frame1.input get 1.0 end\]
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy $inputBox(toplevelName)}
      } {
        catch {destroy $inputBox(toplevelName)}
      }
      $inputBoxCommandOk"
    bind $inputBox(toplevelName).frame1.input <Meta-Return> "
      global inputBox
      set inputBox($inputBox(toplevelName),inputMulti) \[$inputBox(toplevelName).frame1.input get 1.0 end\]
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy $inputBox(toplevelName)}
      } {
        catch {destroy $inputBox(toplevelName)}
      }
      $inputBoxCommandOk"

    # packing
    pack append $inputBox(toplevelName).frame1 \
                $inputBox(toplevelName).frame1.vscroll "$inputBox(scrollSide) filly" \
                $inputBox(toplevelName).frame1.input {left fill expand}
  }
  
  button $inputBox(toplevelName).frame0.button0 \
    -text "OK" \
    -command "
      global inputBox
      if {$lineNum == 1} {
        set inputBox($inputBox(toplevelName),inputOne) \[$inputBox(toplevelName).frame1.input get\]
      } {
        set inputBox($inputBox(toplevelName),inputMulti) \[$inputBox(toplevelName).frame1.input get 1.0 end\]
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy $inputBox(toplevelName)}
      } {
        catch {destroy $inputBox(toplevelName)}
      }
      $inputBoxCommandOk"
  catch "$inputBox(toplevelName).frame0.button0 config $tmpButtonOpt"

  button $inputBox(toplevelName).frame0.button1 \
    -text "Cancel" \
    -command "
      global inputBox
      if {$lineNum == 1} {
        set inputBox($inputBox(toplevelName),inputOne) \"\"
      } {
        set inputBox($inputBox(toplevelName),inputMulti) \"\"
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy $inputBox(toplevelName)}
      } {
        catch {destroy $inputBox(toplevelName)}
      }
      $inputBoxCommandCancel"
  catch "$inputBox(toplevelName).frame0.button1 config $tmpButtonOpt"

  pack append $inputBox(toplevelName).frame0 \
              $inputBox(toplevelName).frame0.button0 {left fill expand} \
              $inputBox(toplevelName).frame0.button1 {left fill expand}

  pack append $inputBox(toplevelName) \
              $inputBox(toplevelName).frame0 {bottom fill} \
              $inputBox(toplevelName).frame1 {bottom fill expand} \
              $inputBox(toplevelName).message1 {top fill}
}

# eof

