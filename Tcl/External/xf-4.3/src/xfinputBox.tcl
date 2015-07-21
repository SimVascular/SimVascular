# XFNoParsing
# Program: template
# Description: input box
#
# $Header: xfinputBox.tcl[2.3] Wed Mar 10 12:06:25 1993 garfield@garfield frozen $

global xfInputBox
set xfInputBox(activeBackground) ""
set xfInputBox(activeForeground) ""
set xfInputBox(anchor) n
set xfInputBox(background) ""
set xfInputBox(erase) 1
set xfInputBox(font) ""
set xfInputBox(foreground) ""
set xfInputBox(justify) center
set xfInputBox(scrollActiveForeground) ""
set xfInputBox(scrollBackground) ""
set xfInputBox(scrollForeground) ""
set xfInputBox(toplevelName) .xfInputBox

proc XFInputBoxOne {{xfInputBoxMessage "Input box:"} {xfInputBoxCommandOk ""} {xfInputBoxCommandCancel ""} {xfInputBoxGeometry 350x150} {xfInputBoxTitle "Input box"}} {# xf ignore me 5
##########
# Procedure: XFInputBoxOne
# Description: show input box with one text line
# Arguments: {xfInputBoxMessage} - message to display
#            {xfInputBoxCommandOk} - the command to call after ok
#            {xfInputBoxCommandCancel} - the command to call after cancel
#            {xfInputBoxGeometry} - the geometry for the window
#            {xfInputBoxTitle} - the title for the window
# Returns: The entered text
# Sideeffects: none
# Notes: there exist also a function called:
#          XFInputBoxMulti - to enter multiline text
##########
#
# global xfInputBox(activeBackground) - active background color
# global xfInputBox(activeForeground) - active foreground color
# global xfInputBox(anchor) - anchor for message box
# global xfInputBox(background) - background color
# global xfInputBox(erase) - erase previous text
# global xfInputBox(font) - message font
# global xfInputBox(foreground) - foreground color
# global xfInputBox(justify) - justify for message box
# global xfInputBox(scrollActiveForeground) - scrollbar active background color
# global xfInputBox(scrollBackground) - scrollbar background color
# global xfInputBox(scrollForeground) - scrollbar foreground color
# global xfInputBox(scrollSide) - side where scrollbar is located
# global xfInputBox(toplevelName) - the toplevel name
# global xfInputBox(toplevelName,inputOne) - the text in the entry widget

  global xfInputBox

  if {$xfInputBox(erase)} {
    set xfInputBox($xfInputBox(toplevelName),inputOne) ""
  } {
    if {![info exists xfInputBox($xfInputBox(toplevelName),inputOne)]} {
      set xfInputBox($xfInputBox(toplevelName),inputOne) ""
    }
  }
  XFInputBoxInternal $xfInputBoxMessage $xfInputBoxCommandOk $xfInputBoxCommandCancel $xfInputBoxGeometry $xfInputBoxTitle 1

  # wait for the box to be destroyed
  update idletask
  grab $xfInputBox(toplevelName)
  tkwait window $xfInputBox(toplevelName)

  return $xfInputBox($xfInputBox(toplevelName),inputOne)
}

proc XFInputBoxMulti {{xfInputBoxMessage "Input box:"} {xfInputBoxCommandOk ""} {xfInputBoxCommandCancel ""} {xfInputBoxGeometry 350x150} {xfInputBoxTitle "Input box"}} {# xf ignore me 5
##########
# Procedure: XFInputBoxMulti
# Description: show input box with one text line
# Arguments: {xfInputBoxMessage} - message to display
#            {xfInputBoxCommandOk} - the command to call after ok
#            {xfInputBoxCommandCancel} - the command to call after cancel
#            {xfInputBoxGeometry} - the geometry for the window
#            {xfInputBoxTitle} - the title for the window
# Returns: The entered text
# Sideeffects: none
# Notes: there exist also a function called:
#          XFInputBoxOne - to enter one line text
##########
#
# global xfInputBox(activeBackground) - active background color
# global xfInputBox(activeForeground) - active foreground color
# global xfInputBox(anchor) - anchor for message box
# global xfInputBox(background) - background color
# global xfInputBox(erase) - erase previous text
# global xfInputBox(font) - message font
# global xfInputBox(foreground) - foreground color
# global xfInputBox(justify) - justify for message box
# global xfInputBox(scrollActiveForeground) - scrollbar active background color
# global xfInputBox(scrollBackground) - scrollbar background color
# global xfInputBox(scrollForeground) - scrollbar foreground color
# global xfInputBox(scrollSide) - side where scrollbar is located
# global xfInputBox(toplevelName) - the toplevel name
# global xfInputBox(toplevelName,inputMulti) - the text in the text widget

  global xfInputBox

  if {"$xfInputBoxGeometry" == ""} {
    set xfInputBoxGeometry 350x150
  }
  if {$xfInputBox(erase)} {
    set xfInputBox($xfInputBox(toplevelName),inputMulti) ""
  } {
    if {![info exists xfInputBox($xfInputBox(toplevelName),inputMulti)]} {
      set xfInputBox($xfInputBox(toplevelName),inputMulti) ""
    }
  }
  XFInputBoxInternal $xfInputBoxMessage $xfInputBoxCommandOk $xfInputBoxCommandCancel $xfInputBoxGeometry $xfInputBoxTitle 2

  # wait for the box to be destroyed
  update idletask
  grab $xfInputBox(toplevelName)
  tkwait window $xfInputBox(toplevelName)

  return $xfInputBox($xfInputBox(toplevelName),inputMulti)
}

##########
# Procedure: XFInputBoxInternal
# Description: show input box internal
# Arguments: xfInputBoxMessage - the text to display
#            xfInputBoxCommandOk - the command to call after ok
#            xfInputBoxCommandCancel - the command to call after cancel
#            xfInputBoxGeometry - the geometry for the window
#            xfInputBoxTitle - the title for the window
#            lineNum - number of lines
# Returns: none
# Sideeffects: none
##########
proc XFInputBoxInternal {xfInputBoxMessage xfInputBoxCommandOk xfInputBoxCommandCancel xfInputBoxGeometry xfInputBoxTitle lineNum} {# xf ignore me 6
  global xfInputBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$xfInputBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfInputBox(activeBackground)\" "
  }
  if {"$xfInputBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfInputBox(activeForeground)\" "
  }
  if {"$xfInputBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfInputBox(background)\" "
    append tmpFrameOpt "-background \"$xfInputBox(background)\" "
    append tmpMessageOpt "-background \"$xfInputBox(background)\" "
  }
  if {"$xfInputBox(font)" != ""} {
    append tmpButtonOpt "-font \"$xfInputBox(font)\" "
    append tmpMessageOpt "-font \"$xfInputBox(font)\" "
  }
  if {"$xfInputBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfInputBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfInputBox(foreground)\" "
  }
  if {"$xfInputBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$xfInputBox(scrollActiveForeground)\" "
  }
  if {"$xfInputBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$xfInputBox(scrollBackground)\" "
  }
  if {"$xfInputBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$xfInputBox(scrollForeground)\" "
  }

  XFTmpltToplevel $xfInputBox(toplevelName) "$xfInputBoxGeometry" "$xfInputBoxTitle"

  message $xfInputBox(toplevelName).message1 \
    -anchor "$xfInputBox(anchor)" \
    -justify "$xfInputBox(justify)" \
    -relief raised \
    -text "$xfInputBoxMessage"
  catch "$xfInputBox(toplevelName).message1 config $tmpMessageOpt"

  set xfTmpWidth \
    [string range $xfInputBoxGeometry 0 [expr [string first x $xfInputBoxGeometry]-1]]
  if {"$xfTmpWidth" != ""} {
    # set message size
    catch "$xfInputBox(toplevelName).message1 configure \
      -width [expr $xfTmpWidth-10]"
  } {
    $xfInputBox(toplevelName).message1 configure \
      -aspect 1500
  }

  frame $xfInputBox(toplevelName).frame0 \
    -borderwidth 0 \
    -relief raised
  catch "$xfInputBox(toplevelName).frame0 config $tmpFrameOpt"

  frame $xfInputBox(toplevelName).frame1 \
    -borderwidth 0 \
    -relief raised
  catch "$xfInputBox(toplevelName).frame1 config $tmpFrameOpt"

  if {$lineNum == 1} {
    scrollbar $xfInputBox(toplevelName).frame1.hscroll \
      -orient "horizontal" \
      -highlightthickness 0 \
      -relief raised \
      -command "$xfInputBox(toplevelName).frame1.input xview"
    catch "$xfInputBox(toplevelName).frame1.hscroll config $tmpScrollOpt"

    entry $xfInputBox(toplevelName).frame1.input \
      -relief raised \
      -xscrollcommand "$xfInputBox(toplevelName).frame1.hscroll set"
    catch "$xfInputBox(toplevelName).frame1.input config $tmpMessageOpt"

    $xfInputBox(toplevelName).frame1.input insert 0 \
      $xfInputBox($xfInputBox(toplevelName),inputOne)
    
    # bindings
    bind $xfInputBox(toplevelName).frame1.input <Return> "
      global xfInputBox
      set xfInputBox($xfInputBox(toplevelName),inputOne) \[$xfInputBox(toplevelName).frame1.input get\]
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy $xfInputBox(toplevelName)}
      } {
        catch {destroy $xfInputBox(toplevelName)}
      }
      $xfInputBoxCommandOk"
    
    # packing
    pack append $xfInputBox(toplevelName).frame1 \
                $xfInputBox(toplevelName).frame1.hscroll {bottom fill} \
                $xfInputBox(toplevelName).frame1.input {top fill expand}
  } {
    text $xfInputBox(toplevelName).frame1.input \
      -relief raised \
      -wrap none \
      -highlightthickness 0 \
      -borderwidth 2 \
      -yscrollcommand "$xfInputBox(toplevelName).frame1.vscroll set" \
      -xscrollcommand "$xfInputBox(toplevelName).frame1.hscroll set"
    catch "$xfInputBox(toplevelName).frame1.input config $tmpMessageOpt"

    scrollbar $xfInputBox(toplevelName).frame1.vscroll \
      -relief raised \
      -highlightthickness 0 \
      -command "$xfInputBox(toplevelName).frame1.input yview"
    catch "$xfInputBox(toplevelName).frame1.vscroll config $tmpScrollOpt"

    scrollbar $xfInputBox(toplevelName).frame1.hscroll \
      -relief raised \
      -orient horizontal \
      -highlightthickness 0 \
      -command "$xfInputBox(toplevelName).frame1.input xview"
    catch "$xfInputBox(toplevelName).frame1.vscroll config $tmpScrollOpt"

    $xfInputBox(toplevelName).frame1.input insert 1.0 \
      $xfInputBox($xfInputBox(toplevelName),inputMulti)

    # bindings
    bind $xfInputBox(toplevelName).frame1.input <Control-Return> "
      global xfInputBox
      set xfInputBox($xfInputBox(toplevelName),inputMulti) \[$xfInputBox(toplevelName).frame1.input get 1.0 end\]
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy $xfInputBox(toplevelName)}
      } {
        catch {destroy $xfInputBox(toplevelName)}
      }
      $xfInputBoxCommandOk"
    bind $xfInputBox(toplevelName).frame1.input <Meta-Return> "
      global xfInputBox
      set xfInputBox($xfInputBox(toplevelName),inputMulti) \[$xfInputBox(toplevelName).frame1.input get 1.0 end\]
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy $xfInputBox(toplevelName)}
      } {
        catch {destroy $xfInputBox(toplevelName)}
      }
      $xfInputBoxCommandOk"

    # packing
    pack append $xfInputBox(toplevelName).frame1 \
                $xfInputBox(toplevelName).frame1.vscroll "$xfInputBox(scrollSide) filly" \
                $xfInputBox(toplevelName).frame1.hscroll {bottom fillx} \
                $xfInputBox(toplevelName).frame1.input {left fill expand}
  }
  
  button $xfInputBox(toplevelName).frame0.button0 \
    -text "OK" \
    -command "
      global xfInputBox
      if {$lineNum == 1} {
        set xfInputBox($xfInputBox(toplevelName),inputOne) \[$xfInputBox(toplevelName).frame1.input get\]
      } {
        set xfInputBox($xfInputBox(toplevelName),inputMulti) \[$xfInputBox(toplevelName).frame1.input get 1.0 end\]
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy $xfInputBox(toplevelName)}
      } {
        catch {destroy $xfInputBox(toplevelName)}
      }
      $xfInputBoxCommandOk"
  catch "$xfInputBox(toplevelName).frame0.button0 config $tmpButtonOpt"

  button $xfInputBox(toplevelName).frame0.button1 \
    -text "Cancel" \
    -command "
      global xfInputBox
      if {$lineNum == 1} {
        set xfInputBox($xfInputBox(toplevelName),inputOne) \"\"
      } {
        set xfInputBox($xfInputBox(toplevelName),inputMulti) \"\"
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy $xfInputBox(toplevelName)}
      } {
        catch {destroy $xfInputBox(toplevelName)}
      }
      $xfInputBoxCommandCancel"
  catch "$xfInputBox(toplevelName).frame0.button1 config $tmpButtonOpt"

  pack append $xfInputBox(toplevelName).frame0 \
              $xfInputBox(toplevelName).frame0.button0 {left fill expand} \
              $xfInputBox(toplevelName).frame0.button1 {left fill expand}

  pack append $xfInputBox(toplevelName) \
              $xfInputBox(toplevelName).frame0 {bottom fill} \
              $xfInputBox(toplevelName).frame1 {bottom fill expand} \
              $xfInputBox(toplevelName).message1 {top fill}
}

# eof

