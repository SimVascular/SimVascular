# Program: xf
# Description: procedures that implement external functionality
#
# $Header: xfprocOther.tcl[1.2] Wed Mar 10 12:08:05 1993 garfield@garfield frozen $

proc XFProcColorBox {xfResource xfEntryW {xfTargetW ""}} {
##########
# Procedure: XFProcColorBox
# Description: show the color selection box
# Arguments: xfResource - the resource name of the target field
#            xfEntryW - the widget name for the resulting color name
#            {xfTargetW} - the widget we configure
# Returns: none
# Sideeffects: none
##########
  global xfFile

  XFEditSetStatus "Calling color selection..."
  XFColorBox $xfFile(colors) $xfResource $xfEntryW $xfTargetW
  XFEditSetStatus "Calling color selection...done"
}

proc XFProcCursorBox {xfMessage xfEntryW {xfTargetW ""}} {
##########
# Procedure: XFProcCursorBox
# Description: show the cursor selection box
# Arguments: xfMessage - the message to display
#            xfEntryW - the widget name for the resulting cursor name
#            {xfTargetW} - the widget we configure
# Returns: none
# Sideeffects: none
##########
  global xfFile

  XFEditSetStatus "Calling cursor selection..."
  XFCursorBox $xfFile(cursors) $xfFile(colors) $xfMessage $xfEntryW $xfTargetW
  XFEditSetStatus "Calling cursor selection...done"
}

proc XFProcError {xfMessage} {
##########
# Procedure: XFProcError
# Description: show the a error message in a box
# Arguments: xfMessage - the message to display
# Returns: none
# Sideeffects: none
##########
  global xfAlertBox
  global xfConf
  global xfStatus

  if {"$xfConf(fontMessage)" != ""} {
    set tmpFont $xfAlertBox(font)
    set xfAlertBox(font) $xfConf(fontMessage)
  }
  if {$xfStatus(hasColor)} {
    set tmpActiveBackground $xfAlertBox(activeBackground)
    set tmpActiveForeground $xfAlertBox(activeForeground)
    set tmpBackground $xfAlertBox(background)
    set tmpForeground $xfAlertBox(foreground)
    set xfAlertBox(activeBackground) red4
    set xfAlertBox(activeForeground) white
    set xfAlertBox(background) red1
    set xfAlertBox(foreground) white
  }
  set tmpAnchor $xfAlertBox(anchor)
  set tmpJustify $xfAlertBox(justify)
  set xfAlertBox(anchor) nw
  set xfAlertBox(justify) left
  XFAlertBox $xfMessage "" 400x200 "XF error"
  if {"$xfConf(fontMessage)" != ""} {
    set xfAlertBox(font) $tmpFont
  }
  if {$xfStatus(hasColor)} {
    set xfAlertBox(activeBackground) $tmpActiveBackground
    set xfAlertBox(activeForeground) $tmpActiveForeground
    set xfAlertBox(background) $tmpBackground
    set xfAlertBox(foreground) $tmpForeground
  }
  set xfAlertBox(anchor) $tmpAnchor
  set xfAlertBox(justify) $tmpJustify
}

proc XFProcFontBox {xfResource xfEntryW {xfTargetW ""}} {
##########
# Procedure: XFProcFontBox
# Description: show the font selection box
# Arguments: xfResource - the resource name of the target field
#            xfEntryW - the widget name for the resulting font name
#            {xfTargetW} - the widget we configure
# Returns: none
# Sideeffects: none
##########
  global xfConf
  global xfFile

  XFEditSetStatus "Calling font selection..."
  XFFontBox $xfFile(fonts) $xfResource $xfEntryW $xfTargetW
  XFEditSetStatus "Calling font selection...done"
}

proc XFProcFSBox {xfMessage xfActionOk xfActionCancel {xfFileName ""}} {
##########
# Procedure: XFProcFSBox
# Description: show the file selection box
# Arguments: xfMessage - the message to display
#            xfActionOk - the ok action
#            xfActionCancel - the cancel action
#            xfFileName - predefined (selected) file name
# Returns: selected filename, or nothing
# Sideeffects: none
##########

  XFEditSetStatus "Calling file selection..."
  set xfTmpFile [XFFSBox $xfMessage $xfFileName $xfActionOk $xfActionCancel]
  XFEditSetStatus "Calling file selection...done"
  return $xfTmpFile
}

proc XFProcFSBoxFile {xfW} {
##########
# Procedure: XFProcFSBoxFile
# Description: show the file selection box, which inserts the resulting
#              value into the specified widget
# Arguments: xfW - the widget that contains the filename
# Returns: none
# Sideeffects: none
##########
  global xfFSBox

  XFEditSetStatus "Calling file selection..."
  set xfTmpFile [XFMiscGetText $xfW]
  auto_load XFFSBox
  if {[XFMiscIsFile $xfTmpFile]} {
    set xfFSBox(path) [file dirname $xfTmpFile]
    set xfFSBox(name) [file tail $xfTmpFile]
  } {
    set xfFSBox(path) "./"
    set xfFSBox(name) ""
  }
  if {[string match "./*" $xfFSBox(path)]} {
    set xfFSBox(path) [pwd]/[string range $xfFSBox(path) 2 end]
  } {
    if {"$xfFSBox(path)" == "."} {
      set xfFSBox(path) [pwd]
    }
  }
  XFFSBox "Select file:" "$xfFSBox(name)" "
    if {\"\$xfFSBox(path)\" != \"\" && \[XFMiscIsDir \$xfFSBox(path)\] &&
        \"\$xfFSBox(name)\" != \"\" &&
        \[XFMiscIsFile \$xfFSBox(path)/\$xfFSBox(name)\]} {
      XFMiscSetTextIntoWidget $xfW \"\[string trimright \$xfFSBox(path) \{/@\}\]/\$xfFSBox(name)\"
    }
    XFEditSetStatus \"Calling file selection...done\"
  " {XFEditSetStatus "Calling file selection...aborted"}
}

proc XFProcFSBoxPath {xfW} {
##########
# Procedure: XFProcFSBoxPath
# Description: show the path selection box, which inserts the resulting
#              value into the specified widget
# Arguments: xfW - the widget that contains the pathname
# Returns: none
# Sideeffects: none
##########
  global xfFSBox

  XFEditSetStatus "Calling path selection..."
  auto_load XFFSBox
  set xfFSBox(path) [XFMiscGetText $xfW]
  if {[string match "./*" $xfFSBox(path)]} {
    set xfFSBox(path) [pwd]/[string range $xfFSBox(path) 2 end]
  } {
    if {"$xfFSBox(path)" == "."} {
      set xfFSBox(path) [pwd]
    } {
      if {"$xfFSBox(path)" == ".."} {
        set xfFSBox(path) [file dirname [pwd]]
      }
    }
  }
  XFFSBox "Select path:" "" "
    if {\"\$xfFSBox(path)\" != \"\" && \[XFMiscIsDir \$xfFSBox(path)\]} {
      XFMiscSetTextIntoWidget $xfW \"\$xfFSBox(path)\"
    }
    XFEditSetStatus \"Calling path selection...done\"
  " {XFEditSetStatus "Calling path selection...aborted"}
}

proc XFProcFSBoxPixmap {xfW} {
##########
# Procedure: XFProcFSBoxPixmap
# Description: show the pixmap selection box, which inserts the resulting
#              value into the specified widget
# Arguments: xfW - the widget that contains the pathname
# Returns: none
# Sideeffects: none
##########
  global xfFSBox

  XFEditSetStatus "Calling pixmap selection..."
  auto_load XFFSBox
  set xfFSBox(showPixmap) 1
  set xfTmpFile [string trimleft [XFMiscGetText $xfW] @]
  if {[XFMiscIsFile $xfTmpFile]} {
    set xfFSBox(path) [file dirname $xfTmpFile]
    set xfFSBox(name) [file tail $xfTmpFile]
  } {
    set xfFSBox(path) "./"
    set xfFSBox(name) ""
  }
  if {[string match "./*" $xfFSBox(path)]} {
    set xfFSBox(path) [pwd]/[string range $xfFSBox(path) 2 end]
  } {
    if {"$xfFSBox(path)" == "."} {
      set xfFSBox(path) [pwd]
    }
  }
  XFFSBox "Select pixmap:" "" "
    if {\"\$xfFSBox(path)\" != \"\" && 
        \[XFMiscIsDir \[string trimleft \$xfFSBox(path) @\]\] &&
        \"\$xfFSBox(name)\" != \"\" &&
        \[XFMiscIsFile \[string trimleft \$xfFSBox(path) @\]/\$xfFSBox(name)\]} {
      XFMiscSetTextIntoWidget $xfW \"\$xfFSBox(path)/\$xfFSBox(name)\"
    }
    XFEditSetStatus \"Calling pixmap selection...done\"
  " {XFEditSetStatus "Calling pixmap selection...aborted"}
  set xfFSBox(showPixmap) 0
}

proc XFProcIconBarRemove {xfName {xfPathName ""}} {
##########
# Procedure: XFProcIconBarRemove
# Description: remove the named iconbar
# Arguments: xfName - the name of the iconbar
#            xfPathName - the pathname for the iconbar
# Returns: none
# Sideeffects: none
##########

  XFIconBarRemove $xfName $xfPathName
}

proc XFProcIconBarShow {xfName {xfPathName ""}} {
##########
# Procedure: XFProcIconBarShow
# Description: show the named iconbar
# Arguments: xfName - the name of the iconbar
#            xfPathName - the pathname for the iconbar
# Returns: none
# Sideeffects: none
##########

  XFIconBarShow $xfName $xfPathName
}

proc XFProcInputBoxOne {xfMessage xfGeometry xfTitle} {
##########
# Procedure: XFProcInputBoxOne
# Description: show the one line input box
# Arguments: xfMessage - the message to display
#            xfGeometry - the geometry of the window
#            xfTitle - the title of the window
# Returns: none
# Sideeffects: none
##########

  XFInputBoxOne $xfMessage "" "" $xfGeometry $xfTitle
}

proc XFProcInputBoxMulti {xfMessage xfGeometry xfTitle} {
##########
# Procedure: XFProcInputBoxMulti
# Description: show the multi line input box
# Arguments: xfMessage - the message to display
#            xfGeometry - the geometry of the window
#            xfTitle - the title of the window
# Returns: none
# Sideeffects: none
##########

  XFInputBoxMulti $xfMessage "" "" $xfGeometry $xfTitle
}

proc XFProcKeysymBox {xfMessage xfEntryW} {
##########
# Procedure: XFProcKeysymBox
# Description: show the keysym selection box
# Arguments: xfMessage - the message to display
#            xfEntryW - the widget name for the resulting keysym name
# Returns: none
# Sideeffects: none
##########
  global xfFile

  XFEditSetStatus "Calling keysym selection..."
  XFKeysymBox $xfFile(keysyms) $xfMessage $xfEntryW
  XFEditSetStatus "Calling keysym selection...done"
}

proc XFProcMessage {xfMessage xfGeometry xfTitle xfJustify xfAction} {
##########
# Procedure: XFProcMessage
# Description: show a message in a box
# Arguments: xfMessage - the message to display
#            xfGeometry - the geometry of the window
#            xfTitle - the title of the window
#            xfJustify - the justification of the text
#            xfAction - the action linked to the button
# Returns: none
# Sideeffects: none
##########
  global xfAlertBox

  set tmpAnchor $xfAlertBox(anchor)
  set tmpJustify $xfAlertBox(justify)
  case $xfJustify in {
    {left} {
      set xfAlertBox(anchor) nw
      set xfAlertBox(justify) left
    }
    {right} {
      set xfAlertBox(anchor) ne
      set xfAlertBox(justify) right
    }
    default {
      set xfAlertBox(anchor) n
      set xfAlertBox(justify) $xfJustify
    }
  }
  XFAlertBox $xfMessage $xfAction $xfGeometry $xfTitle
  set xfAlertBox(anchor) $tmpAnchor
  set xfAlertBox(justify) $tmpJustify
}

proc XFProcMessageFile {xfFileName xfGeometry xfTitle xfJustify xfAction} {
##########
# Procedure: XFProcMessageFile
# Description: show a file in a box
# Arguments: xfFileName - the file to display
#            xfGeometry - the geometry of the window
#            xfTitle - the title of the window
#            xfJustify - the justification of the text
#            xfAction - the action linked to the button
# Returns: none
# Sideeffects: none
##########
  global xfAlertBox

  set tmpAnchor $xfAlertBox(anchor)
  set tmpJustify $xfAlertBox(justify)
  case $xfJustify in {
    {left} {
      set xfAlertBox(anchor) nw
      set xfAlertBox(justify) left
    }
    {right} {
      set xfAlertBox(anchor) ne
      set xfAlertBox(justify) right
    }
    default {
      set xfAlertBox(anchor) n
      set xfAlertBox(justify) $xfJustify
    }
  }
  XFAlertBoxFile $xfFileName $xfAction $xfGeometry $xfTitle
  set xfAlertBox(anchor) $tmpAnchor
  set xfAlertBox(justify) $tmpJustify
}

proc XFProcText {xfMessage xfGeometry xfTitle xfAction} {
##########
# Procedure: XFProcText
# Description: show a text in a box
# Arguments: xfMessage - the text to display
#            xfGeometry - the geometry of the window
#            xfTitle - the title of the window
#            xfAction - the action linked to the button
# Returns: none
# Sideeffects: none
##########

  XFTextBox $xfMessage $xfAction $xfGeometry $xfTitle
}

proc XFProcTextFile {xfFileName xfGeometry xfTitle xfAction} {
##########
# Procedure: XFProcText
# Description: show a text in a box
# Arguments: xfFileName - the file to display
#            xfGeometry - the geometry of the window
#            xfTitle - the title of the window
#            xfAction - the action linked to the button
# Returns: none
# Sideeffects: none
##########

  XFTextBoxFile $xfFileName $xfAction $xfGeometry $xfTitle
}

proc XFProcYesNo {xfMessage {xfGeometry 350x150}} {
##########
# Procedure: XFProcYesNo
# Description: show a yes/no box
# Arguments: xfMessage - the message to display
#            {xfGeometry} - the geometry of the window
# Returns: none
# Sideeffects: none
##########

  XFYesNoBox $xfMessage $xfGeometry
}

# eof

