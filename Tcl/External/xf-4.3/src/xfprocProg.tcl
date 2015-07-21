# Program: xf
# Description: procedures that implement the info handling
#
# $Header: xfprocProg.tcl[2.5] Wed Mar 10 12:08:07 1993 garfield@garfield frozen $

proc XFProcProgCommands {{xfEntryW ""}} {
##########
# Procedure: XFProcProgCommands
# Description: show/edit list of commands
# Arguments: xfEntryW - the target entry widget
# Returns: none
# Sideeffects: none
##########

  XFInfoCommands $xfEntryW
}

proc XFProcProgEditScript {} {
##########
# Procedure: XFProcProgEditScript
# Description: edit the current script
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfConf
  global xfPath
  global xfStatus
  global xfTextBox

  XFEditSetStatus "Editing script..."
  XFSave $xfPath(tmp)/eb$xfStatus(uniqueId)

  set xfSavProgName $xfConf(programName)
  set xfTextBox(state) normal
  if {"$xfConf(externalEditor)" == ""} {
    if {[XFTextBoxFile $xfPath(tmp)/eb$xfStatus(uniqueId) "" "" \
          "XF: Current script" "OK" "Cancel"] == 0} {
      XFMiscClearInterpreter
      eval $xfTextBox(contents)
      XFMiscBindWidgetTree .
      XFEditSetShowWindows
      XFEditSetPath .
    }
  } {
    # call external editor
     XFMiscCallExternalEditor2 $xfPath(tmp)/eb$xfStatus(uniqueId) \
       cmplts cmplt
  }

  set xfConf(programName) $xfSavProgName
  XFEditSetStatus "Editing script...done"
}

proc XFProcProgEndSrc {} {
##########
# Procedure: XFProcProgEndSrc
# Description: show/edit the end source
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfConf

  if {"$xfConf(externalEditor)" == ""} {
    XFInfoProcedures "" EndSrc
  } {
    XFMiscCallExternalEditor procs EndSrc
  }
}

proc XFProcProgErrors {} {
##########
# Procedure: XFProcProgErrors
# Description: show the error status
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling error status..."
  XFInfoErrors
  XFEditSetStatus "Calling error status...done"
}

proc XFProcProgGlobals {{xfEntryW ""}} {
##########
# Procedure: XFProcProgGlobals
# Description: show/edit list of global variables
# Arguments: xfEntryW - the target entry widget
# Returns: none
# Sideeffects: none
##########

  XFInfoGlobals $xfEntryW
}

proc XFProcProgProcs {{xfEntryW ""}} {
##########
# Procedure: XFProcProgProcs
# Description: show/edit list of procedures
# Arguments: xfEntryW - the target entry widget
# Returns: none
# Sideeffects: none
##########

  XFInfoProcedures $xfEntryW
}

proc XFProcProgShowScript {} {
##########
# Procedure: XFProcProgShowScript
# Description: show the current script
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfPath
  global xfStatus
  global xfTextBox

  XFEditSetStatus "Showing script..."
  XFSave $xfPath(tmp)/sb$xfStatus(uniqueId)
  set xfTextBox(toplevelName) .xfTextBoxScript
  XFProcTextFile $xfPath(tmp)/sb$xfStatus(uniqueId) 400x400 {XF current script} {}
  set xfTextBox(toplevelName) .xfTextBox
  catch "Rm $xfPath(tmp)/sb$xfStatus(uniqueId)"
  XFEditSetStatus "Showing script...done"
}

proc XFProcProgStartupSrc {} {
##########
# Procedure: XFProcProgStartupSrc
# Description: show/edit the startup source
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfConf

  if {"$xfConf(externalEditor)" == ""} {
    XFInfoProcedures "" StartupSrc
  } {
    XFMiscCallExternalEditor procs StartupSrc
  }
}

proc XFProcProgWidgetTree {} {
##########
# Procedure: XFProcProgWidgetTree
# Description: show the widget tree in a canvas
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Show widget tree..."
  XFInfoWidgetTree
  XFEditSetStatus "Show widget tree...done"
}

# eof

