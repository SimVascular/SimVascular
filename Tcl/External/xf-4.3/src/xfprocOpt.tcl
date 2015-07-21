# Program: xf
# Description: procedures that implement the option handling
#
# $Header: xfprocOpt.tcl[2.4] Wed Mar 10 12:08:01 1993 garfield@garfield frozen $

proc XFProcOptionsBindings {} {
##########
# Procedure: XFProcOptionsBindings
# Description: binding options
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling binding options..."
  XFOptionsBind
  XFEditSetStatus "Calling binding options...done"
}

proc XFProcOptionsGeneral {} {
##########
# Procedure: XFProcOptionsGeneral
# Description: general options
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling general options..."
  XFOptionsGeneral
  XFEditSetStatus "Calling general options...done"
}

proc XFProcOptionsIconBar {{xfName "edit"}} {
##########
# Procedure: XFProcOptionsIconBar
# Description: iconbar configuration
# Arguments: {xfName} - the name of the iconbar to configure
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling icon bar configuration..."
  set xfProcList ""
  lappend xfProcList XFProcFileInsert XFProcFileLoad XFProcFileNew XFProcFileQuit XFProcFileEnterTCL XFProcFileSave XFProcFileSaveAs XFProcConfAddCurrentItem XFProcConfBinding XFProcConfBindingAll XFProcConfBindingClass XFProcConfGeometryDefault XFProcConfInsertTemplate XFProcConfInsertWidgetDefault
  lappend xfProcList XFProcConfInsertWidgetConfig XFProcConfLayout XFProcConfPacking XFProcConfParametersDefault XFProcConfParametersGeneral XFProcConfParameters Groups XFProcConfParametersSmall XFProcConfParametersSpecial XFProcConfPlacing XFProcEditClearCut XFProcEditCopy XFProcEditCut
  lappend xfProcList XFProcEditDelete XFProcEditLoadCut XFProcEditLoadTemplate XFProcEditMakeAProc XFProcEditPaste XFProcEditSaveCut XFProcEditSaveCutAsTemplate XFProcEditShowCut XFProcProgCommands XFProcProgEditScript XFProcProgEndSrc XFProcProgErrors XFProcProgGlobals XFProcProgProcs XFProcProgShowScript XFProcProgStartupSrc XFProcProgWidgetTree XFProcMiscAliases

  lappend xfProcList XFProcMiscAppDefaults XFProcMiscHardcopy XFProcMiscModules XFProcMiscImages XFProcMiscPixmaps XFProcMiscTestProgram XFProcOptionsBindings XFProcOptionsGeneral XFProcOptionsIconBar XFProcOptionsInterpreter XFProcOptionsMenuBar XFProcOptionsPathFile XFProcOptionsSource XFProcOptionsVersion XFProcOptionsSaveOptions XFProcOptionsSaveModuleList

  lappend xfProcList XFProcOptionsSavePositions XFProcHelpAbout XFProcHelpHelp XFProcHelpTutorial XFProcColorBox XFProcCursorBox XFProcError XFProcFontBox XFProcFSBox XFProcFSBoxFile XFProcFSBoxPath XFProcIconBarShow XFProcInputBoxOne XFProcInputBoxMulti XFProcKeysymBox XFProcMessage XFProcMessageFd XFProcMessageFile XFProcText XFProcTextFd XFProcTextFile XFProcYesNo
  XFIconBarConf $xfName .xfEdit.frame10 [lsort $xfProcList]
  XFEditSetStatus "Calling icon bar configuration...done"
}

proc XFProcOptionsInterpreter {} {
##########
# Procedure: XFProcOptionsInterpreter
# Description: interpreter options
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling interpreter options..."
  XFOptionsInterpreter
  XFEditSetStatus "Calling interpreter options...done"
}

proc XFProcOptionsMenuBar {} {
##########
# Procedure: XFProcOptionsMenuBar
# Description: menubar configuration
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling menubar configuration..."
  XFMenuBarConf .xfEdit.frame1
  XFEditSetStatus "Calling menubar configuration...done"
}

proc XFProcOptionsPathFile {} {
##########
# Procedure: XFProcOptionsPathFile
# Description: path options
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling path/file options..."
  XFOptionsPath
  XFEditSetStatus "Calling path/file options...done"
}

proc XFProcOptionsSource {} {
##########
# Procedure: XFProcOptionsSource
# Description: source options
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling source options..."
  XFOptionsSource
  XFEditSetStatus "Calling source options...done"
}

proc XFProcOptionsVersion {} {
##########
# Procedure: XFProcOptionsVersion
# Description: version options
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling version control options..."
  XFOptionsVersion
  XFEditSetStatus "Calling version control options...done"
}

proc XFProcOptionsWindow {} {
##########
# Procedure: XFProcOptionsWindow
# Description: window options
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling window options..."
  XFOptionsWindow
  XFEditSetStatus "Calling window options...done"
}

proc XFProcOptionsSaveModuleList {} {
##########
# Procedure: XFProcOptionsSaveModuleList
# Description: save module list
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Saving save module list..."
  XFSaveModuleList
  XFEditSetStatus "Saving save module list...done"
}

proc XFProcOptionsSaveOptions {} {
##########
# Procedure: XFProcOptionsSaveOptions
# Description: save options
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfBind
  global xfBindSaveLevel
  global xfBindShowLevel
  global xfComment
  global xfConf
  global xfFile
  global xfLoadPath
  global xfPath
  global xfProcSaveLevel
  global xfProcShowLevel
  global xfVersionNr
  global xfVersion

  if {"$xfFile(config)" != ""} {
    XFEditSetStatus "Saving options..."
    # save old options file
    catch "Cp $xfFile(config) $xfFile(config)~"
    if {[catch "open $xfFile(config) w" xfOutFile]} {
      XFProcError "$xfOutFile"
    } {
      puts $xfOutFile "# xf options file"
      puts $xfOutFile "set xfOptionVersionNr $xfVersionNr"
      puts $xfOutFile "# misc"
      puts $xfOutFile "set xfLoadPath \"$xfLoadPath\""
      puts $xfOutFile "# bindings"
      foreach xfCounter [lsort [array names xfBind]] {
        puts $xfOutFile "set xfBind($xfCounter) \{[set xfBind($xfCounter)]\}"
      }
      puts $xfOutFile "# configuration"
      foreach xfCounter [lsort [array names xfConf]] {
        if {"$xfCounter" == "programName" || "$xfCounter" == "programNameOld"} {
          puts $xfOutFile "set xfConf($xfCounter) \{main.tcl\}"
        } {
          puts $xfOutFile "set xfConf($xfCounter) \{[set xfConf($xfCounter)]\}"
        }
      }
      puts $xfOutFile "# file"
      foreach xfCounter [lsort [array names xfFile]] {
        puts $xfOutFile "set xfFile($xfCounter) \{[set xfFile($xfCounter)]\}"
      }
      puts $xfOutFile "# path"
      foreach xfCounter [lsort [array names xfPath]] {
        puts $xfOutFile "set xfPath($xfCounter) \{[set xfPath($xfCounter)]\}"
      }
      puts $xfOutFile "# version control"
      foreach xfCounter [lsort [array names xfVersion]] {
        puts $xfOutFile "set xfVersion($xfCounter) \{[set xfVersion($xfCounter)]\}"
      }
      puts $xfOutFile "# comments"
      foreach xfCounter [lsort [array names xfComment]] {
        puts $xfOutFile "set xfComment($xfCounter) \{[set xfComment($xfCounter)]\}"
      }
      puts $xfOutFile "# binding levels"
      foreach xfCounter [lsort [array names xfBindSaveLevel]] {
        puts $xfOutFile "set xfBindSaveLevel($xfCounter) \{[set xfBindSaveLevel($xfCounter)]\}"
      }
      foreach xfCounter [lsort [array names xfBindShowLevel]] {
        puts $xfOutFile "set xfBindShowLevel($xfCounter) \{[set xfBindShowLevel($xfCounter)]\}"
      }
      puts $xfOutFile "# procedure levels"
      foreach xfCounter [lsort [array names xfProcSaveLevel]] {
        puts $xfOutFile "set xfProcSaveLevel($xfCounter) \{[set xfProcSaveLevel($xfCounter)]\}"
      }
      foreach xfCounter [lsort [array names xfProcShowLevel]] {
        puts $xfOutFile "set xfProcShowLevel($xfCounter) \{[set xfProcShowLevel($xfCounter)]\}"
      }
      puts $xfOutFile "#"
      puts $xfOutFile "global tk_strictMotif"
      puts $xfOutFile "set tk_strictMotif [set xfConf(strictMotif)]"
      puts $xfOutFile "# eof"
      close $xfOutFile
    }
    XFEditSetStatus "Saving options...done"
  }
}

proc XFProcOptionsSavePositions {} {
##########
# Procedure: XFProcOptionsSavePositions
# Description: save window positions
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfFile
  global xfPos

  if {"$xfFile(positions)" != ""} {
    XFEditSetStatus "Saving window positions..."
    # save old options file
    catch "Cp $xfFile(positions) $xfFile(positions)~"
    if {[catch "open $xfFile(positions) w" xfOutFile]} {
      XFProcError "$xfOutFile"
    } {
      puts $xfOutFile "# xf window position file"
      foreach xfCounter [lsort [array names xfPos]] {
        puts $xfOutFile "set xfPos($xfCounter) \"[set xfPos($xfCounter)]\""
      }
      puts $xfOutFile "# eof"
      close $xfOutFile
    }
    XFEditSetStatus "Saving window positions...done"
  }
}

# eof

