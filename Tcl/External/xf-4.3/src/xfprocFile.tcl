# Program: xf
# Description: procedures that implement the file handling
#
# $Header: xfprocFile.tcl[2.4] Wed Mar 10 12:07:50 1993 garfield@garfield frozen $

proc XFProcFileInsert {} {
##########
# Procedure: XFProcFileInsert
# Description: insert additional source
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfMisc

  XFEditSetStatus "Inserting file..."
  XFProcFSBox "Select source file to insert:" {
    global auto_path
    global xfFSBox
    global xfPath
    global xfLoadPath
    if {"$xfFSBox(name)" != "" &&
        [file exists $xfFSBox(path)/$xfFSBox(name)]} {
      XFEditSetStatus "Inserting...in progress"
      if {[catch "source $xfFSBox(path)/$xfFSBox(name)" xfResult]} {
        set xfTmpAutoPath ""
        foreach xfCounter "$xfPath(src) [split $xfLoadPath $xfMisc(separator)]" {
          if {[file exists $xfCounter/tclIndex] &&
              [lsearch $auto_path $xfCounter] == -1 &&
              [lsearch $xfTmpAutoPath $xfCounter] == -1} {
            lappend xfTmpAutoPath $xfCounter
          }
        }
        set auto_path "$xfTmpAutoPath $auto_path"
        catch "unset auto_index"
        catch "unset auto_oldpath"
        catch "unset auto_execs"
        XFProcError "$xfResult"
      } {
        set xfTmpAutoPath ""
        foreach xfCounter "$xfPath(src) [split $xfLoadPath $xfMisc(separator)]" {
          if {[file exists $xfCounter/tclIndex] &&
              [lsearch $auto_path $xfCounter] == -1 &&
              [lsearch $xfTmpAutoPath $xfCounter] == -1} {
            lappend xfTmpAutoPath $xfCounter
          }
        }
        set auto_path "$xfTmpAutoPath $auto_path"
        catch "unset auto_index"
        catch "unset auto_oldpath"
        catch "unset auto_execs"
      }
      XFMiscBindWidgetTree .
      XFEditSetShowWindows
      XFEditSetPath .
      XFEditSetStatus "Inserting file...done"
    } {
      XFEditSetStatus "Inserting file...aborted"
    }
  } {
    XFEditSetStatus "Inserting file...aborted"
  } {}
}

proc XFProcFileLoad {} {
##########
# Procedure: XFProcFileLoad
# Description: load new file
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfMisc

  XFEditSetStatus "Loading file..."

  XFProcFSBox "Select file to load:" {
    global auto_path
    global xfConf
    global xfFSBox
    global xfPath
    global xfLoadPath
    global xfMisc

    if {"$xfFSBox(name)" != "" &&
        [file exists $xfFSBox(path)/$xfFSBox(name)]} {
      XFEditSetStatus "Loading file...in progress"
      XFMiscClearInterpreter
      wm title . $xfFSBox(name)
      if {[catch "source $xfFSBox(path)/$xfFSBox(name)" xfResult]} {
        set xfTmpAutoPath ""
        foreach xfCounter "$xfPath(src) [split $xfLoadPath $xfMisc(separator)]" {
          if {[file exists $xfCounter/tclIndex] &&
              [lsearch $auto_path $xfCounter] == -1 &&
              [lsearch $xfTmpAutoPath $xfCounter] == -1} {
            lappend xfTmpAutoPath $xfCounter
          }
        }
        set auto_path "$xfTmpAutoPath $auto_path"
        catch "unset auto_index"
        catch "unset auto_oldpath"
        catch "unset auto_execs"
        XFProcError "$xfResult"
      } {
        set xfTmpAutoPath ""
        foreach xfCounter "$xfPath(src) [split $xfLoadPath $xfMisc(separator)]" {
          if {[file exists $xfCounter/tclIndex] &&
              [lsearch $auto_path $xfCounter] == -1 &&
              [lsearch $xfTmpAutoPath $xfCounter] == -1} {
            lappend xfTmpAutoPath $xfCounter
          }
        }
        set auto_path "$xfTmpAutoPath $auto_path"
        catch "unset auto_index"
        catch "unset auto_oldpath"
        catch "unset auto_execs"
        set xfConf(programName) $xfFSBox(name)
        set xfConf(programPath) $xfFSBox(path)
        XFMiscUpdateModuleList
      }
      XFMiscBindWidgetTree .
      XFEditSetShowWindows
      XFEditSetPath .
      XFEditSetStatus "Loading file...done"
    } {
      XFEditSetStatus "Loading file...aborted"
    }
  } {
    XFEditSetStatus "Loading file...aborted"
  } {}
}

proc XFProcFileNew {} {
##########
# Procedure: XFProcFileNew
# Description: clear XF
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  if {[XFProcYesNo "Really remove all\nexisting definitions ?"]} {
    XFMiscClearInterpreter
  }
}

proc XFProcFileQuit {{xfRetVal 0}} {
##########
# Procedure: XFProcFileQuit
# Description: quit XF
# Arguments: {xfRetVal} - the return value
# Returns: none
# Sideeffects: none
##########

    if {[XFProcYesNo "Really Quit XF and the Application ?"]} {
    XFMiscQuit $xfRetVal
  }
}

proc XFProcFileEnterTCL {} {
##########
# Procedure: XFProcFileEnterTCL
# Description: call the tcl-code input box
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFReadBox
}

proc XFProcFileSave {} {
##########
# Procedure: XFProcFileSave
# Description: save the script under current name
# Arguments: none
# Returns: none
# Sideeffects: none
##########
  global xfConf
  global xfStatus

  XFEditSetStatus "Saving file..."
  if {"$xfConf(programName)" != "" &&
      [XFMiscIsDir $xfConf(programPath)]} {
    XFEditSetStatus "Saving...in progress"
    set xfStatus(saving) 0
    XFSaveModules $xfConf(programPath)/$xfConf(programName)
    if {$xfConf(writeTclIndex)} {
      XFSaveTclIndex
    }
    XFEditSetStatus "Saving file...done"
  } {
    XFEditSetStatus "Saving file...aborted"
  }
}

proc XFProcFileSaveAs {} {
##########
# Procedure: XFProcFileSaveAs
# Description: save the script under new name
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfConf
  global xfFSBox

  XFEditSetStatus "Saving file as..."
  auto_load XFFSBox
  set xfFSBox(path) $xfConf(programPath)
  XFProcFSBox "Select filename to save to:" {
    global moduleList
    global xfConf
    global xfStatus
    if {"$xfFSBox(name)" != "" &&
        [XFMiscIsDir $xfFSBox(path)]} {
      XFEditSetStatus "Saving file as...in progress"
      if {[lsearch [array names moduleList] $xfFSBox(name)] != -1} {
        XFEditSetStatus "Saving product...aborted"
        XFProcError "The given name is used as a development module !"
        return
      } {
        if {[file exists $xfFSBox(path)/$xfFSBox(name)] &&
            ![XFProcYesNo "Overwrite existing file ?"]} {
          XFEditSetStatus "Saving file as...aborted"
          return
        }
      }
      wm title . $xfFSBox(name)
      set xfConf(programName) $xfFSBox(name)
      XFMiscUpdateModuleList
      set xfStatus(saving) 0
      XFSave $xfFSBox(path)/$xfFSBox(name)
      if {$xfConf(writeShellScript)} {
        XFSaveScript $xfFSBox(name)
      }
      XFEditSetStatus "Saving file as...done"
    } {
      XFEditSetStatus "Saving file as...aborted"
    }
  } {
    XFEditSetStatus "Saving file as...aborted"
  } "$xfConf(programName)"
}

# eof

