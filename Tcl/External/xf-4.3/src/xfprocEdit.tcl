# Program: xf
# Description: procedures that implement the cut and paste
#
# $Header: xfprocEdit.tcl[1.2] Wed Mar 10 12:07:48 1993 garfield@garfield frozen $

proc XFProcEditClearCut {} {
##########
# Procedure: XFProcEditClearCut
# Description: clear the current cutbuffer
# Arguments: none
# Returns: none
# Sideeffects: none
##########
  global xfPath
  global xfStatus

  set xfStatus(cutBuffer) 0
  if {[file exists $xfPath(tmp)/cb$xfStatus(uniqueId)]} {
    catch "Rm $xfPath(tmp)/cb$xfStatus(uniqueId)"
  }
  XFEditSetCutInfo
  XFPasteUpdate
}

proc XFProcEditCopy {{xfW ""}} {
##########
# Procedure: XFProcEditCopy
# Description: copy specified path to cutbuffer
# Arguments: {xfW} - the widget to copy
# Returns: none
# Sideeffects: none
##########
  global xfPath
  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  set xfStatus(cutBuffer) 1
  XFEditSetStatus "Copy widgets..."
  XFSaveSubTree $xfW $xfPath(tmp)/cb$xfStatus(uniqueId)
  XFEditSetCutInfo
  XFPasteUpdate
  XFEditSetStatus "Copy widgets...done"
}

proc XFProcEditCut {{xfW ""}} {
##########
# Procedure: XFProcEditCut
# Description: cut specified path to cutbuffer
# Arguments: {xfW} - the widget to cut
# Returns: none
# Sideeffects: none
##########
  global xfPath
  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  set xfStatus(cutBuffer) 1
  XFEditSetStatus "Cut widgets..."
  XFSaveSubTree $xfW $xfPath(tmp)/cb$xfStatus(uniqueId)
  XFEditSetCutInfo
  if {"$xfW" != "."} {
    catch "XFDestroy $xfW"
    set xfStatus(path) [string range $xfW 0 [expr [string last "." $xfW]-1]]
    if {"$xfStatus(path)" == ""} {
      set xfStatus(path) "."
    }
    XFPasteUpdate
  }
  XFEditSetShowWindows
  XFEditSetPath $xfStatus(path)
  XFEditSetStatus "Cut widgets...done"
}

proc XFProcEditDelete {{xfW ""}} {
##########
# Procedure: XFProcEditDelete
# Description: delete specified path
# Arguments: {xfW} - the widget to delete
# Returns: none
# Sideeffects: none
##########
  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  XFEditSetStatus "Delete widgets..."
  if {"$xfW" == "."} {
    if {[XFProcYesNo "Really delete all\nchildren of . ?"]} {
      XFMiscClearInterpreter
    }
  } {
    if {"[winfo class $xfW]" == "Toplevel" ||
        "[winfo class $xfW]" == "Frame"} {
      if {[XFProcYesNo "Really delete this\n complex widget structure ?"]} {
        global xfStatus
        catch "XFDestroy $xfW"
        set xfStatus(path) \
          [string range $xfW 0 [expr [string last . $xfW]-1]]
        if {"$xfStatus(path)" == ""} {
          set xfStatus(path) .
        }
        XFEditSetShowWindows
        XFEditSetPath $xfStatus(path)
      }
    } {
      catch "XFDestroy $xfW"
      set xfStatus(path) \
        [string range $xfW 0 [expr [string last "." $xfW]-1]]
      if {"$xfStatus(path)" == ""} {
        set xfStatus(path) "."
      }
      XFEditSetShowWindows
      XFEditSetPath $xfStatus(path)
    }
  }
  XFEditSetStatus "Delete widgets...done"
}

proc XFProcEditLoadCut {} {
##########
# Procedure: XFProcEditLoadCut
# Description: load cutbuffer from file
# Arguments: none
# Returns: none
# Sideeffects: none
##########
  global xfStatus

  XFEditSetStatus "Loading cut buffer..."
  set xfStatus(cutBuffer) 1
  XFProcFSBox "Select file to load in cut buffer:" {
    global xfPath
    global xfStatus
    if {"$xfFSBox(name)" != "" &&
        [file exists $xfFSBox(path)/$xfFSBox(name)] &&
        [XFMiscIsDir $xfPath(tmp)]} {
      XFEditSetStatus "Loading cut buffer...in progress"
      catch "Cp $xfFSBox(path)/$xfFSBox(name) $xfPath(tmp)/cb$xfStatus(uniqueId)"
      XFEditSetCutInfo
      XFPasteUpdate
    }
    XFEditSetStatus "Loading cut buffer...done"
  } {
    XFEditSetStatus "Loading cut buffer...aborted"
  } {}
}

proc XFProcEditLoadTemplate {} {
##########
# Procedure: XFProcEditLoadTemplate
# Description: load template from file
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfPath
  global xfStatus
  global xfFSBox
  global xfMisc

  XFEditSetStatus "Loading template..."
  auto_load XFFSBox
  foreach xfPathElement [split $xfPath(templates) $xfMisc(separator)] {
    if {[file exists $xfPathElement/$xfStatus(tmpltPath)]} {
      set xfFSBox(path) $xfPathElement
      break
    }
  }
  
  XFProcFSBox "Select file to load as template:" {
    global xfPath
    global xfStatus
    if {"$xfFSBox(name)" != "" &&
        [file exists $xfFSBox(path)/$xfFSBox(name)]} {
      XFEditSetStatus "Loading template...in progress"
      XFPasteFile $xfStatus(path) $xfFSBox(path)/$xfFSBox(name)
      XFEditSetShowWindows
      XFEditSetPath $xfStatus(path)
    }
    XFEditSetStatus "Loading template...done"
  } {
    XFEditSetStatus "Loading template...aborted"
  } {}
}

proc XFProcEditMakeAProc {{xfW ""}} {
##########
# Procedure: XFProcEditMakeAProc
# Description: make specified path a procedure
# Arguments: {xfW} - the widget to copy to a procedure
# Returns: none
# Sideeffects: none
##########
  global xfPath
  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  XFEditSetStatus "Make $xfW a procedure named V[winfo class $xfW]$xfW..."
  XFSaveSubTree $xfW $xfPath(tmp)/st$xfStatus(uniqueId) 1
  XFPasteFile $xfW $xfPath(tmp)/st$xfStatus(uniqueId) 1
  XFEditSetStatus "Make $xfW a procedure named V[winfo class $xfW]$xfW...done"
}

proc XFProcEditPaste {{xfW ""}} {
##########
# Procedure: XFProcEditPaste
# Description: paste cutbuffer to current path
# Arguments: {xfW} - the widget to paste to
# Returns: none
# Sideeffects: none
##########
  global xfPath
  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  XFEditSetStatus "Pasting..."
  XFPasteFile $xfW $xfPath(tmp)/cb$xfStatus(uniqueId)
  XFEditSetShowWindows
  XFEditSetPath $xfStatus(path)
  XFEditSetStatus "Pasting...done"
}

proc XFProcEditSaveCut {} {
##########
# Procedure: XFProcEditSaveCut
# Description: save cutbuffer to file
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Saving cut buffer..."
  XFProcFSBox "Select file name to save cut buffer to:" {
    global xfPath
    global xfStatus
    if {"$xfFSBox(name)" != "" &&
        [file exists $xfPath(tmp)/cb$xfStatus(uniqueId)] &&
        [XFMiscIsDir $xfFSBox(path)]} {
      XFEditSetStatus "Saving cut buffer...in progress"
      catch "Cp $xfPath(tmp)/cb$xfStatus(uniqueId) $xfFSBox(path)/$xfFSBox(name)"
    }
    XFEditSetStatus "Saving cut buffer...done"
  } {
    XFEditSetStatus "Saving cut buffer...aborted"
  } {}
}

proc XFProcEditSaveCutAsTemplate {xfType} {
##########
# Procedure: XFProcEditSaveCutAsTemplate
# Description: save cutbuffer to template file
# Arguments: xfType - the file type to copy (cb for cutbuffer)
# Returns: none
# Sideeffects: none
##########

  global env
  global xfPath
  global xfStatus
  global xfFSBox
  global xfMisc

  XFEditSetStatus "Saving cut buffer as template..."
  auto_load XFFSBox
  set xfTmpPath $env(HOME)
  foreach xfPathElement [split $xfPath(templates) $xfMisc(separator)] {
    if {[file exists $xfPathElement/$xfStatus(tmpltPath)] &&
        [file writable $xfPathElement/$xfStatus(tmpltPath)]} {
      set xfTmpPath $xfPathElement/$xfStatus(tmpltPath)
    }
  }
  set xfFSBox(path) $xfTmpPath
  set xfFSBox(pattern) *.t
  XFProcFSBox "Select file name to save template (cut buffer) to:" "
    global xfPath
    global xfStatus
    if {\"\$xfFSBox(name)\]\" != \"\" &&
        \[file exists \$xfPath(tmp)/$xfType\$xfStatus(uniqueId)\] &&
        \[XFMiscIsDir \$xfFSBox(path)\]} {
      XFEditSetStatus \"Saving cut buffer as template...in progress\"
      catch \"Cp \$xfPath(tmp)/$xfType\$xfStatus(uniqueId) \$xfFSBox(path)/\[file rootname \$xfFSBox(name)\].t\"
    }
    XFEditReadTemplates \$xfStatus(tmpltPath)
    XFEditSetStatus \"Saving cut buffer as template...done\"
  " {
    XFEditSetStatus "Saving cut buffer as template...aborted"
  } {}
}

proc XFProcEditShowCut {xfType} {
##########
# Procedure: XFProcEditShowCut
# Description: show the current cutbuffer
# Arguments: xfType - what to show (tree or script)
# Returns: none
# Sideeffects: none
##########
  global xfPath
  global xfStatus
  global xfTextBox

  if {[file exists $xfPath(tmp)/cb$xfStatus(uniqueId)]} {
    if {"$xfType" == "script"} {
      set xfStatus(pasteScriptDisplayed) 1
      set xfTextBox(toplevelName) .xfCutbuffer
      XFProcTextFile $xfPath(tmp)/cb$xfStatus(uniqueId) 400x300 \
	{XF current cut buffer} \
          {global xfStatus; set xfStatus(pasteScriptDisplayed) 0}
      set xfTextBox(toplevelName) .xfTextBox
    } {
      set xfStatus(pasteTreeDisplayed) 1
      XFPasteShowTree
    }
  }
}

# eof

