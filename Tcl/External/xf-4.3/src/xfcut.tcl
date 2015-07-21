# Program: xf
# Description: paste functions
#
# $Header: xfcut.tcl[2.3] Wed Mar 10 12:05:36 1993 garfield@garfield frozen $

##########
# Procedure: XFPasteFile
# Description: paste file to current path
# Arguments: xfW - the widget to paste to
#            xfFileName - the filename to paste from
#            {xfSaveAsProc} - paste with variable as pathname
# Returns: none
# Sideeffects: none
##########
proc XFPasteFile {xfW xfFileName {xfSaveAsProc 0}} {
  global xfPath xfStatus

  # get the cut widget name
  if {![catch "open $xfFileName r" xfInFile]} {
    set xfFileContents [read $xfInFile]
    close $xfInFile
    
    if {![regexp "^#\[ \t\]+(\[^ \t\n\]+)\[ \t\]*\n" $xfFileContents xfDummy xfCutName]} {
      XFProcError "Pastebuffer not in correct format"
      return
    }
    
    if {$xfCutName == "XFNoParsing"} {
      if {[catch "source $xfFileName" xfResult]} {
	XFProcError $xfResult
      }
      return
    }
    
    # if xfCutName is .ist.eine.kleine ... then xfPasteElement is kleine
    
    set xfPasteElement [string range $xfCutName [expr [string last . $xfCutName]+1] end]
    set xfNewPasteElement $xfPasteElement
    
    # maybe could use this somehow:
    # regexp {^(.*[^0-9])?([0-9]*)$} $x d class number

    set xfPasteName paste
    set xfPasteNumber 0
    regexp {[A-Za-z0-9_-]+} $xfPasteElement xfPasteName
    regexp {[A-Za-z_-]+} $xfPasteElement xfPasteClass
    regexp {[0-9]+} $xfPasteElement xfPasteNumber
    
    set xfNewPasteElement [XFMiscGetUniqueName $xfPasteName $xfPasteClass $xfW]

    # hopefuly this sequence does not appear in the file
    set xfRoot {never1show2up}

    if {$xfW == "."} {
      set xfPatt $xfRoot\\.?
      set xfRebind .$xfNewPasteElement
      set xfNewPath .
    } {
      set xfPatt $xfRoot
      set xfRebind $xfW.$xfNewPasteElement
      if {$xfSaveAsProc} {
        set xfNewPath \$insertWidgetPath
      } {
        set xfNewPath $xfW
      }
    }

    if {$xfPasteNumber > $xfStatus(elementCounter)} {
      incr xfStatus(elementCounter)
    }

    set xfOldPath [string range $xfCutName 0 [expr [string last . $xfCutName]-1]]
    set xfPEQ [XFMiscExpandRegexp $xfPasteElement]
    if {$xfOldPath == ""} {
      regsub -all (\[\[\{\ \t\n\;\])\\.($xfPEQ) $xfFileContents \\1$xfRoot.\\2 xfFileContents
      # this next one really ought to be fussier
      regsub -all (\[\[\{\ \t\n\;\])\\. $xfFileContents \\1$xfRoot xfFileContents
    } {
      set xfOPQ [XFMiscExpandRegexp $xfOldPath]
      regsub -all (\[\[\{\ \t\n\;\])$xfOPQ $xfFileContents \\1$xfRoot xfFileContents
    }

    # now $xfRoot marks all places the initial path occurred
    
    regsub -all $xfRoot\\.$xfPEQ $xfFileContents $xfRoot.$xfNewPasteElement xfFileContents

    # now the new name is set

    regsub -all $xfPatt $xfFileContents $xfNewPath xfFileContents

    # now the new path is inserted - everything is ready!

    if {[catch $xfFileContents xfResult]} {
      XFProcError $xfResult
    }
    XFMiscBindWidgetTree $xfRebind
    XFEditSetPath $xfStatus(path)
  }
}

##########
# Procedure: XFPasteUpdate
# Description: update the displayed pastebuffer
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFPasteUpdate {} {
  global xfPath
  global xfStatus

  if {![file exists $xfPath(tmp)/cb$xfStatus(uniqueId)]} {
    if {$xfStatus(pasteScriptDisplayed)} {
      set xfStatus(pasteScriptDisplayed) 0
      destroy .xfCutbuffer
    }
    if {$xfStatus(pasteTreeDisplayed)} {
      set xfStatus(pasteTreeDisplayed) 0
      destroy .xfCutPaste
    }
  } {
    if {$xfStatus(pasteScriptDisplayed)} {
      XFMiscSetText .xfCutbuffer.frame0.text1 \
        [Cat $xfPath(tmp)/cb$xfStatus(uniqueId)]
    }
    if {$xfStatus(pasteTreeDisplayed)} {
      foreach xfCounter [winfo children .xfCutPaste.frame1] {
        destroy $xfCounter
      }
      XFPasteFile .xfCutPaste.frame1 $xfPath(tmp)/cb$xfStatus(uniqueId)
    }
  }
}

##########
# Procedure: XFPasteShowTree
# Description: show the current cutbuffer as tree
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFPasteShowTree {} {
  global xfPath
  global xfStatus

  # build widget structure
  XFTmpltToplevel .xfCutPaste 200x200 {XF current cut buffer}
  
  XFTmpltFrame .xfCutPaste.frame1

  XFTmpltFrame .xfCutPaste.frame2 0

  button .xfCutPaste.frame2.ok \
    -text {OK} \
    -command {
      global xfStatus
      set xfStatus(pasteTreeDisplayed) 0
      destroy .xfCutPaste}

  # paste tree
  XFPasteFile .xfCutPaste.frame1 $xfPath(tmp)/cb$xfStatus(uniqueId)

  # packing
  pack append .xfCutPaste.frame2 \
              .xfCutPaste.frame2.ok {left fill expand}
  pack append .xfCutPaste \
              .xfCutPaste.frame2 {bottom fill} \
              .xfCutPaste.frame1 {top fill expand}
}

# eof

