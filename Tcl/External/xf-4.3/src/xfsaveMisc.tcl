# Program: xf
# Description: save misc stuff (almost everything :-)
#
# $Header: xfsaveMisc.tcl[2.5] Wed Mar 10 12:08:20 1993 garfield@garfield frozen $

##########
# Procedure: XFSaveBind
# Description: save bindings
# Arguments: xfOutFile - the output descriptor
#            xfW - the widget, or widget class
#            xfNoLeftOffset - don't indent output
# Returns: none
# Sideeffects: none
##########
proc XFSaveBind {xfOutFile xfW {xfNoLeftOffset 0}} {
  global xfConf

  # get list of bindings
  set xfTmpString ""
  if {$xfNoLeftOffset} {
    set xfLeftOffset ""
  } {
    set xfLeftOffset "  "
  }
  foreach xfCounter [lsort [bind $xfW]] {
    if {[XFMiscCorrectLevel bindsave [bind $xfW $xfCounter]]} {
      if {$xfConf(encloseBinding)} {
        append xfTmpString "\n${xfLeftOffset}bind $xfW $xfCounter \{[bind $xfW $xfCounter]\}"
      } {
        append xfTmpString "\n${xfLeftOffset}bind $xfW $xfCounter \"[bind $xfW $xfCounter]\""
      }
    }
  }
  if {"$xfTmpString" != ""} {
    puts $xfOutFile "${xfLeftOffset}# bindings$xfTmpString"
  }
}

##########
# Procedure: XFSaveComment
# Description: save comment
# Arguments: xfOutFile - the output descriptor
#            xfType - the comment type (file, module, proc)
#            xfName - the name (either filename, modulename, procname)
# Returns: none
# Sideeffects: none
##########
proc XFSaveComment {xfOutFile xfType xfName} {
  global env
  global tk_version
  global xfConf
  global xfComment
  global xfVersionNr

  set xfTmpComment ""
  case $xfType in {
    {file} {
      set xfTmpComment $xfComment(file)
      regsub -all {\$programName} $xfTmpComment $xfName xfTmpComment
    }
    {module} {
      set xfTmpComment $xfComment(module)
      regsub -all {\$moduleName} $xfTmpComment $xfName xfTmpComment
    }
    {template} {
      set xfTmpComment $xfComment(template)
      regsub -all {\$templateName} $xfTmpComment $xfName xfTmpComment
    }
    {proc} {
      set xfTmpComment $xfComment(proc)
      regsub -all {\$procedureName} $xfTmpComment $xfName xfTmpComment
    }
  }
  regsub -all {\$interpreter} $xfTmpComment $xfConf(interpreter) xfTmpComment
  if {[info exists env(USER)]} {
    regsub -all {\$user} $xfTmpComment $env(USER) xfTmpComment
  }
  if {[info exists env(LOGNAME)]} {
    regsub -all {\$logname} $xfTmpComment $env(LOGNAME) xfTmpComment
  }
  if {[info exists env(HOST)]} {
    regsub -all {\$host} $xfTmpComment $env(HOST) xfTmpComment
  }
  regsub -all {\$tk_version} $xfTmpComment $tk_version xfTmpComment
  regsub -all {\$tclVersion} $xfTmpComment [info tclversion] xfTmpComment
  regsub -all {\$xfVersion} $xfTmpComment $xfVersionNr xfTmpComment
  regsub -all {\$magicCookie} $xfTmpComment Tcl/Tk/XF xfTmpComment
  puts $xfOutFile "$xfTmpComment"
}

##########
# Procedure: XFSaveGlobals
# Description: save global variables
# Arguments: xfOutFile - the output descriptor
#            xfSavePlain - save the output as plain tcl file
# Returns: none
# Sideeffects: none
##########
proc XFSaveGlobals {xfOutFile {xfSavePlain 0}} {
  global xfAppDefToplevels

  if {!$xfSavePlain} {
    set leftOffset "  "
    puts $xfOutFile "\n# initialize global variables"
    puts $xfOutFile "proc InitGlobals {} {"
    # make interpreter happy }
  } {
    set leftOffset ""
    puts $xfOutFile "\n# initialize global variables"
  }
  foreach xfCounter [lsort [info globals]] {
    if {[XFMiscIsXFElement $xfCounter]} {
      continue
    }
    if {[string match "xf*" $xfCounter]} {
      continue
    }
    if {"[string trim $xfCounter]" != ""} {
      global $xfCounter
      puts $xfOutFile "${leftOffset}global \{$xfCounter\}"
# May 17, 1997  - Dennis LaBelle
#			Modified the following line to account for empty array variables.
#			Used "array exists" command instead of "array names".
      if {![array exists $xfCounter]} {
        puts $xfOutFile "${leftOffset}set \{$xfCounter\} \{[set $xfCounter]\}"
      } {
        foreach xfCounter2 [lsort [array names $xfCounter]] {
          set xfArrayName ""
          append xfArrayName $xfCounter ( $xfCounter2 )
          puts $xfOutFile "${leftOffset}set \{$xfArrayName\} \{[set $xfArrayName]\}"
        }
      }
    }
  }
  puts $xfOutFile "\n${leftOffset}# please don't modify the following"
  puts $xfOutFile "${leftOffset}# variables. They are needed by xf."
  foreach xfCounter "symbolicName xfWmSetPosition xfWmSetSize" {
    global $xfCounter
    puts $xfOutFile "${leftOffset}global \{$xfCounter\}"
# May 17, 1997  - Dennis LaBelle
#			Modified the following line to account for empty array variables.
#			Used "array exists" command instead of "array names".
    if {![array exists $xfCounter]} {
      puts $xfOutFile "${leftOffset}set \{$xfCounter\} \{[set $xfCounter]\}"
    } {
      foreach xfCounter2 [lsort [array names $xfCounter]] {
        set xfArrayName ""
        append xfArrayName $xfCounter ( $xfCounter2 )
        puts $xfOutFile "${leftOffset}set \{$xfArrayName\} \{[set $xfArrayName]\}"
      }
    }
  }
  set xfTmpValue ""
  foreach xfCounter $xfAppDefToplevels {
    if {"[info commands $xfCounter]" != "" ||
        "[info commands ShowWindow$xfCounter]" != ""} {
      append xfTmpValue "$xfCounter "
    }
  }
  puts $xfOutFile "${leftOffset}global \{xfAppDefToplevels\}"
  puts $xfOutFile "${leftOffset}set \{xfAppDefToplevels\} \{$xfTmpValue\}"
  if {!$xfSavePlain} {
    # make interpreter happy {
    puts $xfOutFile "}"
  }
}

##########
# Procedure: XFSaveImg
# Description: save image code
# Arguments: xfOutFile - the output descriptor
#            xfImgName - the image name        
# Returns: none
# Sideeffects: none
##########
proc XFSaveImg {xfOutFile xfImgName} {
  global xfMisc

  puts -nonewline $xfOutFile "\nimage create [image type $xfImgName] $xfImgName"
  foreach attrb [$xfImgName config] {
    if {[lindex $attrb 4] != ""} {
      puts -nonewline $xfOutFile " \\\n  [lindex $attrb 0] \{[lindex $attrb 4]\}"
    }
  }
  puts -nonewline $xfOutFile "\n"
}

##########
# Procedure: XFSavePack
# Description: save packing
# Arguments: xfOutFile - the output descriptor
#            xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFSavePack {xfOutFile xfW} {
  global xfConf
  global xfMisc

  # sugared version - makes pleasant output
  set xfSlaves [pack slaves $xfW]
  if {$xfSlaves != ""} {
    set xfDefault(-anchor) center
    set xfDefault(-expand) 0
    set xfDefault(-fill) none
    set xfDefault(-ipadx) 0
    set xfDefault(-ipady) 0
    set xfDefault(-padx) 0
    set xfDefault(-pady) 0
    set xfDefault(-side) top
    puts $xfOutFile "\n  # pack master $xfW"
    foreach xfSlave $xfSlaves {
      if {![string match xf* [winfo name $xfSlave]]} {
        set xfDefault(-in) [winfo parent $xfSlave]
        set xfConfig [pack info $xfSlave]
        puts -nonewline $xfOutFile "  pack configure $xfSlave"
        while {$xfConfig != ""} {
          set xfKey [lindex $xfConfig 0]
          set xfValue [lindex $xfConfig 1]
          set xfConfig [lrange $xfConfig 2 end]
          if {$xfValue != $xfDefault($xfKey)} {
            puts -nonewline $xfOutFile " \\\n    $xfKey $xfValue"
          }
        }
        puts $xfOutFile ""
      }
    }
  }
}

##########
# Procedure: XFSavePackOne
# Description: save packing for one child
# Arguments: xfOutFile - the output descriptor
#            xfW - the widget
#            xfSlave - the slave we want to pack
#            xfSaveAsProc - this is for a procedure
# Returns: none
# Sideeffects: none
##########
proc XFSavePackOne {xfOutFile xfW xfSlave {xfSaveAsProc 0}} {
  global xfConf

  if {![catch {pack info $xfSlave} xfInfo]} {
    set xfDefault(-anchor) center
    set xfDefault(-expand) 0
    set xfDefault(-fill) none
    set xfDefault(-ipadx) 0
    set xfDefault(-ipady) 0
    set xfDefault(-padx) 0
    set xfDefault(-pady) 0
    set xfDefault(-side) top
    puts $xfOutFile "\n  # pack slave $xfSlave"
    set xfDefault(-in) [winfo parent $xfSlave]
    set xfConfig [pack info $xfSlave]
    puts -nonewline $xfOutFile "  pack configure $xfSlave"
    while {$xfConfig != ""} {
      set xfKey [lindex $xfConfig 0]
      set xfValue [lindex $xfConfig 1]
      set xfConfig [lrange $xfConfig 2 end]
      if {$xfValue != $xfDefault($xfKey)} {
        puts -nonewline $xfOutFile " \\\n    $xfKey $xfValue"
      }
    }
    puts $xfOutFile ""
  }
}

##########
# Procedure: XFSavePlace
# Description: save placing
# Arguments: xfOutFile - the output descriptor
#            xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFSavePlace {xfOutFile xfW} {

  # get list of place elements
  set xfTmpString ""
  foreach xfCounter [place slaves $xfW] {
    if {![string match "xf*" [winfo name $xfCounter]]} {
      append xfTmpString "\n  # place\n"
      append xfTmpString "  place $xfCounter [place info $xfCounter]\n"
    }
  }
  if {"$xfTmpString" != ""} {
    puts $xfOutFile "$xfTmpString"
  }
}

##########
# Procedure: XFSavePlaceOne
# Description: save placing for one child
# Arguments: xfOutFile - the output descriptor
#            xfW - the widget
#            xfSlave - the slave we want to place
#            xfSaveAsProc - this is for a procedure
# Returns: none
# Sideeffects: none
##########
proc XFSavePlaceOne {xfOutFile xfW xfSlave {xfSaveAsProc 0}} {

  # get list of place elements
  set xfTmpString ""
  foreach xfCounter [place slaves $xfW] {
    if {"$xfCounter" == "$xfSlave"} {
      append xfTmpString "\n  # place\n"
      append xfTmpString "  place $xfCounter [place info $xfCounter]\n"
      break
    }
  }
  if {"$xfTmpString" != ""} {
    puts $xfOutFile "$xfTmpString"
  }
}

##########
# Procedure: XFSaveProc
# Description: save named procedure
# Arguments: xfOutFile - the output descriptor
#            xfProcName - the procedure name
# Returns: none
# Sideeffects: none
##########
proc XFSaveProc {xfOutFile xfProcName} {

  # get arg list for procedure
  set xfArgList [info args $xfProcName]
  set xfArguments ""
  foreach xfArg $xfArgList {
    if {[info default $xfProcName $xfArg xfDefault]} {
      append xfArguments " \{$xfArg \"$xfDefault\"\}"
    } {
      append xfArguments " $xfArg"
    }
  }
  set xfBodyList [string trimright [info body $xfProcName]]

  if {[XFMiscIsXFElement $xfProcName] ||
      ![XFMiscCorrectLevel procsave $xfBodyList]} {
    return
  }

  puts $xfOutFile "\n" 
  XFSaveComment $xfOutFile proc $xfProcName
  # write procedure
  if {[XFMiscIsXFSpecialElement $xfProcName]} {
    puts $xfOutFile "if {\"\[info procs $xfProcName\]\" == \"\"} {"
    puts $xfOutFile "proc $xfProcName {$xfArguments} {"
    if {[string index $xfBodyList 0] == "\n"} {
      puts $xfOutFile [string range $xfBodyList 1 end]
    } {
      puts $xfOutFile $xfBodyList
    }
    puts $xfOutFile "}"
    puts $xfOutFile "}"
  } {
    puts $xfOutFile "proc $xfProcName {$xfArguments} {"
    if {[string index $xfBodyList 0] == "\n"} {
      puts $xfOutFile [string range $xfBodyList 1 end]
    } {
      puts $xfOutFile $xfBodyList
    }
    puts $xfOutFile "}"
  }
}

##########
# Procedure: XFSaveRootToplevelChild
# Description: save the a child widget of .
# Arguments: xfOutFile - the output descriptor
#            xfName - the name of the window
# Returns: none
# Sideeffects: none
##########
proc XFSaveRootToplevelChild {xfOutFile xfName} {
  global xfAppDefToplevels
  global xfMisc

  set xfMisc(menuBarTraversalList) ""
  set xfMisc(specialSaveString) ""
  puts $xfOutFile "\n\n# procedure to show window $xfName"
  puts $xfOutFile "proc ShowWindow$xfName {args} {# xf ignore me 7"
  if {"[info procs StartupSrc$xfName]" != ""} {
    puts $xfOutFile "\nStartupSrc$xfName"
  }
  XFSaveWidget $xfOutFile $xfName
  XFSaveBind $xfOutFile $xfName
  XFSaveWidgetSpecial $xfOutFile $xfName
  XFSaveSubwindow $xfOutFile $xfName
  if {"[info procs MiddleSrc$xfName]" != ""} {
    puts $xfOutFile "\nMiddleSrc$xfName"
  }
  XFSavePack $xfOutFile $xfName
  XFSavePlace $xfOutFile $xfName
  if {[lsearch $xfAppDefToplevels $xfName] != -1} {
    puts $xfOutFile "  if {\"\[info procs XFLocalSetAppDefs\]\" != \"\"} {"
    puts $xfOutFile "    XFLocalSetAppDefs"
    puts $xfOutFile "  }"
  }
  if {"$xfMisc(specialSaveString)" != ""} {
    puts $xfOutFile "\n$xfMisc(specialSaveString)\n"
  }
  if {"[info procs EndSrc$xfName]" != ""} {
    puts $xfOutFile "\nEndSrc$xfName"
  }
  puts $xfOutFile "\n  if {\"\[info procs XFEdit\]\" != \"\"} {"
  puts $xfOutFile "    catch \"XFMiscBindWidgetTree $xfName\""
  puts $xfOutFile "    after 2 \"catch \{XFEditSetShowWindows\}\""
  puts $xfOutFile "  }"
  puts $xfOutFile "}"
}

##########
# Procedure: XFSaveShowWindow
# Description: save the show window procedure
# Arguments: xfOutFile - the output descriptor
#            xfName - the name of the window
# Returns: none
# Sideeffects: none
##########
proc XFSaveShowWindow {xfOutFile xfName} {

  set xfCounter xfShowWindow[string range $xfName 10 \
    [expr [string length $xfName]-1]]
  if {"[info globals $xfCounter]" != ""} {
    global $xfCounter
    if {[set $xfCounter] == 0} {
      set xfWinName [string range $xfCounter 13 end]
      # get arg list for procedure
      set xfArgList [info args $xfName]
      set xfArguments ""
      foreach xfArg $xfArgList {
        if {[info default $xfName $xfArg xfDefault]} {
          set xfArguments "$xfArguments \{$xfArg \"$xfDefault\"\}"
        } {
          set xfArguments "$xfArguments $xfArg"
        }
      }
      # get procedure body
      set xfBodyList [string trimright [info body $xfName]]
      puts $xfOutFile "\n\n# procedure to show window $xfName"
      puts $xfOutFile "proc $xfName {$xfArguments} {"
      if {[string index $xfBodyList 0] == "\n"} {
        puts $xfOutFile [string range $xfBodyList 1 end]
      } {
        puts $xfOutFile $xfBodyList
      }
      puts $xfOutFile "}"
      XFSaveSourceDestroyWindow $xfOutFile .$xfWinName
      XFSaveShowWindowTail $xfOutFile .$xfWinName
    }
  }
}

##########
# Procedure: XFSaveShowWindowTail
# Description: save the appendix of the show window procedure
# Arguments: xfOutFile - the output descriptor
#            xfWinName - the name of the window
# Returns: none
# Sideeffects: none
##########
proc XFSaveShowWindowTail {xfOutFile xfWinName} {

  if {"[info procs StartupSrc$xfWinName]" != ""} {
    set xfBodyList [string trimright [info body StartupSrc$xfWinName]]
    puts $xfOutFile "\nproc StartupSrc$xfWinName {args} {"
    if {[string index $xfBodyList 0] == "\n"} {
      puts $xfOutFile [string range $xfBodyList 1 end]
    } {
      puts $xfOutFile $xfBodyList
    }
    puts $xfOutFile "}"
  }
  if {"[info procs MiddleSrc$xfWinName]" != ""} {
    set xfBodyList [string trimright [info body MiddleSrc$xfWinName]]
    puts $xfOutFile "\nproc MiddleSrc$xfWinName {} {"
    if {[string index $xfBodyList 0] == "\n"} {
      puts $xfOutFile [string range $xfBodyList 1 end]
    } {
      puts $xfOutFile $xfBodyList
    }
    puts $xfOutFile "}"
  }
  if {"[info procs EndSrc$xfWinName]" != ""} {
    set xfBodyList [string trimright [info body EndSrc$xfWinName]]
    puts $xfOutFile "\nproc EndSrc$xfWinName {} {"
    if {[string index $xfBodyList 0] == "\n"} {
      puts $xfOutFile [string range $xfBodyList 1 end]
    } {
      puts $xfOutFile $xfBodyList
    }
    puts $xfOutFile "}"
  }
}

##########
# Procedure: XFSaveSubwindowBuild
# Description: save the subwindows
# Arguments: xfOutFile - the output descriptor
#            xfParent - the parent widget
# Returns: none
# Sideeffects: none
##########
proc XFSaveSubwindowBuild {xfOutFile xfParent} {

  foreach xfCounter [winfo children $xfParent] {
    if {![string match ".xf*" $xfCounter] &&
        ![string match "xf*" [winfo name $xfCounter]] &&
        !($xfParent == "." && [winfo class $xfCounter] == "Toplevel")} {
      XFSaveWidget $xfOutFile $xfCounter
      XFSaveBind $xfOutFile $xfCounter
      XFSaveWidgetSpecial $xfOutFile $xfCounter
      XFSaveSubwindowBuild $xfOutFile $xfCounter
    }
  }
}

##########
# Procedure: XFSaveSubwindowGeometry
# Description: save the subwindows geometry
# Arguments: xfOutFile - the output descriptor
#            xfParent - the parent widget
# Returns: none
# Sideeffects: none
##########
proc XFSaveSubwindowGeometry {xfOutFile xfParent} {

  foreach xfCounter [winfo children $xfParent] {
    if {![string match ".xf*" $xfCounter] &&
        ![string match "xf*" [winfo name $xfCounter]] &&
        !($xfParent == "." && [winfo class $xfCounter] == "Toplevel")} {
      XFSavePack $xfOutFile $xfCounter
      XFSavePlace $xfOutFile $xfCounter
      XFSaveSubwindowGeometry $xfOutFile $xfCounter
    }
  }
}

##########
# Procedure: XFSaveSubwindow
# Description: save the subwindows
# Arguments: xfOutFile - the output descriptor
#            xfParent - the parent widget
# Returns: none
# Sideeffects: none
##########
proc XFSaveSubwindow {xfOutFile xfParent} {
  global xfConf

  if {![string match ".xf*" $xfParent] &&
      ("$xfParent" == "." || ![string match "xf*" [winfo name $xfParent]])} {
    XFSaveSubwindowBuild $xfOutFile $xfParent
    XFSaveSubwindowGeometry $xfOutFile $xfParent
  }
}

##########
# Procedure: XFSaveWidget
# Description: save widget
# Arguments: xfOutFile - the output descriptor
#            xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFSaveWidget {xfOutFile xfW} {
  global xfConf
  global xfNoWidgetSave
  global xfPath
  global xfMisc

  if {"$xfW" == "."} {
    set xfClass Toplevel
  } {
    set xfClass [winfo class $xfW]
  }
  if {[lsearch $xfNoWidgetSave $xfClass] == -1} {
    if {"[info procs XFSaveWidget.$xfClass]" == ""} {
      if {[file exists "$xfPath(elements)/$xfClass"]} {
        source "$xfPath(elements)/$xfClass"
      } {
        foreach xfPathElement [split $xfPath(additionals) $xfMisc(separator)] {
          if {[XFMiscIsDir $xfPathElement]} {
            if {[file exists "$xfPathElement/$xfClass"]} {
              source "$xfPathElement/$xfClass"
              break
            }
          }
        }
      }
    }
  }
  if {"[info procs XFSaveWidget.$xfClass]" != ""} {
    XFSaveWidget.$xfClass $xfOutFile $xfW
  } {
    # what are we doing here ?
    puts $xfOutFile "\n  # build widget $xfW"

    set xfUseConfig 1
    # write widget type and path name

    case $xfClass in {
      {Button Canvas Checkbutton Entry Frame Label Listbox Menu Menubutton Message Radiobutton Scale Scrollbar Text Toplevel} {
        puts $xfOutFile "  [string tolower $xfClass] $xfW" nonewline
        set xfUseConfig 0
      }
      {TButton TCheckbutton TCombobox TEntry TFrame TLabel TLabelframe TMenubutton TNotebook TPanedwindow TProgressbar TRadiobutton TScrollbar TSeparator TSizegrip TScale TSpinbox} {
	  puts $xfOutFile "  ttk::[string tolower [string range $xfClass 1 end]] $xfW" nonewline
        set xfUseConfig 0 
      }
      {Treeview} {
	  puts $xfOutFile "  ttk::treeview $xfW" nonewline
        set xfUseConfig 0 
      }
      {default} {
        set xfType [string tolower [string index $xfClass 0]]
        append xfType [string range $xfClass 1 end]
        if {"[info commands $xfType]" == ""} {
          set xfType [string tolower $xfClass]
        }

        puts $xfOutFile "  if {\[catch \"$xfType $xfW\"\]} {"
        puts $xfOutFile "    if {\"\[info procs XFEdit\]\" != \"\"} {"
        puts $xfOutFile "      XFProcError \"Unknown widget type: $xfType\""
        puts $xfOutFile "      return"
        puts $xfOutFile "    } {"
        puts $xfOutFile "      puts stderr \"Unknown widget type: $xfType\""
# 12/05/98 - D. LaBelle - Commented following lines to allow better user feedback on error.
#        puts $xfOutFile "      catch \"destroy .\""
#        puts $xfOutFile "      catch \"exit 0\""
        puts $xfOutFile "    }"
        puts $xfOutFile "  }"
      }
    }

    # write options
    if {$xfUseConfig} {
      puts $xfOutFile "  $xfW config" nonewline
    }
    XFSaveWidgetResource $xfOutFile $xfW
    puts $xfOutFile ""
  }
}

##########
# Procedure: XFSaveWidgetResource
# Description: save widget resource values
# Arguments: xfOutFile - the output descriptor
#            xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFSaveWidgetResource {xfOutFile xfW} {
  global xfConf

  foreach xfCounter [$xfW configure] {
    # only handle options with 5 items per option entry
    if {[llength $xfCounter] == 5} {
      if {("[lindex $xfCounter 0]" == "-xscrollcommand" ||
           "[lindex $xfCounter 0]" == "-yscrollcommand" ||
           "[lindex $xfCounter 0]" == "-scrollcommand") &&
           "[lindex $xfCounter 4]" == "NoFunction"} {
        continue
      }
      if {("[winfo class $xfW]" == "Scale" ||
           "[winfo class $xfW]" == "Scrollbar") &&
           "[lindex $xfCounter 0]" == "-command" &&
           "[lindex $xfCounter 4]" == "NoFunction"} {
        continue
      }
      if {"[lindex $xfCounter 3]" != "[lindex $xfCounter 4]"} {
	# intentionally skip -cursor since it can cause problems on linux sometimes!
	if {[lindex $xfCounter 0] == {-cursor}} {
	    #puts "skipping [lindex $xfCounter 0] [lindex $xfCounter 4]"
	    continue
	}
        if {$xfConf(encloseConfigure)} {
          puts $xfOutFile " \\\n    [lindex $xfCounter 0] {[lindex $xfCounter 4]}" nonewline
        } {
          puts $xfOutFile " \\\n    [lindex $xfCounter 0] \"[lindex $xfCounter 4]\"" nonewline
        }
      }
    }
  }
}

##########
# Procedure: XFSaveWidgetSpecial
# Description: save widget specific code
# Arguments: xfOutFile - the output descriptor
#            xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFSaveWidgetSpecial {xfOutFile xfW} {
  global xfNoSpecialSave
  global xfMisc
  global xfPath

  if {"$xfW" == "."} {
    set xfClass Toplevel
  } {
    set xfClass [winfo class $xfW]
  }

  if {[lsearch $xfNoSpecialSave $xfClass] == -1} {
    if {"[info procs XFSaveSpecial.$xfClass]" == ""} {
      if {[file exists "$xfPath(elements)/$xfClass"]} {
        source "$xfPath(elements)/$xfClass"
      } {
        foreach xfPathElement [split $xfPath(additionals) $xfMisc(separator)] {
          if {[XFMiscIsDir $xfPathElement]} {
            if {[file exists "$xfPathElement/$xfClass"]} {
              source "$xfPathElement/$xfClass"
              break
            }
          }
        }
      }
    }
    if {"[info procs XFSaveSpecial.$xfClass]" != ""} {
      append xfMisc(specialSaveString) [XFSaveSpecial.$xfClass $xfW]
    }
  }
}

# eof

