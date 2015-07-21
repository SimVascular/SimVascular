# Program: xf
# Description: misc routines
#
# $Header: xfmisc.tcl[2.7] Wed Mar 10 12:07:02 1993 garfield@garfield frozen $

##########
# Procedure: XFIncludeModule
# Description: Module load procedure
# Arguments: moduleName - the module to load
# Returns: none
# Sideeffects: Tcl code is read
##########
proc XFIncludeModule {{moduleName ""}} {
  global env
  global xfLoadInfo
  global xfLoadPath
  global xfStatus
  global xfMisc

  foreach p [split $xfLoadPath $xfMisc(separator)] {
    if {[file exists "$p/$moduleName"]} {
      if {![file readable "$p/$moduleName"]} {
        puts stderr "Cannot read $p/$moduleName (permission denied)"
        continue
      }
      if {$xfLoadInfo} {
        puts stdout "Loading $p/$moduleName..."
      }
      source "$p/$moduleName"
      return 1
    }
    # first see if we have a load command
    if {[info exists env(XF_VERSION_SHOW)]} {
      set xfCommand $env(XF_VERSION_SHOW)
      regsub -all {\$xfFileName} $xfCommand $p/$moduleName xfCommand
      if {$xfLoadInfo} {
        puts stdout "Loading $p/$moduleName...($xfCommand)"
      }
      if {[catch "exec $xfCommand" contents]} {
        continue
      } {
        eval $contents
        return 1
      }
    }
  }
  puts stderr "Cannot load module $moduleName -- check your xf load path"
  puts stderr "Specify a xf load path with the environment variable:"
  puts stderr "  XF_LOAD_PATH (e.g \"export XF_LOAD_PATH=.\")"
}

##########
# Procedure: XFMiscAutoSave
# Description: Auto save
# Arguments: xfPassedInterval - the interval when this after
#                               call was activated
# Returns: none
# Sideeffects: none
##########
proc XFMiscAutoSave {xfPassedInterval} {
  global xfConf
  global xfPath
  global xfStatus

  # save interval was changed
  if {$xfConf(saveInterval) != $xfPassedInterval} {
    return
  }
  if {$xfConf(saveInterval) > 0} {
    if {$xfConf(maxSaveId) < 1} {
      XFEditSetStatus "Saving recommended..."
      return
    }
    if {$xfStatus(saveId) >= $xfConf(maxSaveId)} {
      set xfStatus(saveId) 0
    }

    update idletask
    toplevel .xfAutoSaving
    wm title .xfAutoSaving "Auto saving XF..."
    label .xfAutoSaving.mess1 \
      -anchor c \
      -background $xfConf(flash) \
      -font $xfConf(fontMessage) \
      -text "Auto saving to: $xfPath(tmp)/as$xfStatus(uniqueId)$xfStatus(saveId)..."
    pack append .xfAutoSaving .xfAutoSaving.mess1 {top fill expand}
    update idletask

    XFEditSetStatus "Auto saving to: $xfPath(tmp)/as$xfStatus(uniqueId)$xfStatus(saveId)..."
    catch "XFSave $xfPath(tmp)/as$xfStatus(uniqueId)$xfStatus(saveId)"
    XFEditSetStatus "Auto saving to: $xfPath(tmp)/as$xfStatus(uniqueId)$xfStatus(saveId)...done"
    destroy .xfAutoSaving

    incr xfStatus(saveId) 1
    after [expr $xfConf(saveInterval)*60000] XFMiscAutoSave $xfConf(saveInterval)
  }
}

##########
# Procedure: XFMiscBind
# Description: bind global acceletators to widget
# Arguments: xfW - the widget
#            args - the help page for this widget
# Returns: none
# Sideeffects: none
##########
proc XFMiscBind {xfW args} {
  global xfBind

  if {("[info commands $xfW]" == "" && "$xfW" != ".")  ||
      [string match ".xf*" $xfW] ||
      ([string match "xf*" [winfo name $xfW]] && "$xfW" != ".")} {
    return
  }

  # keep only root windows bindings to avoid multiple triggers
  if {$xfW != "." && [winfo class $xfW] != "Toplevel"} {
    return
  }
  # 10/21/98 - D. LaBelle - added check for existence of current protocol command
  set curcmd [wm protocol $xfW WM_DELETE_WINDOW]
  if {$curcmd == ""} {
     wm protocol $xfW WM_DELETE_WINDOW "XFProcError \{Application windows can not be destroyed.
Please use the \"Current widget path:\" to show/hide windows.\}"
     }

  # settings for the workspace
  if {[catch "bind $xfW $xfBind(select) \"# xf ignore me 9
XFEditSetPath %W\"" xfResult]} {
    puts stderr "$xfResult"
  }
  if {[catch "bind $xfW $xfBind(showName) \"# xf ignore me 9
XFBindShowName %W %x %y\"" xfResult]} {
    puts stderr "$xfResult"
  }
  if {[catch "bind $xfW $xfBind(removeName) \"# xf ignore me 9
XFBindRemoveName\"" xfResult]} {
    puts stderr "$xfResult"
  }
  if {[catch "bind $xfW $xfBind(configure) \"# xf ignore me 9
XFProcConfParametersDefault %W\"" xfResult]} {
    puts stderr "$xfResult"
  }
  if {[catch "bind $xfW $xfBind(placing) \"# xf ignore me 9
XFLayoutPosPress %W %X %Y %x %y\"" xfResult]} {
    puts stderr "$xfResult"
  }
  if {[catch "bind $xfW $xfBind(placingMotion) \"# xf ignore me 9
XFLayoutPosMove %W %X %Y %x %y\"" xfResult]} {
    puts stderr "$xfResult"
  }
  if {[catch "bind $xfW $xfBind(placingRelease) \"# xf ignore me 9
XFLayoutPosRelease %W %X %Y %x %y\"" xfResult]} {
    puts stderr "$xfResult"
  }
}

##########
# Procedure: XFMiscBindWidgetTree
# Description: set the bindings for all widgets in the specified
#              tree to the default values
# Arguments: xfW - the current widget
#            args - the help page for this widget
# Returns: none
# Sideeffects: none
##########
proc XFMiscBindWidgetTree {xfW args} {
  global xfNoSpecialBind
  global xfPath
  global xfMisc

  if {"[info commands $xfW]" == "" && "$xfW" != "."} {
    return
  }
  XFMiscBind $xfW $args
  set xfClass [winfo class $xfW]
  if {[lsearch $xfNoSpecialBind $xfClass] != -1} {
    if {"[info procs XFAdd.$xfClass]" == ""} {
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
  if {"[info proc XFBind.$xfClass]" != ""} {
    XFBind.$xfClass $xfW
  }
  foreach xfCounter [winfo children $xfW] {
    XFMiscBindWidgetTree $xfCounter $args
  }
}

##########
# Procedure: XFMiscCallExternalEditor
# Description: call external editor in background
# Arguments: xfType - the type of procedure editing
#            {xfProcName} - the procedure we edit
# Returns: none
# Sideeffects: none
##########
proc XFMiscCallExternalEditor {xfType {xfProcName ""}} {
  global xfPath
  global xfStatus

  if {"$xfType" == "StartupSrc"} {
    set xfEditFile $xfPath(tmp)/start$xfStatus(editors)$xfStatus(uniqueId).tcl
    set xfShortName starts$xfStatus(editors).tcl
  } {
    if {"$xfType" == "EndSrc"} {
      set xfEditFile $xfPath(tmp)/end$xfStatus(editors)$xfStatus(uniqueId).tcl
      set xfShortName ends$xfStatus(editors).tcl
    } {
      if {"$xfType" == "cmds"} {
        set xfEditFile $xfPath(tmp)/cmds$xfStatus(editors)$xfStatus(uniqueId).tcl
        set xfShortName cmdss$xfStatus(editors).tcl
      } {
        set xfEditFile $xfPath(tmp)/$xfType$xfStatus(editors)$xfStatus(uniqueId).tcl
        set xfShortName ${xfType}s$xfStatus(editors).tcl
      }
    }
  }

  incr xfStatus(editors) 1
  if {[catch "open $xfEditFile w" xfOutFile]} {
    XFProcError "$xfOutFile"
  } {
    set xfArguments ""
    set xfBodyList ""
    if {"$xfProcName" != "" &&
        "[info proc $xfProcName]" != ""} {
      set xfBodyList [string trimright [info body $xfProcName]]
      set xfArgList [info args $xfProcName]
      foreach xfCounter $xfArgList {
        if {[info default $xfProcName $xfCounter xfDefault]} {
          set xfArguments "$xfArguments \{$xfCounter \"$xfDefault\"\}"
        } {
          set xfArguments "$xfArguments $xfCounter"
        }
      }
    }
    puts $xfOutFile "proc $xfProcName \{$xfArguments\} \{"
    if {[string index $xfBodyList 0] == "\n"} {
      puts $xfOutFile "[string range $xfBodyList 1 end]"
    } {
      puts $xfOutFile "$xfBodyList"
    }
    puts $xfOutFile "\}"
    close $xfOutFile

   # call external editor
   XFMiscCallExternalEditor1 $xfEditFile $xfShortName $xfType $xfProcName
  }
}

##########
# Procedure: XFMiscCallExternalEditor1
# Description: call external editor in background
# Arguments: xfFileName - the filename to edit
#            xfShortName - the short file name
#            xfType - the type of edit procedure that called this proc
#            {xfProcName} - the procedure we edit
# Returns: none
# Sideeffects: none
##########
proc XFMiscCallExternalEditor1 {xfFileName xfShortName xfType {xfProcName ""}} {
  global xfEditing

  set xfAskUser 0
  if {"$xfProcName" != ""} {
    foreach xfCounter [array names xfEditing] {
      if {"$xfEditing($xfCounter)" == "$xfProcName"} {
        set xfAskUser 1
        if {[XFProcYesNo "You are already editing the procedure:\n$xfProcName\nStill call the editor ?"]} {
          XFMiscCallExternalEditor2 $xfFileName $xfShortName \
            $xfType $xfProcName
          return
        }
      }
    }
    set xfEditing($xfProcName) $xfProcName
  }
  if {!$xfAskUser} {
    XFMiscCallExternalEditor2 $xfFileName $xfShortName $xfType $xfProcName
  }
}

##########
# Procedure: XFMiscCallExternalEditor2
# Description: actually call external editor in background
# Arguments: xfFileName - the filename to edit
#            xfShortName - the short file name
#            xfType - the type of edit procedure that called this proc
#            {xfProcName} - the procedure we edit
# Returns: none
# Sideeffects: none
##########
proc XFMiscCallExternalEditor2 {xfFileName xfShortName xfType {xfProcName ""}} {
  global xfConf
  global xfPath
  global xfStatus

  set xfTmpEditorCmd $xfConf(externalEditor)
  regsub -all {\$xfFileName} $xfConf(externalEditor) $xfFileName xfTmpEditorCmd
  if {[catch "open $xfPath(tmp)/$xfShortName$xfStatus(uniqueId) w" xfOutFile]} {
    XFProcError "$xfOutFile"
  } {
    puts $xfOutFile "wm withdraw ."
    puts $xfOutFile "while {1} {"
    if {"$xfConf(externalEditor)" == "point"} {
      puts $xfOutFile "  if {\[catch \"send point OpenWindow $xfFileName\"\]} {"
      puts $xfOutFile "    catch \"exec $xfTmpEditorCmd\""
      puts $xfOutFile "  }"
    } {
      puts $xfOutFile "  catch \"exec $xfTmpEditorCmd\""
    }
    puts $xfOutFile "  if {\[catch \"send \\\"[winfo name .]\\\" winfo name .\" xfResult\]} {"
    puts $xfOutFile "    puts stderr \"XF was aborted during edit session.\""
    puts $xfOutFile "    puts stderr \"I got: \$xfResult\""
    puts $xfOutFile "    catch \"conole show\""
    puts $xfOutFile "  }"
    puts $xfOutFile "  if {\[file exists $xfFileName\] &&"
    puts $xfOutFile "      \[file size $xfFileName\] > 1} {"
    if {"$xfType" == "cmplt"} {
      puts $xfOutFile "    catch \"send \\\"[winfo name .]\\\" XFMiscClearInterpreter\""
    }
    puts $xfOutFile "    if {\[catch \"send \\\"[winfo name .]\\\" source $xfFileName\" xfResult\]} {"
    puts $xfOutFile "      puts stderr \"Error when loading $xfFileName\\nI got:\$xfResult\""
    puts $xfOutFile "      continue"
    puts $xfOutFile "    }"
    if {"$xfType" == "procs"} {
      puts $xfOutFile "    catch \"send \\\"[winfo name .]\\\" XFMiscSetInfo procs .xfInfoProc.procs.procs 0\""
    }
    if {"$xfType" == "cmds"} {
      puts $xfOutFile "    catch \"send \\\"[winfo name .]\\\" XFMiscSetInfo commands .xfInfoCmd.commands.commands 0\""
    }
    if {"$xfType" == "cmplt"} {
      puts $xfOutFile "    catch \"send \\\"[winfo name .]\\\" XFMiscBindWidgetTree .\""
      puts $xfOutFile "    catch \"send \\\"[winfo name .]\\\" XFEditSetShowWindows\""
      puts $xfOutFile "    catch \"send \\\"[winfo name .]\\\" XFEditSetPath .\""
    }
    puts $xfOutFile "  }"
    puts $xfOutFile "  catch \"exec rm $xfFileName\""
    puts $xfOutFile "  catch \"exec rm $xfFileName~\""
    puts $xfOutFile "  catch \"exec rm [file dirname $xfFileName]/#[file tail $xfFileName]\""
    puts $xfOutFile "  catch \"exec rm [file dirname $xfFileName]/#[file tail $xfFileName]~\""
    puts $xfOutFile "  catch \"exec rm $xfPath(tmp)/$xfShortName$xfStatus(uniqueId)\""
    puts $xfOutFile "  catch \"exec rm $xfPath(tmp)/$xfShortName$xfStatus(uniqueId)~\""
    puts $xfOutFile "  catch \"exec rm $xfPath(tmp)/#$xfShortName$xfStatus(uniqueId)\""
    puts $xfOutFile "  catch \"exec rm $xfPath(tmp)/#$xfShortName$xfStatus(uniqueId)~\""
    puts $xfOutFile "  catch \"exec command.com /c del $xfFileName >@stderr\""
    puts $xfOutFile "  catch \"exec command.com /c del $xfFileName~ >@stderr\""
    puts $xfOutFile "  catch \"exec command.com /c del [file dirname $xfFileName]/#[file tail $xfFileName] >@stderr\""
    puts $xfOutFile "  catch \"exec command.com /c del [file dirname $xfFileName]/#[file tail $xfFileName]~ >@stderr\""
    puts $xfOutFile "  catch \"exec command.com /c del $xfPath(tmp)/$xfShortName$xfStatus(uniqueId) >@stderr\""
    puts $xfOutFile "  catch \"exec command.com /c del $xfPath(tmp)/$xfShortName$xfStatus(uniqueId)~ >@stderr\""
    puts $xfOutFile "  catch \"exec command.com /c del $xfPath(tmp)/#$xfShortName$xfStatus(uniqueId) >@stderr\""
    puts $xfOutFile "  catch \"exec command.com /c del $xfPath(tmp)/#$xfShortName$xfStatus(uniqueId)~ >@stderr\""
    puts $xfOutFile "  catch \"send \\\"[winfo name .]\\\" \\\"global xfEditing;" nonewline
    puts $xfOutFile "  unset \\\{xfEditing($xfProcName)\\\}\\\"\""
    puts $xfOutFile "  catch \"console show\""
    puts $xfOutFile "}"
    close $xfOutFile

    catch "exec $xfConf(interpreterEdit) -width 4000 -height 4000 -name edit$xfShortName $xfPath(tmp)/$xfShortName$xfStatus(uniqueId) &"
  }
}

##########
# Procedure: XFMiscClearInterpreter
# Description: remove all procedures and windows
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFMiscClearInterpreter {} {
  global internalAliasList
  global autoLoadList
  global moduleList
  global symbolicName
  global xfConf
  global xfEditing
  global xfStatus

  foreach xfCounter [info procs ShowWindow.*] {
    rename $xfCounter {}
  }
  foreach xfCounter [info procs DetroyWindow.*] {
    rename $xfCounter {}
  }

  foreach xfCounter [image names] {
    image delete $xfCounter
  }

  foreach xfCounter [info globals xfShowWindow.*] {
    global $xfCounter
    unset $xfCounter
  }
  foreach xfCounter [info procs *] {
    if {(![XFMiscIsXFElement $xfCounter] &&
         ![XFMiscIsXFSpecialElement $xfCounter]) ||
        [string match Startup* $xfCounter] ||
        [string match Middle* $xfCounter] ||
        [string match End* $xfCounter]} {
      rename $xfCounter {}
    }
  }
  foreach xfCounter [info globals *] {
    if {![XFMiscIsXFElement $xfCounter]} {
      global $xfCounter
      unset $xfCounter
    }
  }

  # initialize variables
  foreach xfName [array names moduleList] {
    unset moduleList($xfName)
  }
  foreach xfName [array names autoLoadList] {
    unset autoLoadList($xfName)
  }
  foreach xfName [array names symbolicName] {
    unset symbolicName($xfName)
  }
  set moduleList(main.tcl) ""
  set autoLoadList(main.tcl) 0
  set symbolicName(root) .
  set internalAliasList ""
  set xfConf(programName) "main.tcl"
  set xfConf(programNameOld) "main.tcl"
  set xfEditing(xfInternal) "xfInternal"
  set xfStatus(elementCounter) 0
  set xfStatus(path) .
  set xfStatus(pasteScriptDisplayed) 0
  set xfStatus(pasteTreeDisplayed) 0

  # delete subwindows
  foreach xfCounter [winfo children .] {
    if {"$xfCounter" != ".xfEdit" &&
        "$xfCounter" != ".xfYesNo"} {
      catch "XFDestroy $xfCounter"
    }
  }
  wm title . xf

  XFEditSetShowWindows
  XFEditSetPath .
  update
}

##########
# Procedure: XFMiscClearList
# Description: clear listbox widget
# Arguments: xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFMiscClearList {xfW} {

  if {[$xfW size] > 0} {
    $xfW delete 0 end
  }
}

##########
# Procedure: XFMiscClearText
# Description: clear text widget
# Arguments: xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFMiscClearText {xfW} {

  set xfStatus [lindex [$textWidget config -state] 4]
  $textWidget config -state normal
  $textWidget delete 1.0 end
  $textWidget config -state $xfStatus
}

##########
# Procedure: XFMiscCorrectLevel
# Description: check if given string contains a level indicator
#              and this level indicator allows displaying/saving
# Arguments: xfType - the type of string we check (bind or proc)
#            xfString - the string to check
# Returns: 1 if string is allowd for displaying/saving, 0 otherwise
# Sideeffects: none
##########
proc XFMiscCorrectLevel {xfType xfString} {
  global xfBindSaveLevel
  global xfBindShowLevel
  global xfProcSaveLevel
  global xfProcShowLevel

  set xfTmpString [string trimleft $xfString]
  if {[string match "# xf ignore me 9*" $xfTmpString]} {
    return 0
  }
  if {[string match "# xf ignore me*" $xfTmpString]} {
    set xfLevel [string index $xfTmpString 15]
    if {"$xfType" == "bindsave"} {
      return $xfBindSaveLevel($xfLevel)
    } {
      if {"$xfType" == "bindshow"} {
        return $xfBindShowLevel($xfLevel)
      } {
        if {"$xfType" == "procsave"} {
          return $xfProcSaveLevel($xfLevel)
        } {
          return $xfProcShowLevel($xfLevel)
        }
      }
    }
  }
  return 1
}

##########
# Procedure: XFMiscDeleteMenuChilds
# Description: delete all children of the given menu
# Arguments: xfW - the menu widget
# Returns: none
# Sideeffects: none
##########
proc XFMiscDeleteMenuChilds {xfW} {

  $xfW delete 0 last
}

##########
# Procedure: XFMiscExpandRegexp
# Description: expand characters with special meaning
# Arguments: xfExpression - the string
# Returns: string - the expanded regular expression
# Sideeffects: none
##########
proc XFMiscExpandRegexp {xfExpression} {

  # replace period
  set xfPosition 0
  set xfResult ""
  while {$xfPosition < [string length $xfExpression]} {
    set xfCurrent [string index $xfExpression $xfPosition]
    if {[string match $xfCurrent "."] &&
        ![string match $xfCurrent "\*"]} {
      append xfResult "\\$xfCurrent"
    } {
      append xfResult $xfCurrent
    }
    incr xfPosition 1
  }
  return $xfResult
}

##########
# Procedure: XFMiscFlash
# Description: flash the widget
# Arguments: xfW - the widget
# Returns: none
# Sideeffects: none
# MYCHECK
##########
proc XFMiscFlash {xfW} {
  global xfConf

  if {"[info commands $xfW]" == ""} {
    return
  }
  if {"$xfW" != "."} {
    if {"[winfo class $xfW]" == "Frame"} {
      if {"[lindex [$xfW config -width] 4]" == "30x30" &&
          "[pack slave $xfW]" != ""} {
        $xfW config -width ""
      }
    }
    if {![catch {set xfSaveBackColor [lindex [$xfW configure -background] 4]}]} {
      catch "$xfW configure -background $xfConf(flash)"
      update
      catch "$xfW configure -background \"$xfSaveBackColor\""
      update
      catch "$xfW configure -background $xfConf(flash)"
      update
      catch "$xfW configure -background \"$xfSaveBackColor\""
      update
    }
  }
}

##########
# Procedure: XFMiscGetUniqueName
# Description: create a unique name for a newly inserted widget
# Arguments: xfName - the element name
#            xfType - the type we want to create
#            {xfW} - the parent widget
# Returns: the created unique name
# Sideeffects: none
##########
proc XFMiscGetUniqueName {xfName xfType {xfW ""}} {
  global xfConf
  global xfInputBox
  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  if {"$xfName" == ""} {
    set xfName $xfType$xfStatus(elementCounter)
  }
  if {$xfConf(getWidgetName)} {
    set xfTextBox(.xfWidgetName,inputOne) $xfName
  }
  if {"$xfW" == "."} {
    set xfRepeat 1
    while {$xfRepeat} {
      set xfRepeat 0
      if {$xfConf(getWidgetName)} {
        auto_load XFInputBoxOne
        set xfInputBox(toplevelName) ".xfWidgetName"
        set xfInputBox(erase) 0
        set xfInputBox(.xfWidgetName,inputOne) $xfName
        set xfName [string trim [XFProcInputBoxOne "Enter widget name:" 400x100 "XF widget name"]]
        set xfInputBox(toplevelName) ".xfTextBox"
        if {"$xfName" == ""} {
          error ""
        }
      }
      foreach xfCounter [winfo children .] {
        if {".$xfName" == "$xfCounter"} {
          set xfRepeat 1
          if {!$xfConf(getWidgetName)} {
            set xfName $xfType$xfStatus(elementCounter)
            incr xfStatus(elementCounter)
          }
        }
      }
      foreach xfCounter [info commands ShowWindow.*] {
        if {".$xfName" == "[string range $xfCounter 10 end]"} {
          set xfRepeat 1
          if {!$xfConf(getWidgetName)} {
            set xfName $xfType$xfStatus(elementCounter)
            incr xfStatus(elementCounter)
          }
        }
      }
    }
  } {
    set xfRepeat 1
    while {$xfRepeat} {
      set xfRepeat 0
      if {$xfConf(getWidgetName)} {
        auto_load XFInputBoxOne
        set xfInputBox(toplevelName) ".xfWidgetName"
        set xfInputBox(erase) 0
        set xfInputBox(.xfWidgetName,inputOne) $xfName
        set xfName [string trim [XFProcInputBoxOne "Enter widget name:" 400x100 "XF widget name"]]
        set xfInputBox(toplevelName) ".xfTextBox"
        if {"$xfName" == ""} {
          error ""
        }
      }
      foreach xfCounter [winfo children $xfW] {
        if {"$xfW.$xfName" == "$xfCounter"} {
          set xfRepeat 1
          if {!$xfConf(getWidgetName)} {
            set xfName $xfType$xfStatus(elementCounter)
            incr xfStatus(elementCounter)
          }
        }
      }
      foreach xfCounter [info commands ShowWindow.*] {
        if {"$xfW.$xfName" == "[string range $xfCounter 10 end]"} {
          set xfRepeat 1
          if {!$xfConf(getWidgetName)} {
            set xfName $xfType$xfStatus(elementCounter)
            incr xfStatus(elementCounter)
          }
        }
      }
    }
  }
  return $xfName
}

##########
# Procedure: XFMiscGetText
# Description: get current contents of text widget
# Arguments: xfW - the text widget
# Returns: the entered text in the text widget
# Sideeffects: none
##########
proc XFMiscGetText {xfW} {

  if {"[winfo class $xfW]" == "Text"} {
    set str [$xfW get 1.0 end]
    if {[string index $str [expr [string length $str] - 1]] == "\n"} {
      set str [string trimright $str "\n"]
    }
    return $str
  } {
    if {"[winfo class $xfW]" == "TkEmacs" ||
        "[winfo class $xfW]" == "Entry"} {
      return [$xfW get]
    }
  }
}

##########
# Procedure: XFMiscHandleHiding
# Description: handle hiding of procedures
# Arguments: xfType - the procedure type we handle
#            xfW - the list widget to update
#            xfName - the procedure ti hide/unhide
# Returns: none
# Sideeffects: none
##########
proc XFMiscHandleHiding {xfType xfW xfName} {
  global xfStatus
  global hiddenProcs
  global hiddenBodys

  if {$xfStatus(hiddenProcs)} {
    if {[info exists hiddenProcs($xfName)]} {
      catch "proc $xfName $hiddenBodys($xfName)" res
      unset hiddenProcs($xfName)
      unset hiddenBodys($xfName)
      XFMiscSetInfo hidden$xfType $xfW 1
    }
  } {
    if {"[info procs $xfName]" != ""} {
      set xfArguments ""
      set xfBodyList ""
      set xfBodyList [string trimright [info body $xfName]]
      set xfArgList [info args $xfName]
      foreach xfCounter $xfArgList {
        if {[info default $xfName $xfCounter xfDefault]} {
          append xfArguments " \{$xfCounter \"$xfDefault\"\}"
        } {
          append xfArguments " $xfCounter"
        }
      }
      set hiddenProcs($xfName) ""
      if {[string index $xfBodyList 0] == "\n"} {
        set hiddenBodys($xfName) " {$xfArguments} {[string range $xfBodyList 1 end]}"
      } {
        set hiddenBodys($xfName) " {$xfArguments} {$xfBodyList}"
      }
      rename $xfName {}
      XFMiscSetInfo $xfType $xfW 1
    }
  }
}

##########
# Procedure: XFMiscInitElement
# Description: set the default parameters of a element
# Arguments: xfW - the root widget
# Arguments: xfName - the element name
# Returns: none
# Sideeffects: none
##########
proc XFMiscInitElement {xfW xfName} {
  global xfStatus

  XFMiscSetResource $xfW.params1.params2.$xfName.label$xfName width \
    $xfStatus(elementWidth)
  XFMiscSetResource $xfW.params1.params2.$xfName.label$xfName anchor e
  XFMiscSetResource $xfW.params1.params2.$xfName.label$xfName relief flat
  XFMiscSetResource $xfW.params1.params2.$xfName.$xfName relief sunken
}

##########
# Procedure: XFMiscInsertText
# Description: insert text into contents of text widget
# Arguments: xfW - the text widget
#            xfContents - the text to insert
# Returns: none
# Sideeffects: none
##########
proc XFMiscInsertText {xfW {xfContents ""}} {

  if {"[winfo class $xfW]" == "Text" ||
      "[winfo class $xfW]" == "TkEmacs" ||
      "[winfo class $xfW]" == "Entry"} {
    set xfOldStatus [lindex [$xfW config -state] 4]
    $xfW config -state normal
    $xfW insert insert $xfContents
    $xfW config -state $xfOldStatus
  }
}

##########
# Procedure: XFMiscInsertTextIntoWidget
# Description: insert text into a specified widget
# Arguments: xfW - the widget
#            xfText - the text
# Returns: none
# Sideeffects: none
##########
proc XFMiscInsertTextIntoWidget {xfW xfText} {
  
  if {[winfo exists $xfW]} {
    XFMiscInsertText $xfW $xfText
  }
}

##########
# Procedure: XFMiscInModule
# Description: check if element is in module
# Arguments: xfName - the name of the module item
# Returns: 0 - module is not in module list
#          1 - module is in module list
# Sideeffects: none
##########
proc XFMiscInModule {xfName} {
  global moduleList
  global xfConf

  XFMiscUpdateModuleList
  foreach xfModName [array names moduleList] {
    foreach xfCounter [set moduleList($xfModName)] {
      if {[lsearch [set moduleList($xfModName)] $xfName] >= 0} {
        if {"$xfModName" == "$xfConf(programName)"} {
          return 0
        } {
          return 1
        }
      }
    }
  }
  return 0
}

##########
# Procedure: XFMiscIsDir
# Description: check if path is a directory
# Arguments: xfPathName - the path to check
# Returns: 1 if its a directory, otherwise 0
# Sideeffects: none
##########
proc XFMiscIsDir {xfPathName} {

  if {[file isdirectory $xfPathName]} {
    return 1
  } {
    catch "file type $xfPathName" xfType
    if {"$xfType" == "link"} {
      if {[catch "file readlink $xfPathName" xfLinkName]} {
        return 0
      }
      catch "file type $xfLinkName" xfType
      while {"$xfType" == "link"} {
        if {[catch "file readlink $xfLinkName" xfLinkName]} {
          return 0
        }
        catch "file type $xfLinkName" xfType
      }
      return [file isdirectory $xfLinkName]
    }
  }
  return 0
}

##########
# Procedure: XFMiscIsFile
# Description: check if filename is a file
# Arguments: xfFileName - the path to check
# Returns: 1 if its a file, otherwise 0
# Sideeffects: none
##########
proc XFMiscIsFile {xfFileName} {

  if {[file isfile $xfFileName]} {
    return 1
  } {
    catch "file type $xfFileName" xfType
    if {"$xfType" == "link"} {
      if {[catch "file readlink $xfFileName" xfLinkName]} {
        return 0
      }
      catch "file type $xfLinkName" xfType
      while {"$xfType" == "link"} {
        if {[catch "file readlink $xfLinkName" xfLinkName]} {
          return 0
        }
        catch "file type $xfLinkName" xfType
      }
      return [file isfile $xfLinkName]
    }
  }
  return 0
}

##########
# Procedure: XFMiscIsSymlink
# Description: check if path is a symbolic link
# Arguments: xfPathName - the path to check
# Returns: 1 if its a directory, otherwise 0
# Sideeffects: none
##########
proc XFMiscIsSymlink {xfPathName} {

  catch "file type $xfPathName" xfType
  if {"$xfType" == "link"} {
    return 1
  }
  return 0
}

##########
# Procedure: XFMiscIsXFElement
# Description: check if element is a valid (application specific) element
# Arguments: xfName - the name of the module item
# Returns: 0 - name is not a xf element
#          1 - name is a xf element
# Sideeffects: none
##########
proc XFMiscIsXFElement {xfName} {
  global xfStartupGlobal
  global xfStartupProcs

  case $xfName in {
    {tk* xf* .xf* XF*} {
      return 1
    }
    {ShowWindow* DestroyWindow* StartupSrc* MiddleSrc* EndSrc*} {
      return 1
    }
    {auto_*} {
      if {"$xfName" == "auto_execok" ||
          "$xfName" == "auto_load" ||
          "$xfName" == "auto_mkindex" ||
          "$xfName" == "auto_oldpath" ||
          "$xfName" == "auto_reset" ||
          "$xfName" == "auto_execs" ||
          "$xfName" == "auto_index" ||
          "$xfName" == "auto_path"} {
        return 1
      }
    }
    {InitGlobals PreloadPixmaps autoLoadList moduleList internalAliasList preloadList symbolicName} {
      return 1
    }
    {button checkbutton entry frame label listbox menu menubutton message radiobutton scrollbar scale text toplevel argc argv destroy env errorCode errorInfo exit parray postedMenu selectedButton tk_version tkerror bgerror unknown} {
      return 1
    }
  }
  if {"$xfName" != "." && [string match ".*" $xfName] &&
      [winfo exists $xfName]} {
    if {[string match "xf*" [winfo name $xfName]]} {
      return 1
    }
  }
  if {[lsearch $xfStartupProcs $xfName] != -1 ||
      [lsearch $xfStartupGlobal $xfName] != -1} {
    return 1
  }
  return 0
}

##########
# Procedure: XFMiscIsXFSpecialElement
# Description: check if element is a special xf element
# Arguments: xfName - the name of the module item
# Returns: 0 - name is not a xf element
#          1 - name is a xf element
# Sideeffects: none
##########
proc XFMiscIsXFSpecialElement {xfName} {

  if {"$xfName" == "Alias" ||
      "$xfName" == "Unalias" ||
      "$xfName" == "GetSelection" ||
      "$xfName" == "NoFunction" ||
      "$xfName" == "SymbolicName" ||
      "$xfName" == "Ls" ||
      "$xfName" == "Cp" ||
      "$xfName" == "Cat" ||
      "$xfName" == "Rm" ||
      "$xfName" == "Chmod" ||
      "$xfName" == "SN"} {
    return 1
  }
  return 0
}

##########
# Procedure: XFMiscParseGeometry
# Description: split a x style geometry into a list
# Arguments: xfGeometry - the geometry
#            xfW - the toplevel
# Returns: a list containing the geometry
# Sideeffects: none
##########
proc XFMiscParseGeometry {xfGeometry {xfW ""}} {

  set xfCounter [string length $xfGeometry]
  set xfEnd $xfCounter
  set xfRootX 0
  set xfRootY 0
  set xfRootWidth 0
  set xfRootHeight 0
  if {[string first "+" $xfGeometry] != -1 ||
      [string first "-" $xfGeometry] != -1} {
    while {$xfCounter >= 0} {
      if {[string compare [string index $xfGeometry $xfCounter] +] == 0 ||
          [string compare [string index $xfGeometry $xfCounter] -] == 0} {
        set xfRootY [string range $xfGeometry $xfCounter end]
        set xfEnd $xfCounter
        break
      }
      incr xfCounter -1
    }
    while {$xfCounter >= 0} {
      if {[string compare [string index $xfGeometry $xfCounter] +] != 0 &&
          [string compare [string index $xfGeometry $xfCounter] -] != 0} {
        break
      }
      incr xfCounter -1
      incr xfEnd -1
    }
    while {$xfCounter >= 0} {
      if {[string compare [string index $xfGeometry $xfCounter] +] == 0 ||
          [string compare [string index $xfGeometry $xfCounter] -] == 0} {
        set xfRootX [string range $xfGeometry $xfCounter $xfEnd]
        set xfEnd $xfCounter
        break
      }
      incr xfCounter -1
    }
    while {$xfCounter >= 0} {
      if {[string compare [string index $xfGeometry $xfCounter] +] != 0 &&
          [string compare [string index $xfGeometry $xfCounter] -] != 0} {
        break
      }
      incr xfCounter -1
      incr xfEnd -1
    }
  } {
    set xfRootX [winfo rootx $xfW]
    set xfRootY [winfo rooty $xfW]
  }
  if {[string first "x" $xfGeometry] != -1} {
    while {$xfCounter >= 0} {
      if {"[string index $xfGeometry $xfCounter]" == "x"} {
        set xfRootHeight [string range $xfGeometry [expr $xfCounter+1] $xfEnd]
        set xfEnd $xfCounter
        break
      }
      incr xfCounter -1
    }
    while {$xfCounter >= 0} {
      if {"[string index $xfGeometry $xfCounter]" != "x"} {
        break
      }
      incr xfCounter -1
      incr xfEnd -1
    }
    set xfRootWidth [string range $xfGeometry 0 $xfEnd]
  } {
    set xfRootWidth [winfo reqwidth $xfW]
    set xfRootHeight [winfo reqwidth $xfW]
  }
  return [list $xfRootWidth $xfRootHeight $xfRootX $xfRootY]
}

##########
# Procedure: XFMiscPathName
# Description: cut the path from the widget name
# Arguments: xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFMiscPathName {xfW} {

  if {[string last . $xfW] >= 0} {
    set xfResult [string range $xfW [expr [string last . $xfW]+1] end]
    return $xfResult
  } {
    return $xfW
  }
}

##########
# Procedure: XFMiscPathTail
# Description: cut the tail of a path from the widget name
# Arguments: xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFMiscPathTail {xfW} {

  set xfResult [split $xfW .]
  if {"$xfW" == "."} {
    return .
  } {
    if {[llength $xfResult] > 3} {
      return ....[lindex $xfResult [expr [llength $xfResult]-2]].[lindex $xfResult [expr [llength $xfResult]-1]]
    } {
      if {[llength $xfResult] == 3} {
        return .[lindex $xfResult [expr [llength $xfResult]-2]].[lindex $xfResult [expr [llength $xfResult]-1]]
      } {
        if {[llength $xfResult] == 2} {
          return .[lindex $xfResult [expr [llength $xfResult]-1]]
        } {
          return .
        }
      }
    }
  }
}

##########
# Procedure: XFMiscPositionWidget
# Description: position a newly created widget
# Arguments: xfW - the widget
#            {xfPackerOptions} - options for the packer
#            {xfPlacerOptions} - options for the placer
# Returns: none
# Sideeffects: none
##########
proc XFMiscPositionWidget {xfW {xfPackerOptions ""} {xfPlacerOptions ""}} {
  global xfConf

  if {"[winfo class [winfo parent $xfW]]" == "Canvas"} {
    [winfo parent $xfW] create window 0 0 -anchor nw -window $xfW
    return
  }

  if {"$xfConf(geometry)" == "packer"} {
    if {"$xfPackerOptions" == ""} {
      set xfPackString ""
      set xfChildClass ""
      set xfTmpPacking ""
      if {"$xfTmpPacking" != ""} {
        foreach xfElement $xfTmpPacking {
          case $xfElement {
            {top} {
              append xfPackString "top "
            }
            {left} {
              append xfPackString "left "
            }
            {right} {
              append xfPackString "right "
            }
            {bottom} {
              append xfPackString "bottom "
            }
            {fillx} {
              append xfPackString "fillx "
            }
            {filly} {
              append xfPackString "filly "
            }
            {fill} {
              append xfPackString "fill "
            }
            {expand} {
              if {"$xfChildClass" != "" &&
                  "$xfChildClass" == "[winfo class $xfW]"} {
                append xfPackString "expand "
              }
            }
          }
        }
      }
      if {"$xfPackString" == ""} {
        if {"[winfo class [winfo parent $xfW]]" == "XFForm"} {
          pack append [winfo parent $xfW] \
            $xfW {top fillx}
        } {
          pack append [winfo parent $xfW] \
            $xfW {top}
        }
      } {
        pack append [winfo parent $xfW] \
          $xfW $xfPackString
      }
    } {
      pack append [winfo parent $xfW] \
        $xfW "$xfPackerOptions"
    }
  } {
    if {"$xfPackerOptions" == ""} {
      place $xfW -x 0 -y 0
    } {
      eval place $xfW $xfPackerOptions
    }
  }
}

##########
# Procedure: XFMiscPutFileInList
# Description: fill a list with the contents of the file
# Arguments: xfW - the widget
#            xfFileName - filename to read
# Returns: none
# Sideeffects: none
##########
proc XFMiscPutFileInList {xfW xfFileName} {

  # check file existance
  if {[catch "open $xfFileName r" xfInFile]} {
    XFProcError "$xfInFile"
  } {
    set xfFileContents [read $xfFileName]
    close $xfInFile
    foreach xfLine [split $xfFileContents "\n"] {
      $xfW insert end $xfLine
    }
  }
}

##########
# Procedure: XFMiscPutFileInText
# Description: fill a text with the contents of the file
# Arguments: xfW - the widget
#            xfFileName - filename to read
# Returns: none
# Sideeffects: none
##########
proc XFMiscPutFileInText {xfW xfFileName} {

  # check file existance
  if {[catch "open $xfFileName r" xfInFile]} {
    XFProcError "$xfInFile"
  } {
    $xfW insert end "[read $xfInFile]"
    close $xfInFile
  }
}

##########
# Procedure: XFMiscQuit
# Description: quit xf
# Arguments: xfRetVal - return value
# Returns: none
# Sideeffects: none
##########
proc XFMiscQuit {{xfRetVal 0}} {
  global xfConf
  global xfPath
  global xfStatus

  if {$xfConf(saveOptions)} {
    XFProcOptionsSaveOptions
  }
  if {$xfConf(savePositions)} {
    XFProcOptionsSavePositions
  }

  foreach xfCounter [winfo children .] {
    catch "XFDestroy $xfCounter"
  }
  catch "send \"XF-tutorial\" \"catch \\\"destroy .\\\"; catch \\\"conole show\\\"\""
  catch "Rm $xfPath(tmp)/cb$xfStatus(uniqueId)"
  catch "Rm $xfPath(tmp)/tb$xfStatus(uniqueId)"
  catch "Rm $xfPath(tmp)/lc$xfStatus(uniqueId)"
  catch "Rm $xfPath(tmp)/tr$xfStatus(uniqueId).grl"  
  catch "Rm $xfPath(tmp)/et$xfStatus(uniqueId)"
  catch "Rm $xfPath(tmp)/st$xfStatus(uniqueId)"

  set xfCounter 0
  while {$xfCounter < $xfConf(maxSaveId)} {
    if {[file exists $xfPath(tmp)/as$xfStatus(uniqueId)$xfCounter]} {
      catch "Rm $xfPath(tmp)/as$xfStatus(uniqueId)$xfCounter"
    }
    if {[file exists $xfPath(tmp)/as$xfStatus(uniqueId)$xfCounter~]} {
      catch "Rm $xfPath(tmp)/as$xfStatus(uniqueId)$xfCounter~"
    }
    incr xfCounter 1
  }

  flush stdout
  flush stderr

  catch "XFDestroy ."
  XFExit $xfRetVal
}

##########
# Procedure: XFMiscReadTree
# Description: read the complete widget tree
# Arguments: xfW - the current widget
#            xfList - the list to fill
#            xfType - read all, or only descendants
#            xfSymbol - read symbolic names or not (sym)
# Returns: none
# Sideeffects: none
##########
proc XFMiscReadTree {xfW xfList {xfType ""} {xfSymbol 0}} {
  global symbolicName

  XFMiscClearList $xfList
  set xfCounter 1
  if {"[winfo class $xfW]" != "Toplevel"} {
    set xfRoot [string range $xfW 0 [string first . [string range $xfW 1 end]]]
  } {
    set xfRoot $xfW
  }
  if {"$xfRoot" == ""} {
    set xfRoot .
  } {
    if {"[winfo class $xfRoot]" != "Toplevel"} {
      set xfRoot .
    }
  }
  if {$xfSymbol == 0} {
    $xfList insert end "1 $xfRoot"
  } {
    set xfCounter3 ""
    foreach xfCounter2 [array names symbolicName] {
      set xfArrayName ""
      append xfArrayName symbolicName ( $xfCounter2 )
      if {"$xfRoot" == "[set $xfArrayName]"} {
        set xfCounter3 $xfCounter2
        break
      }
    }
    if {"$xfCounter3" != ""} {
      $xfList insert end "1 $xfRoot = $xfCounter3"
    } {
      $xfList insert end "1 $xfRoot"
    }
  }
  XFMiscReadTreeBuild $xfRoot $xfList $xfType 2 $xfSymbol

  set xfCounter 0
  set xfLength [$xfList size]
  while {$xfCounter < $xfLength} {
    if {"[lindex [$xfList get $xfCounter] 1]" == "$xfW"} {
      $xfList selection set $xfCounter
    }
    incr xfCounter 1
  }
}

##########
# Procedure: XFMiscReadTreeBuild
# Description: read the complete widget tree and enter it into list
# Arguments: xfW - the current widget
#            xfList - the list to fill
#            xfType - read all, or only descendants
#            xfLevel - the level
#            xfSymbol - read symbolic names or not (sym)
# Returns: none
# Sideeffects: none
##########
proc XFMiscReadTreeBuild {xfW xfList xfType xfLevel {xfSymbol 0}} {
  global symbolicName

  set xfCounter 0
  set xfOffset ""
  while {$xfCounter < $xfLevel} {
    append xfOffset "  "
    incr xfCounter 1
  }
  foreach xfCounter [winfo children $xfW] {
    if {![string match ".xf*" $xfCounter] &&
        ![string match "xf*" [winfo name $xfCounter]]} {
      if {"[winfo class $xfCounter]" != "Toplevel" || "$xfType" == "all"} {
        if {$xfSymbol == 0} {
          $xfList insert end "$xfLevel$xfOffset$xfCounter"
        } {
          set xfCounter3 ""
          foreach xfCounter2 [array names symbolicName] {
            set xfArrayName ""
            append xfArrayName symbolicName ( $xfCounter2 )
            if {"$xfCounter" == "[set $xfArrayName]"} {
              set xfCounter3 $xfCounter2
              break
            }
          }
          if {"$xfCounter3" != ""} {
            $xfList insert end "$xfLevel$xfOffset$xfCounter = $xfCounter3"
          } {
            $xfList insert end "$xfLevel$xfOffset$xfCounter"
          }
        }
        XFMiscReadTreeBuild $xfCounter $xfList $xfType \
          [expr $xfLevel+1] $xfSymbol
      }
    }
  }
}

##########
# Procedure: XFMiscRemoveBindWidgetTree
# Description: remove the bindings for all widgets in the specified
#              tree
# Arguments: xfW - the current widget
#            xfType - all to remove bindings for all widgets (notall else)
# Returns: none
# Sideeffects: none
##########
proc XFMiscRemoveBindWidgetTree {xfW xfType} {
  global xfBind

  if {(![string match ".xf*" $xfW] ||
       ![string match "xf*" [winfo name $xfW]]) ||
      "$xfW" == "." || "$xfType" == "all"} {
    catch "bind $xfW $xfBind(configure) \"\""
    catch "bind $xfW $xfBind(placing) \"\""
    catch "bind $xfW $xfBind(placingMotion) \"\""
    catch "bind $xfW $xfBind(placingRelease) \"\""
    catch "bind $xfW $xfBind(select) \"\""
    catch "bind $xfW $xfBind(showName) \"\""
    catch "bind $xfW $xfBind(removeName) \"\""
  }
  foreach xfCounter [winfo children $xfW] {
    if {(![string match ".xf*" $xfCounter] &&
         ![string match "xf*" [winfo name $xfCounter]]) ||
        "$xfType" == "all"} {
      XFMiscRemoveBindWidgetTree $xfCounter $xfType
    }
  }
}

##########
# Procedure: XFMiscSaveError
# Description: save the error message passed as parameter
# Arguments: xfMessage - the error message
#            xfInfo - the error info
#            xfCode - the error code
# Returns: none
# Sideeffects: none
##########
proc XFMiscSaveError {xfMessage {xfInfo ""} {xfCode ""}} {
  global env
  global xfPath
  global xfStatus

  catch "Chmod a+w $xfPath(tmp)/xferrors$xfStatus(uniqueId)"
  catch "Chmod a+r $xfPath(tmp)/xferrors$xfStatus(uniqueId)"
  if {![catch "open $xfPath(tmp)/xferrors$xfStatus(uniqueId) a" xfOutFile]} {
    catch "Chmod a+w $xfPath(tmp)/xferrors$xfStatus(uniqueId)"
    catch "Chmod a+r $xfPath(tmp)/xferrors$xfStatus(uniqueId)"
    set xfIdString ""
    if {[info exists env(USER)]} {
      append xfIdString "$env(USER)"
    }
    if {[info exists env(HOST)]} {
      append xfIdString "@$env(HOST)"
    }
    append xfIdString "; [clock format [clock seconds]]"
    puts $xfOutFile "#################:$xfIdString\n$xfMessage\n\n"
    close $xfOutFile
  }
}

##########
# Procedure: XFMiscSetI
# Description: set the contents of the info list
# Arguments: xfType - what should be displayed (procs, globals, commands, images)
#            xfList - the list we want to feed
#            xfSet - the type of setting (1 = set always, 0 = set
#                    only if permanent apply is on)
# Returns: none
# Sideeffects: none
##########
proc XFMiscSetInfo {xfType xfList xfSet} {
  global hiddenProcs
  global xfConf
  global xfStatus

  if {!$xfSet && !$xfStatus(rescanInfo)} {
    return
  }
  XFMiscClearList $xfList
  if {"$xfType" == "hiddenprocs" ||
      "$xfType" == "hiddencommands"} {
    if {[info exists hiddenProcs]} {
      set xfElementList [array names hiddenProcs]
    } {
      set xfElementList ""
    }

  } elseif {"$xfType" == "images"} {
    set xfElementList [image names]

  } {
    set xfElementList [lsort [info $xfType]]
  }

  set xfResult $xfStatus(includeExclude)
  foreach xfInfoCounter $xfElementList {
    if {"$xfStatus(includeExcludeString)" != ""} {
      if {[catch "regexp \{$xfStatus(includeExcludeString)\} $xfInfoCounter" xfResult]} {
        set xfResult 0
      }
    } {
        set xfResult 1
    }
    if {$xfResult == $xfStatus(includeExclude)} {
      case $xfType in {
        {globals} {
          if {[XFMiscIsXFElement $xfInfoCounter]} {
            continue
          }
          global $xfInfoCounter
          if {[array names $xfInfoCounter] == ""} {
            if {"[string trim $xfInfoCounter]" != ""} {
              $xfList insert end $xfInfoCounter
            }
          } {
            foreach xfCounter2 [lsort [array names $xfInfoCounter]] {
              set xfArrayName ""
              append xfArrayName $xfInfoCounter ( $xfCounter2 )
              if {"[string trim $xfArrayName]" != ""} {
                $xfList insert end $xfArrayName
              }
            }
          }
          continue
        }
        {procs | commands | hiddenprocs | hiddencommands} {
          if {[XFMiscIsXFElement $xfInfoCounter]} {
            continue
          }
          if {"$xfType" == "procs"} {
            if {"$xfInfoCounter" == "destroy" ||
                "$xfInfoCounter" == "exit"} {
              continue
            }
          }
          if {"[info procs $xfInfoCounter]" != ""} {
            if {![XFMiscCorrectLevel procshow [info body $xfInfoCounter]]} {
              continue
            }
          }
          $xfList insert end $xfInfoCounter
          continue
        }
        {images} {
          $xfList insert end $xfInfoCounter
        }
      }
    }
  }

  set xfInfoCounter 0
  set xfListLength [$xfList size]
  if {"$xfListLength" == "none"} {
    set xfListLength -1
  }
  case $xfType in {
    {commands | hiddencommands} {
      set xfCurrentName $xfStatus(cmdName)
      set xfCurrentIndex $xfStatus(cmdIndex)
    }
    {globals} {
      set xfCurrentName $xfStatus(globalName)
      set xfCurrentIndex $xfStatus(globalIndex)
    }
    {procs | hiddenprocs} {
      set xfCurrentName $xfStatus(procName)
      set xfCurrentIndex $xfStatus(procIndex)
    } 
    {images} {
      set xfCurrentName $xfStatus(imgName)
      set xfCurrentIndex $xfStatus(imgIndex)
    }
  }
  while {$xfInfoCounter < $xfListLength} {
    if {"$xfCurrentName" == "[$xfList get $xfInfoCounter]"} {
      $xfList select anchor $xfInfoCounter
      $xfList select set $xfInfoCounter
      return
    }
    incr xfInfoCounter 1
  }
  if {$xfCurrentIndex < $xfListLength} {
    $xfList select anchor $xfCurrentIndex
    $xfList select set $xfCurrentIndex
  } {
    if {$xfListLength >= 0} {
      $xfList select anchor $xfListLength
      $xfList select set $xfListLength
    }
  }
}

##########
# Procedure: XFMiscSetRootPos
# Description: set position of root window
# Arguments: xfName - the widget to place
#            xfGeometry - the geometry to set
#            xfLeader - the leading window
# Returns: none
# Sideeffects: none
##########
proc XFMiscSetRootPos {xfName xfGeometry xfLeader} {

  set xfTmpGeometry [XFMiscParseGeometry $xfGeometry $xfName]
  set xfLeaderWidth [winfo width $xfLeader]
  set xfLeaderHeight [winfo height $xfLeader]
  set xfWidth [lindex $xfTmpGeometry 0]
  set xfHeight [lindex $xfTmpGeometry 1]
  set xfXPos [lindex $xfTmpGeometry 2]
  set xfYPos [lindex $xfTmpGeometry 3]
  if {$xfWidth == 0 || $xfHeight == 0} {
    set xfTmpGeometry [XFMiscParseGeometry [winfo geometry $xfLeader]]
    set xfWidth [lindex $xfTmpGeometry 0]
    set xfHeight [lindex $xfTmpGeometry 1]
  }
  if {$xfXPos == 0 || $xfYPos == 0} {
    set xfTmpGeometry [XFMiscParseGeometry [winfo geometry $xfLeader]]
    set xfXPos [lindex $xfTmpGeometry 2]
    set xfYPos [lindex $xfTmpGeometry 3]
  }
  if {$xfXPos == 0 || $xfYPos == 0} {
    set xfXPos [winfo x $xfLeader]
    set xfYPos [winfo y $xfLeader]
  }

  if {$xfWidth < $xfLeaderWidth} {
    set xfWidth $xfLeaderWidth
  }
  if {$xfHeight < $xfLeaderHeight} {
    set xfHeight $xfLeaderHeight
  }
  wm geometry $xfName ${xfWidth}x${xfHeight}+${xfXPos}+${xfYPos}
}

##########
# Procedure: XFMiscSetSymbolicName
# Description: set the symbolic name
# Arguments: xfW - the widget
#            xfName - the new symbolic name
# Returns: none
# Sideeffects: none
##########
proc XFMiscSetSymbolicName {xfW {xfName ""}} {
  global symbolicName

  # first check if a name is in use
  if {"$xfName" != ""} {
    set xfArrayName ""
    append xfArrayName symbolicName ( $xfName )
    if {![catch "set \"$xfArrayName\"" xfTmpName]} {
      if {"$xfW" != "$xfTmpName"} {
        if {"[info commands $xfTmpName]" != ""} {
          XFProcError "The symbolic name:\n$xfName\nis already in use for:\n$xfTmpName"
          return
        } {
          XFProcError "The symbolic name:\n$xfName\nwas already in use for:\n$xfTmpName\nThis widget was destroyed or is not visible. $xfW now uses this name!"
        }
      }
    }
  }
  # first remove old name
  foreach xfCounter [array names symbolicName] {
    set xfArrayName ""
    append xfArrayName symbolicName ( $xfCounter )
    if {"$xfW" == "[set "$xfArrayName"]"} {
      unset $xfArrayName
    }
  }
  if {"$xfName" != ""} {
    set xfArrayName ""
    append xfArrayName symbolicName ( $xfName )
    set $xfArrayName $xfW
  }
}

##########
# Procedure: XFMiscSetText
# Description: set contents of text widget
# Arguments: xfW - the widget
#            xfContents - the new text
# Returns: none
# Sideeffects: none
##########
proc XFMiscSetText {xfW {xfContents ""}} {

  if {"[winfo class $xfW]" == "Text"} {
    set xfOldStatus [lindex [$xfW config -state] 4]
    $xfW config -state normal
    $xfW delete 1.0 end
    $xfW insert 1.0 $xfContents
    $xfW config -state $xfOldStatus
  } {
    if {"[winfo class $xfW]" == "TkEmacs"} {
      $xfW delete top end
      $xfW insert top $xfContents
    } {
      if {"[winfo class $xfW]" == "Entry"} {
        set xfOldStatus [lindex [$xfW config -state] 4]
        $xfW config -state normal
        $xfW delete 0 end
        $xfW insert 0 $xfContents
        $xfW config -state $xfOldStatus
      }
    }
  }
}

##########
# Procedure: XFMiscSetTextHeight
# Description: set height of text widget
# Arguments: xfW - the widget
#            xfHeight - the height in lines (or something near to this)
# Returns: none
# Sideeffects: none
# MYCHECK
##########
proc XFMiscSetTextHeight {xfW xfHeight} {

  if {"[winfo class [lindex $xfW 0]]" == "Text"} {
    [lindex $xfW 0] configure \
      -height $xfHeight
  } {
    if {"[winfo class [lindex $xfW 0]]" == "TkEmacs"} {
      set xfTmpVal [lindex [[lindex $xfW 0] configure -width] 4]
      set xfTmpGeometry "[string range $xfTmpVal 0 [string first x $xfTmpVal]][expr [expr 2+$xfHeight]*14]"
      [lindex $xfW 0] configure \
        -width $xfTmpGeometry
    }
  }
}

##########
# Procedure: XFMiscSetTextFont
# Description: set font of text widget
# Arguments: xfW - the widget
#            xfFont - the new font
# Returns: none
# Sideeffects: none
##########
proc XFMiscSetTextFont {xfW xfFont} {

  if {"[winfo class $xfW]" == "Text" ||
      "[winfo class $xfW]" == "TkEmacs" ||
      "[winfo class $xfW]" == "Entry"} {
    catch "$xfW configure -font $xfFont"
  }
}

##########
# Procedure: XFMiscSetResource
# Description: set the resource
# Arguments: xfW - the widget
#            xfName - the resource name
#            xfValue - the new value
# Returns: none
# Sideeffects: none
##########
proc XFMiscSetResource {xfW xfName xfValue} {
  global xfConf

  if {"[info commands $xfW]" == ""} {
    return
  }
  case [winfo class $xfW] in {
    {Scale} {
      if {"$xfName" == "command"} {
        if {"$xfValue" == ""} {
          set xfValue NoFunction
        }
      }
    }
    {Scrollbar} {
      if {"$xfName" == "command"} {
        if {"$xfValue" == ""} {
          set xfValue NoFunction
        }
      }
    }
    {default} {
      if {"$xfName" == "activebackground" ||
          "$xfName" == "activeforeground" ||
          "$xfName" == "background" ||
          "$xfName" == "bitmap" ||
          "$xfName" == "relief" ||
          "$xfName" == "disabledforeground" ||
          "$xfName" == "font" ||
          "$xfName" == "foreground" ||
          "$xfName" == "selector" ||
          "$xfName" == "variable"} {
        if {"$xfValue" == ""} {
          return
        }
      }
      if {"$xfName" == "scrollcommand" ||
          "$xfName" == "xscrollcommand" ||
          "$xfName" == "yscrollcommand"} {
        if {"$xfValue" == ""} {
          set xfValue NoFunction
        }
      }
      if {"$xfName" == "bitmap"} {
        if {"[string index $xfValue 0]" != "@" &&
            [file exists $xfValue]} {
          set xfValue "@$xfValue"
        }
      }
    }
  }
  if {$xfW != "."} {
    puts "attempt to configure: $xfW configure -$xfName {$xfValue}"
    if {$xfConf(encloseConfigure)} {
      if {[catch "$xfW configure -$xfName {$xfValue}" xfResult]} {
	  XFProcError "$xfW\n-$xfName \{$xfValue\}\n$xfResult"
      }
    } {
      if {[catch "$xfW configure -$xfName \"$xfValue\"" xfResult]} {
       XFProcError "$xfW\n-$xfName \{$xfValue\}\n$xfResult"
      }
    }
  }
}

##########
# Procedure: XFMiscSetResourceToTree
# Description: set the resource to a complete widget tree
# Arguments: xfW - the widget
#            xfName - the resource name
#            xfValue - the new value
# Returns: none
# Sideeffects: none
##########
proc XFMiscSetResourceToTree {xfW xfName xfValue} {
  global xfConf

  if {$xfW != "."} {
    if {$xfConf(encloseConfigure)} {
      catch "$xfW configure -$xfName {$xfValue}"
    } {
      catch "$xfW configure -$xfName \"$xfValue\""
    }
  }
  foreach counter [winfo children $xfW] {
    XFMiscSetResourceToTree $counter $xfName $xfValue
  }
}

##########
# Procedure: XFMiscSetTextIntoWidget
# Description: insert text into a specified widget
# Arguments: xfW - the widget
#            xfText - the text
# Returns: none
# Sideeffects: none
##########
proc XFMiscSetTextIntoWidget {xfW xfText} {

  if {[winfo exists $xfW]} {
    XFMiscSetText $xfW $xfText
  }
}

##########
# Procedure: XFMiscToplevelRemove
# Description: remove toplevel
# Arguments: xfW - the toplevel to remove
# Returns: none
# Sideeffects: none
##########
proc XFMiscToplevelRemove {xfW} {

  XFSaveAsProc $xfW
  XFEditSetShowWindows
  XFEditSetPath .
}

##########
# Procedure: XFMiscToplevelShow
# Description: show toplevel
# Arguments: xfW - the toplevel to show
# Returns: none
# Sideeffects: none
##########
proc XFMiscToplevelShow {xfW} {

  eval ShowWindow.$xfW
  XFEditSetShowWindows
  XFEditSetPath .
}

##########
# Procedure: XFMiscUpdateModuleList
# Description: update the module list
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFMiscUpdateModuleList {} {
  global autoLoadList
  global moduleList
  global xfConf

  if {"$xfConf(programName)" != "$xfConf(programNameOld)"} {
    foreach xfCounter [array names moduleList] {
      if {"$xfConf(programNameOld)" == "$xfCounter"} {
        if {[info exists moduleList($xfConf(programNameOld))]} {
          set moduleList($xfConf(programName)) \
            [set moduleList($xfConf(programNameOld))]
          unset moduleList($xfConf(programNameOld))
        }
        if {[info exists autoLoadList($xfConf(programNameOld))]} {
          set autoLoadList($xfConf(programName)) \
            [set autoLoadList($xfConf(programNameOld))]
          unset autoLoadList($xfConf(programNameOld))
        }
      }
    }
    set xfConf(programNameOld) $xfConf(programName)
  }
  if {[catch "set moduleList($xfConf(programName))"]} {
    set moduleList($xfConf(programName)) ""
  }
  if {[catch "set autoLoadList($xfConf(programName))"]} {
    set autoLoadList($xfConf(programName)) 0
  }
}

proc XFMenuBarInit {xfMenuBarUserFile xfMenuBarFile} {
##########
# Procedure: XFMenuBarInit
# Description: initialize the configuration of menubuttons and
#              menus of specified pathnames
# Arguments: xfMenuBarUserFile - the user specific loadfile
#            xfMenuBarFile - the default loadfile
# Returns: none
# Sideeffects: none
##########

  global xfMenuBar

  set xfMenuBar(initialized) 1
  set xfMenuBar(file) $xfMenuBarFile
  set xfMenuBar(userFile) $xfMenuBarUserFile
  if {[file exists $xfMenuBar(userFile)]} {
    if {[catch "source \"$xfMenuBar(userFile)\"" xfMenuBarResult]} {
      puts stderr $xfMenuBarResult
    }
  } {
    if {[file exists $xfMenuBar(file)]} {
      if {[catch "source \"$xfMenuBar(file)\"" xfMenuBarResult]} {
        puts stderr $xfMenuBarResult
      }
    }
  }
}

# eof

