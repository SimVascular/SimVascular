# Program: xf
# Description: info routine for procedures
#
# $Header: xfinfoProcs.tcl[2.5] Wed Mar 10 12:06:18 1993 garfield@garfield frozen $

##########
# Procedure: XFInfoProcedures
# Description: show the selected information
# Arguments: xfTarget - put current selection to this entry
#            xfSelectProc - procedure name to select initially
# Returns: none
# Sideeffects: none
##########
proc XFInfoProcedures {xfTarget {xfSelectProc ""}} {
  global xfBind
  global xfConf
  global xfStatus

  XFEditSetStatus "Calling procedure list..."

  # building widget structure
  XFTmpltToplevel .xfInfoProc 440x500 {XF procedures}

  # 11/22/98 - D. LaBelle - Added some widgets to perform string searches on the procedure text
  XFTmpltFrame .xfInfoProc.framefind 0
  label .xfInfoProc.framefind.findtext \
    -text {Find:}
  entry .xfInfoProc.framefind.string \
    -borderwidth 1 \
    -textvariable xfProcFindstr
  bind .xfInfoProc.framefind.string <Key-Return> {
focus .xfInfoProc.frame4.text.text
set xfsearchloc [.xfInfoProc.frame4.text.text search -forwards $xfProcFindstr "insert + 1 chars"]
if {$xfsearchloc != ""} {
    .xfInfoProc.frame4.text.text mark set insert $xfsearchloc
    .xfInfoProc.frame4.text.text see $xfsearchloc
   }
}

  button .xfInfoProc.framefind.back \
    -borderwidth 1 \
    -highlightthickness 0 \
    -pady 0 \
    -text {<<-} \
    -command {
focus .xfInfoProc.frame4.text.text
set xfsearchloc [.xfInfoProc.frame4.text.text search -backwards $xfProcFindstr insert]
if {$xfsearchloc != ""} {
    .xfInfoProc.frame4.text.text mark set insert $xfsearchloc
    .xfInfoProc.frame4.text.text see $xfsearchloc
   }
     }
  button .xfInfoProc.framefind.fwd \
    -borderwidth 1 \
    -highlightthickness 0 \
    -pady 0 \
    -text {->>} \
    -command {
focus .xfInfoProc.frame4.text.text
set xfsearchloc [.xfInfoProc.frame4.text.text search -forwards $xfProcFindstr "insert + 1 chars"]
if {$xfsearchloc != ""} {
    .xfInfoProc.frame4.text.text mark set insert $xfsearchloc
    .xfInfoProc.frame4.text.text see $xfsearchloc
   }
     }
  # 11/22/98 - End of search feature widget addition

  XFTmpltFrame .xfInfoProc.frame1 0

  button .xfInfoProc.frame1.add \
    -text {Insert} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoProc.current.current get]" != ""} {
        set xfCurrentName [.xfInfoProc.current.current get]
      }
      if {"$xfCurrentName" != ""} {
        if {"[info procs $xfCurrentName]" != ""} {
          if {![XFMiscCorrectLevel procshow [info body $xfCurrentName]]} {
            XFProcError "Invisible procedures with that name exitst"
            return
          }
        }
        XFInfoProceduresSet $xfCurrentName
      }}

  button .xfInfoProc.frame1.edit \
    -text {Edit} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoProc.current.current get]" != ""} {
        set xfCurrentName [.xfInfoProc.current.current get]
      } {
        if {"[.xfInfoProc.procs.procs curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoProc.procs.procs get [.xfInfoProc.procs.procs curselect]]
        }
      }
      if {"$xfCurrentName" != ""} {
        if {"[info procs $xfCurrentName]" != ""} {
          if {![XFMiscCorrectLevel procshow [info body $xfCurrentName]]} {
            XFProcError "Invisible procedures cannot be edited"
            return
          }
        }
        XFInfoProceduresSet $xfCurrentName
      }}

  button .xfInfoProc.frame1.rename \
    -text {Rename} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoProc.current.current get]" != ""} {
        set xfCurrentName [.xfInfoProc.current.current get]
      } {
        if {"[.xfInfoProc.procs.procs curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoProc.procs.procs get [.xfInfoProc.procs.procs curselect]]
        }
      }
      set xfNewName [XFProcInputBoxOne "New procedure name:" 300x100 "XF new procedure name"]
      if {"$xfCurrentName" != "" && "$xfNewName" != "" &&
          "$xfCurrentName" != "$xfNewName"} {
        if {"[info procs $xfCurrentName]" != ""} {
          if {![XFMiscCorrectLevel procshow [info body $xfCurrentName]]} {
            XFProcError "Invisible procedures cannot be renamed"
            return
          }
        }
        if {"[info procs $xfNewName]" != ""} {
          if {![XFMiscCorrectLevel procshow [info body $xfNewName]]} {
            XFProcError "Invisible procedures with that name exists"
            return
          }
        }
        catch "rename $xfCurrentName $xfNewName"
        .xfInfoProc.current.current delete 0 end
        catch "XFMiscSetText .xfInfoProc.frame4.args.args \"\""
        catch "XFMiscSetText .xfInfoProc.frame4.text.text \"\""
        XFMiscSetInfo procs .xfInfoProc.procs.procs 1
      }}

  button .xfInfoProc.frame1.remove \
    -text {Remove} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoProc.current.current get]" != ""} {
        set xfCurrentName [.xfInfoProc.current.current get]
      } {
        if {"[.xfInfoProc.procs.procs curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoProc.procs.procs get [.xfInfoProc.procs.procs curselect]]
        }
      }
      if {"$xfCurrentName" != ""} {
        if {"[info procs $xfCurrentName]" != ""} {
          if {![XFMiscCorrectLevel procshow [info body $xfCurrentName]]} {
            XFProcError "Invisible procedures cannot be removed"
            return
          }
        }
        catch "rename $xfCurrentName {}"
        XFMiscSetText .xfInfoProc.current.current ""
        catch "XFMiscSetText .xfInfoProc.frame4.args.args \"\""
        catch "XFMiscSetText .xfInfoProc.frame4.text.text \"\""
        XFMiscSetInfo procs .xfInfoProc.procs.procs 1
      }}

  button .xfInfoProc.frame1.hide \
    -text {Hide} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoProc.current.current get]" != ""} {
        set xfCurrentName [.xfInfoProc.current.current get]
      } {
        if {"[.xfInfoProc.procs.procs curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoProc.procs.procs get [.xfInfoProc.procs.procs curselect]]
        }
      }
      if {"$xfCurrentName" != ""} {
        XFMiscHandleHiding procs .xfInfoProc.procs.procs $xfCurrentName
      }}

  button .xfInfoProc.frame1.clear \
    -text {Clear} \
    -command {
      .xfInfoProc.current.current delete 0 end
      catch "XFMiscSetText .xfInfoProc.frame4.args.args \"\""
      catch "XFMiscSetText .xfInfoProc.frame4.text.text \"\""}

  button .xfInfoProc.frame1.help \
    -text {Help} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoProc.current.current get]" != ""} {
        set xfCurrentName [.xfInfoProc.current.current get]
      } {
        if {"[.xfInfoProc.procs.procs curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoProc.procs.procs get [.xfInfoProc.procs.procs curselect]]
        }
      }
      XFProcHelpHelp * commands $xfCurrentName}

  XFTmpltFrame .xfInfoProc.frame2 0

  radiobutton .xfInfoProc.frame2.include \
    -text {Include} \
    -value {Include} \
    -command {
      global xfStatus
      set xfStatus(includeExclude) 1
      set xfStatus(includeExcludeString) \
        [.xfInfoProc.pattern.pattern get]
      XFMiscSetInfo procs .xfInfoProc.procs.procs 1}

  radiobutton .xfInfoProc.frame2.exclude \
    -text {Exclude} \
    -value {Exclude} \
    -command {
      global xfStatus
      set xfStatus(includeExclude) 0
      set xfStatus(includeExcludeString) \
        [.xfInfoProc.pattern.pattern get]
      XFMiscSetInfo procs .xfInfoProc.procs.procs 1}

  if {$xfStatus(includeExclude)} {
    .xfInfoProc.frame2.include select
  } {
    .xfInfoProc.frame2.exclude select
  }

  XFTmpltFrame .xfInfoProc.frame3 0

  button .xfInfoProc.frame3.ok \
    -text {OK} \
    -command "
      XFInfoProceduresTest \"$xfTarget\""

  checkbutton .xfInfoProc.frame3.hiddens \
    -command {
      global xfStatus
      if {$xfStatus(hiddenProcs)} {
        .xfInfoProc.frame1.hide config -text "Unhide"
        .xfInfoProc.frame1.add config -state disabled
        .xfInfoProc.frame1.edit config -state disabled
        .xfInfoProc.frame1.rename config -state disabled
        .xfInfoProc.frame1.remove config -state disabled
        .xfInfoProc.frame1.clear config -state disabled
        .xfInfoProc.frame1.help config -state disabled
        XFMiscSetInfo hiddenprocs .xfInfoProc.procs.procs 1
      } {
        .xfInfoProc.frame1.hide config -text "Hide"
        .xfInfoProc.frame1.add config -state normal
        .xfInfoProc.frame1.edit config -state normal
        .xfInfoProc.frame1.rename config -state normal
        .xfInfoProc.frame1.remove config -state normal
        .xfInfoProc.frame1.clear config -state normal
        .xfInfoProc.frame1.help config -state normal
        XFMiscSetInfo procs .xfInfoProc.procs.procs 1
      }
      .xfInfoProc.procs.procs select anchor 0
      .xfInfoProc.procs.procs select set 0
      XFMiscSetText .xfInfoProc.current.current ""
      if {"[info commands .xfInfoProc.frame4.args.args]" != ""} {
        XFMiscSetText .xfInfoProc.frame4.args.args ""
        XFMiscSetText .xfInfoProc.frame4.text.text ""
      }} \
    -text {Show hidden} \
    -variable xfStatus(hiddenProcs) \
    -onvalue 1 \
    -offvalue 0

  button .xfInfoProc.frame3.rescan \
    -text {Rescan} \
    -command "XFMiscSetInfo procs .xfInfoProc.procs.procs 1"

  checkbutton .xfInfoProc.frame3.rescanperm \
    -text {Rescan permanently} \
    -variable xfStatus(rescanInfo) \
    -onvalue 1 \
    -offvalue 0

  XFTmpltListbox .xfInfoProc procs

  XFTmpltLabledEntry .xfInfoProc current {Name:} {}

  if {"$xfConf(externalEditor)" == ""} {
    XFTmpltFrame .xfInfoProc.frame4 0

    XFTmpltTextLong .xfInfoProc.frame4 text 0 "" 0
    XFMiscSetTextHeight .xfInfoProc.frame4.text.text 10

    XFTmpltLabledEntry .xfInfoProc.frame4 args {Parameters:} {}

    pack append .xfInfoProc.frame4 \
                .xfInfoProc.frame4.args {top frame center fill} \
                .xfInfoProc.frame4.text {top frame center fill expand}
  }

  XFTmpltLabledEntry .xfInfoProc pattern {Pattern:} \
    $xfStatus(includeExcludeString)

  if {"$xfSelectProc" != ""} {
    if {"[info procs $xfSelectProc]" == ""} {
      catch "proc $xfSelectProc {} {}"
    }
  }

  XFMiscSetInfo procs .xfInfoProc.procs.procs 1

  if {"$xfSelectProc" == ""} {
    catch ".xfInfoProc.procs.procs get 0" xfSelectProc
  } {
    set xfCounter 0
    set xfLast [.xfInfoProc.procs.procs size]
    if {"$xfLast" == "none"} {
      set xfLast -1
    }
    while {$xfCounter < $xfLast} {
      if {"$xfSelectProc" == "[.xfInfoProc.procs.procs get $xfCounter]"} {
        .xfInfoProc.procs.procs select anchor $xfCounter
        .xfInfoProc.procs.procs select set $xfCounter
        break
      }
      incr xfCounter
    }
  }

  XFMiscSetText .xfInfoProc.current.current $xfSelectProc

  # bindings
  if {"$xfTarget" != ""} {
    bind .xfInfoProc.procs.procs $xfBind(select1) "
      if {\[%W size\] > 0} {
        XFBindSelectOneIntoEntry %W %y .xfInfoProc.current.current
        XFMiscInsertTextIntoWidget \"$xfTargetP\" \
          \[.xfInfoProc.current.current get\]
        destroy .xfInfoProc
      }"
  }

  if {"$xfConf(externalEditor)" == ""} {
    # set text fields
    set xfArguments ""
    set xfBodyList ""
    if {"$xfSelectProc" != "" && "[info proc $xfSelectProc]" != ""} {
      set xfBodyList [string trimright [info body $xfSelectProc]]
      set xfArgList [info args $xfSelectProc]
      foreach xfCounter $xfArgList {
        if {[info default $xfSelectProc $xfCounter xfDefault]} {
          append xfArguments " \{$xfCounter \"$xfDefault\"\}"
        } {
          append xfArguments " $xfCounter"
        }
      }
    }
    XFMiscSetText .xfInfoProc.frame4.args.args $xfArguments
    if {[string index $xfBodyList 0] == "\n"} {
      XFMiscSetText .xfInfoProc.frame4.text.text \
        [string range $xfBodyList 1 end]
    } {
      XFMiscSetText .xfInfoProc.frame4.text.text $xfBodyList
    }
  }

  bind .xfInfoProc.pattern.pattern <Return> {
    global xfStatus
    set xfStatus(includeExcludeString) \
      [.xfInfoProc.pattern.pattern get]
    XFMiscSetInfo procs .xfInfoProc.procs.procs 1}

  bind .xfInfoProc.procs.procs <ButtonPress-1> {
    XFInfoProceduresSelect %W %y}
  bind .xfInfoProc.procs.procs <Button1-Motion> {
    XFInfoProceduresSelect %W %y}
  bind .xfInfoProc.procs.procs <Shift-Button1-Motion> {
    XFInfoProceduresSelect %W %y}
  bind .xfInfoProc.procs.procs <Shift-ButtonPress-1> {
    XFInfoProceduresSelect %W %y}

  # packing
  pack append .xfInfoProc.framefind \
              .xfInfoProc.framefind.findtext left \
              .xfInfoProc.framefind.string {left fillx expand} \
              .xfInfoProc.framefind.back {left padx 4} \
              .xfInfoProc.framefind.fwd {left}
  pack append .xfInfoProc.frame1 \
              .xfInfoProc.frame1.add {left fill expand} \
              .xfInfoProc.frame1.edit {left fill expand} \
              .xfInfoProc.frame1.rename {left fill expand} \
              .xfInfoProc.frame1.remove {left fill expand} \
              .xfInfoProc.frame1.hide {left fill expand} \
              .xfInfoProc.frame1.clear {left fill expand} \
              .xfInfoProc.frame1.help {left fill expand}
  pack append .xfInfoProc.frame2 \
              .xfInfoProc.frame2.include {left fill expand} \
              .xfInfoProc.frame2.exclude {left fill expand}
  pack append .xfInfoProc.frame3 \
              .xfInfoProc.frame3.ok {left fill expand} \
              .xfInfoProc.frame3.hiddens {left fill expand} \
              .xfInfoProc.frame3.rescan {left fill expand} \
              .xfInfoProc.frame3.rescanperm {left fill expand}
  if {"$xfConf(externalEditor)" == ""} {
    pack append .xfInfoProc \
                .xfInfoProc.frame3 {bottom fill} \
                .xfInfoProc.frame2 {bottom fill} \
                .xfInfoProc.pattern {bottom fill} \
                .xfInfoProc.frame1 {bottom fill} \
                .xfInfoProc.framefind {bottom fillx} \
                .xfInfoProc.frame4 {bottom fill expand} \
                .xfInfoProc.current {bottom fill} \
                .xfInfoProc.procs {top fill}

    XFBindFormConnect {.xfInfoProc.current .xfInfoProc.frame4.args .xfInfoProc.frame4.text}
  } {
    pack append .xfInfoProc \
                .xfInfoProc.frame3 {bottom fill} \
                .xfInfoProc.frame2 {bottom fill} \
                .xfInfoProc.pattern {bottom fill} \
                .xfInfoProc.frame1 {bottom fill} \
                .xfInfoProc.framefind {bottom fillx} \
                .xfInfoProc.current {bottom fill} \
                .xfInfoProc.procs {top fill}

    XFBindFormConnect {.xfInfoProc.current}
  }
  XFEditSetStatus "Calling procedure list...done"
}

##########
# Procedure: XFInfoProceduresSelect
# Description: select a procedure
# Arguments: xfProcName - the procedure name
# Returns: none
# Sideeffects: none
##########
proc XFInfoProceduresSelect {xfW xfY} {
  global xfConf
  global xfStatus

  if {[$xfW size] > 0} {
    XFBindSelectOneIntoEntry $xfW $xfY .xfInfoProc.current.current
    set xfStatus(procIndex) [$xfW nearest $xfY]
    set xfStatus(procName) [.xfInfoProc.current.current get]
    if {"$xfConf(externalEditor)" == ""} {
      # set text fields
      set xfSelectProc $xfStatus(procName)
      set xfArguments ""
      set xfBodyList ""
      if {"$xfSelectProc" != "" && "[info proc $xfSelectProc]" != ""} {
        set xfBodyList [string trimright [info body $xfSelectProc]]
        set xfArgList [info args $xfSelectProc]
        foreach xfCounter $xfArgList {
          if {[info default $xfSelectProc $xfCounter xfDefault]} {
            append xfArguments " \{$xfCounter \"$xfDefault\"\}"
          } {
            append xfArguments " $xfCounter"
          }
        }
      }
      XFMiscSetText .xfInfoProc.frame4.args.args $xfArguments
      if {[string index $xfBodyList 0] == "\n"} {
        XFMiscSetText .xfInfoProc.frame4.text.text \
          [string range $xfBodyList 1 end]
      } {
        XFMiscSetText .xfInfoProc.frame4.text.text $xfBodyList
      }
    }
  }
}

##########
# Procedure: XFInfoProceduresSet
# Description: insert the current procedure
# Arguments: xfProcName - the procedure name
# Returns: none
# Sideeffects: none
##########
proc XFInfoProceduresSet {xfProcName} {
  global xfConf

  if {"$xfConf(externalEditor)" == ""} {
    set xfArgs [.xfInfoProc.frame4.args.args get]
    set xfBody [XFMiscGetText .xfInfoProc.frame4.text.text]
    if {"$xfProcName" != ""} {
      if {[catch "proc $xfProcName {$xfArgs} {\n$xfBody}" xfResult]} {
        XFProcError "$xfResult"
      } {
        XFMiscSetInfo procs .xfInfoProc.procs.procs 0
      }
    } {
      XFMiscSetInfo procs .xfInfoProc.procs.procs 0
    }
  } {
    # call external editor
    XFMiscCallExternalEditor procs $xfProcName
  }
}

##########
# Procedure: XFInfoProceduresTest
# Description: test the current procedure
# Arguments: xfTarget - put current selection to this entry
# Returns: none
# Sideeffects: none
##########
proc XFInfoProceduresTest {xfTarget} {

  set xfProcName [.xfInfoProc.current.current get]

  if {"$xfProcName" != "" &&
      "[info commands .xfInfoProc.frame4.args.args]" != "" &&
      "[info commands .xfInfoProc.frame4.text.text]" != ""} {
    set xfArgs [string trim [.xfInfoProc.frame4.args.args get]]
    set xfBody [string trim [XFMiscGetText .xfInfoProc.frame4.text.text]]
    set xfArgList [info args $xfProcName]
    set xfCurArgs ""
    foreach xfCounter $xfArgList {
      if {[info default $xfProcName $xfCounter xfDefault]} {
        append xfCurArgs " \{$xfCounter \"$xfDefault\"\}"
      } {
        append xfCurArgs " $xfCounter"
      }
    }
    if {"[info procs $xfProcName]" != ""} {
      if {"[string trim $xfCurArgs]" != "$xfArgs" ||
          "[string trim [info body $xfProcName]]" != "$xfBody"} {
        if {[XFProcYesNo "Modify the currently edited procedure ?"]} {
          XFInfoProceduresSet $xfProcName
        }
      }
    } { 
      if {"$xfBody" != ""} {
        if {[XFProcYesNo "Insert the currently edited procedure ?"]} {
          XFInfoProceduresSet $xfProcName
        }
      }
    }
  }

  if {"$xfTarget" != ""} {
    XFMiscInsertTextIntoWidget $xfTarget [.xfInfoProc.current.current get]
  }
  destroy .xfInfoProc
}

# eof

