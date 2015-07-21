# Program: xf
# Description: info routine for commands
#
# $Header: xfinfoCmds.tcl[2.5] Wed Mar 10 12:06:07 1993 garfield@garfield frozen $

##########
# Procedure: XFInfoCommands
# Description: show the selected information
# Arguments: xfTarget - put current selection to this entry
# Returns: none
# Sideeffects: none
##########
proc XFInfoCommands {xfTarget} {
  global xfBind
  global xfConf
  global xfStatus

  XFEditSetStatus "Calling command list..."

  # build widget structure
  XFTmpltToplevel .xfInfoCmd 430x500 {XF commands}

  XFTmpltFrame .xfInfoCmd.frame1 0

  button .xfInfoCmd.frame1.add \
    -text {Insert} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoCmd.current.current get]" != ""} {
        set xfCurrentName [.xfInfoCmd.current.current get]
      }
      if {"$xfCurrentName" != ""} {
        if {"[info procs $xfCurrentName]" != ""} {
          if {![XFMiscCorrectLevel procshow [info body $xfCurrentName]]} {
            XFProcError "Invisible procedures with that name exists"
            return
          }
        }
        if {"[info procs $xfCurrentName]" == "" &&
            "[info commands $xfCurrentName]" != ""} {
          if {[XFProcYesNo "Really modify a build in command ?"]} {
            XFInfoCommandsSet $xfCurrentName
          }
        } {
          XFInfoCommandsSet $xfCurrentName
        }
      }}

  button .xfInfoCmd.frame1.edit \
    -text {Edit} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoCmd.current.current get]" != ""} {
        set xfCurrentName [.xfInfoCmd.current.current get]
      } {
        if {"[.xfInfoCmd.commands.commands curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoCmd.commands.commands get [.xfInfoCmd.commands.commands curselect]]
        }
      }
      if {"$xfCurrentName" != ""} {
        if {"[info procs $xfCurrentName]" != ""} {
          if {![XFMiscCorrectLevel procshow [info body $xfCurrentName]]} {
            XFProcError "Invisible procedures cannot be edited"
            return
          }
        }
        if {"[info procs $xfCurrentName]" == "" &&
            "[info commands $xfCurrentName]" != ""} {
          if {[XFProcYesNo "Really modify a build in command ?"]} {
            XFInfoCommandsSet $xfCurrentName
          }
        } {
          XFInfoCommandsSet $xfCurrentName
        }
      }}

  button .xfInfoCmd.frame1.rename \
    -text {Rename} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoCmd.current.current get]" != ""} {
        set xfCurrentName [.xfInfoCmd.current.current get]
      } {
        if {"[.xfInfoCmd.commands.commands curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoCmd.commands.commands get [.xfInfoCmd.commands.commands curselect]]
        }
      }
      set xfNewName [XFProcInputBoxOne "New procedure name:" 300x100 "XF new procedure name"]
      if {"$xfCurrentName" != "" && "$xfNewName" != "" &&
          "$xfCurrentName" != "$xfNewName" &&
          "[info commands $xfCurrentName]" != ""} {
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
        if {"[info procs $xfCurrentName]" == ""} {
          if {[XFProcYesNo "Really rename a build in command ?"]} {
            catch "rename $xfCurrentName $xfNewName"
            .xfInfoCmd.current.current delete 0 end
            catch "XFMiscSetText .xfInfoCmd.frame4.args.args \"\""
            catch "XFMiscSetText .xfInfoCmd.frame4.text.text \"\""
            XFMiscSetInfo commands .xfInfoCmd.commands.commands 1
          }
        } {
          catch "rename $xfCurrentName $xfNewName"
          .xfInfoCmd.current.current delete 0 end
          catch "XFMiscSetText .xfInfoCmd.frame4.args.args \"\""
          catch "XFMiscSetText .xfInfoCmd.frame4.text.text \"\""
          XFMiscSetInfo commands .xfInfoCmd.commands.commands 1
        }
      }}

  button .xfInfoCmd.frame1.remove \
    -text {Remove} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoCmd.current.current get]" != ""} {
        set xfCurrentName [.xfInfoCmd.current.current get]
      } {
        if {"[.xfInfoCmd.commands.commands curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoCmd.commands.commands get [.xfInfoCmd.commands.commands curselect]]
        }
      }
      if {"$xfCurrentName" != "" &&
          "[info commands $xfCurrentName]" != ""} {
        if {"[info procs $xfCurrentName]" != ""} {
          if {![XFMiscCorrectLevel procshow [info body $xfCurrentName]]} {
            XFProcError "Invisible procedures cannot be removed"
            return
          }
        }
        if {"[info procs $xfCurrentName]" == ""} {
          if {[XFProcYesNo "Really remove a build in command ?"]} {
            catch "rename $xfCurrentName {}"
            .xfInfoCmd.current.current delete 0 end
            catch "XFMiscSetText .xfInfoCmd.frame4.args.args \"\""
            catch "XFMiscSetText .xfInfoCmd.frame4.text.text \"\""
            XFMiscSetInfo commands .xfInfoCmd.commands.commands 1
          }
        } {
          catch "rename $xfCurrentName {}"
          .xfInfoCmd.current.current delete 0 end
          catch "XFMiscSetText .xfInfoCmd.frame4.args.args \"\""
          catch "XFMiscSetText .xfInfoCmd.frame4.text.text \"\""
          XFMiscSetInfo commands .xfInfoCmd.commands.commands 1
        }
      }}

  button .xfInfoCmd.frame1.hide \
    -text {Hide} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoCmd.current.current get]" != ""} {
        set xfCurrentName [.xfInfoCmd.current.current get]
      } {
        if {"[.xfInfoCmd.commands.commands curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoCmd.commands.commands get [.xfInfoCmd.commands.commands curselect]]
        }
      }
      if {"$xfCurrentName" != ""} {
        XFMiscHandleHiding commands .xfInfoCmd.commands.commands $xfCurrentName
      }}

  button .xfInfoCmd.frame1.clear \
    -text {Clear} \
    -command {
      .xfInfoCmd.current.current delete 0 end
      catch "XFMiscSetText .xfInfoCmd.frame4.args.args \"\""
      catch "XFMiscSetText .xfInfoCmd.frame4.text.text \"\""}

  button .xfInfoCmd.frame1.help \
    -text {Help} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoCmd.current.current get]" != ""} {
        set xfCurrentName [.xfInfoCmd.current.current get]
      } {
        if {"[.xfInfoCmd.commands.commands curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoCmd.commands.commands get [.xfInfoCmd.commands.commands curselect]]
        }
      }
      XFProcHelpHelp * $xfCurrentName}

  XFTmpltFrame .xfInfoCmd.frame2 0

  radiobutton .xfInfoCmd.frame2.include \
    -text {Include} \
    -value {Include} \
    -command {
      global xfStatus
      set xfStatus(includeExclude) 1
      set xfStatus(includeExcludeString) \
        [.xfInfoCmd.pattern.pattern get]
      XFMiscSetInfo commands .xfInfoCmd.commands.commands 1}

  radiobutton .xfInfoCmd.frame2.exclude \
    -text {Exclude} \
    -value {Exclude} \
    -command {
      global xfStatus
      set xfStatus(includeExclude 0
      set xfStatus(includeExcludeString) \
        [.xfInfoCmd.pattern.pattern get]
      XFMiscSetInfo commands .xfInfoCmd.commands.commands 1}

  if {$xfStatus(includeExclude)} {
    .xfInfoCmd.frame2.include select
   } {
    .xfInfoCmd.frame2.exclude select
  }

  XFTmpltFrame .xfInfoCmd.frame3 0

  button .xfInfoCmd.frame3.ok \
    -text {OK} \
    -command "
      XFInfoCommandsTest \"$xfTarget\""

  checkbutton .xfInfoCmd.frame3.hiddens \
    -command {
      global xfStatus
      if {$xfStatus(hiddenProcs)} {
        .xfInfoCmd.frame1.hide config -text "Unhide"
        .xfInfoCmd.frame1.add config -state disabled
        .xfInfoCmd.frame1.edit config -state disabled
        .xfInfoCmd.frame1.rename config -state disabled
        .xfInfoCmd.frame1.remove config -state disabled
        .xfInfoCmd.frame1.clear config -state disabled
        .xfInfoCmd.frame1.help config -state disabled
        XFMiscSetInfo hiddencommands .xfInfoCmd.commands.commands 1
      } {
        .xfInfoCmd.frame1.hide config -text "Hide"
        .xfInfoCmd.frame1.add config -state normal
        .xfInfoCmd.frame1.edit config -state normal
        .xfInfoCmd.frame1.rename config -state normal
        .xfInfoCmd.frame1.remove config -state normal
        .xfInfoCmd.frame1.clear config -state normal
        .xfInfoCmd.frame1.help config -state normal
        XFMiscSetInfo commands .xfInfoCmd.commands.commands 1
      }
      .xfInfoCmd.commands.commands select anchor 0
      .xfInfoCmd.commands.commands select set 0
      XFMiscSetText .xfInfoCmd.current.current ""
      if {"[info commands .xfInfoCmd.frame4.args.args]" != ""} {
        XFMiscSetText .xfInfoCmd.frame4.args.args ""
        XFMiscSetText .xfInfoCmd.frame4.text.text ""
      }} \
    -text {Show hidden} \
    -variable xfStatus(hiddenProcs) \
    -onvalue 1 \
    -offvalue 0

  button .xfInfoCmd.frame3.rescan \
    -text {Rescan} \
    -command "XFMiscSetInfo commands .xfInfoCmd.commands.commands 1"

  checkbutton .xfInfoCmd.frame3.rescanperm \
    -text {Rescan permanently} \
    -variable xfStatus(rescanInfo) \
    -onvalue 1 \
    -offvalue 0

  XFTmpltListbox .xfInfoCmd commands

  XFTmpltLabledEntry .xfInfoCmd current {Name:} {}

  if {"$xfConf(externalEditor)" == ""} {
    XFTmpltFrame .xfInfoCmd.frame4 0

    XFTmpltTextLong .xfInfoCmd.frame4 text 0 "" 0
    XFMiscSetTextHeight .xfInfoCmd.frame4.text.text 10

    XFTmpltLabledEntry .xfInfoCmd.frame4 args {Parameters:} {}

    pack append .xfInfoCmd.frame4 \
                .xfInfoCmd.frame4.args {top frame center fill} \
                .xfInfoCmd.frame4.text {top frame center fill expand}
  }

  XFTmpltLabledEntry .xfInfoCmd pattern {Pattern:} \
    $xfStatus(includeExcludeString)

  XFMiscSetInfo commands .xfInfoCmd.commands.commands 1

  catch ".xfInfoCmd.commands.commands get 0" xfSelectProc

  XFMiscSetText .xfInfoCmd.current.current $xfSelectProc

  # bindings
  if {"$xfTarget" != ""} {
    bind .xfInfoCmd.commands.commands $xfBind(select1) "
      XFBindSelectOneIntoEntry %W %y .xfInfoCmd.current.current
      XFMiscInsertTextIntoWidget \"$xfTarget\" \
        \[.xfInfoCmd.current.current get\]
      destroy .xfInfoCmd"
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
    XFMiscSetText .xfInfoCmd.frame4.args.args $xfArguments
    if {[string index $xfBodyList 0] == "\n"} {
      XFMiscSetText .xfInfoCmd.frame4.text.text \
        [string range $xfBodyList 1 end]
    } {
      XFMiscSetText .xfInfoCmd.frame4.text.text $xfBodyList
    }
  }

  bind .xfInfoCmd.pattern.pattern <Return> {
    global xfStatus
    set xfStatus(includeExcludeString) \
      [.xfInfoCmd.pattern.pattern get]
    XFMiscSetInfo commands .xfInfoCmd.commands.commands 1}

  bind .xfInfoCmd.commands.commands <ButtonPress-1> {
    XFInfoCommandsSelect %W %y}
  bind .xfInfoCmd.commands.commands <Button1-Motion> {
    XFInfoCommandsSelect %W %y}
  bind .xfInfoCmd.commands.commands <Shift-Button1-Motion> {
    XFInfoCommandsSelect %W %y}
  bind .xfInfoCmd.commands.commands <Shift-ButtonPress-1> {
    XFInfoCommandsSelect %W %y}

  # packing
  pack append .xfInfoCmd.frame1 \
              .xfInfoCmd.frame1.add {left fill expand} \
              .xfInfoCmd.frame1.edit {left fill expand} \
              .xfInfoCmd.frame1.rename {left fill expand} \
              .xfInfoCmd.frame1.remove {left fill expand} \
              .xfInfoCmd.frame1.hide {left fill expand} \
              .xfInfoCmd.frame1.clear {left fill expand} \
              .xfInfoCmd.frame1.help {left fill expand}
  pack append .xfInfoCmd.frame2 \
              .xfInfoCmd.frame2.include {left fill expand} \
              .xfInfoCmd.frame2.exclude {left fill expand}
  pack append .xfInfoCmd.frame3 \
              .xfInfoCmd.frame3.ok {left fill expand} \
              .xfInfoCmd.frame3.hiddens {left fill expand} \
              .xfInfoCmd.frame3.rescan {left fill expand} \
              .xfInfoCmd.frame3.rescanperm {left fill expand}
  if {"$xfConf(externalEditor)" == ""} {
    pack append .xfInfoCmd \
                .xfInfoCmd.frame3 {bottom fill} \
                .xfInfoCmd.frame2 {bottom fill} \
                .xfInfoCmd.pattern {bottom fill} \
                .xfInfoCmd.frame1 {bottom fill} \
                .xfInfoCmd.frame4 {bottom fill} \
                .xfInfoCmd.current {bottom fill} \
                .xfInfoCmd.commands {left expand fill}

    XFBindFormConnect {.xfInfoCmd.current .xfInfoCmd.frame4.args .xfInfoCmd.frame4.text}
  } {
    pack append .xfInfoCmd \
                .xfInfoCmd.frame3 {bottom fill} \
                .xfInfoCmd.frame2 {bottom fill} \
                .xfInfoCmd.pattern {bottom fill} \
                .xfInfoCmd.frame1 {bottom fill} \
                .xfInfoCmd.current {bottom fill} \
                .xfInfoCmd.commands {left expand fill}

    XFBindFormConnect {.xfInfoCmd.current}
  }

  XFEditSetStatus "Calling command list...done"
}

##########
# Procedure: XFInfoCommandsSelect
# Description: select a command
# Arguments: xfCmdName - the command name
# Returns: none
# Sideeffects: none
##########
proc XFInfoCommandsSelect {xfW xfY} {
  global xfConf
  global xfStatus

  if {[$xfW size] > 0} {
    XFBindSelectOneIntoEntry $xfW $xfY .xfInfoCmd.current.current
    set xfStatus(cmdIndex) [$xfW nearest $xfY]
    set xfStatus(cmdName) [.xfInfoCmd.current.current get]
    if {"$xfConf(externalEditor)" == ""} {
      if {"[info procs $xfStatus(cmdName)]" != ""} {
        # set text fields
        set xfSelectProc $xfStatus(cmdName)
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
        XFMiscSetText .xfInfoCmd.frame4.args.args $xfArguments
        if {[string index $xfBodyList 0] == "\n"} {
          XFMiscSetText .xfInfoCmd.frame4.text.text \
            [string range $xfBodyList 1 end]
        } {
          XFMiscSetText .xfInfoCmd.frame4.text.text $xfBodyList
        }
      } {
        XFMiscSetText .xfInfoCmd.frame4.args.args ""
        XFMiscSetText .xfInfoCmd.frame4.text.text ""
      }
    }
  }
}

##########
# Procedure: XFInfoCommandsSet
# Description: insert the current command
# Arguments: xfCmdName - the command name
# Returns: none
# Sideeffects: none
##########
proc XFInfoCommandsSet {xfCmdName} {
  global xfConf

  if {"$xfConf(externalEditor)" == ""} {
    set xfArgs [.xfInfoCmd.frame4.args.args get]
    set xfBody [XFMiscGetText .xfInfoCmd.frame4.text.text]
    if {"$xfCmdName" != ""} {
      if {[catch "proc $xfCmdName {$xfArgs} {\n$xfBody}" xfResult]} {
        XFProcError "$xfResult"
      } {
        XFMiscSetInfo commands .xfInfoCmd.commands.commands 0
      }
    } {
      XFMiscSetInfo commands .xfInfoCmd.commands.commands 0
    }
  } {
    # call external editor
    XFMiscCallExternalEditor cmds $xfCmdName
  }
}

##########
# Procedure: XFInfoCommandsTest
# Description: test the current procedure
# Arguments: xfTarget - put current selection to this entry
# Returns: none
# Sideeffects: none
##########
proc XFInfoCommandsTest {xfTarget} {

  set xfCmdName [.xfInfoCmd.current.current get]

  if {"$xfCmdName" != "" &&
      "[info procs $xfCmdName]" != "" &&
      "[info commands .xfInfoCmd.frame4.args.args]" != "" &&
      "[info commands .xfInfoCmd.frame4.text.text]" != ""} {
    set xfArgs [string trim [.xfInfoCmd.frame4.args.args get]]
    set xfBody [string trim [XFMiscGetText .xfInfoCmd.frame4.text.text]]
    set xfArgList [info args $xfCmdName]
    set xfCurArgs ""
    foreach xfCounter $xfArgList {
      if {[info default $xfCmdName $xfCounter xfDefault]} {
        append xfCurArgs " \{$xfCounter \"$xfDefault\"\}"
      } { 
        append xfCurArgs " $xfCounter"
      }
    }
    if {"[info procs $xfCmdName]" != ""} {
      if {"[string trim $xfCurArgs]" != "$xfArgs" ||
          "[string trim [info body $xfCmdName]]" != "$xfBody"} {
        if {[XFProcYesNo "Modify the currently edited command ?"]} {
          XFInfoCommandsSet $xfCmdName
        }
      }
    } { 
      if {"$xfBody" != ""} {
        if {[XFProcYesNo "Insert the currently edited command ?"]} {
          XFInfoCommandsSet $xfCmdName
        }
      }
    }
  }

  if {"$xfTarget" != ""} {
    XFMiscInsertTextIntoWidget $xfTarget [.xfInfoCmd.current.current get]
  }
  destroy .xfInfoCmd
}

# eof

