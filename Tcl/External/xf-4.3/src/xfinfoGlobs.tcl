# Program: xf
# Description: info routine for global variables
#
# $Header: xfinfoGlobs.tcl[2.5] Wed Mar 10 12:06:12 1993 garfield@garfield frozen $

##########
# Procedure: XFInfoGlobals
# Description: show the global variables
# Arguments: xfTarget - put current selection to this entry
# Returns: none
# Sideeffects: none
##########
proc XFInfoGlobals {xfTarget} {
  global xfBind
  global xfConf
  global xfStatus

  XFEditSetStatus "Calling variable list..."

  # building widget structure
  XFTmpltToplevel .xfInfoGlobal 400x400 {XF global variables}

  XFTmpltFrame .xfInfoGlobal.frame1 0

  button .xfInfoGlobal.frame1.insert \
    -text {Insert} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoGlobal.current.current get]" != ""} {
        set xfCurrentName [.xfInfoGlobal.current.current get]
      } {
        if {"[.xfInfoGlobal.globals.globals curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoGlobal.globals.globals get [.xfInfoGlobal.globals.globals curselect]]
        }
      }
      if {"$xfCurrentName" != ""} {
        XFInfoGlobalsSet $xfCurrentName
      }}

  button .xfInfoGlobal.frame1.rename \
    -text {Rename} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoGlobal.current.current get]" != ""} {
        set xfCurrentName [.xfInfoGlobal.current.current get]
      } {
        if {"[.xfInfoGlobal.globals.globals curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoGlobal.globals.globals get [.xfInfoGlobal.globals.globals curselect]]
        }
      }
      set xfNewName [XFProcInputBoxOne "New variable name:" 300x100 "XF new variable name"]
      if {"$xfCurrentName" != "" && "$xfNewName" != "" &&
          "$xfCurrentName" != "$xfNewName"} {
        if {[string first "(" $xfNewName] > 0} {
          set xfTmpNewName [string range $xfNewName 0 [expr [string first "(" $xfNewName]-1]]
        } {
          set xfTmpNewName $xfNewName
        }
        if {[string first "(" $xfCurrentName] > 0} {
          set xfTmpCurrentName [string range $xfCurrentName 0 [expr [string first "(" $xfCurrentName]-1]]
        } {
          set xfTmpCurrentName $xfCurrentName
        }
        if {"[info globals $xfTmpCurrentName]" != ""} {
          global $xfTmpNewName
          global $xfTmpCurrentName
          set $xfNewName [set $xfCurrentName]
          .xfInfoGlobal.current.current delete 0 end
          XFMiscSetText .xfInfoGlobal.text2.text2
          catch "unset $xfCurrentName"
        }
        unset xfNewName
        unset xfTmpNewName
        unset xfCurrentName
        unset xfTmpCurrentName
        XFMiscSetInfo globals .xfInfoGlobal.globals.globals 1
      } {
        unset xfNewName
        unset xfCurrentName
      }}

  button .xfInfoGlobal.frame1.remove \
    -text {Remove} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoGlobal.current.current get]" != ""} {
        set xfCurrentName [.xfInfoGlobal.current.current get]
      } {
        if {"[.xfInfoGlobal.globals.globals curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoGlobal.globals.globals get [.xfInfoGlobal.globals.globals curselect]]
        }
      }
      if {"$xfCurrentName" != ""} {
        catch "unset $xfCurrentName"
        .xfInfoGlobal.current.current delete 0 end
        XFMiscSetText .xfInfoGlobal.text2.text2
        XFMiscSetInfo globals .xfInfoGlobal.globals.globals 1
      }}

  button .xfInfoGlobal.frame1.clear \
    -text {Clear} \
    -command {
      .xfInfoGlobal.current.current delete 0 end
      XFMiscSetText .xfInfoGlobal.text2.text2}

  XFTmpltFrame .xfInfoGlobal.frame2 0

  radiobutton .xfInfoGlobal.frame2.include \
    -text {Include} \
    -value {Include} \
    -command {
      global xfStatus
      set xfStatus(includeExclude) 1
      set xfStatus(includeExcludeString) \
        [.xfInfoGlobal.pattern.pattern get]
      XFMiscSetInfo globals .xfInfoGlobal.globals.globals 1}

  radiobutton .xfInfoGlobal.frame2.exclude \
    -text {Exclude} \
    -value {Exclude} \
    -command {
      global xfStatus
      set xfStatus(includeExclude) 0
      set xfStatus(includeExcludeString) \
        [.xfInfoGlobal.pattern.pattern get]
      XFMiscSetInfo globals .xfInfoGlobal.globals.globals 1}

  if {$xfStatus(includeExclude)} {
    .xfInfoGlobal.frame2.include select
  } {
    .xfInfoGlobal.frame2.exclude select
  }

  XFTmpltFrame .xfInfoGlobal.frame3 0

  button .xfInfoGlobal.frame3.ok \
    -text {OK} \
    -command "
      XFInfoGlobalsTest \"$xfTarget\""

  button .xfInfoGlobal.frame3.rescan \
    -text {Rescan} \
    -command "XFMiscSetInfo globals .xfInfoGlobal.globals.globals 1"

  checkbutton .xfInfoGlobal.frame3.rescanperm \
    -text {Rescan permanently} \
    -variable xfConf(rescanInfo) \
    -onvalue 1 \
    -offvalue 0

  XFTmpltListbox .xfInfoGlobal globals

  XFTmpltLabledEntry .xfInfoGlobal current {Name:} {}

  XFTmpltText .xfInfoGlobal text2 0
  XFMiscSetTextHeight .xfInfoGlobal.text2.text2 10

  XFTmpltLabledEntry .xfInfoGlobal pattern {Pattern:} \
    $xfStatus(includeExcludeString)

  XFMiscSetInfo globals .xfInfoGlobal.globals.globals 1

  catch ".xfInfoGlobal.globals.globals get 0" xfSelectGlob
  if {"$xfSelectGlob" != ""} {
    .xfInfoGlobal.current.current insert 0 $xfSelectGlob
    set tmpPos [string first "(" $xfSelectGlob]
    if {$tmpPos != -1} {
      global [string range $xfSelectGlob 0 [expr $tmpPos-1]]
      XFMiscSetTextIntoWidget .xfInfoGlobal.text2.text2 \
        [set $xfSelectGlob]
    } {
      global $xfSelectGlob
      XFMiscSetTextIntoWidget .xfInfoGlobal.text2.text2 \
        [set $xfSelectGlob]
    }
    .xfInfoGlobal.globals.globals select anchor 0
    .xfInfoGlobal.globals.globals select set 0
  }

  # bindings
  if {"$xfTarget" != ""} {
    bind .xfInfoGlobal.globals.globals $xfBind(select1) "
      if {\[%W size\] > 0} {
        XFBindSelectOneIntoEntry %W %y .xfInfoGlobal.current.current
        XFMiscInsertTextIntoWidget \"$xfTarget\" \
          \[.xfInfoGlobal.current.current get\]
        destroy .xfInfoGlobal
      }"
  }

  bind .xfInfoGlobal.pattern.pattern <Return> {
    global xfStatus
    set xfStatus(includeExcludeString) \
      [.xfInfoGlobal.pattern.pattern get]
    XFMiscSetInfo globals .xfInfoGlobal.globals.globals 1}

  bind .xfInfoGlobal.globals.globals <ButtonPress-1> {
    global xfStatus
    if {[%W size] > 0} {
      XFBindSelectOneIntoEntry %W %y .xfInfoGlobal.current.current
      XFMiscSetTextIntoWidget .xfInfoGlobal.text2.text2 \
        [set [.xfInfoGlobal.current.current get]]
      set xfStatus(globalIndex) [%W nearest %y]
      set xfStatus(globalName) [.xfInfoGlobal.current.current get]}}
  bind .xfInfoGlobal.globals.globals <Button1-Motion> {
    global xfStatus
    if {[%W size] > 0} {
      XFBindSelectOneIntoEntry %W %y .xfInfoGlobal.current.current
      XFMiscSetTextIntoWidget .xfInfoGlobal.text2.text2 \
        [set [.xfInfoGlobal.current.current get]]
      set xfStatus(globalIndex) [%W nearest %y]
      set xfStatus(globalName) [.xfInfoGlobal.current.current get]}}
  bind .xfInfoGlobal.globals.globals <Shift-Button1-Motion> {
    global xfStatus
    if {[%W size] > 0} {
      XFBindSelectOneIntoEntry %W %y .xfInfoGlobal.current.current
      XFMiscSetTextIntoWidget .xfInfoGlobal.text2.text2 \
        [set [.xfInfoGlobal.current.current get]]
      set xfStatus(globalIndex) [%W nearest %y]
      set xfStatus(globalName) [.xfInfoGlobal.current.current get]}}
  bind .xfInfoGlobal.globals.globals <Shift-ButtonPress-1> {
    global xfStatus
    if {[%W size] > 0} {
      XFBindSelectOneIntoEntry %W %y .xfInfoGlobal.current.current
      XFMiscSetTextIntoWidget .xfInfoGlobal.text2.text2 \
        [set [.xfInfoGlobal.current.current get]]
      set xfStatus(globalIndex) [%W nearest %y]
      set xfStatus(globalName) [.xfInfoGlobal.current.current get]}}

  # packing
  pack append .xfInfoGlobal.frame1 \
              .xfInfoGlobal.frame1.insert {left fill expand} \
              .xfInfoGlobal.frame1.rename {left fill expand} \
              .xfInfoGlobal.frame1.remove {left fill expand} \
              .xfInfoGlobal.frame1.clear {left fill expand}
  pack append .xfInfoGlobal.frame2 \
              .xfInfoGlobal.frame2.include {left fill expand} \
              .xfInfoGlobal.frame2.exclude {left fill expand}
  pack append .xfInfoGlobal.frame3 \
              .xfInfoGlobal.frame3.ok {left fill expand} \
              .xfInfoGlobal.frame3.rescan {left fill expand} \
              .xfInfoGlobal.frame3.rescanperm {left fill expand}
  pack append .xfInfoGlobal \
              .xfInfoGlobal.frame3 {bottom fill} \
              .xfInfoGlobal.frame2 {bottom fill} \
              .xfInfoGlobal.pattern {bottom fill} \
              .xfInfoGlobal.frame1 {bottom fill} \
              .xfInfoGlobal.text2 {bottom fill expand} \
              .xfInfoGlobal.current {bottom fill} \
              .xfInfoGlobal.globals {top expand fill}

  XFBindFormConnect {.xfInfoGlobal.current .xfInfoGlobal.text2}

  XFEditSetStatus "Calling variable list...done"
}

##########
# Procedure: XFInfoGlobalsSet
# Description: set the value of the current variable
# Arguments: xfVarName - the variable name
# Returns: none
# Sideeffects: none
##########
proc XFInfoGlobalsSet {xfVarName} {

  set xfValue [XFMiscGetText .xfInfoGlobal.text2.text2]
  set xfTmpValue ""
  set xfCounter 0
  set xfTmpLength [string length $xfValue]
  while {$xfCounter < $xfTmpLength} {
    if {"[string index $xfValue $xfCounter]" == "\\"} {
      append xfTmpValue "\\\\\\\\\\\\"
    } {
      if {"[string index $xfValue $xfCounter]" == "\""} {
        append xfTmpValue "\\\\\\\""
      } {
        if {"[string index $xfValue $xfCounter]" == "\{"} {
          append xfTmpValue "\\\\\\\{"
        } {
          if {"[string index $xfValue $xfCounter]" == "\}"} {
            append xfTmpValue "\\\\\\\}"
          } {
            if {"[string index $xfValue $xfCounter]" == "\]"} {
              append xfTmpValue "\\\\\\\]"
            } {
              if {"[string index $xfValue $xfCounter]" == "\["} {
                append xfTmpValue "\\\\\\\["
              } {
                append xfTmpValue [string index $xfValue $xfCounter]
              }
            }
          }
        }
      }
    }
    incr xfCounter 1
  }
  set xfValue $xfTmpValue

  if {"$xfVarName" != ""} {
    set tmpPos [string first "(" $xfVarName]
    if {$tmpPos != -1} {
      set xfGlobVarName [string range $xfVarName 0 [expr $tmpPos-1]]
    } {
      set xfGlobVarName $xfVarName
    }
    if {[catch "global $xfGlobVarName; set \{$xfVarName\} \{$xfValue\}" xfResult]} {
      XFProcError "$xfResult"
    } {
      XFMiscSetInfo globals .xfInfoGlobal.globals.globals 0
    }
  } {
    XFMiscSetInfo globals .xfInfoGlobal.globals.globals 0
  }
}

##########
# Procedure: XFInfoGlobalsTest
# Description: test the current value
# Arguments: xfTarget - put current selection to this entry
# Returns: none
# Sideeffects: none
##########
proc XFInfoGlobalsTest {xfTarget} {

  set xfVarName ""
  if {"[.xfInfoGlobal.current.current get]" != ""} {
    set xfVarName [.xfInfoGlobal.current.current get]
  } {
    if {"[.xfInfoGlobal.globals.globals curselect]" != ""} {
      set xfVarName \
        [.xfInfoGlobal.globals.globals get [.xfInfoGlobal.globals.globals curselect]]
    }
  }

  if {"$xfVarName" != "" &&
      "[info commands .xfInfoGlobal.text2.text2]" != ""} {
    set xfValue [XFMiscGetText .xfInfoGlobal.text2.text2]
    if {"[info globals $xfVarName]" != ""} {
      set xfTmpValue ""
      set xfCounter 0
      set xfTmpLength [string length $xfValue]
      while {$xfCounter < $xfTmpLength} {
        if {"[string index $xfValue $xfCounter]" == "\\"} {
          append xfTmpValue "\\\\\\\\\\\\"
        } {
          if {"[string index $xfValue $xfCounter]" == "\""} {
            append xfTmpValue "\\\\\\\""
          } {
            if {"[string index $xfValue $xfCounter]" == "\{"} {
              append xfTmpValue "\\\\\\\{"
            } {
              if {"[string index $xfValue $xfCounter]" == "\}"} {
                append xfTmpValue "\\\\\\\}"
              } {
                if {"[string index $xfValue $xfCounter]" == "\]"} {
                  append xfTmpValue "\\\\\\\]"
                } {
                  if {"[string index $xfValue $xfCounter]" == "\["} {
                    append xfTmpValue "\\\\\\\["
                  } {
                    append xfTmpValue [string index $xfValue $xfCounter]
                  }
                }
              }
            }
          }
        }
        incr xfCounter 1
      }
      set xfValue $xfTmpValue
  
      global $xfVarName
      if {"[set $xfVarName]" != "$xfValue"} {
        if {[XFProcYesNo "Modify the currently edited variable ?"]} {
          XFInfoGlobalsSet $xfVarName
        }
      }
    } {
      if {"$xfValue" != ""} {
        if {[XFProcYesNo "Insert the currently edited variable ?"]} {
          XFInfoGlobalsSet $xfVarName
        }
      }
    }
  }

  if {"$xfTarget" != ""} {
    XFMiscInsertTextIntoWidget $xfTarget [.xfInfoGlobal.current.current get]
  }
  destroy .xfInfoGlobal
}

# eof

