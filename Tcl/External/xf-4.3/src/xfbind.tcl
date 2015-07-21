# Program: xf
# Description: bindings
#
# $Header: xfbind.tcl[2.3] Wed Mar 10 12:05:23 1993 garfield@garfield frozen $

##########
# Procedure: XFBindRemoveName
# Description: action to remove the displayed widget name
# Arguments: xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFBindRemoveName {} {

  catch "destroy .xfShowName"
}

##########
# Procedure: XFBindFormConnect
# Description: connect all packed entry/text childs
# Arguments: xfWList - the widgetlist
#            {xfCommand} - an optional command to bind to <Return>
#            {xfInternal} - an internal flag
# Returns: none
# Sideeffects: none
##########
proc XFBindFormConnect {xfWList {xfCommand ""} {xfInitial 1}} {

  set xfType 0
  set xfPackList ""
  foreach xfW $xfWList {
    if {("[winfo class $xfW]" == "Entry" ||
         "[winfo class $xfW]" == "Text" ||
         "[winfo class $xfW]" == "TkEmacs")} {
      lappend xfPackList $xfW
    }
    foreach xfElement [pack slave $xfW] {
      if {$xfType == 0} {
        if {("[winfo class $xfElement]" == "Entry" ||
             "[winfo class $xfElement]" == "Text" ||
             "[winfo class $xfElement]" == "TkEmacs")} {
          eval lappend xfPackList $xfElement
        }
        if {"[pack slave $xfElement]" != ""} {
          if {"[info procs XFBindFormConnect]" != ""} {
            set xfTmpPackList [XFBindFormConnect $xfElement $xfCommand 0]
          } {
            if {"[info procs XFLocalFormConnect]" != ""} {
              set xfTmpPackList [XFLocalFormConnect $xfElement $xfCommand 0]
            } {
              set xfTmpPackList ""
            }
          }
          if {"$xfTmpPackList" != ""} {
            eval lappend xfPackList $xfTmpPackList
          }
        }
        set xfType 1
      } {
        set xfType 0
      }
    }
  }
  if {$xfInitial} {
    set xfFromWidget ""
    set xfToWidget ""
    set xfCounter 1
    foreach xfElement $xfPackList {
      set xfToWidget [lindex $xfPackList $xfCounter]
      if {"[info procs XFBindForms]" != ""} {
        XFBindForms $xfElement $xfFromWidget $xfToWidget $xfCommand
      } {
        if {"[info procs XFLocalFormBind]" != ""} {
          XFLocalFormBind $xfElement $xfFromWidget $xfToWidget $xfCommand
        }
      }
      set xfFromWidget $xfElement
      incr xfCounter
    }
  } {
    return $xfPackList
  }
}

##########
# Procedure: XFBindFocusIn
# Description: focus on text
# Arguments: xfW - the widget
# Returns: none
# Sideeffects: none
##########
proc XFBindFocusIn {xfW} {

  if {"[winfo class $xfW]" == "Entry"} {
    $xfW icursor 0
  } {
    if {"[winfo class $xfW]" == "Text"} {
      $xfW mark set insert 1.0
    } {
      if {"[winfo class $xfW]" == "TkEmacs"} {
        $xfW mark set insert 0.0
      }
    }
  }
  focus $xfW
}

##########
# Procedure: XFBindForms
# Description: specify bindings for form style handling of text fields
# Arguments: xfW - the widget
#            xfPrevW - the previous widget
#            xfNextW - the next widget
#            {xfCommand} - a command to bind to <Return>
# Returns: none
# Sideeffects: none
##########
proc XFBindForms {xfW xfPrevW xfNextW {xfCommand ""}} {

  if {"[info procs XFBindFocusIn]" != ""} {
    set xfFocusCmd XFBindFocusIn
  } {
    if {"[info procs XFLocalFocusIn]" != ""} {
      set xfFocusCmd XFLocalFocusIn
    } {
      return
    }
  }

  if {"$xfPrevW" != ""} {
    if {"[winfo class $xfW]" == "Entry"} {
      bind $xfW <Up> "$xfFocusCmd $xfPrevW"
      bind $xfW <Control-Up> "$xfFocusCmd $xfPrevW"
      bind $xfW <Meta-Up> "$xfFocusCmd $xfPrevW"
    } {
      if {"[winfo class $xfW]" == "Text"} {
        bind $xfW <Control-Up> "$xfFocusCmd $xfPrevW"
        bind $xfW <Meta-Up> "$xfFocusCmd $xfPrevW"
      } {
        if {"[winfo class $xfW]" == "TkEmacs"} {
          bind $xfW <Control-Up> "$xfFocusCmd $xfPrevW"
          bind $xfW <Meta-Up> "$xfFocusCmd $xfPrevW"
        }
      }
    }
  } {
    if {"[winfo class $xfW]" == "Entry"} {
      catch "bind $xfW <Up> {NoFunction}"
      catch "bind $xfW <Control-Up> {NoFunction}"
      catch "bind $xfW <Meta-Up> {NoFunction}"
    } {
      if {"[winfo class $xfW]" == "Text"} {
        catch "bind $xfW <Control-Up> {NoFunction}"
        catch "bind $xfW <Meta-Up> {NoFunction}"
      } {
        if {"[winfo class $xfW]" == "TkEmacs"} {
          catch "bind $xfW <Control-Up> {NoFunction}"
          catch "bind $xfW <Meta-Up> {NoFunction}"
        }
      }
    }
  }
  if {"$xfNextW" != ""} {
    if {"[winfo class $xfW]" == "Entry"} {
      bind $xfW <Down> "$xfFocusCmd $xfNextW"
      bind $xfW <Control-Down> "$xfFocusCmd $xfNextW"
      bind $xfW <Meta-Down> "$xfFocusCmd $xfNextW"
      bind $xfW <Return> "$xfCommand; $xfFocusCmd $xfNextW"
      bind $xfW <Control-Return> "$xfCommand; $xfFocusCmd $xfNextW"
      bind $xfW <Meta-Return> "$xfCommand; $xfFocusCmd $xfNextW"
    } {
      if {"[winfo class $xfW]" == "Text"} {
        bind $xfW <Control-Down> "$xfFocusCmd $xfNextW"
        bind $xfW <Meta-Down> "$xfFocusCmd $xfNextW"
        bind $xfW <Control-Return> "$xfCommand; $xfFocusCmd $xfNextW"
        bind $xfW <Meta-Return> "$xfCommand; $xfFocusCmd $xfNextW"
      } {
        if {"[winfo class $xfW]" == "TkEmacs"} {
          bind $xfW <Control-Down> "$xfFocusCmd $xfNextW"
          bind $xfW <Meta-Down> "$xfFocusCmd $xfNextW"
          bind $xfW <Control-Return> "$xfCommand; $xfFocusCmd $xfNextW"
          bind $xfW <Meta-Return> "$xfCommand; $xfFocusCmd $xfNextW"
        }
      }
    }
  } {
    if {"[winfo class $xfW]" == "Entry"} {
      catch "bind $xfW <Down> {NoFunction}"
      catch "bind $xfW <Control-Down> {NoFunction}"
      catch "bind $xfW <Meta-Down> {NoFunction}"
      catch "bind $xfW <Return> \"$xfCommand; NoFunction\""
      catch "bind $xfW <Control-Return> \"$xfCommand; NoFunction\""
      catch "bind $xfW <Meta-Return> \"$xfCommand; NoFunction\""
    } {
      if {"[winfo class $xfW]" == "Text"} {
        catch "bind $xfW <Control-Down> {NoFunction}"
        catch "bind $xfW <Meta-Down> {NoFunction}"
        catch "bind $xfW <Control-Return> \"$xfCommand; NoFunction\""
        catch "bind $xfW <Meta-Return> \"$xfCommand; NoFunction\""
      } {
        if {"[winfo class $xfW]" == "TkEmacs"} {
          catch "bind $xfW <Control-Down> {NoFunction}"
          catch "bind $xfW <Meta-Down> {NoFunction}"
          catch "bind $xfW <Control-Return> \"$xfCommand; NoFunction\""
          catch "bind $xfW <Meta-Return> \"$xfCommand; NoFunction\""
        }
      }
    }
  }
}

##########
# Procedure: XFBindShowName
# Description: action to show the current pathname and to store him
#              into the selection buffer for later pasting
# Arguments: xfW - the widget
#            xfX - x position in widget
#            xfY - y position in widget
# Returns: none
# Sideeffects: none
##########
proc XFBindShowName {xfW xfX xfY} {
  global symbolicName
  global xfConf

  XFTmpltToplevel .xfShowName 200x80 {XF widget name}

  if {$xfConf(autoPos)} {
    set xfPosX [expr $xfX+[winfo rootx $xfW]+30]
    set xfPosY [expr $xfY+[winfo rooty $xfW]+30]
    wm positionfrom .xfShowName user
    wm geometry .xfShowName "200x80+$xfPosX+$xfPosY"
  }

  label .xfShowName.descr1 \
    -anchor c \
    -relief raised \
    -text "This widget is named:"
  
  set xfName $xfW
  set xfShowName $xfW
  foreach xfCounter [array names symbolicName] {
    set xfArrayName ""
    append xfArrayName symbolicName ( $xfCounter )
    if {"$xfName" == "[set $xfArrayName]"} {
      set xfName $xfCounter
      set xfShowName [SymbolicName "$xfCounter"]
    }
  }

  message .xfShowName.descr2 \
    -justify center \
    -width 190 \
    -anchor n \
    -relief raised \
    -text "$xfName"
  
  .xfEdit.curSelected delete 0 end
  .xfEdit.curSelected insert 0 $xfShowName
  .xfEdit.curSelected select from 0
  .xfEdit.curSelected select to 0

  pack append .xfShowName \
              .xfShowName.descr1 {top fill} \
              .xfShowName.descr2 {top fill expand}
}

##########
# Procedure: XFBindSelectOne
# Description: action to select the current list item
# Arguments: xfW - the widget
#            xfY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc XFBindSelectOne {xfW xfY} {

  set xfNearest [$xfW nearest $xfY]
  if {$xfNearest >= 0} {
    $xfW selection clear 0 end
    $xfW selection anchor $xfNearest
    $xfW selection set $xfNearest
  }
}

##########
# Procedure: XFBindSelectOneIntoEntry
# Description: action to select the current list item,
#              and insert the value into a entry widget
# Arguments: xfW - the widget
#            xfY - the y position in the listbox
#            xfEntry -the entry
# Returns: none
# Sideeffects: none
##########
proc XFBindSelectOneIntoEntry {xfW xfY xfEntry} {

  set xfNearest [$xfW nearest $xfY]
  if {$xfNearest >= 0} {
    $xfW select anchor $xfNearest
    $xfW select set $xfNearest
    $xfEntry delete 0 end
    $xfEntry insert 0 [$xfW get $xfNearest]
  }
}

# eof

