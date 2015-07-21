# Program: xf
# Description: specify module structure
#
# $Header: xfmodules.tcl[2.4] Wed Mar 10 12:07:07 1993 garfield@garfield frozen $

##########
# Procedure: XFModules
# Description: specify module strucrure
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFModules {} {
  global moduleList
  global xfStatus

  XFMiscUpdateModuleList

  # build widget structure
  XFTmpltToplevel .xfModules 540x420 \
   "XF module structure"

  label .xfModules.message1 \
    -relief raised \
    -text "Module structure:"

  XFTmpltFrame .xfModules.frame1 0

  button .xfModules.frame1.ok \
    -text {OK} \
    -command {destroy .xfModules}

  button .xfModules.frame1.save \
    -text {Save} \
    -command {
      global xfPath
      global xfStatus
      if {$xfStatus(handleTemplates)} {
        foreach xfCounter $xfStatus(tmpltList) {
          if {![catch "winfo class $xfCounter"]} {
            XFEditSetStatus "Creating template..."
            XFSaveSubTree $xfCounter $xfPath(tmp)/tb$xfStatus(uniqueId)
            XFEditSetStatus "Creating template...done"
          }
        }
        XFSaveProcsTmplt
        XFProcEditSaveCutAsTemplate tb
      } {
        XFProcFileSave
      }}

  checkbutton .xfModules.frame1.tmplt \
    -text {Handle Templates} \
    -variable xfStatus(handleTemplates) \
    -command {
      global xfStatus
      XFModulesReadProcs
      XFModulesReadTree
      XFModulesReadModules
      XFModulesReadContents
      if {$xfStatus(handleTemplates)} {
        .xfModules.frame2.toplevels.toplevelsMess configure \
          -text Widgets:
      } {
        .xfModules.frame2.toplevels.toplevelsMess configure \
          -text Toplevels:
      }}

  button .xfModules.frame1.rescan \
    -text {Rescan} \
    -command {
      XFModulesReadProcs
      XFModulesReadTree
      XFModulesReadModules
      XFModulesReadContents}

  XFTmpltFrame .xfModules.frame3 0

  button .xfModules.frame3.insert \
    -text {Add module} \
    -command {
      global autoLoadList
      global moduleList
      global xfSaveModuleList
      global xfStatus
      set xfModName [lindex [.xfModules.modname.modname get] 0]
      if {"$xfModName" != ""} {
        catch "set {moduleList($xfModName)} \"\""
        catch "set {autoLoadList($xfModName)} 0"
        if {[llength $xfSaveModuleList] > 0} {
          if {[lsearch $xfSaveModuleList $xfModName] == -1} {
            lappend xfSaveModuleList $xfModName
            set xfStatus(saveModuleList) 1
          }
        }
        XFModulesReadModules
        XFModulesReadContents
      }}

  button .xfModules.frame3.erase \
    -text {Remove module} \
    -command {
      global autoLoadList
      global moduleList
      global xfSaveModuleList
      set xfModName [lindex [.xfModules.modname.modname get] 0]
      if {"$xfModName" != ""} {
        catch "unset {moduleList($xfModName)}"
        catch "unset {autoLoadList($xfModName)}"
        if {[llength $xfSaveModuleList] > 0} {
          set xfTmpIndex [lsearch $xfSaveModuleList $xfModName]
          if {$xfTmpIndex != -1} {
            set xfSaveModuleList [lreplace $xfSaveModuleList $xfTmpIndex $xfTmpIndex]
          }
        }
        XFModulesReadModules      
        XFModulesReadContents
      }}

  button .xfModules.frame3.erase2 \
    -text {Delete element} \
    -command {
      set xfCurSelect [.xfModules.frame2.contents.contents curselect]
      if {$xfCurSelect >= 0} {
        XFModulesRemoveFromModule \
          [.xfModules.frame2.contents.contents get $xfCurSelect]
      }}

  XFTmpltLabledEntry .xfModules modname "Module name:"

  checkbutton .xfModules.modname.save \
    -text {Save module} \
    -variable xfStatus(saveModuleList) \
    -command {
      global moduleList
      global xfConf
      global xfSaveModuleList
      global xfStatus
      set xfModName [.xfModules.modname.modname get]
      if {[llength $xfSaveModuleList] == 0} {
        foreach xfCounter [array names moduleList] {
          lappend xfSaveModuleList $xfCounter
        }
        if {[lsearch $xfSaveModuleList $xfConf(programName)] == -1} {
          lappend xfSaveModuleList $xfConf(programName)
        }
      }
      if {$xfStatus(saveModuleList)} {
        if {[lsearch $xfSaveModuleList $xfModName] == -1} {
          lappend xfSaveModuleList $xfModName
        }
      } {
        set xfTmpIndex [lsearch $xfSaveModuleList $xfModName]
        if {$xfTmpIndex != -1} {
          set xfSaveModuleList [lreplace $xfSaveModuleList $xfTmpIndex $xfTmpIndex]
        }
      }}

  checkbutton .xfModules.modname.autoload \
    -text {Auto load} \
    -variable xfStatus(autoLoadModule) \
    -command {
      global autoLoadList
      global xfStatus
      if {![catch "set moduleList([.xfModules.modname.modname get])"]} {
        set autoLoadList([.xfModules.modname.modname get]) \
          $xfStatus(autoLoadModule)
      }}

  XFTmpltFrame .xfModules.frame2 0

  XFTmpltListbox .xfModules.frame2 procs
  .xfModules.frame2.procs.procs configure \
    -width 14 -height 30

  label .xfModules.frame2.procs.procsMess \
    -relief raised \
    -text {Procedures:}

  XFTmpltListbox .xfModules.frame2 toplevels
  .xfModules.frame2.toplevels.toplevels configure \
    -width 14 -height 30

  label .xfModules.frame2.toplevels.toplevelsMess \
    -relief raised \
    -text {Toplevels:}

  XFTmpltListbox .xfModules.frame2 modules
  .xfModules.frame2.modules.modules configure \
    -width 14 -height 30

  label .xfModules.frame2.modules.modulesMess \
    -relief raised \
    -text {Modules:}

  XFTmpltListbox .xfModules.frame2 contents

  label .xfModules.frame2.contents.contentsMess \
    -relief raised \
    -text {Contents:}

  scale .xfModules.frame2.mover \
    -orient vertical \
    -width 8 \
    -sliderlength 15 \
    -from 0 \
    -command {XFModulesReposition}

  if {"[array names moduleList]" != ""} {
    .xfModules.modname.modname insert 0 [lindex [array names moduleList] 0]
  }

  XFModulesReadProcs
  XFModulesReadTree
  XFModulesReadModules
  XFModulesReadContents

  # bindings
  bind .xfModules.frame2.procs.procs <ButtonPress-1> {
    XFBindSelectOne %W %y
    .xfModules.frame2.toplevels.toplevels select clear 0 end
    set xfCurSelect [.xfModules.frame2.procs.procs curselect]
    if {$xfCurSelect >= 0} {
      XFModulesAddToModule \
        [.xfModules.frame2.procs.procs get $xfCurSelect]
    }}
  bind .xfModules.frame2.procs.procs <Button1-Motion> {
    XFBindSelectOne %W %y
    .xfModules.frame2.toplevels.toplevels select clear 0 end}
  bind .xfModules.frame2.procs.procs <Shift-ButtonPress-1> {
    XFBindSelectOne %W %y
    .xfModules.frame2.toplevels.toplevels select clear 0 end}
  bind .xfModules.frame2.procs.procs <Shift-Button1-Motion> {
    XFBindSelectOne %W %y
    .xfModules.frame2.toplevels.toplevels select clear 0 end}

  bind .xfModules.frame2.toplevels.toplevels <ButtonPress-1> {
    XFBindSelectOne %W %y
    .xfModules.frame2.procs.procs select clear 0 end
    set xfCurSelect [.xfModules.frame2.toplevels.toplevels curselect]
    if {$xfCurSelect >= 0} {
      XFModulesAddToModule \
        [.xfModules.frame2.toplevels.toplevels get $xfCurSelect]
    }}
  bind .xfModules.frame2.toplevels.toplevels <Button1-Motion> {
    XFBindSelectOne %W %y
    .xfModules.frame2.procs.procs select clear 0 end}
  bind .xfModules.frame2.toplevels.toplevels <Shift-ButtonPress-1> {
    XFBindSelectOne %W %y
    .xfModules.frame2.procs.procs select clear 0 end}
  bind .xfModules.frame2.toplevels.toplevels <Shift-Button1-Motion> {
    XFBindSelectOne %W %y
    .xfModules.frame2.procs.procs select clear 0 end}

  bind .xfModules.frame2.modules.modules <ButtonPress-1> {
    XFBindSelectOneIntoEntry %W %y .xfModules.modname.modname
    XFModulesReadContents}
  bind .xfModules.frame2.modules.modules <Button1-Motion> {
    XFBindSelectOneIntoEntry %W %y .xfModules.modname.modname
    XFModulesReadContents}
  bind .xfModules.frame2.modules.modules <Shift-ButtonPress-1> {
    XFBindSelectOneIntoEntry %W %y .xfModules.modname.modname
    XFModulesReadContents}
  bind .xfModules.frame2.modules.modules <Shift-Button1-Motion> {
    XFBindSelectOneIntoEntry %W %y .xfModules.modname.modname
    XFModulesReadContents}

  bind .xfModules.frame2.contents.contents <Double-ButtonPress-1> {
    set xfCurSelect [.xfModules.frame2.contents.contents curselect]
    if {$xfCurSelect >= 0} {
      XFModulesRemoveFromModule \
        [.xfModules.frame2.contents.contents get $xfCurSelect]
    }}
  bind .xfModules.frame2.contents.contents <ButtonPress-1> {
    XFBindSelectOne %W %y}
  bind .xfModules.frame2.contents.contents <Button1-Motion> {
    XFBindSelectOne %W %y}
  bind .xfModules.frame2.contents.contents <Shift-ButtonPress-1> {
    XFBindSelectOne %W %y}
  bind .xfModules.frame2.contents.contents <Shift-Button1-Motion> {
    XFBindSelectOne %W %y}

  bind .xfModules.modname.modname <Return> {
    global autoLoadList
    global moduleList
    global xfSaveModuleList
    global xfStatus
    set xfModName [lindex [.xfModules.modname.modname get] 0]
    if {"$xfModName" != ""} {
      catch "set {moduleList($xfModName)} \"\""
      catch "set {autoLoadList($xfModName)} 0"
      if {[llength $xfSaveModuleList] > 0} {
        if {[lsearch $xfSaveModuleList $xfModName] == -1} {
          lappend xfSaveModuleList $xfModName
          set xfStatus(saveModuleList) 1
        }
      }
      XFModulesReadModules
      XFModulesReadContents
    }}

  # packing
  pack after .xfModules.modname.modname \
             .xfModules.modname.autoload {right fill} \
             .xfModules.modname.save {right fill}
  pack before .xfModules.frame2.procs.vscroll \
              .xfModules.frame2.procs.procsMess {top fillx}
  pack before .xfModules.frame2.toplevels.vscroll \
              .xfModules.frame2.toplevels.toplevelsMess {top fillx}
  pack before .xfModules.frame2.modules.vscroll \
              .xfModules.frame2.modules.modulesMess {top fillx}
  pack before .xfModules.frame2.contents.vscroll \
              .xfModules.frame2.contents.contentsMess {top fillx}
  pack append .xfModules.frame1 \
              .xfModules.frame1.ok {left fill expand} \
              .xfModules.frame1.save {left fill expand} \
              .xfModules.frame1.tmplt {left fill expand} \
              .xfModules.frame1.rescan {left fill expand}
  pack append .xfModules.frame2 \
              .xfModules.frame2.procs {left fill} \
              .xfModules.frame2.toplevels {left fill} \
              .xfModules.frame2.modules {left fill} \
              .xfModules.frame2.mover {right fill} \
              .xfModules.frame2.contents {left fill expand}
  pack append .xfModules.frame3 \
              .xfModules.frame3.insert {left fill expand} \
              .xfModules.frame3.erase {left fill expand} \
              .xfModules.frame3.erase2 {left fill expand}
  pack append .xfModules \
              .xfModules.frame1 {bottom fill} \
              .xfModules.message1 {top fillx} \
              .xfModules.frame3 {bottom fill} \
              .xfModules.modname {bottom fill} \
              .xfModules.frame2 {top fill expand}
}

##########
# Procedure: XFModulesAddToModule
# Description: add selected item to module
# Arguments: xfName - the name of the item to add
# Returns: none
# Sideeffects: none
##########
proc XFModulesAddToModule {xfName} {
  global moduleList
  global xfStatus

  if {"$xfName" != ""} {
    if {$xfStatus(handleTemplates)} {
      if {[llength $xfName] == 2} {
        set xfName [lindex $xfName 1]
      }
      set xfModuleList $xfStatus(tmpltList)
      set xfStatus(tmpltList) ""
      foreach xfCounter $xfModuleList {
        if {"$xfCounter" != "$xfName"} {
          if {[catch "winfo class $xfCounter"] ||
              [catch "winfo class $xfName"]} {
            append xfStatus(tmpltList) " $xfCounter"
          }
        }
      }
      if {"$xfName" != "."} {
        append xfStatus(tmpltList) " $xfName"
      }
      XFModulesReadContents
    } {
      if {![catch "set moduleList([.xfModules.modname.modname get])"]} {
        foreach xfCounter [lsort [array names moduleList]] {
          set xfModuleList [set moduleList($xfCounter)]
          set moduleList($xfCounter) ""
          foreach xfCounter2 $xfModuleList {
            if {"$xfCounter2" != "$xfName"} {
              append moduleList($xfCounter) " $xfCounter2"
            }
          }
        }
        append moduleList([.xfModules.modname.modname get]) " $xfName"
        XFModulesReadContents
      }
    }
  }
}

##########
# Procedure: XFModulesReadContents
# Description: read module contents
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFModulesReadContents {} {
  global autoLoadList
  global moduleList
  global xfConf
  global xfSaveModuleList
  global xfStatus

  if {$xfStatus(handleTemplates)} {
    XFMiscClearList .xfModules.frame2.contents.contents
    foreach xfCounter $xfStatus(tmpltList) {
      .xfModules.frame2.contents.contents insert end $xfCounter
    }
  } {
    XFMiscUpdateModuleList
    set xfModName [.xfModules.modname.modname get]
    if {"$xfModName" != ""} {
      if {"$xfModName" == $xfConf(programName)} {
        XFMiscClearList .xfModules.frame2.contents.contents
        set xfTmpList ""
        foreach xfCounter2 [set moduleList($xfModName)] {
          if {![XFMiscInModule $xfCounter2]} {
            .xfModules.frame2.contents.contents insert end $xfCounter2
            lappend xfTmpList "$xfCounter2"
          }
        }
        set xfCounter 0
        set xfLength [.xfModules.frame2.procs.procs size]
        while {$xfCounter < $xfLength} {
          set xfCounter2 [.xfModules.frame2.procs.procs get $xfCounter]
          if {![XFMiscInModule $xfCounter2] &&
              [lsearch $xfTmpList $xfCounter2] == -1} {
            .xfModules.frame2.contents.contents insert end $xfCounter2
          }
          incr xfCounter 1
        }
        set xfCounter 0
        set xfLength [.xfModules.frame2.toplevels.toplevels size]
        while {$xfCounter < $xfLength} {
          set xfCounter2 [.xfModules.frame2.toplevels.toplevels get $xfCounter]
          if {![XFMiscInModule $xfCounter2] &&
              [lsearch $xfTmpList $xfCounter2] == -1} {
            .xfModules.frame2.contents.contents insert end $xfCounter2
          }
          incr xfCounter 1
        }
      } {
        foreach xfCounter [lsort [array names moduleList]] {
          if {[string compare $xfCounter $xfModName] == 0} {
            XFMiscClearList .xfModules.frame2.contents.contents
            foreach xfCounter2 [set moduleList($xfCounter)] {
              .xfModules.frame2.contents.contents insert end $xfCounter2
            }
          }
        }
      }
      if {[llength $xfSaveModuleList] > 0} {
        if {[lsearch $xfSaveModuleList $xfModName] != -1} {
          set xfStatus(saveModuleList) 1
        } {
          set xfStatus(saveModuleList) 0
        }
      } {
        foreach xfCounter [array names moduleList] {
          lappend xfSaveModuleList $xfCounter
        }
        if {[lsearch $xfSaveModuleList $xfConf(programName)] == -1} {
          lappend xfSaveModuleList $xfConf(programName)
        }
        set xfStatus(saveModuleList) 1
      }
      if {[info exists autoLoadList($xfModName)]} {
        set xfStatus(autoLoadModule) $autoLoadList($xfModName)
      } {
        set xfStatus(autoLoadModule) 0
      }
    }
  }
  if {[.xfModules.frame2.contents.contents size] > 0} {
    .xfModules.frame2.mover configure \
      -to [.xfModules.frame2.contents.contents size]
  } {
    .xfModules.frame2.mover configure \
      -to 1
  }
}

##########
# Procedure: XFModulesReadModules
# Description: read module list
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFModulesReadModules {} {
  global moduleList

  XFMiscUpdateModuleList
  XFMiscClearList .xfModules.frame2.modules.modules
  foreach xfCounter [lsort [array names moduleList]] {
    .xfModules.frame2.modules.modules insert end $xfCounter
  }
}

##########
# Procedure: XFModulesReadProcs
# Description: read procedures
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFModulesReadProcs {} {

  XFMiscClearList .xfModules.frame2.procs.procs
  set xfElementList [lsort [info procs]]

  foreach xfCounter $xfElementList {
    if {![catch "winfo class $xfCounter"] ||
        [XFMiscIsXFElement $xfCounter]} {
      continue
    }
    .xfModules.frame2.procs.procs insert end $xfCounter
  }
}

##########
# Procedure: XFModulesReadTree
# Description: read widget tree
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFModulesReadTree {} {
  global xfStatus

  if {$xfStatus(handleTemplates)} {
    XFMiscReadTree . .xfModules.frame2.toplevels.toplevels all
  } {
    XFMiscClearList .xfModules.frame2.toplevels.toplevels

    .xfModules.frame2.toplevels.toplevels insert end .
    foreach xfCounter [winfo children .] {
      if {![string match ".xf*" $xfCounter] &&
          ![string match "xf*" [winfo name $xfCounter]] &&
          "[winfo class $xfCounter]" == "Toplevel"} {
        .xfModules.frame2.toplevels.toplevels insert end $xfCounter
      }
    }
    foreach xfCounter [info procs ShowWindow.*] {
      if {[catch "winfo class [string range $xfCounter 10 end]"]} {
        .xfModules.frame2.toplevels.toplevels insert end \
          [string range $xfCounter 10 end]
      }
    }
  }
}

##########
# Procedure: XFModulesRemoveFromModule
# Description: remove selected item from module
# Arguments: xfName - the name of the item to remove
# Returns: none
# Sideeffects: none
##########
proc XFModulesRemoveFromModule {xfName} {
  global moduleList
  global xfStatus

  if {$xfStatus(handleTemplates)} {
    set xfModuleList $xfStatus(tmpltList)
    set xfStatus(tmpltList) ""
    foreach xfCounter $xfModuleList {
      if {"$xfCounter" != "$xfName"} {
        append xfStatus(tmpltList) " $xfCounter"
      }
      XFModulesReadContents
    }
  } {
    if {"$xfName" != ""} {
      if {![catch "set moduleList([.xfModules.modname.modname get])"]} {
        set xfModuleList [set moduleList([.xfModules.modname.modname get])]
        set moduleList([.xfModules.modname.modname get]) ""
        foreach xfCounter $xfModuleList {
          if {"$xfCounter" != "$xfName"} {
            append moduleList([.xfModules.modname.modname get]) " $xfCounter"
          }
        }
        XFModulesReadContents
      }
    }
  }
}

##########
# Procedure: XFModulesReposition
# Description: reposition the seletected module item
# Arguments: xfPosition - scale position
# Returns: none
# Sideeffects: none
##########
proc XFModulesReposition {xfPosition} {
  global moduleList

  set xfCurSelect [.xfModules.frame2.contents.contents curselect]
  if {$xfCurSelect >= 0} {
    set xfTmpVal [.xfModules.frame2.contents.contents get $xfCurSelect]
    .xfModules.frame2.contents.contents delete $xfCurSelect
    .xfModules.frame2.contents.contents insert $xfPosition $xfTmpVal
    .xfModules.frame2.contents.contents select anchor $xfPosition
    .xfModules.frame2.contents.contents select set $xfPosition

    set xfModuleName [.xfModules.modname.modname get]
    if {![catch "set moduleList($xfModuleName)"]} {
      set xfModuleList [set moduleList($xfModuleName)]
      set moduleList($xfModuleName) ""
      set xfCounter 0
      set xfSize [.xfModules.frame2.contents.contents size]
      while {$xfCounter < $xfSize} {
        append moduleList($xfModuleName) \
          " [.xfModules.frame2.contents.contents get $xfCounter]"
        incr xfCounter
      }
    }
  }
}

# eof

