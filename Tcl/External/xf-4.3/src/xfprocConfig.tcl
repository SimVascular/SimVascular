# Program: xf
# Description: procedures that implement the editing
#
# $Header: xfprocConfig.tcl[2.4] Wed Mar 10 12:07:45 1993 garfield@garfield frozen $

proc XFProcConfAddCurrentItem {xfType} {
##########
# Procedure: XFProcConfAddCurrentItem
# Description: insert the currently selected type of item
# Arguments: xfType - type of inserting (add, config)
# Returns: none
# Sideeffects: none
##########

  global xfPath
  global xfStatus
  global xfMisc

  set xfAddNumber 1
  if {"[info commands .xfEdit.frame5.addNumber]" != ""} {
    set xfAddNumber [.xfEdit.frame5.addNumber get]
    .xfEdit.frame5.addNumber set 1
  }
  if {$xfStatus(itemList) == 2} {
    set xfCounter 0
    while {$xfCounter < $xfAddNumber} {
      XFEditInsertTmplt $xfStatus(type)
      incr xfCounter 1
    }
  } {
    if {"[info procs XFAdd.$xfStatus(type)]" == ""} {
      if {[file exists "$xfPath(elements)/$xfStatus(type)"]} {
          source "$xfPath(elements)/$xfStatus(type)"
      } {
        foreach xfPathElement [split $xfPath(additionals) $xfMisc(separator)] {
          if {[XFMiscIsDir $xfPathElement]} {
            if {[file exists "$xfPathElement/$xfStatus(type)"]} {
              source "$xfPathElement/$xfStatus(type)"
              break
            }
          }
        }
      }
    }
    if {"$xfType" == "add"} {
      if {"[info procs XFAdd.$xfStatus(type)]" != ""} {
        set xfCounter 0
        while {$xfCounter < $xfAddNumber} {
          if {[catch "XFAdd.$xfStatus(type) {} {} add" xfResult]} {
            if {"$xfResult" != ""} {
              XFProcError $xfResult
            }
            XFEditSetStatus "Insertion of widget...aborted"
          }
          incr xfCounter 1
        }
        if {"$xfStatus(type)" == "Toplevel"} {
          XFEditSetShowWindows
        }
      }
    } {
      if {"[info procs XFAdd.$xfStatus(type)]" != ""} {
        if {"[info procs XFAddTmp.$xfStatus(type)]" != ""} {
          XFAddTmp.$xfStatus(type)
        } {
          catch "destroy .xfEdit.tmplt"
          if {[catch "[string tolower $xfStatus(type)] .xfEdit.tmplt"]} {
             XFProcConfConfigure undefined "" "" $xfStatus(type) add
          } {
             XFProcConfConfigure .xfEdit.tmplt "" "" $xfStatus(type) add
          }
        }
      }
    }
  }
}

proc XFProcConfBinding {{xfW ""} {xfLeader ""}} {
##########
# Procedure: XFProcConfBinding
# Description: call binding for the specified widget
# Arguments: {xfW} - the widget to configure, or empty
#                    to use current widget path
#            {xfLeader} - the leading toplevel
# Returns: none
# Sideeffects: none
##########

  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  XFProcConfConfigure $xfW 3 $xfLeader
}

##########
# Procedure: XFProcConfConfigure
# Description: configure selected widget
# Arguments: xfW - the widget to configure
#            xfDialog - which type of configuration
#                     0 = packing
#                     1 = placing
#                     2 = geometry
#                     3 = bindings
#                     4 = default parameters
#                     5 = special parameters
#                     .... more dialogs
#            xfLeader - the leading window
#            xfClass - the widget class we configure
#            xfType - type of configuration (add, config)
# Returns: none
# Sideeffects: none
##########
proc XFProcConfConfigure {xfW {xfDialog ""} {xfLeader ""} {xfClass ""} {xfType ""}} {
  global xfConf
  global xfDefaultConf
  global xfNoSpecialBinding
  global xfNoSpecialPacking
  global xfNoSpecialPlacing
  global xfPath
  global xfMisc

  if {"$xfW" != "." && "[info commands $xfW]" == ""} {
    return
  }
  if {"$xfClass" == ""} {
    if {"$xfW" == "."} {
      set xfClass Toplevel
    } {
      set xfClass [winfo class $xfW]
    }
  }
  if {$xfDialog == 0} {
    if {[lsearch $xfNoSpecialPacking $xfClass] != -1} {
      XFPacking $xfW $xfType $xfClass $xfLeader
      return
    }
  } {
    if {$xfDialog == 1} {
      if {[lsearch $xfNoSpecialPlacing $xfClass] != -1} {
        XFPlacing $xfW $xfType $xfClass $xfLeader
        return
      }
    } {
      if {$xfDialog == 2} {
        if {"$xfConf(geometry)" == "packer"} {
          if {[lsearch $xfNoSpecialPacking $xfClass] != -1} {
            XFPacking $xfW $xfType $xfClass $xfLeader
            return
          }
        } {
          if {[lsearch $xfNoSpecialPlacing $xfClass] != -1} {
            XFPlacing $xfW $xfType $xfClass $xfLeader
            return
          }
        }
      } {
        if {$xfDialog == 3} {
          if {[lsearch $xfNoSpecialBinding $xfClass] != -1} {
            XFBinding $xfW $xfType $xfClass $xfLeader
            return
          }
        }
      }
    }
  }
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
  if {"$xfDialog" != ""} {
    if {"[info proc XFConfig.$xfClass$xfDialog]" != ""} {
      XFConfig.$xfClass$xfDialog $xfW config $xfClass $xfLeader
    } {
      if {$xfDialog == 0} {
        XFPacking $xfW $xfType $xfClass $xfLeader
      } {
        if {$xfDialog == 1} {
          XFPlacing $xfW $xfType $xfClass $xfLeader
        } {
          if {$xfDialog == 2} {
            if {"$xfConf(geometry)" == "packer"} {
              XFPacking $xfW $xfType $xfClass $xfLeader
            } {
              XFPlacing $xfW $xfType $xfClass $xfLeader
            }
          } {
            if {$xfDialog == 3} {
              XFBinding $xfW $xfType $xfClass $xfLeader
            }
          }
        }
      }
    }
  } {
    if {"$xfType" == "add"} {
      if {"[info proc XFConfig.${xfClass}4]" != ""} {
        if {$xfConf(onlyOneWindow)} {
          XFConfig.${xfClass}4 $xfW add 0
        } {
          XFConfig.${xfClass}4 $xfW add $xfClass
        }
      }
    } {
      if {[info exists xfDefaultConf([string tolower $xfClass])]} {
        if {"[info proc XFConfig.$xfClass$xfDefaultConf([string tolower $xfClass])]" != ""} {
          if {$xfConf(onlyOneWindow)} {
           XFConfig.$xfClass$xfDefaultConf([string tolower $xfClass]) \
              $xfW config 0
          } {
            XFConfig.$xfClass$xfDefaultConf([string tolower $xfClass]) \
              $xfW config $xfClass
          }
        } {
          if {$xfDefaultConf([string tolower $xfClass]) == 0} {
            XFPacking $xfW $xfType $xfClass $xfLeader
          } {
            if {$xfDefaultConf([string tolower $xfClass]) == 1} {
              XFPlacing $xfW $xfType $xfClass $xfLeader
            } {
              if {$xfDefaultConf([string tolower $xfClass]) == 2} {
                if {"$xfConf(geometry)" == "packer"} {
                  XFPacking $xfW $xfType $xfClass $xfLeader
                } {
                  XFPlacing $xfW $xfType $xfClass $xfLeader
                }
              } {
                if {$xfDefaultConf([string tolower $xfClass]) == 3} {
                  XFBinding $xfW $xfType $xfClass $xfLeader
                }
              }
            }
          }
        }
      }
    }
  }
}

proc XFProcConfGeometryDefault {{xfW ""} {xfLeader ""}} {
##########
# Procedure: XFProcConfGeometryDefault
# Description: call default geometry handler
# Arguments: {xfW} - the widget to configure, or empty
#                    to use current widget path
#            {xfLeader} - the leading toplevel
# Returns: none
# Sideeffects: none
##########

  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  XFProcConfConfigure $xfW 2 $xfLeader
}

proc XFProcConfInsertTemplate {xfName} {
##########
# Procedure: XFProcConfInsertTemplate
# Description: insert the specified template
# Arguments: xfName - the relative pathname of the template
# Returns: none
# Sideeffects: none
##########

  global xfPath
  global xfStatus
  global xfMisc

  foreach xfPathElement [split $xfPath(templates) $xfMisc(separator)] {
    if {[XFMiscIsFile $xfPathElement/$xfName.t]} {
      XFEditSetStatus "Inserting template [file tail $xfName]..."
      XFPasteFile $xfStatus(path) $xfPathElement/$xfName.t
      XFEditSetStatus "Inserting template [file tail $xfName]...done"
      break
    }
  }
}

proc XFProcConfInsertWidgetDefault {xfClass} {
##########
# Procedure: XFProcConfInsertWidgetDefault
# Description: insert the specified class of widget
# Arguments: xfClass - the class to insert 
# Returns: none
# Sideeffects: none
##########

  global xfStatus

  set xfListNum 0
  if {[lsearch $xfStatus(elementList) $xfClass] != -1} {
    set xfListNum 0
  } {
    if {[lsearch $xfStatus(additionalList) $xfClass] != -1} {
      set xfListNum 1
    } {
      set xfListNum 2
    }
  }
  XFEditSetType $xfClass $xfListNum
  XFProcConfAddCurrentItem add
}

proc XFProcConfInsertWidgetConfig {xfClass} {
##########
# Procedure: XFProcConfInsertWidgetConfig
# Description: insert the specified class of widget and configure it
# Arguments: xfClass - the class to insert 
# Returns: none
# Sideeffects: none
##########

  set xfListNum 0
  if {[lsearch $xfStatus(elementList) $xfClass] != -1} {
    set xfListNum 0
  } {
    if {[lsearch $xfStatus(additionalList) $xfClass] != -1} {
      set xfListNum 1
    } {
      set xfListNum 2
    }
  }
  XFEditSetType $xfClass $xfListNum
  XFProcConfAddCurrentItem conf
}

proc XFProcConfLayout {} {
##########
# Procedure: XFProcConfLayout
# Description: call layouting for widgets
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling layouting..."
  XFLayout
  XFEditSetStatus "Calling layouting...done"
}

proc XFProcConfPacking {{xfW ""} {xfLeader ""}} {
##########
# Procedure: XFProcConfPacking
# Description: call packing for the specified widget
# Arguments: {xfW} - the widget to configure, or empty
#                    to use current widget path
#            {xfLeader} - the leading toplevel
# Returns: none
# Sideeffects: none
##########

  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  XFProcConfConfigure $xfW 0 $xfLeader
}

proc XFProcConfParametersDefault {{xfW ""} {xfLeader ""}} {
##########
# Procedure: XFProcConfParametersDefault
# Description: call default configuration dialog for the specified widget
# Arguments: {xfW} - the widget to configure, or empty
#                    to use current widget path
#            {xfLeader} - the leading toplevel
# Returns: none
# Sideeffects: none
##########

  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  XFProcConfConfigure $xfW "" $xfLeader
}

proc XFProcConfParametersGeneral {{xfW ""} {xfLeader ""}} {
##########
# Procedure: XFProcConfParametersGeneral
# Description: call general parameter dialog for the specified widget
# Arguments: {xfW} - the widget to configure, or empty
#                    to use current widget path
#            {xfLeader} - the leading toplevel
# Returns: none
# Sideeffects: none
##########

  global xfStatus

  if {"$xfW" == "" || "[info commands $xfW]" == ""} {
    set xfW $xfStatus(path)
  }
  XFParameters $xfW $xfLeader
}

proc XFProcConfParametersGroups {} {
##########
# Procedure: XFProcConfParametersGroups
# Description: set parameters for groups of widgets
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling parameter setting for groups..."
  XFGroups
  XFEditSetStatus "Calling parameter setting for groups...done"
}

proc XFProcConfParametersSmall {{xfW ""} {xfLeader ""}} {
##########
# Procedure: XFProcConfParametersSmall
# Description: call small parameter dialog for the specified widget
# Arguments: {xfW} - the widget to configure, or empty
#                    to use current widget path
#            {xfLeader} - the leading toplevel
# Returns: none
# Sideeffects: none
##########

  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  XFProcConfConfigure $xfW 4 $xfLeader
}

proc XFProcConfParametersSpecial {{xfW ""} {xfLeader ""}} {
##########
# Procedure: XFProcConfParametersSpecial
# Description: call special parameter dialog for the specified widget
# Arguments: {xfW} - the widget to configure, or empty
#                    to use current widget path
#            {xfLeader} - the leading toplevel
# Returns: none
# Sideeffects: none
##########

  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  XFProcConfConfigure $xfW 5 $xfLeader
}

proc XFProcConfPlacing {{xfW ""} {xfLeader ""}} {
##########
# Procedure: XFProcConfPlacing
# Description: call placing for the specified widget
# Arguments: {xfW} - the widget to configure, or empty
#                    to use current widget path
#            {xfLeader} - the leading toplevel
# Returns: none
# Sideeffects: none
##########

  global xfStatus

  if {"$xfW" == ""} {
    set xfW $xfStatus(path)
  }
  XFProcConfConfigure $xfW 1 $xfLeader
}

# eof

