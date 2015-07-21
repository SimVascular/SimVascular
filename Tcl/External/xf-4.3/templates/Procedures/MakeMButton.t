# XFNoParsing
# Program: template
# Description: build a menubutton from a list
#
# $Header: MakeMButton.t[2.3] Wed Mar 10 12:03:21 1993 garfield@garfield frozen $

proc MakeMButton {widgetName buttonLabel itemType itemList {itemFunction ""}} {# xf ignore me 5
##########
# Procedure: MakeMButton
# Description: build a menubutton from a list
# Arguments: widgetName - the widgetname to create
#            buttonLabel - the label of the menubutton
#            itemType - the type of menuitems (command,
#                       check or radio)
#            itemList - the list of item entrys
#            itemFunction - the list of variables/functions
# Returns: none
# Sideeffects: none
##########

  if {"[info command $widgetName]" != ""} {
    $widgetName config -text "$buttonLabel" -menu $widgetName.m
  } {
    menubutton $widgetName -text "$buttonLabel" -menu $widgetName.m
  }
  if {"[info command $widgetName.m]" != ""} {
    $widgetName.m delete 0 last
  } {
    menu $widgetName.m
  }
  set xfCounter 0
  foreach counter $itemList {
    case $itemType {
      {command} {
        if {"$itemFunction" == ""} {
          $widgetName.m add command \
            -label "$counter"
        } {
          if {[llength $itemFunction] == 1} {
            $widgetName.m add command \
              -label "$counter" \
              -command "[lindex $itemFunction 0] $counter"
          } {
            $widgetName.m add command \
              -label "$counter" \
              -command "[lindex $itemFunction $xfCounter] $counter"
          }
        }
      }
      {check} {
        if {"$itemFunction" == ""} {
          $widgetName.m add checkbutton \
            -label "$counter" \
            -variable "$counter"
        } {
          if {[llength $itemFunction] == 1} {
            $widgetName.m add checkbutton \
              -label "$counter" \
              -variable "[lindex $itemFunction 0]"
          } {
            $widgetName.m add checkbutton \
              -label "$counter" \
              -variable "[lindex $itemFunction $xfCounter]"
          }
        }
      }
      {radio} {
        if {"$itemFunction" == ""} {
          $widgetName.m add radiobutton \
            -label "$counter" \
            -variable "$counter"
        } {
          if {[llength $itemFunction] == 1} {
            $widgetName.m add radiobutton \
              -label "$counter" \
              -variable "[lindex $itemFunction 0]"
          } {
            $widgetName.m add radiobutton \
              -label "$counter" \
              -variable "[lindex $itemFunction $xfCounter]"
          }
        }
      }
    }
    incr xfCounter
  }
}

# eof

