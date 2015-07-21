# XFNoParsing
# Program: template
# Description: clear a list
#
# $Header: ClearList.t[2.3] Wed Mar 10 12:02:38 1993 garfield@garfield frozen $

proc ClearList {listWidget} {# xf ignore me 5
##########
# Procedure: ClearList
# Description: clear listbox widget
# Arguments: listWidget - the widget to clear
# Returns: none
# Sideeffects: the list widget is cleared
##########

  if {[$listWidget size] > 0} {
    $listWidget delete 0 end
  }
}

# eof

