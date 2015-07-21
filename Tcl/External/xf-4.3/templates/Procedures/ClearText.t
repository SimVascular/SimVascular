# XFNoParsing
# Program: template
# Description: clear a text
#
# $Header: ClearText.t[2.3] Wed Mar 10 12:02:40 1993 garfield@garfield frozen $

proc ClearText {textWidget} {# xf ignore me 5
##########
# Procedure: ClearText
# Description: clear text widget
# Arguments: textWidget - the widget to clear
# Returns: none
# Sideeffects: the text widget is cleared
##########

  set xfStatus [lindex [$textWidget config -state] 4]
  $textWidget config -state normal
  $textWidget delete 1.0 end
  $textWidget config -state $xfStatus
}

# eof

