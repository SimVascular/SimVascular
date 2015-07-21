# XFNoParsing
# Program: template
# Description: put a given file into a list
#
# $Header: FileInList.t[2.3] Wed Mar 10 12:02:59 1993 garfield@garfield frozen $

proc FdInList {listWidget {fileInFile ""}} {# xf ignore me 5
##########
# Procedure: FdInList
# Description: fill a list with the contents of a filedescriptor
# Arguments: listWidget - the widget
#            {fileInFile} - a filedescriptor to read. The descriptor
#                           is closed after reading
# Returns: none
# Sideeffects: the list widget is filled
# Notes: there exists also a function called:
#          FileInList - to open and read a file automatically
##########

  # check file existance
  if {"$fileInFile" == ""} {
    puts stderr "no filedescriptor specified"
    return
  }

  set listValue [read $fileInFile]
  close $fileInFile
  foreach fileLine [split $listValue "\n"] {
    $listWidget insert end $fileLine
  }
}

proc FileInList {listWidget {fileName ""}} {# xf ignore me 5
##########
# Procedure: FileInList
# Description: fill a list with the contents of the file
# Arguments: listWidget - the widget
#            {fileName} - filename to read
# Returns: none
# Sideeffects: the list widget is filled
# Notes: there exists also a function called:
#          FdInList - to read from an already opened filedescriptor
##########

  # check file existance
  if {"$fileName" == ""} {
    puts stderr "no filename specified"
    return
  }
  if {[catch "open $fileName r" fileInFile]} {
    puts stderr "$fileInFile"
    return
  }

  set listValue [read $fileInFile]
  close $fileInFile
  foreach fileLine [split $listValue "\n"] {
    $listWidget insert end $fileLine
  }
}

# eof

