# XFNoParsing
# Program: template
# Description: put a given file into a text
#
# $Header: FdInText.t[2.3] Wed Mar 10 12:02:58 1993 garfield@garfield frozen $

proc FdInText {textWidget {fileInFile ""}} {# xf ignore me 5
##########
# Procedure: FdInText
# Description: fill a text with the contents of a filedescriptor
# Arguments: textWidget - the widget
#            {fileInFile} - a filedescriptor to read. The descriptor
#                           is closed after reading
# Returns: none
# Sideeffects: the text widget is filled
# Notes: there exists also a function called:
#          FileInText - to open and read a file automatically
##########

  # check file existance
  if {"$fileInFile" == ""} {
    puts stderr "no filedescriptor specified"
    return
  }

  set textValue [read $fileInFile]
  $textWidget insert end "$textValue"
  close $fileInFile
}

proc FileInText {textWidget {fileName ""}} {# xf ignore me 5
##########
# Procedure: FileInText
# Description: fill a text with the contents of the file
# Arguments: textWidget - the widget
#            {fileName} - filename to read
# Returns: none
# Sideeffects: the text widget is filled
# Notes: there exists also a function called:
#          FdInText - to read from an already opened filedescriptor
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

  set textValue [read $fileInFile]
  $textWidget insert end "$textValue"
  close $fileInFile
}

# eof

