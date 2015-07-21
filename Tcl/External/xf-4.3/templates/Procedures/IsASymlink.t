# XFNoParsing
# Program: template
# Description: check if name is a symbolic link
#
# $Header: IsASymlink.t[2.3] Wed Mar 10 12:03:17 1993 garfield@garfield frozen $

proc IsASymlink {fileName} {# xf ignore me 5
##########
# Procedure: IsASymlink
# Description: check if filename is a symbolic link
# Arguments: fileName - the path/filename to check
# Returns: none
# Sideeffects: none
##########

  catch "file type $fileName" fileType
  if {"$fileType" == "link"} {
    return 1
  }
  return 0
}

# eof

