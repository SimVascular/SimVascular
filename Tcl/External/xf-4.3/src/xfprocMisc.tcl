# Program: xf
# Description: procedures that implement misc functionality
#
# $Header: xfprocMisc.tcl[2.3] Wed Mar 10 12:07:57 1993 garfield@garfield frozen $

proc XFProcMiscAliases {} {
##########
# Procedure: XFProcMiscAliases
# Description: show/edit list of aliases
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFInfoAliases
}

proc XFProcMiscAppDefaults {xfFileName} {
##########
# Procedure: XFProcMiscAppDefaults
# Description: edit application defaults
# Arguments: xfFileName - file to edit
# Returns: none
# Sideeffects: none
##########
  global env xfMisc

  XFEditSetStatus "Calling application defaults..."

  set xfFileList ""
  if {[info exists env(XUSERFILESEARCHPATH)]} {
    append xfFileList [split $env(XUSERFILESEARCHPATH) $xfMisc(separator)]
  }
  if {[info exists env(XAPPLRESDIR)]} {
    append xfFileList [split $env(XAPPLRESDIR) $xfMisc(separator)]
  }
  if {[info exists env(XFILESEARCHPATH)]} {
    append xfFileList [split $env(XFILESEARCHPATH) $xfMisc(separator)]
  }
  append xfFileList " /usr/lib/X11/app-defaults"
  append xfFileList " /usr/X11/lib/X11/app-defaults"
  append xfFileList [file dirname $xfFileName]

  set xfClasses [string toupper [string range [file rootname [file tail $xfFileName]] 0 0]][string range [file rootname [file tail $xfFileName]] 1 end]
  lappend xfClasses [file rootname [file tail $xfFileName]]
  lappend xfClasses [file tail $xfFileName]

  foreach xfCounter1 $xfClasses {
    foreach xfCounter2 $xfFileList {
      set xfPathName $xfCounter2
      if {[regsub -all "%N" "$xfPathName" "$xfCounter1" xfResult]} {
       set xfPathName $xfResult
      }
      if {[regsub -all "%T" "$xfPathName" "app-defaults" xfResult]} {
        set xfPathName $xfResult
      }
      if {[regsub -all "%S" "$xfPathName" "" xfResult]} {
        set xfPathName $xfResult
      }
      if {[regsub -all "%C" "$xfPathName" "" xfResult]} {
        set xfPathName $xfResult
      }
      if {[file exists $xfPathName] &&
          [file readable $xfPathName] &&
          [file writable $xfPathName] &&
          ("[file type $xfPathName]" == "file" ||
           "[file type $xfPathName]" == "link")} {
        if {[catch "exec xfappdef $xfPathName &" xfResult]} {
          XFProcError $xfResult
        }
        XFEditSetStatus "Calling application defaults...done"
        return
      }
    }
  }
  exec xfappdef $xfFileName &
  XFEditSetStatus "Calling application defaults...done"
}

proc XFProcMiscHardcopy {} {
##########
# Procedure: XFProcMiscHardcopy
# Description: run the hardcopy program
# Arguments: none
# Returns: none
# Sideeffects: none
##########
  global env

  XFEditSetStatus "Calling hardcopy program..."

  exec xfhardcopy &
  XFEditSetStatus "Calling hardcopy program...done"
}

proc XFProcMiscModules {} {
##########
# Procedure: XFProcMiscModules
# Description: call module structure editor
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFEditSetStatus "Calling module structure editor..."
  XFModules
  XFEditSetStatus "Calling module structure editor...done"
}

proc XFProcMiscImages {} {
##########
# Procedure: XFProcMiscImages
# Description: show/edit list of image widgets
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  XFInfoImages ""
}

proc XFProcMiscTestProgram {} {
##########
# Procedure: XFProcMiscTestProgram
# Description: save the script under tmp name, and execute
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global xfConf
  global xfPath
  global xfStatus

  XFEditSetStatus "Testing..."
  XFSave $xfPath(tmp)/et$xfStatus(uniqueId)
  if {[catch "exec $xfConf(interpreterTest) -file $xfPath(tmp)/et$xfStatus(uniqueId) &" xfResult]} {
    XFProcError "$xfResult"
  }
  XFEditSetStatus "Testing...called"
}

# eof



