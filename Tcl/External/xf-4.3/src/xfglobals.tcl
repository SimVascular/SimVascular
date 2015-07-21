# Program: xf
# Description: global routines (not xf specific)
#
# $Header: xfglobals.tcl[2.2] Mon Mar  8 01:57:16 1993 garfield@garfield frozen $

proc Alias {args} {# xf ignore me 7
##########
# Procedure: Alias
# Description: establish an alias for a procedure
# Arguments: args - no argument means that a list of all aliases
#                   is returned. Otherwise the first parameter is
#                   the alias name, and the second parameter is
#                   the procedure that is aliased.
# Returns: nothing, the command that is bound to the alias or a
#          list of all aliases - command pairs. 
# Sideeffects: internalAliasList is updated, and the alias
#              proc is inserted
##########
  global internalAliasList

  if {[llength $args] == 0} {
    return $internalAliasList
  } {
    if {[llength $args] == 1} {
      set xfTmpIndex [lsearch $internalAliasList "[lindex $args 0] *"]
      if {$xfTmpIndex != -1} {
        return [lindex [lindex $internalAliasList $xfTmpIndex] 1]
      }
    } {
      if {[llength $args] == 2} {
        eval "proc [lindex $args 0] {args} {#xf ignore me 4
return \[eval \"[lindex $args 1] \$args\"\]}"
        set xfTmpIndex [lsearch $internalAliasList "[lindex $args 0] *"]
        if {$xfTmpIndex != -1} {
          set internalAliasList [lreplace $internalAliasList $xfTmpIndex $xfTmpIndex "[lindex $args 0] [lindex $args 1]"]
        } {
          lappend internalAliasList "[lindex $args 0] [lindex $args 1]"
        }
      } {
        error "Alias: wrong number or args: $args"
      }
    }
  }
}

proc Unalias {aliasName} {# xf ignore me 7
##########
# Procedure: Unalias
# Description: remove an alias for a procedure
# Arguments: aliasName - the alias name to remove
# Returns: none
# Sideeffects: internalAliasList is updated, and the alias
#              proc is removed
##########
  global internalAliasList

  set xfIndex [lsearch $internalAliasList "$aliasName *"]
  if {$xfIndex != -1} {
    rename $aliasName ""
    set internalAliasList [lreplace $internalAliasList $xfIndex $xfIndex]
  }
}

proc GetSelection {} {# xf ignore me 7
##########
# Procedure: GetSelection
# Description: get current selection
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  # the save way
  set xfSelection ""
  catch "selection get" xfSelection
  if {"$xfSelection" == "selection doesn't exist or form \"STRING\" not defined"} {
    return ""
  } {
    return $xfSelection
  }
}

proc NoFunction args {# xf ignore me 7
##########
# Procedure: NoFunction
# Description: do nothing (especially with scales and scrollbars)
# Arguments: args - a number of ignored parameters
# Returns: none
# Sideeffects: none
##########

}

proc SymbolicName {{xfName ""}} {# xf ignore me 7
##########
# Procedure: SymbolicName
# Description: map a symbolic name to the widget path
# Arguments: xfName
# Returns: the symbolic name
# Sideeffects: none
##########

  global symbolicName

  if {"$xfName" != ""} {
    set xfArrayName ""
    append xfArrayName symbolicName ( $xfName )
    if {![catch "set \"$xfArrayName\"" xfValue]} {
      return $xfValue
    } {
      if {"[info commands XFProcError]" != ""} {
        XFProcError "Unknown symbolic name:\n$xfName"
      } {
        puts stderr "XF error: unknown symbolic name:\n$xfName"
      }
    }
  }
  return ""
}

proc SN {{xfName ""}} {# xf ignore me 7
##########
# Procedure: SN
# Description: map a symbolic name to the widget path
# Arguments: xfName
# Returns: the symbolic name
# Sideeffects: none
##########

  SymbolicName $xfName
}

################
# BEGIN  Procedures WINDOWS compatibles
################

proc Cat {filename} {# xf ignore me 7
##########
# Procedure: Cat
# Description: emulate UNIX cat for one file
# Arguments: filename
# Returns: file contents
# Sideeffects: none
##########
global tcl_platform
if {$tcl_platform(platform) == "unix"} {exec cat $filename} {
   set fileid [open $filename "r"]
   set data [read $fileid]
   close $fileid
   return $data
}
}

proc Ls {args} {# xf ignore me 7
##########
# Procedure: Ls
# Description: emulate UNIX ls
# Arguments: like UNIX (switches authorized -F -a [-a is ignored])
# Returns: directory list
# Sideeffects: none
##########
global tcl_platform
if {$tcl_platform(platform) == "unix"} {eval exec ls $args} {
   set last [lindex $args end]
   if {[lsearch $last -* ] >= 0 || $last == "" }\
      {set path "*"}\
      {set path $last/*}
   set lst [glob $path]
   set lst1 ""
   if {[lsearch -exact $args "-F"] >= 0} then {set car "/"} else {set car ""}
   foreach f $lst {
      if {[file isdirectory $f]} \
         {set f [file tail $f]$car}\
         {set f [file tail $f]}
      lappend lst1 $f
   }
   return [lsort $lst1]
}
}

proc Rm {filename} {# xf ignore me 7
##########
# Procedure: Rm
# Description: emulate UNIX rm with DOS DEL
# Arguments: filename(s)
# Returns: nothing
# Sideeffects: none
##########
#global tcl_platform
#if {$tcl_platform(platform) == "unix"} {exec rm -f $filename} {
#   regsub -all {/} $filename {\\\\} filename
#   eval exec command.com /c del $filename >@stderr
#}
# 03/16/98 - Dennis LaBelle - Replaced above code with TCL function
file delete -force $filename
}

proc Cp {file1 file2} {# xf ignore me 7
##########
# Procedure: Cp
# Description: emulate UNIX cp with DOS COPY
# Arguments: file1 file2/directory
# Returns: nothing
# Sideeffects: none
##########
#global tcl_platform
#if {$tcl_platform(platform) == "unix"} {eval exec cp $file1 $file2} {
#   regsub -all {/} $file1 {\\\\} file1
#   regsub -all {/} $file2 {\\\\} file2
#   eval exec command.com /c copy /y $file1 $file2 >@stderr
#}
# 03/16/98 - Dennis LaBelle - Replaced above code with TCL function
file copy -force $file1 $file2
}

proc Chmod {mode file} {# xf ignore me 7
##########
# Procedure: Chmod
# Description: ignore UNIX chmod under DOS
# Arguments: file1 file2/directory
# Returns: nothing
# Sideeffects: none
##########
global tcl_platform
if {$tcl_platform(platform) == "unix"} {eval exec chmod $mode $file} {
   regsub -all {/} $file1 {\\\\} file1
   regsub -all {/} $file2 {\\\\} file2
   eval exec command.com /c copy /y $file1 $file2 >@stderr
}
}

################
# END Procedures WINDOWS compatibles
################

# eof

