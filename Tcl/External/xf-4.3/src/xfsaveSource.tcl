# Program: xf
# Description: save complete pieces pf source
#
# $Header: xfsaveSource.tcl[2.3] Wed Mar 10 12:08:24 1993 garfield@garfield frozen $

##########
# Procedure: XFSaveSourceAppendix
# Description: save the appendix. Initialisations etc.
# Arguments: xfOutFile - the output descriptor
#            {xfSavePlain} - save the output as plain tcl file
# Returns: none
# Sideeffects: none
##########
proc XFSaveSourceAppendix {xfOutFile {xfSavePlain 0}} {
  global autoLoadList
  global moduleList
  global preloadList
  global xfConf
  global xfStatus
  global xfMisc

  puts $xfOutFile "\n\n"
  if {"[array names moduleList]" != "" &&
      "[array names moduleList]" != "$xfConf(programName)"} {
    puts $xfOutFile "# module load procedure"
    puts $xfOutFile "proc XFLocalIncludeModule {{moduleName \"\"}} {" nonewline
    puts $xfOutFile [info body XFIncludeModule] nonewline
    puts $xfOutFile "}\n"
  }

  if {$xfConf(createAppdefCode)} {
    puts $xfOutFile "# application parsing procedure"
    puts $xfOutFile "proc XFLocalParseAppDefs {xfAppDefFile} {" nonewline
    puts $xfOutFile [info body XFParseAppDefs] nonewline
    puts $xfOutFile "}\n"
    puts $xfOutFile "# application loading procedure"
    puts $xfOutFile "proc XFLocalLoadAppDefs {{xfClasses \"\"} {xfPriority \"startupFile\"} {xfAppDefFile \"\"}} {" nonewline
    puts $xfOutFile [info body XFLoadAppDefs] nonewline
    puts $xfOutFile "}\n"
    puts $xfOutFile "# application setting procedure"
    puts $xfOutFile "proc XFLocalSetAppDefs {{xfWidgetPath \".\"}} {" nonewline
    puts $xfOutFile [info body XFSetAppDefs] nonewline
    puts $xfOutFile "}\n"
  }

  if {$xfConf(createClassBinding)} {
    puts $xfOutFile "# class bindings"
    foreach xfCounter $xfStatus(elementList) {
      XFSaveBind $xfOutFile $xfCounter 1
    }
    foreach xfCounter $xfStatus(additionalList) {
      XFSaveBind $xfOutFile $xfCounter 1
    }
    puts $xfOutFile ""
  }

  if {$xfConf(createFormCode)} {
    puts $xfOutFile "# form bindings"
    puts $xfOutFile "proc XFLocalFormBind {xfW xfPrevW xfNextW {xfCommand \"\"}} {" nonewline
    puts $xfOutFile [info body XFBindForms] nonewline
    puts $xfOutFile "}\n"
    puts $xfOutFile "# focus in a text/entry widget"
    puts $xfOutFile "proc XFLocalFocusIn {xfW} {" nonewline
    puts $xfOutFile [info body XFBindFocusIn] nonewline
    puts $xfOutFile "}\n"
    puts $xfOutFile "# form support procedure"
    puts $xfOutFile "proc XFLocalFormConnect {xfWList {xfCommand \"\"} {xfInitial 1}} {" nonewline
    puts $xfOutFile [info body XFBindFormConnect] nonewline
    puts $xfOutFile "}\n"
  }

  if {"[bind all]" != ""} {
    #puts $xfOutFile "# initialize bindings for all widgets"
    #puts $xfOutFile "proc XFInitAllBindings {} {"
    #XFSaveBind $xfOutFile all
    #puts $xfOutFile "}"
  }

  if {$xfConf(createPixmapCode) &&
      "[array names preloadList]" != "" &&
      "[array names preloadList]" != "xfInternal"} {
    XFSaveSourcePreloadPixmaps $xfOutFile $xfSavePlain
  }

  XFSaveSourceStartupEndProc $xfOutFile

  if {!$xfSavePlain} {
    if {"[array names moduleList]" != "" &&
        "[array names moduleList]" != "$xfConf(programName)"} {
      puts $xfOutFile ""
    }
    foreach xfCounter [lsort [array names moduleList]] {
      if {"$xfCounter" == "$xfConf(programName)"} {
        continue
      }
      if {[info exists autoLoadList($xfCounter)] &&
          $autoLoadList($xfCounter)} {
        puts $xfOutFile "if {\"\[info procs XFShowHelp\]\" != \"\"} {"
        puts $xfOutFile "  XFLocalIncludeModule $xfCounter"
        puts $xfOutFile "}"
      } {
        puts $xfOutFile "XFLocalIncludeModule $xfCounter"
      }
    }
    if {"[array names moduleList]" != "" &&
        "[array names moduleList]" != "$xfConf(programName)"} {
      puts $xfOutFile ""
    }
  }

  if {$xfConf(writeTclIndex) && !$xfSavePlain} {
    puts $xfOutFile "\n# prepare auto loading"
    puts $xfOutFile "global auto_path"
    puts $xfOutFile "global tk_library"
    puts $xfOutFile "global xfLoadPath"
#  May 11, 1997 - Dennis LaBelle 
#			Added use of xfMisc(separator) to next line
    puts $xfOutFile "foreach xfElement \[eval list \[split \$xfLoadPath $xfMisc(separator)\] \$auto_path\] {"
    puts $xfOutFile "  if {\[file exists \$xfElement/tclIndex\]} {"
    puts $xfOutFile "    lappend auto_path \$xfElement"
    puts $xfOutFile "  }"
    puts $xfOutFile "}"
#    puts $xfOutFile "catch \"unset auto_index\"\n"
#    puts $xfOutFile "catch \"unset auto_oldpath\"\n"
#    puts $xfOutFile "catch \"unset auto_execs\"\n"
  }

  if {"[info procs StartupSrc]" != ""} {
    puts $xfOutFile "\n# startup source"
    puts $xfOutFile "StartupSrc"
  }

  XFSaveGlobals $xfOutFile $xfSavePlain

  if {!$xfSavePlain} {
    puts $xfOutFile "\n# initialize global variables"
    puts $xfOutFile "InitGlobals"
    if {$xfConf(createPixmapCode) &&
        "[array names preloadList]" != "" &&
        "[array names preloadList]" != "xfInternal"} {
      puts $xfOutFile "\n# preload pixmaps"
      puts $xfOutFile "PreloadPixmaps"
    }
  }
  puts $xfOutFile "\n# display/remove toplevel windows."
  puts $xfOutFile "ShowWindow."
  foreach xfCounter [lsort [info globals xfShowWindow.*]] {
    if {$xfCounter == "xfShowWindow.tkcon"} continue
    global $xfCounter
    puts $xfOutFile "\nglobal $xfCounter"
    puts $xfOutFile "set $xfCounter [set $xfCounter]"
    if {[set $xfCounter]} {
      puts $xfOutFile "ShowWindow.[string range $xfCounter 13 \
                         [expr [string length $xfCounter]-1]]"
    }
  }

  puts $xfOutFile "\n# load default bindings."
  puts $xfOutFile "if {\[info exists env(XF_BIND_FILE)\] &&"
  puts $xfOutFile "    \"\[info procs XFShowHelp\]\" == \"\"} {"
  puts $xfOutFile "  source \$env(XF_BIND_FILE)"
  puts $xfOutFile "}"

  if {"[bind all]" != ""} {
    #puts $xfOutFile "\n# initialize bindings for all widgets."
    #puts $xfOutFile "XFInitAllBindings\n"
  }

  if {$xfConf(createAppdefCode)} {
    set xfApplicationClass [string toupper [string range [file rootname $xfConf(programName)] 0 0]][string range [file rootname $xfConf(programName)] 1 end]
    puts $xfOutFile "\n# parse and apply application defaults."
    puts $xfOutFile "XFLocalLoadAppDefs $xfApplicationClass"
    puts $xfOutFile "XFLocalSetAppDefs"
  }

  if {"[info procs EndSrc]" != ""} {
    puts $xfOutFile "\n# end source"
    puts $xfOutFile "EndSrc"
  }
}

##########
# Procedure: XFSaveSourceDestroyWindow
# Description: save widget destroy code
# Arguments: xfOutFile - the output descriptor
#            xfW - the widget to destroy
# Returns: none
# Sideeffects: none
##########
proc XFSaveSourceDestroyWindow {xfOutFile xfW} {

  puts $xfOutFile "\nproc DestroyWindow$xfW {} {# xf ignore me 7"
  puts $xfOutFile "  if {\"\[info procs XFEdit\]\" != \"\"} {"
  puts $xfOutFile "    if {\"\[info commands $xfW\]\" != \"\"} {"
  puts $xfOutFile "      global xfShowWindow$xfW"
  puts $xfOutFile "      set xfShowWindow$xfW 0"
  puts $xfOutFile "      XFEditSetPath ."
  puts $xfOutFile "      after 2 \"XFSaveAsProc $xfW; XFEditSetShowWindows\""
  puts $xfOutFile "    }"
  puts $xfOutFile "  } {"
  puts $xfOutFile "    catch \"destroy $xfW\""
  puts $xfOutFile "    update"
  puts $xfOutFile "  }"
  puts $xfOutFile "}"
}

##########
# Procedure: XFSaveSourcePrefix
# Description: save prefix code, module loading parse source etc.
# Arguments: xfOutFile - the output descriptor
#            {xfSavePlain} - save the output as plain tcl file
# Returns: none
# Sideeffects: none
##########
proc XFSaveSourcePrefix {xfOutFile {xfSavePlain 0}} {
  global xfConf
  global xfLoadPath
  global xfMisc

# December 5, 1998 -- Dennis LaBelle
#                     Added code to automatically load currently loaded extensions.

# ignore since I don't want cygwin tcl extensions! --nate

if { 0 == 1} {
  foreach extension [info loaded] {
	    set plain_name [string tolower [lindex $extension 1]]
	    if {$plain_name != "tk"} {
		   puts $xfOutFile "\n"
		   if {$plain_name == "blt"} {
			 # Account for undefined variables in BLT 2.4 (a bug in BLT, perhaps??)
			 puts $xfOutFile "set default {}"
			 puts $xfOutFile "set blt_version 2.4"
			}
		   set full_name [lindex $extension 0]
		   if {$full_name != ""} { puts $xfOutFile "catch {load $full_name}" }
		   puts $xfOutFile "catch {namespace import ${plain_name}::*}"
		 }
        }
}
  if {!$xfSavePlain} {
    puts $xfOutFile "\n# module inclusion"
    puts $xfOutFile "global env"
    puts $xfOutFile "global xfLoadPath"
    puts $xfOutFile "global xfLoadInfo"
# May 4, 1997 -- Dennis LaBelle
#		     Added definition of xfMisc(separator) to output
    puts $xfOutFile "set xfMisc(separator) $xfMisc(separator)"
    puts $xfOutFile "set xfLoadInfo 0"
    puts $xfOutFile "if {\[info exists env(XF_LOAD_PATH)\]} {"
    puts $xfOutFile "  if {\[string first \$env(XF_LOAD_PATH) $xfLoadPath\] == -1} {"
# May 4, 1997 -- Dennis LaBelle
#		     Replaced use of colon with xfMisc(separator) in next line
    puts $xfOutFile "    set xfLoadPath \$env(XF_LOAD_PATH)$xfMisc(separator)$xfLoadPath"
    puts $xfOutFile "  } {"
    puts $xfOutFile "    set xfLoadPath $xfLoadPath"
    puts $xfOutFile "  }"
    puts $xfOutFile "} {"
    puts $xfOutFile "  set xfLoadPath $xfLoadPath"
    puts $xfOutFile "}\n"
    if {$xfConf(createParseCode)} {
      puts $xfOutFile "global argc"
      puts $xfOutFile "global argv"
      puts $xfOutFile "set tmpArgv \"\""
      puts $xfOutFile "for {set counter 0} {\$counter < \$argc} {incr counter 1} {"
      puts $xfOutFile "  case \[string tolower \[lindex \$argv \$counter\]\] in {"
      puts $xfOutFile "    {-xfloadpath} {"
      puts $xfOutFile "      incr counter 1"
# May 4, 1997 -- Dennis LaBelle
#		     Replaced use of colon with xfMisc(separator) in next line
      puts $xfOutFile "      set xfLoadPath \"\[lindex \$argv \$counter\]$xfMisc(separator)\$xfLoadPath\""
      puts $xfOutFile "    }"
      puts $xfOutFile "    {-xfstartup} {"
      puts $xfOutFile "      incr counter 1"
      puts $xfOutFile "      source \[lindex \$argv \$counter\]"
      puts $xfOutFile "    }"
      puts $xfOutFile "    {-xfbindfile} {"
      puts $xfOutFile "      incr counter 1"
      puts $xfOutFile "      set env(XF_BIND_FILE) \"\[lindex \$argv \$counter\]\""
      puts $xfOutFile "    }"
      puts $xfOutFile "    {-xfcolorfile} {"
      puts $xfOutFile "      incr counter 1"
      puts $xfOutFile "      set env(XF_COLOR_FILE) \"\[lindex \$argv \$counter\]\""
      puts $xfOutFile "    }"
      puts $xfOutFile "    {-xfcursorfile} {"
      puts $xfOutFile "      incr counter 1"
      puts $xfOutFile "      set env(XF_CURSOR_FILE) \"\[lindex \$argv \$counter\]\""
      puts $xfOutFile "    }"
      puts $xfOutFile "    {-xffontfile} {"
      puts $xfOutFile "      incr counter 1"
      puts $xfOutFile "      set env(XF_FONT_FILE) \"\[lindex \$argv \$counter\]\""
      puts $xfOutFile "    }"
      puts $xfOutFile "    {-xfmodelmono} {"
      puts $xfOutFile "      tk colormodel . monochrome"
      puts $xfOutFile "    }"
      puts $xfOutFile "    {-xfmodelcolor} {"
      puts $xfOutFile "      tk colormodel . color"
      puts $xfOutFile "    }"
      puts $xfOutFile "    {-xfloading} {"
      puts $xfOutFile "      set xfLoadInfo 1"
      puts $xfOutFile "    }"
      puts $xfOutFile "    {-xfnoloading} {"
      puts $xfOutFile "      set xfLoadInfo 0"
      puts $xfOutFile "    }"
      puts $xfOutFile "    {default} {"
      puts $xfOutFile "      lappend tmpArgv \[lindex \$argv \$counter\]"
      puts $xfOutFile "    }"
      puts $xfOutFile "  }"
      puts $xfOutFile "}"
      puts $xfOutFile "set argv \$tmpArgv"
      puts $xfOutFile "set argc \[llength \$tmpArgv\]"
      puts $xfOutFile "unset counter"
      puts $xfOutFile "unset tmpArgv"
    }
  }
}

##########
# Procedure: XFSaveSourcePreloadPixmaps
# Description: save source for pixmap preloading
# Arguments: xfOutFile - the output descriptor
#            xfSavePlain - save the output as plain tcl file
# Returns: none
# Sideeffects: none
##########
proc XFSaveSourcePreloadPixmaps {xfOutFile {xfSavePlain 0}} {
  global preloadList
  global xfConf

  if {"[info commands pinfo]" == ""} {
    if {!$xfSavePlain} {
      puts $xfOutFile "\n# predefine pixmaps"
      puts $xfOutFile "proc PreloadPixmaps {} {\n"
      puts $xfOutFile "}"
    }
  } {
    if {!$xfSavePlain} {
      set leftOffset "  "
      puts $xfOutFile "\n# predefine pixmaps"
      puts $xfOutFile "proc PreloadPixmaps {} {\n"
      # make interpreter happy }
    } {
      set leftOffset ""
      puts $xfOutFile "\n# predefine pixmaps"
    }
    puts $xfOutFile "${leftOffset}if {\"\[info commands pinfo\]\" != \"\"} {"
    foreach xfCounter [lsort [pinfo names]] {
      if {[info exists preloadList($xfCounter)] &&
          "$xfCounter" != "gray25" && "$xfCounter" != "gray50"} {
        if {[pinfo depth $xfCounter] == 1} {
          puts $xfOutFile "${leftOffset}  pinfo define $xfCounter \{[pinfo data $xfCounter bitmap]\}"
        } {
          puts $xfOutFile "${leftOffset}  pinfo define $xfCounter \{[pinfo data $xfCounter xpm3]\}"
        }
      }
    }
    puts $xfOutFile "${leftOffset}}\n"
    if {!$xfSavePlain} {
      # make interpreter happy {
      puts $xfOutFile "}"
    }
  }
}

##########
# Procedure: XFSaveSourceStartupEndProc
# Description: save startup/end source procedure
# Arguments: xfOutFile - the output descriptor
# Returns: none
# Sideeffects: none
##########
proc XFSaveSourceStartupEndProc {xfOutFile} {

  if {"[info procs StartupSrc]" != ""} {
    puts $xfOutFile "\n\n# startup source"
    set xfBodyList [string trimright [info body StartupSrc]]
    puts $xfOutFile "proc StartupSrc {args} {"
    if {[string index $xfBodyList 0] == "\n"} {
      puts $xfOutFile [string range $xfBodyList 1 end]
    } {
      puts $xfOutFile $xfBodyList
    }
    puts $xfOutFile "}"
  }
  if {"[info procs EndSrc]" != ""} {
    puts $xfOutFile "\n\n# end source"
    set xfBodyList [string trimright [info body EndSrc]]
    puts $xfOutFile "proc EndSrc {} {"
    if {[string index $xfBodyList 0] == "\n"} {
      puts $xfOutFile [string range $xfBodyList 1 end]
    } {
      puts $xfOutFile $xfBodyList
    }
    puts $xfOutFile "}"
  }
}

# eof

