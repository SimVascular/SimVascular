# Program: xf
# Description: external interface to save facilities
#
# $Header: xfsave.tcl[2.5] Wed Mar 10 12:08:14 1993 garfield@garfield frozen $

auto_load XFSaveBind
auto_load XFSaveSourceAppendix

##########
# Procedure: XFSave
# Description: save current definition
# Arguments: xfFileName - the output file
# Returns: none
# Sideeffects: none
##########
proc XFSave {xfFileName} {
  global xfConf
  global xfMisc
  global xfPath
  global xfStatus

  if {$xfStatus(saving)} {
    return
  }
  set xfStatus(saving) 1
  XFMiscUpdateModuleList
  # clear up the window showing stuff
  XFEditSetShowWindows

  # save current file
  if {[file exists $xfFileName]} {
    catch "file copy -force $xfFileName $xfFileName~"
    catch "file copy $xfFileName $xfPath(tmp)/lc$xfStatus(uniqueId)"
  }
  if {[catch "open $xfFileName w" xfOutFile]} {
    XFProcError "$xfOutFile"
  } {
    fconfigure $xfOutFile -translation lf
    XFSaveComment $xfOutFile file [file rootname [file tail $xfFileName]]
    XFSaveSourcePrefix $xfOutFile 1
    puts $xfOutFile "\n\npackage require tile\n\n"
    # save all visible children of .
    foreach xfCounter [lsort [winfo children .]] {
      if {![string match ".tkcon*" $xfCounter] && ![string match ".xf*" $xfCounter] &&
          ![string match "xf*" [winfo name $xfCounter]] &&
          "[winfo class $xfCounter]" == "Toplevel"} {
        XFSaveRootToplevelChild $xfOutFile $xfCounter
        XFSaveSourceDestroyWindow $xfOutFile $xfCounter
        XFSaveShowWindowTail $xfOutFile $xfCounter
      }
    }
    # save all invisible children of .
    foreach xfProcName [lsort [info procs ShowWindow.*]] {
      XFSaveShowWindow $xfOutFile $xfProcName
    }
    # save .
    set xfMisc(menuBarTraversalList) ""
    set xfMisc(specialSaveString) ""
    puts $xfOutFile "\n\n# procedure to show window ."
    puts $xfOutFile "proc ShowWindow. {args} {# xf ignore me 7"
    if {"[info procs StartupSrc.]" != ""} {
      puts $xfOutFile "\nStartupSrc."
    }
    XFSaveWidget $xfOutFile .
    XFSaveBind $xfOutFile .
    XFSaveWidgetSpecial $xfOutFile .
    XFSaveSubwindow $xfOutFile .
    if {"[info procs MiddleSrc.]" != ""} {
      puts $xfOutFile "\nMiddleSrc."
    }
    XFSavePack $xfOutFile .
    XFSavePlace $xfOutFile .
    if {"$xfMisc(specialSaveString)" != ""} {
      puts $xfOutFile "\n$xfMisc(specialSaveString)\n"
    }
    if {"[info procs EndSrc.]" != ""} {
      puts $xfOutFile "\nEndSrc."
    }
    puts $xfOutFile "\n  if {\"\[info procs XFEdit\]\" != \"\"} {"
    puts $xfOutFile "    catch \"XFMiscBindWidgetTree .\""
    puts $xfOutFile "    after 2 \"catch \{XFEditSetShowWindows\}\""
    puts $xfOutFile "  }"
    puts $xfOutFile "}"
    XFSaveShowWindowTail $xfOutFile .

    # save procedures
    foreach xfProcName [lsort [info procs]] {
      if {[string range $xfProcName 0 3] == "tcl_"} continue
      XFSaveProc $xfOutFile $xfProcName
    }

    # save images
    foreach xfImgName [image names] {
      XFSaveImg $xfOutFile $xfImgName
    }

    XFSaveSourceAppendix $xfOutFile 1
    puts $xfOutFile "\n# eof"
    puts $xfOutFile "#"
    puts $xfOutFile ""
    close $xfOutFile
  }
  catch "chmod u+x $xfFileName"
  set xfStatus(saving) 0
}

##########
# Procedure: XFSaveModules
# Description: save current definition splitted in modules
# Arguments: xfFileName - the output file
# Returns: none
# Sideeffects: none
##########
proc XFSaveModules {xfFileName} {
  global autoLoadList
  global moduleList
  global xfConf
  global xfMisc
  global xfPath
  global xfSaveModuleList
  global xfStatus

  if {$xfStatus(saving)} {
    return
  }
  set xfStatus(saving) 1
  XFMiscUpdateModuleList
  # clear up the window showing stuff
  XFEditSetShowWindows

  # save modules
  foreach xfModFileName [lsort [array names moduleList]] {
    if {"$xfModFileName" == $xfConf(programName) ||
        ([llength $xfSaveModuleList] > 0 &&
         [lsearch $xfSaveModuleList $xfModFileName] == -1)} {
      continue
    }
    if {[file exists $xfModFileName]} {
      catch "Cp $xfModFileName $xfModFileName~"
    }
    if {[catch "open $xfModFileName w" xfOutFile]} {
      XFProcError "$xfOutFile"
    } {
      XFSaveComment $xfOutFile module $xfModFileName
      puts $xfOutFile "\n# module contents"
      puts $xfOutFile "global moduleList"
      puts $xfOutFile "global autoLoadList"
      if {[info exists moduleList($xfModFileName)]} {
        #puts $xfOutFile "set moduleList($xfModFileName) \{[set moduleList($xfModFileName)]\}"
      }
      if {[info exists autoLoadList($xfModFileName)]} {
        #puts $xfOutFile "set autoLoadList($xfModFileName) \{[set autoLoadList($xfModFileName)]\}"
      }

      puts $xfOutFile "\n# procedures to show toplevel windows"

      # save all visible children of . in this module
      foreach xfCounter [lsort [winfo children .]] {
        if {[lsearch [set moduleList($xfModFileName)] $xfCounter] >= 0} {
          XFSaveRootToplevelChild $xfOutFile $xfCounter
          XFSaveSourceDestroyWindow $xfOutFile $xfCounter
          XFSaveShowWindowTail $xfOutFile $xfCounter
        }
      }
      # save all invisible children of . in this module
      foreach xfProcName [lsort [info procs ShowWindow.*]] {
        if {[lsearch [set moduleList($xfModFileName)] \
              [string range $xfProcName 10 end]] >= 0} {
          XFSaveShowWindow $xfOutFile $xfProcName
        }
      }

      # save .
      set xfMisc(menuBarTraversalList) ""
      set xfMisc(specialSaveString) ""
      if {[lsearch [set moduleList($xfModFileName)] .] >= 0} {
        puts $xfOutFile "\n\n# procedure to show window ."
        puts $xfOutFile "proc ShowWindow. {args} {# xf ignore me 7"
        if {"[info procs StartupSrc.]" != ""} {
          puts $xfOutFile "\nStartupSrc."
        }
        XFSaveWidget $xfOutFile .
        XFSaveBind $xfOutFile .
        XFSaveWidgetSpecial $xfOutFile .
        XFSaveSubwindow $xfOutFile .
        if {"[info procs MiddleSrc.]" != ""} {
          puts $xfOutFile "\nMiddleSrc."
        }
        XFSavePack $xfOutFile .
        XFSavePlace $xfOutFile .
        if {"$xfMisc(specialSaveString)" != ""} {
          puts $xfOutFile "\n$xfMisc(specialSaveString)\n"
        }
        if {"[info procs EndSrc.]" != ""} {
          puts $xfOutFile "\nEndSrc."
        }
        puts $xfOutFile "\n  if {\"\[info procs XFEdit\]\" != \"\"} {"
        puts $xfOutFile "    catch \"XFMiscBindWidgetTree .\""
        puts $xfOutFile "    after 2 \"catch \{XFEditSetShowWindows\}\""
        puts $xfOutFile "  }"
        puts $xfOutFile "}"
        XFSaveShowWindowTail $xfOutFile .
      }

      # save procedures
      puts $xfOutFile "\n\n# User defined procedures"
      foreach xfProcName [set moduleList($xfModFileName)] {
        if {"[info procs $xfProcName]" != ""} {
          if {[XFMiscIsXFSpecialElement $xfProcName]} {
            continue
          }
          XFSaveProc $xfOutFile $xfProcName
        }
      }

      # save images
# 03/16/98 - Dennis LaBelle - Commented the following 4 lines to limit image
#                             saves to the main module.
#      puts $xfOutFile "\n\n# User defined images"
#      foreach xfImgName [image names] {
#          XFSaveImg $xfOutFile $xfImgName
#      }

      puts $xfOutFile "\n\n# Internal procedures"
      foreach xfProcName [set moduleList($xfModFileName)] {
        if {"[info procs $xfProcName]" != ""} {
          if {[XFMiscIsXFSpecialElement $xfProcName]} {
            XFSaveProc $xfOutFile $xfProcName
          }
        }
      }
      puts $xfOutFile "\n# eof"
      puts $xfOutFile "#"
      puts $xfOutFile ""
      close $xfOutFile
    }
  }

  # save main part
  if {([llength $xfSaveModuleList] > 0 &&
       [lsearch $xfSaveModuleList $xfFileName] == -1) ||
       [llength $xfSaveModuleList] == 0} {
    if {[file exists $xfFileName]} {
      catch "Cp $xfFileName $xfFileName~"
      catch "Cp $xfFileName $xfPath(tmp)/lc$xfStatus(uniqueId)"
    }
    if {[catch "open $xfFileName w" xfOutFile]} {
      XFProcError "$xfOutFile"
    } {
      XFSaveComment $xfOutFile file [file rootname $xfConf(programName)]
      XFSaveSourcePrefix $xfOutFile

      # save all visible children of . in this module
      foreach xfCounter [lsort [winfo children .]] {
        if {[XFMiscInModule $xfCounter] == 0 &&
            [XFMiscIsXFElement $xfCounter] == 0 &&
            "[winfo class $xfCounter]" == "Toplevel"} {
          XFSaveRootToplevelChild $xfOutFile $xfCounter
          XFSaveSourceDestroyWindow $xfOutFile $xfCounter
          XFSaveShowWindowTail $xfOutFile $xfCounter
        }
      }
      # save all invisible children of . in this module
      foreach xfProcName [lsort [info procs ShowWindow.*]] {
        if {[XFMiscInModule [string range $xfProcName 10 end]] == 0 &&
            [XFMiscIsXFElement [string range $xfProcName 10 end]] == 0} {
          XFSaveShowWindow $xfOutFile $xfProcName
        }
      }

      # save .
      if {[XFMiscInModule .] == 0 &&
          [XFMiscIsXFElement .] == 0} {
        set xfMisc(menuBarTraversalList) ""
        set xfMisc(specialSaveString) ""
        puts $xfOutFile "\n\n# procedure to show window ."
        puts $xfOutFile "proc ShowWindow. {args} {# xf ignore me 7"
        if {"[info procs StartupSrc.]" != ""} {
          puts $xfOutFile "\nStartupSrc."
        }
        XFSaveWidget $xfOutFile .
        XFSaveBind $xfOutFile .
        XFSaveWidgetSpecial $xfOutFile .
        XFSaveSubwindow $xfOutFile .
        if {"[info procs MiddleSrc.]" != ""} {
          puts $xfOutFile "\nMiddleSrc."
        }
        XFSavePack $xfOutFile .
        XFSavePlace $xfOutFile .
        if {"$xfMisc(specialSaveString)" != ""} {
          puts $xfOutFile "\n$xfMisc(specialSaveString)\n"
        }
        if {"[info procs EndSrc.]" != ""} {
          puts $xfOutFile "\nEndSrc."
        }
        puts $xfOutFile "\n  if {\"\[info procs XFEdit\]\" != \"\"} {"
        puts $xfOutFile "    catch \"XFMiscBindWidgetTree .\""
        puts $xfOutFile "    after 2 \"catch \{XFEditSetShowWindows\}\""
        puts $xfOutFile "  }"
        puts $xfOutFile "}"
        XFSaveShowWindowTail $xfOutFile .
      }

      # save procedures
      puts $xfOutFile "\n\n# User defined procedures"
      set xfTmpList ""
      foreach xfProcName [set moduleList($xfConf(programName))] {
        if {[XFMiscInModule $xfProcName] == 0 &&
            [XFMiscIsXFElement $xfProcName] == 0 &&
            "[info procs $xfProcName]" != ""} {
          if {[XFMiscIsXFSpecialElement $xfProcName]} {
            continue
          }
          XFSaveProc $xfOutFile $xfProcName
          lappend xfTmpList "$xfProcName"
        }
      }
      foreach xfProcName [lsort [info procs]] {
        if {[XFMiscInModule $xfProcName] == 0 &&
            [XFMiscIsXFElement $xfProcName] == 0 &&
            [lsearch $xfTmpList $xfProcName] == -1} {
          if {[XFMiscIsXFSpecialElement $xfProcName]} {
            continue
          }
          XFSaveProc $xfOutFile $xfProcName
        }
      }

      # save images
      puts $xfOutFile "\n\n# User defined images"
      foreach xfImgName [image names] {
          XFSaveImg $xfOutFile $xfImgName
      }

      puts $xfOutFile "\n\n# Internal procedures"
      set xfTmpList ""
      foreach xfProcName [set moduleList($xfConf(programName))] {
        if {[XFMiscInModule $xfProcName] == 0 &&
            [XFMiscIsXFElement $xfProcName] == 0 &&
            "[info procs $xfProcName]" != ""} {
          if {[XFMiscIsXFSpecialElement $xfProcName]} {
            XFSaveProc $xfOutFile $xfProcName
            lappend xfTmpList "$xfProcName"
          }
        }
      }
      foreach xfProcName [lsort [info procs]] {
        if {[XFMiscInModule $xfProcName] == 0 &&
            [XFMiscIsXFElement $xfProcName] == 0 &&
            [lsearch $xfTmpList $xfProcName] == -1} {
          if {[XFMiscIsXFSpecialElement $xfProcName]} {
            XFSaveProc $xfOutFile $xfProcName
          }
        }
      }
 
      XFSaveSourceAppendix $xfOutFile
      puts $xfOutFile "\n# eof"
      puts $xfOutFile "#"
      puts $xfOutFile ""
      close $xfOutFile
    }
    catch "chmod u+x $xfFileName"
  }
  set xfStatus(saving) 0
}

##########
# Procedure: XFSaveModuleList
# Description: save current save module selection
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFSaveModuleList {} {
  global xfSaveModuleList

  if {[catch "open .xf-save-modules w" xfOutFile]} {
    XFProcError "$xfOutFile"
  } {
    puts $xfOutFile "global xfSaveModuleList"
    puts $xfOutFile "set xfSaveModuleList {[set xfSaveModuleList]}"
    puts $xfOutFile "\n# eof"
    puts $xfOutFile "#"
    puts $xfOutFile ""
    close $xfOutFile
  }
}

##########
# Procedure: XFSaveAsProc
# Description: save selected toplevel to procedure
# Arguments: xfW
# Returns: none
# Sideeffects: none
##########
proc XFSaveAsProc {xfW} {
  global xfConf
  global xfPath
  global xfStatus

  if {$xfStatus(saving)} {
    return
  }
  set xfStatus(saving) 1

  if {"[info commands $xfW]" != ""} {
    if {[catch "open $xfPath(tmp)/tp$xfStatus(uniqueId) w" xfOutFile]} {
      XFProcError "Could not create toplevel procedure\nI will not remove this toplevel\n$xfOutFile"
    } {
      XFSaveRootToplevelChild $xfOutFile $xfW
      XFSaveSourceDestroyWindow $xfOutFile $xfW
      close $xfOutFile

      if {[catch "source $xfPath(tmp)/tp$xfStatus(uniqueId)" xfResult]} {
        XFProcError "Could not create toplevel procedure\nI will not remove this toplevel\n$xfResult"
      } {
        XFDestroy $xfW
        update
        catch "Rm $xfPath(tmp)/tp$xfStatus(uniqueId)"
      }
    }
  }
  set xfStatus(saving) 0
}

##########
# Procedure: XFSaveProcsTmplt
# Description: save procedures in template file
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFSaveProcsTmplt {} {
  global xfPath
  global xfStatus

  if {[catch "open $xfPath(tmp)/tb$xfStatus(uniqueId) a" xfOutFile] != 0} {
    XFProcError "$xfOutFile"
  } {
    puts $xfOutFile "# XFInternalString"
    puts $xfOutFile "# Procedures"
    foreach xfProcName $xfStatus(tmpltList) {
      if {[catch "winfo class $xfProcName"]} {
        XFSaveProc $xfOutFile $xfProcName
      }
    }
    close $xfOutFile
  }
}

##########
# Procedure: XFSaveScript
# Description: save a startup script
# Arguments: xfProgramName - the prograqm name
# Returns: none
# Sideeffects: none
##########
proc XFSaveScript {xfProgramName} {
  global xfConf
  global xfLoadPath

  if {![catch "open [file rootname $xfProgramName].sh w" outFile]} {
    puts $outFile "#!/bin/sh"
    puts $outFile "#"
    puts $outFile "# Program: [file rootname $xfProgramName]"
    puts $outFile "#"
    puts $outFile "# This file is an automatically created shell script"
    puts $outFile "# for starting the application named: $xfProgramName"
    puts $outFile "#"
    puts $outFile "# adapt the following variables to fit your"
    puts $outFile "# local site"
    puts $outFile "#"
    puts $outFile "# WHIS_CMD is the wish interpreter to use"
    puts $outFile "WISH_CMD=$xfConf(interpreter)"
    puts $outFile "#"
    puts $outFile "# XF_LOAD_PATH is the path were the tcl modules"
    puts $outFile "# for this application are located"
    puts $outFile "if test \"\$XF_LOAD_PATH\" = \"\"; then"
    puts $outFile "  XF_LOAD_PATH=$xfLoadPath"
    puts $outFile "else"
    puts $outFile "  XF_LOAD_PATH=\$XF_LOAD_PATH:$xfLoadPath"
    puts $outFile "fi"
    puts $outFile "#"
    puts $outFile "#"
    puts $outFile "ARGC=\$#"
    puts $outFile "COMMANDLINE="
    puts $outFile "while \[ \$ARGC -gt 0 \]; do"
    puts $outFile "  C=\$1"
    puts $outFile "  shift"
    puts $outFile "  ARGC=`expr \$ARGC - 1`"
    puts $outFile "  case \$C in"
    puts $outFile "    -xfloadpath)"
    puts $outFile "      if \[ \$ARGC -gt 0 \]; then"
    puts $outFile "        C=\$1"
    puts $outFile "        shift"
    puts $outFile "        ARGC=`expr \$ARGC - 1`"
    puts $outFile "        XF_LOAD_PATH=\$C:\$XF_LOAD_PATH"
    puts $outFile "      else"
    puts $outFile "        echo \"$xfProgramName: expected path for -xfloadpath\""
    puts $outFile "        exit 2"
    puts $outFile "      fi;;"
    puts $outFile "    *)"
    puts $outFile "      COMMANDLINE=\$COMMANDLINE\" \"\$C;;"
    puts $outFile "  esac"
    puts $outFile "done"
    puts $outFile "#"
    puts $outFile "export XF_LOAD_PATH"
    puts $outFile "for p in `echo \$XF_LOAD_PATH|awk 'BEGIN{RS=\":\"}"
    puts $outFile "{print \$0}'`; do"
    puts $outFile "  if test -f \$p/$xfProgramName; then "
    puts $outFile "    exec \$WISH_CMD -name [file rootname $xfProgramName] -file \$p/$xfProgramName \$COMMANDLINE "
    puts $outFile "  fi"
    puts $outFile "done"
    puts $outFile "echo \"Could not find: $xfProgramName\""
    puts $outFile "# eof"
    puts $outFile ""
    close $outFile
    catch "chmod u+x [file rootname $xfProgramName].sh"
  } {
    XFProcError "Could not open [file rootname $xfProgramName].sh"
  }
}

##########
# Procedure: XFSaveSubTree
# Description: save the widget sub tree
# Arguments: xfW - the widget
#            xfFileName - the output file
#            xfSaveAsProc - save with variable as pathname
# Returns: none
# Sideeffects: none
##########
proc XFSaveSubTree {xfW xfFileName {xfSaveAsProc 0}} {
  global symbolicName
  global xfMisc
  global xfAppDefToplevels

  if {"$xfW" == "."} {
    XFProcError "Sorry, no cutting from ."
    return
  }
  if {[catch "open $xfFileName w" xfOutFile]} {
    XFProcError "$xfOutFile"
  } {
    set xfMisc(menuBarTraversalList) ""
    set xfMisc(specialSaveString) ""
    puts $xfOutFile "# $xfW"
    puts $xfOutFile "# The above line makes pasting MUCH easier for me."
    puts $xfOutFile "# It contains the pathname of the cutted widget."
    XFSaveComment $xfOutFile template [file rootname [file tail $xfFileName]]
    if {$xfSaveAsProc} {
      puts $xfOutFile "\nproc V[winfo class $xfW]$xfW { insertWidgetPath args} {"
      puts $xfOutFile "\n  set xfCounter 0"
      puts $xfOutFile "  set xfLength \[llength \$args\]"
      puts $xfOutFile "  while {\$xfCounter < \$xfLength} {"
      puts $xfOutFile "    set xfElement \[lindex \$args \$xfCounter\]"
      puts $xfOutFile "    if {\"\$xfElement\" == \"-startupSrc\" ||"
      puts $xfOutFile "        \"\$xfElement\" == \"-middleSrc\" ||"
      puts $xfOutFile "        \"\$xfElement\" == \"-endSrc\"} {"
      puts $xfOutFile "      if {\$xfLength > \[expr \$xfCounter+1\]} {"
      puts $xfOutFile "        incr xfCounter"
      puts $xfOutFile "        set xfSource(\$xfElement) \[lindex \$args \$xfCounter\]"
      puts $xfOutFile "      }"
      puts $xfOutFile "    } {"
      puts $xfOutFile "      if {\[string match -* \$xfElement\]} {"
      puts $xfOutFile "        if {\$xfLength > \[expr \$xfCounter+1\]} {"
      puts $xfOutFile "          incr xfCounter"
      puts $xfOutFile "          set xfGenResource(\$xfElement) \[lindex \$args \$xfCounter\]"
      puts $xfOutFile "        }"
      puts $xfOutFile "      } {"
      puts $xfOutFile "        if {\[string match .* \$xfElement\]} {"
      puts $xfOutFile "          if {\$xfLength > \[expr \$xfCounter+2\]} {"
      puts $xfOutFile "            incr xfCounter"
      puts $xfOutFile "            set xfSpecResource(\$xfElement) \[lindex \$args \$xfCounter\]"
      puts $xfOutFile "            incr xfCounter"
      puts $xfOutFile "            lappend xfSpecResource(\$xfElement) \[lindex \$args \$xfCounter\]"
      puts $xfOutFile "          }"
      puts $xfOutFile "        }"
      puts $xfOutFile "      }"
      puts $xfOutFile "    }"
      puts $xfOutFile "    incr xfCounter"
      puts $xfOutFile "  }"
      puts $xfOutFile "\n  if {\"\[info commands \$insertWidgetPath\]\" == \"\"} {"
      puts $xfOutFile "  if {\[info exists xfSource(-startupSrc)\]} {"
      puts $xfOutFile "    if {\[catch \"\$xfSource(-startupSrc) \$insertWidgetPath\" xfResult\]} {"
      puts $xfOutFile "      puts stderr \$xfResult"
      puts $xfOutFile "    }"
      puts $xfOutFile "  }"
      puts $xfOutFile "  set widgetCode {"
      # make interpreter happy }}}
    }
    XFSaveWidget $xfOutFile $xfW
    XFSaveBind $xfOutFile $xfW
    XFSaveWidgetSpecial $xfOutFile $xfW
    XFSaveSubwindow $xfOutFile $xfW
    if {$xfSaveAsProc} {
      # make interpreter happy {
      puts $xfOutFile "  }"
      puts $xfOutFile "  set subst \"\""
      puts $xfOutFile "  append subst \\\\ \[string trim \{ $xfW \}\]"
      puts $xfOutFile "  regsub -all \$subst \$widgetCode \$insertWidgetPath widgetCode"
      puts $xfOutFile "  regsub -all {%ThisTopWidget} \$widgetCode \$insertWidgetPath widgetCode"
      puts $xfOutFile "  eval \$widgetCode"
      puts $xfOutFile "\n  if {\[info exists xfSource(-middleSrc)\]} {"
      puts $xfOutFile "    if {\[catch \"\$xfSource(-middleSrc) \$insertWidgetPath\" xfResult\]} {"
      puts $xfOutFile "      puts stderr \$xfResult"
      puts $xfOutFile "    }"
      puts $xfOutFile "  }"
      puts $xfOutFile "  set geometryCode {"
      # make interpreter happy }
    }
    XFSavePack $xfOutFile $xfW
    XFSavePlace $xfOutFile $xfW
    set xfParent [string range $xfW 0 [expr [string last . $xfW]-1]]
    if {"$xfParent" == ""} {
      set xfParent .
    }
    XFSavePackOne $xfOutFile $xfParent $xfW $xfSaveAsProc
    XFSavePlaceOne $xfOutFile $xfParent $xfW $xfSaveAsProc
    if {[lsearch $xfAppDefToplevels $xfW] != -1} {
      puts $xfOutFile "  if {\"\[info procs XFLocalSetAppDefs\]\" != \"\"} {"
      puts $xfOutFile "    XFLocalSetAppDefs"
      puts $xfOutFile "  }"
    }
    if {"$xfMisc(specialSaveString)" != ""} {
      puts $xfOutFile "\n$xfMisc(specialSaveString)\n"
    }
    if {$xfSaveAsProc} {
      # make interpreter happy {{
      puts $xfOutFile "  }"
      puts $xfOutFile "  set subst \"\""
      puts $xfOutFile "  append subst \\\\ \[string trim \{ $xfW \}\]"
      puts $xfOutFile "  regsub -all \$subst \$geometryCode \$insertWidgetPath geometryCode"
      puts $xfOutFile "  regsub -all {\\\$insertWidgetPath} \$geometryCode \[winfo parent \$insertWidgetPath\] geometryCode"
      puts $xfOutFile "  eval \$geometryCode"
      puts $xfOutFile "\n  if {\[info exists xfSource(-endSrc)\]} {"
      puts $xfOutFile "    if {\[catch \"\$xfSource(-endSrc) \$insertWidgetPath\" xfResult\]} {"
      puts $xfOutFile "      puts stderr \$xfResult"
      puts $xfOutFile "    }"
      puts $xfOutFile "  }"
      puts $xfOutFile "  }"
      puts $xfOutFile "\n  if {\[info exists xfGenResource\]} {"
      puts $xfOutFile "    set xfWidgetList \"\""
      puts $xfOutFile "    set xfTmpWidgetList \$insertWidgetPath"
      puts $xfOutFile "    while {1} {"
      puts $xfOutFile "      if {\[llength \$xfTmpWidgetList\] == 0} {"
      puts $xfOutFile "        break"
      puts $xfOutFile "      }"
      puts $xfOutFile "      set xfFirstWidget \[lindex \$xfTmpWidgetList 0\]"
      puts $xfOutFile "      lappend xfWidgetList \$xfFirstWidget"
      puts $xfOutFile "      set xfTmpWidgetList \[lreplace \$xfTmpWidgetList 0 0\]"
      puts $xfOutFile "      if {\"\[winfo children \$xfFirstWidget\]\" != \"\"} {"
      puts $xfOutFile "        eval lappend xfTmpWidgetList \[winfo children \$xfFirstWidget\]"
      puts $xfOutFile "      }"
      puts $xfOutFile "    }"
      puts $xfOutFile "    foreach xfCounter \$xfWidgetList {"
      puts $xfOutFile "      if {\[info exists xfGenResource\]} {"
      puts $xfOutFile "        foreach xfResource \[array names xfGenResource\] {"
      puts $xfOutFile "          catch \"\$xfCounter config \$xfResource \[set xfGenResource(\$xfResource)\]\""
      puts $xfOutFile "        }"
      puts $xfOutFile "      }"
      puts $xfOutFile "    }"
      puts $xfOutFile "  }"
      puts $xfOutFile "  if {\[info exists xfSpecResource\]} {"
      puts $xfOutFile "    foreach xfCounter \[array names xfSpecResource\] {"
      puts $xfOutFile "      if {\"\[info commands \$xfCounter\]\" != \"\"} {"
      puts $xfOutFile "        catch \"\$xfCounter config \[lindex \$xfSpecResource(\$xfCounter) 0\] \[lindex \$xfSpecResource(\$xfCounter) 1\]\""
      puts $xfOutFile "      }"
      puts $xfOutFile "    }"
      puts $xfOutFile "  }"
      puts $xfOutFile "\n  if {\"\[info procs XFEdit\]\" != \"\"} {"
      puts $xfOutFile "    catch \"XFMiscBindWidgetTree \$insertWidgetPath\""
      puts $xfOutFile "    after 2 \"catch \{XFEditSetShowWindows\}\""
      puts $xfOutFile "  }"
      puts $xfOutFile "  return \$insertWidgetPath"
      # make interpreter happy {
      puts $xfOutFile "}"
    }
    puts $xfOutFile "# end of widget tree\n"
    close $xfOutFile
  }
}

##########
# Procedure: XFSaveTclIndex
# Description: save the tclIndex file
# Arguments: xfOutFile - the output descriptor
#            xfParent - the parent widget
# Returns: none
# Sideeffects: none
##########
proc XFSaveTclIndex {} {
  global autoLoadList
  global moduleList
  global xfConf

  set xfTclIndex ""
  foreach xfCounter [array names moduleList] {
    if {"$xfCounter" == "$xfConf(programName)"} {
      continue
    }
    if {"$moduleList($xfCounter)" != ""} {
      append xfTclIndex "# $xfCounter\n"
    }
    foreach xfElement $moduleList($xfCounter) {
      if {[catch "winfo class $xfElement"]} {
        if {$xfConf(writeNewTclIndex)} {
          if {"[info procs ShowWindow$xfElement]" != ""} {
	    append xfTclIndex "set [list auto_index(ShowWindow$xfElement)]"
	    append xfTclIndex " \"source \$dir/$xfCounter\"\n"
          } {
	    append xfTclIndex "set [list auto_index($xfElement)]"
	    append xfTclIndex " \"source \$dir/$xfCounter\"\n"
          }
          if {"[info procs DestroyWindow$xfElement]" != ""} {
	    append xfTclIndex "set [list auto_index(DestroyWindow$xfElement)]"
	    append xfTclIndex " \"source \$dir/$xfCounter\"\n"
          } {
	    append xfTclIndex "set [list auto_index($xfElement)]"
	    append xfTclIndex " \"source \$dir/$xfCounter\"\n"
          }
        } {
          if {"[info procs ShowWindow$xfElement]" != ""} {
            append xfTclIndex "ShowWindow$xfElement $xfCounter\n"
          } {
            append xfTclIndex "$xfElement $xfCounter\n"
          }
          if {"[info procs DestroyWindow$xfElement]" != ""} {
            append xfTclIndex "DestroyWindow$xfElement $xfCounter\n"
          } {
            append xfTclIndex "$xfElement $xfCounter\n"
          }
        }
      } {
        if {$xfConf(writeNewTclIndex)} {
          if {"[info procs ShowWindow$xfElement]" != ""} {
	    append xfTclIndex "set [list auto_index(ShowWindow$xfElement)]"
	    append xfTclIndex " \"source \$dir/$xfCounter\"\n"
          }
          if {"[info procs DestroyWindow$xfElement]" != ""} {
	    append xfTclIndex "set [list auto_index(DestroyWindow$xfElement)]"
	    append xfTclIndex " \"source \$dir/$xfCounter\"\n"
          }
          if {"[info procs StartupSrc$xfElement]" != ""} {
	    append xfTclIndex "set [list auto_index(StartupSrc$xfElement)]"
	    append xfTclIndex " \"source \$dir/$xfCounter\"\n"
          }
          if {"[info procs MiddleSrc$xfElement]" != ""} {
	    append xfTclIndex "set [list auto_index(MiddleSrc$xfElement)]"
	    append xfTclIndex " \"source \$dir/$xfCounter\"\n"
          }
          if {"[info procs EndSrc$xfElement]" != ""} {
	    append xfTclIndex "set [list auto_index(EndSrc$xfElement)]"
	    append xfTclIndex " \"source \$dir/$xfCounter\"\n"
          }
        } {
          if {"[info procs ShowWindow$xfElement]" != ""} {
            append xfTclIndex "ShowWindow$xfElement $xfCounter\n"
          }
          if {"[info procs DestroyWindow$xfElement]" != ""} {
            append xfTclIndex "DestroyWindow$xfElement $xfCounter\n"
          }
          if {"[info procs StartupSrc$xfElement]" != ""} {
            append xfTclIndex "StartupSrc$xfElement $xfCounter\n"
          }
          if {"[info procs MiddleSrc$xfElement]" != ""} {
            append xfTclIndex "MiddleSrc$xfElement $xfCounter\n"
          }
          if {"[info procs EndSrc$xfElement]" != ""} {
            append xfTclIndex "EndSrc$xfElement $xfCounter\n"
          }
        }
      }
    }
    if {"$moduleList($xfCounter)" != ""} {
      append xfTclIndex "\n"
    }
  }

  if {"$xfTclIndex" != ""} {
    if {[catch "open tclIndex w" xfOutFile]} {
      XFProcError "$xfOutFile"
    } {
      if {$xfConf(writeNewTclIndex)} {
        puts $xfOutFile "# Tcl autoload index file, version 2.0"
        puts $xfOutFile "# This file is generated by the \"auto_mkindex\" command"
        puts $xfOutFile "# and sourced to set up indexing information for one or"
        puts $xfOutFile "# more commands.  Typically each line is a command that"
        puts $xfOutFile "# sets an element in the auto_index array, where the"
        puts $xfOutFile "# element name is the name of a command and the value is"
        puts $xfOutFile "# a script that loads the command.\n"
      } {
        puts $xfOutFile "# Tcl autoload index file: each line identifies a Tcl"
        puts $xfOutFile "# procedure and the file where that procedure is"
        puts $xfOutFile "# defined.  Generated by the \"auto_mkindex\" command.\n"
      }
      puts $xfOutFile "$xfTclIndex"
      puts $xfOutFile "# end of tclIndex"
      puts $xfOutFile ""
      close $xfOutFile
    }
  }
}

# eof

