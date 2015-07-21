# Program: xf
# Description: main part
#
# $Header: xfmain.tcl[2.8] Wed Mar 10 12:06:38 1993 garfield@garfield frozen $

#### global definitions ; don't modify this section...
global env tcl_platform xfMisc
# normal modifier mapping for ALT key
global xf_ALT
# root directory of xf
global xf_DIR
# temporary directory
global xf_TMP
# config file
global xf_CONFIGFILE
# wish interpreter
global xf_WISH
# tk shareable library
global xf_TKSHARLIB
# file path separator used by XF
global xfMisc(separator)

#  D.LaBelle - 03/10/98 - Set default font for all widgets to 10 point.
   option add *font {Helvetica 10}

# determine XF root directory
#regsub -all {\\} [file dirname [file dirname $argv0]] / xf_DIR
#set xf_DIR C:/Work/software/languages/tcl-helpers/xf/xf43
set xforgdir [pwd]
cd $xf_DIR
set xf_DIR [pwd]
cd $xforgdir

# set these in the environment if you want to
# env(HELPTCLTK): help command for Tcl/Tk (Unix: xman, Windows: winhlp32)
# env(HELPXF)   : help command for Xf (must be a html browser)

#################
# Customize here!!!
#################
if {$tcl_platform(platform) == "unix"} {
  #### Customize here for Unix platform
  #
  #### D. LaBelle - These settings should now work "out-of-the-box" for most
  #    11/23/98     UNIX setups. They can be configured by the user
  #                 once he has a better understanding of what they mean.
  set xf_ALT Mod1  ;# Define here what the ALT key looks like to TCL/TK under your version of UNIX
  set xf_TMP "/tmp"
  set xf_CONFIGFILE "$xf_DIR/xf-config.tcl"
  set xf_WISH wish
  set xfMisc(separator) "@"
} {
   #### Customize here for Window95/NT platform
   #
   #### D. LaBelle - These settings should now work "out-of-the-box" for almost
   #    11/23/98     all Windows setups. They can be configured by the user
   #                 once he has a better understanding of what they mean.
   set xf_ALT Mod2
   set xf_TMP "C:/tmp"
   if {![file isdir $xf_TMP]} { catch {file mkdir $xf_TMP} }
   set xf_CONFIGFILE "$xf_DIR/xf-config.tcl"
   set xf_WISH wish
   set env(HELPTCLTK) {}
   set xf_TKSHARLIB {}
   set xfMisc(separator) "@"
#  D.LaBelle - 03/10/98 - Always display console under Windows NT
   catch {console show}
}

#################
# End of customization
#################

##########
# Procedure: XFShowHelp
# Description: show a help text
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFShowHelp {} {

  # the usage message
  puts stdout {}
  puts stdout {usage:}
  puts stdout {xf } nonewline
  puts stdout {[-cmd <arg>] } nonewline
  puts stdout {[-testcmd <arg>] } nonewline
  puts stdout {[-xf <arg>] } nonewline
  puts stdout {[-xfadditionals <arg>] }
  puts stdout {[-xfbind <arg>] }
  puts stdout {   } nonewline
  puts stdout {[-xfcolors <arg>] } nonewline
  puts stdout {[-xfconfig <arg>] } nonewline
  puts stdout {[-xfcursors <arg>] } nonewline
  puts stdout {[-xfelements <arg>] }
  puts stdout {   } nonewline
  puts stdout {[-xffonts <arg>] } nonewline
  puts stdout {[-xfhelp] } nonewline
  puts stdout {[-xficonbar <arg>] }
  puts stdout {   } nonewline
  puts stdout {[-xficons <arg>] } nonewline
  puts stdout {[-xfignore] } nonewline
  puts stdout {[-xfkeysyms <arg>] } nonewline
  puts stdout {[-xflib <arg>] } nonewline
  puts stdout {[-xfmenu <arg>] }
  puts stdout {   } nonewline
  puts stdout {[-xfmodelcolor] } nonewline
  puts stdout {[-xfmodelmono] } nonewline
  puts stdout {[-xfpos <arg>] } nonewline
  puts stdout {[-xfprocedures <arg>] }
  puts stdout {   } nonewline
  puts stdout {[-xftmp <arg>] } nonewline
  puts stdout {[-xftmplt <arg>] } nonewline
  puts stdout {[-xfsrc <arg>] } nonewline
  puts stdout {[-xfstartup <arg>] } nonewline
  puts stdout {[-xfversion] }
}

##########
# Procedure: XFShowXFVersion
# Description: show a version text
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFShowXFVersion {} {
  global xfPath

  # display text
  puts stdout "XF:"
  catch "Cat $xfPath(base)/Version" xfResult
  puts -nonewline stderr $xfResult
}

##########
# Procedure: XFShowStartup
# Description: show a startup message
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFShowStartup {} {

  # display startup message
  update idletask
  toplevel .xfLoading
  wm title .xfLoading "Loading XF..."
  wm geometry .xfLoading 200x30
  wm maxsize .xfLoading 200 30
  wm minsize .xfLoading 200 30

  label .xfLoading.mess1 \
    -anchor c \
    -text "Loading XF..."

  pack append .xfLoading .xfLoading.mess1 {top fill expand}

  update idletask
}

##########
# Procedure: XFParseAppDefs
# Description: parse application defaults for widget resources
# Arguments: xfAppDefFile - the application defaults file
# Returns: none
# Sideeffects: none
##########
proc XFParseAppDefs {xfAppDefFile} {
  global xfAppDefaults tcl_platform

  # basically from: Michael Moore
  if {[file exists $xfAppDefFile] &&
      [file readable $xfAppDefFile] &&
      "[file type $xfAppDefFile]" == "link"} {
    catch "file type $xfAppDefFile" xfType
    while {"$xfType" == "link"} {
      if {[catch "file readlink $xfAppDefFile" xfAppDefFile]} {
        return
      }
      catch "file type $xfAppDefFile" xfType
    }
  }
  if {!("$xfAppDefFile" != "" &&
        [file exists $xfAppDefFile] &&
        [file readable $xfAppDefFile] &&
        "[file type $xfAppDefFile]" == "file")} {
    return
  }
  if {![catch "open $xfAppDefFile r" xfResult]} {
    set xfAppFileContents [read $xfResult]
    close $xfResult
    foreach line [split $xfAppFileContents "\n"] {
      # backup indicates how far to backup.  It applies to the
      # situation where a resource name ends in . and when it
      # ends in *.  In the second case you want to keep the *
      # in the widget name for pattern matching, but you want
      # to get rid of the . if it is the end of the name. 
      set backup -2  
      set line [string trim $line]
      if {[string index $line 0] == "#" || "$line" == ""} {
        # skip comments and empty lines
        continue
      }
      if {![string compare "windows" $tcl_platform(platform)]} {
        set list [split $line ";"]
      } {
        set list [split $line ":"]
      }
      set resource [string trim [lindex $list 0]]
      set i [string last "." $resource]
      set j [string last "*" $resource]
      if {$j > $i} { 
        set i $j
        set backup -1
      }
      incr i
      set name [string range $resource $i end]
      incr i $backup
      set widname [string range $resource 0 $i]
      set value [string trim [lindex $list 1]]
      if {"$widname" != "" && "$widname" != "*"} {
        # insert the widget and resourcename to the application
        # defaults list.
        if {![info exists xfAppDefaults]} {
          set xfAppDefaults ""
        }
        lappend xfAppDefaults [list $widname [string tolower $name] $value]
      }
    }
  }
}

##########
# Procedure: XFLoadAppDefs
# Description: load application defaults
# Arguments: {xfClasses} - the classes we look for
#            {xfPriority} - the priority of the resources
#            {xfAppDefFile} - the application defaults file
# Returns: none
# Sideeffects: none
##########
proc XFLoadAppDefs {{xfClasses ""} {xfPriority "startupFile"} {xfAppDefFile ""}} {
  global env tcl_platform

  if {![string compare "windows" $tcl_platform(platform)]} {
    set separator ";"
  } {
    set separator ":"
  }
  if {"$xfAppDefFile" == ""} {
    set xfFileList ""
    if {[info exists env(XUSERFILESEARCHPATH)]} {
      eval lappend xfFileList [split $env(XUSERFILESEARCHPATH) $separator]
    }
    if {[info exists env(XAPPLRESDIR)]} {
      eval lappend xfFileList [split $env(XAPPLRESDIR) $separator]
    }
    if {[info exists env(XFILESEARCHPATH)]} {
      eval lappend xfFileList [split $env(XFILESEARCHPATH) $separator]
    }
    append xfFileList " /usr/lib/X11/app-defaults"
    append xfFileList " /usr/X11/lib/X11/app-defaults"

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
            ("[file type $xfPathName]" == "file" ||
             "[file type $xfPathName]" == "link")} {
          catch "option readfile $xfPathName $xfPriority"
          if {"[info commands XFParseAppDefs]" != ""} {
            XFParseAppDefs $xfPathName
          } {
            if {"[info commands XFLocalParseAppDefs]" != ""} {
              XFLocalParseAppDefs $xfPathName
            }
          }
        } {
          if {[file exists $xfCounter2/$xfCounter1] &&
              [file readable $xfCounter2/$xfCounter1] &&
              ("[file type $xfCounter2/$xfCounter1]" == "file" ||
               "[file type $xfCounter2/$xfCounter1]" == "link")} {
            catch "option readfile $xfCounter2/$xfCounter1 $xfPriority"
            if {"[info commands XFParseAppDefs]" != ""} {
              XFParseAppDefs $xfCounter2/$xfCounter1
            } {
              if {"[info commands XFLocalParseAppDefs]" != ""} {
                XFLocalParseAppDefs $xfCounter2/$xfCounter1
              }
            }
          }
        }
      }
    }
  } {
    # load a specific application defaults file
    if {[file exists $xfAppDefFile] &&
        [file readable $xfAppDefFile] &&
        ("[file type $xfAppDefFile]" == "file" ||
         "[file type $xfAppDefFile]" == "link")} {
      catch "option readfile $xfAppDefFile $xfPriority"
      if {"[info commands XFParseAppDefs]" != ""} {
        XFParseAppDefs $xfAppDefFile
      } {
        if {"[info commands XFLocalParseAppDefs]" != ""} {
          XFLocalParseAppDefs $xfAppDefFile
        }
      }
    }
  }
}

##########
# Procedure: XFSetAppDefs
# Description: set application defaults for created widgets resources
# Arguments: {xfWidgetPath} - the widgetpath to set
# Returns: none
# Sideeffects: widget resources are set
##########
proc XFSetAppDefs {{xfWidgetPath "."}} {
  global xfAppDefaults

  if {![info exists xfAppDefaults]} {
    return
  }
  foreach xfCounter $xfAppDefaults {
    if {"$xfCounter" == ""} {
      break
    }
    set widname [lindex $xfCounter 0]
    if {[string match $widname ${xfWidgetPath}] ||
        [string match "${xfWidgetPath}*" $widname]} {
      set name [string tolower [lindex $xfCounter 1]]
      set value [lindex $xfCounter 2]
      # Now lets see how many tcl commands match the name
      # pattern specified.
      set widlist [info command $widname]
      if {"$widlist" != ""} {
        foreach widget $widlist {
          # make sure this command is a widget.
          if {![catch "winfo id $widget"] &&
              [string match "${xfWidgetPath}*" $widget]} {
            catch "$widget configure -$name $value" 
          }
        }
      }
    }
  }
}

##########
# Procedure: XFGetUniqueId
# Description: Create a unique user id
# Arguments: none
# Returns: the unique id
# Sideeffects: none
##########
proc XFGetUniqueId {} {
  global errorCode
  global env

  if {[info exists env(XF_UNIQUE_ID)]} {
    return $env(XF_UNIQUE_ID)
  }

  return xf[pid]
}

##########
# Procedure: XFStartup
# Description: the program startup
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFStartup {} {
  global xf_ALT
  global xf_DIR
  global xf_TMP
  global xf_CONFIGFILE
  global xf_WISH
  global xf_TKSHARLIB

  # test tk version
  global tk_version
  if {$tk_version < 4.0} {
    puts stderr "\nXF error: wrong TK version: need at least 4.0\n"
  }

  # initialize the program
  XFShowStartup

  # external globals
  global argc
  global argv
  global auto_path
  global tk_library
  global tk_strictMotif
  global tk_version
  global tcl_platform

  # catch the current globals and procedures
  global xfStartupGlobal
  set xfStartupGlobal ""
  foreach xfElement [info globals] {
    if {![string match "tk_*" $xfElement] &&
        ![string match ".xf*" $xfElement] &&
        ![string match "xf*" $xfElement] &&
        ![string match "XF*" $xfElement] &&
        "$xfElement" != "tk_version" &&
        "$xfElement" != "errorCode" &&
        "$xfElement" != "errorInfo" &&
        "$xfElement" != "env" &&
        "$xfElement" != "argv" &&
        "$xfElement" != "argc" &&
        "$xfElement" != "auto_oldpath" &&
        "$xfElement" != "auto_execs" &&
        "$xfElement" != "auto_index" &&
        "$xfElement" != "auto_path"} {
      lappend xfStartupGlobal $xfElement
    }
  }
  global xfStartupProcs
  set xfStartupProcs ""
  foreach xfElement [info procs] {
    if {![string match "tk_*" $xfElement] &&
        ![string match ".xf*" $xfElement] &&
        ![string match "xf*" $xfElement] &&
        ![string match "XF*" $xfElement] &&
        "$xfElement" != "parray" &&
        "$xfElement" != "unknown" &&
        "$xfElement" != "auto_execok" &&
        "$xfElement" != "auto_load" &&
        "$xfElement" != "auto_reset" &&
        "$xfElement" != "auto_mkindex"} {
      lappend xfStartupProcs $xfElement
    }
  }

  # symbolic name and module handling
  global symbolicName
  set symbolicName(root) .
  global moduleList
  set moduleList(main.tcl) ""
  global autoLoadList
  set autoLoadList(main.tcl) 0
  global preloadList
  set preloadList(xfInternal) ""
  global internalAliasList
  set internalAliasList ""
  global xfAppDefToplevels
  set xfAppDefToplevels ""
  global xfWmSetPosition
  set xfWmSetPosition ""
  global xfWmSetSize
  set xfWmSetSize ""
  global xfSaveModuleList
  set xfSaveModuleList ""
  global xfVersionNr
  set xfVersionNr {4.3}

  # hidden procedures
  global hiddenProcs 
  global hiddenBodys

  # widget initialisations
  global tk_xfScale
  set tk_xfScale(focus) ""

  global xfMisc

  # path names
  global xfLoadPath
  set xfLoadPath $xf_DIR
  global xfPath
  set xfPath(base) $xf_DIR
  set xfPath(additionals) "$xf_DIR/additionals"
  set xfPath(elements) "$xf_DIR/elements"
  # D. LaBelle - 03/10/98 - Removed reference to X11/bitmaps directory for Windows compatibility
#  set xfPath(icons) "$xf_DIR/lib/icons$xfMisc(separator)/usr/include/X11/bitmaps"
  set xfPath(icons) "$xf_DIR/lib/icons"
  set xfPath(lib) "$xf_DIR/lib"
  set xfPath(procedures) "$xf_DIR/procedures"
  set xfPath(src) "$xf_DIR/src"
  set xfPath(templates) "$xf_DIR/templates"
  set xfPath(tmp) $xf_TMP
  
  # file names
  global xfFile
  set xfFile(appdef) "$xf_DIR/xf.ad"
  set xfFile(bindings) "$xf_DIR/lib/xfdefbind.tcl"
  set xfFile(colors) "$xf_DIR/lib/Colors"
  set xfFile(config) $xf_CONFIGFILE
  set xfFile(cursors) "$xf_DIR/lib/Cursors"
  set xfFile(emacsCmd) "TkEmacs"
  set xfFile(emacsLisp) "tkemacs.el"
  set xfFile(fonts) "$xf_DIR/lib/Fonts"
  set xfFile(iconbar) "$xf_DIR/.xf-iconbar"
  set xfFile(keysyms) "$xf_DIR/lib/Keysyms"
  set xfFile(menu) "~/.xf-menubar"
  set xfFile(positions) "~/.xf-positions"
  set xfFile(startup) ".xf-init"

  # default bindings
  global xfBind
  set xfBind(configure) "<Double-Button-3>"
  set xfBind(popup) "3"
  set xfBind(select) "<Double-Button-2>"
  set xfBind(select1) "<Double-Button-1>"
  set xfBind(select2) "<Double-Button-2>"
  set xfBind(select3) "<Double-Button-3>"
  if {![string compare "windows" $tcl_platform(platform)]} {
    set xfBind(placing) "<Mod2-Button-1>"
    set xfBind(placingMotion) "<Mod2-B1-Motion>"
    set xfBind(placingRelease) "<Mod2-ButtonRelease-1>"
    set xfBind(showName) "<Mod2-ButtonPress-2>"
    set xfBind(removeName) "<Any-ButtonRelease-2>"
  } {
    set xfBind(placing) "<Mod1-Button-1>"
    set xfBind(placingMotion) "<Mod1-B1-Motion>"
    set xfBind(placingRelease) "<Mod1-ButtonRelease-1>"
    set xfBind(showName) "<Mod1-ButtonPress-2>"
    set xfBind(removeName) "<Any-ButtonRelease-2>"
  }

  # window positions and sizes
  global xfPos
  set xfPos(binding) {420 520 500 500}
  set xfPos(config) {300 300 300 300}
  set xfPos(edit) {450 480 200 1}
  set xfPos(editProc) {300 300 100 100}
  set xfPos(fs) {300 300 1 300}
  set xfPos(groups) {300 300 1 300}
  set xfPos(iconBarEdit) {520 400 400 400}
  set xfPos(iconBar) {0 0 400 400}
  set xfPos(infoAliases) {450 400 500 500}
  set xfPos(infoCmds) {440 500 100 100}
  set xfPos(infoErrors) {300 300 1 1}
  set xfPos(infoGlob) {400 400 100 100}
  set xfPos(infoPixmaps) {800 300 1 1}
  set xfPos(infoProc) {450 500 100 100}
  set xfPos(layout) {360 170 600 500}
  set xfPos(messages) {300 300 600 500}
  set xfPos(optionsBind) {400 310 600 500}
  set xfPos(optionsGeneral) {440 600 400 400}
  set xfPos(optionsInterp) {400 245 600 500}
  set xfPos(optionsPath) {400 610 500 400}
  set xfPos(optionsSource) {430 600 600 500}
  set xfPos(optionsVersion) {400 280 600 500}
  set xfPos(optionsWindow) {400 420 600 500}
  set xfPos(menuBarConf) {530 440 400 400}
  set xfPos(modules) {540 420 400 400}
  set xfPos(packing) {540 600 400 400}
  set xfPos(parameters) {400 420 400 400}
  set xfPos(pasteScript) {300 300 200 200}
  set xfPos(pasteTree) {300 300 100 100}
  set xfPos(placing) {500 510 400 400}
  set xfPos(read) {350 150 300 300}
  set xfPos(script) {400 400 200 200}
  set xfPos(sizing) {350 100 300 300}
  set xfPos(showName) {200 80 1 1}
  set xfPos(widgetTree) {400 400 200 200}

  # config variables
  global xfConf
  set xfConf(applyBinding) 1
  set xfConf(applyPacking) 1
  set xfConf(applyPlacing) 1
  set xfConf(applyParameters) 1
  set xfConf(autoPos) 0
  set xfConf(autoRootPos) 1
  set xfConf(autoSize) 1
  set xfConf(autoStack) 0
  set xfConf(createAppdefCode) 1
  set xfConf(createClassBinding) 0
  set xfConf(createFormCode) 0
  set xfConf(createParseCode) 1
  set xfConf(createPixmapCode) 1
  set xfConf(editListsHidden) 0
  set xfConf(encloseBinding) 1
  set xfConf(encloseConfigure) 1
  set xfConf(externalEditor) {}
  set xfConf(flash) HotPink
  set xfConf(fontMessage) "*-helvetica-bold-r-normal--18-*"
  set xfConf(geometry) packer
  set xfConf(getWidgetName) 0
  set xfConf(gridX) 0
  set xfConf(gridY) 0
  set xfConf(iconBar) "child"
  set xfConf(iconBarHidden) 0
  set xfConf(interpreter) $xf_WISH
  set xfConf(interpreterTut) $xf_WISH
  set xfConf(interpreterEdit) $xf_WISH
  set xfConf(interpreterTest) $xf_WISH
  set xfConf(interpreterHasTkemacs) 0
  set xfConf(kanji) 0
  set xfConf(layoutAlways) 1
  set xfConf(layoutBorder) 3
  set xfConf(maxSaveId) 10
  set xfConf(menuBarHidden) 0
  set xfConf(onlyOneWindow) 0
  set xfConf(pathNameHidden) 0
  set xfConf(programName) "main.tcl"
  set xfConf(programNameOld) "main.tcl"
  set xfConf(programPath) [pwd]
  set xfConf(saveInterval) 30
  set xfConf(saveOptions) 0
  set xfConf(savePositions) 0
  set xfConf(scanTree) 1
  set xfConf(scrollSide) left
  set xfConf(statusHidden) 0
  set xfConf(strictMotif) $tk_strictMotif
  set xfConf(writeTclIndex) 1
  set xfConf(writeNewTclIndex) 1
  set xfConf(writeShellScript) 1

  # status variables
  global xfCanvasPos
  set xfCanvasPos(xfLastX) 0
  set xfCanvasPos(xfLastY) 0
  global xfEditing
  set xfEditing(xfInternal) "xfInternal"
  global xfStatus
  set xfStatus(additionalList) ""
  set xfStatus(autoLoadModule) 0
  set xfStatus(borderWidth) 2
  set xfStatus(clearSelection) 1
  set xfStatus(cmdIndex) 0
  set xfStatus(cmdName) ""
  set xfStatus(comment) "file"
  set xfStatus(currentComment) "file"
  set xfStatus(cutBuffer) 0
  set xfStatus(editors) 0
  set xfStatus(elementCounter) 0
  set xfStatus(elementList) ""
  set xfStatus(elementWidth) 18
  set xfStatus(firstPacking) 1
  set xfStatus(firstPlacing) 1
  set xfStatus(globalIndex) 0
  set xfStatus(globalName) ""
  set xfStatus(handleTemplates) 0
  set xfStatus(hasColor) [regexp -nocase {color} [winfo screenvisual .]]
  set xfStatus(hiddenProcs) 0
  set xfStatus(imgIndex) 0
  set xfStatus(imgName) ""

  set xfStatus(includeExclude) 1
  set xfStatus(includeExcludeString) ""
  set xfStatus(itemList) 0
  set xfStatus(menus) 0
  set xfStatus(path) .
  set xfStatus(pasteScriptDisplayed) 0
  set xfStatus(pasteTreeDisplayed) 0
  set xfStatus(placeCurMaster) ""
  set xfStatus(placeCurX) ""
  set xfStatus(placeCurY) ""
  set xfStatus(placeCurRelX) ""
  set xfStatus(placeCurRelY) ""
  set xfStatus(placeCurWidth) ""
  set xfStatus(placeCurHeight) ""
  set xfStatus(placeCurRelWidth) ""
  set xfStatus(placeCurRelHeight) ""
  set xfStatus(placeOffsetX) 0
  set xfStatus(placeOffsetY) 0
  set xfStatus(procIndex) 0
  set xfStatus(procName) ""
  set xfStatus(rescanInfo) 1
  set xfStatus(saveId) 0
  set xfStatus(saving) 0
  set xfStatus(tmpltPath) ""
  set xfStatus(tmpltList) ""
  set xfStatus(type) "Frame"
  set xfStatus(uniqueId) 12345

  # misc variables
  set xfMisc(advise) 0
  set xfMisc(anchor) 0
  set xfMisc(canvasType) "<line> "
  set xfMisc(exportSelection) 0
  set xfMisc(focus) 0
  set xfMisc(iconify) 0
  set xfMisc(justify) 0
  set xfMisc(layout) 1
  set xfMisc(layoutFillX) 0
  set xfMisc(layoutFillY) 0
  set xfMisc(layoutWidth) 1
  set xfMisc(layoutHeight) 1
  set xfMisc(layoutX) 1
  set xfMisc(layoutY) 1
  set xfMisc(layoutExpand) 0
  set xfMisc(orient) 0
  set xfMisc(pos) 0
  set xfMisc(relief) 0
  set xfMisc(showValue) 0
  set xfMisc(size) 0
  set xfMisc(state) 0
  set xfMisc(widgetTreeRoot) .
  set xfMisc(widgetTreeHidden) ""
  set xfMisc(wrap) 0
  set xfMisc(autoPos) 0
  set xfMisc(autoRootPos) 0
  set xfMisc(autoSize) 0
  set xfMisc(autoStack) 0
  set xfMisc(createAppdefCode) 1
  set xfMisc(createClassBinding) 0
  set xfMisc(createFormCode) 1
  set xfMisc(createParseCode) 1
  set xfMisc(createPixmapCode) 1
  set xfMisc(editListsHidden) 0
  set xfMisc(getWidgetName) 0
  set xfMisc(iconBar) "child"
  set xfMisc(iconBarHidden) 0
  set xfMisc(interpreterHasTkemacs) 0
  set xfMisc(onlyOneWindow) 0
  set xfMisc(layoutAlways) 0
  set xfMisc(menuBarHidden) 0
  set xfMisc(noRedraw) 0
  set xfMisc(pathNameHidden) 0
  set xfMisc(saveOptions) 0
  set xfMisc(savePositions) 0
  set xfMisc(savedBindFocus) ""
  set xfMisc(scrollSide) left
  set xfMisc(statusHidden) 0
  set xfMisc(strictMotif) 0
  set xfMisc(writeTclIndex) 1
  set xfMisc(writeNewTclIndex) 1
  set xfMisc(writeShellScript) 1
  set xfMisc(commentfile) {}
  set xfMisc(commentmodule) {}
  set xfMisc(commentproc) {}
  set xfMisc(bindLevel1) 0
  set xfMisc(bindLevel2) 0
  set xfMisc(bindLevel3) 0
  set xfMisc(bindLevel4) 0
  set xfMisc(bindLevel5) 0
  set xfMisc(bindLevel6) 0
  set xfMisc(bindLevel7) 0
  set xfMisc(bindLevel8) 0
  set xfMisc(procLevel1) 0
  set xfMisc(procLevel2) 0
  set xfMisc(procLevel3) 0
  set xfMisc(procLevel4) 0
  set xfMisc(procLevel5) 0
  set xfMisc(procLevel6) 0
  set xfMisc(procLevel7) 0
  set xfMisc(procLevel8) 0

  # active save/display levels
  global xfBindSaveLevel
  set xfBindSaveLevel(1) 1
  set xfBindSaveLevel(2) 1
  set xfBindSaveLevel(3) 1
  set xfBindSaveLevel(4) 1
  set xfBindSaveLevel(5) 1
  set xfBindSaveLevel(6) 1
  set xfBindSaveLevel(7) 1
  set xfBindSaveLevel(8) 1
  global xfBindShowLevel
  set xfBindShowLevel(1) 1
  set xfBindShowLevel(2) 1
  set xfBindShowLevel(3) 1
  set xfBindShowLevel(4) 1
  set xfBindShowLevel(5) 1
  set xfBindShowLevel(6) 0
  set xfBindShowLevel(7) 0
  set xfBindShowLevel(8) 0
  global xfProcSaveLevel
  set xfProcSaveLevel(1) 1
  set xfProcSaveLevel(2) 1
  set xfProcSaveLevel(3) 1
  set xfProcSaveLevel(4) 1
  set xfProcSaveLevel(5) 1
  set xfProcSaveLevel(6) 1
  set xfProcSaveLevel(7) 1
  set xfProcSaveLevel(8) 1
  global xfProcShowLevel
  set xfProcShowLevel(1) 1
  set xfProcShowLevel(2) 1
  set xfProcShowLevel(3) 1
  set xfProcShowLevel(4) 1
  set xfProcShowLevel(5) 1
  set xfProcShowLevel(6) 0
  set xfProcShowLevel(7) 0
  set xfProcShowLevel(8) 0

  # source comments
  global xfComment
  set xfComment(file) {#   Copyright (c) 1998-2007 Stanford University,
#   Charles Taylor, Nathan Wilson, Ken Wang.
#
#   See SimVascular Acknowledgements file for additional
#   contributors to the source code. 
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including 
# without limitation the rights to use, copy, modify, merge, publish, 
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
# 
# The above copyright notice and this permission notice shall be included 
# in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#!$interpreter -f
# Program: $programName
# Tcl version: $tclVersion ($magicCookie)
# Tk version: $tk_version
# XF version: $xfVersion
#}
  set xfComment(module) {# Module: $moduleName
# Tcl version: $tclVersion ($magicCookie)
# Tk version: $tk_version
# XF version: $xfVersion
#}
  set xfComment(proc) {# Procedure: $procedureName}
  set xfComment(template) {#   Copyright (c) 1998-2007 Stanford University,
#   Charles Taylor, Nathan Wilson, Ken Wang.
#
#   See SimVascular Acknowledgements file for additional
#   contributors to the source code. 
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including 
# without limitation the rights to use, copy, modify, merge, publish, 
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
# 
# The above copyright notice and this permission notice shall be included 
# in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# Tcl version: $tclVersion ($magicCookie)
# Tk version: $tk_version
# XF version: $xfVersion
#}

  # widget optimization
  global xfNoSpecialBind
  set xfNoSpecialBind {Button Canvas Checkbutton CheckButton Entry Frame Label Listbox Menu Menubutton Message Radiobutton RadioButton Scale Scrollbar Text Toplevel}
  global xfNoSpecialBinding
  set xfNoSpecialBinding {Button Canvas Checkbutton CheckButton Entry Frame Label Listbox Menu Menubutton Message Radiobutton RadioButton Scale Scrollbar Text Toplevel}
  global xfNoSpecialPacking
  set xfNoSpecialPacking {Button Canvas Checkbutton CheckButton Entry Frame Label Listbox Menu Menubutton Message Radiobutton RadioButton Scale Scrollbar Text Toplevel}
  global xfNoSpecialPlacing
  set xfNoSpecialPlacing {Button Canvas Checkbutton CheckButton Entry Frame Label Listbox Menu Menubutton Message Radiobutton RadioButton Scale Scrollbar Text Toplevel}
  global xfNoSpecialSave
  set xfNoSpecialSave {Button Checkbutton CheckButton Frame Label Menu Message Radiobutton RadioButton Scale Scrollbar Toplevel}
  global xfNoWidgetSave
  set xfNoWidgetSave {Button Canvas Checkbutton CheckButton Entry Frame Label Listbox Menubutton Message Radiobutton RadioButton Scale Scrollbar Text XYGraph Barchart Bargraph Dial Pie Stripchart Photo}
  global xfWrongName
  set xfWrongName {{Hypertext htext} {XFForm frame} {TkEmacs frame} {TkSteal frame} {TkGS frame}}

  # menu configuration
  global xfMenuBar
  set xfMenuBar(colorFile) $xfFile(colors)
  set xfMenuBar(fontFile) $xfFile(fonts)
  set xfMenuBar(file) ""
  set xfMenuBar(userFile) ""

  # iconbar configuration
  global xfIconBar
  set xfIconBar(iconPath) $xfPath(icons)
  set xfIconBar(file) ""
  set xfIconBar(userFile) ""

  # file selection
  global xfFSAll
  set xfFSAll 0
  global xfFSExtensions
  set xfFSExtensions 0
  global xfFSName
  set xfFSName ""
  global xfFSPath
  set xfFSPath [pwd]
  global xfFSInternalPath
  set xfFSInternalPath [pwd]
  global xfFSPattern
  set xfFSPattern *

  # font selection
  global xfFontFamily
  set xfFontFamily *
  global xfFontPixels
  set xfFontPixels *
  global xfFontSlant
  set xfFontSlant *
  global xfFontSWidth
  set xfFontSWidth *
  global xfFontWeight
  set xfFontWeight *


  # hide tk functions and bindings
  foreach xfProcName [info procs] {
    if {[string match XF* $xfProcName]} {
      continue
    }
    set xfArgList [info args $xfProcName]
    set xfArguments ""
    foreach xfArg $xfArgList {
      if {[info default $xfProcName $xfArg xfDefault]} {
        append xfArguments " \{$xfArg \"$xfDefault\"\}"
      } {
        append xfArguments " $xfArg"
      }
    }
    set xfBodyList "# xf ignore me 9\n[string trimright [info body $xfProcName]]"
    regsub -all "bind \[a-zA-Z0-9\$\]* <Any-ButtonRelease-1> tk_mbUnpost" $xfBodyList {bind $w <Any-ButtonRelease-1> {tk_mbUnpost}} xfBodyList
    regsub -all "bind \[a-zA-Z0-9\$\]* <\[a-zA-Z0-9-\]*> \{" $xfBodyList "&# xf ignore me 9\n" xfBodyList
    proc $xfProcName $xfArguments $xfBodyList
  }
  foreach xfClass {Button Canvas CheckButton Checkbutton Entry Frame Label Listbox Menu Menubutton Message RadioButton Radiobutton Scale Scrollbar Text Toplevel Barchart Bargraph Dial Hypertext Photo Pie Stripchart TkEmacs TkSteal TkGS TkMegaWidget XFForm XYGraph} {
    foreach xfCounter [bind $xfClass] {
      set xfTmpValue "# xf ignore me 9\n[bind $xfClass $xfCounter]"
      bind $xfClass $xfCounter $xfTmpValue
    }
  }

  update idletasks
  # parse arguments for config file
  set xfArgv ""
  for {set xfCounter 0} {$xfCounter < $argc} {incr xfCounter 1} {
    set xfArgv [string tolower [lindex $argv $xfCounter]]
    case $xfArgv in {
      {-xfconfig} {
        incr xfCounter 1
        set xfFile(config) [lindex $argv $xfCounter]
      }
      {-xfhelp} {
        XFShowXFVersion
        XFShowHelp
      }
      {-xfversion} {
        XFShowXFVersion
      }
      {-console} {
        catch "console show"
      }
    }
  }

  # load configuration
  if {[file exists $xfFile(config)] &&
      [file readable $xfFile(config)] &&
      ("[file type $xfFile(config)]" == "file" ||
       "[file type $xfFile(config)]" == "link")} {
    if {[catch {source $xfFile(config)} xfResult]} {
      puts stderr "error in config file: $xfFile(config)"
      puts stderr "please remove this file!"
      return
    }
    if {![info exists xfOptionVersionNr]} {
      puts stderr "warning: this config file was written by an"
      puts stderr "unknown version of XF!"
    } elseif {$xfVersionNr != $xfOptionVersionNr} {
              puts stderr "warning: this config file was written by a"
              puts stderr "different version of XF!"
             }
    if {![info exists xfPath(base)]} {
      puts stderr "error in config file: $xfFile(config)"
      puts stderr "please remove this file!"
      return
    }
  }

  # initialisation
  global xfColorBox
  set xfColorBox(scrollSide) $xfConf(scrollSide)
  global xfCursorBox
  set xfCursorBox(scrollSide) $xfConf(scrollSide)
  global xfFSBox
  set xfFSBox(scrollSide) $xfConf(scrollSide)
  global xfFontBox
  set xfFontBox(scrollSide) $xfConf(scrollSide)
  global xfIconBar
  set xfIconBar(scrollSide) $xfConf(scrollSide)
  global xfInputBox
  set xfInputBox(scrollSide) $xfConf(scrollSide)
  global xfKeysymBox
  set xfKeysymBox(scrollSide) $xfConf(scrollSide)
  global xfMenuBar
  set xfMenuBar(scrollSide) $xfConf(scrollSide)
  global xfReadBox
  set xfReadBox(scrollSide) $xfConf(scrollSide)
  global xfTextBox
  set xfTextBox(scrollSide) $xfConf(scrollSide)

  set xfConf(programPath) [pwd]
  set xfArgv ""
  set xfFileList ""
  set xfResult ""
  for {set xfCounter 0} {$xfCounter < $argc} {incr xfCounter 1} {
    case [string tolower [lindex $argv $xfCounter]] in {
      {-cmd} {
        incr xfCounter 1
        set xfConf(interpreter) [lindex $argv $xfCounter]
      }
      {-testcmd} {
        incr xfCounter 1
        set xfConf(interpreterTest) [lindex $argv $xfCounter]
      }
      {-xf} {
        incr xfCounter 1
        set xfPath(base) [lindex $argv $xfCounter]
      }
      {-xfadditionals} {
        incr xfCounter 1
        set xfPath(additionals) [lindex $argv $xfCounter]
      }
      {-xfbind} {
        incr xfCounter 1
        set xfFile(bindings) [lindex $argv $xfCounter]
      }
      {-xfcolors} {
        incr xfCounter 1
        set xfFile(colors) [lindex $argv $xfCounter]
      }
      {-xfconfig} {
        incr xfCounter 1
      }
      {-xfcursors} {
        incr xfCounter 1
        set xfFile(cursors) [lindex $argv $xfCounter]
      }
      {-xfelements} {
        incr xfCounter 1
        set xfPath(elements) [lindex $argv $xfCounter]
      }
      {-xffonts} {
        incr xfCounter 1
        set xfFile(fonts) [lindex $argv $xfCounter]
      }
      {-xficonbar} {
        incr xfCounter 1
        set xfFile(iconbar) [lindex $argv $xfCounter]
      }
      {-xficons} {
        incr xfCounter 1
        set xfPath(icons) [lindex $argv $xfCounter]
      }
      {-xfignore} {
        incr xfCounter 1
        set xfArgv [lrange $argv $xfCounter $argc]
        set xfCounter $argc
      }
      {-xfkeysyms} {
        incr xfCounter 1
        set xfFile(keysyms) [lindex $argv $xfCounter]
      }
      {-xflib} {
        incr xfCounter 1
        set xfPath(lib) [lindex $argv $xfCounter]
      }
      {-xfmenubar} {
        incr xfCounter 1
        set xfFile(menu) [lindex $argv $xfCounter]
      }
      {-xfmodelmono} {
        tk colormodel . monochrome
      }
      {-xfmodelcolor} {
        tk colormodel . color
      }
      {-xfpos} {
        incr xfCounter 1
        set xfFile(positions) [lindex $argv $xfCounter]
      }
      {-xfprocedures} {
        incr xfCounter 1
        set xfPath(procedures) [lindex $argv $xfCounter]
      }
      {-xfsrc} {
        incr xfCounter 1
        set xfPath(src) [lindex $argv $xfCounter]
      }
      {-xfstartup} {
        incr xfCounter 1
        set xfFile(startup) [lindex $argv $xfCounter]
      }
      {-xftmp} {
        incr xfCounter 1
        set xfPath(tmp) [lindex $argv $xfCounter]
      }
      {-xftemplates} {
        incr xfCounter 1
        set xfPath(templates) [lindex $argv $xfCounter]
      }
      {default} {
        # look if we must load
        if {[file exists [lindex $argv $xfCounter]] &&
    	    [file readable [lindex $argv $xfCounter]] &&
            ("[file type [lindex $argv $xfCounter]]" == "file" ||
             "[file type [lindex $argv $xfCounter]]" == "link") ||
             "$xfFileList" == ""} {
          # it is a file to load, or a non existing file to create
# 03/16/98 - Dennis LaBelle - Modified the next 8 lines in order to substitute / for \
#					under Windows.
#          append xfFileList [lindex $argv $xfCounter] " "
#          set xfConf(programName) [file tail [lindex $argv $xfCounter]]
#          set xfConf(programPath) [file dirname [lindex $argv $xfCounter]]
#          set moduleList([file tail [lindex $argv $xfCounter]]) ""
#          catch "unset moduleList($xfConf(programNameOld))"
#          set xfConf(programNameOld) [file tail [lindex $argv $xfCounter]]

          regsub -all {\\} [lindex $argv $xfCounter] / newname
          append xfFileList $newname " "
          set xfConf(programName) [file tail $newname]
          set xfConf(programPath) [file dirname $newname]
          set moduleList([file tail $newname]) ""
          catch "unset moduleList($xfConf(programNameOld))"
          set xfConf(programNameOld) [file tail $newname]
          
          # initialize the program path
          if {[string match "./*" $newname] != 1 &&
              "$xfConf(programPath)" == "."} {
            set xfConf(programPath) [pwd]
          }
        } {
          # it is a program option
          append xfArgv [lindex $argv $xfCounter] " "
        }
      }
    }
  }

  # load positions
  if {[file exists $xfFile(positions)] &&
      [file readable $xfFile(positions)] &&
      ("[file type $xfFile(positions)]" == "file" ||
       "[file type $xfFile(positions)]" == "link")} {
    if {[catch "source $xfFile(positions)" xfResult]} {
      puts stderr "$xfResult"
    }
  }
  # load default bindings
  if {[file exists $xfFile(bindings)] &&
      [file readable $xfFile(bindings)] &&
      ("[file type $xfFile(bindings)]" == "file" ||
       "[file type $xfFile(bindings)]" == "link")} {
    if {[catch "source $xfFile(bindings)" xfResult]} {
      puts stderr "$xfResult"
    }
  }
  # load save module list
  if {[file exists .xf-save-modules] &&
      [file readable .xf-save-modules] &&
      ("[file type .xf-save-modules]" == "file" ||
       "[file type .xf-save-modules]" == "link")} {
    if {[catch "source .xf-save-modules" xfResult]} {
      puts stderr "$xfResult"
    }
  }

  # new program options
  set argv $xfArgv
  set argc [llength $xfArgv]

  # load application defaults
  XFLoadAppDefs XF

  # load files
# D. LaBelle - April 24, 1997
# Replaced next line to allow loading of file from command line under Windows
#  set xfFileList ""
  regsub -all {\\} $xfFileList / xfFileList
  foreach xfCounter $xfFileList {
    if {[file exists $xfCounter] &&
        [file readable $xfCounter] &&
        ("[file type $xfCounter]" == "file" ||
         "[file type $xfCounter]" == "link")} {
      puts stderr "source $xfCounter"
      if {[catch "source $xfCounter" xfResult]} {
        XFShowXFVersion
        puts stderr "$xfResult"
      }
    }
  }

  # prepare auto loading for xf
  set auto_path ""
  foreach xfElement [eval list $xfPath(src) [split $xfLoadPath $xfMisc(separator)] $tk_library [info library]] {
    if {[file exists $xfElement/tclIndex]} {
      lappend auto_path $xfElement
    }
  }
#  catch "unset auto_index"
#  catch "unset auto_oldpath"
#  catch "unset auto_execs"

  # load startup file
  if {[file exists $xfFile(startup)] &&
      [file readable $xfFile(startup)] &&
      ("[file type $xfFile(startup)]" == "file" ||
       "[file type $xfFile(startup)]" == "link")} {
    if {[catch "source $xfFile(startup)" xfResult]} {
      puts stderr "$xfResult"
    }
  }

  # test if we can write to xfPath(procedures)
  if {[file writable $xfPath(procedures)] == 0} {
    puts stdout "XF warning: You have no write permission to:\n$xfPath(procedures)"
    puts stdout "  ==> You cannot save procedures into the procedure pool!"
    puts stdout "      (change under \"Options|Path names\")"
  }

  # test if we can write to xfPath(tmp)
  if {[file writable $xfPath(tmp)] == 0} {
    puts stdout "XF warning: You have no write permission to:\n$xfPath(tmp)"
    puts stdout "  ==> Should be changed immediately!!! (XF needs that!)"
    puts stdout "      (change under \"Options|Path names\")"
  }

  # load general modules
  if {[file isdirectory $xfPath(src)] == 1} {
    source "$xfPath(src)/xfbind.tcl"
    source "$xfPath(src)/xfedit.tcl"
    source "$xfPath(src)/xfelements.tcl"
    source "$xfPath(src)/xfglobals.tcl"
    source "$xfPath(src)/xfmisc.tcl"
    source "$xfPath(src)/xfprocConfig.tcl"
    source "$xfPath(src)/xfprocMain.tcl"
    source "$xfPath(src)/xftmplt.tcl"
    source "$xfPath(src)/xfalertBox.tcl"
    source "$xfPath(src)/xficonBar.tcl"
    source "$xfPath(src)/xftextBox.tcl"
    source "$xfPath(src)/xfpacking.tcl"
  } {
    XFShowXFVersion
    XFShowHelp
    puts stdout "\nInvalid source path: $xfPath(src)\n"
  }

  # run startup functions
  foreach xfCounter [info commands XFExternalInitProc*] {
    if {[catch "$xfCounter" xfResult]} {
      puts stdout $xfResult    
    }
  }

  # make a unique identifier for this xf
  set xfStatus(uniqueId) [XFGetUniqueId]

  # show startup window
  XFProcMain

  # set application defaults for created widgets
  XFSetAppDefs

  # remove loading message
  XFDestroy .xfLoading
}

XFStartup

# eof

