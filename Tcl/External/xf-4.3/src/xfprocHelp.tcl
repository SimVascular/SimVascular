# Program: xf
# Description: procedures that implement the help functionality
#
# $Header: xfprocHelp.tcl[2.4] Wed Mar 10 12:07:53 1993 garfield@garfield frozen $

proc XFProcHelpAbout {} {
##########
# Procedure: XFProcHelpAbout
# Description: show the about box
# Arguments: none
# Returns: none
# Sideeffects: none
##########
  global xfAlertBox
  global xfPath

  if {[file exists $xfPath(base)/lib/About.H]} {
    set xfAlertBox(toplevelName) .about
    XFProcMessageFile $xfPath(base)/lib/About.H 290x300 {XF about} center {}
    set xfAlertBox(toplevelName) .xfAlertBox
  }
}

proc XFProcHelpHelp {args} {
##########
# Procedure: XFProcHelpHelp
# Description: show help in a external wish
# Arguments: args - the section (page) to show
# Returns: none
# Sideeffects: none
##########
  global env
  global xfPath
  global tcl_platform

  if {[info exists env([lindex $args 0])]} {
    if {[catch "eval $env([lindex $args 0])" xfResult]} {
      XFProcError $xfResult
    }
  } {
      if {$tcl_platform(platform) == "windows"} {
          # retrieve name of default browser from the registry
          catch {package require registry}
	  set browser [lindex [split [registry get {HKEY_LOCAL_MACHINE\SOFTWARE\Classes\http\shell\open\command} {}] \"] 1]
	  regsub -all {\\} $browser / browser
          if {[catch "exec \"$browser\" $xfPath(base)/Help/index.html &" xfResult]} {
             XFProcError $xfResult
	  }
      }
  }

}

proc XFProcHelpTutorial {} {
##########
# Procedure: XFProcHelpTutorial
# Description: start tutorial
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  global tcl_platform
  global xfConf
  global xfPath
  global xf_WISH
  global xf_TKSHARLIB

  if {[XFProcYesNo "Running the tutorial will erase your current work.\n Is this ok ?"]} {
    if {![string compare "windows" $tcl_platform(platform)]} {
      if { ! [interp exists xftut] } {
        interp create xftut
	load $xf_TKSHARLIB Tk xftut
	#load {} Tk xftut
        # redefine "send" as an "alias" command "eval" in slave interpreter
        xftut alias send eval
# 03/16/98 - Dennis LaBelle - Replaced the following line to enable running tutorial under Windows
#        xftut eval set xf_TUTINTERP $xf_WISH
        xftut eval set xf_TUTINTERP \"\"
      }
      if {$xfConf(kanji)} {
        xftut eval set xf_TUTSCRIPT $xfPath(base)/xftutorial/script.xfjp
        xftut eval source $xfPath(base)/xftutorial/xftutorial.tcl
      } {
        xftut eval set xf_TUTSCRIPT $xfPath(base)/xftutorial/script.xf
        xftut eval source $xfPath(base)/xftutorial/xftutorial.tcl
      }
    } {
      if {$xfConf(kanji)} {
        catch "exec $xfConf(interpreterTut) $xfPath(base)/xftutorial/xftutorial.tcl -name \"XF-tutorial\" -- $xfPath(base)/xftutorial/script.xfjp [winfo name .] &"
      } {
        catch "exec $xfConf(interpreterTut) $xfPath(base)/xftutorial/xftutorial.tcl -name \"XF-tutorial\" -- $xfPath(base)/xftutorial/script.xf [winfo name .] &"
      }
    }
  }
}

# eof

