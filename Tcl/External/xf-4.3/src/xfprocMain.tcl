# Program: xf
# Description: procedures that implement the top level functionality
#
# $Header: xfprocMain.tcl[2.4] Wed Mar 10 12:07:55 1993 garfield@garfield frozen $

proc XFProcMain {} {
##########
# Procedure: XFProcMain
# Description: show the main edit window
# Arguments: none
# Returns: none
# Sideeffects: none
##########
  global xfConf
  global xfPath
  global xfStatus

  # set toplevel wm maxsize
  if {"[lindex [wm maxsize .] 0]" == "" && \
      "[lindex [wm maxsize .] 1]" == ""} {
    wm maxsize . [winfo screenwidth .] [winfo screenheight .]
  }
  if {$xfConf(autoRootPos)} {
    # prevent the placing of the decoration outside of the window...
    # temporarily bug fix
    wm geometry . +10+10
  }

  # force auto-load of bgerror
  catch "bgerror"

  # erase error protocol
  catch "Rm $xfPath(tmp)/xferrors$xfStatus(uniqueId)"

  # new global bindings
  XFMiscBindWidgetTree .

  if {"[info commands tkerror]" == "" ||
      ("[info commands tkerror]" != "" &&
       "[string trim [info body tkerror]]" == "global errorInfo
    puts stdout \"\$errorInfo\"")} {
    catch "proc tkerror {errMessage} {
             global errorInfo
             set errInfo \$errorInfo
             puts stdout \"\$errInfo\"
             if {\"\[info commands XFMiscSaveError\]\" != \"\"} {
               XFMiscSaveError \$errMessage \$errInfo
             }
           }"
  }

  # wrapper around exit
  rename exit XFExit
  catch "proc exit args {
           global xfAlertBox
           if {\"\[info commands .xfEdit\]\" != \"\"} {
             if {\[llength \$args\] > 0} {
               set xfAlertBox(toplevelName) .exit
               XFProcMessage \"Exit the program with: \[lindex \$args 0\]\" 250x100 \"Exit\" center {}
               set xfAlertBox(toplevelName) .xfAlertBox
             } {
               set xfAlertBox(toplevelName) .exit
               XFProcMessage \"Exit the program\" 250x100 \"Exit\" center {}
               set xfAlertBox(toplevelName) .xfAlertBox
             }
           } {
             catch \"XFExit \$args\"
           }
         }"

  # wrapper around destroy
  rename destroy XFDestroy
  catch "proc destroy xfArgs {
           if {\"\[info commands .xfEdit\]\" != \"\"} {
             if {\[string match \".xf*\" \$xfArgs\] ||
                 \[string match \".bgerror*\" \$xfArgs\] ||
                 \[string match \"xf*\" \[winfo name \$xfArgs\]\]} {
               XFDestroy \$xfArgs
             } {
               if {\[XFProcYesNo \"Destroy widget:\$xfArgs
really destroy ???\"\]} {
                 XFDestroy \$xfArgs
               }
             }
           } {
             XFDestroy \$xfArgs
           }
         }"

  # show main edit window
  XFEdit

  # start autosaving
  if {$xfConf(saveInterval) > 0} {
    after [expr $xfConf(saveInterval)*60000] XFMiscAutoSave $xfConf(saveInterval)
  }
}

# eof

