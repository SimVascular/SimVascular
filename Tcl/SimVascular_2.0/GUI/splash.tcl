# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                           University of California, San Diego.
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
# AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
# THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
#

#!wish -f
# Program: splash-2
# Tcl version: 8.5 (Tcl/Tk/XF)
# Tk version: 8.5
# XF version: 4.3
#

package require tile

# procedure to show window ShowWindow.splash
proc ShowWindow.splash { args} {
# xf ignore me 7
  # build widget .splash
  if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy .splash"
  } {
    catch "destroy .splash"
  }
  toplevel .splash   -background {white}

  # Window manager configurations
  wm positionfrom .splash ""
  wm sizefrom .splash ""
  wm geometry .splash 1000x500+100+100
  wm maxsize .splash 1000 1000
  wm minsize .splash 115 10
  wm protocol .splash WM_DELETE_WINDOW {XFProcError {Application windows can not be destroyed.
Please use the "Current widget path:" to show/hide windows.}}
  wm title .splash {SimVascular Copyright Notice}


  # build widget .splash.tframe1
  ttk::frame .splash.tframe1  -borderwidth {0}  -relief {flat}  -width {21}  -height {30}

  # build widget .splash.tframe1.tlabel6
  ttk::label .splash.tframe1.tlabel6  -background {white}  -font {Helvetica 10}  -relief {flat}  -anchor {center}  -image {logo}

  # build widget .splash.tframe2
  ttk::frame .splash.tframe2  -borderwidth {0}  -relief {flat}  -width {30}  -height {30}

  # build widget .splash.tframe2.tlabel4
  ttk::label .splash.tframe2.tlabel4  -background {white}  -foreground {blue}  -font {Helvetica 14}  -relief {groove}  -anchor {center}  -justify {center}  -text {VERSION REPLACE_SIMVASCULAR_FULL_VER_NO}  -textvariable {gOptions(simvascular_version)}  -width {60}

  # build widget .splash.tframe3
  ttk::frame .splash.tframe3  -borderwidth {0}  -relief {flat}  -width {30}  -height {30}

  # build widget .splash.tframe3.text5
  text .splash.tframe3.text5  -background {white}  -font {Helvetica 10}  -foreground {black}  -height {10}  -relief {flat}  -wrap {word}

  # pack master .splash.tframe1
  pack configure .splash.tframe1.tlabel6  -expand 1  -fill both

  # pack master .splash.tframe2
  pack configure .splash.tframe2.tlabel4  -fill both

  # pack master .splash.tframe3
  pack configure .splash.tframe3.text5  -expand 1  -fill both

  # pack master .splash
  pack configure .splash.tframe1  -expand 1  -fill both
  pack configure .splash.tframe2  -fill both
  pack configure .splash.tframe3  -fill both

  .splash.tframe3.text5 insert end {(c) 2014-2015 The Regents of the University of California. All Rights Reserved.
Portions of this software are owned by UGS Corp. (c) 1986-2007.  All Rights Reserved.
Portions of this software are owned by Simmetrix Inc. (c) 2007.  All Rights Reserved.
Portions of this Software are copyright Stanford University, Rensselaer Polytechnic Institute, and others.
This Software contains third party files and software code reproduced and distributed under open source or BSD style licenses ("Open Source Software").}



  if {"[info procs XFEdit]" != ""} {
    catch "XFMiscBindWidgetTree .splash"
    after 2 "catch {XFEditSetShowWindows}"
  }
}

proc DestroyWindow.splash {} {# xf ignore me 7
  if {"[info procs XFEdit]" != ""} {
    if {"[info commands .splash]" != ""} {
      global xfShowWindow.splash
      set xfShowWindow.splash 0
      XFEditSetPath .
      after 2 "XFSaveAsProc .splash; XFEditSetShowWindows"
    }
  } {
    catch "destroy .splash"
    update
  }
}


# procedure to show window .
proc ShowWindow. {args} {# xf ignore me 7

  # Window manager configurations
      #puts "show window."
  wm positionfrom . user
  wm sizefrom . ""
  wm maxsize . 1604 1178
  wm minsize . 115 1
  wm protocol . WM_DELETE_WINDOW {XFProcError {Application windows can not be destroyed.
Please use the "Current widget path:" to show/hide windows.}}
  wm title . {splash-2.tcl}


  if {"[info procs XFEdit]" != ""} {
    catch "XFMiscBindWidgetTree ."
    after 2 "catch {XFEditSetShowWindows}"
  }
}

if {![info exists env(SIMVASCULAR_HOME)]} {
   catch {image create photo logo \
  -file {../../splash.gif} \
  -gamma {1.0} \
  -height {0} \
  -width {0}} errmsg
} else {

    catch {image create photo logo -file [file join $env(SIMVASCULAR_HOME) Tcl SimVascular_2.0 splash.gif] } errmsg

} 
    #puts "No Splash $errmsg"
set thisFile [dict get [ info frame [ info frame ] ] file ]
set thisDir [file dirname $thisFile]

# initialize global variables
global {gOptions}

if { [file exists [file join $simvascular_home Tcl splash_configure.tcl]]} {
  source [file join $simvascular_home Tcl splash_configure.tcl]
} else {
set {gOptions(simvascular_version)} "VERSION REPLACE_SIMVASCULAR_FULL_VER_NO"
}
# please don't modify the following
# variables. They are needed by xf.
global {symbolicName}
set {symbolicName(root)} {.}
set {symbolicName(splash_textbox)} {.splash.tframe3.text5}
global {xfWmSetPosition}
set {xfWmSetPosition} {}
global {xfWmSetSize}
set {xfWmSetSize} {}
global {xfAppDefToplevels}
set {xfAppDefToplevels} {}

# display/remove toplevel windows.
ShowWindow.

global xfShowWindow.splash
set xfShowWindow.splash 0

# load default bindings.
if {[info exists env(XF_BIND_FILE)] &&
    "[info procs XFShowHelp]" == ""} {
  source $env(XF_BIND_FILE)
}

# eof
#

