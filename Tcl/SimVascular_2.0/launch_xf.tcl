# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
#
#
# See SimVascular Acknowledgements file for additional
# contributors to the source code. 
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# --------------------------
# Read environment variables
# --------------------------

global env
set envnames [array names env]

if {[lsearch -exact [array names env] SIMVASCULAR_HOME] < 0} {
  puts "FATAL ERROR: Need to set environment variable SIMVASCULAR_HOME."
  exit
}

if {!(([file exists $env(SIMVASCULAR_HOME)]) && ([file isdirectory $env(SIMVASCULAR_HOME)]))} {
  puts "FATAL ERROR: Invalid value for SIMVASCULAR_HOME variable ($env(SIMVASCULAR_HOME))."
  exit
}

set simvascular_home $env(SIMVASCULAR_HOME)

#source [file join $env(SIMVASCULAR_HOME) Tcl SimVascular_2.0 simvascular_vtk_init.tcl]

# need package xml
lappend auto_path [file join $env(SIMVASCULAR_HOME) Tcl External tclxml3.2]

# ----------------------
# load required packages
# ----------------------

catch {package require Plotchart}
catch {package require math::geometry}
catch {package require math::constants}
catch {package require math::linearalgebra}
catch {package require math::complexnumbers}
catch {package require getstring}
catch {package require xml}
catch {package require swaplist}
global env

# -----------------------
# Create empty namespaces
# -----------------------

namespace eval vis {}
namespace eval poly {}
namespace eval u {}

puts "Optional Runtimes:"
# Load TKCXImage
if {[info exists env(TKCXIMAGE_DLL)]} {
  if [catch {load $env(TKCXIMAGE_DLL) Tkcximage} msg] {
     #return -code error "ERROR: Uh oh, can't load Tkcximage!"
     puts [format "  %-12s %s" "Tkcximage:" unavailable]
  }
} else {
  if [catch {load Tkcximage} msg] {
     #return -code error "ERROR: Uh oh, can't load Tkcximage!"
     puts [format "  %-12s %s" "Tkcximage:" unavailable]
  }
}

puts ""

# -------------------------
# Launch gui if interactive
# -------------------------

  # tcl 8.4.x no longer exports tkTabToWindow
  if [catch {tk::unsupported::ExposePrivateCommand tkTabToWindow}] {
    proc tkTabToWindow {foo} {}
  }

source [file join $env(SIMVASCULAR_HOME)/Tcl/External/tkcon.tcl]

  # try and pick a nice looking theme if it exists

  if {$tcl_platform(os) == "Linux"} {
     catch {ttk::style theme use clam}
  }

  if {$tcl_platform(platform) == "windows"} {
     catch {ttk::style theme use winnative}
     catch {ttk::style theme use vista}
  }

  set ::tkcon::PRIV(WWW) 1
  set ::tkcon::OPT(rows) 20
  set ::tkcon::OPT(cols) 80
  set ::tkcon::OPT(showmenu) 1
  set ::tkcon::COLOR(bg) white
  global symbolicName

  set ::tkcon::PRIV(displayWin) .tkcon
  set ::tkcon::PRIV(root) .tkcon

  ::tkcon::Init -exec ""
  catch {wm title .tkcon "SimVascular Command Console"}

# -------------------------
# Source script if provided
# -------------------------

set xf_DIR [file join $env(SIMVASCULAR_HOME)/Tcl/External/xf-4.3]

source [file join $env(SIMVASCULAR_HOME)/Tcl/External/xf-4.3/src/xfmain.tcl]
if {$argc >= 3} {
  ShowWindow.
}


