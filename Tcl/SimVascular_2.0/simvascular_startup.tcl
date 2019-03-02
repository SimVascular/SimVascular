# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
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

if {[lsearch -exact [array names env] SV_HOME] < 0} {
  puts "FATAL ERROR: Need to set environment variable SV_HOME."
  exit
}

if {!(([file exists $env(SV_HOME)]) && ([file isdirectory $env(SV_HOME)]))} {
  puts "FATAL ERROR: Invalid value for SV_HOME variable ($env(SV_HOME))."
  exit
}

set simvascular_home $env(SV_HOME)

global SV_BUILD_ID
if { [file exists [file join $simvascular_home/Tcl/startup_configure.tcl]]} {
  source [file join $simvascular_home/Tcl/startup_configure.tcl]
  if { [file exists [file join $simvascular_home/release-date]] } {
      set SV_RELEASE_BUILD 1
      set fp [open "$simvascular_home/release-date" r]
      set timestamp [read $fp]
      set SV_BUILD_ID $timestamp
      close $fp
  } else {
      set SV_RELEASE_BUILD 0
      set SV_BUILD_ID "developer build"
  }
} else {
    set SV_FULL_VER_NO REPLACE_SV_FULL_VER_NO
    set SV_MAJOR_VER_NO REPLACE_SV_MAJOR_VER_NO
    set SV_TIMESTAMP   REPLACE_SV_TIMESTAMP
    set SV_PLATFORM    REPLACE_SV_PLATFORM
    set SV_VERSION     REPLACE_SV_VERSION

    if {[string range $SV_FULL_VER_NO 0 6] != "REPLACE"} {
      set SV_RELEASE_BUILD 1
      set timestamp [file tail $simvascular_home]
      set SV_BUILD_ID $timestamp
      } else {
        set SV_RELEASE_BUILD 0
        set SV_BUILD_ID "developer build"
      }
}

set SV_USE_TK_GUI 1
if {[info exists env(SV_BATCH_MODE)]} {
  if {$env(SV_BATCH_MODE)} {
    set SV_USE_TK_GUI 0 
  }
}

if {[info exists SV_NO_RENDERER] == 0} {
  global SV_NO_RENDERER
  set SV_NO_RENDERER "0"
}

global SV_BUILD_TYPE
if {[info exists SV_BUILD_TYPE] == 0} {
  set SV_BUILD_TYPE "MAKE"
}

#
#  Load SimVascular Modules (static or dynamic)
#

if {$tcl_platform(platform) == "unix"} {
    if {$tcl_platform(os) == "Darwin"} {
      set lib_prefix "lib_"
      set so_postfix ".dylib"
    } else {
      set lib_prefix "lib_"
      set so_postfix ".so"
    }
}

if {$tcl_platform(platform) == "windows"} {
  set lib_prefix "lib_"
  set so_postfix ".dll"
}

set gSimVascularTclInitLibs [list \
				 [list Repos ${lib_prefix}simvascular_repository${so_postfix}] \
				 [list myVtk {}] \
				 [list Lset ${lib_prefix}simvascular_lset${so_postfix}] \
				 [list Itklset ${lib_prefix}simvascular_itk_lset${so_postfix}] \
				 [list Geom ${lib_prefix}simvascular_geom${so_postfix}] \
				 [list Image ${lib_prefix}simvascular_image${so_postfix}] \
				 [list Utils ${lib_prefix}simvascular_utils${so_postfix}] \
				 [list Post ${lib_prefix}simvascular_post${so_postfix}] \
				 [list Solid ${lib_prefix}simvascular_solid${so_postfix}] \
				 [list Polydatasolid ${lib_prefix}simvascular_polydata_solid${so_postfix}] \
				 [list Occtsolid ${lib_prefix}simvascular_opencascade_solid${so_postfix}] \
				 [list Mesh ${lib_prefix}simvascular_mesh${so_postfix}] \
				 [list Mmgmesh ${lib_prefix}simvascular_mmg_mesh${so_postfix}] \
				 [list Tetgenmesh ${lib_prefix}simvascular_tetgen_mesh${so_postfix}] \
				 [list Adapt ${lib_prefix}simvascular_adaptor${so_postfix}] \
				 [list Tetgenadapt ${lib_prefix}simvascular_tetgen_adaptor${so_postfix}] \
				 [list Vmtkutils ${lib_prefix}simvascular_vmtk_utils${so_postfix}] \
				 ]

set gSimVascularTclInitLicensedLibs [list \
                                   [list Meshsimmesh ${lib_prefix}simvascular_meshsim_mesh${so_postfix}] \
                                   [list Meshsimadapt ${lib_prefix}simvascular_meshsim_adaptor${so_postfix}] \
                                   [list Meshsimsolid ${lib_prefix}simvascular_meshsim_solid${so_postfix}] \
                                   [list Meshsimdiscretesolid ${lib_prefix}simvascular_meshsim_discrete_solid${so_postfix}] \
                                   [list Parasolidsolid ${lib_prefix}simvascular_parasolid_solid${so_postfix}] \
					  ]

foreach lib $gSimVascularTclInitLibs {
    if {[lindex $lib 1] == ""} {
	continue
    }
    # try dynamic lib first
    if [catch {load [lindex $lib 1] [lindex $lib 0]} msg] {
	# then static lib
	if [catch {load {} [lindex $lib 0]} msg] {
          if {$SV_RELEASE_BUILD == 0} {
	    puts "error ([lindex $lib 0]) $msg"
          }
	}
    } else {
      if {$SV_RELEASE_BUILD == 0} {
	puts "loaded [lindex $lib 0] dynamically"
      }
    }
}

foreach lib $gSimVascularTclInitLicensedLibs {
    if {[lindex $lib 1] == ""} {
        continue
    }
    # try dynamic lib first
    if [catch {load [lindex $lib 1] [lindex $lib 0]} msg] {
        # then static lib
        if [catch {load {} [lindex $lib 0]} msg] {
          if {$SV_RELEASE_BUILD == 0} {
            puts "optional licensed library not found ([lindex $lib 0]) $msg"
          }
        }
    } else {
      if {$SV_RELEASE_BUILD == 0} {
        puts "loaded [lindex $lib 0] dynamically"
      }
    }
}

puts "Loading vtk procs..."
source [file join $env(SV_HOME) Tcl SimVascular_2.0 simvascular_vtk_init.tcl]
puts "Done loading vtk procs..."

# need package xml
lappend auto_path [file join $env(SV_HOME) Tcl External tclxml3.2]

# ----------------------
# load required packages
# ----------------------

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

# ---------------
# Import tcl code
# ---------------

if {$SV_RELEASE_BUILD} {

  # why do I need to prevent this from being read on windows?
  if {$tcl_platform(platform) != "windows"} {
    source [file join $env(SV_HOME) Tcl SimVascular_2.0 General-code.tcl]
  }
  set codefiles {Core-code.tcl General-code.tcl Plugins-code.tcl Vis-code.tcl Visualization-code.tcl}
  foreach codefile $codefiles {
    source [file join $env(SV_HOME) Tcl SimVascular_2.0 $codefile]
  }

  # seems to be missing sometimes
  lappend auto_path [file join $env(SV_HOME)/lib]
  
} else {

  source [file join $env(SV_HOME) Tcl SimVascular_2.0 simvascular_developer_startup.tcl]
  # set the global variables, some for the gui, some not!
  source [file join $env(SV_HOME) Tcl SimVascular_2.0 Core simvascular_init.tcl]

}

if {$SV_USE_TK_GUI} {
    if {$SV_RELEASE_BUILD} {
	source [file join $env(SV_HOME) Tcl SimVascular_2.0 GUI-code.tcl]
    } else {
        source [file join $env(SV_HOME) Tcl SimVascular_2.0 GUI gui.tcl]
    }
    catch {source [file join $env(SV_HOME) Tcl SimVascular_2.0 GUI splash.tcl]}
}

# Set executables
source [file join $env(SV_HOME) Tcl SimVascular_2.0 simvascular_find_external_execs.tcl]

# -------------------------
# Launch gui if interactive
# -------------------------

if {$SV_USE_TK_GUI} {

  # get last running directory from user's registry
  global tcl_platform
  if {$tcl_platform(platform) == "windows"} {
    package require registry
    if [catch {set lastdir [registry get "HKEY_CURRENT_USER\\Software\\SimVascular\\SimVascular\\$SV_TIMESTAMP" LastProjectDir]} msg] {
      catch {cd}
      set lastdir [pwd]
      if [catch {registry set "HKEY_CURRENT_USER\\Software\\SimVascular\\SimVascular\\$SV_TIMESTAMP" LastProjectDir $lastdir} msg] {
         puts "ERROR updating LastProjectDir in registry! ($msg)"
      }
    }

    # try and fix dir name if it no longer exists, go up one level
    if {![file isdirectory $lastdir]} {
      set lastdir [file dirname $lastdir]
    }

    puts "trying to return to: $lastdir"
    if [catch {cd $lastdir}] {
      catch {cd}
    }
  }

  # ------------------
  # Show splash screen
  # ------------------

  if {![info exists env(SV_BATCH_MODE)]} {
    catch {ShowWindow.splash} errmsg
    if {$errmsg !=""} {puts "tried splash.tcl $errmsg"}
  }
  catch {wm withdraw .}

  #  whenever a user tries to close a window by clicking on the
  #  x, this function gets called.  Ignore for now!
  #
  proc XFProcError args {
    puts "Please use \"CLOSE\" button instead!"
  }

  #
  #  NoFunction is currently required by text windows
  #  created by XF
  #

  if {[llength [info commands NoFunction]] == 0} {
    proc NoFunction {} {
	return
    }
  }

  # initialize our vtk graphics layer
  catch {vis_init}
  
  # tcl 8.4.x no longer exports tkTabToWindow
  if [catch {tk::unsupported::ExposePrivateCommand tkTabToWindow}] {
    proc tkTabToWindow {foo} {}
  }

  catch {package require Plotchart}
  
  # try and pick a nice looking theme if it exists

  source [file join $env(SV_HOME)/Tcl/External/tkcon.tcl]
  source [file join $env(SV_HOME)/Tcl/External/graph.tcl]

  #Start main gui but keep transparent while tkcon loads
  #Wait for 6 seconds and then close the splash screen
  #and show the main window with correct sizing
  mainGUI
  wm attributes .guiCV -alpha 0.0

  if {$tcl_platform(os) == "Linux"} {
     catch {ttk::style theme use aqua}
  }

  if {$tcl_platform(platform) == "windows"} {
     catch {ttk::style theme use winnative}
     catch {ttk::style theme use vista}
  }

  if {$tcl_platform(os) == "Darwin"} {
     catch {ttk::style theme use aqua}
  }

  after 6000 {set splash_delay_done 1}
  vwait splash_delay_done

  DestroyWindow.splash
  wm attributes .guiCV -alpha 1.0

  # --------------------------------
  # aliases (alias proc is in Tkcon)
  # --------------------------------
  catch {guiPREOPupdateImageDataType}

  set ::tkcon::PRIV(WWW) 1
  set ::tkcon::OPT(rows) 10
  set ::tkcon::OPT(cols) 40
  set ::tkcon::OPT(showmenu) 0
  set ::tkcon::COLOR(bg) white
  global symbolicName

  if { $SV_NO_RENDERER == "1" } {
    puts "Starting up in no render mode"
  } else {
    guiCV_display_windows 3d_only

  }

  set topbottom $symbolicName(main_top_bottom_right_panedwindow)
  set leftright $symbolicName(main_left_right_panedwindow)

  set w [winfo width $leftright]
  puts "width: $w"
  set sash0 [expr int($w/2)]
  puts "vert sashpos: [$leftright sashpos 0 $sash0]"

  set h [winfo height $topbottom]
  puts "height: $h"
  set sash0 [expr int($h/3)]
  puts "horz sashpos: [$topbottom sashpos 0 $sash0]"

  if {[info exists env(SV_REDIRECT_STDERR_STDOUT)]} {
  proc handle { args } {
       puts -nonewline [ set [ lindex $args 0 ] ]
  }
  set ::tail_stdout {}
  set ::tail_stderr {}

  trace variable ::tail_stdout w handle
  trace variable ::tail_stderr w handle

  tail stdout .+ 2000 ::tail_stdout
  tail stderr .+ 2000 ::tail_stderr
  }

  if {![info exists env(SV_NO_EMBED_TKCON)]} {
    if [info exists symbolicName(tkcon)] {
      set ::tkcon::PRIV(displayWin) $symbolicName(tkcon)
      set ::tkcon::PRIV(root) $symbolicName(tkcon)
    }
  }
  ::tkcon::Init -exec ""
  catch {wm title .tkcon "SimVascular Command Console"}

} else {

  #set env(HOME) $env(REALHOME)

}

# -------------------------
# Source script if provided
# -------------------------

if {$argc >= 1} {
   puts "argc: $argc  argv: $argv"
   if {[file exists [lindex $argv 0]]} {
       source [lindex $argv 0]
   } else {
       puts "ignored line: $argv"
   }
}


