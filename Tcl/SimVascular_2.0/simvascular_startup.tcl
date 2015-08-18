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
global SIMVASCULAR_BUILD_ID
if { [file exists [file join $simvascular_home/Tcl/startup_configure.tcl]]} {
  source [file join $simvascular_home/Tcl/startup_configure.tcl]
  if { [file exists [file join $simvascular_home/release-date]] } {
      set SIMVASCULAR_RELEASE_BUILD 1
      set fp [open "$simvascular_home/release-date" r]
      set timestamp [read $fp]
      set SIMVASCULAR_BUILD_ID $timestamp
      close $fp
  } else {
      set SIMVASCULAR_RELEASE_BUILD 0
      set SIMVASCULAR_BUILD_ID "developer build"
  }
} else {
    set SIMVASCULAR_FULL_VER_NO REPLACE_SIMVASCULAR_FULL_VER_NO
    set SIMVASCULAR_MAJOR_VER_NO REPLACE_SIMVASCULAR_MAJOR_VER_NO
    set SIMVASCULAR_TIMESTAMP   REPLACE_SIMVASCULAR_TIMESTAMP
    set SIMVASCULAR_PLATFORM    REPLACE_SIMVASCULAR_PLATFORM
    set SIMVASCULAR_VERSION     REPLACE_SIMVASCULAR_VERSION
    if {[string range $SIMVASCULAR_FULL_VER_NO 0 6] != "REPLACE"} { 
      set SIMVASCULAR_RELEASE_BUILD 1
      set timestamp [file tail $simvascular_home]
      set SIMVASCULAR_BUILD_ID $timestamp
      } else { 
        set SIMVASCULAR_RELEASE_BUILD 0
        set SIMVASCULAR_BUILD_ID "developer build"
      }
}


# if { $SIMVASCULAR_RELEASE_BUILD == 1}  {
#   puts "\nSimVascular Version $SIMVASCULAR_VERSION-$SIMVASCULAR_FULL_VER_NO (Released [clock format [clock scan $timestamp -format %y%m%d%H%M%S] ])"
# } else {
#   set SIMVASCULAR_VERSION $env(SIMVASCULAR_VERSION)
#   puts "\nSimVascular $SIMVASCULAR_VERSION-$SIMVASCULAR_FULL_VER_NO (developers build)"
# }
# puts "Copyright (c) 2014-2015 The Regents of the University of California."
# puts "                         All Rights Reserved."
# puts "----------------------------------------------------------------------------\n"

source [file join $env(SIMVASCULAR_HOME) Tcl SimVascular_2.0 simvascular_vtk_init.tcl]
#package require vtk

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

# ---------------
# Import tcl code
# ---------------

proc modules_registry_query {regpath regpathwow key} {
    global tcl_platform
    set value {}
    if {$tcl_platform(platform) == "windows"} {
      package require registry
	if {![catch {set value [registry get $regpath $key]} msg]} {
	  return $value
        } else {
	  if {![catch {set value [registry get $regpathwow $key]} msg]} {
	    return $value
	  }
	}
    } else {
	return
    }
}

if {$SIMVASCULAR_RELEASE_BUILD != 0} {
  catch {package require tbcload}
  # why do I need to prevent this from being read on windows?
  if {$tcl_platform(platform) != "windows"} {
    source [file join $env(SIMVASCULAR_HOME) Tcl SimVascular_2.0 General-code.tcl]
  }
  foreach codefile [glob [file join $env(SIMVASCULAR_HOME) Tcl SimVascular_2.0 *code.tcl]] {
    source $codefile
  }

  if {$tcl_platform(platform) == "windows"} {    
    set parasoliddll [modules_registry_query HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\Modules\\ParasolidAndMeshSim \
			  HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\Modules\\ParasolidAndMeshSim \
			  SIMVASCULAR_PARASOLID_DLL]
    
    if {$parasoliddll != ""} {
      puts "Found Parasolid.  Loading..."
      load $parasoliddll Parasolidsolid
    }

    set pschema_dir [modules_registry_query HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\Modules\\ParasolidAndMeshSim \
			  HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\Modules\\ParasolidAndMeshSim \
			  SIMVASCULAR_PARASOLID_PSCHEMA_DIR]
    
    if {$pschema_dir != ""} {
      puts "Found Parasolid Schema Directory."
      global env
      set env(P_SCHEMA) $pschema_dir
    }
 
    set discretedll [modules_registry_query HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\Modules\\ParasolidAndMeshSim \
			  HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\Modules\\ParasolidAndMeshSim \
			  SIMVASCULAR_MESHSIM_DISCRETE_DLL]
    
    if {$discretedll != ""} {
      puts "Found MeshSim Discrete Model.  Loading..."
      load $discretedll Meshsimdiscretesolid
    }

    set meshsimdll [modules_registry_query HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\Modules\\ParasolidAndMeshSim \
			  HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\Modules\\ParasolidAndMeshSim \
  			  SIMVASCULAR_MESHSIM_MESH_DLL]
    
    if {$meshsimdll != ""} {
      puts "Found MeshSim Mesh.  Loading..."
      load $meshsimdll Meshsimmesh
    }
  }

  if {$tcl_platform(platform) == "unix"} {
    catch {load $env(SIMVASCULAR_HOME)/lib_simvascular_meshsim_discrete.so Meshsimdiscretesolid} 
    catch {load $env(SIMVASCULAR_HOME)/lib_simvascular_meshsim_mesh.so Meshsimmesh}
    catch {load $env(SIMVASCULAR_HOME)/lib_simvascular_parasolid.so  Parasolidsolid}
  }

} else {
  if {$tcl_platform(platform) == "unix"} {
    catch {load $env(SIMVASCULAR_HOME)/BuildWithMake/Lib/lib_simvascular_meshsim_discrete.so Meshsimdiscretesolid} 
    catch {load $env(SIMVASCULAR_HOME)/BuildWithMake/Lib/lib_simvascular_meshsim_mesh.so Meshsimmesh}
    catch {load $env(SIMVASCULAR_HOME)/BuildWithMake/Lib/lib_simvascular_parasolid.so  Parasolidsolid}
  }

  source [file join $env(SIMVASCULAR_HOME) Tcl SimVascular_2.0 simvascular_developer_startup.tcl]
  if {[lsearch -exact $envnames SIMVASCULAR_BATCH_MODE] < 0} {
     catch {source [file join $env(SIMVASCULAR_HOME) Tcl SimVascular_2.0 GUI splash.tcl]}
  }
}

# ------------------
# Show splash screen
# ------------------

if {![info exists env(SIMVASCULAR_BATCH_MODE)]} {
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
if {[info exists env(SIMVASCULAR_BATCH_MODE)] == 0} {
  catch {vis_init}
}

if {$SIMVASCULAR_RELEASE_BUILD == 0} {
  source [file join $env(SIMVASCULAR_HOME) Tcl SimVascular_2.0 Core simvascular_init.tcl]
}

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
# ----------------
# Find executables
# ----------------

global tcl_platform
if {![file exists [file join $simvascular_home/Tcl/externals_configure.tcl]] } {
    if {$tcl_platform(platform) == "windows"} {
      package require registry
        if {[string range $SIMVASCULAR_VERSION end-1 end] == "32"} {
          if [catch {set rundir [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_MAJOR_VER_NO" RunDir]} msg] {
            puts "ERROR:  could not find registry key!"
            set rundir ""
          }
      } else {
        if [catch {set rundir [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_MAJOR_VER_NO" RunDir]} msg] {
          if [catch {set rundir [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_MAJOR_VER_NO" RunDir]} msg] {
            puts "ERROR: could not find registry key:\nHKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_MAJOR_VER_NO RunDir"
            set rundir ""
          }
        } 
      }
      set execext {.exe}
      set execbinext {-bin.exe}
    } else {
      if {$SIMVASCULAR_RELEASE_BUILD} {
        set rundir /usr/local/bin
      } else {
      set rundir ""
      }
      set execext {}
      set execbinext {}
    }
}
# get running directory from user's registry
global tcl_platform
if {$tcl_platform(platform) == "windows"} {
  package require registry
  if [catch {set lastdir [registry get "HKEY_CURRENT_USER\\Software\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_MAJOR_VER_NO" LastProjectDir]} msg] {
    catch {cd}
    set lastdir [pwd]
    if [catch {registry set "HKEY_CURRENT_USER\\Software\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_MAJOR_VER_NO" LastProjectDir $lastdir} msg] {
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

# for windows, we call the executables directly so don't append
# "32" to names like on linux when running 32-bit versions
global gExternalPrograms
if {[file exists [file join $simvascular_home/Tcl/externals_configure.tcl]] } {
  source [file join $simvascular_home/Tcl/externals_configure.tcl]
  } else {
    set executable_home $simvascular_home
    if {($SIMVASCULAR_VERSION == "simvascular32") && ($tcl_platform(platform) != "windows")} {
      set gExternalPrograms(cvpresolver) [file join $rundir presolver32$execbinext]
      set gExternalPrograms(cvpostsolver) [file join $rundir postsolver32$execbinext]
      set gExternalPrograms(cvadaptor) [file join $rundir adaptor32$execbinext]
      set gExternalPrograms(cvtetadaptor) [file join $rundir tetadaptor32$execbinext]
      set gExternalPrograms(cvflowsolver) [file join $rundir flowsolver32$execbinext]
      set gExternalPrograms(cvsolver) [file join $rundir solver32$execext]
      set gExternalPrograms(mpiexec) mpiexec
      set gExternalPrograms(dicom2) [file join $rundir dicom2$execext]
      if {$tcl_platform(platform) == "windows"} {
        set gExternalPrograms(dcmodify) [file join $rundir dcmodify$execext]
        set gExternalPrograms(dcmdump) [file join $rundir dcmdump$execext]
        } else {
          set gExternalPrograms(dcmodify) dcmodify
          set gExternalPrograms(dcmdump) dcmdump
        }
        set gExternalPrograms(gdcmdump) gdcmdump
        set gExternalPrograms(rundir) $rundir
        } else {
          if {$SIMVASCULAR_RELEASE_BUILD == 1} {
            set gExternalPrograms(cvpresolver) [file join $executable_home presolver$execbinext]
            set gExternalPrograms(cvpostsolver) [file join $executable_home postsolver$execbinext]
	    set gExternalPrograms(cvadaptor) [file join $executable_home adaptor$execbinext]
	    if {$tcl_platform(platform) == "windows"} {
	      if {![file exists $gExternalPrograms(cvadaptor)]} {
                  set meshsimdll [modules_registry_query HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\Modules\\ParasolidAndMeshSim \
			         HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\Modules\\ParasolidAndMeshSim \
				 SIMVASCULAR_MESHSIM_MESH_DLL]
		  if {$meshsimdll != ""} {
		      set gExternalPrograms(cvadaptor) [file join [file dirname $meshsimdll] [file tail $gExternalPrograms(cvadaptor)]]
		  }
	      } 
	    }
            set gExternalPrograms(cvtetadaptor) [file join $executable_home tetadaptor$execbinext]
            set gExternalPrograms(cvflowsolver) [file join $executable_home flowsolver$execbinext]
            #    set gExternalPrograms(cvsolver) [file join $simvascular_home solver$execext]
            #    TODO What to do for mpiexec?
            set gExternalPrograms(mpiexec) mpiexec
            set gExternalPrograms(dicom2) [file join $simvascular_home dicom2$execext]
            } else {
              set gExternalPrograms(cvpresolver) [file join $executable_home presolver$execbinext]
              set gExternalPrograms(cvpostsolver) [file join $simvascular_home mypost]
              set gExternalPrograms(cvadaptor) [file join $simvascular_home myadaptor]
              set gExternalPrograms(cvtetadaptor) [file join $simvascular_home mytetadaptor]
              set gExternalPrograms(cvflowsolver) [file join $simvascular_home mysolver]
              #    set gExternalPrograms(cvsolver) [file join $simvascular_home mysolver$execext]
              #    TODO What to do for mpiexec?
              set gExternalPrograms(mpiexec) mpiexec
              set gExternalPrograms(dicom2) [file join $simvascular_home dicom2$execext]

            }
            if {$tcl_platform(platform) == "windows"} {
              set gExternalPrograms(dcmodify) [file join $rundir dcmodify$execext]
              set gExternalPrograms(dcmdump) [file join $rundir dcmdump$execext]
              } else {
                set gExternalPrograms(dcmodify) dcmodify
                set gExternalPrograms(dcmdump) dcmdump
              }
              set gExternalPrograms(gdcmdump) gdcmdump
              set gExternalPrograms(rundir) $rundir
            }

            # try and find the default mpiexec on ubuntu
            #if {$tcl_platform(platform) != "windows" && $SIMVASCULAR_RELEASE_BUILD} {
            #  set gExternalPrograms(mpiexec) [file join $env(SIMVASCULAR_HOME) [file tail $gExternalPrograms(mpiexec)]]
            #}

          }

#
#  use registry to find mpiexec on windows
#

if {$tcl_platform(platform) == "windows"} {
  if {![catch {set mpi_install_dir [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\Microsoft\\MPI" InstallRoot]} msg]} {
      regsub -all {\\} $mpi_install_dir/bin/mpiexec.exe / gExternalPrograms(mpiexec)
  }
}

# -------------------------
# Launch gui if interactive
# -------------------------

if {[lsearch -exact $envnames SIMVASCULAR_BATCH_MODE] < 0} {
  # tcl 8.4.x no longer exports tkTabToWindow
  if [catch {tk::unsupported::ExposePrivateCommand tkTabToWindow}] {
    proc tkTabToWindow {foo} {}
  }

  source [file join $env(SIMVASCULAR_HOME)/Tcl/External/tkcon.tcl]
  source [file join $env(SIMVASCULAR_HOME)/Tcl/External/graph.tcl]

  after 5000 {set splash_delay_done 1}
  vwait splash_delay_done

  DestroyWindow.splash

  # try and pick a nice looking theme if it exists

  if {$tcl_platform(os) == "Linux"} {
     catch {ttk::style theme use clam}
  }

  if {$tcl_platform(platform) == "windows"} {
     catch {ttk::style theme use winnative}
     catch {ttk::style theme use vista}
  }

  mainGUI
  catch {wm withdraw .}

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

  after 5000 {set tkcon_delay_done 1}
  vwait tkcon_delay_done
  guiCV_display_windows 3d_only

  set topbottom $symbolicName(main_top_bottom_right_panedwindow)
  set leftright $symbolicName(main_left_right_panedwindow)

  set w [winfo width $leftright]
  #puts "width: $w"
  set sash0 300
  puts "sashpos: [$leftright sashpos 0 $sash0]"

  set h [winfo height $topbottom]
  #puts "height: $h"
  set sash0 320
  puts "sashpos: [$topbottom sashpos 0 $sash0]"

  if {[info exists env(SIMVASCULAR_REDIRECT_STDERR_STDOUT)]} {
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

  if {![info exists env(SIMVASCULAR_NO_EMBED_TKCON)]} {
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
   source [lindex $argv 0]
}
