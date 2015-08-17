#
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
#
# All rights reserved.
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
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#

global auto_path

if {$SIMVASCULAR_RELEASE_BUILD == 0} {
  source $simvascular_home/Tcl/Common/General/tmpobj.tcl
  source $simvascular_home/Tcl/Common/General/helpers.tcl
}

# ----------------
# Set up auto_path
# ----------------
set auto_path [linsert $auto_path 0 $simvascular_home/Tcl/SimVascular_2.0/Core]

# need package xml
lappend auto_path [file join $simvascular_home Tcl External tclxml3.2]

# gui stuff
if {[info exists env(SIMVASCULAR_BATCH_MODE)] == 0} {

  if {$SIMVASCULAR_RELEASE_BUILD == 0} {
    source $simvascular_home/Tcl/Common/Vis/actor.tcl
    source $simvascular_home/Tcl/Common/Vis/obj.tcl
    source $simvascular_home/Tcl/Common/Vis/img.tcl
    source $simvascular_home/Tcl/Common/Vis/ren.tcl
    source $simvascular_home/Tcl/Common/Vis/tk.tcl
    source $simvascular_home/Tcl/Common/Vis/init.tcl
    source $simvascular_home/Tcl/Common/Vis/text.tcl
    source $simvascular_home/Tcl/Common/Vis/poly.tcl
  }
    set auto_path [linsert $auto_path 0 $simvascular_home/Tcl/Common/Visualization]
    set auto_path [linsert $auto_path 0 $simvascular_home/Tcl/Common/Vis]
    set auto_path [linsert $auto_path 0 $simvascular_home/Tcl/Common/General]
    set auto_path [linsert $auto_path 0 $simvascular_home/Tcl/OSMSC]
    package require tile
    set auto_path [linsert $auto_path 0 $simvascular_home/Tcl/SimVascular_2.0/Core]
    set auto_path [linsert $auto_path 0 $simvascular_home/Tcl/SimVascular_2.0/GUI]
    set auto_path [linsert $auto_path 0 $simvascular_home/Tcl/SimVascular_2.0/Plugins]
}

proc upix {} {
  global auto_path
  set start [pwd]
  foreach dir $auto_path {
    if {[file owned $dir]} {
      cd $dir
      if {[llength [glob -nocomplain *.tcl]] > 0} {
        catch {auto_mkindex . *.tcl}
      }
    }
  }
  cd $start
}

# wrap the call to upix so that it *can* be disabled in released versions
if {$SIMVASCULAR_RELEASE_BUILD == 0} {
  # if we are running in batch mode, can't
  # regenerate the indexes constantly or
  # else auto_index gets confused
  if {[info exists env(SIMVASCULAR_BATCH_MODE)] == 0} {
    upix
  }
}

# load packages if dynamically build
catch {load $env(SIMVASCULAR_HOME)/BuildWithMake/Lib/lib_simvascular_meshsim_mesh.dll Meshsimmesh}
catch {load $env(SIMVASCULAR_HOME)/BuildWithMake/Lib/lib_simvascular_parasolid.dll Parasolidsolid}
catch {load $env(SIMVASCULAR_HOME)/BuildWithMake/Lib/lib_simvascular_meshsim_discrete.dll Meshsimdiscretesolid}
