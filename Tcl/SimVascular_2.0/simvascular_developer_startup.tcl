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

global auto_path

if {$SV_RELEASE_BUILD == 0} {
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
if {[info exists env(SV_BATCH_MODE)] == 0} {

  if {$SV_RELEASE_BUILD == 0} {
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
if {$SV_RELEASE_BUILD == 0} {
  # if we are running in batch mode, can't
  # regenerate the indexes constantly or
  # else auto_index gets confused
  if {[info exists env(SV_BATCH_MODE)] == 0} {
    upix
  }
}
