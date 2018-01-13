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

namespace eval u {}

proc ::u::printArray {name args} {
    #@author Nathan Wilson
    #@c Pretty printout of an array
    #@a name:  tcl array name
    #@a args:  optional output filename
    upvar 1 $name var
    #global $name
    foreach i [lsort [array names var]] {
       eval set val \$var\($i\)
       if {[string length $i] < 40} {
	   puts "[format %40s $i] = $val"
       } else {
           puts "$i = $val"
       }
       if {$args != ""} {
	 set fp [open $args a]
         if {[string length $i] < 40} {
	   puts $fp "[format %40s $i] = $val"
         } else {
           puts $fp "$i = $val"
         }
         close $fp
       }
    }
}


proc ::u::printList {mylist args} {

    #@author Nathan Wilson
    #@c Pretty printout of a tcl list
    #@a mylist:  tcl list (value)
    #@a args:  optional output filename

    if {$args != ""} {

      set fp [open $args a]
      foreach i $mylist {
        puts $fp $i
      }
      close $fp

    } else {

      foreach i $mylist {
        puts $i
      }

    }

}


proc ::u::mkPolygon {ptList dstName} {

    #@author Nathan Wilson
    #@c Create a vtkPolygon from a list of points
    #@a ptList:  tcl list of points to define polygon
    #@a dstName:  name of polygon to be generated

    set numPts [llength $ptList]
    if {$numPts < 3} {
	return -code error
    }

    vtkPolygon $dstName
    [$dstName GetPointIds] SetNumberOfIds $numPts

    for {set i 0} {$i < $numPts} {incr i} {
	set pt [lindex $ptList $i]
	set x [lindex $pt 0]
	set y [lindex $pt 1]
        if {[llength $pt] == 2} {
          set z 0
	} else {
	  set z [lindex $pt 2]
	}

        [$dstName GetPoints] InsertNextPoint $x $y $z
        [$dstName GetPointIds] SetId $i $i
    }

}


proc ::u::reverseList {inList} {
  #@author Nathan Wilson
  #@c Reverses a given tcl list.
  #@a inList: original list to reverse
  #@r reversed list
  for {set i [expr [llength $inList] - 1]} {$i >= 0} {incr i -1} {
    lappend rtnList [lindex $inList $i]
  }
  return $rtnList
}
