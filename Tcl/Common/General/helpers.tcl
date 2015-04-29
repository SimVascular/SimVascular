#
#   Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                           University of California, San Diego.
#
#   All rights reserved.
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
