#===========================================================================
#    
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                           University of California, San Diego.
#
# All rights reserved.
#
# Portions of the code Copyright (c) 1998-2007 Stanford University,
# Charles Taylor, Nathan Wilson, Ken Wang.
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
#===========================================================================

# ------------------------------
# vis_utilsGetRGBfromColorString
# ------------------------------
#

proc vis_utilsGetRGBfromColorString {color} {

  # if color is specified in hex, convert to rgb
  if {[string index $color 0] == "\#"} {
    set color [string trim $color "\#"]
    puts "color: $color"
    if {[string length $color] == "4"} {
       set rgb [list [expr 0x[string index $color 0]]  \
                     [expr 0x[string index $color 1]]  \
                     [expr 0x[string index $color 2]]]
      set rgb [math_scaleVec $rgb [expr 1.0/[expr pow(2,4) - 1.0]]]
    } elseif {[string length $color] == "6"} {
       set rgb [list [expr 0x[string range $color 0 1]]  \
                     [expr 0x[string range $color 2 3]]  \
                     [expr 0x[string range $color 4 5]]]
       set rgb [math_scaleVec $rgb [expr 1.0/[expr pow(2,8) - 1.0]]]
    } elseif {[string length $color] == "9"} {
       set rgb [list [expr 0x[string range $color 0 2]] \
                     [expr 0x[string range $color 3 5]] \
                     [expr 0x[string range $color 6 8]]]
      set rgb [math_scaleVec $rgb [expr 1.0/[expr pow(2,12) - 1.0]]]
    } elseif {[string length $color] == "12"} {
       set rgb [list [expr 0x[string range $color 0 3]] \
                     [expr 0x[string range $color 4 7]] \
                     [expr 0x[string range $color 8 11]]]
      set rgb [math_scaleVec $rgb [expr 1.0/[expr pow(2,16) - 1.0]]]
    } else {
       return -code error "Invalid color length ($color)"
    }

  } else {

    # use vtk colors as default, unix colors otherwise case
    # note: unix color defaults to 0.5 0.5 0.5 if color not found
    # check to make sure color exists
    if {[info globals $color] == ""} {
     set rgb [gdscGetUnixColor $color]
    } else {
     global $color
     set rgb [eval set $color]
    }

  }

  #puts "rgb: $rgb"
  return $rgb

}
