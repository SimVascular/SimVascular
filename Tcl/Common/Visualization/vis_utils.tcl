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
     set rgb [svGetUnixColor $color]
    } else {
     global $color
     set rgb [eval set $color]
    }

  }

  #puts "rgb: $rgb"
  return $rgb

}
