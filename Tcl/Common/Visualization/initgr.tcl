#===========================================================================
#    
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
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

# ------
# initgr
# ------

proc initgr {renWin} {

    # Adding an ad hoc mechanism for naming the window:
    set pos [string first _ $renWin]
    if {$pos < 0} {
	set winName $renWin
    } else {
	set winName [string range $renWin [expr $pos + 1] end]
    }

    # -------
    # Set ren
    # -------

    if {![cmdExists $renWin]} {
	set ren [vis_renNew $renWin]
	[[$renWin GetInteractor] GetInteractorStyle] \
		SetTrackballModeToTrackball
    } else {
	[$renWin GetRenderers] InitTraversal
	set ren [[$renWin GetRenderers] GetNextItem]
    }
    if {[string length $winName] > 0} {
	$renWin SetWindowName $winName
    }
    vis_renSetBackground $ren 0 0 0

    # --------------------------------------------
    # Start a new picker which provides annotation
    # --------------------------------------------

    if {![cmdExists mypicker]} {
	vtkCellPicker mypicker
    }
    mypicker SetEndPickMethod annotatePick

    iren_$renWin SetPicker mypicker

    return $ren
}


# ------------
# annotatePick
# ------------

proc annotatePick {} {
    global ren
    set id [mypicker GetCellId]
    if {$id < 0} {
	vis_textSet $ren ""
	return
    }
    set a [findCmds _append_]
    [$a GetInput] InitTraversal
    for {set i 0} {$i < $id} {incr i} {
	[$a GetInput] GetNextItem
    }
    set pd [[$a GetInput] GetNextItem]
    set fn [[$pd GetSource] GetFileName]
    vis_textSet $ren [format "\[%d\]: %s" $id $fn]
}
