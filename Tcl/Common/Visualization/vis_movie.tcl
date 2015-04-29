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

proc vis_movieInit {ren} {
  # init global variable to track saved states
  global gMoviesSavedCameraStates
  set gMoviesSavedCameraStates($ren) {}
  global gMoviesTrackState
  set gMoviesTrackState 0

  global symbolicName
  set r1 $symbolicName(gRen3d)

  # when the user presses "m" in the view window, add the state
  # to our tracked list
  bind $r1 <KeyPress-m> {vis_movieAddCameraState %W}
  bind $r1 <KeyPress-M> {vis_movieRemoveLastCameraState %W}

}

proc vis_movieGetNumberOfSavedStates {ren} {
  #author Nathan Wilson
  #@c return the number of saved camera states
  #@a ren: render
  #@r the current list of saved states
  global gMoviesSavedCameraStates
  return [llength $gMoviesSavedCameraStates($ren)]

}

proc vis_movieAddCameraState {widget} {
  puts "in it"
  #@author Nathan Wilson
  global gMoviesSavedCameraStates
  set rendercollection [[$widget GetRenderWindow] GetRenderers]
  $rendercollection InitTraversal
  for {set i 0} {$i < [$rendercollection GetNumberOfItems]} {incr i} {
    set ren [$rendercollection GetItemAsObject $i]
    set cam [$ren GetActiveCamera]
    set setMethods [FindSetMethods $cam]
    set getMethods [FindGetMethods $cam]
    set currentState {}
    puts "$cam $ren"
    foreach gm $getMethods {
	set root [string range $gm 1 end]
	set target [format "S%s" $root]
	set id [lsearch -exact $setMethods $target]
        puts "target: $target"
	if {$id >= 0} {
	    set sm [lindex $setMethods $id]
	    lappend currentState [list $target [$cam $gm]]
	}
    }
    lappend gMoviesSavedCameraStates($ren) $currentState
    # can only handle one render at the moment
    break
  }

}

proc vis_movieCreateSmoothStates {ren numPts numModes} {

  global gMoviesSavedCameraStates
  set maxval [vis_movieGetNumberOfSavedStates $ren]
  set clippingRange {}
  set focalPoint {}
  set position {}
  set viewUp {}
  for {set id 0} {$id < $maxval} {incr id} {
    set myState [lindex $gMoviesSavedCameraStates($ren) $id]
    foreach setting $myState {
      set arg1 [lindex $setting 0]
      set arg2 [lindex $setting 1]
      if {$arg1 == "SetClippingRange"} {
	 lappend clippingRange [list [lindex $arg2 0] [lindex $arg2 1] 0]
      } elseif {$arg1 == "SetFocalPoint"} {
         lappend focalPoint $arg2
      } elseif {$arg1 == "SetPosition"} {
         lappend position $arg2
      } elseif {$arg1 == "SetViewUp"} {
         lappend viewUp $arg2
      }
    }
  }

  set inClip  [math_linearInterpCurve -pts $clippingRange -numInterpPts $numPts -closed 1]
  set inFocal [math_linearInterpCurve -pts $focalPoint    -numInterpPts $numPts -closed 1]
  set inView  [math_linearInterpCurve -pts $viewUp        -numInterpPts $numPts -closed 1]
  set inPos   [math_linearInterpCurve -pts $position      \
                                      -numInterpPts [expr $numPts * 2 +1] -closed 1]
  set forPos  [math_smoothCurve -pts $inPos -closed 1 -numModes $numModes -numInterpPts $numPts]

  set gMoviesSavedCameraStates($ren,clippingRange) $inClip
  set gMoviesSavedCameraStates($ren,focalPoint) $inFocal
  set gMoviesSavedCameraStates($ren,viewUp) $inView
  set gMoviesSavedCameraStates($ren,position) $forPos
  set gMoviesSavedCameraStates($ren,numPts) $numPts
#  geom_openLinesFromPts $inPos nate
#  geom_openLinesFromPts $forPos seth

}


proc vis_movieRemoveLastCameraState {widget} {

  global gMoviesSavedCameraStates
  set rendercollection [[$widget GetRenderWindow] GetRenderers]
  $rendercollection InitTraversal
  for {set i 0} {$i < [$rendercollection GetNumberOfItems]} {incr i} {
    set ren [$rendercollection GetItemAsObject $i]
    set gMoviesSavedCameraStates($ren) [lrange $gMoviesSavedCameraStates($ren) 0 end-1]
    # can only handle one render at the moment
    break
  }

}

proc vis_movieRetrieveState {ren id} {
  global gMoviesSavedCameraStates
  set maxval [vis_movieGetNumberOfSavedStates $ren]
  if {$id >= $maxval} {
     return -code error "ERROR:  exceeded maximum allowable state."
  }
  set cam [$ren GetActiveCamera]
  set myState [lindex $gMoviesSavedCameraStates($ren) $id]
    set clippingRange {}
    set focalPoint {}
    set position {}
    set viewUp {}
  foreach setting $myState {
    set arg1 [lindex $setting 0]
    set arg2 [lindex $setting 1]
    if {$arg1 == "SetClippingRange"} {
       set clippingRange $arg2
    } elseif {$arg1 == "SetFocalPoint"} {
       set focalPoint $arg2
    } elseif {$arg1 == "SetPosition"} {
       set position $arg2
       puts "$id $arg2"
    } elseif {$arg1 == "SetViewUp"} {
       set viewUp $arg2
    }
  }

  if {$clippingRange == "" || $focalPoint == "" \
      || $position == "" || $viewUp == ""} {
    return -code error "ERROR:  missing necessary parameter."
  }

  eval $cam SetClippingRange  $clippingRange
  eval $cam SetFocalPoint     $focalPoint
  eval $cam SetPosition       $position
  $cam ComputeViewPlaneNormal
  eval $cam SetViewUp         $viewUp

  [$ren GetRenderWindow] Render
}



proc vis_movieRetrieveSmoothState {ren id} {

  global gMoviesSavedCameraStates
  set maxval $gMoviesSavedCameraStates($ren,numPts)

  if {$id >= $maxval} {
     return -code error "ERROR:  exceeded maximum allowable state."
  }
  set cam [$ren GetActiveCamera]
  set clippingRange [lindex $gMoviesSavedCameraStates($ren,clippingRange) $id]
  set focalPoint    [lindex $gMoviesSavedCameraStates($ren,focalPoint) $id]
  set position      [lindex $gMoviesSavedCameraStates($ren,position) $id]
  set viewUp        [lindex $gMoviesSavedCameraStates($ren,viewUp) $id]

  $cam SetClippingRange [lindex $clippingRange 0] [lindex $clippingRange 1]
  eval $cam SetFocalPoint     $focalPoint
  eval $cam SetPosition       $position
  $cam ComputeViewPlaneNormal
  eval $cam SetViewUp         $viewUp

  [$ren GetRenderWindow] Render
}


proc vis_movieRetrieveNextState {ren} {

  set returnflag 0

  set maxval [vis_movieGetNumberOfSavedStates $ren]

  global gMoviesTrackState

  if {$gMoviesTrackState >= $maxval} {
     set returnflag 1
     #puts "starting over!"
     set gMoviesTrackState 0
  }

  vis_movieRetrieveState $ren $gMoviesTrackState
  incr gMoviesTrackState
  return $returnflag

}


proc vis_movieRetrieveNextSmoothState {ren} {

  set returnflag 0

  global gMoviesSavedCameraStates
  set maxval $gMoviesSavedCameraStates($ren,numPts)

  global gMoviesTrackState

  if {$gMoviesTrackState >= $maxval} {
     set returnflag 1
     #puts "starting over!"
     set gMoviesTrackState 0
  }

  vis_movieRetrieveSmoothState $ren $gMoviesTrackState
  incr gMoviesTrackState
  return $returnflag

}


proc vis_movieInitTraversal {ren} {

  global gMoviesTrackState
  set gMoviesTrackState 0

}


proc vis_movieSaveCameraStatesToFile {filename} {
  global gMoviesSavedCameraStates
  set fp [open $filename w]
  puts $fp "# geodesic_movie_states 1.0"
  puts $fp "global gMoviesSavedCameraStates"
  foreach ren [array names gMoviesSavedCameraStates] {
    puts $fp "set gMoviesSavedCameraStates($ren) \\"
    puts $fp "  \[list \\"
    foreach state $gMoviesSavedCameraStates($ren) {
      puts $fp "   \[list \\"
        foreach opt $state {
	  puts $fp "    \{\{[lindex $opt 0]\} \{[lindex $opt 1]\}\} \\"
	}
      puts $fp "   \] \\"
    }
    puts $fp "  \]"
  }
  puts $fp ""
  close $fp
}

proc vis_movieLoadCameraStatesFromFile {filename} {
   global gMoviesSavedCameraStates
   global gMoviesTrackState
   set gMoviesTrackState 0
   unset gMoviesSavedCameraStates
   source $filename
}

proc vis_movieSaveImages {ren numPts numModes filePrefix} {
   vis_movieCreateSmoothStates $ren $numPts $numModes
   vis_movieInitTraversal $ren
   set id 1
   while {![vis_movieRetrieveNextSmoothState $ren]} {
      vis_renWriteJPEG $ren $filePrefix-[format "%03i" $id].jpg
      incr id
   }
}

proc vis_moviePlay     {ren numPts numModes} {
   vis_movieCreateSmoothStates $ren $numPts $numModes
   vis_movieInitTraversal $ren
   set id 1
   while {![vis_movieRetrieveNextSmoothState $ren]} {
      incr id
   }
}
