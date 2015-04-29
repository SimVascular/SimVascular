#===========================================================================
#    
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
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


proc vis_splineCircleWidgetAdd [list ren input name EnableEvent StartInteractionEvent \
                              InteractionEvent EndInteractionEvent] {

  set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
  # create interactive cursor
  catch {$interactor Delete}
  vtkSplineWidget $interactor
  $interactor SetInputData $input
  $interactor PlaceWidget

  set iren genericInteractor_$ren

  # Associate the point widget with the interactor
  $interactor SetInteractor $iren
  $interactor EnabledOff
  $interactor AddObserver EnableEvent $EnableEvent
  $interactor AddObserver StartInteractionEvent $StartInteractionEvent
  $interactor AddObserver InteractionEvent $InteractionEvent
  $interactor AddObserver EndInteractionEvent $EndInteractionEvent

  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
      # create interactive cursor
      catch {$interactor Delete}
      vtkSplineWidget $interactor
      $interactor SetInputData $input
      $interactor PlaceWidget

      set iren genericInteractor_$ren

      # Associate the point widget with the interactor
      $interactor SetInteractor $iren
      $interactor AddObserver EnableEvent $EnableEvent
      $interactor AddObserver StartInteractionEvent $StartInteractionEvent
      $interactor AddObserver InteractionEvent $InteractionEvent
      $interactor AddObserver EndInteractionEvent $EndInteractionEvent
    }
  }
}

proc vis_splineCircleWidgetRm {ren name} {
  set interactor "vis_splineCircleWidget_interactor_$ren\_$name"

  vis_splineCircleWidgetOff $ren $name

  set num [vis_splineCircleWidgetGetNumHandles $ren $name]
  for {set i 0} {$i < $num} {incr i} {
    set circleobj vis_splineCircleWidget_interactor_$ren\_$name\_circle$i
    catch {repos_delete -obj $circleobj}
    catch {vis_pRm $ren $circleobj}
  }

  $interactor Delete

  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
      $interactor Delete
    }
  }
}

proc vis_splineCircleWidgetOn {ren name} {
  set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
  $interactor EnabledOn
  # having problems getting spline to display correctly,
  # do a dummy update of handle 0 to update display
  set dummy [$interactor GetHandlePosition 0]
  eval $interactor SetHandlePosition 0 $dummy
  vis_render $ren
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
      $interactor EnabledOn
      # having problems getting spline to display correctly,
      # do a dummy update of handle 0 to update display
      set dummy [$interactor GetHandlePosition 0]
      eval $interactor SetHandlePosition 0 $dummy
      vis_render $ren
    }
  }
}

proc vis_splineCircleWidgetOff {ren name} {
  set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
  $interactor EnabledOff
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
      $interactor EnabledOff
    }
  }
}


proc vis_splineCircleWidgetSetPts {ren name pd} {

  set interactor "vis_splineCircleWidget_interactor_$ren\_$name"

  catch {$splineX Delete}
  catch {$splineY Delete}
  catch {$splineZ Delete}

  set pts {}
  geom_getPts $pd pts
  set numPts [llength $pts]

  $interactor SetNumberOfHandles $numPts

  set ptid 0
  foreach i $pts {
    $interactor SetHandlePosition $ptid [lindex $i 0] \
                                        [lindex $i 1] \
                                        [lindex $i 2]
    incr ptid
  }

  vis_splineCircleWidgetCalcTrans $ren $name $pts 1

  for {set i 0} {$i < $numPts} {incr i} {
    set transform "vis_splineCircleWidget_interactor_$ren\_$name\_trans$i"
    set circleobj "vis_splineCircleWidget_interactor_$ren\_$name\_circle$i"
    catch {repos_delete -obj $circleobj}
    geom_circle 30 0 0 $circleobj
    set actor [vis_pRepos $ren $circleobj]
    $actor SetUserTransform $transform
  }

  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
      $interactor SetNumberOfHandles $numPts
      set ptid 0
      foreach i $pts {
      $interactor SetHandlePosition $ptid [lindex $i 0] \
                                          [lindex $i 1] \
                                          [lindex $i 2]
      incr ptid
      }
    }
  }
}


proc vis_splineCircleWidgetUpdatePts {ren name pts} {

  set interactor "vis_splineCircleWidget_interactor_$ren\_$name"

  set numPts [llength $pts]

  set ptid 0
  foreach i $pts {
    $interactor SetHandlePosition $ptid [lindex $i 0] \
                                        [lindex $i 1] \
                                        [lindex $i 2]
    incr ptid
  }

  vis_splineCircleWidgetCalcTrans $ren $name $pts 0

  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
      set ptid 0
      foreach i $pts {
      $interactor SetHandlePosition $ptid [lindex $i 0] \
                                          [lindex $i 1] \
                                          [lindex $i 2]
      incr ptid
      }
    }
  }

}


proc vis_splineCircleWidgetGetPts {ren name} {
  set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
  set pts {}
  set numPts [$interactor GetNumberOfHandles]
  for {set i 0} {$i < $numPts} {incr i} {
    lappend pts [$interactor GetHandlePosition $i]
  }
  return $pts
}


proc vis_splineCircleWidgetSetNumHandles {ren name num} {
  set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
  $interactor SetNumberOfHandles $num

  set pts [vis_splineCircleWidgetGetPts $ren $name]
  set numPts [llength $pts]

  vis_splineCircleWidgetCalcTrans $ren $name $pts 1

  for {set i 0} {$i < $numPts} {incr i} {
    set transform "vis_splineCircleWidget_interactor_$ren\_$name\_trans$i"
    set circleobj "vis_splineCircleWidget_interactor_$ren\_$name\_circle$i"
    catch {repos_delete -obj $circleobj}
    geom_circle 30 0 0 $circleobj
    set actor [vis_pRepos $ren $circleobj]
    $actor SetUserTransform $transform
  }

  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
      $interactor SetNumberOfHandles $num
    }
  }
}


proc vis_splineCircleWidgetGetNumHandles {ren name} {
  set interactor "vis_splineCircleWidget_interactor_$ren\_$name"
  return [$interactor GetNumberOfHandles]
}


proc vis_splineCircleWidgetCalcTrans {ren name pts createTransforms} {
  set splineX "vis_splineCircleWidget_interactor_$ren\_$name\_splineX"
  set splineY "vis_splineCircleWidget_interactor_$ren\_$name\_splineY"
  set splineZ "vis_splineCircleWidget_interactor_$ren\_$name\_splineZ"
  catch {$splineX Delete}
  catch {$splineY Delete}
  catch {$splineZ Delete}
  vtkCardinalSpline $splineX
  $splineX ClampValueOff
  $splineX ClosedOff
  vtkCardinalSpline $splineY
  $splineY ClampValueOff
  $splineY ClosedOff
  vtkCardinalSpline $splineZ
  $splineZ ClampValueOff
  $splineZ ClosedOff

  set numPts [llength $pts]
  for {set i 0} {$i < $numPts} {incr i} {
    set pt [lindex $pts $i]
    $splineX AddPoint $i [lindex $pt 0]
    $splineY AddPoint $i [lindex $pt 1]
    $splineZ AddPoint $i [lindex $pt 2]
  }

  for {set i 0} {$i < $numPts} {incr i} {
     set transform "vis_splineCircleWidget_interactor_$ren\_$name\_trans$i"
     if {$createTransforms == 1} {
	 catch {$transform Delete}
         vtkTransform $transform
     }

     set x [$splineX Evaluate $i]
     set y [$splineY Evaluate $i]
     set z [$splineZ Evaluate $i]
     set pt1 [list $x $y $z]

     if {$i < [expr $numPts - 1]} {
       set x [$splineX Evaluate [expr $i + 0.1]]
       set y [$splineY Evaluate [expr $i + 0.1]]
       set z [$splineZ Evaluate [expr $i + 0.1]]
     } else {
       set x [$splineX Evaluate [expr $i - 0.1]]
       set y [$splineY Evaluate [expr $i - 0.1]]
       set z [$splineZ Evaluate [expr $i - 0.1]]
     }
     set pt2 [list $x $y $z]
     puts "pt1: $pt1"
     puts "pt2: $pt2"
     set norm [math_normalize [math_subVectors $pt2 $pt1]]
     set angle [math_radToDeg [math_angleBtw3DVectors {0 0 1} $norm]]
     set cross [math_cross {0 0 1} $norm]
     #catch {repos_delete -obj line_$i}
     #geom_mkLinesFromPts [list $pt1 \
     #        [math_addVectors $pt1 [math_scaleVec $norm 20]]] line_$i 0
     $transform Identity
     $transform PostMultiply
     $transform RotateWXYZ $angle [lindex $cross 0] [lindex $cross 1] [lindex $cross 2]
     $transform Translate [lindex $pt1 0] [lindex $pt1 1] [lindex $pt1 2]
  }
}


