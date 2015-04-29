# Copyright (c) 2009-2011 Open Source Medical Software Corporation, 
# University of California, San Diego.
#
# Portions of the code Copyright (c) 1998-2007 Stanford University,
# Charles Taylor, Nathan Wilson, Ken Wang.
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

proc ::vis::tkBindTkRenderWidget {widget} {

    #@author Nathan Wilson
    #@c Attach default interaction bindings to TkRenderWidget
    #@a widget:  widget name

    bind $widget <Any-ButtonPress> {::vis::tkStartMotion %W %x %y}
    bind $widget <Any-ButtonRelease> {::vis::tkEndMotion %W %x %y}
#    bind $widget <B1-Motion> {::vis::tkRotate %W %x %y}
#    bind $widget <B1-ButtonRelease> {::vis::tkB1release %W %x %y}
    bind $widget <B2-Motion> {::vis::tkPan %W %x %y}
    bind $widget <B3-Motion> {::vis::tkZoom %W %x %y}
#    bind $widget <Shift-B1-Motion> {::vis::tkPan %W %x %y}
#    bind $widget <Control-B1-Motion> {::vis::tkSpecialRotate %W %x %y}
    bind $widget <KeyPress> {::vis::tkEnter %W %x %y}
#    bind $widget <KeyPress-i> {::vis::tkToggle %W %x %y}
    bind $widget <KeyPress-r> {::vis::tkReset %W %x %y}
    bind $widget <KeyPress-u> {wm deiconify .vtkInteract}
    bind $widget <KeyPress-w> {::vis::tkWireframe %W}
    bind $widget <KeyPress-s> {::vis::tkSurface %W}
    bind $widget <KeyPress-p> {::vis::tkPickActor %W %x %y}
# hack that assumes you've used code similar to vis_initTKgr to create
# window (hack no longer seems to work with paned windows)
    #bind .[lindex [split $widget .] 1] <FocusIn> "focus $widget"
    #bind $widget <Enter> {::vis::tkEnter %W %x %y}
    #bind $widget <Enter> {Enter %W %x %y}
    #bind $widget <Leave> {Leave $::vis::tkvars(oldFocus)}
    bind $widget <Expose> {::vis::tkExpose %W}
    bind $widget <Control-B1-Motion> {::vis::tkIncrementColorMap %W %x %y 0 0}
    bind $widget <Control-r> {::vis::tkResetColorMap %W %x %y}
    bind $widget <Key-Left>  {::vis::tkIncrementColorMap %W %x %y -1  0}
    bind $widget <Key-Right> {::vis::tkIncrementColorMap %W %x %y  1  0}
    bind $widget <Key-Up>    {::vis::tkIncrementColorMap %W %x %y  0  1}
    bind $widget <Key-Down>  {::vis::tkIncrementColorMap %W %x %y  0 -1}
    bind $widget <Key-plus>  {set ::vis::tkvars(PLUSMINUS) 1}
    bind $widget <Key-minus> {set ::vis::tkvars(PLUSMINUS) -1}
}


# a litle more complex than just "bind $widget <Expose> {%W Render}"
# we have to handle all pending expose events otherwise they que up.
proc ::vis::tkExpose {widget} {

    #@author Nathan Wilson
    #@c This code handles switching focus
    #@a widget:  render widget name

    if [info exists ::vis::tkvars(InExpose_$widget)] { 
      if {$::vis::tkvars(InExpose_$widget) == 1} {
	  return
      }
    }
    set ::vis::tkvars(InExpose_$widget) 1 
    [$widget GetRenderWindow] SetDesiredUpdateRate $::vis::tkvars(StillUpdateRate)
    update
    [$widget GetRenderWindow] Render
    set ::vis::tkvars(InExpose_$widget) 0

}

# Create event bindings
#
proc ::vis::tkRender {widget} {

    #@author Nathan Wilson
    #@c Render the widget using the current camera info pointed to in tkvars
    #@a widget:  render widget name

    eval $::vis::tkvars(CurrentLight) SetPosition [$::vis::tkvars(CurrentCamera) GetPosition]
    eval $::vis::tkvars(CurrentLight) SetFocalPoint [$::vis::tkvars(CurrentCamera) GetFocalPoint]

    $widget Render

}

proc ::vis::tkUpdateRenderer {widget x y} {

    #@author Nathan Wilson
    #@c Update the widget
    #@a widget:  render widget name
    #@a x:  x location
    #@a y:  y location

    # Get the renderer window dimensions
    set WindowX [lindex [$widget configure -width] 4]
    set ::vis::tkvars(WindowY) [lindex [$widget configure -height] 4]

    # Find which renderer event has occurred in
    set ::vis::tkvars(CurrentRenderWindow) [$widget GetRenderWindow]
    set renderers [$::vis::tkvars(CurrentRenderWindow) GetRenderers]
    set numRenderers [$renderers GetNumberOfItems]

    $renderers InitTraversal; set ::vis::tkvars(RendererFound) 0
    for {set i 0} {$i < $numRenderers} {incr i} {
        set ::vis::tkvars(CurrentRenderer) [$renderers GetNextItem]
        set vx [expr double($x) / $WindowX]
        set vy [expr ($::vis::tkvars(WindowY) - double($y)) / $::vis::tkvars(WindowY)]
        set viewport [$::vis::tkvars(CurrentRenderer) GetViewport]
        set vpxmin [lindex $viewport 0]
        set vpymin [lindex $viewport 1]
        set vpxmax [lindex $viewport 2]
        set vpymax [lindex $viewport 3]
        if { $vx >= $vpxmin && $vx <= $vpxmax && \
        $vy >= $vpymin && $vy <= $vpymax} {
            set ::vis::tkvars(RendererFound) 1
            set ::vis::tkvars(WindowCenterX) [expr double($WindowX)*(($vpxmax - $vpxmin)/2.0\
                                + $vpxmin)]
            set ::vis::tkvars(WindowCenterY) [expr double($::vis::tkvars(WindowY))*(($vpymax - $vpymin)/2.0\
                                + $vpymin)]
            break
        }
    }
    
    set ::vis::tkvars(CurrentCamera) [$::vis::tkvars(CurrentRenderer) GetActiveCamera]
    set lights [$::vis::tkvars(CurrentRenderer) GetLights]
    $lights InitTraversal; set ::vis::tkvars(CurrentLight) [$lights GetNextItem]
   
    set ::vis::tkvars(LastX) $x
    set ::vis::tkvars(LastY) $y
}

proc ::vis::tkEnterEnter {widget x y} {

    #@author Nathan Wilson
    #@c Take focus
    #@a widget:  render widget name
    #@a x:  x location
    #@a y:  y location

    set ::vis::tkvars(oldFocus) [focus]
    focus $widget
    ::vis::tkUpdateRenderer $widget $x $y
}

proc ::vis::tkStartMotion {widget x y} {

    #@author Nathan Wilson
    #@c Start motion based on location
    #@a widget:  render widget name
    #@a x:  x location
    #@a y:  y location

    if {[focus] != $widget} {
       focus $widget
    }
    ::vis::tkUpdateRenderer $widget $x $y
    if { ! $::vis::tkvars(RendererFound) } { return }

    $::vis::tkvars(CurrentRenderWindow) SetDesiredUpdateRate $::vis::tkvars(InteractiveUpdateRate)
}

proc ::vis::tkEndMotion {widget x y} {

    #@author Nathan Wilson
    #@c End motion
    #@a widget:  render widget name
    #@a x:  x location
    #@a y:  y location

    if { ! $::vis::tkvars(RendererFound) } {return}
    $::vis::tkvars(CurrentRenderWindow) SetDesiredUpdateRate $::vis::tkvars(StillUpdateRate)

    ::vis::tkRender $widget
}

proc ::vis::tkRotateRotate {widget x y {shouldRender 1}} {

    #@author Nathan Wilson
    #@c Rotate render window based on cursor location
    #@a widget:  render widget name
    #@a x:  x location
    #@a y:  y location
    #@a shouldRender:  update display (default 1)

    if { ! $::vis::tkvars(RendererFound) } { return }

    $::vis::tkvars(CurrentCamera) Azimuth [expr ($::vis::tkvars(LastX) - $x)]
    $::vis::tkvars(CurrentCamera) Elevation [expr ($y - $::vis::tkvars(LastY))]
    $::vis::tkvars(CurrentCamera) OrthogonalizeViewUp

    set ::vis::tkvars(LastX) $x
    set ::vis::tkvars(LastY) $y

    if $shouldRender {
	::vis::tkRender $widget
    }

}


proc ::vis::tkPan {widget x y} {

    #@author Nathan Wilson
    #@c Pan the window
    #@a widget:  render widget name
    #@a x:  x location
    #@a y:  y location

    if { ! $::vis::tkvars(RendererFound) } { return }

    set FPoint [$::vis::tkvars(CurrentCamera) GetFocalPoint]
        set FPoint0 [lindex $FPoint 0]
        set FPoint1 [lindex $FPoint 1]
        set FPoint2 [lindex $FPoint 2]

    set PPoint [$::vis::tkvars(CurrentCamera) GetPosition]
        set PPoint0 [lindex $PPoint 0]
        set PPoint1 [lindex $PPoint 1]
        set PPoint2 [lindex $PPoint 2]

    $::vis::tkvars(CurrentRenderer) SetWorldPoint $FPoint0 $FPoint1 $FPoint2 1.0
    $::vis::tkvars(CurrentRenderer) WorldToDisplay
    set DPoint [$::vis::tkvars(CurrentRenderer) GetDisplayPoint]
    set focalDepth [lindex $DPoint 2]

    set APoint0 [expr $::vis::tkvars(WindowCenterX) + ($x - $::vis::tkvars(LastX))]
    set APoint1 [expr $::vis::tkvars(WindowCenterY) - ($y - $::vis::tkvars(LastY))]

    $::vis::tkvars(CurrentRenderer) SetDisplayPoint $APoint0 $APoint1 $focalDepth
    $::vis::tkvars(CurrentRenderer) DisplayToWorld
    set RPoint [$::vis::tkvars(CurrentRenderer) GetWorldPoint]
        set RPoint0 [lindex $RPoint 0]
        set RPoint1 [lindex $RPoint 1]
        set RPoint2 [lindex $RPoint 2]
        set RPoint3 [lindex $RPoint 3]
    if { $RPoint3 != 0.0 } {
        set RPoint0 [expr $RPoint0 / $RPoint3]
        set RPoint1 [expr $RPoint1 / $RPoint3]
        set RPoint2 [expr $RPoint2 / $RPoint3]
    }

    $::vis::tkvars(CurrentCamera) SetFocalPoint \
      [expr ($FPoint0 - $RPoint0)/2.0 + $FPoint0] \
      [expr ($FPoint1 - $RPoint1)/2.0 + $FPoint1] \
      [expr ($FPoint2 - $RPoint2)/2.0 + $FPoint2]

    $::vis::tkvars(CurrentCamera) SetPosition \
      [expr ($FPoint0 - $RPoint0)/2.0 + $PPoint0] \
      [expr ($FPoint1 - $RPoint1)/2.0 + $PPoint1] \
      [expr ($FPoint2 - $RPoint2)/2.0 + $PPoint2]

    set ::vis::tkvars(LastX) $x
    set ::vis::tkvars(LastY) $y

    ::vis::tkRender $widget
}

proc ::vis::tkZoom {widget x y} {

    #@author Nathan Wilson
    #@c Zoom in the window
    #@a widget:  render widget name
    #@a x:  x location
    #@a y:  y location

    if { ! $::vis::tkvars(RendererFound) } { return }

    set zoomFactor [expr pow(1.02,(0.5*($y - $::vis::tkvars(LastY))))]

    if {[$::vis::tkvars(CurrentCamera) GetParallelProjection]} {
	set parallelScale [expr [$::vis::tkvars(CurrentCamera) GetParallelScale] * $zoomFactor];
	$::vis::tkvars(CurrentCamera) SetParallelScale $parallelScale;
    } else {
	$::vis::tkvars(CurrentCamera) Dolly $zoomFactor
	$::vis::tkvars(CurrentRenderer) ResetCameraClippingRange
    }

    set ::vis::tkvars(LastX) $x
    set ::vis::tkvars(LastY) $y

    ::vis::tkRender $widget
}

proc ::vis::tkReset {widget x y} {

    #@author Nathan Wilson
    #@c Reset the view window
    #@a widget:  render widget name
    #@a x:  x location
    #@a y:  y location

    # Get the renderer window dimensions
    set WindowX [lindex [$widget configure -width] 4]
    set ::vis::tkvars(WindowY) [lindex [$widget configure -height] 4]

    # Find which renderer event has occurred in
    set ::vis::tkvars(CurrentRenderWindow) [$widget GetRenderWindow]
    set renderers [$::vis::tkvars(CurrentRenderWindow) GetRenderers]
    set numRenderers [$renderers GetNumberOfItems]

    $renderers InitTraversal; set ::vis::tkvars(RendererFound) 0
    for {set i 0} {$i < $numRenderers} {incr i} {
        set ::vis::tkvars(CurrentRenderer) [$renderers GetNextItem]
        set vx [expr double($x) / $WindowX]
        set vy [expr ($::vis::tkvars(WindowY) - double($y)) / $::vis::tkvars(WindowY)]

        set viewport [$::vis::tkvars(CurrentRenderer) GetViewport]
        set vpxmin [lindex $viewport 0]
        set vpymin [lindex $viewport 1]
        set vpxmax [lindex $viewport 2]
        set vpymax [lindex $viewport 3]
        if { $vx >= $vpxmin && $vx <= $vpxmax && \
        $vy >= $vpymin && $vy <= $vpymax} {
            set ::vis::tkvars(RendererFound) 1
            break
        }
    }

    if { $::vis::tkvars(RendererFound) } {$::vis::tkvars(CurrentRenderer) ResetCamera}

    ::vis::tkRender $widget
}

proc ::vis::tkWireframe {widget} {

    #@author Nathan Wilson
    #@c Switch to wireframe
    #@a widget:  render widget name

    set actors [$::vis::tkvars(CurrentRenderer) GetActors]

    $actors InitTraversal
    set actor [$actors GetNextItem]
    while { $actor != "" } {
        [$actor GetProperty] SetRepresentationToWireframe
        set actor [$actors GetNextItem]
    }

    ::vis::tkRender $widget
}

proc ::vis::tkSurface {widget} {

    #@author Nathan Wilson
    #@c Switch to surface
    #@a widget:  render widget name

    set actors [$::vis::tkvars(CurrentRenderer) GetActors]

    $actors InitTraversal
    set actor [$actors GetNextItem]
    while { $actor != "" } {
        [$actor GetProperty] SetRepresentationToSurface
        set actor [$actors GetNextItem]
    }

    ::vis::tkRender $widget
}

# proc to change all the displayed texture map scalar
# ranges by deltamin deltamax
proc ::vis::tkIncrementColorMap {widget x y deltamin deltamax} {

  #@author Nathan Wilson
  #@c Incremental update all the color maps in the given widget
  #@a widget:  render widget name
  #@a x:  x location
  #@a y:  y location
  #@a deltamin:  change in minimum
  #@a deltamax:  change in the maximum

  if {$deltamin == 0 && $deltamax == 0} {
    set winx $::vis::tkvars(window_level_prev_X)
    set winy $::vis::tkvars(window_level_prev_Y)
    set ::vis::tkvars(window_level_prev_X) $x 
    set ::vis::tkvars(window_level_prev_Y) $y
    if {$winx < 0 || $winy < 0} {
        return
    }
    if {$winy == $y && $winx == $x} {
        return
    }
    # change min if maximal change is in y
    if {[expr abs($winy - $y)] > [expr abs($winx - $x)]} {
      if {$winy < $y} {
        set deltamin -1
        set deltamax 0 
      } elseif {$winy > $y} {
        set deltamin 1
        set deltamax 0 
      }
    } else {
      if {$winx < $x} {
        set deltamin 0
        set deltamax -1 
      } elseif {$winx > $x} {
        set deltamin 0
        set deltamax 1 
      }
    }
  }

  # loop over all of the currently displayed actors looking for
  # texture maps
  set rendercollection [[$widget GetRenderWindow] GetRenderers]
  $rendercollection InitTraversal
  for {set i 0} {$i < [$rendercollection GetNumberOfItems]} {incr i} {
    set ren [$rendercollection GetItemAsObject $i]
    set actors [$ren GetActors]
      for {set j 0} {$j < [$actors GetNumberOfItems]} {incr j} {
      set actor [$actors GetItemAsObject $j]
      set prop [$actor GetProperty]
      set texture [$actor GetTexture]
      if {$texture == ""} {
        # do nothing for non-texture maps right now
        #if {[[$actor GetMapper] GetLookupTable] != ""} {
        #  [$actor GetMapper] SetScalarRange foo bar
        #}
      } else {
        set lookupTable [$texture GetLookupTable]
        set scalarMin [lindex [$lookupTable GetTableRange] 0]
        set scalarMax [lindex [$lookupTable GetTableRange] 1]
     
        set win_incr $::vis::tkvars(window_level_increment)

	if {$win_incr > 0.99} {
          set incrScalarMin [expr int($deltamin*$win_incr)]
          set incrScalarMax [expr int($deltamax*$win_incr)]
          set scalarMin [expr $scalarMin + $incrScalarMin]
          set scalarMax [expr $scalarMax + $incrScalarMax]
	} else {
          set mapper [$actor GetMapper]
          set scalar_range [$mapper GetScalarRange]
          set range [expr [lindex $scalar_range 1] - [lindex $scalar_range 0]]
          set scalarMin [expr $scalarMin+$deltamin*$range*$win_incr]
          set scalarMax [expr $scalarMax+$deltamax*$range*$win_incr]
          #puts "scalar range: $scalarMin $scalarMax"
	}
	#if {$scalarMin < 0} {
        #   set scalarMin 0
	#}
        if {$scalarMax <= $scalarMin} {
           set scalarMax [expr $scalarMin + 1]
        }
        $lookupTable SetTableRange $scalarMin $scalarMax
        #puts "$scalarMin $scalarMax"
      }
    }
  }
  [$widget GetRenderWindow] Render
}


# proc to change all the displayed texture map
proc ::vis::tkResetColorMap {widget x y} {

  #@author Nathan Wilson
  #@c Reset the color map to the full range 
  #@a widget:  render widget name
  #@a x:  x location
  #@a y:  y location

  # loop over all of the currently displayed actors looking for
  # texture maps
  set rendercollection [[$widget GetRenderWindow] GetRenderers]
  $rendercollection InitTraversal
  for {set i 0} {$i < [$rendercollection GetNumberOfItems]} {incr i} {
    set ren [$rendercollection GetItemAsObject $i]
    set actors [$ren GetActors]
      for {set j 0} {$j < [$actors GetNumberOfItems]} {incr j} {
      set actor [$actors GetItemAsObject $j]
      set prop [$actor GetProperty]
      set texture [$actor GetTexture]
      if {$texture == ""} {
        # do nothing for non-texture maps right now
        #if {[[$actor GetMapper] GetLookupTable] != ""} {
        #  [$actor GetMapper] SetScalarRange foo bar
        #}
      } else {
        set lookupTable [$texture GetLookupTable]
        set mapper [$actor GetMapper]
        set scalar_range [$mapper GetScalarRange]
        set scalarMin [lindex $scalar_range 0]
        set scalarMax [lindex $scalar_range 1]
     
        if {$scalarMax <= $scalarMin} {
           set scalarMax [expr $scalarMin + 1]
        }
        $lookupTable SetTableRange $scalarMin $scalarMax
        #puts "$scalarMin $scalarMax"
      }
    }
  }
  [$widget GetRenderWindow] Render
}


proc ::vis::tkPickActor {widget x y} {

    #@author Nathan Wilson
    #@c Select the closest actor to position x,y
    #@a widget:  render widget name
    #@a x:  x location
    #@a y:  y location

    set ::vis::tkvars(WindowY) [lindex [$widget configure -height] 4]

    #puts "inside PickActor"

    if { ! $::vis::tkvars(RendererFound) } { return }

    set ::vis::tkvars(PickedRenderWindow) [$widget GetRenderWindow]

    $::vis::tkvars(ActorPicker) Pick $x [expr $::vis::tkvars(WindowY) - $y - 1] 0.0 $::vis::tkvars(CurrentRenderer)

    # first try to grab an actor, then a prop if no actor found
    set assembly [ $::vis::tkvars(ActorPicker) GetActor]
    if {$assembly == ""} {
      set assembly [ $::vis::tkvars(ActorPicker) GetViewProp]
    }
    set ::vis::tkvars(PickedAssemblyRenderer) $::vis::tkvars(CurrentRenderer)

    # if current picked assembly has been deleted somehow,
    # reset the pick.  That is, if the tcl command
    # (i.e. vtk object) doesn't exist, reset the pick.
    if {$::vis::tkvars(PickedAssembly) != "" && [info commands $::vis::tkvars(PickedAssembly)] == ""} {
      set ::vis::tkvars(PickedAssembly) ""
      if {$::vis::tkvars(PrePickedProperty) != ""} {
        # release hold on the property
        $::vis::tkvars(PrePickedProperty) UnRegister $::vis::tkvars(PrePickedProperty)
        set ::vis::tkvars(PrePickedProperty) ""
      }
      # make sure the selected color still exists
      if {[info commands $::vis::tkvars(PickedProperty)] == ""} {
        vtkProperty $::vis::tkvars(PickedProperty)
      }
      $::vis::tkvars(PickedProperty) SetColor 1 0 0
      $::vis::tkvars(PickedProperty) SetOpacity 1
      return
    }

    if { $::vis::tkvars(PickedAssembly) != "" && $::vis::tkvars(PrePickedProperty) != "" } {

        # need to check if object is a lod obj
        if {[$::vis::tkvars(PickedAssembly) GetClassName] == "vtkLODProp3D"} {
          set first [string first act $::vis::tkvars(PickedAssembly)]
          set last [expr $first + 2]
          set property [string replace $::vis::tkvars(PickedAssembly) $first $last prop]
          # need to reset property for all actors in lod obj
	  set propCol [tmpobj]
          vtkPropCollection $propCol
          $::vis::tkvars(PickedAssembly) GetActors $propCol

          for {set i 0} {$i < [$propCol GetNumberOfItems]} {incr i} {
             set actor [$propCol GetItemAsObject $i]
             $actor SetProperty $::vis::tkvars(PrePickedProperty)
	  }
          $propCol Delete

	} else {

          $::vis::tkvars(PickedAssembly) SetProperty $::vis::tkvars(PrePickedProperty)

	}

        # release hold on the property
        $::vis::tkvars(PrePickedProperty) UnRegister $::vis::tkvars(PrePickedProperty)
        set ::vis::tkvars(PrePickedProperty) ""
        set ::vis::tkvars(PickedAssembly) ""
    }

    # make sure the selected color still exists
    if {[info commands $::vis::tkvars(PickedProperty)] == ""} {
      vtkProperty $::vis::tkvars(PickedProperty)
    }
    $::vis::tkvars(PickedProperty) SetColor 1 0 0
    $::vis::tkvars(PickedProperty) SetOpacity 1

    if { $assembly != "" } {

        set ::vis::tkvars(PickedAssembly) $assembly

        if {[$::vis::tkvars(PickedAssembly) GetClassName] == "vtkLODProp3D"} {
          set first [string first act $::vis::tkvars(PickedAssembly)]
          set last [expr $first + 2]
          set property [string replace $::vis::tkvars(PickedAssembly) $first $last prop]

          set ::vis::tkvars(PrePickedProperty) $property
          # hold onto the property
          $::vis::tkvars(PrePickedProperty) Register $::vis::tkvars(PrePickedProperty)

          # need to reset property for all actors in lod obj
	  set propCol [tmpobj]
          vtkPropCollection $propCol
          $::vis::tkvars(PickedAssembly) GetActors $propCol

          for {set i 0} {$i < [$propCol GetNumberOfItems]} {incr i} {
             set actor [$propCol GetItemAsObject $i]
             $actor SetProperty $::vis::tkvars(PickedProperty)
	  }
          $propCol Delete

	} else {

          set ::vis::tkvars(PrePickedProperty) [$::vis::tkvars(PickedAssembly) GetProperty]
          # hold onto the property
          $::vis::tkvars(PrePickedProperty) Register $::vis::tkvars(PrePickedProperty)
          $::vis::tkvars(PickedAssembly) SetProperty $::vis::tkvars(PickedProperty)

	}

    }

    ::vis::tkRender $widget

}


proc ::vis::tkDeselectPickedActor {} {

  #@author Nathan Wilson
  #@c Deselect the currently selected actor

  # set the actor here
  if {$::vis::tkvars(PickedAssembly) == ""} {
    return
  }

  # if current picked assembly has been deleted somehow,
  # reset the pick.  That is, if the tcl command
  # (i.e. vtk object) doesn't exist, reset the pick.
  if {[info commands $::vis::tkvars(PickedAssembly)] == ""} {
    set ::vis::tkvars(PickedAssembly) ""
    if {$::vis::tkvars(PrePickedProperty) != ""} {
      # release hold on the property
      $::vis::tkvars(PrePickedProperty) UnRegister $::vis::tkvars(PrePickedProperty)
      set ::vis::tkvars(PrePickedProperty) ""
    }
    # make sure the selected color still exists
    if {[info commands $::vis::tkvars(PickedProperty)] == ""} {
      vtkProperty $::vis::tkvars(PickedProperty)
    }
    $::vis::tkvars(PickedProperty) SetColor 1 0 0
    $::vis::tkvars(PickedProperty) SetOpacity 1
    return
  }

  if { $::vis::tkvars(PickedAssembly) != "" && $::vis::tkvars(PrePickedProperty) != "" } {

        # need to check if object is a lod obj
        if {[$::vis::tkvars(PickedAssembly) GetClassName] == "vtkLODProp3D"} {
          set first [string first act $::vis::tkvars(PickedAssembly)]
          set last [expr $first + 2]
          set property [string replace $::vis::tkvars(PickedAssembly) $first $last prop]
          # need to reset property for all actors in lod obj
          set propCol PickActorPropCollection
	  catch {$propCol Delete}
          vtkPropCollection $propCol
          $::vis::tkvars(PickedAssembly) GetActors $propCol

          for {set i 0} {$i < [$propCol GetNumberOfItems]} {incr i} {
             set actor [$propCol GetItemAsObject $i]
             $actor SetProperty $::vis::tkvars(PrePickedProperty)
	  }

	} else {

          $::vis::tkvars(PickedAssembly) SetProperty $::vis::tkvars(PrePickedProperty)

	}

        # release hold on the property
        $::vis::tkvars(PrePickedProperty) UnRegister $::vis::tkvars(PrePickedProperty)
        set ::vis::tkvars(PrePickedProperty) ""
  }

}


proc ::vis::tkEnter {W x y} {
    #@author Nathan Wilson
    #@c Enter event in widget
    #@a W:  render widget name
    #@a x:  x location
    #@a y:  y location
  ::vis::tkGeneric EnterEvent $W $x $y
  ::vis::tkEnterEnter $W $x $y
}

proc ::vis::tkLeave {W x y} {
    #@author Nathan Wilson
    #@c Leave event in widget
    #@a W:  render widget name
    #@a x:  x location
    #@a y:  y location
  ::vis::tkGeneric LeaveEvent $W $x $y
  focus $::vis::tkvars(oldFocus)
}

proc ::vis::tkGeneric {event W x y} {
    #@author Nathan Wilson
    #@c Call generic vtk interactor
    #@a event:  interactor event
    #@a W:  render widget name
    #@a x:  x location
    #@a y:  y location
  set ctrl 0
  set shift 0
  set keycode 0
  set repeatcount 0
  set keysym 0
  # y is flipped upside down
  set yorg $y
  set height [lindex [$W configure -height] 4]
  set y [expr $height - $y]

  #puts "interactorGeneric: $W $x $yorg $y [$W configure]"
  set ren [$W GetRenderWindow]_ren1
  set ::vis::tkvars(InteractorRen) $ren
  genericInteractor_$ren SetEventInformation $x $y $ctrl \
                         $shift $keycode $repeatcount $keysym
  genericInteractor_$ren $event
}

proc ::vis::tkRotate {W x y {shouldRender 1}} {
    #@author Nathan Wilson
    #@c Rotate view inside of renderer
    #@a W:  render widget name
    #@a x:  x location
    #@a y:  y location
    #@a shouldRender: should update display after rotate (default 1)
  if {$::vis::tkvars(B1PRESSED) == 0} {
    set ::vis::tkvars(B1PRESSED) 1
    ::vis::tkGeneric LeftButtonPressEvent $W $x $y
  } else {
    ::vis::tkGeneric MouseMoveEvent $W $x $y
  }
  if {$::vis::tkvars(InteractorInteracting) == 0} {
    ::vis::tkRotateRotate $W $x $y $shouldRender
  }
}

proc ::vis::tkSpecialRotate {W x y} {
    #@author Nathan Wilson
    #@c Special rotate inside of renderer
    #@a W:  render widget name
    #@a x:  x location
    #@a y:  y location
    ::vis::tkRotate $W [expr ($::vis::tkvars(LastX)+90)] $::vis::tkvars(LastY) 0
    ::vis::tkRotate $W $::vis::tkvars(LastX) $y 0
    ::vis::tkRotate $W [expr ($::vis::tkvars(LastX)-90)] $::vis::tkvars(LastY)
}

proc ::vis::tkB1release {W x y} {
    #@author Nathan Wilson
    #@c release of button 1 on mouse
    #@a W:  render widget name
    #@a x:  x location
    #@a y:  y location
  ::vis::tkGeneric LeftButtonReleaseEvent $W $x $y
  set ::vis::tkvars(B1PRESSED) 0
}

#
#  globals must be created at the end so tkautodoc can still work
#

set ::vis::tkvars(ActorPicker) [tmpobj]
set ::vis::tkvars(PickedProperty) [tmpobj]

vtkPropPicker $::vis::tkvars(ActorPicker)

vtkProperty $::vis::tkvars(PickedProperty)
    $::vis::tkvars(PickedProperty) SetColor 1 0 0


set ::vis::tkvars(InteractiveUpdateRate) 15.0
set ::vis::tkvars(StillUpdateRate) 0.0
# Used to support picking operations
set ::vis::tkvars(PickedAssembly) ""
set ::vis::tkvars(PickedRenderWindow) ""
set ::vis::tkvars(PickedAssemblyRenderer) ""
set ::vis::tkvars(PrePickedProperty) ""
# Global variable keeps track of whether active renderer was found
set ::vis::tkvars(RendererFound) 0
set ::vis::tkvars(B1PRESSED) 0
set ::vis::tkvars(B2PRESSED) 0
set ::vis::tkvars(B3PRESSED) 0
set ::vis::tkvars(InteractorInteracting) 0
set ::vis::tkvars(InteractorRen) {}
set ::vis::tkvars(PLUSMINUS) 0
set ::vis::tkvars(window_level_increment) 20
set ::vis::tkvars(window_level_prev_X) -1
set ::vis::tkvars(window_level_prev_Y) -1
set ::vis::tkvars(sample) ""
set ::vis::tkvars(sample_ren) ""