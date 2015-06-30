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


# generate a "unique" name for a widget variable
proc GetWidgetVariable {widget varName} {
   regsub -all {\.} $widget "_" base

   return "$varName$base"
}


# sets the value of a widget variable
proc SetWidgetVariableValue {widget varName value} {
   set var [GetWidgetVariable $widget $varName]
   global $var
   set $var $value
}

# This proc has alway eluded me.
proc GetWidgetVariableValue {widget varName} {
   set var [GetWidgetVariable $widget $varName]
   global $var
   set temp ""
   catch {eval "set temp [format {$%s} $var]"}

   return $temp
}


## Procedure should be called to set bindings and initialize variables
#

set TkInteractor_StartRenderMethod ""
set TkInteractor_EndRenderMethod ""
set TkInteractor_InteractiveUpdateRate 15.0
set TkInteractor_StillUpdateRate 0.0

proc BindTkRenderWidget {widget} {
    global tcl_platform
    bind .guiCV <Key-Return> {ChangeWaitVar}
    bind $widget <Any-ButtonPress> {StartMotion %W %x %y}
    bind $widget <Any-ButtonRelease> {EndMotion %W %x %y}
    bind $widget <B1-Motion> {vis_interactorRotate %W %x %y}
    bind $widget <B1-ButtonRelease> {vis_interactorB1release %W %x %y}
    if {$tcl_platform(os) == "Darwin"} {
      bind $widget <B3-Motion> {Pan %W %x %y}
      bind $widget <B2-Motion> {Zoom %W %x %y}
    } else {
      bind $widget <B2-Motion> {Pan %W %x %y}
      bind $widget <B3-Motion> {Zoom %W %x %y}
    }
    bind $widget <Shift-B1-Motion> {Pan %W %x %y}
    bind $widget <Control-B1-Motion> {vis_interactorSpecialRotate %W %x %y}

#    bind $widget <Shift-B3-Motion> {RubberZoom %W %x %y}
    bind $widget <KeyPress> {vis_interactorEnter %W %x %y}
    bind $widget <KeyPress-i> {vis_interactorToggle %W %x %y}
    bind $widget <KeyPress-r> {Reset %W %x %y}
    bind $widget <KeyPress-u> {wm deiconify .vtkInteract}
    bind $widget <KeyPress-w> {Wireframe %W}
    bind $widget <KeyPress-s> {Surface %W}
    bind $widget <KeyPress-e> {SurfaceWithEdges %W}
    bind $widget <KeyPress-p> {PickActor %W %x %y}
    bind $widget <KeyPress-t> {PickPointNoCrosshairs %W %x %y}
    bind $widget <KeyPress-g> {PickSeedNoCrosshairs %W %x %y}
    bind $widget <KeyPress-c> {PickPolyDataCell %W %x %y 0 0}
    bind $widget <KeyPress-1> {CameraRoll %W %x %y 1}
    bind $widget <KeyPress-2> {CameraRoll %W %x %y -1}
    bind $widget <KeyPress-3> {CameraRoll %W %x %y 10}
    bind $widget <KeyPress-4> {CameraRoll %W %x %y -10}
    bind $widget <KeyPress-k> {PrintCameraView %W %x %y}
    bind $widget <KeyPress-l> {RenderCameraView %W %x %y}
    bind $widget <KeyPress-C> {PickPolyDataCell %W %x %y 1 0}
    bind $widget <KeyPress-h> {PickPolyDataPoint %W %x %y 0 0}
    bind $widget <Control-KeyPress-h> {PickPolyDataPoint %W %x %y 1 0}
# hack that assumes you've used code similar to vis_initTKgr to create
# window (hack no longer seems to work with paned windows)
    #bind .[lindex [split $widget .] 1] <FocusIn> "focus $widget"
    #bind $widget <Enter> {vis_interactorEnter %W %x %y}
    #bind $widget <Enter> {Enter %W %x %y}
    #bind $widget <Leave> {Leave $oldFocus}
    bind $widget <Expose> {Expose %W}
    bind $widget <Key-Left>  {IncrementColorMap %W %x %y -1  0}
    bind $widget <Key-Right> {IncrementColorMap %W %x %y  1  0}
    bind $widget <Key-Up>    {IncrementColorMap %W %x %y  0  1}
    bind $widget <Key-Down>  {IncrementColorMap %W %x %y  0 -1}
    bind $widget <Key-plus>  {set gPlusMinus 1}
    bind $widget <Key-minus> {set gPlusMinus -1}
}


# a litle more complex than just "bind $widget <Expose> {%W Render}"
# we have to handle all pending expose events otherwise they que up.
proc Expose {widget} {
    global TkInteractor_StillUpdateRate
    if {[GetWidgetVariableValue $widget InExpose] == 1} {
	return
    }
    SetWidgetVariableValue $widget InExpose 1    
    [$widget GetRenderWindow] SetDesiredUpdateRate $TkInteractor_StillUpdateRate
    update
    [$widget GetRenderWindow] Render
    SetWidgetVariableValue $widget InExpose 0
}

# Global variable keeps track of whether active renderer was found
set RendererFound 0

# Create event bindings
#
proc Render {widget} {
    global CurrentCamera CurrentLight
    global TkInteractor_StartRenderMethod
    global TkInteractor_EndRenderMethod

    if { $TkInteractor_StartRenderMethod != "" } {
	$TkInteractor_StartRenderMethod
    }

    eval $CurrentLight SetPosition [$CurrentCamera GetPosition]
    eval $CurrentLight SetFocalPoint [$CurrentCamera GetFocalPoint]

    $widget Render

    if { $TkInteractor_EndRenderMethod != "" } {
	$TkInteractor_EndRenderMethod
    }
}

proc UpdateRenderer {widget x y} {
    global CurrentCamera CurrentLight 
    global CurrentRenderWindow CurrentRenderer
    global RendererFound LastX LastY
    global WindowCenterX WindowCenterY

    # Get the renderer window dimensions
    set WindowX [lindex [$widget configure -width] 4]
    set WindowY [lindex [$widget configure -height] 4]

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    # Find which renderer event has occurred in
    set CurrentRenderWindow [$widget GetRenderWindow]
    set renderers [$CurrentRenderWindow GetRenderers]
    set numRenderers [$renderers GetNumberOfItems]

    $renderers InitTraversal; set RendererFound 0
    for {set i 0} {$i < $numRenderers} {incr i} {
        set CurrentRenderer [$renderers GetNextItem]
        set vx [expr double($x) / $WindowX]
        set vy [expr ($WindowY - double($y)) / $WindowY]
        set viewport [$CurrentRenderer GetViewport]
        set vpxmin [lindex $viewport 0]
        set vpymin [lindex $viewport 1]
        set vpxmax [lindex $viewport 2]
        set vpymax [lindex $viewport 3]
        if { $vx >= $vpxmin && $vx <= $vpxmax && \
        $vy >= $vpymin && $vy <= $vpymax} {
            set RendererFound 1
            set WindowCenterX [expr double($WindowX)*(($vpxmax - $vpxmin)/2.0\
                                + $vpxmin)]
            set WindowCenterY [expr double($WindowY)*(($vpymax - $vpymin)/2.0\
                                + $vpymin)]
            break
        }
    }
    
    set CurrentCamera [$CurrentRenderer GetActiveCamera]
    set lights [$CurrentRenderer GetLights]
    $lights InitTraversal; set CurrentLight [$lights GetNextItem]
   
    set LastX $x
    set LastY $y
}

proc ChangeWaitVar {} {
  global gWaitVar
  set gWaitVar 1
}

proc Enter {widget x y} {
    global oldFocus

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    set oldFocus [focus]
    focus $widget
    UpdateRenderer $widget $x $y
}

proc StartMotion {widget x y} {
    global CurrentCamera CurrentLight 
    global CurrentRenderWindow CurrentRenderer
    global LastX LastY
    global RendererFound
    global TkInteractor_InteractiveUpdateRate
    global RubberZoomPerformed

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    if {[focus] != $widget} {
       #puts "oldfocus: [focus]  widget: $widget"
       focus $widget
    }
    UpdateRenderer $widget $x $y
    if { ! $RendererFound } { return }

    set RubberZoomPerformed 0

    $CurrentRenderWindow SetDesiredUpdateRate $TkInteractor_InteractiveUpdateRate
}

proc EndMotion {widget x y} {
    global CurrentRenderWindow
    global RendererFound
    global TkInteractor_StillUpdateRate
    global RubberZoomPerformed
    global CurrentRenderer

    if { ! $RendererFound } {return}
    $CurrentRenderWindow SetDesiredUpdateRate $TkInteractor_StillUpdateRate


    if { $RubberZoomPerformed } {
	$CurrentRenderer RemoveViewProp RubberBandActor
	DoRubberZoom $widget
    }

    Render $widget
}

proc Rotate {widget x y {shouldRender 1}} {
    global CurrentCamera 
    global LastX LastY
    global RendererFound

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    if { ! $RendererFound } { return }

    $CurrentCamera Azimuth [expr ($LastX - $x)]
    $CurrentCamera Elevation [expr ($y - $LastY)]
    $CurrentCamera OrthogonalizeViewUp

    set LastX $x
    set LastY $y

    if $shouldRender {
	Render $widget
    }

}


proc CameraRoll {widget x y {val 1}} {

global CurrentRenderer CurrentCamera
    global WindowCenterX WindowCenterY LastX LastY
    global RendererFound
    if { ! $RendererFound } { return }

    set roll [$CurrentCamera GetRoll]
    set newRoll [expr {$roll + $val}]
    # puts " roll: [$CurrentCamera GetRoll]"
    $CurrentCamera SetRoll $newRoll
    Render $widget
}


proc PrintCameraView {widget x y} {
    global CurrentRenderer CurrentCamera
    puts "Camera Location Saved!"
    set outstr "widget {$widget} FocalPoint {[$CurrentCamera GetFocalPoint]} Position {[$CurrentCamera GetPosition]} ViewUp {[$CurrentCamera GetViewUp]}"
    puts "\"$outstr\""
    global gSavedView
    array set gSavedView $outstr

}
proc SetRenderCameraView {viewstr} {
    global gSavedView
    array set gSavedView $viewstr
}
proc RenderCameraView {widget x y} {
    global gSavedView
    global CurrentRenderer CurrentCamera
    $CurrentCamera SetPosition [lindex $gSavedView(Position) 0] [lindex $gSavedView(Position) 1] [lindex $gSavedView(Position) 2]
    $CurrentCamera SetFocalPoint [lindex $gSavedView(FocalPoint) 0] [lindex $gSavedView(FocalPoint) 1] [lindex $gSavedView(FocalPoint) 2] 
    $CurrentCamera SetViewUp [lindex $gSavedView(ViewUp) 0] [lindex $gSavedView(ViewUp) 1] [lindex $gSavedView(ViewUp) 2]

    Render $widget

}


proc Pan {widget x y} {
    global CurrentRenderer CurrentCamera
    global WindowCenterX WindowCenterY LastX LastY
    global RendererFound

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    if { ! $RendererFound } { return }

    set FPoint [$CurrentCamera GetFocalPoint]
        set FPoint0 [lindex $FPoint 0]
        set FPoint1 [lindex $FPoint 1]
        set FPoint2 [lindex $FPoint 2]

    set PPoint [$CurrentCamera GetPosition]
        set PPoint0 [lindex $PPoint 0]
        set PPoint1 [lindex $PPoint 1]
        set PPoint2 [lindex $PPoint 2]

    $CurrentRenderer SetWorldPoint $FPoint0 $FPoint1 $FPoint2 1.0
    $CurrentRenderer WorldToDisplay
    set DPoint [$CurrentRenderer GetDisplayPoint]
    set focalDepth [lindex $DPoint 2]

    set APoint0 [expr $WindowCenterX + ($x - $LastX)]
    set APoint1 [expr $WindowCenterY - ($y - $LastY)]

    $CurrentRenderer SetDisplayPoint $APoint0 $APoint1 $focalDepth
    $CurrentRenderer DisplayToWorld
    set RPoint [$CurrentRenderer GetWorldPoint]
        set RPoint0 [lindex $RPoint 0]
        set RPoint1 [lindex $RPoint 1]
        set RPoint2 [lindex $RPoint 2]
        set RPoint3 [lindex $RPoint 3]
    if { $RPoint3 != 0.0 } {
        set RPoint0 [expr $RPoint0 / $RPoint3]
        set RPoint1 [expr $RPoint1 / $RPoint3]
        set RPoint2 [expr $RPoint2 / $RPoint3]
    }

    $CurrentCamera SetFocalPoint \
      [expr ($FPoint0 - $RPoint0)/2.0 + $FPoint0] \
      [expr ($FPoint1 - $RPoint1)/2.0 + $FPoint1] \
      [expr ($FPoint2 - $RPoint2)/2.0 + $FPoint2]

    $CurrentCamera SetPosition \
      [expr ($FPoint0 - $RPoint0)/2.0 + $PPoint0] \
      [expr ($FPoint1 - $RPoint1)/2.0 + $PPoint1] \
      [expr ($FPoint2 - $RPoint2)/2.0 + $PPoint2]

    set LastX $x
    set LastY $y

    Render $widget
}

proc Zoom {widget x y} {
    global CurrentCamera CurrentRenderer
    global LastX LastY
    global RendererFound

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    if { ! $RendererFound } { return }

    set zoomFactor [expr pow(1.02,(0.5*($y - $LastY)))]

    if {[$CurrentCamera GetParallelProjection]} {
	set parallelScale [expr [$CurrentCamera GetParallelScale] * $zoomFactor];
	$CurrentCamera SetParallelScale $parallelScale;
    } else {
	$CurrentCamera Dolly $zoomFactor
	$CurrentRenderer ResetCameraClippingRange
    }

    set LastX $x
    set LastY $y

    Render $widget
}

proc Reset {widget x y} {
    global CurrentRenderWindow
    global RendererFound
    global CurrentRenderer

    # Get the renderer window dimensions
    set WindowX [lindex [$widget configure -width] 4]
    set WindowY [lindex [$widget configure -height] 4]

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    # Find which renderer event has occurred in
    set CurrentRenderWindow [$widget GetRenderWindow]
    set renderers [$CurrentRenderWindow GetRenderers]
    set numRenderers [$renderers GetNumberOfItems]

    $renderers InitTraversal; set RendererFound 0
    for {set i 0} {$i < $numRenderers} {incr i} {
        set CurrentRenderer [$renderers GetNextItem]
        set vx [expr double($x) / $WindowX]
        set vy [expr ($WindowY - double($y)) / $WindowY]

        set viewport [$CurrentRenderer GetViewport]
        set vpxmin [lindex $viewport 0]
        set vpymin [lindex $viewport 1]
        set vpxmax [lindex $viewport 2]
        set vpymax [lindex $viewport 3]
        if { $vx >= $vpxmin && $vx <= $vpxmax && \
        $vy >= $vpymin && $vy <= $vpymax} {
            set RendererFound 1
            break
        }
    }

    if { $RendererFound } {$CurrentRenderer ResetCamera}

    Render $widget
}

proc Wireframe {widget} {
    global CurrentRenderer

    set actors [$CurrentRenderer GetActors]

    $actors InitTraversal
    set actor [$actors GetNextItem]
    while { $actor != "" } {
        [$actor GetProperty] SetRepresentationToWireframe
        set actor [$actors GetNextItem]
    }

    Render $widget
}

proc Surface {widget} {
    global CurrentRenderer

    set actors [$CurrentRenderer GetActors]

    $actors InitTraversal
    set actor [$actors GetNextItem]
    while { $actor != "" } {
        [$actor GetProperty] EdgeVisibilityOff
        [$actor GetProperty] SetRepresentationToSurface
        set actor [$actors GetNextItem]
    }

    Render $widget
}

proc SurfaceWithEdges {widget} {
    global CurrentRenderer

    set actors [$CurrentRenderer GetActors]

    $actors InitTraversal
    set actor [$actors GetNextItem]
    while { $actor != "" } {
        [$actor GetProperty] SetRepresentationToSurface
        [$actor GetProperty] EdgeVisibilityOn
        set actor [$actors GetNextItem]
    }

    Render $widget
}

# proc to change all the displayed texture map scalar
# ranges by deltamin deltamax
proc IncrementColorMap {widget x y deltamin deltamax} {
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
        global gOptions
        set win_incr $gOptions(window_level_increment)
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


# Used to support picking operations
#
set PickedAssembly ""
vtkCellPicker CellPicker
vtkPointPicker PolyDataPointPicker
vtkPropPicker ActorPicker

vtkProperty PickedProperty
    PickedProperty SetColor 1 1 0
set PrePickedProperty ""

vtkPropPicker PointPicker
proc PickPointNoCrosshairs {widget x y} {
    global guiPPchooserExists    

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    if { [info exists guiPPchooserExists] } {

      global CurrentRenderer RendererFound

      set WindowY [lindex [$widget configure -height] 4]
      PointPicker Pick $x [expr $WindowY - $y - 1] 0.0 $CurrentRenderer
      guiPPchooserAddSpecifiedPoint [PointPicker GetPickPosition]
    }
}

proc PickSeedNoCrosshairs {widget x y} {
    global guiPICKSEEDExists

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    if { [info exists guiPICKSEEDExists] } {
      global CurrentRenderer RendererFound
      set WindowY [lindex [$widget configure -height] 4]
      PointPicker Pick $x [expr $WindowY - $y - 1] 0.0 $CurrentRenderer
      guiPICKSEEDAddSpecifiedPoint [PointPicker GetPickPosition]
    }
}

proc PickActor {widget x y} {
    global CurrentRenderer RendererFound
    global PickedAssembly PrePickedProperty WindowY
    global PickedAssemblyRenderer
    global gActorPicked

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    set WindowY [lindex [$widget configure -height] 4]

    if { ! $RendererFound } { return }
    ActorPicker Pick $x [expr $WindowY - $y - 1] 0.0 $CurrentRenderer
    
    #set assembly [ActorPicker GetAssembly]
    #puts "assembly: $assembly"
    #set pickedActors [ActorPicker GetActors]
    #$pickedActors InitTraversal
    #set pickedActor [$pickedActors GetNextActor]
    #puts "pickedActor: $pickedActor"
    #set assembly $pickedActor

    # first try to grab an actor, then a prop if no actor found
    set assembly [ActorPicker GetActor]
    if {$assembly == ""} {
      set assembly [ActorPicker GetViewProp]
    }
    set PickedAssemblyRenderer $CurrentRenderer
    #puts "assembly: $assembly"

    # if current picked assembly has been deleted somehow,
    # reset the pick.  That is, if the tcl command
    # (i.e. vtk object) doesn't exist, reset the pick.
    if {$PickedAssembly != "" && [info commands $PickedAssembly] == ""} {
      set gActorPicked 1
      set PickedAssembly ""
      if {$PrePickedProperty != ""} {
        # release hold on the property
        $PrePickedProperty UnRegister $PrePickedProperty
        set PrePickedProperty ""
      }
      # make sure the selected color still exists
      if {[info commands PickedProperty] == ""} {
        vtkProperty PickedProperty
      }
      PickedProperty SetColor 1 1 0
      PickedProperty SetOpacity 1
      return
    }

    if { $PickedAssembly != "" && $PrePickedProperty != "" } {
        set gActorPicked 1
        # need to check if object is a lod obj
        if {[$PickedAssembly GetClassName] == "vtkLODProp3D"} {
          set first [string first act $PickedAssembly]
          set last [expr $first + 2]
          set property [string replace $PickedAssembly $first $last prop]
          # need to reset property for all actors in lod obj
          set propCol PickActorPropCollection
	  catch {$propCol Delete}
          vtkPropCollection $propCol
          $PickedAssembly GetActors $propCol

          for {set i 0} {$i < [$propCol GetNumberOfItems]} {incr i} {
             set actor [$propCol GetItemAsObject $i]
             $actor SetProperty $PrePickedProperty
	  }

	} else {

          $PickedAssembly SetProperty $PrePickedProperty

	}

        # release hold on the property
        $PrePickedProperty UnRegister $PrePickedProperty
        set PrePickedProperty ""
    }

    # make sure the selected color still exists
    if {[info commands PickedProperty] == ""} {
      vtkProperty PickedProperty
    }
    PickedProperty SetColor 1 1 0
    PickedProperty SetOpacity 1

    if { $assembly != "" } {

        set PickedAssembly $assembly
        set gActorPicked 1

        if {[$PickedAssembly GetClassName] == "vtkLODProp3D"} {
          set first [string first act $PickedAssembly]
          set last [expr $first + 2]
          set property [string replace $PickedAssembly $first $last prop]

          set PrePickedProperty $property
          # hold onto the property
          $PrePickedProperty Register $PrePickedProperty

          # need to reset property for all actors in lod obj
          set propCol PickActorPropCollection
	  catch {$propCol Delete}
          vtkPropCollection $propCol
          $PickedAssembly GetActors $propCol

          for {set i 0} {$i < [$propCol GetNumberOfItems]} {incr i} {
             set actor [$propCol GetItemAsObject $i]
             $actor SetProperty PickedProperty
	  }

	} else {

          global guiRWH
          set guiRWH(actor) $PickedAssembly
          set guiRWH(render) $CurrentRenderer
          guiLaunchRenWinHelper

          set PrePickedProperty [$PickedAssembly GetProperty]
          # hold onto the property
          $PrePickedProperty Register $PrePickedProperty
          $PickedAssembly SetProperty PickedProperty

	}

      guiSV_model_update_actor_selection $PickedAssembly
    }

    Render $widget

}

proc VirtualPickActor {actor} {
    global CurrentRenderer RendererFound
    global PickedAssembly PrePickedProperty WindowY
    global PickedAssemblyRenderer
    global gActorPicked

    # first try to grab an actor, then a prop if no actor found
    set assembly $actor
    if {$assembly == ""} {
      set assembly [ActorPicker GetViewProp]
    }
    set PickedAssemblyRenderer $CurrentRenderer
    #puts "assembly: $assembly"

    # if current picked assembly has been deleted somehow,
    # reset the pick.  That is, if the tcl command
    # (i.e. vtk object) doesn't exist, reset the pick.
    if {$PickedAssembly != "" && [info commands $PickedAssembly] == ""} {
      set gActorPicked 1
      set PickedAssembly ""
      if {$PrePickedProperty != ""} {
        # release hold on the property
        $PrePickedProperty UnRegister $PrePickedProperty
        set PrePickedProperty ""
      }
      # make sure the selected color still exists
      if {[info commands PickedProperty] == ""} {
        vtkProperty PickedProperty
      }
      PickedProperty SetColor 1 1 0
      PickedProperty SetOpacity 1
      return
    }

    if { $PickedAssembly != "" && $PrePickedProperty != "" } {
        set gActorPicked 1
        # need to check if object is a lod obj
        if {[$PickedAssembly GetClassName] == "vtkLODProp3D"} {
          set first [string first act $PickedAssembly]
          set last [expr $first + 2]
          set property [string replace $PickedAssembly $first $last prop]
          # need to reset property for all actors in lod obj
          set propCol PickActorPropCollection
	  catch {$propCol Delete}
          vtkPropCollection $propCol
          $PickedAssembly GetActors $propCol

          for {set i 0} {$i < [$propCol GetNumberOfItems]} {incr i} {
             set actor [$propCol GetItemAsObject $i]
             $actor SetProperty $PrePickedProperty
	  }

	} else {

          $PickedAssembly SetProperty $PrePickedProperty

	}

        # release hold on the property
        $PrePickedProperty UnRegister $PrePickedProperty
        set PrePickedProperty ""
    }

    # make sure the selected color still exists
    if {[info commands PickedProperty] == ""} {
      vtkProperty PickedProperty
    }
    PickedProperty SetColor 1 1 0
    PickedProperty SetOpacity 1

    if { $assembly != "" } {

        set PickedAssembly $assembly
        set gActorPicked 1

        if {[$PickedAssembly GetClassName] == "vtkLODProp3D"} {
          set first [string first act $PickedAssembly]
          set last [expr $first + 2]
          set property [string replace $PickedAssembly $first $last prop]

          set PrePickedProperty $property
          # hold onto the property
          $PrePickedProperty Register $PrePickedProperty

          # need to reset property for all actors in lod obj
          set propCol PickActorPropCollection
	  catch {$propCol Delete}
          vtkPropCollection $propCol
          $PickedAssembly GetActors $propCol

          for {set i 0} {$i < [$propCol GetNumberOfItems]} {incr i} {
             set actor [$propCol GetItemAsObject $i]
             $actor SetProperty PickedProperty
	  }

	} else {

          global guiRWH
          set guiRWH(actor) $PickedAssembly
          set guiRWH(render) $CurrentRenderer
          guiLaunchRenWinHelper

          set PrePickedProperty [$PickedAssembly GetProperty]
          # hold onto the property
          $PrePickedProperty Register $PrePickedProperty
          $PickedAssembly SetProperty PickedProperty

	}

    }
}

proc PickPolyDataCell {widget x y add delete} {
    global CurrentRenderer RendererFound
    global PickedAssembly PrePickedProperty WindowY
    global PickedAssemblyRenderer
    global gActorPicked
    global gNumPickedCells
    global gPickedCellIds

    if {$delete == 1} {
      for {set i 1} {$i <= $gNumPickedCells} {incr i} {
	catch {$CurrentRenderer RemoveActor newActor_$gPickedCellIds($i)}
	catch {newActor_$gPickedCellIds($i) Delete}
      }
      array unset gPickedCellIds *
      set gNumPickedCells 0
    } else {

      set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
      set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

      set WindowY [lindex [$widget configure -height] 4]

      if {! $RendererFound } {return}
      CellPicker Pick $x [expr $WindowY - $y - 1] 0.0 $CurrentRenderer

#      set worldPosition [CellPicker GetPickPosition]
      set cellId [CellPicker GetCellId]
       
      set deletedActor 0
      if {$add == 0} {
	for {set i 1} {$i <= $gNumPickedCells} {incr i} {
	  catch {$CurrentRenderer RemoveActor newActor_$gPickedCellIds($i)}
	  catch {newActor_$gPickedCellIds($i) Delete}
	}
	array unset gPickedCellIds *
	set gNumPickedCells 0
      } elseif {$add == 1} {
        set count 0
	for {set i 1} {$i <= $gNumPickedCells} {incr i} {
	  if {$cellId == $gPickedCellIds($i)} {
	    #catch {$CurrentRenderer RemoveActor newActor_$cellId}
	    #catch {newActor_$cellId Delete}
	    #set deletedActor 1
	    return
	  } else {
	    incr count
	    set tempCellIds($count) $gPickedCellIds($i)
	  }
	}
	array unset gPickedCellIds *
	set gNumPickedCells $count
	for {set i 1} {$i <= $count} {incr i} {
	  set gPickedCellIds($i) $tempCellIds($i)
	}
      }	

      if {$cellId !=  -1 && $deletedActor == 0} {

	incr gNumPickedCells
	set gPickedCellIds($gNumPickedCells) $cellId 

  #      puts "Position is: "
  #      puts "[lindex $worldPosition 0],"
  #      puts "[lindex $worldPosition 1],"
  #      puts "[lindex $worldPosition 2],"

	catch {ids Delete}
	catch {selectionNode Delete}
	catch {extractSelection Delete}
	catch {newSelection Delete}
	catch {selected Delete}
	catch {newMapper Delete}

	vtkIdTypeArray ids
	ids SetNumberOfComponents 1
	ids InsertNextValue $cellId

	vtkSelectionNode selectionNode
	#Field Type 0 is CELL
	selectionNode SetFieldType 0
	#Content Type 4 is INDICES
	selectionNode SetContentType 4
	selectionNode SetSelectionList ids
	
	vtkSelection newSelection
	newSelection AddNode selectionNode

	vtkExtractSelection extractSelection
	extractSelection SetInputData 0 [CellPicker GetDataSet]
	extractSelection SetInputData 1 newSelection
	extractSelection Update

	vtkUnstructuredGrid selected
	selected ShallowCopy [extractSelection GetOutput]

#	puts "Number of Cells: [selected GetNumberOfCells]"
#	puts "Number of Points: [selected GetNumberOfPoints]"

	vtkDataSetMapper newMapper
	newMapper SetInputData selected

	vtkActor newActor_$cellId
	newActor_$cellId SetMapper newMapper
	[newActor_$cellId GetProperty] SetEdgeVisibility 1
	[newActor_$cellId GetProperty] SetColor 0 0 1
	[newActor_$cellId GetProperty] SetEdgeColor 0 1 0
	[newActor_$cellId GetProperty] SetLineWidth 3

	 $CurrentRenderer AddActor newActor_$cellId
      }
      vis_render $CurrentRenderer
    }
}

proc PickPolyDataPoint {widget x y add delete} {
    global CurrentRenderer RendererFound
    global PickedAssembly PrePickedProperty WindowY
    global PickedAssemblyRenderer
    global gActorPicked
    global gNumPickedPoints
    global gPickedPointIds

    if {$delete == 1} {
      for {set i 1} {$i <= $gNumPickedPoints} {incr i} {
	catch {$CurrentRenderer RemoveActor newActor_$gPickedPointIds($i)}
	catch {newActor_$gPickedPointIds($i) Delete}
      }
      array unset gPickedPointIds *
      set gNumPickedPoints 0
    } else {

      set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
      set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

      set WindowY [lindex [$widget configure -height] 4]

      if {! $RendererFound } {return}
      PolyDataPointPicker SetTolerance 0.0001
      PolyDataPointPicker Pick $x [expr $WindowY - $y - 1] 0.0 $CurrentRenderer

      set worldPosition [PolyDataPointPicker GetPickPosition]
      set pointId [PolyDataPointPicker GetPointId]
       
      set deletedActor 0
      if {$add == 0} {
	for {set i 1} {$i <= $gNumPickedPoints} {incr i} {
	  catch {$CurrentRenderer RemoveActor newActor_$gPickedPointIds($i)}
	  catch {newActor_$gPickedPointIds($i) Delete}
	}
	array unset gPickedPointIds *
	set gNumPickedPoints 0
      } elseif {$add == 1} {
        set count 0
	for {set i 1} {$i <= $gNumPickedPoints} {incr i} {
	  if {$pointId == $gPickedPointIds($i)} {
	    catch {$CurrentRenderer RemoveActor newActor_$pointId}
	    catch {newActor_$pointId Delete}
	    set deletedActor 1
	  } else {
	    incr count
	    set tempPointIds($count) $gPickedPointIds($i)
	  }
	}
	array unset gPickedPointIds *
	set gNumPickedPoint $count
	for {set i 1} {$i <= $count} {incr i} {
	  set gPickedPointIds($i) $tempPointIds($i)
	}
      }	

      if {$pointId !=  -1 && $deletedActor == 0} {

	incr gNumPickedPoints
	set gPickedPointIds($gNumPickedPoints) $pointId 

        puts "Position is: "
        puts "[lindex $worldPosition 0],"
        puts "[lindex $worldPosition 1],"
        puts "[lindex $worldPosition 2],"

	catch {ids Delete}
	catch {selectionNode Delete}
	catch {extractSelection Delete}
	catch {newSelection Delete}
	catch {selected Delete}
	catch {newMapper Delete}

	vtkIdTypeArray ids
	ids SetNumberOfComponents 1
	ids InsertNextValue $pointId

	vtkSelectionNode selectionNode
	#Field Type 0 is CELL
	selectionNode SetFieldType 0
	#Content Type 4 is INDICES
	selectionNode SetContentType 4
	selectionNode SetSelectionList ids
	
	vtkSelection newSelection
	newSelection AddNode selectionNode

	vtkExtractSelection extractSelection
	extractSelection SetInputData 0 [PolyDataPointPicker GetDataSet]
	extractSelection SetInputData 1 newSelection
	extractSelection Update

	vtkUnstructuredGrid selected
	selected ShallowCopy [extractSelection GetOutput]

#	puts "Number of Cells: [selected GetNumberOfCells]"
#	puts "Number of Points: [selected GetNumberOfPoints]"

	vtkDataSetMapper newMapper
	newMapper SetInputData selected

	vtkActor newActor_$pointId
	newActor_$pointId SetMapper newMapper
	[newActor_$pointId GetProperty] SetRepresentationToPoints
	[newActor_$pointId GetProperty] SetPointSize 100
	[newActor_$pointId GetProperty] SetEdgeVisibility 1
	[newActor_$pointId GetProperty] SetColor 0 0 1
	[newActor_$pointId GetProperty] SetEdgeColor 1 1 1
	[newActor_$pointId GetProperty] SetLineWidth 100

	 $CurrentRenderer AddActor newActor_$pointId
      }
      vis_render $CurrentRenderer
    }
}

proc DeselectPickedActor {} {

  global CurrentRenderer RendererFound
  global PickedAssembly PrePickedProperty WindowY
  global PickedAssemblyRenderer

  # set the actor here
  if {$PickedAssembly == ""} {
    return
  }

  # if current picked assembly has been deleted somehow,
  # reset the pick.  That is, if the tcl command
  # (i.e. vtk object) doesn't exist, reset the pick.
  if {[info commands $PickedAssembly] == ""} {
    set PickedAssembly ""
    if {$PrePickedProperty != ""} {
      # release hold on the property
      $PrePickedProperty UnRegister $PrePickedProperty
      set PrePickedProperty ""
    }
    # make sure the selected color still exists
    if {[info commands PickedProperty] == ""} {
      vtkProperty PickedProperty
    }
    PickedProperty SetColor 1 1 0
    PickedProperty SetOpacity 1
    return
  }

  if { $PickedAssembly != "" && $PrePickedProperty != "" } {

        # need to check if object is a lod obj
        if {[$PickedAssembly GetClassName] == "vtkLODProp3D"} {
          set first [string first act $PickedAssembly]
          set last [expr $first + 2]
          set property [string replace $PickedAssembly $first $last prop]
          # need to reset property for all actors in lod obj
          set propCol PickActorPropCollection
	  catch {$propCol Delete}
          vtkPropCollection $propCol
          $PickedAssembly GetActors $propCol

          for {set i 0} {$i < [$propCol GetNumberOfItems]} {incr i} {
             set actor [$propCol GetItemAsObject $i]
             $actor SetProperty $PrePickedProperty
	  }

	} else {

          $PickedAssembly SetProperty $PrePickedProperty

	}

        # release hold on the property
        $PrePickedProperty UnRegister $PrePickedProperty
        set PrePickedProperty ""
  }

}


#catch {genericInteractor delete}
#catch {genericInteractorStyle delete}
#vtkGenericRenderWindowInteractor genericInteractor
#vtkInteractorStyleSwitch genericInteractorStyle
#genericInteractorStyle SetCurrentStyleToTrackballCamera
#genericInteractor SetInteractorStyle genericInteractorStyle

global RubberZoomPerformed
global gB1PRESSED
global gB2PRESSED
global gB3PRESSED
global gInteractorInteracting
global gInteractorRen
set gB1PRESSED 0
set gB2PRESSED 0
set gB3PRESSED 0
set gInteractorInteracting 0
set gInteractorRen {}

proc vis_interactorToggle {W x y} {
  global guiRIB
  if {[info exists guiRIB(interactor_displayed)] == 0} {
    return
  }
  set curdisp $guiRIB(interactor_displayed)
  global gRen3d
  if {$curdisp == 1} {
    set state [vis_boxWidgetGetEnabled $gRen3d resampleGUI]
    if {$state == 0} {
      vis_boxWidgetOn $gRen3d resampleGUI
    } else {
      vis_boxWidgetOff $gRen3d resampleGUI
    }
  }
}

proc vis_interactorEnter {W x y} {

  set x [expr [winfo pointerx $W] - [winfo rootx $W]] 
  set y [expr [winfo pointery $W] - [winfo rooty $W]] 

  vis_interactorGeneric EnterEvent $W $x $y
  Enter $W $x $y
}

proc vis_interactorLeave {W x y} {

  set x [expr [winfo pointerx $W] - [winfo rootx $W]] 
  set y [expr [winfo pointery $W] - [winfo rooty $W]] 

  vis_interactorGeneric LeaveEvent $W $x $y
  global oldFocus
  focus $oldFocus
}

proc vis_interactorGeneric {event W x y} {

  set x [expr [winfo pointerx $W] - [winfo rootx $W]] 
  set y [expr [winfo pointery $W] - [winfo rooty $W]] 

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
  global gInteractorRen
  set gInteractorRen $ren
  genericInteractor_$ren SetEventInformation $x $y $ctrl \
                         $shift $keycode $repeatcount $keysym
  genericInteractor_$ren $event
}

proc vis_interactorRotate {W x y {shouldRender 1}} {

  set x [expr [winfo pointerx $W] - [winfo rootx $W]] 
  set y [expr [winfo pointery $W] - [winfo rooty $W]] 
  
  global gB1PRESSED
  if {$gB1PRESSED == 0} {
    set gB1PRESSED 1
    vis_interactorGeneric LeftButtonPressEvent $W $x $y
  } else {
    vis_interactorGeneric MouseMoveEvent $W $x $y
  }
  global gInteractorInteracting
  if {$gInteractorInteracting == 0} {
    Rotate $W $x $y $shouldRender
  }
}



proc vis_interactorSpecialRotate {W x y} {
    global LastX LastY
    vis_interactorRotate $W [expr ($LastX+90)] $LastY 0
    vis_interactorRotate $W $LastX $y 0
    vis_interactorRotate $W [expr ($LastX-90)] $LastY
}

proc vis_interactorB1release {W x y} {
  vis_interactorGeneric LeftButtonReleaseEvent $W $x $y
  global gB1PRESSED
  set gB1PRESSED 0
}

# Objects used to display rubberband
vtkPoints            RubberBandPoints
vtkCellArray         RubberBandLines
vtkFloatArray RubberBandScalars
vtkPolyData          RubberBandPolyData
vtkPolyDataMapper2D  RubberBandMapper
vtkActor2D           RubberBandActor
vtkLookupTable       RubberBandColors

RubberBandPolyData SetPoints      RubberBandPoints
RubberBandPolyData SetLines       RubberBandLines
RubberBandMapper   SetInputDataObject   RubberBandPolyData
RubberBandMapper   SetLookupTable RubberBandColors
RubberBandActor    SetMapper      RubberBandMapper

RubberBandColors SetNumberOfTableValues 2
RubberBandColors SetNumberOfColors 2
RubberBandColors SetTableValue 0 1.0 0.0 0.0 1.0
RubberBandColors SetTableValue 1 1.0 1.0 1.0 1.0

[RubberBandPolyData GetPointData] SetScalars RubberBandScalars

RubberBandMapper SetScalarRange 0 1

RubberBandPoints InsertPoint 0  0  0  0
RubberBandPoints InsertPoint 1  0 10  0
RubberBandPoints InsertPoint 2 10 10  0
RubberBandPoints InsertPoint 3 10  0  0

RubberBandLines  InsertNextCell 5
RubberBandLines  InsertCellPoint 0
RubberBandLines  InsertCellPoint 1
RubberBandLines  InsertCellPoint 2
RubberBandLines  InsertCellPoint 3
RubberBandLines  InsertCellPoint 0

RubberBandScalars InsertNextTuple1 0
RubberBandScalars InsertNextTuple1 1
RubberBandScalars InsertNextTuple1 0
RubberBandScalars InsertNextTuple1 1

RubberBandMapper ScalarVisibilityOn

# Called when the mouse button is release - do the zoom
proc DoRubberZoom { widget } {
    global CurrentCamera CurrentRenderer
    global RendererFound
    global StartRubberZoomX StartRubberZoomY
    global EndRubberZoomX EndRubberZoomY

    # Return if there is no renderer, or the rubber band is less
    # that 5 pixels in either direction
    if { ! $RendererFound } { return }
    if { [expr $StartRubberZoomX - $EndRubberZoomX] < 5 && \
	    [expr $StartRubberZoomX - $EndRubberZoomX] > -5 } { return }
    if { [expr $StartRubberZoomY - $EndRubberZoomY] < 5 && \
	    [expr $StartRubberZoomY - $EndRubberZoomY] > -5 } { return }
    
    # We'll need the window height later
    set WindowY [lindex [$widget configure -height] 4]

    # What is the center of the rubber band box in pixels?
    set centerX [expr ($StartRubberZoomX + $EndRubberZoomX)/2.0]
    set centerY [expr ($StartRubberZoomY + $EndRubberZoomY)/2.0]

    # Convert the focal point to a display coordinate in order to get the
    # depth of the focal point in display units
    set FPoint [$CurrentCamera GetFocalPoint]
        set FPoint0 [lindex $FPoint 0]
        set FPoint1 [lindex $FPoint 1]
        set FPoint2 [lindex $FPoint 2]
    $CurrentRenderer SetWorldPoint $FPoint0 $FPoint1 $FPoint2 1.0
    $CurrentRenderer WorldToDisplay
    set DPoint [$CurrentRenderer GetDisplayPoint]
    set focalDepth [lindex $DPoint 2]

    # Convert the position of the camera to a display coordinate in order
    # to get the depth of the camera in display coordinates. Note this is
    # a negative number (behind the near clipping plane of 0) but it works
    # ok anyway
    set PPoint [$CurrentCamera GetPosition]
        set PPoint0 [lindex $PPoint 0]
        set PPoint1 [lindex $PPoint 1]
        set PPoint2 [lindex $PPoint 2]
    $CurrentRenderer SetWorldPoint $PPoint0 $PPoint1 $PPoint2 1.0
    $CurrentRenderer WorldToDisplay
    set DPoint [$CurrentRenderer GetDisplayPoint]
    set positionDepth [lindex $DPoint 2]

    # Find out the world position of where our new focal point should
    # be - it will be at the center of the box, back at the same focal depth
    # Don't actually set it now - we need to do all our computations before
    # we modify the camera
    $CurrentRenderer SetDisplayPoint $centerX $centerY $focalDepth
    $CurrentRenderer DisplayToWorld
    set newFocalPoint [$CurrentRenderer GetWorldPoint]
    set newFocalPoint0 [lindex $newFocalPoint 0]
    set newFocalPoint1 [lindex $newFocalPoint 1]
    set newFocalPoint2 [lindex $newFocalPoint 2]
    set newFocalPoint3 [lindex $newFocalPoint 3]
    if { $newFocalPoint3 != 0.0 } {
        set newFocalPoint0 [expr $newFocalPoint0 / $newFocalPoint3]
        set newFocalPoint1 [expr $newFocalPoint1 / $newFocalPoint3]
        set newFocalPoint2 [expr $newFocalPoint2 / $newFocalPoint3]
    }

    # Find out where the new camera position will be - at the center of
    # the rubber band box at the position depth. Don't set it yet...
    $CurrentRenderer SetDisplayPoint $centerX $centerY $positionDepth
    $CurrentRenderer DisplayToWorld
    set newPosition [$CurrentRenderer GetWorldPoint]
    set newPosition0 [lindex $newPosition 0]
    set newPosition1 [lindex $newPosition 1]
    set newPosition2 [lindex $newPosition 2]
    set newPosition3 [lindex $newPosition 3]
    if { $newPosition3 != 0.0 } {
        set newPosition0 [expr $newPosition0 / $newPosition3]
        set newPosition1 [expr $newPosition1 / $newPosition3]
        set newPosition2 [expr $newPosition2 / $newPosition3]
    }

    # We figured out how to position the camera to be centered, now we
    # need to "zoom". In parallel, this is simple since we only need to
    # change our parallel scale to encompass the entire y range of the
    # rubber band box. In perspective, we assume the box is drawn on the
    # near plane - this means that it is not possible that someone can
    # draw a rubber band box around a nearby object and dolly past it. It 
    # also means that you won't get very close to distance objects - but that
    # seems better than getting lost.
    if {[$CurrentCamera GetParallelProjection]} {
	# the new scale is just based on the y size of the rubber band box
	# compared to the y size of the window
	set ydiff [expr ($StartRubberZoomX - $EndRubberZoomX)]
	if { $ydiff < 0.0 } { set ydiff [expr $ydiff * -1.0] }
	set newScale [$CurrentCamera GetParallelScale]
	set newScale [expr $newScale * $ydiff / $WindowY]

	# now we can actually modify the camera
	$CurrentCamera SetFocalPoint $newFocalPoint0 $newFocalPoint1 $newFocalPoint2
	$CurrentCamera SetPosition $newPosition0 $newPosition1 $newPosition2
	$CurrentCamera SetParallelScale $newScale

    } else {
	# find out the center of the rubber band box on the near plane
	$CurrentRenderer SetDisplayPoint $centerX $centerY 0.0
	$CurrentRenderer DisplayToWorld
	set nearFocalPoint [$CurrentRenderer GetWorldPoint]
	set nearFocalPoint0 [lindex $nearFocalPoint 0]
	set nearFocalPoint1 [lindex $nearFocalPoint 1]
	set nearFocalPoint2 [lindex $nearFocalPoint 2]
	set nearFocalPoint3 [lindex $nearFocalPoint 3]
	if { $nearFocalPoint3 != 0.0 } {
	    set nearFocalPoint0 [expr $nearFocalPoint0 / $nearFocalPoint3]
	    set nearFocalPoint1 [expr $nearFocalPoint1 / $nearFocalPoint3]
	    set nearFocalPoint2 [expr $nearFocalPoint2 / $nearFocalPoint3]
	}

	# find the world coordinates of the point centered on the rubber band box
	# in x, on the border in y, and at the near plane depth.
	$CurrentRenderer SetDisplayPoint $centerX $StartRubberZoomY  0.0
	$CurrentRenderer DisplayToWorld
	set focalEdge [$CurrentRenderer GetWorldPoint]
        set focalEdge0 [lindex $focalEdge 0]
        set focalEdge1 [lindex $focalEdge 1]
        set focalEdge2 [lindex $focalEdge 2]
        set focalEdge3 [lindex $focalEdge 3]
	if { $focalEdge3 != 0.0 } {
	    set focalEdge0 [expr $focalEdge0 / $focalEdge3]
	    set focalEdge1 [expr $focalEdge1 / $focalEdge3]
	    set focalEdge2 [expr $focalEdge2 / $focalEdge3]
	}

	# how far is this "rubberband edge point" from the focal point?
	set ydist [expr \
		sqrt( \
		($nearFocalPoint0 - $focalEdge0)*($nearFocalPoint0 - $focalEdge0) + \
		($nearFocalPoint1 - $focalEdge1)*($nearFocalPoint1 - $focalEdge1) + \
		($nearFocalPoint2 - $focalEdge2)*($nearFocalPoint2 - $focalEdge2) )]

	# We need to know how far back we must be so that when we view the scene
	# with the current view angle, we see all of the y range of the rubber
	# band box. Use a simple tangent equation - opposite / adjacent = tan theta
	# where opposite is half the y height of the rubber band box on the near
	# plane, adjacent is the distance we are solving for, and theta is half
	# the viewing angle. This distance that we solve for is the new distance
	# to the near plane - to find the new distance to the focal plane we
	# must take the old distance to the focal plane, subtract the near plane
	# distance, and add in the distance we solved for.
	set angle [expr 0.5 * (3.141592 / 180.0) * [$CurrentCamera GetViewAngle]]
	set d [expr $ydist/tan($angle)]
	set range [$CurrentCamera GetClippingRange]
	set nearplane [lindex $range 0]
	set factor [expr [$CurrentCamera GetDistance] / \
		([$CurrentCamera GetDistance] - $nearplane + $d)]

	# now we can actually modify the camera
	$CurrentCamera SetFocalPoint $newFocalPoint0 $newFocalPoint1 $newFocalPoint2
	$CurrentCamera SetPosition $newPosition0 $newPosition1 $newPosition2
	$CurrentCamera Dolly $factor
	$CurrentRenderer ResetCameraClippingRange
    }    
}

proc RubberZoom {widget x y} {
    global RendererFound
    global CurrentRenderer
    global RubberZoomPerformed
    global LastX LastY
    global StartRubberZoomX StartRubberZoomY
    global EndRubberZoomX EndRubberZoomY

    set x [expr [winfo pointerx $widget] - [winfo rootx $widget]] 
    set y [expr [winfo pointery $widget] - [winfo rooty $widget]] 

    if { ! $RendererFound } { return }

    set WindowY [lindex [$widget configure -height] 4]

    if { ! $RubberZoomPerformed } {
	$CurrentRenderer AddProp RubberBandActor

	set StartRubberZoomX $x
	set StartRubberZoomY [expr $WindowY - $y - 1]
	
	set RubberZoomPerformed 1
    }

    set EndRubberZoomX $x
    set EndRubberZoomY [expr $WindowY - $y - 1]

    RubberBandPoints SetPoint 0 $StartRubberZoomX $StartRubberZoomY  0
    RubberBandPoints SetPoint 1 $StartRubberZoomX $EndRubberZoomY    0
    RubberBandPoints SetPoint 2 $EndRubberZoomX   $EndRubberZoomY    0
    RubberBandPoints SetPoint 3 $EndRubberZoomX   $StartRubberZoomY  0

    Render $widget
}
