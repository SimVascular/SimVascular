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



proc vis_splineClosedWidgetAdd [list ren input name EnableEvent StartInteractionEvent \
                              InteractionEvent EndInteractionEvent] {

  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
  # create interactive cursor
  catch {$interactor Delete}
  vtkSplineWidget $interactor
  $interactor SetHandleSize 0.01
  $interactor SetPlaceFactor 0.01

  $interactor ClosedOn
  $interactor SetInputData $input
  $interactor PlaceWidget
  [$interactor GetHandleProperty] SetOpacity 0.33

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
      set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
      # create interactive cursor
      catch {$interactor Delete}
      vtkSplineWidget $interactor
      $interactor SetInputData $input
      $interactor PlaceWidget
      [$interactor GetHandleProperty] SetOpacity 0.33

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

proc vis_splineClosedWidgetRm {ren name} {
  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"

  vis_splineClosedWidgetOff $ren $name

  $interactor Delete
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
      $interactor Delete
    }
  }
}

proc vis_splineClosedWidgetOn {ren name} {
  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
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
      set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
      $interactor EnabledOn
      # having problems getting spline to display correctly,
      # do a dummy update of handle 0 to update display
      set dummy [$interactor GetHandlePosition 0]
      eval $interactor SetHandlePosition 0 $dummy
      vis_render $ren
    }
  }
}

proc vis_splineClosedWidgetOff {ren name} {
  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
  $interactor EnabledOff
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
      $interactor EnabledOff
    }
  }
}


proc vis_splineClosedWidgetSetPts {ren name pd numHandles} {

  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"

  set sampledPD /tmp/vis_splineClosedWidgetSetPts/pd
  catch {repos_delete -obj $sampledPD}
  # first sample the polygon to a higher density of evenly spaced points
  geom_sampleLoop -src $pd -dst $sampledPD -num [expr 2 * [geom_numPts -obj $pd]]
  set pts {}
  geom_getPts $sampledPD pts
  repos_delete -obj $sampledPD
  set numPts [llength $pts]

  set splineX vis_splineClosedWidgetSetPts-splineX
  set splineY vis_splineClosedWidgetSetPts-splineY
  set splineZ vis_splineClosedWidgetSetPts-splineZ
  catch {$splineX Delete}
  catch {$splineY Delete}
  catch {$splineZ Delete}
  vtkCardinalSpline $splineX
  vtkCardinalSpline $splineY
  vtkCardinalSpline $splineZ
  $splineX ClosedOn
  $splineY ClosedOn
  $splineZ ClosedOn

  set ptid 0
  foreach i $pts {
    $splineX AddPoint $ptid [lindex $i 0]
    $splineY AddPoint $ptid [lindex $i 1]
    $splineZ AddPoint $ptid [lindex $i 2]
    incr ptid
  }
  $splineX Compute
  $splineY Compute
  $splineZ Compute

  $interactor SetNumberOfHandles $numHandles

  set pts {}
  set numPts $numHandles
  set dx [expr double($ptid)/($numHandles)]
  set ptid 0
  for {set i 0} {$i < $numHandles} {incr i} {
    set d [expr $dx * $i]
    $interactor SetHandlePosition $ptid [$splineX Evaluate $d] \
                                        [$splineY Evaluate $d] \
                                        [$splineZ Evaluate $d]
    lappend pts [list [$splineX Evaluate $d] \
                      [$splineY Evaluate $d] \
                      [$splineZ Evaluate $d]]
    incr ptid
  }
  $splineX Delete
  $splineY Delete
  $splineZ Delete

  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren2 $gRen3dCopies {
      set interactor "vis_splineClosedWidget_interactor_$ren2\_$name"
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

  vis_splineClosedWidgetCalcTrans $ren $name $pts 1

}


proc vis_splineClosedWidgetUpdatePts {ren name pts} {

  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"

  set numPts [llength $pts]

  set ptid 0
  foreach i $pts {
    $interactor SetHandlePosition $ptid [lindex $i 0] \
                                        [lindex $i 1] \
                                        [lindex $i 2]
    incr ptid
  }

  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
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


proc vis_splineClosedWidgetGetPts {ren name} {
  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
  set pts {}
  set numPts [$interactor GetNumberOfHandles]
  for {set i 0} {$i < $numPts} {incr i} {
    lappend pts [$interactor GetHandlePosition $i]
  }
  return $pts
}


proc vis_splineClosedWidgetGetSamplePts {ren name numRtnPts} {

  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"

  set pts [vis_splineClosedWidgetGetPts $ren $name]

  set numPts [llength $pts]

  set splineX vis_splineClosedWidgetSetPts-splineX
  set splineY vis_splineClosedWidgetSetPts-splineY
  set splineZ vis_splineClosedWidgetSetPts-splineZ
  catch {$splineX Delete}
  catch {$splineY Delete}
  catch {$splineZ Delete}
  vtkCardinalSpline $splineX
  vtkCardinalSpline $splineY
  vtkCardinalSpline $splineZ
  $splineX ClosedOn
  $splineY ClosedOn
  $splineZ ClosedOn

  set ptid 0
  foreach i $pts {
    $splineX AddPoint $ptid [lindex $i 0]
    $splineY AddPoint $ptid [lindex $i 1]
    $splineZ AddPoint $ptid [lindex $i 2]
    incr ptid
  }
  $splineX Compute
  $splineY Compute
  $splineZ Compute

  set rtnpts {}
  set dx [expr double($ptid)/($numRtnPts-1)]
  set ptid 0
  for {set i 0} {$i < $numRtnPts} {incr i} {
    set d [expr $dx * $i]
    lappend rtnpts [list [$splineX Evaluate $d] \
                      [$splineY Evaluate $d] \
                      [$splineZ Evaluate $d]]
    incr ptid
  }
  $splineX Delete
  $splineY Delete
  $splineZ Delete

  return $rtnpts

}


proc vis_splineClosedWidgetSetNumHandles {ren name num} {
  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
  $interactor SetNumberOfHandles $num
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
      $interactor SetNumberOfHandles $num
    }
  }
}


proc vis_splineClosedWidgetGetNumHandles {ren name} {
  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"
  return [$interactor GetNumberOfHandles]
}


proc vis_splineClosedWidgetUpdatePtsPlaneOnly {ren name pts} {

  set interactor "vis_splineClosedWidget_interactor_$ren\_$name"

  set numPts [llength $pts]

  set ptid 0
  set newpts {}
  for {set i 0} {$i < $numPts} {incr i} {
    set pt2 [lindex $pts $i]
    set transform "vis_splineClosedWidget_interactor_$ren\_$name\_trans"
    set pt1 [$transform GetPosition]
    set nrm [math_normalize [$transform TransformNormal 0 0 1]]
    set v [math_subVectors $pt2 $pt1]
    set inp [math_subVectors $v [math_scaleVec $nrm [math_dot $v $nrm]]]
    set newpt [math_addVectors $pt1 $inp]
    $interactor SetHandlePosition $ptid [lindex $newpt 0] \
                                        [lindex $newpt 1] \
                                        [lindex $newpt 2]
    lappend newpts $newpt
    incr ptid
  }

  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren2 $gRen3dCopies {
      set interactor "vis_splineClosedWidget_interactor_$ren2\_$name"
      set ptid 0
      foreach i $newpts {
      $interactor SetHandlePosition $ptid [lindex $i 0] \
                                          [lindex $i 1] \
                                          [lindex $i 2]
      incr ptid
      }
    }
  }

  vis_splineClosedWidgetCalcSlice $ren $name $newpts 0

}


proc vis_splineClosedWidgetCalcTrans {ren name pts createTransforms} {

     set transform     "vis_splineClosedWidget_interactor_$ren\_$name\_trans"

     if {$createTransforms == 1} {
	 catch {$transform Delete}
         vtkTransform $transform
     }

     set poly vis_splineClosedWidget-tmp-poly
     set polyedges $poly\-edges

     catch {repos_delete -obj $poly}
     catch {repos_delete -obj $polyedges}

     geom_polygonFromPts $pts $poly
     set pt1 [geom_getPolyCentroid -src $poly]
     geom_getFreeEdges $poly $polyedges
     set norm [math_normalize [geom_polygonNorm -obj $polyedges]]

     repos_delete -obj $poly
     repos_delete -obj $polyedges

     set angle [math_radToDeg [math_angleBtw3DVectors {0 0 1} $norm]]
     set cross [math_cross {0 0 1} $norm]

     $transform Identity
     $transform PostMultiply
     $transform RotateWXYZ $angle [lindex $cross 0] [lindex $cross 1] [lindex $cross 2]
     $transform Translate [lindex $pt1 0] [lindex $pt1 1] [lindex $pt1 2]

}


proc vis_splineClosedWidgetCalcSlice {ren name pts createTransforms} {


    if {$createTransforms == 0} {
       vis_splineClosedWidgetCalcTrans $ren $name $pts $createTransforms
       return
    }

    set src [repos_exportToVtk -src volume_image]
    global gOptions
    set ext $gOptions(resliceDims)

    set oimg   [$src GetOrigin]       ;# *vtk* image origin
    set vdims  [$src GetSpacing]
    set rng    [$src GetScalarRange]
    set vmin   [math_minVec $vdims]

    set pdimx [expr [lindex $ext 0] * $vmin]
    set pdimy [expr [lindex $ext 1] * $vmin]

    # It appears to me (NW) that the origin doesn't belong here at all.  Of
    # course, I might be off by half a pixel now.  (see Ken's original code).
    set opln [list  [expr - 0.5 * $vmin - 0.5 * $pdimx]  [expr - 0.5 * $vmin - 0.5 * $pdimy]  0.0]

    set ors [list  [expr [lindex $opln 0] + 0.5 * $vmin]  [expr [lindex $opln 1] + 0.5 * $vmin]  [expr [lindex $opln 2]]]

    set pt1 [list  [expr [lindex $opln 0] + [lindex $ext 0] * $vmin]  [lindex $opln 1]  [lindex $opln 2]]

    set pt2 [list  [lindex $opln 0]  [expr [lindex $opln 1] + [lindex $ext 1] * $vmin]  [lindex $opln 2]]

    catch {vis_splineClosedWidgetRmSlice $ren $name}

    set numPts [llength $pts]

    vis_splineClosedWidgetCalcTrans $ren $name $pts 1

      set tr     "vis_splineClosedWidget_interactor_$ren\_$name\_trans"
      set rs     "vis_splineClosedWidget_interactor_$ren\_$name\_rs"
      set txt    "vis_splineClosedWidget_interactor_$ren\_$name\_txt"
      set pln    "vis_splineClosedWidget_interactor_$ren\_$name\_pln"
      set tmap   "vis_splineClosedWidget_interactor_$ren\_$name\_tmap"
      set mapper "vis_splineClosedWidget_interactor_$ren\_$name\_mapper"
      set act    "vis_splineClosedWidget_interactor_$ren\_$name\_act"
      set outflt "vis_splineClosedWidget_interactor_$ren\_$name\_outflt"
      set outmap "vis_splineClosedWidget_interactor_$ren\_$name\_outmap"
      set outact "vis_splineClosedWidget_interactor_$ren\_$name\_outact"

      vtkImageReslice $rs

      $rs SetInputDataObject $src

      $rs SetResliceTransform $tr
      $rs SetOutputSpacing $vmin $vmin $vmin
      $rs SetOutputOrigin [lindex $ors 0] [lindex $ors 1] [lindex $ors 2]
      $rs SetOutputExtent 0 [expr [lindex $ext 0] - 1]  0 [expr [lindex $ext 1] - 1]  0 0
      $rs InterpolateOn
      $rs Update

      vtkTexture $txt
      $txt SetInputDataObject [$rs GetOutput]
      $txt SetLookupTable g8bitGrayscaleLUT
      $txt Render $ren
      $txt RepeatOff

      vtkPlaneSource $pln
      $pln SetResolution 1 1
      $pln SetOrigin [lindex $opln 0] [lindex $opln 1] [lindex $opln 2]
      $pln SetPoint1 [lindex $pt1 0] [lindex $pt1 1] [lindex $pt1 2]
      $pln SetPoint2 [lindex $pt2 0] [lindex $pt2 1] [lindex $pt2 2]

      vtkTextureMapToPlane $tmap
      $tmap SetInputDataObject [$pln GetOutput]

      vtkDataSetMapper $mapper
      $mapper SetInputDataObject [$tmap GetOutput]
      $mapper SetScalarRange [lindex $rng 0] [lindex $rng 1]

      vtkActor $act
      $act SetMapper $mapper
      $act SetTexture $txt
      $act SetUserMatrix [$tr GetMatrix]

      vis_renAddActor $ren $act

      vtkOutlineFilter $outflt
      $outflt SetInputDataObject [$pln GetOutput]

      vtkPolyDataMapper $outmap
      $outmap SetInputDataObject [$outflt GetOutput]

      vtkActor $outact
      $outact SetMapper $outmap
      $outact SetUserMatrix [$tr GetMatrix]
      [$outact GetProperty] SetColor 0 1 0

      vis_renAddActor $ren $outact

      $tr Update
      $rs Update
      vis_render $ren

}


proc vis_splineClosedWidgetRmSlice {ren name} {

    set tr     "vis_splineClosedWidget_interactor_$ren\_$name\_trans"
    set rs     "vis_splineClosedWidget_interactor_$ren\_$name\_rs"
    set txt    "vis_splineClosedWidget_interactor_$ren\_$name\_txt"
    set pln    "vis_splineClosedWidget_interactor_$ren\_$name\_pln"
    set tmap   "vis_splineClosedWidget_interactor_$ren\_$name\_tmap"
    set mapper "vis_splineClosedWidget_interactor_$ren\_$name\_mapper"
    set act    "vis_splineClosedWidget_interactor_$ren\_$name\_act"
    set outflt "vis_splineClosedWidget_interactor_$ren\_$name\_outflt"
    set outmap "vis_splineClosedWidget_interactor_$ren\_$name\_outmap"
    set outact "vis_splineClosedWidget_interactor_$ren\_$name\_outact"

    catch {vis_renRmActor $ren $act}
    catch {vis_renRmActor $ren $outact}

    catch {$tr     Delete}
    catch {$rs     Delete}
    catch {$txt    Delete}
    catch {$pln    Delete}
    catch {$tmap   Delete}
    catch {$mapper Delete}
    catch {$act    Delete}
    catch {$outflt Delete}
    catch {$outmap Delete}
    catch {$outact Delete}

}

