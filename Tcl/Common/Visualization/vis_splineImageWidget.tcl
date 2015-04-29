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

proc vis_splineImageWidgetAdd [list ren input name EnableEvent StartInteractionEvent \
                              InteractionEvent EndInteractionEvent] {

  set interactor "vis_splineImageWidget_interactor_$ren\_$name"

  global $interactor\_showids
  set $interactor\_showids 0

  # create interactive cursor
  catch {$interactor Delete}
  vtkSplineWidget $interactor
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
    foreach ren2 $gRen3dCopies {
      set interactor "vis_splineImageWidget_interactor_$ren2\_$name"
      # create interactive cursor
      catch {$interactor Delete}
      vtkSplineWidget $interactor
      $interactor SetInputData $input
      $interactor PlaceWidget
      [$interactor GetHandleProperty] SetOpacity 0.33

      set iren genericInteractor_$ren2

      # Associate the point widget with the interactor
      $interactor SetInteractor $iren
      $interactor AddObserver EnableEvent $EnableEvent
      $interactor AddObserver StartInteractionEvent $StartInteractionEvent
      $interactor AddObserver InteractionEvent $InteractionEvent
      $interactor AddObserver EndInteractionEvent $EndInteractionEvent
    }
  }
}

proc vis_splineImageWidgetRm {ren name} {
  set interactor "vis_splineImageWidget_interactor_$ren\_$name"

  vis_splineImageWidgetOff $ren $name

  vis_splineImageWidgetRmSlices $ren $name

  $interactor Delete

  vis_render $ren

  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren2 $gRen3dCopies {
      set interactor "vis_splineImageWidget_interactor_$ren2\_$name"
#      vis_splineImageWidgetRmSlices $ren2 $name
      $interactor Delete
      vis_render $ren2
    }
  }

  vis_render $ren

}

proc vis_splineImageWidgetOn {ren name} {
  set interactor "vis_splineImageWidget_interactor_$ren\_$name"
  $interactor EnabledOn
  # having problems getting spline to display correctly,
  # do a dummy update of handle 0 to update display
  set dummy [$interactor GetHandlePosition 0]
  eval $interactor SetHandlePosition 0 $dummy
  vis_render $ren
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren2 $gRen3dCopies {
      set interactor "vis_splineImageWidget_interactor_$ren2\_$name"
      $interactor EnabledOn
      # having problems getting spline to display correctly,
      # do a dummy update of handle 0 to update display
      set dummy [$interactor GetHandlePosition 0]
      eval $interactor SetHandlePosition 0 $dummy
      vis_render $ren
    }
  }
}

proc vis_splineImageWidgetOff {ren name} {
  set interactor "vis_splineImageWidget_interactor_$ren\_$name"
  $interactor EnabledOff
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren2 $gRen3dCopies {
      set interactor "vis_splineImageWidget_interactor_$ren2\_$name"
      $interactor EnabledOff
    }
  }
}


proc vis_splineImageWidgetSetPts {ren name pd numHandles} {

  set interactor "vis_splineImageWidget_interactor_$ren\_$name"

  set pts {}
  geom_getPts $pd pts
  set numPts [llength $pts]

  set splineX vis_splineImageWidgetSetPts-splineX
  set splineY vis_splineImageWidgetSetPts-splineY
  set splineZ vis_splineImageWidgetSetPts-splineZ
  catch {$splineX Delete}
  catch {$splineY Delete}
  catch {$splineZ Delete}
  vtkCardinalSpline $splineX
  vtkCardinalSpline $splineY
  vtkCardinalSpline $splineZ
  $splineX ClosedOff
  $splineY ClosedOff
  $splineZ ClosedOff

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
  set dx [expr double($ptid)/($numHandles-1)]
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
      set interactor "vis_splineImageWidget_interactor_$ren2\_$name"
      $interactor SetNumberOfHandles $numPts
      set ptid 0
      foreach i $pts {
      $interactor SetHandlePosition $ptid [lindex $i 0] \
                                          [lindex $i 1] \
                                          [lindex $i 2]
      incr ptid
      }
      #vis_splineImageWidgetCalcSlices $ren $name $pts 1
    }
  }
  vis_splineImageWidgetCalcTrans  $ren $name $pts 1
  vis_splineImageWidgetCalcSlices $ren $name $pts 1

}


proc vis_splineImageWidgetUpdatePts {ren name pts} {

  set interactor "vis_splineImageWidget_interactor_$ren\_$name"

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
    foreach ren2 $gRen3dCopies {
      set interactor "vis_splineImageWidget_interactor_$ren2\_$name"
      set ptid 0
      foreach i $pts {
      $interactor SetHandlePosition $ptid [lindex $i 0] \
                                          [lindex $i 1] \
                                          [lindex $i 2]
      incr ptid
      }
      #vis_splineImageWidgetCalcSlices $ren $name $pts 0
    }
  }

  vis_splineImageWidgetCalcSlices $ren $name $pts 0

}



proc vis_splineImageWidgetUpdatePtsPlaneOnly {ren name pts} {

  set interactor "vis_splineImageWidget_interactor_$ren\_$name"

  set numPts [llength $pts]

  set ptid 0
  set newpts {}
  for {set i 0} {$i < $numPts} {incr i} {
    set pt2 [lindex $pts $i]
    set transform "vis_splineImageWidget_interactor_$ren\_$name\_trans$i"
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
      set interactor "vis_splineImageWidget_interactor_$ren2\_$name"
      set ptid 0
      foreach i $newpts {
      $interactor SetHandlePosition $ptid [lindex $i 0] \
                                          [lindex $i 1] \
                                          [lindex $i 2]
      incr ptid
      }
      #vis_splineImageWidgetCalcSlices $ren $name $newpts 0
    }
  }

  vis_splineImageWidgetCalcSlices $ren $name $newpts 0

}


proc vis_splineImageWidgetGetPts {ren name} {
  set interactor "vis_splineImageWidget_interactor_$ren\_$name"
  set pts {}
  set numPts [$interactor GetNumberOfHandles]
  for {set i 0} {$i < $numPts} {incr i} {
    lappend pts [$interactor GetHandlePosition $i]
  }
  return $pts
}


proc vis_splineImageWidgetSetNumHandles {ren name num} {

  # clean out the old slices first
  vis_splineImageWidgetRmSlices $ren $name

  set interactor "vis_splineImageWidget_interactor_$ren\_$name"
  $interactor SetNumberOfHandles $num

  set pts [vis_splineImageWidgetGetPts $ren $name]
  set numPts [llength $pts]

  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren2 $gRen3dCopies {
      set interactor "vis_splineImageWidget_interactor_$ren2\_$name"
      $interactor SetNumberOfHandles $num
      #vis_splineImageWidgetCalcSlices $ren $name $pts 1
    }
  }

  vis_splineImageWidgetCalcSlices $ren $name $pts 1

  vis_render $ren

}


proc vis_splineImageWidgetGetNumHandles {ren name} {
  set interactor "vis_splineImageWidget_interactor_$ren\_$name"
  return [$interactor GetNumberOfHandles]
}


proc vis_splineImageWidgetCalcTrans {ren name pts createTransforms} {
  set splineX "vis_splineImageWidget_interactor_$ren\_$name\_splineX"
  set splineY "vis_splineImageWidget_interactor_$ren\_$name\_splineY"
  set splineZ "vis_splineImageWidget_interactor_$ren\_$name\_splineZ"
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

     set transform "vis_splineImageWidget_interactor_$ren\_$name\_trans$i"
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
     #puts "pt1: $pt1"
     #puts "pt2: $pt2"
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



proc vis_splineImageWidgetCalcSlices {ren name pts createTransforms} {


    if {$createTransforms == 0} {
vis_splineImageWidgetCalcTrans $ren $name $pts $createTransforms
return
       if [catch {vis_splineImageWidgetCalcTrans $ren $name $pts $createTransforms} errmsg] {
          puts "errmsg: $errmsg"
       }
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

    catch {vis_splineImageWidgetRmSlices $ren $name}

    set numPts [llength $pts]

    global vis_splineImageWidget_interactor_$ren\_$name\_showids
    eval set dids \$vis_splineImageWidget_interactor_$ren\_$name\_showids

    vis_splineImageWidgetCalcTrans $ren $name $pts 1

    for {set i 0} {$i < $numPts} {incr i} {

      set tr     "vis_splineImageWidget_interactor_$ren\_$name\_trans$i"
      set rs     "vis_splineImageWidget_interactor_$ren\_$name\_rs$i"
      set txt    "vis_splineImageWidget_interactor_$ren\_$name\_txt$i"
      set pln    "vis_splineImageWidget_interactor_$ren\_$name\_pln$i"
      set tmap   "vis_splineImageWidget_interactor_$ren\_$name\_tmap$i"
      set mapper "vis_splineImageWidget_interactor_$ren\_$name\_mapper$i"
      set act    "vis_splineImageWidget_interactor_$ren\_$name\_act$i"
      set outflt "vis_splineImageWidget_interactor_$ren\_$name\_outflt$i"
      set outmap "vis_splineImageWidget_interactor_$ren\_$name\_outmap$i"
      set outact "vis_splineImageWidget_interactor_$ren\_$name\_outact$i"

      #vtkTransform $tr

      if {[lsearch $dids $i] < 0} {
        continue
      }

      vtkImageReslice $rs

      $rs SetInputDataObject $src

      $rs SetResliceTransform $tr
      $rs SetOutputSpacing $vmin $vmin $vmin
      $rs SetOutputOrigin [lindex $ors 0] [lindex $ors 1] [lindex $ors 2]
      $rs SetOutputExtent 0 [expr [lindex $ext 0] - 1]  0 [expr [lindex $ext 1] - 1]  0 0
      $rs InterpolateOn
      $rs Update

      vtkTexture $txt
      $txt SetInputConnection [$rs GetOutputPort]
      $txt SetLookupTable g8bitGrayscaleLUT
      $txt Render $ren
      $txt RepeatOff

      vtkPlaneSource $pln
      $pln SetResolution 1 1
      $pln SetOrigin [lindex $opln 0] [lindex $opln 1] [lindex $opln 2]
      $pln SetPoint1 [lindex $pt1 0] [lindex $pt1 1] [lindex $pt1 2]
      $pln SetPoint2 [lindex $pt2 0] [lindex $pt2 1] [lindex $pt2 2]

      vtkTextureMapToPlane $tmap
      $tmap SetInputConnection [$pln GetOutputPort]

      vtkDataSetMapper $mapper
      $mapper SetInputConnection [$tmap GetOutputPort]
      $mapper SetScalarRange [lindex $rng 0] [lindex $rng 1]

      vtkActor $act
      $act SetMapper $mapper
      $act SetTexture $txt
      $act SetUserMatrix [$tr GetMatrix]

      vis_renAddActor $ren $act

      vtkOutlineFilter $outflt
      $outflt SetInputConnection [$pln GetOutputPort]

      vtkPolyDataMapper $outmap
      $outmap SetInputConnection [$outflt GetOutputPort]

      vtkActor $outact
      $outact SetMapper $outmap
      $outact SetUserMatrix [$tr GetMatrix]
      [$outact GetProperty] SetColor 0 1 0

      vis_renAddActor $ren $outact

    }

    global vis_splineImageWidget_interactor_$ren\_$name\_showids
    eval set dids \$vis_splineImageWidget_interactor_$ren\_$name\_showids

    for {set i 0} {$i < $numPts} {incr i} {

      if {[lsearch $dids $i] < 0} {
        continue
      }

      set tr     "vis_splineImageWidget_interactor_$ren\_$name\_trans$i"
      set rs     "vis_splineImageWidget_interactor_$ren\_$name\_rs$i"
      set txt    "vis_splineImageWidget_interactor_$ren\_$name\_txt$i"
      set pln    "vis_splineImageWidget_interactor_$ren\_$name\_pln$i"
      set tmap   "vis_splineImageWidget_interactor_$ren\_$name\_tmap$i"
      set mapper "vis_splineImageWidget_interactor_$ren\_$name\_mapper$i"
      set act    "vis_splineImageWidget_interactor_$ren\_$name\_act$i"
      set outflt "vis_splineImageWidget_interactor_$ren\_$name\_outflt$i"
      set outmap "vis_splineImageWidget_interactor_$ren\_$name\_outmap$i"
      set outact "vis_splineImageWidget_interactor_$ren\_$name\_outact$i"

      $tr Update
      $rs Update
      vis_render $ren
# what is this?  this seems to destroy one of the render windows!
#      $act Render $ren $mapper
#      $outact Render $ren $mapper
    }
}


proc vis_splineImageWidgetRmSlices {ren name} {

  set num [vis_splineImageWidgetGetNumHandles $ren $name]
  for {set i 0} {$i < $num} {incr i} {
    set tr     "vis_splineImageWidget_interactor_$ren\_$name\_trans$i"
    set rs     "vis_splineImageWidget_interactor_$ren\_$name\_rs$i"
    set txt    "vis_splineImageWidget_interactor_$ren\_$name\_txt$i"
    set pln    "vis_splineImageWidget_interactor_$ren\_$name\_pln$i"
    set tmap   "vis_splineImageWidget_interactor_$ren\_$name\_tmap$i"
    set mapper "vis_splineImageWidget_interactor_$ren\_$name\_mapper$i"
    set act    "vis_splineImageWidget_interactor_$ren\_$name\_act$i"
    set outflt "vis_splineImageWidget_interactor_$ren\_$name\_outflt$i"
    set outmap "vis_splineImageWidget_interactor_$ren\_$name\_outmap$i"
    set outact "vis_splineImageWidget_interactor_$ren\_$name\_outact$i"

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

}


proc vis_splineImageWidgetSetShowIds {ren name ids} {
  set myvar vis_splineImageWidget_interactor_$ren\_$name\_showids
  global $myvar
  set $myvar $ids
}


proc vis_splineImageWidgetGetShowIds {ren name} {
  set myvar vis_splineImageWidget_interactor_$ren\_$name\_showids
  global $myvar
  eval set rtn \$$myvar
  return $rtn
}

