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

proc vis_boxWidgetAdd [list ren input name EnableEvent StartInteractionEvent \
                              InteractionEvent EndInteractionEvent] {

  set interactor "vis_boxWidget_interactor_$ren\_$name"
  # create interactive cursor
  catch {$interactor Delete}
  vtkBoxWidget $interactor
  $interactor SetInputData $input
  $interactor PlaceWidget
  $interactor OutlineCursorWiresOff
  $interactor RotationEnabledOff
  $interactor TranslationEnabledOn
  set property [$interactor GetHandleProperty]
  $property SetOpacity 0.2
  set property [$interactor GetOutlineProperty]
  $property SetLineWidth 1
  $interactor SetHandleSize 0.005

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
      set interactor "vis_boxWidget_interactor_$ren\_$name"
      # create interactive cursor
      catch {$interactor Delete}
      vtkBoxWidget $interactor
      $interactor SetInputData $input
      $interactor PlaceWidget
      $interactor OutlineCursorWiresOff
      $interactor RotationEnabledOff
      $interactor TranslationEnabledOn
      set property [$interactor GetHandleProperty]
      $property SetOpacity 0.2
      set property [$interactor GetOutlineProperty]
      $property SetLineWidth 1
      $interactor SetHandleSize 0.005

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

proc vis_boxWidgetRm {ren name} {
  set interactor "vis_boxWidget_interactor_$ren\_$name"

  vis_boxWidgetOff $ren $name

  $interactor Delete
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_boxWidget_interactor_$ren\_$name"
      $interactor Delete
    }
  }
}

proc vis_boxWidgetOn {ren name} {
  set interactor "vis_boxWidget_interactor_$ren\_$name"
  $interactor EnabledOn
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_boxWidget_interactor_$ren\_$name"
      $interactor EnabledOn
    }
  }
}

proc vis_boxWidgetOff {ren name} {
  set interactor "vis_boxWidget_interactor_$ren\_$name"
  $interactor EnabledOff
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_boxWidget_interactor_$ren\_$name"
      $interactor EnabledOff
    }
  }
}

proc vis_boxWidgetRotateOn {ren name} {
  set interactor "vis_boxWidget_interactor_$ren\_$name"
  $interactor RotationEnabledOn
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_boxWidget_interactor_$ren\_$name"
      $interactor RotationEnabledOn
    }
  }
}

proc vis_boxWidgetRotateOff {ren name} {
  set interactor "vis_boxWidget_interactor_$ren\_$name"
  $interactor RotationEnabledOff
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_boxWidget_interactor_$ren\_$name"
      $interactor RotationEnabledOff
    }
  }
}


proc vis_boxWidgetGetBB {ren name} {
  set interactor "vis_boxWidget_interactor_$ren\_$name"
  set pd "vis_boxWidget_interactor_$ren\_$name\_pd"
  catch {$pd Delete}
  vtkPolyData $pd
  $pd Allocate 100 1000
  $interactor GetPolyData $pd

  $pd ComputeBounds
  set bounds [$pd GetBounds]
  $pd Delete
  return $bounds
}

# Procedure: vis_boxWidgetGetPlanes
# Returns a vtkPlanes object representing the implicit function marking the bounding box of the boxWidget
# Author: r2cheng

proc vis_boxWidgetGetPlanes {ren name} {
	set interactor "vis_boxWidget_interactor_$ren\_$name"
	set planes "vis_boxWidget_interactor_$ren\_$name\_planes"
	catch {$planes Delete}

	vtkPlanes $planes
	$interactor GetPlanes $planes
	return $planes
}


proc vis_boxWidgetGetPlanesPtsAndNrms {ren name} {
  set interactor "vis_boxWidget_interactor_$ren\_$name"
  set planes "vis_boxWidget_interactor_$ren\_$name\_planes"
  catch {$planes Delete}
  vtkPlanes $planes
  $interactor GetPlanes $planes

  set rtnstr {}
  for {set i 0} {$i < [[$planes GetPoints] GetNumberOfPoints]} {incr i} {
    set rtnstr "$rtnstr [[$planes GetPoints] GetPoint $i] [[$planes GetNormals] GetTuple3 $i]"
  }
  $planes Delete
  return $rtnstr

}

proc vis_boxWidgetGetEnabled {ren name} {
  set interactor "vis_boxWidget_interactor_$ren\_$name"
  return [$interactor GetEnabled]
}





