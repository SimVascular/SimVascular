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

proc vis_pointWidgetAdd [list ren input name EnableEvent StartInteractionEvent \
                              InteractionEvent EndInteractionEvent] {

  set interactor "vis_pointWidget_interactor_$ren\_$name"
  # create interactive cursor
  catch {$interactor Delete}
  vtkPointWidget $interactor
  $interactor SetInputData $input
  $interactor AllOff
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
      set interactor "vis_pointWidget_interactor_$ren\_$name"
      # create interactive cursor
      catch {$interactor Delete}
      vtkPointWidget $interactor
      $interactor SetInputData $input
      $interactor AllOff
      $interactor PlaceWidget

      set iren genericInteractor_$ren

      # Associate the point widget with the interactor
      $interactor SetInteractor $iren
      $interactor EnabledOff
      $interactor AddObserver EnableEvent $EnableEvent
      $interactor AddObserver StartInteractionEvent $StartInteractionEvent
      $interactor AddObserver InteractionEvent $InteractionEvent
      $interactor AddObserver EndInteractionEvent $EndInteractionEvent
    }
  }

}

proc vis_pointWidgetRm {ren name} {
  set interactor "vis_pointWidget_interactor_$ren\_$name"
  vis_pointWidgetOff $ren $name
  $interactor Delete
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_pointWidget_interactor_$ren\_$name"
      #vis_pointWidgetOff $ren $name
      $interactor Delete
    }
  }
}

proc vis_pointWidgetOn {ren name} {
  set interactor "vis_pointWidget_interactor_$ren\_$name"
  $interactor EnabledOn
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_pointWidget_interactor_$ren\_$name"
      $interactor EnabledOn
    }
  }
}

proc vis_pointWidgetOff {ren name} {
  set interactor "vis_pointWidget_interactor_$ren\_$name"
  $interactor EnabledOff
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_pointWidget_interactor_$ren\_$name"
      $interactor EnabledOff
    }
  }
}

proc vis_pointWidgetSetPosition {ren name x y z} {
  set interactor "vis_pointWidget_interactor_$ren\_$name"
  $interactor SetPosition $x $y $z
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_pointWidget_interactor_$ren\_$name"
      $interactor SetPosition $x $y $z
    }
  }
}

proc vis_pointWidgetGetPosition {ren name} {
  set interactor "vis_pointWidget_interactor_$ren\_$name"
  return [$interactor GetPosition]
}







