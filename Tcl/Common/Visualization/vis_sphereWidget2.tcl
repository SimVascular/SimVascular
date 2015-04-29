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

proc vis_sphereWidget2Add [list ren input name EnableEvent StartInteractionEvent \
                              InteractionEvent EndInteractionEvent] {

  set interactor "vis_sphereWidget2_interactor_$ren\_$name"
  # create interactive cursor
  catch {$interactor Delete}
  vtkSphereWidget2 $interactor
  #$interactor SetInputDataObject $input
  #$interactor PlaceWidget
  #$interactor OutlineCursorWiresOff
  #$interactor RotationEnabledOff
  $interactor ScalingEnabledOn
  $interactor TranslationEnabledOn
  #set property [$interactor GetHandleProperty]
  #$property SetOpacity 0.5
  #$interactor SetHandleSize 0.005

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
      set interactor "vis_sphereWidget2_interactor_$ren\_$name"
      # create interactive cursor
      catch {$interactor Delete}
      vtkBoxWidget $interactor
      $interactor SetInputDataObject $input
      #$interactor PlaceWidget
      $interactor ScalingEnabledOn
      $interactor TranslationEnabledOn
      #set property [$interactor GetHandleProperty]
      #$property SetOpacity 0.5
      #$interactor SetHandleSize 0.005

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

proc vis_sphereWidget2Rm {ren name} {
  set interactor "vis_sphereWidget2_interactor_$ren\_$name"

  vis_sphereWidget2Off $ren $name

  $interactor Delete
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_sphereWidget2_interactor_$ren\_$name"
      $interactor Delete
    }
  }
}

proc vis_sphereWidget2On {ren name} {
  set interactor "vis_sphereWidget2_interactor_$ren\_$name"
  $interactor EnabledOn
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_sphereWidget2_interactor_$ren\_$name"
      $interactor EnabledOn
    }
  }
}

proc vis_sphereWidget2Off {ren name} {
  set interactor "vis_sphereWidget2_interactor_$ren\_$name"
  $interactor EnabledOff
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      set interactor "vis_sphereWidget2_interactor_$ren\_$name"
      $interactor EnabledOff
    }
  }
}


proc vis_sphereWidget2GetSphere {ren name pd} {
  set interactor "vis_sphereWidget2_interactor_$ren\_$name"
  catch {$pd Delete}
  vtkPolyData $pd
  $pd Allocate 100 1000
  $interactor GetPolyData $pd
  return $pd
}


proc vis_sphereWidget2GetBB {ren name} {
  set interactor "vis_sphereWidget2_interactor_$ren\_$name"
  set pd "vis_sphereWidget2_interactor_$ren\_$name\_pd"
  catch {$pd Delete}
  vtkPolyData $pd
  $pd Allocate 100 1000
  $interactor GetPolyData $pd
  
  $pd ComputeBounds
  set bounds [$pd GetBounds]
  $pd Delete
  return $bounds
}


proc vis_sphereWidget2GetEnabled {ren name} {
  set interactor "vis_sphereWidget2_interactor_$ren\_$name"
  return [$interactor GetEnabled]
}


  
  

