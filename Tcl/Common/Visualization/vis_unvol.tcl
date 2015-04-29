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

# ------------
# vis_unvolRepos
# ------------

proc vis_unvolRepos {renWin objName} {

  #@author Nathan Wilson
  #@c Use ray casting to render volume data set inside of
  #@c of render window.
  #@a renWin: Render Window.
  #@a objName:  Repository StructuredPoints object.

#  if {[repos_exists -obj $objName] == 0} {
#    return -code error "ERROR:  Object does not exist."
#  }
#  if {[repos_type -obj $objName] != "StructuredPts"} {
#    return -code error "Incorrect object type."
#  }

  set caster  vis_unvol_caster_$objName
  set opacityTransferFunction  vis_unvol_opacityTransferFunction_$objName
  set colorTransferFunction  vis_unvol_colorTransferFunction_$objName
  set gradientTransferFunction vis_unvol_gradientTransferFunction_$objName
  set volumeProperty  vis_unvol_volumeProperty_$objName
  set volumeMapper  vis_unvol_volumeMapper_$objName
  set compositeFunction  vis_unvol_compositeFunction_$objName
  set outline  vis_unvol_outline_$objName
  set outlineMapper  vis_unvol_outlineMapper_$objName
  set outlineProperty  vis_unvol_outlineProperty_$objName
  set lod  vis_unvol_lod_$objName

  # delete objects if they already exist
  foreach i [list caster opacityTransferFunction colorTransferFunction \
                  volumeProperty compositeFunction outline outlineMapper \
                  outlineProperty lod volumeMapper gradientTransferFunction] {
     eval set obj \$$i
     catch {$obj Delete}
  }

#  vtkImageCast $caster
#  $caster SetOutputScalarTypeToUnsignedShort
#  $caster SetInputDataObject [repos_exportToVtk -src $objName]
# $caster Update

  # Create transfer functions for opacity and color
  vtkPiecewiseFunction $opacityTransferFunction
      $opacityTransferFunction AddPoint    0  0.1
      $opacityTransferFunction AddPoint   10  0.3
      $opacityTransferFunction AddPoint   60  0.8
      $opacityTransferFunction AddPoint   80  0.9
      $opacityTransferFunction AddPoint  100  1.0
      $opacityTransferFunction AddPoint 1000  1.0

  vtkColorTransferFunction $colorTransferFunction
      $colorTransferFunction AddRGBPoint       0.0 0.0 0.0 1.0
      $colorTransferFunction AddRGBPoint     100.0 1.0 0.0 0.0

  vtkPiecewiseFunction $gradientTransferFunction
      $gradientTransferFunction AddPoint   0     1.0
      $gradientTransferFunction AddPoint   1     1.0
      $gradientTransferFunction AddPoint   255   1.0
      $gradientTransferFunction AddPoint   512   1.0

  # Create properties, mappers, volume actors, and ray cast function
  vtkVolumeProperty $volumeProperty
      $volumeProperty SetColor $colorTransferFunction
      $volumeProperty SetScalarOpacity $opacityTransferFunction
      $volumeProperty SetGradientOpacity $gradientTransferFunction
      $volumeProperty SetInterpolationTypeToLinear

  global gOptions

  vtkUnstructuredGridVolumeRayCastMapper $volumeMapper
  vtkUnstructuredGridBunykRayCastFunction $compositeFunction
  
#  $volumeMapper SetInputDataObject [$caster GetOutput]
  $volumeMapper SetInputDataObject $objName

  vtkVolume $lod
  $lod SetProperty $volumeProperty
  $lod SetMapper $volumeMapper
  $renWin AddViewProp $lod

  return $lod
}


# ---------
# vis_unvolRm
# ---------

proc vis_unvolRm {ren objName} {

  #@author Nathan Wilson
  #@c Remove volume rendered object.
  #@a ren: Render Window.
  #@a objName:  Repository StructuredPoints object.

  set caster  vis_unvol_caster_$objName
  set opacityTransferFunction  vis_unvol_opacityTransferFunction_$objName
  set colorTransferFunction  vis_unvol_colorTransferFunction_$objName
  set gradientTransferFunction vis_unvol_gradientTransferFunction_$objName
  set volumeProperty  vis_unvol_volumeProperty_$objName
  set volumeMapper  vis_unvol_volumeMapper_$objName
  set compositeFunction  vis_unvol_compositeFunction_$objName
  set outline  vis_unvol_outline_$objName
  set outlineMapper  vis_unvol_outlineMapper_$objName
  set outlineProperty  vis_unvol_outlineProperty_$objName
  set lod  vis_unvol_lod_$objName

  # remove from rendering window
  if {[info commands $lod] == ""} {return}
  if {[[$ren GetViewProps] IsItemPresent $lod] > 0} {
       $ren RemoveViewProp $lod
       # delete all related objects
       foreach i [list caster opacityTransferFunction colorTransferFunction \
                  gradientTransferFunction \
                  volumeProperty compositeFunction outline outlineMapper \
                  outlineProperty lod volumeMapper] {
         eval set obj \$$i
         catch {$obj Delete}
        }
  }

  vis_render $ren

}


# ---------------------------------
# vis_unvolSetOpacityTransferFunction
# ---------------------------------

proc vis_unvolSetOpacityTransferFunction {ren objName values} {

  #@author Nathan Wilson
  #@c Change opacity transfer function.
  #@a ren: Render Window.
  #@a objName:  Repository StructuredPoints object.
  #@a values: Tcl list of scalar - opacity pairs.

  set lod vis_unvol_lod_$objName
  set opacityTransferFunction  vis_unvol_opacityTransferFunction_$objName

  if {[llength $values] == 0} {return}

  if {[info commands $opacityTransferFunction] != ""} {
    # clear old values
    $opacityTransferFunction RemoveAllPoints
    $opacityTransferFunction PrepareForNewData
    foreach i $values {
      if {[llength $i] != 2} {
        puts "Warning:  incorrect scalar-opacity pair ($i).  Ignored."
        continue
      }
      $opacityTransferFunction AddPoint  [lindex $i 0] [lindex $i 1]
    }

    #$lod SetSelectedLODID 0
    vis_render $ren

  }

  return
}


# ---------------------------------
# vis_unvolSetColorTransferFunction
# ---------------------------------

proc vis_unvolSetColorTransferFunction {ren objName values} {

  #@author Nathan Wilson
  #@c Change opacity transfer function.
  #@a ren: Render Window.
  #@a objName:  Repository StructuredPoints object.
  #@a values: Tcl list of scalar - opacity pairs.

  set lod vis_unvol_lod_$objName
  set colorTransferFunction  vis_unvol_colorTransferFunction_$objName

  if {[llength $values] == 0} {return}

  if {[info commands $colorTransferFunction] != ""} {
    # clear old values
    $colorTransferFunction RemoveAllPoints
    #$colorTransferFunction PrepareForNewData
    foreach i $values {
      if {[llength $i] != 4} {
        puts "Warning:  incorrect scalar-RGB pair ($i).  Ignored."
        continue
      }
      $colorTransferFunction AddRGBPoint  [lindex $i 0] [lindex $i 1] \
                                          [lindex $i 2] [lindex $i 3]
    }

    #$lod SetSelectedLODID 0
    vis_render $ren

  }

  return
}


# ----------------------------------
# vis_unvolSetGradientTransferFunction
# ----------------------------------

proc vis_unvolSetGradientTransferFunction {ren objName values} {

  #@author Nathan Wilson
  #@c Change gradient transfer function.
  #@a ren: Render Window.
  #@a objName:  Repository StructuredPoints object.
  #@a values: Tcl list of scalar - gradient pairs.

#  set lod vis_unvol_lod_$objName
  set gradientTransferFunction  vis_unvol_gradientTransferFunction_$objName

  if {[llength $values] == 0} {return}

  if {[info commands $gradientTransferFunction] != ""} {
    # clear old values
    $gradientTransferFunction RemoveAllPoints
    foreach i $values {
      if {[llength $i] != 2} {
        puts "Warning:  incorrect scalar-opacity pair ($i).  Ignored."
        continue
      }
      $gradientTransferFunction AddPoint  [lindex $i 0] [lindex $i 1]
    }

#    $lod SetSelectedLODID 0
    vis_render $ren

  }

  return
}


# ---------------------------
# vis_unvolSetShadingParameters
# ---------------------------

proc vis_unvolSetShadingParameters {ren objName ambient diffuse specular specular_power} {

  #@author Nathan Wilson
  #@c Change shading parameters.
  #@a ren: Render Window.
  #@a objName:  Repository StructuredPoints object.
  #@a ambient: ambient coefficient.
  #@a diffuse: diffuse coefficient.
  #@a specular: specular coefficient.
  #@a specular_power: specular power.

  set volumeProperty  vis_unvol_volumeProperty_$objName

  if {[info commands $volumeProperty] != ""} {
    $volumeProperty SetAmbient $ambient
    $volumeProperty SetDiffuse $diffuse
    $volumeProperty SetSpecular $specular
    $volumeProperty SetSpecularPower $specular_power
    vis_render $ren
  }

  return
}


# --------------
# vis_unvolShadeOn
# --------------

proc vis_unvolShadeOn {ren objName} {

  #@author Nathan Wilson
  #@c Change shading.
  #@a ren: Render Window.
  #@a objName:  Repository StructuredPoints object.

  set volumeProperty  vis_unvol_volumeProperty_$objName

  if {[info commands $volumeProperty] != ""} {
    $volumeProperty ShadeOn
    vis_render $ren
  }

  return
}


# ---------------
# vis_unvolShadeOff
# ---------------

proc vis_unvolShadeOff {ren objName} {

  #@author Nathan Wilson
  #@c Change shading.
  #@a ren: Render Window.
  #@a objName:  Repository StructuredPoints object.

  set volumeProperty  vis_unvol_volumeProperty_$objName

  if {[info commands $volumeProperty] != ""} {
    $volumeProperty ShadeOff
    vis_render $ren
  }

  return
}


# ------------------------
# vis_unvolSetSampleDistance
# ------------------------

proc vis_unvolSetSampleDistance {ren objName distance} {

  #@author Nathan Wilson
  #@c Change shading.
  #@a ren: Render Window.
  #@a objName:  Repository StructuredPoints object.
  #@a distance:  distance.

  set volumeMapper  vis_unvol_volumeMapper_$objName

  if {[info commands $volumeMapper] != ""} {
    $volumeMapper AutoAdjustSampleDistancesOff
    $volumeMapper SetImageSampleDistance $distance
    vis_render $ren
  }

  return
}


