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

# -----------
# ::vis::poly
# -----------

proc ::vis::poly {ren obj} {

    #@author Nathan Wilson
    #@c Display a vtkPolyData object in the given renderer
    #@a ren: renderer
    #@a obj: object name

    ::vis::polyRm $ren $obj

    set tag [format "%s_%s" $ren $obj]

    set map [::vis::newobj p_map_$tag]
    set act [::vis::newobj p_act_$tag]

    vtkPolyDataMapper $map
    $map SetInputDataObject $obj
    $map ScalarVisibilityOff

    vtkActor $act
    $act SetMapper $map
    [$act GetProperty] SetColor 1 0 0

    ::vis::renAddActor $ren $act

    ::vis::render $ren

    return $act
}


# -------------------
# ::vis::polyGetActor
# -------------------

proc ::vis::polyGetActor {ren obj} {
  #@author Nathan Wilson
  #@a ren: renderer
  #@a obj: object name
  #@r actor object name
  return [::vis::getobj p_act_$ren\_$obj]
}


# ---------------
# ::vis::polyNorm
# ---------------

proc ::vis::polyNorm {ren obj} {

    #@c Add vtkPolyDataNormals to the vtkPolyData viewing pipeline.  
    #@note there is no proc to perform the reverse operation (i.e. remove 
    #@note the normals).  This is because the caller can presumably just 
    #@note turn off normal-based properties via the actor's vtkProperty.
    #@a ren: renderer
    #@a obj: object name

    set tag [format "%s_%s" $ren $obj]

    set map [::vis::getobj p_map_$tag]
    set act [::vis::getobj p_act_$tag]
    set nrm [::vis::newobj p_nrm_$tag]

    vtkPolyDataNormals $nrm
    $nrm SetInputDataObject [$map GetInput]
    $map SetInputDataObject [$nrm GetOutput]
    $nrm Update

    return $act

}


# -------------
# ::vis::polyRm
# -------------

proc ::vis::polyRm {ren obj} {

    #@c Remove the poly from the given renderer
    #@a ren: renderer
    #@a obj: object name

    if [catch {set act [::vis::getobj p_act_$ren\_$obj]}] {
      return
    }

    if {$act == $::vis::tkvars(PickedAssembly)} {
      ::vis::tkDeselectPickedActor
    }

    catch {::vis::polyUnshow $ren $obj}

    ::vis::rmobj p_grayscaleLUT_$ren\_$obj
    ::vis::rmobj p_blueToRedLUT_$ren\_$obj
    ::vis::rmobj p_nrm_$ren\_$obj
    ::vis::rmobj p_map_$ren\_$obj
    ::vis::rmobj p_act_$ren\_$obj
    ::vis::rmobj p_lmap_$ren\_$obj
    ::vis::rmobj p_labels_$ren\_$obj

    ::vis::render $ren

}


# ----------------
# ::vis::polyRmAll
# ----------------

proc ::vis::polyRmAll {ren} {

    #@c Remove all polys from renderer
    #@a ren: renderer

    ::vis::polyUnshowAll $ren

    ::vis::rmall p_grayscaleLUT_$ren\_
    ::vis::rmall p_blueToRedLUT_$ren\_
    ::vis::rmall p_nrm_$ren\_
    ::vis::rmall p_map_$ren\_
    ::vis::rmall p_act_$ren\_
    ::vis::rmobj p_lmap_$ren\_
    ::vis::rmobj p_labels_$ren\_

    ::vis::render $ren

    return

}


# ---------------
# ::vis::polyShow
# ---------------
# Note that Renderer::AddActor should only add an actor if it is not
# already present.

proc ::vis::polyShow {ren obj} {

    #@c Show object in renderer
    #@a ren: renderer
    #@a obj: object name

    ::vis::polyUnshow $ren $obj
    set actor [::vis::getobj p_act_$ren\_$obj]
    $ren AddActor $actor
    ::vis::render $ren

}


# -----------------
# ::vis::polyUnshow
# -----------------

proc ::vis::polyUnshow {ren obj} {

    #@c Remove actor from renderer
    #@a ren: renderer
    #@a obj: object name

    catch {::vis::polyUnshowIds $ren $obj}

    set actor [::vis::getobj p_act_$ren\_$obj]

    if {[[$ren GetActors] IsItemPresent $actor] > 0} {
	$ren RemoveActor $actor
    }
    ::vis::render $ren

}


# --------------------
# ::vis::polyUnshowAll
# --------------------

proc ::vis::polyUnshowAll {ren} {

    #@c Remove all poly actors from renderer
    #@a ren: renderer

    set allnames [::vis::getall p_labels_$ren\_]

    foreach name $allnames {      
      set actor [::vis::getobj $name]
      ::vis::actor2DRm $ren $actor
    }

    set allnames [::vis::getall p_act_$ren\_]

    foreach name $allnames {      
      set actor [::vis::getobj $name]
      ::vis::actorRm $ren $actor
    }

}


# ----------------------
# ::vis::polyShowScalars
# ----------------------

proc ::vis::polyShowScalars {ren obj} {

    #@c Show scalar values on poly in renderer in grayscale
    #@a ren: renderer
    #@a obj: object name

    set lookupGrayscale [::vis::newobj p_grayscaleLUT_$ren\_$obj]

    # Grayscale colormap
    # --------------------
    vtkWindowLevelLookupTable $lookupGrayscale
    $lookupGrayscale SetWindow 1023
    $lookupGrayscale SetLevel 512
    $lookupGrayscale SetWindow 0.5
    $lookupGrayscale SetLevel 0.5
    $lookupGrayscale SetHueRange 0.0 0.0
    $lookupGrayscale SetSaturationRange 0.0 0.0
    $lookupGrayscale SetValueRange 0.0 1.0
    $lookupGrayscale SetNumberOfColors 16384
    $lookupGrayscale Build

    set act [::vis::polyGetActor $ren $obj]
    [$act GetMapper] SetLookupTable $lookupGrayscale
    [$act GetMapper] ScalarVisibilityOn
    set range [[[[[$act GetMapper] GetInput] GetPointData] GetScalars] GetRange]
    [$act GetMapper] SetScalarRange [lindex $range 0] [lindex $range 1]

    ::vis::render $ren

}


# ---------------------------
# ::vis::polyShowColorScalars
# ---------------------------

proc ::vis::polyShowColorScalars {ren obj} {

    #@c Show scalar values on poly in renderer in color
    #@a ren: renderer
    #@a obj: object name

    set lookupColor     [::vis::newobj p_blueToRedLUT_$ren\_$obj]

    # Red-to-blue colormap
    # --------------------
    vtkLookupTable $lookupColor
    $lookupColor SetHueRange 0.6667 0.0
    $lookupColor SetSaturationRange 1.0 1.0
    $lookupColor SetValueRange 1.0 1.0
    $lookupColor SetAlphaRange 1.0 1.0
    $lookupColor SetNumberOfColors 16384
    $lookupColor Build

    set act [::vis::polyGetActor $ren $obj]
    [$act GetMapper] SetLookupTable $lookupColor
    [$act GetMapper] ScalarVisibilityOn
    set range [[[[[$act GetMapper] GetInput] GetPointData] GetScalars] GetRange]
    [$act GetMapper] SetScalarRange [lindex $range 0] [lindex $range 1]

    ::vis::render $ren

}


# ------------------
# ::vis::polyShowIds
# ------------------

proc ::vis::polyShowIds {ren obj} {

  #@c Show the point ids of a given poly object in renderer
  #@a ren: renderer
  #@a obj: object name

  catch {::vis::polyUnshowIds $ren $obj}

  set lmap [::vis::newobj p_lmap_$ren\_$obj]
  set labels [::vis::newobj p_labels_$ren\_$obj]

  vtkLabeledDataMapper $lmap
  $lmap SetLabelModeToLabelIds
  $lmap SetInputDataObject $obj
  $lmap SetLabelFormat "%g"
  vtkActor2D $labels
  $labels SetMapper $lmap
  $ren AddActor2D $labels

  ::vis::render $ren

}


# ----------------------
# ::vis::polyShowCellIds
# ----------------------

proc ::vis::polyShowCellIds {ren obj} {

  #@c Show the cell ids of a given poly object in a renderer
  #@a ren: renderer
  #@a obj: object name

  catch {::vis::polyUnshowCellIds $ren $obj}

  set lmap [::vis::newobj p_lmapc_$ren\_$obj]
  set labels [::vis::newobj p_labelsc_$ren\_$obj]
  set ids  [::vis::newobj p_cellids_$ren\_$obj]

  set filt [tmpobj]
  vtkCellCenters $filt
  $filt SetInputDataObject $obj
  $filt Update
  ::poly::copy [$filt GetOutput] $ids
  $filt Delete

  vtkLabeledDataMapper $lmap
  $lmap SetLabelModeToLabelIds
  $lmap SetInputDataObject $ids
  $lmap SetLabelFormat "%g"
  vtkActor2D $labels
  $labels SetMapper $lmap
  $ren AddActor2D $labels

  $ids Delete

  ::vis::render $ren

}


# ---------------------------
# ::vis::polyShowScalarValues
# ---------------------------

proc ::vis::polyShowScalarValues {ren obj} {

  #@c Show the scalar values for object in renderer
  #@a ren: renderer
  #@a obj: object name

  catch {::vis::polyUnshowScalarValues $ren $obj}
  set lmap [::vis::newobj p_lmap_$ren\_$obj]
  set labels [::vis::newobj p_labels_$ren\_$obj]

  vtkLabeledDataMapper $lmap
  $lmap SetLabelModeToLabelScalars
  $lmap SetInputDataObject $obj
  $lmap SetLabelFormat "%g"
  vtkActor2D $labels
  $labels SetMapper $lmap
  $ren AddActor2D $labels

  ::vis::render $ren

}


# --------------------
# ::vis::polyUnshowIds
# --------------------

proc ::vis::polyUnshowIds {ren obj} {

  #@c Remove the point ids from the poly in renderer
  #@a ren: renderer
  #@a obj: object name

  set lmap [::vis::getobj p_lmap_$ren\_$obj]
  set labels [::vis::getobj p_labels_$ren\_$obj]

  if {[[$ren GetActors2D] IsItemPresent $labels] > 0} {
	$ren RemoveActor2D $labels
  }
  ::vis::render $ren

}


# ------------------------
# ::vis::polyUnshowCellIds
# ------------------------

proc ::vis::polyUnshowCellIds {ren obj} {

  #@c Remove the cell ids from the poly in renderer
  #@a ren: renderer
  #@a obj: object name

  set lmap [::vis::getobj p_lmapc_$ren\_$obj]
  set labels [::vis::getobj p_labelsc_$ren\_$obj]

  if {[[$ren GetActors2D] IsItemPresent $labels] > 0} {
	$ren RemoveActor2D $labels
  }
  ::vis::render $ren

}


# -----------------------------
# ::vis::polyUnshowScalarValues
# -----------------------------

proc ::vis::polyUnshowScalarValues {ren obj} {
  #@c Remove the scalar values of object from renderer
  #@a ren: renderer
  #@a obj: object name

  ::vis::polyUnshowIds $ren $obj
}


# ---------------------------
# ::vis::polyDisplayWireframe
# ---------------------------

proc ::vis::polyDisplayWireframe {ren obj} {
    #@c Display object as wireframe
    #@a ren: renderer
    #@a obj: object name

    set actor [::vis::getobj p_act_$ren\_$obj]
    [$actor GetProperty] SetRepresentationToWireframe
    ::vis::render $ren

}


# ------------------------
# ::vis::polyDisplayPoints
# ------------------------

proc ::vis::polyDisplayPoints {ren obj} {
    #@c Display object as points
    #@a ren: renderer
    #@a obj: object name

    set actor [::vis::getobj p_act_$ren\_$obj]
    [$actor GetProperty] SetRepresentationToPoints
    ::vis::render $ren

}


# -------------------------
# ::vis::polyDisplaySurface
# -------------------------

proc ::vis::polyDisplaySurface {ren obj} {
    #@c Display object as surface
    #@a ren: renderer
    #@a obj: object name

    set actor [::vis::getobj p_act_$ren\_$obj]
    [$actor GetProperty] SetRepresentationToSurface
    ::vis::render $ren

}


# -------------------------
# ::vis::polyGetScalarRange
# -------------------------

proc ::vis::polyGetTableRange {ren obj} {

    #@author Nathan Wilson
    #@c Get the scalar range
    #@a ren: renderer
    #@a obj: image object

    set mapper [::vis::getobj p_map_$ren\_$obj]
    return [$mapper GetScalarRange]

}


# -------------------------
# ::vis::polySetScalarRange
# -------------------------

proc ::vis::polySetScalarRange {ren obj min max} {

    #@author Nathan Wilson
    #@c Set the scalar range
    #@a ren:  renderer
    #@a obj: image object
    #@a min: minimum value
    #@a max: maximum value

    if {$min >= $max} {
      return
    }

    set mapper [::vis::getobj p_map_$ren\_$obj]
    $mapper SetScalarRange $min $max

}
