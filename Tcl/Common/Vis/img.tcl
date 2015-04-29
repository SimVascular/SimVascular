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

#
#  display vtkImageData
#

proc ::vis::img {ren obj args} {
    
    #@author Nathan Wilson
    #@a ren: renderer
    #@c Display a 2-D image object
    #@a obj: 2-D img object to display
    #@a args: sets the table range

    # be careful here.  If you add the same actor to renderer
    # twice it will seg fault if you try and pick the given
    # actor.  For this reason we blow away any existing
    # image actor for this render window.
    ::vis::imgRm $ren $obj

    set txt   [::vis::newobj img_texture_$ren\_$obj\_defaultTexture]
    set plane [::vis::newobj img_plane_$ren\_$obj]
    set tmap  [::vis::newobj img_tmap_$ren\_$obj]
    set cast  [::vis::newobj img_cast_$ren\_$obj]
    set map   [::vis::newobj img_map_$ren\_$obj]
    set actor [::vis::newobj img_actor_$ren\_$obj]
    set lookupGrayscale [::vis::newobj img_grayscaleLUT_$ren\_$obj]
    set lookupColor     [::vis::newobj img_blueToRedLUT_$ren\_$obj]

    vtkTexture $txt
    $txt ReleaseDataFlagOn   
    $txt ReleaseGraphicsResources [$ren GetRenderWindow]

    vtkPlaneSource $plane
    $plane SetResolution 1 1
    $plane SetOrigin 0.0 0.0 0.0

    vtkTextureMapToPlane $tmap
    $tmap SetInputDataObject [$plane GetOutput]

    vtkCastToConcrete $cast
    $cast SetInputDataObject [$tmap GetOutput]

    vtkPolyDataMapper $map
    $map SetInputDataObject [$cast GetOutput]

    vtkActor $actor
    $actor SetMapper $map
    $actor SetTexture $txt

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

    # Red-to-blue colormap
    # --------------------
    vtkLookupTable $lookupColor
    $lookupColor SetHueRange 0.6667 0.0
    $lookupColor SetSaturationRange 1.0 1.0
    $lookupColor SetValueRange 1.0 1.0
    $lookupColor SetAlphaRange 1.0 1.0
    $lookupColor SetNumberOfColors 16384
    $lookupColor Build

    set vorig [$obj GetOrigin]
    set vspc  [$obj GetSpacing]
    set vdims [$obj GetExtent]
    set irng  [$obj GetScalarRange]

    if { ([lindex $vdims 4] != [lindex $vdims 5]) && \
	    ([lindex $vdims 0] != [lindex $vdims 1]) && \
	    ([lindex $vdims 2] != [lindex $vdims 3]) } {
	return -code error "$obj not a planar image"
    }

    # Logical image dims:
    set ldims [list \
	    [expr [lindex $vdims 1] - [lindex $vdims 0] + 1] \
	    [expr [lindex $vdims 3] - [lindex $vdims 2] + 1] \
	    [expr [lindex $vdims 5] - [lindex $vdims 4] + 1]]

    # Physical image dimensions (spatial extent covered by pixels):
    set pdims [list \
	    [expr [lindex $ldims 0] * [lindex $vspc 0]] \
	    [expr [lindex $ldims 1] * [lindex $vspc 1]] \
	    [expr [lindex $ldims 2] * [lindex $vspc 2]]]

    # Physical origin of the physical extent:
    set porig [list \
	    [expr [lindex $vorig 0] - 0.5 * [lindex $vspc 0]] \
	    [expr [lindex $vorig 1] - 0.5 * [lindex $vspc 1]] \
	    [expr [lindex $vorig 2] - 0.5 * [lindex $vspc 2]]]

    $txt SetInputDataObject $obj
    $txt Render $ren
    $txt SetLookupTable $lookupGrayscale
    $txt RepeatOff

#    $plane SetOrigin [lindex $porig 0] [lindex $porig 1] [lindex $porig 2]
    $plane SetOrigin [lindex $porig 0] [lindex $porig 1] 0.0

    $plane SetPoint1 \
	    [expr [lindex $porig 0] + [lindex $pdims 0]] \
	    [lindex $porig 1] \
	    0.0

    $plane SetPoint2 \
	    [lindex $porig 0] \
	    [expr [lindex $porig 1] + [lindex $pdims 1]] \
	    0.0

    $plane SetNormal 0.0 0.0 1.0
    $plane Push -0.01

    # is map scalar range ignored?   we also set table range
    # since it seems to control texture map display

    $map SetScalarRange [lindex $irng 0] [lindex $irng 1]

    if {$args == ""} { 
      $lookupGrayscale SetTableRange [lindex $irng 0] [lindex $irng 1]
      $lookupColor SetTableRange [lindex $irng 0] [lindex $irng 1]
    } else {
      $lookupGrayscale SetTableRange [lindex $args 0] [lindex $args 1]
      $lookupColor SetTableRange [lindex $args 0] [lindex $args 1]
    }

    $ren AddActor $actor

    ::vis::render $ren
}


# ------------------
# ::vis::imgSetPlane
# ------------------

proc ::vis::imgSetPlane {ren obj newobj} {

    #@author Nathan Wilson
    #@c change the origin of the plane with the mapped texture map
    #@a ren:  renderer
    #@a obj:  object to render
    #@a newobj: object to get origin, spacing, and extent from for new plane

    set plane [::vis::getobj img_plane_$ren\_$obj]

    set vorig [$newobj GetOrigin]
    set vspc  [$newobj GetSpacing]
    set vdims [$newobj GetExtent]
 
    if { ([lindex $vdims 4] != [lindex $vdims 5]) && \
	    ([lindex $vdims 0] != [lindex $vdims 1]) && \
	    ([lindex $vdims 2] != [lindex $vdims 3]) } {
	return -code error "$obj not a planar image"
    }

    # Logical image dims:
    set ldims [list \
	    [expr [lindex $vdims 1] - [lindex $vdims 0] + 1] \
	    [expr [lindex $vdims 3] - [lindex $vdims 2] + 1] \
	    [expr [lindex $vdims 5] - [lindex $vdims 4] + 1]]

    # Physical image dimensions (spatial extent covered by pixels):
    set pdims [list \
	    [expr [lindex $ldims 0] * [lindex $vspc 0]] \
	    [expr [lindex $ldims 1] * [lindex $vspc 1]] \
	    [expr [lindex $ldims 2] * [lindex $vspc 2]]]

    # Physical origin of the physical extent:
    set porig [list \
	    [expr [lindex $vorig 0] - 0.5 * [lindex $vspc 0]] \
	    [expr [lindex $vorig 1] - 0.5 * [lindex $vspc 1]] \
	    [expr [lindex $vorig 2] - 0.5 * [lindex $vspc 2]]]

#    $plane SetOrigin [lindex $porig 0] [lindex $porig 1] [lindex $porig 2]
    $plane SetOrigin [lindex $porig 0] [lindex $porig 1] 0.0

    $plane SetPoint1 \
	    [expr [lindex $porig 0] + [lindex $pdims 0]] \
	    [lindex $porig 1] \
	    0.0

    $plane SetPoint2 \
	    [lindex $porig 0] \
	    [expr [lindex $porig 1] + [lindex $pdims 1]] \
	    0.0

    $plane SetNormal 0.0 0.0 1.0
    $plane Push -0.01

    ::vis::render $ren
}


# --------------------
# ::vis::imgShiftPlane
# --------------------

proc ::vis::imgShiftPlane {ren obj shift} {

    #@author Nathan Wilson
    #@c shift the origin of the plane with the mapped texture map
    #@a ren:  renderer
    #@a obj:  object to render
    #@a shift: tcl list giving x y z shift

    set plane [::vis::getobj img_plane_$ren\_$obj]

    set orgOrigin [$plane GetOrigin]
    set orgPoint1 [$plane GetPoint1]
    set orgPoint2 [$plane GetPoint2]

    $plane SetOrigin [expr [lindex $orgOrigin 0] + [lindex $shift 0]] \
	             [expr [lindex $orgOrigin 1] + [lindex $shift 1]] \
	             [expr [lindex $orgOrigin 2] + [lindex $shift 2]]

    $plane SetPoint1 [expr [lindex $orgPoint1 0] + [lindex $shift 0]] \
	             [expr [lindex $orgPoint1 1] + [lindex $shift 1]] \
	             [expr [lindex $orgPoint1 2] + [lindex $shift 2]]

    $plane SetPoint2 [expr [lindex $orgPoint2 0] + [lindex $shift 0]] \
	             [expr [lindex $orgPoint2 1] + [lindex $shift 1]] \
	             [expr [lindex $orgPoint2 2] + [lindex $shift 2]]

    ::vis::render $ren
}


# --------------
# ::vis::imgShow
# --------------

proc ::vis::imgShow {ren obj} {

    #@author Nathan Wilson
    #@c Show image object in given renderer
    #@a ren: renderer
    #@a obj: image object to show
    #@note Renderer::AddActor should only add an actor if it is not
    #@note already present.

    ::vis::imgUnshow $ren $obj
    set actor [::vis::getobj img_actor_$ren\_$obj]
    $ren AddActor $actor
    ::vis::render $ren

}


# ----------------
# ::vis::imgUnshow
# ----------------

proc ::vis::imgUnshow {ren obj} {

    #@author Nathan Wilson
    #@c Remove image object from given renderer
    #@a ren: renderer
    #@a obj: image object to unshow

    set actor [::vis::getobj img_actor_$ren\_$obj]

    if {[[$ren GetActors] IsItemPresent $actor] > 0} {
	$ren RemoveActor $actor
    }
    ::vis::render $ren

}


# -------------------
# ::vis::imgUnshowAll
# -------------------

proc ::vis::imgUnshowAll {ren} {

    #@author Nathan Wilson
    #@c Remove all image objects from given renderer
    #@a ren: renderer
    set allnames [::vis::getall img_actor_$ren\_]

    foreach name $allnames {      
      set actor [::vis::getobj $name]
      ::vis::actorRm $ren $actor
    }

}


# ------------
# ::vis::imgRm
# ------------

proc ::vis::imgRm {ren obj} {

    #@author Nathan Wilson
    #@c Remove image object from renderer
    #@a ren: renderer
    #@a obj: image object

    catch {::vis::imgUnshow $ren $obj}

    ::vis::rmall img_texture_$ren\_$obj
    ::vis::rmobj img_plane_$ren\_$obj
    ::vis::rmobj img_tmap_$ren\_$obj
    ::vis::rmobj img_cast_$ren\_$obj
    ::vis::rmobj img_map_$ren\_$obj
    ::vis::rmobj img_actor_$ren\_$obj
    ::vis::rmobj img_grayscaleLUT_$ren\_$obj
    ::vis::rmobj img_blueToRedLUT_$ren\_$obj

    return

}


# ---------------
# ::vis::imgRmAll
# ---------------

proc ::vis::imgRmAll {ren} {

    #@author Nathan Wilson
    #@c Remove all images from given renderer
    #@a ren: renderer

    ::vis::imgUnshowAll $ren

    set allnames [::vis::getall img_actor_$ren\_]

    foreach name $allnames {
      set str [string range $name 10 end]
      puts "str: $str"
      ::vis::rmall img_texture_$str
      ::vis::rmobj img_plane_$str
      ::vis::rmobj img_tmap_$str
      ::vis::rmobj img_cast_$str
      ::vis::rmobj img_map_$str
      ::vis::rmobj img_actor_$str
      ::vis::rmobj img_grayscaleLUT_$str
      ::vis::rmobj img_blueToRedLUT_$str
    }

    return

}


# ----------------------
# ::vis::imgInverseVideo
# ----------------------

proc ::vis::imgInverseVideo {ren obj flag} {

  #@author Nathan Wilson
  #@c Invert the LUT
  #@a ren: renderer
  #@a obj: image object
  #@a flag: used for SetInverseVideo method of grayscale LUT 
  set lookupGrayscale [::vis::getobj img_grayscaleLUT_$ren\_$obj]
  $lookupGrayscale SetInverseVideo $flag
  ::vis::render $ren
  return

}


# ------------------------
# ::vis::imgGetLookupTable
# ------------------------

proc ::vis::imgGetLookupTable {ren obj} {
    #@author Nathan Wilson
    #@c Get the LUT for the given object in renderer
    #@a ren: renderer
    #@a obj: image object
    #@r LUT object name
    return [::vis::getobj img_grayscaleLUT_$ren\_$obj]
}


# -----------------------
# ::vis::imgCreateTexture
# -----------------------

proc ::vis::imgCreateTexture {ren obj LUT uniqueId} {

    #@author Nathan Wilson
    #@c Create a texture for image object given LUT
    #@a ren: renderer
    #@a obj: image object
    #@a LUT: LookupTable
    #@a uniqueId:  unique identifier
    #@r texture name

    set txt [::vis::newobj img_texture_$ren\_$obj\_imgUniqueId$uniqueId]

    vtkTexture $txt
    $txt SetInputDataObject $obj
    $txt Render $ren
    $txt SetLookupTable $LUT
    $txt RepeatOff

    $txt ReleaseDataFlagOn   
    $txt ReleaseGraphicsResources [$ren GetRenderWindow]

    return $txt

}


# ---------------------
# ::vis::imgSwapTexture
# ---------------------

proc ::vis::imgSwapTexture {ren obj textureObj} {

    #@author Nathan Wilson
    #@c Swap textures 
    #@a ren: renderer
    #@a obj: image object
    #@a textureObj: new texture
    #@r previous texture

    set actor [::vis::getobj img_actor_$ren\_$obj]

    set rtntxt [$actor GetTexture]
    $actor SetTexture $textureObj 
    ::vis::render $ren
    return $rtntxt

}


# --------------------
# ::vis::imgGetTexture
# --------------------

proc ::vis::imgGetTexture {ren obj} {

    #@author Nathan Wilson
    #@c Get texture
    #@a ren: renderer
    #@a obj: image object
    #@r texture name

    set actor [::vis::getobj img_actor_$ren\_$obj]
    return [$actor GetTexture]

}


# ------------------
# ::vis::imgGetActor
# ------------------

proc ::vis::imgGetActor {ren obj} {
  #@author Nathan Wilson
  #@c Get actor displayed in renderer
  #@a ren: renderer
  #@a obj: image object
  #@r actor name 
  return [::vis::getobj img_actor_$ren\_$obj]
}


# -----------------------
# ::vis::imgSetTableRange
# -----------------------

proc ::vis::imgSetTableRange {ren obj min max} {

    #@author Nathan Wilson
    #@c Set the LUT table range
    #@a ren:  renderer
    #@a obj: image object
    #@a min: minimum value
    #@a max: maximum value

    if {$min >= $max} {
      return
    }

    set lookupGrayscale [::vis::getobj img_grayscaleLUT_$ren\_$obj]
    $lookupGrayscale SetTableRange $min $max

}


# -----------------------
# ::vis::imgGetTableRange
# -----------------------

proc ::vis::imgGetTableRange {ren obj} {

    #@author Nathan Wilson
    #@c Get the LUT table range
    #@a ren: renderer
    #@a obj: image object

    set lookupGrayscale [::vis::getobj img_grayscaleLUT_$ren\_$obj]
    return [$lookupGrayscale GetTableRange]

}


# --------------------------
# ::vis::imgSetTableRangeAll
# --------------------------

proc ::vis::imgSetTableRangeAll {ren min max} {

    #@author Nathan Wilson
    #@c Set the LUT table range for all textures in renderer
    #@a ren: renderer
    #@a min: minimum value
    #@a max: maximum value

    if {$min >= $max} {
      return
    }

    set allnames [::vis::getall img_grayscaleLUT_$ren\_]

    foreach name $allnames {      
      set lookupGrayscale [::vis::getobj $name]
      $lookupGrayscale SetTableRange $min $max
    }

}

