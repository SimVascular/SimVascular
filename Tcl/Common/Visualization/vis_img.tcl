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

# -----------
# vis_imgInit
# -----------

proc vis_imgInit {ren} {

    set rdr   vis_img_reader_$ren
    set txt   vis_img_texture_$ren
    set plane vis_img_plane_$ren
    set tmap  vis_img_tmap_$ren
    set cast  vis_img_cast_$ren
    set map   vis_img_map_$ren
    set actor vis_img_actor_$ren
    set lookupGrayscale vis_img_g8bitGrayscaleLUT_$ren
    set lookupColor vis_img_gBlueToRedLUT_$ren

    if {[cmdExists $rdr]} {
	return
    }

    vtkImageReader $rdr
    $rdr SetDataScalarTypeToShort
#    $rdr FileLowerLeftOn
    $rdr SetFileDimensionality 2
    $rdr SetDataByteOrderToBigEndian

    vtkTexture $txt

    vtkPlaneSource $plane
    $plane SetResolution 1 1
    $plane SetOrigin 0.0 0.0 0.0

    vtkTextureMapToPlane $tmap
    $tmap SetInputConnection [$plane GetOutputPort]

    vtkCastToConcrete $cast
    $cast SetInputConnection [$tmap GetOutputPort]

    vtkPolyDataMapper $map
    $map SetInputConnection [$cast GetOutputPort]

    vtkActor $actor
    $actor SetMapper $map
    $actor SetTexture $txt

    # 8-bit grayscale lookup table
    # ----------------------------
    vtkLookupTable $lookupGrayscale
    $lookupGrayscale SetHueRange 0.0 0.0
    $lookupGrayscale SetSaturationRange 0.0 0.0
    $lookupGrayscale SetValueRange 0.0 1.0
    #$lookupGrayscale SetNumberOfColors 256
    $lookupGrayscale SetNumberOfColors 16384
    $lookupGrayscale Build

    # Red-to-blue colormap
    # --------------------
    vtkLookupTable $lookupColor
    $lookupColor SetHueRange 0.6667 0.0
    $lookupColor SetSaturationRange 1.0 1.0
    $lookupColor SetValueRange 1.0 1.0
    $lookupColor SetAlphaRange 1.0 1.0
    #$lookupColor SetNumberOfColors 256
    $lookupColor SetNumberOfColors 16384
    $lookupColor Build

}


# ----------------
# vis_imgSetOrigin
# ----------------
# Don't want to control vtkImageReader's origin, but rather the origin
# of the vtkPlaneSource being texture mapped.

proc vis_imgSetOrigin {ren x y z} {

    set plane vis_img_plane_$ren

    if {![cmdExists $plane]} {
	return
    }

    $plane SetOrigin $x $y $z
    vis_render $ren
}


# --------------
# vis_imgSetFile
# --------------

proc vis_imgSetFile {ren name dims spacing} {

    set rdr   vis_img_reader_$ren
    set txt   vis_img_texture_$ren
    set plane vis_img_plane_$ren
    set actor vis_img_actor_$ren

    if {![cmdExists $rdr]} {
	vis_imgInit $ren
    }

    set xsize [expr [lindex $dims 0] * [lindex $spacing 0]]
    set ysize [expr [lindex $dims 1] * [lindex $spacing 1]]

    $rdr Modified
    $rdr SetFileName $name
    $rdr SetDataExtent 0 [expr [lindex $dims 0] - 1] \
	    0 [expr [lindex $dims 1] - 1] 1 1
    $rdr SetDataSpacing [lindex $spacing 0] [lindex $spacing 1] 1.0
    $rdr Update

    $txt SetInputConnection [$rdr GetOutputPort]
    $txt Render $ren
    $txt SetLookupTable g8bitGrayscaleLUT
    $txt RepeatOff

    $plane SetPoint1 $xsize 0.0 0.0
    $plane SetPoint2 0.0 $ysize 0.0

    #$actor Update
    $ren AddActor $actor
    vis_render $ren
}


# ------------
# vis_imgRepos
# ------------
# We probably want an additional proc vis_imgUpdate which takes in a
# new texture, but which avoids the plane instantiation which occurs
# here.

proc vis_imgRepos {ren imgObj} {

    #set rdr   vis_img_reader_$ren

    #if {![cmdExists $rdr]} {
    #	vis_imgInit $ren
    #}

    # be careful here.  If you add the same actor to renderer
    # twice it will seg fault if you try and pick the given
    # actor.  For this reason we blow away any existing
    # image actor for this render window.
    catch {vis_imgRm $ren}
    vis_imgInit $ren

    if {[repos_type -obj $imgObj] != "StructuredPts"} {
	return -code error "$imgObj not an image"
    }

    set vimg  [repos_exportToVtk -src $imgObj]
    set vorig [$vimg GetOrigin]
    set vspc  [$vimg GetSpacing]
    set vdims [$vimg GetExtent]
    set irng  [$vimg GetScalarRange]

    if { ([lindex $vdims 4] != [lindex $vdims 5]) && \
	    ([lindex $vdims 0] != [lindex $vdims 1]) && \
	    ([lindex $vdims 2] != [lindex $vdims 3]) } {
	return -code error "$imgObj not a planar image"
    }

    set txt   vis_img_texture_$ren
    set plane vis_img_plane_$ren
    set map   vis_img_map_$ren
    set actor vis_img_actor_$ren
    set lookupGrayscale vis_img_g8bitGrayscaleLUT_$ren
    set lookupColor vis_img_gBlueToRedLUT_$ren

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

    $txt SetInputDataObject $vimg
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
    $lookupGrayscale SetTableRange [lindex $irng 0] [lindex $irng 1]
    $lookupColor SetTableRange [lindex $irng 0] [lindex $irng 1]

    #$actor Update
    $ren AddActor $actor
    #$ren ResetCamera
    vis_render $ren
}


# -----------
# vis_imgShow
# -----------
# Note that Renderer::AddActor should only add an actor if it is not
# already present.

proc vis_imgShow {ren} {

    set actor vis_img_actor_$ren

    if [cmdExists $actor] {
	return
    }

    $ren AddActor $actor
    $ren ResetCamera
    vis_render $ren
}


# -------------
# vis_imgUnshow
# -------------

proc vis_imgUnshow {ren} {

    set actor vis_img_actor_$ren

    if {![cmdExists $actor]} {
	return
    }

    if {[[$ren GetActors] IsItemPresent $actor] > 0} {
	$ren RemoveActor $actor
    }
    vis_render $ren
}


# ---------
# vis_imgRm
# ---------
# Delete entire pipeline.

proc vis_imgRm {ren} {

    set cmds [info commands vis_img_*_$ren]
    foreach c $cmds {
	if {[[$ren GetActors] IsItemPresent $c] > 0} {
	    $ren RemoveActor $c
	}
	$c Delete
    }

    vis_render $ren
    return
}
