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

# ------------
# vis_vGetName
# ------------

proc vis_vGetName {act} {
    #@author Nathan Wilson
    #@c Returns the tag used to create the actor.
    #@a act: actor.
    #@r tag
    return [string range $act 10 end]
}


# -------------
# vis_vGetActor
# -------------

proc vis_vGetActor {ren objName} {
    #@author Nathan Wilson
    #@c Returns the actor for the given obj name.
    #@a ren: vtkRenderer.
    #@a objName: repository object name.
    #@r actor

    set tag [format "%s_%s" $ren $objName]
    set act vis_v_act_$tag
    if {[cmdExists $act]} {
	return $act
    }
    return ""
}


# -------
# vis_vRm
# -------

proc vis_vRm {ren objName} {
    #@author Nathan Wilson
    #@c Removes objName from the given Renderer.
    #@a ren:  vtkRenderer.
    #@a objName:  Displayed repository object name.
    #@r status
    set tag [format "%s_%s" $ren $objName]

    set hog vis_v_hog_$tag
    set map vis_v_map_$tag
    set act vis_v_act_$tag
    set cone vis_v_cone_$tag
    set glyph vis_v_glyph_$tag
    set transform vis_v_transform_$tag
    set transformF vis_v_transformF_$tag

    global PickedAssembly
    if {$act == $PickedAssembly} {
      DeselectPickedActor
    }
    if {[[$ren GetActors] IsItemPresent $act] > 0} {
      $ren RemoveActor $act
    }
    catch {$hog Delete}
    catch {$map Delete}
    catch {$act Delete}
    catch {$cone Delete}
    catch {$glyph Delete}
    catch {$transform Delete}
    catch {$transformF Delete}

    vis_render $ren
    return SV_OK
}


# ----------
# vis_vRmAll
# ----------

proc vis_vRmAll {ren} {
    #@author Nathan Wilson
    #@c Removes all displayed vector actors from the given Renderer.
    #@a ren:  vtkRenderer.
    #@r status
    set tag [format "%s_%s" $ren *]
    set cmds [info commands vis_v_act_$tag]

    foreach c $cmds {
      global PickedAssembly
      if {$c == $PickedAssembly} {
        DeselectPickedActor
      }
      if {[[$ren GetActors] IsItemPresent $c] > 0} {
        $ren RemoveActor $c
        set tag [vis_vGetName $c]
        set hog vis_v_hog_$tag
        set map vis_v_map_$tag
        set act vis_v_act_$tag
        set cone vis_v_cone_$tag
        set glyph vis_v_glyph_$tag
        set transform vis_v_transform_$tag
        set transformF vis_v_transformF_$tag
        catch {$hog Delete}
        catch {$map Delete}
        catch {$act Delete}
        catch {$cone Delete}
        catch {$glyph Delete}
        catch {$transform Delete}
        catch {$transformF Delete}
      }
    }

    vis_render $ren
    return SV_OK
}


# ----------
# vis_vRepos
# ----------

proc vis_vRepos {ren objName args} {

    #@author Nathan Wilson
    #@c Display vectors of repository object objName in ren.
    #@a ren: vtkRenderer.
    #@a objName:  Object to display.
    #@a args:  Optional arguments.  Possible values are:&p
    #@a args:  scale s&p
    #@a args:  color r g b&p
    #@a args:  scalarsVisible&p
    #@a args:  scalarsInvisible&p
    #@r Newly created actor name.&p
    set tag [format "%s_%s" $ren $objName]

    set hog vis_v_hog_$tag
    set map vis_v_map_$tag
    set act vis_v_act_$tag
    set cone vis_v_cone_$tag
    set glyph vis_v_glyph_$tag
    set transform vis_v_transform_$tag
    set transformF vis_v_transformF_$tag

    if {[cmdExists $act]} {
        vis_vRm $ren $objName
    }

    set vtkName [repos_exportToVtk -src $objName]

    # it appears there is a bug in vtkHedgeHog which
    # requires that if the scalars exist they must have
    # the same number of values as the vectors.  Here we
    # check for this condition and return an error to avoid
    # the seg fault.
    set scobj [[$vtkName GetPointData] GetScalars]
    if {$scobj != ""} {
      set nvec [[[$vtkName GetPointData] GetVectors] GetNumberOfTuples]
      set nsca [[[$vtkName GetPointData] GetScalars] GetNumberOfTuples]
      if {$nvec != $nsca} {
        return -code error "Number of vectors ($nvec) does not match number of scalars ($nsca)."
      }
    }

    # default params
    set color {0 0 1}
    set scale 1.0
    set scalarVis ScalarVisibilityOff

    # override defaults
    foreach i $args {
      if {[lindex $i 0] == "scale"} {
         set scale [lindex $i 1]
      }
      if {[lindex $i 0] == "color"} {
         set color [lrange $i 1 3]
      }
      if {[lindex $i 0] == "scalarsVisible"} {
         puts "scalars on"
         set scalarVis ScalarVisibilityOn
      }
      if {[lindex $i 0] == "scalarsInvisible"} {
         set scalarVis ScalarVisibilityOff
      }
    }

    global gOptions
    if {$gOptions(vis_vectors_with_glyphs) == 1} {
      vtkConeSource $cone
      $cone SetResolution 6
      vtkTransform $transform
      $transform Translate 0.5 0.0 0.0
      vtkTransformPolyDataFilter $transformF
      $transformF SetInputDataObject [$cone GetOutput]
      $transformF SetTransform $transform
      vtkGlyph3D $glyph
      $glyph SetInputDataObject $vtkName
      $glyph SetSource [$transformF GetOutput]
      $glyph SetVectorModeToUseVector
      $glyph SetScaleFactor $scale
    } else {
      vtkHedgeHog $hog
      $hog SetInputDataObject $vtkName
      $hog SetScaleFactor $scale
      $hog Update
    }

    vtkPolyDataMapper $map
    $map $scalarVis
    if {$gOptions(vis_vectors_with_glyphs) == 1} {
      $map SetInputDataObject [$glyph GetOutput]
    } else {
      $map SetInputDataObject [$hog GetOutput]
    }
    $map Update

    vtkActor $act
    $act SetMapper $map
    [$act GetProperty] SetColor [lindex $color 0] [lindex $color 1] \
                                [lindex $color 2]
    $ren AddActor $act
    vis_render $ren
    return $act
}


# --------------
# vis_vSetColor
# --------------

proc vis_vSetColor {ren objName r g b} {
    #@author Nathan Wilson
    #@c Sets the color of objName in the given Renderer.
    #@a ren:  vtkRenderer.
    #@a objName:  Displayed repository object name.
    #@a r: red
    #@a g: green
    #@a b: blue
    #@note Color values range from 0 to 1.
    #@r status
    set tag [format "%s_%s" $ren $objName]

    set act vis_v_act_$tag

    if {[cmdExists $act]} {
	[$act GetProperty] SetDiffuseColor $r $g $b
	vis_render $ren
    }
    return SV_OK
}


# -------------
# vis_vSetScale
# -------------

proc vis_vSetScale {ren objName s} {
    #@author Nathan Wilson
    #@c Sets the scale for objName in the given Renderer.
    #@a ren:  vtkRenderer.
    #@a objName:  Displayed repository object name.
    #@a s: scale value.
    #@r status
    set tag [format "%s_%s" $ren $objName]
    set hog vis_v_hog_$tag
    set glyph vis_v_glyph_$tag

    global gOptions
    if {$gOptions(vis_vectors_with_glyphs) == 1} {
      set obj $glyph
    } else {
      set obj $hog
    }
    if {[cmdExists $obj]} {
	$obj SetScaleFactor $s
	vis_render $ren
    }
    return SV_OK
}


# ---------------------
# vis_vColorWithScalars
# ---------------------

proc vis_vColorWithScalars {ren objName} {
    #@author Nathan Wilson
    #@c Use scalars to color objName in the given Renderer.
    #@a ren:  vtkRenderer.
    #@a objName:  Displayed repository object name.
    #@r status
    set tag [format "%s_%s" $ren $objName]

    set map vis_v_map_$tag

    if {[cmdExists $map]} {
	$map ScalarVisibilityOn
	vis_render $ren
    }
    return SV_OK
}


# ------------------------
# vis_vColorWithoutScalars
# ------------------------

proc vis_vColorWithoutScalars {ren objName} {
    #@author Nathan Wilson
    #@c Do not use scalars to color objName in the given Renderer.
    #@a ren:  vtkRenderer.
    #@a objName:  Displayed repository object name.
    #@r status
    set tag [format "%s_%s" $ren $objName]

    set map vis_v_map_$tag

    if {[cmdExists $map]} {
	$map ScalarVisibilityOff
	vis_render $ren
    }
    return SV_OK
}
