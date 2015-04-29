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

# -------------
# vis_warpSetup
# -------------

proc vis_warpSetup {ren} {

    if {[cmdExists vis_warp_geomFilter_$ren]} {
	return
    }

    vtkGeometryFilter vis_warp_geomFilter_$ren

    vtkWarpScalar vis_warp_warp_$ren
    vis_warp_warp_$ren SetInputDataObject [vis_warp_geomFilter_$ren GetOutput]

    vtkTransform vis_warp_transform_$ren
    vis_warp_transform_$ren Translate 0 0 0

    vtkTransformPolyDataFilter vis_warp_transformFilter_$ren
    vis_warp_transformFilter_$ren SetInputDataObject \
	    [vis_warp_warp_$ren GetPolyDataOutput]
    vis_warp_transformFilter_$ren SetTransform vis_warp_transform_$ren

    vtkPolyDataMapper vis_warp_mapper_$ren
    vis_warp_mapper_$ren SetInputDataObject [vis_warp_transformFilter_$ren GetOutput]
    vis_warp_mapper_$ren SetLookupTable g8bitGrayscaleLUT
    vis_warp_mapper_$ren ScalarVisibilityOff

    vtkActor vis_warp_actor_$ren
    vis_warp_actor_$ren SetMapper vis_warp_mapper_$ren
    [vis_warp_actor_$ren GetProperty] SetRepresentationToSurface
    [vis_warp_actor_$ren GetProperty] SetOpacity 0.5
    vis_warpSetColor $ren 0.2 0.2 0.2
}


# ----------------
# vis_warpSetColor
# ----------------

proc vis_warpSetColor {ren r g b} {

    if {![cmdExists vis_warp_geomFilter_$ren]} {
	return
    }

    [vis_warp_actor_$ren GetProperty] SetDiffuseColor $r $g $b
}


# ----------------
# vis_warpColorize
# ----------------

proc vis_warpColorize {ren} {

    if {![cmdExists vis_warp_mapper_$ren]} {
	return
    }
    foreach i [info commands vis_warp_mapper_$ren*] {
      $i ScalarVisibilityOn
      $i SetLookupTable gBlueToRedLUT
      $i Update
    }
    vis_render $ren
}


# ------------------
# vis_warpDecolorize
# ------------------

proc vis_warpDecolorize {ren} {

    if {![cmdExists vis_warp_mapper_$ren]} {
	return
    }

    vis_warp_mapper_$ren ScalarVisibilityOff
    vis_warp_mapper_$ren Update
}


# ------------------
# vis_warpSetOpacity
# ------------------

proc vis_warpSetOpacity {ren o} {

    if {![cmdExists vis_warp_geomFilter_$ren]} {
	return
    }
    foreach i [info commands vis_warp_actor_$ren*] {
      [$i GetProperty] SetOpacity $o
    }
}


# -------------------
# vis_warpScaleFactor
# -------------------

proc vis_warpScaleFactor {ren f} {

    if {![cmdExists vis_warp_geomFilter_$ren]} {
    	return
    }
    foreach i [info commands vis_warp_warp_$ren*] {
      $i SetScaleFactor $f
    }
    vis_render $ren
}


# ---------------
# vis_warpScalars
# ---------------

proc vis_warpScalars {ren flag} {

    if {![cmdExists vis_warp_actor_$ren]} {
	return
    }

    if {$flag == 0} {
	vis_warp_mapper_$ren ScalarVisibilityOff
    } elseif {$flag == 1} {
	vis_warp_actor_$ren ScalarVisibilityOn
    } else {
	puts "ERR: need boolean flag"
    }
}


# ------------
# vis_warpSurf
# ------------

proc vis_warpSurf {ren flag} {

    if {![cmdExists vis_warp_actor_$ren]} {
	return
    }

    if {$flag == 0} {
	[vis_warp_actor_$ren GetProperty] SetRepresentationToWireframe
    } elseif {$flag == 1} {
	[vis_warp_actor_$ren GetProperty] SetRepresentationToSurface
    } else {
	puts "ERR: need boolean flag"
    }
}


# -----------
# vis_warpClr
# -----------

proc vis_warpClr {ren} {
    #set act vis_warp_actor_$ren
    #set actors [$ren GetActors]
    #if {[$actors IsItemPresent $act]} {
    #	$ren RemoveActor $act
    #}
    set glob_pat [format "vis_warp_actor_%s_*" $ren]
    set cmds [info commands $glob_pat]
    foreach c $cmds {
	if {[[$ren GetActors] IsItemPresent $c] > 0} {
	    $ren RemoveActor $c
	}
	$c Delete
    }

    vis_render $ren
}


# ----------
# vis_warpRm
# ----------

proc vis_warpRm {ren} {
    if {![cmdExists vis_warp_geomFilter_$ren]} {
	return
    }
    vis_warpClr $ren
    set __vis_warp_roots [list \
	    vis_warp_geomFilter \
	    vis_warp_warp \
	    vis_warp_transform \
	    vis_warp_transformFilter \
	    vis_warp_mapper \
	    vis_warp_actor ]
    vis_objRmAll $ren __vis_warp_roots
    vis_render $ren
    return
}


# -----------------
# vis_warpTranslate
# -----------------
proc vis_warpTranslate {ren objName vec} {
      set t vis_warp_transform_$ren\_$objName
      if {[info commands $t] == ""} {
         return -code error "warp transform object for $objName does not exist!"
      }
      if {[llength $vec] != 3} {
         return -code error "Must specify a 3-d translation vector ($vec)."
      }
      vis_warp_transform_$ren\_$objName Translate [lindex $vec 0] [lindex $vec 1] [lindex $vec 2]
      vis_warp_transformFilter_$ren\_$objName Update
      vis_render $ren
}


# -----------------
# vis_warpShowRepos
# -----------------

proc vis_warpShowRepos {ren objName} {

    vis_warpSetup $ren

    if {![repos_exists -obj $objName]} {
	puts [format "ERR: %s not found in repository" $objName]
	return
    }

    set vtkName [repos_exportToVtk -src $objName]


    if {![cmdExists vis_warp_geomFilter_$ren\_$objName]} {

      vtkGeometryFilter vis_warp_geomFilter_$ren\_$objName

      vtkWarpScalar vis_warp_warp_$ren\_$objName
      vis_warp_warp_$ren\_$objName SetInputDataObject [vis_warp_geomFilter_$ren\_$objName GetOutput]

      vtkTransform vis_warp_transform_$ren\_$objName
      vis_warp_transform_$ren\_$objName Translate 0 0 0

      vtkTransformPolyDataFilter vis_warp_transformFilter_$ren\_$objName
      vis_warp_transformFilter_$ren\_$objName SetInputDataObject \
	    [vis_warp_warp_$ren\_$objName GetPolyDataOutput]
      vis_warp_transformFilter_$ren\_$objName SetTransform vis_warp_transform_$ren\_$objName

      vtkPolyDataMapper vis_warp_mapper_$ren\_$objName
      vis_warp_mapper_$ren\_$objName SetInputDataObject [vis_warp_transformFilter_$ren\_$objName GetOutput]
      vis_warp_mapper_$ren\_$objName SetLookupTable g8bitGrayscaleLUT
      vis_warp_mapper_$ren\_$objName ScalarVisibilityOff

      vtkActor vis_warp_actor_$ren\_$objName
      vis_warp_actor_$ren\_$objName SetMapper vis_warp_mapper_$ren\_$objName
      [vis_warp_actor_$ren\_$objName GetProperty] SetRepresentationToSurface
      [vis_warp_actor_$ren\_$objName GetProperty] SetOpacity 0.5
      vis_warpSetColor $ren 0.2 0.2 0.2

    }


    vis_warp_geomFilter_$ren\_$objName SetInputDataObject $vtkName

    vis_warp_mapper_$ren\_$objName Update
    set range [[vis_warp_mapper_$ren\_$objName GetInput] GetScalarRange]
    vis_warp_mapper_$ren\_$objName SetScalarRange [lindex $range 0] [lindex $range 1]

    set actors [$ren GetActors]
    set act vis_warp_actor_$ren\_$objName
    if {[$actors IsItemPresent $act] == 0} {
	$ren AddActor $act
    }

    vis_render $ren

    return $act

}
