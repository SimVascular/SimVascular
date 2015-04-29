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

# -------------
# vis_nodeSetup
# -------------

proc vis_nodeSetup {ren} {

    set vs vis_node_static

    if {[cmdExists $vs\_actor_$ren]} {
	return
    }

    catch {vis_nodeRmAll $ren}

    # Set up a pipeline:

    vtkSphereSource $vs\_sphere_$ren
    $vs\_sphere_$ren SetRadius 0.1
    $vs\_sphere_$ren Update
    
    vtkAppendPolyData $vs\_append_$ren

    vtkGlyph3D $vs\_glyph_$ren
    $vs\_glyph_$ren SetScaleModeToDataScalingOff
    $vs\_glyph_$ren SetScaleFactor 1.0

    $vs\_glyph_$ren SetSourceConnection \
	[$vs\_sphere_$ren GetOutputPort]

    # creating a transform always set to zero seems unnecessary
    vtkTransform $vs\_transform_$ren
    $vs\_transform_$ren Translate 0 0 0

    vtkTransformPolyDataFilter $vs\_transformFilter_$ren
    $vs\_transformFilter_$ren SetInputConnection \
	    [$vs\_glyph_$ren GetOutputPort]
    $vs\_transformFilter_$ren SetTransform \
	    $vs\_transform_$ren

    vtkPolyDataMapper $vs\_mapper_$ren
    $vs\_mapper_$ren SetScalarVisibility 0
    $vs\_mapper_$ren SetInputConnection \
	    [$vs\_transformFilter_$ren GetOutputPort]
    
    vtkActor $vs\_actor_$ren
    $vs\_actor_$ren SetMapper $vs\_mapper_$ren
    [$vs\_actor_$ren GetProperty] SetDiffuseColor 0.3 0.3 0.3

}


# --------------
# vis_nodeExists
# --------------

proc vis_nodeExists {ren} {
    set r vis_node_static_sphere_$ren
    if {[cmdExists $r]} {
	return 1
    }
    return 0
}

# ------------
# vis_nodeSize
# ------------

proc vis_nodeSize {ren r} {

    if {![cmdExists vis_node_static_sphere_$ren]} {
	return
    }

    vis_node_static_sphere_$ren SetRadius $r
    vis_render $ren

    return
}


# ----------------
# vis_nodeAddRepos
# ----------------

proc vis_nodeAddRepos {ren objName} {

    if {![repos_exists -obj $objName]} {
	return -code error "obj $objName does not exist in repository"
    }

    set vname [repos_exportToVtk -src $objName]
    vis_nodeAddVtk $ren $vname
}


# ---------------
# vis_nodeRmRepos
# ---------------

proc vis_nodeRmRepos {ren objName} {

    if {![repos_exists -obj $objName]} {
	return -code error "obj $objName does not exist in repository"
    }

    set vname [repos_exportToVtk -src $objName]
    vis_nodeRmVtk $ren $vname
}


# --------------
# vis_nodeAddVtk
# --------------

proc vis_nodeAddVtk {ren vtkName} {

    vis_nodeSetup $ren

    set app vis_node_static_append_$ren
    set gly vis_node_static_glyph_$ren
    set act vis_node_static_actor_$ren

    $app AddInputData $vtkName
    $app Update

    $gly SetInputConnection [$app GetOutputPort]
    $gly Update

    set actors [$ren GetActors]
    if {[$actors IsItemPresent $act] == 0} {
	$ren AddActor $act
    }
    vis_render $ren

    return
}


# -------------
# vis_nodeRmVtk
# -------------

proc vis_nodeRmVtk {ren vtkName} {

    vis_nodeSetup $ren

    set app vis_node_static_append_$ren
    set gly vis_node_static_glyph_$ren
    set act vis_node_static_actor_$ren

    catch {$app RemoveInput $vtkName}
    $app Update

    $gly Update

    set actors [$ren GetActors]
    if {[$actors IsItemPresent $act] == 0} {
	$ren AddActor $act
    }
    vis_render $ren

    return
}


# -----------
# vis_nodeClr
# -----------

proc vis_nodeClr {ren} {
    set act vis_node_static_actor_$ren
    set acts [$ren GetActors]
    if {[$acts IsItemPresent $act]} {
	$ren RemoveActor $act
    }
    vis_render $ren
}


# -------------
# vis_nodeRmAll
# -------------

proc vis_nodeRmAll {ren} {

    catch {vis_nodeClr $ren}

    set vs vis_node_static

    catch {$vs\_actor_$ren Delete}
    catch {$vs\_mapper_$ren Delete}
    catch {$vs\_transformFilter_$ren Delete}
    catch {$vs\_transform_$ren Delete}
    catch {$vs\_glyph_$ren Delete}
    catch {$vs\_append_$ren Delete}
    catch {$vs\_sphere_$ren Delete}
 
}


# ----------------
# vis_nodeSetColor
# ----------------

proc vis_nodeSetColor {ren r g b} {
    set act vis_node_static_actor_$ren
    if {[cmdExists $act]} {
	[$act GetProperty] SetDiffuseColor $r $g $b
	vis_render $ren
    }
}	


# ------------------------
# vis_nodeShowOriginMarker
# ------------------------

proc vis_nodeShowOriginMarker {ren r} {

    set sph vis_node_origin_marker_sphere_$ren
    set map vis_node_origin_marker_mapper_$ren
    set act vis_node_origin_marker_actor_$ren

    if {[cmdExists $sph]} {
	$sph SetRadius $r
        $sph Update
	$ren AddActor $act
	vis_render $ren
	return
    }

    vtkSphereSource $sph
    $sph SetRadius $r
    $sph SetCenter 0 0 0
    $sph Update

    vtkPolyDataMapper $map
    $map SetInputDataObject [$sph GetOutput]

    vtkActor $act
    $act SetMapper $map

    $ren AddActor $act
    vis_render $ren
}


# ----------------------
# vis_nodeRmOriginMarker
# ----------------------

proc vis_nodeRmOriginMarker {ren} {

    set act vis_node_origin_marker_actor_$ren

    if {[[$ren GetActors] IsItemPresent $act] > 0} {
	$ren RemoveActor $act
    }
    vis_render $ren
}
