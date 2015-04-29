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

# ----------
# vis_pRepos
# ----------
# Grab a given vtkPolyData object from the repository, and render it in the given rendering window.

proc vis_pRepos {ren objName} {

    if {![repos_exists -obj $objName]} {
	return -code error "$objName not in repository"
    }

    if {[repos_type -obj $objName] != "PolyData"} {
	return -code error "$objName not of type PolyData"
    }

    set tag [format "%s_%s" $ren $objName]

    set map vis_p_map_$tag
    set act vis_p_act_$tag

    # if the obj is already displayed, remove it first
    # then redisplay
    if [catch {vis_register $ren $objName}] {
       vis_pRm $ren $objName
       vis_register $ren $objName
    }

    set vtkName [repos_exportToVtk -src $objName]

    vtkPolyDataMapper $map
    $map SetInputDataObject $vtkName
    $map ScalarVisibilityOff

    vtkActor $act
    $act SetMapper $map
    [$act GetProperty] SetColor 1 0 0

    vis_renAddActor $ren $act

    vis_render $ren

    return $act
}

proc vis_pReposColor {ren objName color} {

    if {![repos_exists -obj $objName]} {
	return -code error "$objName not in repository"
    }

    if {[repos_type -obj $objName] != "PolyData"} {
	return -code error "$objName not of type PolyData"
    }

    set tag [format "%s_%s" $ren $objName]

    set map vis_p_map_$tag
    set act vis_p_act_$tag

    # if the obj is already displayed, remove it first
    # then redisplay
    if [catch {vis_register $ren $objName}] {
       vis_pRm $ren $objName
       vis_register $ren $objName
    }

    set vtkName [repos_exportToVtk -src $objName]

    vtkPolyDataMapper $map
    $map SetInputDataObject $vtkName
    $map ScalarVisibilityOff

    vtkActor $act
    $act SetMapper $map
    [$act GetProperty] SetColor 

    vis_renAddActor $ren $act

    vis_render $ren

    return $act
}

# ----------
# vis_pReposForUpdate
# ----------
# Grab a given vtkPolyData object from the repository, and render it in the given rendering window.
# And then return the actor and mapper

proc vis_pReposForUpdate {ren objName color opacity} {

    if {![repos_exists -obj $objName]} {
	return -code error "$objName not in repository"
    }

    if {[repos_type -obj $objName] != "PolyData"} {
	return -code error "$objName not of type PolyData"
    }

    set tag [format "%s_%s" $ren $objName]

    set map vis_p_map_$tag
    set act vis_p_act_$tag

    # if the obj is already displayed, remove it first
    # then redisplay
    if [catch {vis_register $ren $objName}] {
       vis_pRm $ren $objName
       vis_register $ren $objName
    }

    set vtkName [repos_exportToVtk -src $objName]

    vtkPolyDataMapper $map
    $map SetInputData $vtkName
    $map ScalarVisibilityOff

    vtkActor $act
    $act SetMapper $map
    [$act GetProperty] SetColor [lindex $color 0] [lindex $color 1] [lindex $color 2]
    [$act GetProperty] SetOpacity $opacity

    vis_renAddActor $ren $act

    vis_render $ren

    return [list $act $map]
}

# ----------
# vis_pExists
# ----------

proc vis_pExists {ren objName} {

  set exists 0
  set tag [format "%s_%s" $ren $objName]
  set act vis_p_act_$tag
  if {[cmdExists $act]} {
    set exists 1
  }
  return $exists
}


# ----------
# vis_pCurve
# ----------
# Places a glyph at the first point of the curve.

proc vis_pCurve {ren objName} {

    if {[catch {vis_pRepos $ren $objName} msg]} {
	return -code error $msg
    }

    set tag [format "%s_%s" $ren $objName]

    set map vis_p_map_$tag
    set act vis_p_act_$tag
    set pts vis_p_pts_$tag
    set vts vis_p_vts_$tag
    set ppd vis_p_ppd_$tag
    set vca vis_p_vca_$tag

    if {[cmdExists $ppd]} {
	return $act
    }

    # We have to do the following instead of just taking point 0
    # because the pick operation can create a PolyData with
    # unconnected points (i.e. the picked region is connected, but
    # other points remain).  So we don't know that we want the first
    # point in the global point list.

    set pd [repos_exportToVtk -src $objName]
    set id [[[$pd GetLines] GetData] GetValue 1]
    set pos [[$pd GetPoints] GetPoint $id]


    catch {$vts Delete}
    vtkVertex $vts
#    eval $vts SetPoint $pos    

    catch {$pts Delete}    
    vtkPoints $pts
    eval $pts InsertNextPoint $pos

    catch {$vca Delete}
    vtkCellArray $vca
    $vca InsertNextCell $vts

    vtkPolyData $ppd
    $ppd SetPoints $pts
    $ppd SetVerts $vca

    vis_nodeAddVtk $ren $ppd

    if {[cmdExists $map] || [cmdExists $act]} {
	#$act Update
	vis_render $ren
	return $act
    }

    vis_render $ren
    return $act
}


# ---------
# vis_pEdge
# ---------

proc vis_pEdge {ren objName} {

    set tag [format "%s_%s" $ren $objName]

    set map vis_p_map_$tag
    set act vis_p_act_$tag
    set edg vis_p_edg_$tag

    if {![cmdExists $map]} {
	return -code error "$objName not currently being displayed in $ren"
    }

    if {![cmdExists $edg]} {
	vtkExtractEdges $edg
	$edg SetInputDataObject [$map GetInput]
	$map SetInputDataObject [$edg GetOutput]
    }

    return $act
}


# ---------
# vis_pSurf
# ---------

proc vis_pSurf {ren objName} {

    set tag [format "%s_%s" $ren $objName]

    set map vis_p_map_$tag
    set act vis_p_act_$tag
    set edg vis_p_edg_$tag

    if {![cmdExists $map]} {
	return -code error "$objName not currently being displayed in $ren"
    }

    if {![cmdExists $edg]} {
	return
    }

    $map SetInputDataObject [$edg GetInput]
    $edg Delete

    return $act
}


# ---------
# vis_pNorm
# ---------
# Add vtkPolyDataNormals to the PolyData viewing pipeline.  Note that
# there is no proc to perform the reverse operation (i.e. remove the
# normals).  This is because the caller can presumably just turn off
# normal-based properties via the actor's vtkProperty.

proc vis_pNorm {ren objName} {

    set tag [format "%s_%s" $ren $objName]

    set map vis_p_map_$tag
    set act vis_p_act_$tag
    set nrm vis_p_nrm_$tag

    if {![cmdExists $map]} {
	return -code error "$objName not currently being displayed in $ren"
    }

    if {![cmdExists $nrm]} {
	vtkPolyDataNormals $nrm
	$nrm SetInputDataObject [$map GetInput]
	$map SetInputDataObject [$nrm GetOutput]
    }
    $nrm Update
    return $act
}


# ------------
# vis_pGetName
# ------------

proc vis_pGetName {ren act} {
    set rlen [string length $ren]
    set startIx [expr 10 + $rlen + 1]
    return [string range $act $startIx end]
}


# -------------
# vis_pGetActor
# -------------

proc vis_pGetActor {ren objName} {
    set tag [format "%s_%s" $ren $objName]
    set act vis_p_act_$tag
    if {[cmdExists $act]} {
	return $act
    }
    return ""
}


# -----------------------
# vis_pGetDisplayedActors
# -----------------------
# Returns object names corresponding to all objects currently
# displayed in the given renderer which were posted via the "vis_p"
# set of functions.

proc vis_pGetDisplayedActors {ren} {
    set num [[$ren GetActors] GetNumberOfItems]
    set prefix [format "vis_p_act_%s_" $ren]
    set ix [string length $prefix]
    set objs {}
    for {set i 0} {$i < $num} {incr i} {
	set a [[$ren GetActors] GetItemAsObject $i]
	if {[string first $prefix $a] == 0} {
	    set obj [string range $a $ix end]
	    set objs [lappend objs $obj]
	}
    }
    return $objs
}


# -------
# vis_pRm
# -------

proc vis_pRm {ren objName} {

    set tag [format "%s_%s" $ren $objName]
    set ppd vis_p_ppd_$tag

    set cmds [info commands vis_p_*_$tag]
    foreach c $cmds {
        # before sure to delete actor if its picked
        global PickedAssembly
        if {$c == $PickedAssembly} {
           DeselectPickedActor
	}
	if {$c == $ppd} {
	    vis_nodeRmVtk $ren $ppd
	}
	if {[[$ren GetActors] IsItemPresent $c] > 0} {
	    vis_renRmActor $ren $c
	}
	$c Delete
    }

    vis_unregister $ren $objName
    vis_render $ren

}


# ----------
# vis_pRmAll
# ----------

proc vis_pRmAll {ren} {

    set glob_pat [format "vis_p_ppd_%s_*" $ren]
    set cmds [info commands $glob_pat]
    foreach c $cmds {
        global PickedAssembly
        if {$c == $PickedAssembly} {
          DeselectPickedActor
	}
	catch {vis_nodeRmVtk $ren $c}
    }

    set glob_pat [format "vis_p_???_%s_*" $ren]
    set cmds [info commands $glob_pat]
    foreach c $cmds {
	if {[[$ren GetActors] IsItemPresent $c] > 0} {
	    $ren RemoveActor $c
	}
	$c Delete
    }
    vis_render $ren
}
