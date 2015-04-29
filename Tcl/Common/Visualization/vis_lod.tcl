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
# vis_lodRepos
# ------------

proc vis_lodRepos {ren objName} {

    if {![repos_exists -obj $objName]} {
	return -code error "$objName not in repository"
    }

    if {[repos_type -obj $objName] != "PolyData"} {
	return -code error "$objName not of type PolyData"
    }

    set tag [format "%s_%s" $ren $objName]

    set mapLow vis_lod_maplow_$tag
    set mapHigh vis_lod_maphigh_$tag
    set act vis_lod_act_$tag
    set propLow vis_lod_prop_$tag
    set propHigh vis_lod_prop_$tag

    global gOptions
    set max_polys $gOptions(vis_lod_max_num_polys)

    if {$max_polys == ""} {
       return -code error "ERROR:  Must specific gOptions(vis_lod_max_polys)"
    }

    # this is probably wrong
    if {[cmdExists $act]} {
	vis_lodRm $ren $objName
    }

    set vtkName [repos_exportToVtk -src $objName]

    # we need to create a decimated version
    if {[$vtkName GetNumberOfPolys] >= $max_polys} {
      set decimator /tmp/vis_lRepos/decimator
      catch {$decimator Delete}
      vtkDecimatePro $decimator
      $decimator AccumulateErrorOff
      $decimator SetInputDataObject $vtkName
      $decimator SetTargetReduction [expr 1.0-double($max_polys)/double([$vtkName GetNumberOfPolys])]
      $decimator Update
      puts "num polys in decimated: [[$decimator GetOutput] GetNumberOfPolys]"
      vtkPolyDataMapper $mapLow
      $mapLow SetInputDataObject [$decimator GetOutput]
    } else {
      # no decimation needed
      vtkPolyDataMapper $mapLow
      $mapLow SetInputDataObject $vtkName
    }

    vtkPolyDataMapper $mapHigh
    $mapHigh SetInputDataObject $vtkName

    catch {$propLow Delete}
    vtkProperty $propLow
    $propLow SetColor 1 0 0
    $propLow SetOpacity 1.0
    $propLow SetAmbient 0.1
    $propLow SetDiffuse 0.9
    $propLow SetSpecular 0.2

    catch {$propHigh Delete}
    vtkProperty $propHigh
    $propHigh SetColor 1 0 0
    $propHigh SetOpacity 1.0
    $propHigh SetAmbient 0.1
    $propHigh SetDiffuse 0.9
    $propHigh SetSpecular 0.2

    vtkLODProp3D $act
    $act AddLOD $mapLow $propLow 0.0
    $act AddLOD $mapHigh $propHigh 0.0

    vis_renAddProp $ren $act

    vis_render $ren
    return $act
}


# ---------
# vis_lodRm
# ---------

proc vis_lodRm {ren objName} {

    set tag [format "%s_%s" $ren $objName]

    set mapLow vis_lod_maplow_$tag
    set mapHigh vis_lod_maphigh_$tag
    set act vis_lod_act_$tag
    set propLow vis_lod_prop_$tag
    set propHigh vis_lod_prop_$tag

    # remove from rendering window
    if {[[$ren GetViewProps] IsItemPresent $act] > 0} {
       vis_renRmProp $ren $act
       # delete all related objects
       foreach i [list $mapLow $mapHigh $act $propLow $propHigh] {
	 catch {$i Delete}
       }
   }
   vis_render $ren

}


# ---------------
# vis_lodSetColor
# ---------------

proc vis_lodSetColor {ren objName r g b} {

    set tag [format "%s_%s" $ren $objName]

    set mapLow vis_lod_maplow_$tag
    set mapHigh vis_lod_maphigh_$tag
    set act vis_lod_act_$tag
    set propLow vis_lod_prop_$tag
    set propHigh vis_lod_prop_$tag

    catch {$propLow SetColor $r $g $b}
    catch {$propHigh SetColor $r $g $b}

    vis_render $ren
    return

}


# -----------------
# vis_lodSetOpacity
# -----------------

proc vis_lodSetOpacity {ren objName opacity} {

    set tag [format "%s_%s" $ren $objName]

    set mapLow vis_lod_maplow_$tag
    set mapHigh vis_lod_maphigh_$tag
    set act vis_lod_act_$tag
    set propLow vis_lod_prop_$tag
    set propHigh vis_lod_prop_$tag

    catch {$propLow SetOpacity $opacity}
    catch {$propHigh SetOpacity $opacity}

    vis_render $ren
    return

}
