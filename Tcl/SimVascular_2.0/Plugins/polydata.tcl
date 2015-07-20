#===========================================================================
#    
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
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
#===========================================================================    

# Procedure: guiMMextractBoundaries
proc guiMMextractBoundaries {} {
  global gOptions
  global smasherInputName
  global guiMMvars
  global guiPDvars

  set extraction_angle $guiPDvars(angleForBoundaries)
  if {$smasherInputName == "none"} {
    tk_messageBox -title "Load Solid Model Please"  -type ok -message "WARNING: You need to load the solid before extracting faces"
    return
  }

  if {$gOptions(meshing_solid_kernel) == "PolyData"} {
    global gPolyDataFaceNames
    global gPolyDataFaceNamesInfo
    catch {unset gPolyDataFaceNames}
    catch {unset gPolyDataFaceNamesInfo}

    puts "Running Boundary Extraction Filter..."
    $smasherInputName GetBoundaryFaces -angle $extraction_angle
    puts "Got Boundaries"

    set allids [$smasherInputName GetFaceIds]
    foreach id $allids {
      set gPolyDataFaceNames($id) "noname_$id"
    }
  } else {
    return -code error "Solid Must be of type PolyData"
  } 

  crd_ren gRenWin_3D_ren1
  smasherGUIupdateViewWindow
}

proc CombinePolyDataFaces {solid faceid1 faceid2} {
  global gOptions
  global gObjects

  set kernel $gOptions(meshing_solid_kernel)

  if {$kernel == "PolyData"} {
    $solid CombineFaces -targetface $faceid1 -loseface $faceid2
  } else {
    return -code error "Invalid solid kernel ($gOptions(meshing_solid_kernel))"
  }
}

proc PolyDataCombine {} {
  global gPolyDataFaceNames
  global smasherInputName
  global guiPDvars
  global gObjects

  set solid $gObjects(polydata_solid)

  set iter 0
  foreach name $guiPDvars(selected_groups) {
    set faceIds [$smasherInputName GetFaceIds]
    set two {-1}
    foreach faceid $faceIds {
      if {$gPolyDataFaceNames($faceid) == $name} { 
	puts "Face Id # $faceid"
	puts "Name: $gPolyDataFaceNames($faceid)"
        if {$iter == 0} {
          set one $faceid
        } else {
	  set two $faceid
	}
      }
    }
    if {$iter > 0 && $two != {-1}} {
      CombinePolyDataFaces $one $two
    }
    incr iter
  }

  set guiPDvars(selected_groups) {}
  crd_ren gRenWin_3D_ren1
  set smasherInputName $solid
  smasherGUIupdateViewWindow
}

proc PolyDataDeleteCells {} {
  global CurrentRenderer
  global gRenWin_3D_vars
  global gNumPickedCells
  global gPickedCellIds
  global guiPDvars
  global gOptions
  global gObjects
  global gui3Dvars
  global smasherInputName
  global symbolicName
  global model
  global gKernel

  set tv $symbolicName(guiSV_model_tree)

  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "error: one model can be selected to delete cells"
  }
  set kernel $gKernel($model)
  if {$kernel != "PolyData"} {
    return -code error "ERROR: Solid kernel must be PolyData for operation"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel
  if {[lindex [$tv item .models.$kernel.$model -values] 0] != "X"} {
    return -code error "error: model must be visualized as full model to delete cells"
  }

  set deleteList {}
  for {set i 1} {$i <= $gNumPickedCells} {incr i} {
    lappend deleteList $gPickedCellIds($i)
  }

  if {$gNumPickedCells != 0} {
    set modelpd /models/$kernel/$model   

    if {[repos_exists -obj $modelpd] == 0} {
      $model GetPolyData -result $modelpd
    }

    guiSV_model_add_to_backup_list $kernel $model

    set delete 1
    PickPolyDataCell widget x y add $delete
    $model DeleteFaces -faces $deleteList

    guiSV_model_update_tree
    guiSV_model_update_view_model $kernel $model

  } else {
    return -code error "No Cells selected to Delete"
  }
}

proc PolyDataFillHoles {} {
  global CurrentRenderer
  global gRenWin_3D_vars
  global gNumPickedCells
  global gPickedCellIds
  global gPolyDataFaceNames
  global gObjects
  global guiPDvars
  global gui3Dvars

  set openpd /tmp/polydata/open
  set closedpd /tmp/polydata/closed
  catch {repos_delete -obj $openpd}
  catch {repos_delete -obj $closedpd}

  set solid $gObjects(polydata_solid)
  $solid GetPolyData -result $openpd

  geom_fillHoles $openpd $closedpd

  $solid SetVtkPolyData -obj $closedpd
  if {[array size gPolyDataFaceNames] != 0 } { 
    unset gPolyDataFaceNames
  }

  crd_ren gRenWin_3D_ren1
  guiMMloadPolyData $solid
}

proc PolyDataRemeshSurfaces {solid excludelist} {
  global gOptions
  global gObjects
  global guiPDvars
  global symbolicName
  global gPolyDataFaceNames
  global smasherInputName
  
  set kernel $gOptions(meshing_solid_kernel)
  set size $guiPDvars(remeshFaceSize)

  if {[llength $excludelist] == 0} {
    lappend excludelist 0
  }
  $solid RemeshFace -excludelist $excludelist -size $size

  set guiPDvars(selected_groups) {}
}

proc PolyDataDeleteRegions {solid deletelist} {
  global gOptions
  global gObjects
  global guiPDvars
  global symbolicName
  global gPolyDataFaceNames
  
  set kernel $gOptions(meshing_solid_kernel)

  if {$kernel != "PolyData"} {
    return -code error "Invalid solid kernel ($gOptions(meshing_solid_kernel)), must be PolyData"
  }

  foreach id $deletelist {
    $solid DeleteRegion -regionid $id
  }
}

proc return_pd_region_names {} {
  global smasherInputName
  global gOptions
  global gObjects
  global gPolyDataFaceNames
  
  set kernel $gOptions(meshing_solid_kernel)
  set solid $smasherInputName
  set gObjects(polydata_solid) $smasherInputName

  if {$smasherInputName == "none"} {
    tk_messageBox -title "Load Solid Model Please"  -type ok -message "WARNING: You need to load the solid and extract the boundaries before getting regions"
    return
  }

  set faceIds [$solid GetFaceIds]

  if {$kernel == "PolyData"} {
    foreach faceid $faceIds {
      lappend names $gPolyDataFaceNames($faceid)
    }
  } else {
    return -code error "Invalid solid kernel ($gOptions(meshing_solid_kernel))"
  }
    return $names
}

proc PolyDataThreshold {solid} {
  global gOptions
  global gObjects
  global gActorPicked
  global gPolyDataFaceNames
  global symbolicName
  global guiPDvars

  set gActorPicked 0

  tk_messageBox -message "Select the wall surface of the model. To do this, place cursor over surface in 3D Window and press 'p'." -icon info -title "Select Surface to Keep"
  vwait gActorPicked

  set facename ""
  set facename [$symbolicName(smasherAttNameLabel) get]
  if {$facename == ""} {
    return -code error "ERROR: Must select a face to keep from threshold"
  }

  set geom /tmp/polydata/extract
  set wall /tmp/polydata/wall
  set thresholder tmp-thresholder
  set surfacer tmp-surfacer
  catch {repos_delete -obj $geom}
  catch {repos_delete -obj $wall}
  catch {$thresholder Delete}
  catch {$surfacer Delete}

  set wallid {-1}
  set faceids [$solid GetFaceIds]
  foreach id $faceids {
    if {$gPolyDataFaceNames($id) == $facename} {
      set wallid $id
    }
  }
  set guiPDvars(wall_id) $wallid

  $solid GetPolyData -result $geom 

  vtkThreshold $thresholder
  $thresholder SetInputData [repos_exportToVtk -src $geom]
  $thresholder SetInputArrayToProcess 0 0 0 1 "ModelFaceID" 
  $thresholder ThresholdBetween $wallid $wallid
  $thresholder Update

  vtkDataSetSurfaceFilter $surfacer 
  $surfacer SetInputData [$thresholder GetOutput]
  $surfacer Update

  repos_importVtkPd -src [$surfacer GetOutput] -dst $wall

  $solid SetVtkPolyData -obj $wall
#  unset gPolyDataFaceNames

#  crd_ren gRenWin_3D_ren1
#  guiMMloadPolyData $solid
}

proc PolyDataVMTKGetCenterIds {obj objType} {
  global gOptions
  global gObjects
  global gPolyDataFaceNames
  global gCenterlineIds

  set kernel $gOptions(meshing_solid_kernel)

  if {$kernel != "PolyData"} {
    return -code error "ERROR: invalid solid kernel $kernel"
  }

  set polydata /tmp/polydata/precap
  set cappedgeom /tmp/polydata/postcap
  catch {repos_delete -obj $polydata}
  catch {repos_delete -obj $cappedgeom}

  if {$objType == "solid"} {
    $obj GetPolyData -result $polydata 
  } elseif {$objType == "mesh"} {
    $obj GetSolid -result $polydata 
  } 

  set gCenterlineIds [geom_cap -src $polydata -result $cappedgeom -captype 1] 

  return $cappedgeom
}

proc PolyDataVMTKCenterlines {polydata original objType} {
  global gOptions
  global gObjects
  global gPolyDataFaceNames
  global gCenterlineIds
  global guiPDvars

  set kernel $gOptions(meshing_solid_kernel)

  if {$kernel != "PolyData"} {
    return -code error "ERROR: invalid solid kernel $kernel"
  }

  set originalsolid /tmp/polydata/originalsolid
  set centerlines [string trim $original]_centerlines
  set voronoi /tmp/polydata/voronoi
  set distance /tmp/polydata/distance
  catch {repos_delete -obj $originalsolid}
  catch {repos_delete -obj $centerlines}
  catch {repos_delete -obj $voronoi}
  catch {repos_delete -obj $distance}
  set guiPDvars(centerlines) $centerlines

  if {$objType == "solid"} {
    $original GetPolyData -result $originalsolid
  } elseif {$objType == "mesh"} {
    $original GetSolid -result $originalsolid
  }

  geom_centerlines -src $polydata -sourcelist [lindex $gCenterlineIds 0] -targetlist [lrange $gCenterlineIds 1 [llength $gCenterlineIds]] -linesresult $centerlines -voronoiresult $voronoi 

  geom_distancetocenterlines -src $originalsolid -lines $centerlines -result $distance

  return [list $distance $centerlines]
}

proc set_facenames_as_groupnames {solid names withCaps} {

  global gPolyDataFaceNames
  global gPolyDataFaceNamesInfo

  catch {unset gPolyDataFaceNames}
  catch {unset gPolyDataFaceNamesInfo}

  if {$withCaps} {
    set pdsrc /guiBOOLEAN/capsolid
    set pdresult /guiBOOLEAN/capnewsolid

    catch {repos_delete -obj $pdsrc}
    catch {repos_delete -obj $pdresult}

    $solid GetPolyData -result $pdsrc

    set twocapface [geom_set_ids_for_caps -src $pdsrc -result $pdresult]
    $solid SetVtkPolyData -obj $pdresult
  }


  set numnames [llength $names]
  for {set i 1} {$i <= $numnames} {incr i} {
    set gPolyDataFaceNames($i) "wall_[lindex $names [expr $i-1]]"
    if {$withCaps} {
      set gPolyDataFaceNames([expr $i + $numnames]) "cap_[lindex $names [expr $i-1]]"
      set twocapval [lindex $twocapface [expr $i-1]]
      if {$twocapval != 0} {
        set gPolyDataFaceNames([expr 2*$numnames+$twocapval]) "cap_[lindex $names [expr $i-1]]_2"
      }
    }
  }

}

proc set_capids_for_pd {pd {value -1}} {
  set capArray $pd/caparray
  catch {$capArray Delete}
  vtkIntArray $capArray
  $capArray SetName "CapID"
  for {set i 0} {$i < [[repos_exportToVtk -src $pd] GetNumberOfCells]} {incr i} {
    $capArray InsertNextValue $value
  }
  [[repos_exportToVtk -src $pd] GetCellData] AddArray $capArray
}

proc check_surface_for_capids {pd} {

  return [[[repos_exportToVtk -src $pd] GetCellData] HasArray "CapID"]
  
}
