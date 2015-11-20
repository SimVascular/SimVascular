# Procedure: guiSV_model_create_model_opencascade
proc guiSV_model_create_model_opencascade {} {
   global symbolicName
   global createPREOPgrpKeptSelections
   global gFilenames
   global gObjects
   global gLoftedSolids
   global gOptions

   set gOptions(meshing_solid_kernel) OpenCASCADE
   solid_setKernel -name $gOptions(meshing_solid_kernel)
   set kernel $gOptions(meshing_solid_kernel)
   set gOptions(meshing_solid_kernel) $kernel
   solid_setKernel -name $kernel

   set tv $symbolicName(guiSV_group_tree)
  set children [$tv children {}]

   if {[lsearch -exact $children .groups.all] >= 0} {
     set children [$tv children .groups.all]
     if {$children == ""} {
       return
     }
   }

   set createPREOPgrpKeptSelections {}
   puts "children: $children"

    foreach child $children {
      if {[lindex [$tv item $child -values] 0] == "X"} {
  lappend createPREOPgrpKeptSelections [string range $child 12 end]
      }
    }

   set modelname [guiSV_model_new_surface_name 0]
   catch {repos_delete -obj $modelname}
   if {[model_create $kernel $modelname] != 1} {
     guiSV_model_delete_model $kernel $modelname
     catch {repos_delete -obj /models/$kernel/$modelname}
     model_create $kernel $modelname
   }
   guiSV_model_update_tree

    #set modelname $gObjects(preop_solid)

   if {[llength $createPREOPgrpKeptSelections] == 0} {
      puts "No solid models selected.  Nothing done!"
      return
   }

   puts "Will union together the following SolidModel objects:"
   puts "  $createPREOPgrpKeptSelections"

   if {[repos_exists -obj $modelname] == 1} {
      puts "Warning:  object $modelname existed and is being replaced."
      repos_delete -obj $modelname
   }

    if {[repos_exists -obj /models/$kernel/$modelname] == 1} {
      repos_delete -obj /model/$kernel/$modelname
    }
    # create solids
    foreach i $createPREOPgrpKeptSelections {
      set cursolid ""
      catch {set cursolid $gLoftedSolids($i)}
      # loft solid from group
      global gPathBrowser
      set keepgrp $gPathBrowser(currGroupName)
      set gPathBrowser(currGroupName) $i
      #puts "align"
      #vis_img_SolidAlignProfiles;
      #puts "fit"
      #vis_img_SolidFitCurves;
      #puts "loft"
      #vis_img_SolidLoftSurf;
      #vis_img_SolidCapSurf;
      # set it back to original
      global gRen3dFreeze
      set oldFreeze $gRen3dFreeze
      set gRen3dFreeze 1
      makeSurfOCCT
      set gRen3dFreeze $oldFreeze
      set gPathBrowser(currGroupName) $keepgrp
    }

    set shortname [lindex $createPREOPgrpKeptSelections 0]
    set cursolid $gLoftedSolids($shortname)
    solid_copy -src $cursolid -dst $modelname
    puts "copy $cursolid to preop model."

    foreach i [lrange $createPREOPgrpKeptSelections 1 end] {
      set cursolid $gLoftedSolids($i)
      puts "union $cursolid into preop model."
      if {[repos_type -obj $cursolid] != "SolidModel"} {
         puts "Warning:  $cursolid is being ignored."
         continue
      }
       solid_union -result /tmp/preop/$modelname -a $cursolid -b $modelname

       repos_delete -obj $modelname
       solid_copy -src /tmp/preop/$modelname -dst $modelname

       repos_delete -obj /tmp/preop/$modelname
    }

    if {[repos_exists -obj /tmp/preop/$modelname] == 1} {
      repos_delete -obj /tmp/preop/$modelname
    }

    #global tcl_platform
    #if {$tcl_platform(os) == "Darwin"} {
    #  #Find face areas and remove two smaller ones
    #  set num [llength $createPREOPgrpKeptSelections]
    #  if { $num > 1} {
    #    guiSV_model_opencascade_fixup $modelname $num
    #  }
    #}

    global gOCCTFaceNames
    crd_ren gRenWin_3D_ren1
    set pretty_names {}
    set all_ids {}
    foreach i [$modelname GetFaceIds] {
      catch {set type [$modelname GetFaceAttr -attr gdscName -faceId $i]}
      catch {set parent [$modelname GetFaceAttr -attr parent -faceId $i]}
      set facename "[string trim $type]_[string trim $parent]"
      lappend pretty_names $facename
      set gOCCTFaceNames($i) $facename
      $modelname SetFaceAttr -attr gdscName -faceId $i -value $facename
      lappend all_ids $i
    }
    set isdups 0
    if {[llength [lsort -unique $pretty_names]] != [llength $pretty_names]} {
     set isdups 1
     set duplist [lsort -dictionary $pretty_names]
     foreach i [lsort -unique $pretty_names] {
        set idx [lsearch -exact $duplist $i]
        set duplist [lreplace $duplist $idx $idx]
     }
     set msg "Duplicate faces found!\n\n"
     set duplistids {}
     foreach dup $duplist {
       set id [lindex $all_ids [lindex [lsearch -exact -all $pretty_names $dup] end]]
       lappend duplistids $id
     }
     for {set i 0} {$i < [llength $duplist]} {incr i} {
       set dup [lindex $duplist $i]
       set dupid [lindex $duplistids $i]
       set newname [string trim $dup]_2
       set msg "$msg  Duplicate face name $dup is being renamed to $newname\n"
       set gOCCTFaceNames($dupid) $newname
       $modelname SetFaceAttr -attr gdscName -faceId $dupid -value $newname
     }
  }

  guiSV_model_add_faces_to_tree $kernel $modelname
  guiSV_model_display_only_given_model $modelname 1
  if {$isdups == 1} {
    tk_messageBox -title "Duplicate Face Names" -type ok -message $msg
  }

}

proc guiSV_model_opencascade_fixup {model num} {
  global symbolicName
  global gRen3d

  set kernel "OpenCASCADE"

  for {set i 0} {$i < [expr ($num-1)*2]} {incr i} {
    set currentids [$model GetFaceIds]
    set min_area 1.0e20
    set min_id -1
    set min_face ""
    foreach id $currentids {
      catch {repos_delete -obj /tmp/face/pd}
      $model GetFacePolyData -result /tmp/face/pd -face $id
      set tmp_area [geom_surfArea -src /tmp/face/pd]
      puts "Calcing Area $tmp_area for face $id"
      if {$tmp_area < $min_area} {
	set min_area $tmp_area
	set min_id $id
      }
    }
    if {$min_id != -1} {
      puts "Deleting $min_id"
      $model DeleteFaces -faces [list $min_id]
    }
  }

}

proc makeSurfOCCT {} {

    global gPathBrowser
    global guiLOFTvars
    set ren $gPathBrowser(ren)

    switch -exact $gPathBrowser(align_mtd_radio) {
	dist {
	    set vecFlag false
	}
	vec {
	    set vecFlag true
	}
	default {
	    return -code error "ERR: Unexpected profile alignment \
		    option \[$gPathBrowser(align_mtd_radio)\]"
	}
    }

    puts ">>> Vector-based alignment flag:  \[$vecFlag\]"

    set grp $gPathBrowser(currGroupName)
    if {($grp == "") || (![group_exists $grp])} {
	return -code error "Current group $grp not valid."
    }
    set sortedList [group_get $grp]
    if {[llength $sortedList] == 0} {
	return -code error "No profiles found for sampling."
    }

    #
    # supersample profiles
    #

    # sample all to the same resolution as the maximal resolution
    set numSuperPts 0
    foreach profile $sortedList {
      set numpts [geom_numPts -obj $profile]
      if {$numpts > $numSuperPts} {
        set numSuperPts $numpts
      }
    }

    set sample $gPathBrowser(solid_sample)
    puts ">>> Num fit pts per curve:  \[$sample\]"
    if {$sample > $numSuperPts} {
       set numSuperPts $sample
    }

    if {$numSuperPts == 0} {
      return -code error "ERROR: numSuperPts cant be zero!"
    }

    puts "Sampling all contours to a resolution of $numSuperPts points."

    foreach profile $sortedList {
      catch {repos_delete -obj $profile/supersample}
      geom_sampleLoop -src $profile -num $numSuperPts -dst $profile/supersample
    }


    #
    #  align profiles
    #

    set prof [lindex $sortedList 0]
    catch {repos_delete -obj $prof/aligned}
    geom_copy -src $prof/supersample -dst $prof/aligned

    for {set i 1} {$i < [llength $sortedList]} {incr i} {

	set p [lindex $sortedList [expr $i - 1]]
	set q [lindex $sortedList $i]

        catch {repos_delete -obj $q/aligned}

	geom_alignProfile \
		-ref $p/aligned \
		-src $q/supersample -dst $q/aligned \
		-vecMtd $vecFlag

    }

    #
    #  sample profiles
    #

    set sample $gPathBrowser(solid_sample)
    puts ">>> Num fit pts per curve:  \[$sample\]"

    foreach profile $sortedList {
      catch {repos_delete -obj $profile/sample}
      geom_sampleLoop -src $profile/aligned -num $sample -dst $profile/sample
    }

    #
    #  fit curves
    #

    set curveList {}
    foreach profile $sortedList {

      catch {repos_delete -obj $profile/curve}
      catch {repos_delete -obj $profile/curve/pd}

      if [catch {solid_makeInterpCurveLoop -src_pd $profile/sample -dst $profile/curve} errmsg] {
         puts "ERROR:  $errmsg"
         return -code error $errmsg
      }

      $profile/curve GetPolyData -result $profile/curve/pd

      lappend curveList $profile/curve

      set a [vis_pCurve $ren $profile/curve/pd]
      [$a GetProperty] SetLineWidth 3

    }

    vis_render $ren

    #
    #  loft solid
    #

    set c0 [lindex $curveList 0]
    if {[llength $curveList] < 2} {
	return -code error "Must have >= 2 curves to loft."
    }
    catch {repos_delete -obj $c0/surf}
    catch {repos_delete -obj $c0/surf/pd}
    solid_setKernel -name OpenCASCADE
    if {[catch {solid_makeLoftedSurf -srcs $curveList -dst $c0/surf -continuity 2 -partype 0 -w1 1.0 -w2 1.0 -w3 1.0 -smooth 0}]} {
	return -code error "Error lofting surface."
    }
    global tcl_platform
    #if {$tcl_platform(os) != "Darwin"} {
      catch {repos_delete -obj $c0/surf/capped}
      catch {repos_delete -obj $c0/surf/capped/pd}
      solid_capSurfToSolid -src $c0/surf -dst $c0/surf/capped
    #}

    #
    # bound solid
    #

    global gRen3d
    set ren $gRen3d

    #if {$tcl_platform(os) != "Darwin"} {
      set surf $c0/surf/capped
    #} else {
    #  set surf $c0/surf
    #}
    solid_setKernel -name OpenCASCADE

    global gOptions
    #if {$gOptions(facet_max_edge_size) != ""} {
    #  $surf GetPolyData -result $surf/pd \
    #              -max_edge_size $gOptions(facet_max_edge_size)
    #} else {
      $surf GetPolyData -result $surf/pd
    #}

    set a [vis_pRepos $ren $surf/pd]
    vis_pNorm $ren $surf/pd
    [$a GetProperty] SetOpacity $gPathBrowser(solid_opacity)

    # ugly way to keep track of solids created for each group
    global gLoftedSolids
    set gLoftedSolids($grp) $surf

    # clean up the tags with the group names
    set solid $surf
    foreach i [$solid GetFaceIds] {
      set facename {}
      $solid SetFaceAttr -attr parent -faceId $i -value $grp
    }

}

proc guiSV_model_blend_selected_models_occt {} {
  global gObjects
  global symbolicName
  global gOptions
  global gKernel
  global gOCCTFaceNames


  set tv $symbolicName(guiSV_model_tree)
  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: Can only blend one Parasolid model at a time"
  }
  set kernel $gKernel($model)
  if {$kernel != "OpenCASCADE"} {
    return -code error "ERROR: Solid kernel must be Parasolid or OpenCASCADE for operation"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  #model_create $kernel $oldmodel
  set faceids [$model GetFaceIds]
  foreach id $faceids {
    set facename [$model GetFaceAttr -attr gdscName -faceId $id]
    if {$facename != ""} {
      set ids($facename) $id
    }
  }
  set params [$symbolicName(guiOCCTBLENDSscript) get 0.0 end]

  set broken [split $params "\n"]
  if {[llength $broken] > 2} {
    return -code error "ERROR: Can only do one blend at a time"
  }

  set trimmed [string trim [lindex $broken 0]]
  if {$trimmed == ""} {continue}
  if {[llength $trimmed] != 4} {
    puts "ERROR: line ($trimmed) ignored!"
  }
  set faceA -1
  set faceB -1
  catch {set faceA [lindex $trimmed 0]}
  catch {set faceB [lindex $trimmed 1]}
  set r [lindex $trimmed 2]
  set minr [lindex $trimmed 3]
  if {$faceA < 0 || $faceB < 0} {
     return -code error "ERROR: invalid values in line ($trimmed)."
  }

  #set oldmodel "[string trim $model]_blended"
  guiSV_model_add_to_backup_list $kernel $model

  $model CreateEdgeBlend -faceA $faceA -faceB $faceB -radius $r -minradius $minr -fillshape 0

  set faceids [$model GetFaceIds]
  foreach id $faceids {
    set facename [$model GetFaceAttr -attr gdscName -faceId $id]
    set gOCCTFaceNames($id) $facename
  }

  set pretty_names {}
  foreach i [$model GetFaceIds] {
    catch {lappend pretty_names [$model GetFaceAttr -attr gdscName -faceId $i]}
  }
  if {[llength [lsort -unique $pretty_names]] != [llength $pretty_names]} {
    set duplist [lsort -dictionary $pretty_names]
    foreach i [lsort -unique $pretty_names] {
      set idx [lsearch -exact $duplist $i]
      set duplist [lreplace $duplist $idx $idx]
    }
    set msg "Duplicate faces found!\n\n"
    foreach dup $duplist {
      set msg "$msg  name: $dup\n"
    }
    tk_messageBox -title "Duplicate Face Names" -type ok -message $msg
  }

  guiSV_model_add_faces_to_tree $kernel $model
  guiSV_model_update_view_model $kernel $model
}

# Procedure: guiSV_model_get_occt_more_segs {} {
proc guiSV_model_create_model_opencascade_from_splines {} {
  global gRen3d
  global guiPDvars
  global gui3Dvars
  global gOptions
  global guiSVvars

  global guiBOOLEANvars
  set gOptions(meshing_solid_kernel) OpenCASCADE
  set kernel $gOptions(meshing_solid_kernel)
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  set ordered_names    $guiBOOLEANvars(selected_groups)
  set ordered_names2    $guiBOOLEANvars(selected_seg3d)
  set sampling_default $guiBOOLEANvars(sampling_default)
  set lin_multiplier   $guiBOOLEANvars(linear_sampling_along_length_multiplier)
  set useLinearSampleAlongLength   $guiBOOLEANvars(use_linear_sampling_along_length)
  set numModes         $guiBOOLEANvars(num_modes_for_FFT)
  array set overrides  $guiBOOLEANvars(sampling_overrides)
  set useFFT           $guiBOOLEANvars(use_FFT)
  set sample_per_segment $guiBOOLEANvars(sampling_along_length_multiplier)
  set addCaps          $guiBOOLEANvars(add_caps_to_vessels)
  set noInterOut       $guiBOOLEANvars(no_inter_output)
  set tol 	       $guiBOOLEANvars(tolerance)
  set spline           $guiBOOLEANvars(spline_type)


  set model [guiSV_model_new_surface_name 0]
  catch {repos_delete -obj $model}
  if {[model_create $kernel $model] != 1} {
    guiSV_model_delete_model $kernel $model
    catch {repos_delete -obj /models/$kernel/$model}
    model_create $kernel $model
  }

  foreach grp $ordered_names {

    set numOutPtsInSegs $sampling_default

    if [info exists overrides($grp)] {
      set numOutPtsInSegs $overrides($grp)
      puts "overriding default ($sampling_default) with ($numOutPtsInSegs) for ($grp)."
    }

    set vecFlag 0

    set numSegs [llength [group_get $grp]]

    set numOutPtsAlongLength [expr $sample_per_segment * $numSegs]

    set numPtsInLinearSampleAlongLength [expr $lin_multiplier *$numOutPtsAlongLength]

    puts "num pts along length: $numPtsInLinearSampleAlongLength"

    set outPD /guiGROUPS/polydatasurface/$grp
    catch {repos_delete -obj $outPD}

    #global gRen3dFreeze
    #set oldFreeze $gRen3dFreeze
    #set gRen3dFreeze 1
    solid_setKernel -name OpenCASCADE
    get_even_segs_along_length $grp $vecFlag  $useLinearSampleAlongLength $numPtsInLinearSampleAlongLength  $useFFT $numModes  $numOutPtsInSegs $numOutPtsAlongLength $addCaps $outPD
    #set gRen3dFreeze $oldFreeze

  }

  global gLoftedSolids
  set shortname [lindex $ordered_names 0]
  set cursolid $gLoftedSolids($shortname)
  solid_copy -src $cursolid -dst $model
  puts "copy $cursolid to preop model."

  foreach i [lrange $ordered_names 1 end] {
    set cursolid $gLoftedSolids($i)
    puts "union $cursolid into preop model."
    if {[repos_type -obj $cursolid] != "SolidModel"} {
       puts "Warning:  $cursolid is being ignored."
       continue
    }
     solid_union -result /tmp/preop/$model -a $cursolid -b $model

     repos_delete -obj $model
     solid_copy -src /tmp/preop/$model -dst $model

     repos_delete -obj /tmp/preop/$model
  }

  if {[repos_exists -obj /tmp/preop/$model] == 1} {
    repos_delete -obj /tmp/preop/$model
  }

  #global tcl_platform
  #if {$tcl_platform(os) == "Darwin"} {
  #  #Find face areas and remove two smaller ones
  #  set num [llength $createPREOPgrpKeptSelections]
  #  if { $num > 1} {
  #    guiSV_model_opencascade_fixup $model $num
  #  }
  #}

  global gOCCTFaceNames
  #crd_ren gRenWin_3D_ren1
  set pretty_names {}
  set all_ids {}
  foreach i [$model GetFaceIds] {
    catch {set type [$model GetFaceAttr -attr gdscName -faceId $i]}
    catch {set parent [$model GetFaceAttr -attr parent -faceId $i]}
    set facename "[string trim $type]_[string trim $parent]"
    lappend pretty_names $facename
    set gOCCTFaceNames($i) $facename
    $model SetFaceAttr -attr gdscName -faceId $i -value $facename
    lappend all_ids $i
  }
  set isdups 0
  if {[llength [lsort -unique $pretty_names]] != [llength $pretty_names]} {
   set isdups 1
   set duplist [lsort -dictionary $pretty_names]
   foreach i [lsort -unique $pretty_names] {
      set idx [lsearch -exact $duplist $i]
      set duplist [lreplace $duplist $idx $idx]
   }
   set msg "Duplicate faces found!\n\n"
   set duplistids {}
   foreach dup $duplist {
     set id [lindex $all_ids [lindex [lsearch -exact -all $pretty_names $dup] end]]
     lappend duplistids $id
   }
   for {set i 0} {$i < [llength $duplist]} {incr i} {
     set dup [lindex $duplist $i]
     set dupid [lindex $duplistids $i]
     set newname [string trim $dup]_2
     set msg "$msg  Duplicate face name $dup is being renamed to $newname\n"
     set gOCCTFaceNames($dupid) $newname
     $model SetFaceAttr -attr gdscName -faceId $dupid -value $newname
   }
}

guiSV_model_add_faces_to_tree $kernel $model
#guiSV_model_display_only_given_model $model 1
if {$isdups == 1} {
  tk_messageBox -title "Duplicate Face Names" -type ok -message $msg
}

}

proc get_even_segs_along_length {grp vecFlag useLinearSampleAlongLength numPtsInLinearSampleAlongLength useFFT numModes numOutPtsInSegs numOutPtsAlongLength addCaps outPD} {

    #puts "Vec Flag: $vecFlag"
    #puts "UseLinearSampleAlongLength $useLinearSampleAlongLength"
    #puts "NumPtsInLinearSampleAlongLength $numPtsInLinearSampleAlongLength"
    #puts "UseFFT $useFFT"
    #puts "NumModes $numModes"
    #puts "NumOutPtsInSegs $numOutPtsInSegs"
    #puts "NUmOutPtsAlongLength $numOutPtsAlongLength"
    #puts "AddCaps $addCaps"

    set unorientedPD /tmp/polysolid_create_vessel_from_group/tmp/unorientedPD
    catch {repos_delete -obj $unorientedPD}

    if {($grp == "") || (![group_exists $grp])} {
	return -code error "Current group $grp not valid."
    }
    set sortedList [group_get $grp]
    if {[llength $sortedList] == 0} {
	return -code error "No profiles found for sampling."
    }

    #
    # supersample profiles
    #

    # sample all to the same resolution as the maximal resolution
    set numSuperPts 0
    foreach profile $sortedList {
      set numpts [geom_numPts -obj $profile]
      if {$numpts > $numSuperPts} {
        set numSuperPts $numpts
      }
    }

    if {$numOutPtsInSegs > $numSuperPts} {
       set numSuperPts $numOutPtsInSegs
    }

    if {$numSuperPts == 0} {
      return -code error "ERROR: numSuperPts cant be zero!"
    }

    puts "Sampling all contours to a resolution of $numSuperPts points."

    foreach profile $sortedList {
      catch {repos_delete -obj $profile/supersample}
      geom_sampleLoop -src $profile -num $numSuperPts -dst $profile/supersample
    }

    #
    #  align profiles
    #

    set prof [lindex $sortedList 0]
    catch {repos_delete -obj $prof/aligned}
    geom_copy -src $prof/supersample -dst $prof/aligned

    for {set i 1} {$i < [llength $sortedList]} {incr i} {

	set p [lindex $sortedList [expr $i - 1]]
	set q [lindex $sortedList $i]

        catch {repos_delete -obj $q/aligned}

	geom_alignProfile  -ref $p/aligned  -src $q/supersample -dst $q/aligned  -vecMtd $vecFlag

    }

    #
    #  sample profiles
    #

    foreach profile $sortedList {
      catch {repos_delete -obj $profile/sample}
      geom_sampleLoop -src $profile/aligned -num $numOutPtsInSegs -dst $profile/sample
    }

    catch {repos_deleteList [repos_subList /guiGROUPS/segment/$grp/*]}
    catch {repos_delete -obj /guiGROUPS/polydatasurface/tmp/$grp}
    catch {repos_delete -obj /guiGROUPS/polydatasurface/$grp}

    set branch_segs {}
    set numSegs [llength $sortedList]

    catch {unset org_pts}
    catch {unset line_pts}
    catch {unset sampled_pts}
    catch {unset seg_ordered_pts}
    catch {unset all_seg_ordered_pts}

    for {set i 0} {$i < $numSegs} {incr i} {
      geom_getPts [lindex $sortedList $i]/sample org_pts($i)
    }

    for {set i 0} {$i < $numSegs} {incr i} {
      for {set j 0} {$j < $numOutPtsInSegs} {incr j} {
	lappend line_pts($j) [lindex $org_pts($i) $j]
      }
    }

    for {set i 0} {$i < $numOutPtsInSegs} {incr i} {

      set pts $line_pts($i)

      set mysplineX tmp-mysplineX
      set mysplineY tmp-mysplineY
      set mysplineZ tmp-mysplineZ
      catch {$mysplineX Delete}
      catch {$mysplineY Delete}
      catch {$mysplineZ Delete}
      vtkCardinalSpline $mysplineX
      vtkCardinalSpline $mysplineY
      vtkCardinalSpline $mysplineZ
      $mysplineX RemoveAllPoints
      $mysplineY RemoveAllPoints
      $mysplineZ RemoveAllPoints
      for {set n 0} {$n < [llength $pts]} {incr n} {
	set pt [lindex $pts $n]
	$mysplineX AddPoint $n [lindex $pt 0]
	$mysplineY AddPoint $n [lindex $pt 1]
	$mysplineZ AddPoint $n [lindex $pt 2]
      }
      set pts {}

      if {!$useLinearSampleAlongLength} {
        for {set n 0} {$n < $numOutPtsAlongLength} {incr n} {
          set t [expr 1.0*[$mysplineX GetNumberOfPoints]/(1.0*$numOutPtsAlongLength-1)*$n]
	  lappend pts [list [$mysplineX Evaluate $t] [$mysplineY Evaluate $t] [$mysplineZ Evaluate $t]]
        }
      } else {
        # first super sample along spline to get smoothness
        for {set n 0} {$n < $numPtsInLinearSampleAlongLength} {incr n} {
          set t [expr 1.0*[$mysplineX GetNumberOfPoints]/(1.0*$numPtsInLinearSampleAlongLength-1)*$n]
	  lappend pts [list [$mysplineX Evaluate $t] [$mysplineY Evaluate $t] [$mysplineZ Evaluate $t]]
        }
        # now linearly interpolate
	set pts [math_linearInterpCurve -pts $pts -closed 0 -numInterpPts $numOutPtsAlongLength]
      }

      if {$useFFT} {

        # create a "periodic" function from the path pts
        set biglist $pts
        set firstPt [lindex $pts 0]
        set lastPt [lindex $pts end]
	set count 0
        for {set j [expr [llength $pts] - 1]} {$j >= 0} {incr j -1} {
          lappend biglist [lindex $pts $j]
        }
        set newpts [math_smoothCurve -pts $biglist -closed 0 -numModes $numModes -numInterpPts [expr 2*$numOutPtsAlongLength]]
        set pts [lrange $newpts 0 [expr [llength $pts]-1]]

        # need to make sure we didn't shorten, but really need to do a linear sample now!
        set pts [linsert $pts 0 $firstPt]
        lappend pts $lastPt

        # now linearly interpolate
	set pts [math_linearInterpCurve -pts $pts -closed 0 -numInterpPts $numOutPtsAlongLength]

        #if {!$useLinearSampleAlongLength} {
        #   return -code error "ERROR: must use linear sample if you use fft!"
	#}
	#set pts [math_linearInterpCurve -pts $pts -closed 0 -numInterpPts $numPtsInLinearSampleAlongLength]

        #set pts [math_linearInterpCurve -pts $pts -closed 0 -numInterpPts $numOutPtsAlongLength]

      }

      set sampled_pts($i) $pts

    }

    for {set j 0} {$j < $numOutPtsInSegs} {incr j} {
      for {set i 0} {$i < $numOutPtsAlongLength} {incr i} {
	lappend seg_ordered_pts($i) [lindex $sampled_pts($j) $i]
      }
    }

    for {set i 0} {$i < $numOutPtsAlongLength} {incr i} {
       lappend all_seg_ordered_pts $seg_ordered_pts($i)
    }

    global gRen3d
    set ren $gRen3d
    set curveList {}
    for {set i 0} {$i < $numOutPtsAlongLength} {incr i} {
      set tmpSplineSlice($i) /$grp/spline/slice/$i
      catch {repos_delete -obj $tmpSplineSlice($i)}
      geom_mkLinesFromPts $seg_ordered_pts($i) $tmpSplineSlice($i) 1

      set profile $tmpSplineSlice($i)

      catch {repos_delete -obj $profile/curve}
      catch {repos_delete -obj $profile/curve/pd}

      if [catch {solid_makeInterpCurveLoop -src_pd $profile -dst $profile/curve} errmsg] {
         puts "ERROR:  $errmsg"
         return -code error $errmsg
      }

      $profile/curve GetPolyData -result $profile/curve/pd

      lappend curveList $profile/curve

      set a [vis_pCurve $ren $profile/curve/pd]
      [$a GetProperty] SetLineWidth 3

    }

    vis_render $ren

    #
    #  loft solid
    #

    set c0 [lindex $curveList 0]
    if {[llength $curveList] < 2} {
        return -code error "Must have >= 2 curves to loft."
    }
    catch {repos_delete -obj $c0/surf}
    catch {repos_delete -obj $c0/surf/pd}
    solid_setKernel -name OpenCASCADE
    if {[catch {solid_makeLoftedSurf -srcs $curveList -dst $c0/surf -continuity 2 -partype 0 -w1 1.0 -w2 1.0 -w3 1.0 -smooth 0}]} {
        return -code error "Error lofting surface."
    }
    global tcl_platform
    #if {$tcl_platform(os) != "Darwin"} {
      catch {repos_delete -obj $c0/surf/capped}
      catch {repos_delete -obj $c0/surf/capped/pd}
      solid_capSurfToSolid -src $c0/surf -dst $c0/surf/capped
    #}

    #
    # bound solid
    #

    global gRen3d
    set ren $gRen3d

    #if {$tcl_platform(os) != "Darwin"} {
      set surf $c0/surf/capped
    #} else {
    #  set surf $c0/surf
    #}
    solid_setKernel -name OpenCASCADE

    global gOptions
    global gPathBrowser
    #if {$gOptions(facet_max_edge_size) != ""} {
    #  $surf GetPolyData -result $surf/pd \
    #              -max_edge_size $gOptions(facet_max_edge_size)
    #} else {
      $surf GetPolyData -result $surf/pd
    #}

    set a [vis_pRepos $ren $surf/pd]
    vis_pNorm $ren $surf/pd
    [$a GetProperty] SetOpacity $gPathBrowser(solid_opacity)

    # ugly way to keep track of solids created for each group
    global gLoftedSolids
    set gLoftedSolids($grp) $surf

    # clean up the tags with the group names
    set solid $surf
    foreach i [$solid GetFaceIds] {
      set facename {}
      $solid SetFaceAttr -attr parent -faceId $i -value $grp
    }


}

# Procedure: createPREOPgrpSaveGroups
proc saveResampledGroup {grp numSuperPts} {

  set sortedList [group_get $grp]
  if {[llength $sortedList] == 0} {
      return -code error "No profiles found for sampling."
  }
  set resampleList {}
  foreach profile $sortedList {
    catch {repos_delete -obj $profile/supersample}
    geom_sampleLoop -src $profile -num $numSuperPts -dst $profile/supersample
    lappend resampleList $profile/supersample
  }

  # create the directory if it doesn't exist
  global gFilenames
  set mydir [tk_chooseDirectory -mustexist 0 -title "Save SimVascular Groups To Directory"  -initialdir $gFilenames(groups_dir)]
  set grpdir $gFilenames(groups_dir)
  puts $grpdir
  set contents_file [file join $grpdir group_contents.tcl]

  if {[file exists $grpdir] == 1} {
    if {[file isdirectory $grpdir] == 0} {
      puts "ERROR:  Group directory $grpdir is apparently a file!"
      return -code error "ERROR:  Group directory $grpdir is apparently a file!"
    }
  } else {
      if [catch {file mkdir $grpdir}] {
      puts "ERROR:  Could not create directory $grpdir."
      return -code error "ERROR:  Could not create directory $grpdir."
    }
  }

  set fp [open $contents_file "w"]
  puts $fp "# geodesic_groups_file 2.3"
  puts $fp ""
  puts $fp "#"

  puts $fp "proc group_autoload {} {"
  puts $fp "  global gFilenames"
  puts $fp "  set grpdir \$gFilenames(groups_dir)"
  puts $fp "  # Group Stuff"

  puts $fp "  group_readProfiles \{$grp\} \[file join \$grpdir \{$grp\}\]"
  saveResampledSegments $grp "[file join $grpdir $grp]_resampled" $resampleList $sortedList

  puts $fp "}"

  close $fp
}

# ------------------
# saveResampledSegments
# ------------------

proc saveResampledSegments {name filename items infoList} {
  #@author Nathan Wilson
  #@c  Routine to save a group of profiles to a file.
  #@a name:  Group to be written to the file.
  #@a filename:  File to be created.
  set fp [open $filename w]
  set count 0
  foreach i $items {
        # need to protect against empty segmentations
        if {[[repos_exportToVtk -src $i] GetNumberOfPoints] == 0} {
          continue
	}
	set keys [repos_getLabelKeys -obj $i]
	catch {unset arr}
	foreach k $keys {
	    set arr($k) [repos_getLabel -obj $i -key $k]
	}
	set str [array get arr]
	set id [group_itemid $name [lindex $infoList $count]]
	set ptList [geom_getOrderedPts -obj $i]
	puts $fp $i
	puts $fp $id
	puts $fp $str
	foreach pt $ptList {
	    set x [lindex $pt 0]
	    set y [lindex $pt 1]
	    set z [lindex $pt 2]
	    puts $fp "$x $y $z"
	}
	puts $fp ""
  	incr count
  }
  close $fp
}

