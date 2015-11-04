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
    if {[catch {solid_makeLoftedSurf -srcs $curveList -dst $c0/surf -continuity 2 -partype 0 -w1 0.4 -w2 0.2 -w3 0.4 -smooth 0}]} {
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

  #set oldmodel "[string trim $model]_blended"
  guiSV_model_add_to_backup_list $kernel $model
  #model_create $kernel $oldmodel
  set faceids [$model GetFaceIds]
  foreach id $faceids {
    set facename [$model GetFaceAttr -attr gdscName -faceId $id]
    if {$facename != ""} {
      set ids($facename) $id
    }
  }
  set params [$symbolicName(guiOCCTBLENDSscript) get 0.0 end]

  set blended -1
  set broken [split $params "\n"]
  for {set i 0} {$i < [llength $broken]} {incr i} {
    set trimmed [string trim [lindex $broken $i]]
    if {$trimmed == ""} {continue}
    if {[llength $trimmed] != 3} {
      puts "ERROR: line ($trimmed) ignored!"
    }
    set faceA -1
    set faceB -1
    catch {set faceA [lindex $trimmed 0]}
    catch {set faceB [lindex $trimmed 1]}
    set r [lindex $trimmed 2]
    if {$faceA < 0 || $faceB < 0} {
       puts "ERROR: invalid values in line ($trimmed).  Line Ignored."
       continue
    }
    $model CreateEdgeBlend -faceA $faceA -faceB $faceB -radius $r

    set faceids [$model GetFaceIds]
    foreach id $faceids {
      set facename [$model GetFaceAttr -attr gdscName -faceId $id]
      set gOCCTFaceNames($id) $facename
    }
    set blended 1
  }
  if {$blended == -1} {
    return -code error "ERROR: No params, model was not blended"
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
