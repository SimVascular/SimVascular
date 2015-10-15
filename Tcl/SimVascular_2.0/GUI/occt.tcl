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

    # for convenience, we offer to make any missing solid models for the
    # the user.  In general, people shouldn't do this, but what can you
    # do?

    set recreateAllChoice [tk_messageBox -title "Recreate all vessels?"  -message "Recreate all solids?"  -icon question -type yesno]

    foreach i $createPREOPgrpKeptSelections {
      set cursolid ""

      if {$cursolid == "" || [repos_exists -obj $cursolid] == 0 || $recreateAllChoice == "yes"} {
         # solid  doesn't exist for current object, ask to create
         set choice [tk_messageBox -title "Missing capped solid branch!"  -message "Create missing solids using defaults?"  -icon question -type yesnocancel]
         switch -- $choice {
           yes {
              # create solids
              foreach j $createPREOPgrpKeptSelections {
                set cursolid ""
    catch {set cursolid $gLoftedSolids($j)}
      if {$cursolid == "" || [repos_exists -obj $cursolid] == 0 || $recreateAllChoice == "yes"} {
                  # loft solid from group
                  global gPathBrowser
                  set keepgrp $gPathBrowser(currGroupName)
                  set gPathBrowser(currGroupName) $j
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
              }
              break
     }
           cancel {
              return -code error "Missing solid branches.  Create preop model failed."
     }
           no {
              return -code error "Missing solid braches.  Create preop model failed."
     }
         }

      } elseif {[repos_type -obj $cursolid] != "SolidModel"} {
         puts  "ERROR: Expected SolidModel for $cursolid."
         return -code error "ERROR: Expected SolidModel for $cursolid."
      }
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
       solid_union -result /tmp/preop/$modelname -a $modelname -b $cursolid
       repos_delete -obj $modelname
       solid_copy -src /tmp/preop/$modelname -dst $modelname
       repos_delete -obj /tmp/preop/$modelname
    }

    if {[repos_exists -obj /tmp/preop/$modelname] == 1} {
      repos_delete -obj /tmp/preop/$modelname
    }

    #Find face areas and remove two smaller ones
    set num [llength $createPREOPgrpKeptSelections]
    if { $num > 1} {
      guiSV_model_opencascade_fixup $modelname $num
    }

    #If not on mac and want caps, cap the surface
    global tcl_platform
    if {$tcl_platform(os) != "Darwin"} {
      set copy "_copy"
      set copymod $modelname$copy
      catch {repos_delete -obj $copymod}
      solid_capSurfToSolid -src $modelname -dst $copymod
      catch {repos_delete -obj $modelname}

      solid_copy -src $copymod -dst $modelname
    }

    global gOCCTFaceNames
    crd_ren gRenWin_3D_ren1
    set pretty_names {}
    set all_ids {}
    foreach i [$modelname GetFaceIds] {
      puts "What id number $i"
      lappend pretty_names "noname_$i"
      set gOCCTFaceNames($i) "noname_$i"
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
       set gOCCTFaceNames($dupid) "noname_dup_$dupid"
       #$modelname SetFaceAttr -attr gdscName -faceId $dupid -value $newname
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
      $model DeleteRegion -regionid $min_id
      puts "Deleting $min_id"
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
    if {[catch {solid_makeLoftedSurf -srcs $curveList -dst $c0/surf -continuity 4 -partype 0 -w1 0.4 -w2 0.2 -w3 0.4}]} {
	return -code error "Error lofting surface."
    }

    #
    # bound solid
    #

    global gRen3d
    set ren $gRen3d

    set surf $c0/surf
    solid_setKernel -name OpenCASCADE

    global gOptions
    if {$gOptions(facet_max_edge_size) != ""} {
      $surf GetPolyData -result $surf/pd \
                  -max_edge_size $gOptions(facet_max_edge_size)
    } else {
      $surf GetPolyData -result $surf/pd
    }

    set a [vis_pRepos $ren $surf/pd]
    vis_pNorm $ren $surf/pd
    [$a GetProperty] SetOpacity $gPathBrowser(solid_opacity)

    # ugly way to keep track of solids created for each group
    global gLoftedSolids
    set gLoftedSolids($grp) $surf

    # clean up the tags with the group names
    #set solid $surf
    #foreach i [$solid GetFaceIds] {
    #  set facename {}
    #  catch {set facename [$solid GetFaceAttr -attr gdscName -faceId $i]}
    #  if {$facename != "" && $facename != "inflow" && $facename != "inlet"} {
    #    $solid SetFaceAttr -attr gdscName -faceId $i -value $grp
    #  } else {
    #    # we have a wall
    #    $solid SetFaceAttr -attr gdscName -faceId $i -value wall_$grp
    #  }
    #}

}
