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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
# AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
# THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
#

proc polysolid_connect_curves_internal {allpts dstName} {

  set numOrgProfiles [llength $allpts]
  set numInterpPts [expr 1*$numOrgProfiles]

  puts "numOrgProfiles: $numOrgProfiles"

  set vpts tmp-geomConnectTwoProfilesPts
  set vconnA tmp-geomConnectTwoProfilesConnA
  set vconnB tmp-geomConnectTwoProfilesConnB
  set vpd tmp-geomConnectTwoProfilesPd

  catch {$vpts Delete}
  catch {$vconnA Delete}
  catch {$vconnB Delete}
  catch {$vpd Delete}

  vtkPoints $vpts
  $vpts Allocate 200 400

  vtkIdList $vconnA
  $vconnA Initialize
  $vconnA Allocate 200 400
  vtkIdList $vconnB
  $vconnB Initialize
  $vconnB Allocate 200 400

  vtkPolyData $vpd
  $vpd Initialize
  $vpd Allocate 200 400

  # create vtkPoints array
  set numCurvePts [llength [lindex $allpts 0]]

  puts "numCurvePts: $numCurvePts"

  # create all the points
  for {set i 0} {$i < [llength $allpts]} {incr i} {
     set sampled_pts($i) [lindex $allpts $i]
  }

  for {set i 0} {$i < $numInterpPts} {incr i} {
    for {set j 0} {$j < $numCurvePts} {incr j} {
      eval $vpts InsertNextPoint [lindex $sampled_pts($i) $j]
      #puts "Point!: [lindex $sampled_pts($i) $j]"
    }
  }

  $vpd SetPoints $vpts
  puts "NumberOfPoints: [$vpd GetNumberOfPoints]"
  puts "NumInterpPts: $numInterpPts"
  puts "NumCurvePts: $numCurvePts"

  for {set i 0} {$i < [expr $numInterpPts - 1]} {incr i} {
    set offset [expr $i * $numCurvePts]
    for {set j 0} {$j < $numCurvePts} {incr j} {
      # create connectivity for polygon
      $vconnA InsertNextId [expr $j + $offset]
      if {$j == [expr $numCurvePts  - 1]} {
	$vconnA InsertNextId [expr 0 + $offset]
        $vconnA InsertNextId [expr $numCurvePts + $offset ]
        $vconnB InsertNextId [expr $numCurvePts + $offset ]
      } else {
        $vconnA InsertNextId [expr $j + 1 + $offset]
        $vconnA InsertNextId [expr $numCurvePts + $j + 1 + $offset]
        $vconnB InsertNextId [expr $numCurvePts + $j + 1 + $offset]
      }
      $vconnB InsertNextId [expr $numCurvePts + $j + $offset]
      $vconnB InsertNextId [expr $j + $offset]
      set VTK_TRIANGLE 5
      $vpd InsertNextCell $VTK_TRIANGLE $vconnA
      $vpd InsertNextCell $VTK_TRIANGLE $vconnB
      $vconnA Initialize
      $vconnB Initialize
    }
  }
  puts "NumberOfCells: [$vpd GetNumberOfCells]"

  repos_importVtkPd -src $vpd -dst $dstName
  $vconnA Delete
  $vconnB Delete
  $vpts Delete
  $vpd Delete
}


proc polysolid_orient_open_vessel {inPD outPD} {

  catch {repos_delete -obj $outPD}

  set tmpPD /tmp/polysolid_orient_open_vessel/tmpPD
  catch {repos_delete -obj $tmpPD}

  set nrmls tmp-guiGROUPSalignAllProfiles-nrmls
  set removecaps tmp-guiGROUPSalignAllProfiles-removecaps

  catch {$nrmls Delete}
  catch {$removecaps Delete}

  # hack-and-a-half: assume that cap tris get tacked on the end of
  # the current polys

  set original_num_tris [[repos_exportToVtk -src $inPD] GetNumberOfCells]

  # temporarily cap solid so we can properly orient with outward normals
  geom_fillHoles $inPD $tmpPD

  # orient polys

  vtkPolyDataNormals $nrmls
  $nrmls SplittingOff
  $nrmls ConsistencyOn
  $nrmls AutoOrientNormalsOn
  $nrmls ComputeCellNormalsOn
  $nrmls ComputePointNormalsOff
  $nrmls SetInputData [repos_exportToVtk -src $tmpPD]
  $nrmls Update

  # remove caps
 
  vtkPolyData $removecaps
  $removecaps DeepCopy [$nrmls GetOutput]

  set VTK_TRIANGLE 5
  for {set i [expr [$removecaps GetNumberOfCells] - 1]} {$i >= $original_num_tris} {incr i -1} {
    if {[$removecaps GetCellType $i] == $VTK_TRIANGLE} {
      $removecaps DeleteCell $i
    }
  }
  $removecaps RemoveDeletedCells

  puts "org num tris ($original_num_tris)  new size ([$removecaps GetNumberOfCells])"

  # import uncapped result
  repos_importVtkPd -src $removecaps -dst $outPD

  # clean up
  $nrmls Delete
  $removecaps Delete
  repos_delete -obj $tmpPD

}


proc polysolid_orient_closed_vessel {inPD outPD} {

  catch {repos_delete -obj $outPD}

  set tmpPD /tmp/polysolid_orient_open_vessel/tmpPD
  catch {repos_delete -obj $tmpPD}

  set nrmls tmp-guiGROUPSalignAllProfiles-nrmls
 
  catch {$nrmls Delete}
  catch {$removecaps Delete}

  # cap solid

  set fillId 0
  set fillType 0
  geom_fillHolesWithIds $inPD $tmpPD $fillId $fillType

  # orient polys

  vtkPolyDataNormals $nrmls
  $nrmls SplittingOff
  $nrmls ConsistencyOn
  $nrmls AutoOrientNormalsOn
  $nrmls ComputeCellNormalsOn
  $nrmls ComputePointNormalsOff
  $nrmls SetInputData [repos_exportToVtk -src $tmpPD]
  $nrmls Update

  repos_importVtkPd -src [$nrmls GetOutput] -dst $outPD

  # clean up
  $nrmls Delete
  repos_delete -obj $tmpPD

}


proc polysolid_create_vessel_from_group {grp vecFlag useLinearSampleAlongLength numPtsInLinearSampleAlongLength useFFT numModes numOutPtsInSegs numOutPtsAlongLength addCaps outPD} {
  
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

                                        
    polysolid_connect_curves_internal $all_seg_ordered_pts $unorientedPD

    if {$addCaps} {
      polysolid_orient_closed_vessel $unorientedPD $outPD
    } else {
      polysolid_orient_open_vessel $unorientedPD $outPD
    }

    catch {repos_delete -obj $unorientedPD}

}

proc polysolid_c_create_vessel_from_group {grp vecFlag useLinearSampleAlongLength numPtsInLinearSampleAlongLength useFFT numModes numOutPtsInSegs numOutPtsAlongLength addCaps splineType outPD} {
  
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
      lappend all_segs $profile/sample
    }
    global guiBOOLEANvars
    set bias             $guiBOOLEANvars(bias)
    set tension          $guiBOOLEANvars(tension)
    set continuity       $guiBOOLEANvars(continuity)

    geom_loftSolid -srclist $all_segs -numOutInSegs $numOutPtsInSegs -numOutAlongLength $numOutPtsAlongLength -numLinearPtsAlongLength $numPtsInLinearSampleAlongLength -numModes $numModes -useFFT $useFFT -useLinearSampleAlongLength $useLinearSampleAlongLength -result $unorientedPD -splineType $splineType -bias $bias -tension $tension -continuity $continuity

    if {$addCaps} {
      polysolid_orient_closed_vessel $unorientedPD $outPD
    } else {
      polysolid_orient_open_vessel $unorientedPD $outPD
    }

    catch {repos_delete -obj $unorientedPD}

}


proc polysolid_create_all_vessels {selected_groups} {

  global gPathBrowser
  #set vecFlag $gPathBrowser(align_mtd_radio)
  set numOutPtsInSegs $gPathBrowser(solid_sample)

  set vecFlag 0

  set addCaps 0

  foreach grp $selected_groups {
 
    set numSegs [llength [group_get $grp]]

    set useLinearSampleAlongLength 0
    set useFFT 0

    set numOutPtsAlongLength [expr 6 * $numSegs]

    set numPtsInLinearSampleAlongLength [expr 10*$numOutPtsAlongLength]
   
    puts "num pts along length: $numPtsInLinearSampleAlongLength"

    set numModes 20

    set outPD /guiGROUPS/polydatasurface/$grp
    catch {repos_delete -obj $outPD}

    polysolid_create_vessel_from_group $grp $vecFlag \
         $useLinearSampleAlongLength $numPtsInLinearSampleAlongLength \
         $useFFT $numModes \
         $numOutPtsInSegs $numOutPtsAlongLength $addCaps \
         $outPD
  }

}
