# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved.
#
# Portions of this code Copyright (c) 2009-2011 Open Source Medical Software Corporation.
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
#===========================================================================  

proc geom_appendPds {pdList outPd} {

    set vtkPds {}
    set numPds [llength $pdList]
    foreach pd $pdList {
	lappend vtkPds [repos_exportToVtk -src $pd]
    }
    set numPds [llength $vtkPds]
    
    set appender tmp-guiTRIM-appender
    catch {$appender Delete}

    vtkAppendPolyData $appender
    $appender UserManagedInputsOn
    $appender SetNumberOfInputs $numPds
    for {set i 0} {$i < $numPds} {incr i} {
	$appender SetInputDataByNumber $i [lindex $vtkPds $i]
    }

    $appender Update

    repos_importVtkPd -src [$appender GetOutput] -dst $outPd

}


proc geom_densify {inPd numDivs outPd} {

  set densy tmp-geom_densify
  catch {$densy Delete}

  vtkDensifyPolyData $densy
  $densy SetInputDataObject [repos_exportToVtk -src $inPd]
  $densy SetNumberOfSubdivisions $numDivs
  $densy Update

  repos_importVtkPd -src [$densy GetOutput] -dst $outPd
  $densy Delete

}


proc geom_cap_with_delaunay {inPd cutPd outPd} {

  set flat             /tmp/geom_cap_with_delaunay/flat
  set useMeForNormalPd /tmp/geom_cap_with_delaunay/use-me-for-normal-pd
  set dPd              /tmp/geom_cap_with_delaunay/delaunay2D/output
  set dIn3D            /tmp/geom_cap_with_delaunay/delaunay2D/output/3D
  set combined         /tmp/geom_cap_with_delaunay/wow

  catch {repos_delete -obj $flat}
  catch {repos_delete -obj $useMeForNormalPd}
  catch {repos_delete -obj $dPd}
  catch {repos_delete -obj $dIn3D}
  catch {repos_delete -obj $combined}

  set delaunay         tmp-geom_cap_with_delaunay-delaunay-2D-triangulator
  catch {$delaunay Delete}

  set pts [geom_getOrderedPts -obj $inPd]
  geom_polygonFromPts $pts $useMeForNormalPd

  set normal [geom_avgNormal $useMeForNormalPd]
  set normal [math_normalize $normal]

  set boolean 1
  set rotated_norm {}
 
  geom_flatten $normal $boolean $inPd rotated_norm $flat

  set avgpt [geom_avgPt -obj $flat]
  set bbox [geom_bbox -obj $flat]

  if {[expr abs([lindex $bbox 5] - [lindex $bbox 4])] > 0.1} {
    catch {repos_delete -obj $flat}
    set flux non-planar
    return -code error "ERROR:  face not flat, value of zero being returned!"
  }

  [[repos_exportToVtk -src $flat] GetPoints] InsertNextPoint [lindex $avgpt 0] [lindex $avgpt 1] [lindex $avgpt 2]

  #geom_translate -src $flat  -vec [list 0 0 [expr -1.0*[lindex $bbox 4]]]  -dst $flat_0

  vtkDelaunay2D $delaunay
  $delaunay SetInputDataObject [repos_exportToVtk -src $flat]
  $delaunay Update
  
  repos_importVtkPd -src [$delaunay GetOutput] -dst $dPd

  # rotate back
  set boolean 0
  set back_to_norm $rotated_norm
  geom_flatten $normal $boolean $dPd back_to_norm $dIn3D

  geom_appendPds [list $inPd $cutPd $dIn3D] $combined
  geom_mergePts -src $combined -dst $outPd

}


proc geom_butterfly {inPd numDivs outPd} {

  set butt tmp-geom_butterfly
  catch {$butt Delete}

  vtkButterflySubdivisionFilter $butt
  $butt SetInputDataObject [repos_exportToVtk -src $inPd]
  $butt SetNumberOfSubdivisions $numDivs
  $butt Update

  repos_importVtkPd -src [$butt GetOutput] -dst $outPd
  $butt Delete

}

proc geom_localOperation {operation inPd outPd} {
  global gui3Dvars
  global gKernel
  global symbolicName
  global gPickedCellIds
  global gNumPickedCells
  set tv $symbolicName(guiSV_model_tree)
  set tbox $symbolicName(guiLocalSurfaceOperationParametersTextBox)
  set gui3Dvars(localControlAttributes) [$tbox get 0.0 end]
  
  set tmp1Pd /tmp/models/local/first
  set tmp2Pd /tmp/models/local/second
  catch {repos_delete -obj $tmp1Pd}
  geom_copy -src $inPd -dst $tmp1Pd

  set cell_lines 0
  set broke [split $gui3Dvars(localControlAttributes) "\n"]
  for {set i 0} {$i < [llength $broke]} {incr i} {
    set trimmed [string trim [lindex $broke $i]]
    if {$trimmed != ""} {
      catch {repos_delete -obj $tmp2Pd}
      set cmd [lindex $trimmed 0]
      if {$cmd == "sphere"} {
	set radius [lindex $trimmed 1]
	set center [lrange $trimmed 2 end-2]
	set outarray [lindex $trimmed end-1]
	set datatype [lindex $trimmed end]
        geom_set_array_for_local_op_sphere -src $tmp1Pd -result $tmp2Pd -radius $radius -center $center -outarray $outarray -datatype $datatype
      } elseif {$cmd == "faces"} {
	set changelist [lrange $trimmed 1 end-3] 
	set inarray [lindex $trimmed end-2]
	set outarray [lindex $trimmed end-1]
	set datatype [lindex $trimmed end]
	if {$changelist == ""} {return }
        geom_set_array_for_local_op_face -src $tmp1Pd -result $tmp2Pd -array $inarray -values $changelist -outarray $outarray -datatype $datatype
      } elseif {$cmd == "cells"} {
	set changelist [lrange $trimmed 1 end-2]
	set outarray [lindex $trimmed end-1]
	set datatype [lindex $trimmed end]
        if {$gNumPickedCells != 0} {
          geom_set_array_for_local_op_cells -src $tmp1Pd -result $tmp2Pd -values $changelist -outarray $outarray -datatype $datatype
	}
	set cell_lines 1
      } elseif {$cmd == "blend"} {
	set faceids [lrange $trimmed 1 end-4]
	set radius [lindex $trimmed end-3]
	set splitarray [lindex $trimmed end-2]
	set outarray [lindex $trimmed end-1]
	set datatype [lindex $trimmed end]
	geom_set_array_for_local_op_face_blend -src $tmp1Pd -result $tmp2Pd -values $faceids -radius $radius -array $splitarray -outarray $outarray -datatype $datatype
      }
      catch {repos_delete -obj $tmp1Pd}
      geom_copy -src $tmp2Pd -dst $tmp1Pd
    }
  }
  if {$cell_lines} {
    $symbolicName(guiLocalSurfaceOperationParametersTextBox) delete 0.0 end
  }

  set tmp3Pd /tmp/model/local/third
  catch {repos_delete -obj $tmp3Pd}
  if {$operation == "lDec"} {
    set target $gui3Dvars(local_quad_target)
    geom_local_decimation -src $tmp2Pd -result $tmp3Pd -target $target -cellarray "ActiveCells"
  } elseif {$operation == "lLap"} {
    set iters $gui3Dvars(local_smooth_num_iters)
    set relax $gui3Dvars(local_smooth_relax_factor)
    geom_local_laplacian_smooth -src $tmp2Pd -result $tmp3Pd -numiters $iters -relax $relax -cellarray "ActiveCells"
  } elseif {$operation == "lSub"} {
    set iters $gui3Dvars(local_linear_subdivisions)
    geom_local_subdivision -src $tmp2Pd -result $tmp3Pd -numiters $iters -cellarray "ActiveCells"
  } elseif {$operation == "lCon"} {
    set iters $gui3Dvars(local_cgsmooth_num_iters)
    set constrain $gui3Dvars(local_cgsmooth_constrain_factor)
    geom_local_constrain_smooth -src $tmp2Pd -result $tmp3Pd -numiters $iters -constrainfactor $constrain -cellarray "ActiveCells"
  } else {
    return -code error "ERROR: Invalid local surface operation"
  }

  set removeTmp /tmp/model/local/fourth
  catch {repos_delete -obj $removeTmp}
  catch {repos_delete -obj $outPd}
  set removeTmp [repos_exportToVtk -src $tmp3Pd]
  [$removeTmp GetCellData] RemoveArray "ActiveCells"
  repos_importVtkPd -src $removeTmp -dst $outPd
  set delete 1
  PickPolyDataCell widget x y add $delete
}

proc geom_fillHoles {inPd outPd} {

  set tmpPd tmp-geom_fillHoles-pd
  set filler tmp-geom_fillHoles-filler
  catch {$filler Delete}
  vtkFillHolesFilter $filler
  $filler SetHoleSize [$filler GetHoleSizeMaxValue]

  $filler SetInputDataObject [repos_exportToVtk -src $inPd]
  $filler Update

  catch {repos_delete -obj $tmpPd}
  repos_importVtkPd -src [$filler GetOutput] -dst $tmpPd

  geom_orientPd $tmpPd $outPd

  catch {repos_delete -obj $tmpPd}
 
  $filler Delete

}

proc geom_fillHolesWithIds {inPd outPd fillId filltype} {

  set tmpPd tmp-geom_fillHoles-pd

  catch {repos_delete -obj $tmpPd}
  geom_cap_with_ids -src $inPd -result $tmpPd -fillnum $fillId -filltype $filltype

  geom_orientPd $tmpPd $outPd

  catch {repos_delete -obj $tmpPd}
 
}


proc geom_quadraticDecimation {inPd target outPd} {

  set decimator tmp-geom_quadraticDecimation-decimator
  catch {$decimator Delete}
  vtkQuadricDecimation $decimator
  $decimator SetTargetReduction $target
  $decimator SetInputDataObject [repos_exportToVtk -src $inPd]
  $decimator Update
  puts "actual decimation: [$decimator GetActualReduction]"
  catch {repos_delete -obj $outPd}
  repos_importVtkPd -src [$decimator GetOutput] -dst $outPd
  $decimator Delete

}


proc geom_fillSurfaceVoid {inPd outPd id} {

  set freeEdges /tmp/guiTRIM/freeEdges
  set capPd /tmp/guiTRIM/capPd
  #set capTriPd /tmp/guiTRIM/capTriPd
 
  catch {repos_delete -obj $freeEdges}
  catch {repos_delete -obj $capPd}
  catch {repos_delete -obj $capTriPd} 

  #set vtkidobj tmp-guiTRIM-ids
  #catch {$vtkidobj Delete}

  # find the free edges
  geom_getFreeEdges $inPd $freeEdges
  
  # find the points on the free edges
  set pts [geom_getOrderedPts -obj $freeEdges]
  
  # assume planar, make polygon
  geom_polygonFromPts $pts $capPd
  
  # associate id to polygon
  geom_tagAllCellsWithId $capPd $id
  #vtkIntArray $vtkidobj
  #$vtkidobj SetNumberOfComponents 1
  #$vtkidobj SetNumberOfTuples 1
  #$vtkidobj SetTuple1 0 $id   

  #set pd [repos_exportToVtk -src $capPd] 
  #[$pd GetCellData] SetScalars $vtkidobj

  geom_triangulate $capPd $outPd

}


proc geom_largestConnected {inPd outPd} {

  set polyPD tmp-largestPolysOnly
  set reposPolyPD /tmp/geom_largestConnected
  set reposPolyPD2 /tmp/geom_largestConnected2
  catch {repos_delete -obj $reposPolyPD}
  catch {repos_delete -obj $reposPolyPD2}
  catch {$polyPD Delete}
  vtkPolyData $polyPD
  $polyPD SetPoints [[repos_exportToVtk -src $inPd] GetPoints]
  $polyPD SetPolys  [[repos_exportToVtk -src $inPd] GetPolys]
  repos_importVtkPd -src $polyPD -dst $reposPolyPD
  geom_mergePts -src $reposPolyPD -dst $reposPolyPD2

  set connfilt tmp-geom_largestConnected
  catch {$connfilt Delete}
  vtkPolyDataConnectivityFilter $connfilt
  $connfilt SetInputDataObject [repos_exportToVtk -src $reposPolyPD2]
  $connfilt SetExtractionModeToLargestRegion
  $connfilt Update
  repos_importVtkPd -src [$connfilt GetOutput] -dst $outPd

}


proc geom_mkSphere {x y z r outPd} {

  set s tmp-mk_sphere-source
  catch {$s Delete}
  vtkSphereSource $s
  $s SetRadius $r
  $s SetCenter $x $y $z
  $s Update
  repos_importVtkPd -src [$s GetOutput] -dst $outPd
  $s Delete

}


proc geom_orientPd {inPd outPd} {
  
  set tmpPd tmp-orientedPd-pd
  catch {repos_delete -obj $tmpPd}

  set cleaner tmp-geom_orientedPd-cleaner
  set orienter tmp-orientedPd-normls
  catch {$cleaner Delete}
  catch {$orienter Delete}

  vtkCleanPolyData $cleaner
  $cleaner PointMergingOn
  $cleaner ConvertLinesToPointsOff
  $cleaner ConvertPolysToLinesOff
  $cleaner SetInputDataObject [repos_exportToVtk -src $inPd]
  $cleaner Update

  vtkPolyDataNormals $orienter 
  $orienter SetInputDataObject [$cleaner GetOutput]
  $orienter AutoOrientNormalsOn
  $orienter ComputePointNormalsOn
  puts "flipper [$orienter GetFlipNormals]"
  $orienter FlipNormalsOn
  $orienter SplittingOff
  $orienter ComputeCellNormalsOn
  $orienter ConsistencyOn
  $orienter NonManifoldTraversalOff
  $orienter Update
  repos_importVtkPd -src [$orienter GetOutput] -dst $outPd

}


proc geom_avgNormal {inPd} {
  
  set tmpPd tmp-avgNormPd-pd
  catch {repos_delete -obj $tmpPd}

  geom_mergePts -src $inPd -dst $tmpPd -tol 0.0001

  set orienter tmp-orientedPd-normls

  catch {$orienter Delete}
  vtkPolyDataNormals $orienter 
  $orienter SetInputDataObject [repos_exportToVtk -src $tmpPd]
  $orienter AutoOrientNormalsOn
  $orienter ComputePointNormalsOn
  #puts "flipper [$orienter GetFlipNormals]"
  $orienter FlipNormalsOn
  $orienter SplittingOn
  $orienter ComputeCellNormalsOn
  $orienter ConsistencyOn
  $orienter NonManifoldTraversalOff
  $orienter Update

  set normals [[[$orienter GetOutput] GetPointData] GetNormals]

  set x 0.0
  set y 0.0
  set z 0.0
  set numPts [$normals GetNumberOfTuples]
  for {set i 0} {$i < $numPts} {incr i} {
    set nrm [$normals GetTuple3 $i]
    set x [expr $x + [lindex $nrm 0]]
    set y [expr $y + [lindex $nrm 1]]
    set z [expr $z + [lindex $nrm 2]]
  }
  set x [expr $x/(1.0*$numPts)]
  set y [expr $y/(1.0*$numPts)]
  set z [expr $z/(1.0*$numPts)]

  $orienter Delete
  catch {repos_delete -obj $tmpPd}
  
  return [list $x $y $z]

}


proc geom_smooth {inPd relaxFactor numIters outPd} {

  set smoother tmp-geom_smooth
  catch {$smoother Delete}

  vtkSmoothPolyDataFilter $smoother
  $smoother SetInputDataObject [repos_exportToVtk -src $inPd]
  $smoother SetRelaxationFactor $relaxFactor
  $smoother SetNumberOfIterations $numIters
  #$smoother SetFeatureAngle 30.0
  $smoother FeatureEdgeSmoothingOff
  $smoother BoundarySmoothingOff
  $smoother Update

  repos_importVtkPd -src [$smoother GetOutput] -dst $outPd
  $smoother Delete

}


proc geom_syncsmooth {inPd passBandValue numIters outPd} {

  set smoother tmp-geom_syncsmooth
  catch {$smoother Delete}

  vtkWindowedSincPolyDataFilter $smoother
  $smoother SetInputDataObject [repos_exportToVtk -src $inPd]
  $smoother SetPassBand $passBandValue
  $smoother SetNumberOfIterations $numIters
  #$smoother SetFeatureAngle 30.0
  $smoother FeatureEdgeSmoothingOff
  $smoother BoundarySmoothingOff
  $smoother Update

  repos_importVtkPd -src [$smoother GetOutput] -dst $outPd
  $smoother Delete

}



proc geom_tagAllCellsWithId {inPd id} {
  set pd [repos_exportToVtk -src $inPd]
  set vtkidobj tmp-guiTRIM-ids
  catch {$vtkidobj Delete}
  vtkIntArray $vtkidobj
  $vtkidobj SetName ModelFaceID
  $vtkidobj SetNumberOfComponents 1
  $vtkidobj SetNumberOfTuples [$pd GetNumberOfPolys]
  $vtkidobj FillComponent 0 $id
  [$pd GetCellData] SetScalars $vtkidobj
}


proc img_loadVTI {filename objName} {

    # read image data
    set xmlreader __xmlVolReader
    catch {$xmlreader Delete}
    vtkXMLImageDataReader $xmlreader
    $xmlreader SetFileName $filename
    $xmlreader Update

    # create a repository vtkImg object
    catch {repos_delete -obj $objName}
    repos_importVtkImg -src [$xmlreader GetOutput] -dst $objName
    $xmlreader Delete

}


proc img_mkThrImg {inImg thrVal outImg} {

  set thr tmp-im_mkThrImg
  catch {$thr Delete}
  vtkImageThreshold $thr
  $thr SetInputDataObject [repos_exportToVtk -src $inImg]
  $thr SetReplaceIn 1
  $thr SetReplaceOut 1
  $thr SetInValue 255
  $thr SetOutValue 0
  if {[llength $thrVal] == 1} {
    $thr ThresholdByUpper $thrVal
  } else {
      $thr ThresholdBetween [lindex $thrVal 0] [lindex $thrVal 1]
  }
  $thr Update
  repos_importVtkImg -src [$thr GetOutput] -dst $outImg
  $thr Delete

}


proc img_maskImg {inImg maskImg outImg} {

  set masker tmp-im_maskImg
  catch {$masker Delete}
  vtkImageMask $masker
  $masker SetInput1Data [repos_exportToVtk -src $inImg]
  $masker SetInput2Data [repos_exportToVtk -src $maskImg]
  $masker SetMaskedOutputValue 0
  $masker Update
  repos_importVtkImg -src [$masker GetOutput] -dst $outImg
  $masker Delete

}


proc img_castToShort {inImg outImg} {
  set caster tmp-img_floatToShort-caster
  catch {$caster Delete}
  vtkImageCast $caster
  $caster SetInputDataObject [repos_exportToVtk -src $inImg]
  $caster SetOutputScalarTypeToShort
  $caster Update
  catch {repos_delete -obj $outImg}
  repos_importVtkImg -src [$caster GetOutput] -dst $outImg
  $caster Delete
}
proc img_castToFloat {inImg outImg} {
  set caster tmp-img_floatToShort-caster
  catch {$caster Delete}
  vtkImageCast $caster
  $caster SetInputDataObject [repos_exportToVtk -src $inImg]
  $caster SetOutputScalarTypeToFloat
  $caster Update
  catch {repos_delete -obj $outImg}
  repos_importVtkImg -src [$caster GetOutput] -dst $outImg
  $caster Delete
}


proc solid_mkDiscreteModel {pd outModel fn} {
 set curKernel [solid_getKernel]
 solid_setKernel -name Discrete
 solid_poly3dSolid -result $outModel -src $pd -facet Union
 $outModel WriteNative -file $fn
 solid_setKernel -name $curKernel
}


#
#  EXPERIMENTAL CODE!!!
#

proc TAC_calculator {R C period units} {
   set bn 1
   set flow_fn model_inflow.dat
   set ratio 0.056
   RCR_parameter_estimator $R $C $period $ratio $units $bn $flow_fn
}

proc Resistance_Ratio_Calculator {R C period units bn} {
   set flow_fn inflow_data.dat
   set ratio 0.056
   # loading the flow data
   set fp [open $flow_fn r]
   while {[gets $fp line] >= 0} {
     lappend all_values [string trim $line]
   }
   close $fp
   set bn_values [lindex $all_values [expr $bn - 1]]
   set R [lindex $bn_values 0]
   set C [lindex $bn_values 1]
   RCR_parameter_estimator $R $C $period $ratio $units $bn $flow_fn
}

proc RCR_parameter_estimator {R C period ratio units bn flow_fn} {
  # ins:
  #
  #  R      == What is the model resistance determined from the mean pressure and inflow
  #  C      == What is your estimate for the total arterial capacitance
  #  period == Please enter the duration of one cardiac cycle in seconds
  #  ratio  == ratio (default 0.056)
  #  untis  == Are you working in cm or mm (cm = 1, mm = 2)
  #  bn     == default 1
  #  flow_fn == flow files

  ## From Westerhof 1971:
  ## Rc=80;
  ## Rd=1200;
  ## C=0.0008;
  ## These parameters give approximately normal systolic and diastolic pressures.
  ## Rc=121;
  ## Rd=1700;
  ## C=0.0008;
  ## This set of parameters allows adjustments of this ratio: Rc/R
  ## R=0.2114;
  ## ratio=0.06;
    set I [::math::complexnumbers::complex 0 1]

    # loading the flow data
    set fp [open $flow_fn r]
    while {[gets $fp line] >= 0} {
      if {[string trim $line] == ""} continue
      lappend all_flow_values [string trim $line]
    }
    close $fp
    #puts "all_flow_values: $all_flow_values"

    set number_of_points_per_cycle [llength $all_flow_values]

    foreach datapt $all_flow_values {
      lappend flow [lindex $datapt [expr $bn - 1]]
    }
    #puts "flow: $flow"

    # calculate mean flow
    set mean_q 0
    foreach q $flow {
	set mean_q [expr $mean_q + $q]
    }
    set mean_q [expr $mean_q/(1.0*[llength $flow])]
    
    # Calculating the time
    set dt [expr double($period)/($number_of_points_per_cycle)]
    for {set i 0} {$i < [llength $flow]} {incr i} {
	set t [expr 0 + $i*$dt]
        lappend flow_with_time [list $t [lindex $flow $i]]
    }
    lappend t $period
    lappend flow_with_time [list $period [lindex $flow 0]]
    #puts "flow_with_time: $flow_with_time"

    set Rc [expr $R*$ratio]
    set Rd [expr $R*(1-$ratio)]

    set johnsWay 0

    if { $johnsWay } { 
  
      set num_kept_modes [llength $all_flow_values]

      for {set i 0} {$i <= [expr $number_of_points_per_cycle / 2]} {incr i} {
	set freq [expr 1.0/$period*$i]
        lappend frequencies $freq
        lappend omega [expr 2.0*[math_pi]*$freq]
      }
 
      for {set i [expr -$number_of_points_per_cycle/2+1]} {$i <= -1} {incr i} {
	set freq [expr 1.0/$period*$i]
        lappend frequencies $freq
        lappend omega [expr 2.0*[math_pi]*$freq]
      }

    } else {

      set num_kept_modes [expr [llength $all_flow_values] / 1]
      for {set i 0} {$i < $num_kept_modes} {incr i} {
	set freq [expr 1.0/$period*$i]
        lappend frequencies $freq
        lappend omega [expr 2.0*[math_pi]*$freq]
      }

    }
    #puts "freq: $frequencies"
    #puts "omega: $omega"
 
    foreach omg $omega {
        #  this is the expression we need to calculate with complex numbers
        #  $Rc+1.0/(1.0/$Rd+$I*$omg*$C)
        set den1 [list [expr 1.0/$Rd] 0]
        set den2 [::math::complexnumbers::* [list 0 1] [list [expr $omg*$C] 0]]
        set den [::math::complexnumbers::+ $den1 $den2]
        set part2 [::math::complexnumbers::/ [list 1.0 0] $den]
        set Z [::math::complexnumbers::+ [list $Rc 0] $part2]
	lappend Z_RCR $Z
    }
  
    if { $johnsWay } {
      set midpos [expr $number_of_points_per_cycle/2]
      set Z_RCR [lreplace $Z_RCR $midpos $midpos [list [::math::complexnumbers::real [lindex $Z_RCR $midpos]] 0]]
    }

    set modes [math_FFT -pts $flow_with_time -nterms $num_kept_modes -numInterpPts 1024]
    #puts "modes: $modes"
    
    #puts "num_kept_modes $num_kept_modes  length Z_RCR [llength $Z_RCR]"
    for {set i 0} {$i < [llength $Z_RCR]} {incr i} {
      lappend modified_modes [::math::complexnumbers::* [lindex $modes $i] [lindex $Z_RCR $i]]
    }
    
    set pomega [expr 2.0*[math_pi]*1.0/$period]
    set pressure [math_inverseFFT -terms $modified_modes -t0 0 -dt $dt -omega $pomega -numPts [expr $number_of_points_per_cycle + 1]]
    #puts "pressure: $pressure"

    # this is imepdance in time
    #set z_rcr [math_inverseFFT -terms $Z_RCR -t0 0 -dt $dt -omega $pomega -numPts [expr $number_of_points_per_cycle + 1]]

    # this is a correction for the Aspire def of iDFT 
    #foreach i $z_rcr {
    #	lappend aspire_z_rcr [expr $z_rcr * [llength $z_rcr]]
    #}
   
    if {$units == "cm"} {
      set pressure_units_in_1_mmHg 1333.2237
    } elseif {$units == "mm"} {
      set pressure_units_in_1_mmHg 133.32237
    } else {
      return -code error "ERROR:  invalid units option ($units)!"
    }

    foreach p [lrange $pressure 0 end-1] {
	lappend scaled_pressure [expr [lindex $p 1] / double($pressure_units_in_1_mmHg)]
    }
    #puts "scaled_pressure: $scaled_pressure"
    set sorted_p [lsort -real -increasing $scaled_pressure]
    #puts "sorted_p: $sorted_p"

    set systolic_pressure [lindex $sorted_p end]
    set diastolic_pressure [lindex $sorted_p 0]
    set pulse_pressure [expr $systolic_pressure-$diastolic_pressure]
     
    set mean_p 0
    foreach p $scaled_pressure  {
	set mean_p [expr $mean_p + $p]
    }
    set mean_p [expr $mean_p/(1.0*[llength $scaled_pressure])]

    puts [format "%20s %10.2f" "diastolic pressure:" $diastolic_pressure]
    puts [format "%20s %10.2f" "mean pressure:" $mean_p]
    puts [format "%20s %10.2f" "systolic pressure:" $systolic_pressure]
    puts [format "%20s %10.2f" "pulse pressure:" $pulse_pressure]
    puts [format "%20s %10.2f" "mean flow:" $mean_q]
    
}


proc post_tweak_pulsatile_bc {solid_fn mesh_surf_dir mesh_fn res_fns faceinfo out_fn} {

  set solid /tmp/post_tweak_steady_bc/solid
  set mesh  /tmp/post_tweak_steady_bc/mesh
  set res   /tmp/post_tweak_steady_bc/res

  catch {repos_delete -obj $solid}
  catch {repos_delete -obj $mesh}
  catch {repos_delete -obj $res}
  repos_deleteList [repos_subList /tmp/post_tweak_steady_bc/face/*]

  # load the solid model
  solid_readNative -file $solid_fn -obj $solid

  # loop over the named faces on the specified solid model
  set faces {}
  foreach id [$solid GetFaceIds] {
     set face [$solid GetFaceAttr -attr gdscName -faceId $id]
     if {$face != ""} {
       lappend faces $face
       set unitOutwardNormal [$solid GetFaceNormal -face $id -u 0 -v 0]
       set unitOutwardNormal [math_normalize $unitOutwardNormal]
       set normals($face) $unitOutwardNormal
     }
   }

   if {[llength $faces] == 0} {
     return -code error "ERROR:  no named faces found on solid."
   }

   # read vis file mesh
   global guiVISvars
   puts "Reading mesh..."
   if [catch {post_readVisMesh -file $mesh_fn -obj $mesh}] {
      return -code error "COULD NOT READ THE MESH."
   }
   set guiVISvars(mesh_vtk_obj) [repos_exportToVtk -src $mesh]
   set theMesh $guiVISvars(mesh_vtk_obj)

   # read in the vtk objects for the named faces
 
   foreach face $faces {
      set fn [file join $mesh_surf_dir $face.vtk]
      #puts "Reading ($face) from file ($fn)."
      repos_readVtkPolyData -file $fn -obj /tmp/post_tweak_steady_bc/face/$face
   }

   ## first read in all the results from file

   foreach stepfn $res_fns {

     catch {repos_delete -obj $res}
     # read vis results file
     if [catch {post_readVisRes -file $stepfn -result $res -grid $mesh} msg] {
       return -code error "Could not read vis result file ($msg)."
     }

     # associate it with the mesh
     set vtkpd [repos_exportToVtk -src $res]

     [$theMesh GetPointData] SetScalars [[$vtkpd GetPointData] GetScalars]
     [$theMesh GetPointData] SetVectors [[$vtkpd GetPointData] GetVectors]
     $theMesh Update

     foreach fi $faceinfo {
       set face  [lindex $fi 0]
       set id    [lindex $fi 1]
       set dflow [lindex $fi 2]
       set flow {}
       set pressure {}
       guiVISflowThruFace /tmp/post_tweak_steady_bc/face/$face $normals($face) flow flow
       lappend flow_results($face) $flow
       #puts "face flow ($face): $flow" 
       guiVISflowThruFace /tmp/post_tweak_steady_bc/face/$face $normals($face) pressure pressure
       lappend pressure_results($face) $pressure
       #puts "face pressure ($face): $pressure"
     }

   }

   ## now find average values
   foreach fi $faceinfo {
       set face  [lindex $fi 0]
       set id    [lindex $fi 1]
       set dflow [lindex $fi 2]
       set flow 0
       set pressure 0
       #puts "flow_results($face): $flow_results($face)"
       foreach val $flow_results($face) {
	   set flow [expr $flow + $val]
       }
       #puts "pressure_results($face): $pressure_results($face)"
       foreach val $pressure_results($face) {
           set pressure [expr $pressure + $val]
       }
       set max_pressure($face) [lindex [lsort -real -increasing $pressure_results($face)] end]
       set min_pressure($face) [lindex [lsort -real -increasing $pressure_results($face)] 0]

       set flow [expr $flow / double([llength $flow_results($face)])]
       set pressure [expr $pressure / double([llength $pressure_results($face)])]

       set avg_flow_results($face) $flow
       #puts "avg face flow ($face): $flow" 
       set avg_pressure_results($face) $pressure
       #puts "avg face pressure ($face): $pressure"
       set resistance [expr double($pressure)/double($flow)]
       set resistance_results($face) $resistance
       set desired_r  [expr double($pressure)/double($dflow)]
       set desired_r_results($face) $desired_r
       set r_for_solver($id) $desired_r
       set comments_for_solver($id) "\#\#  [format %10s $id] == [format %30s $face] ([expr round($desired_r)])"
       #puts "desired_r ($face): $desired_r"

   }

   set fp [open $out_fn w]
   fconfigure $fp -translation lf
   puts $fp "#"
   puts $fp [format "\#\#%30s %10s %15s %15s %15s %15s %15s %15s %15s " face_name id min_P mean_P max_P flow actual_resistance desired_flow suggested_resistance]
   foreach fi $faceinfo {
       set face  [lindex $fi 0]
       set id    [lindex $fi 1]
       set dflow [lindex $fi 2]
       puts $fp [format "\#\#%30s %10i %15.5g %15.5g %15.5g %15.5g %15.5g %15.5g %15.5g " $face $id [expr $min_pressure($face) / 1333.33] [expr $avg_pressure_results($face) / 1333.33] [expr $max_pressure($face) / 1333.33] $avg_flow_results($face) $resistance_results($face) $dflow $desired_r_results($face)]
   }
   puts $fp "#"
   puts $fp [format "\#\#%30s %10s %15s %15s %15s %15s %15s %15s %15s " face_name id min_P mean_P max_P flow actual_resistance desired_flow suggested_resistance]
   foreach fi $faceinfo {
       set face  [lindex $fi 0]
       set id    [lindex $fi 1]
       set dflow [lindex $fi 2]
       puts $fp [format "\#\#%30s %10i %15.5g %15.5g %15.5g %15.5g %15.5g %15.5g %15.5g " $face $id [expr $min_pressure($face)] [expr $avg_pressure_results($face)] [expr $max_pressure($face)] $avg_flow_results($face) $resistance_results($face) $dflow $desired_r_results($face)]
   }
   puts $fp "\#\n\#"
   puts $fp [format "\#\#%30s %10s %10s %10s %10s" face_name id flow desired_flow error(%)]
   foreach fi $faceinfo {
       set face  [lindex $fi 0]
       set id    [lindex $fi 1]
       set dflow [lindex $fi 2]
       puts $fp [format "\#\#%30s %10i %10.3g %10.3g %10.2f" $face $id $avg_flow_results($face) $dflow [expr ($dflow-$avg_flow_results($face))/$dflow*100.0]]
   }
  
  puts $fp "\#\n\#"
  puts $fp "\#\#\# for solver"
  set sorted_ids [lsort -integer -increasing [array names r_for_solver]]
  foreach i $sorted_ids  {
      puts $fp "$comments_for_solver($i)"
  }
  puts $fp "\#\#\#"
  puts $fp "Number of Resistance Surfaces: [llength $sorted_ids]"	
  puts $fp "List of Resistance Surfaces: $sorted_ids"
  puts -nonewline $fp "Resistance Values: "
  foreach i [lsort -integer -increasing [array names r_for_solver]] {
      puts -nonewline $fp "[expr round($r_for_solver($i))] "
  }
  puts $fp ""
  close $fp
 
}


proc math_factorial {number} {

  #-------------------------------
  # calculate factorial: number! 
  # -------------------------------

  set s 1
  for {set i 1} {$i <= $number} {incr i} {
    set s [expr $s*$i]
  }

  return $s

}


proc math_matrix_add {mat_A mat_B} {

  #-------------------------------
  # Add two 3x3 matrices
  # -------------------------------

  set mat_A_r1 [lindex $mat_A 0]
  set mat_A_r2 [lindex $mat_A 1]
  set mat_A_r3 [lindex $mat_A 2]

  set mat_B_r1 [lindex $mat_B 0]
  set mat_B_r2 [lindex $mat_B 1]
  set mat_B_r3 [lindex $mat_B 2]

  set a_r1 [math_addVectors $mat_A_r1 $mat_B_r1]
  set a_r2 [math_addVectors $mat_A_r2 $mat_B_r2]
  set a_r3 [math_addVectors $mat_A_r3 $mat_B_r3]

  set mat "{$a_r1} {$a_r2} {$a_r3}"

  return $mat

}


proc math_matrix_exp {mat_A n} {

  #-------------------------------
  # Calculate matrix exponential e^M
  # -------------------------------

  set mat_s {{1 0 0} {0 1 0} {0 0 1}}

  for {set i 1} {$i <= $n} {incr i} {
    set mat1 [math_matrix_power $mat_A $i]
    set scale [expr 1.0/[math_factorial $i]]
    set mat2 [math_matrix_mult_scale $mat1 $scale]
    set mat_s [math_matrix_add $mat_s $mat2]
  }

  return $mat_s

}


proc math_matrix_mult {mat_A mat_B} {

  #-------------------------------
  # Multiply two 3x3 matrices
  # -------------------------------

  set mat_A_r1 [lindex $mat_A 0]
  set mat_A_r2 [lindex $mat_A 1]
  set mat_A_r3 [lindex $mat_A 2]

  set mat_B_c1 "[lindex $mat_B {0 0}] [lindex $mat_B {1 0}] [lindex $mat_B {2 0}]"
  set mat_B_c2 "[lindex $mat_B {0 1}] [lindex $mat_B {1 1}] [lindex $mat_B {2 1}]"
  set mat_B_c3 "[lindex $mat_B {0 2}] [lindex $mat_B {1 2}] [lindex $mat_B {2 2}]"

  set a11 [math_dot $mat_A_r1 $mat_B_c1]
  set a12 [math_dot $mat_A_r1 $mat_B_c2]
  set a13 [math_dot $mat_A_r1 $mat_B_c3]
  set a21 [math_dot $mat_A_r2 $mat_B_c1]
  set a22 [math_dot $mat_A_r2 $mat_B_c2]
  set a23 [math_dot $mat_A_r2 $mat_B_c3]
  set a31 [math_dot $mat_A_r3 $mat_B_c1]
  set a32 [math_dot $mat_A_r3 $mat_B_c2]
  set a33 [math_dot $mat_A_r3 $mat_B_c3]

  set mat "{$a11 $a12 $a13} {$a21 $a22 $a23} {$a31 $a32 $a33}"
  return $mat

}


proc math_matrix_mult_3x1 {mat_A mat_B} {

  #-------------------------------
  # Multiply 3x3 matrix by 3x1 matrix
  # -------------------------------

  set mat_A_r1 [lindex $mat_A 0]
  set mat_A_r2 [lindex $mat_A 1]
  set mat_A_r3 [lindex $mat_A 2]

  set a1 [math_dot $mat_A_r1 $mat_B]
  set a2 [math_dot $mat_A_r2 $mat_B]
  set a3 [math_dot $mat_A_r3 $mat_B]

  set mat "$a1 $a2 $a3"
  return $mat

}


proc math_matrix_mult_scale {mat_A scale} {

  #-------------------------------
  # Multiply 3x3 matrix by scalar
  # -------------------------------

  set mat_A_r1 [lindex $mat_A 0]
  set mat_A_r2 [lindex $mat_A 1]
  set mat_A_r3 [lindex $mat_A 2]

  set a_r1 [math_scaleVec $mat_A_r1 $scale]
  set a_r2 [math_scaleVec $mat_A_r2 $scale]
  set a_r3 [math_scaleVec $mat_A_r3 $scale]

  set mat "{$a_r1} {$a_r2} {$a_r3}"
  return $mat

}


proc math_matrix_power {mat n} {

  #-------------------------------
  # Calculate n-power of 3x3 matrices
  # -------------------------------

  set n_minus [expr $n-1]
  set mat_s $mat
  for {set i 0} {$i < $n_minus} {incr i} {
    set mat_s [math_matrix_mult $mat $mat_s]
  }

  return $mat_s

}


proc calc_rotation_matrix {vector th n} {

  #-------------------------------
  # calculate rotation matrix based on screw motion
  # vector: rotation axis
  # th: rotation angle in radian
  # n: number of Taylor expansion
  #-------------------------------

  set unit_w [math_normalize $vector]
  set w11 0
  set w12 [expr -1*[lindex $unit_w 2]]
  set w13 [lindex $unit_w 1]

  set w21 [lindex $unit_w 2]
  set w22 0
  set w23 [expr -1*[lindex $unit_w 0]]

  set w31 [expr -1*[lindex $unit_w 1]]
  set w32 [lindex $unit_w 0]
  set w33 0

  set w "{$w11 $w12 $w13} {$w21 $w22 $w23} {$w31 $w32 $w33}"
  set th_rad [math_degToRad $th]
  set w_th [math_matrix_mult_scale $w $th_rad]

  #w = [ 0 -unit_w(3) unit_w(2)
  #    unit_w(3) 0 -unit_w(1)
  #    -unit_w(2) unit_w(1) 0]*th;
  #screw_mat = real(exp(1)^w);

  set screw_mat [math_matrix_exp $w_th $n]

  return $screw_mat

}

proc create_pvd {insrc outfn} {

  if {[llength $insrc] == 1} {
    if {[file isdirectory $insrc]} {
      set insrc [glob [file join $insrc *.vtu]]
    }
  }
  set fp [open $outfn w]
  puts $fp {<?xml version="1.0"?>}
  puts $fp {<VTKFile type="Collection" version="0.1" byte_order="LittleEndian" compressor="vtkZLibDataCompressor">}
  puts $fp "<Collection>"
  set timestep 0
  foreach fn [lsort -dictionary $insrc] {
    puts $fp "  <DataSet timestep=\"$timestep\" file=\"$fn\"/>"
    incr timestep
  }
  puts $fp "</Collection>"
  puts $fp "</VTKFile>"

  close $fp
}


proc post_TKE {fullsimdir filePrefix startStepNo stopStepNo outerIncr maxStepNo periodIncr fulloutdir} {

  set myMeshRepos /tmp/tke/mesh
  set myResRepos  /tmp/tke/res/
  set myTKERepos  /tmp/tke/rms

  catch {repos_delete -obj $myMeshRepos}
  catch {repos_deleteList [repos_subList $myResRepos*]}

  post_readVisMesh -file [file join $fullsimdir $filePrefix\_mesh.vis.gz] -obj $myMeshRepos
  set theMesh [repos_exportToVtk -src $myMeshRepos]

  set final_cycle_step_num 0

  for {set outer_loop $startStepNo} {$outer_loop <= $stopStepNo} {incr outer_loop $outerIncr} {

    puts "outer_loop:  $outer_loop\n\t"
    for {set i $outer_loop} {$i <= $maxStepNo} {incr i $periodIncr} {
      catch {repos_delete -obj $myResRepos$i}
      puts -nonewline " $i"
      set res_name $myResRepos$i
      post_readVisRes -file [file join $fullsimdir $filePrefix\_res$i.vis.gz] -grid $myMeshRepos -result $res_name
    }
    puts "\n"

    catch {repos_delete -obj $myTKERepos}
    post_calcTKE -inputPdList [repos_subList $myResRepos*] -result $myTKERepos
 
    set pressure [[[repos_exportToVtk -src $res_name] GetPointData] GetScalars]
    set velocity [[[repos_exportToVtk -src $res_name] GetPointData] GetVectors]
    set kinetic_energy [[[repos_exportToVtk -src $myTKERepos] GetPointData] GetScalars]
    set fluctuation_velocity [[[repos_exportToVtk -src $myTKERepos] GetPointData] GetVectors]
    #puts "kinetic_energy: $kinetic_energy"
    #puts "fluctation_velocity: $fluctuation_velocity"
    $pressure SetName pressure
    $velocity SetName velocity
    $kinetic_energy SetName kinetic_energy
    $fluctuation_velocity SetName fluctuation_velocity
    [$theMesh GetPointData] SetScalars $pressure
    [$theMesh GetPointData] SetVectors $velocity
    [$theMesh GetPointData] AddArray $kinetic_energy
    [$theMesh GetPointData] AddArray $fluctuation_velocity
    $theMesh Update

    catch {xwrite Delete}
    vtkXMLDataSetWriter xwrite
    xwrite SetInputDataObject $theMesh
    xwrite SetFileName [file join $fulloutdir $filePrefix-tke-[format "%05i" $final_cycle_step_num].vtu]
    xwrite Write
    xwrite Delete

    incr final_cycle_step_num

    [$theMesh GetPointData] RemoveArray pressure
    [$theMesh GetPointData] RemoveArray velocity
    [$theMesh GetPointData] RemoveArray kinetic_energy
    [$theMesh GetPointData] RemoveArray fluctuation_velocity

    repos_deleteList [repos_subList $myResRepos*]
    repos_delete -obj $myTKERepos

    # cant figure out why I need to explicitly delete these
    # to prevent memory leak
    catch {$kinetic_energy Delete}
    catch {$fluctuation_velocity Delete}
  }

  repos_delete -obj $myMeshRepos
  create_pvd [glob [file join $fulloutdir $filePrefix-tke-*.vtu]] [file join $fulloutdir $filePrefix-tke.pvd]

}
