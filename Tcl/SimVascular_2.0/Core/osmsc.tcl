# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved.
#
# Portions of this code Copyright (c) 2012 Open Source Medical Software Corporation.
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

proc vtu_extractFace {vtuObjs faceObj readFiles args} {

  # note: readFiles == 0, just use objects
  #       readFiles != 1, all operations done assuming filenames
  #       optOutFn = optional file written out
  #       faceObj is INPUT & OUTPUT (i.e. MODIFIED) by this routine!

  if {$args != ""} {
    set optOutFn $args
  } else {
    set optOutFn {}
  }

  if $readFiles {
    set realFaceObj tmp-inflow-face
    catch {repos_delete -obj $realFaceObj}
    repos_readVtkPolyData -file $faceObj -obj $realFaceObj
    [[[repos_exportToVtk -src $realFaceObj] GetPointData] GetScalars] SetName "GlobalNodeID"     
  } else {
    set realFaceObj $faceObj
  }
 
  set numPtsFace   [geom_numPts -obj $realFaceObj]
  set nodeIds [[[repos_exportToVtk -src $realFaceObj] GetPointData] GetScalars]

  foreach vtuObj $vtuObjs {

    if $readFiles {
       puts "Load mesh ($vtuObj)..."
       shortwait
       set ug_reader tmp-ug_reader
       catch {$ug_reader Delete}
       vtkXMLUnstructuredGridReader $ug_reader
       $ug_reader SetFileName $vtuObj
       $ug_reader Update
       set ug tmp-ug-[file rootname [file tail $vtuObj]]
       catch {$ug Delete}
       vtkUnstructuredGrid $ug
       $ug ShallowCopy [$ug_reader GetOutput]
       $ug_reader Delete
       set prettyname [file rootname [file tail $vtuObj]]
    } else {
       puts "Working with ($vtuObj)..."
       set ug $vtuObj
       set prettyname [lindex [split $vtuObj -] end]
    }

    set pointData [$ug GetPointData]
    #set velocities [$pointData GetArray velocity]
    #set pressures  [$pointData GetArray pressure]
    set velocities [$pointData GetVectors]
    set pressures  [$pointData GetScalars]

    set myvel timepoint_vel_$prettyname
    set prettynameV velocity_$prettyname
    set myp timepoint_p_$prettyname
    set prettynameP pressure_$prettyname

    catch {$myvel Delete}
    vtkDoubleArray $myvel
    $myvel SetNumberOfComponents 3
    $myvel Allocate 100 100
    $myvel SetName $prettynameV

    catch {$myp Delete}
    vtkDoubleArray $myp
    $myp SetNumberOfComponents 1
    $myp Allocate 100 100
    $myp SetName $prettynameP

    for {set i 0} {$i < $numPtsFace} {incr i} {
       set node [expr int([$nodeIds GetValue $i])]
       # nodes in vtk start at 0, so subtract 1 off
       set node [expr $node - 1]
       set vec [$velocities GetTuple3 $node]
       set p   [$pressures GetTuple1 $node]
       #puts "processing vtk node: $node vec: $vec"
       $myvel InsertNextTuple3 [lindex $vec 0] [lindex $vec 1] [lindex $vec 2]
       $myp   InsertNextTuple1 $p
    }

    [[repos_exportToVtk -src $realFaceObj] GetPointData] AddArray $myvel
    [[repos_exportToVtk -src $realFaceObj] GetPointData] AddArray $myp
    [[repos_exportToVtk -src $realFaceObj] GetPointData] SetActiveVectors $prettynameV
    [[repos_exportToVtk -src $realFaceObj] GetPointData] SetActiveScalars $prettynameP

  }

  if {$optOutFn != ""} {
    repos_writeVtkPolyData -file $optOutFn -obj $realFaceObj -type ascii
  }

}

proc vtp_integrateFace {inflowFace rtnP rtnQ rtnA} {

  upvar $rtnP p
  upvar $rtnQ q
  upvar $rtnA a

  set pressures {}
  set velocities {}
  set pointData [[repos_exportToVtk -src $inflowFace] GetPointData]
  for {set i 0} {$i < [$pointData GetNumberOfArrays]} {incr i} {
    set myarray [$pointData GetAbstractArray $i]
    set name [$myarray GetName]
    if {[string range $name 0 8] == "pressure_"} {
      lappend pressures $name
    } elseif {[string range $name 0 8] == "velocity_"} {
      lappend velocities $name
    }
  }
  set pressures [lsort -dictionary $pressures]
  set velocities [lsort -dictionary $velocities]

  #puts "$pressures"
  #puts "$velocities"

  foreach pressure $pressures {
    $pointData SetActiveScalars $pressure
    set presarea [geom_integrateSurface2 -obj $inflowFace -tensorType 0]
    set force [lindex $presarea 0]
    set area [lindex $presarea 1]
    # intentionally divide by area for pressure calc
    set a([string range $pressure 9 end]) $area
    set p([string range $pressure 9 end]) [expr $force / $area]
  }

  foreach velocity $velocities {
    $pointData SetActiveVectors $velocity
    set fluxarea [geom_integrateSurface2 -obj $inflowFace -tensorType 1]
    set q([string range $velocity 9 end]) [lindex $fluxarea 0]
  }

}

proc bctdat_to_vtp {bctdatFileName inflowFaceFileName old_units new_units shift outFaceFileName outFlowFileName} {

  set inflowFace tmp-inflow-face
  catch {repos_delete -obj $inflowFace}
  set reorientedPD /tmp/bctdat_to_vtp/inflow-face-reoriented
  catch {repos_delete -obj $reorientedPD}
  set reoriented  tmp-bctdat_to_vtp-inflow-face-reoriented
  catch {$reoriented Delete}

  repos_readXMLPolyData $inflowFaceFileName $inflowFace
  [[[repos_exportToVtk -src $inflowFace] GetPointData] GetScalars] SetName "GlobalNodeID"

  set nmpl tmp-nonmergingpointlocator
  catch {$nmpl Delete}
  vtkNonMergingPointLocator $nmpl
  $nmpl SetDataSet [repos_exportToVtk -src $inflowFace]
  $nmpl Initialize
  $nmpl BuildLocator

  # check the number of points
  set numPtsFace   [geom_numPts -obj $inflowFace]

  set fp [open $bctdatFileName r]
  gets $fp line
  set numPtsBctDat [lindex $line 0]
  set numSamplePts [lindex $line 1] 

  if {$numPtsBctDat != $numPtsFace} {
     close $fp
     return -code error "ERROR:  mismatch in num pts ($numPtsBctDat != $numPtsFace)"
  }

  catch {unset nodeVelocity}

  for {set n 0} {$n < $numPtsBctDat} {incr n} {
    gets $fp line
    set ptx [lindex $line 0]
    set pty [lindex $line 1]
    set ptz [lindex $line 2]

    set numSamplePts [lindex $line 3]
    set localNodeID [$nmpl FindClosestPoint $ptx $pty $ptz]
    #puts "found local $localNodeID"
    catch {unset vel}
    catch {unset mytime}
    for {set j 0} {$j < $numSamplePts} {incr j} {
      gets $fp line
      lappend vel [lrange $line 0 2]
      lappend mytime [lindex $line 3]
    }
    set nodeVelocity($localNodeID) $vel  
  }
  close $fp

  for {set i 0} {$i < [llength $mytime]} {incr i} {
    set myvel timepoint_[format %04i $i]
    set prettyname "velocity_[format %06.4f [lindex $mytime $i]]"
    catch {$myvel Delete}
    vtkDoubleArray $myvel
    $myvel SetNumberOfComponents 3
    $myvel Allocate 100 100
    $myvel SetName $prettyname
    foreach j [lsort -integer [array names nodeVelocity]] {
      set v [lindex $nodeVelocity($j) $i]
      $myvel InsertNextTuple3 [lindex $v 0] [lindex $v 1] [lindex $v 2]
    }
    [[repos_exportToVtk -src $inflowFace] GetPointData] AddArray $myvel
    [[repos_exportToVtk -src $inflowFace] GetPointData] SetActiveVectors $prettyname
  }

  set stress_mags {}
  set stress_vectors {}

  catch {$outobj Delete}

  vtkobj_translate_shift [repos_exportToVtk -src $inflowFace] $old_units $new_units $shift $stress_mags $stress_vectors $reoriented

  repos_writeXMLPolyData $reoriented $outFaceFileName
  repos_importVtkPd -src $reoriented -dst $reorientedPD

  vtp_integrateFace $reorientedPD p q a
  set fp [open $outFlowFileName w]
  puts $fp "\# bct.dat filename: $bctdatFileName"
  puts $fp "\# face: $inflowFaceFileName"
  foreach i [lsort -dictionary [array names q]] {
      puts $fp "$i\t$q($i)"
  }
  close $fp

}


proc vtu_calc_flow_thru_faces {vtuFileNames mesh_surface_vtk_filenames skipWalls outFlowFileName outPressureFileName} {

  catch {unset face_file face_objs}
  set all_faces {}
  set tmpid 0

  #
  #  read non-wall faces
  #

  foreach facefile $mesh_surface_vtk_filenames  {
    set facename [file rootname [file tail $facefile]]
    if $skipWalls {
      if {[string range $facename 0 3] == "wall"} continue
    }
    lappend all_faces $facename
    set face_obj tmp-face-obj-$tmpid
    incr tmpid
    catch {repos_delete -obj $face_obj}
    repos_readVtkPolyData -file $facefile -obj $face_obj
    [[[repos_exportToVtk -src $face_obj] GetPointData] GetScalars] SetName "GlobalNodeID" 
    set face_file($facename) $facefile
    set face_objs($facename) $face_obj
  }

  set all_faces [lsort -dictionary $all_faces]

  #
  #  read sim results
  #

  set sim_objs {}

  catch {unset pretty_names}

  foreach vtuObj $vtuFileNames {
    puts "Load mesh ($vtuObj)..."
    set ug_reader tmp-ug_reader
    catch {$ug_reader Delete}
    vtkXMLUnstructuredGridReader $ug_reader
    $ug_reader SetFileName $vtuObj
    $ug_reader Update
    set ug tmp-ug-[file rootname [file tail $vtuObj]]
    catch {$ug Delete}
    vtkUnstructuredGrid $ug
    $ug ShallowCopy [$ug_reader GetOutput]
    $ug_reader Delete
    set pretty_names($ug) [file rootname [file tail $vtuObj]]
    lappend sim_objs $ug
  }
 
  set sim_objs [lsort -dictionary $sim_objs]

  #
  #  loop over faces
  #

  foreach face $all_faces {
    puts "Working on face ($face)..."
    vtu_extractFace $sim_objs $face_objs($face) 0
    catch {unset p_$face q_$face a_$face}
    vtp_integrateFace $face_objs($face) p_$face q_$face a_$face
  }

  set flowfp [open $outFlowFileName w]
  set presfp [open $outPressureFileName w]

  puts -nonewline $flowfp "step\t"
  puts -nonewline $presfp "step\t"

  foreach face $all_faces {
    puts -nonewline $flowfp "$face\t"
    puts -nonewline $presfp "$face\t"
  }
  puts $flowfp ""
  puts $presfp ""

  foreach myrow $sim_objs {
    puts -nonewline $flowfp "$pretty_names($myrow)\t"
    puts -nonewline $presfp "$pretty_names($myrow)\t"
    foreach face $all_faces {
	eval puts -nonewline $flowfp \"\$q_$face\($myrow\)\\t\"
	eval puts -nonewline $presfp \"\$p_$face\($myrow\)\\t\"
    }
    puts $flowfp ""
    puts $presfp ""
  }

  close $flowfp
  close $presfp

  # delete loaded meshes
  foreach i $sim_objs {
    $i Delete
  }

  # delete loaded mesh surfaces
  foreach i [array names face_objs] {
    repos_delete -obj $face_objs($i)
  }

}


proc vtu_combine_steps {vtuFileNames tractionName inflowFaceFileName outFileName} {

  set needMesh 1

  set ug tmp-combo-ug
  catch {$ug Delete}
  vtkUnstructuredGrid $ug

  foreach vtuObj $vtuFileNames {

    puts "Load mesh ($vtuObj)..."
    set ug_reader tmp-ug_reader
    catch {$ug_reader Delete}
    vtkXMLUnstructuredGridReader $ug_reader
    $ug_reader SetFileName $vtuObj
    $ug_reader Update

    if $needMesh {
      $ug CopyStructure [$ug_reader GetOutput]
      set needMesh 0
    }

    set prettyname [file rootname [file tail $vtuObj]]

    set pointData [[$ug_reader GetOutput] GetPointData]
    [$pointData GetArray pressure] SetName pressure_$prettyname  
    [$pointData GetArray velocity] SetName velocity_$prettyname
    [$pointData GetArray $tractionName] SetName WSS_$prettyname

    [$ug GetPointData] AddArray [$pointData GetArray pressure_$prettyname]
    [$ug GetPointData] AddArray [$pointData GetArray velocity_$prettyname]
    [$ug GetPointData] AddArray [$pointData GetArray WSS_$prettyname]

    $ug_reader Delete

  }

  set ug_writer tmp-ug_writer
  catch {$ug_writer Delete}
  vtkXMLUnstructuredGridWriter $ug_writer
  $ug_writer SetCompressorTypeToZLib
  $ug_writer EncodeAppendedDataOff
  $ug_writer SetFileName $outFileName
  $ug_writer SetInputDataObject $ug
  $ug_writer Write

  $ug_writer Delete
  $ug Delete

}


proc vtp_momentumInFace {faceFileName rho outFileName} {

  set inflow_face_no_normals tmp-inflow-no-normals
  set inflow_face tmp-inflow-face
  set mycalc tmp-calc-energy
  set aa tmp-energy-aa
  set output_obj tmp-output-obj

  catch {repos_delete -obj $inflow_face_no_normals}
  catch {repos_delete -obj $inflow_face}
  catch {$mycalc Delete}
  catch {$aa Delete}
  catch {repos_delete -obj $output_obj}

  repos_readVtkPolyData -file $faceFileName -obj $inflow_face_no_normals
  geom_createNormals $inflow_face_no_normals $inflow_face

  set pointData [[repos_exportToVtk -src $inflow_face] GetPointData]

  vtkArrayCalculator $mycalc
  $mycalc SetAttributeModeToUsePointData
  $mycalc SetInputDataObject [repos_exportToVtk -src $inflow_face]

  set pid 0
  set vid 0

  for {set i 0} {$i < [$pointData GetNumberOfArrays]} {incr i} {
    set abstractArrayName [[$pointData GetAbstractArray $i] GetName]
    if {[string range $abstractArrayName 0 8] == "pressure_"} {
      #puts "$abstractArrayName"
      $mycalc AddScalarVariable s$pid $abstractArrayName 0
      incr pid 
    } elseif {[string range $abstractArrayName 0 8] == "velocity_"} {
      #puts "$abstractArrayName"
      $mycalc AddVectorVariable v$vid $abstractArrayName 0 1 2
      incr vid 
    }
  }

  $mycalc AddVectorVariable n Normals 0 1 2

  #vmag = sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2])
  #VdotN = nrm[0]*v[0]+nrm[1]*v[1]+nrm[2]*v[2]
  #uvalues[j] = ( p + 0.5 * rho * vmag * vmag ) * VdotN

  set myfunc "( 0 "
  for {set i 0} {$i < $pid} {incr i} {
    set myfunc "$myfunc + ((s$i + 0.5 * $rho * mag(v$i) * mag(v$i)) * v$i.n)"
  }
  set myfunc "$myfunc ) / $pid"

  #puts "myfunc: $myfunc"

  $mycalc SetFunction $myfunc
  $mycalc SetResultArrayName momentum_average
  $mycalc Update

  vtkAssignAttribute $aa
  $aa SetInputConnection [$mycalc GetOutputPort]
  $aa Assign momentum_average SCALARS POINT_DATA
  $aa Update

  repos_importVtkPd -src [$aa GetOutput] -dst $output_obj
  repos_writeVtkPolyData -file $outFileName -obj $output_obj -type ascii

}


proc vtu_create_wss_osi_pulse {vtu_files tractionName wssFN osiFN pulseFN} {

  set reader  myreader
  set filter  myfilter
  set cleaner mycleaner

  set wss   tmp-vtu_create_wss_osi_pulse-wss
  set pulse tmp-vtu_create_wss_osi_pulse-pulse
  set osi   tmp-vtu_create_wss_osi_pulse-osi

  catch {repos_delete -obj $wss}
  catch {repos_delete -obj $pulse}
  catch {repos_delete -obj $osi}

  set shearPds {}

  foreach fn [lsort -dictionary $vtu_files] {

    puts "Working with file ($fn)"
    shortwait

    catch {$reader Delete}
    catch {$filter Delete}
    catch {$cleaner Delete}

    set objname /tmp/wsspd/$fn

    catch {repos_delete -obj $objname}

    vtkXMLUnstructuredGridReader $reader
    $reader SetFileName $fn
    $reader Update

    vtkGeometryFilter $filter
    $filter MergingOff
    $filter SetInputDataObject [$reader GetOutput]
    $filter Update

    vtkCleanPolyData $cleaner
    $cleaner PointMergingOff
    $cleaner PieceInvariantOff
    $cleaner SetInputDataObject [$filter GetOutput]
    $cleaner Update

    # replace vectors with traction 
    [[$cleaner GetOutput] GetPointData] SetActiveVectors $tractionName 
    repos_importVtkPd -src [$cleaner GetOutput] -dst $objname

    lappend shearPds $objname 
  }

  puts "calculating wss..."
  shortwait
  post_calcWallShearMean -shearPdList $shearPds -surfaceMesh [lindex $shearPds 0] -result $wss
  puts "calculating pulse..."
  shortwait
  post_calcWallShearPulse -shearPdList $shearPds -surfaceMesh [lindex $shearPds 0] -result $pulse
  puts "calculating osi..."
  shortwait
  post_calcOSI -meanPd $wss -pulsePd $pulse -result $osi -surfaceMesh $wss

  repos_writeVtkPolyData -obj $wss -type ascii -file $wssFN
  repos_writeVtkPolyData -obj $pulse -type ascii -file $pulseFN
  repos_writeVtkPolyData -obj $osi -type ascii -file $osiFN

  $reader Delete
  $filter Delete
  $cleaner Delete

  foreach i $shearPds {
    repos_delete -obj $i
  }

  repos_delete -obj $wss
  repos_delete -obj $pulse
  repos_delete -obj $osi

}


proc vtp_create_wss_osi_pulse {vtp_file tractionName outFN} {

  set wss   tmp-vtu_create_wss_osi_pulse-wss
  set pulse tmp-vtu_create_wss_osi_pulse-pulse
  set osi   tmp-vtu_create_wss_osi_pulse-osi
  set outpd tmp-vtu_create_wss_osi_pulse-output-pd

  catch {repos_delete -obj $wss}
  catch {repos_delete -obj $pulse}
  catch {repos_delete -obj $osi}
  catch {$outpd Delete}

  set shearPds {}

  set svpd /tmp/vtk_create_wss_osi_pulse/inpd
  catch {repos_delete -obj $svpd}
  repos_readXMLPolyData $vtp_file $svpd
  set vtkpd [repos_exportToVtk -src $svpd]

  set pointData [$vtkpd GetPointData]

  # create shell output object
  vtkPolyData $outpd
  $outpd CopyStructure $vtkpd
  [$outpd GetPointData] AddArray [$pointData GetAbstractArray "GlobalNodeID"]
  [$outpd GetCellData] AddArray [[$vtkpd GetCellData] GetAbstractArray "GlobalElementID"]

  # hack to keep the avg pressures since I want them
  [$outpd GetPointData] AddArray [$pointData GetAbstractArray "pressure_avg_mmHg"]

  for {set i 0} {$i < [$pointData GetNumberOfArrays]} {incr i} {

    set abstractArrayName [[$pointData GetAbstractArray $i] GetName]

    if {[string range $abstractArrayName 0 [string length $tractionName]] == "$tractionName\_"} {

      puts "$abstractArrayName"
      set objname /tmp/wsspd/$i
      catch {repos_delete -obj $objname}
      set pd tmp-wsspd-$i
      vtkPolyData $pd
      $pd CopyStructure $vtkpd
      [$pd GetPointData] AddArray [$pointData GetAbstractArray $i]

      # replace vectors with traction 
      [$pd GetPointData] SetActiveVectors $abstractArrayName
 
      repos_importVtkPd -src $pd -dst $objname

      $pd Delete

      lappend shearPds $objname

    }

  }

  puts "calculating wss..."
  shortwait
  post_calcWallShearMean -shearPdList $shearPds -surfaceMesh [lindex $shearPds 0] -result $wss
  puts "calculating pulse..."
  shortwait
  post_calcWallShearPulse -shearPdList $shearPds -surfaceMesh [lindex $shearPds 0] -result $pulse
  puts "calculating osi..."
  shortwait
  post_calcOSI -meanPd $wss -pulsePd $pulse -result $osi -surfaceMesh $wss

  [[[repos_exportToVtk -src $wss] GetPointData] GetScalars] SetName "TAWSS"
  [$outpd GetPointData] AddArray [[[repos_exportToVtk -src $wss] GetPointData] GetScalars]

  [[[repos_exportToVtk -src $pulse] GetPointData] GetScalars] SetName "shear_pulse"
  [$outpd GetPointData] AddArray [[[repos_exportToVtk -src $pulse] GetPointData] GetScalars]

  [[[repos_exportToVtk -src $osi] GetPointData] GetScalars] SetName "OSI"
  [$outpd GetPointData] AddArray [[[repos_exportToVtk -src $osi] GetPointData] GetScalars]

  repos_writeXMLPolyData $outpd $outFN

  foreach i $shearPds {
    repos_delete -obj $i
  }

  repos_delete -obj $wss
  repos_delete -obj $pulse
  repos_delete -obj $osi

  repos_delete -obj $svpd

  $outpd Delete

}


# ----------------------------
# geom_scale
# ----------------------------

proc geom_scale {src scale dst} {

  #@author Nathan Wilson
  #@c transform a src PolyData with an isotropic scaling.
  #@a src: input PolyData object.
  #@a dst: output PolyData object.
  #@a scale:  scale factor.

  set t tmp-geom_scale-trans
  set f tmp-geom_scale-filt

  catch {$t Delete}
  catch {$f Delete}

  vtkTransform $t
  $t Scale $scale $scale $scale

  vtkTransformPolyDataFilter $f
  $f SetTransform $t
  $f SetInputDataObject [repos_exportToVtk -src $src]
  $f Update
  repos_importVtkPd -src [$f GetOutput] -dst $dst

  $t Delete
  $f Delete

}



proc vtu_extractFaceFromFiles {vtuFileName faceFileName faceObj} {

  # load mesh
  puts "Load mesh ($vtuFileName)..."

  shortwait

  set ug_reader tmp-vtu-extractfacefromfiles-ug_reader
  set ug        tmp-vtu-extractfacefromfiles-ug

  catch {$ug_reader Delete}
  catch {$ug Delete}

  vtkXMLUnstructuredGridReader $ug_reader
  $ug_reader SetFileName $vtuFileName
  $ug_reader Update

  vtkUnstructuredGrid $ug
  $ug ShallowCopy [$ug_reader GetOutput]
  $ug_reader Delete

  vtu_extractFaceFileFromUG $ug $faceFileName $faceObj

  vtu_extractSingleFace $ug [repos_exportToVtk -src $faceObj]
 
  $ug Delete

}


proc vtu_extractFaceFileFromUG {ug faceFileName faceObj} {

  catch {repos_delete -obj $faceObj}
 
  # load mesh face
  puts "Load mesh face ($faceFileName)..."

  repos_readVtkPolyData -file $faceFileName -obj $faceObj

  # old school mesh-surfaces don't have the array named
  [[[repos_exportToVtk -src $faceObj] GetPointData] GetScalars] SetName "GlobalNodeID"

  vtu_extractSingleFace $ug [repos_exportToVtk -src $faceObj]
 
}


proc vtu_extractSingleFace {ug faceObj} {

  set numPtsFace [$faceObj GetNumberOfPoints]
  set nodeIds [[$faceObj GetPointData] GetArray "GlobalNodeID"]

  set pointData [$ug GetPointData]

  # get the complete list of arrays

  set pid 0
  set vid 0

  set scalars {}
  set vectors {}

  for {set i 0} {$i < [$pointData GetNumberOfArrays]} {incr i} {
    set abstractArrayName [[$pointData GetAbstractArray $i] GetName]
    #puts "abstractArrayName: $abstractArrayName"
    if {[string range $abstractArrayName 0 8] == "pressure_"} {
      lappend scalars $abstractArrayName
      incr pid
    } elseif {[string range $abstractArrayName 0 8] == "velocity_"} {
      lappend vectors $abstractArrayName      
      incr vid 
    }
  }

  puts "scalars: $scalars"
  puts "vectors: $vectors"

  foreach s $scalars {

    set myp tmp-singleface-scalars-$s
    catch {$myp Delete}
    vtkDoubleArray $myp
    $myp SetNumberOfComponents 1
    $myp Allocate 100 100
    $myp SetName $s

    set abstractArray [$pointData GetAbstractArray $s]

    for {set i 0} {$i < $numPtsFace} {incr i} {
       set node [expr int([$nodeIds GetValue $i])]
       # nodes in vtk start at 0, so subtract 1 off
       set node [expr $node - 1]
       set p   [$abstractArray GetTuple1 $node]
       $myp   InsertNextTuple1 $p
    }

    [$faceObj GetPointData] AddArray $myp

  }

  foreach v $vectors {

    set myv tmp-singleface-vectors-$v
    catch {$myv Delete}
    vtkDoubleArray $myv
    $myv SetNumberOfComponents 3
    $myv Allocate 100 100
    $myv SetName $v

    set abstractArray [$pointData GetAbstractArray $v]

    for {set i 0} {$i < $numPtsFace} {incr i} {
       set node [expr int([$nodeIds GetValue $i])]
       # nodes in vtk start at 0, so subtract 1 off
       set node [expr $node - 1]
       set vec [$abstractArray GetTuple3 $node]
       $myv InsertNextTuple3 [lindex $vec 0] [lindex $vec 1] [lindex $vec 2]
    }

    [$faceObj GetPointData] AddArray $myv

  }

}


proc vtp_extractSingleFace {pd faceObj} {

  set numPtsFace [$faceObj GetNumberOfPoints]
  set nodeIds {}
  set nodeIds [[$faceObj GetPointData] GetArray "GlobalNodeID"]
  if {$nodeIds == ""} {
      puts "setting name GlobalNodeID to default scalars for ($faceObj)."
      set nodeIds [[$faceObj GetPointData] GetScalars]
      $nodeIds SetName "GlobalNodeID"
  }

  set pointData [$pd GetPointData]

  # get the complete list of arrays

  set pid 0
  set vid 0

  set scalars {}
  set vectors {}

  for {set i 0} {$i < [$pointData GetNumberOfArrays]} {incr i} {
    set abstractArrayName [[$pointData GetAbstractArray $i] GetName]
    #puts "abstractArrayName: $abstractArrayName"
    if {$abstractArrayName == "pressure_avg" || \
	 $abstractArrayName == "pressure_avg_mmHg"} {
      continue
    }
    if {[string range $abstractArrayName 0 8] == "pressure_"} {
      lappend scalars $abstractArrayName
      incr pid
    } elseif {[string range $abstractArrayName 0 8] == "velocity_"} {
      lappend vectors $abstractArrayName      
      incr vid 
    }
  }

  puts "scalars: $scalars"
  puts "vectors: $vectors"

  # need to build an explicit mapping between global node ids
  # and local numbering in pd
  set pd_gids [$pointData GetAbstractArray "GlobalNodeID"]

  for {set i 0} {$i < [$pd GetNumberOfPoints]} {incr i} {
       set gnode [expr int([$pd_gids GetValue $i])]
       set map_local_to_global($i) $gnode
       set map_global_to_local($gnode) $i
    }

  foreach s $scalars {

    set myp tmp-singleface-scalars-$s
    catch {$myp Delete}
    vtkDoubleArray $myp
    $myp SetNumberOfComponents 1
    $myp Allocate 100 100
    $myp SetName $s

    set abstractArray [$pointData GetAbstractArray $s]

    for {set i 0} {$i < $numPtsFace} {incr i} {
       set gnode [expr int([$nodeIds GetValue $i])]
       set node $map_global_to_local($gnode)
       set p   [$abstractArray GetTuple1 $node]
       $myp   InsertNextTuple1 $p
    }

    [$faceObj GetPointData] AddArray $myp

  }

  foreach v $vectors {

    set myv tmp-singleface-vectors-$v
    catch {$myv Delete}
    vtkDoubleArray $myv
    $myv SetNumberOfComponents 3
    $myv Allocate 100 100
    $myv SetName $v

    set abstractArray [$pointData GetAbstractArray $v]

    for {set i 0} {$i < $numPtsFace} {incr i} {
       set gnode [expr int([$nodeIds GetValue $i])]
       set node $map_global_to_local($gnode)
       set vec [$abstractArray GetTuple3 $node]
       $myv InsertNextTuple3 [lindex $vec 0] [lindex $vec 1] [lindex $vec 2]
    }

    [$faceObj GetPointData] AddArray $myv

  }

}


proc vtx_TemporalFlowThruFaces {vtxFileName mesh_surface_vtk_filenames skipWalls sim_units outFlowFileName outPressureFileName outAveragesFileName outAveragesUnitsFileName} {

  catch {unset face_file face_objs}
  set all_faces {}
  set tmpid 0

  #
  #  read non-wall faces
  #

  foreach facefile $mesh_surface_vtk_filenames  {
    set facename [file rootname [file tail $facefile]]
    if $skipWalls {
      if {[string range $facename 0 3] == "wall"} continue
    }
    lappend all_faces $facename
    set face_obj tmp-face-obj-$tmpid
    incr tmpid
    catch {repos_delete -obj $face_obj}
    repos_readXMLPolyData $facefile $face_obj
    set nodeIds {}
    set nodeIds [[[repos_exportToVtk -src $face_obj] GetPointData] GetArray "GlobalNodeID"]
    if {$nodeIds == ""} {
      puts "setting name GlobalNodeID to default scalars for ($face_obj)."
      set nodeIds [[$face_obj GetPointData] GetScalars]
      $nodeIds SetName "GlobalNodeID"
    }
    [[repos_exportToVtk -src $face_obj] GetPointData] SetActiveScalars GlobalNodeID 
    set face_file($facename) $facefile
    set face_objs($facename) $face_obj
  }

  set all_faces [lsort -dictionary $all_faces]

  # load sim results
  if {[file extension $vtxFileName] == ".vtu"} {
    set vtuIsTrue 1 
    set ug_reader tmp-vtu-temporalflowthrufaces-ug_reader
    set ug        tmp-vtu-temporalflowthrufaces-ug

    catch {$ug_reader Delete}
    catch {$ug Delete}

    vtkXMLUnstructuredGridReader $ug_reader
    $ug_reader SetFileName $vtxFileName
    $ug_reader Update

    vtkUnstructuredGrid $ug
    $ug ShallowCopy [$ug_reader GetOutput]
    $ug_reader Delete
  } else {
    set vtuIsTrue 0
    set sim_pd_obj tmp-sim-results-pd-obj-for-flow  
    catch {repos_delete -obj $sim_pd_obj}
    repos_readXMLPolyData $vtxFileName $sim_pd_obj
  }

  #
  #  loop over faces
  #

  foreach face $all_faces {

    puts "Working on face ($face)..."
    if {$vtuIsTrue} {
      vtu_extractSingleFace $ug [repos_exportToVtk -src $face_objs($face)]
    } else {
      puts "using PD instead of ug"
      vtp_extractSingleFace [repos_exportToVtk -src $sim_pd_obj] [repos_exportToVtk -src $face_objs($face)]
    }
    catch {unset p_$face q_$face a_$face}
    vtp_integrateFace $face_objs($face) p_$face q_$face a_$face

  }

  set flowfp  [open $outFlowFileName w]
  set presfp  [open $outPressureFileName w]
  set resfp   [open $outAveragesFileName w]
  set unitsfp [open $outAveragesUnitsFileName w]

  puts -nonewline $flowfp "step\t"
  puts -nonewline $presfp "step\t"

  foreach face $all_faces {
    puts -nonewline $flowfp "$face\t"
    puts -nonewline $presfp "$face\t"
  }
  puts $flowfp ""
  puts $presfp ""

  # problems when things look like octal numbers
  set sorted {}
  if {[catch {set sorted [lsort -real [array names p_[lindex $all_faces 0]]]}]} {
     set sorted [lsort -dictionary [array names p_[lindex $all_faces 0]]]
  }

  foreach myrow $sorted {
    puts -nonewline $presfp "$myrow\t"
    foreach face $all_faces {
        upvar 0 p_$face parray
	#eval puts -nonewline $presfp \"\$\{p_$face\($myrow\)\}\\t\"
        puts -nonewline $presfp "$parray($myrow)\t"
    }
    puts $presfp ""
  }
  close $presfp

  # problems when things look like octal numbers
  set sorted {}
  if {[catch {set sorted [lsort -real [array names q_[lindex $all_faces 0]]]}]} {
    set sorted [lsort -dictionary [array names q_[lindex $all_faces 0]]]
  }

  foreach myrow $sorted {
    puts -nonewline $flowfp "$myrow\t"
    foreach face $all_faces {
        upvar 0 q_$face qarray
	#eval puts -nonewline $flowfp \"\$q_$\{face\($myrow\)\}\\t\"
        puts -nonewline $flowfp "$qarray($myrow)\t"
    }
    puts $flowfp ""
  }
  close $flowfp

  foreach face $all_faces {

    upvar 0 p_$face parray
    upvar 0 q_$face qarray

    # problems when things look like octal numbers
    set tsteps {}
    if [catch {set tsteps [lsort -real [array names parray]]}] {
      set tsteps [lsort -dictionary [array names parray]]
    }

    set step0 [lindex $tsteps 0]

    set pmin($face) [list $parray($step0) $step0] 
    set pmax($face) [list $parray($step0) $step0]
    set pavg($face) 0
    set qmin($face) [list $qarray($step0) $step0]
    set qmax($face) [list $qarray($step0) $step0]
    set qavg($face) 0
  
    foreach step $tsteps {
      eval set p $parray($step)
      eval set q $qarray($step)
      if {$p < [lindex $pmin($face) 0]} {
	set pmin($face) [list $p $step]
      }
      if {$p > [lindex $pmax($face) 0]} {
	set pmax($face) [list $p $step]
      }
      if {$q < [lindex $qmin($face) 0]} {
	set qmin($face) [list $q $step]
      }
      if {$q > [lindex $qmax($face) 0]} {
	set qmax($face) [list $q $step]
      }
      set pavg($face) [expr $pavg($face) + $p]
      set qavg($face) [expr $qavg($face) + $q]
    }
    set pavg($face) [expr 1.0*$pavg($face)/[llength $tsteps]]
    set qavg($face) [expr 1.0*$qavg($face)/[llength $tsteps]]

    puts $resfp "Face\t$face\tPavg\t$pavg($face)\tQavg\t$qavg($face)\tPmin\t[lindex $pmin($face) 0]\tPmin_Time\t[lindex $pmin($face) 1]\tPmax\t[lindex $pmax($face) 0]\tPmax_Time\t[lindex $pmax($face) 1]\tQmin\t[lindex $qmin($face) 0]\tQmin_Time\t[lindex $qmin($face) 1]\tQmax\t[lindex $qmax($face) 0]\tQmax_Time\t[lindex $qmax($face) 1]"

    if {$sim_units == "mm"} {
	puts $unitsfp "Face\t$face\tPavg\t[expr 760.0 / 101325.0 * $pavg($face)]\tQavg\t[expr 60.0 / 1000.0 / 1000.0 * $qavg($face)]\tPmin\t[expr 760.0 / 101325.0 * [lindex $pmin($face) 0]]\tPmin_Time\t[lindex $pmin($face) 1]\tPmax\t[expr 760.0 / 101325.0 * [lindex $pmax($face) 0]]\tPmax_Time\t[lindex $pmax($face) 1]\tQmin\t[expr 60.0 / 1000.0 / 1000.0 * [lindex $qmin($face) 0]]\tQmin_Time\t[lindex $qmin($face) 1]\tQmax\t[expr 60.0 / 1000.0 / 1000.0 * [lindex $qmax($face) 0]]\tQmax_Time\t[lindex $qmax($face) 1]"
    } else {
	puts $unitsfp "Face\t$face\tPavg\t[expr 76.0 / 101325.0 * $pavg($face)]\tQavg\t[expr 60.0 / 1000.0 * $qavg($face)]\tPmin\t[expr 76.0 / 101325.0 * [lindex $pmin($face) 0]]\tPmin_Time\t[lindex $pmin($face) 1]\tPmax\t[expr 76.0 / 101325.0 * [lindex $pmax($face) 0]]\tPmax_Time\t[lindex $pmax($face) 1]\tQmin\t[expr 60.0 / 1000.0 * [lindex $qmin($face) 0]]\tQmin_Time\t[lindex $qmin($face) 1]\tQmax\t[expr 60.0 / 1000.0 * [lindex $qmax($face) 0]]\tQmax_Time\t[lindex $qmax($face) 1]"
    }

  }

  close $resfp
  close $unitsfp

  catch {$ug Delete}
  catch {repos_delete -obj $sim_pd_obj}

  # delete loaded mesh surfaces
  foreach i [array names face_objs] {
    repos_delete -obj $face_objs($i)
  }

}


proc img_copy_sorted_images {scan_list dstTopDir} {

  #@author Nathan Wilson
  #@c Sort passes for 5X image data.
  #@a scan_list: Tcl list of directories containing 5X formatted image volumes.
  #@r dstTopDir:    Top level directory for result files.

  file mkdir $dstTopDir

  set progressfp [open [file join $dstTopDir progress] w]

  foreach major_scan $scan_list {

    puts "Working on $major_scan."
    puts $progressfp "Working on $major_scan."
    flush $progressfp

    set sorted [img_sortPasses $major_scan]
    set numPasses [lindex $sorted 0]
    set passes [lindex $sorted 1]
    set collapsed [lindex $sorted 2]

    puts "  found $numPasses passes."

    set dstDir [file join $dstTopDir $major_scan]

    set MIPs_dir [file join $dstDir MIPs]
    file mkdir $MIPs_dir
    # relocate collapse images
    foreach fn $collapsed {
      puts "  copying collapse image $fn to $MIPs_dir"
      file copy $fn $MIPs_dir
    }

    for {set curPass 1} {$curPass <= $numPasses} {incr curPass} {

      puts $progressfp "  current pass $curPass."
      flush $progressfp

      set 5X_uncorr_dir [file join $dstDir pass$curPass 5X_uncorr]
      #set 5X_corr_dir [file join $dstDir pass$curPass 5X_corr]

      file mkdir $5X_uncorr_dir
      #file mkdir $5X_corr_dir

      set 5Xfiles [lindex $passes [expr $curPass - 1]]

      foreach fn $5Xfiles {
        file copy $fn $5X_uncorr_dir
      }

      continue

      # figure out first image file
      set header [img_readHeader -file [lindex $5Xfiles 0]]
      foreach piece $header {
	set [lindex $piece 0] [lindex $piece 1]
      }
      set startindex $im_no

      # figure out last image file
      set header [img_readHeader -file [lindex $5Xfiles end]]
      foreach piece $header {
	set [lindex $piece 0] [lindex $piece 1]
      }
      set endindex $im_no

      puts "grad3d $5X_uncorr_dir/ $startindex $endindex $coilsfile 1 5000 $5X_corr_dir/"
      exec bash -c "grad3d $5X_uncorr_dir/ $startindex $endindex $coilsfile 1 5000 $5X_corr_dir/ > [file join $dstTopDir gradwarp.$im_seno-$startindex-$endindex.log]"

    }

  close $progressfp

  }

}


proc sort_GE_MR_files {in_files} {

  set speed {}
  set collapse {}
  set r {}
  set a {}
  set s {}
  set mag {}

  foreach i $in_files {
    if [catch {set header [img_readHeader -file $i]}] {
      puts "Ignoring file ($i) due to errors."
      continue
    }

    foreach piece $header {
      set [lindex $piece 0] [lindex $piece 1]
    }
    if {$vas_collapse == 0 || $vas_collapse == 7 ||  $vas_collapse == 12 || $vas_collapse == 33} {
      #puts "$i: $vas_collapse is speed"
      lappend speed $i
    } elseif {$vas_collapse == 1 || $vas_collapse == 13} {
      #puts "$i: $vas_collapse is collapse"
      lappend collapse $i
    } elseif {$vas_collapse == 2 || $vas_collapse == 8} {
      #puts  "$i: $vas_collapse is mag"
      lappend mag $i
    } elseif {$vas_collapse == 3 || $vas_collapse == 9} {
      #puts "$i: $vas_collapse is R/L"
      lappend r $i
    } elseif {$vas_collapse == 4 || $vas_collapse == 10} {
      #puts  "$i: $vas_collapse is A/P"
      lappend a $i
    } elseif {$vas_collapse == 5 || $vas_collapse == 11} {
      #puts  "$i: $vas_collapse is S/I"
      lappend s $i
    }
  }
  return [list $speed $collapse $r $a $s $mag]

}


proc repos_readXMLPolyData {filename rtnobj} {

  if {[file exists $filename] == 0} {
     return -code error "ERROR: file ($filename) does not exist."
  }
  if {[file extension $filename] == ".vtk"} {
    repos_readVtkPolyData -file $filename -obj $rtnobj 
  } elseif {[file extension $filename] == ".vtp"} {
    set reader /tmp/geom_readXMLPolyData/xmlreader
    catch {reader Delete}
    vtkXMLPolyDataReader $reader
    $reader SetFileName $filename
    $reader Update
    repos_importVtkPd -src [$reader GetOutput] -dst $rtnobj
    $reader Delete
  } else {
    return -code error "ERROR: invalid file extension on ($filename)."
  }

}


proc repos_writeXMLPolyData {inobj outfn} {

  #@a Nathan Wilson
  #@c write a PolyData or vtkPolyData object to a file
  
  if [repos_exists -obj $inobj] {
    set inobj [repos_exportToVtk -src $inobj]
  } else {
    set objtype {}
    if [catch {set objtype [$inobj GetClassName]}] {
      return -code error "ERROR: Object ($inobj) does not exist!"
    }
    if {$objtype != "vtkPolyData"} {
       return -code error "ERROR:  wrong type ($objtype)!"
    }
  }

  set polywriter tmp-repos_writeXMLPolyData-polywriter
  catch {$polywriter Delete}
  vtkXMLPolyDataWriter $polywriter
  $polywriter SetCompressorTypeToZLib
  $polywriter EncodeAppendedDataOff
  $polywriter SetInputDataObject $inobj
  $polywriter SetFileName $outfn
  $polywriter Write
  $polywriter Delete

}



proc mesh_writeCompleteMeshFromFiles {mesh_fn solid_fn prefix outdir} {

   if {![file exists $mesh_fn]} {
      return -code error "ERROR: file does not exist ($mesh_fn)."
   }
   if {![file exists $solid_fn]} {
      return -code error "ERROR: file does not exist ($solid_fn)."
   }

   set solid /tmp/mesh_writeCompleteMeshFromFiles/solid
   set mesh /tmp/mesh_writeCompleteMeshFromFiles/mesh

   catch {repos_delete -obj $solid}
   catch {repos_delete -obj $mesh}

   puts "reading solid ($solid_fn)..."
   solid_readNative -file $solid_fn -obj $solid

   puts "creating mesh object..."
   mesh_newObject -result $mesh
   puts "loading model into mesh object..."
   $mesh SetSolidKernel -name [$solid GetKernel]
   $mesh LoadModel -file $solid_fn
   puts "reading mesh ($mesh_fn)..."
   $mesh LoadMesh -file $mesh_fn
   puts "write files ($outdir)..."
   mesh_writeCompleteMesh $mesh $solid $prefix $outdir

   catch {repos_delete -obj $solid}
   catch {repos_delete -obj $mesh}
    
}


proc mesh_writeCompleteMesh {mesh solid prefix outdir} {

  global guiMMvars
  global gFilenames
  set kernel [$solid GetKernel]

  file mkdir [file join $outdir mesh-surfaces]
  file mkdir [file join $outdir misc]

  set ug     myug
  set pd     mypd
  set facepd myfacepd

  set pdwriter mypdwriter
  set ugwriter myugwriter

  catch {repos_delete -obj $ug}
  catch {repos_delete -obj $pd}
  catch {repos_delete -obj $facepd}

  catch {$pdwriter Delete}
  catch {$ugwriter Delete}

  if {$kernel == "PolyData"  && $guiMMvars(meshGenerateVolumeMesh) != 1} {
  } else {
    $mesh GetUnstructuredGrid -result $ug

    vtkXMLUnstructuredGridWriter $ugwriter
    $ugwriter SetCompressorTypeToZLib
    $ugwriter EncodeAppendedDataOff
    $ugwriter SetInputDataObject [repos_exportToVtk -src $ug]
    $ugwriter SetFileName $outdir/$prefix.mesh.vtu
    $ugwriter Update
    $ugwriter Delete
  }

  $mesh GetPolyData -result $pd

  vtkXMLPolyDataWriter $pdwriter
  $pdwriter SetCompressorTypeToZLib
  $pdwriter EncodeAppendedDataOff
  $pdwriter SetInputDataObject [repos_exportToVtk -src $pd]
  $pdwriter SetFileName $outdir/$prefix.exterior.vtp
  $pdwriter Update

  set foundWall 0
  set appender tmp-wall-appender
  catch {$appender Delete}
  vtkAppendPolyData $appender
  $appender UserManagedInputsOff

  set foundStent 0
  set Sappender tmp-stent-appender
  catch {$Sappender Delete}
  vtkAppendPolyData $Sappender
  $Sappender UserManagedInputsOff

  catch {unset name_to_identifier}
  catch {unset identifier_to_name}

  foreach faceinfo [$mesh GetModelFaceInfo] {
     puts "faceinfo: $faceinfo"
     set i [lindex $faceinfo 0]
     set ident [lindex $faceinfo 1]
     if {$kernel == "Parasolid"} {
       set facename [lindex $faceinfo 2]
     } elseif {$kernel == "Discrete"} {
       global gDiscreteModelFaceNames
       catch {set facename $gDiscreteModelFaceNames($i)}
     } elseif {$kernel == "PolyData"} {
       global gPolyDataFaceNames
       catch {set facename $gPolyDataFaceNames($i)}
     } else {
       return -code error "ERROR: invalid solid kernel ($kernel)"
     }
     if {$facename == ""} {
       set facename "missing_$i\_$ident"
     } elseif {[string match -nocase "*noname*" $facename]} {
       puts "$facename is not being written because it has no_name"
     } else {
       set name_to_identifier($facename) $ident
       set identifier_to_name($ident) $facename 
       catch {repos_delete -obj $facepd}
       if [catch {$mesh GetFacePolyData -result $facepd -face $i}] {
         return -code error "ERROR: i ($i) not found.  skip face?"
       }
       $pdwriter SetInputDataObject [repos_exportToVtk -src $facepd]
       $pdwriter SetFileName $outdir/mesh-surfaces/$facename.vtp
       $pdwriter Update
       if {[string range [string trim $facename] 0 3] == "wall"} { 
         set foundWall 1
         $appender AddInputData [repos_exportToVtk -src $facepd]
       }
       if {[string range [string trim $facename] 0 4] == "stent"} { 
         set foundStent 1
         $Sappender AddInputData [repos_exportToVtk -src $facepd]
       }
       catch {repos_delete -obj $facepd}
     }
  }

  if {$foundWall} {
    set cleaner tmp-wall-cleaner
    $appender Update
    vtkCleanPolyData $cleaner
    $cleaner PointMergingOn
    $cleaner PieceInvariantOff
    $cleaner SetInputDataObject [$appender GetOutput]
    $cleaner Update
    $pdwriter SetFileName $outdir/walls_combined.vtp
    $pdwriter SetInputDataObject [$cleaner GetOutput]
    $pdwriter Update
    $cleaner Delete
  }
  $appender Delete

  if {$foundStent} {
    set Scleaner tmp-stent-cleaner
    $Sappender Update
    vtkCleanPolyData $Scleaner
    $Scleaner PointMergingOn
    $Scleaner PieceInvariantOff
    $Scleaner SetInputDataObject [$Sappender GetOutput]
    $Scleaner Update
    $pdwriter SetFileName $outdir/stent_combined.vtp
    $pdwriter SetInputDataObject [$Scleaner GetOutput]
    $pdwriter Update
    $Scleaner Delete
  }
  $Sappender Delete

  $pdwriter Delete

  #$mesh WriteMetisAdjacency -file $outdir/$prefix.xadj
  set mesh_kernel [$mesh GetKernel] 
  if {$mesh_kernel == "TetGen"} {
    set gFilenames(tet_mesh_script_file) $outdir/$prefix
    guiMMcreateTetGenScriptFile
  } elseif {$mesh_kernel == "MeshSim"} {
    set gFilenames(mesh_script_file) $outdir/$prefix
    guiMMcreateMeshSimScriptFile
    $mesh WriteMesh -file $outdir/$prefix.sms
  }

  catch {repos_delete -obj $ug}
  catch {repos_delete -obj $pd}
  catch {repos_delete -obj $facepd}

}


proc mvres {startnum stopnum} {

  #puts -nonewline "enter starting number: "
  #flush stdout
  #gets stdin startnum 
  #puts -nonewline "enter stopping number: "
  #flush stdout
  #gets stdin stopnum

  #puts -nonewline "move files from $startnum to $stopnum into ../skipme? (y/n) "
  #flush stdout

  file mkdir ../skipme

  #gets stdin yesno

  #if {[string tolower $yesno] == "n"} exit

  for {set i $startnum} {$i <= $stopnum} {incr i} {

    set files {}
    if {![catch {set files [glob restart.$i.*]}]} {
      foreach j $files {
        file rename $j ../skipme
      }
    }

  }

  #puts -nonewline "delete flux* forces* restart.latest* files? (y/n) "
  #flush stdout

  #gets stdin yesno

  #if {[string tolower $yesno] == "n"} exit

  set files {}
  set files [glob -nocomplain flux* forces_s* restart.latest.*]

  if {$files == ""} return

  foreach j $files {
    file delete $j
  }

}


proc bctdat_from_temporal_vtk {temporal_vtk addZeroPt outfn} {

  set vtkface [repos_exportToVtk -src $temporal_vtk]
  set pressures {}
  set velocities {}
  set pointData [$vtkface GetPointData]
  for {set i 0} {$i < [$pointData GetNumberOfArrays]} {incr i} {
    set myarray [$pointData GetAbstractArray $i]
    set name [$myarray GetName]
    if {[string range $name 0 8] == "pressure_"} {
      lappend pressures $name
    } elseif {[string range $name 0 8] == "velocity_"} {
      lappend velocities $name
      set v_array($name) $myarray
    }
  }

  set pressures [lsort -dictionary $pressures]
  set velocities [lsort -dictionary $velocities]

  puts "$pressures"
  puts "$velocities"

  if {$addZeroPt} {
      set v_array(velocity_0.00000) $v_array([lindex $velocities end])
      lappend velocities velocity_0.00000
      set velocities [lsort -dictionary $velocities]
  }  

  # scale factor in case we want to scale the velocities being written out for
  # some reason (e.g. unit coversion).
  set factor  1.0

  # output the velocities to the files

  set fp [open $outfn "w"]
  fconfigure $fp -translation lf

  set nodeIds [$pointData GetArray "GlobalNodeID"]
  set numNodes [$vtkface GetNumberOfPoints]
  set numPeriods 1
  set numPtsInPeriod [llength $velocities]

  # write out header
  puts $fp "$numNodes $numPtsInPeriod"

  for {set nodeLoop 0} {$nodeLoop < $numNodes} {incr nodeLoop} {
    if {![expr $nodeLoop % 10]} {
      puts "working on node $nodeLoop of [expr $numNodes - 1]."
    }
    for {set i 0} {$i < $numPeriods} {incr i} {
      set pt [$vtkface GetPoint $nodeLoop]
      puts $fp [format "%e %e %e %i" [lindex $pt 0] [lindex $pt 1] [lindex $pt 2] $numPtsInPeriod]
      foreach j $velocities {
	set v [$v_array($j) GetTuple3 $nodeLoop]
	set node [$nodeIds GetTuple1 $nodeLoop]
	set time [format "%e" [string range $j 9 end]]
        puts $fp [format "%e %e %e %e" [expr $factor*[lindex $v 0]]  [expr $factor*[lindex $v 1]]  [expr $factor*[lindex $v 2]]  $time]
      }
    }
  }
  close $fp

}



proc vtkobj_translate_shift {inobj old_units new_units shift stress_mags stress_vectors outobj} {

  set scaleTransform tmp-vtkobj_translate_shift-scale
  set filtScale      tmp-vtkobj_translate_shift-filtscale
  set shiftTransform tmp-vtkobj_translate_shift-shift
  set filtShift      tmp-vtkobj_translate_shift-filtshift

  catch {$outobj Delete}
  catch {$scaleTransform Delete} 
  catch {$filtScale Delete}
  catch {$shiftTransform Delete}
  catch {$filtShift Delete}

  if {$old_units == $new_units} {
    set length_scale 1.0
    set pressure_scale 1.0
    set wss_scale 1.0
  } elseif {($old_units == "mm") && ($new_units == "cm")} {
    # only handle this case for now
    set length_scale 0.1
    set pressure_scale 10.0
    set wss_scale 10.0
  } else {
     return -code error "ERROR: situaton not handled ($old_units,$new_units)!"
  }

  set useme $inobj
  if {$length_scale != ""} {
     vtkTransform $scaleTransform
     $scaleTransform Scale $length_scale $length_scale $length_scale
     vtkTransformFilter $filtScale
     $filtScale SetTransform $scaleTransform
     $filtScale SetInputDataObject $inobj
     $filtScale Update
     set useme [$filtScale GetOutput]
  }

  if {$shift != ""} {
     vtkTransform $shiftTransform
     eval $shiftTransform Translate $shift
     vtkTransformFilter $filtShift
     $filtShift SetTransform $shiftTransform
     $filtShift SetInputDataObject $useme
     $filtShift Update
     set useme [$filtShift GetOutput]
  }

  [$inobj GetClassName] $outobj
  $outobj CopyStructure $useme

  # pass through element ids if they exist
  if {[$inobj GetCellData] != ""} {
    set elemarray [[$inobj GetCellData] GetArray GlobalElementID]
    if {$elemarray != ""} {
      [$outobj GetCellData] AddArray $elemarray
    }
  }

  set pointData [$useme GetPointData]

  set pressures {}
  set velocities {}
  set tractions {}
  set displacements {}

  for {set i 0} {$i < [$pointData GetNumberOfArrays]} {incr i} {

    set myarray [$pointData GetAbstractArray $i]
    set name [$myarray GetName]

    #puts "name: $name"

    if {$name == "GlobalNodeID"} {

     [$outobj GetPointData] AddArray $myarray

    } elseif {$name == "OSI"} {

     [$outobj GetPointData] AddArray $myarray

    } elseif {$name == "scalars"} {

     [$outobj GetPointData] AddArray $myarray

    } elseif {$name == "vectors"} {

     [$outobj GetPointData] AddArray $myarray

    } elseif {[string range $name 0 8] == "pressure_"} {

      if {$name == "pressure_avg_mmHg"} {

        [$outobj GetPointData] AddArray $myarray

      } else {

        set mycalcP   tmp-vtkobj_translate_shift-calcP
        catch {$mycalcP Delete}
        vtkArrayCalculator $mycalcP
        $mycalcP SetAttributeModeToUsePointData
        $mycalcP SetInputDataObject $useme
        $mycalcP AddScalarVariable p $name 0
        $mycalcP SetFunction "p * $pressure_scale"
        $mycalcP SetResultArrayName tmp_pressure
        $mycalcP Update
        [$outobj GetPointData] AddArray [[[$mycalcP GetOutput] GetPointData] GetArray tmp_pressure]
        [[$outobj GetPointData] GetArray tmp_pressure] SetName $name
        $mycalcP Delete

        lappend pressures $name

      }

    } elseif {[string range $name 0 8] == "velocity_"} {

      set mycalcV   tmp-vtkobj_translate_shift-calcV
      catch {$mycalcV Delete}
      vtkArrayCalculator $mycalcV
      $mycalcV SetAttributeModeToUsePointData
      $mycalcV SetInputDataObject $useme
      $mycalcV AddVectorVariable v $name 0 1 2
      $mycalcV SetFunction "$length_scale * v"
      $mycalcV SetResultArrayName tmp_velocity
      $mycalcV Update
      [$outobj GetPointData] AddArray [[[$mycalcV GetOutput] GetPointData] GetArray tmp_velocity]
      [[$outobj GetPointData] GetArray tmp_velocity] SetName $name
      $mycalcV Delete

      lappend velocities $name

    } elseif {[string range $name 0 12] == "displacement_"} {

      set mycalcU   tmp-vtkobj_translate_shift-calcU
      catch {$mycalcU Delete}
      vtkArrayCalculator $mycalcU
      $mycalcU SetAttributeModeToUsePointData
      $mycalcU SetInputDataObject $useme
      $mycalcU AddVectorVariable u $name 0 1 2
      $mycalcU SetFunction "$length_scale * u"
      $mycalcU SetResultArrayName tmp_displacement
      $mycalcU Update
      [$outobj GetPointData] AddArray [[[$mycalcU GetOutput] GetPointData] GetArray tmp_displacement]
      [[$outobj GetPointData] GetArray tmp_displacement] SetName $name
      $mycalcU Delete

      lappend displacements $name

    } else {

      set foundStress 0

      foreach stresstype $stress_vectors {

	if {[string range $name 0 [expr [string length $stresstype] - 1]] == $stresstype} {

          set mycalcWSS tmp-vtkobj_translate_shift-calcWSS
	  catch {$mycalcWSS Delete}
          vtkArrayCalculator $mycalcWSS
          $mycalcWSS SetAttributeModeToUsePointData
          $mycalcWSS SetInputDataObject $useme
          $mycalcWSS AddVectorVariable mywss $name 0 1 2
          $mycalcWSS SetFunction "mywss * $wss_scale"
          $mycalcWSS SetResultArrayName tmp_mywss
          $mycalcWSS Update
          [$outobj GetPointData] AddArray [[[$mycalcWSS GetOutput] GetPointData] GetArray tmp_mywss]
          [[$outobj GetPointData] GetArray tmp_mywss] SetName $name
          $mycalcWSS Delete
          lappend tractions $name
          set foundStress 1
          break
        }

      }

      foreach stresstype $stress_mags {

        if {$name == $stresstype} {

          set mycalcWSS tmp-vtkobj_translate_shift-calcWSS
	  catch {$mycalcWSS Delete}
          vtkArrayCalculator $mycalcWSS
          $mycalcWSS SetAttributeModeToUsePointData
          $mycalcWSS SetInputDataObject $useme
          $mycalcWSS AddScalarVariable mywss $name 0
          $mycalcWSS SetFunction "mywss * $wss_scale"
          $mycalcWSS SetResultArrayName tmp_mywss
          $mycalcWSS Update
          [$outobj GetPointData] AddArray [[[$mycalcWSS GetOutput] GetPointData] GetArray tmp_mywss]
          [[$outobj GetPointData] GetArray tmp_mywss] SetName $name
          $mycalcWSS Delete
          lappend tractions $name
          set foundStress 1
          break
        }

      }

      if {!$foundStress} {
        puts "unknown array ($name) ignored!!"
      }

    }

  }

  set pressures [lsort -dictionary $pressures]
  set velocities [lsort -dictionary $velocities]
  set tractions [lsort -dictionary $tractions]
  set displacements [lsort -dictionary $displacements]

  #puts "pressures: $pressures"
  #puts "velocities: $velocities"
  #puts "tractions: $tractions"
  #puts "displacements: $displacements"
  
  catch {$scaleTransform} 
  catch {$filtScale}
  catch {$shiftTransform}
  catch {$filtShift}

}


proc create_polydata_model_from_mesh_surfaces {all_face_files outfn} {

    global gPolyDataFaceNames
    global gPolyDataFaceNamesInfo

    catch {unset gPolyDataFaceNames}
    catch {unset gPolyDataFaceNamesInfo}

    set fn $outfn

    set solidname my-solid
    catch {repos_delete -obj $solidname}

    package require md5

    if {$fn == ""} return 

    set id 1
     
    foreach face $all_face_files {
  
      catch {repos_delete -obj $face}

      repos_readXMLPolyData $face $face

      geom_tagAllCellsWithId $face $id

      set gPolyDataFaceNames($id) [file rootname [file tail $face]]

      incr id

    }

    geom_appendPds $all_face_files $solidname

    [[repos_exportToVtk -src $solidname] GetPointData] RemoveArray GlobalNodeID
    [[repos_exportToVtk -src $solidname] GetPointData] RemoveArray BC_FaceID

    catch {repos_delete -obj mergePtsPD}
    geom_mergePts -src $solidname -dst mergePtsPD
    repos_writeXMLPolyData mergePtsPD $fn

    puts "Writing file ($fn.facenames)"
 
    if {[array size gPolyDataFaceNames] != 0} {
      puts "Writing file ($fn.facenames)"
      set mymd5 [::md5::md5 -hex -file $fn]
      set fp [open $fn.facenames w]
      fconfigure $fp -translation lf
      set allids [lsort -integer [array names gPolyDataFaceNames]]
      puts $fp "\# user defined face id to name mapping for model file ($fn)"
      set timestamp [clock seconds]
      puts $fp "\# timestamp: $timestamp  ([clock format $timestamp])"
      puts $fp ""
      puts $fp "global gPolyDataFaceNames"
      puts $fp "global gPolyDataFaceNamesInfo"
      puts $fp ""
      puts $fp "set gPolyDataFaceNamesInfo(timestamp) \{$timestamp\}"
      puts $fp "set gPolyDataFaceNamesInfo(model_file_md5) \{$mymd5\}"
      puts $fp "set gPolyDataFaceNamesInfo(model_file_name) \{[file tail $fn]\}"
      puts $fp ""
      foreach id $allids {
	puts $fp "set gPolyDataFaceNames($id) \{$gPolyDataFaceNames($id)\}"
      }
      close $fp
      puts "Done writing facenames file."
    }

}

