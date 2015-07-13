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

proc seg_writeVolumeMha {name fname} {
	catch {writer Delete}
  vtkMetaImageWriter writer
  set sp [[repos_exportToVtk -src $name] GetSpacing]
  set ext [[repos_exportToVtk -src $name] GetExtent]
  puts $sp
  puts $ext
	writer SetInputData [repos_exportToVtk -src $name]
	writer SetFileName $fname
	writer Write
	writer Delete
}

proc seg_convertModeltoVolume { {objName temp} } { 

	global smasherInputName
  global gKernel
	set modelPd pd_$objName

  set model [lindex [model_names] 0]

	catch {repos_delete -obj $modelPd}
  catch {repos_delete -obj $objName}
	$model GetPolyData -result $modelPd
	itkutils_PdToVol -src $modelPd -dst $objName -ref volume_image

	catch {repos_delete -obj $modelPd}
}

proc seg_saveSmasherPolyData { {fn "smasher.vtp"} } { 

global smasherInputName
set modelPd /tmp/pd

if {[repos_exists -obj $smasherInputName] == 0} {
    puts "ERROR:  Object $smasherInputName does not exist."
    return
  }

catch {repos_delete -obj $modelPd}
puts "re saving smasher obj as $fn"
$smasherInputName GetPolyData -result $modelPd
repos_writeXMLPolyData $modelPd $fn
}

# proc seg_reSaveModelPolydata {{fn ""}} {
    
#     global gFilenames
#     if {$fn == ""} {
#       set fnOrg $gFilenames(generic_solid_file)
#       set basename [file rootname $fnOrg]
#       set fn $basename.vtp
#     }
#     if {$fn == ""} return

#     puts "saving as $fn"
#     puts "Warning Face names will not be saved!"
#     seg_saveSmasPolyData $fn
# }

proc seg_reSaveModelParasolidAsPolydata { {model ""}} {

  if {$model == ""} {
    set model [lindex [model_names] 0]
  }
  global gFilenames
  set fn_base $gFilenames(generic_solid_file)
  if {$fn_base == ""} return
  set basename [file rootname $fn_base]
  set fn $basename.vtp

  set modelpd /models/parasolid/$model


  set modelPd /tmp/pd-$model
  catch {repos_delete -obj $modelPd}
  $model GetPolyData -result $modelPd
  repos_writeXMLPolyData $modelPd $fn
}


proc seg_SavePathsAsPolyData { {pathsIds -1} } {

  seg_AppendSplines $pathsIds
  global gFilenames
  set pathfn_base $gFilenames(path_file)
  if {$pathfn_base == ""} return
  set pathfn_base [file rootname $pathfn_base]
  set pathfn $pathfn_base-paths.vtp
  set splinesPd /tmp/splinesPd
  repos_writeXMLPolyData $splinesPd $pathfn

}


proc seg_convertPdtoVolume { pd {objName temp} } { 

	set modelPd pd_$objName

	catch {repos_delete -obj $modelPd}
	itkutils_PdToVol -src $pd -dst $objName -ref volume_image
	catch {repos_delete -obj $modelPd}
}

proc seg_convertPdtoVolumeAndSave { pd fn } { 
    set objName /tmp/pdvol
    catch {repos_delete -obj $objName}
    seg_convertPdtoVolume $pd $objName
    seg_writeVolumeMha $objName $fn

}

proc seg_writeAllPathDistanceMap { {fnamebase temp} {pathIds -1} } {

	global gPathPoints
	global symbolicName
	global gRen3d

	if {$pathIds == "-1" } {
		# find the current list of paths to choose from
		set names [lsort -dictionary [array names gPathPoints *,name]]
		set pathIds {}
		foreach n $names {
			set busted [split [string trim $n] ,]
			lappend pathIds [lindex $busted 0]
		}
	}
	vtkAppendPolyData vtkapd
	vtkCleanPolyData vtkcleaner
	vtkImplicitModeller imp
	vtkImageChangeInformation changer
	vtkMetaImageWriter writer

	foreach id $pathIds {

		set splinePd /tmp/guiPPchooser/spline/$id
		catch {repos_delete -obj $splinePd}
		catch {vis_pRm $gRen3d $splinePd}

		if {[info exists gPathPoints($id,splinePts)] == 0} {
			continue
		}


		set splinePts $gPathPoints($id,splinePts)

		if {[llength $splinePts] < 2} {
			continue
		}

		puts "Appending path with id: $id"
		path_MakePolyData $splinePts $splinePd
		repos_setLabel -obj $splinePd -key color -value white
		gdscGeneralView $gRen3d $splinePd
		vtkapd AddInputData [repos_exportToVtk -src $splinePd]
	}
	set gRen3dFreeze 0
  	vis_render $gRen3d

	puts "Cleaning combined polydata!"
	vtkcleaner SetInputConnection [vtkapd GetOutputPort]
	imp SetInputConnection [vtkcleaner GetOutputPort]
	changer SetInputConnection [imp GetOutputPort]


	set i [lindex [[repos_exportToVtk -src volume_image] GetDimensions] 0]
	set j [lindex [[repos_exportToVtk -src volume_image] GetDimensions] 1]
	set k [lindex [[repos_exportToVtk -src volume_image] GetDimensions] 2]

	set spx [lindex [[repos_exportToVtk -src volume_image] GetSpacing] 0]
	set spy [lindex [[repos_exportToVtk -src volume_image] GetSpacing] 1]
	set spz [lindex [[repos_exportToVtk -src volume_image] GetSpacing] 2]

	set ox [lindex [[repos_exportToVtk -src volume_image] GetOrigin] 0]
	set oy [lindex [[repos_exportToVtk -src volume_image] GetOrigin] 1]
	set oz [lindex [[repos_exportToVtk -src volume_image] GetOrigin] 2]

	set e0 [lindex [[repos_exportToVtk -src volume_image] GetExtent] 0]
	set e1 [lindex [[repos_exportToVtk -src volume_image] GetExtent] 1]
	set e2 [lindex [[repos_exportToVtk -src volume_image] GetExtent] 2]
	set e3 [lindex [[repos_exportToVtk -src volume_image] GetExtent] 3]
	set e4 [lindex [[repos_exportToVtk -src volume_image] GetExtent] 4]
	set e5 [lindex [[repos_exportToVtk -src volume_image] GetExtent] 5]

	set b0 [expr $e0*$spx+$ox]
	set b1 [expr $e1*$spx+$ox]
	set b2 [expr $e2*$spy+$oy]
	set b3 [expr $e3*$spy+$oy]
	set b4 [expr $e4*$spz+$oz]
	set b5 [expr $e5*$spz+$oz]

	puts "Sample Dimensions: $i $j $k"
	puts "Model Bounds: $b0 $b1 $b2 $b3 $b4 $b5"
	puts "Output Extent: Start $e0 $e2 $e4"
	puts "Output Origin: $ox $oy $oz"
	puts "Output Spacing: $spx $spy $spz"



	imp SetSampleDimensions $i $j $k
	imp SetModelBounds $b0 $b1 $b2 $b3 $b4 $b5
	imp ScaleToMaximumDistanceOn
	imp SetOutputScalarTypeToChar
	puts "Computing distance map (this may take a while)"

	imp Update

	changer SetOutputExtentStart $e0 $e2 $e4
	changer SetOutputSpacing $spx $spy $spz
	changer SetOutputOrigin $ox $oy $oz
	changer Update


	writer SetInputConnection [changer GetOutputPort]
	writer SetFileName $fnamebase
	writer Write

	vtkapd Delete
	changer Delete
	vtkcleaner Delete
	imp Delete
	writer Delete

}

proc contourVolumeTests {inPd outImgFn} {

set testPd /test/pd
set testVol /test/vol

catch {repos_delete -obj $testPd}
catch {conn Delete}
vtkConnectivityFilter conn
conn SetInputData [repos_exportToVtk -src $inPd]
conn SetExtractionModeToAllRegions
conn Update

catch {usgWriter Delete}
vtkXMLUnstructuredGridWriter usgWriter
usgWriter SetFileName "test-cont.vtu"
usgWriter SetInputData [conn GetOutput]
usgWriter Write

catch {thresh Delete}
vtkThreshold thresh
thresh SetInputData [conn GetOutput]
thresh ThresholdBetween 0 0
thresh Update


usgWriter SetFileName "test-thres.vtu"
usgWriter SetInputData [thresh GetOutput]
usgWriter Write

catch {surfacer Delete}
vtkDataSetSurfaceFilter surfacer
surfacer SetInputData [thresh GetOutput]
surfacer Update

repos_importVtkPd -src [surfacer GetOutput] -dst $testPd


global gRen3d
repos_setLabel -obj $testPd -key color -value blue
gdscGeneralView $gRen3d $testPd

catch {repos_delete -obj $testVol}

seg_convertPdtoVolume $testPd $testVol
seg_writeVolumeMha $testVol $outImgFn

conn Delete
thresh Delete
usgWriter Delete
surfacer Delete

}

proc seg_writeSliceAndMaskTiff {pathId posList {path .} args} {

  set modelImg /tmp/modelImg
  seg_convertModeltoVolume  $modelImg
  seg_writeSliceTiff $pathId $posList volume_image $path ""


  seg_writeSliceTiff $pathId $posList $modelImg $path "/mask/"


}

proc seg_writeVolumeXML {name fname} {

    vtkXMLImageDataWriter writer
    writer SetInputData [repos_exportToVtk -src $name]
    writer SetFileName $fname
    writer Write

    writer Delete

  }
proc reorientProfile { lsres pathId posId } {

    global gOptions
    global gPathPoints
    puts "reorienting $lsres to path: $pathId  position: $posId"
    set path $gPathPoints($pathId,splinePts)

    puts "$path"
    array set items [lindex $path $posId]
    set pos [TupleToList $items(p)]
    set nrm [TupleToList $items(t)]
    set xhat [TupleToList $items(tx)]

    set lsoriented /lsGUI/$pathId/$posId/ls/oriented
    puts "reoriented id: $lsoriented"
    catch {repos_delete -obj $lsoriented}
    geom_orientProfile -src $lsres -path_pos $pos -path_tan $nrm -path_xhat $xhat -dst $lsoriented


    global lsGUIcurrentPositionNumber
    set  lsGUIcurrentPositionNumber $posId
    global lsGUIcurrentPathNumber
    set lsGUIcurrentPathNumber $pathId
    global lsGUIcurrentGroup
    set group $lsGUIcurrentGroup
    puts "adding to group $group"
    lsGUIaddToGroup levelset
  }



proc seg_ShowSplines { {pathIds -1} } {
	global gPathPoints
	global symbolicName
	global gRen3d

	# find the current list of paths to choose from
	if {$pathIds == "-1"} {
		set names [lsort -dictionary [array names gPathPoints *,name]]
		set allPathIds {}
		foreach n $names {
			set busted [split [string trim $n] ,]
			lappend allPathIds [lindex $busted 0]
		}
	} else {
		set allPathIds $pathIds
		}
  # do show path stuff
  global gRen3dFreeze
  set gRen3dFreeze 1

  global gOptions
  set color green
  set thick $gOptions(line_width_for_all_splines)

  foreach id $allPathIds {

    set splinePd /tmp/guiPPchooser/spline/$id
    catch {repos_delete -obj $splinePd}
    catch {vis_pRm $gRen3d $splinePd}

    if {[info exists gPathPoints($id,splinePts)] == 0} {
      continue
    }

    set splinePts $gPathPoints($id,splinePts)

    if {[llength $splinePts] < 2} {
      continue
    }

    path_MakePolyData $splinePts $splinePd

     repos_setLabel -obj $splinePd -key color -value $color
     gdscGeneralView $gRen3d $splinePd
     set myact [vis_pGetActor $gRen3d $splinePd]
     set myprop [$myact GetProperty]
     $myprop SetLineWidth $thick
     $myprop SetInterpolationToFlat
  }

  set gRen3dFreeze 0
  vis_render $gRen3d
  return GDSC_OK
}

proc seg_AppendSplines { {pathIds -1} } {
  global gPathPoints
  global symbolicName
  global gRen3d

  # find the current list of paths to choose from
  if {$pathIds == "-1"} {
    set names [lsort -dictionary [array names gPathPoints *,name]]
    set allPathIds {}
    foreach n $names {
      set busted [split [string trim $n] ,]
      lappend allPathIds [lindex $busted 0]
    }
  } else {
    set allPathIds $pathIds
    }
  # do show path stuff
  global gRen3dFreeze
  set gRen3dFreeze 1

  global gOptions
  set color green
  set thick $gOptions(line_width_for_all_splines)

  set splinePds {}
  foreach id $allPathIds {

    set splinePd /tmp/guiPPchooser/spline/$id
    catch {repos_delete -obj $splinePd}
    catch {vis_pRm $gRen3d $splinePd}

    if {[info exists gPathPoints($id,splinePts)] == 0} {
      continue
    }

    set splinePts $gPathPoints($id,splinePts)

    if {[llength $splinePts] < 2} {
      continue
    }

    path_MakePolyData $splinePts $splinePd
    lappend splinePds $splinePd
    
     # repos_setLabel -obj $splinePd -key color -value $color
     # gdscGeneralView $gRen3d $splinePd
     # set myact [vis_pGetActor $gRen3d $splinePd]
     # set myprop [$myact GetProperty]
     # $myprop SetLineWidth $thick
     # $myprop SetInterpolationToFlat
  }

  set splinesPd /tmp/splinesPd
  catch {repos_delete -obj $splinesPd}
  puts "Appending PDs"
  geom_appendPds $splinePds $splinesPd
  gdscGeneralView $gRen3d $splinesPd
  set gRen3dFreeze 0
  vis_render $gRen3d
  return GDSC_OK
}

proc seg_PrintSelectedGroups {} {
	global symbolicName
	set tv $symbolicName(guiSV_group_tree)
	set children [$tv children {}]
	puts "children1: $children"
	if {[lsearch -exact $children .groups.all] >= 0} {
		set children [$tv children .groups.all]
		if {$children == ""} {
			return
		}
	}

	set selected_groups {}
	foreach child $children {
		if {[$tv item $child -values] == "YES"} {
			lappend selected_groups [string range $child 12 end]
		}
	}
	puts "$selected_groups"
}

proc seg_writeSegData { {pathIds -1} } {


global gFilenames
set segfn_base $gFilenames(generic_solid_file)
if {$segfn_base == ""} return
set seg_basename [file rootname $segfn_base]
set segfn $seg_basename.mha

# set distfn_base $gFilenames(path_file)
# if {$distfn_base == ""} return
# set distfn_base [file rootname $distfn_base]
# set distfn $distfn_base.vtp


#puts "distfn $distfn"




seg_convertModeltoVolume tmp-pd
puts "segfn $segfn"
seg_writeVolumeMha tmp-pd $segfn
global gImageVol
set imgfn_base $gImageVol(xml_filename)
if {$imgfn_base == ""} return
puts "imgfn $imgfn"
set imgfn_base [file rootname $imgfn_base]
set imgfn $imgfn_base.mha
seg_writeVolumeMha volume_image $imgfn
#seg_writeAllPathDistanceMap $distfn "$pathIds"

}

proc seg_getPDSliceAtPathPoint {{value 0} } {

    #global lsGUIcurrentGroup
    #set group $lsGUIcurrentGroup
    #set pd /guiGROUPS/polydatasurface/$group

    if {$value == "0"} {
        global gui3Dvars
        set pd $gui3Dvars(ls_finalSegmentation)
    } else {
        set pd $value
    }
    if {![repos_exists -obj $pd]} return


    global gOptions
    set ext $gOptions(resliceDims)

    global gPathPoints
    global lsGUIcurrentPathNumber
    global lsGUIcurrentPositionNumber
    set pathId $lsGUIcurrentPathNumber
    set path $gPathPoints($pathId,splinePts)
    set posId $lsGUIcurrentPositionNumber

    array set items [lindex $path $posId]
    set pos [TupleToList $items(p)]
    set nrm [TupleToList $items(t)]
    set xhat [TupleToList $items(tx)]

    catch {plane Delete}
    vtkPlane plane
    plane SetOrigin [lindex $pos 0] [lindex $pos 1] [lindex $pos 2] 
    plane SetNormal [lindex $nrm 0] [lindex $nrm 1] [lindex $nrm 2]

    catch {cutter Delete}
    vtkCutter cutter
    cutter SetCutFunction plane 

    cutter SetInputData [repos_exportToVtk -src $pd]
    cutter GenerateValues 1 0 1
    cutter Update

    global gRen3d
    global lsGUImagWindow

    set segPd tmpPD
    set segPd2 tmpPD2
    catch {repos_delete -obj $segPd}
    catch {repos_delete -obj $segPd2}
    repos_importVtkPd -src [cutter GetOutput] -dst $segPd
    set bd2 [[repos_exportToVtk -src $segPd] GetBounds]
    puts "bd2 $bd2"

    geom_disorientProfile -src $segPd -dst $segPd2 -path_pos $pos \
             -path_tan $nrm -path_xhat $xhat

    repos_setLabel -obj $segPd2 -key color -value blue
    repos_setLabel -obj $segPd2 -key opacity -value 1
    return $segPd2

}


proc seg_writeSliceTiff {pathId posList {src volume_image} {path .} } {

global gPathPoints

set path $gPathPoints($pathId,splinePts)

global gOptions
set ext $gOptions(resliceDims)

# get the image slice


for {set idx 0} {$idx < [llength $posList]} {incr idx 1} {

    set posId [lindex $posList $idx]
    puts "posId: $posId\n"

    set rtnImg /img/$pathId/$posId/mag
    set rtnPot /img/$pathId/$posId/pot
    catch {repos_delete -obj $rtnImg}
    catch {repos_delete -obj $rtnPot}
    img_getSliceAtPathPoint $src $path $posId $ext $rtnImg $rtnPot


#   repos_writeVtkStructuredPoints -obj $rtnImg -type ascii -file $posId.vtk
    seg_writeTiff $rtnImg ./$pathId/
    seg_writeTiff $rtnPot ./$pathId/

    }

}

# proc seg_writeTiff {img directory} {
#         file mkdir $directory
#         regsub -all "^/" $img "" img2
#         regsub -all "/" $img2 "-" fname
#         set ffname $directory/$fname.tiff
        
#         vtkImageCast caster
#         puts "caster - $img"
#         caster SetInputData [repos_exportToVtk -src $img]
#         caster SetOutputScalarTypeToFloat
#         caster ClampOverflowOff
#         caster Update
        
#         vtkTIFFWriter writer
    
#         writer SetFileName $ffname 
#         writer SetInputData [caster GetOutput]
#         writer Write
        
#         writer Delete
#         caster Delete
#         #ITKLevelSet_WriteImage -src $img -fname $ffname
# }


proc getPosList {pathId str} {
  global gPathPoints
  set minFrame 0
  set maxFrame [expr {$gPathPoints($pathId,numSplinePts) -1}]
  set posList [string_parse $str $minFrame $maxFrame]
  if {[llength $posList] == 0} {
    puts "ERROR:  No frames specified."
  }
  return $posList
}


proc seg_writeSliceSegEdgePNG {pathId posList {fnamebase tmp} {pathfn .} } {

  set modelImg /tmp/modelImg
  catch {repos_delete -obj $modelImg}
  seg_convertModeltoVolume $modelImg

  file mkdir $pathfn
  global gPathPoints
  set pathPts $gPathPoints($pathId,splinePts)
  global gOptions
  set ext $gOptions(resliceDims)

  for {set idx 0} {$idx < [llength $posList]} {incr idx 1} {

    set posId [lindex $posList $idx]

    set fnamebase2 [file join $pathfn "$fnamebase-$pathId-$posId"]

    set magImg /img/$pathId/$posId/mag
    set potImg /img/$pathId/$posId/pot
    set segImg /img/$pathId/$posId/seg
    set edgeImg /img/$pathId/$posId/edge
    catch {repos_delete -obj $magImg}
    catch {repos_delete -obj $potImg}
    catch {repos_delete -obj $segImg}
    catch {repos_delete -obj $edgeImg}
    img_getSliceAtPathPoint volume_image $pathPts $posId $ext $magImg $potImg
    img_getSliceAtPathPoint $modelImg $pathPts $posId $ext $segImg ->

    catch {thrshSeg Delete}
    vtkImageThreshold thrshSeg
    thrshSeg SetInputData [repos_exportToVtk -src $segImg]
    thrshSeg ThresholdByUpper 50
    thrshSeg SetInValue 255
    thrshSeg SetOutValue 0
    thrshSeg ReplaceOutOn
    thrshSeg ReplaceInOn
    thrshSeg Update

    catch {repos_delete -obj $segImg}
    repos_importVtkImg -src [thrshSeg GetOutput] -dst $segImg

    catch {gradMag Delete}
    vtkImageGradientMagnitude gradMag
    gradMag SetDimensionality 2
    gradMag SetInputData [repos_exportToVtk -src $segImg]
    gradMag Update



    catch {stat Delete}
    vtkImageHistogramStatistics stat
    stat SetInputData [gradMag GetOutput]
    stat GenerateHistogramImageOff
    stat Update

    set max [stat GetMaximum]

    set thr [expr {.5*$max}]





    catch {thrshSeg Delete}
    vtkImageThreshold thrshSeg
    thrshSeg SetInputData [gradMag GetOutput]
    thrshSeg ThresholdByUpper $thr
    thrshSeg SetInValue 255
    thrshSeg SetOutValue 0
    thrshSeg ReplaceOutOn
    thrshSeg ReplaceInOn
    thrshSeg Update

    catch {repos_delete -obj $edgeImg}
    repos_importVtkImg -src [thrshSeg GetOutput] -dst $edgeImg


    set magfn "$fnamebase2-img.png"
    set potfn "$fnamebase2-pot.png"
    set segfn "$fnamebase2-seg.png"
    set edgefn "$fnamebase2-edge.png"


  catch {stat Delete}
  vtkImageHistogramStatistics stat
  stat SetInputData [repos_exportToVtk -src $segImg]
  stat GenerateHistogramImageOff
  stat Update
  
 
    set min [stat GetMinimum]
    set max [stat GetMaximum]
    if { $min != $max} {
      seg_writePNG $magImg $magfn
      seg_writePNG $potImg $potfn
      seg_writePNG $segImg $segfn 1
      seg_writePNG $edgeImg $edgefn 1
    } else {
      puts "skipping pathid: $pathId: $posId min/max: $min $max"
    }

  }
  catch {repos_delete -obj $modelImg}
}


proc seg_writePNG {img filename {usestat 0} } {
        
  set directory [file path $filename]
  file mkdir $directory

  catch {stat Delete}
  vtkImageHistogramStatistics stat
  stat SetInputData [repos_exportToVtk -src $img]
  stat GenerateHistogramImageOff
  stat Update
  set range [ [repos_exportToVtk -src $img] GetScalarRange ]
  if { $usestat == "1"} {
    set range "[stat GetMinimum] [stat GetMaximum]"
  }

  
  puts "$filename $img $range, min, max [stat GetMinimum] [stat GetMaximum]"

  catch {shifter Delete}
  vtkImageShiftScale shifter
  shifter SetShift [ expr -1.0*[lindex $range 0] ]
  shifter SetScale [ expr 255.0 /( [lindex $range 1] - [lindex $range 0] ) ]
  shifter SetOutputScalarTypeToUnsignedChar
  shifter SetInputData [repos_exportToVtk -src $img]
  shifter ReleaseDataFlagOff
  shifter Update

  catch {writer Delete}
  vtkPNGWriter writer
  writer SetFileName $filename 
  writer SetInputData [shifter GetOutput]
  writer Write

  writer Delete
  shifter Delete
  #ITKLevelSet_WriteImage -src $img -fname $ffname
  }

proc seg_writeSliceSegEdgeTIFF {pathId posList {fnamebase tmp} {pathfn .} } {

  set modelImg /tmp/modelImg
  catch {repos_delete -obj $modelImg}
  seg_convertModeltoVolume $modelImg

  file mkdir $pathfn
  global gPathPoints
  set pathPts $gPathPoints($pathId,splinePts)
  global gOptions
  set ext $gOptions(resliceDims)

  for {set idx 0} {$idx < [llength $posList]} {incr idx 1} {

    set posId [lindex $posList $idx]

    set fnamebase2 [file join $pathfn "$fnamebase-$pathId-$posId"]

    set magImg /img/$pathId/$posId/mag
    set potImg /img/$pathId/$posId/pot
    set segImg /img/$pathId/$posId/seg
    set edgeImg /img/$pathId/$posId/edge
    catch {repos_delete -obj $magImg}
    catch {repos_delete -obj $potImg}
    catch {repos_delete -obj $segImg}
    catch {repos_delete -obj $edgeImg}
    img_getSliceAtPathPoint volume_image $pathPts $posId $ext $magImg $potImg
    img_getSliceAtPathPoint $modelImg $pathPts $posId $ext $segImg ->

    catch {thrshSeg Delete}
    vtkImageThreshold thrshSeg
    thrshSeg SetInputData [repos_exportToVtk -src $segImg]
    thrshSeg ThresholdByLower 50
    thrshSeg SetInValue 255
    thrshSeg SetOutValue 0
    thrshSeg ReplaceOutOn
    thrshSeg ReplaceInOn
    thrshSeg Update

    catch {thrshSeg gradMag}
    vtkImageGradientMagnitude gradMag
    gradMag SetDimensionality 2
    gradMad SetInputData [repos_exportToVtk -src $segImg]
    gradMad Update

    set magfn "$fnamebase2-img.png"
    set potfn "$fnamebase2-pot.png"
    set segfn "$fnamebase2-seg.png"
    set edgefn "$fnamebase2-edge.png"


  catch {stat Delete}
  vtkImageHistogramStatistics stat
  stat SetInputData [repos_exportToVtk -src $segImg]
  stat GenerateHistogramImageOff
  stat Update
  
 
    set min [stat GetMinimum]
    set max [stat GetMaximum]
    if { $min != $max} {
      seg_writeTIFF $magImg $magfn 0 0
      seg_writeTIFF $potImg $potfn 0 0
      seg_writeTIFF $segImg $segfn 1 1
      seg_writeTIFF $edgeImg $edgefn 1 1
    } else {
      puts "skipping $posId min/max: $min $max"
    }

  }
  catch {repos_delete -obj $modelImg}
}



proc seg_writeTIFF {img filename {rescale 0} {usestat 0} } {
      
  set directory [file path $filename]
  file mkdir $directory

  catch {stat Delete}
  vtkImageHistogramStatistics stat
  stat SetInputData [repos_exportToVtk -src $img]
  stat GenerateHistogramImageOff
  stat Update
  set range [ [repos_exportToVtk -src $img] GetScalarRange ]
  if { $usestat == "1"} {
    set range "[stat GetMinimum] [stat GetMaximum]"
  }


  puts "$filename $img $range, min, max [stat GetMinimum] [stat GetMaximum]"

  catch {caster Delete}

  if {$rescale != "1"} {
    vtkImageShiftScale caster
    caster SetShift [ expr -1.0*[lindex $range 0] ]
    caster SetScale [ expr 255.0 /( [lindex $range 1] - [lindex $range 0] ) ]
    caster SetOutputScalarTypeToUnsignedChar
    caster SetInputData [repos_exportToVtk -src $img]
    caster ReleaseDataFlagOff
    caster Update
  } else {
    vtkImageCast caster
    caster SetInputData [repos_exportToVtk -src $img]
    caster SetOutputScalarTypeToFloat
    caster ClampOverflowOff
    caster Update
  }

  vtkTIFFWriter writer
  writer SetFileName $ffname 
  writer SetInputData [caster GetOutput]
  writer Write

  writer Delete
  caster Delete
  #ITKLevelSet_WriteImage -src $img -fname $ffname
}



proc seg_LoadAll {imgfn pathfn modelfn} {

  puts $imgfn
  puts $pathfn
  puts $modelfn


  # Load image
  if {$imgfn == ""} return
  global gImageVol
  if {[file extension $imgfn] == ".vti"} {
      set gImageVol(xml_filename) "$imgfn"
  }
  if {[file extension $imgfn] == ".mha"} {
      set gImageVol(mha_filename) "$imgfn"
  }
  createPREOPloadsaveLoadVol 

  ## Load Model
  if {$modelfn == ""} return
  guiSV_model_load_model "$modelfn"

  ## Load paths

  if {$pathfn == ""} return
  global gFilenames
  set gFilenames(path_file) "$pathfn"
  guiFNMloadHandPaths
  after 100

  #seg_extractParasolidStuff
  #mainGUIexit 1

}

proc seg_3DRenForScreen { } {
  global gRen3d
  global CurrentRenderer
  set CurrentRenderer $gRen3d
  guiCV_display_windows "3d_only"
  set win .guiCV.tframe3.tpanedwindow4
  $win sashpos 0 1000

  vis_renSetBackground $gRen3d .9 .9  .9

  set  widget ".guiCV.tframe3.tpanedwindow4.tframe5.tpanedwindow45.tframe36.tpanedwindow0.tframe1.pane0.pane1.f3.r1"
  UpdateRenderer $widget 0 0
}


proc seg_takeScreenShots {{num 0}} {
  global gRen3d
  global CurrentRenderer
  set CurrentRenderer $gRen3d
  global CurrentCamera
  global gSavedView
  set  widget ".guiCV.tframe3.tpanedwindow4.tframe5.tpanedwindow45.tframe36.tpanedwindow0.tframe1.pane0.pane1.f3.r1"

  set outstr "widget {$widget} FocalPoint {[$CurrentCamera GetFocalPoint]} Position {[$CurrentCamera GetPosition]} ViewUp {[$CurrentCamera GetViewUp]}"
  global gSavedView
  array set gSavedView $outstr
  if {$num == "0"} {
  set gSavedView(Position) {0 1000 0}
  set gSavedView(ViewUp) {0 1 0}
  }
  if {$num == "1"} {
  set gSavedView(Position) {1000 0 0}
  set gSavedView(ViewUp) {0 0 1}
  }
  if {$num == "2"} {
  set gSavedView(Position) {0 0 1000}
  set gSavedView(ViewUp) {1 0 0}
  }
  
  global gFilenames
  set fn_base $gFilenames(generic_solid_file)
  if {$fn_base == ""} return
  set basename [file rootname $fn_base]
  set fn $basename-$num.jpg
  puts $fn
  RenderCameraView $widget 0 0
  Reset $widget 0 0
  $CurrentRenderer ResetCamera
  puts "take screen"
  global gRen3d
  vis_renSetBackground $gRen3d .99 .99  .99
  vis_renWriteJPEG $gRen3d $fn
  after 100

}

proc seg_extractParasolidStuff {} {
  seg_3DRenForScreen
  seg_SavePathsAsPolyData
  seg_takeScreenShots 0
  seg_takeScreenShots 1
  seg_takeScreenShots 2
  seg_reSaveModelParasolidAsPolydata
  seg_writeSegData
  puts "Done!!"
}