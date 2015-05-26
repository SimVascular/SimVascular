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

proc itklsDo { value} {
	global itklsGUIParams
	#itklsChangeFrame 0
	if {$value < 0} {
		#puts "value $value"
		set seedPd /tmp/lsGUI/seedPd
		catch {repos_delete -obj $seedPd}
		return
	}
	if {$value == "0" || $itklsGUIParams(recompSeed)=="1"} {
		set seedPd /tmp/lsGUI/seedPd
		catch {repos_delete -obj $seedPd}
	}

	if {$value == "1" || $itklsGUIParams(recompStg1)=="1"} {
		itkLSStg1
	}

	if {$value == "2" || $itklsGUIParams(recompStg2)=="1"} {
		itkLSStg2
	}

	itklsChangeFrame 0
}
proc itklsChangeFrame { value } {

	global lsGUImagWindow
	global lsGUIpotWindow
	global gOptions

	global itklsGUIParams

	set renMag $lsGUImagWindow(ren)
	set renPot $lsGUIpotWindow(ren)

	global lsGUIpreviousPositionNumber
	global lsGUIpreviousPathNumber

	# set base string
	global lsGUIcurrentPositionNumber
	set posId $lsGUIcurrentPositionNumber
	global lsGUIcurrentPathNumber
	set pathId $lsGUIcurrentPathNumber

	global gPathPoints
  set path $gPathPoints($pathId,splinePts)
  array set items [lindex $path $posId]
  set pos [TupleToList $items(p)]
  set nrm [TupleToList $items(t)]
  set xhat [TupleToList $items(tx)]

	set baseName /lsGUI/$pathId/$posId

	set lsGUIpreviousPositionNumber $posId
	set lsGUIpreviousPathNumber $pathId

	set magImg /tmp/lsGUI/mag
	set potImg /tmp/lsGUI/pot
	catch {repos_delete -obj $potImg}

	#puts "$itklsGUIParams(showPot)"
	if { $itklsGUIParams(useEdgeImage) == "0"} {
		switch -exact $itklsGUIParams(showPot) {
			Stg1 {
				itkutils_GradientMagnitudeGaussian -src $magImg -dst $potImg \
				-sigma $itklsGUIParams(gSigma1)
			}
			Stg2 {
				itkutils_GradientMagnitudeGaussian -src $magImg -dst $potImg \
				-sigma $itklsGUIParams(gSigma2)
			}
			Default {
				itkutils_GradientMagnitudeGaussian -src $magImg -dst $potImg \
				-sigma 0
			}
		}
	} elseif { $itklsGUIParams(useEdgeImage) == "distance"} {
		global gOptions
  		set ext $gOptions(resliceDims)
  		set src $itklsGUIParams(edgeImage)

		  set inpImg /tmp/lsGUI/user
		  set potImg /tmp/lsGUI/pot

		  catch {repos_delete -obj $potImg}
		  catch {repos_delete -obj $inpImg}

		  img_getSliceAtPathPoint $src $path $posId $ext $inpImg ->
		  if { $itklsGUIParams(showDistanceMap) == "1"} {

			  switch -exact $itklsGUIParams(showPot) {
					Stg1 {
						itkutils_DistanceImage -src $inpImg -dst $potImg -thres $itklsGUIParams(gSigma1)
					}
					Stg2 {
						itkutils_DistanceImage -src $inpImg -dst $potImg -thres $itklsGUIParams(gSigma2)
					}
					Default {
						puts "default"
						catch {repos_delete -obj $potImg}
						img_getSliceAtPathPoint $src $path $posId $ext $potImg ->
					}
				}
			} else {
				switch -exact $itklsGUIParams(showPot) {
					Stg1 {
						itkutils_ThresholdImage -src $inpImg -dst $potImg -thres $itklsGUIParams(gSigma1)
					}
					Stg2 {
						itkutils_ThresholdImage -src $inpImg -dst $potImg -thres $itklsGUIParams(gSigma2)
					}
					Default {
						puts "default"
						catch {repos_delete -obj $potImg}
						img_getSliceAtPathPoint $src $path $posId $ext $potImg ->
					}
				}
		}

	}


	foreach guy { mag pot } {
		set w lsGUI[subst $guy]Window
		set img /tmp/lsGUI/$guy
		if { [info exists [subst $w](ren)] &&
			[subst $[subst $w](ren)] != "" } then {
				itklsUpdateView $w $img $baseName 
				} else {
					puts "No render window in $w, so no view to update" 
				}
			}
			# indent )
# indent (
	global itklsGUIParamsBatch
	global lsGUIcurrentGroup
	set {itklsGUIParamsBatch(groupName)} "$lsGUIcurrentGroup"
	global lsGUIcurrentPathNumber
	set {itklsGUIParamsBatch(pathId)} $lsGUIcurrentPathNumber
}

proc itklsGUITabOpen { } {
	
	global itklsGUIParams
	itklsGUIUpdatePhyRadius -1
	global itklsGUIParamsBatch
	global lsGUIcurrentGroup
	set {itklsGUIParamsBatch(groupName)} "$lsGUIcurrentGroup"
	global lsGUIcurrentPathNumber
	set {itklsGUIParamsBatch(pathId)} $lsGUIcurrentPathNumber
}


proc itklsUpdateView { window img baseName } {
	global $window
	global gOptions

	foreach i [array names $window] {
		set $i [subst $[subst $window]($i)]
	}

	global itklsGUIParams
	foreach i [array names itklsGUIParams] {
		set $i $itklsGUIParams($i)
	}

	catch {crd_ren $ren}
	gdscGeneralView $ren $img

	if {$showInitSphere=="1"} {
		set seedPd /tmp/lsGUI/seedPd
		catch {repos_delete -obj $seedPd}
		itkutils_GenerateCircle -result $seedPd -r $phyRadius -x $phyCntrX -y $phyCntrY -z 0
		repos_setLabel -obj $seedPd -key color -value $gOptions(color_for_seeds)
		repos_setLabel -obj $seedPd -key width -value $gOptions(line_width_for_seeds)
      	repos_setLabel -obj $seedPd -key opacity -value $gOptions(opacity_for_seeds)
		gdscGeneralView $ren $seedPd    
	}
	
	if {$showLS =="1"} {
		if {[repos_exists -obj $baseName/ls/unclean] == 1} {
			
			gdscGeneralView $ren $baseName/ls/unclean
		}

		if {[repos_exists -obj $baseName/ls] == 1} {
			set pdname $baseName/ls
			catch {repos_clearLabel -obj $pdname -key color}
			catch {repos_clearLabel -obj $pdname -key width}
			repos_setLabel -obj $pdname -key color -value $gOptions(color_for_new_segmentations)
			repos_setLabel -obj $pdname -key width -value $gOptions(line_width_for_new_segmentations)
			gdscGeneralView $ren $pdname
		}

	}

}

proc itkLSStg1 { } {
	global lsGUIcurrentPositionNumber
	set posId $lsGUIcurrentPositionNumber
	global lsGUIcurrentPathNumber
	set pathId $lsGUIcurrentPathNumber
	global gPathPoints
	global gOptions

	global itklsGUIParams
	foreach i [array names itklsGUIParams] {
		set $i $itklsGUIParams($i)
	}

	set itklset /tmp/lsGUI/itklset
	if {![cmdExists $itklset]} {
		itkls2d $itklset
	}
	$itklset SetDebug -input 0

	set src volume_image
	set path $gPathPoints($pathId,splinePts)
	set ext $gOptions(resliceDims)

	set magImg /img/$pathId/$posId/mag
	catch {repos_delete -obj $magImg}
	img_getSliceAtPathPoint $src $path $posId $ext $magImg ->

	if { $itklsGUIParams(useEdgeImage) == "0"} {
		set inImg $magImg
		$itklset SetUseInputImageAsFeature -input 0
		puts "normal segmenation"
	} elseif { $itklsGUIParams(useEdgeImage) == "distance"} {

			set src $itklsGUIParams(edgeImage)
		  set inpImg /img/$pathId/$posId/user
		  set distImg /img/$pathId/$posId/dist

		  catch {repos_delete -obj $inpImg}
		  catch {repos_delete -obj $distImg}

		  img_getSliceAtPathPoint $src $path $posId $ext $inpImg ->
		  itkutils_DistanceImage -src $inpImg -dst $distImg -thres $gSigma1
		  
		  set inImg $distImg
			$itklset SetUseInputImageAsFeature -input 1
			puts "using edge image!"
	}
	set lsres_unclean /lsGUI/$pathId/$posId/ls/unclean

	set seedPd /tmp/lsGUI/seedPd
	if {![repos_exists -obj $seedPd] } {
		itkutils_GenerateCircle -result $seedPd -r $phyRadius -x $phyCntrX -y $phyCntrY -z 0
	}


	$itklset SetMaxIterations -input $maxIter1
	$itklset SetMaxRMSError -input $maxErr1
	$itklset SetAdvectionScaling -input 1
	$itklset SetCurvatureScaling -input 1
	$itklset SetInputs -image $inImg -seed $seedPd
	# $itklset SetUseNormalVectorCurvature -input 0

	if { $itklsGUIParams(GACls) != "1"} {
		$itklset PhaseOneLevelSet -Kc $kThr -expRising $expRise -expFalling $expFall -sigmaFeat $gSigma1 -sigmaAdv $advSigma1
	} else {
		$itklset GACLevelSet -expFactor $expFall -sigmaSpeed $gSigma1
	}
	catch {repos_delete -obj $lsres_unclean}
	puts "extracting $lsres_unclean"
	$itklset GetFront -out $lsres_unclean

	repos_setLabel -obj $lsres_unclean -key color -value yellow
}    

proc itkLSStg2 { } {

	global lsGUIcurrentPositionNumber
	set posId $lsGUIcurrentPositionNumber
	global lsGUIcurrentPathNumber
	set pathId $lsGUIcurrentPathNumber
	global gPathPoints
	global gOptions

	global itklsGUIParams
	foreach i [array names itklsGUIParams] {
		set $i $itklsGUIParams($i)
		#puts "$i $itklsGUIParams($i)"
	}
	set itklset /tmp/lsGUI/itklset2
	if { ![cmdExists $itklset] } {
		ITKLevelSet2D $itklset
	}
	$itklset SetDebug -input 0
	set src volume_image
	set path $gPathPoints($pathId,splinePts)
	set ext $gOptions(resliceDims)

	set magImg /img/$pathId/$posId/mag
	catch {repos_delete -obj $magImg}
	img_getSliceAtPathPoint $src $path $posId $ext $magImg ->

	if { $itklsGUIParams(useEdgeImage) == "0"} {
		set inImg $magImg
		$itklset SetUseInputImageAsFeature -input 0
		puts "normal segmenation"
	} elseif { $itklsGUIParams(useEdgeImage) == "distance"} {

			set src $itklsGUIParams(edgeImage)
		  set inpImg /img/$pathId/$posId/user
		  set distImg /img/$pathId/$posId/dist

		  catch {repos_delete -obj $inpImg}
		  catch {repos_delete -obj $distImg}

		  img_getSliceAtPathPoint $src $path $posId $ext $inpImg ->
		  itkutils_DistanceImage -src $inpImg -dst $distImg -thres $gSigma2
		  
		  set inImg $distImg
			$itklset SetUseInputImageAsFeature -input 1
			puts "using edge image!"
	}

	set lsres_unclean /lsGUI/$pathId/$posId/ls/unclean
	set lsres_unclean2 /lsGUI/$pathId/$posId/ls/unclean2
	set lsres /lsGUI/$pathId/$posId/ls
	set lsoriented /lsGUI/$pathId/$posId/ls/oriented

	if {![repos_exists -obj $lsres_unclean]} {
		itkLSStg1
	}
	puts $inImg
	$itklset SetMaxIterations -input $maxIter2
	$itklset SetMaxRMSError -input $maxErr2
	$itklset SetAdvectionScaling -input 1
	$itklset SetCurvatureScaling -input 1
	$itklset SetInputs -image $inImg -seed $lsres_unclean
	#$itklset SetUseNormalVectorCurvature -input 0

	$itklset PhaseTwoLevelSet -Klow $kLow -Kupp $kUpp -sigmaFeat $gSigma2 -sigmaAdv $advSigma2

	catch {repos_delete -obj $lsres_unclean2}
	catch {repos_delete -obj $lsres}

	puts "extracting $lsres_unclean2"
	$itklset GetFront -out $lsres_unclean2 


	geom_mergePts -src $lsres_unclean2 -dst $lsres -tol 0.001
	repos_setLabel -obj $lsres -key color -value red

	# orient profile
	global symbolicName
	global lsGUIcurrentPathNumber
	set pathId $lsGUIcurrentPathNumber

	global gPathPoints 
	set path $gPathPoints($pathId,splinePts)

	global lsGUIcurrentPositionNumber
	set posId $lsGUIcurrentPositionNumber

	array set items [lindex $path $posId]
	set pos [TupleToList $items(p)]
	set nrm [TupleToList $items(t)]
	set xhat [TupleToList $items(tx)]

	catch {repos_delete -obj $lsoriented}
	geom_orientProfile -src $lsres -path_pos $pos -path_tan $nrm -path_xhat $xhat -dst $lsoriented

	# Set Labels 
	foreach tagobj [list $lsres $lsoriented] {
		repos_setLabel -obj $tagobj -key pathId -value $pathId
		repos_setLabel -obj $tagobj -key posId -value $posId
		repos_setLabel -obj $tagobj -key pos -value $pos
		repos_setLabel -obj $tagobj -key nrm -value $nrm
		repos_setLabel -obj $tagobj -key xhat -value $xhat
		repos_setLabel -obj $tagobj -key creation_method -value "itklevelset"
		repos_setLabel -obj $tagobj -key creation_type -value "itklevelset"
		foreach param [array names itklsGUIParams] {
			set val $itklsGUIParams($param)
			repos_setLabel -obj $tagobj -key $param -value $val
		}
	}
}

proc seg_LoadEdgeImageMha {fn} {

  global gOptions
  global gImageVol
  global gImageFoo
  global gPathBrowser
  global gRen3d
  global gInitCamera
  global itklsGUIParams

  global gOptions

    if {![file exists $fn]} {
  return -code error "couldn't find $fn"
    }
  catch {reader Delete}
  vtkMetaImageReader reader
  reader SetFileName $fn
  reader Update

  set imgobj user_edge_image
  set itklsGUIParams(edgeImage) $imgobj
  catch {repos_delete -obj $imgobj}
  repos_importVtkImg -src [reader GetOutput] -dst $imgobj
}


proc itklsGUIUpdatePhyRadius { {value -1} } {
	global itklsGUIParams
    global lsGUIradius
    global lsGUIctrX
    global lsGUIctrY

	set sp [lindex [[repos_exportToVtk -src /tmp/lsGUI/mag] GetSpacing] 0]

	set {itklsGUIParams(phyRadius)} [expr $lsGUIradius * $sp]

	#set {itklsGUIParams(phyRadius)} [expr $itklsGUIParams(radius) * $sp]

	itklsDo $value
}

proc itklsGUIUpdateRadius { {value -1} } {
	global itklsGUIParams
	global lsGUIradius
	global lsGUIctrX
	global lsGUIctrY

	set sp [lindex [[repos_exportToVtk -src /tmp/lsGUI/mag] GetSpacing] 0]

	set {lsGUIradius} [expr $itklsGUIParams(phyRadius) / $sp]

	itklsDo $value
}

proc itklsUpdatePhyCenter { {value -1} } {

	global itklsGUIParams
	global lsGUIradius
	global lsGUIctrX
	global lsGUIctrY
	set spx [lindex [[repos_exportToVtk -src /tmp/lsGUI/mag] GetSpacing] 0]
	set spy [lindex [[repos_exportToVtk -src /tmp/lsGUI/mag] GetSpacing] 1]

	set {itklsGUIParams(phyCntrX)} [expr $lsGUIctrX * $spx]
	set {itklsGUIParams(phyCntrY)} [expr $lsGUIctrY * $spy]

	itklsDo $value
}

proc itklsUpdateCenter {{value -1}} {
	global itklsGUIParams
	global lsGUIradius
	global lsGUIctrX
	global lsGUIctrY
	set spx [lindex [[repos_exportToVtk -src /tmp/lsGUI/mag] GetSpacing] 0]
	set spy [lindex [[repos_exportToVtk -src /tmp/lsGUI/mag] GetSpacing] 1]

	set {lsGUIctrX} [expr $itklsGUIParams(phyCntrX) / $spx]
	set {lsGUIctrY} [expr $itklsGUIParams(phyCntrY) / $spy]

	itklsDo $value
}
proc gui_toggle_2d_views { } {
	global guiSVvars
	puts "last toggle $guiSVvars(last_2d_view_toggle)"
	if {$guiSVvars(last_2d_view_toggle) != "0"} {
		resetViews
	} else {
		unset2DViews
	}
}

proc gui_toggle_3d_seed { {value -1}} {

global gui3Dvars
if {$value =="-1"} {set value $gui3Dvars(show_seed_default)}
set gui3Dvars(chooserShowCursorFlag) $value
set gui3Dvars(chooserShowSphereFlag) $value
set gui3Dvars(chooserShowAllSeedsFlag) $value 

gui3DchooserShowSphere clicked
gui3DchooserShowCursor
gui3DchooserShowAllSeeds clicked

}
proc gui_toggle_3d_branch { {value -1}} {

global gui3Dvars
global lsGUIdisplayPathFlag
global lsGUIdisplayProbeFlag
if {$value =="-1"} {set value $gui3Dvars(show_branch_default)}

set lsGUIdisplayPathFlag $value
set lsGUIdisplayProbeFlag $value

lsGUIdisplayPath
lsGUIdisplayProbe
lsGUIupdatePositionScale 0

}

proc gui_show_seg_slice { } {
	global guiSVvars
	puts "last toggle $guiSVvars(showSegSlice)"
	if {$guiSVvars(showSegSlice) != "0"} {
		global lsGUImagWindow
		set lsGUImagWindow(showSegSlice) 1

		global lsGUIpotWindow
		set lsGUIpotWindow(showSegSlice) 1
		
	} else {
		global lsGUImagWindow
		set lsGUImagWindow(showSegSlice) 0

		global lsGUIpotWindow
		set lsGUIpotWindow(showSegSlice) 0
	}
	lsGUIchangeFrame 0
}
proc resetViews { } { 

	global lsGUIcurrentPathNumber
	global lsGUIcurrentPositionNumber
	global lsGUIcurrentGroup

	global gPathPoints
	set pathIds ""
	set pathNames ""

	# get path names and ids
	set funkynames [lsort -dictionary [array names gPathPoints *,name]]
	foreach funkyname $funkynames {
		set pathId [lindex [split [string trim $funkyname] ,] 0]
		set pathName [lindex [split [string trim $funkyname] ,] 1]
		foreach i [array names gPathPoints] {
      set ab [split $i ,]
      set a [lindex $ab 0]
      set b [lindex $ab 1]
      if {$a == $pathId} {
	        if {$b == "name"} {
	         	set pathName $gPathPoints($i)
	        } elseif {$b == "numSplinePts"} {
	            set numSplinePts $gPathPoints($i)
	        } elseif {$b == "splinePts"} {
	                  # skip splined path
	        } elseif {$b == "creationTimestamp"} {
	                  # skip
	        } elseif {$b == "lastUpdateTimestamp"} {
	                  # skip
	        } else {
	                #skip
	        }
      }
    }
	lappend pathIds $pathId
	lappend pathNames $pathName
	}

	
	# search for current selected path
	puts "pathIds: $pathIds"
	puts "pathNames: $pathNames"
	set lind [lsearch -exact $pathIds lsGUIcurrentPathNumber]

	#if selected path does not exist, set the path to the first path in path ids
	if { $lind < 0 } {
		set lsGUIcurrentPathNumber [lindex $pathIds 0]
		global lsGUIcurrentPathLabel
		set lsGUIcurrentPathLabel [lindex $pathNames 0]
		lsGUIupdatePath
	}
	
	# Set the display to 3d and 2d
	guiCV_display_windows 3d_and_2d
	
	# Update 2d pot window
	global lsGUImagWindow
	set lsGUImagWindow(showLS) 1
	set lsGUImagWindow(showInitSphere) 1
	set lsGUImagWindow(showWindow) 1
	 lsGUIupdateMagWindow

	global lsGUIpotWindow
	set lsGUIpotWindow(showLS) 1
	set lsGUIpotWindow(showInitSphere) 1
	set lsGUIpotWindow(showWindow) 1
	lsGUIupdatePotWindow

	#global lsGUIparallelProjectionFlag
  	#set $lsGUIparallelProjectionFlag 1

  	lsGUIupdatePositionScale 0
	#lsGUIparallelProjection
	#itklsChangeFrame 0
}

proc unset2DViews { } { 

	lsGUIupdatePositionScale 0

	#global lsGUIparallelProjectionFlag
  	#set $lsGUIparallelProjectionFlag 0
	# Update 2d pot window
	global lsGUImagWindow
	set lsGUImagWindow(showLS) 0
	set lsGUImagWindow(showInitSphere) 0
	set lsGUImagWindow(showWindow) 0
	lsGUIupdateMagWindow
	lsGUIupdateMagWindow

	global lsGUIpotWindow
	set lsGUIpotWindow(showLS) 0
	set lsGUIpotWindow(showInitSphere) 0
	set lsGUIpotWindow(showWindow) 0
	lsGUIupdatePotWindow
	lsGUIupdatePotWindow

	guiCV_display_windows 3d_only

	lsGUIupdateMagWindow
	lsGUIupdatePotWindow
	lsGUIupdatePositionScale 0
	#itklsChangeFrame 0
}

proc itkAddSegToGroup { } {

	lsGUIaddToGroup levelset
}

proc itklsBatchUpdateList { {value 0} } {

	itklsUpdateBatchInfo
	global itklsGUIParamsBatch
	foreach i [array names itklsGUIParamsBatch] {
		set $i $itklsGUIParamsBatch($i)
	}

	if {[string is integer $begin] &&
		[string is integer $end] &&
		[string is integer $inc] } then {
			set posList [numlist $begin $end $inc] 
		}
		set {itklsGUIParamsBatch(posList)} $posList
		# indent) 
}

proc itklsUpdateBatchInfo {} {
	global lsGUIcurrentGroup
	set {itklsGUIParamsBatch(groupName)} "$lsGUIcurrentGroup"
	global lsGUIcurrentPathNumber
	set {itklsGUIParamsBatch(pathId)} $lsGUIcurrentPathNumber
}
proc itklsBatchRun { } { 
	# find min/max frame number
	global itklsGUIParamsBatch
	global symbolicName

	global lsGUIcurrentGroup
	set {itklsGUIParamsBatch(groupName)} "$lsGUIcurrentGroup"
	global lsGUIcurrentPathNumber
	set {itklsGUIParamsBatch(pathId)} $lsGUIcurrentPathNumber

	foreach i [array names itklsGUIParamsBatch] {
		set $i $itklsGUIParamsBatch($i)
	}
	set minFrame [expr int([$symbolicName(lsGUIpathPositionScale) cget -from])]
	set maxFrame [expr int([$symbolicName(lsGUIpathPositionScale) cget -to])]
	set posList [string_parse $itklsGUIParamsBatch(posList) $minFrame $maxFrame]
	if {[llength $posList] == 0} {
		puts "ERROR:  No frames specified."
		return -code error "ERROR:  No frames specified."
	}
	puts "$pathId $posList $groupName"
	itkLSDoBatch $pathId "$posList" $groupName
}

proc printOptions { var } {
	global $var
	foreach i [array names $var] {
		puts "$i: [subst $[subst $var]($i)]"
	}
}

proc itkLSOnPos {pathId posId} {

	global gOptions
	global gPathPoints
	global itklsGUIParams

	set src volume_image
	set path $gPathPoints($pathId,splinePts)
	set ext $gOptions(resliceDims)

	set rtnImg /img/$pathId/$posId/mag
	set rtnPot /img/$pathId/$posId/pot
	catch {repos_delete -obj $rtnImg}
	catch {repos_delete -obj $rtnPot}
	img_getSliceAtPathPoint $src $path $posId $ext $rtnImg $rtnPot

	if { $itklsGUIParams(useEdgeImage) == "0"} {
		puts "normal segmenation"
	} elseif { $itklsGUIParams(useEdgeImage) == "distance"} {

			set src $itklsGUIParams(edgeImage)
		  set inpImg /tmp/lsGUI/user
		  set potImg /img/$pathId/$posId/mag

		  catch {repos_delete -obj $potImg}
		  catch {repos_delete -obj $inpImg}

		  img_getSliceAtPathPoint $src $path $posId $ext $inpImg ->
		  itkutils_DistanceImage -src $inpImg -dst $potImg -thres $itklsGUIParams(gSigma1)

	} elseif { $itklsGUIParams(useEdgeImage) == "blur"} {
		if { [repos_exists -obj $itklsGUIParams(edgeImage)] != "0"} {
			global gOptions
  		set ext $gOptions(resliceDims)
  		set src $itklsGUIParams(edgeImage)

		  set inpImg /tmp/lsGUI/user
		  set potImg /img/$pathId/$posId/mag

		  catch {repos_delete -obj $potImg}
		  catch {repos_delete -obj $inpImg}

		  img_getSliceAtPathPoint $src $path $posId $ext $inpImg ->

			switch -exact $itklsGUIParams(showPot) {
				Stg1 {
					itkutils_GaussianBlur -src $inpImg -dst $potImg \
					-sigma $itklsGUIParams(gSigma1)
				}
				Stg2 {
					itkutils_GaussianBlur -src $inpImg -dst $potImg \
					-sigma $itklsGUIParams(gSigma2)
				}
				Default {
					itkutils_GaussianBlur -src $inpImg -dst $potImg \
					-sigma 0
				}
			}
		} else {
			puts "No edge Image!"
		}
	}


	if { [array exists params] } {
		unset params
	}


	array set params [array get itklsGUIParams]

	array names params
	#printOptions params

	global lsGUIradius
    global lsGUIctrX
    global lsGUIctrY

	set spx [lindex [[repos_exportToVtk -src $rtnImg] GetSpacing] 0]
	set spy [lindex [[repos_exportToVtk -src $rtnImg] GetSpacing] 1]

	set $params(phyRadius) [expr $lsGUIradius * $spx]
	set $params(phyCntrX) [expr $lsGUIctrX * $spx]
	set $params(phyCntrY) [expr $lsGUIctrY * $spy]

	puts "doing segmentation"
	set lsres /lsGUI/$pathId/$posId/ls
	catch {repos_delete -obj $lsres}
	itkLSOnImage $rtnImg $lsres [array get params]

	# orient profile
	set path $gPathPoints($pathId,splinePts)

	array set items [lindex $path $posId]
	set pos [TupleToList $items(p)]
	set nrm [TupleToList $items(t)]
	set xhat [TupleToList $items(tx)]

	set lsoriented /lsGUI/$pathId/$posId/ls/oriented
	catch {repos_delete -obj $lsoriented}
	geom_orientProfile -src $lsres -path_pos $pos -path_tan $nrm -path_xhat $xhat -dst $lsoriented
}	

proc itkLSOnImage {img lsres paramsstr} {

	array set params $paramsstr
	foreach i [array names params] {
		set $i $params($i)
		puts "$i $params($i)"
	}

	set seedPd $lsres/seedPd
	catch {repos_delete -obj $seedPd}
	puts "Generating seed at $seedPd"
	itkutils_GenerateCircle -result $seedPd -r $phyRadius -x $phyCntrX -y $phyCntrY -z 0

	set lsres_unclean $lsres/unclean
	set lsres_unclean2 $lsres/unclean2
	catch {repos_delete -obj $lsres_unclean}
	catch {repos_delete -obj $lsres_unclean2}

	set itklset /tmp/lsGUI/itklset
	if {![cmdExists $itklset]} {
		ITKLevelSet2D $itklset
	}
	$itklset SetDebug -input 0
	set magImg $img

	$itklset SetMaxIterations -input $maxIter1
	$itklset SetMaxRMSError -input $maxErr1
	$itklset SetAdvectionScaling -input 1
	$itklset SetCurvatureScaling -input 1
	$itklset SetInputs -image $magImg -seed $seedPd

	$itklset SetMaxIterations -input $maxIter1
	$itklset SetMaxRMSError -input $maxErr1
	$itklset SetAdvectionScaling -input 1
	$itklset SetCurvatureScaling -input 1
	$itklset SetInputs -image $magImg -seed $seedPd

	if { $useEdgeImage == "0"} {
		#set magImg /tmp/lsGUI/mag
		$itklset SetUseInputImageAsFeature -input 0
	} else {
		$itklset SetUseInputImageAsFeature -input 1
		puts "using edge image!"
	}

	puts "performing stage one levelset"
	$itklset PhaseOneLevelSet -Kc $kThr -expRising $expRise -expFalling $expFall -sigmaFeat $gSigma1 -sigmaAdv $advSigma1

	puts "extracting stage 1 into $lsres_unclean"
	$itklset GetFront -out $lsres_unclean 
	repos_setLabel -obj $lsres_unclean -key color -value yellow

	set itklset2 /tmp/lsGUI/itklset2
	if {![cmdExists $itklset2]} {
		ITKLevelSet2D $itklset2
	}

	if { $useEdgeImage == "0"} {
		$itklset2 SetUseInputImageAsFeature -input 0
	} else {
		$itklset2 SetUseInputImageAsFeature -input 1
		puts "using edge image!"
	}


	$itklset2 SetMaxIterations -input $maxIter2
	$itklset2 SetMaxRMSError -input $maxErr2
	$itklset2 SetAdvectionScaling -input 1
	$itklset2 SetCurvatureScaling -input 1
	$itklset2 SetInputs -image $magImg -seed $lsres_unclean

	$itklset2 PhaseTwoLevelSet -Klow $kLow -Kupp $kUpp -sigmaFeat $gSigma2 -sigmaAdv $advSigma2

	puts "extracting stage 2 into $lsres_unclean2"
	$itklset2 GetFront -out $lsres_unclean2 

	puts "cleaning into $lsres"
	geom_mergePts -src $lsres_unclean2 -dst $lsres -tol 0.001
	repos_setLabel -obj $lsres -key color -value green
}


proc itkLSDoBatch {pathId posList groupName} {

	# set base string
	global lsGUIcurrentPositionNumber
	global lsGUIcurrentPathNumber
	global lsGUIcurrentGroup
	global gOptions
	global gPathPoints

	set orgPosId $lsGUIcurrentPositionNumber
	set orgPathId $lsGUIcurrentPathNumber
	set orgGroup $lsGUIcurrentGroup
	set lsGUIcurrentPathNumber $pathId
	set lsGUIcurrentGroup $groupName
	
	global itklsGUIParamsBatch
	set addToGroup $itklsGUIParamsBatch(addToGroup)
	set smooth $itklsGUIParamsBatch(smooth)

	set pll [llength $posList]
	#puts "pathId: $pathId \n"
	set notDoneList {}
	for {set idx 0} {$idx < [llength $posList]} {incr idx 1} {
		set lsGUIcurrentGroup $groupName
		
		set posId [lindex $posList $idx]
		global lsGUIcurrentPositionNumber
		set lsGUIcurrentPositionNumber $posId
		itkLSOnPos $pathId $posId
		set seg /lsGUI/$pathId/$posId/ls/oriented

		set addToGrp $itklsGUIParamsBatch(addToGroup)
		if { $itklsGUIParamsBatch(smooth) == "1" } {
		    if { [lsGUIfourierSmoothSeg itklevelset 1] != "0"} {
		    	set addToGrp "0"
		    }
		}

		catch {repos_delete -obj tmp/pd}
		if { [catch { geom_sampleLoop -src $seg -num 20 -dst tmp/pd} fid] } {
			puts "Cannot loft:\n$fid"
			set addToGrp "0"
			lappend notDoneList $posId
			catch {repos_delete -obj tmp/pd}
		}
		catch {repos_delete -obj tmp/pd}

	
		if { $addToGrp == "1" } {
			lsGUIaddToGroup levelset
		}	
		after 1
		lsGUIupdatePositionScale 0
		itklsChangeFrame 0

	}

puts "notDoneList $notDoneList"

} 

proc numlist {a b c} {
	#puts "[expr ($b-$a)/($c)]"
	eval return \[list $a [string repeat "\[incr a $c\] " [expr ($b-$a)/($c)]] \]
}

proc itkls3dDoNothing {{value 0} args} {
	tk_messageBox -title "Not Available"  -type ok -message "This functionality is not yet supported!"
    return
}

proc itk3dLSHalt { {value 0} } {
	tk_messageBox -title "Not Available"  -type ok -message "This functionality is not yet supported!"
    return
}

proc itk3dLSSave { {value 0} } {
	global gui3Dvars
	set segPd $gui3Dvars(ls_finalSegmentation)

	set fn [tk_getSaveFile -initialdir [pwd] -filetypes {{"VTK" .vtp}} -defaultextension .vtp]
	if {$fn == ""} return

	repos_writeXMLPolyData $segPd $fn
}

proc itk3dLSRun {img seedPd out} {
	
	set itklset /tmp/lsGUI/itk3dlset
	if {![cmdExists $itklset]} {
		itkls3d $itklset
	}

	$itklset SetDebug -input 0
	global gRen3d
	global gui3Dvars
	foreach i [array names gui3Dvars] {
			set $i $gui3Dvars($i)
		}
	#Common Params (probably)
	$itklset SetMaxIterations -input $ls_ls_numiter
	$itklset SetMaxRMSError -input 0
	$itklset SetAdvectionScaling -input $ls_ls_adv
	$itklset SetCurvatureScaling -input $ls_ls_curv
	$itklset SetPropagationScaling -input $ls_ls_prop
	$itklset SetInputs -image $img -seed $seedPd

	if {$ls_init_option == "smooth_poly"} {
		$itklset CopyFrontToSeed
	} else {
		$itklset SetBinarySeed -input 1
	}

	#GAC Params and run
	if {$ls_ls_type =="GeoSmooth" } {
		$itklset SetLaplacianScaling -input $ls_ls_iso
		$itklset GACLevelSet -expFactor $ls_edge_prox_exponent -sigmaSpeed $ls_edge_prox_sigma -kappa $ls_edge_prox_kappa -iso $ls_ls_iso
	} elseif {$ls_ls_type =="LaplFastEdge" } {
		$itklset LaplacianLevelSet -expFactor $ls_edge_prox_exponent -sigmaSpeed $ls_edge_prox_sigma -kappa $ls_edge_prox_kappa -iso $ls_ls_iso
	}
	catch {repos_delete -obj $out}
	puts "extracting $out"
	$itklset GetFront -out $out
	itk3dLS_updateEdgeImage 0

}

proc itk3dLS_updateEdgeImage {value} {

global gRen3dFreeze
global gRen3d
global guiVIB
set oldFreeze $gRen3dFreeze
set gRen3dFreeze 1
global gui3Dvars
foreach i [array names gui3Dvars] {
		set $i $gui3Dvars($i)
	}

if { ![info exists guiVIB(current_color_map)]} {
	set guiVIB(current_color_map) $guiVIB(color_map)
}

if {$ls_image_show_option == "edge"} {

	set potImg /tmp/lsGUI/pot
	catch {repos_delete -obj $potImg}
	itkutils_FractEdgeProximity3D -src volume_image -dst $potImg -sigma $ls_edge_prox_sigma -kappa $ls_edge_prox_kappa -exponent $ls_edge_prox_exponent
	set imgobj $potImg
	set guiVIB(show_vol_render) 0
	volGUIupdateVolRender guiVIB all
	set guiVIB(repos_image_obj_alt) $imgobj
	set guiVIB(use_alt_image) 1
	set cmap native
	set guiVIB(current_color_map) $guiVIB(color_map)
} else {
	set guiVIB(use_alt_image) 0
	set guiVIB(repos_image_obj) 
	set guiVIB(use_alt_image) 0
	set imgobj volume_image
	set cmap $guiVIB(current_color_map)
}

vis_img_VolImgBrwsr2 $imgobj vis_img_vol_browser

set guiVIB(color_map) $cmap
volGUIupdateColorMaps guiVIB
volGUIupdateSlider guiVIB RL $guiVIB(sliderx_value)
volGUIupdateSlider guiVIB AP $guiVIB(slidery_value)
volGUIupdateSlider guiVIB SI $guiVIB(sliderz_value)
set gRen3dFreeze oldFreeze
vis_render $gRen3d
}

proc lsGUI3dSwapSeedpanel {{value 0}} {
	global gui3Dvars
	global symbolicName
	puts $value
	set move_type "$gui3Dvars(seed_move_type)"

	if {$move_type == "path"} {
		guiCV_swapframes $symbolicName(gui3d_seed_move_frame_xyz) $symbolicName(gui3d_seed_move_frame_path)
		lsGUI3dSeedupdatePositionScale 0
	} else {
		guiCV_swapframes $symbolicName(gui3d_seed_move_frame_path) $symbolicName(gui3d_seed_move_frame_xyz)
		
	}
}

proc lsGUI3dSwapLSPanel {{value 0}} {
global gui3Dvars

set ls_ls_type $gui3Dvars(ls_ls_type)

if {$ls_ls_type =="GeoSmooth" } {
	set {gui3Dvars(ls_ls_numiter)} {400}
	set {gui3Dvars(ls_ls_adv)} {.6}
	set {gui3Dvars(ls_ls_curv)} {.05}
	set {gui3Dvars(ls_ls_prop)} {.6}
	set {gui3Dvars(ls_ls_iso)} {.5}

} elseif {$ls_ls_type =="LaplFastEdge" } {
	set {gui3Dvars(ls_ls_numiter)} {1000}
	set {gui3Dvars(ls_ls_adv)} {1}
	set {gui3Dvars(ls_ls_curv)} {0}
	set {gui3Dvars(ls_ls_prop)} {1}
	set {gui3Dvars(ls_ls_iso)} {.5}

} else {
	puts "Unknown LS Type"
}

}


# Procedure: lsGUIupdatePositionScale
proc lsGUI3dSeedupdatePositionScale { value} {
	global symbolicName
	global lsGUIcurrentPathNumber
	global gPathPoints

	if {$lsGUIcurrentPathNumber == "-1"} {
	lsGUIselectPathSetup
	return
	}

	global lsGUIcurrentPathNumber
	set pathId $lsGUIcurrentPathNumber
	set path $gPathPoints($pathId,splinePts)

	global lsGUIcurrentPositionNumber
	set posId $lsGUIcurrentPositionNumber

	array set items [lindex $path $posId]
	set pos [TupleToList $items(p)]

	set posx [lindex $pos 0]
	set posy [lindex $pos 1]
	set posz [lindex $pos 2]
	puts  "$posx $posy $posz"

	global gImageVol
	global gOptions
	if {$gOptions(orientImgVolToRAS) == 0} {
		# old xyz system
		set org_x [lindex $gImageVol(vtk_org_xyz) 0]
		set org_y [lindex $gImageVol(vtk_org_xyz) 1]
		set org_z [lindex $gImageVol(vtk_org_xyz) 2]
		set ext_i $gImageVol(ext_i)
		set ext_j $gImageVol(ext_j)
		set ext_k $gImageVol(ext_k)
		set vdims_x $gImageVol(vdims_x)
		set vdims_y $gImageVol(vdims_y)
		set vdims_z $gImageVol(vdims_z)
		set voi_x0 $gImageVol(voi_x0)
		set voi_x1 $gImageVol(voi_x1)
		set voi_y0 $gImageVol(voi_y0)
		set voi_y1 $gImageVol(voi_y1)
		set voi_z0 $gImageVol(voi_z0)
		set voi_z1 $gImageVol(voi_z1)
	} else {
	# properly oriented volume in ras space
		set org_x [lindex $gImageVol(vtk_org_ras) 0]
		set org_y [lindex $gImageVol(vtk_org_ras) 1]
		set org_z [lindex $gImageVol(vtk_org_ras) 2]
		set ext_i [lindex $gImageVol(ext_ras) 0]
		set ext_j [lindex $gImageVol(ext_ras) 1]
		set ext_k [lindex $gImageVol(ext_ras) 2]
		set vdims_x [lindex $gImageVol(vdims_ras) 0]
		set vdims_y [lindex $gImageVol(vdims_ras) 1]
		set vdims_z [lindex $gImageVol(vdims_ras) 2]
		set voi_x0 [lindex $gImageVol(voi_ras) 0]
		set voi_x1 [lindex $gImageVol(voi_ras) 1]
		set voi_y0 [lindex $gImageVol(voi_ras) 2]
		set voi_y1 [lindex $gImageVol(voi_ras) 3]
		set voi_z0 [lindex $gImageVol(voi_ras) 4]
		set voi_z1 [lindex $gImageVol(voi_ras) 5]
	}

	set axes_x [expr ($posx-$org_x)/$vdims_x]
	set axes_y [expr ($posy-$org_y)/$vdims_y]
	set axes_z [expr ($posz-$org_z)/$vdims_z]


  set gImageVol(axes-x) $axes_x
  set gImageVol(axes-y) $axes_y
  set gImageVol(axes-z) $axes_z
  gui3DchooserUpdateXYZ

  lsGUIupdatePositionScale $value

}

proc lsGUIloftGroup {group} {
  global guiBOOLEANvars
  global gOptions
  set gOptions(meshing_solid_kernel) PolyData

  global lsGUIcurrentPositionNumber
  set posId $lsGUIcurrentPositionNumber
  global lsGUIcurrentPathNumber
  set pathId $lsGUIcurrentPathNumber
  global lsGUIcurrentGroup

  set sampling_default $guiBOOLEANvars(sampling_default)
  set lin_multiplier   $guiBOOLEANvars(linear_sampling_along_length_multiplier)
  set useLinearSampleAlongLength   $guiBOOLEANvars(use_linear_sampling_along_length)
  set numModes         $guiBOOLEANvars(num_modes_for_FFT)
  array set overrides  $guiBOOLEANvars(sampling_overrides)
  set useFFT           $guiBOOLEANvars(use_FFT)
  set sample_per_segment $guiBOOLEANvars(sampling_along_length_multiplier)
  set addCaps          $guiBOOLEANvars(add_caps_to_vessels)
  set addCaps          0
  set noInterOut       $guiBOOLEANvars(no_inter_output)

  set vecFlag 0
  set numOutPtsInSegs $sampling_default
  set numSegs [llength [group_get $group]]
  set numOutPtsAlongLength [expr $sample_per_segment * $numSegs]
  set numPtsInLinearSampleAlongLength [expr $lin_multiplier *$numOutPtsAlongLength]
  set outPD /guiGROUPS/polydatasurface/$group

  global guiPDvars
  global gRen3d

  if {$numSegs > 1} {

  	# if the object exist, get the labels before we delete
  	if {[repos_exists -obj $outPD] != 0} {
		foreach key [repos_getLabelKeys -obj $outPD] {
			set localKeys($key) [repos_getLabel -obj $outPD -key $key]
		}
  	}
  	catch {repos_delete -obj $outPD}

  	if {![info exists localKeys(color)]} {
  		set localKeys(color) $gOptions(color_for_saved_surface)
  	}
  	if {![info exists localKeys(opacity)]} {
  		set localKeys(opacity) $gOptions(opacity_for_saved_surface)
  	}

    solid_setKernel -name PolyData
    #polysolid_c_create_vessel_from_group $group $vecFlag  $useLinearSampleAlongLength $numPtsInLinearSampleAlongLength  $useFFT $numModes  $numOutPtsInSegs $numOutPtsAlongLength $addCaps $outPD
    polysolid_c_create_vessel_from_group $group $vecFlag  $useLinearSampleAlongLength $numPtsInLinearSampleAlongLength  $useFFT $numModes  $numOutPtsInSegs $numOutPtsAlongLength $addCaps 0 $outPD

    foreach key [array names localKeys] {
		repos_setLabel -obj $outPD -key $key -value $localKeys($key)
    }
    if [catch {vis_register $gRen3d $outPD}] {
       vis_pRm $gRen3d $outPD
    }



    gdscGeneralView $gRen3d $outPD
  } else {
    if {[vis_pGetActor $gRen3d $outPD] != ""} {
      vis_pRm $gRen3d $outPD
    }
  }

}







