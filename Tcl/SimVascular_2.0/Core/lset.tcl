#
#   Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                           University of California, San Diego.
#
#   All rights reserved.
#
#   Copyright (c) 1998-2007 Stanford University,
#   Charles Taylor, Nathan Wilson, Ken Wang.
#
#   All rights reserved.
#
#   See SimVascular Acknowledgements file for additional
#   contributors to the source code. 
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

# ---------
# lset_step
# ---------

proc lset_step {core zls vel v w rebuildPhiFreq} {
    global gLsetGridType
    if {[$core Vanished]} {
	return 0
    }
    if { ! [$core EvolveOneTimeStep] } {
	if {$v} {
	    puts "End lset evolution."
	}
	return 0
    }
    set ts [$core GetTimeStep]
    set maxV [$core FindMaxV]

    if {$gLsetGridType == "SparseGrid"} {
	if {[$core GridModified] || ($ts == 0)} {
	    $core SaveGrid -result /grid/$ts
	    puts "grid reconstructed --> /grid/$ts"
	    puts "grid stats:       [$core GetGridStats]"
	    puts "lsetCore sz (KB): [expr [$core GetMemoryUsage] / 1024.0]"
	}
    }

    if {$v} {
	puts [format "End time step \[%d\], maxV \[%f\]." $ts $maxV]
    }
    if {$zls} {
	$core ExtractFront -out /lset/$ts
#	$core ExtractPhi -out /phi/$ts
    }
    if {$vel} {
	$core ExtractVel -out /velocity/[expr $ts - 1]
    }
    if {[expr $ts % $rebuildPhiFreq] == 0} {
	$core RebuildPhi
    }
    if {$w} {
	if {![file exists repos]} {
	    exec mkdir repos
	}
	set pn [format "repos/lset-%d.vtk" $ts]
	set vn [format "repos/vel-%d.vtk" [expr $ts - 1]]
	catch {repos_writeVtkPolyData -obj /lset/$ts -type bin -file $pn}
	catch {repos_writeVtkPolyData -obj /velocity/[expr $ts - 1] \
		-type bin -file $vn}
    }
    return 1
}


# --------
# lset_for
# --------
# Returns a boolean value indicating whether the end of the given
# number of steps was reached.  False (end of loop not reached)
# indicates that a stopping condition was reached before the specified
# number of steps was taken.

proc lset_for {core nsteps maxStep renList v w rebuildPhiFreq} {
    global lset_for_last_vis_obj
    set code 1
    set zls 0
    set vel 0
    set endGfx 1
    if {[llength $renList] > 0} {
	set zls 1
	set vel 1
    }
    for {set i 0} {$i < $nsteps} {incr i} {
	if { ! [lset_step $core $zls $vel $v $w $rebuildPhiFreq]} {
	    set code 0
	    break
	}
	set ts [$core GetTimeStep]   
        if {![expr int($ts) % int(10)]} {
	  foreach ren $renList {
	    if [catch {vis_pRm $ren $lset_for_last_vis_obj} msg] {
              #puts "error: $msg"
	    }
            set lset_for_last_vis_obj /lset/[expr $ts - 1]
	    vis_pRepos $ren $lset_for_last_vis_obj
	    #catch {vis_vRepos $ren /velocity/[expr $ts - 1]}
	  }
	}
	if {($maxStep >= 0) && ($ts >= $maxStep)} {
	    if {$i < [expr $nsteps - 1]} {
		set code 0
	    }
	    set endGfx 0
	    break
	}
        global gOptions
        if {$gOptions(lset_allow_interrupt) != 0} {
          set chkstnow 0
          after 100 {set chkstnow 1}
          vwait chkstnow
          if {$gOptions(lset_interrupt) != 0} {
            set gOptions(lset_interrupt) 0
            return -code error "User Requested Halt of Segmentation!"
          }
	}
    }
    if {$endGfx} {
	set ts [$core GetTimeStep]
	foreach ren $renList {
	    if [catch {vis_pRm $ren $lset_for_last_vis_obj} msg] {
              #puts "error: $msg"
	    }
	    #catch {vis_pRm $ren $lset_for_last_vis_obj}
	    set lset_for_last_vis_obj /lset/$ts
	    vis_pRepos $ren $lset_for_last_vis_obj
	    #catch {vis_vRm $ren /velocity/[expr $ts - 1]}
	    #catch {vis_vRepos $ren /velocity/$ts}
	}
    }
    return $code
}


# ---------
# lset_getK
# ---------

proc lset_getK {core} {
    set ts [$core GetTimeStep]
    array set garr [$core GetGrid]
    set dim_z $garr(-gridz)
    if {$dim_z == 1} {
	$core GetBoundaryData -field curvature -result /K/$ts
	puts "--> /K/$ts"
	return
    }
    $core GetBoundaryData -field curvature_3d_m -result /Km/$ts
    puts "--> /Km/$ts"
    $core GetBoundaryData -field curvature_3d_g -result /Kg/$ts
    puts "--> /Kg/$ts"
    $core GetBoundaryData -field curvature_3d_k1 -result /k1/$ts
    puts "--> /k1/$ts"
    $core GetBoundaryData -field curvature_3d_k2 -result /k2/$ts
    puts "--> /k2/$ts"
    return
}


# ------------
# lset_getMinH
# ------------

proc lset_getMinH {core} {
    if {[catch {$core GetGrid} data]} {
	return -code error "couldn't get grid information"
    }
    array set arr $data
    set hx $arr(-hx)
    set hy $arr(-hy)
    set hz $arr(-hz)
    set minh [expr ($hx < $hy) ? $hx : $hy]
    set minh [expr ($minh < $hz) ? $minh : $hz]
    return $minh
}


# -------------
# lset_gridType
# -------------

proc lset_gridType {core} {
    if {[catch {$core GetGrid} items]} {
	return -code error
    }
    array set arr $items
    return $arr(-type)
}


# -------------
# lset_gridDims
# -------------

proc lset_gridDims {dom_pd h} {
    if {![repos_exists -obj $dom_pd]} {
	return -code error "$dom_pd not a repository object"
    }
    if {[repos_type -obj $dom_pd] != "PolyData"} {
	return -code error "$dom_pd not a PolyData object"
    }
    set bbox [geom_bbox -obj $dom_pd]
    set dims [list \
	    [expr int( floor( ([lindex $bbox 1] - [lindex $bbox 0]) \
	    / [lindex $h 0] ) ) + 1 ] \
	    [expr int( floor( ([lindex $bbox 3] - [lindex $bbox 2]) \
	    / [lindex $h 1] ) ) + 1 ] \
	    [expr int( floor( ([lindex $bbox 5] - [lindex $bbox 4]) \
	    / [lindex $h 2] ) ) + 1 ] ]
    return $dims
}


# -------------
# lset_gridOrig
# -------------

proc lset_gridOrig {dom_pd} {
    if {![repos_exists -obj $dom_pd]} {
	return -code error "$dom_pd not a repository object"
    }
    if {[repos_type -obj $dom_pd] != "PolyData"} {
	return -code error "$dom_pd not a PolyData object"
    }
    set bbox [geom_bbox -obj $dom_pd]
    set origin [list [lindex $bbox 0] [lindex $bbox 2] [lindex $bbox 4]]
    return $origin
}


# ---------------
# lset_configureV
# ---------------

proc lset_configureV {v core} {
    $core SetVelocity -vobj $v
    $core SetTime -cflFactor 1.0  ;# may be re-set by v-specific proc's
    set c [$v GetClassName]
    switch $c {
	default        {}
    }
}


# ----------
# lset_showV
# ----------

proc lset_showV {v core} {

    set nameLen [expr [string length $v] + 4]
    puts ""
    puts ""
    for {set i 0} {$i < $nameLen} {incr i} { puts -nonewline "-" }
    puts ""
    puts "  $v"
    for {set i 0} {$i < $nameLen} {incr i} { puts -nonewline "-" }
    puts ""

    set cls [$v GetClassName]
    puts [format "%-12s %s" "class:" $cls]

    set stopV [$v GetStopV]
    puts [format "%-12s %f" "stopV:" $stopV]

    switch $cls {
	lsetVPotential -
	lsetVSmooth    -
	lsetVExpDecay  {lset_showVConst $v}
	default        {}
    }

    array set ti [$core GetTime]
    set cfl $ti(-cflFactor)
    puts [format "%-12s %f" "cflFactor:" $cfl]
    puts ""
}


# ------------------
# lset_cfgVPotential
# ------------------
# See notes, week of Feb. 7, 2000.

proc lset_cfgVPotential {core v klow kupp stopV pFactor 3d_curv} {

    if {[catch {$v GetMagGradRange} MGrng]} {
	return -code error "must set potential before further configuration"
    }
    if {[catch {lset_getMinH $core} minh]} {
	return -code error $minh
    }

    set ek 1

    $v SetStopV -value $stopV

    # Again, there should be some sort of image signal metric we can
    # apply to choose factor, which essentially relates to the shape
    # of |grad(P)| around the edge.
#    set factor 0.1

    set factor $pFactor
    set maxMGp [lindex $MGrng 1]

    if {$pFactor >= 0} {
      set thr [expr $factor * $maxMGp]
    } else {
      # set potential explicitly
      set thr [expr abs($factor)]
      set factor [expr 1.0*$thr/$maxMGp]
    }

    set ep [expr $stopV / $thr]

    if {[catch {$v SetConst -eP $ep -eK $ek -Klow $klow -Kupp $kupp} msg]} {
	return -code error $msg
    }

    set cfl_a [expr 2 * $factor]

    if {$klow == $kupp} {
	set cfl $cfl_a
    } else {
	if {$kupp == 0} {
	    set dist [expr 0.0 - (1.0 / $klow)]
	} else {
	    set dist [expr (1.0 / $kupp) - 0.0]
	}
	set cfl_b [expr $dist / $minh]
	set cfl [expr ($cfl_a < $cfl_b) ? $cfl_a : $cfl_b]
    }

    set cfl [expr ($cfl < 1.0) ? $cfl : 1.0]

    $core SetTime -cflFactor $cfl
    $core SetVelocity -vobj $v
}


# -----------------
# lset_cfgVExpDecay
# -----------------

proc lset_cfgVExpDecay {core v kt expand stopV mgFactor 3d_curv} {

    if {[catch {$v GetMagGradRange} MGrng]} {
	return -code error "must set image before further configuration"
    }
    if {$stopV <= 0.0} {
	return -code error "stop velocity must be > 0"
    }
    if {$kt <= 0.0} {
	return -code error "tolerated curvature must be > 0"
    }

    set maxMGi [lindex $MGrng 1]

    # There should be some sort of image signal metric we can apply
    # with which to arrive at the factor to use in computing MGi_c.
    # The idea is that edge regions can be delineated once we've
    # surpassed some level of gradient magnitude.
#    set MGi_c [expr 0.2 * $maxMGi]
#    set MGi_c [expr 0.33 * $maxMGi]

    if {$mgFactor >= 0} {
      set MGi_c [expr $mgFactor * $maxMGi]
    } else {
      # set potential explicitly
      set MGi_c [expr abs($mgFactor)]
    }
#    puts [format "stopV: %f" [set stopV [expr $kt / 100.0]]]

    set ei [expr log( $kt / $stopV ) / $MGi_c]

    # You should only set eIneg to something different than eI if you
    # know what you are doing!
#    set eineg [expr $ei * 2.0]
    set eineg $ei

    $v SetStopV -value $stopV

    # Mean, Gaussian, Default
    if {[catch {$v SetConst -eI $ei -eIneg $eineg -Kt $kt \
	    -expand $expand -clamp true -3d_curv $3d_curv} msg]} {
	widget_ErrorDialog $msg
	return -code error
    }
    $core SetTime -cflFactor 1.0
    $core SetVelocity -vobj $v
}


# ---------------
# lset_cfgVSmooth
# ---------------

proc lset_cfgVSmooth {core v klow kupp 3d_curv} {

    if {[catch {lset_getMinH $core} minh]} {
	return -code error $minh
    }

    if {$klow == $kupp} {
	set cfl 0.1
    } else {
	if {$kupp == 0} {
	    set dist [expr 0.0 - (1.0 / $klow)]
	} else {
	    set dist [expr (1.0 / $kupp) - 0.0]
	}
	set cfl [expr $dist / $minh]
    }

    # Clamp cfl factor at 0.1:
    set cfl [expr ($cfl < 0.1) ? $cfl : 0.1]

    $v SetStopV -value 0.001
    $v SetConst -lower_Kt $klow -upper_Kt $kupp -3d_curv $3d_curv

    $core SetTime -cflFactor $cfl
    $core SetVelocity -vobj $v
}


# --------------
# lset_cfgVConst
# --------------

proc lset_cfgVConst {core v} {
    $v SetConst -v 1.0
    $core SetTime -cflFactor 1.0
    $core SetVelocity -vobj $v
}


# ---------------
# lset_showVConst
# ---------------

proc lset_showVConst {v} {
    array set consts [$v GetConst]
    set names [array names consts]
    foreach n $names {
	set c $consts($n)
	puts [format "%-12s %s" "[string range $n 1 end]:" $c]
    }
}


#
# -------------
# lset_initCore
# -------------
# Use initData to set the lsetCore seed appropriately.  Some of these
# seed strategies will make use of an (x,y,z) coordinate if included
# in initData, but not all strategies use such a coordinate.  Straight
# image intensity seeding (via SetImageSeed) just uses intensity
# values and how they compare to a given threshold to determine level
# set sign values.  And in PolyData- and SolidModel-based seeding, all
# the seed geometry is already embedded in the seed object.

proc lset_initCore {core initData img} {
    set initType [lindex $initData 0]
    if {[llength $initData] > 2} {
	set x [lindex $initData 2]
	set y [lindex $initData 3]
	set z [lindex $initData 4]
    } else {
	set x 0.0
	set y 0.0
	set z 0.0
    }

    puts "\n====="
    puts "lsetCore::Init"

    switch -exact $initType {

	circle {
	    set r [lindex $initData 1]
	    puts "circle: $r"
	    if {[catch {$core SetCircleSeed -r $r -x $x -y $y -z $z} msg]} {
		return -code error $msg
	    }
	}

	image {

	    # Use SetImageSeed
	    # ----------------
	    set t [lindex $initData 1]
	    puts "image: $t"
	    if {[catch {$core SetImageSeed -img $img -thr $t} msg]} {
		return -code error $msg
	    }
	}

	contour {

	    # Use a picked closed contour
	    # ---------------------------
	    set c [lindex $initData 1]
	    puts "contour: $c"
	    set vtk_img [repos_exportToVtk -src $img]
	    set vtk_pd [img_contour $vtk_img $c]
	    set pd /initCore/tmp/pd
	    set seed /initCore/tmp/seed

	    catch {repos_delete -obj $pd}
	    catch {repos_delete -obj $pd/merge}
	    catch {repos_delete -obj $pd/tmp}
	    catch {repos_delete -obj $seed}

	    repos_importVtkPd -src $vtk_pd -dst $pd
	    geom_mergePts -src $pd -dst $pd/merge

	    set pd_bbox [geom_bbox -obj $pd/merge]
	    if {[lindex $pd_bbox 4] != [lindex $pd_bbox 5]} {
		widget_ErrorDialog "Non-planar contour geometry."
		return -code error
	    }
	    set z [lindex $pd_bbox 4]
	    if {$z != 0.0} {
		geom_copy -src $pd/merge -dst $pd/tmp
		repos_delete -obj $pd/merge
		geom_translate -src $pd/tmp -vec [list 0 0 -$z] -dst $pd/merge
	    }
	    set pos [list $x $y $z]
	    if {[catch {geom_pick -obj $pd/merge -pos $pos -result $seed}]} {
		widget_ErrorDialog "Couldn't find a seed contour."
		return -code error
	    }
	    puts [geom_bbox -obj $seed]
	    if {[catch {$core SetPdSeed -polydata $seed} msg]} {
		return -code error $msg
	    }
	}

	polydata {

	    # Use SetPdSeed
	    # -------------
	    set obj [lindex $initData 1]
	    puts "PolyData seed: $obj"
	    if {[catch {$core SetPdSeed -polydata $obj} msg]} {
		return -code error $msg
	    }
	}

	solidmodel {

	    # Use SetSolidSeed
	    # ----------------
	    set obj [lindex $initData 1]
	    puts "SolidModel seed: $obj"
	    if {[catch {$core SetSolidSeed -solid $obj} msg]} {
		return -code error $msg
	    }
	}

	default {
	    return -code error "$initType not a recognized init type"
	}
    }
    puts "====="
}


# -------------
# lset_runClear
# -------------

proc lset_runClear {} {

    set core __lset_slice_core
    set ve   __lset_slice_vExpDecay
    set vp   __lset_slice_vPotential
    set vs   __lset_slice_vSmooth
    set vc   __lset_slice_vConst

    catch {rename $core {}}
    catch {rename $ve {}}
    catch {rename $vp {}}
    catch {rename $vs {}}
    catch {rename $vc {}}
}


# --------
# lset_run
# --------
# Intended to be DIMENSION-INDEPENDENT.
# Currently called by the following 3 proc's:
#   1. lset_slice
#   2. lset_segPlane
#   3. mc_Segment3D

# Further notes on access paths to lset_run from GUI invocation:
#   a) Segment-->Slice    ==> lset_onePath ==> lset_slice ==> lset_run
#   b) Segment-->Path     ==> lset_onePath ==> lset_slice ==> lset_run
#   c) Segment-->z plane  ==> lset_segPlane ==> lset_run
#   d) Segment-->3D       ==> mc_Segment3D ==> lset_run

proc lset_run {img pot initData kt klow kupp \
	serialInit gridFactor initOnly \
	sparseFlag isotropicFlag expandFlag \
	convFlag timerFlag \
	maxTSFlag maxTS \
	writeIntermediate \
	runVpot runVSmooth runVConst numVConst \
	magGradFactor pFactor \
	rens objPrefix verbose stopV rebuildPhiFreq ac acInterval 3d_curv segPd finalGrid} {

    #@author Ken Wang
    #@c This is macro to run a levelset simulation to segment a 2-D
    #@c contour from a slice of image data.
    #@a img:		image data (vtkImageData) 
    #@a pot:		potential well data	      
    #@a initData:	intialization procedures
    #@a kt:		curvature value used for vExpDecay function
    #@a klow:		used by vPotential & vSmooth
    #@a kupp:		used by vPotential & vSmooth
    #@a serialInit:	(0/1) indicates whether we are running a set of
    #@a serialInit:                segmentations in which we want to use the level
    #@a serialInit:                set function as it was left by the previous item
    #@a gridFactor:      grid spacing (h) based on min voxel size * gridFactor
    #@a initOnly:       boolean which determines if you actually run
    #@a initOnly:	simulation or just set up for it 
    #@a sparseFlag:	whether to use the sparse or dense grid
    #@a isotropicFlag:   boolean to use a uniformly spaced grid in all dimensions
    #@a expandFlag:	boolean used to determine if front in 
    #@a convFlag:	boolean determines if area/volume convergence is checked
    #@a timerFlag:       $core SetTimers -flag $timerFlag
    #@a maxTSFlag:       boolean determining whether to exist after a fixed
    #@a maxTSFlag:		number of steps
    #@a maxTS:		maximum number of time steps to run
    #@a writeIntermediate: ** probably output intermediate objects flag **
    #@a runVSmooth:	  boolean to run vSmooth
    #@a runVConst:	  boolean to run VConst		 
    #@a numVConst:	  number of steps to run VConst
    #@a magGradFactor:	  magnitude of intensity gradient cut-off for vExpDecay
    #@a pFactor:	  potential well tolerance for vPotential
    #@a rens:		  render windows (set to {} for no graphics output)
    #@a objPrefix:	all objects to be saved created in repository with
    #@a objPrefix:		this prefix
    #@a verbose:		boolean to output solution results along the way
    #@a stopV:  stop velocity
    #@a rebuildPhiFreq:  frequency to rebuild Phi

    global gLsetGridType

    set core __lset_slice_core
    set ve   __lset_slice_vExpDecay
    set vp   __lset_slice_vPotential
    set vs   __lset_slice_vSmooth
    set vc   __lset_slice_vConst

    # Initialize
    # ----------
    # The variable serialInit indicates whether we are running a set
    # of segmentations in which we want to use the level set function
    # as it was left by the previous item as the initial condition for
    # the current item.  If this is NOT the case, then we want to be
    # sure to clear all items and start with a new lsetCore object.
    # If this IS the case, then we want to avoid clearing the
    # currently existing lsetCore, and simply re-use it.

    if { ! $serialInit } {
	catch {rename $core {}}
    }
    catch {rename $ve {}}
    catch {rename $vp {}}
    catch {rename $vs {}}
    catch {rename $vc {}}

    catch {repos_delete -obj $objPrefix/lset/0}
    catch {repos_delete -obj $objPrefix/grid/0}

    repos_deleteList [repos_subList /lset/*]
    repos_deleteList [repos_subList /velocity/*]
    repos_deleteList [repos_subList /grid/*]

    set vtk_p [repos_exportToVtk -src $pot]
    set h [$vtk_p GetSpacing]
    set dims [$vtk_p GetDimensions]
    set orig [$vtk_p GetOrigin]

    set domExtx [expr [lindex $h 0] * [lindex $dims 0]]
    set domExty [expr [lindex $h 1] * [lindex $dims 1]]
    set domExtz [expr [lindex $h 2] * [lindex $dims 2]]
    set domExt [list $domExtx $domExty $domExtz]

    set hx [expr $gridFactor * [lindex $h 0]]
    set hy [expr $gridFactor * [lindex $h 1]]
    set hz [expr $gridFactor * [lindex $h 2]]
    set minh [expr ($hx < $hy) ? $hx : $hy]
    set maxh [expr ($hx > $hy) ? $hx : $hy]

    # NOTE: The following determination of grid dimensionality assumes
    # that 2D level set calculations are always done in the xy plane,
    # not any other plane (i.e. xz, yz).

    if {[lindex $dims 2] > 1} {
	set h [list $hx $hy $hz]
	set minh [expr ($minh < $hz) ? $minh : $hz]
	set maxh [expr ($maxh > $hz) ? $maxh : $hz]
	if {$expandFlag} {
	    set band [list [expr -3 * $maxh] [expr 3 * $maxh]]
	    set band [list [expr -3 * $minh] [expr 10 * $minh]]
	} else {
	    set band [list [expr -10 * $minh] [expr 3 * $minh]]
	}
	set gridDim 3

    } else {
	set h [list $hx $hy $hx] ;# this is the 2D case
	set band [list [expr -10 * $minh] [expr 10 * $minh]]
	set orig [lreplace $orig 2 2 0.0]
	set gridDim 2
    }

    set dimFactor [expr 1.0 / $gridFactor]
    set dimx [expr int( ceil($dimFactor * [lindex $dims 0]) )]
    set dimy [expr int( ceil($dimFactor * [lindex $dims 1]) )]
    if {$gridDim == 3} {
	set dimz [expr int( ceil($dimFactor * [lindex $dims 2]) )]
    } else {
	set dimz 1
    }
    set dims [list $dimx $dimy $dimz]

    if {$isotropicFlag} {
	set h [list $minh $minh $minh]

	# Round to some number of decimal places (if we don't do this,
	# precision-related errors can crop up):
	set prec 100000.0
	set ext_x [expr round( $prec * [lindex $dims 0] * $hx ) / $prec]
	set ext_y [expr round( $prec * [lindex $dims 1] * $hy ) / $prec]
	set ext_z [expr round( $prec * [lindex $dims 2] * $hz ) / $prec]

	set dim_x [expr int( ceil( $ext_x / $minh ) )]
	set dim_y [expr int( ceil( $ext_y / $minh ) )]
	set dim_z [expr int( ceil( $ext_z / $minh ) )]
	set dims [list $dim_x $dim_y $dim_z]

	# The following was done in order to set the origin an equal
	# spacing from the domain bounding box in all 3 coordinate
	# directions.  However, we definitely don't want to do this
	# when that bounding box does not itself have a corner at
	# (0,0,0).
#	set orig_i [expr $minh / 2.0]
#	set orig [list $orig_i $orig_i $orig_i]

	if {$gridDim == 2} {
	    set dims [lreplace $dims 2 2 1]
	    set orig [lreplace $orig 2 2 0.0]
	}
    }


    # Area/volume convergence criterion: If successive checks indicate
    # that shape is not changing within this factor (as computed by
    # the goodness metric), then exit the current phase of front
    # evolution.  Note that these are only applied when convFlag is
    # true.

    #set ac 0.99

    # The area/vol convergence test interval should probably be a
    # function of grid size, since at smaller grid sizes,
    # displacements per time step (and per interval) will be smaller
    # in physical units.

    #set acInterval [expr 50 / $gridFactor]
    #set acInterval 1

    # This is some stuff which is relevant for properly handling a
    # maximum time step.

    if {$maxTSFlag} {
	set maxStep $maxTS
    } else {
	set maxStep -1
    }
    set maxTSreached 0

    puts "\n====="
    puts "Additional configuration notes:"
    puts "  mag gradient factor:  $magGradFactor"
    puts "  potential factor:     $pFactor"
    puts "  area/vol criterion:   $ac"
    puts "  area/vol interval:    $acInterval"
    puts "  max time step:        $maxStep"
    puts "====="


    # Inputs:
    # -------
    #   - seed radius  (init)
    #   - kt           (vExpDecay)
    #   - klow, kupp   (vPotential, vSmooth)

    if {![cmdExists $core]} {

	lsetCore $core
	$core SetTimers -flag $timerFlag

	# Grid parameters
	# ---------------
	#   - h:    dynamically retrieved from image potential
	#   - dims: dynamically retrieved from image potential
	#   - orig: dynamically retrieved from image potential
	#   - minh: dynamically retrieved from image potential

	puts "\n====="
	puts "lsetCore::SetGrid"
	if {$sparseFlag} {
	    puts "$core SetGrid -h $h -dim $dims -origin $orig \
		    -type SparseGrid \
		    -bandExt $band"
	    puts "====="
	    $core SetGrid -h $h -dim $dims -origin $orig \
		    -type SparseGrid \
		    -bandExt $band
	} else {
	    puts "$core SetGrid -h $h -dim $dims -origin $orig \
		    -type DenseGrid"
	    puts "====="
	    $core SetGrid -h $h -dim $dims -origin $orig \
		    -type DenseGrid
	}

	if {[catch {lset_initCore $core $initData $img} msg]} {
	    widget_ErrorDialog "Error on initialization."
	    return -code error $msg
	}

    } else {

	if {$serialInit} {
	    $core ResetTimeStep
	}
    }

    lsetVExpDecay $ve
    $ve SetImageObj -src $img
    if {[catch {lset_cfgVExpDecay $core $ve $kt $expandFlag \
	    $stopV $magGradFactor $3d_curv} msg]} {
	widget_ErrorDialog "Error on vExpDecay config."
	return -code error $msg
    }

    if {![$core IsInit]} {
	puts "\n====="
#	puts "res sz (KB):     [set pre [process_ResSizeKB]]"
	puts "lsetCore::Init"

	$core Init

	puts "grid stats:      [$core GetGridStats]"
#	puts "res sz (KB):     [set post [process_ResSizeKB]]"
#	puts "mem growth (KB): [expr $post - $pre]"
	puts "lsetCore sz(KB): [expr [$core GetMemoryUsage] / 1024.0]"
	puts "====="
    }
    $core ExtractFront -out $objPrefix/lset/0 -closed 1

    set gLsetGridType [lset_gridType $core]
    if {$gLsetGridType == "SparseGrid"} {
	$core SaveGrid -result $objPrefix/grid/0
    }

    lset_showV $ve $core

    # The objects $prevState and $currState will be used to track area
    # convergence.  Specifically, at any particular area convergence
    # checkpoint (as determined by $acInterval), we want to determine
    # how much the zero level set has changed since the last
    # checkpoint.  $prevState will hold the front as it existed at the
    # last checkpoint (or at t=0 for the first check).  $currState
    # will be the current front.  Note that between velocity phases,
    # both objects are cleared and $prevState is re-extracted.  This
    # incurs an extra front extraction, which is a non-negligible
    # operation in terms of computation, but doing so helps to
    # eliminate erroneous situations in which no valid $prevState
    # exists due to a SIGINT for example.

    set prevState /lset_slice/convergence/test/prev
    set currState /lset_slice/convergence/test/curr
    set prevStatePhi /lset_slice/convergence/test/prev/phi
    set currStatePhi /lset_slice/convergence/test/curr/phi

    catch {repos_delete -obj $prevState}
    catch {repos_delete -obj $currState}
    catch {repos_delete -obj $prevStatePhi}
    catch {repos_delete -obj $currStatePhi}

    geom_copy -src $objPrefix/lset/0 -dst /lset/0
    geom_copy -src $objPrefix/lset/0 -dst $prevState

    $core ExtractPhi -out $prevStatePhi

    # As you can tell, the semantics of these update loops (one per
    # velocity phases) are getting increasingly complicated with the
    # introduction of various controls (e.g. initOnly, convFlag,
    # acInterval, runVSmooth, etc.).

    # Note that when convFlag is false (i.e. the caller has asked that
    # the area/volume spatial convergence test not be applied), the
    # loop should function correctly.  While the value of $a will
    # never be anything other than 0.0, the status returned by
    # lset_for will continue to represent the internal stopping
    # criterion status of the lsetCore object.

    if {!$initOnly} {
	set a 0.0
	set vanishedMsg "Level set exited grid domain."
	while {$a < $ac} {
	    set status [lset_for $core $acInterval $maxStep \
		    $rens $verbose \
		    $writeIntermediate $rebuildPhiFreq]
	    if {[$core Vanished]} {
		puts $vanishedMsg
		return -code error $vanishedMsg
	    }

	    # The extracted geometry may or may not be manifold.  A loop
	    # in the geometry will cause failure in AreaMetric_poly's
	    # calls to solid_poly.  Normally, we invoke a geom_pick
	    # operation to select a closed region of interest.  However,
	    # if the geometry has a loop, geom_pick will also fail, so we
	    # can not in general expect that to work, especially at a
	    # point which is potentially in the middle of the time step
	    # loop when the geometry is still evolving.  As a result, here
	    # we are simply taking the attitude that the area convergence
	    # criterion will not be satisfied in such a condition.

	    if {($status) && ($convFlag)} {
		catch {repos_delete -obj $currState}
                catch {repos_delete -obj $currStatePhi}
                $core ExtractFront -out $currState -closed 1
                $core ExtractPhi -out $currStatePhi
		if {[catch {AreaMetric_3 $gridDim $prevState $currState $prevStatePhi $currStatePhi} a]} {
                    puts "msg: $a"
		    set a 0.0
		    if {$status} {
			continue
		    }
		}
		catch {repos_delete -obj $prevState}
		geom_copy -src $currState -dst $prevState
                # we have to play a trick here since I can't figure out a way
                # to import a vtkUnstructuredGrid into the repos
                set tmp $prevStatePhi
                set prevStatePhi $currStatePhi
                set currStatePhi $tmp
                catch {repos_delete -obj $currStatePhi}
	    }

	    if {!$status} {
		break
	    }
	}
	if {($convFlag) && ($a >= $ac)} {
	    puts "Area convergence: $a"
	}
	set ts [$core GetTimeStep]
	if {($maxStep >= 0) && ($ts >= $maxStep)} {
	    set maxTSreached 1
	}
    }


    lsetVPotential $vp
    $vp SetImageObj -src $pot
    lset_cfgVPotential $core $vp $klow $kupp $stopV $pFactor $3d_curv
    $core SetTime -cflFactor 0.5
    lset_showV $vp $core

    if {(!$initOnly) && (!$maxTSreached) && $runVpot} {

	if {$convFlag} {
	    catch {repos_delete -obj $prevState}
	    catch {repos_delete -obj $currState}
            catch {repos_delete -obj $prevStatePhi}
            catch {repos_delete -obj $currStatePhi}
            $core ExtractPhi -out $prevStatePhi
	    $core ExtractFront -out $prevState -closed 1
	}

	set a 0.0
	while {$a < $ac} {

	    set status [lset_for $core $acInterval $maxStep \
		    $rens $verbose \
		    $writeIntermediate $rebuildPhiFreq]
	    if {[$core Vanished]} {
		puts $vanishedMsg
		return -code error $vanishedMsg
	    }

	    if {($status) && ($convFlag)} {
		catch {repos_delete -obj $currState}
                catch {repos_delete -obj $currStatePhi}
                $core ExtractFront -out $currState -closed 1
                $core ExtractPhi -out $currStatePhi
		if {[catch {AreaMetric_3 $gridDim $prevState $currState $prevStatePhi $currStatePhi} a]} {
                    puts "msg: $a"
		    set a 0.0
		    if {$status} {
			continue
		    }
		}
		catch {repos_delete -obj $prevState}
		geom_copy -src $currState -dst $prevState
                # we have to play a trick here since I can't figure out a way
                # to import a vtkUnstructuredGrid into the repos
                set tmp $prevStatePhi
                set prevStatePhi $currStatePhi
                set currStatePhi $tmp
                catch {repos_delete -obj $currStatePhi}
	    }

	    if {!$status} {
		break
	    }

	    # Reduce the cfl factor after each block of $acInterval
	    # steps.  This causes smaller and smaller time steps as
	    # vPotential continues.

#	    array set items [$core GetTime]
#	    set cfl [expr $items(-cflFactor) / 2.0]
#	    set cfl [expr $cfl < 0.05 ? 0.05 : $cfl]
#	    $core SetTime -cflFactor $cfl
	}
	if {($convFlag) && ($a >= $ac)} {
	    puts "Area convergence: $a"
	}
	set ts [$core GetTimeStep]
	if {($maxStep >= 0) && ($ts >= $maxStep)} {
	    set maxTSreached 1
	}
    }


    if {$runVSmooth} {
	lsetVSmooth $vs
	lset_cfgVSmooth $core $vs $klow $kupp $3d_curv
	lset_showV $vs $core

	if {(!$initOnly) && (!$maxTSreached)} {

	    if {$convFlag} {
		catch {repos_delete -obj $prevState}
		catch {repos_delete -obj $currState}
                catch {repos_delete -obj $prevStatePhi}
                catch {repos_delete -obj $currStatePhi}
                $core ExtractPhi -out $prevStatePhi
		$core ExtractFront -out $prevState -closed 1
	    }

	    set a 0.0
	    while {$a < $ac} {
		set status [lset_for $core $acInterval $maxStep \
			$rens $verbose \
			$writeIntermediate $rebuildPhiFreq]
		if {[$core Vanished]} {
		    puts $vanishedMsg
		    return -code error $vanishedMsg
		}

		if {($status) && ($convFlag)} {
		    catch {repos_delete -obj $currState}
                    catch {repos_delete -obj $currStatePhi}
                    $core ExtractFront -out $currState -closed 1
                    $core ExtractPhi -out $currStatePhi
		    if {[catch {AreaMetric_3 $gridDim $prevState $currState $prevStatePhi $currStatePhi} a]} {
                      puts "msg: $a"
		      set a 0.0
		      if {$status} {
			continue
		      }
		    }
		    catch {repos_delete -obj $prevState}
		    geom_copy -src $currState -dst $prevState
                    # we have to play a trick here since I can't figure out a way
                    # to import a vtkUnstructuredGrid into the repos
                    set tmp $prevStatePhi
                    set prevStatePhi $currStatePhi
                    set currStatePhi $tmp
                    catch {repos_delete -obj $currStatePhi}
		}

		if {!$status} {
		    break
		}
	    }
	    if {($convFlag) && ($a >= $ac)} {
		puts "Area convergence: $a"
	    }
	    set ts [$core GetTimeStep]
	    if {($maxStep >= 0) && ($ts >= $maxStep)} {
		set maxTSreached 1
	    }
	}
    }

    if {$runVConst} {
	lsetVConst $vc
	lset_cfgVConst $core $vc
	lset_showV $vc $core

	if {(!$initOnly) && (!$maxTSreached)} {
	    lset_for $core $numVConst $maxStep \
		    $rens $verbose $writeIntermediate $rebuildPhiFreq
	    if {[$core Vanished]} {
		puts $vanishedMsg
		return -code error $vanishedMsg
	    }
	}
    }

    # Finish
    # ------

    if {$initOnly} {
	return ""
    }

    catch {repos_delete -obj $segPd}
    if {[catch {$core ExtractFront -out $segPd -closed 1} msg]} {
	#widget_ErrorDialog "$msg"
	return -code error "ERR: $msg"
    }
    catch {repos_delete -obj $finalGrid}
    if {[catch {$core ExtractPhi -out $finalGrid} msg]} {
	return -code error "ERR: $msg"
    }

    return $segPd
}


# ---------------
# AreaMetric_3
# ---------------

proc AreaMetric_3 {gridDim truePgn approxPgn truePhi approxPhi} {
    #@author Nathan Wilson
    #@c Calculates the goodness metric from two 3-D objects.
    #@a truePgn:  True 3D PolyData object.
    #@a approxPgn:  Approximate 3D PolyData object.
    #@a truePhi: level set scalar function StructuredPts object.
    #@a approxPhi: level set scalar function StructuredPts object.
    #@r goodness metric between 0 and 1.
    #@note  The equation for the goodness metric is: &p
    #@note  &p
    #@note  goodness = sqrt ( (Aintersect / Atrue) * (Aintersect / Aapprox) )
    #@danger This routine currently only works for SparseGrid.

    catch {repos_delete -obj $truePgn/merge}
    catch {repos_delete -obj $truePgn/merge/pd}
    catch {repos_delete -obj $approxPgn/merge}
    catch {repos_delete -obj $approxPgn/merge/pd}

    set interPD /tmp/area_metric/inter/pd
    set interMergedPD /tmp/area_metric/inter/merge
    set interLargestPD /tmp/area_metric/inter/largest
    catch {repos_delete -obj $interPD}
    catch {repos_delete -obj $interMergedPD}
    catch {repos_delete -obj $interLargestPD}

    # clean up point list
    geom_mergePts -src $truePgn -dst $truePgn/merge

    # select only the largest region
    geom_largestConnected $truePgn/merge $truePgn/merge/pd

    set vcalculator tmp-area_metric-masser
 
    catch {$vcalculator Delete}
    vtkMassProperties $vcalculator

    # get volume
    $vcalculator SetInputDataObject [repos_exportToVtk -src $truePgn/merge/pd]
    $vcalculator Update
    set aTrue [$vcalculator GetVolume]

    # clean up point list
    geom_mergePts -src $approxPgn -dst $approxPgn/merge

    # select only the largest region
    geom_largestConnected $approxPgn/merge $approxPgn/merge/pd

    # get volume
    $vcalculator SetInputDataObject [repos_exportToVtk -src $approxPgn/merge/pd]
    $vcalculator Update
    set aApprox [$vcalculator GetVolume]

    puts [format "A(%s) = \t %10.4f" $truePgn $aTrue]
    puts [format "A(%s) = \t %10.4f" $approxPgn $aApprox]
 
    set vTruePhi [repos_exportToVtk -src $truePhi]
    set vApproxPhi [repos_exportToVtk -src $approxPhi]
    if {[$vTruePhi GetNumberOfPoints] == [$vApproxPhi GetNumberOfPoints]} {
       
      [[$vTruePhi   GetPointData] GetScalars] SetName s1
      [[$vApproxPhi GetPointData] GetScalars] SetName s2
      [$vTruePhi   GetPointData] AddArray [[$vApproxPhi GetPointData] GetScalars]

      set calc tmp-area-metric-vtu-math
      catch {$calc Delete}

      vtkArrayCalculator $calc
      $calc SetInputDataObject $vTruePhi
      $calc SetAttributeModeToUsePointData
      $calc AddScalarArrayName s1 0
      $calc AddScalarArrayName s2 0
      $calc SetFunction "max(s1,s2)"
      $calc SetResultArrayName resArray
      $calc ReplaceInvalidValuesOn
      $calc SetReplacementValue 0
      $calc Update

      set aa tmp-area-metric-aa
      catch {$aa Delete}
      vtkAssignAttribute $aa
      $aa SetInputConnection [$calc GetOutputPort]
      $aa Assign resArray SCALARS POINT_DATA
      $aa Update

      repos_importVtkPd -src [img_contour [$aa GetOutput] 0] -dst $interPD

      # clean up point list
      geom_mergePts -src $interPD -dst $interMergedPD

      # select only the largest region
      geom_largestConnected $interMergedPD $interLargestPD

      # get volume
      $vcalculator SetInputDataObject [repos_exportToVtk -src $interLargestPD]
      $vcalculator Update
      set ainter [$vcalculator GetVolume]

      puts [format "A(overlap) = \t %10.4f" $ainter]

      set fi [expr $ainter / $aTrue]
      set fj [expr $ainter / $aApprox]
      set result [expr sqrt($fi * $fj)]
    
      $aa Delete
      $calc Delete
      repos_delete -obj $interPD
      repos_delete -obj $interMergedPD
      repos_delete -obj $interLargestPD
      $vcalculator Delete

    } else {

	puts "grids different size -- skip check ([$vTruePhi GetNumberOfPoints],[$vApproxPhi GetNumberOfPoints])"
     set result 0

    }

    puts "g(a,b) = $result   (used level set boolean)"

    return $result

}



proc lset_combine {allGrids outPd} {

  # get the final output dimensions using the first grid
  set vd [repos_exportToVtk -src [lindex $allGrids 0]]
  set origin [$vd GetOrigin]
  set extent [$vd GetExtent]
  set spacing [$vd GetSpacing]

  set tmp /tmp/combine_lsets/grid
  catch {repos_delete -obj $tmp}
  set rs tmp-combine-reslice

  repos_importVtkImg -src [repos_exportToVtk -src [lindex $allGrids 0]] -dst $tmp

  foreach vol [lrange $allGrids 1 end] {
    puts "using vol: $vol"
    catch {$rs Delete}
    vtkImageReslice $rs
    $rs SetInputDataObject [repos_exportToVtk -src $vol]
    $rs SetInterpolationModeToLinear
    eval $rs SetOutputOrigin $origin
    eval $rs SetOutputExtent $extent
    eval $rs SetOutputSpacing $spacing
    $rs Update

    set imath tmp-combine_lsets-math
      catch {$imath Delete}

    vtkImageMathematics $imath
    $imath SetInput1Data [repos_exportToVtk -src $tmp]
    $imath SetInput2Data [$rs GetOutput]
    $imath SetOperationToMin
    $imath Update
#    set tmp2 forgetit
#    catch {repos_delete -obj $tmp2}
#    repos_importVtkImg -src [$rs GetOutput] -dst $tmp2
#    $rs Delete
#    catch {repos_delete -obj $tmp}
#    repos_importVtkImg -src [repos_exportToVtk -src $tmp2] -dst $tmp
    catch {repos_delete -obj $tmp}
    repos_importVtkImg -src [$rs GetOutput] -dst $tmp
    $rs Delete
  }

  set contour [img_contour [$imath GetOutput] 0]
  repos_importVtkPd -src $contour -dst $outPd
  $imath Delete
  #catch {repos_delete -obj $tmp}
}

 

