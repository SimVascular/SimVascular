# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                           University of California, San Diego.
#
#
# Portions of the code Copyright (c) 1998-2007 Stanford University,
#   Charles Taylor, Nathan Wilson, Ken Wang.
#
# See SimVascular Acknowledgements file for additional
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

proc nateAFLB {} {

    global gPathBrowser
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
    solid_setKernel -name Parasolid
    if {[catch {solid_makeLoftedSurf -srcs $curveList -dst $c0/surf}]} {
	return -code error "Error lofting surface."
    }

    #
    # bound solid
    #

    global gRen3d
    set ren $gRen3d

    set surf $c0/surf
    catch {repos_delete -obj $surf/capped}
    catch {repos_delete -obj $surf/capped/pd}
    solid_setKernel -name Parasolid
    if {[catch {solid_capSurfToSolid -src $surf -dst $surf/capped} msg]} {
	return -code error $msg
    }
    puts "--> $surf/capped"
    puts "\n\n"

    global gOptions
    if {$gOptions(facet_max_edge_size) != ""} {
      $surf/capped GetPolyData -result $surf/capped/pd \
                  -max_edge_size $gOptions(facet_max_edge_size)
    } else {
      $surf/capped GetPolyData -result $surf/capped/pd
    }

    set a [vis_pRepos $ren $surf/capped/pd]
    vis_pNorm $ren $surf/capped/pd
    [$a GetProperty] SetOpacity $gPathBrowser(solid_opacity)

    # ugly way to keep track of solids created for each group
    global gLoftedSolids
    set gLoftedSolids($grp) $surf/capped

    # clean up the tags with the group names
    set solid $surf/capped
    foreach i [$solid GetFaceIds] {
      set facename {}
      catch {set facename [$solid GetFaceAttr -attr gdscName -faceId $i]}
      if {$facename != "" && $facename != "inflow" && $facename != "inlet"} {
        $solid SetFaceAttr -attr gdscName -faceId $i -value $grp
      } else {
        # we have a wall
        $solid SetFaceAttr -attr gdscName -faceId $i -value wall_$grp
      }
    }

}

#
#  A dot B
#
proc gdscAdotB {a b result} {
  upvar $result adotb
  if {[llength $a] != 3} {
    puts "Error:  Invalid length of vector $a!"
    return -code error GDSC_ERR
  }
  if {[llength $b] != 3} {
    puts "Error:  Invalid length of vector $b!"
    return -code error GDSC_ERR
  }
  set adotb [expr double([lindex $a 0]) * double ([lindex $b 0]) + \
            double([lindex $a 1]) * double ([lindex $b 1]) + \
            double([lindex $a 2]) * double ([lindex $b 2])]
  return GDSC_OK
}

#
#  calculate the magnitude of a vector
#
proc gdscVectorMagnitude {v} {
  if {[llength $v] != 3} {
     puts "Error:  Vector not of correct length!"
     return -code error GDSC_ERR
  }
  set v1 [lindex $v 0]
  set v2 [lindex $v 1]
  set v3 [lindex $v 2]
  return [expr sqrt(double($v1*$v1) + double($v2*$v2) +double($v3*$v3))]
}

#
#  normalize a vector
#
proc gdscVectorNormalize {v} {
  if {[llength $v] != 3} {
     puts "Error:  Vector not of correct length!"
     return -code error GDSC_ERR
  }
  set v1 [lindex $v 0]
  set v2 [lindex $v 1]
  set v3 [lindex $v 2]
  set mag [gdscVectorMagnitude $v]
  if {$mag < 0.00001} {
     puts "Warning:  Trying to normalize a zero vector!"
     return "0 0 0"
     #return -code error GDSC_ERR
  }
  set v1 [expr double($v1)/double($mag)]
  set v2 [expr double($v2)/double($mag)]
  set v3 [expr double($v3)/double($mag)]
  return "$v1 $v2 $v3"
}

#
#  calculate the angle between two vectors.
#  Note:  we "overload" the operator here.  If
#  either of the inputs is a list, a vector
#  is calculated from the points specified, otherwise
#  it is assumed to already be a vector.
#

proc gdscAngleBetweenVectors {in1 in2 return_angle} {

  upvar $return_angle angle
  set angle {}

  # if the input consists of a set of points, calculate the
  # vector.

  if {[llength $in1] > 3} {
    puts "Error:  Vector with [llength $in1] components not of correct length!"
    return -code error GDSC_ERR
  }
  if {[llength $in2] > 3} {
    puts "Error:  Vector with [llength $in2] components not of correct length!"
    return -code error GDSC_ERR
  }

  set a1 {}

  if {[llength $in1] == 3} {
    set a1 [expr double([lindex $in1 0])]
    set a2 [expr double([lindex $in1 1])]
    set a3 [expr double([lindex $in1 2])]
  } elseif {[llength $in1] == 2} {
    set a1 [expr double([lindex [lindex $in1 1] 0] - [lindex [lindex $in1 0] 0])]
    set a2 [expr double([lindex [lindex $in1 1] 1] - [lindex [lindex $in1 0] 1])]
    set a3 [expr double([lindex [lindex $in1 1] 2] - [lindex [lindex $in1 0] 2])]
  } else {
    puts "Error:  Invalid length of vector $in1!"
    return -code error GDSC_ERR
  }

  set b1 {}

  if {[llength $in2] == 3} {
    set b1 [expr double([lindex $in2 0])]
    set b2 [expr double([lindex $in2 1])]
    set b3 [expr double([lindex $in2 2])]
  } elseif {[llength $in2] == 2} {
    set b1 [expr double([lindex [lindex $in2 1] 0] - [lindex [lindex $in2 0] 0])]
    set b2 [expr double([lindex [lindex $in2 1] 1] - [lindex [lindex $in2 0] 1])]
    set b3 [expr double([lindex [lindex $in2 1] 2] - [lindex [lindex $in2 0] 2])]
  } else {
    puts "Error:  Invalid length of vector $in2!"
    return -code error GDSC_ERR
  }

  # calculate the angle between vector a and b.
  # This corresponds to a . b = | a | | b | cos (theta)
  set a [gdscVectorNormalize [list $a1 $a2 $a3]]
  set b [gdscVectorNormalize [list $b1 $b2 $b3]]
  #set a [list $a1 $a2 $a3]
  #set b [list $b1 $b2 $b3]

  set magA [gdscVectorMagnitude $a]
  set magB [gdscVectorMagnitude $b]

  set AdotB 0.0
  gdscAdotB $a $b AdotB
  puts "a: $a  b: $b"
  puts "adotb: $AdotB mags: $magA  $magB  value:[expr double($AdotB)/double($magA*$magB)]"
  set angle [expr acos(double($AdotB)/($magA*$magB))]
  puts "angle: $angle"
  return GDSC_OK
}

#
#  proc to return a list of points for the nth polygon
#  in a vtkPolyData object
#

proc gdscGetPointsFromNthPoly {nth pd result} {

  upvar $result points
  set points {}

  if {[info commands $pd] == ""} {
    puts "Error: vtkPolyData object $pd does not exist!"
    return -code error GDSC_ERR
  }

  set mypoints [$pd GetPoints]
  set numRegions [$pd GetNumberOfPolys]

  if {$nth >= $numRegions} {
    puts "Error: Requested $nth cell exceeds limit of $numRegions in $pd."
    return -code error GDSC_ERR
  }

  set mypolys [$pd GetPolys]
  $mypolys InitTraversal
  set mydata [$mypolys GetData]
  set numItems [$mydata GetSize]

  set j 0
  for {set i 0} {$i < $numRegions && $j < $numItems} {incr i} {
    set numPts [$mydata GetValue $j]
    incr j
    if {$i == $nth} {
      set points {}
      for {set k 0} {$k < $numPts && $j < $numItems} {incr k} {
        set id [$mydata GetValue $j]
        set pt [$mypoints GetPoint $id]
        lappend points $pt
        incr j
      }
      break
    } else {
      for {set k 0} {$k < $numPts && $j < $numItems} {incr k} {
        set id [$mydata GetValue $j]
        incr j
      }
    }
  }

  return GDSC_OK

}




#
#  vsclrCreateTrimBox
#
#  Code which creates rectangular solid for trimming of vascular models.
#
#  Inputs:  topLeft, topRight, botRight == points of phase contrast
#                                          plane
#           sizeofbox                   == height of box
#
#  Outputs: joy_solid == a 3d solid created by (effectively) extruding
#                        the rectangle defined by pts in the direction
#                        of normal, with a height of "sizeofbox".
#

proc vsclrCreateTrimBox {topLeft topRight botRight sizeofbox phaseplane} {

  ###
  ###  Do work
  ###

  #
  #  temporary vtk objects:  ptsToExtrude, conn, extrudeMe
  #  temporary repos objs:   joy_polygon_pd, joy_polygon
  #

  # calculate origin.

  set tmpvect [list [expr double([lindex $botRight 0]) - double([lindex $topRight 0])] \
                    [expr double([lindex $botRight 1]) - double([lindex $topRight 1])] \
                    [expr double([lindex $botRight 2]) - double([lindex $topRight 2])]]

  set origin [list [expr double([lindex $topLeft 0]) + double([lindex $tmpvect 0])] \
                   [expr double([lindex $topLeft 1]) + double([lindex $tmpvect 1])] \
                   [expr double([lindex $topLeft 2]) + double([lindex $tmpvect 2])]]

  # need to calculate two vectors in the plane to get the normal
  #   nrm =  a X b

  set a1  [expr double([lindex $botRight 0]) - double([lindex $origin 0])]
  set a2  [expr double([lindex $botRight 1]) - double([lindex $origin 1])]
  set a3  [expr double([lindex $botRight 2]) - double([lindex $origin 2])]

  set b1  [expr double([lindex $topLeft 0]) - double([lindex $origin 0])]
  set b2  [expr double([lindex $topLeft 1]) - double([lindex $origin 1])]
  set b3  [expr double([lindex $topLeft 2]) - double([lindex $origin 2])]

  set n1 [expr double($a2)*double($b3) - double($a3)*double($b2)]
  set n2 [expr double($a3)*double($b1) - double($a1)*double($b3)]
  set n3 [expr double($a1)*double($b2) - double($a2)*double($b1)]

  # normalize normal and then modify magnitude to create the
  # desired height of the box
  set nrmmag [expr sqrt(double($n1*$n1) + double($n2*$n2) +double($n3*$n3))]
  set n1 [expr double($n1)/double($nrmmag) * double($sizeofbox)]
  set n2 [expr double($n2)/double($nrmmag) * double($sizeofbox)]
  set n3 [expr double($n3)/double($nrmmag) * double($sizeofbox)]

  set nrm "$n1 $n2 $n3"
  puts "nrm: $nrm"

  #
  #  Points used to create polygon.  In the case of trimming for
  #  vascular applications, these would be the coordinates of the phase
  #  contrast plane.
  #
  #  These points should be counter-clockwise and define the bottom
  #  of the rectangular solid.
  #

  set pts [list $origin $botRight $topRight $topLeft]

  #  temporary vtk objects:  ptsToExtrude, conn, extrudeMe
  #  temporary repos objs:   joy_polygon_pd, joy_polygon

  # delete vtk objects if they exist

  if {[info commands ptsToExtrude] != ""} {
    ptsToExtrude Delete
  }
  if {[info commands conn] != ""} {
    conn Delete
  }
  if {[info commands extrudeMe] != ""} {
    extrudeMe Delete
  }
  if {[info commands myTrans] != ""} {
    myTrans Delete
  }
  if {[info commands myFilter] != ""} {
    myFilter Delete
  }
  # delete objects in repository if they already exist

  if {[repos_exists -obj joy_polygon_pd] == "1"} {
    repos_delete -obj joy_polygon_pd
  }
  if {[repos_exists -obj joy_polygon] == "1"} {
    repos_delete -obj joy_polygon
  }
  if {[repos_exists -obj joy_solid] == "1"} {
    repos_delete -obj joy_solid
  }

  # allocate vtk objects

  vtkPoints ptsToExtrude
  ptsToExtrude Allocate 200 400

  vtkIdList conn
  conn Initialize
  conn Allocate 200 400

  vtkPolyData extrudeMe
  extrudeMe Initialize
  extrudeMe Allocate 200 400

  # create vtkPoints array
  for {set i 0} {$i < [llength $pts]} {incr i} {
    set pt [lindex $pts $i]
    puts "pt $i: $pt"
    ptsToExtrude InsertNextPoint [lindex $pt 0] [lindex $pt 1] [lindex $pt 2]
  }

  # create connectivity for polygon
  for {set i 0} {$i < [llength $pts]} {incr i} {
  conn InsertNextId $i
  }

  extrudeMe SetPoints ptsToExtrude
  set VTK_POLYGON 7
  extrudeMe InsertNextCell $VTK_POLYGON conn

  # calculate the angle between z-axis and the current plane.
  # This corresponds to a . b = | a | | b | cos (theta)
  # Where a & b are vectors.  In this case, b = <0,0,1>
  # and a = normal, therefore the angle is just theta = acos(n[2]*1)

  puts "normal: $nrm"
  set normal [gdscVectorNormalize $nrm]
  set theta [expr acos(double([lindex $normal 2]))]

  # calculate rotation vector
  # This corresponds to r = a X b
  # where b = z axis <0,0,1>
  # and a = normal

  set rotvector [list [expr -1.0*[lindex $nrm 1]] \
                [lindex $nrm 0] \
                0.0]

  # if the plane is already in z=const, then magnitude
  # of the cross product of normal and z axis zero
  set magrot [gdscVectorMagnitude $rotvector]

  puts "magrot: $magrot"
  puts "rotvector: $rotvector"
  if {$magrot > 0.00001} {
    set rotvector [gdscVectorNormalize $rotvector]
    set rtheta [expr double(2.0*3.14159265358979323846-double($theta))]
    # angle must be in degrees for vtk
    set rtheta [expr double($rtheta)*180.0/3.14159265358979323846]
    puts "theta: $rtheta"
    vtkTransform myTrans
    myTrans PostMultiply
    myTrans RotateWXYZ $rtheta [lindex $rotvector 0] \
                               [lindex $rotvector 1] \
                               [lindex $rotvector 2]
    vtkTransformPolyDataFilter myFilter
    myFilter SetInputDataObject extrudeMe
    myFilter SetTransform myTrans
    myFilter Update
    set transformed [myFilter GetOutput]
    repos_importVtkPd -src $transformed -dst joy_polygon_pd
  } else {
    repos_importVtkPd -src extrudeMe -dst joy_polygon_pd
  }

  set dodo [repos_exportToVtk -src joy_polygon_pd]
  set keepz [lindex [$dodo GetPoint 0] 2]

  #ignore all the crap about the phase contrast plane

  if {0 == 1} {

  vtkTransform t1
  vtkTransform t2
  vtkTransform t3

  vtkTransform prayer
  prayer PostMultiply 
  prayer RotateWXYZ 180.0 1 0 0
  
  gdscGetPointsFromNthPoly 0 $dodo points
  puts "points: $points"
  # first rotate
  gdscAngleBetweenVectors [list [lindex $points 0] [lindex $points 1]] \
                          [list 1 0 0] angle
  gdscAngleBetweenVectors [list [lindex $points 2] [lindex $points 3]] \
                          [list 1 0 0] angle
  t1 PostMultiply
  t1 RotateWXYZ [expr double(270.0-double($angle)*180.0/3.14159265358979323846)] \
                        0 0 1
  #t1 RotateWXYZ [expr 180+15] 0 0 1
  # then translate
  set rotorg [lindex $points 0]
  t2 Translate [lindex $rotorg 0] [lindex $rotorg 1] [lindex $rotorg 2]

  # then rotate again
  t3 PostMultiply
  t3 RotateWXYZ [expr double(180.0/3.14159265358979323846*$theta)] [lindex $rotvector 0] \
                                      [lindex $rotvector 1] \
                                      [lindex $rotvector 2]

  vtkTransformPolyDataFilter f0
  vtkTransformPolyDataFilter f1
  vtkTransformPolyDataFilter f2
  vtkTransformPolyDataFilter f3

  #f0 SetInputDataObject $phaseplane
  #f0 SetTransform prayer
  #f0 Update

  f1 SetInputDataObject $phaseplane
#  f1 SetInputDataObject [f0 GetOutput]
  f1 SetTransform t1
  f1 Update

  f2 SetInputDataObject [f1 GetOutput]
  f2 SetTransform t2
  f2 Update

  f3 SetInputDataObject [f2 GetOutput]
  f3 SetTransform t3
  f3 Update

  #vtkTransformPolyDataFilter f4
  #f4 SetTransform t2
  #f4 SetInputDataObject $phaseplane
  #f4 Update
  #repos_importVtkPd -src [f4 GetOutput] -dst movedit

  set transformedPC [f3 GetOutput]
  repos_importVtkPd -src $transformedPC -dst phase_contrast
  repos_setLabel -obj phase_contrast -key showK -value 1

  }




  solid_polyPts -src joy_polygon_pd -dst joy_polygon

  # now move it back to where it belongs in space
  joy_polygon Translate -vec [list 0 0 $keepz]
  if {$magrot > 0.00001} {
    joy_polygon Rotate -axis $rotvector -rad [expr double($theta)]
  }

  # create a solid model (surface) from the vtkPolyData
  #solid_poly3dSurface -result joy_polygon -src joy_polygon_pd -facet Union

  # extrude the surface into a 3d solid along the given path.

  # Note: the magnitude of the extrusion is the magnitude of the vector:
  #       v = pt2 - pt1
  #
  #       pt1 should lie on the plane being extruded.
  #       pt2 should be defined on the normal to the plane at pt1.
  #

  # calculate points
  set pt1 [lindex $pts 0]
  set pt2 [list [expr double([lindex $pt1 0]) + double([lindex $nrm 0])] \
                [expr double([lindex $pt1 1]) + double([lindex $nrm 1])] \
                [expr double([lindex $pt1 2]) + double([lindex $nrm 2])]]

  solid_extrude -src joy_polygon -dst joy_solid -pt1 $pt1 -pt2 $pt2

  # only for display purposes by vfabView
  repos_setLabel -obj joy_solid -key color -value red
  repos_setLabel -obj joy_solid -key opacity -value 0.5
}

proc rotatePlaneIntoXYZ {input output} {

  if {[repos_exists -obj $input] == "0"} {
    puts "Error: input $input does not exist in the repository!"
    return -code error GDSC_ERR
  }
  if {[repos_exists -obj $output] == "1"} {
    puts "Error: output $output exists!"
    return -code error GDSC_ERR
  }
  if {[repos_type -obj $input] != "PolyData"} {
    puts "Error: input $input is not of type PolyData!"
    return -code error GDSC_ERR
  }

  set t1 tmpRotatePlaneXYZ1
  set t2 tmpRotatePlaneXYZ2
  set t3 tmpRotatePlaneXYZ3
  set t4 tmpRotatePlaneXYZ4
  set f1 tmpRotatePlaneXYZ5
  set f2 tmpRotatePlaneXYZ6
  set f3 tmpRotatePlaneXYZ7
  set f4 tmpRotatePlaneXYZ8

  catch {$t1 Delete}
  catch {$t2 Delete}
  catch {$t3 Delete}
  catch {$t4 Delete}
  catch {$f1 Delete}
  catch {$f2 Delete}
  catch {$f3 Delete}
  catch {$f4 Delete}

  vtkTransform $t1
  vtkTransform $t2
  vtkTransform $t3
  vtkTransform $t4

  set XYZ_origin {0.0 0.0 0.0}
  set RAS_origin {-159.5 -101.4 -62.2}
  # translate slice to origin
  $t1 Translate [expr [lindex $XYZ_origin 0]-[lindex $RAS_origin 0]] \
                [expr [lindex $XYZ_origin 1]-[lindex $RAS_origin 1]] \
                [expr [lindex $XYZ_origin 2]-[lindex $RAS_origin 2]]

  # now perform double rotation to get RAS -> XYZ
  $t2 PostMultiply
  $t2 RotateWXYZ 180.0 0 1 0

  $t3 PostMultiply
  $t3 RotateWXYZ  90.0 1 0 0

  # Assuming data origin is the bottom right corner of the first
  # (furthest back) image slice, need to move that to the global
  # coordinate system origin first before rotating.  Set
  # vector to the equivalent XYZ coordinates of the minimum
  # point (in RAS space)  for the data block.  Determine XYZ
  # coordinates by number of pixels * pixel size
  #$t1 Translate -300 0 0
  $t1 Translate -300 0 0

  vtkTransformPolyDataFilter $f1
  vtkTransformPolyDataFilter $f2
  vtkTransformPolyDataFilter $f3
  vtkTransformPolyDataFilter $f4

  $f1 SetInputDataObject [repos_exportToVtk -src $input]
  $f1 SetTransform $t1
  $f1 Update

  $f2 SetInputDataObject [$f1 GetOutput]
  $f2 SetTransform $t2
  $f2 Update

  $f3 SetInputDataObject [$f2 GetOutput]
  $f3 SetTransform $t3
  $f3 Update

  $f4 SetInputDataObject [$f3 GetOutput]
  $f4 SetTransform $t4
  $f4 Update

  set transformed [$f4 GetOutput]
  repos_importVtkPd -src $transformed -dst $output
  repos_setLabel -obj $output -key showK -value 1

  #catch {$t1 Delete}
  #catch {$t2 Delete}
  #catch {$t3 Delete}
  #catch {$t4 Delete}
  #catch {$f1 Delete}
  #catch {$f2 Delete}
  #catch {$f3 Delete}
  #catch {$f4 Delete}

}


#
#  main code
#

proc trimSolid {} {

  set trimTmp /tmp/trimTmp
  catch {repos_delete -obj $trimTmp}

  global symbolicName
  global gImageVol
  global gBC

  global gFilenames
  global gObjects

  set solid $gObjects(preop_solid)
  set solid_trimmed $gObjects(preop_trimmed1)
  set solid_trimmed_2 $gObjects(preop_trimmed2)

  catch {repos_delete -obj $solid_trimmed}
  catch {repos_delete -obj $solid_trimmed_2}

  solid_copy -src $solid -dst $trimTmp


  # Transform the model to be in the RAS coordinate system
  global gOptions
  if {$gOptions(orientImgVolToRAS) == 0} {

    # Assuming data origin is the bottom right corner of the first
    # (furthest back) image slice, need to move that to the global
    # coordinate system origin first before rotating.  Set
    # vector to the equivalent XYZ coordinates of the minimum
    # point (in RAS space)  for the data block.  Determine XYZ
    # coordinates by number of pixels * pixel size
    $trimTmp Translate -vec "-[expr $gImageVol(ext_i)*$gImageVol(vdims_x)] 0 0"

    $trimTmp Rotate -axis {0 1 0} -rad 3.14159265
    $trimTmp Rotate -axis {1 0 0} -rad 1.570796325

    # Vector is RAS coordinates of image origin (take the minimum
    # of all RAS values for each coordinate from MRA data set)
    #img_guessRASmin $gImageVol(filename)
    $trimTmp Translate -vec $gImageVol(min_RAS)
  }

  solid_setKernel -name Parasolid

  # Coordinates for inlet flow plane
  set topLeft $gBC(top_left_corner)
  set topRight $gBC(top_right_corner)
  set botRight $gBC(bottom_right_corner)
  set sizeofbox $gBC(size_of_trim_box)

  vsclrCreateTrimBox $topLeft $topRight $botRight $sizeofbox blank

  # tag surfaces so we now where the inflow is
  foreach i [joy_solid GetFaceIds] {
     joy_solid SetFaceAttr -faceId $i -attr gdscName -value "inflow"
  }

  # Now do a Boolean subtraction
  solid_subtract -result $solid_trimmed -a $trimTmp -b joy_solid

  # for now, create a solid with the negative of sizeofbox just
  # in case
  vsclrCreateTrimBox $topLeft $topRight $botRight [expr -1.0*$sizeofbox] blank

  # tag surfaces so we now where the inflow is
  foreach i [joy_solid GetFaceIds] {
     joy_solid SetFaceAttr -faceId $i -attr gdscName -value "inflow"
  }

  # Now do a Boolean subtraction
  solid_subtract -result $solid_trimmed_2 -a $trimTmp -b joy_solid

  # Visualize results + planes for sanity check
  if {0 == 1} {
  set plane trimSolidPlane1
  catch {$plane Delete}
  vtkPlaneSource $plane
  $plane SetOrigin [lindex $topRight 0] [lindex $topRight 1] [lindex $topRight 2]
  $plane SetPoint1 [lindex $topLeft 0] [lindex $topLeft 1] [lindex $topLeft 2]
  $plane SetPoint2 [lindex $botRight 0] [lindex $botRight 1] [lindex $botRight 2]
  $plane Update
  catch {repos_delete -obj /tmp/trim/plane1}
  repos_importVtkPd -src [$plane GetOutput] -dst /tmp/trim/plane1
  repos_setLabel -obj /tmp/trim/plane1 -key opacity -value 0.75
  repos_setLabel -obj /tmp/trim/plane1 -key color -value green
  $plane Delete
  }

  return GDSC_OK
}

# ------------
# mc_LoadImage
# ------------

proc mc_LoadImage {} {

    global gImageVol
    global gImageFoo
    global gPathBrowser
    global gRen3d
    global gInitCamera

    set x $gImageVol(vdims_x)
    set y $gImageVol(vdims_y)
    set z $gImageVol(vdims_z)
    if {(![math_isDouble $x]) || (![math_isDouble $y]) || \
	    (![math_isDouble $z])} {
	return -code error "error in voxel dims: $x $y $z"
	return -code error
    }
    set vdims [list $x $y $z]

    set i $gImageVol(ext_i)
    set j $gImageVol(ext_j)
    set k $gImageVol(ext_k)
    if {(![math_isInt $i]) || (![math_isInt $j]) || (![math_isInt $k])} {
	return -code error "error in logical extent: $i $j $k"
	return -code error
    }
    set ext [list $i $j $k]

    set x0 $gImageVol(voi_x0)
    set y0 $gImageVol(voi_y0)
    set z0 $gImageVol(voi_z0)
    set x1 $gImageVol(voi_x1)
    set y1 $gImageVol(voi_y1)
    set z1 $gImageVol(voi_z1)

    set hdrsz $gImageVol(file_hdr_size)
    if {![math_isInt $hdrsz]} {
	return -code error "error in header size: $hdrsz"
	return -code error
    }

    set fn $gImageVol(filename)
    if {![file exists $fn]} {
	return -code error "couldn't find file $fn"
	return -code error
    }

    set toks [cleanList [split $fn /]]
    set tok [lrange $toks end end]
    set ftoks [split $tok .]
    if {[llength $ftoks] != 2} {
	return -code error "unexpected image file name format"
	return -code error
    }

    set prefix [lindex $ftoks 0]
    set suffix [lindex $ftoks 1]
    set numlen [string length $suffix]
    set start [trimLeadZeros $suffix]
    if {![math_isInt $start]} {
	return -code error "unexpected image file suffix $suffix"
	return -code error
    }
    set pathToks [lrange $toks 0 [expr [llength $toks] - 2]]
    set pathToks [lappend pathToks $prefix]

    # if the path is give as an absolute UNIX path,
    # we have to explicitly reattach the first slash
    set fileFirstChar [string index [string trim $fn] 0]
    if {$fileFirstChar == "/"} {
      set fullPrefix [format "/%s" [join $pathToks /]]
    } else {
      set fullPrefix [join $pathToks /]
    }

    set b 0
    if {[catch {cmdExists $gRen3d} b] || (!$b)} {
	if {$gPathBrowser(use_vtkTkRenderWidget)} {
	    #set gRen3d [vis_initTKgr gRenWin_3D "Geodesic 3-D Graphics Window" {}]
            set gRen3d [vis_gRenWin_3D]
	} else {
	    set gRen3d [vis_initgr gRenWin_3D]
	}
    }

    if {(![math_isInt $x0]) || (![math_isInt $y0]) || \
	    (![math_isInt $z0]) || (![math_isInt $x1]) || \
	    (![math_isInt $y1]) || (![math_isInt $z1])} {
	set voiExt [list \
		0 [expr [lindex $ext 0] - 1] \
		0 [expr [lindex $ext 1] - 1] \
		0 [expr [lindex $ext 2] - 1]]
    } else {
	set voiExt [list $x0 $x1 $y0 $y1 $z0 $z1]
    }

    set filePattern $gImageVol(filePattern)
    set voi [img_GetVOI $fullPrefix $vdims $ext $start $hdrsz $voiExt $filePattern]

    vis_img_VtkVolImg $voi $ext $gRen3d gImageFoo

    #vis_img_VolImgBrowser gImageFoo
    vis_img_VolImgBrwsr2 volume_image vis_img_vol_browser
    
    global guiVIB
    set guiVIB(use_alt_image) 0

    set guiVIB(show_image) 1

    # check and see if we are using the new gui 
    #if {[info procs ShowWindow.guiSVIMG] != ""} {
    #  ShowWindow.guiSVIMG
    #} else {
    #  ShowWindow.volGUI
    #}
    volGUIsetup guiVIB

    vis_renReset $gRen3d
    set w [$gRen3d GetRenderWindow]
    if {!($gPathBrowser(use_vtkTkRenderWidget))} {
      $w SetSize 500 500
      $w SetPosition 5 23
    }
    if {[llength [array names gInitCamera]] == 0} {
	vis_renSaveCamera $gRen3d gInitCamera
    }

}


# ---------------
# mc_LoadImageXML
# ---------------

proc mc_LoadImageXML {} {

    global gOptions
    global gImageVol
    global gImageFoo
    global gPathBrowser
    global gRen3d
    global gInitCamera

    set fn $gImageVol(xml_filename)
    if {![file exists $fn]} {
	return -code error "couldn't find file $fn"
	return -code error
    }

    # read image data
    set xmlreader __xmlVolReader
    catch {$xmlreader Delete}
    vtkXMLImageDataReader $xmlreader
    $xmlreader SetFileName $fn

    global gOptions
    if {$gOptions(show_progress_widget) == 1} {
    }

    $xmlreader Update

    # create a repository vtkImg object
    set imgobj volume_image
    catch {repos_delete -obj $imgobj}
    repos_importVtkImg -src [$xmlreader GetOutput] -dst $imgobj
    set voi [repos_exportToVtk -src $imgobj]
    $xmlreader Delete

    set vdims [$voi GetSpacing]
    set ext [$voi GetDimensions]
    set voiExt [$voi GetExtent]

    set gImageVol(vdims_x) [lindex $vdims 0]
    set gImageVol(vdims_y) [lindex $vdims 1]
    set gImageVol(vdims_z) [lindex $vdims 2]

    set gImageVol(ext_i) [lindex $ext 0]
    set gImageVol(ext_j) [lindex $ext 1]
    set gImageVol(ext_k) [lindex $ext 2]

    set gImageVol(voi_x0) [lindex $voiExt 0]
    set gImageVol(voi_x1) [lindex $voiExt 1]
    set gImageVol(voi_y0) [lindex $voiExt 2]
    set gImageVol(voi_y1) [lindex $voiExt 3]
    set gImageVol(voi_z0) [lindex $voiExt 4]
    set gImageVol(voi_z1) [lindex $voiExt 5]

    set gImageVol(file_hdr_size) 0
    set gImageVol(directionCosines) {}
    set gImageVol(filePattern) {}
    set gImageVol(filename) {}
    set gOptions(image_data_type) generic

    set gImageVol(min_RAS) [math_subVectors [$voi GetOrigin] [math_scaleVec $vdims 0.5]]
    set gImageVol(ext_ras) $ext
    set gImageVol(vdims_ras) $vdims
    set gImageVol(voi_ras) $voiExt
    set gImageVol(vtk_org_ras) [$voi GetOrigin]

    set b 0
    if {[catch {cmdExists $gRen3d} b] || (!$b)} {
	if {$gPathBrowser(use_vtkTkRenderWidget)} {
	    #set gRen3d [vis_initTKgr gRenWin_3D "Geodesic 3-D Graphics Window" {}]
            set gRen3d [vis_gRenWin_3D]
	} else {
	    set gRen3d [vis_initgr gRenWin_3D]
	}
    }

    vis_img_VtkVolImg $voi $ext $gRen3d gImageFoo
    vis_img_VolImgBrwsr2 volume_image vis_img_vol_browser
    # check and see if we are using the new gui 

    global guiVIB
    set guiVIB(use_alt_image) 0
    set guiVIB(show_image) 1

    #if {[info procs ShowWindow.guiSVIMG] != ""} {
    #  ShowWindow.guiSVIMG
    #} else {
    #  ShowWindow.volGUI
    #}
    volGUIsetup guiVIB

    vis_renReset $gRen3d
    set w [$gRen3d GetRenderWindow]
    if {!($gPathBrowser(use_vtkTkRenderWidget))} {
      $w SetSize 500 500
      $w SetPosition 5 23
    }
    if {[llength [array names gInitCamera]] == 0} {
	vis_renSaveCamera $gRen3d gInitCamera
    }

    global gOptions
    if {$gImageVol(vdims_x) < 0.5 ||  $gImageVol(vdims_y) < 0.5 || $gImageVol(vdims_z) < 0.5} {
	if {($gOptions(facet_max_edge_size) > 2) || ($gOptions(pathDisplayPointSize) > 1)} { 
          set yesno [tk_messageBox -default yes  -message "It appears some global display parameters are set for mm instead of cm.  Would you like the software to adjust settings?"  -title "Adjust Global Display Settings"  -type yesno]
          switch -- $yesno {
            yes {
              set gOptions(pathDisplayPointSize) 0.3
              set gOptions(facet_max_edge_size) 1.0
            }
            no {
            }
          }
	}
    }

}


# -----------------
# mc_LoadImageDICOM
# -----------------

proc mc_LoadImageDICOM {change_to_cm} {

    global gImageVol
    global gImageFoo
    global gPathBrowser
    global gRen3d
    global gInitCamera

    set fn $gImageVol(data_dirname)
    if {![file exists $fn]} {
	return -code error "couldn't find $fn"
    }

    # create a repository vtkImg object
    set imgobj volume_image
    catch {repos_delete -obj $imgobj}
    img_readDICOM $gImageVol(data_dirname) $change_to_cm $imgobj

    set voi [repos_exportToVtk -src $imgobj]

    set vdims [$voi GetSpacing]
    set ext [$voi GetDimensions]
    set voiExt [$voi GetExtent]

    set gImageVol(vdims_x) [lindex $vdims 0]
    set gImageVol(vdims_y) [lindex $vdims 1]
    set gImageVol(vdims_z) [lindex $vdims 2]

    set gImageVol(ext_i) [lindex $ext 0]
    set gImageVol(ext_j) [lindex $ext 1]
    set gImageVol(ext_k) [lindex $ext 2]

    set gImageVol(voi_x0) [lindex $voiExt 0]
    set gImageVol(voi_x1) [lindex $voiExt 1]
    set gImageVol(voi_y0) [lindex $voiExt 2]
    set gImageVol(voi_y1) [lindex $voiExt 3]
    set gImageVol(voi_z0) [lindex $voiExt 4]
    set gImageVol(voi_z1) [lindex $voiExt 5]

    #set gImageVol(file_hdr_size) 0
    #set gImageVol(directionCosines) {}
    #set gImageVol(filePattern) {}
    #set gImageVol(filename) {}

    set gImageVol(min_RAS) [math_subVectors [$voi GetOrigin] [math_scaleVec $vdims 0.5]]
    set gImageVol(ext_ras) $ext
    set gImageVol(vdims_ras) $vdims
    set gImageVol(voi_ras) $voiExt
    set gImageVol(vtk_org_ras) [$voi GetOrigin]

    set b 0
    if {[catch {cmdExists $gRen3d} b] || (!$b)} {
	if {$gPathBrowser(use_vtkTkRenderWidget)} {
	    #set gRen3d [vis_initTKgr gRenWin_3D "Geodesic 3-D Graphics Window" {}]
            set gRen3d [vis_gRenWin_3D]
	} else {
	    set gRen3d [vis_initgr gRenWin_3D]
	}
    }

    vis_img_VtkVolImg $voi $ext $gRen3d gImageFoo
    vis_img_VolImgBrwsr2 volume_image vis_img_vol_browser
    # check and see if we are using the new gui 

    global guiVIB
    set guiVIB(use_alt_image) 0
    set guiVIB(show_image) 1

    #if {[info procs ShowWindow.guiSVIMG] != ""} {
    #  ShowWindow.guiSVIMG
    #} else {
    #  ShowWindow.volGUI
    #}
    volGUIsetup guiVIB

    vis_renReset $gRen3d
    set w [$gRen3d GetRenderWindow]
    if {!($gPathBrowser(use_vtkTkRenderWidget))} {
      $w SetSize 500 500
      $w SetPosition 5 23
    }
    if {[llength [array names gInitCamera]] == 0} {
	vis_renSaveCamera $gRen3d gInitCamera
    }

    global gOptions
    if {$gImageVol(vdims_x) < 0.5 ||  $gImageVol(vdims_y) < 0.5 || $gImageVol(vdims_z) < 0.5} {
	if {($gOptions(facet_max_edge_size) > 2) || ($gOptions(pathDisplayPointSize) > 1)} { 
          set yesno [tk_messageBox -default yes  -message "It appears some global display parameters are set for mm instead of cm.  Would you like the software to adjust settings?"  -title "Adjust Global Display Settings"  -type yesno]
          switch -- $yesno {
            yes {
              set gOptions(pathDisplayPointSize) 0.3
              set gOptions(facet_max_edge_size) 1.0
            }
            no {
            }
          }
	}
    }
}
# -----------------
# seg_LoadImageMha
# -----------------
proc seg_LoadImageMha {} {

  global gOptions
  global gImageVol
  global gImageFoo
  global gPathBrowser
  global gRen3d
  global gInitCamera

  global gOptions

  set fn $gImageVol(mha_filename)
    if {![file exists $fn]} {
  return -code error "couldn't find $fn"
    }
  catch {reader Delete}
  vtkMetaImageReader reader
  reader SetFileName $fn
  reader Update


  set imgobj volume_image
  catch {repos_delete -obj $imgobj}
  repos_importVtkImg -src [reader GetOutput] -dst $imgobj

  set voi [repos_exportToVtk -src $imgobj]

  reader Delete

  set vdims [$voi GetSpacing]
  set ext [$voi GetDimensions]
  set voiExt [$voi GetExtent]
  set voiBounds [$voi GetBounds]
  
  set voiOrgX [expr ([lindex $voiBounds 0] + [lindex $voiBounds 1])/2]
  set voiOrgY [expr ([lindex $voiBounds 2] + [lindex $voiBounds 3])/2]
  set voiOrgZ [expr ([lindex $voiBounds 4] + [lindex $voiBounds 5])/2]
  
  set voiOrigin "$voiOrgX $voiOrgY $voiOrgZ"
  
  #puts "voiOrigin: $voiOrigin"
  #puts "l1: [lindex $voiOrigin 0]"

  set gImageVol(vdims_x) [lindex $vdims 1]
  set gImageVol(vdims_y) [lindex $vdims 0]
  set gImageVol(vdims_z) [lindex $vdims 2]

  set gImageVol(ext_i) [lindex $ext 1]
  set gImageVol(ext_j) [lindex $ext 0]
  set gImageVol(ext_k) [lindex $ext 2]

  set gImageVol(voi_x0) [lindex $voiExt 0]
  set gImageVol(voi_x1) [lindex $voiExt 1]
  set gImageVol(voi_y0) [lindex $voiExt 2]
  set gImageVol(voi_y1) [lindex $voiExt 3]
  set gImageVol(voi_z0) [lindex $voiExt 4]
  set gImageVol(voi_z1) [lindex $voiExt 5]

  set gImageVol(file_hdr_size) 0
  set gImageVol(directionCosines) {}
  set gImageVol(filePattern) {}
  set gImageVol(filename) {}
  set gOptions(image_data_type) generic

  
  #puts "[math_subVectors [$voi GetOrigin] [math_scaleVec $vdims 0.5]] = math_subVectors ([$voi GetOrigin]) ([math_scaleVec $vdims 0.5])"
  set gImageVol(min_RAS) [math_subVectors [$voi GetOrigin] [math_scaleVec $vdims 0.5]]
  set gImageVol(ext_ras) $ext
  set gImageVol(vdims_ras) $vdims
  set gImageVol(voi_ras) $voiExt
  set gImageVol(vtk_org_ras) $voiOrigin

  set b 0
  if {[catch {cmdExists $gRen3d} b] || (!$b)} {
   if {$gPathBrowser(use_vtkTkRenderWidget)} {
     #set gRen3d [vis_initTKgr gRenWin_3D "Geodesic 3-D Graphics Window" {}]
     set gRen3d [vis_gRenWin_3D]
     } else {
       set gRen3d [vis_initgr gRenWin_3D]
     }
   }

   vis_img_VtkVolImg $voi $ext $gRen3d gImageFoo
   vis_img_VolImgBrwsr2 volume_image vis_img_vol_browser
   # check and see if we are using the new gui 

   global guiVIB
   set guiVIB(use_alt_image) 0
   set guiVIB(show_image) 1

   #if {[info procs ShowWindow.guiSVIMG] != ""} {
    #  ShowWindow.guiSVIMG
    #} else {
      #  ShowWindow.volGUI
      #}
      volGUIsetup guiVIB

      vis_renReset $gRen3d
      set w [$gRen3d GetRenderWindow]
      if {!($gPathBrowser(use_vtkTkRenderWidget))} {
        $w SetSize 500 500
        $w SetPosition 5 23
      }
      if {[llength [array names gInitCamera]] == 0} {
       vis_renSaveCamera $gRen3d gInitCamera
     }

     global gOptions
     if {$gImageVol(vdims_x) < 0.5 ||  $gImageVol(vdims_y) < 0.5 || $gImageVol(vdims_z) < 0.5} {
       if {($gOptions(facet_max_edge_size) > 2) || ($gOptions(pathDisplayPointSize) > 1)} { 
        set yesno [tk_messageBox -default yes  -message "It appears some global display parameters are set for mm instead of cm.  Would you like the software to adjust settings?"  -title "Adjust Global Display Settings"  -type yesno]
        switch -- $yesno {
          yes {
            set gOptions(pathDisplayPointSize) 0.3
            set gOptions(facet_max_edge_size) 1.0
          }
          no {
          }
        }
      }
    }
  }



# -----------------
# vis_img_VtkVolImg
# -----------------
# This is an alternate method of initializing the volume image
# pipeline.  Instead of reading image data files, this proc starts
# with a pre-existing vtk image object (e.g. from some independent VOI
# extraction).

proc vis_img_VtkVolImg {vtk_img totExt ren createdObjs} {

    upvar $createdObjs result

    set winlevflt   vis_img_vol_winlevflt_$vtk_img
    set conv        vis_img_vol_convert_$vtk_img
    set outsrc      vis_img_vol_outsrc_$vtk_img
    set outmap      vis_img_vol_outmap_$vtk_img
    set outact      vis_img_vol_outact_$vtk_img


    set voxelDims [$vtk_img GetSpacing]
    set logicalDims [$vtk_img GetDimensions]

    set totImgMin [list 0 0 0]
    set totImgMax [list \
	    [expr [lindex $totImgMin 0] + \
	    [lindex $totExt 0] * [lindex $voxelDims 0]] \
	    [expr [lindex $totImgMin 1] + \
	    [lindex $totExt 1] * [lindex $voxelDims 1]] \
	    [expr [lindex $totImgMin 2] + \
	    [lindex $totExt 2] * [lindex $voxelDims 2]] ]


    # Delete last pipeline (naming is a function of $vtk_img, so names
    # generated on successive calls may not be the same).
    # -----
    if {[info exists result(outlineActor)]} {
	if {[cmdExists $result(outlineActor)]} {
	    set ren $result(ren)
	    catch {vis_renRemoveActor $ren $result(outlineActor)}
	    set objs [list $result(conv) $result(outlineSrc) \
		    $result(outlineMap) $result(outlineActor)]
	    foreach o $objs {
		catch {$o Delete}
	    }
	}
    }


    # Build pipeline
    # --------------

    set imgOrigin [$vtk_img GetOrigin]

    set imgMin [list \
	    [expr [lindex $imgOrigin 0] - ([lindex $voxelDims 0] / 2.0)] \
	    [expr [lindex $imgOrigin 1] - ([lindex $voxelDims 1] / 2.0)] \
	    [expr [lindex $imgOrigin 2] - ([lindex $voxelDims 2] / 2.0)] ]

    set physicalDims [list \
	    [expr double([lindex $logicalDims 0]) * [lindex $voxelDims 0]] \
	    [expr double([lindex $logicalDims 1]) * [lindex $voxelDims 1]] \
	    [expr double([lindex $logicalDims 2]) * [lindex $voxelDims 2]]]

    set imgMax [list \
	    [expr [lindex $imgMin 0] + [lindex $physicalDims 0]] \
	    [expr [lindex $imgMin 1] + [lindex $physicalDims 1]] \
	    [expr [lindex $imgMin 2] + [lindex $physicalDims 2]]]


    # Outline
    # -------

    if {[cmdExists $outsrc]} {
	$outsrc Delete
    }
    vtkOutlineSource $outsrc
    $outsrc SetBounds \
	    [lindex $imgMin 0] [lindex $imgMax 0] \
	    [lindex $imgMin 1] [lindex $imgMax 1] \
	    [lindex $imgMin 2] [lindex $imgMax 2]
    $outsrc Update

    if {[cmdExists $outmap]} {
	$outmap Delete
    }
    vtkPolyDataMapper $outmap
    $outmap SetInputConnection [$outsrc GetOutputPort]
    $outmap Update

    if {[cmdExists $outact]} {
	puts foo!
	if {[catch {vis_renRemoveActor $ren $outact} msg]} {
	    return -code error $msg
	}
	$outact Delete
    }
    vtkActor $outact
    $outact SetMapper $outmap
    [$outact GetProperty] SetColor 0 0 1
    #$outact Update
    vis_renAddActor $ren $outact


    set result(winlevIn)     $vtk_img
    set result(winlev)       {}
    set result(conv)         $conv
    set result(outlineSrc)   $outsrc
    set result(outlineMap)   $outmap
    set result(outlineActor) $outact
    set result(ren)          $ren

    set result(voxelDims)    $voxelDims
    set result(logicalDims)  $logicalDims
    set result(physicalDims) $physicalDims
    set result(imgMin)       $imgMin
    set result(imgMax)       $imgMax
    set result(totImgMin)    $totImgMin
    set result(totImgMax)    $totImgMax
    set result(scalarRange)  [$vtk_img GetScalarRange]

    return
}


# ---------------------
# vis_img_VolImgBrowser
# ---------------------
# The image READER is not referenced anywhere below this point.  The
# cast object $conv serves as the point of abstraction in the pipeline
# between upstream objects (i.e. reader, window-level filter, etc.)
# and downstream ones (e.g. plane slicers, etc.).

# --------------------
# vis_img_VolImgBrwsr2
# --------------------

proc vis_img_VolImgBrwsr2 {vol basename} {

    set imgobj [repos_exportToVtk -src $vol]
    set ldims [$imgobj GetDimensions]
    set vdims [$imgobj GetSpacing]
    set rng [$imgobj GetScalarRange]

    set pdims [list \
	    [expr double([lindex $ldims 0]) * [lindex $vdims 0]] \
	    [expr double([lindex $ldims 1]) * [lindex $vdims 1]] \
	    [expr double([lindex $ldims 2]) * [lindex $vdims 2]]]

    # the calculations below assume the origin of the grid is
    # specified, NOT the centroid which is returned from
    # the structured points vtk object.  We there shift the
    # origin by half a pixel.
    set imgOrigin [$imgobj GetOrigin]
    set imgorig [list  [expr [lindex $imgOrigin 0] - ([lindex $vdims 0] / 2.0)]  [expr [lindex $imgOrigin 1] - ([lindex $vdims 1] / 2.0)]  [expr [lindex $imgOrigin 2] - ([lindex $vdims 2] / 2.0)] ]

    global gRen3d
    set ren       $gRen3d

    # ------------
    # Vtk pipeline
    # ------------

    set voix  $basename\_voix
    set voiy  $basename\_voiy
    set voiz  $basename\_voiz

    set txtx  $basename\_texturex
    set txty  $basename\_texturey
    set txtz  $basename\_texturez

    set tx    $basename\_transformx
    set ty    $basename\_transformy
    set tz    $basename\_transformz

    set px    $basename\_planex
    set py    $basename\_planey
    set pz    $basename\_planez

    set tfx   $basename\_tpdfx
    set tfy   $basename\_tpdfy
    set tfz   $basename\_tpdfz

    set tmapx $basename\_tmapx
    set tmapy $basename\_tmapy
    set tmapz $basename\_tmapz

    set castx $basename\_castx
    set casty $basename\_casty
    set castz $basename\_castz

    set mx    $basename\_mapperx
    set my    $basename\_mappery
    set mz    $basename\_mapperz

    set img(mapperx) $mx
    set img(mappery) $my
    set img(mapperz) $mz

    set ax    $basename\_actorx
    set ay    $basename\_actory
    set az    $basename\_actorz

    # Restrict volume to planes along each axis
    # -----------------------------------------
    if {![cmdExists $voix]} {
	vtkExtractVOI $voix
    }
    $voix SetInputDataObject $imgobj
    $voix SetVOI 0 0 \
	    0 [expr [lindex $ldims 1] - 1] \
	    0 [expr [lindex $ldims 2] - 1]
    $voix Update

    if {![cmdExists $voiy]} {
	vtkExtractVOI $voiy
    }
    $voiy SetInputDataObject $imgobj
    $voiy SetVOI 0 [expr [lindex $ldims 0] - 1] \
	    0 0 \
	    0 [expr [lindex $ldims 2] - 1]
    $voiy Update

    if {![cmdExists $voiz]} {
	vtkExtractVOI $voiz
    }
    $voiz SetInputDataObject $imgobj
    $voiz SetVOI 0 [expr [lindex $ldims 0] - 1] \
	    0 [expr [lindex $ldims 1] - 1] \
	    0 0
    $voiz Update

    # Textures
    # --------
    if {![cmdExists $txtx]} {
	vtkTexture $txtx
    }
    $txtx SetInputConnection [$voix GetOutputPort]
    $txtx SetLookupTable g8bitGrayscaleLUT
    $txtx Render $ren
    $txtx RepeatOff

    if {![cmdExists $txty]} {
	vtkTexture $txty
    }
    $txty SetInputConnection [$voiy GetOutputPort]
    $txty SetLookupTable g8bitGrayscaleLUT
    $txty Render $ren
    $txty RepeatOff

    if {![cmdExists $txtz]} {
	vtkTexture $txtz
    }
    $txtz SetInputConnection [$voiz GetOutputPort]
    $txtz SetLookupTable g8bitGrayscaleLUT
    $txtz Render $ren
    $txtz RepeatOff

    # Planes
    # ------
    if {![cmdExists $px]} {
	vtkPlaneSource $px
    }
    $px SetResolution 1 1
    $px SetNormal 1 0 0

    if {![cmdExists $py]} {
	vtkPlaneSource $py
    }
    $py SetResolution 1 1
    $py SetNormal 0 1 0

    if {![cmdExists $pz]} {
	vtkPlaneSource $pz
    }
    $pz SetResolution 1 1
    $pz SetNormal 0 0 1


    # Transforms
    # ----------
    if {![cmdExists $tx]} {
	vtkTransform $tx
    }
    $tx Identity
    $tx Translate \
	    [expr (0 + 0.5) * [lindex $vdims 0] \
	    + [lindex $imgorig 0]] \
	    [expr [lindex $ldims 1] * [lindex $vdims 1] / 2.0 \
	    + [lindex $imgorig 1]] \
	    [expr [lindex $ldims 2] * [lindex $vdims 2] / 2.0 \
	    + [lindex $imgorig 2]]
    $tx Scale 1.0 \
	    [expr [lindex $ldims 1] * [lindex $vdims 1]] \
	    [expr [lindex $ldims 2] * [lindex $vdims 2]]
    $tx Update

    if {![cmdExists $ty]} {
	vtkTransform $ty
    }
    $ty Identity
    $ty Translate \
	    [expr [lindex $ldims 0] * [lindex $vdims 0] / 2.0 \
	    + [lindex $imgorig 0]] \
	    [expr (0 + 0.5) * [lindex $vdims 1] \
	    + [lindex $imgorig 1]] \
	    [expr [lindex $ldims 2] * [lindex $vdims 2] / 2.0 \
	    + [lindex $imgorig 2]]
    $ty Scale [expr [lindex $ldims 0] * [lindex $vdims 0]] \
	    1.0 \
	    [expr [lindex $ldims 2] * [lindex $vdims 2]]
    $ty Update

    if {![cmdExists $tz]} {
	vtkTransform $tz
    }
    $tz Identity
    $tz Translate \
	    [expr [lindex $ldims 0] * [lindex $vdims 0] / 2.0 \
	    + [lindex $imgorig 0]] \
	    [expr [lindex $ldims 1] * [lindex $vdims 1] / 2.0 \
	    + [lindex $imgorig 1]] \
	    [expr (0 + 0.5) * [lindex $vdims 2] \
	    + [lindex $imgorig 2]]
    $tz Scale [expr [lindex $ldims 0] * [lindex $vdims 0]] \
	    [expr [lindex $ldims 1] * [lindex $vdims 1]] \
	    1.0
    $tz Update

    # Transform filters
    # -----------------
    if {![cmdExists $tfx]} {
	vtkTransformPolyDataFilter $tfx
    }
    $tfx SetTransform $tx
    $tfx SetInputConnection [$px GetOutputPort]

    if {![cmdExists $tfy]} {
	vtkTransformPolyDataFilter $tfy
    }
    $tfy SetTransform $ty
    $tfy SetInputConnection [$py GetOutputPort]

    if {![cmdExists $tfz]} {
	vtkTransformPolyDataFilter $tfz
    }
    $tfz SetTransform $tz
    $tfz SetInputConnection [$pz GetOutputPort]

    # Create texture coordinates with vtkTextureMapToPlane
    # ----------------------------------------------------
    if {![cmdExists $tmapx]} {
	vtkTextureMapToPlane $tmapx
    }
    $tmapx SetOrigin [lindex $imgorig 0] [lindex $imgorig 1] \
	    [lindex $imgorig 2]
    $tmapx SetPoint1 [lindex $imgorig 0] \
	    [expr [lindex $imgorig 1] + [lindex $pdims 1]] \
	    [lindex $imgorig 2]
    $tmapx SetPoint2 [lindex $imgorig 0] \
	    [lindex $imgorig 1] \
	    [expr [lindex $imgorig 2] + [lindex $pdims 2]]
    $tmapx SetInputConnection [$tfx GetOutputPort]

    if {![cmdExists $tmapy]} {
	vtkTextureMapToPlane $tmapy
    }
    $tmapy SetOrigin [lindex $imgorig 0] [lindex $imgorig 1] \
	    [lindex $imgorig 2]
    $tmapy SetPoint1 [expr [lindex $imgorig 0] + [lindex $pdims 0]] \
	    [lindex $imgorig 1] \
	    [lindex $imgorig 2]
    $tmapy SetPoint2 [lindex $imgorig 0] \
	    [lindex $imgorig 1] \
	    [expr [lindex $imgorig 2] + [lindex $pdims 2]]
    $tmapy SetInputConnection [$tfy GetOutputPort]

    if {![cmdExists $tmapz]} {
	vtkTextureMapToPlane $tmapz
    }
    $tmapz SetOrigin [lindex $imgorig 0] [lindex $imgorig 1] \
	    [lindex $imgorig 2]
    $tmapz SetPoint1 [expr [lindex $imgorig 0] + [lindex $pdims 0]] \
	    [lindex $imgorig 1] \
	    [lindex $imgorig 2]
    $tmapz SetPoint2 [lindex $imgorig 0] \
	    [expr [lindex $imgorig 1] + [lindex $pdims 1]] \
	    [lindex $imgorig 2]
    $tmapz SetInputConnection [$tfz GetOutputPort]

    # Cast to concrete
    # ----------------
    if {![cmdExists $castx]} {
	vtkCastToConcrete $castx
    }
    $castx SetInputConnection [$tmapx GetOutputPort]

    if {![cmdExists $casty]} {
	vtkCastToConcrete $casty
    }
    $casty SetInputConnection [$tmapy GetOutputPort]

    if {![cmdExists $castz]} {
	vtkCastToConcrete $castz
    }
    $castz SetInputConnection [$tmapz GetOutputPort]

    # Mappers
    # -------
    if {![cmdExists $mx]} {
	vtkPolyDataMapper $mx
    }
    $mx SetInputConnection [$castx GetOutputPort]
    $mx SetScalarRange [lindex $rng 0] [lindex $rng 1]

    if {![cmdExists $my]} {
	vtkPolyDataMapper $my
    }
    $my SetInputConnection [$casty GetOutputPort]
    $my SetScalarRange [lindex $rng 0] [lindex $rng 1]

    if {![cmdExists $mz]} {
	vtkPolyDataMapper $mz
    }
    $mz SetInputConnection [$castz GetOutputPort]
    $mz SetScalarRange [lindex $rng 0] [lindex $rng 1]


    # Actors
    # ------
    if {![cmdExists $ax]} {
	vtkActor $ax
    }
    $ax SetMapper $mx
    $ax SetTexture $txtx

    if {![cmdExists $ay]} {
	vtkActor $ay
    }
    $ay SetMapper $my
    $ay SetTexture $txty

    if {![cmdExists $az]} {
	vtkActor $az
    }
    $az SetMapper $mz
    $az SetTexture $txtz

}

#  transform into z=constant plane if boolean=1
#  transform back out if boolean=0
#

proc geom_flatten {normal boolean inpd rotated_norm outpd} {

  if {[repos_exists -obj $inpd] == "0"} {
    puts "ERROR:  Input PolyData $pd doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $inpd] != "PolyData"} {
    puts "ERROR:  Object $pd not of type PolyData."
    return -code error GDSC_ERROR
  }
  if {[repos_exists -obj $outpd] == "1"} {
    puts "ERROR:  Output object $pd already exists."
    return -code error GDSC_ERROR
  }

  upvar $rotated_norm new_norm

  set myTrans tmp-bc_rotateIntoConstZ-myTrans
  set myFilter tmp-bc_rotateIntoConstZ-myFilter
  set tmpPD tmp-pd-geom_flatten
  set tmpPt tmp-pt-geom_fllaten
  set tmpV tmp-v-geom_flatten

  catch {$myTrans Delete}
  catch {$myFilter Delete}
  catch {$tmpPD Delete}
  catch {$tmpPt Delete}
  catch {$tmpV Delete}

  # need to rotate the points into the z=constant plane
  # calculate rotation vector
  # This corresponds to r = a X b
  # where b = z axis <0,0,1>
  # and a = normal

  set theta [expr acos(double([lindex $normal 2]))]

  set rotvector [list [expr -1.0*[lindex $normal 1]] \
                [lindex $normal 0] \
                0.0]

  # if the plane is already in z=const, then magnitude
  # of the cross product of normal and z axis zero
  set magrot [gdscVectorMagnitude $rotvector]

  #puts "\nCalculate the rotation angle and vector to transform the points"
  #puts "into a z=constant plane."
  #puts "  angle: $magrot rotvector: <$rotvector>"

  # tmp object to check the orientation of the resulting flat plane
  vtkPolyData $tmpPD
  $tmpPD Allocate 10 10
  vtkPoints $tmpPt
  $tmpPt Allocate 10 10
  set anypt [[repos_exportToVtk -src $inpd] GetPoint 0]
  $tmpPt InsertNextPoint [lindex $anypt 0] [lindex $anypt 1] [lindex $anypt 2]
  vtkFloatArray $tmpV
  $tmpV SetNumberOfComponents 3
  $tmpV Allocate 10 10
  if {$boolean == 1} {
    $tmpV InsertNextTuple3 [lindex $normal 0] \
                           [lindex $normal 1] \
                           [lindex $normal 2]
  } else {
    $tmpV InsertNextTuple3 [lindex $new_norm 0] \
                           [lindex $new_norm 1] \
                           [lindex $new_norm 2]
  }
  $tmpPD SetPoints $tmpPt
  [$tmpPD GetPointData] SetVectors $tmpV

 if {$magrot > 0.00001} {
    set rotvector [gdscVectorNormalize $rotvector]
    set rtheta [expr double(2.0*3.14159265358979323846-double($theta))]
    # angle must be in degrees for vtk
    set rtheta [expr double($rtheta)*180.0/3.14159265358979323846]
    if {$boolean == 0} {
      set rtheta [expr -1.0*$rtheta]
    }
    #puts "  theta: $rtheta"
    vtkTransform $myTrans
    $myTrans PostMultiply
    $myTrans RotateWXYZ $rtheta [lindex $rotvector 0] \
                                [lindex $rotvector 1] \
                                [lindex $rotvector 2]

    vtkTransformPolyDataFilter $myFilter
    $myFilter SetTransform $myTrans

    # calculate rotated normal
    $myFilter SetInputDataObject $tmpPD
    $myFilter Update
    set new_norm [[[[$myFilter GetOutput] GetPointData] GetVectors] GetTuple3 0]
    set new_norm [math_normalize $new_norm]

    # transform polydata
    $myFilter SetInputDataObject [repos_exportToVtk -src $inpd]
    $myFilter SetTransform $myTrans
    $myFilter Update
    set transformed [$myFilter GetOutput]

  } else {

    # no transformation needed
    set new_norm $normal
    set transformed [repos_exportToVtk -src $inpd]

  }

  repos_importVtkPd -src $transformed -dst $outpd

  #puts "\norg norm: $normal \n new norm: $new_norm\n"

  catch {$myTrans Delete}
  catch {$myFilter Delete}
  catch {$tmpPD Delete}
  catch {$tmpPt Delete}
  catch {$tmpV Delete}

  return GDSC_OK
}


# -------------------
# geom_createRatioMap
# -------------------

proc geom_createRatioMap {inlet_mesh_face radmax result} {

  #@author Nathan Wilson
  #@c  This proc maps the scalars given on an input PolyData
  #@c  object to a destination PolyData object and returns
  #@c  a new PolyData.  This routine is not general and makes
  #@c  numerous assumptions about the nature of each point set.  See
  #@c  notes.
  #@a  velocityMap:  Input PolyData with defined scalar data.
  #@a  inlet_mesh_face:  PolyData onto which to map the scalar data.
  #@a  result:  Name of new repository PolyData object to be
  #@a  result:  created.
  #@r  status
  #@note  This code does a simple mapping of the Womersley analytic
  #@note  profile on to the points of the
  #@note  the output PolyData.  This mapping assumes that the objects
  #@note  are similar in that if you send a ray from the center of each
  #@note  object the relationship between inner (r) and outer (R) radius
  #@note  is given by r_out = r_in * R_out / R_in for any given angle.
  #@note  We also scale the result scalars assuming they represent a
  #@note  a through plane flux so that the input and output PolyData's
  #@note  have the same through plane flux.  This code requires that both
  #@note  PolyData's be in the z=0 plane.  Each should consist of a single
  #@note  region.  Iso-parametric interpolation functions are used to
  #@note  evaluate the scalar values and for calculating through plane
  #@note  flow rate.
  #@note  The absolute value of the ratio is the radius ratio,
  #@note  where positive values denote interior and negative values
  #@note  denote boundary nodes.
  #@note  This code would be much faster if it were rewritten in C.

  if {[repos_exists -obj $inlet_mesh_face] == "0"} {
    puts "ERROR:  Input PolyData $inlet_mesh_face doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $inlet_mesh_face] != "PolyData"} {
    puts "ERROR:  Object $inlet_mesh_face not of type PolyData."
    return -code error GDSC_ERROR
  }
  if {[repos_exists -obj $result] == "1"} {
    puts "ERROR:  Output object $result exists."
    return -code error GDSC_ERROR
  }

  set myFE /tmp/geom_mapScalars/myFE
  set meshFreeEdges /tmp/geom_mapScalars/free_edges
  set myLinFilt /tmp/geom_mapScalars/myLinFilt
  set extrudedMeshWall /tmp/geom_mapScalars/extruded/mesh/wall
  set extrudedSegWall /tmp/geom_mapScalars/extruded/seg/wall
  set segmentation /tmp/geom_mapScalars/segmentation
  set vScalars tmp-geom_mapScalars-scalars
  set vVectors tmp-geom_mapScalars-vectors
  set pointLocator tmp-geom_mapScalars-pointlocator
  set outputObj tmp-geom_mapScalars-outputObj
  set tmpresult tmp-geom_mapScalars-tmpresult

  catch {$myFE Delete}
  catch {repos_delete -obj $meshFreeEdges}
  catch {repos_delete -obj $segmentation}
  catch {$myLinFilt Delete}
  catch {repos_delete -obj $extrudedMeshWall}
  catch {repos_delete -obj $extrudedSegWall}
  catch {repos_delete -obj $tmpresult}
  catch {$outputObj Delete}
  catch {$pointLocator Delete}
  catch {$vVectors Delete}
  catch {$vScalars Delete}

  # extract the free edges from mesh
  puts "Find free edges of inlet mesh face."
  vtkFeatureEdges $myFE
  $myFE SetInputDataObject [repos_exportToVtk -src $inlet_mesh_face]
  $myFE FeatureEdgesOff
  $myFE NonManifoldEdgesOff
  $myFE BoundaryEdgesOn
  $myFE ColoringOff
  $myFE Update
  repos_importVtkPd -src [$myFE GetOutput] -dst $meshFreeEdges

  # need extruded object to do intersections

  # extrude mesh free edges
  vtkLinearExtrusionFilter $myLinFilt
  $myLinFilt SetExtrusionTypeToVectorExtrusion
  $myLinFilt SetInputDataObject [repos_exportToVtk -src $meshFreeEdges]
  $myLinFilt SetVector 0 0 1
  $myLinFilt SetScaleFactor 2
  $myLinFilt Update
  repos_importVtkPd -src [$myLinFilt GetOutput] -dst $extrudedMeshWall

  # create an interior (non-boundary) node list
  puts "Find interior nodes on mesh face (i.e. non-boundary nodes)."
  geom_getSubsetOfPts $inlet_mesh_face $meshFreeEdges 0.001 nodes
  puts "Found [llength $nodes] interior nodes."

  # get the centers of each object
  set ctrMesh [geom_avgPt -obj $meshFreeEdges]
  puts "Center of mesh face: $ctrMesh"

  # create a new set of scalars and vectors
  vtkFloatArray $vScalars
  $vScalars SetNumberOfComponents 1
  $vScalars Allocate 100 100
  vtkFloatArray $vVectors
  $vVectors SetNumberOfComponents 3
  $vVectors Allocate 100 100

  # create point locator
  vtkPointLocator $pointLocator
  $pointLocator SetDataSet [repos_exportToVtk -src $inlet_mesh_face]
  $pointLocator AutomaticOn
  $pointLocator SetTolerance 0.001
  $pointLocator BuildLocator

  # this loop effectively sets only the boundary nodes to have a value of
  # -radmax
  for {set i 0} {$i < [[repos_exportToVtk -src $inlet_mesh_face] GetNumberOfPoints]} {incr i} {
    $vScalars InsertNextTuple1 [expr -1.0*$radmax]
  }

  # now loop over the nodes calculating the velocity for each mesh node

  set counter 0

  foreach node $nodes {

    set r_m_pt [math_subVectors $node $ctrMesh]
    set r_m_pt [list [lindex $r_m_pt 0] [lindex $r_m_pt 1] 0]
    set r_m [math_magnitude $r_m_pt]

    set angleDeg [math_radToDeg [expr atan2(double([lindex $r_m_pt 1]),double([lindex $r_m_pt 0]))]]

    set circle [math_circlePt $angleDeg 150.0]
    set outsidePtMesh [list [expr [lindex $ctrMesh 0]+[lindex $circle 0]] \
                            [expr [lindex $ctrMesh 1]+[lindex $circle 1]] 1]

    # if we can't intersect directly, try and get close
    if [catch {set bdryPtMesh [geom_intersectWithLine -obj $extrudedMeshWall \
                            -pt0 [list [lindex $ctrMesh 0] [lindex $ctrMesh 1] 1] \
				   -pt1 $outsidePtMesh]}] {
      puts "set bdryPtMesh \[geom_intersectWithLine -obj $extrudedMeshWall \
                            -pt0 [list [lindex $ctrMesh 0] [lindex $ctrMesh 1] 1] \
                            -pt1 $outsidePtMesh\]"
      # arbitrarily add a degree and try again and then give up
      set circle [math_circlePt [expr $angleDeg + 1.0] 10.0]
      set outsidePtMesh [list [expr [lindex $ctrMesh 0]+[lindex $circle 0]] \
                              [expr [lindex $ctrMesh 1]+[lindex $circle 1]] 1]
      set bdryPtMesh [geom_intersectWithLine -obj $extrudedMeshWall \
                            -pt0 [list [lindex $ctrMesh 0] [lindex $ctrMesh 1] 1] \
				   -pt1 $outsidePtMesh]
    }

    set bdryPtMesh [list [lindex $bdryPtMesh 0] [lindex $bdryPtMesh 1] 0]

    set R_m_pt [math_subVectors $bdryPtMesh $ctrMesh]
    set R_m_pt [list [lindex $R_m_pt 0] [lindex $R_m_pt 1] 0]
    set R_m [math_magnitude $R_m_pt]

    if {$r_m > $R_m} {
      puts "ERROR:  inside radius ($r_m) exceeds outside radius ($R_m)."
      return -code error "ERROR:  inside radius ($r_m) exceeds outside radius ($R_m)."
    }

    set R_pc $radmax

    set r_pc [expr double($r_m*$R_pc)/double($R_m)]

    if {$r_pc > $R_pc} {
      puts "ERROR:  inside radius ($r_pc) exceeds outside radius ($R_pc)."
      return -code error "ERROR:  inside radius ($r_pc) exceeds outside radius ($R_pc)."
    }

    # debugging graphics
    if {$counter < 0} {
        set r 0.1
	catch {repos_delete -obj line1}
        catch {repos_delete -obj line2}
        catch {repos_delete -obj sCtrMesh}
        catch {repos_delete -obj sCtrPC}
        catch {repos_delete -obj sNode}
        catch {repos_delete -obj sPt}
        catch {repos_delete -obj sBdryMesh}
        catch {repos_delete -obj sBdryPC}
        solid_sphere -r $r -ctr $ctrMesh -result sCtrMesh
        solid_sphere -r $r -ctr $ctrPCMRI -result sCtrPC
        solid_sphere -r $r -ctr $node -result sNode
        solid_sphere -r $r -ctr $pt -result sPt
        solid_sphere -r $r -ctr $bdryPtMesh -result sBdryMesh
        solid_sphere -r $r -ctr $bdryPtSeg -result sBdryPC
        geom_mkLinesFromPts [list $ctrMesh $bdryPtMesh] line1 0
        geom_mkLinesFromPts [list $ctrPCMRI $bdryPtSeg] line2 0
	repos_setLabel -obj line1 -key color -value blue
        repos_setLabel -obj line2 -key color -value red
        repos_setLabel -obj sCtrMesh -key color -value yellow
        repos_setLabel -obj sCtrPC -key color -value yellow
        repos_setLabel -obj sNode -key color -value green
        repos_setLabel -obj sPt -key color -value green
        repos_setLabel -obj sBdryMesh -key color -value white
        repos_setLabel -obj sBdryPC -key color -value white
        catch {repos_setLabel -obj $meshFreeEdges -key color -value blue}
        catch {repos_setLabel -obj $segmentation -key color -value red}
        gdscView sCtrMesh sCtrPC sNode sPt sBdryMesh sBdryPC line1 line2 $segmentation $meshFreeEdges
        incr counter
    }

    # update velocities for mesh
    set ptId [$pointLocator FindClosestPoint [lindex $node 0] [lindex $node 1] [lindex $node 2]]
    $vScalars SetTuple1 $ptId $r_pc

    #puts "r_m: $r_m  R_m: $R_m  r_pc: $r_pc  R_pc: $R_pc  angle: $angleDeg ptId: $ptId"
  }

  # create the output object
  vtkPolyData $outputObj
  $outputObj SetPoints [[repos_exportToVtk -src $inlet_mesh_face] GetPoints]
  $outputObj CopyStructure [repos_exportToVtk -src $inlet_mesh_face]
  [$outputObj GetPointData] SetScalars $vScalars
  catch {repos_delete -obj $result}
  repos_importVtkPd -src $outputObj -dst $result

  # clean up
  catch {$myFE Delete}
  catch {repos_delete -obj $meshFreeEdges}
  catch {repos_delete -obj $segmentation}
  catch {$myLinFilt Delete}
  catch {repos_delete -obj $extrudedMeshWall}
  catch {repos_delete -obj $extrudedSegWall}
  catch {repos_delete -obj $tmpresult}
  catch {$outputObj Delete}
  catch {$pointLocator Delete}
  catch {$vVectors Delete}
  catch {$vScalars Delete}

  return GDSC_OK

}


# -----------------------
# geom_createWomersleyMap
# -----------------------

proc geom_mapWomersleyMap {terms time viscosity omega density radmax flow_rate inlet_mesh_face radiusMap outwardUnitNormal result} {

  if {[repos_exists -obj $inlet_mesh_face] == "0"} {
    puts "ERROR:  Input PolyData $inlet_mesh_face doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $inlet_mesh_face] != "PolyData"} {
    puts "ERROR:  Object $inlet_mesh_face not of type PolyData."
    return -code error GDSC_ERROR
  }
  if {[repos_exists -obj $radiusMap] == "0"} {
    puts "ERROR:  Input radius map PolyData $radiusMap doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $radiusMap] != "PolyData"} {
    puts "ERROR:  Object $radiusMap not of type PolyData."
    return -code error GDSC_ERROR
  }
  if {[repos_exists -obj $result] == "1"} {
    puts "ERROR:  Output object $result exists."
    return -code error GDSC_ERROR
  }

  set vScalars tmp-geom_mapScalars-scalars
  set vVectors tmp-geom_mapScalars-vectors
  set pointLocator tmp-geom_mapScalars-pointlocator
  set outputObj tmp-geom_mapScalars-outputObj
  set tmpresult tmp-geom_mapScalars-tmpresult

  catch {repos_delete -obj $tmpresult}
  catch {$outputObj Delete}
  catch {$vVectors Delete}
  catch {$vScalars Delete}

  set r_pc_map [[[repos_exportToVtk -src $radiusMap] GetPointData] GetScalars]

  # create a new set of scalars and vectors
  vtkFloatArray $vScalars
  $vScalars SetNumberOfComponents 1
  $vScalars Allocate 100 100
  vtkFloatArray $vVectors
  $vVectors SetNumberOfComponents 3
  $vVectors Allocate 100 100

  # inserting a through plane component of zero for all points
  set numNodes [[repos_exportToVtk -src $inlet_mesh_face] GetNumberOfPoints]

  $vScalars SetNumberOfTuples $numNodes
  $vVectors SetNumberOfTuples $numNodes

  global guiABC
  set type_of_profile $guiABC(type_of_profile)

  for {set i 0} {$i < $numNodes} {incr i} {

    set R_pc $radmax

    set r_pc [$r_pc_map GetTuple1 $i]
    #puts "$r_pc $R_pc"

    # a negative value indicates we're on the boundary
    if {$r_pc < 0} {
      #puts "on edge ($r_pc) ($R_pc)."
      $vScalars SetTuple1 $i 0
      $vVectors SetTuple3 $i 0 0 0
      continue
    }

    set r_pc [expr abs($r_pc)]

    if {$r_pc > $R_pc} {
      #puts "ERROR:  inside radius ($r_pc) exceeds outside radius ($R_pc)."
      $vScalars SetTuple1 $i 0
      $vVectors SetTuple3 $i 0 0 0
      #return -code error "ERROR:  inside radius ($r_pc) exceeds outside radius ($R_pc)."
    } else {
      if {$type_of_profile == "womersley"} {
        set vel [math_computeWomersley -terms $terms -time $time \
             -viscosity $viscosity -omega $omega -density $density \
             -radmax $radmax -radius $r_pc]
      } elseif {$type_of_profile == "plug"} {
        set vel 1.0
      } elseif {$type_of_profile == "parabolic"} {
        set vel [expr 1.0-double($r_pc*$r_pc)/double($radmax*$radmax)]
      } else {
        return -code error "ERROR: invalid type_of_profile ($type_of_profile)"
      }
      $vScalars SetTuple1 $i $vel
      set directedVel [math_scaleVec $outwardUnitNormal $vel]
      $vVectors SetTuple3 $i [lindex $directedVel 0] [lindex $directedVel 1] \
                             [lindex $directedVel 2]
    }
  }

  # create the output object
  vtkPolyData $outputObj
  $outputObj SetPoints [[repos_exportToVtk -src $inlet_mesh_face] GetPoints]
  $outputObj CopyStructure [repos_exportToVtk -src $inlet_mesh_face]
  [$outputObj GetPointData] SetScalars $vScalars
  [$outputObj GetPointData] SetVectors $vVectors
  catch {repos_delete -obj $result}
  repos_importVtkPd -src $outputObj -dst $result

  # calculate the flow rates for the original and mapped mesh
  set orgFlow $flow_rate
  geom_copy -src $result -dst $tmpresult
  set newFlow [geom_integrateSurfaceFlux -obj $tmpresult -nrm $outwardUnitNormal -tensorType 1]
  if {$guiABC(preserve_flow_by_scaling) == 1} {
    set scaleFactor [expr $orgFlow/$newFlow]
  } else {
    set scaleFactor 1.0
  }
  puts "orgFlow: $orgFlow  newFlow: $newFlow  ratio: $scaleFactor"
  catch {repos_delete -obj $result}
  geom_scaleVectorComponents $tmpresult $scaleFactor $result
  puts "scaled flow rate: [geom_integrateSurfaceFlux -obj $result -nrm $outwardUnitNormal -tensorType 1]"

  # clean up
  catch {repos_delete -obj $tmpresult}
  catch {$outputObj Delete}
  catch {$vScalars Delete}
  catch {$vVectors Delete}

  return GDSC_OK

}
# ---------
# cmdExists
# ---------

proc cmdExists {cmd} {
    #@author Ken Wang
    #@c Check to see if the command exists.
    #@a cmd:  command name.
    #@r 1 if true, 0 if false.
    set cmds [info commands]
    foreach c $cmds {
	if {[string compare $c $cmd] == 0} {
	    return 1
	}
    }
    return 0
}

# --------------
# FindSetMethods
# --------------

proc FindSetMethods {obj} {
    # @author Ken Wang
    # @c Find the set methods for a given vtk object.
    # @a obj: vtk object name
    # @r list of set methods
    set result {}
    set methods [split [$obj ListMethods] "\n"]
    foreach m $methods {
	set toks [cleanList [split $m]]
	set name [string trim [lindex $toks 0]]
	if {[string first ":" $name] >= 0} {
	    continue
	}
	if {[string first "Set" $name] == 0} {
	    lappend result $name
	}
    }
    return $result
}


# --------------
# FindGetMethods
# --------------

proc FindGetMethods {obj} {
    # @author Ken Wang
    # @c Find the get methods for a given vtk object.
    # @a obj: vtk object name
    # @r list of get methods
    set result {}
    set methods [split [$obj ListMethods] "\n"]
    foreach m $methods {
	set toks [cleanList [split $m]]
	set name [string trim [lindex $toks 0]]
	if {[string first ":" $name] >= 0} {
	    continue
	}
	if {[string first "Get" $name] == 0} {
	    lappend result $name
	}
    }
    return $result
}


proc printBuildID {} {
  global SIMVASCULAR_BUILD_ID
  puts "Build Identifier: $SIMVASCULAR_BUILD_ID"
}

proc printExternals {} {
  set var gExternalPrograms
  global $var
  foreach i [array names $var] {
    puts "$i: [subst $[subst $var]($i)]"
  }
}

proc printExternals {} {
  set var gExternalPrograms
  global $var
  foreach i [array names $var] {
    set p [subst $[subst $var]($i)]
    puts "$i: [subst $[subst $var]($i)]"
    set pe [file exists $p]
    puts "$i exists: $pe"
    puts "\n"

  }
}


proc gui_sv_surface_window {{cmd 0}} {
  global symbolicName
  set tv $symbolicName(guiSV_group_tree)
  set x [winfo rootx $tv]
  set y [winfo rooty $tv]
  set selected [guiSV_group_get_tree_current_groups_selected]
  if {$selected != "" || $cmd == "new" || $cmd == "0"} {
    ShowWindow.svGroupWindow $cmd
    move_window .svGroupWindow $x $y
    return
  }

  set selected [guiSV_group_get_tree_current_surfaces_selected]
  if {$selected != ""} {
    ShowWindow.svSaveSegWindow $cmd
    move_window .svSaveSegWindow  $x $y
    return
  }
}

proc center_window {w} {
  wm withdraw $w
  update idletasks
  set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
  - [winfo vrootx [winfo parent $w]]]
  set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
  - [winfo vrooty [winfo parent $w]]]
  wm geom $w +$x+$y
  wm deiconify $w
}
proc move_window_mouse {w {xoffset 0 } {yoffset 0} } {
  set curWindow [lindex [wm stackorder .] end]
  wm withdraw $w
  update idletasks
  # Everything below will work with anything from Tk 8.0 onwards
  set x [expr {[winfo pointerx .] - [winfo rootx $curWindow]-$xoffset}]
  set y [expr {[winfo pointery .] - [winfo rooty $curWindow]-$yoffset}]
  puts "the mouse is at ($x,$y) in window $curWindow"
  wm geom $w +$x+$y
  wm deiconify $w
}
proc move_window {w {x 0 } {y 0} } {
  set curWindow [lindex [wm stackorder .] end]
  wm geom $w +$x+$y
  wm deiconify $w
}

proc shrink_mainpane {} {
  .guiCV.tframe3.tpanedwindow4.tframe6.tpanedwindow0 sashpos 0 300
}

