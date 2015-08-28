#===========================================================================
#    
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical
# Software Corporation, University of California, San Diego.
#
# Copyright (c) 1998-2007 Stanford University,
# Charles Taylor, Nathan Wilson, Ken Wang.
#
# See SimVascular Acknowledgements file for additional
# contributors to the source code. 
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

proc bctdat_combine {inFiles outFile} {
  #author Nathan Wilson
  #@c This procedure creates a new bct.dat file by appending
  #@c all input bct.dat files.  In addition, each line is terminated
  #@c with only a linefeed.  Any carriage return / linefeed
  #@c sequence at the end of an input line of a text will be
  #@c replaced with just a linefeed (unix-style text file).
  #@c WARNING:  This code does no sanity checks of the input
  #@c WARNING:  files whatsoever.
  #@a inFiles:  List of input text files.
  #@a outFile:  Output text file.
  #@r status

    foreach inFile $inFiles {
      if {[file exists $inFile] == 0} {
        puts "ERROR: Input file $inFile does not exist."
        return -code error "ERROR: Input file $inFile does not exist."
      }
    }

    if {[file exists $outFile] == 1} {
      puts "ERROR: Output file $outFile exists."
      return -code error "ERROR: Output file $outFile exists."
    }

    puts ""
    # read the headers to combine the total number of vectors
    set totalnumnodes 0
    set numPtsInPeriod {}
    foreach inFile $inFiles {
      set infp [open "$inFile" "r"]
      gets $infp line
      set numNodesInFile [lindex $line 0]
      puts "file $inFile apparently contains $numNodesInFile with [lindex $line 1] pts per node."
      close $infp
      set totalnumnodes [expr $totalnumnodes + $numNodesInFile]
      if {$numPtsInPeriod == ""} {
           set numPtsInPeriod [lindex $line 1]
	} else {
	   if {[lindex $line 1] != $numPtsInPeriod} {
              puts "\nWARNING!!  the number of pts in period is different between files ($numPtsInPeriod != [lindex $line 1]).  Is this a problem??\n"
	   }
      }
    }

    puts "\n\nA combined bct.dat formatted file will be created with a total of $totalnumnodes and $numPtsInPeriod time points per node.\n"

    set outfp [open "$outFile" "w"]
    fconfigure $outfp -translation lf

    # output header
    puts $outfp "$totalnumnodes $numPtsInPeriod"

    puts ""

    foreach inFile $inFiles {
      set infp [open "$inFile" "r"]
      # skip header
      gets $infp line
      puts "skipping first line ($line) in file $inFile."
      while {[gets $infp line] >= 0} {
        puts $outfp $line
      }
      close $infp
    }
    close $outfp
    return GDSC_OK
}


proc bctdat_checkFile {bctdat_file} {

  #@author Nathan Wilson
  #@c Verifies the header by actually counting the number
  #@c of nodal definitions found in the file.
  #@a bctdat_file
  #@r Causes an error if the number of nodes in the file does
  #@r not match the number specified in the header.

  if {[file exists $bctdat_file] == 0} {
     return -code error "ERROR:  file \"$bctdat_file\" does not exist."
  }

  set fp [open $bctdat_file "r"]

  # read the first line to find out how many values we have for each
  # node

  gets $fp line

  scan $line "%i %i" num_bctdat_nodes num_bctdat_values

  set numNodes 0
  # read in the data from the bct.dat file
  while {[gets $fp line] >= 0} {
    if {[scan $line "%f %f %f %i" x y z np] != 4} {
      close $fp
      return -code error "Error in line: $line"
    }
    puts "$numNodes nodes found so far"
    for {set i 0} {$i < $np} {incr i} {
      gets $fp line
      if {[scan $line "%f %f %f %f" vx vy vz t] != 4} {
        close $fp
        return -code error "ERROR in line: $line"
      }
    }
    incr numNodes
  }

  puts "  Number of Nodes: $numNodes"
  close $fp

  return GDSC_OK

}


proc bctdat_getNumNodes {bctdat_file} {

  #@author Nathan Wilson
  #@c Return the number of nodes in a phasta bct.dat file.

  if {[file exists $bctdat_file] == 0} {
     return -code error "ERROR:  file \"$bctdat_file\" does not exist."
  }

  set fp [open $bctdat_file "r"]

  # read the first line to find out how many values we have for each
  # node

  if {[gets $fp line] <= 0} {
     close $fp
     return -code error "ERROR:  Could not read line."
  }

  close $fp

  scan $line "%i %i" num_bctdat_nodes num_bctdat_values

  return $num_bctdat_nodes

}


# --------
# findCmds
# --------

proc findCmds {substr} {
    #@author Ken Wang
    #@c find commands starting with substr.
    #@a substr:  String to match.
    #@r List of commands starting with substr.
    set cmds [info commands]
    set result {}
    foreach c $cmds {
	if {[string first $substr $c] >= 0} {
	    lappend result $c
	}
    }
    return $result
}


# --------
# showCmds
# --------

proc showCmds {substr} {
    #@author Ken Wang
    #@c Print commands starting with substr to stdout (1 per line).
    #@a substr: String to match.
    #@note This is a thin wrapper around <p findCmds>
    #@note which takes the returned list and outputs it.
    set result [findCmds $substr]
    foreach r $result {
	puts $r
    }
}


# -----
# pause
# -----

proc pause {} {
    #@author Ken Wang
    #@c Wait for the user to hit a key.
    #@r 1 if a key was hit, 0 otherwise.
    gets stdin line
    if {[llength $line] > 0} {
	return 1
    } else {
	return 0
    }
}


# ----
# quit
# ----
# Convenience.

proc quit {} {
    #@author Ken Wang
    #@c Alias for "exit."
    exit
}

# -------------
# file_dos2unix
# -------------

proc file_dos2unix {inFile outFile} {
  #author Nathan Wilson
  #@c This procedure removes the carriage return / linefeed
  #@c sequence from the end of each line of a text in a text
  #@c file and replaces it with just a linefeed (unix-style
  #@c text file).
  #@a inFile:  Input text file.
  #@a outFile:  Output text file.
  #@r status

    if {[file exists $inFile] == 0} {
      puts "ERROR: Input file $inFile does not exist."
      return -code error "ERROR: Input file $inFile does not exist."
    }

    if {[file exists $outFile] == 1} {
      puts "ERROR: Output file $outFile exists."
      return -code error "ERROR: Output file $outFile exists."
    }

    set infp [open "$inFile" "r"]
    set outfp [open "$outFile" "w"]
    fconfigure $outfp -translation lf
    while {[gets $infp line] >= 0} {
        puts $outfp $line
    }
    close $infp
    close $outfp
    return GDSC_OK
}


# --------
# file_cat
# --------

proc file_cat {inFiles outFile} {
  #author Nathan Wilson
  #@c This procedure creates a new output file by appending
  #@c all of the input files.  In addition, each line is terminated
  #@c with only a linefeed.  Any carriage return / linefeed
  #@c sequence at the end of an input line of a text will be
  #@c replaced with just a linefeed (unix-style text file).
  #@a inFiles:  List of input text files.
  #@a outFile:  Output text file.
  #@r status

    foreach inFile $inFiles {
      if {[file exists $inFile] == 0} {
        puts "ERROR: Input file $inFile does not exist."
        return -code error "ERROR: Input file $inFile does not exist."
      }
    }

    if {[file exists $outFile] == 1} {
      puts "ERROR: Output file $outFile exists."
      return -code error "ERROR: Output file $outFile exists."
    }

    set outfp [open "$outFile" "w"]
    fconfigure $outfp -translation lf

    foreach inFile $inFiles {
      set infp [open "$inFile" "r"]
      while {[gets $infp line] >= 0} {
        puts $outfp $line
      }
      close $infp
    }
    close $outfp
    return GDSC_OK
}


proc file_append {inFiles outFile} {

  #author Nathan Wilson
  #@c This procedure creates a new output file by appending
  #@c all of the input files.
  #@a inFiles:  List of input text files.
  #@a outFile:  Output text file.
  #@r status

    foreach inFile $inFiles {
      if {[file exists $inFile] == 0} {
        puts "ERROR: Input file $inFile does not exist."
        return -code error "ERROR: Input file $inFile does not exist."
      }
    }

    if {[file exists $outFile] == 1} {
      puts "ERROR: Output file $outFile exists."
      return -code error "ERROR: Output file $outFile exists."
    }

    global tcl_platform
    if {$tcl_platform(platform) == "windows"} {
	set cmdstr "copy /Y /B [file nativename [lindex $inFiles 0]] "
        foreach inFile [lrange $inFiles 1 end] {
	    set cmdstr "$cmdstr /B +[file nativename $inFile] "
	}
        set cmdstr "$cmdstr [file nativename $outFile] /B"
        puts "$cmdstr"
        exec cmd /c $cmdstr
    } else {

      foreach inFile $inFiles {
        exec cat $inFile >> $outFile
      }

    }

    return GDSC_OK
}


proc file_find {dir wildcard args} {
  #author Nathan Wilson
  #@c This procedure recursively searches for
  #@c pattern and returns a tcl list of the files
  #@c matching pattern.
  #@a dir:  starting directory
  #@a wildcard: pattern to match
  #@a args:  optional return variable
  #@r status

  if {[llength $args] == 0} {
     set rtnme {}
  } else {
     upvar rtnme rtnme
  }

  foreach j $dir {
    set files [glob -nocomplain [file join $j $wildcard]]
    # print out headers
    foreach i $files {
      #puts "found file: $i"
      lappend rtnme $i
    }

    set files [glob -nocomplain [file join $j *]]
    foreach i $files {
      if {[file isdirectory $i] == 1} {
        file_find $i $wildcard 1
      }
    }
  }
  return [lsort -unique $rtnme]

}


# ---------------------
# geom_pdFromOrderedPts
# ---------------------
# Makes a closed polygon.

proc geom_pdFromOrderedPts {ptList dstName} {
    #@author Ken Wang
    #@c Makes a closed polygon.
    #@a ptList: List of points.
    #@a dstName:  destination PolyData name.
    #@r Returns a Tcl error if there is a problem.
    #@note Thin wrapper proc that calls <p geom_mkLinesFromPts> with closed=1.
    set closed 1
    return [geom_mkLinesFromPts $ptList $dstName $closed]
}


# ---------------------
# geom_openLinesFromPts
# ---------------------
# Makes an open set of lines.

proc geom_openLinesFromPts {ptList dstName} {
    #@author Ken Wang
    #@c Makes an open set of lines.
    #@a ptList: List of points.
    #@a dstName: destination PolyData name.
    #@r Returns a Tcl error if there is a problem.
    #@note Thin wrapper proc that calls <p geom_mkLinesFromPts> with closed=0.
    set closed 0
    return [geom_mkLinesFromPts $ptList $dstName $closed]
}


# -------------------
# geom_mkLinesFromPts
# -------------------

proc geom_mkLinesFromPts {ptList dstName closed} {
    #@author Ken Wang
    #@c Makes a repository PolyData object consisting of a set of lines
    #@c defined by the given points.
    #@a ptList: List of points.
    #@a dstName: destination PolyData name.
    #@a closed: 0=open ,  1=closed
    #@r Returns a Tcl error if there is a problem.
    set p /tmp/mkLinesFromPts/reposPd
    catch {repos_delete -obj $p}

    if {[repos_exists -obj $dstName]} {
	widget_ErrorDialog "object $dstName already exists"
	return -code error
    }
    set numPts [llength $ptList]
    if {$numPts < 2} {
	widget_ErrorDialog "geom_mkLinesFromPts requires >= 2 pts"
	return -code error
    }
    if {[cmdExists pts]} {
	pts Delete
    }
    vtkPoints pts
    for {set i 0} {$i < $numPts} {incr i} {
	set pt [lindex $ptList $i]
	set x [lindex $pt 0]
	set y [lindex $pt 1]
	set z [lindex $pt 2]
	pts InsertNextPoint $x $y $z
    }
    if {[cmdExists lines]} {
	lines Delete
    }
    vtkCellArray lines
    lines InitTraversal
    for {set i 0} {$i < $numPts} {incr i} {
	lines InsertNextCell 2
	lines InsertCellPoint $i
	lines InsertCellPoint [expr ($i + 1) % $numPts]
	if {$i == [expr $numPts - 2]} {
	    if {!$closed} {
		break
	    }
	}
    }
    if {[cmdExists pd]} {
	pd Delete
    }
    vtkPolyData pd
    pd SetPoints pts
    pd SetLines lines
    repos_importVtkPd -src pd -dst $p
    geom_copy -src $p -dst $dstName
}


# ------------
# geom_mkPtsPd
# ------------

proc geom_mkPtsPd {ptList dstName} {

    #@author Ken Wang
    #@c Makes a repository PolyData object consisting of the set of
    #@c given points.  The reason this is useful is because there are
    #@c considerations related to vtk which should be hidden from the
    #@c caller (i.e. that points define geometry, while vertices are
    #@c the topological entity which can actually be rendered).  The
    #@c objects created by this proc can be used in conjunction with the
    #@c vis_node* procs to visualize nodes.
    #@a ptList: List of points.
    #@a dstName: destination PolyData name.
    #@r Returns a Tcl error if there is a problem.

    set p /tmp/mkPtsPd/reposPd
    catch {repos_delete -obj $p}

    if {[repos_exists -obj $dstName]} {
	widget_ErrorDialog "object $dstName already exists"
	return -code error
    }
    set numPts [llength $ptList]
    if {[cmdExists __pts]} {
	__pts Delete
    }
    vtkPoints __pts
    for {set i 0} {$i < $numPts} {incr i} {
	set pt [lindex $ptList $i]
	set x [lindex $pt 0]
	set y [lindex $pt 1]
	set z [lindex $pt 2]
	__pts InsertNextPoint $x $y $z
    }
    if {[cmdExists __verts]} {
	__verts Delete
    }
    vtkCellArray __verts
    __verts InitTraversal
    for {set i 0} {$i < $numPts} {incr i} {
	__verts InsertNextCell 1
	__verts InsertCellPoint $i
    }
    if {[cmdExists __pd]} {
	__pd Delete
    }
    vtkPolyData __pd
    __pd SetPoints __pts
    __pd SetVerts __verts
    repos_importVtkPd -src __pd -dst $p
    geom_copy -src $p -dst $dstName
}


# --------------------
# geom_getFreeEdges
# --------------------

proc geom_getFreeEdges {srcPd dstName} {

    #@author Ken Wang
    #@c Uses vtkFeatureEdges to extract the boundary edges of the given
    #@c PolyData object.  Returns a newly-constructed PolyData result.
    #@a srcPd: Input PolyData object.
    #@a dstName: Destination PolyData name.
    #@r Returns a Tcl error if there is a problem.

    set p /tmp/getFeatureEdges/reposPd
    catch {repos_delete -obj $p}

    if {[repos_type -obj $srcPd] != "PolyData"} {
	widget_ErrorDialog "object $srcPd not of type PolyData"
	return -code error
    }
    if {[repos_exists -obj $dstName]} {
	widget_ErrorDialog "object $dstName already exists"
	return -code error
    }

    if {[cmdExists __edgeFilter]} {
	__edgeFilter Delete
    }
    vtkFeatureEdges __edgeFilter
    __edgeFilter BoundaryEdgesOn
    __edgeFilter FeatureEdgesOff
    __edgeFilter ManifoldEdgesOff
    __edgeFilter NonManifoldEdgesOff
    __edgeFilter SetInputDataObject [repos_exportToVtk -src $srcPd]
    __edgeFilter Update

    repos_importVtkPd -src [__edgeFilter GetOutput] -dst $p
    geom_copy -src $p -dst $dstName
}


# --------------------
# geom_getFeatureEdges
# --------------------

proc geom_getFeatureEdges {srcPd dstName} {

    #@author Ken Wang
    #@c Uses vtkFeatureEdges to extract the boundary edges of the given
    #@c PolyData object.  Returns a newly-constructed PolyData result.
    #@a srcPd: Input PolyData object.
    #@a dstName: Destination PolyData name.
    #@r Returns a Tcl error if there is a problem.

    set p /tmp/getFeatureEdges/reposPd
    catch {repos_delete -obj $p}

    if {[repos_type -obj $srcPd] != "PolyData"} {
	widget_ErrorDialog "object $srcPd not of type PolyData"
	return -code error
    }
    if {[repos_exists -obj $dstName]} {
	widget_ErrorDialog "object $dstName already exists"
	return -code error
    }

    if {[cmdExists __edgeFilter]} {
	__edgeFilter Delete
    }
    vtkFeatureEdges __edgeFilter
    __edgeFilter BoundaryEdgesOff
    __edgeFilter FeatureEdgesOn
    __edgeFilter ManifoldEdgesOff
    __edgeFilter NonManifoldEdgesOff
    __edgeFilter SetInputDataObject [repos_exportToVtk -src $srcPd]
    __edgeFilter Update

    repos_importVtkPd -src [__edgeFilter GetOutput] -dst $p
    geom_copy -src $p -dst $dstName
}


# -----------
# geom_circle
# -----------
# Make a circular profile with the given radius.  The profile will be
# in the xy plane centered at the origin.

proc geom_circle {r x0 y0 dstName} {
    #@author Ken Wang
    #@c Make a circular profile with the given radius.  The profile will be
    #@c in the xy plane.
    #@a r: radius
    #@a x0: x-center of circle.
    #@a y0: y-center of circle.
    #@a dstName: Desired repository PolyData name.

    if {[repos_exists -obj $dstName]} {
	widget_ErrorDialog "object $dstName already exists"
	return -code error
    }

    set ptList {}
    for {set i 0} {$i < 360} {incr i 10} {
	set x [expr $r * cos( [math_degToRad $i] ) + $x0]
	set y [expr $r * sin( [math_degToRad $i] ) + $y0]
	set ptList [lappend ptList [list $x $y 0]]
    }
    geom_pdFromOrderedPts $ptList $dstName
}


# ------------
# geom_ellipse
# ------------

proc geom_ellipse {a b x0 y0 dstName} {
    #@author Nathan Wilson
    #@c Make an ellipse.  The profile will be in the xy plane.
    #@c Major axis is horizontal.
    #@a a: a.
    #@a b: b.
    #@a x0: x-center of ellipse.
    #@a y0: y-center of ellipse.
    #@a dstName: Desired repository PolyData name.

    if {[repos_exists -obj $dstName]} {
	widget_ErrorDialog "object $dstName already exists"
	return -code error
    }

    set ptList {}
    for {set i 0} {$i < 360} {incr i 10} {
	set x [expr $a * cos( [math_degToRad $i] ) + $x0]
	set y [expr $b * sin( [math_degToRad $i] ) + $y0]
	set ptList [lappend ptList [list $x $y 0]]
    }
    geom_pdFromOrderedPts $ptList $dstName
}


#--------------------
# geom_polygonFromPts
#--------------------

proc geom_polygonFromPts {pts dstName} {

  #@author Nathan Wilson
  #@c Creates a single vtk polygon from points.
  #@a pts: Order set of points.
  #@a dstName:  PolyData result.
  #@r status

  set VTK_POLYGON 7

  if {[repos_exists -obj $dstName] == 1} {
    puts "Error: Object $dstName already exists!"
    return -code error GDSC_ERROR
  }

  set mypd tmp-polygonFromPts-pd
  set myconn tmp-polygonFromPts-conn
  set mypts tmp-polygonFromPts-mypts

  catch {$mypd Delete}
  catch {$myconn Delete}
  catch {$mypts Delete}

  # allocate vtk objects
  vtkPoints $mypts
  $mypts Allocate 200 400

  vtkIdList $myconn
  $myconn Initialize
  $myconn Allocate 200 400

  vtkPolyData $mypd
  $mypd Initialize
  $mypd Allocate 200 400

  # create vtkPoints array
  for {set i 0} {$i < [llength $pts]} {incr i} {
    set pt [lindex $pts $i]
    $mypts InsertNextPoint [lindex $pt 0] [lindex $pt 1] [lindex $pt 2]
  }

  # create connectivity for polygon
  for {set i 0} {$i < [llength $pts]} {incr i} {
  $myconn InsertNextId $i
  }

  $mypd SetPoints $mypts
  $mypd InsertNextCell $VTK_POLYGON $myconn

  repos_importVtkPd -src $mypd -dst $dstName

  catch {$mypd Delete}
  catch {$myconn Delete}
  catch {$mypts Delete}

  return GDSC_OK
}


# --------------
# geom_closestPt
# --------------

proc geom_closestPt {polyObj desiredPt} {

  #@c Searches a PolyData object for the closest point to pt.
  #@a polyObj:  PolyData object.
  #@a desiredPt:  Point to find closest point.
  #@r Closest point found in PolyData to pt.

  if {[repos_exists -obj $polyObj] == 0} {
    puts "ERROR: Object $polyObj does not exist."
    return -code error "ERROR: Object $polyObj does not exist."
  }
  if {[repos_type -obj $polyObj] != "PolyData"} {
    puts "ERROR: Object $polyObj not of type PolyData."
    return -code error "ERROR: Object $polyObj not of type PolyData."
  }

  set myobj [repos_exportToVtk -src $polyObj]
  set num [$myobj GetNumberOfPoints]

  set closestPt [$myobj GetPoint 0]
  set mindist 1000000.0

  for {set i 0} {$i < $num} {incr i} {
      set pt [$myobj GetPoint $i]
      set dist [math_distance $pt $desiredPt]
      if {$dist < $mindist} {
       set mindist $dist
       set closestPt $pt
      }
  }

  return $closestPt

}


# -----------------
# geom_scaleScalars
# -----------------

proc geom_scaleScalars {src scaleFactor dst} {

  #@author Nathan Wilson
  #@c This routine uniformly scales all of the scalars
  #@c values on src (i.e. newScalar = scaleFactor*oldScalar)
  #@c and creates a new object dst.
  #@a src:  Source PolyData.
  #@a dst:  New repository PolyData object to be created.
  #@a scaleFactor:  Multiplicitive factor.
  #@r status
  #@note  This routine should be rewritten in C.

  if {[repos_exists -obj $src] == "0"} {
    puts "ERROR:  Input PolyData $src doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $src] != "PolyData"} {
    puts "ERROR:  Object $src not of type PolyData."
    return -code error GDSC_ERROR
  }
  if {[repos_exists -obj $dst] == "1"} {
    puts "ERROR:  Output object $dst exists."
    return -code error GDSC_ERROR
  }

  # tmp objects
  set tmpobj /tmp/geom_scaleScalars
  catch {repos_delete -obj $tmpobj}

  # do work
  geom_copy -src $src -dst $tmpobj
  set obj [repos_exportToVtk -src $tmpobj]
  set scalars [[$obj GetPointData] GetScalars]
  for {set i 0} {$i < [$scalars GetNumberOfTuples]} {incr i} {
    $scalars SetTuple1 $i [expr [$scalars GetTuple1 $i]*$scaleFactor]
  }
  repos_importVtkPd -src $obj -dst $dst

  # clean up
  catch {repos_delete -obj $tmpobj}
  return GDSC_OK

}


# --------------------------
# geom_scaleVectorComponents
# --------------------------

proc geom_scaleVectorComponents {src scaleFactor dst} {

  #@author Nathan Wilson
  #@c This routine uniformly scales all 3 components of the vectors
  #@c values on src (i.e. comp = scaleFactor*comp)
  #@c and creates a new object dst.
  #@a src:  Source PolyData.
  #@a dst:  New repository PolyData object to be created.
  #@a scaleFactor:  Multiplicitive factor.
  #@r status
  #@note  This routine should be rewritten in C.

  if {[repos_exists -obj $src] == "0"} {
    puts "ERROR:  Input PolyData $src doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $src] != "PolyData"} {
    puts "ERROR:  Object $src not of type PolyData."
    return -code error GDSC_ERROR
  }
  if {[repos_exists -obj $dst] == "1"} {
    puts "ERROR:  Output object $dst exists."
    return -code error GDSC_ERROR
  }

  # tmp objects
  set tmpobj /tmp/geom_scaleVectors
  catch {repos_delete -obj $tmpobj}

  # do work
  geom_copy -src $src -dst $tmpobj
  set obj [repos_exportToVtk -src $tmpobj]
  set vectors [[$obj GetPointData] GetVectors]
  for {set i 0} {$i < [$vectors GetNumberOfTuples]} {incr i} {
    set v [$vectors GetTuple3 $i]
    $vectors SetTuple3 $i [expr double($scaleFactor)*[lindex $v 0]] \
                          [expr double($scaleFactor)*[lindex $v 1]] \
                          [expr double($scaleFactor)*[lindex $v 2]]
  }
  repos_importVtkPd -src $obj -dst $dst

  # clean up
  catch {repos_delete -obj $tmpobj}
  return GDSC_OK

}


# -------------------
# geom_getSubsetOfPts
# -------------------

proc geom_getSubsetOfPts {pd1 pd2 tol rnodes} {

  #@author Nathan Wilson
  #@c This routine creates a Tcl list of points
  #@c contained in pd1 not found in pd2.
  #@a pd1:  PolyData object (initial set of points).
  #@a pd2:  PolyData object (exclude set of points).
  #@a tol:  Tolerance.  Points less than a distance of tol apart
  #@a tol:  are assumed to be the same.
  #@a rnodes:  Tcl list of nodes in pd1 not contained in pd2.
  #@note  This routine is horribly slow for large data sets.  This
  #@note  is due to a simplisitic implementation and the fact this
  #@note  code should be written in C.

  upvar $rnodes nodes
  if {[repos_exists -obj $pd1] == "0"} {
    puts "ERROR:  Input PolyData $pd1 doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $pd1] != "PolyData"} {
    puts "ERROR:  Object $pd1 not of type PolyData."
    return -code error GDSC_ERROR
  }
  if {[repos_exists -obj $pd2] == "0"} {
    puts "ERROR:  Input PolyData $pd2 doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $pd2] != "PolyData"} {
    puts "ERROR:  Object $pd2 not of type PolyData."
    return -code error GDSC_ERROR
  }

  # create a new polydata object containing only
  # the points on the interior of the inlet face

  set nodes {}
  geom_getPts $pd1 pall
  geom_getPts $pd2 pedge
  foreach p $pall {
    set onEdge 0
    for {set i 0} {$i < [llength $pedge]} {incr i} {
      set pe [lindex $pedge $i]
      if {[math_distance $p $pe] < $tol} {
        set onEdge 1
	break
        set pedge [lreplace $pedge $i $i]
      }
    }
    if {$onEdge == 0} {
      lappend nodes $p
    }
  }

  return GDSC_OK

}


# -----------
# geom_getPts
# -----------

proc geom_getPts {pd rnodes} {

  #@author Nathan Wilson
  #@c Simple proc to create a Tcl list containing the
  #@c the points in a PolyData object.
  #@a pd:  Input PolyData object.
  #@a rnodes:  Returned Tcl list of points.
  #@r status
  #@note  This code should be rewritten in C.

  upvar $rnodes nodes
  if {[repos_exists -obj $pd] == "0"} {
    puts "ERROR:  Input PolyData $all_pd doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $pd] != "PolyData"} {
    puts "ERROR:  Object $all_pd not of type PolyData."
    return -code error GDSC_ERROR
  }

  set nodes {}
  set poly [[repos_exportToVtk -src $pd] GetPoints]
  for {set i 0} {$i < [$poly GetNumberOfPoints]} {incr i} {
    set p [$poly GetPoint $i]
    lappend nodes $p
  }

  return GDSC_OK

}


# ---------------
# geom_mapVectors
# ---------------

proc geom_mapVectors {velocityMap inlet_mesh_face result} {

  #@author Nathan Wilson
  #@c  This proc maps the vectors given on an input PolyData
  #@c  object to a destination PolyData object and returns
  #@c  a new PolyData.  This routine is not general and makes
  #@c  numerous assumptions about the nature of each point set.  See
  #@c  notes.
  #@a  velocityMap:  Input PolyData with defined vector data.
  #@a  inlet_mesh_face:  PolyData onto which to map the vector data.
  #@a  result:  Name of new repository PolyData object to be
  #@a  result:  created.
  #@r  status
  #@note  This code does a simple mapping of the vector data from
  #@note  the points of the input PolyData to the points of the
  #@note  the output PolyData.  This mapping assumes that the objects
  #@note  are similar in that if you send a ray from the center of each
  #@note  object the relationship between inner (r) and outer (R) radius
  #@note  is given by r_out = r_in * R_out / R_in for any given angle.
  #@note  We also scale the result vectors assuming they represent a
  #@note  flux so that the input and output PolyData's
  #@note  have the same through plane flux.  This code requires that both
  #@note  PolyData's be in the z=0 plane.  Each should consist of a single
  #@note  region.  Iso-parametric interpolation functions are used to
  #@note  evaluate the vector values and for calculating through plane
  #@note  flow rate.
  #@note  This code would be much faster if it were rewritten in C.

  if {[repos_exists -obj $velocityMap] == "0"} {
    puts "ERROR:  Input PolyData $velocityMap doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $velocityMap] != "PolyData"} {
    puts "ERROR:  Object $velocityMap not of type PolyData."
    return -code error GDSC_ERROR
  }
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

  # extract free edges from pcmri
  $myFE SetInputDataObject [repos_exportToVtk -src $velocityMap]
  $myFE Update
  repos_importVtkPd -src [$myFE GetOutput] -dst $segmentation

  # need extruded object to do intersections

  # extrude mesh free edges
  vtkLinearExtrusionFilter $myLinFilt
  $myLinFilt SetExtrusionTypeToVectorExtrusion
  $myLinFilt SetInputDataObject [repos_exportToVtk -src $meshFreeEdges]
  $myLinFilt SetVector 0 0 1
  $myLinFilt SetScaleFactor 2
  $myLinFilt Update
  repos_importVtkPd -src [$myLinFilt GetOutput] -dst $extrudedMeshWall

  # extrude segmentation
  $myLinFilt SetInputDataObject [repos_exportToVtk -src $segmentation]
  $myLinFilt Update
  repos_importVtkPd -src [$myLinFilt GetOutput] -dst $extrudedSegWall

  # create an interior (non-boundary) node list
  puts "Find interior nodes on mesh face (i.e. non-boundary nodes)."
  geom_getSubsetOfPts $inlet_mesh_face $meshFreeEdges 0.001 nodes
  puts "Found [llength $nodes] interior nodes."

  # get the centers of each object
  set ctrMesh [geom_avgPt -obj $meshFreeEdges]
  puts "Center of mesh face: $ctrMesh"
  set ctrPCMRI [geom_avgPt -obj $velocityMap]
  puts "Center of PCMRI segmentation: $ctrPCMRI"

  # create a new set of scalars and vectors
  vtkFloatArray $vScalars
  $vScalars Allocate 100 100
  vtkFloatArray $vVectors; $vVectors SetNumberOfComponents 3
  $vVectors Allocate 100 100

  # create point locator
  vtkPointLocator $pointLocator
  $pointLocator SetDataSet [repos_exportToVtk -src $inlet_mesh_face]
  $pointLocator AutomaticOn
  $pointLocator SetTolerance 0.001
  $pointLocator BuildLocator

  # inserting a through plane component of zero for all points
  for {set i 0} {$i < [[repos_exportToVtk -src $inlet_mesh_face] GetNumberOfPoints]} {incr i} {
    $vScalars InsertNextTuple1 0.0
    $vVectors InsertNextTuple3 0 0 0
  }

  # now loop over the nodes calculating the velocity for each mesh node

  set counter 0

  foreach node $nodes {

    set r_m_pt [math_subVectors $node $ctrMesh]
    set r_m_pt [list [lindex $r_m_pt 0] [lindex $r_m_pt 1] 0]
    set r_m [math_magnitude $r_m_pt]

    set angleDeg [math_radToDeg [expr atan2(double([lindex $r_m_pt 1]),double([lindex $r_m_pt 0]))]]

    set circle [math_circlePt $angleDeg 500.0]
    set outsidePtMesh [list [expr [lindex $ctrMesh 0]+[lindex $circle 0]] \
                            [expr [lindex $ctrMesh 1]+[lindex $circle 1]] 1]

    set bdryPtMesh [geom_intersectWithLine -obj $extrudedMeshWall \
                            -pt0 [list [lindex $ctrMesh 0] [lindex $ctrMesh 1] 1] \
                            -pt1 $outsidePtMesh]
    set bdryPtMesh [list [lindex $bdryPtMesh 0] [lindex $bdryPtMesh 1] 0]

    set R_m_pt [math_subVectors $bdryPtMesh $ctrMesh]
    set R_m_pt [list [lindex $R_m_pt 0] [lindex $R_m_pt 1] 0]
    set R_m [math_magnitude $R_m_pt]

    if {$r_m > $R_m} {
      puts "ERROR:  inside radius ($r_m) exceeds outside radius ($R_m)."
      return -code error "ERROR:  inside radius ($r_m) exceeds outside radius ($R_m)."
    }

    set outsidePtSeg [list [expr [lindex $ctrPCMRI 0]+[lindex $circle 0]] \
                           [expr [lindex $ctrPCMRI 1]+[lindex $circle 1]] 1]

    set bdryPtSeg [geom_intersectWithLine -obj $extrudedSegWall \
                            -pt0 [list [lindex $ctrPCMRI 0] [lindex $ctrPCMRI 1] 1] \
                            -pt1 $outsidePtSeg]
    set bdryPtSeg [list [lindex $bdryPtSeg 0] [lindex $bdryPtSeg 1] 0]

    set R_pc_pt [math_subVectors $bdryPtSeg $ctrPCMRI]
    set R_pc_pt [list [lindex $R_pc_pt 0] [lindex $R_pc_pt 1] 0]
    set R_pc [math_magnitude $R_pc_pt]

    set r_pc [expr double($r_m*$R_pc)/double($R_m)]

    if {$r_pc > $R_pc} {
      puts "ERROR:  inside radius ($r_m) exceeds outside radius ($R_m)."
      return -code error "ERROR:  inside radius ($r_m) exceeds outside radius ($R_m)."
    }

    set circle [math_circlePt $angleDeg $r_pc]

    set pt [list [expr [lindex $ctrPCMRI 0]+[lindex $circle 0]] \
                 [expr [lindex $ctrPCMRI 1]+[lindex $circle 1]] 0]

    set vel [geom_interpolateVector -obj $velocityMap -pt $pt]

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
    $vScalars SetTuple1 $ptId [lindex $vel 2]
    $vVectors SetTuple3 $ptId [lindex $vel 0] [lindex $vel 1] [lindex $vel 2]

    #puts "r_m: $r_m  R_m: $R_m  r_pc: $r_pc  R_pc: $R_pc  angle: $angleDeg  vel: $vel ptId: $ptId"
  }

  # create the output object
  vtkPolyData $outputObj
  $outputObj SetPoints [[repos_exportToVtk -src $inlet_mesh_face] GetPoints]
  $outputObj CopyStructure [repos_exportToVtk -src $inlet_mesh_face]
  [$outputObj GetPointData] SetScalars $vScalars
  [$outputObj GetPointData] SetVectors $vVectors
  catch {repos_delete -obj $result}
  repos_importVtkPd -src $outputObj -dst $result
  catch {repos_delete -obj $tmpresult}
  geom_copy -src $result -dst $tmpresult

  # calculate the flow rates for the original and mapped mesh
  set orgFlow [geom_integrateSurfaceFlux -obj $velocityMap -nrm {0 0 1} -tensorType 1]
  # in true bizarre fashion, we let the user scale the volumetric flow via an external
  # file specifying a volumetric flow which must be in the correct units!
  global gOptions
  if {[info exists gOptions(scale_mapped_vel_by_file)] == 1} {
    if {[file exists $gOptions(scale_mapped_vel_by_file)] == 1} {
       # read flow rate from file
       set bizfp [open $gOptions(scale_mapped_vel_by_file)]
       set bizpts {}
       while {[gets $bizfp bizline] >= 0} {
         set bizline [string trim $bizline]
         if {[string index $bizline 0] == "#" || $bizline == ""} {
          continue
         }
         if {[scan $bizline "%lf %lf" biztime bizflow] != 2} {
            close $bizfp
            return -code error "Incorrect format of line in flow rate file ($bizline)."
         }
         lappend bizpts [list $biztime $bizflow]
      }
      close $bizfp
      # need to do linear interpolation to get flow value for current time
      # step
      global gBC
      global bcGUIcurrentFrameNumber
      set bizpts [math_linearInterp -pts $bizpts -numInterpPts $gBC(numSlices)]
      set bizFlow [lindex [lindex $bizpts [expr $bcGUIcurrentFrameNumber - 1]] 1]
      puts "Using flow from file ($bizFlow) instead of original flow for scaling ($orgFlow)."
      set orgFlow $bizFlow
    } else {
      puts "Could not find file $gOptions(scale_mapped_vel_by_file).  Option ignored."
    }
  }

  set newFlow [geom_integrateSurfaceFlux -obj $result -nrm {0 0 1} -tensorType 1]
  set scaleFactor [expr $orgFlow/$newFlow]
  puts "orgFlow: $orgFlow  newFlow: $newFlow  ratio: $scaleFactor"
  catch {repos_delete -obj $result}
  geom_scaleVectorComponents $tmpresult $scaleFactor $result
  puts "scaled flow rate: [geom_integrateSurfaceFlux -obj $result -nrm {0 0 1} -tensorType 1]"

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


# ----------------------------
# geom_applyTransformMatrix
# ----------------------------

proc geom_applyTransformMatrix {src matrix dst} {

  #@author Nathan Wilson
  #@c transform a src PolyData with a vtkMatrix4x4
  #@c matrix.
  #@a src: input PolyData object.
  #@a dst: output PolyData object.
  #@a matrix:  vtkMatrix4x4 transformation matrix.

  set t tmp-geom_transformPD-trans
  set f tmp-geom_transformPD-filt

    catch {$t Delete}
    catch {$f Delete}

  vtkTransform $t
  $t SetMatrix $matrix

  vtkTransformPolyDataFilter $f
  $f SetTransform $t
  $f SetInputDataObject [repos_exportToVtk -src $src]
  $f Update
  repos_importVtkPd -src [$f GetOutput] -dst $dst

  $t Delete
  $f Delete

}


# ----------------
# geom_triangulate
# ----------------

proc geom_triangulate {input output} {

  #@author Nathan Wilson
  #@c Triangulate the input polygon.
  #@a input:  PolyData to triangulate.
  #@a output: Resulting PolyData object.
  #@r status

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

  set triFilter tmp-geom_triangulate-filter

  catch {$triFilter Delete}
  vtkTriangleFilter $triFilter
  $triFilter PassVertsOff
  $triFilter PassLinesOff
  $triFilter SetInputDataObject [repos_exportToVtk -src $input]
  $triFilter Update
  repos_importVtkPd -src [$triFilter GetOutput] -dst $output

  catch {$triFilter Delete}

  return GDSC_OK

}


# -----------------------------
# geom_calcTransformMatrixToRAS
# -----------------------------

proc geom_calcTransformMatrixToRAS {xyzPts rasPts rtnMatrix4x4} {

  #@author Joy Ku
  #@c Calculates the transformations required to move a plane specified
  #@c by 3 points in (XYZ) space into the appropriate location in
  #@c 3-D (RAS) space.  Hardcoded tolerances used!
  #@a xyzPts:  list of 3D points in XYZ space in the following order:  
  #@a          top left corner, top right corner, bottom right corner.
  #@a          Each point is represented as a list of its coordinates.
  #@a rasPts:  list of 3D points in RAS space in the following order:  
  #@a          top left corner, top right corner, bottom right corner.
  #@a          Each point is represented as a list of its coordinates.
  #@a rtnMatrix4x4: vtkMatrix4x4 matrix to transform image into 3-D space.
  #@note Method is exact copy of img_calcTransformMatrixToRAS (CVS-1.36).  
  #@note Only inputs are changed.
  #@r status

  geom_calcTransformMatrixToRASWithTol $xyzPts $rasPts 0.001 0.00001 0.0001 0 $rtnMatrix4x4

}

# ------------------------------------
# geom_calcTransformMatrixToRASWithTol
# ------------------------------------

proc geom_calcTransformMatrixToRASWithTol {xyzPts rasPts diffTol dirTol rotVectTol debugOn rtnMatrix4x4} {

  #@author Joy Ku
  #@c Calculates the transformations required to move a plane specified
  #@c by 3 points in (XYZ) space into the appropriate location in
  #@c 3-D (RAS) space.
  #@a xyzPts:  list of 3D points in XYZ space in the following order:  
  #@a          top left corner, top right corner, bottom right corner.
  #@a          Each point is represented as a list of its coordinates.
  #@a rasPts:  list of 3D points in RAS space in the following order:  
  #@a          top left corner, top right corner, bottom right corner.
  #@a          Each point is represented as a list of its coordinates.
  #@a diffTol: how much the final points can vary from the initial points.
  #@a dirTol:  tolerance used to determine parallel vectors.
  #@a rotVectTol:  tolerance used to determine A x B equals zero vector.
  #@a debugOn:  flag to output simple debugging information.
  #@a rtnMatrix4x4: vtkMatrix4x4 matrix to transform image into 3-D space.
  
  #@note Method is exact copy of img_calcTransformMatrixToRAS (CVS-1.36).  
  #@note Only inputs are changed.
  #@r status

  # make a fake flat triangle for testing relocation
  set pts $xyzPts

  set fake_slice /tmp/calcTransformMatrixToRAS/fake_slice
  catch {repos_delete -obj $fake_slice}
  geom_polygonFromPts $pts $fake_slice
  repos_setLabel -obj $fake_slice -key color -value yellow

  # create a triangle in 3-D space corresponding to corners of plane
  set ptList $rasPts
  set top_left_corner [lindex $rasPts 0]
  set top_right_corner [lindex $rasPts 1]
  set bottom_right_corner [lindex $rasPts 2]

  set t1 tmp-calcTransformMatrixToRAS-1
  set t2 tmp-calcTransformMatrixToRAS-2
  set t3 tmp-calcTransformMatrixToRAS-3
  set t4 tmp-calcTransformMatrixToRAS-4
  set t5 tmp-calcTransformMatrixToRAS-5
  set t6 tmp-calcTransformMatrixToRAS-6
  set t7 tmp-calcTransformMatrixToRAS-7

  set f1 tmp-calcTransformMatrixToRAS-8
  set f2 tmp-calcTransformMatrixToRAS-9
  set f3 tmp-calcTransformMatrixToRAS-10
  set f4 tmp-calcTransformMatrixToRAS-11
  set f5 tmp-calcTransformMatrixToRAS-12
  set f6 tmp-calcTransformMatrixToRAS-13
  set f7 tmp-calcTransformMatrixToRAS-14

  catch {$t1 Delete}
  catch {$t2 Delete}
  catch {$t3 Delete}
  catch {$t4 Delete}
  catch {$t5 Delete}
  catch {$t6 Delete}
  catch {$t7 Delete}

  catch {$f1 Delete}
  catch {$f2 Delete}
  catch {$f3 Delete}
  catch {$f4 Delete}
  catch {$f5 Delete}
  catch {$f6 Delete}
  catch {$f7 Delete}

  vtkTransform $t1
  vtkTransform $t2
  vtkTransform $t3
  vtkTransform $t4
  vtkTransform $t5
  vtkTransform $t6
  vtkTransform $t7

  vtkTransformPolyDataFilter $f1
  vtkTransformPolyDataFilter $f2
  vtkTransformPolyDataFilter $f3
  vtkTransformPolyDataFilter $f4
  vtkTransformPolyDataFilter $f5
  vtkTransformPolyDataFilter $f6
  vtkTransformPolyDataFilter $f7

  set p1 [lindex $pts 0]
  set move_origin [math_subVectors $top_left_corner $p1]

  #
  # step 1: translate
  #

  $t1 Translate [lindex $move_origin 0] [lindex $move_origin 1] \
                [lindex $move_origin 2]


  $f1 SetInputDataObject [repos_exportToVtk -src $fake_slice]
  $f1 SetTransform $t1
  $f1 Update

  set tlc [[$f1 GetOutput] GetPoint 0]
  set trc [[$f1 GetOutput] GetPoint 1]
  set brc [[$f1 GetOutput] GetPoint 2]
  if {$debugOn != 0} {
  puts "After step 1 (translate vector: $move_origin)"
  puts "  tlc: [format {%.6e %.6e %.6e} [lindex $tlc 0] [lindex $tlc 1] [lindex $tlc 2]]"
  puts "  TLC: [format {%.6e %.6e %.6e} [lindex $top_left_corner 0] [lindex $top_left_corner 1] [lindex $top_left_corner 2]]"
  puts "  trc: [format {%.6e %.6e %.6e} [lindex $trc 0] [lindex $trc 1] [lindex $trc 2]]"
  puts "  brc: [format {%.6e %.6e %.6e} [lindex $brc 0] [lindex $brc 1] [lindex $brc 2]]"
  }

  #
  # step 2: rotate
  #
  set a [math_subVectors [[$f1 GetOutput] GetPoint 1] $top_left_corner]
  set a [math_normalize $a]
  set b [math_subVectors $top_right_corner $top_left_corner]
  set b [math_normalize $b]
  #puts "a: $a  b: $b"

  set rot_vect [math_cross $a $b]
  # if the rotation vector is small in mangitude, the vectors are parallel
  if {[math_magnitude $rot_vect] < $rotVectTol} {
    # use the normal of the plane for rotation
    set c [math_subVectors [[$f1 GetOutput] GetPoint 2] [[$f1 GetOutput] GetPoint 1]]
    set rot_vect [math_cross $a $c]
    set rot_vect [math_normalize $rot_vect]
    # check if the vectors are in the same direction
    if {[math_magnitude [math_subVectors $a $b]] > $dirTol} {
      # vectors in opposite directions
      #puts "vectors parallel and in opposite directions"
      set rot_ang_drad [math_pi]
      set rot_ang_deg 180.0
    } else {
      # vectors in the same direction
      #puts "vectors parallel and in the same direction"
      set rot_ang_drad 0
      set rot_ang_deg 0
    }
  } else {
    #puts "vectors not parallel"
    set rot_vect [math_normalize $rot_vect]
    set rot_ang_rad [math_angleBtw3DVectors $a $b]
    set rot_ang_deg [math_radToDeg $rot_ang_rad]
  }

  set mv_to_org [math_subVectors {0 0 0} $top_left_corner]
  $t2 Translate [lindex $mv_to_org 0] [lindex $mv_to_org 1] [lindex $mv_to_org 2]

  $f2 SetInputDataObject [$f1 GetOutput]
  $f2 SetTransform $t2
  $f2 Update

  $t3 PostMultiply
  $t3 RotateWXYZ  $rot_ang_deg [lindex $rot_vect 0] [lindex $rot_vect 1] [lindex $rot_vect 2]

  $f3 SetInputDataObject [$f2 GetOutput]
  $f3 SetTransform $t3
  $f3 Update

  set mv_back [math_subVectors $top_left_corner {0 0 0}]
  $t4 Translate [lindex $mv_back 0] [lindex $mv_back 1] [lindex $mv_back 2]

  $f4 SetInputDataObject [$f3 GetOutput]
  $f4 SetTransform $t4
  $f4 Update

  set tlc [[$f4 GetOutput] GetPoint 0]
  set trc [[$f4 GetOutput] GetPoint 1]
  set brc [[$f4 GetOutput] GetPoint 2]
  if {$debugOn != 0} {
  puts "After step 2 (rotate vector: $rot_vect  angle_deg: $rot_ang_deg)"
  puts "  tlc: [format {%.6e %.6e %.6e} [lindex $tlc 0] [lindex $tlc 1] [lindex $tlc 2]]"
  puts "  TLC: [format {%.6e %.6e %.6e} [lindex $top_left_corner 0] [lindex $top_left_corner 1] [lindex $top_left_corner 2]]"
  puts "  trc: [format {%.6e %.6e %.6e} [lindex $trc 0] [lindex $trc 1] [lindex $trc 2]]"
  puts "  TRC: [format {%.6e %.6e %.6e} [lindex $top_right_corner 0] [lindex $top_right_corner 1] [lindex $top_right_corner 2]]"
  puts "  brc: [format {%.6e %.6e %.6e} [lindex $brc 0] [lindex $brc 1] [lindex $brc 2]]"
  }

  #
  # step 3: rotate
  #
  set a [math_subVectors [[$f4 GetOutput] GetPoint 2] $top_right_corner]
  set a [math_normalize $a]
  set b [math_subVectors $bottom_right_corner $top_right_corner]
  set b [math_normalize $b]
  set rot_vect [math_cross $a $b]

  # if the rotation vector is small in mangitude, the vectors are parallel
  if {[math_magnitude $rot_vect] < $rotVectTol} {
    # just rotate around the aligned edge
    #puts "just rotate around the aligned edge"
    set rot_vect [math_subVectors $top_right_corner $top_left_corner]
    set rot_vect [math_normalize $rot_vect]
    # check if the vectors are in the same direction
    if {[math_magnitude [math_subVectors $a $b]] > $dirTol} {
      # vectors in opposite directions
      #puts "vectors parallel and in opposite directions"
      set rot_ang_drad [math_pi]
      set rot_ang_deg 180.0
    } else {
      # vectors in the same direction
      #puts "vectors parallel and in the same direction"
      set rot_ang_drad 0
      set rot_ang_deg 0
    }
  } else {
    #puts "vectors not parallel"
    set rot_vect [math_normalize $rot_vect]
    set rot_ang_rad [math_angleBtw3DVectors $a $b]
    set rot_ang_deg [math_radToDeg $rot_ang_rad]
  }

  set mv_to_org [math_subVectors {0 0 0} $top_right_corner]
  $t5 Translate [lindex $mv_to_org 0] [lindex $mv_to_org 1] [lindex $mv_to_org 2]

  $f5 SetInputDataObject [$f4 GetOutput]
  $f5 SetTransform $t5
  $f5 Update

  $t6 PostMultiply
  $t6 RotateWXYZ  $rot_ang_deg [lindex $rot_vect 0] [lindex $rot_vect 1] [lindex $rot_vect 2]

  $f6 SetInputDataObject [$f5 GetOutput]
  $f6 SetTransform $t6
  $f6 Update

  set mv_back [math_subVectors $top_right_corner {0 0 0}]
  $t7 Translate [lindex $mv_back 0] [lindex $mv_back 1] [lindex $mv_back 2]

  $f7 SetInputDataObject [$f6 GetOutput]
  $f7 SetTransform $t7
  $f7 Update

  set tlc [[$f7 GetOutput] GetPoint 0]
  set trc [[$f7 GetOutput] GetPoint 1]
  set brc [[$f7 GetOutput] GetPoint 2]
  if {$debugOn != 0} {
  puts "After step 3 (rotate vector: $rot_vect  angle_deg: $rot_ang_deg)"
  puts "  tlc: [format {%3.6e %3.6e %3.6e} [lindex $tlc 0] [lindex $tlc 1] [lindex $tlc 2]]"
  puts "  TLC: [format {%3.6e %3.6e %3.6e} [lindex $top_left_corner 0] [lindex $top_left_corner 1] [lindex $top_left_corner 2]]"
  puts "  trc: [format {%3.6e %3.6e %3.6e} [lindex $trc 0] [lindex $trc 1] [lindex $trc 2]]"
  puts "  TRC: [format {%3.6e %3.6e %3.6e} [lindex $top_right_corner 0] [lindex $top_right_corner 1] [lindex $top_right_corner 2]]"
  puts "  brc: [format {%3.6e %3.6e %3.6e} [lindex $brc 0] [lindex $brc 1] [lindex $brc 2]]"
  puts "  BRC: [format {%3.6e %3.6e %3.6e} [lindex $bottom_right_corner 0] [lindex $bottom_right_corner 1] [lindex $bottom_right_corner 2]]"
  }

  # create a matrix to return
  catch {$rtnMatrix4x4 Delete}
  vtkMatrix4x4 $rtnMatrix4x4
  $rtnMatrix4x4 Identity

  # create a tmp matrix
  set tmpmat tmp-bc_calc-tmpmat
  catch {$tmpmat Delete}
  vtkMatrix4x4 $tmpmat

  foreach i [list $t1 $t2 $t3 $t4 $t5 $t6 $t7] {
    $rtnMatrix4x4 Multiply4x4 [$i GetMatrix] $rtnMatrix4x4 $tmpmat
    $rtnMatrix4x4 DeepCopy $tmpmat
  }

  # for debugging
  if {$debugOn != 0} {

    set pc_triangle should_be_this
    catch {repos_delete -obj $pc_triangle}
    geom_polygonFromPts $ptList $pc_triangle

    catch {repos_delete -obj move_1}
    repos_importVtkPd -src [$f1 GetOutput] -dst move_1
    repos_setLabel -obj move_1 -key color -value red

    catch {repos_delete -obj move_2}
    repos_importVtkPd -src [$f4 GetOutput] -dst move_2
    repos_setLabel -obj move_2 -key color -value green

    catch {repos_delete -obj move_3}
    repos_importVtkPd -src [$f7 GetOutput] -dst move_3
    repos_setLabel -obj move_3 -key color -value blue

    set t8 tmp-calcTransformMatrixToRAS-t8
    set f8 tmp-calcTransformMatrixToRAS-f8
    catch {$t8 Delete}
    catch {$f8 Delete}

    vtkTransform $t8
    $t8 SetMatrix $rtnMatrix4x4

    vtkTransformPolyDataFilter $f8
    $f8 SetInputDataObject [repos_exportToVtk -src $fake_slice]
    $f8 SetTransform $t8
    $f8 Update

    catch {repos_delete -obj move_all}
    repos_importVtkPd -src [$f8 GetOutput] -dst move_all
    repos_setLabel -obj move_all -key color -value white

    catch {$t8 Delete}
    catch {$f8 Delete}

  }

  # clean up
  catch {$t1 Delete}
  catch {$t2 Delete}
  catch {$t3 Delete}
  catch {$t4 Delete}
  catch {$t5 Delete}
  catch {$t6 Delete}
  catch {$t7 Delete}

  catch {$f1 Delete}
  catch {$f2 Delete}
  catch {$f3 Delete}
  catch {$f4 Delete}
  catch {$f5 Delete}
  catch {$f6 Delete}
  catch {$f7 Delete}

  # sanity check
  set lengthMetric [math_subVectors $top_right_corner $top_left_corner]
  set lengthMetric [math_magnitude $lengthMetric]
  set diffA [math_magnitude [math_subVectors $tlc $top_left_corner]]
  set diffB [math_magnitude [math_subVectors $trc $top_right_corner]]
  set diffC [math_magnitude [math_subVectors $brc $bottom_right_corner]]
  set diffA [expr $diffA / $lengthMetric]
  set diffB [expr $diffB / $lengthMetric]
  set diffC [expr $diffC / $lengthMetric]

  if {$debugOn != 0} {
    puts "diffs: $diffA $diffB $diffC"
  }

  if {$diffA > $diffTol || $diffB > $diffTol || $diffC > $diffTol} {
    puts "ERROR:  internal error in geom_calcTransformMatrixToRas"
    return -code error "ERROR:  internal error in geom_calcTransformMatrixToRAS."
  }

  return GDSC_OK

}


# --------------------
# geom_createNormals
# --------------------

proc geom_createNormals {srcPd dstName} {

    #@author Nathan Wilson
    #@c Uses vtkPolyDataNormals to create normals for input pd.
    #@a srcPd: Input PolyData object.
    #@a dstName: Destination PolyData name.
    #@r Returns a Tcl error if there is a problem.

    set p /tmp/geom_createNormals/reposPd
    catch {repos_delete -obj $p}

    if {[repos_type -obj $srcPd] != "PolyData"} {
	widget_ErrorDialog "object $srcPd not of type PolyData"
	return -code error
    }
    if {[repos_exists -obj $dstName]} {
	widget_ErrorDialog "object $dstName already exists"
	return -code error
    }

    set mknrm tmp-geom_createNormals-maker
    catch {$mknrm Delete}

    vtkPolyDataNormals $mknrm
    $mknrm ConsistencyOff
    $mknrm SplittingOff
    $mknrm ComputePointNormalsOn
    $mknrm FlipNormalsOff

    $mknrm SetInputDataObject [repos_exportToVtk -src $srcPd]
    $mknrm Update

    repos_importVtkPd -src [$mknrm GetOutput] -dst $p
    geom_copy -src $p -dst $dstName
    $mknrm Delete

}


# ------------
# group_exists
# ------------

proc group_exists {name} {
    #@author Ken Wang
    #@c Check to see if group exists.
    #@a name:  Name of group.
    #@r Boolean (0 or 1).
    global gGroup
    set elems [array names gGroup]
    if {[lsearch -exact $elems $name] < 0} {
      return 0
    } else {
      return 1
    }
}


# ------------
# group_create
# ------------

proc group_create {name} {
    #@author Ken Wang
    #@c Create a group if it doesn't exist.
    #@a name: Name of group to create.
    #@r status (0 if group already existed, 1 if it was created).
    global gGroup
    if {[group_exists $name]} {return 0}
    set gGroup($name) {}
    return 1
}


# ------------
# group_delete
# ------------

proc group_delete {name} {
    #@author Ken Wang
    #@c Delete a group.
    #@a name: Name of group to delete.
    #@r status (0 if group didn't exist, 1 if it was deleted).
    global gGroup
    global symbolicName
    if {![group_exists $name]} {
	return 0
    }
    unset gGroup($name)

    set tv $symbolicName(guiSV_group_tree)
    catch {$tv delete .groups.all.$name}
    return 1
}


# ---------
# group_get
# ---------
# Returns the (inherently sorted) strings stored in the named group.

proc group_get {name} {
    #@author Ken Wang
    #@c Get the list of objects stored in the group.
    #@a name: Group name.
    #@r Returns the (inherently sorted) strings stored in the named group.
    #@r Returns an empty string if the group doesn't exist.
    global gGroup
    if {![group_exists $name]} {
	return {}
    }
    set out {}
    set l $gGroup($name)
    foreach obj $l {
	set out [lappend out [lindex $obj 1]]
    }
    return $out
}

proc group_set_color {name color} {
    global gGroup
    global gGroupColor
    if {![group_exists $name]} {return 0}

    set gGroupColor($name) $color
}

proc group_get_color {name} {
    global gGroupColor
    global gOptions
    if {![group_exists $name]} {return 0}

    if {[lsearch -exact [array names gGroupColor] $name] < 0} {
      set gGroupColor($name) $gOptions(color_for_groups)
    } 
      
    return $gGroupColor($name)
}

# ------------
# group_itemix
# ------------
# Returns the position of obj in group name.

proc group_itemix {name obj} {
    #@author Ken Wang
    #@c Returns the position of obj in group name.
    #@a name: group name.
    #@a obj: The object to locate in the group.
    #@r Returns the position of obj in group name.
    if {![group_exists $name]} {
	return -1
    }
    set objs [group_get $name]
    set ix [lsearch -exact $objs $obj]
    return $ix
}


# ------------
# group_itemid
# ------------
# Returns the id associated with the given obj.

proc group_itemid {name obj} {
    #@author Ken Wang
    #@c Returns the id associated with the given obj.
    #@a name: group name.
    #@a obj: object you want the id for.
    #@r id of the given object.
    global gGroup
    set ix [group_itemix $name $obj]
    if {$ix < 0} {
	return -code error "object $obj not found in group $name"
    }
    set item [lindex $gGroup($name) $ix]
    return [lindex $item 0]
}


# -------------
# group_iditems
# -------------

proc group_iditems {name id} {
    #@author Ken Wang
    #@c Return a string of ids with the name of the given id interspersed.
    #@a name: name of group.
    #@a id: the id to insert the object name into the return string.
    #@r A string of ids with the object name corresponding to id in place.
    #@note  This routine confuses me quite a bit as it is not obvious how
    #@note  the output of this procedure would be useful (N.W.)
    global gGroup
    if {![group_exists $name]} {
	return {}
    }
    set l $gGroup($name)
    set out {}
    foreach obj $l {
	set i [lappend out [lindex $obj 0]]
	if {$id == $i} {
	    set out [lappend out [lindex $obj 1]]
	}
    }
    return $out
}


# -------------
# group_idtaken
# -------------

proc group_idtaken {name id} {
    #@author Ken Wang
    #@c Check to see if the id is taken in the given group.
    #@a name: group name.
    #@a id: id number.
    #@r returns an empty string if the group doesn't exist,
    #@r a 1 if the id is used and a 0 if it isn't.
    global gGroup
    if {![group_exists $name]} {
	return {}
    }
    set l $gGroup($name)
    foreach obj $l {
        set i [lindex $obj 0]
	if {$id == $i} {
	    return 1
	}
    }
    return 0
}

proc group_idvalue {name id} {
    #@author Ken Wang
    #@c Check to see if the id is taken in the given group.
    #@a name: group name.
    #@a id: id number.
    #@r returns an empty string if the group doesn't exist,
    #@r a 1 if the id is used and a 0 if it isn't.
    global gGroup
    if {![group_exists $name]} {
	return {}
    }
    set l $gGroup($name)
    set iter 0
    foreach obj $l {
        set i [lindex $obj 0]
	if {$id == $i} {
	    return $iter
	}
	incr iter
    }
    return 0
}


# ---------
# group_add
# ---------
# The string $obj is stored in the group $name, and its position of
# insertion is governed by a "-dictionary" sort on $id.  This means
# that numbers will be treated numerically (as opposed to the
# situation with "-ascii") and characters are treated alphabetically
# (case-insensitive).

# Note that different strings $obj can be stored in the same group
# with the same $id... this will just lead to equivalence between
# these items in the sorting process.

proc group_add {name obj id} {
    #@author Ken Wang
    #@c Add an object to a group.
    #@note The string $obj is stored in the group $name, and its position of
    #@note insertion is governed by a "-dictionary" sort on $id.  This means
    #@note that numbers will be treated numerically (as opposed to the
    #@note situation with "-ascii") and characters are treated alphabetically
    #@note (case-insensitive).&p

    #@note Different strings $obj can be stored in the same group
    #@note with the same $id... this will just lead to equivalence between
    #@note these items in the sorting process.

    #@a name: group name.
    #@a obj: object name.
    #@a id: integer id to associate object with.

    global gGroup
    if {![group_exists $name]} {
	return 0
    }
    set ix [group_itemix $name $obj]
    if {$ix >= 0} {
	return 1
    }
    set item [list $id $obj]
    set l $gGroup($name)
    set l [lappend l $item]
    set gGroup($name) [lsort -dictionary -index 0 $l]
    return 1
}


# ------------
# group_remove
# ------------

proc group_remove {name obj} {
    #@author Ken Wang
    #@c remove an object from a group.
    #@a name: group name.
    #@a obj: object to remove.
    #@r 0 if the group doesn't exist, 1 if the
    #@r the object doesn't exist or was deleted.
    global gGroup
    if {![group_exists $name]} {
	return 0
    }
    set l $gGroup($name)
    set ix [group_itemix $name $obj]
    if {$ix < 0} {
	return 1
    }
    set gGroup($name) [lreplace $l $ix $ix]
    return 1
}


# -----------
# group_names
# -----------

proc group_names {} {
    #@author Ken Wang
    #@c Return the group names.
    #@r list of group names.
    global gGroup
    return [array names gGroup]
}


# ----------
# group_size
# ----------

proc group_size {name} {
    #@author Ken Wang
    #@c Return the size (length) of the group.
    #@a name: group name.
    #@r length of group (-1 if group doesn't exist).
    global gGroup
    if {![group_exists $name]} {
	return -1
    }
    set l $gGroup($name)
    return [llength $l]
}



proc group_copy_member { i oldname newname} {

  set orgid [group_itemid $oldname $i]
  puts "id $orgid"

  set newobjname /group/$newname/$orgid
  puts "rename: $i to $newobjname"
  catch {repos_delete -obj $newobjname}
  geom_copy -src $i -dst $newobjname
  foreach key [repos_getLabelKeys -obj $i] {
   if { $key !="color" || $key !="width" || $key !="opacity"} {
      catch {repos_clearLabel -obj $newobjname -key $key}
      repos_setLabel -obj $newobjname -key $key -value [repos_getLabel -obj $i -key $key]
    }
  }

group_add $newname $newobjname $orgid


}

# ------------------
# group_readProfiles
# ------------------

proc group_readProfiles {name filename} {
  #@author Nathan Wilson
  #@c Routine to read a group of profiles from a file.
  #@a filename: Input filename.
  #@a name: group name to be created.
  if {[group_exists $name]} {
	#return -code error "ERROR: Group $name already exists."
        puts "WARNING:  group existed and is being replaced!!"
        group_delete $name
  }
  group_create $name

  set fp [open $filename r]
  while {[gets $fp line] >= 0} {
	while {[regexp {^#} $line]} {
	    gets $fp line
	}

	# NOTE: Ken ignores the first line in his gui.
        #       I use it to recreate the group just as it
        #       was created.
        set path [file dirname $line]
        set objName $line
	gets $fp id
	gets $fp str

	set ptList {}
	while {[gets $fp line] > 0} {
	    scan $line "%f %f %f" x y z
	    set ptList [lappend ptList [list $x $y $z]]
	}

        catch {repos_delete -obj $objName}
	geom_pdFromOrderedPts $ptList $objName

	catch {unset arr}
	array set arr $str
	set names [array names arr]
	foreach n $names {
	    repos_setLabel -obj $objName -key $n -value $arr($n)
	}

	group_add $name $objName $id
    }
    close $fp
}


# ------------------
# group_saveProfiles
# ------------------

proc group_saveProfiles {name filename} {
  #@author Nathan Wilson
  #@c  Routine to save a group of profiles to a file.
  #@a name:  Group to be written to the file.
  #@a filename:  File to be created.
  set items [group_get $name]
  set fp [open $filename w]
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
	set id [group_itemid $name $i]
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
  }
  close $fp
}


# ---------------
# group_calcAreas
# ---------------

proc group_calcAreas {grpname} {

  #@author Nathan Wilson
  #@c Calculates the area of each segmentation in a given group
  #@c and output it to stdout.
  #@a grpname:  group name.

  set mypoly /tmp/group_calcAreas/poly
  set mypolyFlat /tmp/group_calcAreas/flat

  if {[group_exists $grpname] == 0} {
     return -code error "ERROR:  group ($grpname) does not exist."
  }
  set results {}
  foreach i [group_get $grpname] {
    catch {repos_delete -obj $mypoly}
    catch {repos_delete -obj $mypolyFlat}
    geom_polygonFromPts [geom_getOrderedPts -obj $i] $mypoly
    set nrm [geom_polygonNorm -obj $i]
    geom_flatten $nrm 1 $mypoly rnrm $mypolyFlat
    set area [geom_surfArea -src $mypolyFlat]
    set diameter [expr 2.0*sqrt(double($area)/[math_pi])]
    set center [geom_avgPt -obj $mypoly]
    lappend results "$i:  area: [format {%8.3f} $area]   effective diameter: [format {%8.3f} $diameter]   center: [format {%10.3f %10.3f %10.3f} [lindex $center 0] [lindex $center 1] [lindex $center 2]]"
  }

  foreach i $results {
    puts $i
  }

  catch {repos_delete -obj $mypoly}
  catch {repos_delete -obj $mypolyFlat}

}


proc group_details {name args} {
  #@author: Nathan Wilson
  #@c  This dumps a pretty output on the details of the members
  #@c  of the group.
  #@a  grpName:  valid group name.
  #@a  args: file to be created containing information.
  #@r  status
  if {[group_exists $name] == 0} {
    return -code error "ERROR:  group $grpName does not exist."
  }

  puts ""
  foreach i [group_get $name] {
    puts "$i"
    puts "------------------------------------------------"
    foreach j [lsort -dictionary [repos_getLabelKeys -obj $i]] {
      puts [format "%-25s = %s" $j [repos_getLabel -obj $i -key $j]]
    }
    puts ""
  }

  if {$args != ""} {
    puts "writing file ($args) with group details"
    set fp [open $args w]
    foreach i [group_get $name] {
      puts $fp "$i"
      puts $fp "------------------------------------------------"
      foreach j [lsort -dictionary [repos_getLabelKeys -obj $i]] {
        puts $fp [format "%-25s = %s" $j [repos_getLabel -obj $i -key $j]]
      }
      puts $fp ""
    }
    close $fp
  }
  return GDSC_OK
}


# --------------
# group_renumber
# --------------

proc group_renumber {name incrby} {
    #@author Nathan Wilson
    #@c Renumbers all the members in the group by adding incrby to each
    #@c original id number.
    #@a name: group name.
    #@a incrby: integer value each id should have added to it.
    #@r returns an error if group doesn't exist.
    global gGroup
    if {![group_exists $name]} {
	return -code error "ERROR: group $name doesn't exist."
    }
    set groupmembers [group_get $name]
    foreach guy $groupmembers {
      set orgid [group_itemid $name $guy]
      set newid [expr $orgid + $incrby]
      # assume group members follow Ken's standard filename like convention
      set newobjname [file dirname $guy]/$newid
      puts "replacing (id = $orgid obj = $guy) with (id = $newid obj = $newobjname)"
      catch {repos_delete -obj $newobjname}
      geom_copy -src $guy -dst $newobjname
      set keys [repos_getLabelKeys -obj $guy]
      foreach key $keys {
         repos_setLabel -obj $newobjname -key $key \
                        -value [repos_getLabel -obj $guy -key $key]
      }
      group_remove $name $guy
      repos_delete -obj $guy
      group_add $name $newobjname $newid
    }

}


# ----------------------
# group_restorePreopSegs
# ----------------------

proc group_restorePreopSegs {name} {
    #@author Nathan Wilson
    #@c Attempts to place the segmentations saved in a group back into
    #@c the gui so they can be manipulated / viewed just as if they
    #@c were created in the curent session.
    #@a name: group name.
    #@r returns an error if group doesn't exist.
    #@warning  This routine does no error checking.  If you tell
    #@warning  it to do dumb things, it probably will.
    global gGroup
    if {![group_exists $name]} {
	return -code error "ERROR: group $name doesn't exist."
    }
    set groupmembers [group_get $name]
    foreach guy $groupmembers {
      if [catch {set pathId [repos_getLabel -obj $guy -key paste_pathId]}] {
	  set pathId [repos_getLabel -obj $guy -key pathId]
      }
      if [catch {set posId [repos_getLabel -obj $guy -key paste_posId]}] {
          set posId [repos_getLabel -obj $guy -key posId]
      }
      if [catch {set path_pos [repos_getLabel -obj $guy -key paste_pos]}] {
        set path_pos [repos_getLabel -obj $guy -key pos]
      }
      if [catch {set path_tan [repos_getLabel -obj $guy -key paste_nrm]}] {
        set path_tan [repos_getLabel -obj $guy -key nrm]
      }
      if [catch {set path_xhat [repos_getLabel -obj $guy -key paste_xhat]}] {
        set path_xhat [repos_getLabel -obj $guy -key xhat]
      }
      set creation_type [repos_getLabel -obj $guy -key creation_type]
 
      if {$creation_type == "levelset"} {
        set seg /lsGUI/$pathId/$posId/ls/oriented
        set segdisoriented /lsGUI/$pathId/$posId/ls
      } elseif {$creation_type == "threshold"} {
        set seg /lsGUI/$pathId/$posId/thr/oriented
        set segdisoriented /lsGUI/$pathId/$posId/thr/selected
      } else {
        puts "ERROR: invalid creation type for $guy, object ignored."
        continue
      }
      
      # check and see if we have done a bulk scaling
      if {[regexp bulk_scale_factor    [repos_getLabelKeys -obj $guy]] || \
	  [regexp shifted_origin       [repos_getLabelKeys -obj $guy]] || \
	  [regexp transformed_from_LPS [repos_getLabelKeys -obj $guy]]} {
        global gPathPoints
        set numSplinePts $gPathPoints($pathId,numSplinePts)
	set splinePts $gPathPoints($pathId,splinePts)
        array set splinePt [lindex $splinePts $posId]
        set path_pos [TupleToList $splinePt(p)]
        #puts "bulk_scale_factor $path_pos"
      }

      catch {repos_delete -obj $seg}
      catch {repos_delete -obj $segdisoriented}
      geom_copy -src $guy -dst $seg
      geom_disorientProfile -src $seg -dst $segdisoriented -path_pos $path_pos \
         -path_tan $path_tan -path_xhat $path_xhat
      set keys [repos_getLabelKeys -obj $guy]
      foreach key $keys {
         repos_setLabel -obj $seg -key $key \
                        -value [repos_getLabel -obj $guy -key $key]
         repos_setLabel -obj $segdisoriented -key $key \
                        -value [repos_getLabel -obj $guy -key $key]
      }
    }

}


# ----------------------
# group_restorePCMRISegs
# ----------------------

proc group_restorePCMRISegs {name} {
    #@author Nathan Wilson
    #@c Attempts to place the segmentations saved in a group back into
    #@c the gui so they can be manipulated / viewed just as if they
    #@c were created in the curent session.
    #@a name: group name.
    #@r returns an error if group doesn't exist.
    #@warning  This routine does no error checking.  If you tell
    #@warning  it to do dumb things, it probably will.
    global gGroup
    if {![group_exists $name]} {
	return -code error "ERROR: group $name doesn't exist."
    }
    set groupmembers [group_get $name]
    foreach guy $groupmembers {
      set posId [file tail $guy]
      set creation_type [repos_getLabel -obj $guy -key creation_type]
      if {$creation_type == "levelset"} {
        set seg /gBC/pcdata/mag/$posId/ls
      } elseif {$creation_type == "threshold"} {
        set seg /gBC/pcdata/mag/$posId/thr/selected
      } else {
        puts "ERROR: invalid creation type for $guy, object ignored."
        continue
      }
      catch {repos_delete -obj $seg}
      geom_copy -src $guy -dst $seg
      set keys [repos_getLabelKeys -obj $guy]
      foreach key $keys {
         repos_setLabel -obj $seg -key $key \
                        -value [repos_getLabel -obj $guy -key $key]
      }
    }
}


# -------------
# group_summary
# -------------

proc group_summary {grpname} {

  #@author Nathan Wilson
  #@c Provide a summary of the group
  #@c and output it to stdout.
  #@a grpname:  group name.

  if {[group_exists $grpname] == 0} {
     return -code error "ERROR:  group ($grpname) does not exist."
  }

  set num_segs [llength [group_get $grpname]]

  # count the methods used to create the segmentations
  set levelset 0
  set threshold 0
  set unknown 0
  set make_circle 0
  set make_ellipse 0
  set hand_drawn 0
  foreach i [group_get $grpname] {
     if [catch {set tag [repos_getLabel -obj $i -key creation_method]}] {
        incr unknown
     } elseif {$tag == "levelset"} {
       incr levelset
     } elseif {$tag == "threshold"} {
       incr threshold
     } elseif {$tag == "make_circle"} {
       incr make_circle
     } elseif {$tag == "make_ellipse"} {
       incr make_ellipse
     } elseif {$tag == "hand_drawn"} {
       incr hand_drawn
     } else {
       incr unknown
     }
  }

  # count the number of smoothed segmentations
  set num_smooth_modes 0
  set replaced_with_circle 0
  set scale_factor 0
  set paste_posId 0
  foreach i [group_get $grpname] {
    set keys [repos_getLabelKeys -obj $i]
    if {[lsearch $keys "num_smooth_modes"] >= 0} {incr num_smooth_modes}
    if {[lsearch $keys "replaced_with_circle"] >= 0} {incr replaced_with_circle}
    if {[lsearch $keys "paste_posId"] >= 0} {incr paste_posId}
    if {[lsearch $keys "scale_factor"] >= 0} {incr scale_factor}
  }

  puts ""
  puts "-------    GENERAL  -------------"
  puts "group name        : $grpname"
  puts "total num curves  : $num_segs"
  puts "--   TYPES OF SEGMENTATION   ----"
  puts "num. levelset segs: $levelset"
  puts "num. thresholds   : $threshold"
  puts "num. make circles : $make_circle"
  puts "num. make ellipses: $make_ellipse"
  puts "num. hand drawn   : $hand_drawn"
  puts "num. unknown      : $unknown"
  puts "-----      SMOOTHING  -----------"
  puts "num fourier smoothed     : $num_smooth_modes"
  puts "num replaced with circles: $replaced_with_circle"
  puts "num pasted profiles      : $paste_posId"
  puts "num scaled profiles      : $scale_factor"
  puts "-----      SMOOTHING  -----------"

  return [list $grpname $num_segs \
               $levelset $threshold $make_circle $make_ellipse $hand_drawn $unknown \
               $num_smooth_modes $replaced_with_circle $paste_posId $scale_factor]

}

# -----------------
# group_summary_all
# -----------------

proc group_summary_all {args} {

  #@author Nathan Wilson
  #@c Provide a summary of all the groups
  #@c and output it to stdout.
  #@a args:  optionally write out a file containing the summary info

  set allnames [group_names]
  set num_groups [llength $allnames]
  for {set i 0} {$i < $num_groups} {incr i} {
    set keepme($i) [group_summary [lindex $allnames $i]]
  }
  puts "\n\n"
  set mylabels [list "group name               \t" \
                     "total num curves         \t" \
                     "num. levelset segs       \t" \
                     "num. threshold segs      \t" \
                     "num. make circles        \t" \
                     "num. make ellipses       \t" \
                     "num. hand drawn          \t" \
                     "num. unknown             \t" \
                     "num fourier smoothed     \t" \
                     "num replaced with circles\t" \
                     "num pasted profiles      \t" \
                     "num scaled profiles      \t"]

  for {set i 0} {$i < 12} {incr i} {
    puts -nonewline [lindex $mylabels $i]
    for {set j 0} {$j < $num_groups} {incr j} {
       puts -nonewline "[lindex $keepme($j) $i]\t"
    }
    puts ""
  }
  puts ""

  if {$args != ""} {
    puts "Writing summary to file ($args)."
    set fp [open $args w]
    for {set i 0} {$i < 11} {incr i} {
      puts -nonewline $fp [lindex $mylabels $i]
      for {set j 0} {$j < $num_groups} {incr j} {
        puts -nonewline $fp "[lindex $keepme($j) $i]\t"
      }
      puts $fp ""
    }
    close $fp
  }

}



proc group_writeVTK {grpname imagename prefix} {

  #@author Nathan Wilson
  #@c Write segmentations to VTK polydata files.
  #@a grpname:  loaded group
  #@a imagename:  used to transform to RAS, can be ""
  #@a prefix:  prepended to all written files
  #@note  If <a imagename> is blank, then only flat
  #@note  profiles are written.

  set rasseg /tmp/group_writeVTK/ras
  set mymatrix tmp-group_writeVTK-matrix
  catch {repos_delete -obj $rasseg}
  catch {$mymatrix Delete}

  if {[group_size $grpname] <= 0} {
     return -code error "group does not exist or contain segs."
  }

  if {$imagename != ""} {
    img_calcTransformMatrixToRAS $imagename $mymatrix
  }

  foreach i [group_get $grpname] {
    repos_writeVtkPolyData -file $prefix-[file tail $i].vtk -obj $i -type ascii
    catch {repos_delete -obj $rasseg}
    if {$imagename != ""} {
      geom_applyTransformMatrix $i $mymatrix $rasseg
      repos_writeVtkPolyData -file $prefix-[file tail $i]-ras.vtk -obj $rasseg -type ascii
    }
  }

  catch {repos_delete -obj $rasseg}
  catch {$mymatrix Delete}

}


proc seg3d_names {} {
    global gSeg3D
    return [array names gSeg3D]
}

proc seg3d_add {name pd} {

    set segPD /guiGROUPS/3dsurface/$name
    if {[repos_exists -obj $segPD]} {return 1}
    
    global gSeg3D
    geom_copy -src $pd -dst $segPD
    set gSeg3D($name) $segPD
    return 0
}

proc seg3d_exists {name} {
    
    global gSeg3D
    return [expr {[lsearch -exact [array names gSeg3D] $name] > -1}]
}

proc seg3d_delete {name} {

    set segPD /guiGROUPS/3dsurface/$name
    catch {repos_delete -obj $segPD}
    global gSeg3D
    catch {unset gSeg3D($name)}

    global symbolicName
    set tv $symbolicName(guiSV_group_tree)
    catch {$tv delete .groups.3d.$name}
    return 0
}

proc seg3d_addForce {name pd} {

    set segPD /guiGROUPS/3dsurface/$name
    catch {seg3d_delete $name}
    catch {repos_delete -obj $segPD}
    global gSeg3D
    geom_copy -src $pd -dst $segPD
    set gSeg3D($name) $segPD
    return 0
}

proc seg3d_saveSeg {name fn} {
    global gSeg3D
    set pd $gSeg3D($name)
    repos_writeXMLPolyData $pd $fn
}

proc seg3d_loadSeg {name fn} {
  set pd /tmp/segpd
  catch {repos_delete -obj $pd}
  repos_readXMLPolyData $fn $pd
  seg3d_addForce $name $pd
}

proc seg3d_saveSurf {name filename} {
  global gSeg3D
  set pd $gSeg3D($name)
  set dir [file dirname $filename]
  set vtpfn [file join $dir $name.vtp]
  # need to protect against empty segmentations
  if {[[repos_exportToVtk -src $pd] GetNumberOfPoints] == 0} {
    return 1
  }
  package require md5
  repos_writeXMLPolyData $pd $vtpfn
  set vtpmd5 [::md5::md5 -hex -file $vtpfn]
  
  set keys [repos_getLabelKeys -obj $pd]
  catch {unset arr}
  foreach k $keys {
      set arr($k) [repos_getLabel -obj $pd -key $k]
  }
  set str [array get arr]
  set fp [open $filename w]
  puts $fp "name:$name"
  puts $fp "metadata:$str"
  puts $fp "vtp_filename:$vtpfn"
  puts $fp "vtp_md5:$vtpmd5"
  puts $fp ""
  close $fp
}


proc seg3d_readSurf {name filename} {
  
  if {[seg3d_exists $name]} {
  #return -code error "ERROR: Group $name already exists."
        puts "WARNING:  segmentation existed and is being replaced!!"
        seg3d_delete $name
  }

  set fp [open $filename r]
  set file_data [read $fp]
  close $fp
  set data [split $file_data "\n"]
  catch {unset indata}
  foreach line $data {
    set line [string trim $line]
    if { $line != ""} {
      set keyval [split $line ":"]
      set key [lindex $keyval 0]
      set val [lindex $keyval 1]
      set indata($key) $val
    }
  }

  set vtpfn $indata(vtp_filename)
  set invtpmd5 $indata(vtp_md5)
  set inname $indata(name)

  package require md5
  set fnvtpmd5 [::md5::md5 -hex -file $vtpfn]

  if {$invtpmd5 != $fnvtpmd5} {
    puts "md5 does not match!"
  }
    if {$inname != $name} {
    puts "names do not match!"
  }

  set pd /tmp/segpd
  catch {repos_delete -obj $pd}
  repos_readXMLPolyData $vtpfn $pd
  seg3d_add $name $pd
  global gSeg3D
  set objName $gSeg3D($name)

  catch {unset arr}
  array set arr $indata(metadata)
  set names [array names arr]
  foreach n $names {
    repos_setLabel -obj $objName -key $n -value $arr($n)
  }
}



# ----------
# seg_getPDSurf
# ----------
proc seg_getPDSurf {name} {
  #author Jameson Merkow
  #@c This function returns the pd surface assoctiated with the object.
  #@c THis returns either the 3d segmentation or the 3d lofted polydata for a selection


}

# ----------
# img_GetVOI
# ----------

proc img_GetVOI {filePrefix voxelDims logicalDims startImgNum hdrSz subvol filePattern} {
    #@author Ken Wang
    #@c read in a volume data set specifying a specific subvolume of interest.
    #@a filePrefix: This is the value used in the SetFilePrefix method of the
    #@a filePrefix: vtkImageReader.
    #@a voxelDims: List of the dimensions of the voxels (x,y,z)
    #@a logicalDims: List of the extent of the data (i,j,k)
    #@a startImgNum: The number of the first image slice.
    #@a hdrSz: Size of the image header.
    #@a subvol: List of 6 values specifying the min,max in each direction.
    #@note  Some of the key options specified to the vtkImageReader:&p
    #@note  SetDataScalarTypeToShort&p
    #@note  SetDataByteOrderToBigEndian&p
    set rdr       img_GetVOI_rdr
    set streamer  img_GetVOI_streamer
    set voi       img_GetVOI_voi
    set cimg      img_GetVOI_changeinfo
    set rs        img_GetVOI_reslice
    set imgobj    volume_image

    if {[llength $subvol] != 6} {
	return -code error "unexpected extent specification"
    }

    set is [lindex $subvol 0]
    set ie [lindex $subvol 1]
    set js [lindex $subvol 2]
    set je [lindex $subvol 3]
    set ks [lindex $subvol 4]
    set ke [lindex $subvol 5]

    set ldims $logicalDims

    if { ($is < 0) || ($is >= [lindex $ldims 0]) || \
	    ($ie < 0) || ($ie >= [lindex $ldims 0]) || ($ie < $is) || \
	    ($js < 0) || ($js >= [lindex $ldims 1]) || \
	    ($je < 0) || ($je >= [lindex $ldims 1]) || ($je < $js) || \
	    ($ks < 0) || ($ks >= [lindex $ldims 2]) || \
	    ($ke < 0) || ($ke >= [lindex $ldims 2]) || ($ke < $ks) } {
	return -code error "index out of range"
    }

    set imgOrigin [list \
	    [expr [lindex $voxelDims 0] / 2.0] \
	    [expr [lindex $voxelDims 1] / 2.0] \
	    [expr [lindex $voxelDims 2] / 2.0] ]

    if {[cmdExists $rdr]} {
	$rdr Delete
    }
    vtkImageReader $rdr

    global gOptions
    if {$gOptions(show_progress_widget) == 1} {
      # reading
    }

    global gImageVol
    $rdr SetDataScalarTypeTo$gImageVol(pixel_representation)

    # KCW [4/15/00]
    # ---
    # Today, about a year after having gone through all manner of
    # machinations regarding image origins and such in vtk, I now
    # believe that vtkImageReader's FileLowerLeft ivar should be OFF
    # (the default), not on.  The reason we had originally (ahem)
    # turned it on is because this is what causes the first pixel of
    # each slice to be plotted at vtk's world coordinate (x,y) ==
    # (0,0).  Achieving this was part of getting consistency between
    # geometry coordinates in vtk and in matlab.  However, medical
    # image files use a "top left hand corner raster order" in pixel
    # ordering.  HOWEVER, I now realize that "top left hand corner"
    # most likely does NOT refer to the patient's left side.  Rather,
    # it implies the corner that we the viewer see as the upper left
    # corner.  Furthermore, I believe there is an implicit assumption
    # that we're viewing the image slice (at least in the case of
    # coronal slices) as if we're looking at the patient head-on
    # (instead of from the patient's back).  This is most convincingly
    # confirmed by viewing a dataset which includes the subject's
    # heart, which serves as a pretty conclusive asymmetric landmark.

    # --> OH WELL.

    #    $rdr FileLowerLeftOn

    $rdr SetFileDimensionality 2       ;# This is _per_ _file_.
    $rdr SetFilePrefix $filePrefix
    #$rdr SetFilePattern "\%s.\%03d"
    $rdr SetFilePattern $filePattern
    $rdr SetDataByteOrderTo$gImageVol(endian)   ;# SPARC: big endian
                                       ;# MIPS:  big endian / bi-endian
                                       ;# x86:   little endian
    $rdr SetDataSpacing [lindex $voxelDims 0] [lindex $voxelDims 1] \
	    [lindex $voxelDims 2]
    $rdr SetDataExtent 0 [expr [lindex $logicalDims 0] - 1] \
	    0 [expr [lindex $logicalDims 1] - 1] \
	    $startImgNum [expr $startImgNum + [lindex $logicalDims 2] - 1]
    $rdr SetDataOrigin [lindex $imgOrigin 0] [lindex $imgOrigin 1] \
	    [lindex $imgOrigin 2]

    $rdr SetHeaderSize $hdrSz

    # z bounds of VOI must be specified to vtkImageReader within the
    # file numbering indices which were also used with SetDataExtent:
    $rdr SetDataVOI $is $ie $js $je \
	    [expr $ks + $startImgNum] [expr $ke + $startImgNum]

#    $rdr SetTransform $xform

    #$rdr ReleaseDataFlagOff
    $rdr UpdateInformation
    $rdr Update

    # don't need the streamer anymore since we
    # need to transform the volume data

    #if {[cmdExists $streamer]} {
    #	$streamer Delete
    #}
    #vtkImageDataStreamer $streamer
    #$streamer SetInputDataObject [$rdr GetOutput]
    #$streamer SetMemoryLimit 100000  ;# memory limit in kB
    #$streamer SetSplitModeToBlock
    #$streamer UpdateInformation
    #$streamer Update

    global gOptions
    global gImageVol

    # if the format of the data is generic, we cannot orient
    # the data so it is just passed straight through
    if {$gOptions(image_data_type) == "generic"} {
      # update some global parameters
      global gImageVol
      set gImageVol(vtk_org_ras) [[$rdr GetOutput] GetOrigin]
      set ext [[$rdr GetOutput] GetExtent]
      set gImageVol(voi_ras) $ext
      set gImageVol(vdims_ras) [[$rdr GetOutput] GetSpacing]
      set gImageVol(ext_ras) [list [expr [lindex $ext 1]-[lindex $ext 0] + 1] \
                                   [expr [lindex $ext 3]-[lindex $ext 2] + 1] \
                                   [expr [lindex $ext 5]-[lindex $ext 4] + 1]]

      # create a repository vtkImg object
      set imgobj volume_image
      catch {repos_delete -obj $imgobj}
      repos_importVtkImg -src [$rdr GetOutput] -dst $imgobj

      catch {$rdr Delete}
      catch {$streamer Delete}
      catch {$voi Delete}
      catch {$cimg Delete}
      catch {$rs Delete}

      return [repos_exportToVtk -src $imgobj]
    }

    if {$gOptions(orientImgVolToRAS) == 0} {
       catch {repos_delete -obj $imgobj}
       repos_importVtkImg -src [$rdr GetOutput] -dst $imgobj
       catch {$rdr Delete}
       catch {$streamer Delete}
       catch {$voi Delete}
       catch {$cimg Delete}
       catch {$rs Delete}
       set gImageVol(vtk_org_xyz) [[repos_exportToVtk -src $imgobj] GetOrigin]
       return [repos_exportToVtk -src $imgobj]
    }

    catch {$rs Delete}
    vtkImageReslice $rs
    $rs SetInputDataObject [$rdr GetOutput]

    if {$gOptions(show_progress_widget) == 1} {
      # reorienting
    }

    if {$gImageVol(file_hdr_size) == 0} {
       # must explicitly specify the direction cosines and
       # origin if there is no header info
       if {([info exists gImageVol(directionCosines)] == 0) || \
	       ([info exists gImageVol(min_RAS)] == 0)} {
          return -code error "You cannot orient volume to RAS without explicitly specifying directionCosines and min_RAS when the image has no header information."
       }
       set dircosX [lindex $gImageVol(directionCosines) 0]
       set dircosY [lindex $gImageVol(directionCosines) 1]
       set dircosZ [lindex $gImageVol(directionCosines) 2]
       $rs SetResliceAxesDirectionCosines [lindex $dircosX 0] [lindex $dircosX 1] [lindex $dircosX 2] \
                                          [lindex $dircosY 0] [lindex $dircosY 1] [lindex $dircosY 2] \
                                          [lindex $dircosZ 0] [lindex $dircosZ 1] [lindex $dircosZ 2]
    } else {
       # okay, now the fun begions.  Get slice direction orientation
       img_findPCMRIorientation [format $filePattern $filePrefix $startImgNum] xyz directions

       if {$xyz == "r s a"} {
	 set gImageVol(directionCosines) {{-1 0 0}  {0 0 1} {0 1 0}}
         $rs SetResliceAxesDirectionCosines -1 0 0   0 0 1   0 1 0
       } elseif {$xyz == "r a s"} {
	 set gImageVol(directionCosines) {{-1 0 0} {0 1 0} {0 0 -1}}
         $rs SetResliceAxesDirectionCosines -1 0 0   0 1 0   0 0 -1
       } elseif {$xyz == "a s r"} {
	 if {$gImageVol(vdims_z_signed) < 0} {
	   set gImageVol(directionCosines)   {{0 0 1}  {-1 0 0} {0 1 0}}
           $rs SetResliceAxesDirectionCosines  0 0 1    -1 0 0   0 1 0
	 } else {
	   set gImageVol(directionCosines)   {{0 0 -1} {-1 0 0} {0 1 0}}
           $rs SetResliceAxesDirectionCosines  0 0 -1   -1 0 0   0 1 0
	 }
       } elseif {$xyz == "a r s"} {
	 set gImageVol(directionCosines) {{0 1 0} {1 0 0} {0 0 -1}}
         $rs SetResliceAxesDirectionCosines  0 1 0  1 0 0  0 0 -1
       } else {
         puts "ERROR:  Slice orientation not handled!"
         return -code error "ERROR:  Slice orientation not handled."
       }
   }

    $rs Update

    global gImageVol

    catch {$cimg Delete}
    vtkImageChangeInformation $cimg

    $cimg SetInputDataObject [$rs GetOutput]
    $cimg SetInformationInputData [$rs GetOutput]

    # recall that the vtk origin is defined at the centroid of the bottom
    # lower left corner voxel, not the grid origin as defined in the image
    # header
    set spacing [[$rs GetOutput] GetSpacing]
    set org_x [expr [lindex $gImageVol(min_RAS) 0] + [lindex $spacing 0]/2.0]
    set org_y [expr [lindex $gImageVol(min_RAS) 1] + [lindex $spacing 1]/2.0]
    set org_z [expr [lindex $gImageVol(min_RAS) 2] + [lindex $spacing 2]/2.0]

    $cimg SetOutputOrigin $org_x $org_y $org_z
    $cimg Update

    set image [$cimg GetOutput]

    # update some global parameters
    global gImageVol
    set gImageVol(vtk_org_ras) [$image GetOrigin]
    set ext [$image GetExtent]
    set gImageVol(voi_ras) $ext
    set gImageVol(vdims_ras) [$image GetSpacing]
    set gImageVol(ext_ras) [list [expr [lindex $ext 1]-[lindex $ext 0] + 1] \
                                 [expr [lindex $ext 3]-[lindex $ext 2] + 1] \
                                 [expr [lindex $ext 5]-[lindex $ext 4] + 1]]

    # create a repository vtkImg object
    set imgobj volume_image
    catch {repos_delete -obj $imgobj}
    repos_importVtkImg -src $image -dst $imgobj

    catch {$rdr Delete}
    catch {$streamer Delete}
    catch {$voi Delete}
    catch {$cimg Delete}
    catch {$rs Delete}

    return [repos_exportToVtk -src $imgobj]

}


# ----------------
# img_readSlice_5X
# ----------------
# Returns a vtk object name.

proc img_readSlice_5X {fn dims spacing hdrSz} {
    #@author Ken Wang
    #@c Read in a 2-D image file from disk.
    #@a fn: Image filename.
    #@a spacing: List of the dimensions of the voxels (x,y)
    #@a dims: List of the extent of the data (i,j)
    #@a hdrSz: Size of the image header.
    #@note  Some of the key options specified to the vtkImageReader:&p
    #@note  SetDataScalarTypeToShort&p
    #@note  SetDataByteOrderToBigEndian&p
    #@r Returns a vtkImageData object.
    if {![file exists $fn]} {
	return -code error "couldn't find file $fn"
    }

    set rdr __img_readSlice_reader
    catch {$rdr Delete}
    vtkImageReader $rdr

    # glob also does tilde filename expansion:
    $rdr SetFileName [glob $fn]
    $rdr SetFileDimensionality 2
    $rdr SetDataScalarTypeToShort
    $rdr SetDataByteOrderToBigEndian   ;# SPARC: big endian
                                       ;# MIPS:  big endian / bi-endian
                                       ;# x86:   little endian
    $rdr SetDataExtent 0 [expr [lindex $dims 0] - 1] \
	    0 [expr [lindex $dims 1] - 1] \
	    1 1
    eval $rdr SetDataSpacing $spacing
    $rdr SetDataOrigin [expr [lindex $spacing 0] / 2.0] \
	    [expr [lindex $spacing 1] / 2.0] \
	    0.0
    $rdr SetHeaderSize $hdrSz
#    $rdr FileLowerLeftOn
    $rdr Update

    return [$rdr GetOutput]
}


# -------------------
# img_readSliceROI_5X
# -------------------
# Returns a vtk object name.

proc img_readSliceROI_5X {fn dims spacing hdrSz roi} {
    #@author Ken Wang
    #@c Read in a region of interest from a 2-D image file.
    #@a fn: Image filename.
    #@a spacing: List of the dimensions of the voxels (x,y)
    #@a dims: List of the extent of the data (i,j)
    #@a hdrSz: Size of the image header.
    #@a roi: List (minX, maxX, minY, maxY)
    #@note  Some of the key options specified to the vtkImageReader:&p
    #@note  SetDataScalarTypeToShort&p
    #@note  SetDataByteOrderToBigEndian&p
    #@r Returns a vtkImageData object.
    if {![file exists $fn]} {
	return -code error "couldn't find file $fn"
    }

    set rdr __img_readSlice_reader

    catch {$rdr Delete}
    vtkImageReader $rdr

    # glob also does tilde filename expansion:
    $rdr SetFileName [glob $fn]
    $rdr SetFileDimensionality 2
    $rdr SetDataScalarTypeToShort
    $rdr SetDataByteOrderToBigEndian   ;# SPARC: big endian
                                       ;# MIPS:  big endian / bi-endian
                                       ;# x86:   little endian
    $rdr SetDataExtent 0 [expr [lindex $dims 0] - 1] \
	    0 [expr [lindex $dims 1] - 1] \
	    1 1
    eval $rdr SetDataSpacing $spacing
    $rdr SetDataOrigin [expr [lindex $spacing 0] / 2.0] \
	    [expr [lindex $spacing 1] / 2.0] \
	    0.0
    $rdr SetHeaderSize $hdrSz
#    $rdr FileLowerLeftOn
    $rdr SetDataVOI [lindex $roi 0] [lindex $roi 1] \
	    [lindex $roi 2] [lindex $roi 3] \
	    0 0
    $rdr Update

    return [$rdr GetOutput]
}


# -----------
# img_magGrad
# -----------
# Takes in a repository name, but returns a vtk object name.

proc img_magGrad {img} {
    #@author Ken Wang
    #@c Takes a repository imageData object and returns
    #@c the magnitude of the gradient calculated using
    #@c vtkImageGradientMagnitude.
    #@a img: repository ImageData object.
    #@r vtkImageData object.
    #@danger  Always remember this routine takes a
    #@danger  repository object but returns a vtk object!

    set cast __img_magGrad_cast
    set gm   __img_magGrad_gm

    if {![repos_exists -obj $img]} {
	return -code error "$img not found in repository"
    }

    catch {$cast Delete}
    catch {$gm Delete}
    vtkImageCast $cast
    vtkImageGradientMagnitude $gm

    set vtkimg [repos_exportToVtk -src $img]

    $cast SetInputDataObject $vtkimg
    $cast SetOutputScalarTypeToFloat
    $cast Update

    $gm HandleBoundariesOn
    $gm SetInputDataObject [$cast GetOutput]
    $gm Update

    return [$gm GetOutput]
}


# -----------
# img_contour
# -----------

proc img_contour {vtkImg val} {
    #@author Ken Wang
    #@c Threshold a vtkImageData object using the vtkContourFilter.
    #@a vtkImg: vtkImageData object.
    #@a val: Threshold value.
    #@r vtkPolyData object of the result.
    set con __img_magGrad_con

    if {![cmdExists $con]} {
	vtkContourFilter $con
    }

    $con SetInputDataObject $vtkImg
    $con SetValue 0 $val
    $con Update

    return [$con GetOutput]
}

# ------------------
# img_guessVolParams
# ------------------

proc img_guessVolParams {filename} {

  # @author Nathan Wilson
  # @c  This routine guesses the appropriate parameters
  # @c  needed for a volume image data set.
  # @c  It starts by reading the voxel dimensions (x,y) and
  # @c  and the extent (x,y) from the image header for the given filename.
  # @c  To get the extent in the z direction, it assumes that all of the
  # @c  image files contained in the same directory are for that volume dataset.
  # @c  It then simple counts the number of image files in the base directory of
  # @c  filename.  To calculate the physical voxel dimension in the z-direction, it
  # @c  reads the next sequentially numbered file and subtracts the z-coord of the
  # @c  top left hand corner.
  # @note  This routine assumes mr data, and that the slices were acquired in
  # @note  in the "usual" way, i.e. xy-slices of data.  It should be easy to modify
  # @note  this routine so that it detects the actual orientation of the acquired
  # @note  images and acts appropriately.
  # @a filename:  first image file in volume sequence
  # @r status

  if {[file exists $filename] == 0} {
    puts "Error:  Filename $filename does not exist (or you don't have permission to read)."
    return -code error "Error:  Filename $filename does not exist (or you don't have permission to read)."
  }

  set speed {}
  set collapse {}
  set r {}
  set a {}
  set s {}
  set mag {}

  if {[file isdirectory $filename] == 0} {
     set filename [file dirname $filename]
  }
  if [catch {glob [file join $filename I.*]}] {
    puts "ERROR: No image slices found in directory $filename."
    return -code error "ERROR: No image slices found in directory $filename."
  }

  foreach i [lsort -dictionary [glob [file join $filename I.*]]] {
        if [catch {set header [img_readHeader -file $i]}] continue
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

  if {[llength $speed] == 0} {
        puts "ERROR:  No magnitude image slices found!"
        return -code error "ERROR:  No magnitude image slices found!"
  }

  set filename [lindex [lsort -dictionary $speed] 0]
  global gImageVol
  set gImageVol(filename) $filename

  if {[catch {set params [img_readHeader -file $filename]} rtnstring] == 1} {
    puts "Error:  $rtnstring"
    set gImageVol(filename) {}
    return -code error "Error:  $rtnstring"
  }

  #puts "$filename"
  global gImageVol
  foreach i $params {
    set option [lindex $i 0]
    set value [lindex $i 1]
      if {$option == "extent"} {
       set gImageVol(ext_i) [lindex $value 0]
       set gImageVol(ext_j) [lindex $value 1]
      } elseif {$option == "voxel_dims"} {
       set gImageVol(vdims_x) [lindex $value 0]
       set gImageVol(vdims_y) [lindex $value 1]
      } elseif {$option == "file_hdr_size"} {
       set gImageVol(file_hdr_size) $value
      } elseif {$option == "top_left_corner"} {
       set topLeft $value
       set topLeftA [lindex $value 1]
      }
  }

  # now we assume that all of the image files contained in the
  # current directory are in the volume data set.

  # special case of dicom files with dcm extension
  set dcmext {}
  if {[file extension $filename] == ".dcm"} {
     # set dcmext ".dcm"
      set filename [file rootname $filename]
  }

  set rootname [file rootname $filename]
  set number [string trim [file extension $filename] {.}]
  # for some bizarre reason integers cant start with a zero
  # or they are considered octal, so...
  set number [string trimleft $number 0]
  set gImageVol(ext_k) [llength $speed]

  # now find top left corner for next slice
  incr number
  set nextSliceFile [format $gImageVol(filePattern) $rootname $number]

  if {[file exists $nextSliceFile] == 0} {
    puts "Error:  Filename $nextSliceFile does not exist (or you don't have permission to read)."
    return -code error "Error:  Filename $nextSliceFile does not exist (or you don't have permission to read)."
  }

  if {[catch {set params [img_readHeader -file $nextSliceFile]} rtnstring] == 1} {
    puts "Error:  $rtnstring"
    return -code error "Error:  $rtnstring"
  }

  #puts "$nextSliceFile"

  set gImageVol(vdims_z) 0
  foreach i $params {
    set option [lindex $i 0]
    set value [lindex $i 1]
    set $option $value
  }

  img_findPCMRIorientation $nextSliceFile xyz directions

  switch [lindex $xyz 2] {
    r {
      set myindex 0
    }
    a {
      set myindex 1
    }
    s {
      set myindex 2
    }
  }

  set gImageVol(vdims_z) [expr abs([lindex $topLeft $myindex] - [lindex $top_left_corner $myindex])]
  set gImageVol(vdims_z_signed) [expr [lindex $topLeft $myindex] - [lindex $top_left_corner $myindex]]

  set magdist [math_magnitude [math_subVectors $topLeft $top_left_corner]]
  
  puts "original vdims_z: $gImageVol(vdims_z)"
  puts "setting vdims_z with magnitude of difference: $magdist"
  set gImageVol(vdims_z) $magdist
 
  #puts "$topLeft"
  #puts "$top_left_corner"
  #puts "$gImageVol(vdims_z_signed)"

  # set the volume of interest to the entire extent by default
  set gImageVol(voi_x0) 0
  set gImageVol(voi_y0) 0
  set gImageVol(voi_z0) 0
  set gImageVol(voi_x1) [expr $gImageVol(ext_i) - 1]
  set gImageVol(voi_y1) [expr $gImageVol(ext_j) - 1]
  set gImageVol(voi_z1) [expr $gImageVol(ext_k) - 1]

  #set gImageVol(filename) $filename

  return GDSC_OK
}


# ---------------
# img_guessRASmin
# ---------------

proc img_guessRASmin {filename} {

  # @author Nathan Wilson
  # @c  This routine finds the minimum in RAS coordinates of the volume
  # @c  data set starting with filename, assuming all of the image slices
  # @c  contained in the base directory name are part of the volume data set.
  # @note  This routine assumes mr data.
  # @a filename:  first image file in volume sequence
  # @r status

  # find the minimum in RAS space for all the slices
  set minx 1000000
  set miny 1000000
  set minz 1000000

  if {[file exists $filename] == 0} {
    puts "Error:  Filename $filename does not exist (or you don't have permission to read)."
    return -code error "Error:  Filename $filename does not exist (or you don't have permission to read)."
  }

  set speed {}
  set collapse {}
  set r {}
  set a {}
  set s {}
  set mag {}

  if {[file isdirectory $filename] == 0} {
     set filename [file dirname $filename]
  }
  if [catch {glob [file join $filename I.*]}] {
    puts "ERROR: No image slices found in directory $filename."
    return -code error "ERROR: No image slices found in directory $filename."
  }

  foreach i [lsort -dictionary [glob [file join $filename I.*]]] {
        if [catch {set header [img_readHeader -file $i]}] continue
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

  if {[llength $speed] == 0} {
        puts "ERROR:  No magnitude image slices found!"
        return -code error "ERROR:  No magnitude image slices found!"
  }

  set image_files $speed

  foreach image $image_files {
    if {[file exists $image] == 0} {
      puts "Error:  Filename $image does not exist (or you don't have permission to read)."
      return -code error GDSC_ERR
    }

    if {[catch {set params [img_readHeader -file $image]} rtnstring] == 1} {
      puts "Error:  $rtnstring"
      return -code error GDSC_ERR
    }

    foreach i $params {
      set option [lindex $i 0]
      if {$option == "top_left_corner"} {
       set topLeft [lindex $i 1]
      }
      if {$option == "top_right_corner"} {
       set topRight [lindex $i 1]
      }
      if {$option == "bottom_right_corner"} {
       set botRight [lindex $i 1]
      }
    }

    # calculate lower left hand corner
    set tmpvect [list [expr double([lindex $botRight 0]) - double([lindex $topRight 0])] \
                    [expr double([lindex $botRight 1]) - double([lindex $topRight 1])] \
                    [expr double([lindex $botRight 2]) - double([lindex $topRight 2])]]

    set botLeft [list [expr double([lindex $topLeft 0]) + double([lindex $tmpvect 0])] \
                   [expr double([lindex $topLeft 1]) + double([lindex $tmpvect 1])] \
                   [expr double([lindex $topLeft 2]) + double([lindex $tmpvect 2])]]

    # now find minimum  of all points
    set points [list $topLeft $topRight $botRight $botLeft]
    foreach point $points {
      if {[lindex $point 0] < $minx} {
        set minx [lindex $point 0]
      }
      if {[lindex $point 1] < $miny} {
        set miny [lindex $point 1]
      }
      if {[lindex $point 2] < $minz} {
        set minz [lindex $point 2]
      }
    }
  }

  global gImageVol
  set gImageVol(min_RAS) [list $minx $miny $minz]

  return GDSC_OK
}


# ---------------
# img_guessRASmax
# ---------------

proc img_guessRASmax {filename} {

  # @author Nathan Wilson
  # @c  This routine finds the maximum in RAS coordinates of the volume
  # @c  data set starting with filename, assuming all of the image slices
  # @c  contained in the base directory name are part of the volume data set.
  # @note  This routine assumes mr data.
  # @a filename:  first image file in volume sequence
  # @r status

  # find the minimum in RAS space for all the slices
  set maxx -1000000
  set maxy -1000000
  set maxz -1000000

  if {[file exists $filename] == 0} {
    puts "Error:  Filename $filename does not exist (or you don't have permission to read)."
    return -code error "Error:  Filename $filename does not exist (or you don't have permission to read)."
  }

  set speed {}
  set collapse {}
  set r {}
  set a {}
  set s {}
  set mag {}

  if {[file isdirectory $filename] == 0} {
     set filename [file dirname $filename]
  }
  if [catch {glob [file join $filename I.*]}] {
    puts "ERROR: No image slices found in directory $filename."
    return -code error "ERROR: No image slices found in directory $filename."
  }

  foreach i [lsort -dictionary [glob [file join $filename I.*]]] {
        if [catch {set header [img_readHeader -file $i]}] continue
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

  if {[llength $speed] == 0} {
        puts "ERROR:  No magnitude image slices found!"
        return -code error "ERROR:  No magnitude image slices found!"
  }

  set image_files $speed

  foreach image $image_files {
    if {[file exists $image] == 0} {
      puts "Error:  Filename $image does not exist (or you don't have permission to read)."
      return -code error GDSC_ERR
    }

    if {[catch {set params [img_readHeader -file $image]} rtnstring] == 1} {
      puts "Error:  $rtnstring"
      return -code error GDSC_ERR
    }

    foreach i $params {
      set option [lindex $i 0]
      if {$option == "top_left_corner"} {
       set topLeft [lindex $i 1]
      }
      if {$option == "top_right_corner"} {
       set topRight [lindex $i 1]
      }
      if {$option == "bottom_right_corner"} {
       set botRight [lindex $i 1]
      }
    }

    # calculate lower left hand corner
    set tmpvect [list [expr double([lindex $botRight 0]) - double([lindex $topRight 0])] \
                    [expr double([lindex $botRight 1]) - double([lindex $topRight 1])] \
                    [expr double([lindex $botRight 2]) - double([lindex $topRight 2])]]

    set botLeft [list [expr double([lindex $topLeft 0]) + double([lindex $tmpvect 0])] \
                   [expr double([lindex $topLeft 1]) + double([lindex $tmpvect 1])] \
                   [expr double([lindex $topLeft 2]) + double([lindex $tmpvect 2])]]

    # now find minimum  of all points
    set points [list $topLeft $topRight $botRight $botLeft]
    foreach point $points {
      if {[lindex $point 0] > $maxx} {
        set maxx [lindex $point 0]
      }
      if {[lindex $point 1] > $maxy} {
        set maxy [lindex $point 1]
      }
      if {[lindex $point 2] > $maxz} {
        set maxz [lindex $point 2]
      }
    }
  }

  global gImageVol
  set gImageVol(max_RAS) [list $maxx $maxy $maxz]

  return GDSC_OK
}


# -------------------------------
# img_findObliqueCornersMinMaxRAS
# -------------------------------

proc img_findObliqueCornersMinMaxRAS {filename} {

  # @author Nathan Wilson
  # @c  This routine finds the min max corners in RAS coordinates of the volume
  # @c  data set starting with filename, assuming all of the image slices
  # @c  contained in the base directory name are part of the volume data set.
  # @note  This routine assumes mr data.
  # @a filename:  first image file in volume sequence
  # @r status

  if {[file exists $filename] == 0} {
    puts "Error:  Filename $filename does not exist (or you don't have permission to read)."
    return -code error "Error:  Filename $filename does not exist (or you don't have permission to read)."
  }

  set speed {}
  set collapse {}
  set r {}
  set a {}
  set s {}
  set mag {}

  if {[file isdirectory $filename] == 0} {
     set filename [file dirname $filename]
  }
  if [catch {glob [file join $filename I.*]}] {
    puts "ERROR: No image slices found in directory $filename."
    return -code error "ERROR: No image slices found in directory $filename."
  }

  foreach i [lsort -dictionary [glob [file join $filename I.*]]] {
        if [catch {set header [img_readHeader -file $i]}] continue
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

  if {[llength $speed] == 0} {
        puts "ERROR:  No magnitude image slices found!"
        return -code error "ERROR:  No magnitude image slices found!"
  }

  set image_files $speed

  # find the minimum in RAS space for all the slices
  foreach corner [list topLeft topRight botRight botLeft] {
    set minx($corner) 1000000
    set miny($corner) 1000000
    set minz($corner) 1000000
    set maxx($corner) -1000000
    set maxy($corner) -1000000
    set maxz($corner) -1000000
  }

  set topLeftfp  [open topLeft    w]
  set topRightfp [open topRight   w]
  set botLeftfp  [open botLeftfp  w]
  set botRightfp [open botRightfp w]

  foreach image $image_files {
    if {[file exists $image] == 0} {
      puts "Error:  Filename $image does not exist (or you don't have permission to read)."
      return -code error GDSC_ERR
    }

    if {[catch {set params [img_readHeader -file $image]} rtnstring] == 1} {
      puts "Error:  $rtnstring"
      return -code error GDSC_ERR
    }

    foreach i $params {
      set option [lindex $i 0]
      if {$option == "top_left_corner"} {
       set topLeft [lindex $i 1]
      }
      if {$option == "top_right_corner"} {
       set topRight [lindex $i 1]
      }
      if {$option == "bottom_right_corner"} {
       set botRight [lindex $i 1]
      }
    }

    # calculate lower left hand corner
    set tmpvect [list [expr double([lindex $botRight 0]) - double([lindex $topRight 0])] \
                    [expr double([lindex $botRight 1]) - double([lindex $topRight 1])] \
                    [expr double([lindex $botRight 2]) - double([lindex $topRight 2])]]

    set botLeft [list [expr double([lindex $topLeft 0]) + double([lindex $tmpvect 0])] \
                   [expr double([lindex $topLeft 1]) + double([lindex $tmpvect 1])] \
                   [expr double([lindex $topLeft 2]) + double([lindex $tmpvect 2])]]

    puts $topLeftfp $topLeft
    puts $topRightfp $topRight
    puts $botLeftfp $botLeft
    puts $botRightfp $botRight

    # now find minimum and max for each corner
    foreach corner [list topLeft topRight botRight botLeft] {
      eval set point \$$corner
      #puts "$corner $point"
      if {([lindex $point 0] < $minx($corner) || [expr abs([lindex $point 0] - $minx($corner))] < 0.001 ) && \
          ([lindex $point 1] < $miny($corner) || [expr abs([lindex $point 1] - $miny($corner))] < 0.001 ) && \
          ([lindex $point 2] < $minz($corner) || [expr abs([lindex $point 2] - $minz($corner))] < 0.001 )} {
        set minx($corner) [lindex $point 0]
        set miny($corner) [lindex $point 1]
        set minz($corner) [lindex $point 2]
        puts "min($corner) $minx($corner) $miny($corner) $minz($corner)"
      }
      if {([lindex $point 0] > $maxx($corner) || [expr abs([lindex $point 0] - $maxx($corner))] < 0.001 ) && \
          ([lindex $point 1] > $maxy($corner) || [expr abs([lindex $point 1] - $maxy($corner))] < 0.001 ) && \
          ([lindex $point 2] > $maxz($corner) || [expr abs([lindex $point 2] - $maxz($corner))] < 0.001 )} {
        set maxx($corner) [lindex $point 0]
        set maxy($corner) [lindex $point 1]
        set maxz($corner) [lindex $point 2]
        puts "max($corner) $minx($corner) $miny($corner) $minz($corner)"
      }
    }

  }

  close $topLeftfp
  close $topRightfp
  close $botLeftfp
  close $botRightfp

  global gImageVol

  foreach corner [list topLeft topRight botRight botLeft] {
    set gImageVol(min_RAS_$corner) [list $minx($corner) $miny($corner) $minz($corner)]
    set gImageVol(max_RAS_$corner) [list $maxx($corner) $maxy($corner) $maxz($corner)]
  }

  return GDSC_OK
}


# -----------------------
# img_calculateStatistics
# -----------------------

proc img_calculateStatistics {img rtnAvg rtnDeviation rtnMin rtnMax} {

  #@author Nathan Wilson
  #@c This code calculates the average value and standard deviation
  #@c for the pixel values of an image (assuming a normal distribution).
  #@a img:  Repository ImageData slice.
  #@a rtnAvg: Returned average value.
  #@a rtnDeviation: Returned standard deviation.
  #@a rtnMin: Returned minimum value.
  #@a rtnMax: Returned maximum value.
  #@note  This code really should be written in C, but I'm to lazy.

  upvar $rtnAvg avg
  upvar $rtnDeviation deviation
  upvar $rtnMin min
  upvar $rtnMax max

  if {[repos_exists -obj $img] == 0} {
    puts "ERROR: Object $img does not exist."
    return -code error "ERROR: Object $img does not exist."
  }
  if {[repos_type -obj $img] != "StructuredPts"} {
    puts "ERROR: Object $img not of type StructuredPts."
    return -code error "ERROR: Object $img not of type StructuredPts."
  }

  set sp [repos_exportToVtk -src $img]
  set scalars [[$sp GetPointData] GetScalars]
  set numScalars [$scalars GetNumberOfTuples]

  # caclulate the average value
  set total 0
  for {set i 0} {$i < $numScalars} {incr i} {
    set total [expr $total + [$scalars GetTuple1 $i]]
  }
  set avg [expr double($total)/double($numScalars)]

  # calculate the variance
  set total 0
  for {set i 0} {$i < $numScalars} {incr i} {
    set total [expr $total + pow(double([$scalars GetTuple1 $i] - $avg),2)]
  }
  set variance [expr double($total)/double($numScalars)]

  set min [lindex [$scalars GetRange] 0]
  set max [lindex [$scalars GetRange] 1]

  # calculate the standard deviation
  set deviation [expr sqrt(double($variance))]

  #debugging code
  #puts "number of scalars: $numScalars"
  #puts "scalar range: [$scalars GetRange]"
  #puts "average: $avg"
  #puts "variance: $variance"
  #puts "std dev: $deviation"
  #puts "2 sigma out: [expr double($deviation) * 2.0 + double($avg)]"
  #puts "3 sigma out: [expr double($deviation) * 3.0 + double($avg)]"
  #puts "0.33 * max: [expr 0.33*$max]"
  #puts "0.20 * max: [expr 0.2*$max]"

  return GDSC_OK

}


# -----------------
# img2_readSlice_5X
# -----------------

proc img2_readSlice_5X {filename rtnImg} {

  #@author Nathan Wilson
  #@c Read in a slice of image data.
  #@a filename: image filename.
  #@a rtnImg: Repository ImageData object to create.
  #@r status

  if {[file exists $filename] == 0} {
    puts "Error:  Filename $filename does not exist (or you don't have permission to read)."
    return -code error GDSC_ERR
  }

  #  The following few lines will read and create the following variables:
  #
  #  extent
  #  voxel_dims
  #  file_hdr_size
  #  top_left_corner
  #  top_right_corner
  #  bottom_right_corner
  #

  set imageInfo [img_readHeader -file $filename]
  foreach i $imageInfo {
    set [lindex $i 0] [lindex $i 1]
  }

  # read in the image data
  set fn $filename
  set dims [list [lindex $extent 0] [lindex $extent 1] 0]
  set spacing [list [lindex $voxel_dims 0] [lindex $voxel_dims 1] 0]
  set hdrSz $file_hdr_size

  set myslice [img_readSlice_5X $fn $dims $spacing $hdrSz]

  repos_importVtkImg -src $myslice -dst $rtnImg

  # delete here?
  $myslice Delete

  return GDSC_OK
}


# --------------------
# img2_readSliceROI_5X
# --------------------

proc img2_readSliceROI_5X {filename rtnImg roi} {

  #@author Nathan Wilson
  #@c Read in a slice of image data.
  #@a filename: image filename.
  #@a rtnImg: Repository ImageData object to create.
  #@a roi: List (minX, maxX, minY, maxY)
  #@r status

  if {[file exists $filename] == 0} {
    puts "Error:  Filename $filename does not exist (or you don't have permission to read)."
    return -code error GDSC_ERR
  }

  #  The following few lines will read and create the following variables:
  #
  #  extent
  #  voxel_dims
  #  file_hdr_size
  #  top_left_corner
  #  top_right_corner
  #  bottom_right_corner
  #

  set imageInfo [img_readHeader_5X -file $filename]
  foreach i $imageInfo {
    set [lindex $i 0] [lindex $i 1]
  }

  # read in the image data
  set fn $filename
  set dims [list [lindex $extent 0] [lindex $extent 1] 0]
  set spacing [list [lindex $voxel_dims 0] [lindex $voxel_dims 1] 0]
  set hdrSz $file_hdr_size

  set myslice [img_readSliceROI_5X $fn $dims $spacing $hdrSz $roi]

  repos_importVtkImg -src $myslice -dst $rtnImg

  # delete here?
  $myslice Delete

  return GDSC_OK
}


# ------------------
# img_createSegVelPD
# ------------------

proc img_createSegVelPD {velImageX velImageY velImageZ correctX correctY correctZ directions segmentationPolyData ignoreFactor zeroBoundFlag resultPolyData} {

  #@author Nathan Wilson
  #@c  This routine creates a PolyData object containing either through plane or 3-component velocity
  #@c  given in velocityImage(s).  This is done as follows.  First a point
  #@c  classification is performed on each voxel centroid (i.e. is the centroid inside
  #@c  of the segmentation).  If it is, the location (x,y) of the centroid is added
  #@c  to a vtkPoints list.  Associated with the given point is the corresponding velocities
  #@c  of the voxel containing the centroid.  Optionally centroids within a given distance
  #@c  from the boundary can be ignored.  In this case, you specify a non-negative value of
  #@c  of ignoreFactor (which is a multiplier of the grid spacing).  If the distance of
  #@c  the centroid from the lumen wall (segmentation) is greater than ignoreFactor*spacing_in_x
  #@c  the point is included (otherwise it is ignored).  Once we have the point set, we perform
  #@c  a delaunay triangulation of the points to generate a connectivity between nearby points.
  #@c  Note that this usually turns the domain into a convex hall (we do NOT constrain the
  #@c  resulting grid by the segmentation boundary).  The resulting PolyData is returned.  Note
  #@c  that the scalar is also set to the through plane component in addition to the z-direction
  #@c  of the velocity vector.
  #@a velImageX:  Input velocity X image.
  #@a velImageY:  Input velocity Y image.
  #@a velImageZ:  Input velocity Z image.
  #@a correctX:  baseline correction equation for X direction (empty string or 0 == no correction).
  #@a correctY:  baseline correction equation for Y direction (empty string or 0 == no correction).
  #@a correctZ:  baseline correction equation for z direction (empty string or 0 == no correction).
  #@a segmentationPolyData:  Input segmentation boundary (PolyData).
  #@a directions:  a tcl list of length three that contains multipliers of +1 or -1
  #@a directions:  to maintain or change the direction of each velocity comp. as appropriate
  #@a ignoreFactor:  Multiplictive constant (times * spacing_in_x).
  #@a zeroBoundFlag:  whether or not to set velocities of boundary to 0 (1 == set velocities to zero).
  #@a resultPolyData:  Resulting PolyData object.
  #@r status
  #@note  To do through-plane only set velImageX and velImageY to null strings.

  set map(X) 0
  set map(Y) 0
  set map(Z) 0

  foreach i {X Y Z} {
    eval set slice \$velImage$i
    if {$slice == ""} {continue}
    if {[repos_exists -obj $slice] == "0"} {
      puts "ERROR:  Input Image $slice doesn't exist."
      return -code error GDSC_ERROR
    }
    if {[repos_type -obj $slice] != "StructuredPts"} {
      puts "ERROR:  Object $slice not of type Image."
      return -code error GDSC_ERROR
    }
    set image($i) [repos_exportToVtk -src $slice]
    set imageScalars($i) [[$image($i) GetPointData] GetScalars]
    set map($i) 1
  }
  if {$map(Z) == 0} {
    puts "ERROR:  Must have through plane component of velocity!"
    return -code error GDSC_ERROR
  }

  if {[repos_exists -obj $segmentationPolyData] == "0"} {
    puts "ERROR:  Input PolyData $segmentationPolyData doesn't exist."
    return -code error GDSC_ERROR
  }
  if {[repos_type -obj $segmentationPolyData] != "PolyData"} {
    puts "ERROR:  Object $segmentationPolyData not of type PolyData."
    return -code error GDSC_ERROR
  }
  if {[repos_exists -obj $resultPolyData] == "1"} {
    puts "ERROR:  Output object $resultPolyData exists."
    return -code error GDSC_ERROR
  }

  if {[llength $directions] != 3} {
    puts "ERROR:  Incorrect length of directions parameter ([llength $directions] != 3)"
    return -code error GDSC_ERROR
  }

  # convert empty strings to zero values
  if {[string trim $correctX] == ""} {
     set correctX 0
  }
  if {[string trim $correctY] == ""} {
     set correctY 0
  }
  if {[string trim $correctZ] == ""} {
     set correctZ 0
  }

  set vPoints tmp-ctpvm-points
  set vScalars tmp-ctpvm-scalars
  set vVectors tmp-ctpvm-vectors
  set vPolyData tmp-ctpvm-polydata
  set delaunay tmp-ctpvm-delaunay

  catch {$vPoints Delete}
  catch {$vScalars Delete}
  catch {$vVectors Delete}
  catch {$vPolyData Delete}
  catch {$delaunay Delete}

  # get vtk pointers
  set segvtkobj [repos_exportToVtk -src $segmentationPolyData]

  # image parameters
  # assume all the same as Z image
  set origin [$image(Z) GetOrigin]
  set spacing [$image(Z) GetSpacing]
  set extent [$image(Z) GetExtent]
  set dims [list [expr [lindex $extent 1]+1] [expr [lindex $extent 3]+1]]

  set numScalars [$imageScalars(Z) GetNumberOfTuples]

  # create a new set of points, scalars, and vectors
  vtkPoints $vPoints
  $vPoints Allocate 100 100
  vtkFloatArray $vScalars
  $vScalars Allocate 100 100
  vtkFloatArray $vVectors; $vVectors SetNumberOfComponents 3
  $vVectors Allocate 100 100

  # inserting a through plane component of zero for each boundary point
  # if user specifies this;  otherwise, assign the corresponding velocity
  # in the image to this point. 
  # also zero z-component of point?
   if {$zeroBoundFlag == 1} {      
       for {set i 0} {$i < [$segvtkobj GetNumberOfPoints]} {incr i} {
	   set pt [$segvtkobj GetPoint $i]
	   $vPoints InsertNextPoint [lindex $pt 0] [lindex $pt 1] 0.0
	   $vScalars InsertNextTuple1 0.0
	   $vVectors InsertNextTuple3 0 0 0
	   #puts "([lindex $pt 0],[lindex $pt 1]) 0.0   boundary"
       }
   } else {
 for {set i 0} {$i < [$segvtkobj GetNumberOfPoints]} {incr i} {
      
	  # Initialize variables
	  set vx 0
	  set vy 0

	  set pt [$segvtkobj GetPoint $i]
	  set x [lindex $pt 0]
          set y [lindex $pt 1]

	  # Need to find corresponding pixel number for this point.  Since this is a
	  # structured data set, coordinates of origin represent centroid for pixel at
	  # bottom-left of image instead of bottom-left corner of image.  Adjust 
	  # origin just for calculation purposes.
	  set adjOrig [math_subVectors $origin [math_scaleVec $spacing 0.5]]
	  set rowIx [expr int([expr [expr [lindex $pt 1] - [lindex $adjOrig 1]] / [lindex $spacing 1]])]
	  set colIx [expr int([expr [expr [lindex $pt 0] - [lindex $adjOrig 0]] / [lindex $spacing 0]])]
	  set pixel [expr $rowIx * [lindex $dims 0] + $colIx]
	  
	  #  Here's a check to make sure we computed the right pixel number
	  set pixcoord [$image(Z) GetPoint $pixel]
	  set coordDiff [math_subVectors $pixcoord $pt]
	  set coordDiffX [expr abs([lindex $coordDiff 0])]
	  set coordDiffY [expr abs([lindex $coordDiff 1])]
	  set coordDiffZ [expr abs([lindex $coordDiff 2])]

	  #puts "pixel $pixel ($rowIx, $colIx): $pixcoord"
	  #puts "desired point:  $pt"
	  #puts "difference:  $coordDiff"
	    
	  if {$coordDiffX > [expr 1.01*([lindex $spacing 0] / 2.0)] || \
	      $coordDiffY > [expr 1.01*([lindex $spacing 1] / 2.0)] || \
	      $coordDiffZ > [expr 1.01*([lindex $spacing 2] / 2.0)]} {
	           puts "Error:  Wrong point found"
	           puts "pixel $pixel ($rowIx, $colIx): $pixcoord"
	           puts "desired point:  $pt"
	           puts "difference:  $coordDiff"
	           return
	  } 

	  
	  set pixval [$imageScalars(Z) GetTuple1 $pixel]
	  set vz [expr double([lindex $directions 2])*($pixval-[expr $correctZ])]
	  if {$map(X) == 1} {
	      set pixval [$imageScalars(X) GetTuple1 $pixel]
	      set vx [expr double([lindex $directions 0])*($pixval-[expr $correctX])]
	  }
	  if {$map(Y) == 1} {
	      set pixval [$imageScalars(Y) GetTuple1 $pixel]
	      set vy [expr double([lindex $directions 1])*($pixval-[expr $correctY])]
	  }


	  #  Add point
	  $vPoints InsertNextPoint [lindex $pt 0] [lindex $pt 1] 0.0
	  $vScalars InsertNextTuple1 $vz
	  $vVectors InsertNextTuple3 $vx $vy $vz
      }
  }

  geom_ptInPoly -pgn $segmentationPolyData -pt [list 0 0] -usePrevPoly 0
  for {set pixel 0} {$pixel < $numScalars} {incr pixel} {

    # calculate the actual x,y coordinate of the pixel
    set planeIx [expr $pixel / ([lindex $dims 0] * [lindex $dims 1])]
    set planeOffset [expr $pixel % ([lindex $dims 0] * [lindex $dims 1])]
    set rowIx [expr $planeOffset / [lindex $dims 0]]
    set colIx [expr $planeOffset % [lindex $dims 0]]

    set ptX [expr [lindex $origin 0] + ($colIx * [lindex $spacing 0])]
    set ptY [expr [lindex $origin 1] + ($rowIx * [lindex $spacing 1])]

    #puts "planeIx: $planeIx  rowIx: $rowIx  colIx: $colIx pt: $ptX $ptY"

    set classify [geom_ptInPoly -pgn $segmentationPolyData -pt [list $ptX $ptY] -usePrevPoly 1]
    #puts "pt: ($ptX,$ptY) classify: $classify"
    if {$classify == 1} {
      set x $ptX
      set y $ptY
      set distToBoundary [geom_findDistance -obj $segmentationPolyData -pt [list $ptX $ptY 0]]
      set vx 0
      set vy 0
      set pixval [$imageScalars(Z) GetTuple1 $pixel]
      set vz [expr double([lindex $directions 2])*($pixval-[expr $correctZ])]
      if {$map(X) == 1} {
           set pixval [$imageScalars(X) GetTuple1 $pixel]
           set vx [expr double([lindex $directions 0])*($pixval-[expr $correctX])]
      }
      if {$map(Y) == 1} {
           set pixval [$imageScalars(Y) GetTuple1 $pixel]
           set vy [expr double([lindex $directions 1])*($pixval-[expr $correctY])]
      }
      if {$distToBoundary > [expr [lindex $spacing 0]*$ignoreFactor]} {
        $vScalars InsertNextTuple1 $vz
        $vVectors InsertNextTuple3 $vx $vy $vz
        $vPoints InsertNextPoint $ptX $ptY 0
        #puts "($ptX,$ptY) = $vx $vy $vz [$imageScalars(Z) GetTuple1 $pixel]"
      } else {
        #puts "($ptX,$ptY) = $vx $vy $vz  skipped $distToBoundary"
      }

    }

  }

  #puts "number of points: [$vPoints GetNumberOfPoints]"
  #puts "number of scalars: [$vScalars GetNumberOfTuples]"

  # make a point set

  vtkPolyData $vPolyData
  $vPolyData SetPoints $vPoints
  [$vPolyData GetPointData] SetScalars $vScalars
  [$vPolyData GetPointData] SetVectors $vVectors

  # now do delaunay triangulation of the new points
  vtkDelaunay2D $delaunay
  $delaunay SetInputDataObject $vPolyData
  $delaunay Update

  repos_importVtkPd -src [$delaunay GetOutput] -dst $resultPolyData

  # clean up
  catch {$vPoints Delete}
  catch {$vScalars Delete}
  catch {$vVectors Delete}
  catch {$vPolyData Delete}
  catch {$delaunay Delete}

  return GDSC_OK

}


# -----------------------
# img_getSliceAtPathPoint
# -----------------------

proc img_getSliceAtPathPoint {volumeImage path ptId ext rtnImg rtnPot} {

    #@author Nathan Wilson
    #@c  This is a convenience function that creates a vtkImageData
    #@c  slice of a volume data set based on a given pathPlan path
    #@c  and point id.
    #@a volumeImage:  a volume image (StructuredPts) object.
    #@a path:  A tcl list containing exactly one path in pathPlan
    #@a path:  format.
    #@a ptId:  A point id (index in the <a path> list).
    #@a ext:  Extent.
    #@a rtnImg:  Name of StructuredPoints repository object
    #@a rtnImg:  object to create.  It is the image intensity on the
    #@a rtnImg:  slice.
    #@a rtnPot:  Name of StructuredPoints repository object
    #@a rtnPot:  object to create.  It is the magnitude of the intensity
    #@a rtnPot:  gradient for the given slice.
    #@r status

    # should check for errors in the arguments here!

    if {[repos_exists -obj $volumeImage] == 0} {
       return -code error "ERROR:  Object $volumeImage does not exist."
    }
    if {[repos_type -obj $volumeImage] != "StructuredPts"} {
       return -code error "ERROR:  Object $volumeImage not of type StructuredPts."
    }
    set volImg [repos_exportToVtk -src $volumeImage]

    set tr tmp-img-getsliceatpp-tr
    set rs tmp-img-getsliceatpp-rs

    catch {$tr Delete}
    catch {$rs Delete}

    vtkTransform $tr

    # pick a point along the path and calculate a transform
    # needed by the reslice object
    array set items [lindex $path $ptId]
    set pos [TupleToList $items(p)]
    set nrm [TupleToList $items(t)]
    set xhat [TupleToList $items(tx)]
    path_ApplyTransform $tr $pos $nrm $xhat

    vtkImageReslice $rs

    set oimg   [$volImg GetOrigin]       ;# *vtk* image origin
    set vdims  [$volImg GetSpacing]
    set rng    [$volImg GetScalarRange]
    set vmin   [math_minVec $vdims]

    set pdimx [expr [lindex $ext 0] * $vmin]
    set pdimy [expr [lindex $ext 1] * $vmin]

    #set opln [list \
    #	    [expr [lindex $oimg 0] - 0.5 * [lindex $vdims 0] - 0.5 * $pdimx] \
    #    [expr [lindex $oimg 1] - 0.5 * [lindex $vdims 1] - 0.5 * $pdimy] \
    #    0.0]

    # It appears to me (NW) that the origin doesn't belong here at all.  Of
    # course, I might be off by half a pixel now.
    set opln [list \
	    [expr - 0.5 * $vmin - 0.5 * $pdimx] \
	    [expr - 0.5 * $vmin - 0.5 * $pdimy] \
	    0.0]

    set ors [list \
	    [expr [lindex $opln 0] + 0.5 * $vmin] \
	    [expr [lindex $opln 1] + 0.5 * $vmin] \
	    [expr [lindex $opln 2]]]


    $rs SetInputDataObject $volImg

    $rs SetResliceTransform $tr
    $rs SetOutputSpacing $vmin $vmin $vmin
    $rs SetOutputOrigin [lindex $ors 0] [lindex $ors 1] [lindex $ors 2]
    $rs SetOutputExtent 0 [expr [lindex $ext 0] - 1] \
	    0 [expr [lindex $ext 1] - 1] \
	    0 0
    $rs InterpolateOn
    $rs Update

    catch {[repos_delete -obj $rtnImg]}
    catch {[repos_delete -obj $rtnPot]}

    repos_importVtkImg -src [$rs GetOutput] -dst $rtnImg
    # calculate the magnitude of the gradient
    set vtkpot [img_magGrad $rtnImg]
    repos_importVtkImg -src $vtkpot -dst $rtnPot

    catch {$rs Delete}
    catch {$rs Delete}

    return GDSC_OK

}


# ------------------------
# img_findPCMRIorientation
# ------------------------

proc img_findPCMRIorientation {fn rtnxyz rtndirections} {
  #@c Code to find the mapping between x,y,z directions
  #@c and the r,a,s directions for a slice of PCMRI data.
  #@a fn: filename.
  #@a rtnxyz: returned 3-component list of mapping.
  #@a rtndirections: 3-component list of directions corresponding
  #@a rtndirections: that r/a/s are in the same or opposite
  #@a rtndirections: direction of the coordinate axes
  upvar $rtnxyz xyz
  upvar $rtndirections directions

  # check to make sure file exists
  if {[file exists $fn] == 0} {
    puts "ERROR: could not read file $fn"
    return -code error "ERROR:  could not read file $fn"
  }

  set params [img_readHeader -file $fn]

  foreach i $params {
    set [lindex $i 0] [lindex $i 1]
  }

  # calculate top edge of image slice
  set dxTop [expr abs([lindex $top_right_corner 0] - [lindex $bottom_right_corner 0])]
  set dyTop [expr abs([lindex $top_right_corner 1] - [lindex $bottom_right_corner 1])]
  set dzTop [expr abs([lindex $top_right_corner 2] - [lindex $bottom_right_corner 2])]

  set posTop [math_findMaxPos $dxTop $dyTop $dzTop]

  puts "dxTop: $dxTop  dyTop: $dyTop  dzTop: $dzTop  posTop: $posTop"

  # calculate side edge of image slice
  set dxSide [expr abs([lindex $top_left_corner 0] - [lindex $top_right_corner 0])]
  set dySide [expr abs([lindex $top_left_corner 1] - [lindex $top_right_corner 1])]
  set dzSide [expr abs([lindex $top_left_corner 2] - [lindex $top_right_corner 2])]

  set posSide [math_findMaxPos $dxSide $dySide $dzSide]

  puts "dxSide: $dxSide  dySide: $dySide  dzSide: $dzSide  posTop: $posSide"

  set signFlipSide 0
  if {[lindex $top_right_corner $posSide] > [lindex $top_left_corner $posSide]} {
    set signFlipSide 1
  }

  set signFlipTop 0
  if {[lindex $top_right_corner $posTop] > [lindex $bottom_right_corner $posTop]} {
    set signFlipTop 1
  }

  # figure out the third direction
  if {[expr $posSide + $posTop] == 1} {
     set posThru 2
  } elseif {[expr $posSide + $posTop] == 2} {
     set posThru 1
  } elseif {[expr $posSide + $posTop] == 3} {
     set posThru 0
  } else {
     return -code error "ERROR figuring out through plane direction"
  }

  # NOTE:  Table is probably consistently wrong.
  #
  # calculate the direction of the out of plane vector
  #

  #              x-dir-vec  flipped-x   y-dir-vec  flipped-y  *  flip-z

  set o [list   {    s          0          r           0           0   } \
                {    s          1          r           0           1   } \
                {    s          0          r           1           1   } \
                {    s          1          r           1           0   } \
                {    r          0          s           0           1   } \
                {    r          1          s           0           0   } \
                {    r          0          s           1           0   } \
                {    r          1          s           1           1   } \
                {    a          0          s           0           0   } \
                {    a          1          s           0           1   } \
                {    a          0          s           1           1   } \
                {    a          1          s           1           0   } \
                {    s          0          a           0           1   } \
                {    s          1          a           0           0   } \
                {    s          0          a           1           0   } \
                {    s          1          a           1           1   } \
                {    a          0          r           0           1   } \
                {    a          1          r           0           0   } \
                {    a          0          r           1           0   } \
                {    a          1          r           1           1   } \
                {    r          0          a           0           0   } \
                {    r          1          a           0           1   } \
                {    r          0          a           1           1   } \
                {    r          1          a           1           0   }]

  set xdir [lindex {r a s} $posSide]
  set ydir [lindex {r a s} $posTop]

  set signFlipThru -1

  foreach i $o {
    if {([lindex $i 0] == $xdir) && ([lindex $i 1] == $signFlipSide) && \
        ([lindex $i 2] == $ydir) && ([lindex $i 3] == $signFlipTop)} {
        set signFlipThru [lindex $i 4]

        #
        #  HACK!!!  Sign seems to be consistently wrong,
        #   therefore we flip it here!
        #
        if {$signFlipThru == 0} {
          set signFlipThru 1
        } else {
          set signFlipThru 0
        }

        break
    }
  }

  if {$signFlipThru == -1} {
     return -code error "ERROR:  Could not find orientation of slice!"
  }


  set labels(0) {R A S}
  set labels(1) {L P I}

  puts "\n\n\n"
  puts "[format {%+08.3f} [lindex $top_left_corner 0]]        [lindex $labels([expr !$signFlipTop]) $posTop]          [format {%+08.3f} [lindex $top_right_corner 0]]"
  puts "[format {%+08.3f} [lindex $top_left_corner 1]]  ---------------  [format {%+08.3f} [lindex $top_right_corner 1]]"
  puts "[format {%+08.3f} [lindex $top_left_corner 2]]  -             -  [format {%+08.3f} [lindex $top_right_corner 2]]"
  puts "          -             -     "
  puts "    [lindex $labels($signFlipSide) $posSide]     -             -     [lindex $labels([expr !$signFlipSide]) $posSide]"
  puts "          -             -     "
  puts "          -             -     "
  puts "          -             -  [format {%+08.3f} [lindex $bottom_right_corner 0]]   "
  puts "          ---------------  [format {%+08.3f} [lindex $bottom_right_corner 1]]  "
  puts "               [lindex $labels($signFlipTop) $posTop]           [format {%+08.3f} [lindex $bottom_right_corner 2]]"
  puts "\n\n"

  if {$signFlipSide == 0} {
     set signchange "(no sign change)"
  } else {
     set signchange "(multiply by -1)"
  }
  puts "Files for [lindex $labels(0) $posSide]/[lindex $labels(1) $posSide] correspond to X axis $signchange"
  if {$signFlipTop == 0} {
     set signchange "(no sign change)"
  } else {
     set signchange "(multiply by -1)"
  }
  puts "Files for [lindex $labels(0) $posTop]/[lindex $labels(1) $posTop] correspond to Y axis $signchange"
  if {$signFlipThru == 0} {
     set signchange "(no sign change)"
  } else {
     set signchange "(multiply by -1)"
  }
  puts "Files for [lindex $labels(0) $posThru]/[lindex $labels(1) $posThru] correspond to Z axis $signchange"
  puts "\n"
  puts "dxTop: $dxTop  dyTop: $dyTop  dzTop: $dzTop"
  puts "dxSide: $dxSide  dySide: $dySide  dzSide: $dzSide"

  puts "signFlipTop: $signFlipTop  signFlipSide: $signFlipSide  signFlipThru: $signFlipThru"

  set xyz [list $xdir $ydir [lindex {r a s} $posThru]]

  set signX 1
  set signY 1
  set signZ 1
  if {$signFlipSide == 1} {
    set signX -1
  }
  if {$signFlipTop == 1} {
    set signY -1
  }
  if {$signFlipThru == 1} {
    set signZ -1
  }
  set directions [list $signX $signY $signZ]

  set a [math_scaleVec [math_subVectors $top_right_corner $top_left_corner] $signX]
  set b [math_scaleVec [math_subVectors $top_right_corner $bottom_right_corner] $signY]

  puts "xdir: $xdir   ydir: $ydir"

  if {$xdir == "a" && $ydir == "r"} {
     puts "a x r = [math_normalize [math_cross $a $b]]"
     puts "normal_to_plane = $normal_to_plane"
  }
  if {$xdir == "r" && $ydir == "s"} {
     puts "r x s = [math_normalize [math_cross $a $b]]"
     puts "normal_to_plane = $normal_to_plane"
  }
  if {$xdir == "r" && $ydir == "a"} {
     puts "r x a = [math_normalize [math_cross $a $b]]"
     puts "normal_to_plane = $normal_to_plane"
  }
  if {$xdir == "a" && $ydir == "s"} {
     puts "a x s = [math_normalize [math_cross $a $b]]"
     puts "normal_to_plane = $normal_to_plane"
  }

  return GDSC_OK

}


# ----------------------------
# img_calcTransformMatrixToRAS
# ----------------------------

proc img_calcTransformMatrixToRAS {imageFilename rtnMatrix4x4} {

  #@author Nathan Wilson
  #@c Calculates the transformations required to move a single image slice read with
  #@c <p img_readSlice> or <p img_readSliceROI> into the appropriate location in
  #@c 3-D (RAS) space.
  #@a imageFilename:  image file.
  #@a rtnMatrix4x4: vtkMatrix4x4 matrix to transform image into 3-D space.
  #@note The image is never really read in, just its header.
  #@r status

  #  get info from the image header
  set imageInfo [img_readHeader -file $imageFilename]
  foreach i $imageInfo {
    set [lindex $i 0] [lindex $i 1]
  }

  # ras coordinates obtained from image data
  set ptList [list $top_left_corner $top_right_corner $bottom_right_corner]

  # make a fake flate triangle for testing relocation (xyz coordinates)
  set pts {}

  # top left
  lappend pts [list 0 \
                    [expr double([lindex $extent 1])*double([lindex $voxel_dims 1])] \
                    0]
  # top right
  lappend pts [list [expr double([lindex $extent 0])*double([lindex $voxel_dims 0])] \
                    [expr double([lindex $extent 1])*double([lindex $voxel_dims 1])] \
                    0]

  # bottom right
  lappend pts [list [expr double([lindex $extent 0])*double([lindex $voxel_dims 0])] \
                    0 \
                    0]

  geom_calcTransformMatrixToRAS $pts $ptList $rtnMatrix4x4
  
  return GDSC_OK

}


# -----------------------------
# img_calcBaselineCorrectionEqn
# -----------------------------

proc img_calcBaselineCorrectionEqn {region_list image_list order} {

   #@author Nathan Wilson
   #@c Calculate the baseline correction equation given a set of
   #@c regions and a set of velocity images.
   #@a region_list:  A tcl list specifying a set of PolyData objects
   #@a region_list:  which define the regions to be used to calc
   #@a region_list:  the baseline correction equation.
   #@a image_list:   A tcl list of velocity images (StructuredPts)
   #@a image_list:   used to calc. baseline correction equation.
   #@a order:  Polynomial order (valid values are 0,1,2).
   #@r equation (error if there is a problem).
   #@note This code needs to be rewritten in C.

   # check that the region list consists of polygons that exist,
   # and each only have 1 closed polygon
   foreach i $region_list {
     if {[repos_exists -obj $i] == 0} {
       return -code error "ERROR:  Object $i does not exist."
     }
     if {[repos_type -obj $i] != "PolyData"} {
       return -code error "ERROR:  Object $i not of type PolyData."
     }
     # check that there is only one closed polygon in the region
     #if {[geom_numClosedLineRegions -obj $i] != 1} {
     #   return -code error "ERROR:  To many closed regions in $i."
     #}
   }

   set numRegions [llength $region_list]

   if {$numRegions == 0} {
     return -code error "ERROR:  No region_list provided."
   }

   # check the images exist
   foreach i $image_list {
     if {[repos_exists -obj $i] == 0} {
       return -code error "ERROR:  Object $i does not exist."
     }
     if {[repos_type -obj $i] != "StructuredPts"} {
       return -code error "ERROR:  Object $i not of type StructuredPts."
     }
   }

   # check the range on the order of the requested polynomial
   if {$order < 0 || $order > 2} {
       return -code error "ERROR:  Order of $order not permitted."
   }

   # assuming all of the image slices are in the same exact
   # location, lets get the extent and spacing info from the first

   set tmpptr [repos_exportToVtk -src [lindex $image_list 0]]
   set spacing [$tmpptr GetSpacing]
   set extent [$tmpptr GetExtent]
   set vtk_origin [$tmpptr GetOrigin]
   set dimensions [$tmpptr GetDimensions]

   puts "spacing: $spacing"
   puts "extent: $extent"
   puts "vtk_origin: $vtk_origin"

   # to speed things up, we first find the bounding box of each
   # region and convert these into voxel indices.  Only then will
   # we do the point classification to determine the voxels inside
   # of the region.

   foreach region $region_list {
      set bbox($region) [geom_bbox -obj $region]
      set imin [expr int(([lindex $bbox($region) 0] - [lindex $vtk_origin 0])/[lindex $spacing 0])]
      set imax [expr int(([lindex $bbox($region) 1] - [lindex $vtk_origin 0])/[lindex $spacing 0])]
      set jmin [expr int(([lindex $bbox($region) 2] - [lindex $vtk_origin 1])/[lindex $spacing 1])]
      set jmax [expr int(([lindex $bbox($region) 3] - [lindex $vtk_origin 1])/[lindex $spacing 1])]
      set ijbox($region) [list $imin $imax $jmin $jmax]
      if {$imin < [lindex $extent 0] || $imax > [lindex $extent 1] || \
	  $jmin < [lindex $extent 2] || $jmax > [lindex $extent 3]} {
         return -code error "ERROR:  $region bounding box not completely inside of domain."
      }
   }

   # for debugging
   showArray ijbox

   # now do point a point classification on the polygon
   # to find voxels to be used in calculating equation
   set voxs {}
   foreach region $region_list {
      set ijext $ijbox($region)
      set imin [lindex $ijext 0]
      set imax [lindex $ijext 1]
      set jmin [lindex $ijext 2]
      set jmax [lindex $ijext 3]
      for {set i $imin} {$i <= $imax} {incr i} {
	for {set j $jmin} {$j <= $jmax} {incr j} {
            set x [expr $i*[lindex $spacing 0]+[lindex $vtk_origin 0]]
            set y [expr $j*[lindex $spacing 1]+[lindex $vtk_origin 1]]
            #puts "x  y: $x $y"
            if {[geom_ptInPoly -pgn $region -pt [list $x $y]] == 1} {
               set voxs "$voxs $i,$j"
               puts "$i $j inside of selected region"
                catch {repos_delete -obj b_$i\_$j}
		#solid_box3d -dims {0.3 0.3 0.3} -ctr [list $x $y 0] -result b_$i\_$j
                #global bcGUIbaselineRen
                #gdscGeneralView $bcGUIbaselineRen b_$i\_$j
	    }
	}
      }
    }

    # sort the voxels just to eliminate duplicates
    set voxs [lsort -unique -dictionary $voxs]
    foreach v $voxs {
      #puts "$v"
    }
    puts $voxs

    puts "requested order: $order"

    # depending on the requested order set up the matrices
    # for the least squares fit

    # for testing start with linear

    set xmatrix {}
    set ymatrix {}
    foreach voxel $voxs {
      set ij [split $voxel ,]
      set i [lindex $ij 0]
      set j [lindex $ij 1]
      set x [expr $i*[lindex $spacing 0]+[lindex $vtk_origin 0]]
      set y [expr $j*[lindex $spacing 1]+[lindex $vtk_origin 1]]
      set pixel [expr $i+$j*[lindex $dimensions 0]]
      foreach image $image_list {
        set value [[[[repos_exportToVtk -src $image] GetPointData] GetScalars] GetTuple1 $pixel]
        switch $order {
          0 {
	      set row {}
	  }
          1 {
              set row [list 1 $x $y]
	  }
          2 {
              set row [list 1 $x $y [expr $x*$y] [expr $x*$x] [expr $y*$y]]
	  }
        }
        puts "$row  ::  $value"
        lappend xmatrix $row
        lappend ymatrix $value
      }
    }

    # do a fit
    switch $order {
      0 {
          # just average the points?
          set total 0
	  for {set i 0} {$i < [llength $ymatrix]} {incr i} {
            set total [expr $total + [lindex $ymatrix $i]]
	  }
          set myfit [expr $total/double([llength $ymatrix])]
       }
       1 {
          # linear regression
          puts "Do a linear least squares fit"
          set myfit [math_fitLeastSquares -X $xmatrix -Y $ymatrix -xOrder 3 -yOrder 1]
          puts "linear fit: $myfit"
       }
       2 {
          # quadratic fit
          puts "Fit a quadratic polynomial"
          set myfit [math_fitLeastSquares -X $xmatrix -Y $ymatrix -xOrder 6 -yOrder 1]
          puts "quadratic fit: $myfit"
       }
     }

     return $myfit

 }


proc img_createOrientedPolyData {fn rtnPD} {

  #@author Nathan Wilson
  #@c create a properly oriented (in 3-space) PolyData
  #@c given a Genesis formatted image slice.
  #@a fn: Genesis image slice filename.
  #@a rtnPD:  repository PolyData object to be created.
  #@r status

  if {[repos_exists -obj $rtnPD] == 1} {
     return -code error "ERROR:  object $rtnPD already exists in repository."
  }

  set myfilt tmp-img_createOrientedPolyData-myfilt
  set mymatrix tmp-img_createOrientedPolyData-matrix
  set tmpobj /tmp/img_createOrientedPolyData/pd
  set tmpimg /tmp/img_createOrientedPolyData/img

  catch {repos_delete -obj $rtnPD}
  catch {repos_delete -obj $tmpobj}
  catch {repos_delete -obj $tmpimg}
  catch {$myfilt Delete}
  catch {$mymatrix Delete}

  # read in image
  img2_readSlice $fn $tmpimg

  vtkImageDataGeometryFilter  $myfilt
  $myfilt SetInputDataObject [repos_exportToVtk -src $tmpimg]
  $myfilt Update

  img_calcTransformMatrixToRAS $fn $mymatrix

  repos_importVtkPd -src [$myfilt GetOutput] -dst $tmpobj
  geom_applyTransformMatrix $tmpobj $mymatrix $rtnPD

  repos_setLabel -obj $rtnPD -key showK -value 1

  catch {repos_delete -obj $tmpobj}
  catch {repos_delete -obj $tmpimg}
  catch {$myfilt Delete}
  catch {$mymatrix Delete}

}


proc img_sortPasses {dirname} {

  # @author Nathan Wilson

  set speed {}
  set collapse {}
  set r {}
  set a {}
  set s {}
  set mag {}

  if {[file exists $dirname] == 0} {
    puts "Error:  Directory $dirname does not exist (or you don't have permission to read)."
    return -code error "Error:  Directory $dirname does not exist (or you don't have permission to read)."
  }

  if {[file isdirectory $dirname] == 0} {
     puts "Error:  $dirname is not a directory."
     return -code error "Error:  $dirname is not a directory."
  }
  if [catch {set filenames [glob [file join $dirname I*]]}] {
    puts "ERROR: No image slices found in directory $dirname."
    return -code error "ERROR: No image slices found in directory $dirname."
  }

  foreach i [lsort -dictionary $filenames] {
        if [catch {set header [img_readHeader -file $i]}] continue
        foreach piece $header {
          set [lindex $piece 0] [lindex $piece 1]
        }
        if {$vas_collapse == 0 || $vas_collapse == 7 ||  $vas_collapse == 12 || $vas_collapse == 33} {
           #puts "$i: $vas_collapse is speed"
           lappend speed $i
        } elseif {$vas_collapse == 1 || $vas_collapse == 13} {
           puts "$i: $vas_collapse is collapse"
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

  if {[llength $speed] == 0} {
        puts "ERROR:  No magnitude image slices found!"
        return -code error "ERROR:  No magnitude image slices found!"
  }

  # first non-mip image
  set filename [lindex $speed 0]

  if {[catch {set params [img_readHeader -file $filename]} rtnstring] == 1} {
    puts "Error:  $rtnstring"
    return -code error "Error:  $rtnstring"
  }

  foreach i $params {
    set option [lindex $i 0]
    set value [lindex $i 1]
    set $option $value
  }

  set topLeft $top_left_corner
  set topRight $top_right_corner
  set botRight $bottom_right_corner

  set passnum 1
  set imgfiles($passnum) [lindex $speed 0]

  for {set i 1} {$i < [llength $speed]} {incr i} {
    set filename [lindex $speed $i]
    if {[catch {set params [img_readHeader -file $filename]} rtnstring] == 1} {
      puts "Error:  $rtnstring"
      return -code error "Error:  $rtnstring"
    }
    foreach j $params {
      set option [lindex $j 0]
      set value [lindex $j 1]
      set $option $value
    }
    set diffTLC [math_subVectors $top_left_corner $topLeft]
    set diffTRC [math_subVectors $top_right_corner $topRight]
    set diffBRC [math_subVectors $bottom_right_corner $botRight]
    if {[math_magnitude $diffTLC] < 0.001 && \
        [math_magnitude $diffTRC] < 0.001 && \
        [math_magnitude $diffBRC] < 0.001} {
      incr passnum
      puts "found new pass ($passnum)"
      set imgfiles($passnum) $filename
    } else {
      lappend imgfiles($passnum) $filename
    }
  }

  set rtnstring {}
  for {set i 1} {$i <= $passnum} {incr i} {
    lappend rtnstring $imgfiles($i)
  }

  return [list $passnum $rtnstring $collapse]

}

proc img_getNumPasses {dirname} {
  return [lindex [img_sortPasses $dirname] 0]
}

proc img_getPassImages {dirname passnum} {
  return [lindex [lindex [img_sortPasses $dirname] 1] $passnum]
}

proc img_getAllPassImages {dirname passnum} {
  return [lindex [img_sortPasses $dirname] 1]
}

proc img_getCollapseImages {dirname} {
  return [lindex [img_sortPasses $dirname] 2]
}

proc img_writeXML {vimg fn} {
  set vtkobj [repos_exportToVtk -src $vimg]
  set writer img_writeXML-writer
  catch {$writer Delete}
  vtkXMLDataSetWriter $writer
  $writer SetInputDataObject $vtkobj
  $writer SetFileName $fn
  $writer Write
  $writer Delete  
}


proc img_createMIP {files rtnImg} {

  if {[llength $files] < 2} {
    return -code error
  }
  set i 0
  set robjA /tmp/img_createMIP-A
  set robjB /tmp/img_createMIP-B
  catch {repos_delete -obj $robjA}
  img2_readSlice [lindex $files 0] $robjA
  set vobjA [repos_exportToVtk -src $robjA]
  for {set i 1} {$i < [llength $files]} {incr i} {
    puts "i: $i"
    set imgmerger tmp-img_createMIP-merger$i
    catch {$imgmerger Delete}
    vtkImageMathematics $imgmerger
    catch {repos_delete -obj $robjB}
    img2_readSlice [lindex $files $i] $robjB
    set vobjB [repos_exportToVtk -src $robjB]
    $imgmerger SetInput1Data $vobjA
    $imgmerger SetInput2Data $vobjB
    $imgmerger SetOperationToMax
    $imgmerger Update
    set vobjA [$imgmerger GetOutput]
    #$imgmerger Delete
  }
  repos_importVtkImg -src [$imgmerger GetOutput] -dst $rtnImg

}


proc img_createMIPfromVolume {imgvol direction slices mipImg} {

  set imgobj [repos_exportToVtk -src $imgvol]

  set voi       tmp-img_createMIP-extractvoi
  set mip       tmp-img_createMIP-mipx
  set miptmp    tmp-img_createMIP-miptmp
  set imgmerger tmp-img_createMIP-merger
  set mover     tmp-img_createMIPfromVol-mover
  set finalMIP tmp-img_createMIPfromVolume-finalMIP

  catch {$voi       Delete}
  catch {$mip       Delete}
  catch {$miptmp    Delete}
  catch {$imgmerger Delete}
  catch {$mover     Delete}
  catch {$finalMIP  Delete}

  set ldims [$imgobj GetDimensions]

  vtkExtractVOI $voi
  vtkImageData  $mip
  vtkImageData  $miptmp
  vtkImageChangeInformation $mover
  vtkImageData $finalMIP

  set min [lindex [$imgobj GetExtent] [expr $direction*2]]
  set max [lindex [$imgobj GetExtent] [expr $direction*2+1]]
  set allSlices [string_parse $slices $min $max]
  set firstSlice [lindex $allSlices 0]
  #puts "user selected: $allSlices"

  foreach i $allSlices {
    $voi SetInputDataObject $imgobj
    if {$direction == 2} {
      $voi SetVOI 0 [expr [lindex $ldims 0] - 1] \
	    0 [expr [lindex $ldims 1] - 1] \
	    $i $i
      set trans [list 0 0 -$i]
    } elseif {$direction == 0} {
      $voi SetVOI $i $i \
	    0 [expr [lindex $ldims 1] - 1] \
	    0 [expr [lindex $ldims 2] - 1]
      set trans [list -$i 0 0]
    } elseif {$direction == 1} {
      $voi SetVOI 0 [expr [lindex $ldims 0] - 1] \
	    $i $i \
	    0 [expr [lindex $ldims 2] - 1]
      set trans [list 0 -$i 0]
    } else {
      return -code error "illegal direction!"
    }
    $voi Update
    $mover SetInputDataObject [$voi GetOutput]
    eval $mover SetExtentTranslation $trans
    $mover Update
    if {$i == $firstSlice} {
      $mip DeepCopy [$mover GetOutput]
    } else {
      vtkImageMathematics $imgmerger
      $imgmerger SetInput1Data $mip
      $imgmerger SetInput2Data [$mover GetOutput]
      $imgmerger SetOperationToMax
      $imgmerger Update
      #puts "range: [[$imgmerger GetOutput] GetScalarRange]"
      $miptmp DeepCopy [$imgmerger GetOutput]
      $imgmerger Delete
      $mip DeepCopy $miptmp     
    }
  }


  set spacing [$mip GetSpacing]
  set dims    [$mip GetOrigin]
  $mover SetOutputOrigin 0 0 0
  if {$direction == 0} {
    $mover SetOutputSpacing [lindex $spacing 1] [lindex $spacing 2] 0
  } elseif {$direction == 1} {
    $mover SetOutputSpacing [lindex $spacing 0] [lindex $spacing 2] 0
  }
  $mover SetInputDataObject $mip
  $mover Update

  $finalMIP DeepCopy [$mover GetOutput]
  if {$direction == 0} {
    set ext [$mip GetExtent]
    $finalMIP SetExtent [lindex $ext 2] [lindex $ext 3] [lindex $ext 4] [lindex $ext 5] 0 0
  } elseif {$direction == 1} {
    set ext [$mip GetExtent]
    $finalMIP SetExtent [lindex $ext 0] [lindex $ext 1] [lindex $ext 4] [lindex $ext 5] 0 0
  }
  
  repos_importVtkImg -src $finalMIP -dst $mipImg

  $voi       Delete
  $mip       Delete
  $miptmp    Delete
  $mover     Delete
  $finalMIP  Delete

}

proc img_readDICOMwriteXML {dirname fn change_to_cm_flag} {

  set rs     img_readDICOMwriteXML-reslice
  set rdr    img_readDICOMwriteXML-reader
  set writer img_readDICOMwriteXML-writer
  set cimg   img_readDICOMwriteXML-changer

  catch {$rs Delete}
  catch {$rdr Delete}
  catch {$writer Delete}
  catch {$cimg Delete}

  # read
  puts "Reading files in directory ($dirname)."
  vtkDICOMImageReader $rdr
  $rdr SetDirectoryName $dirname
  $rdr Update

  # reslice
  puts "Reslice."
  vtkImageReslice $rs
  $rs SetInputDataObject [$rdr GetOutput]

  set directionCosines {{-1 0 0} {0 1 0}  {0 0 -1}}
  set dircosX [lindex $directionCosines 0]
  set dircosY [lindex $directionCosines 1]
  set dircosZ [lindex $directionCosines 2]
  $rs SetResliceAxesDirectionCosines [lindex $dircosX 0] [lindex $dircosX 1] [lindex $dircosX 2] \
                                     [lindex $dircosY 0] [lindex $dircosY 1] [lindex $dircosY 2] \
                                     [lindex $dircosZ 0] [lindex $dircosZ 1] [lindex $dircosZ 2]
  $rs Update

  if {$change_to_cm_flag > 0} {
    vtkImageChangeInformation $cimg
    $cimg SetInputDataObject [$rs GetOutput]
    $cimg SetInformationInputData [$rs GetOutput]

    set spacing [[$rs GetOutput] GetSpacing]
    set spacing [math_scaleVec $spacing 0.1]

    set origin [[$rs GetOutput] GetOrigin]
    set origin [math_scaleVec $origin 0.1]

    $cimg SetOutputSpacing [lindex $spacing 0] [lindex $spacing 1] [lindex $spacing 2]
    $cimg SetOutputOrigin [lindex $origin 0] [lindex $origin 1] [lindex $origin 2]
    $cimg Update
  }

  # write output
  puts "Write output file ($fn)."
  vtkXMLDataSetWriter $writer
  if {$change_to_cm_flag > 0} {
    $writer SetInputDataObject [$cimg GetOutput]
  } else {
    $writer SetInputDataObject [$rs GetOutput]
  }
  $writer SetFileName $fn
  $writer Write

  catch {$cimg Delete}
  $writer Delete
  $rs Delete
  $rdr Delete
}


proc img_readDICOM {dirname change_to_cm_flag rtnobj} {

  global gImageVol

  set rs     img_readDICOM-reslice
  set rdr    img_readDICOM-reader
  set cimg   img_readDICOM-changer

  catch {$rs Delete}
  catch {$rdr Delete}
  catch {$cimg Delete}

  # read
  puts "Reading files in directory ($dirname)."
  vtkDICOMImageReader $rdr
  $rdr SetDirectoryName $dirname
  $rdr Update

  # reslice
  puts "Reslice."
  vtkImageReslice $rs
  $rs SetInputDataObject [$rdr GetOutput]

  # okay, now the fun begions.  Get slice direction orientation
  img_findPCMRIorientation [lindex [lsort -dictionary [glob [file join $dirname I.*]]] 0] xyz directions

  #
  #  NOTE (2012/12/12):  I believe that vtk's DICOM reader sorts the 
  #    slices into lowest to highest patient position, meaning 
  #    the file name ordering is arbitrary (unlike 5X where we have
  #    to worry about it).  Hence the different handling of cases
  #    like "a s r" don't require special handling like it does for
  #    5X.               
  #

  if {$xyz == "r s a"} {
     set gImageVol(directionCosines) {{-1 0 0}  {0 0 1} {0 1 0}}
  } elseif {$xyz == "r a s"} {
     set gImageVol(directionCosines) {{-1 0 0} {0 1 0} {0 0 -1}}
  } elseif {$xyz == "a s r"} {
     set gImageVol(directionCosines) {{0 0 -1} {-1 0 0} {0 1 0}}
  } elseif {$xyz == "a r s"} {
     set gImageVol(directionCosines) {{0 1 0} {1 0 0} {0 0 -1}}
  } else {
     puts "ERROR:  Slice orientation not handled!"
     return -code error "ERROR:  Slice orientation not handled."
  }

  set directionCosines $gImageVol(directionCosines)
  set dircosX [lindex $directionCosines 0]
  set dircosY [lindex $directionCosines 1]
  set dircosZ [lindex $directionCosines 2]
  $rs SetResliceAxesDirectionCosines [lindex $dircosX 0] [lindex $dircosX 1] [lindex $dircosX 2] \
                                     [lindex $dircosY 0] [lindex $dircosY 1] [lindex $dircosY 2] \
                                     [lindex $dircosZ 0] [lindex $dircosZ 1] [lindex $dircosZ 2]
  $rs Update

  catch {$cimg Delete}
  vtkImageChangeInformation $cimg

  $cimg SetInputDataObject [$rs GetOutput]
  $cimg SetInformationInputData [$rs GetOutput]

  set spacing [[$rs GetOutput] GetSpacing]
  if {$change_to_cm_flag > 0} {
    set spacing [math_scaleVec $spacing 0.1]
  }

  set origin [[$rs GetOutput] GetOrigin]
  puts "original origin: $origin"

  set org_x [expr [lindex $gImageVol(min_RAS) 0] + [lindex $spacing 0]/2.0]
  set org_y [expr [lindex $gImageVol(min_RAS) 1] + [lindex $spacing 1]/2.0]
  set org_z [expr [lindex $gImageVol(min_RAS) 2] + [lindex $spacing 2]/2.0]

  set origin [list $org_x $org_y $org_z]

  if {$change_to_cm_flag > 0} {
    set origin [math_scaleVec $origin 0.1]
  }
  $cimg SetOutputSpacing [lindex $spacing 0] [lindex $spacing 1] [lindex $spacing 2]
  $cimg SetOutputOrigin [lindex $origin 0] [lindex $origin 1] [lindex $origin 2]
  $cimg Update

  catch {repos_delete -obj $rtnobj}

  repos_importVtkImg -src [$cimg GetOutput] -dst $rtnobj

  # update some global parameters
  global gImageVol
  set gImageVol(vtk_org_ras) [[$cimg GetOutput] GetOrigin]

  $cimg Delete
  $rs Delete
  $rdr Delete
}


# ---------
# cleanList
# ---------

proc cleanList {list} {
    #@author Ken Wang
    #@c Removes all empty strings from a list.
    #@a list:  input list.
    #@r List without empty strings.
    set result {}
    foreach i $list {
	if {[string length $i] > 0} {
	    lappend result $i
	}
    }
    return $result
}


# ---------
# showArray
# ---------

proc showArray {arrayName} {
    #@author Ken Wang
    #@c Output an array to stdout (1 item per line).
    #@a arrayName:  array to show.
    upvar $arrayName a
    foreach el [lsort [array names a]] {
	puts [format "%-10s = %s" $el $a($el)]
    }
}


# ---------
# printList
# ---------

proc printList {listIn} {
    #@author Ken Wang
    #@c Print a list to stdout (1 item per line).
    #@a listIn: List to output.
    foreach i $listIn {
	puts $i
    }
}


# -----------
# TupleToList
# -----------
# Converts a comma-separated, parenthesis-enclosed tuple into a Tcl list.

proc TupleToList {tuple} {
    #@author Ken Wang
    #@c Converts a comma-separated, parenthesis-enclosed tuple into a Tcl list.
    #@a tuple: Comma-separated, parathesis-enclosed tuple.
    #@r List.
    set toks [cleanList [split $tuple "(,)"]]
    set result {}
    foreach t $toks {
	lappend result $t
    }
    return $result
}


# -----------
# ListToTuple
# -----------
# Converts a Tcl list into a comma-separated, parenthesis-enclosed tuple.

proc ListToTuple {list} {
    #@author Ken Wang
    #@c Converts a Tcl list into a comma-separated, parenthesis-enclosed tuple.
    #@a list: Input list.
    #@r Tuple.
    set result [format "(%s)" [join $list ","]]
    return $result
}


# --------------
# RemoveFromList
# --------------

proc RemoveFromList {in item} {
    #@author Ken Wang
    #@c Remove the item from the list if it exists.
    #@a in: Input list.
    #@a item:  Item to remove.
    #@r List without item.
    set result {}
    set ix [lsearch -exact $in $item]
    if {$ix < 0} {
	return $in
    }
    set result [lreplace $in $ix $ix]
    return $result
}

# --------------
# lset2_gridType
# --------------

proc lset2_gridType {core} {
    if {[catch {$core GetGrid} items]} {
	return -code error
    }
    array set arr $items
    return $arr(-type)
}


# -------------
# lset2_getMinH
# -------------

proc lset2_getMinH {core} {
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


# -----------
# lset2_showV
# -----------

proc lset2_showV {v core} {

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
	lsetVExpDecay  {lset2_showVConst $v}
	default        {}
    }

    array set ti [$core GetTime]
    set cfl $ti(-cflFactor)
    puts [format "%-12s %f" "cflFactor:" $cfl]
    puts ""
}


# ----------------
# lset2_showVConst
# ----------------

proc lset2_showVConst {v} {
    array set consts [$v GetConst]
    set names [array names consts]
    foreach n $names {
	set c $consts($n)
	puts [format "%-12s %s" "[string range $n 1 end]:" $c]
    }
}


# ------------------
# lset2_cfgVExpDecay
# ------------------

proc lset2_cfgVExpDecay {core v kt expand stopV calcDecayConstBy mgFactor potentialValueDecay} {

    if {[catch {$v GetMagGradRange} MGrng]} {
	return -code error "must set image before further configuration"
    }
    if {$stopV <= 0.0} {
	return -code error "stop velocity must be > 0"
    }
    if {$kt <= 0.0} {
	return -code error "tolerated curvature must be > 0"
    }

    $v SetStopV -value $stopV

    if {$calcDecayConstBy == "multiply"} {

      set maxMGi [lindex $MGrng 1]

      # There should be some sort of image signal metric we can apply
      # with which to arrive at the factor to use in computing MGi_c.
      # The idea is that edge regions can be delineated once we've
      # surpassed some level of gradient magnitude.
      set MGi_c [expr $mgFactor * $maxMGi]

      set ei [expr log( $kt / $stopV ) / $MGi_c]

    } else {

      set ei [expr log( $kt / $stopV ) / $potentialValueDecay]

    }

    if {[catch {$v SetConst -eI $ei -eIneg $ei -Kt $kt \
	    -expand $expand -clamp true} msg]} {
	puts "$msg"
	return -code error $msg
    }
    $core SetTime -cflFactor 1.0
    $core SetVelocity -vobj $v

}


# -------------------
# lset2_cfgVPotential
# -------------------

proc lset2_cfgVPotential {core v klow kupp stopV calcPotConstBy pFactor potentialValuePotential} {

    if {[catch {$v GetMagGradRange} MGrng]} {
	return -code error "must set potential before further configuration"
    }
    if {[catch {lset2_getMinH $core} minh]} {
	return -code error $minh
    }

    set ek 1

    $v SetStopV -value $stopV

    # Again, there should be some sort of image signal metric we can
    # apply to choose factor, which essentially relates to the shape
    # of |grad(P)| around the edge.
#    set factor 0.1

    set factor $pFactor

    if {$calcPotConstBy == "multiply"} {

      set maxMGp [lindex $MGrng 1]
      set thr [expr $factor * $maxMGp]

    } else {

      set thr $potentialValuePotential

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


# ----------
# lset2_loop
# ----------

proc lset2_loop {core acInterval ac maxTS rebuildPhiFreq} {

  set gridDim 2

  global gLsetGridType

  # use a unique color for each velocity function
  set vtype [[$core GetVelocity] GetClassName]
  if {$vtype == "lsetVExpDecay"} {
    set color "1 0 0"
  } elseif {$vtype == "lsetVPotential"} {
    set color "0 1 0"
  } else {
    set color "0 0 1"
  }

  #  We will evolve the level set function until one of the following
  #  happens:
  #
  #    - the level set function exists the domain (returns error)
  #    - we exceed the maximum time step  (returns error)
  #    - the area has sufficiently convergened (ok, moves on)

  # for area convergence test
  set prevState /lset_slice/convergence/test/prev
  set currState /lset_slice/convergence/test/curr
  set prevStatePhi /lset_slice/convergence/test/prev/phi
  set currStatePhi /lset_slice/convergence/test/curr/phi

  catch {repos_delete -obj $prevState}
  catch {repos_delete -obj $currState}
  catch {repos_delete -obj $prevStatePhi}
  catch {repos_delete -obj $currStatePhi}

  $core ExtractFront -out $prevState -closed 1
  $core ExtractPhi -out $prevStatePhi
#  $core SaveGrid -result $prevStatePhi

  set nextStepToCheck $acInterval
  set converged 0

  for {set numIter 0} {$numIter < $maxTS} {incr numIter 1} {

    # return error if core has vanished
    if {[$core Vanished]} {
      puts "ERROR: Level set exited grid domain."
      return -code error "ERROR: Level set exited grid domain."
    }

    # done loop if velocity hits internal stopping criteria
    if { ! [$core EvolveOneTimeStep] } {
      puts "End lset evolution."
      set converged 1
      break
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

    puts [format "End time step \[%d\], maxV \[%f\]." $ts $maxV]

    #$core ExtractFront -out /lset/$ts
    #$core ExtractPhi -out /phi/$ts
    #$core ExtractVel -out /velocity/[expr $ts - 1]

    # check the global options array to see if we should interactively display
    # the segmentation
    global gOptions
    if {$gOptions(lsetDisplayWindows) != ""} {
      catch {repos_delete -obj /lsetRen/watch}
      $core ExtractFront -out /lsetRen/watch
      foreach renWin $gOptions(lsetDisplayWindows) {
          set renId 1
          set ren [format "%s_ren%d" $renWin $renId]
	  catch {vis_pRm $ren /lsetRen/watch}
          catch {set actor [vis_pRepos $ren /lsetRen/watch]}
          catch {[$actor GetProperty] SetColor [lindex $color 0] [lindex $color 1] [lindex $color 2]}
          catch {[$actor GetProperty] SetLineWidth 2}
          catch {[$ren GetRenderWindow] Render}
      }
    }

    if {$gOptions(lset_allow_interrupt) != 0} {
        set chkstnow 0
        after 1 {set chkstnow 1}
        vwait chkstnow
        if {$gOptions(lset_interrupt) != 0} {
           set gOptions(lset_interrupt) 0
           return -code error "User Requested Halt of Segmentation!"
        }
    }

    # rebuild phi every 5 time steps????
    if {[expr $ts % $rebuildPhiFreq] == 0} {
	$core RebuildPhi
    }

    # check for area convergence if its time
    #puts "numIter: $numIter  nextStepToCheck: $nextStepToCheck"
    if {$numIter == $nextStepToCheck} {
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

      catch {repos_delete -obj $currState}
      catch {repos_delete -obj $currStatePhi}
      $core ExtractFront -out $currState -closed 1
      $core ExtractPhi -out $currStatePhi
      #$core SaveGrid -result $currStatePhi
      if {[catch {AreaMetric_go 2 $prevState $currState $prevStatePhi $currStatePhi} a]} {
         puts "output: $a"
         set a 0.0
      }

      # call to shapes-based check
      #if {[catch {AreaMetric_go $gridDim $prevState $currState} a]} {
      #   set a 0.0
      #}

      catch {repos_delete -obj $prevState}
      geom_copy -src $currState -dst $prevState
      # we have to play a trick here since I can't figure out a way
      # to import a vtkUnstructuredGrid into the repos
      set tmp $prevStatePhi
      set prevStatePhi $currStatePhi
      set currStatePhi $tmp
      catch {repos_delete -obj $currStatePhi}

      if {$a >= $ac} {
        puts "Area convergence: $a"
        set converged 1
        break
      }
      incr nextStepToCheck $acInterval

    }

  }

  if {$converged == 0} {
    puts "ERROR: Exceeded maximum number of time steps."
    return -code error "ERROR: Exceeded maximum number of time steps."
  }

  return GDSC_OK

}


# ----------
# lset2_main
# ----------

proc lset2_main [list img pot \
                      gridType maxTS gridFactor stopV \
                      isotropicFlag rebuildPhiFreq \
                      goodnessCriteria acInterval \
                      r x y z kt \
                      calcDecayConstBy magGradFactor potentialValueDecay \
                      klow kupp  \
                      calcPotConstBy pFactor potentialValuePotential \
                      rtnPd] {

  # always assume expanding front
  set expandFlag 1

  # for now
  set objPrefix {nate}

  # global variable with grid type
  global gLsetGridType
  set gLsetGridType $gridType

  # lset objects
  set core __lset_slice_core
  set ve   __lset_slice_vExpDecay
  set vp   __lset_slice_vPotential

  # delete lset objects if they already exist
  catch {rename $core {}}
  catch {rename $ve {}}
  catch {rename $vp {}}

  # some repos objects we will create
  catch {repos_delete -obj $objPrefix/lset/0}
  catch {repos_delete -obj $objPrefix/grid/0}
  repos_deleteList [repos_subList /lset/*]
  repos_deleteList [repos_subList /velocity/*]
  repos_deleteList [repos_subList /grid/*]
  repos_deleteList [repos_subList /phi/*]

  # get the required params from the image object
  set vtk_p [repos_exportToVtk -src $pot]
  set h [$vtk_p GetSpacing]
  set dims [$vtk_p GetDimensions]
  set orig [$vtk_p GetOrigin]

  # calculate the original extent of the domain
  set domExtx [expr [lindex $h 0] * [lindex $dims 0]]
  set domExty [expr [lindex $h 1] * [lindex $dims 1]]
  set domExtz [expr [lindex $h 2] * [lindex $dims 2]]
  set domExt [list $domExtx $domExty $domExtz]

  # calculate the level set grid spacing as a factor of the image
  # spacing
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
        # set orig_i [expr $minh / 2.0]
        # set orig [list $orig_i $orig_i $orig_i]

	if {$gridDim == 2} {
	    set dims [lreplace $dims 2 2 1]
	    set orig [lreplace $orig 2 2 0.0]
	}
  }

  # Inputs:
  # -------
  #   - seed radius  (init)
  #   - kt           (vExpDecay)
  #   - klow, kupp   (vPotential, vSmooth)

  lsetCore $core
  $core SetTimers -flag 0

  # Grid parameters
  # ---------------
  #   - h:    dynamically retrieved from image potential
  #   - dims: dynamically retrieved from image potential
  #   - orig: dynamically retrieved from image potential
  #   - minh: dynamically retrieved from image potential

  puts "\n====="
  puts "lsetCore::SetGrid"
  if {$gLsetGridType == "SparseGrid"} {
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

  # initialize lset core with circle
  puts "circle: $r"
  if {[catch {$core SetCircleSeed -r $r -x $x -y $y -z $z} msg]} {
    puts "Error on initialization."
    return -code error $msg
  }

    lsetVExpDecay $ve
    $ve SetImageObj -src $img
    if {[catch {lset2_cfgVExpDecay $core $ve $kt $expandFlag \
	    $stopV $calcDecayConstBy $magGradFactor $potentialValueDecay} msg]} {
	puts "Error on vExpDecay config."
	return -code error $msg
    }

  puts "\n====="
  puts "lsetCore::Init"
  $core Init
  puts "grid stats:      [$core GetGridStats]"
  puts "====="

  $core ExtractFront -out $objPrefix/lset/0 -closed 1

  set gLsetGridType [lset2_gridType $core]
  if {$gLsetGridType == "SparseGrid"} {
	$core SaveGrid -result $objPrefix/grid/0
  }

  #lset_showV $ve $core

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

  catch {repos_delete -obj $prevState}
  catch {repos_delete -obj $currState}

  geom_copy -src $objPrefix/lset/0 -dst /lset/0
  geom_copy -src $objPrefix/lset/0 -dst $prevState


  # Area/volume convergence criterion: If successive checks indicate
  # that shape is not changing within this factor (as computed by
  # the goodness metric), then exit the current phase of front
  # evolution.  Note that these are only applied when convFlag is
  # true.

  set ac $goodnessCriteria

  # The area/vol convergence test interval should probably be a
  # function of grid size, since at smaller grid sizes,
  # displacements per time step (and per interval) will be smaller
  # in physical units.

  #set acInterval [expr 50 / $gridFactor]
  #set acInterval 50

  puts "\n====="
  puts "Additional configuration notes:"
  puts "  mag gradient factor:  $magGradFactor"
  puts "  area/vol criterion:   $ac"
  puts "  area/vol interval:    $acInterval"
  puts "  max time step:        $maxTS"
  puts "====="

  # find thresholding of mag. of image gradient
  lset2_loop $core $acInterval $ac $maxTS $rebuildPhiFreq

  # set up potential well velocity function
  lsetVPotential $vp
  $vp SetImageObj -src $pot

  # Again, there should be some sort of image signal metric we can
  # apply to choose factor, which essentially relates to the shape
  # of |grad(P)| around the edge.

  #set pFactor 0.1

  lset2_cfgVPotential $core $vp $klow $kupp $stopV $calcPotConstBy $pFactor \
                      $potentialValuePotential
  $core SetTime -cflFactor 0.5
  lset2_showV $vp $core

  lset2_loop $core $acInterval $ac $maxTS $rebuildPhiFreq

  # Finish
  # ------

  set seg $objPrefix/seg
  catch {repos_delete -obj $rtnPd}
  if {[catch {$core ExtractFront -out $rtnPd -closed 1} msg]} {
	puts "$msg"
	return -code error "ERR: $msg"
  }

  #return $seg
  return GDSC_OK

}


proc intersect_lset {truePhi approxPhi interPgn} {

  #@author Nathan Wilson
  #@c Calculate the intersection of two objects defined implicitly by
  #@c level set functions.
  #@c Routines to do boolean operations on objects represented by
 #@c level set grids (see Sethian 99, pg. 278).
  #@a truePhi: Levelset function representing object A.
  #@a approxPhi:  Levelset function representing object B.
  #@a interPgn:  PolyData intersection object returned.
  #@r status
  #@danger This routine alters truePhi!!

  # get the vtk objs
  set true [repos_exportToVtk -src $truePhi]
  set approx [repos_exportToVtk -src $approxPhi]

  set myScalars /tmp/intersect_lset/unionedScalars
  catch {$myScalars Delete}

  set trueScalars [[$true GetPointData] GetScalars]
  set approxScalars [[$approx GetPointData] GetScalars]

  # implicitly union the 2 objects on the level set grid
  set trueNum [$trueScalars GetNumberOfTuples]
  set approxNum [$approxScalars GetNumberOfTuples]
  if {$trueNum != $approxNum} {
    puts "ERROR: Different number of scalars in intersect_lset ($trueNum != $approxNum)."
    return -code error "ERROR: Different number of scalars in intersect_lset."
  }
  vtkFloatArray $myScalars
  $myScalars Allocate 1 1
  $myScalars Initialize
  for {set i 0} {$i < $trueNum} {incr i} {
    set a [$trueScalars GetTuple1 $i]
    set b [$approxScalars GetTuple1 $i]
    if {$a < $b} {
     set value $b
    } else {
     set value $a
    }
    $myScalars InsertNextTuple1 $value
  }

  # this destroys the original truePhi, but we don't need
  # it anymore anyway.
  [$true GetPointData] SetScalars $myScalars
  $true Update

  # return the contour of the unioned phi vtk dataobj
  repos_importVtkPd -src [img_contour $true 0] -dst $interPgn
  return GDSC_OK
}


# -----------------------
# mesh_writeInflowFaceVtk
# -----------------------

proc mesh_writeInflowFaceVtk {solidfile atrfile outfile} {

  #@author Nathan Wilson
  #@c This is a pure convience function which reads in a mesh
  #@c and writes out a vtkPolyData file containing the nodes and
  #@c and connectivity for the surface named "inflow."  This file
  #@c is intended to be used by the boundary condition GUI.
  #@note  There is no rotation of translation of the face done.
  #@a solidfile:  Filename for a Parasolid solid model which
  #@a solidfile:  contains 1 face tagged with the name "inflow".
  #@a atrfile:  Scorec meshing attribute file for the mesh to be
  #@a atrfile:  read in.
  #@a outfile:  Filename of the vtkPolyData file to be written.
  #@note If more then one face is named inflow, only the first
  #@note will be written.

  set mesh /tmp/mesh_writeInflowFaceVtk/mesh
  set solid /tmp/mesh_writeInflowFaceVtk/solid
  set facePD /tmp/mesh_writeInflowFaceVtk/face

  # deleting meshes doesn't really work
  #catch {repos_delete -obj $mesh}
  catch {repos_delete -obj $solid}
  catch {repos_delete -obj $facePD}

  if {[file exists $solidfile] == 0} {
    puts "ERROR:  Solid model file $atrfile does not exist."
    return -code error "ERROR:  Solid model file $atrfile does not exist."
  }
  if {[file exists $atrfile] == 0} {
    puts "ERROR:  Attribute file $atrfile does not exist."
    return -code error "ERROR:  Attribute file $atrfile does not exist."
  }
  if {[file exists $outfile] == 1} {
    puts "ERROR:  Destination file $outfile exists."
    return -code error "ERROR:  Destination file $outfile exists."
  }
  solid_setKernel -name Parasolid
  solid_readNative -file $solidfile -obj $solid
  set faceid -1
  foreach face [$solid GetFaceIds] {
    if {[$solid GetFaceAttr -attr gdscName -faceId $face] == "inflow"} {
      set faceid $face
      break
    }
  }
  if {$faceid < 0} {
    puts "ERROR:  Could not find \"inflow\" face!"
    return -code error "ERROR:  Could not find \"inflow\" face!"
  }

  # don't know if I should output this little messages or not
  puts "Loading mesh (from file $atrfile)."
  mesh_newObject -result $mesh -attributefile $atrfile -kernel Parasolid
  $mesh Update
  puts "Getting Inflow Mesh Face PolyData (face $faceid)."
  $mesh GetFacePolyData -result $facePD -face $faceid
  puts "Writing file $outfile."
  repos_writeVtkPolyData -file $outfile -type ascii -obj $facePD

  # deleting meshes doesn't really work
  #catch {repos_delete -obj $mesh}
  catch {repos_delete -obj $solid}
  catch {repos_delete -obj $facePD}

  return GDSC_OK

}


proc mesh_readMSS {filename resObj} {

  #@author Nathan Wilson
  #@c This function processes a simmetrix style meshing
  #@c script file.
  #@a filename:  script filename.
  #@a resObj:  resulting repository MeshObject.
  #@note resObj is not deleted, even if the script file
  #@note specifies deleteModel and deleteMesh.

  if {[repos_exists -obj $resObj] != 0} {
    return -code error "object $resObj already exists!"
  }

  set solid /tmp/mesh_readMSS/solid
  catch {repos_delete -obj $solid}

  global gOptions
  global guiMMvars

  # lookup for type
  set types(1) 1
  set types(2) 2
  set types(absolute) 1
  set types(relative) 2
  set types(abs) 1
  set types(rel) 2
  set sides(negative) 0
  set sides(positive) 1
  set sides(both) 2
  set sides(0) 0
  set sides(1) 1
  set sides(2) 2

  mesh_setKernel -name $gOptions(meshing_kernel)
  mesh_newObject -result $resObj
  $resObj SetSolidKernel -name $gOptions(meshing_solid_kernel)

  set fp [open $filename r]
  while {[gets $fp line] >= 0} {
      set line [string trim $line]
      # skip comment lines
      if {[string index $line 0] == "\#"} {
         puts "ignoring line: <$line>"
         continue
      }
      puts "line: $line"
      # supported commands
      if {[lindex $line 0] == "logon"} {
         mesh_logon -file [lindex $line 1]
      } elseif {[lindex $line 0] == "logoff"} {
         mesh_logoff
      } elseif {[lindex $line 0] == "newMesh"} {
        $resObj NewMesh
      } elseif {[lindex $line 0] == "generateMesh"} {
        $resObj GenerateMesh
      } elseif {[lindex $line 0] == "loadModel"} {
        $resObj LoadModel -file [lrange $line 1 end]
        catch {repos_delete -obj $solid}
        solid_readNative -file [lrange $line 1 end] -obj $solid
        if {$gOptions(meshing_solid_kernel) == "Discrete"} {
           global gDiscreteModelFaceNames
           global gDiscreteModelFaceNamesInfo
           catch {unset gDiscreteModelFaceNames}
           catch {unset gDiscreteModelFaceNamesInfo}
	   if [file exists [lrange $line 1 end].facenames] {
	     puts "sourcing [lrange $line 1 end].facenames"
	     source [lrange $line 1 end].facenames
             package require md5
	     set mymd5 [::md5::md5 -hex -file [lrange $line 1 end]]
             if {$mymd5 != $gDiscreteModelFaceNamesInfo(model_file_md5)} {
	       return -code error "ERROR: dsm model ([lrange $line 1 end]) file doesn't match one used to generate facenames ([lindex $line 1].facenames)!"
             }
	   }
	}
        set faceids [$solid GetFaceIds]
        foreach id $faceids {
	  if {$gOptions(meshing_solid_kernel) == "Parasolid"} { 
            set ident [$solid GetFaceAttr -attr identifier -faceId $id]
            set facename [$solid GetFaceAttr -attr gdscName -faceId $id]
	  } elseif {$gOptions(meshing_solid_kernel) == "Discrete"} { 
            set ident $id
            set facename $gDiscreteModelFaceNames($id)
	  } else {
            return -code error "ERROR: invalid solid kernel ($gOptions(meshing_solid_kernel))"
	  }
          if {$facename != ""} {
            set ids($facename) $ident
          }
          set ids($ident) $ident
        }
        showArray ids
      } elseif {[lindex $line 0] == "gsize"} {
        $resObj SetMeshOptions -options "GlobalEdgeSize" -values [list $types([string tolower [lindex $line 1]]) [lindex $line 2]]
      } elseif {[lindex $line 0] == "size"} {
        $resObj SetMeshOptions -options "LocalEdgeSize" -values [list $ids([lindex $line 1]) $types([string tolower [lindex $line 2]]) [lindex $line 3]]
      } elseif {[lindex $line 0] == "gcurv"} {
        $resObj SetMeshOptions -options "GlobalCurvature" -values [list $types([string tolower [lindex $line 1]]) [lindex $line 2]]
      } elseif {[lindex $line 0] == "curv"} {
        $resObj SetMeshOptions -options "LocalCurvature" -values [list $ids([lindex $line 1]) $types([string tolower [lindex $line 2]]) [lindex $line 3]]
      } elseif {[lindex $line 0] == "GlobalCurvatureMin"} {
        $resObj SetMeshOptions -options "M" -values [list $types([string tolower [lindex $line 1]]) [lindex $line 2]]
      } elseif {[lindex $line 0] == "LocalCurvatureMin"} {
        $resObj SetMeshOptions -options "m" -values [list $ids([lindex $line 1]) $types([string tolower [lindex $line 2]]) [lindex $line 3]]
      } elseif {[lindex $line 0] == "boundaryLayer"} {
        $resObj SetBoundaryLayer -id $ids([lindex $line 1]) -type [lindex $line 2] \
                                 -side $sides([string tolower [lindex $line 3]]) -nL [lindex $line 4] -H [lrange $line 5 end]
      } elseif {[lindex $line 0] == "sphereRefinement"} {
        $resObj SetSphereRefinement -size [lindex $line 1] -r [lindex $line 2] -ctr [lrange $line 3 5]
      } elseif {[lindex $line 0] == "cylinderRefinement"} {
        $resObj SetCylinderRefinement -size [lindex $line 1] -r [lindex $line 2] -length [lindex $line 3] \
                                      -ctr [lrange $line 4 6] -nrm [lrange $line 7 9]
      } elseif {[lindex $line 0] == "writeMesh"} {
        $resObj WriteMesh -file "[lrange $line 1 end-2]" -version [lindex $line end]
      } elseif {[lindex $line 0] == "writeStats"} {
        $resObj WriteStats -file [lindex $line 1]
      } elseif {[lindex $line 0] == "option"} {
	  if {[llength $line] == 3} {
	      if {[lindex $line 1] == "surface"} {
                $resObj SetMeshOptions -options "SurfaceMeshFlag" -values [lindex $line 2]
	      } elseif {[lindex $line 1] == "volume"} {
                $resObj SetMeshOptions -options "VolumeMeshFlag" -values [lindex $line 2]
	      } else {
		  return -code error "bad line: \{$line\}"
	      }
	  } elseif {[llength $line] == 4} {
	    if {[lindex $line 1] == "surface"} {
	      if {[lindex $line 2] == "optimization"} {
		$resObj SetMeshOptions -options "SurfaceOptimization" -values [lindex $line 3]
	      } elseif {[lindex $line 2] == "smoothing"} {
		$resObj SetMeshOptions -options "SurfaceSmoothing" -values [lindex $line 3]
	      } else {
		  return -code error "bad line: \{$line\}"
	      }
	    } elseif {[lindex $line 1] == "volume"} {
	      if {[lindex $line 2] == "optimization"} {
		$resObj SetMeshOptions -options "VolumeOptimization" -values [lindex $line 3]
	      } elseif {[lindex $line 2] == "smoothing"} {
		$resObj SetMeshOptions -options "VolumeSmoothing" -values [lindex $line 3]
	      } else {
		  return -code error "bad line: \{$line\}"
	      }
	    }
	  } else {
	      return -code error "bad line: \{$line\}"
	  }
      } else {
        puts "ignoring line: <$line>"
      }
  }
  close $fp
  catch {repos_delete -obj $solid}
}

#proc mesh_checkForAdapt {smsfile solidmodelfile} {
#
#  #@author Nathan Wilson
#  #@c Check that no elements have all nodes falling on exterior surface.
#  #@a smsfile:  simmetrix mesh database
#  #@a solidmodelfile: parasolid model
#
#  set mymesh tmp-mesh_checkForAdapt-mesh
#  set mysolid tmp-mesh_checkForAdapt-solid
#
#  catch {repos_delete -obj $mymesh}
#  catch {repos_delete -obj $mysolid}
#
#  puts "Loading mesh ($smsfile)..."
#  mesh_newObject -result $mymesh -meshfile $smsfile -solidfile $solidmodelfile
#  $mymesh Update
#  puts "Done loading mesh..."
# 
#  puts "Loading solid model ($solidmodelfile)..."
#  solid_readNative -file $solidmodelfile -obj $mysolid
#  puts "Done loading solid model..."
#
#  # build a list of all nodes on the exterior surface of the mesh
#
#  puts "Find all nodes on exterior..."
#  set surfaceNodes ""
#  foreach face [$mysolid GetFaceIds] {
#    set surfaceNodes "$surfaceNodes [$mymesh GetElementNodesOnModelFace -face $face]"
#  }
#  set surfaceNodes [lsort -unique $surfaceNodes]
#  puts "Done finding all nodes on exterior..."
#
#  # loop over all elements connected to surface and see if all nodes for element
#  # fall on boundary
#
#  puts "Check elements touching the border..."
#  set numProbElems 0
#  foreach surfElem [$mymesh GetExteriorElementFacesOnRegion -region [$mysolid GetRegionIds]] {
#    set conn [$mymesh GetElementConnectivity -element [lindex $surfElem 0]]
#    set allOn 1
#    foreach n $conn {
#      if {[lsearch -exact $surfaceNodes $n] < 0} {
#        set allOn 0
#        break
#      }  
#    }
#    if {$allOn == 1} {
#      puts "PROBLEM: element [lindex $surfElem 0] has all nodes on boundary ($conn)"
#      incr numProbElems
#    }
#  }
#
#  catch {repos_delete -obj $mymesh}
#  catch {repos_delete -obj $mysolid}
#
#  if {$numProbElems == 0} {
#      puts "\nEverything is OK.\n"
#  } else {
#      puts "\nBAD NEWS!  There were ($numProbElems) bad elements.  Run phFixMesh.\n"
#  }
#
#}


# -------------
# AreaMetric_go
# -------------

proc AreaMetric_go {dim truePoly approxPoly truePhi approxPhi} {
    #@author Nathan Wilson
    #@c Calculates the goodness metric from two PolyData objects.
    #@c For dim=2, this routine uses <p AreaMetric_poly>.
    #@c For dim=3, this routine uses <p AreaMetric_shapesPoly3d>.
    #@danger Requires Shapes kernel for 3-D.
    #@a dim: spatial dimension.
    #@a truePoly: PolyData object of dimension dim.
    #@a approxPoly: PolyData object of dimension dim.
    #@a truePhi: level set scalar function StructuredPts object.
    #@a approxPhi: level set scalar function StructuredPts object.
    switch $dim {
	2 {return [AreaMetric_poly $truePoly $approxPoly $truePhi $approxPhi]}
	3 {return -code error "ERROR: 3d currently not supported!"}
    }
    return -code error
}


# ---------------
# AreaMetric_poly
# ---------------

proc AreaMetric_poly {truePgn approxPgn truePhi approxPhi} {
    #@author Nathan Wilson
    #@c Calculates the goodness metric from two 2-D PolyData objects.
    #@a truePgn:  True 2-D PolyData object.
    #@a approxPgn:  Approximate 2-D PolyData object.
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

    set interPgn /tmp/area_metric/inter
    set interPgnPts /tmp/area_metric/inter/merge
    set interPgnPd /tmp/area_metric/inter/pd
    catch {repos_delete -obj $interPgn}
    catch {repos_delete -obj $interPgnPts}
    catch {repos_delete -obj $interPgnPd}

    # clean up point list
    geom_mergePts -src $truePgn -dst $truePgn/merge
    # check that points define single region
    if {[geom_numClosedLineRegions -obj $truePgn/merge] != 1} {
       return -code error "Need just 1 closed region in $truePgn"
    }

    set points [geom_getOrderedPts -obj $truePgn/merge]
    geom_polygonFromPts $points $truePgn/merge/pd
    set aTrue [geom_surfArea -src $truePgn/merge/pd]

    # clean up point list
    geom_mergePts -src $approxPgn -dst $approxPgn/merge
    # check that points define single region
    if {[geom_numClosedLineRegions -obj $approxPgn/merge] != 1} {
       return -code error "Need just 1 closed region in $approxPgn"
    }

    set points [geom_getOrderedPts -obj $approxPgn/merge]
    geom_polygonFromPts $points $approxPgn/merge/pd
    set aApprox [geom_surfArea -src $approxPgn/merge/pd]

    puts [format "A(%s) = \t %10.4f" $truePgn $aTrue]
    puts [format "A(%s) = \t %10.4f" $approxPgn $aApprox]

    intersect_lset $truePhi $approxPhi $interPgn

    # clean up point list
    geom_mergePts -src $interPgn -dst $interPgnPts
    # check that points define single region
    if {[geom_numClosedLineRegions -obj $interPgnPts] != 1} {
       return -code error "Need just 1 closed region in $interPgn"
    }

    set points [geom_getOrderedPts -obj $interPgnPts]
    geom_polygonFromPts $points $interPgnPd
    set ainter [geom_surfArea -src $interPgnPd]

    puts [format "A(overlap) = \t %10.4f   (used level set boolean)" $ainter]

    set fi [expr $ainter / $aTrue]
    set fj [expr $ainter / $aApprox]
    set result [expr sqrt($fi * $fj)]

    puts "g(a,b) = $result   (used level set boolean)"

    return $result

}

# -------------
# math_radToDeg
# -------------

proc math_radToDeg {rad} {
    #@author Ken Wang
    #@c Convert radians to degrees.
    #@a rad: Angle in radians.
    #@r Angle in degrees.
    return [expr $rad * 180 / 3.1415926535]
}


# -------------
# math_degToRad
# -------------

proc math_degToRad {deg} {
    #@author Ken Wang
    #@c Convert degrees to radians.
    #@a deg: Angle in degrees.
    #@r Angle in radians.
    return [expr $deg * 3.1415926535 / 180]
}


# -------
# math_pi
# -------

proc math_pi {} {
    #@author Ken Wang
    #@c Return Pi.
    #@r Pi.
    return 3.1415926535
}


# ----------------
# math_translatePt
# ----------------

proc math_translatePt {pt vec} {
    #@author Ken Wang
    #@c Translate a 2-D point by vector.
    #@a pt: Original point.
    #@a vec: Translation vector.
    #@r Translated point.
    set coord {}
    set x [expr [lindex $pt 0] + [lindex $vec 0]]
    set y [expr [lindex $pt 1] + [lindex $vec 1]]
    lappend coord $x
    lappend coord $y
    set coord
}


# -------------
# math_circlePt
# -------------

proc math_circlePt {angleDeg radius} {
    #@author Ken Wang
    #@c Return a (x,y) point on a circle centered at the origin.
    #@a angleDeg:  Angle in degrees.
    #@a radius: Radius.
    #@r (x,y) point.
    set coord {}
    set x [expr $radius * cos([math_degToRad $angleDeg])]
    set y [expr $radius * sin([math_degToRad $angleDeg])]
    lappend coord $x
    lappend coord $y
    set coord
}


# -------------------
# math_writeCirclePgn
# -------------------

proc math_writeCirclePgn {radius ctrVec fn} {
    #@author Ken Wang
    #@c Create a file with a circle centered at ctrVec.  The file
    #@c format is "x y -1 -1 -1 -1".
    #@a radius: Radius of circle.
    #@a ctrVec: Center of circle.
    #@a fn: Output filename.
    set f [open $fn "w"]
    for {set a 0} {$a < 360} {incr a 10} {
	set crd [math_circlePt $a $radius]
	set crd [math_translatePt $crd $ctrVec]
	puts $f [format "%f %f -1 -1 -1 -1" [lindex $crd 0] [lindex $crd 1]]
    }
    close $f
}


# ----------
# math_isInt
# ----------

proc math_isInt {in} {
    #@author Ken Wang
    #@c Check if value is an integer.
    #@a in: Input value.
    #@r 1 if integer, 0 otherwise.
    if {[regexp {^[0-9]+$} $in]} {
	return 1
    }
    if {[regexp {^-[0-9]+$} $in]} {
	return 1
    }
    return 0
}


# -------------
# math_isDouble
# -------------

proc math_isDouble {in} {
    #@author Ken Wang
    #@c Check if the value is a floating point number.
    #@a in: Input value.
    #@r 1 if floating point number, 0 otherwise.
    if {[math_isInt $in]} {
	return 1
    }
    if {[regexp {^[0-9]*[\.][0-9]*$} $in]} {
	return 1
    }
    if {[regexp {^-[0-9]*[\.][0-9]*$} $in]} {
	return 1
    }
    return 0
}


# --------
# math_d2f
# --------

proc math_d2f {in} {
    #@author Nathan Wilson
    #@c Returns a floating point version of
    #@c a really small or really big double precision number.
    #@a in: double precision number.
    #@r float.
    set s [split $in e]
    if {[llength $s] == 1} {return $in}
    if {[lindex $s 1] >= 38} {
       return 1.0e+38
    }
    if {[lindex $s 1] <= -38} {
       return 0
    }
    return $in
}


# ----------------
# math_linInterp1D
# ----------------

proc math_linInterp1D {domA domB rangeA rangeB domTarget} {
    #@author Ken Wang
    #@c Linerar interpolation between two values.
    #@a domA: domain of point A.
    #@a rangeA: range of point A.
    #@a domB: domain of point B.
    #@a rangeB: range of point B.
    #@a domTarget: domain of point of interest.
    #@r Range of point of interest.
    #@note No checking is done to make sure point is inside of domain.
    set domDelta [expr double($domB - $domA)]
    set rangeDelta [expr double($rangeB - $rangeA)]
    set factor [expr ( $domTarget - $domA ) / $domDelta]
    set rangeTarget [expr $rangeA + ( $factor * $rangeDelta )]
    return $rangeTarget
}


# --------------
# math_magnitude
# --------------

proc math_magnitude {vec} {
    #@author Ken Wang
    #@c Magnitude of an n-vector.
    #@a vec: Input vector.
    #@r Magnitude.
    set dim [llength $vec]
    set result 0.0
    foreach comp $vec {
	set result [expr $result + pow(double($comp),2)]
    }
    return [expr sqrt($result)]
}


# --------------
# math_normalize
# --------------

proc math_normalize {vec} {
    #@author Ken Wang
    #@c Normalize an n-vector.
    #@a vec: Input vector.
    #@r Normalized vector.
    set mag [math_magnitude $vec]
    set result {}
    foreach comp $vec {
	set result [lappend result [expr $comp / $mag]]
    }
    return $result
}


# --------
# math_dot
# --------

proc math_dot {vecA vecB} {
    #@author Ken Wang
    #@c Dot product of a n-dimensional vector.
    #@a vecA: Vector A.
    #@a vecB: Vector B.
    #@r A . B
    #@note Blank string is returned if vectors are of different length.
    set dimA [llength $vecA]
    set dimB [llength $vecB]

    if {$dimA != $dimB} {
	return
    } else {
	set dim $dimA
    }

    set result 0.0
    for {set i 0} {$i < $dim} {incr i} {
	set result [expr $result + [lindex $vecA $i] * [lindex $vecB $i]]
    }

    return $result
}


# ----------
# math_cross
# ----------

proc math_cross {vecA vecB} {
    #@author Ken Wang
    #@c Cross product of two vectors.
    #@a vecA: Vector A
    #@a vecB: Vector B
    #@r A X B.
    #@r If vectors are not of length 3, null string returned.
    set dimA [llength $vecA]
    set dimB [llength $vecB]

    if {$dimA != $dimB} {
	return
    } elseif {$dimA != 3} {
	return
    } else {
	set dim $dimA
    }

    set result {}
    lappend result [expr ( [lindex $vecA 1] * [lindex $vecB 2] ) - \
	    ( [lindex $vecA 2] * [lindex $vecB 1] ) ]

    lappend result [expr ( [lindex $vecA 2] * [lindex $vecB 0] ) - \
	    ( [lindex $vecA 0] * [lindex $vecB 2] ) ]

    lappend result [expr ( [lindex $vecA 0] * [lindex $vecB 1] ) - \
	    ( [lindex $vecA 1] * [lindex $vecB 0] ) ]

    return $result
}


# ----------------------
# math_angleBtw3DVectors
# ----------------------

proc math_angleBtw3DVectors {vecA vecB} {
    #@author Ken Wang
    #@c Angle in radians between to 3-D vectors.
    #@a vecA: Vector A.
    #@a vecB: Vector B.
    #@r Angle in radians.
    set dot [math_dot $vecA $vecB]
    set magA [math_magnitude $vecA]
    set magB [math_magnitude $vecB]
    set cosTheta [expr $dot / ($magA * $magB)]
    if {$cosTheta >= 1} {
      set cosTheta 1
    }
    return [expr acos($cosTheta)]
}


# -------------
# math_distance
# -------------

proc math_distance {a b} {
    #@author Ken Wang
    #@c Calculate distance between two 3-D points.
    #@a a: Point a.
    #@a b: Point b.
    #@r Distance.

    set dx [expr [lindex $a 0] - [lindex $b 0]]
    set dy [expr [lindex $a 1] - [lindex $b 1]]
    set dz [expr [lindex $a 2] - [lindex $b 2]]
    return [math_magnitude [list $dx $dy $dz]]
}


# -----------------
# math_areaTriangle
# -----------------

proc math_areaTriangle {a b c} {
    #@author Ken Wang
    #@c Calculate the area of a triangle in 3-D space.
    #@a a: point a.
    #@a b: point b.
    #@a c: point c.
    #@r Area of the triangle.
    set v1x [expr double([lindex $b 0]) - [lindex $a 0]]
    set v1y [expr double([lindex $b 1]) - [lindex $a 1]]
    set v1z [expr double([lindex $b 2]) - [lindex $a 2]]
    set v1 [list $v1x $v1y $v1z]

    set v2x [expr double([lindex $b 0]) - [lindex $c 0]]
    set v2y [expr double([lindex $b 1]) - [lindex $c 1]]
    set v2z [expr double([lindex $b 2]) - [lindex $c 2]]
    set v2 [list $v2x $v2y $v2z]

    return [expr [math_magnitude [math_cross $v1 $v2]] / 2.0]
}


# ---------------
# math_addVectors
# ---------------

proc math_addVectors {a b} {
    #@author Ken Wang
    #@c Add n-vectors.
    #@a a: Input vector a.
    #@a b: Input vector b.
    #@r Resultant vector.  Returns Tcl error if vectors are of different size.
    if {[llength $a] != [llength $b]} {
	return -code error "can't add vectors of different dimensions"
    }
    set dim [llength $a]
    set result {}
    for {set i 0} {$i < $dim} {incr i} {
	set r [expr [lindex $a $i] + [lindex $b $i]]
	set result [lappend result $r]
    }
    return $result
}


# -------------
# math_scaleVec
# -------------

proc math_scaleVec {v sc} {
    #@author Ken Wang
    #@c Scale a vector by a multiplicative factor.
    #@a v: Vector.
    #@a sc: Multiplicative scale factor.
    #@r Scaled vector.
    set j {}
    foreach i $v {
	set j [lappend j [expr $i * $sc]]
    }
    return $j
}


# ---------------
# math_subVectors
# ---------------
# result = a - b

proc math_subVectors {a b} {
    #@author Ken Wang
    #@c Subtract vector b from vector a.
    #@a a: vector a.
    #@a b: vector b.
    #@r a - b.
    return [math_addVectors $a [math_scaleVec $b -1]]
}


# -----------
# math_minVec
# -----------
# Returns the minimum component of the given vector.

proc math_minVec {v} {
    #@author Ken Wang
    #@c Returns the minimum component of the given vector.
    #@a v: Input vector.
    #@r Minimum component.
    set num [llength $v]
    for {set i 0} {$i < $num} {incr i} {
	set elem [lindex $v $i]
	if {$i == 0} {
	    set result $elem
	    continue
	}
	set result [expr $elem < $result ? $elem : $result]
    }
    return $result
}


# --------------
# max_findMaxPos
# --------------

proc math_findMaxPos {args} {
    #@author Nathan Wilson
    #@c Returns the index position of the maximum coordinate in a vector.
    #@a args: vector (not as a list but rather arg1 arg2 arg3 ...)
    #@r index of maximum component position in vector.
    set numargs [llength $args]
    if {$numargs == 0} {
      return -code error "ERROR: no values passed to findMaxPos"
    }

    set pos 0
    set max [lindex $args 0]

    for {set i 1} {$i < $numargs} {incr i} {
	if {[lindex $args $i] > $max} {
           set pos $i
           set max [lindex $args $i]
	}
    }
    return $pos
}


# -----------------
# path_MakePolyData
# -----------------

proc path_MakePolyData {path objName} {
    #@author Ken Wang
    #@c Create a PolyData object for the given path.
    #@a path: List of lists for a single path.
    #@a objName:  Output repository PolyData object name.

    if {![cmdExists pd_$objName]} {
	vtkPolyData pd_$objName
    }

    if {![cmdExists pts_$objName]} {
	vtkPoints pts_$objName
    }
    set pathLen [llength $path]
    foreach pos $path {
	array set items $pos
	set pt [TupleToList $items(p)]
	pts_$objName InsertNextPoint [lindex $pt 0] [lindex $pt 1] \
		[lindex $pt 2]
    }

    if {![cmdExists lines_$objName]} {
	vtkCellArray lines_$objName
    }
    for {set i 1} {$i < $pathLen} {incr i} {
        set prev [expr $i - 1]
	lines_$objName InsertNextCell 2
	lines_$objName InsertCellPoint $prev
	lines_$objName InsertCellPoint $i
    }

    pd_$objName SetPoints pts_$objName
    pd_$objName SetLines lines_$objName

    pts_$objName Delete
    lines_$objName Delete
    repos_importVtkPd -src pd_$objName -dst $objName
    pd_$objName Delete

    return
}


# -------------------
# path_ApplyTransform
# -------------------

proc path_ApplyTransform {tr pos nrm xhat} {

    #@author Ken Wang
    #@c Create a transform for this path position
    #@c (typically for reslicing the image volume).
    #@a tr: vtkTransform to update.
    #@a pos: path position (point on plane).
    #@a nrm: normal.
    #@a xhat: in plane tangent vector.

    set tmpTr __path_ApplyTransform_tr
    set tmpPd __path_ApplyTransform_pd
    set tmpPt __path_ApplyTransform_pt
    set tmpTf __path_ApplyTransform_tf

    set zhat {0 0 1}
    set theta [math_radToDeg [math_angleBtw3DVectors $zhat $nrm]]
    set axis [math_cross $zhat $nrm]


    # The following is exceedingly ugly.  Avert your eyes...
    # ---

    vtkTransform $tmpTr
    $tmpTr Identity
    eval $tmpTr RotateWXYZ $theta $axis

    vtkPoints $tmpPt
    $tmpPt InsertNextPoint 1 0 0

    vtkPolyData $tmpPd
    $tmpPd SetPoints $tmpPt

    vtkTransformPolyDataFilter $tmpTf
    $tmpTf SetInputDataObject $tmpPd
    $tmpTf SetTransform $tmpTr
    $tmpTf Update
    set pt [[$tmpTf GetOutput] GetPoint 0]

    $tmpTr Delete
    $tmpPt Delete
    $tmpPd Delete
    $tmpTf Delete

    # math_angleBtw3DVectors will return an angle in the interval
    # [0,pi].  But this is only the magnitude of the angle between $pt
    # (i.e. the non-rotationally corrected in-plane x direction) and
    # $xhat (i.e. the final rotated in-plane x direction).

    set rot [math_radToDeg [math_angleBtw3DVectors $pt $xhat]]

    set x [math_cross $pt $xhat]
    set d [math_dot $x $nrm]
    if {$d < 0.0} {
	set rot [expr - $rot]
    }

    # ---
    # End exceeding ugliness.


    $tr Identity
    eval $tr Translate $pos
    eval $tr RotateWXYZ $rot $nrm
    eval $tr RotateWXYZ $theta $axis
}


#
#  NOTE:  THE CODE BELOW IS UNRELATED TO THE CODE TO PROCESS THE PATH PLAN FILES
#         ABOVE.
#

# ------------------
# path_renumberPaths
# ------------------

proc path_renumberPaths {maps} {

#@author Nathan Wilson
#@c This proc renumbers and eliminates unused paths from the global variable
#@c gPathPoints.  The input is a tcl list of lists with the form: &p
#@c  { {oldnum1 newnum2} {oldnum1 newnum2} ..} &p
#@c Any path that is not in the mapping array will be deleted.  This proc is commonly
#@c used after you have subsampled / smoothed several paths and want to eliminate the
#@c the unsmoothed paths.
#@a maps:  tcl list of lists with the form: { {oldnum1 newnum2} {oldnum1 newnum2} ..}

  #set maps [list {1 1} {2 2} {3 3} {4 4} {5 5} {601 6} {701 7} {8 8}]

  if {[llength $maps] == 0} {
    return -code error "ERROR:  no mapping specified."
  }

  global gPathPoints

  foreach map $maps {

    foreach element [array names gPathPoints [lindex $map 0],*] {
       set item [lindex [split $element ,] 1]
       set foo([lindex $map 1],$item) $gPathPoints([lindex $map 0],$item)
    }

# future work to track modification times for paths
#    set foo([lindex $map 1],modification_time) [clock milliseconds]

  }

  catch {unset gPathPoints}

  foreach i [array names foo] {
    set gPathPoints($i) $foo($i)
  }

}


# ---------------
# path_calcLength
# ---------------

proc path_calcLength {pathId string_of_ids} {
    #@author Nathan Wilson
    #@c Calculate the length of a path.
    #@a pathId:  integer path id.
    #@a string_of_ids:  string containing ids to calculate
    #@a string_of_ids:  length of path from.

    global gPathPoints
    set path $gPathPoints($pathId,splinePts)

    set mymin 0
    set mymax [expr [llength $path] -1]
    set ids [string_parse $string_of_ids $mymin $mymax]
    puts "puts ids: $ids"
    set pts {}
    foreach i $ids {
       set pos [lindex $path $i]
       #puts "pos: $pos"
       array set items $pos
       set pt [TupleToList $items(p)]
       #puts "pt: $pt"
       lappend pts $pt
    }
    set curveLength [math_curveLength -pts $pts -closed 0]
    puts "curveLength: $curveLength"

    return $curveLength
}

# -------------
# post_surfMesh
# -------------

proc post_getSurfMesh {mesh reposObj} {

  #@author Nathan Wilson
  #@c Extract the exterior surface mesh.
  #@a mesh: vtkUnstructuredMesh object.
  #@a reposObj: return PolyData object.

  set exterior post_surfMeshallEdges

  catch {$exterior Delete}

  vtkGeometryFilter $exterior
  $exterior SetInputDataObject $mesh
  $exterior Update

  catch {repos_delete -obj $reposObj}
  repos_importVtkPd -src [$exterior GetOutput] -dst $reposObj

  return GDSC_OK

}


# ----------------------
# post_cutAndClipResults
# ----------------------

proc post_cutAndClipResults [list meshobj res_objs \
                                  pcmri_file path_id \
                                  cut_plane_nrm cut_plane_org sphere_center \
                                  sphere_radius \
                                  sliced_results_dir \
                                  vol_flow_res_file \
                                  save_repos_objs_flag \
                                  integrate_surface_flag \
                                  integrate_surface_tensorType \
                                  mesh_scaled_by_factor] {

  #@author Nathan Wilson
  #
  #@c My attempt at a swiss-army-knife proc to slice and dice finite element
  #@c results to obtain interesting quantities such as volumetric flow.
  #@c The most common use of this code will be
  #@c to calculate volumetric flow through a single vessel
  #@c located using a PCMRI slice and a path.
  #
  #@a meshobj: mesh as a vtkUnstructuredGrid.
  #@a res_objs: tcl list of the form ((timestep vector_results scalar_results) (...) ...).
  #@a res_objs: the results should be vtkFloatArrays.  Either of the results type (vector
  #@a res_objs: or scalar) can be set to NULL strings if desired.
  #@a pcmri_file: location to slice model (only reads header info)
  #@a path_id: pathid of the path (vessel) you want to intersect
  #@a sphere_center: you can explicitly specify the center of the
  #@a sphere_center: vtkCutter.  If you specify this point, the
  #@a sphere_center: path_id and pcmri_file are ignored. It also requires
  #@a sphere_center: you specify the cut plane normal (the origin is assumed
  #@a sphere_center: to be at sphere_center).
  #@a cut_plane_nrm: normal of the implicit cutting plane.  Can be NULL.
  #@a cut_plane_org: origin of the implicit cutting plane.  Can be NULL.
  #@a sphere_radius: radius of sphere centered at intersection
  #@a sphere_radius: of path and pcmri plane used to prevent extraneous
  #@a sphere_radius: vessels being include in volumetric flow calcs.
  #@a sliced_results_dir: If non-null string, the directory to write
  #@a sliced_results_dir: slices into.
  #@a vol_flow_res_file: output file to write flux vs time (usually vol. flow vs time).
  #@a save_repos_objs_flag: this saves the resulting slices in the repository
  #@a save_repos_objs_flag: (currently using hardcoded names results/* and
  #@a save_repos_objs_flag:  flat_results/*).
  #@a integrate_surface_flag:  flag for the user to request that the surface flux
  #@a integrate_surface_flag:  be integrated.
  #@a integrate_surface_tensorType:  can be 0 (scalar) or 1 (vector).
  #@a mesh_scaled_by_factor:  for those that insist on scaling the mesh to
  #@a mesh_scaled_by_factor:  to different units than the image data, the
  #@a mesh_scaled_by_factor:  path points get scaled by this factor.  Use 0.1 for
  #@a mesh_scaled_by_factor:  converting to cm.
  #
  #@r surface flux (usually volumetric flow) if vector results given
  #
  #@note If you pass a null string into this routine for sphere_radius,
  #@note it automatically
  #@note creates a sphere with a diameter equivalent to the path probe
  #@note with the current settings.  This is generally a bad idea.
  #
  #@note If you specify a null string for the output directory, no
  #@note slices are written but the surface flux (usually volumetric flow)
  #@note is still calculated if the vector quantity is specified.
  #
  #@note You should specify EITHER a cut plane normal and a sphere center
  #@note OR a pcmri filename and path_id.  If you don't specify the cut plane
  #@note normal and sphere center, it is calculated by the intersection of the
  #@note path with the pcmri plane.
  #
  #@note In general it is not a good idea to cut the results model coplanar with
  #@note a plane external model edge (i.e. outlet surface, inflow surface, etc.)
  #@note If you want the volumetric flow or results on a model face, extract
  #@note the information directly from the results file not via this routine.
  #
  #@note The mesh scaling factor is considered to be a scaling factor of the
  #@note the image data.  To calculate flow rate and create
  #@note flat objects to be compared to PCMRI data, the finite element
  #@note results are scaled by the inverse mesh scaling factor.  This is also
  #@note necessary to make the transformation from RAS space to a flat z=0 plane
  #@note work correctly.  All volumetric flows then are reported in the
  #@note native length scale of the image data!  For example, if you specify
  #@note a mesh scaling of 0.1, this means that your image data is in mm and
  #@note and your mesh is in cm.  This code would return flat geometries
  #@note in mm (the oriented profiles will also be in mm) and volumetric
  #@note flow rates in mm^3/s.
  #
  #@warning  This code currently does not create flat slices when the user
  #@warning  explicitly specifies the location of the cut plane.
  #
  #@warning  This code currently can replace any repository objects starting with
  #@warning  results/* flat_results/*.
  #
  #@warning  If it is requested, the sign of the surface flux is not defined.  The
  #@warning  user must know the overall direction of the flux to make sense of it.
  #@warning  It should, however, be consistent for the given pcmri slice.

  # tmp obj names
  set plane post_cutAndClipResultsPlane
  set planePD post_cutAndClipResultsPlanePD
  set makeItFlatMatrix post_cutAndClipResultsMatrix
  set scaleTransform post_cutAndClipResultsScaleTransform
  set scalePolyData post_cutAndClipResultsScalePD
  set sphere post_cutAndClipResultsSphere
  set cutter post_cutAndClipResultsCutter
  set clipper post_cutAndClipResultsClipper
  set cleaner post_cutAndClipResultsCleaner

  catch {$plane Delete}
  catch {repos_delete -obj $planePD}
  catch {$makeItFlatMatrix Delete}
  catch {$scaleTransform Delete}
  catch {$scalePolyData Delete}
  catch {$sphere Delete}
  catch {$clipper Delete}
  catch {$cutter Delete}
  catch {$cleaner Delete}

  repos_deleteList [repos_subList results/*]
  repos_deleteList [repos_subList flat_results/*]

  # check for sanity
  #  note: these if statements intentionally allow the user to mix a sphere_center
  #        and a pcmri_plane which is undocumented and usually not a good idea.
  if {($cut_plane_nrm != "") && ($pcmri_file != "")} {
    return -code error "ERROR:  Both a cut plane normal and pcmri plane were specified."
  }
  if {($sphere_center != "") && ($path_id != "")} {
    return -code error "ERROR:  Cannot specify both sphere_center and a path_id."
  }
  if {($cut_plane_nrm != "") && ($path_id != "")} {
    return -code error "ERROR:  Cannot mix cut_plane_nrm with a path_id."
  }
  if {($cut_plane_nrm != "") && ($cut_plane_org == "")} {
    return -coder error "ERROR  Must specify cut_plane_org if you specify cut_plane_nrm."
  }
  if {($cut_plane_org != "") && ($cut_plane_nrm == "")} {
    return -coder error "ERROR  Must specify cut_plane_nrm if you specify cut_plane_org."
  }

  # get cut plane location from pcmri slice
  if {$cut_plane_nrm == ""} {

    # check for the pcmri filename and create a plane in the same location
    if {[file exists $pcmri_file] == 0} {
      return -code error "ERROR:  pcmri file does not exist ($pcmri_file)."
    }

    #  get info from the image header
    set imageInfo [img_readHeader -file $pcmri_file]
    foreach i $imageInfo {
      set [lindex $i 0] [lindex $i 1]
    }

    set nrm [math_normalize $normal_to_plane]
    set org $top_left_corner

    if {$mesh_scaled_by_factor != ""} {
      set org [math_scaleVec $org $mesh_scaled_by_factor]
    }

    # create the implicit plane for cutting
    vtkPlane $plane
    $plane SetOrigin [lindex $org 0] [lindex $org 1] [lindex $org 2]
    $plane SetNormal [lindex $nrm 0] [lindex $nrm 1] [lindex $nrm 2]

    # need polydata for intersection
    set pts [list $top_left_corner $top_right_corner $bottom_right_corner]
    lappend pts [math_addVectors $top_left_corner \
                [math_subVectors $bottom_right_corner $top_right_corner]]
    if {$mesh_scaled_by_factor != ""} {
      set scaledpts {}
      for {set i 0} {$i < 4} {incr i} {
        lappend scaledpts [math_scaleVec [lindex $pts $i] $mesh_scaled_by_factor]
      }
      set pts $scaledpts
    }
    geom_polygonFromPts $pts $planePD

    # need the transformation matrix to flatten slices through
    # the image data
    img_calcTransformMatrixToRAS $pcmri_file $makeItFlatMatrix
    $makeItFlatMatrix Invert

    # create a scaling filter if need be
    if {$mesh_scaled_by_factor != ""} {
        vtkTransform $scaleTransform
        $scaleTransform Identity
        $scaleTransform Scale [expr 1.0/$mesh_scaled_by_factor] \
                              [expr 1.0/$mesh_scaled_by_factor] \
                              [expr 1.0/$mesh_scaled_by_factor]
        $scaleTransform Update
        vtkTransformPolyDataFilter $scalePolyData
        $scalePolyData SetTransform $scaleTransform
    }

  }

  # if the user has explicitly specified the implicit cut plane, set it here
  if {$cut_plane_nrm != ""} {
    # create the implicit plane for cutting
    vtkPlane $plane
    $plane SetOrigin [lindex $cut_plane_org 0] [lindex $cut_plane_org 1] [lindex $cut_plane_org 2]
    $plane SetNormal [lindex $cut_plane_nrm 0] [lindex $cut_plane_nrm 1] [lindex $cut_plane_nrm 2]
  }

  # get the sphere center from the path_id
  if {($path_id != "") && ($sphere_center == "")} {

    # quick check for valid path
    global gPathPoints
    if {[info exists gPathPoints($path_id,name)] != 1} {
       return -code error "ERROR: path id $path_id not found!"
    }
    if {[info exists gPathPoints($path_id,0)] == 0 && \
        [info exists gPathPoints($path_id,1)] == 0} {
       return -code error "ERROR: need at least two points in the path!"
    }

    # assume that the path points are numbered consecutively
    set tailId 1
    set pt0 $gPathPoints($path_id,0)
    if {$mesh_scaled_by_factor != ""} {
      set pt0 [math_scaleVec $pt0 $mesh_scaled_by_factor]
    }
    set sphere_center {}
    while {[info exists gPathPoints($path_id,$tailId)]} {
      set pt1 $gPathPoints($path_id,$tailId)
      if {$mesh_scaled_by_factor != ""} {
         set pt1 [math_scaleVec $pt1 $mesh_scaled_by_factor]
      }
      #puts "pt0: $pt0 pt1: $pt1"
      # check for intersection here
      if {![catch {set sphere_center [geom_intersectWithLine -obj $planePD -pt0 $pt0 -pt1 $pt1]}]} {
         puts "path pcmri intersection at $sphere_center."
         break
      }
      set pt0 $pt1
      incr tailId
    }

    if {$sphere_center == ""} {
     return -code error "ERROR:  could not find intersection of pcmri plane and path!"
    }

  }

  # now calculate the radius of the sphere from the voxel dimensions
  # if null
  if {($sphere_radius == "") && ($pcmri_file != "")} {
    global gOptions
    set ext [math_minVec $gOptions(resliceDims)]
    set vmin   [math_minVec $voxel_dims]
    set sphere_radius [expr double($ext * $vmin)/2.0]
    if {$mesh_scaled_by_factor != ""} {
       set sphere_radius [math_scaleVec $sphere_radius $mesh_scaled_by_factor]
    }
    puts "auto sphere radius: $sphere_radius"
  }
  if {$sphere_radius == ""} {
    return -code error "ERROR:  sphere_radius still a NULL string!"
  }

  # create implicit sphere
  vtkSphere $sphere
  $sphere SetCenter [lindex $sphere_center 0] \
                    [lindex $sphere_center 1] \
                    [lindex $sphere_center 2]
  $sphere SetRadius $sphere_radius


  set flow_vs_time {}
  # loop over the results
  foreach step $res_objs {

    set timestep [lindex $step 0]
    set vel_results [lindex $step 1]
    set pressure_results [lindex $step 2]

    # associate results with mesh

    # pressures are optional
    if {$pressure_results != ""} {
      [$meshobj GetPointData] SetScalars $pressure_results
    }
    # velocities are optional
    if {$vel_results != ""} {
      [$meshobj GetPointData] SetVectors $vel_results
    }
    $meshobj Update

    catch {$clipper Delete}
    catch {$cutter Delete}
    catch {$cleaner Delete}

    # do the cutting
    # note: delete the cutter every time to avoid old results from
    #       hanging around
    vtkCutter $cutter
    $cutter SetInputDataObject $meshobj
    $cutter SetCutFunction $plane
    $cutter Update

    # run it through another filter to only get the vessel of
    # interest
    vtkClipPolyData $clipper
    $clipper SetInputDataObject [$cutter GetOutput]
    $clipper SetClipFunction $sphere
    $clipper GenerateClippedOutputOn
    $clipper Update

    # need to eliminate unwanted points
    vtkCleanPolyData $cleaner
    $cleaner ConvertPolysToLinesOff
    $cleaner PointMergingOff
    $cleaner ConvertLinesToPointsOff
    $cleaner SetInputDataObject [$clipper GetClippedOutput]
    $cleaner Update

    # bring the results into Geodesic
    set velocityPD results/$timestep
    set flatPD     flat_results/$timestep
    catch {repos_delete -obj $velocityPD}
    catch {repos_delete -obj $flatPD}
    repos_importVtkPd -src [$cleaner GetOutput] -dst $velocityPD

    # currently only rotate flat if pcmri plane is specified
    # NOTE:  SHOULD BE GENERALIZED HERE TO FLATTEN ARBITRARY CUT PLANE
    #        SPECIFIED BY THE USER!!
    if {$pcmri_file != ""} {
      # rotate the image slice to be flat and line up with the pcmri plane
      # as read in by geodesic

      # need to scale this transform if by inverse of mesh scaling
      if {$mesh_scaled_by_factor != ""} {
        $scalePolyData SetInputDataObject [repos_exportToVtk -src $velocityPD]
        $scalePolyData Update
        repos_delete -obj $velocityPD
        repos_importVtkPd -src [$scalePolyData GetOutput] -dst $velocityPD
      }

      geom_applyTransformMatrix $velocityPD $makeItFlatMatrix $flatPD
      if {$integrate_surface_flag == 1} {
        # for now assume positive unit-z vector, need to make exact
        set flow [geom_integrateSurfaceFlux -obj $flatPD -nrm {0 0 1} \
                     -tensorType $integrate_surface_tensorType]
        lappend flow_vs_time [list $timestep $flow]
        puts "timestep: $timestep flow: $flow"
      }
    }

    # write out the slices if the user requests it
    if {$sliced_results_dir != ""} {
      file mkdir $sliced_results_dir
      repos_writeVtkPolyData -type ascii \
              -file [file join $sliced_results_dir oriented-$timestep.vtk] \
              -obj $velocityPD
      #NOTE:  IF STATEMENT SHOULD BE REMOVED ONCE ABOVE NOTE TO GENERALIZE
      #       IS TAKEN CARE
      if {$pcmri_file != ""} {
      repos_writeVtkPolyData -type ascii \
              -file [file join $sliced_results_dir flat-$timestep.vtk] \
              -obj $flatPD
      }
    }

    # remove the sliced objects from the repository
    # if the user doesn't want them around
    if {$save_repos_objs_flag == 0} {
	catch {repos_delete -obj $velocityPD}
        catch {repos_delete -obj $flatPD}
    }

  }

  # write out flow results here
  if {($vol_flow_res_file != "") && ($integrate_surface_flag == 1)} {
    if {[llength $flow_vs_time] > 0} {
       set fp [open $vol_flow_res_file w]
       fconfigure $fp -translation lf
       foreach flow $flow_vs_time {
	 puts $fp "[lindex $flow 0] [format %.6e [lindex $flow 1]]"
       }
       close $fp
     }
  }

  # clean up
  catch {$plane Delete}
  catch {repos_delete -obj $planePD}
  catch {$makeItFlatMatrix Delete}
  catch {$scaleTransform Delete}
  catch {$scalePolyData Delete}
  catch {$sphere Delete}
  catch {$clipper Delete}
  catch {$cutter Delete}
  catch {$cleaner Delete}

  return $flow_vs_time

}


# -------------------
# post_sampleToVolume
# -------------------

proc post_sampleToVolume {fn dims origin spacing outfn} {

  #@author Nathan Wilson
  #@c Load a vtkUnstructuredGrid file and sample to a fixed grid
  #@c (vtkImageData) format and write out the XML formatted file.
  #@a fn:  vtkUnstructuredGrid (.vtu) input file.
  #@a dims: tcl list of dimensions (x,y,z)
  #@a origin: tcl list of lower left hand corner
  #@a spacing: tcl list of voxel dimensions
  #@a outfn: output filename (.xml)

  set reader tmp-post_sampleToVolume-reader
  catch {$reader Delete}

  vtkXMLUnstructuredGridReader $reader
  $reader SetFileName $fn
  $reader Update

  set sp tmp-post_sampleToVolume-sp
  catch {$sp Delete}
  vtkImageData $sp 
  $sp SetDimensions [lindex $dims 0] [lindex $dims 1] [lindex $dims 2]
  $sp SetExtent 0 [expr [lindex $dims 0] - 1] \
                0 [expr [lindex $dims 1] - 1] \
                0 [expr [lindex $dims 2] - 1]
  $sp SetOrigin [lindex $origin 0] \
                [lindex $origin 1] \
                [lindex $origin 2]
  $sp SetSpacing [lindex $spacing 0] \
                 [lindex $spacing 1] \
                 [lindex $spacing 2]
  $sp SetScalarTypeToDouble

  # create a dummy data array
  set mysize [expr [lindex $dims 0] * [lindex $dims 1] * [lindex $dims 2]]
  set dummy tmp-nate-dummy
  catch {$dummy Delete}
  vtkDoubleArray $dummy
  $dummy SetNumberOfComponents 1
  $dummy Allocate $mysize 1
  $dummy SetNumberOfTuples $mysize
  $dummy FillComponent 0 0

  [$sp GetPointData] SetScalars $dummy
  
  set probe tmp-post_sampleToVolume-probe
  catch {$probe Delete}
  vtkProbeFilter $probe
  $probe SetSource [$reader GetOutput]
  $probe SetInputDataObject $sp
  $probe Update

  set writer tmp-post_sampleVolume-writer
  catch {$writer Delete}
  vtkXMLImageDataWriter $writer
  $writer SetInputDataObject [$probe GetImageDataOutput]
  $writer SetFileName $outfn
  $writer Write

  $writer Delete
  $probe Delete
  $dummy Delete
  $sp Delete

}


# ----------------------
# post_sampleToRawVolume
# ----------------------

proc post_sampleToRawVolume {fn dims origin spacing arrayname component outprefix} {

  #@author Nathan Wilson
  #@c Load a vtkUnstructuredGrid file and sample to a fixed grid
  #@c (vtkImageData) format and write out a UNC RAW formatted image file.
  #@a fn:  vtkUnstructuredGrid (.vtu) input file.
  #@a dims: tcl list of dimensions (x,y,z)
  #@a origin: tcl list of lower left hand corner
  #@a spacing: tcl list of voxel dimensions
  #@a arrayname:  pressure or velocity
  #@a component:  0,1,2 (for vector components)
  #@a outprefix: output filenames (.mhd and .raw extensions added)

  set reader tmp-post_sampleToVolumeRaw-reader
  catch {$reader Delete}

  vtkXMLUnstructuredGridReader $reader
  $reader SetFileName $fn
  $reader Update

  set sp tmp-post_sampleToVolumeRaw-sp
  catch {$sp Delete}
  vtkImageData $sp 
  $sp SetDimensions [lindex $dims 0] [lindex $dims 1] [lindex $dims 2]
  $sp SetExtent 0 [expr [lindex $dims 0] - 1] \
                0 [expr [lindex $dims 1] - 1] \
                0 [expr [lindex $dims 2] - 1]
  $sp SetOrigin [lindex $origin 0] \
                [lindex $origin 1] \
                [lindex $origin 2]
  $sp SetSpacing [lindex $spacing 0] \
                 [lindex $spacing 1] \
                 [lindex $spacing 2]
  $sp SetScalarTypeToDouble

  # create a dummy data array
  set mysize [expr [lindex $dims 0] * [lindex $dims 1] * [lindex $dims 2]]
  set dummy tmp-nate-dummy
  catch {$dummy Delete}
  vtkDoubleArray $dummy
  $dummy SetNumberOfComponents 1
  $dummy Allocate $mysize 1
  $dummy SetNumberOfTuples $mysize
  $dummy FillComponent 0 0

  [$sp GetPointData] SetScalars $dummy
  
  set probe tmp-post_sampleToVolumeRaw-probe
  catch {$probe Delete}
  vtkProbeFilter $probe
  $probe SetSource [$reader GetOutput]
  $probe SetInputDataObject $sp
  $probe Update

  set writeme [$probe GetImageDataOutput]
  # can only write one component at a time, so check what we want to write out
  if {$arrayname == "pressure"} {
     # fall through
  } elseif {$arrayname == "velocity"} {
    set extractor tmp-post_sampleToRAWVolume-extractor
    vtkExtractVectorComponents $extractor
    $extractor SetInputDataObject $writeme
    if {$component == 0} {
      set writeme [$extractor GetVxComponent]
    } elseif {$component == 1} {
      set writeme [$extractor GetVyComponent]
    } elseif {$component == 2} {
      set writeme [$extractor GetVzComponent]
    } else {
      return -code error "invalid component ($component)"
    }
  } else {
    return -code error "invalid arrayname ($arrayname)"
  }

  set writer tmp-post_sampleToVolumeRaw-writer
  catch {$writer Delete}
  vtkMetaImageWriter $writer
  $writer SetInputDataObject $writeme
  $writer SetFileName $outprefix.mhd
  $writer SetRAWFileName $outprefix.raw
  $writer Write

  catch {$extractor Delete}
  $writer Delete
  $probe Delete
  $dummy Delete
  $sp Delete

}



proc post_sample {filenames dims scaleFactorBB} {

  #@author Nathan Wilson
  #@c Wrapper function that calls post_sample* functions
  #@c to write out structured data set version of results.  This
  #@c code automatically calculates the origin and spacing based on
  #@c the bounding box, scale factor, and requested dimensions.
  #@c This code currently only outputs the pressure and velocity fields
  #@c for the RAW files.
  #@a filenames:  tcl list of vtkUnstructuredGrid (.vtu) input files.
  #@a dims: tcl list of dimensions (x,y,z)
  #@a scaleFactorBB:  this factor isotropically scales the bounding box
  #@a scaleFactorBB:  so that the results can be completely contained inside the volume.

  # read in the first filename to get the size of the bounding box
  set reader tmp-post_sample-reader
  catch {$reader Delete}

  vtkXMLUnstructuredGridReader $reader
  $reader SetFileName [lindex $filenames 0]
  $reader Update

  set bbox [[$reader GetOutput] GetBounds]
  puts "original bounds: $bbox"

  $reader Delete

  set xlen [expr 1.0*([lindex $bbox 1] - [lindex $bbox 0])*$scaleFactorBB]
  set xmin [expr ([lindex $bbox 0] + [lindex $bbox 1])/2.0 - $xlen/2.0]
 
  set ylen [expr 1.0*([lindex $bbox 3] - [lindex $bbox 2])*$scaleFactorBB]
  set ymin [expr ([lindex $bbox 2] + [lindex $bbox 3])/2.0 - $ylen/2.0]

  set zlen [expr 1.0*([lindex $bbox 5] - [lindex $bbox 4])*$scaleFactorBB]
  set zmin [expr ([lindex $bbox 4] + [lindex $bbox 5])/2.0 - $zlen/2.0]

  set origin [list $xmin $ymin $zmin]
 
  set sX [expr 1.0*$xlen/double([lindex $dims 0])]
  set sY [expr 1.0*$ylen/double([lindex $dims 1])]
  set sZ [expr 1.0*$zlen/double([lindex $dims 2])]

  set spacing [list $sX $sY $sZ]

  set id 0

  puts "origin  : $origin"
  puts "spacing : $spacing"

  foreach fn $filenames {

    puts "working on file: $fn"

    set pre [file rootname $fn]

    post_sampleToVolume $fn $dims $origin $spacing $pre\-sampled.vti

    set arrayname pressure
    set component 0
    set outprefix $pre\-pressure 

    post_sampleToRawVolume $fn $dims $origin $spacing $arrayname $component $outprefix

    set arrayname velocity
    set component 0
    set outprefix $pre\-velocity-x

    post_sampleToRawVolume $fn $dims $origin $spacing $arrayname $component $outprefix

    set arrayname velocity
    set component 1
    set outprefix $pre\-velocity-y

    post_sampleToRawVolume $fn $dims $origin $spacing $arrayname $component $outprefix

    set arrayname velocity
    set component 2
    set outprefix $pre\-velocity-z

    post_sampleToRawVolume $fn $dims $origin $spacing $arrayname $component $outprefix

  }

}

# ----------------
# repos_deleteList
# ----------------

proc repos_deleteList {objList} {
    #@author Ken Wang
    #@c Delete a list of objects in the repository.
    #@a objList: list of objects to delete.
    foreach o $objList {
	repos_delete -obj $o
    }
}


# -------------
# repos_subList
# -------------

proc repos_subList {pat} {
    #@author Ken Wang
    #@c Return a list of objects in the repository matching the
    #@c the given pattern.
    #@a pat:  Pattern to match.
    #@r A list of objects matching pattern.
    set objs [repos_list]
    set result {}
    foreach o $objs {
	if {[string match $pat $o]} {
	    lappend result $o
	}
    }
    set objs {}
    return $result
}


# ------------
# repos_sorted
# ------------

proc repos_sorted {pat field} {
    #@author Ken Wang
    #@c Uses repos_subList to find all the objects matching 
    #@c the requested pattern, and then sorts based on the given field.
    #@note This routine assumes object names of the form "/a/b/c/..."
    #@a pat:  Pattern to match.
    #@a field:  Object field to use as key to sort.
    #@r Numerically sorted list of objects.
    set objs [repos_subList $pat]
    set numObjs [llength $objs]
    set result {}
    foreach o $objs {
	set toks [lreplace [split $o /] 0 0]
	set tag [lindex $toks $field]
	set resultArr($tag) $o
    }
    set tags [lsort -integer [array names resultArr]]
#    set numOut [llength $tags]
#    if {$numObjs != $numOut} {
#	puts "ERR: not a unique identifier field"
#	return $result
#    }
    foreach t $tags {
	lappend result $resultArr($t)
    }
    return $result
}


# ---------------
# repos_getObjTok
# ---------------

proc repos_getObjTok {name field} {
    #@author Ken Wang
    #@c Return the nth field of the objects name.
    #@note This routine assumes object names of the form "/a/b/c/..."
    #@a name:  Repository name.
    #@a field:  The field number to return.
    #@r The nth field of the object name.
    set toks [lreplace [split $name /] 0 0]
    return [lindex $toks $field]
}


# --------------
# repos_keyInUse
# --------------

proc repos_keyInUse {obj key} {
    #@author Ken Wang
    #@c Check and see if an object already has a key assigned.
    #@a obj:  Repository object name.
    #@a key:  Key to check for.
    #@r Boolean result 0 or 1
    set keys [repos_getLabelKeys -obj $obj]
    if {[lsearch -exact $keys $key] >= 0} {
	return 1
    }
    return 0
}



# --------------------
# repos_delete_wrapper
# --------------------

proc repos_delete_wrapper args {
  #@author Nathan Wilson
  #@c Wrapper to track properties of objects deleted.
  #@a args: parameters passed to repos_delete cmd.

  set trimargs [string trim $args]
  if {$trimargs == ""} {
     return [__repos_delete]
  }
  if {[llength $trimargs] != 2} {
     return [__repos_delete $trimargs]
  }
  if {[lindex $trimargs 0] != "-obj"} {
     return [__repos_delete $trimargs]
  }
  set obj [lindex $trimargs 1]
  global gReposObjDisp
  if [repos_exists -obj $obj] {
    if {[lsearch [array names gReposObjDisp] $obj] >= 0} {
      puts "WARNING: Deleted object $obj appearing in $gReposObjDisp($obj)."
    }
  }
  return [__repos_delete -obj $obj]

}

# ---------------
# solid_instances
# ---------------

proc solid_instances {} {
    #@author Ken Wang
    #@c Return a list of solid models currently in the repository.
    #@r list of repository SolidModel objects.
    set objs [repos_list]
    set solids {}
    foreach o $objs {
	if {[repos_type -obj $o] == "SolidModel"} {
	    set solids [lappend solids $o]
	}
    }
    return $solids
}


# --------------
# solid_unionSet
# --------------

proc solid_unionSet {operands dstName} {
    #@author Ken Wang
    #@c Function which unions a lists of solids together
    #@c to create dstName.
    #@a operands: list of SolidModel objects.
    #@a dstName: resulting solid model name.
    #@note The solids must have all been created with the
    #@note the same solid modeler.
    if {[llength $operands] < 2} {
	return -code error "more than one operand required"
    }
    if {[repos_exists -obj $dstName]} {
	return -code error "object $dstName already exists"
    }
    foreach o $operands {
	if {[repos_type -obj $o] != "SolidModel"} {
	    return -code error "object $o not of type SolidModel"
	}
    }
    set first [lindex $operands 0]
    set k [$first GetKernel]
    set rest {}
    for {set i 1} {$i < [llength $operands]} {incr i} {
	set rest [lappend rem [lindex $operands $i]]
    }
    foreach o $operands {
	if {[$o GetKernel] != $k} {
	    return -code error "objects of multiple kernel types found"
	}
    }
    set tmpSrc __solid_unionSet/tmp/src
    set tmpDst __solid_unionSet/tmp/dst
    catch {repos_delete -obj $tmpSrc}
    catch {repos_delete -obj $tmpDst}

    solid_copy -src $first -dst $tmpSrc
    for {set i 0} {$i < [llength $rest]} {incr i} {
	catch {repos_delete -obj $tmpDst}
	solid_union -result $tmpDst -a $tmpSrc -b [lindex $rest $i]
	repos_delete -obj $tmpSrc
	solid_copy -src $tmpDst -dst $tmpSrc
    }

    solid_copy -src $tmpDst -dst $dstName
}


proc string_parse {mystr min max} {

  #author Nathan Wilson
  #@c Procedure to parse a human friendly specified list of integer
  #@c ids into an explicit list.  For example, a valid string to
  #@c to process is: "begin-end by 5, 1-3, 72 75, 103-101 by -1,25,27"
  #@c etc.
  #@a mystr: String to parse.
  #@a min: minimum allowable value.
  #@a max: maximum allowable value.
  #@r Explicit list (arranged numerically) identical to that specified
  #@r in mystr.

  # return list
  set rtn {}

  # first seperate by commas
  set scommas [split [string trim $mystr] ,]
  foreach i $scommas {
    set s [string trim [string tolower $i]]
    # just a single value
    if {[llength [split $s -]] == 1} {
       foreach little $s {
         if {[math_isInt $little] == 1} {
	   if {$little < $min} {
             lappend rtn $min
	   } elseif {$little > $max} {
             lappend rtn $max
	   } else {
             lappend rtn $little
	   }
         } elseif {$little == "begin"} {
           lappend rtn $min
         } elseif {$little == "end"} {
           lappend rtn $max
         } else {
           puts "Warning: Ignoring $s."
         }
       }
       continue
    }

    set increment 1

    # comma separated list with "by" keyword
    if {[scan $s "%s by %s" l1 l2] == 2} {
       if {[math_isInt $l2] == 1} {
         set increment $l2
       } else {
         puts "Warning: Invalid increment $l2.  Ignoring $s."
         continue
       }
    }

    set begin {}
    set end {}
    set sdash [split [string trim $l1] "-"]
    if {[llength $sdash] != 2} {
      puts "Warning: $sdash has more that two components and is being ignored."
      continue
    }
    if {[math_isInt [lindex $sdash 0]] == 1} {
      set begin [lindex $sdash 0]
      if {$begin < $min} {
        set begin $min
      }
      if {$begin > $max} {
        set begin $max
      }
    } elseif {[lindex $sdash 0] == "begin"} {
       set begin $min
    } elseif {[lindex $sdash 0] == "end"} {
       set begin $max
    } else {
      puts "Warning:  Ignoring [lindex $sdash 0]."
      continue
    }
    if {[math_isInt [lindex $sdash 1]] == 1} {
      set end [lindex $sdash 1]
      if {$end > $max} {
        set end $max
      }
      if {$end < $min} {
        set end $min
      }
    } elseif {[lindex $sdash 1] == "begin"} {
       set end $min
    } elseif {[lindex $sdash 1] == "end"} {
       set end $max
    } else {
      puts "Warning: Ignoring [lindex $sdash 1]."
      continue
    }

    if {$increment > 0} {
      for {set j $begin} {$j <= $end} {incr j $increment} {
	if {$j < $min} break
	if {$j > $max} break
        lappend rtn $j
      }
      lappend rtn $end
    } else {
      for {set j $begin} {$j >= $end} {incr j $increment} {
        if {$j < $min} break
	if {$j > $max} break
        lappend rtn $j
      }
      lappend rtn $end
    }
  }

  set rtn [lsort -unique -dictionary $rtn]
  return $rtn
}


# -------
# trimInt
# -------

proc trimInt {str} {
    #@author Ken Wang
    #@c Remove leading zeros from a string.
    #@a str: string.
    #@r String without zeros or 0.
    while { [string length $str] > 0 } {
        set leadChar [string range $str 0 0]
        if { [string compare $leadChar "0"] == 0 } {
            set str [string range $str 1 end]
        } else {
            return $str
        }
    }
    return 0
}


# -------------
# trimLeadZeros
# -------------

proc trimLeadZeros {str} {
    #@author Ken Wang
    #@c Remove leading zeros from a string.
    #@a str: string.
    #@r String without zeros or "".
    while { [string length $str] > 0 } {
        set leadChar [string range $str 0 0]
        if { [string compare $leadChar "0"] == 0 } {
            set str [string range $str 1 end]
        } else {
            return $str
        }
    }
    return ""
}

 ## ********************************************************
 ##
 ## Name: tail
 ##
 ## Description:
 ## A combination of tail -f and egrep.
 ## Loops forever on a file NAME.
 ## If the file doesn't exist it will wait for it to appear
 ## and then work on it.
 ##
 ## Parameters:
 ## file - a filename
 ## rx - a regular expression pattern to filter the lines on
 ##
 ## Usage:
 ##       tail filename [ regex pattern ] [ delay in ms ]
 ##
 ## If the file disappears or is replaced by a new file with the
 ## same name it is handled transparently.
 ##
 ## The behaviour of this function is based upon tail in the gnu
 ## fileutils alpha release 4.0 package.
 ##
 ## Comments:
 ## Only the filename argument is required.  If a regex pattern is
 ## given as the second argument, it will be used to filter the
 ## lines.
 ## auto-cancel if $var is deleted

 proc tail { file { rx .+ } { delay 2000 } { var "" } { stats "" } { fid "" } } {

      set inode {}
      set size  {}
      set mtime {}
      foreach { inode size mtime } $stats { break }

      if { [ string length $var ] > 0 } {
         if  { ! [ info exist $var ] } {
             catch { unset ${var}_tailId } err
             return
         }
     } else {
         ;## default var name
         set var ::tail_$file
     }
     if  { ! [ regexp {^::} $var ] } {
             set var "::$var"
     }
      ;## if the file exists at this iteration, tail it
      if { [ file exists $file ] } {
         file stat $file fstat
         set _inode $fstat(ino)
         set _size  $fstat(size)
         set _mtime $fstat(mtime)

         ;## if the inode has changed since the last iteration,
         ;## reopen the file.  this is from tail v4.0 in the
         ;## gnu fileutils package.
         if { $_inode != $inode } {
            catch { close $fid; set fid {} }
         } else {
            if { $_size < $size } {
               catch { seek $fid 0 }
            }
            if { $_size == $size && $_mtime != $mtime } {
               catch { seek $fid 0 }
            }
         }

         ;## if the file is not open, open it!
         if { ! [ string length $fid ] } {
            set fid [ open $file r ]
            fconfigure $fid -blocking off
            fconfigure $fid -buffering line
         }

         set inode $_inode
         set size  $_size
         set mtime $_mtime

         ;## set a variable with the content of the
         ;## regex filtered line.
         ;## use a temp var to store variable
         ;## until all lines are read
         ;## then set the global var
         ;## so trace function is not called for every line
         set temp {}

         while { [ gets $fid line ] >= 0 } {
            if { [ regexp -- $rx $line match ] } {
               ;## put a trace on variable to
               ;## read the tail output.
               append temp "$line\n"
            }
         }

         ;## setting this will invoke trace function
         if { [ string length $temp ] } {
            set $var $temp
         }

      ;## if the file doesn't exist, make sure we aren't
      ;## creating an NFS orphan.
      } else {
         ;## maybe the file got nuked? Handle it!
         if { [ string length $fid ] } {
            catch { close $fid; set fid {} }
         }
      }

      ;## lather, rinse, repeat.  This is NOT recursion!
      set stats \{[ list $inode $size $mtime ]\}
      set ${var}_tailId [ after $delay \
        [ subst { tail ${file} $rx $delay ${var} $stats $fid } ] ]
 }
 ## ********************************************************

 ## ********************************************************
 ##
 ## Name: cancelTail
 ##
 ## Description:
 ## Removes the trce associated with am invocation of "tail"
 ## Parameters:
 ## file - filename being tailed
 ## cmd - command handling the trace
 ##
 ## Usage:
 ##  cancelTail filename [ command ]
 ##
 ## Comments:
 ## see the man page for "trace"

 proc cancelTail { file { var "" } { cmd "" } } {

     if  { [ catch {
         if  { ! [ string length $var ] } {
             set var ::tail_$file
             set aftervar ${var}_tailId
         } else {
         set aftervar ${var}_tailId
         }
         catch { after cancel [ set $aftervar ] }
         if { ! [ string length $cmd ] } {
            catch { set cmd [ eval lindex [ trace vinfo $var ] 1 ] }
         }
         catch { trace vdelete $var w $cmd }
     } err ] } {
         return -code error $err
     }
 }
 ## ********************************************************

 ## ********************************************************
 ##
 ## Name: htmlFilter
 ##
 ## Description:
 ## Convenience function for cleaning up html lines
 ## Have your trace handler function call this to
 ## pretty-print html.
 ##
 ## Parameters:
 ##
 ## Usage:
 ##
 ## Comments:
 ## Very simpleminded.  Fairly quick.

 proc htmlFilter { text } {

      ;## table of escape characters
      array set esc {
      lt     <    gt     >    quot   \"   ob     \x7b  cb    \x7d
      nbsp   \xa0 iexcl  \xa1 cent   \xa2 pound  \xa3 curren \xa4
      yen    \xa5 brvbar \xa6 sect   \xa7 uml    \xa8 copy   \xa9
      ordf   \xaa laquo  \xab not    \xac shy    \xad reg    \xae
      hibar  \xaf deg    \xb0 plusmn \xb1 sup2   \xb2 sup3   \xb3
      acute  \xb4 micro  \xb5 para   \xb6 middot \xb7 cedil  \xb8
      sup1   \xb9 ordm   \xba raquo  \xbb frac14 \xbc frac12 \xbd
      frac34 \xbe iquest \xbf Agrave \xc0 Aacute \xc1 Acirc  \xc2
      Atilde \xc3 Auml   \xc4 Aring  \xc5 AElig  \xc6 Ccedil \xc7
      Egrave \xc8 Eacute \xc9 Ecirc  \xca Euml   \xcb Igrave \xcc
      Iacute \xcd Icirc  \xce Iuml   \xcf ETH    \xd0 Ntilde \xd1
      Ograve \xd2 Oacute \xd3 Ocirc  \xd4 Otilde \xd5 Ouml   \xd6
      times  \xd7 Oslash \xd8 Ugrave \xd9 Uacute \xda Ucirc  \xdb
      Uuml   \xdc Yacute \xdd THORN  \xde szlig  \xdf agrave \xe0
      aacute \xe1 acirc  \xe2 atilde \xe3 auml   \xe4 aring  \xe5
      aelig  \xe6 ccedil \xe7 egrave \xe8 eacute \xe9 ecirc  \xea
      euml   \xeb igrave \xec iacute \xed icirc  \xee iuml   \xef
      eth    \xf0 ntilde \xf1 ograve \xf2 oacute \xf3 ocirc  \xf4
      otilde \xf5 ouml   \xf6 divide \xf7 oslash \xf8 ugrave \xf9
      uacute \xfa ucirc  \xfb uuml   \xfc yacute \xfd thorn  \xfe
      yuml   \xff amp    &	 #013	\n
      }

      ;## special handler for list items
      regsub -all {<[Ll][Ii]>}          $text {  * } text
      ;## and for images
      regsub -all {<[Ii][Mm][Gg][^>]+>} $text {* }   text
      ;## all other tags just GO AWAY
      regsub -all {<[^>]+>}             $text {}     text
      ;## escape curlies properly
      regsub -all {\\\}}                $text \}     text
      regsub -all {\\\{}                $text \{     text
      ;## maybe we are rendering something with embedded
      ;## tcl code -- declaw it!
      regsub -all {\$}                  $text {\\$}  text
      regsub -all {\[}                  $text {\\[}  text
      regsub -all {\]}                  $text {\\]}  text
      ;## replace html escape sequences with literals
      regsub -all -nocase {&([0-9a-z#]*);} $text {$esc(\1)} text
      ;## this line causes the $esc() to be evaluated
      set text [ subst $text ]

      ;## and ship it back!
      return $text
 }
 ## ********************************************************


proc view_slice {fn} {
   set slice /tmp/view_slice
   catch {repos_delete -obj $slice}
   img2_readSlice $fn $slice
   gdscView $slice
}

   
