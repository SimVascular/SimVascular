# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
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

# ------------
# vis_gridInit
# ------------

proc vis_gridInit {ren} {

    if {[cmdExists vis_grid_reader_$ren]} {
	return
    }

    vtkImageReader vis_grid_reader_$ren
    vis_grid_reader_$ren SetDataScalarTypeToShort
    vis_grid_reader_$ren FileLowerLeftOn
    vis_grid_reader_$ren SetFileDimensionality 2
    vis_grid_reader_$ren SetFilePattern "\%s.\%03d"
    vis_grid_reader_$ren SetDataByteOrderToBigEndian

    if {![cmdExists vis_grid_lut]} {
	vtkLookupTable vis_grid_lut
	vis_grid_lut SetHueRange 0.0 0.0
	vis_grid_lut SetSaturationRange 0.0 0.0
	vis_grid_lut SetValueRange 0.0 1.0
	vis_grid_lut SetNumberOfColors 256
	vis_grid_lut Build
    }

    vtkDataSetMapper vis_grid_mapper_$ren
    vis_grid_mapper_$ren SetInputDataObject [vis_grid_reader_$ren GetOutput]
    vis_grid_mapper_$ren SetLookupTable vis_grid_lut
    vis_grid_mapper_$ren ScalarVisibilityOff

    vtkActor vis_grid_actor_$ren
    vis_grid_actor_$ren SetMapper vis_grid_mapper_$ren
    [vis_grid_actor_$ren GetProperty] SetRepresentationToWireframe
    [vis_grid_actor_$ren GetProperty] SetOpacity 0.1
    vis_gridSetColor $ren 0 0 0

    $ren AddActor vis_grid_actor_$ren
}


# ---------------------
# vis_gridSetDataOrigin
# ---------------------

proc vis_gridSetDataOrigin {ren x y z} {

    if {![cmdExists vis_grid_reader_$ren]} {
	return
    }

    vis_grid_reader_$ren SetDataOrigin $x $y $z
}


# ----------------------
# vis_gridSetDataSpacing
# ----------------------

proc vis_gridSetDataSpacing {ren x y z} {

    if {![cmdExists vis_grid_reader_$ren]} {
	return
    }

    vis_grid_reader_$ren SetDataSpacing $x $y $z
}


# ---------------------
# vis_gridSetDataExtent
# ---------------------

proc vis_gridSetDataExtent {ren w h} {

    if {![cmdExists vis_grid_reader_$ren]} {
	return
    }

    vis_grid_reader_$ren SetDataExtent 0 [expr $w-1] 0 [expr $h-1] 1 1
}


# ----------------
# vis_gridSetColor
# ----------------

proc vis_gridSetColor {ren r g b} {

    if {![cmdExists vis_grid_reader_$ren]} {
	return
    }

    [vis_grid_actor_$ren GetProperty] SetDiffuseColor $r $g $b
}


# ------------------
# vis_gridSetOpacity
# ------------------

proc vis_gridSetOpacity {ren o} {

    if {![cmdExists vis_grid_reader_$ren]} {
	return
    }

    [vis_grid_actor_$ren GetProperty] SetOpacity $o
}


# ---------------
# vis_gridScalars
# ---------------

proc vis_gridScalars {ren flag} {

    if {![cmdExists vis_grid_actor_$ren]} {
	return
    }

    if {$flag == 0} {
	vis_grid_mapper_$ren ScalarVisibilityOff
	[vis_grid_actor_$ren GetProperty] SetRepresentationToWireframe
    } elseif {$flag == 1} {
	vis_grid_actor_$ren ScalarVisibilityOn
	[vis_grid_actor_$ren GetProperty] SetRepresentationToSurface
    } else {
	puts "ERR: need boolean flag"
    }
}


# ------------
# vis_gridShow
# ------------

proc vis_gridShow {ren objName} {

    global env
    global progname

    if {![cmdExists vis_grid_reader_$ren]} {
	vis_gridInit $ren
    }

    if {![repos_exists -obj $objName]} {
	puts [format "ERR: %s not found in repository" $objName]
	return -code error
    }

#    regsub -all / $objName . tmp
#    set objFilePrefix [format "%s/.%s/%s" $env(HOME) $progname $tmp]
#    set objFileName [format "%s/.%s/%s.001" $env(HOME) $progname $tmp]
#    repos_writeVtkStructuredPoints -obj $objName -type bin -file $objFileName

#    vis_grid_reader_$ren SetFilePrefix $objFilePrefix

    vis_grid_mapper_$ren SetInputDataObject [repos_exportToVtk -src $objName]

    vis_grid_mapper_$ren Update
    set range [[vis_grid_mapper_$ren GetInput] GetScalarRange]
    vis_grid_mapper_$ren SetScalarRange [lindex $range 0] [lindex $range 1]

    [vis_grid_actor_$ren GetProperty] SetRepresentationToWireframe

    vis_render $ren
}
