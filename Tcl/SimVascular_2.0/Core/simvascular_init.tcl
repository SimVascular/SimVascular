#===========================================================================
#    
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical
# Software Corporation, University of California, San Diego.
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

# Default: Use vtkTkRenderWidget.
set gPathBrowser(use_vtkTkRenderWidget) 1

# default for image data type
global gImageVol
set gImageVol(xml_filename) {}
set gImageVol(data_dirname) {}
set gImageVol(filename) {}
set gImageVol(pixel_representation) Short
set gImageVol(endian) BigEndian
# default directionCosines for DICOM data
set gImageVol(directionCosines) {{1 0 0} {0 -1 0}  {0 0 -1}}
set gImageVol(filePattern) "\%s.\%03d"
# set some default options
global gOptions
set gOptions(image_data_type) {DICOM}
set gOptions(resliceDims) {64 64}
set gOptions(lsetDisplayWindows) {}
set gOptions(orientImgVolToRAS) 1
set gOptions(pathDisplayPointSize) .3
set gOptions(calc_flow_with_constant_pixels) 0
set gOptions(facet_max_edge_size) {}
set gOptions(window_level_increment) 0.01
# show_progress_widget currently ignored
# since progress meter code needed to be updated
# for vtk-4.4 (value in gOptions is ignored)
set gOptions(show_progress_widget) 0
set gOptions(facet_max_edge_size) 1.0
set gOptions(max_num_polys_for_isosurface) 250000
set gOptions(vis_lod_max_num_polys) 250000
set gOptions(use_lod_for_isosurface) 0
set gOptions(pathplan_axes_scale_factor) 50.0
set gOptions(pathplan_axes_width) 5.0
set gOptions(batch_server_user) nwilson
set gOptions(vis_vol_render_method) texture3d
set gOptions(display_only_largest_isosurface) 1
# the following is a hack variable
set gOptions(post_read_transport) 0
set gOptions(post_calc_wall_shear) 0
set gOptions(vis_vectors_with_glyphs) 1
set gOptions(lset_allow_interrupt) 1
set gOptions(lset_interrupt) 0
set gOptions(plot_canvas_width) 250
set gOptions(plot_canvas_height) 125
set gOptions(plot_num_format_str) {%8.3f}
set gOptions(post_solver) phasta
set gOptions(smooth_isosurface) 0
set gOptions(smooth_num_iters) 500
set gOptions(smooth_relax_factor) 0.02
#set gOptions(meshing_solid_kernel) Parasolid
#set gOptions(meshing_solid_kernel) Discrete
set gOptions(meshing_solid_kernel) PolyData
#set gOptions(meshing_kernel) MeshSim
set gOptions(meshing_kernel) TetGen
set gOptions(adaptor_sphere) {-1 0 0 0 0}
set gOptions(exclude_walls_from_flows) 1

set gOptions(color_for_isosurface) green
set gOptions(color_for_all_splines) mediumturquoise
set gOptions(color_for_all_paths) royalblue4
set gOptions(color_for_spline) red
set gOptions(color_for_path) lightblue
set gOptions(line_width_for_all_splines) 2
set gOptions(line_width_for_all_paths) 3
set gOptions(line_width_for_spline) 2
set gOptions(line_width_for_path) 3


set gOptions(color_for_seeds) green
set gOptions(line_width_for_seeds) 1
set gOptions(opacity_for_seeds) 1

set gOptions(color_for_groups) white
set gOptions(line_width_for_groups) 3

set gOptions(color_for_new_segmentations) red
set gOptions(line_width_for_new_segmentations) 3

set gOptions(color_for_saved_surface) steelblue
set gOptions(opacity_for_saved_surface) .8
set gOptions(line_width_for_saved_surface) 3

set gOptions(color_for_new_surface) red
set gOptions(opacity_for_new_surface) .8

set gOptions(color_for_model) darkred
set gOptions(opacity_for_model) 1

set gOptions(color_for_faces) firebrick
set gOptions(opacity_for_faces) 1

solid_setKernel -name $gOptions(meshing_solid_kernel)

global gPathBrowser
set gPathBrowser(currGroupName) ""
set gPathBrowser(solid_sample) 20
set gPathBrowser(align_mtd_radio) dist
set gPathBrowser(solid_opacity) 0.5

# main vis window options
set gRen3d {}
set gRen3dCopies 0
set gRen3dFreeze 0

# needed by RCR core procs
set gRCR_stdout_fp {stdout}

