#
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
#
# All rights reserved.
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
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#

set lib_ext so
if {$tcl_platform(os) == "Darwin"} {
  set lib_ext dylib
} elseif {$tcl_platform(platform) == "windows"} {
  set lib_ext dll
}

#Load all the dynamic libraries!
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_tcl_interp.$lib_ext Getinterp} msg]} {
    puts "liblib_simvascular_utils Getinterp $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_math.$lib_ext Math} msg]} {
    puts "liblib_simvascular_utils Math $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_repository.$lib_ext Repos} msg]} {
    puts "liblib_simvascular_repository $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_lsetcore.$lib_ext Lsetcore} msg]} {
    puts "liblib_simvascular_lset Lsetcore $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_lsetv.$lib_ext Lsetv} msg]} {
    puts "liblib_simvascular_lset Lsetv $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_sysgeom.$lib_ext Geom} msg]} {
    puts "liblib_simvascular_sysgeom $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_image.$lib_ext Image} msg]} {
    puts "liblib_simvascular_image $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_gdscpost.$lib_ext Gdscpost} msg]} {
    puts "liblib_simvascular_post $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_solid.$lib_ext Solid} msg]} {
    puts "liblib_simvascular_solid $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_polydatasolid.$lib_ext Polydatasolid} msg]} {
    puts "liblib_simvascular_polydatasolid $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_opencascade.$lib_ext Occtsolid} msg]} {
    puts "liblib_simvascular_opencascade $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_mesh.$lib_ext Gdscmesh} msg]} {
    puts "liblib_simvascular_mesh $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_tetgen_mesh.$lib_ext Tetgenmesh} msg]} {
    puts "liblib_simvascular_tetgen_mesh $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_adaptor.$lib_ext Adapt} msg]} {
    puts "liblib_simvascular_adaptor $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_tet_adaptor.$lib_ext Tetgenadapt} msg]} {
    puts "liblib_simvascular_tet_adaptor $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_cvitk2d.$lib_ext Itkls2d} msg]} {
    puts "liblib_simvascular_tet_cvitk2d $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_cvitk3d.$lib_ext Itkls3d} msg]} {
    puts "liblib_simvascular_tet_cvitk3d $lib_ext: $msg"
}
if {[catch {load $env(SV_HOME)/Lib/liblib_simvascular_itkutils.$lib_ext Itkutils} msg]} {
    puts "liblib_simvascular_tet_itkutils $lib_ext: $msg"
}

