#
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical 
# Software Corporation, University of California, San Diego.
# All rights reserved.
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

proc prepare_simvascular_tcl_release {dist_dir} {

  global tcl_platform

  file mkdir $dist_dir/Tcl/SimVascular_2.0

  foreach dir [list $dist_dir/UnTcl/Common/General \
                    $dist_dir/UnTcl/Common/Vis \
                    $dist_dir/UnTcl/Common/Visualization \
                    $dist_dir/UnTcl/OSMSC \
                    $dist_dir/UnTcl/SimVascular_2.0/Core \
                    $dist_dir/UnTcl/SimVascular_2.0/GUI \
                    $dist_dir/UnTcl/SimVascular_2.0/Plugins] {
    file delete [file tail $dir]-code.tcl

    # skip if directory doesn't exist
    if {![file exists $dir]} continue

    exec touch [file tail $dir]-code.tcl
    foreach fn [glob $dir/*.tcl] {
        if {[file tail $fn] == "pkgIndex.tcl"} continue
	puts "adding code from $fn to [file tail $dir]-code.tcl..."
	exec cat $fn >> [file tail $dir]-code.tcl
        exec echo {} >> [file tail $dir]-code.tcl
    }

    exec mv [file tail $dir]-code.tcl $dist_dir/Tcl/SimVascular_2.0/[file tail $dir]-code.tcl

  }
}

prepare_simvascular_tcl_release [lindex $argv 0]


