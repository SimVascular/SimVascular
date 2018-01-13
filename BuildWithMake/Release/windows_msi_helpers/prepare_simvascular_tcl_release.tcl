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


