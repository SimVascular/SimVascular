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

proc Proc_Show {{namepat *} {file stdout}} {
  foreach proc [info procs $namepat] {
    set space ""
    puts -nonewline $file "proc $proc {"
      foreach arg [info args $proc] {
        if [info default $proc $arg value] {
	   puts -nonewline $file "$space{$arg $value}"
	} else {
	   puts -nonewline $file $space$arg
	}
	   set space " "
	}

	# No newline needed because info body may return a value
	# that starts with a newline

	puts -nonewline $file "} {"
	puts -nonewline $file [info body $proc]
	puts $file "}"
  }
}

#set topdir Core
#set topdir Plugins
#set topdir GUI
if {[llength $argv] == 0} {
    return -code error "ERROR: no top level directory specified!"
}
set topdir [lindex $argv 0]
puts "Sorting ($topdir)"

set all_files [glob $topdir/*.tcl]
file mkdir sorted-$topdir

foreach i $all_files {
    source $i
}

foreach i [info procs *] {
  set fp [open sorted-$topdir/$i.tcl w]
  Proc_Show $i $fp
  close $fp  
}

foreach i {auto_load_index unknown auto_import auto_execok auto_qualify auto_load history tclLog Proc_Show} {
    file delete sorted-$topdir/$i.tcl
}

#foreach i [lsearch tcl_rcFileName tcl_version argv0 argv tcl_interactive auto_path auto_index env tcl_pkgPath tcl_patchLevel argc tcl_library tcl_platform] {
#}

puts "[info globals]"

					
				       
