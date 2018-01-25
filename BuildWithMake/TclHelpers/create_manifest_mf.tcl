#!/bin/tclsh

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

proc create_manifest_qrc_file {symbolicName outFile} {
    set symbolicNamePeriod [regsub -all _ $symbolicName .]
    if [catch {set outfp [open $outFile w]}] {
	return -code error "ERROR: filename does not exist ($inFile)!!"
    }
    puts $outfp "<!DOCTYPE RCC><RCC version=\"1.0\">"
    puts $outfp "<qresource prefix=\"/$symbolicNamePeriod/META-INF\">"
    puts $outfp "<file>MANIFEST.MF</file>"
    puts $outfp "</qresource>"
    puts $outfp "</RCC>"
    close $outfp
}

proc parse_manifest_headers_cmake_file {symbolicName inFile outFile} {
    set symbolicNamePeriod [regsub -all _ $symbolicName .]
    if [catch {set infp [open $inFile r]}] {
	return -code error "ERROR: filename does not exist ($inFile)!!"
    }
    if [catch {set outfp [open $outFile w]}] {
	return -code error "ERROR: filename does not exist ($inFile)!!"
    }
    puts $outfp "Plugin-SymbolicName: $symbolicNamePeriod"
    while {[gets $infp line] >= 0} {
	set args {}
	set line [string trim $line]
	if {$line == ""} continue
	if {[string index $line 0] == "#"} continue
	set line [regsub -all {\(} $line { }]
	set line [regsub -all {\)} $line { }]
	puts "line: $line"
	set quotesplit [split $line {\"}]
	set pline [split $line]
	if {[string tolower [lindex $pline 0]] != "set"} {
	    puts "WARNING: ignoring line ($line)"
	    continue
	}
        if {[lindex $pline 1] == "Require-Plugin"} {
	    set commaseperated [regsub -all { } [lrange $pline 2 end-1] {,}]
	    puts $outfp "Require-Plugin: $commaseperated"
	} else {
	    puts $outfp "[lindex $pline 1]: [lindex $quotesplit 1]"
	}
    }
    close $outfp
    close $infp
}

create_manifest_qrc_file [lindex $argv 0] [lindex $argv 1]
parse_manifest_headers_cmake_file [lindex $argv 0] [lindex $argv 2] [lindex $argv 3]

