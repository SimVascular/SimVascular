#/bin/tclsh
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

