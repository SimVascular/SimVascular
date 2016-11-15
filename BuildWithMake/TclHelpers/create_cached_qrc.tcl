#/bin/tclsh
#proc create_cached_qrc_file args {
    set symbolicName [lindex $argv 0]
    set symbolicNamePeriod [regsub -all _ $symbolicName .]
    set outFile [lindex $argv 1]
    set file_args [lrange $argv 2 end]
    
    if [catch {set outfp [open $outFile w]}] {
	return -code error "ERROR: filename does not exist ($inFile)!!"
    }

    puts $outfp "<!DOCTYPE RCC><RCC version=\"1.0\">"
    puts $outfp "<qresource prefix=\"/$symbolicNamePeriod\">"
    foreach i $file_args {
	puts $outfp "<file>$i</file>"
    }
    puts $outfp "</qresource>"
    puts $outfp "</RCC>"
    
    close $outfp
#}

#create_cached_qrc_file $argv

