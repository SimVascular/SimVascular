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

set cv_classes_names {}

set ofn [open cv_classes_to_update_reversed.txt r]
while {[gets $ofn line] >= 0} {
    set line [string trim $line]
    if {$line != ""} {
	lappend cv_classes_names $line
    }
}
close $ofn

puts "Make local copies of Plugins and Modules..."

exec mkdir -p tmp

exec rm -Rf Plugins
exec rm -Rf Modules
exec cp -Rf ../Plugins .
exec cp -Rf ../Modules .

# replace header filenames and #defines
exec mkdir -p tmp
exec rm -f tmp/sed-change-cv.txt
set ofn [open tmp/sed-change-cv.txt w]

for {set i 0} {$i < [llength $cv_classes_names]} {incr i} {
    puts $ofn "s+[lindex $cv_classes_names $i]+sv2_[string range [lindex $cv_classes_names $i] 2 end]+g"  
}

close $ofn

exec rm -Rf sv4gui
exec mkdir sv4gui

foreach fn [file_find {Modules Plugins} *.h] {
    set newfn [file tail $fn]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-change-cv.txt $fn > sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u sv4gui/[file dirname $fn]/$newfn}
}

foreach fn [file_find {Modules Plugins} *.cxx] {
    set newfn [file tail $fn]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-change-cv.txt $fn > sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u sv4gui/[file dirname $fn]/$newfn}
}

foreach fn [file_find {Modules Plugins} *] {

    if [file isdirectory $fn] continue 
    if {[file extension $fn] == ".h"} continue
#    if {[file extension $fn] == ".ui"} continue
    if {[file extension $fn] == ".cxx"} continue
#    if {[file tail $fn] == "Makefile"} continue
#    if {[file tail $fn] == "files.cmake"} continue
    
    set newfn [file tail $fn]
    puts "copying fn: $fn  ($newfn)"
    exec mkdir -p sv4gui/[file dirname $fn]
    exec cp $fn sv4gui/[file dirname $fn]/$newfn

}
