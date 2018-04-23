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

puts "Make local copies of Plugins and Modules..."

exec rm -Rf Plugins
exec rm -Rf Modules
exec cp -Rf ../Plugins .
exec cp -Rf ../Modules .

puts "Check for existence of headers and #defines..."

set all_header_filenames {}
set all_cxx_filenames {}
set all_pound_defines {}

set ofn [open all-sv-classes-to-replace.txt r]
while {[gets $ofn line] >= 0} {
    set line [string trim $line]
    if {$line != ""} {
	lappend all_header_filenames $line.h
	lappend all_cxx_filenames $line.cxx
	lappend all_pound_defines [string toupper $line]_H
    }
}
close $ofn

####set emacs_edit {}

for {set i 0} {$i < [llength $all_header_filenames]} {incr i} {
    
    set find_me_hdr_fn  [lindex $all_header_filenames $i]
    set find_me_cxx_fn  [lindex $all_cxx_filenames $i]
    set find_me_defines [lindex $all_pound_defines $i]

    # check headers
    
    set found_file_hdr_fn [string trim [file_find {Modules Plugins} $find_me_hdr_fn]]
    if {$found_file_hdr_fn == ""} {
	puts "error finding file: $find_me_hdr_fn"
    } else {
        #puts "found file: $found_file_hdr_fn"
	set rstr ""
	if [catch {set rstr [exec grep $find_me_defines $found_file_hdr_fn]}] {
	    puts "error finding $find_me_defines in $found_file_hdr_fn"
	}
    }

    #check cxx files
    
    set found_file_cxx_fn [string trim [file_find {Modules Plugins} $find_me_cxx_fn]]
    if {$found_file_cxx_fn == ""} {
	puts "error finding file: $find_me_cxx_fn  (hdr: $found_file_hdr_fn)"
	#
	# this commented out code was to create missing .cxx files and requires
	# significant hand editing of the files afterwards!
	#
	#exec cp $found_file_hdr_fn ../[file dirname $found_file_hdr_fn]/$find_me_cxx_fn
	#exec echo \"#include \"$find_me_hdr_fn\" >> ../[file dirname $found_file_hdr_fn]/$find_me_cxx_fn
	#exec git add ../[file dirname $found_file_hdr_fn]/$find_me_cxx_fn
	#lappend emacs_edit ../[file dirname $found_file_hdr_fn]/$find_me_cxx_fn
    } else {
        #puts "found file: $found_file_cxx_fn"
	set rstr ""
	if [catch {set rstr [exec grep $find_me_hdr_fn $found_file_cxx_fn]}] {
	    puts "error finding $find_me_hdr_fn in $found_file_cxx_fn"
	}
    
    }

}

###eval exec emacs $emacs_edit

if {0 == 1} {
puts "delete files to check what I'm not checking..."

for {set i 0} {$i < [llength $all_header_filenames]} {incr i} {
    
    set find_me_hdr_fn  [lindex $all_header_filenames $i]
    set find_me_cxx_fn  [lindex $all_cxx_filenames $i]
    set find_me_defines [lindex $all_pound_defines $i]

    # check headers
    
    set found_file_hdr_fn [string trim [file_find {Modules Plugins} $find_me_hdr_fn]]
    if {$found_file_hdr_fn == ""} {
	puts "error finding file: $find_me_hdr_fn"
    } else {
	#puts "found file: $found_file_hdr_fn"
	exec rm -f $found_file_hdr_fn
    }

    #check cxx files
    
    set found_file_cxx_fn [string trim [file_find {Modules Plugins} $find_me_cxx_fn]]
    if {$found_file_cxx_fn == ""} {
	puts "error finding file: $find_me_cxx_fn  (hdr: $found_file_hdr_fn)"
    } else {
	#puts "found file: $found_file_cxx_fn"
        exec rm -f $found_file_cxx_fn
    }

}
}

# replace header filenames and #defines
exec mkdir -p tmp
exec rm -f tmp/sed-rename-files.txt
set ofn [open tmp/sed-rename-files.txt w]

foreach fn [file_find {Plugins} *.ui] {
    set myroot [string range [file rootname [file tail $fn]] 2 end]
    puts $ofn "s+ui_sv$myroot\\.h+ui_sv4gui_$myroot\\.h+g"
    puts $ofn "s+sv$myroot\\.ui+sv4gui_$myroot\\.ui+g"
}

for {set i 0} {$i < [llength $all_header_filenames]} {incr i} {
    
    set find_me_hdr_fn  [file rootname [lindex $all_header_filenames $i]]\\.h
    set find_me_cxx_fn  [file rootname [lindex $all_cxx_filenames $i]]\\.cxx
    set find_me_defines [lindex $all_pound_defines $i]

    puts $ofn "s+$find_me_hdr_fn+sv4gui_[string range $find_me_hdr_fn 2 end]+g"
    puts $ofn "s+$find_me_cxx_fn+sv4gui_[string range $find_me_cxx_fn 2 end]+g"
    puts $ofn "s+$find_me_defines+SV4GUI\_[string range $find_me_defines 2 end]+g"
    
}

# have to manually handle these three files since they don't really contain sv4gui
# classes!
puts $ofn "s+svberrySingleNodeSelection\.cxx+sv4gui_berrySingleNodeSelection\.cxx+g"
puts $ofn "s+svberrySingleNodeSelection\.h+sv4gui_berrySingleNodeSelection\.h+g"
puts $ofn "s+svmitkIContextMenuAction\.h+sv4gui_mitkIContextMenuAction\.h+g"

close $ofn

exec rm -f tmp/git-changes-to-commit.txt

set gitfn [open tmp/git-changes-to-commit.txt w]

exec rm -Rf sv4gui
exec mkdir sv4gui

foreach fn [file_find {Modules Plugins} *.h] {

    set newfn sv4gui_[string range [file tail $fn] 2 end]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-rename-files.txt $fn > sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u sv4gui/[file dirname $fn]/$newfn}
    puts $gitfn "git rm $fn"
    puts $gitfn "git add [file dirname $fn]/$newfn"

}

foreach fn [file_find {Modules Plugins} *.cxx] {

    set newfn sv4gui_[string range [file tail $fn] 2 end]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-rename-files.txt $fn > sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u sv4gui/[file dirname $fn]/$newfn}
    puts $gitfn "git rm $fn"
    puts $gitfn "git add [file dirname $fn]/$newfn"

}

foreach fn [file_find {Modules Plugins} Makefile] {

    set newfn [file tail $fn]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-rename-files.txt $fn > sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u sv4gui/[file dirname $fn]/$newfn}

}

foreach fn [file_find {Modules Plugins} files.cmake] {

    set newfn [file tail $fn]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-rename-files.txt $fn > sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u sv4gui/[file dirname $fn]/$newfn}

}

foreach fn [file_find {Plugins} *.ui] {
    set myroot [string range [file rootname [file tail $fn]] 2 end]
    set newfn sv4gui_$myroot.ui
    puts "working on ($fn) to ($newfn)"
    exec mkdir -p sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-rename-files.txt $fn > sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u sv4gui/[file dirname $fn]/$newfn}
    puts $gitfn "git rm $fn"
    puts $gitfn "git add [file dirname $fn]/$newfn"
}

foreach fn [file_find {Modules Plugins} *] {

    if [file isdirectory $fn] continue 
    if {[file extension $fn] == ".h"} continue
    if {[file extension $fn] == ".ui"} continue
    if {[file extension $fn] == ".cxx"} continue
    if {[file tail $fn] == "Makefile"} continue
    if {[file tail $fn] == "files.cmake"} continue
    
    set newfn [file tail $fn]
    puts "copying fn: $fn  ($newfn)"
    exec mkdir -p sv4gui/[file dirname $fn]
    exec cp $fn sv4gui/[file dirname $fn]/$newfn

}

close $gitfn

