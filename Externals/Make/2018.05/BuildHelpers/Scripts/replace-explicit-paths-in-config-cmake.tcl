proc file_find_multiple {dir wildcards args} {
    
  if {[llength $args] == 0} {
     set rtnme {}
  } else {
     upvar rtnme rtnme
  }

  foreach j $dir {
    foreach wildcard $wildcards {
      set files [glob -nocomplain [file join $j $wildcard]]
      foreach i $files {
        #puts "found file: $i"
        lappend rtnme $i
      }
    }
    set files [glob -nocomplain [file join $j *]]
    foreach i $files {
      if {[file isdirectory $i] == 1} {
        file_find_multiple $i $wildcards 1
      }
    }
  }
  return [lsort -unique $rtnme]

}

set replaceme C:/cygwin64/usr/local/sv/ext/2018.05/release/bin/msvc/19.0/x64/release

set all_files {}

#set all_files [file_find_multiple bin [list *Config*cmake *Targets*cmake *config*cmake]]
set all_files [file_find_multiple bin [list *.cmake]]

puts "$all_files"

foreach i $all_files {
    set count {}
    catch {set count [exec grep -c $replaceme $i]}
    if {$count != ""} {
	puts "working on ($i)  (count: $count)"
	if [catch {exec sed s+$replaceme+\${SV_EXTERNALS_TOPLEVEL_BIN_DIR}+g $i > $i.new} msg] {
	    puts "error on ($i): $msg"
	} else {
	    catch {exec mv $i $i.org}
	    catch {exec mv $i.new $i}
	}
    }
}

