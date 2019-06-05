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

#set replaceme C:/cygwin64/usr/local/sv/ext/2019.06/release/gl2/bin/msvc/19.0/x64/release
set replaceme C:/OpenSource

set all_files {}

#set all_files [file_find_multiple bin [list *Config*cmake *Targets*cmake *config*cmake]]
set all_files [file_find_multiple bin [list *.cmake]]

puts "$all_files"

puts "C:/OpenSource"
foreach i $all_files {
    set count {}
    catch {set count [exec grep -c C:/OpenSource $i]}
    if {$count != ""} {
	puts "found in ($i)  (count: $count)"
    }
}

puts "C:/cygwin64"

foreach i $all_files {
    set count {}
    catch {set count [exec grep -c C:/cygwin64 $i]}
    if {$count != ""} {
	puts "found in ($i)  (count: $count)"
    }
}

