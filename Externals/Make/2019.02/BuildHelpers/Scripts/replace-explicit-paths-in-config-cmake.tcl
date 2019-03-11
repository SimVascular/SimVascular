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

set toplevel_bin_dir REPLACEME_SV_FULLPATH_BINDIR

set olddir [pwd]
puts "$olddir"
if {$tcl_platform(os) == "CYGWIN_NT-10.0"} {
    set posixpath [exec cygpath -u $toplevel_bin_dir]
    cd $posixpath
} else {
    cd $toplevel_bin_dir
}

set all_files {}

#set all_files [file_find_multiple * [list *Config*cmake *Targets*cmake *config*cmake]]
set all_files [file_find_multiple * [list *.cmake]]

puts "number of cmake config files to check for explicit paths: [llength $all_files]"

foreach i $all_files {
    set count {}
    catch {set count [exec grep -c $toplevel_bin_dir $i]}
    if {$count != ""} {
	puts "working on ($i)  (count: $count)"
	if [catch {exec sed s+$toplevel_bin_dir+\${SV_EXTERNALS_TOPLEVEL_BIN_DIR}+g $i > $i.new} msg] {
	    puts "error on ($i): $msg"
	} else {
	    catch {exec mv $i $i.org}
	    catch {exec mv $i.new $i}
	}
    }
}

cd $olddir

