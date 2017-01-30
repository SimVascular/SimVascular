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

proc file_find_no_recursive {dirs wildcard args} {
  #author Nathan Wilson
  #@c This procedure recursively searches for
  #@c pattern and returns a tcl list of the files
  #@c matching pattern.
  #@a dirs:  search directories
  #@a wildcard: pattern to match
  #@a args:  optional return variable
  #@r status

  if {[llength $args] == 0} {
     set rtnme {}
  } else {
     upvar rtnme rtnme
  }

  foreach j $dirs {
    set files [glob -nocomplain [file join $j $wildcard]]
    # print out headers
    foreach i $files {
      #puts "found file: $i"
      lappend rtnme $i
    }

    set files [glob -nocomplain [file join $j *]]
    foreach i $files {
      if {[file isdirectory $i] == 1} {
        #file_find $i $wildcard 1
      }
    }
  }
  return [lsort -unique $rtnme]

}

proc parse_dumpbin_file_to_get_dependents {fn} {
    set fp [open $fn r]
    if {$fp == ""} {
	return -code error "ERROR: could not open file ($fn)."
    }
    set foundDeps 0
    while {[gets $fp line] >= 0} {
	set line [string trim $line]
	if {[lindex $line 0] == "Image"} {
	    gets $fp line
	    set foundDeps 1
	    break
	}
    }
    if {$foundDeps == 0} {
	close $fp
	return
	#return -code error "ERROR: no \"Image has the following dependencies:\" line found!"
    }
    set libs {}
    while {[gets $fp line] >= 0} {
	set line [string trim $line]
	if {$line == ""} break
	lappend libs [string trim $line]
    }
    close $fp
    return $libs
}

proc find_libs {libs} {
  global search_dirs
  global ignore_system_libs
  global used_libs
  global all_possible_dlls
  foreach i $libs {
    if {[lsearch [array names used_libs] $i] < 0} {
	puts "Adding dependency ($i)"
	if {[lsearch $ignore_system_libs $i] >= 0} {
	    set used_libs($i) IGNORED
	} else {
	    if {[array names all_possible_dlls $i] != ""} {
		set used_libs($i) $all_possible_dlls($i)
		find_dependents $used_libs($i)
	    } else {
		set used_libs($i) MISSING
	    }
	}
	#puts "$i: $used_libs($i)"
    }
  }
}

proc find_dependents {in_files} {
    foreach fn $in_files {
      puts "working on ($fn)"
      if {![file exists $fn]} {
	continue
      }
      file mkdir tmp
      set outfn tmp/[file tail $fn].txt
      if [file exists $outfn] {
	continue
      }
      set win_fn [exec cygpath -m $fn]
      #puts "win_fn: $win_fn"
      exec dumpbin /OUT:$outfn /DEPENDENTS $win_fn
      set libs [parse_dumpbin_file_to_get_dependents $outfn]
	find_libs $libs
    }
}

catch {unset used_libs}
set used_libs(dummy) {}

exec rm -Rf tmp

set search_dirs [split $env(PATH) ":"]

#set search_dirs [list /usr/local/sv/ext/bin/msvc-12.5/x64 \
		      /cygdrive/c/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl \
		      Bin/plugins \
		      [glob Lib/*/*] \
                      ]

set search_dirs [list /usr/local/sv/ext/bin/msvc-12.5/x64 \
		      /cygdrive/c/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl \
                      ]

set ignore_system_libs {SHELL32.dll MSVCP120.dll MSVCR120.dll KERNEL32.dll win32u.dll}

global all_possible_dlls
set all_dlls [file_find $search_dirs *.dll]
foreach i $all_dlls {
    set all_possible_dlls([file tail $i]) $i
}

if {1 == 1} {
find_dependents Bin/simvascular-msvc-12.5-ifort.exe
find_dependents [glob Bin/plugins/*.dll]
find_dependents [glob Lib/*/*/*.dll]
find_dependents [glob /usr/local/sv/ext/bin/msvc-12.5/x64/mitk-nate/bin/plugins/RelWithDebInfo/*.dll]
}

if {0 == 1} {
find_dependents /usr/local/sv/ext/bin/msvc-12.5/x64/mitk-nate/bin/RelWithDebInfo/MitkWorkbench.exe
find_dependents [glob /usr/local/sv/ext/bin/msvc-12.5/x64/mitk-nate/bin/plugins/RelWithDebInfo/*.dll]
#find_dependents [glob /home/nwilson/gitwork/simvascular/releases/2017-01-19-release/BuildWithMake/Lib/x64_cygwin/msvc-12.5-ifort/*.dll]
}

unset used_libs(dummy)

puts "\n\n*** All Dependencies ***\n\n"

foreach i [lsort -dictionary [array names used_libs]] {
  puts "$i: $used_libs($i)"
}

puts "\n\n***  Only MISSING Libs ***\n\n"

foreach i [lsort -dictionary [array names used_libs]] {
  if {$used_libs($i) == "MISSING"} {
      puts "$i: $used_libs($i)"
  }
}

set all_dirs ""
foreach i [array names used_libs] {
    lappend all_dirs [file dirname $used_libs($i)]
}

puts "\n\n***  All Required Paths ***\n\n"

set all_dirs [lsort -unique -dictionary $all_dirs]
foreach i $all_dirs {
    puts "$i"
}

puts "\n\n***  Windows Style Paths  ***\n\n"
set all_dirs [lsort -unique -dictionary $all_dirs]
foreach i $all_dirs {
    puts "set PATH=[exec cygpath -m $i];%PATH%"
}
