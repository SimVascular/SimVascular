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

set module_exports_h [list \
 svCommonExports.h \
 svMeshExports.h \
 svMeshSimExports.h \
 svModelExports.h \
 svModelOCCTExports.h \
 svModelParasolidExports.h \
 svPathExports.h \
 svProjectManagementExports.h \
 svQtWidgetsExports.h \
 svSegmentationExports.h \
 svSimulationExports.h \
]

set module_names [list \
 svCommon \
 svMesh \
 svMeshSim \
 svModel \
 svModelOCCT \
 svModelParasolid \
 svPath \
 svProjectManagement \
 svQtWidgets \
 svSegmentation \
 svSimulation \
		      ]

# replace exports
puts "Make local copies of Plugins and Modules..."

if {![file exists Plugins]} {
    exec mv ../sv4gui/Plugins Plugins
}
if {![file exists Modules]} {
    exec mv ../sv4gui/Modules Modules
}

exec rm -Rf ../sv4gui/Plugins
exec rm -Rf ../sv4gui/Modules

exec mkdir -p tmp

set directories_to_process {Modules Plugins}

exec rm -f tmp/sed-replace-exports-h.txt
set ofn [open tmp/sed-replace-exports-h.txt w]

foreach me $module_exports_h {
    puts $ofn "s+[file rootname $me]\\.h+sv4guiModule[string range [file rootname $me] 2 end]\\.h+g"
}
foreach md $module_names {
    puts $ofn "s+$md+sv4guiModule[string range $md 2 end]+g"
    puts $ofn "s+[string toupper $md]_EXPORT+[string toupper sv4guiModule[string range $md 2 end]]_EXPORT+g"
}

close $ofn

exec rm -f tmp/sed-replace-module-names.txt
set ofn [open tmp/sed-replace-module-names.txt w]

puts $ofn "s+VTKSVCOMMON+PROTECT_VTK_SV_COMMON+g"

foreach md $module_names {
    puts $ofn "s+$md+sv4guiModule[string range $md 2 end]+g"
    puts $ofn "s+[string toupper $md]+[string toupper sv4guiModule[string range $md 2 end]]+g"
}

puts $ofn "s+PROTECT_VTK_SV_COMMON+VTKSVCOMMON+g"

close $ofn


foreach fn [file_find $directories_to_process *.h] {

    set newfn [file tail $fn]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p ../sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-replace-exports-h.txt $fn > ../sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u ../sv4gui/[file dirname $fn]/$newfn}

}

foreach fn [file_find $directories_to_process *.cxx] {

    set newfn [file tail $fn]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p ../sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-replace-exports-h.txt $fn > ../sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u ../sv4gui/[file dirname $fn]/$newfn}

}

foreach fn [file_find $directories_to_process Makefile] {

    set newfn [file tail $fn]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p ../sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-replace-module-names.txt $fn > ../sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u ../sv4gui/[file dirname $fn]/$newfn}

}

foreach fn [file_find $directories_to_process files.cmake] {

    set newfn [file tail $fn]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p ../sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-replace-module-names.txt $fn > ../sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u ../sv4gui/[file dirname $fn]/$newfn}

}

foreach fn [file_find $directories_to_process CMakeLists.txt] {

    set newfn [file tail $fn]
    puts "working on fn: $fn  ($newfn)"
    exec mkdir -p ../sv4gui/[file dirname $fn]
    exec sed -f tmp/sed-replace-module-names.txt $fn > ../sv4gui/[file dirname $fn]/$newfn
    catch {exec d2u ../sv4gui/[file dirname $fn]/$newfn}

}

foreach fn [file_find $directories_to_process *] {

    if [file isdirectory $fn] continue 
    if {[file extension $fn] == ".h"} continue
    if {[file extension $fn] == ".cxx"} continue
    if {[file tail $fn] == "Makefile"} continue
    if {[file tail $fn] == "files.cmake"} continue
    if {[file tail $fn] == "CMakeLists.txt"} continue
   
    set newfn [file tail $fn]
    puts "copying fn: $fn  ($newfn)"
    exec mkdir -p ../sv4gui/[file dirname $fn]
    exec cp $fn ../sv4gui/[file dirname $fn]/$newfn

}


