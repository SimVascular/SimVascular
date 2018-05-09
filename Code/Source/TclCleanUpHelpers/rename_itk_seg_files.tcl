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

set things_to_rename [list \
{VascularLevelSetObserver\.h cv_VascularLevelSetObserver\.h} \
{VascularLevelSetImageFilter\.h cv_VascularLevelSetImageFilter\.h} \
{VascularLevelSetFunction\.hxx cv_VascularLevelSetFunction\.hxx} \
{VascularLevelSetFunction\.h cv_VascularLevelSetFunction\.h} \
{MyUtils\.h cv_ITKLset_ExtraUtils\.h} \
{MyUtils\.cxx cv_ITKLset_ExtraUtils\.cxx} \
{itkVascularPhaseTwoLevelSetImageFilter\.hxx cv_VascularPhaseTwoLevelSetImageFilter\.hxx} \
{itkVascularPhaseTwoLevelSetImageFilter\.h cv_VascularPhaseTwoLevelSetImageFilter\.h} \
{itkVascularPhaseTwoLevelSetFunction\.hxx cv_VascularPhaseTwoLevelSetFunction\.hxx} \
{itkVascularPhaseTwoLevelSetFunction\.h cv_VascularPhaseTwoLevelSetFunction\.h} \
{itkVascularPhaseOneLevelSetImageFilter\.hxx cv_VascularPhaseOneLevelSetImageFilter\.hxx} \
{itkVascularPhaseOneLevelSetImageFilter\.h cv_VascularPhaseOneLevelSetImageFilter\.h} \
{itkVascularPhaseOneLevelSetFunction\.hxx cv_VascularPhaseOneLevelSetFunction\.hxx} \
{itkVascularPhaseOneLevelSetFunction\.h cv_VascularPhaseOneLevelSetFunction\.h} \
{ImgInfo\.h cv_ITKLset_ImgInfo\.h} \
{GeodesicActiveContourLaplacianSmoothLevelSetImageFilter\.h cv_GeodesicActiveContourLaplacianSmoothLevelSetImageFilter\.h} \
{EdgeRemapImageFilter\.h cv_ITKLset_EdgeRemapImageFilter\.h} \
{cvVTKMacros\.h cv_ITKLset_VTK_Macros\.h} \
{cvTCLMacros\.h cv_ITKLset_TCL_Macros\.h} \
{cvPYTHONMacros\.h cv_ITKLSet_PYTHON_Macros\.h} \
{cvMacros\.h cv_ITKLset_Macros\.h} \
{cvITKUtils\.hxx cv_ITKLset_ITKUtils\.hxx} \
{cvITKUtils\.h cv_ITKLset_ITKUtils\.h} \
{cvITKMacros\.h cv_ITKLset_ITK_Macros\.h} \
{ConnectVTKITK\.h cv_ITKLset_ConnectVTKITK\.h} \
		  ]

set sedscript [list \
s+VascularLevelSetObserver\.h+cv_VascularLevelSetObserver\.h+g \
s+VascularLevelSetImageFilter\.h+cv_VascularLevelSetImageFilter\.h+g \
s+VascularLevelSetFunction\.hxx+cv_VascularLevelSetFunction\.hxx+g \
s+VascularLevelSetFunction\.h+cv_VascularLevelSetFunction\.h+g \
s+MyUtils\.h+cv_ITKLset_ExtraUtils\.h+g \
s+MyUtils\.cxx+cv_ITKLset_ExtraUtils\.cxx+g \
s+itkVascularPhaseTwoLevelSetImageFilter\.hxx+cv_VascularPhaseTwoLevelSetImageFilter\.hxx+g \
s+itkVascularPhaseTwoLevelSetImageFilter\.h+cv_VascularPhaseTwoLevelSetImageFilter\.h+g \
s+itkVascularPhaseTwoLevelSetFunction\.hxx+cv_VascularPhaseTwoLevelSetFunction\.hxx+g \
s+itkVascularPhaseTwoLevelSetFunction\.h+cv_VascularPhaseTwoLevelSetFunction\.h+g \
s+itkVascularPhaseOneLevelSetImageFilter\.hxx+cv_VascularPhaseOneLevelSetImageFilter\.hxx+g \
s+itkVascularPhaseOneLevelSetImageFilter\.h+cv_VascularPhaseOneLevelSetImageFilter\.h+g \
s+itkVascularPhaseOneLevelSetFunction\.hxx+cv_VascularPhaseOneLevelSetFunction\.hxx+g \
s+itkVascularPhaseOneLevelSetFunction\.h+cv_VascularPhaseOneLevelSetFunction\.h+g \
s+ImgInfo\.h+cv_ITKLset_ImgInfo\.h+g \
s+GeodesicActiveContourLaplacianSmoothLevelSetImageFilter\.h+cv_GeodesicActiveContourLaplacianSmoothLevelSetImageFilter\.h+g \
s+EdgeRemapImageFilter\.h+cv_ITKLset_EdgeRemapImageFilter\.h+g \
s+cvVTKMacros\.h+cv_ITKLset_VTK_Macros\.h+g \
s+cvTCLMacros\.h+cv_ITKLset_TCL_Macros\.h+g \
s+cvPYTHONMacros\.h+cv_ITKLSet_PYTHON_Macros\.h+g \
s+cvMacros\.h+cv_ITKLset_Macros\.h+g \
s+cvITKUtils\.hxx+cv_ITKLset_ITKUtils\.hxx+g \
s+cvITKUtils\.h+cv_ITKLset_ITKUtils\.h+g \
s+cvITKMacros\.h+cv_ITKLset_ITK_Macros\.h+g \
s+ConnectVTKITK\.h+cv_ITKLset_ConnectVTKITK\.h+g \
		  ]

exec rm -Rf Segmentation-Before-Sed
exec cp -Rf ../sv3/Segmentation Segmentation-Before-Sed

foreach thing $things_to_rename {
    exec mv Segmentation-Before-Sed/[lindex $thing 0] Segmentation-Before-Sed/[lindex $thing 1]
}

exec mkdir -p tmp
exec rm -f tmp/sed-rename-seg-files.txt

set ofn [open tmp/sed-rename-seg-files.txt w]
foreach thing $things_to_rename {
    set orgfn [lindex $thing 0]
    set orgfn [file rootname $orgfn]\\[file extension $orgfn]
    set newfn [lindex $thing 1]
    set newfn [file rootname $newfn]\\[file extension $newfn]
    puts $ofn "s+$orgfn+$newfn+g"
}
close $ofn

exec rm -f tmp/git-changes-to-commit.txt

set gitfn [open tmp/git-changes-to-commit.txt w]

foreach thing $things_to_rename {
    puts $gitfn "git rm [lindex $thing 0]"
    puts $gitfn "git add [lindex $thing 1]"
}
close $gitfn

exec rm -Rf Segmentation
exec mkdir Segmentation

foreach fn [file_find {Segmentation-Before-Sed} *] {

    set newfn [file tail $fn]
    puts "working on fn: $fn  ($newfn)"
    exec sed -f tmp/sed-rename-seg-files.txt $fn > Segmentation/$newfn
    catch {exec d2u Segmentation/$newfn}
}

exit

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

