# Visualization Toolkit (VTK) Tcl package configuration.
#
#  assumes exact same build options in /SV16
#  assumes vtk 6.2

set not_loaded {}
set loaded {}

set all_vtk_kits {CommonCore CommonMath CommonMisc CommonSystem CommonTransforms CommonDataModel CommonColor CommonExecutionModel FiltersCore CommonComputationalGeometry FiltersGeneral ImagingCore ImagingFourier FiltersStatistics FiltersExtraction InfovisCore FiltersGeometry FiltersSources RenderingCore RenderingFreeType RenderingContextIID ChartsCore IOCore IOGeometry IOXMLParser IOXML DomainsChemistry IOLegacy ParallelCore FiltersAMR FiltersFlowPaths FiltersGeneric ImagingSources FiltersHybrid FiltersHyperTree ImagingGeneral FiltersImaging FiltersModeling FiltersParallel FiltersParallelImaging FiltersProgrammable FiltersSMP FiltersSelection FiltersTexture FiltersVerdict IOImage ImagingHybrid InfovisLayout InteractionStyle ImagingColor RenderingAnnotation RenderingVolume InteractionWidgets ViewsCore GeovisCore IOAMR IOEnSight IOExodus RenderingOpenGL RenderingContextOpenGL RenderingGLtoPS RenderingLabel IOExport IOImport IOInfovis IOLSDyna IOMINC IOMovie IONetCDF IOPLY IOParallel IOParallelXML IOSQL IOVideo ImagingMath ImagingMorphological ImagingStatistics ImagingStencil InteractionImage RenderingFreeTypeOpenGL RenderingImage RenderingLIC RenderingLOD RenderingTk RenderingVolumeOpenGL ViewsContextIID ViewsInfovis}

if {($SV_RELEASE_BUILD != 0) && ($tcl_platform(platform) == "windows")} {
    foreach kit $all_vtk_kits {
      set myfn [file join $simvascular_home vtk${kit}TCL-6.2.dll]
      if [catch {load $myfn vtk${kit}TCL} msg] {
        lappend not_loaded vtk${kit}TCL
      } else {
        lappend loaded vtk${kit}TCL
      }
    }
    puts [format "  %-12s %s" "TclVtk:" "Dynamic Libs (not_loaded: [llength $not_loaded])"]
} elseif {($SV_RELEASE_BUILD == 0) && ($tcl_platform(platform) == "windows")} {
    foreach kit $all_vtk_kits {
      #set myfn [file join $simvascular_home vtk${kit}TCL-6.2.dll]
      if [catch {load vtk${kit}TCL-6.2.dll vtk${kit}TCL} msg] {
        lappend not_loaded vtk${kit}TCL
      } else {
        lappend loaded vtk${kit}TCL
      }
    }
    puts [format "  %-12s %s" "TclVtk:" "Dynamic Libs (not_loaded: [llength $not_loaded])"]
} else {
  set auto_path "$auto_path $env(TCLLIBPATH)"
  #puts "auto_path: $auto_path"
  foreach kit $all_vtk_kits {
    if [catch {load {} vtk${kit}TCL} msg] {
      lappend not_loaded vtk${kit}TCL
    } else {
      lappend loaded vtk${kit}TCL
    }
  }
  if {[llength $not_loaded] > 0 && [llength $loaded] > 0} {
    puts "Error loading the following static vtk packages: $not_loaded"
  } elseif {[llength $not_loaded] > 0 && [llength $loaded] == 0} {
    puts [format "  %-12s %s" "TclVtk:" Shared Libs]
    set auto_path "$auto_path $env(TCLLIBPATH)"
    package require vtk
  } else {
    puts [format "  %-12s %s" "TclVtk:" Static Libs]
  }
}
