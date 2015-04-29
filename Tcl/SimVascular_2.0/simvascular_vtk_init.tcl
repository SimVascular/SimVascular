# Visualization Toolkit (VTK) Tcl package configuration.
#
#  assumes exact same build options in /sv_extern
#  assumes static libraries have been built
#

package ifneeded vtkinit {6.0} {
  namespace eval ::vtk::init {
    proc load_library_package {libName libPath {libPrefix {}}} {
      #first try to load a static package, then try the shared package.
      if {[catch "load {} $libName"]} {
        set libExt [info sharedlibextension]
        set currentDirectory [pwd]
        set libFile [file join $libPath "$libPrefix$libName-6.0$libExt"]
        if {[catch "cd {$libPath}; load {$libFile}" errorMessage]} {
          puts $errorMessage
        }
        cd $currentDirectory
      }
    }
    proc require_package {name {version {6.0}}} {
      if {[catch "package require -exact $name $version" errorMessage]} {
        puts $errorMessage
        return 0
      } else {
        return 1
      }
    }
    set version {6.0}
    set kits {}
    foreach kit { base  CommonCore CommonMath CommonMisc CommonSystem CommonTransforms CommonDataModel CommonColor CommonExecutionModel FiltersCore CommonComputationalGeometry FiltersGeneral ImagingCore ImagingFourier FiltersStatistics FiltersExtraction InfovisCore FiltersGeometry FiltersSources IOCore IOImage IOXMLParser RenderingCore RenderingFreeType ImagingHybrid RenderingOpenGL RenderingContextIID ChartsCore IOGeometry IOXML DomainsChemistry IOLegacy ParallelCore FiltersAMR FiltersFlowPaths FiltersGeneric ImagingSources FiltersHybrid FiltersHyperTree ImagingGeneral FiltersImaging FiltersModeling FiltersParallel FiltersParallelImaging FiltersProgrammable FiltersSelection FiltersTexture FiltersVerdict InfovisLayout InteractionStyle ImagingColor RenderingAnnotation RenderingVolume InteractionWidgets ViewsCore GeovisCore IOAMR IOEnSight IOExodus RenderingGLtoPS IOExport IOImport IOInfovis IOLSDyna IOMINC IOMovie IONetCDF IOPLY IOParallel IOSQL IOVideo ImagingMath ImagingMorphological ImagingStatistics ImagingStencil InteractionImage RenderingFreeTypeOpenGL RenderingHybridOpenGL RenderingImage RenderingLOD RenderingLabel RenderingTk RenderingVolumeAMR RenderingVolumeOpenGL ViewsContextIID ViewsInfovis ViewsGeovis } {
      lappend kits [string tolower "${kit}"]
    }
  }
  package provide vtkinit {6.0}
}

package require vtkinit

foreach kit {  CommonCore CommonMath CommonMisc CommonSystem CommonTransforms CommonDataModel CommonColor CommonExecutionModel FiltersCore CommonComputationalGeometry FiltersGeneral ImagingCore ImagingFourier FiltersStatistics FiltersExtraction InfovisCore FiltersGeometry FiltersSources IOCore IOImage IOXMLParser RenderingCore RenderingFreeType ImagingHybrid RenderingOpenGL RenderingContextIID ChartsCore IOGeometry IOXML DomainsChemistry IOLegacy ParallelCore FiltersAMR FiltersFlowPaths FiltersGeneric ImagingSources FiltersHybrid FiltersHyperTree ImagingGeneral FiltersImaging FiltersModeling FiltersParallel FiltersParallelImaging FiltersProgrammable FiltersSelection FiltersTexture FiltersVerdict InfovisLayout InteractionStyle ImagingColor RenderingAnnotation RenderingVolume InteractionWidgets ViewsCore GeovisCore IOAMR IOEnSight IOExodus RenderingGLtoPS IOExport IOImport IOInfovis IOLSDyna IOMINC IOMovie IONetCDF IOPLY IOParallel IOSQL IOVideo ImagingMath ImagingMorphological ImagingStatistics ImagingStencil InteractionImage RenderingFreeTypeOpenGL RenderingHybridOpenGL RenderingImage RenderingLOD RenderingLabel RenderingTk RenderingVolumeAMR RenderingVolumeOpenGL ViewsContextIID ViewsInfovis ViewsGeovis } {
 
    ::vtk::init::load_library_package "vtk${kit}TCL" {}

}
