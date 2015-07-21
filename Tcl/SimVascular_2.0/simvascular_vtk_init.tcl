# Visualization Toolkit (VTK) Tcl package configuration.
#
#  assumes exact same build options in /SV15
#  assumes static libraries have been built (6.2)
#

foreach kit { CommonCore CommonMath CommonMisc CommonSystem CommonTransforms CommonDataModel CommonColor CommonExecutionModel FiltersCore CommonComputationalGeometry FiltersGeneral ImagingCore ImagingFourier FiltersStatistics FiltersExtraction InfovisCore FiltersGeometry FiltersSources RenderingCore RenderingFreeType RenderingContextIID ChartsCore IOCore IOGeometry IOXMLParser IOXML DomainsChemistry IOLegacy ParallelCore FiltersAMR FiltersFlowPaths FiltersGeneric ImagingSources FiltersHybrid FiltersHyperTree ImagingGeneral FiltersImaging FiltersModeling FiltersParallel FiltersParallelImaging FiltersProgrammable FiltersSMP FiltersSelection FiltersTexture FiltersVerdict IOImage ImagingHybrid InfovisLayout InteractionStyle ImagingColor RenderingAnnotation RenderingVolume InteractionWidgets ViewsCore GeovisCore IOAMR IOEnSight IOExodus RenderingOpenGL RenderingContextOpenGL RenderingGLtoPS RenderingLabel IOExport IOImport IOInfovis IOLSDyna IOMINC IOMovie IONetCDF IOPLY IOParallel IOParallelXML IOSQL IOVideo ImagingMath ImagingMorphological ImagingStatistics ImagingStencil InteractionImage RenderingFreeTypeOpenGL RenderingImage RenderingLIC RenderingLOD RenderingTk RenderingVolumeOpenGL ViewsContextIID ViewsInfovis } {
    if [catch {load {} vtk${kit}TCL} msg] {
	puts "Error loading ($kit) $msg"
    }
}




