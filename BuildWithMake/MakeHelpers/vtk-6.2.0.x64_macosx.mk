# ---------------------
# Visualization toolkit
# ---------------------

VTK_SRCDIR = $(OPEN_SOFTWARE_SOURCES_TOPLEVEL)/vtk-6.2.0
VTK_BINDIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/vtk-6.2.0
VTK_INCLUDE_DIR_BASE = $(VTK_BINDIR)/include/vtk-6.2
VTK_LIBDIRS = $(VTK_BINDIR)/lib
VTK_BINDIRS = $(VTK_BINDIR)/bin
VTK_SO_PATH = $(VTK_BINDIR)/lib
VTK_DLLS    = $(VTK_BINDIRS)/*.$(SOEXT)
VTK_TCL_LIB_PATH = $(VTK_LIBDIRS)/tcltk/vtk-6.2
#VTK_SYS_LIBS  = -lGLU -lX11 -lXt -lXext -lpthread -lm -lGL -lm -ldl
VTK_SYS_LIBS  = -lpthread -lm -lm -ldl

VTK_PYTHON_PACKAGES = $(VTK_BINDIR)/lib/python2.7/site-packages

VTK_INCDIRS = \
-I$(VTK_INCLUDE_DIR_BASE) \
-I$(VTK_INCLUDE_DIR_BASE)/vtkexpat \
-I$(VTK_INCLUDE_DIR_BASE)/vtkfreetype \
-I$(VTK_INCLUDE_DIR_BASE)/vtkgl2ps \
-I$(VTK_INCLUDE_DIR_BASE)/vtkjpeg \
-I$(VTK_INCLUDE_DIR_BASE)/vtkjsoncpp \
-I$(VTK_INCLUDE_DIR_BASE)/vtklibproj4 \
-I$(VTK_INCLUDE_DIR_BASE)/vtklibxml2 \
-I$(VTK_INCLUDE_DIR_BASE)/vtkmetaio \
-I$(VTK_INCLUDE_DIR_BASE)/vtknetcdf \
-I$(VTK_INCLUDE_DIR_BASE)/vtkpng \
-I$(VTK_INCLUDE_DIR_BASE)/vtksqlite \
-I$(VTK_INCLUDE_DIR_BASE)/vtktiff \
-I$(VTK_INCLUDE_DIR_BASE)/vtkverdict \
-I$(VTK_INCLUDE_DIR_BASE)/vtkzlib \
-I$(VTK_INCLUDE_DIR_BASE)/alglib \
-I$(VTK_INCLUDE_DIR_BASE)/TclTk

#
#  libraries for svSolver only
#

#VTK_LIBS =      $(LIBPATH_COMPILER_FLAG)$(VTK_LIBDIRS) \
                $(LIBFLAG)vtkIOXML-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOXMLParser-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonExecutionModel-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonDataModel-6.2$(LIBLINKEXT) \
	        $(LIBFLAG)vtkCommonSystem-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonTransforms-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonMisc-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonMath-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkzlib-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkexpat-6.2$(LIBLINKEXT) \
                $(LIBFLAG)vtksys-6.2$(LIBLINKEXT) \
                $(VTK_SYS_LIBS)

#
#  all vtk libs
#


VTK_LIBS =      $(LIBPATH_COMPILER_FLAG)$(VTK_LIBDIRS) \
		$(LIBFLAG)vtkChartsCoreTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonColorTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonComputationalGeometryTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonCoreTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonDataModelTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonExecutionModelTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonMathTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonMiscTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonSystemTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonTransformsTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkDomainsChemistryTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersAMRTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersCoreTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersExtractionTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersFlowPathsTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersGeneralTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersGenericTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersGeometryTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersHybridTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersHyperTreeTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersImagingTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersModelingTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersParallelImagingTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersParallelTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersProgrammableTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersSelectionTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersSourcesTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersStatisticsTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersTextureTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersVerdictTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOAMRTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOCoreTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOEnSightTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOExodusTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOExportTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOGeometryTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOImageTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOImportTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOInfovisTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOLSDynaTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOLegacyTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOMINCTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOMovieTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIONetCDFTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOPLYTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOParallelTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOSQLTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOVideoTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOXMLParserTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOXMLTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingColorTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingCoreTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingFourierTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingGeneralTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingHybridTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingMathTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingMorphologicalTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingSourcesTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingStatisticsTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingStencilTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkInfovisCoreTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkInfovisLayoutTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkInteractionImageTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkInteractionStyleTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkInteractionWidgetsTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkParallelCoreTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingAnnotationTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingContextIIDTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingCoreTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingFreeTypeOpenGLTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingFreeTypeTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingGLtoPSTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingImageTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingLODTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingLabelTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingOpenGLTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingTkTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingVolumeOpenGLTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingVolumeTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkViewsContextIIDTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkViewsCoreTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkViewsInfovisTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingContextOpenGLTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingLICTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersSMPTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkGeovisCoreTCL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOParallelXMLTCL-6.2$(LIBLINKEXT)


VTK_LIBS +=     $(LIBPATH_COMPILER_FLAG)$(VTK_LIBDIRS) \
		$(LIBFLAG)vtkChartsCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonColor-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonComputationalGeometry-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonDataModel-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonExecutionModel-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonMath-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonMisc-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonSystem-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonTransforms-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkDomainsChemistry-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersAMR-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersExtraction-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersFlowPaths-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersGeneral-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersGeneric-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersGeometry-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersHybrid-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersHyperTree-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersImaging-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersModeling-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersParallelImaging-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersParallel-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersProgrammable-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersSelection-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersSources-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersStatistics-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersTexture-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersVerdict-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOAMR-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOEnSight-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOExodus-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOExport-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOGeometry-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOImage-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOImport-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOInfovis-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOLSDyna-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOLegacy-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOMINC-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOMovie-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOPLY-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOParallel-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOSQL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOVideo-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOXML-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOXMLParser-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingColor-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingFourier-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingGeneral-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingHybrid-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingMath-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingMorphological-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingSources-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingStatistics-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkImagingStencil-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkInfovisCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkInfovisLayout-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkInteractionImage-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkParallelCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingAnnotation-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingFreeTypeOpenGL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingFreeType-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingGL2PS-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingImage-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingLOD-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingLabel-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingOpenGL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingVolumeOpenGL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingVolume-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkViewsContext2D-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkViewsCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkViewsInfovis-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingContextOpenGL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingContext2D-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingLIC-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkRenderingOpenGL-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersSMP-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkGeovisCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOParallelXML-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIONetCDF-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkInteractionStyle-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkInteractionWidgets-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkNetCDF_cxx-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkNetCDF-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkNetCDF_cxx-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIONetCDF-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkexoIIc-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkhdf5_hl-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkhdf5-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkWrappingTools-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkDICOMParser-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkalglib-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkexpat-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkftgl-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkfreetype-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkgl2ps-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkjpeg-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkjsoncpp-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtklibxml2-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkmetaio-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkoggtheora-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkpng-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkproj4-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtksqlite-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtksys-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtktiff-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkverdict-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkzlib-6.2$(LIBLINKEXT) \

#
#  libraries for svPre, svPost, svSolver
#

VTK_LIBS +=     $(LIBPATH_COMPILER_FLAG)$(VTK_LIBDIRS) \
                $(LIBFLAG)vtkIOXML-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOXMLParser-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersExtraction-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersModeling-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersGeneral-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersGeometry-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkFiltersCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonExecutionModel-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonDataModel-6.2$(LIBLINKEXT) \
	        $(LIBFLAG)vtkCommonSystem-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonTransforms-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonMisc-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonMath-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkIOCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkCommonCore-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkzlib-6.2$(LIBLINKEXT) \
		$(LIBFLAG)vtkexpat-6.2$(LIBLINKEXT) \
                $(LIBFLAG)vtksys-6.2$(LIBLINKEXT) \
                $(VTK_SYS_LIBS)

ifeq ($(SV_USE_PYTHON),1)
  VTK_LIBS +=  $(LIBFLAG)vtkWrappingPython27Core-6.2$(LIBLINKEXT)
endif

#
#		vtkRenderingHybridOpenGL-6.2.lib \
#		vtkRenderingHybridOpenGLTCL-6.2.lib \
#		vtkRenderingVolumeAMR-6.2.lib \
#		vtkRenderingVolumeAMRTCL-6.2.lib \
#		vtkGeovisCore-6.2.lib \
#		vtkGeovisCoreTCL-6.2.lib
#		vtkViewsGeovis-6.2.lib \
#		vtkViewsGeovisTCL-6.2.lib \
#
