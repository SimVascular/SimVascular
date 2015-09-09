# ---------------------
# Visualization toolkit
# ---------------------

VTK_SRCDIR = $(OPEN_SOFTWARE_SOURCES_TOPLEVEL)/vtk-6.2.0
VTK_BINDIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/vtk-6.2.0-tcltk8.5
VTK_LIBDIRS = $(VTK_BINDIR)/lib/RelWithDebInfo
VTK_BINDIRS = $(VTK_BINDIR)/bin/RelWithDebInfo
VTK_SO_PATH = $(VTK_BINDIRS)
VTK_DLLS    = $(VTK_BINDIRS)/*.$(SOEXT)

VTK_SYS_LIBS  = $(LIBFLAG)kernel32$(LIBLINKEXT) $(LIBFLAG)user32$(LIBLINKEXT) \
                $(LIBFLAG)gdi32$(LIBLINKEXT) $(LIBFLAG)winspool$(LIBLINKEXT) $(LIBFLAG)shell32$(LIBLINKEXT) \
                $(LIBFLAG)ole32$(LIBLINKEXT) $(LIBFLAG)oleaut32$(LIBLINKEXT) $(LIBFLAG)uuid$(LIBLINKEXT) \
                $(LIBFLAG)comdlg32$(LIBLINKEXT) $(LIBFLAG)advapi32$(LIBLINKEXT) \
                $(LIBFLAG)comctl32$(LIBLINKEXT) $(LIBFLAG)wsock32$(LIBLINKEXT) \
                $(LIBFLAG)opengl32$(LIBLINKEXT) $(LIBFLAG)vfw32$(LIBLINKEXT)

VTK_INCDIRS = \
-I$(VTK_SRCDIR)/Wrapping/Tools \
-I$(VTK_BINDIR)/Wrapping/Tools \
-I$(VTK_SRCDIR)/Wrapping/Tcl \
-I$(VTK_BINDIR)/Wrapping/Tcl \
-I$(VTK_SRCDIR)/Views/Infovis \
-I$(VTK_BINDIR)/Views/Infovis \
-I$(VTK_SRCDIR)/Views/Geovis \
-I$(VTK_BINDIR)/Views/Geovis \
-I$(VTK_SRCDIR)/Views/Context2D \
-I$(VTK_BINDIR)/Views/Context2D \
-I$(VTK_SRCDIR)/Rendering/VolumeOpenGL \
-I$(VTK_BINDIR)/Rendering/VolumeOpenGL \
-I$(VTK_SRCDIR)/Rendering/VolumeAMR \
-I$(VTK_BINDIR)/Rendering/VolumeAMR \
-I$(VTK_SRCDIR)/ThirdParty/TclTk \
-I$(VTK_BINDIR)/ThirdParty/TclTk \
-I$(VTK_SRCDIR)/Rendering/Tk \
-I$(VTK_BINDIR)/Rendering/Tk \
-I$(VTK_SRCDIR)/Rendering/LOD \
-I$(VTK_BINDIR)/Rendering/LOD \
-I$(VTK_SRCDIR)/Rendering/Label \
-I$(VTK_BINDIR)/Rendering/Label \
-I$(VTK_SRCDIR)/Rendering/Image \
-I$(VTK_BINDIR)/Rendering/Image \
-I$(VTK_SRCDIR)/Rendering/HybridOpenGL \
-I$(VTK_BINDIR)/Rendering/HybridOpenGL \
-I$(VTK_SRCDIR)/Rendering/FreeTypeOpenGL \
-I$(VTK_BINDIR)/Rendering/FreeTypeOpenGL \
-I$(VTK_SRCDIR)/IO/Video \
-I$(VTK_BINDIR)/IO/Video \
-I$(VTK_SRCDIR)/ThirdParty/sqlite \
-I$(VTK_BINDIR)/ThirdParty/sqlite \
-I$(VTK_SRCDIR)/IO/SQL \
-I$(VTK_BINDIR)/IO/SQL \
-I$(VTK_SRCDIR)/IO/PLY \
-I$(VTK_BINDIR)/IO/PLY \
-I$(VTK_SRCDIR)/IO/Parallel \
-I$(VTK_BINDIR)/IO/Parallel \
-I$(VTK_SRCDIR)/IO/NetCDF \
-I$(VTK_BINDIR)/IO/NetCDF \
-I$(VTK_SRCDIR)/ThirdParty/oggtheora \
-I$(VTK_BINDIR)/ThirdParty/oggtheora \
-I$(VTK_SRCDIR)/IO/Movie \
-I$(VTK_BINDIR)/IO/Movie \
-I$(VTK_SRCDIR)/IO/MINC \
-I$(VTK_BINDIR)/IO/MINC \
-I$(VTK_SRCDIR)/IO/LSDyna \
-I$(VTK_BINDIR)/IO/LSDyna \
-I$(VTK_SRCDIR)/ThirdParty/libxml2 \
-I$(VTK_BINDIR)/ThirdParty/libxml2 \
-I$(VTK_BINDIR)/ThirdParty/libxml2/vtklibxml2 \
-I$(VTK_SRCDIR)/IO/Infovis \
-I$(VTK_BINDIR)/IO/Infovis \
-I$(VTK_SRCDIR)/IO/Import \
-I$(VTK_BINDIR)/IO/Import \
-I$(VTK_SRCDIR)/Rendering/GL2PS \
-I$(VTK_BINDIR)/Rendering/GL2PS \
-I$(VTK_SRCDIR)/IO/Export \
-I$(VTK_BINDIR)/IO/Export \
-I$(VTK_SRCDIR)/IO/Exodus \
-I$(VTK_BINDIR)/IO/Exodus \
-I$(VTK_SRCDIR)/IO/EnSight \
-I$(VTK_BINDIR)/IO/EnSight \
-I$(VTK_SRCDIR)/IO/AMR \
-I$(VTK_BINDIR)/IO/AMR \
-I$(VTK_SRCDIR)/Interaction/Image \
-I$(VTK_BINDIR)/Interaction/Image \
-I$(VTK_SRCDIR)/Imaging/Stencil \
-I$(VTK_BINDIR)/Imaging/Stencil \
-I$(VTK_SRCDIR)/Imaging/Statistics \
-I$(VTK_BINDIR)/Imaging/Statistics \
-I$(VTK_SRCDIR)/Imaging/Morphological \
-I$(VTK_BINDIR)/Imaging/Morphological \
-I$(VTK_SRCDIR)/Imaging/Math \
-I$(VTK_BINDIR)/Imaging/Math \
-I$(VTK_SRCDIR)/ThirdParty/gl2ps \
-I$(VTK_BINDIR)/ThirdParty/gl2ps \
-I$(VTK_SRCDIR)/ThirdParty/libproj4 \
-I$(VTK_BINDIR)/ThirdParty/libproj4 \
-I$(VTK_BINDIR)/ThirdParty/libproj4/vtklibproj4 \
-I$(VTK_SRCDIR)/Views/Core \
-I$(VTK_BINDIR)/Views/Core \
-I$(VTK_SRCDIR)/Rendering/Volume \
-I$(VTK_BINDIR)/Rendering/Volume \
-I$(VTK_SRCDIR)/Imaging/Color \
-I$(VTK_BINDIR)/Imaging/Color \
-I$(VTK_SRCDIR)/Rendering/Annotation \
-I$(VTK_BINDIR)/Rendering/Annotation \
-I$(VTK_SRCDIR)/Interaction/Widgets \
-I$(VTK_BINDIR)/Interaction/Widgets \
-I$(VTK_SRCDIR)/Interaction/Style \
-I$(VTK_BINDIR)/Interaction/Style \
-I$(VTK_SRCDIR)/Infovis/Layout \
-I$(VTK_BINDIR)/Infovis/Layout \
-I$(VTK_SRCDIR)/Geovis/Core \
-I$(VTK_BINDIR)/Geovis/Core \
-I$(VTK_SRCDIR)/ThirdParty/verdict \
-I$(VTK_BINDIR)/ThirdParty/verdict \
-I$(VTK_SRCDIR)/Filters/Verdict \
-I$(VTK_BINDIR)/Filters/Verdict \
-I$(VTK_SRCDIR)/Filters/Texture \
-I$(VTK_BINDIR)/Filters/Texture \
-I$(VTK_SRCDIR)/Filters/Selection \
-I$(VTK_BINDIR)/Filters/Selection \
-I$(VTK_SRCDIR)/Filters/Programmable \
-I$(VTK_BINDIR)/Filters/Programmable \
-I$(VTK_SRCDIR)/Filters/ParallelImaging \
-I$(VTK_BINDIR)/Filters/ParallelImaging \
-I$(VTK_SRCDIR)/Filters/Parallel \
-I$(VTK_BINDIR)/Filters/Parallel \
-I$(VTK_SRCDIR)/Filters/Modeling \
-I$(VTK_BINDIR)/Filters/Modeling \
-I$(VTK_SRCDIR)/Imaging/General \
-I$(VTK_BINDIR)/Imaging/General \
-I$(VTK_SRCDIR)/Filters/Imaging \
-I$(VTK_BINDIR)/Filters/Imaging \
-I$(VTK_SRCDIR)/Filters/HyperTree \
-I$(VTK_BINDIR)/Filters/HyperTree \
-I$(VTK_SRCDIR)/Imaging/Sources \
-I$(VTK_BINDIR)/Imaging/Sources \
-I$(VTK_SRCDIR)/Filters/Hybrid \
-I$(VTK_BINDIR)/Filters/Hybrid \
-I$(VTK_SRCDIR)/Filters/Generic \
-I$(VTK_BINDIR)/Filters/Generic \
-I$(VTK_SRCDIR)/Filters/FlowPaths \
-I$(VTK_BINDIR)/Filters/FlowPaths \
-I$(VTK_SRCDIR)/Utilities/HashSource \
-I$(VTK_BINDIR)/Utilities/HashSource \
-I$(VTK_SRCDIR)/IO/Legacy \
-I$(VTK_BINDIR)/IO/Legacy \
-I$(VTK_SRCDIR)/Parallel/Core \
-I$(VTK_BINDIR)/Parallel/Core \
-I$(VTK_SRCDIR)/Filters/AMR \
-I$(VTK_BINDIR)/Filters/AMR \
-I$(VTK_SRCDIR)/ThirdParty/hdf5 \
-I$(VTK_BINDIR)/ThirdParty/hdf5 \
-I$(VTK_SRCDIR)/ThirdParty/hdf5/vtkhdf5/src \
-I$(VTK_SRCDIR)/ThirdParty/hdf5/vtkhdf5/hl/src \
-I$(VTK_BINDIR)/ThirdParty/hdf5/vtkhdf5 \
-I$(VTK_SRCDIR)/ThirdParty/netcdf \
-I$(VTK_BINDIR)/ThirdParty/netcdf \
-I$(VTK_BINDIR)/ThirdParty/netcdf/vtknetcdf \
-I$(VTK_SRCDIR)/ThirdParty/netcdf/vtknetcdf/include \
-I$(VTK_SRCDIR)/ThirdParty/exodusII \
-I$(VTK_BINDIR)/ThirdParty/exodusII \
-I$(VTK_SRCDIR)/ThirdParty/jsoncpp \
-I$(VTK_BINDIR)/ThirdParty/jsoncpp \
-I$(VTK_SRCDIR)/IO/Geometry \
-I$(VTK_BINDIR)/IO/Geometry \
-I$(VTK_SRCDIR)/IO/XML \
-I$(VTK_BINDIR)/IO/XML \
-I$(VTK_SRCDIR)/Domains/Chemistry \
-I$(VTK_BINDIR)/Domains/Chemistry \
-I$(VTK_SRCDIR)/Utilities/EncodeString \
-I$(VTK_BINDIR)/Utilities/EncodeString \
-I$(VTK_SRCDIR)/Utilities/ParseOGLExt \
-I$(VTK_BINDIR)/Utilities/ParseOGLExt \
-I$(VTK_SRCDIR)/Imaging/Hybrid \
-I$(VTK_BINDIR)/Imaging/Hybrid \
-I$(VTK_SRCDIR)/Rendering/OpenGL \
-I$(VTK_BINDIR)/Rendering/OpenGL \
-I$(VTK_SRCDIR)/ThirdParty/ftgl \
-I$(VTK_BINDIR)/ThirdParty/ftgl \
-I$(VTK_SRCDIR)/ThirdParty/ftgl/src \
-I$(VTK_SRCDIR)/ThirdParty/freetype \
-I$(VTK_BINDIR)/ThirdParty/freetype \
-I$(VTK_SRCDIR)/Rendering/FreeType \
-I$(VTK_BINDIR)/Rendering/FreeType \
-I$(VTK_SRCDIR)/Utilities/MaterialLibrary \
-I$(VTK_BINDIR)/Utilities/MaterialLibrary \
-I$(VTK_SRCDIR)/ThirdParty/expat \
-I$(VTK_BINDIR)/ThirdParty/expat \
-I$(VTK_SRCDIR)/IO/XMLParser \
-I$(VTK_BINDIR)/IO/XMLParser \
-I$(VTK_SRCDIR)/ThirdParty/tiff \
-I$(VTK_BINDIR)/ThirdParty/tiff \
-I$(VTK_SRCDIR)/ThirdParty/png \
-I$(VTK_BINDIR)/ThirdParty/png \
-I$(VTK_SRCDIR)/ThirdParty/jpeg \
-I$(VTK_BINDIR)/ThirdParty/jpeg \
-I$(VTK_SRCDIR)/ThirdParty/zlib \
-I$(VTK_BINDIR)/ThirdParty/zlib \
-I$(VTK_SRCDIR)/IO/Core \
-I$(VTK_BINDIR)/IO/Core \
-I$(VTK_SRCDIR)/IO/Image \
-I$(VTK_BINDIR)/IO/Image \
-I$(VTK_SRCDIR)/Filters/Sources \
-I$(VTK_BINDIR)/Filters/Sources \
-I$(VTK_SRCDIR)/Filters/Geometry \
-I$(VTK_BINDIR)/Filters/Geometry \
-I$(VTK_SRCDIR)/Rendering/Core \
-I$(VTK_BINDIR)/Rendering/Core \
-I$(VTK_SRCDIR)/Rendering/Context2D \
-I$(VTK_BINDIR)/Rendering/Context2D \
-I$(VTK_SRCDIR)/Imaging/Core \
-I$(VTK_BINDIR)/Imaging/Core \
-I$(VTK_SRCDIR)/Imaging/Fourier \
-I$(VTK_BINDIR)/Imaging/Fourier \
-I$(VTK_SRCDIR)/Filters/Statistics \
-I$(VTK_BINDIR)/Filters/Statistics \
-I$(VTK_SRCDIR)/Common/ComputationalGeometry \
-I$(VTK_BINDIR)/Common/ComputationalGeometry \
-I$(VTK_SRCDIR)/Filters/General \
-I$(VTK_BINDIR)/Filters/General \
-I$(VTK_SRCDIR)/Filters/Core \
-I$(VTK_BINDIR)/Filters/Core \
-I$(VTK_SRCDIR)/Common/ExecutionModel \
-I$(VTK_BINDIR)/Common/ExecutionModel \
-I$(VTK_SRCDIR)/Filters/Extraction \
-I$(VTK_BINDIR)/Filters/Extraction \
-I$(VTK_SRCDIR)/Infovis/Core \
-I$(VTK_BINDIR)/Infovis/Core \
-I$(VTK_SRCDIR)/Common/Transforms \
-I$(VTK_BINDIR)/Common/Transforms \
-I$(VTK_SRCDIR)/Common/System \
-I$(VTK_BINDIR)/Common/System \
-I$(VTK_SRCDIR)/Common/Misc \
-I$(VTK_BINDIR)/Common/Misc \
-I$(VTK_SRCDIR)/Utilities/KWSys \
-I$(VTK_BINDIR)/Utilities/KWSys \
-I$(VTK_SRCDIR)/Common/Core \
-I$(VTK_BINDIR)/Common/Core \
-I$(VTK_SRCDIR)/Common/Math \
-I$(VTK_BINDIR)/Common/Math \
-I$(VTK_SRCDIR)/Common/DataModel \
-I$(VTK_BINDIR)/Common/DataModel \
-I$(VTK_SRCDIR)/Common/Color \
-I$(VTK_BINDIR)/Common/Color \
-I$(VTK_SRCDIR)/Charts/Core \
-I$(VTK_BINDIR)/Charts/Core \
-I$(VTK_SRCDIR)/ThirdParty/alglib \
-I$(VTK_BINDIR)/ThirdParty/alglib \
-I$(VTK_BINDIR)/Utilities/DICOMParser \
-I$(VTK_SRCDIR)/Utilities/DICOMParser \
-I$(VTK_BINDIR)/Utilities/MetaIO/vtkmetaio \
-I$(VTK_BINDIR)/Utilities/MetaIO \
-I$(VTK_SRCDIR)/Utilities/MetaIO

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
