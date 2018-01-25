# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

SET(VTK_EXTRA_LIBS vtkCommonCoreTCL vtkCommonMathTCL
	 vtkCommonMiscTCL vtkCommonSystemTCL
	 vtkCommonTransformsTCL vtkCommonDataModelTCL
	 vtkCommonColorTCL vtkCommonExecutionModelTCL
	 vtkFiltersCoreTCL vtkCommonComputationalGeometryTCL
	 vtkFiltersGeneralTCL vtkImagingCoreTCL
	 vtkImagingFourierTCL vtkFiltersStatisticsTCL
	 vtkFiltersExtractionTCL vtkInfovisCoreTCL
	 vtkFiltersGeometryTCL vtkFiltersSourcesTCL
	 vtkIOCoreTCL vtkIOImageTCL
	 vtkIOXMLParserTCL vtkRenderingCoreTCL
	 vtkRenderingFreeTypeTCL vtkImagingHybridTCL
	 vtkRenderingOpenGLTCL vtkRenderingContextIIDTCL
	 vtkChartsCoreTCL vtkIOGeometryTCL
	 vtkIOXMLTCL vtkDomainsChemistryTCL
	 vtkIOLegacyTCL vtkParallelCoreTCL
	 vtkFiltersAMRTCL vtkFiltersFlowPathsTCL
	 vtkFiltersGenericTCL vtkImagingSourcesTCL
	 vtkFiltersHybridTCL vtkFiltersHyperTreeTCL
	 vtkImagingGeneralTCL vtkFiltersImagingTCL
	 vtkFiltersModelingTCL vtkFiltersParallelTCL
	 vtkFiltersParallelImagingTCL vtkFiltersProgrammableTCL
	 vtkFiltersSelectionTCL vtkFiltersTextureTCL
	 vtkFiltersVerdictTCL vtkInfovisLayoutTCL
	 vtkInteractionStyleTCL vtkImagingColorTCL
	 vtkRenderingAnnotationTCL vtkRenderingVolumeTCL
	 vtkInteractionWidgetsTCL vtkViewsCoreTCL
	 vtkGeovisCoreTCL vtkIOAMRTCL
	 vtkIOEnSightTCL vtkIOExodusTCL
	 vtkRenderingGLtoPSTCL vtkIOExportTCL
	 vtkIOImportTCL vtkIOInfovisTCL
	 vtkIOLSDynaTCL vtkIOMINCTCL
	 vtkIOMovieTCL vtkIONetCDFTCL
	 vtkIOPLYTCL vtkIOParallelTCL
	 vtkIOSQLTCL vtkIOVideoTCL
	 vtkImagingMathTCL vtkImagingMorphologicalTCL
	 vtkImagingStatisticsTCL vtkImagingStencilTCL
	 vtkInteractionImageTCL vtkRenderingFreeTypeOpenGLTCL
	 vtkRenderingHybridOpenGLTCL vtkRenderingImageTCL
	 vtkRenderingLODTCL vtkRenderingLabelTCL
	 vtkRenderingTkTCL vtkRenderingVolumeAMRTCL
	 vtkRenderingVolumeOpenGLTCL vtkViewsContextIIDTCL
	 vtkViewsInfovisTCL vtkViewsGeovisTCL
	 vtkCommonColorTCL vtkChartsCore
	 vtkCommonColor vtkDomainsChemistry
	 vtkFiltersFlowPaths vtkFiltersGeneric
	 vtkFiltersHyperTree vtkFiltersParallelImaging
	 vtkFiltersProgrammable vtkFiltersSelection
	 vtkFiltersTexture vtkFiltersVerdict
	 vtkIOAMR
	 vtkIOEnSight vtkIOExodus
	 vtkRenderingGLtoPSTCL
	 vtkIOExport vtkRenderingGL2PS
	 vtkgl2ps vtkIOImport
	 vtkIOInfovis vtklibxml2
	 vtkIOLSDyna vtkIOMINC
	 vtkIOMovie vtkoggtheora
	 vtkIOPLY vtkFiltersParallelTCL
	 vtkIONetCDFTCL vtkIOParallel
	 vtkFiltersParallel vtkIONetCDF
	 vtkNetCDF_cxx vtkexoIIc
	 vtkNetCDF vtkhdf5_hl
	 vtkhdf5 vtkIOSQL
	 vtksqlite vtkIOVideo
	 vtkImagingMath vtkImagingMorphological
	 vtkImagingStatistics vtkImagingStencil
	 vtkRenderingFreeTypeOpenGL vtkRenderingHybridOpenGL
	 vtkRenderingImage vtkRenderingLOD
	 vtkInteractionImage vtkFiltersAMRTCL
	 vtkParallelCoreTCL vtkIOLegacyTCL
	 vtkRenderingVolumeAMR vtkFiltersAMR
	 vtkParallelCore vtkIOLegacy
	 vtkRenderingVolumeOpenGL vtkViewsContext2D
	 vtkGeovisCoreTCL vtkIOXMLTCL
	 vtkIOGeometryTCL vtkViewsInfovisTCL
	 vtkRenderingContextIIDTCL vtkRenderingOpenGLTCL
	 vtkFiltersImagingTCL vtkInfovisLayoutTCL
	 vtkInfovisCoreTCL vtkViewsCoreTCL
	 vtkInteractionWidgetsTCL vtkImagingHybridTCL
	 vtkFiltersHybridTCL vtkImagingGeneralTCL
	 vtkImagingSourcesTCL vtkFiltersModelingTCL
	 vtkInteractionStyleTCL vtkRenderingAnnotationTCL
	 vtkImagingColorTCL vtkRenderingVolumeTCL
	 vtkRenderingLabelTCL vtkRenderingFreeTypeTCL
	 vtkRenderingCoreTCL vtkFiltersExtractionTCL
	 vtkFiltersStatisticsTCL vtkImagingFourierTCL
	 vtkImagingCoreTCL vtkFiltersGeometryTCL
	 vtkFiltersSourcesTCL vtkFiltersGeneralTCL
	 vtkFiltersCoreTCL vtkCommonComputationalGeometryTCL
	 vtkIOImageTCL vtkIOXMLParserTCL
	 vtkIOCoreTCL vtkCommonExecutionModelTCL
	 vtkCommonDataModelTCL vtkCommonMiscTCL
	 vtkCommonSystemTCL vtkCommonTransformsTCL
	 vtkCommonMathTCL vtkCommonCoreTCL
	 vtkViewsGeovis vtkGeovisCore
	 vtkIOXML vtkIOGeometry
	 vtkjsoncpp vtkproj4
	 vtkViewsInfovis vtkRenderingContext2D
	 vtkRenderingOpenGL
	 vtkFiltersImaging vtkInfovisLayout
	 vtkInfovisCore vtkViewsCore
	 vtkInteractionWidgets vtkImagingHybrid
	 vtkFiltersHybrid vtkImagingGeneral
	 vtkImagingSources vtkFiltersModeling
	 vtkInteractionStyle vtkRenderingAnnotation
	 vtkImagingColor vtkRenderingVolume
	 vtkRenderingLabel vtkRenderingFreeType
	 vtkRenderingCore vtkFiltersExtraction
	 vtkFiltersStatistics vtkImagingFourier
	 vtkImagingCore vtkalglib
	 vtkFiltersGeometry vtkFiltersSources
	 vtkFiltersGeneral vtkFiltersCore
	 vtkCommonComputationalGeometry vtkIOImage
	 vtkDICOMParser vtkmetaio
	 vtkjpeg vtkIOXMLParser
	 vtkIOCore vtkCommonExecutionModel
	 vtkCommonDataModel vtkCommonMisc
	 vtkCommonSystem vtkCommonTransforms
	 vtkCommonMath vtkCommonCore
	 vtksys
	 vtkexpat vtkftgl
	 vtkfreetype)
