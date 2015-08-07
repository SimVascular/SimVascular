##=========================================================================
 #
 # Copyright (c) 2014-2015 The Regents of the University of California.
 # All Rights Reserved. 
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
 #
 #=========================================================================#
 
#-----------------------------------------------------------------------------
# Geometry 
#-----------------------------------------------------------------------------

set(lib lib_simvascular_sysgeom)
LIST(APPEND CORELIBS ${lib})

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})

SET(CXXSRCS	cv_sys_geom.cxx cv_ggems.cxx 
	cv_spline.cxx cv_integrate_surface.cxx
	vtkIntegrateAttributes.cxx vtkIntegrateFlowThroughSurface.cxx
	vtkSurfaceVectors.cxx vtkIntersectionPolyDataFilter2.cxx
	vtkBooleanOperationPolyDataFilter2.cxx
	vtkDelaunay2D_60.cxx
	vtkMultiplePolyDataIntersectionFilter.cxx
	vtkFillHolesFilterWithIds.cxx
	vtkLoftPolyDataSolid.cxx
	vtkLocalQuadricDecimation.cxx
	vtkLocalInterpolatingSubdivisionFilter.cxx
	vtkLocalLinearSubdivisionFilter.cxx
	vtkLocalSmoothPolyDataFilter.cxx
	vtkCGSmooth.cxx svMath.cxx sparse_matrix.cxx
	vtkFindSeparateRegions.cxx vtkGetSphereRegions.cxx)
SET(HDRS cv_sys_geom.h cv_ggems.h 
	cv_spline.h cv_integrate_surface.h
	vtkIntegrateAttributes.h vtkIntegrateFlowThroughSurface.h
	vtkSurfaceVectors.h vtkIntersectionPolyDataFilter2.h
	vtkBooleanOperationPolyDataFilter2.h
	vtkDelaunay2D_60.h
	vtkMultiplePolyDataIntersectionFilter.h
	vtkFillHolesFilterWithIds.h
	vtkLoftPolyDataSolid.h
	vtkLocalQuadricDecimation.h
	vtkLocalInterpolatingSubdivisionFilter.h
	vtkLocalLinearSubdivisionFilter.h
	vtkLocalSmoothPolyDataFilter.h
	vtkCGSmooth.h svMath.h sparse_matrix.h
	vtkFindSeparateRegions.h vtkGetSphereRegions.h)

if(SimVascular_USE_GTS)
	SET(CXXSRCS  ${CXXSRCS} vtkSurfaceBooleanOperations.cxx)
	SET(HDRS  ${HDRS} vtkSurfaceBooleanOperations.h)
endif()		

#if(Plugin_VMTK)
#	SET(CXXSRCS ${CXXSRCS} vtkvmtkPolyDataCenterlines.cxx
#		 	       vtkvmtkPolyDataDistanceToCenterlines.cxx
#		               vtkvmtkMinHeap.cxx
#			       vtkvmtkNonManifoldSteepestDescent.cxx
#		               vtkvmtkVoronoiDiagram3D.cxx
#		               vtkvmtkNonManifoldFastMarching.cxx
#		               vtkvmtkSimplifyVoronoiDiagram.cxx
#			       vtkvmtkInternalTetrahedraExtractor.cxx
#			       vtkvmtkSteepestDescentLineTracer.cxx
#			       vtkvmtkCapPolyData.cxx)
#	SET(HDRS ${HDRS} vtkvmtkPolyDataCenterlines.h
#		 	 vtkvmtkPolyDataDistanceToCenterlines.h
#		         vtkvmtkMinHeap.h
#			 vtkvmtkNonManifoldSteepestDescent.h
#		         vtkvmtkVoronoiDiagram3D.h
#		         vtkvmtkNonManifoldFastMarching.h
#		         vtkvmtkSimplifyVoronoiDiagram.h
#			 vtkvmtkSteepestDescentLineTracer.h
#			 vtkvmtkInternalTetrahedraExtractor.h
#			 vtkvmtkCapPolyData.h)
#endif()
add_library(${lib} ${CXXSRCS})

target_link_libraries(${lib} 
	${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
	lib_simvascular_solid lib_simvascular_globals
	)

if(SimVascular_USE_GLIB)
	include_directories(${GLIB_INCLUDE_PATH})
	target_link_libraries(${lib} 
		${GLIB_LIBRARY}
		)
endif()

if(SimVascular_USE_GTS)
	include_directories(${GTS_INCLUDE_PATH})
	target_link_libraries(${lib} 
		${GTS_LIBRARY}
		)
endif()


if(SIMVASCULAR_INSTALL_LIBS)
	install(TARGETS ${lib}
	    RUNTIME DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
		LIBRARY DESTINATION ${SIMVASCULAR_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
		ARCHIVE DESTINATION ${SIMVASCULAR_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
		)
endif()
if(SIMVASCULAR_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
		)
endif()
