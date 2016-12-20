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

set(lib ${SV_LIB_GEOM_NAME})
list(APPEND CORELIBS ${lib})

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})

set(CXXSRCS cv_sys_geom.cxx cv_ggems.cxx
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
	vtkLocalApproximatingSubdivisionFilter.cxx
	vtkLocalLinearSubdivisionFilter.cxx
	vtkLocalButterflySubdivisionFilter.cxx
	vtkLocalLoopSubdivisionFilter.cxx
	vtkLocalSmoothPolyDataFilter.cxx
	vtkCGSmooth.cxx svMath.cxx sparse_matrix.cxx
	vtkFindSeparateRegions.cxx vtkGetSphereRegions.cxx
	vtkConstrainedBlend.cxx)
set(HDRS cv_sys_geom.h cv_ggems.h
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
	vtkLocalApproximatingSubdivisionFilter.h
	vtkLocalLinearSubdivisionFilter.h
	vtkLocalButterflySubdivisionFilter.h
	vtkLocalLoopSubdivisionFilter.h
	vtkLocalSmoothPolyDataFilter.h
	vtkCGSmooth.h svMath.h sparse_matrix.h
	vtkFindSeparateRegions.h vtkGetSphereRegions.h
	vtkConstrainedBlend.h)

add_library(${lib} ${SV_LIBRARY_TYPE} ${CXXSRCS} cv_geom_init.cxx)

target_link_libraries(${lib}
	${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
	${SV_LIB_GLOBALS_NAME} ${SV_LIB_UTILS_NAME} ${SV_LIB_REPOSITORY_NAME} ${SV_LIB_SOLID_NAME}
	)

if(SV_USE_VMTK)
	target_link_libraries(${lib}
		${SV_LIB_THIRDPARTY_VMTK_NAME}
		)
endif()

if(SV_INSTALL_LIBS)
	install(TARGETS ${lib}
	  RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT CoreExecutables
		LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT CoreLibraries
		ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT CoreLibraries
		)
endif()
if(SV_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
		)
endif()
