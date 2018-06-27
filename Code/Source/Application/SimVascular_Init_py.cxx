/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "SimVascular.h"
#include "sv_IOstream.h"

#include <time.h>
#include <stdlib.h>

#include "Python.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

#include "SimVascular_Init_py.h"

#include "vtkToolkits.h"

#include "sv_repos_init_py.h"
#include "sv_geom_init_py.h"
#include "sv2_image_init_py.h"
#include "sv_math_init_py.h"
#include "sv_polydatasolid_init_py.h"

#include "sv_solid_init_py.h"
#include "sv3_PathElement_init_py.h"
#include "sv3_Contour_init_py.h"
#include "sv3_CircleContour_init_py.h"
#include "sv3_LevelSetContour_init_py.h"
#include "sv3_ThresholdContour_init_py.h"
#include "sv3_SplinePolygonContour_init_py.h"
#include "sv3_PolygonContour_init_py.h"
#include "sv4gui_Vis_init_py.h"

#ifdef SV_USE_VMTK
  #include "sv_vmtk_utils_init_py.h"
#endif

#ifdef SV_USE_OpenCASCADE
  #include "sv_occt_init_py.h"
#endif

#ifdef SV_USE_TETGEN
  #include "sv_mesh_init_py.h"
  #include "sv_tetgen_mesh_init_py.h"
#endif

#ifdef SV_USE_MMG
  #include "sv_mesh_init_py.h"
  #include "sv_mmg_mesh_init_py.h"
#endif

#ifdef SV_USE_ITK
  #include "sv3_ITKLset_init_py.h"
#endif

#ifdef SV_USE_TETGEN_ADAPTOR
  #include "sv_adapt_init_py.h"
  #include "sv_tetgen_adapt_init_py.h"
#endif

#if PYTHON_MAJOR_VERSION ==2
void SimVascular_pyInit()
{

    initpyMath();
    initpyRepository();
    initpyGeom();
    initpyImage();
    initpyPath();
    initpyContour();
    initpyThresholdContour();
    initpylevelSetContour();
    initpyCircleContour();
    initpyPolygonContour();
    initpySplinePolygonContour();
    initpySolid2();
    initpySolidPolydata();
    //initpyGUI();
    
#ifdef SV_USE_VMTK
   initpyVMTKUtils();
#endif

#ifdef SV_USE_OpenCASCADE
   initpySolidOCCT();
#endif

#ifdef SV_USE_TETGEN
  initpyMeshObject();
  initpyMeshTetgen();
#endif

#ifdef SV_USE_MMG
  initpyMeshObject();
  initpyMeshUtil();
#endif

#ifdef SV_USE_ITK
  initpyItkls();
#endif

#ifdef SV_USE_TETGEN_ADAPTOR
  initpyMeshAdapt();
  initpyTetGenAdapt();
#endif
    
}
#endif

#if PYTHON_MAJOR_VERSION ==3
void SimVascular_pyInit()
{
    printf("Loading Python Modules\n");
    PyImport_AppendInittab("pyRepository",PyInit_pyRepository);
    PyImport_AppendInittab("pyMath",PyInit_pyMath);
    PyImport_AppendInittab("pyGeom",PyInit_pyGeom);
    PyImport_AppendInittab("pyImage",PyInit_pyImage);
    PyImport_AppendInittab("pyPath",PyInit_pyPath);
    PyImport_AppendInittab("pyContour",PyInit_pyContour);
    PyImport_AppendInittab("pyThresholdContour",PyInit_pyThresholdContour);
    PyImport_AppendInittab("pylevelSetContour",PyInit_pylevelSetContour);
    PyImport_AppendInittab("pyCircleContour",PyInit_pyCircleContour);
    PyImport_AppendInittab("pyPolygonContour",PyInit_pyPolygonContour);
    PyImport_AppendInittab("pySplinePolygonContour",PyInit_pySplinePolygonContour);
    PyImport_AppendInittab("pySolid2",PyInit_pySolid2);
    PyImport_AppendInittab("pySolidPolydata",PyInit_pySolidPolydata);
    //PyImport_AppendInittab("pyGUI",PyInit_pyGUI);
    
#ifdef SV_USE_VMTK
   PyImport_AppendInittab("pyVMTKUtils",PyInit_pyVMTKUtils);
#endif

#ifdef SV_USE_OpenCASCADE
   PyImport_AppendInittab("pySolidOCCT",PyInit_pySolidOCCT);
#endif

#ifdef SV_USE_TETGEN
  PyImport_AppendInittab("pyMeshObject",PyInit_pyMeshObject);
  PyImport_AppendInittab("pyMeshTetgen",PyInit_pyMeshTetgen);
#endif

#ifdef SV_USE_MMG
  PyImport_AppendInittab("pyMeshObject",PyInit_pyMeshObject);
  PyImport_AppendInittab("pyMeshUtil",PyInit_pyMeshUtil);
#endif

#ifdef SV_USE_ITK
  PyImport_AppendInittab("pyItkls",PyInit_pyItkls);
#endif

#ifdef SV_USE_TETGEN_ADAPTOR
  PyImport_AppendInittab("pyMeshAdapt",PyInit_pyMeshAdapt);
  PyImport_AppendInittab("pyTetGenAdapt",PyInit_pyTetGenAdapt);
#endif



    
}
#endif

void SimVascular_pyImport()
{
    
  PyImport_ImportModule("pyRepository");
  PyImport_ImportModule("pyMath");
  PyImport_ImportModule("pyGeom");
  PyImport_ImportModule("pyImage");
  PyImport_ImportModule("pyPath");
  
  PyImport_ImportModule("pyContour");
  PyImport_ImportModule("pyThresholdContour");
  PyImport_ImportModule("pylevelSetContour");
  PyImport_ImportModule("pyCircleContour");
  PyImport_ImportModule("pyPolygonContour");
  PyImport_ImportModule("pySplinePolygonContour");
  
  PyImport_ImportModule("pySolid2");
  PyImport_ImportModule("pySolidPolydata");
  
#ifdef SV_USE_VMTK
   PyImport_ImportModule("pyVMTKUtils");
#endif

#ifdef SV_USE_OpenCASCADE
   PyImport_ImportModule("pySolidOCCT");
#endif

#ifdef SV_USE_TETGEN
  PyImport_ImportModule("pyMeshObject");
  PyImport_ImportModule("pyMeshTetgen");
#endif

#ifdef SV_USE_MMG
  PyImport_ImportModule("pyMeshObject");
  PyImport_ImportModule("pyMeshUtil");
#endif

#ifdef SV_USE_ITK
  PyImport_ImportModule("pyItkls");
#endif

#ifdef SV_USE_TETGEN_ADAPTOR
  PyImport_ImportModule("pyMeshAdapt");
  PyImport_ImportModule("pyTetGenAdapt");
#endif

}