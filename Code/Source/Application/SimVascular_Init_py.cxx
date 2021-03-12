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

//#include "sv_repos_init_py.h"
#include "Geometry_PyModule.h"
#include "Imaging_PyModule.h"
//#include "sv2_image_init_py.h"
#include "Math_PyModule.h"
//#include "sv_polydatasolid_init_py.h"

#include "Modeling_PyModule.h"
#include "PathPlanning_PyModule.h"
//#include "sv3_PathGroup_init_py.h"
#include "Segmentation_PyModule.h"
#include "Simulation_PyModule.h"
//#include "sv3_CircleContour_init_py.h"
//#include "sv3_LevelSetContour_init_py.h"
//#include "sv3_ThresholdContour_init_py.h"
//#include "sv3_SplinePolygonContour_init_py.h"
//#include "sv3_PolygonContour_init_py.h"
#include "Project_PyModule.h"
//#include "sv4gui_Vis_init_py.h"

#ifdef SV_USE_VMTK
  #include "Vmtk_PyModule.h"
#endif

#ifdef SV_USE_TETGEN
  #include "Meshing_PyModule.h"
#endif

#ifdef SV_USE_MMG
  #include "MeshUtils_PyModule.h"
#endif

#ifdef SV_USE_TETGEN_ADAPTOR
  //#include "sv_adapt_init_py.h"
  //#include "sv_tetgen_adapt_init_py.h"
#endif

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 3                         
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 3

//--------------------
// SimVascular_pyInit
//--------------------
// Initialize the SV Python C API modules.
//
// The module names and initialization functions are implemented 
// in *init_py.{cxx,h} files for most sv pipeline functionality:
// Mesh, Path, Model, etc.
//
void SimVascular_pyInit()
{
    //-----------------
    // Primary modules
    //-----------------
    //
    printf("Loading Python Modules ...\n");
    //PyImport_AppendInittab("circle_contour",PyInit_pyCircleContour);
    PyImport_AppendInittab("segmentation", PyInit_PySegmentation);
    PyImport_AppendInittab("geometry", PyInit_PyGeometry);
    PyImport_AppendInittab("imaging", PyInit_PyImaging);
    //PyImport_AppendInittab("levelset_contour",PyInit_pylevelSetContour);
    // [DaveP] Don't expose math module.
    //PyImport_AppendInittab("math", PyInit_PyMath);
    PyImport_AppendInittab("pathplanning", PyInit_PyPathplanning);
    //PyImport_AppendInittab("path_group", PyInit_PyPathGroup);
    //PyImport_AppendInittab("polygon_contour",PyInit_pyPolygonContour);
    //PyImport_AppendInittab("project", PyInit_PyProject);
    //PyImport_AppendInittab("repository", PyInit_pyRepository);
    PyImport_AppendInittab("modeling", PyInit_PyModeling);
    PyImport_AppendInittab("simulation", PyInit_PySimulation);
    //PyImport_AppendInittab("solid_polydata",PyInit_pySolidPolydata);
    //PyImport_AppendInittab("spline_polygon_contour",PyInit_pySplinePolygonContour);
    //PyImport_AppendInittab("threshold_contour", PyInit_pyThresholdContour);

    // [TODO:DaveP] why is pyGUI not loaded here?
    //PyImport_AppendInittab("pyGUI",PyInit_pyGUI);
    
    //------------------
    // Optional modules
    //------------------

    #ifdef SV_USE_VMTK
    PyImport_AppendInittab("vmtk", PyInit_PyVmtk);
    #endif

    #ifdef SV_USE_OpenCASCADE
    //PyImport_AppendInittab("solid_occt", PyInit_pySolidOCCT);
    #endif

    #ifdef SV_USE_TETGEN
    PyImport_AppendInittab("meshing", PyInit_PyMeshing);
    //PyImport_AppendInittab("tetgen_mesh", PyInit_pyMeshTetgen);
    #endif

    #ifdef SV_USE_MMG
    //PyImport_AppendInittab("meshing", PyInit_PyMeshingObject);
    PyImport_AppendInittab("mesh_utils", PyInit_PyMeshUtils);
    #endif

    #ifdef SV_USE_ITK
    // [TODO:DaveP] don't expose this for now.
    //PyImport_AppendInittab("itk_levelset", PyInit_pyItkls);
    #endif

    #ifdef SV_USE_TETGEN_ADAPTOR
    //PyImport_AppendInittab("mesh_adapt",PyInit_pyMeshAdapt);
    //PyImport_AppendInittab("tetgen_adapt",PyInit_pyTetGenAdapt);
    #endif

}
#endif

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 2                         
//---------------------------------------------------------------------------

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
    initpySolid();
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

void SimVascular_pyImport()
{
    
  PyImport_ImportModule("pyRepository");
  PyImport_ImportModule("pyMath");
  PyImport_ImportModule("pyGeom");
  PyImport_ImportModule("pyImage");
  PyImport_ImportModule("pyPath");
  PyImport_ImportModule("pyPathIO");
  
  PyImport_ImportModule("pyContour");
  PyImport_ImportModule("pyThresholdContour");
  PyImport_ImportModule("pylevelSetContour");
  PyImport_ImportModule("pyCircleContour");
  PyImport_ImportModule("pyPolygonContour");
  PyImport_ImportModule("pySplinePolygonContour");
  
  PyImport_ImportModule("pySolid");
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
