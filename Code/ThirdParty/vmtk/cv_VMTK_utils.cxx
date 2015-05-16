/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
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
 *
 *=========================================================================*/

/** @file cv_VMTK_utils.cxx
 *  @brief These functions are utilities that implement vtkvmtk classes
 *  @details Provides boundary layer and surface remeshing.
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "cv_VMTK_utils.h"

#include "SimVascular.h"

#include "vtkMath.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkTriangleFilter.h"
#include "vtkDecimatePro.h"
#include "vtkCleanPolyData.h"
#include "vtkCellData.h"
#include "vtkPolyDataNormals.h"
#include "vtkPointData.h"
#include "vtkIntArray.h"
#include "vtkDoubleArray.h"
#include "vtkThreshold.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkGeometryFilter.h"
#include "vtkCellArray.h"

#include "vtkvmtkPolyDataSurfaceRemeshing.h"
#include "vtkvmtkPolyDataSizingFunction.h"
#include "vtkvmtkSurfaceProjection.h"
#include "vtkvmtkCapPolyData.h"
#include "vtkvmtkSimpleCapPolyData.h"
#include "vtkvmtkAppendFilter.h"
#include "vtkvmtkBoundaryLayerGenerator.h"
#include "cv_polydatasolid_utils.h"
// --------------------
//  VMTKUtils_SurfaceRemeshing
// --------------------
/**
 * @brief Function to remesh a surface
 * @param *surfaceMesh this is the current discrete polydata surface to be
 * remeshed. It is also the new returen surface remesh
 * @param maxEdgeSize This is the edge size that would like to be targeted
 * for the surface remesh
 * @param meshcapsonly This is boolean on whether the whole mesh should be 
 * meshed or just caps. If meshcapsonly is 1, then you must provide a list
 * of excluded ids in the *excludedIds param.
 * @param preservedges This is a boolean on whether the feature edges should
 * be preserved or not. In most cases, this is 1.
 * @param trianglesplit factor. This is just the value for a split triangle
 * when it must be split. Default is 1.0.
 * @param collapseanglethreshold. This is the angle at which a triangle is 
 * bad triangle and should be collapsed. Default is 0.5.
 * @param *excludedIds These are the excluded regions if meshcapsonly is 
 * specified. 
 * @param cellEntityIdsArrayName This is the array name assigned to the nodes
 * of the output surface remesh
 * @param useSizingFunction This is if we want to specify a distance at each
 * node for the mesh. Used for mesh refinement!
 * @param *meshSizingFunction This is the actual meshSizing Function. Must 
 * exist if using a sizing function
 * @return CV_OK is executed correctly. If something goes wrong, CV_ERROR 
 * is returned
 */
int VMTKUtils_SurfaceRemeshing(vtkPolyData *surfaceMesh, double maxEdgeSize,
    int meshcapsonly,int preserveedges,double trianglesplitfactor, 
    double collapseanglethreshold, vtkIdList *excludedIds,
    std::string cellEntityIdsArrayName,int useSizingFunction,
    vtkDoubleArray *meshSizingFunction)
{
  double edgesize;
  vtkIdType i;
  vtkSmartPointer<vtkPolyData> surfacepd = 
    vtkSmartPointer<vtkPolyData>::New();
  surfacepd->DeepCopy(surfaceMesh);

  //Triangle filters and decimator to produce mesh that is remeshable
  vtkSmartPointer<vtkTriangleFilter> tris1 = 
    vtkSmartPointer<vtkTriangleFilter>::New();
  vtkSmartPointer<vtkCleanPolyData> cleaner = 
    vtkSmartPointer<vtkCleanPolyData>::New();
  vtkSmartPointer<vtkCleanPolyData> cleaner2 = 
    vtkSmartPointer<vtkCleanPolyData>::New();
  vtkSmartPointer<vtkvmtkSurfaceProjection> projector = 
    vtkSmartPointer<vtkvmtkSurfaceProjection>::New();
  vtkSmartPointer<vtkPolyDataNormals> normaler =
    vtkSmartPointer<vtkPolyDataNormals>::New();

  //Remeshing through vmtk
  vtkSmartPointer<vtkvmtkPolyDataSurfaceRemeshing> remesher =
    vtkSmartPointer<vtkvmtkPolyDataSurfaceRemeshing>::New();

  //Check the inputs to the function and apply default values if necessary
  if (trianglesplitfactor == NULL)
  {
    trianglesplitfactor = 1.0;
  }
  if (collapseanglethreshold == NULL)
  {
    collapseanglethreshold = 0.5;
  }
  if (meshcapsonly && excludedIds == NULL)
  {
    //Need list of ids to mesh only caps
    fprintf(stderr,"Cannot mesh only caps without Ids to exclude\n");
    return CV_ERROR;
  }

  if (useSizingFunction && meshSizingFunction == NULL)
  {
    fprintf(stderr,"Cannot use sizing function without a function!");
    return CV_ERROR;
  }

  //Apply the Entity Ids array name if not preserving edges and meshing all
  if (!preserveedges && !meshcapsonly)
  {
    vtkSmartPointer<vtkIntArray> cellIds = vtkSmartPointer<vtkIntArray>::New();
    cellIds->SetName(cellEntityIdsArrayName.c_str());
    cellIds->SetNumberOfTuples(surfacepd->GetNumberOfCells());
    cellIds->FillComponent(0,0.0);
    surfacepd->GetCellData()->AddArray(cellIds);
  }
  //If applying a meshsizing function, transfer the values to an array of 
  //areas based on the edge sizes specified
  if (useSizingFunction)
  {
    meshSizingFunction->SetName("SizingFunction");
    for (i=0;i<meshSizingFunction->GetNumberOfTuples();i++)
    {
      edgesize = meshSizingFunction->GetValue(i);
      if (edgesize == 0)
      {
        meshSizingFunction->SetValue(i,(0.25)*pow(3.0,0.5)*pow(maxEdgeSize,2.0));
      }
      else 
      {
        meshSizingFunction->SetValue(i,(0.25)*pow(3.0,0.5)*pow(edgesize,2.0));
      }
    }
    surfacepd->GetPointData()->AddArray(meshSizingFunction);
  }

  cleaner->SetInputData(surfacepd);
  cleaner->Update();
  
  tris1->SetInputData(cleaner->GetOutput());
  tris1->Update();

  //Set up input based on full or cap remesh
  if (meshcapsonly)
  {
    remesher->SetInputData(surfacepd);
  }
  else
  {
    remesher->SetInputData(tris1->GetOutput());
  }

  //Set the remesher options based on the input
  remesher->SetPreserveBoundaryEdges(preserveedges);
  remesher->SetCellEntityIdsArrayName(cellEntityIdsArrayName.c_str());
  if (useSizingFunction)
  {
    remesher->SetElementSizeModeToTargetAreaArray();
    remesher->SetTargetAreaArrayName("SizingFunction");
  }
  else
  {
    remesher->SetElementSizeModeToTargetArea();
    remesher->SetTargetArea((0.25)*pow(3.0,0.5)*pow(maxEdgeSize,2.0));
  }
  remesher->SetMaxArea(1e16);
  remesher->SetMinArea(0.0);
  remesher->SetTriangleSplitFactor(trianglesplitfactor);
  remesher->SetCollapseAngleThreshold(collapseanglethreshold);
  if (meshcapsonly)
  {
    remesher->SetExcludedEntityIds(excludedIds);
  }
  //Remesh the surface
  remesher->Update();

  if (meshcapsonly)
  {
    surfaceMesh->DeepCopy(remesher->GetOutput());
  }
  //If meshing the whole thing, project it back to the original surface,
  //compute the normals, and clean the mesh
  else
  {
    projector->SetInputData(remesher->GetOutput());
    projector->SetReferenceSurface(surfacepd);
    projector->Update();
    
    normaler->SetInputData(projector->GetOutput());
    normaler->SetConsistency(1);
    normaler->SetAutoOrientNormals(1);
    normaler->SetFlipNormals(0);
    normaler->SetComputeCellNormals(0);
    normaler->SplittingOff();
    normaler->Update();

    cleaner2->SetInputData(normaler->GetOutput());
    cleaner2->Update();

    surfaceMesh->DeepCopy(cleaner2->GetOutput());
    surfaceMesh->GetPointData()->GetNormals()->SetName("Normals");
  }

  return CV_OK;
}

// --------------------
//  VMTKUtils_ComputeSizingFunction
// --------------------
/** 
 * @brief Function to compute a sizing function of the current mesh. This
 * is a function describing the size of the mesh at each location
 * @param *inpd This is the input vtkPolyData to compute the funciton on.
 * @param scalefactor This is to scale the output by some factor if necessary
 * Default is 1.2.
 * @param sizingFunctionArrayName This is the array name given to the output
 * sizing function attached to the mesh.
 * @return CV_OK if the sizing function is computed correctly. 
 */
int VMTKUtils_ComputeSizingFunction(vtkPolyData *inpd, double scalefactor,
    std::string sizingFunctionArrayName)
{
  vtkSmartPointer<vtkvmtkPolyDataSizingFunction> sizer =
    vtkSmartPointer<vtkvmtkPolyDataSizingFunction>::New();
  vtkSmartPointer<vtkPolyData> copypd = 
    vtkSmartPointer<vtkPolyData>::New();
  copypd->DeepCopy(inpd);

  if (scalefactor == NULL)
  {
    scalefactor = 1.8;
  }

  sizer->SetInputData(copypd);   
  //"VolumeSizingFunction"
  sizer->SetSizingFunctionArrayName(sizingFunctionArrayName.c_str());
  //sizer->SetScaleFactor(0.8);
  sizer->SetScaleFactor(scalefactor);
  sizer->Update();

  inpd->DeepCopy(sizer->GetOutput());
  
  fprintf(stderr,"Got Volume Mesh Func!\n");
  return CV_OK;
}

// --------------------
//  VMTKUtils_Capper
// --------------------
/** 
 * @brief Function to cap or fill the holes of a vtkPolyData
 * @param *inpd This is the vtkPolyData to apply the filter
 * @param captype This is the type of cap desired. 0 is used for a basic or 
 * simple cap. 1 is used if you would like a cap with a point in the center
 * of the holes.
 * @param trioutput This is boolean telling whether to make sure the output
 * caps have only triangles or not.
 * @param cellEntityIdOffset This is used if an offset from surface is desire
 * @param cellEntityIdsArrayName This is the name given if offset is desired
 * @return CV_OK if the mesh sizing function based on the circle is computed
 * correctly 
 */
int VMTKUtils_Capper(vtkPolyData *inpd,int captype,int trioutput,
    int cellEntityIdOffset,std::string cellEntityIdsArrayName)
{
  vtkSmartPointer<vtkvmtkSimpleCapPolyData> simplecapper = 
    vtkSmartPointer<vtkvmtkSimpleCapPolyData>::New();
  vtkSmartPointer<vtkvmtkCapPolyData> capper = 
    vtkSmartPointer<vtkvmtkCapPolyData>::New();
  vtkSmartPointer<vtkPolyData> copypd = 
    vtkSmartPointer<vtkPolyData>::New();
  vtkSmartPointer<vtkPolyDataNormals> normaler =
    vtkSmartPointer<vtkPolyDataNormals>::New();
  copypd->DeepCopy(inpd);
  vtkSmartPointer<vtkTriangleFilter> tris = 
    vtkSmartPointer<vtkTriangleFilter>::New();

  if (cellEntityIdOffset == NULL)
  {
    cellEntityIdOffset = 1;
  }
  //0 is simple, 1 is center
  if (captype == 0)
  {
    simplecapper->SetInputData(copypd);
    simplecapper->SetCellEntityIdsArrayName(cellEntityIdsArrayName.c_str());
    simplecapper->SetCellEntityIdOffset(cellEntityIdOffset);
    simplecapper->Update();
  }
  else if (captype == 1)
  {
    capper->SetInputData(copypd);
    capper->SetDisplacement(0.0);
    capper->SetInPlaneDisplacement(0.0);
    capper->Update();
  }


  if (trioutput)
  {
    if (captype == 0)
    {
      tris->SetInputData(simplecapper->GetOutput());
    }
    else if (captype == 1)
    {
      tris->SetInputData(capper->GetOutput());
    }
    tris->PassLinesOff();
    tris->PassVertsOff();
    tris->Update();
    
    normaler->SetInputData(tris->GetOutput());
  }
  else
  {
    if (captype == 0)
    {
      normaler->SetInputData(simplecapper->GetOutput());
    }
    else if (captype == 1)
    {
      normaler->SetInputData(capper->GetOutput());
    }
  }

  normaler->AutoOrientNormalsOn();
  normaler->SplittingOff();
  normaler->ConsistencyOn();
  normaler->Update();

  inpd->DeepCopy(normaler->GetOutput());

  return CV_OK;
}

// --------------------
//  VMTKUtils_BoundaryLayerMesh
// --------------------
/** 
 * @brief Function to compute the boundary layer on a mesh
 * @param blMesh This is the returned boundary layer mesh as a vtu
 * @param innerSurface This is the inner surface of the final bl mesh
 * @param edgeSize This is the desired edge size for the bl mesh
 * @param blThicknessFactor This is the portion of the edge size that you 
 * would like to be used for the thickness of the boundary layer
 * @param numSublayers This is the number of layers in the bl mesh.
 * @param sublayerRation This is the ratio at which to decrease the bl mesh 
 * by.
 * @param sidewallCellEntityId This is the value to assign to the tets that 
 * align along the sidewall of the final bl mesh
 * @param innerSurfaceCellEntityId This is the value assigned to the inner
 * surface of the bl mesh
 * @param negateWarpVectors This is whether or not to use the current vector
 * direction or use the exact opposite. Typically this is 1 because you use 
 * the normals as the direction and you would like the bl mesh to go inward
 * (normals are typically oriented outward).
 * @param cellEntityIdsArrayName This is the name of the array given to the 
 * nodes of the output mesh.
 * @return CV_OK if the boundary layer mesh is created properly
 */
int VMTKUtils_BoundaryLayerMesh(vtkUnstructuredGrid *blMesh,
    vtkUnstructuredGrid *innerSurface,
    double edgeSize,double blThicknessFactor,int numSublayers,
    double sublayerRatio,int sidewallCellEntityId,
    int innerSurfaceCellEntityId,int negateWarpVectors, 
    std::string cellEntityIdsArrayName)
{
  vtkSmartPointer<vtkvmtkBoundaryLayerGenerator> layerer = 
    vtkSmartPointer<vtkvmtkBoundaryLayerGenerator>::New();
// needs fixed!!! NMW 2014-08-04
#ifndef WIN32
  vtkSmartPointer<vtkDoubleArray> checkArray = 
    vtkSmartPointer<vtkDoubleArray>::New();
#endif
  vtkSmartPointer<vtkUnstructuredGrid> copyug = 
    vtkSmartPointer<vtkUnstructuredGrid>::New();
  copyug->DeepCopy(blMesh);

  try 
  {
// needs fixed!!!  NMW 2014-08-04, MSVC compiler doesn't like.
// should just check for array, not copy it anyway.
//
// neither does intel on linux!! 2014-08-14

//    checkArray = vtkDoubleArray::SafeDownCast(copyug->GetPointData()->GetArray("Normals"));
//
  }
  catch ( ... )
  {
    fprintf(stderr,"Normals vector doesn't exist in Unstructured Grid \
	and must exist for boundary layer formation\n");
    return CV_ERROR;
  }

  if (blThicknessFactor == NULL)
  {
    blThicknessFactor = 0.5;
  }
  if (numSublayers == NULL)
  {
    numSublayers = 2;
  }
  if (sublayerRatio == NULL)
  {
    sublayerRatio = 0.3;
  }

  layerer->SetInputData(copyug);
  layerer->SetLayerThickness(edgeSize*blThicknessFactor);
  layerer->SetLayerThicknessRatio(blThicknessFactor);
  layerer->SetNumberOfSubLayers(numSublayers);
  layerer->SetSubLayerRatio(sublayerRatio);
  layerer->SetNegateWarpVectors(negateWarpVectors);
  layerer->SetWarpVectorsArrayName("Normals");
  layerer->SetCellEntityIdsArrayName(cellEntityIdsArrayName.c_str());
  layerer->SetConstantThickness(1);
  //9999
  layerer->SetSidewallCellEntityId(sidewallCellEntityId);
  //1
  layerer->SetInnerSurfaceCellEntityId(innerSurfaceCellEntityId);
  layerer->SetIncludeSurfaceCells(0);
  layerer->SetIncludeSidewallCells(1);
  layerer->Update();

  blMesh->DeepCopy(layerer->GetOutput());
  innerSurface->DeepCopy(layerer->GetInnerSurface());

  return CV_OK;
}
// --------------------
//  VMTKUtils_AppendMesh
// --------------------
/** 
 * @brief Function append the final combined boundary layer mesh with the 
 * volume mesh
 * @param meshFromTetGen This is the full vtu output from TetGen
 * @param innerMesh This is the original inner surface extracted from the 
 * interior of the boundary layer mesh
 * @param boundaryMesh This is the actual boundary layer mesh
 * @param surfaceWithSize This is the surface input to tetgen with the mesh
 * sizing function attached to it.
 * @param This is the name of the array within the surfaceWithSize mesh that 
 * contains the values pertaining to whehter or not it is a capped surface or 
 * not.
 * @return CV_OK if the meshes are appended into one final mesh correctly. 
 * The full mesh is returned in the first argument
 */

int VMTKUtils_AppendMesh(vtkUnstructuredGrid *meshFromTetGen, 
    vtkUnstructuredGrid *innerMesh, vtkUnstructuredGrid *boundaryMesh,
    vtkUnstructuredGrid *surfaceWithSize,
    std::string cellEntityIdsArrayName)
{
  vtkSmartPointer<vtkvmtkAppendFilter> appender =
    vtkSmartPointer<vtkvmtkAppendFilter>::New();
  vtkSmartPointer<vtkThreshold> thresholder = 
    vtkSmartPointer<vtkThreshold>::New();
  vtkSmartPointer<vtkUnstructuredGrid> endcaps = 
    vtkSmartPointer<vtkUnstructuredGrid>::New();

  //Threshold out the endcaps from the surface
  thresholder->SetInputData(surfaceWithSize);
  thresholder->ThresholdByUpper(1.5);
  thresholder->SetInputArrayToProcess(0,0,0,1,cellEntityIdsArrayName.c_str());
  thresholder->Update();

  //Set the input data for the mesh appender and run
  endcaps->DeepCopy(thresholder->GetOutput());
  appender->AddInputData(innerMesh);
  appender->AddInputData(boundaryMesh);
  appender->AddInputData(meshFromTetGen);
  appender->AddInputData(endcaps);
  appender->Update();

  fprintf(stdout,"Mesh Appended\n");

  meshFromTetGen->DeepCopy(appender->GetOutput());
  return CV_OK;
}

// --------------------
//  VMTKUtils_InsertIds
// --------------------
/** 
 * @brief Function to attach GlobalElementIDs and GlobalNodeIDs to the 
 * final boundary layer mesh. The original values are destroyed by the 
 * complex process, so the values are reassigned.
 * @param fullmesh This is the full volumetric mesh output by TetGen, VMTK, 
 * and custom procedures. 
 * @param fullpolydata This should be empty coming in. It is then the 
 * extracted surface from the fullmesh.
 * @return CV_OK if the Ids are attached to the surfaces without issue 
 */

int VMTKUtils_InsertIds(vtkUnstructuredGrid *fullmesh, vtkPolyData *fullpolydata)
{ 
  int k;
  int modelId = 1;
  int globalId = 1;
  int numNewCells,numStartCells;
  double pt1[3];
  double pt2[3];
  double pt3[3];
  double pt4[3];
  double a[3];
  double b[3];
  double c[3];
  double norm[3];
  double mydot;
  double tmp;

  vtkIdType i,j;
  vtkIdType vtkId;
  vtkIdType count=0;
  vtkIdType npts = 0;
  vtkIdType *pts = 0;
  vtkIdType numPts,numCells; 
  vtkSmartPointer<vtkIntArray> modelRegionIds = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> globalNodeIds = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> globalElementIds = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> isTriangle = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkUnstructuredGrid> fullcopy = 
    vtkSmartPointer<vtkUnstructuredGrid>::New();
  vtkSmartPointer<vtkDataSetSurfaceFilter> getSurface = 
    vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
  vtkSmartPointer<vtkThreshold> thresholder = 
    vtkSmartPointer<vtkThreshold>::New();

  fullcopy->DeepCopy(fullmesh);
  numPts = fullcopy->GetNumberOfPoints();
  numCells = fullcopy->GetNumberOfCells();


  //Remove all triangle Cells! These are triangles on surface of mesh 
  //left from the append filter
  isTriangle->SetNumberOfComponents(1);
  isTriangle->Allocate(numCells,1000);
  isTriangle->SetNumberOfTuples(numCells);
  isTriangle->SetName("isTriangle");
  for (i=0;i<numCells;i++)
  {
    if (fullcopy->GetCellType(i) == VTK_TRIANGLE)
    {
      isTriangle->InsertTuple1(i,1);
    }
    else
    {
      isTriangle->InsertTuple1(i,0);
    }
  }
  fullcopy->GetCellData()->AddArray(isTriangle);
  fullcopy->GetCellData()->SetActiveScalars("isTriangle");

  thresholder->SetInputData(fullcopy);
  thresholder->SetInputArrayToProcess(0,0,0,1,"isTriangle");
  thresholder->ThresholdBetween(0,0);
  thresholder->Update();

  fullcopy->DeepCopy(thresholder->GetOutput());
  fullcopy->GetCellData()->RemoveArray("isTriangle");
  numPts = fullcopy->GetNumberOfPoints();
  numCells = fullcopy->GetNumberOfCells();
  
  //Add global node Ids to points
  for (i=0;i<numPts;i++)
  {
    globalNodeIds->InsertValue(i,globalId);
    globalId++;
  }

  //Add global element Ids to cells
  globalId = 1;
  for (i=0;i<numCells;i++)
  {
    globalElementIds->InsertValue(i,globalId);
    modelRegionIds->InsertValue(i,modelId);
    globalId++;
  }

  //Make Global Ids active scalars
  modelRegionIds->SetName("ModelRegionID");
  fullcopy->GetCellData()->AddArray(modelRegionIds);
  fullcopy->GetCellData()->SetActiveScalars("ModelRegionID");

  globalNodeIds->SetName("GlobalNodeID");
  fullcopy->GetPointData()->AddArray(globalNodeIds);
  fullcopy->GetPointData()->SetActiveScalars("GlobalNodeID");

  globalElementIds->SetName("GlobalElementID");
  fullcopy->GetCellData()->AddArray(globalElementIds);
  fullcopy->GetCellData()->SetActiveScalars("GlobalElementID");

  //This is the full mesh with scalars
  fullmesh->DeepCopy(fullcopy);

  //Flip the cells within the volume if nodes are numbered wrong
  for (i=0;i<fullmesh->GetNumberOfCells();i++)
  {
    fullmesh->GetCellPoints(i,npts,pts);
    fullmesh->GetPoint(pts[0],pt1);
    fullmesh->GetPoint(pts[1],pt2);
    fullmesh->GetPoint(pts[2],pt3);
    fullmesh->GetPoint(pts[3],pt4);

    for (k=0;k<3;k++)
    {
      a[k] = pt2[k]-pt1[k];
      b[k] = pt3[k]-pt1[k];
      c[k] = pt4[k]-pt1[k];
    }

    vtkMath::Cross(a,b,norm);
    mydot = vtkMath::Dot(norm,c);

    if (mydot < 0)
    {
      tmp = pts[1];
      pts[1] = pts[2];
      pts[2] = tmp;
      fullmesh->ReplaceCell(i,npts,pts);
    }
  }

  getSurface->SetInputData(fullcopy);
  getSurface->Update();

  fullpolydata->DeepCopy(getSurface->GetOutput());

  return CV_OK;
}

