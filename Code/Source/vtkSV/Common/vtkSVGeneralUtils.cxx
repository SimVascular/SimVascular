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

/**
 *  \brief A compilation of general useful functions used throughout code base
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#include "vtkSVGeneralUtils.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCenterOfMass.h"
#include "vtkClipPolyData.h"
#include "vtkCubeSource.h"
#include "vtkCylinderSource.h"
#include "vtkConnectivityFilter.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkExtractGeometry.h"
#include "vtkIdFilter.h"
#include "vtkIdList.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPlaneSource.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkPolyDataNormals.h"
#include "vtkSmartPointer.h"
#include "vtkSortDataArray.h"
#include "vtkThreshold.h"
#include "vtkTransform.h"
#include "vtkTransformFilter.h"
#include "vtkTransformPolyDataFilter.h"
#include "vtkTriangle.h"
#include "vtkTriangleFilter.h"
#include "vtkDataSet.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"

#include <algorithm>
#include <iterator>

// ----------------------
// MakePlane
// ----------------------
int vtkSVGeneralUtils::MakePlane(double pt0[3], double pt1[3], double pt2[3],
                                 int res0, int res1, int triangulate, vtkPolyData *pd)
{
  // Set up plane source
  vtkNew(vtkPlaneSource, makePlane);
  makePlane->SetOrigin(pt0);
  makePlane->SetPoint1(pt1);
  makePlane->SetPoint2(pt2);
  makePlane->SetResolution(res0, res1);
  makePlane->Update();

  // triangulate if asked to
  if (triangulate)
  {
    vtkNew(vtkTriangleFilter, triangulator);
    triangulator->SetInputData(makePlane->GetOutput());
    triangulator->Update();

    // copy output
    pd->DeepCopy(triangulator->GetOutput());
  }
  else
    pd->DeepCopy(makePlane->GetOutput());

  return SV_OK;
}

// ----------------------
// MakeCylinder
// ----------------------
int vtkSVGeneralUtils::MakeCylinder(double r, double length,
                                    double resolution, double center[3],
                                    double axis[3], int triangulate,
                                    vtkPolyData *pd)
{
  // Set up cylinder source
  vtkNew(vtkCylinderSource, cylinder);
  cylinder->SetCenter(0.0,0.0,0.0);
  cylinder->SetHeight(length);
  cylinder->SetRadius(r);
  cylinder->SetResolution(resolution);
  cylinder->Update();

  // Set up transofrm to rotate given axis
  double vec[3]; vec[0] = 0.0; vec[1] = 1.0; vec[2] = 0.0;
  double rotateaxis[3]; vtkMath::Cross(axis,vec,rotateaxis);

  // Temporary vector
  double tmpcross[3];
  vtkMath::Cross(axis,vec,tmpcross);

  // Compute angle in radians and degrees
  double radangle = atan2(vtkMath::Norm(tmpcross), vtkMath::Dot(axis,vec));
  double degangle = vtkMath::DegreesFromRadians(radangle);

  // Transform
  vtkNew(vtkTransform, transformer);
  transformer->RotateWXYZ(degangle,rotateaxis);
  transformer->Translate(center[0],center[1],center[2]);

  // Transformer
  vtkNew(vtkTransformPolyDataFilter, polyDataTransformer);
  polyDataTransformer->SetInputData(cylinder->GetOutput());
  polyDataTransformer->SetTransform(transformer);
  polyDataTransformer->Update();

  // Triangulate
  if (triangulate)
  {
    vtkNew(vtkTriangleFilter, triangulator);
    triangulator->SetInputData(polyDataTransformer->GetOutput());
    triangulator->Update();
    pd->DeepCopy(triangulator->GetOutput());
  }
  else
    pd->DeepCopy(polyDataTransformer->GetOutput());

  return SV_OK;
}

// ----------------------
// MakeCube
// ----------------------
int vtkSVGeneralUtils::MakeCube(double dims[3], double center[3],
                                int triangulate, vtkPolyData *pd)
{
  vtkNew(vtkCubeSource, cube);
  cube->SetCenter(center[0], center[1], center[2]);
  cube->SetXLength(dims[0]);
  cube->SetYLength(dims[1]);
  cube->SetZLength(dims[2]);
  cube->Update();

  if (triangulate)
  {
    vtkNew(vtkTriangleFilter, triangulator);
    triangulator->SetInputData(cube->GetOutput());
    triangulator->Update();
    pd->DeepCopy(triangulator->GetOutput());
  }
  else
    pd->DeepCopy(cube->GetOutput());

  return SV_OK;
}

// ----------------------
// CheckArrayExists
// ----------------------
int vtkSVGeneralUtils::CheckArrayExists(vtkDataSet *ds,
                                        int datatype,
                                        std::string arrayname)
{
  int exists =0;

  if (datatype == 0)
  {
    int numArrays = ds->GetPointData()->GetNumberOfArrays();
    for (int i=0;i<numArrays;i++)
    {
      if (!strcmp(ds->GetPointData()->GetArrayName(i),arrayname.c_str()))
      {
	      exists =1;
      }
    }
  }
  else
  {
    int numArrays = ds->GetCellData()->GetNumberOfArrays();
    for (int i=0;i<numArrays;i++)
    {
      if (!strcmp(ds->GetCellData()->GetArrayName(i),arrayname.c_str()))
      {
	      exists =1;
      }
    }
  }

  return exists;
}

// ----------------------
// GetEdgePolyData
// ----------------------
int vtkSVGeneralUtils::GetEdgePolyData(vtkPolyData *pd, vtkPolyData *edgePd)
{
  // ------------------------------------------------------------------------
  // Start edge insertion for edge table
  int numCells = pd->GetNumberOfCells();
  int numPts   = pd->GetNumberOfPoints();

  vtkNew(vtkEdgeTable, edgeTable);
  edgeTable->InitEdgeInsertion(numPts, 1);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Loop through cells
  vtkNew(vtkIdList, neighborCellIds);
  int totEdges = 0;
  for (int i=0; i<numCells; i++)
  {
    // Get cellpoints
    vtkIdType npts, *pts;
    pd->GetCellPoints(i, npts, pts);
    for (int j=0; j<npts; j++)
    {
      // Get each edge of cell
      vtkIdType p0 = pts[j];
      vtkIdType p1 = pts[(j+1)%npts];

      pd->GetCellEdgeNeighbors(i, p0, p1, neighborCellIds);
      vtkIdType neighborCellId = 0;

      // Check to see if it is a boundary edge
      if (neighborCellIds->GetNumberOfIds() > 0)
        neighborCellId = neighborCellIds->GetId(0);
      else
      {
        neighborCellId = -1;
      }

      // Check to see if edge has already been inserted
      vtkIdType checkEdge = edgeTable->IsEdge(p0, p1);
      if (checkEdge == -1)
      {
        totEdges++;
        // Get new edge id and insert into table
        vtkIdType edgeId = edgeTable->InsertEdge(p0, p1);
      }
    }
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Now make polydata from edge table
  vtkNew(vtkCellArray, edgeCells);
  vtkNew(vtkIdList, newEdgeCell);
  edgeTable->InitTraversal();
  int edgeId = 0;
  for (edgeId = 0; edgeId < totEdges;  edgeId++)
  {
    vtkIdType edgePtId0, edgePtId1;
    edgeTable->GetNextEdge(edgePtId0, edgePtId1);

    newEdgeCell->Reset();
    newEdgeCell->SetNumberOfIds(2);
    newEdgeCell->SetId(0, edgePtId0);
    newEdgeCell->SetId(1, edgePtId1);

    edgeCells->InsertNextCell(newEdgeCell);
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Set cells and points
  edgePd->SetLines(edgeCells);
  edgePd->SetPoints(pd->GetPoints());
  edgePd->BuildLinks();
  // ------------------------------------------------------------------------

  return SV_OK;
}

// ----------------------
// CheckSurface
// ----------------------
int vtkSVGeneralUtils::CheckSurface(vtkPolyData *pd)
{
  pd->BuildLinks();

  int numPts = pd->GetNumberOfPoints();
  int numPolys = pd->GetNumberOfCells();

  for (int i=0; i<numPolys; i++)
  {
    vtkIdType npts, *pts;
    pd->GetCellPoints(i, npts, pts);
    if (npts != 3)
    {
      return SV_ERROR;
    }
    for (int j=0; j<npts; j++)
    {
      vtkIdType p0, p1;
      p0 = pts[j];
      p1 = pts[(j+1)%npts];

      vtkNew(vtkIdList, edgeNeighbor);
      pd->GetCellEdgeNeighbors(i, p0, p1, edgeNeighbor);

      if (edgeNeighbor->GetNumberOfIds() > 1)
      {
        return SV_ERROR;
      }
    }
  }
  return SV_OK;
}

// ----------------------
// CheckSurface
// ----------------------
int vtkSVGeneralUtils::CheckSurface(vtkPolyData *pd,
                                    int &numNonTriangleCells,
                                    int &numNonManifoldEdges,
                                    int &numOpenEdges,
                                    int &surfaceGenus)
{
  pd->BuildLinks();

  int numPts = pd->GetNumberOfPoints();
  int numPolys = pd->GetNumberOfCells();

  // Start edge insertion for edge table
  vtkNew(vtkEdgeTable, surfaceEdgeTable);
  surfaceEdgeTable->InitEdgeInsertion(numPts, 1);

  numOpenEdges        = 0;
  numNonTriangleCells = 0;
  numNonManifoldEdges = 0;
  for (int i=0; i<numPolys; i++)
  {
    vtkIdType npts, *pts;
    pd->GetCellPoints(i, npts, pts);
    if (npts != 3)
    {
      numNonTriangleCells++;
    }
    for (int j=0; j<npts; j++)
    {
      vtkIdType p0, p1;
      p0 = pts[j];
      p1 = pts[(j+1)%npts];

      vtkNew(vtkIdList, edgeNeighbor);
      pd->GetCellEdgeNeighbors(i, p0, p1, edgeNeighbor);

      if (edgeNeighbor->GetNumberOfIds() == 0)
      {
        numOpenEdges++;
      }
      if (edgeNeighbor->GetNumberOfIds() > 1)
      {
        numNonManifoldEdges++;
      }

      // Check to see if edge has already been inserted
      vtkIdType checkEdge = surfaceEdgeTable->IsEdge(p0, p1);
      if (checkEdge == -1)
      {
        // Get new edge id and insert into table
        vtkIdType edgeId = surfaceEdgeTable->InsertEdge(p0, p1);
      }
    }
  }

  int ne = surfaceEdgeTable->GetNumberOfEdges();
  int nv = numPts;
  int nf = numPolys;

  surfaceGenus = ((ne - nv - nf)/2) + 1;

  return SV_OK;
}


// ----------------------
// GetClosestPointConnectedRegion
// ----------------------
int vtkSVGeneralUtils::GetClosestPointConnectedRegion(vtkPolyData *pd,
                                                      double pt[3])
{
  // Connectivity filter
  vtkNew(vtkConnectivityFilter, connector);
  connector->SetInputData(pd);
  connector->SetExtractionModeToClosestPointRegion();
  connector->SetClosestPoint(pt);
  connector->Update();

  // Convert unstructured grid to polydata
  vtkNew(vtkDataSetSurfaceFilter, surfacer);
  surfacer->SetInputData(connector->GetOutput());
  surfacer->Update();

  // Copy the result
  pd->DeepCopy(surfacer->GetOutput());

  return SV_OK;
}

// ----------------------
// GetClosestPointConnectedRegion
// ----------------------
int vtkSVGeneralUtils::GetClosestPointConnectedRegion(vtkPolyData *inPd,
                                                      double pt[3],
                                                      vtkPolyData *outPd)
{
  // Simple, call same function
  outPd->DeepCopy(inPd);
  vtkSVGeneralUtils::GetClosestPointConnectedRegion(outPd, pt);

  return SV_OK;
}

// ----------------------
// GiveIds
// ----------------------
int vtkSVGeneralUtils::GiveIds(vtkPolyData *pd,
                               std::string arrayName)
{
  // Send through Id filter
  vtkNew(vtkIdFilter, ider);
  ider->SetInputData(pd);
  ider->SetIdsArrayName(arrayName.c_str());
  ider->Update();

  pd->DeepCopy(ider->GetOutput());

  return SV_OK;
}

// ----------------------
// GiveIds
// ----------------------
int vtkSVGeneralUtils::GiveIds(vtkPolyData *inPd,
                               std::string arrayName,
                               vtkPolyData *outPd)
{
  // Simple, call same function
  outPd->DeepCopy(inPd);
  vtkSVGeneralUtils::GiveIds(outPd, arrayName);

  return SV_OK;
}

// ----------------------
// IteratePoint
// ----------------------
int vtkSVGeneralUtils::IteratePoint(vtkPolyData *pd, int &pointId, int &prevCellId)
{
  // Get given point cells
  vtkNew(vtkIdList, ptCellIds);
  pd->GetPointCells(pointId, ptCellIds);

  // Get next cell which isnt prevCellId
  int cellId;
  if (ptCellIds->GetId(0) == prevCellId)
    cellId = ptCellIds->GetId(1);
  else
    cellId = ptCellIds->GetId(0);

  // Set prevCellId now to and get next point
  prevCellId = cellId;

  // Get cell points
  vtkIdType npts, *pts;
  pd->GetCellPoints(prevCellId, npts, pts);

  // Get next point which isnt the pointId we started with
  int newId;
  if (pts[0] == pointId)
    newId = pts[1];
  else
    newId = pts[0];

  // Set the pointId to this new Id
  pointId = newId;

  return SV_OK;
}

// ----------------------
// ThresholdPd
// ----------------------
int vtkSVGeneralUtils::ThresholdPd(vtkPolyData *pd, int minVal,
                                   int maxVal, int dataType,
                                   std::string arrayName)
{
  // Set up threshold filter
  vtkNew(vtkThreshold, thresholder);
  thresholder->SetInputData(pd);
  //Set Input Array to 0 port,0 connection, dataType (0 - point, 1 - cell, and Regions is the type name
  thresholder->SetInputArrayToProcess(0, 0, 0, dataType, arrayName.c_str());
  thresholder->ThresholdBetween(minVal, maxVal);
  thresholder->Update();

  // Check to see if the result has points, don't run surface filter
  if (thresholder->GetOutput()->GetNumberOfPoints() == 0)
    return SV_ERROR;

  // Convert unstructured grid to polydata
  vtkNew(vtkDataSetSurfaceFilter, surfacer);
  surfacer->SetInputData(thresholder->GetOutput());
  surfacer->Update();

  // Set the final pd
  pd->DeepCopy(surfacer->GetOutput());

  return SV_OK;
}

// ----------------------
// ThresholdPd
// ----------------------
int vtkSVGeneralUtils::ThresholdPd(vtkPolyData *pd, int minVal,
                                   int maxVal, int dataType,
                                   std::string arrayName,
                                   vtkPolyData *returnPd)
{
  // Simple, call the other implementation
  returnPd->DeepCopy(pd);
  return vtkSVGeneralUtils::ThresholdPd(returnPd, minVal, maxVal, dataType,
                                        arrayName);
}

// ----------------------
// ThresholdUg
// ----------------------
int vtkSVGeneralUtils::ThresholdUg(vtkUnstructuredGrid *ug, int minVal,
                                   int maxVal, int dataType,
                                   std::string arrayName)
{
  // Set up threshold filter
  vtkNew(vtkThreshold, thresholder);
  thresholder->SetInputData(ug);
  //Set Input Array to 0 port,0 connection, dataType (0 - point, 1 - cell, and Regions is the type name
  thresholder->SetInputArrayToProcess(0, 0, 0, dataType, arrayName.c_str());
  thresholder->ThresholdBetween(minVal, maxVal);
  thresholder->Update();

  // Check to see if the result has points, don't run surface filter
  if (thresholder->GetOutput()->GetNumberOfPoints() == 0)
    return SV_ERROR;

  // Set the final ug
  ug->DeepCopy(thresholder->GetOutput());

  return SV_OK;
}

// ----------------------
// ThresholdUg
// ----------------------
int vtkSVGeneralUtils::ThresholdUg(vtkUnstructuredGrid *ug, int minVal,
                                   int maxVal, int dataType,
                                   std::string arrayName,
                                   vtkUnstructuredGrid *returnUg)
{
  // Simple, call the other implementation
  returnUg->DeepCopy(ug);
  return vtkSVGeneralUtils::ThresholdUg(returnUg, minVal, maxVal, dataType,
                                        arrayName);
}


// ----------------------
// GetCentroidOfPoints
// ----------------------
int vtkSVGeneralUtils::GetCentroidOfPoints(vtkPoints *points,
                                           double centroid[3])
{
  // Number of points
  int numPoints = points->GetNumberOfPoints();
  centroid[0] = 0.0; centroid[1] = 0.0; centroid[2] = 0.0;

  // Loop through points
  for (int i=0; i<numPoints; i++)
  {
    // Get point
    double pt[3];
    points->GetPoint(i, pt);

    //Update centroid
    for (int j=0; j<3; j++)
      centroid[j] += pt[j];
  }

  // Divide by the number of points to get centroid
  vtkMath::MultiplyScalar(centroid, 1.0/numPoints);

  return SV_OK;
}


// ----------------------
// GetPointCellsValues
// ----------------------
/** \details The value is not added to the list of values if it is -1 */
int vtkSVGeneralUtils::GetPointCellsValues(vtkPointSet *ps, std::string arrayName,
                                           const int pointId, vtkIdList *valList)
{
  // Get data from pd
  vtkDataArray *valArray =
    ps->GetCellData()->GetArray(arrayName.c_str());
  valList->Reset();

  // Get point cells
  vtkNew(vtkIdList, cellIds);
  ps->GetPointCells(pointId, cellIds);

  // Loop through and check each point
  for (int i=0; i<cellIds->GetNumberOfIds(); i++)
  {
    int value = valArray->GetTuple1(cellIds->GetId(i));

    // Only adding to list if value is not -1
    if (valList->IsId(value) == -1)
      valList->InsertNextId(value);
  }

  return SV_OK;
}

// ----------------------
// GetNeighborCellsValues
// ----------------------
/** \details The value is not added to the list of values if it is -1 */
int vtkSVGeneralUtils::GetNeighborsCellsValues(vtkPolyData *pd, std::string arrayName,
                                               const int cellId, vtkIdList *valList)
{
  // Get data from pd
  vtkDataArray *valArray =
    pd->GetCellData()->GetArray(arrayName.c_str());
  valList->Reset();

  // Get cell points
  vtkIdType npts, *pts;
  pd->GetCellPoints(cellId, npts, pts);

  // Loop through points
  for (int i=0; i<npts; i++)
  {
    int ptId0 = pts[i];
    int ptId1 = pts[(i+1)%npts];

    vtkNew(vtkIdList, cellEdgeNeighbors);
    pd->GetCellEdgeNeighbors(cellId, ptId0, ptId1, cellEdgeNeighbors);

    // Loop through and check each point
    for (int j=0; j<cellEdgeNeighbors->GetNumberOfIds(); j++)
    {
      int value = valArray->GetTuple1(cellEdgeNeighbors->GetId(j));

      // Only adding to list if value is not -1
      if (valList->IsId(value) == -1)
        valList->InsertNextId(value);
    }

  }

  return SV_OK;
}

// ----------------------
// ExtractionCut
// ----------------------
int vtkSVGeneralUtils::ExtractionCut(vtkPolyData *inPd, vtkImplicitFunction *cutFunction,
                                     const int extractBoundaryCells,
                                     const int extractInside,
                                     vtkPolyData *outPd)
{
  // Set up vtkExtractGeometry filter
  vtkNew(vtkExtractGeometry, cutter);
  cutter->SetInputData(inPd);
  cutter->SetImplicitFunction(cutFunction);
  cutter->SetExtractBoundaryCells(extractBoundaryCells);
  cutter->ExtractOnlyBoundaryCellsOff();
  cutter->SetExtractInside(extractInside);
  cutter->Update();

  // Convert unstructured grid to polydata
  vtkNew(vtkDataSetSurfaceFilter, surfacer);
  surfacer->SetInputData(cutter->GetOutput());
  surfacer->Update();

  // Copy output
  outPd->DeepCopy(surfacer->GetOutput());

  return SV_OK;
}

// ----------------------
// ClipCut
// ----------------------
int vtkSVGeneralUtils::ClipCut(vtkPolyData *inPd, vtkImplicitFunction *cutFunction,
                               const int generateClippedOutput,
                               const int extractInside,
                               vtkPolyData *outPd,
                               vtkPolyData *clippedOutPd)
{
  // Set up vtkClipPolyData
  vtkNew(vtkClipPolyData, cutter);
  cutter->SetInputData(inPd);
  cutter->SetClipFunction(cutFunction);
  cutter->SetInsideOut(extractInside);
  cutter->SetGenerateClippedOutput(generateClippedOutput);
  cutter->Update();

  // This clips through cells, need to retriangulate
  vtkNew(vtkTriangleFilter, triangulator);
  triangulator->SetInputData(cutter->GetOutput(0));
  triangulator->Update();

  // Copy output
  outPd->DeepCopy(triangulator->GetOutput());

  // If we are generating clipped output, then get itj
  if (generateClippedOutput)
  {
    // Triangulate clipped output
    triangulator->SetInputData(cutter->GetOutput(1));
    triangulator->Update();

    // Only if pd is provided to we copy output
    if (clippedOutPd != NULL)
      clippedOutPd->DeepCopy(triangulator->GetOutput());
  }

  return SV_OK;
}

// ----------------------
// GetPointsLength
// ----------------------
double vtkSVGeneralUtils::GetPointsLength(vtkPolyData *pd)
{
  int numPts = pd->GetNumberOfPoints();

  double length = 0.0;

  for (int i=1; i<numPts; i++)
  {
    double pt0[3], pt1[3];
    pd->GetPoint(i-1, pt0);
    pd->GetPoint(i, pt1);

    length += vtkSVMathUtils::Distance(pt0, pt1);
  }

  return length;
}

// ----------------------
// ReplaceDataOnCells
// ----------------------
int vtkSVGeneralUtils::ReplaceDataOnCells(vtkPointSet *pointset,
                                                      vtkDataArray *sliceIds,
                                                      const int sliceId,
                                                      const int replaceVal,
                                                      const std::string &arrName)
{
  // Get number of cells in dataset
  int numCells = pointset->GetNumberOfCells();

  // Get the designated array
  if (vtkSVGeneralUtils::CheckArrayExists(pointset, 1, arrName) != SV_OK)
    return SV_ERROR;
  vtkDataArray *cellIds = pointset->GetCellData()->GetArray(arrName.c_str());

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    int cellId = cellIds->GetTuple1(i);
    int currVal = sliceIds->GetTuple1(cellId);
    // We only replace the value if it equal to the designated value
    if (currVal == replaceVal)
      sliceIds->SetTuple1(cellId, sliceId);
  }

  return SV_OK;
}

// ----------------------
// ReplaceDataOnCells
// ----------------------
int vtkSVGeneralUtils::ReplaceDataOnCells(vtkPointSet *pointset,
                                          const int replaceVal,
                                          const int currVal,
                                          const std::string &arrName)
{
  // Get number of cells in dataset
  int numCells = pointset->GetNumberOfCells();

  // Get the designated array
  vtkIntArray *cellIds = vtkIntArray::SafeDownCast(pointset->GetCellData()->GetArray(arrName.c_str()));

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    int val = cellIds->GetValue(i);
    // We only replace the value if it equal to the designated value
    if (val == currVal)
    {
      cellIds->SetValue(i, replaceVal);
    }
  }

  return SV_OK;
}

// ----------------------
// GetCutPlane
// ----------------------
int vtkSVGeneralUtils::GetCutPlane(double endPt[3], double startPt[3],
                                   vtkPlane *cutPlane)
{
  // Get normal from end pt and start pt
  double normal[3];

  // Get normal vector
  vtkMath::Subtract(endPt, startPt, normal);
  vtkMath::Normalize(normal);

  // Set up plane
  cutPlane->SetOrigin(endPt);
  cutPlane->SetNormal(normal);

  return SV_OK;
}

// ----------------------
// ComputeMassCenter
// ----------------------
int vtkSVGeneralUtils::ComputeMassCenter(vtkPolyData *pd, double massCenter[3])
{
  // Initialize to zero
  massCenter[0] = 0.0; massCenter[1] = 0.0; massCenter[2] = 0.0;

  // Set up vtk filter
  vtkNew(vtkCenterOfMass, centerFinder);
  centerFinder->SetInputData(pd);
  centerFinder->Update();
  centerFinder->GetCenter(massCenter);

  return SV_OK;
}

// ----------------------
// GetBarycentricCoordinates
// ----------------------
int vtkSVGeneralUtils::GetBarycentricCoordinates(double f[3], double pt0[3],
                                                 double pt1[3], double pt2[3],
					                                       double &a0, double &a1, double &a2)
{
  // Get the vectors for the edges of the triangle and f to each corner
  double f0[3], f1[3], f2[3], v0[3], v1[3];
  for (int i=0; i<3; i++)
  {
    v0[i] = pt0[i] - pt1[i];
    v1[i] = pt0[i] - pt2[i];
    f0[i] = pt0[i] - f[i];
    f1[i] = pt1[i] - f[i];
    f2[i] = pt2[i] - f[i];
  }

  // Compute the full area, vArea, and the areas of each sub-triangle created with f
  double vArea[3], vA0[3], vA1[3], vA2[3];
  vtkMath::Cross(v0, v1, vArea);
  vtkMath::Cross(f1, f2, vA0);
  vtkMath::Cross(f2, f0, vA1);
  vtkMath::Cross(f0, f1, vA2);

  // Compute the scalar coordinates dividing each sub-triangle by the full area
  double area = vtkMath::Norm(vArea);
  a0 = vtkMath::Norm(vA0)/area;// * Sign(vtkMath::Dot(vArea, vA0));
  a1 = vtkMath::Norm(vA1)/area;// * Sign(vtkMath::Dot(vArea, vA1));
  a2 = vtkMath::Norm(vA2)/area;// * Sign(vtkMath::Dot(vArea, vA2));
  return SV_OK;
}

// ----------------------
// ComputeParametricDerivatives
// ----------------------
int vtkSVGeneralUtils::ComputeParametricDerivatives(double pt0[3], double pt1[3], double pt2[3],
                                                    double pPt0[3], double pPt1[3], double pPt2[3],
                                                    double dXdXi, double dXdEta,
                                                    double dYdXi, double dYdEta,
                                                    double dZdXi, double dZdEta)
{
  double area = vtkTriangle::TriangleArea(pPt0, pPt1, pPt2);

  dXdXi = (1./2) * ((pPt0[1] - pPt1[1]) * pt2[0] +
                    (pPt1[1] - pPt2[1]) * pt0[0] +
                    (pPt1[1] - pPt0[1]) * pt1[0]) / area;
  dXdEta = (1./2) * ((pPt0[0] - pPt1[0]) * pt2[0] +
                    (pPt1[0] - pPt2[0]) * pt0[0] +
                    (pPt1[0] - pPt0[0]) * pt1[0]) / area;

  dYdXi = (1./2) * ((pPt0[1] - pPt1[1]) * pt2[1] +
                    (pPt1[1] - pPt2[1]) * pt0[1] +
                    (pPt1[1] - pPt0[1]) * pt1[1]) / area;
  dYdEta = (1./2) * ((pPt0[0] - pPt1[0]) * pt2[1] +
                    (pPt1[0] - pPt2[0]) * pt0[1] +
                    (pPt1[0] - pPt0[0]) * pt1[1]) / area;

  dZdXi = (1./2) * ((pPt0[1] - pPt1[1]) * pt2[2] +
                    (pPt1[1] - pPt2[1]) * pt0[2] +
                    (pPt1[1] - pPt0[1]) * pt1[2]) / area;
  dZdEta = (1./2) * ((pPt0[0] - pPt1[0]) * pt2[2] +
                    (pPt1[0] - pPt2[0]) * pt0[2] +
                    (pPt1[0] - pPt0[0]) * pt1[2]) / area;

  return SV_OK;
}

// ----------------------
// ComputeJacobianDerivatives
// ----------------------
int vtkSVGeneralUtils::ComputeJacobianDerivatives(double pt0[3], double pt1[3], double pt2[3],
                                                  double pPt0[3], double pPt1[3], double pPt2[3],
                                                  double dXdXi, double dXdEta,
                                                  double dYdXi, double dYdEta,
                                                  double dZdXi, double dZdEta)
{
  double area = vtkTriangle::TriangleArea(pPt0, pPt1, pPt2);

  dXdXi = (1./2) * ((pPt0[1] - pPt1[1]) * pt2[0] +
                    (pPt1[1] - pPt2[1]) * pt0[0] +
                    (pPt1[1] - pPt0[1]) * pt1[0]) / area;
  dXdEta = (1./2) * ((pPt0[0] - pPt1[0]) * pt2[0] +
                    (pPt1[0] - pPt2[0]) * pt0[0] +
                    (pPt1[0] - pPt0[0]) * pt1[0]) / area;

  dYdXi = (1./2) * ((pPt0[1] - pPt1[1]) * pt2[1] +
                    (pPt1[1] - pPt2[1]) * pt0[1] +
                    (pPt1[1] - pPt0[1]) * pt1[1]) / area;
  dYdEta = (1./2) * ((pPt0[0] - pPt1[0]) * pt2[1] +
                    (pPt1[0] - pPt2[0]) * pt0[1] +
                    (pPt1[0] - pPt0[0]) * pt1[1]) / area;

  dZdXi = (1./2) * ((pPt0[1] - pPt1[1]) * pt2[2] +
                    (pPt1[1] - pPt2[1]) * pt0[2] +
                    (pPt1[1] - pPt0[1]) * pt1[2]) / area;
  dZdEta = (1./2) * ((pPt0[0] - pPt1[0]) * pt2[2] +
                    (pPt1[0] - pPt2[0]) * pt0[2] +
                    (pPt1[0] - pPt0[0]) * pt1[2]) / area;

  return SV_OK;
}

// ----------------------
// GetParametricPoints
// ----------------------
int vtkSVGeneralUtils::GetParametricPoints(double pt0[3], double pt1[3], double pt2[3],
                                           double pPt0[3], double pPt1[3], double pPt2[3])
{
  // GIVE THE CORRECT RESULT IF WE NEED TO USE
  double l0 = vtkSVMathUtils::Distance(pt0, pt1);
  double l1 = vtkSVMathUtils::Distance(pt1, pt2);
  double l2 = vtkSVMathUtils::Distance(pt2, pt0);

  double x = (pow(l2, 2.0) -
              pow(l1, 2.0) +
              pow(l0, 2.0))/ (2 * l0);
  double y = std::sqrt( pow(l2, 2.0) - pow(x, 2.0));

  pPt0[0] = 0.0;
  pPt0[1] = 0.0;
  pPt0[2] = 0.0;

  pPt1[0] = l0;
  pPt1[1] = 0.0;
  pPt1[2] = 0.0;

  pPt2[0] = x;
  pPt2[1] = y;
  pPt2[2] = 0.0;

  //vtkSVGeneralUtils::TransformTriangleToXYPlane(pt0, pt1, pt2,
  //                                              pPt0, pPt1, pPt2);

  return SV_OK;
}


// ----------------------
// GetPointNeighbors
// ----------------------
int vtkSVGeneralUtils::GetPointNeighbors(vtkIdType p0,
                                       vtkPolyData *pd,
						                           vtkIdList *pointNeighbors)
{
  //Assuming that pointNeighbors is set with no neighbors already
  vtkNew(vtkIdList, cellIdList);
  pd->GetPointCells(p0, cellIdList);

  // Loop through list of cell neighbors
  for (int i=0; i<cellIdList->GetNumberOfIds(); i++)
  {
    vtkIdType cellId = cellIdList->GetId(i);

    // Get points of touching cell
    vtkIdType npts, *pts;
    pd->GetCellPoints(cellId, npts, pts);

    // Loop through neighbor cell points
    for (int j=0; j<npts; j++)
    {
      // If neighboring point isnt the one we are working with and we
      // haven't added it already, we add to list
      vtkIdType neighborPoint = pts[j];
      if (neighborPoint != p0)
      {
        pointNeighbors->InsertUniqueId(neighborPoint);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// GetEdgeCotangentAngle
// ----------------------
int vtkSVGeneralUtils::GetEdgeCotangentAngle(double pt0[3], double pt1[3],
                                           double pt2[3], double &angle)
{
  // Get the area of the three points
  double area = vtkSVMathUtils::ComputeTriangleArea(pt0, pt1, pt2);

  // If the area is negative, we switch the edge points so that they are in
  // CCW order
  if (area < 0)
  {
    double tmpPoint[3];
    for (int i=0; i<3; i++)
    {
      tmpPoint[i] = pt0[i];
      pt0[i] = pt1[i];
      pt1[i] = tmpPoint[i];
    }
  }

  // Compute the vector between each point in the edge and the third point
  double vec0[3];
  double vec1[3];
  for (int i=0; i<3; i++)
  {
    vec0[i] = pt0[i] - pt2[i];
    vec1[i] = pt1[i] - pt2[i];
  }

  // Get the angle between the two vectors
  double numerator = vtkMath::Dot(vec0, vec1);
  double cross[3];
  vtkMath::Cross(vec0, vec1, cross);
  double denominator = vtkMath::Norm(cross);
  angle = numerator/denominator;

  return SV_OK;
}

// ----------------------
// CreateEdgeTable
// ----------------------
/**
 * \details Adds information about boundary points, edge weights using
 * harmonic edge weights, and edge neighbors.
 */
int vtkSVGeneralUtils::CreateEdgeTable(vtkPolyData *pd,
                                     vtkEdgeTable *edgeTable,
                                     vtkFloatArray *edgeWeights,
                                     vtkIntArray *edgeNeighbors,
                                     vtkIntArray *isBoundary)
{
  // Get number of points and cells
  int numPts = pd->GetNumberOfPoints();
  int numTris = pd->GetNumberOfCells();

  // Start edge insertion for edge table
  edgeTable->InitEdgeInsertion(numPts, 1);
  isBoundary->SetNumberOfValues(numPts);

  // Initialize the boundary array to zero
  for (int i=0; i<numPts; i++)
    isBoundary->InsertValue(i, 0);

  // Loop through cells
  for (int i=0; i<numTris; i++)
  {
    // Get cellpoints
    vtkIdType npts, *pts;
    pd->GetCellPoints(i, npts, pts);
    for (int j=0; j<npts; j++)
    {
      // Get each edge of cell
      vtkIdType p0 = pts[j];
      vtkIdType p1 = pts[(j+1)%npts];
      vtkNew(vtkIdList, neighborCellIds);
      pd->GetCellEdgeNeighbors(i, p0, p1, neighborCellIds);
      vtkIdType neighborCellId = 0;

      // Check to see if it is a boundary edge
      if (neighborCellIds->GetNumberOfIds() > 0)
        neighborCellId = neighborCellIds->GetId(0);
      else
      {
        neighborCellId = -1;
        isBoundary->InsertValue(p0, 1);
        isBoundary->InsertValue(p1, 1);
      }

      // Check to see if edge has already been inserted
      vtkIdType checkEdge = edgeTable->IsEdge(p0, p1);
      if (checkEdge == -1)
      {
        //Compute Edge Weight
        double weight = 0.0;
        vtkSVGeneralUtils::ComputeHarmonicEdgeWeight(pd, i, neighborCellId,
                                                     p0, p1, weight);

        // Get new edge id and insert into table
        vtkIdType edgeId = edgeTable->InsertEdge(p0, p1);

        // Insert edge weights and neighboring cells`
        edgeWeights->InsertValue(edgeId, weight);
        edgeNeighbors->InsertComponent(edgeId, 0, i);
        edgeNeighbors->InsertComponent(edgeId, 1, neighborCellId);
      }
    }
  }

  return SV_OK;
}


// ----------------------
// ComputeHarmonicEdgeWeight
// ----------------------
int vtkSVGeneralUtils::ComputeHarmonicEdgeWeight(vtkPolyData *pd,
                                                 vtkIdType cellId,
                                                 vtkIdType neighborCellId,
                                                 vtkIdType p0,
                                                 vtkIdType p1,
                                                 double &weight)
{
  //Add the edge weights based on the angle of the edge
  vtkIdType cellIds[2];
  cellIds[0] = cellId;
  cellIds[1] = neighborCellId;
  weight = 0.0;

  // Get the two points of the edge
  double v0[3]; double v1[3]; double v2[3];
  pd->GetPoint(p0, v0);
  pd->GetPoint(p1, v1);

  // increment through each edge neighboring cell
  for (int i=0; i<2; i++)
  {
    // If cell id != -1 (i.e. not boundary), then its on!
    vtkIdType npts, *pts;
    if (cellIds[i] != -1)
    {
      // Get Cell points
      pd->GetCellPoints(cellIds[i], npts, pts);
      for (int k=0; k<npts; k++)
      {
        // We are looking for the third point making the triangle
        if (pts[k] != p0 && pts[k] != p1)
        {
          pd->GetPoint(pts[k], v2);

          // Compute the angle the point makes with the edge
          double angle = 0.0;
          vtkSVGeneralUtils::GetEdgeCotangentAngle(v0, v1, v2, angle);

          // The weight is 1/2 the sum of the two edge angles
          weight += 0.5*angle;
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// ConvertFieldToPolyData
// ----------------------
int vtkSVGeneralUtils::ConvertFieldToPolyData(vtkPolyData *inPd, std::string fieldName, vtkPolyData *outPd)
{
  // Get number of cells and points
  int numCells = inPd->GetNumberOfCells();
  int numPts   = inPd->GetNumberOfPoints();

  // Set up new array of the right size
  vtkNew(vtkPoints, fieldPts);;
  fieldPts->SetNumberOfPoints(numPts);

  // Can keep the same cells, just need new points
  vtkNew(vtkCellArray, fieldCells);
  fieldCells = inPd->GetPolys();

  // Check to make sure array exists
  if (vtkSVGeneralUtils::CheckArrayExists(inPd, 0, fieldName) != SV_OK)
    return SV_ERROR;

  // Get data array
  vtkFloatArray *fieldArray;
  fieldArray = vtkFloatArray::SafeDownCast(
    inPd->GetPointData()->GetArray(fieldName.c_str()));

  if (fieldArray->GetNumberOfComponents() != 3)
    return SV_ERROR;

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    // Get cell points
    vtkIdType npts, *pts;
    inPd->GetCellPoints(i, npts, pts);
    for (int j=0; j<npts; j++)
    {
      // Fill in points with data array tuple
      double pt[3];
      fieldArray->GetTuple(pts[j], pt);
      fieldPts->SetPoint(pts[j], pt);
    }
  }

  // Set output polydata cells and points
  outPd->SetPolys(fieldCells);
  outPd->SetPoints(fieldPts);
  outPd->BuildLinks();

  return SV_OK;
}

// ----------------------
// ComputeNormals
// ----------------------
int vtkSVGeneralUtils::ComputeNormals(vtkPolyData *pd)
{
  // compute normals
  vtkNew(vtkPolyDataNormals, normaler);
  normaler->SetInputData(pd);
  normaler->AutoOrientNormalsOn();
  normaler->SplittingOff();
  normaler->Update();

  // Copy output
  pd->DeepCopy(normaler->GetOutput());
  pd->BuildLinks();

  return SV_OK;
}

// ----------------------
// ComputeMeshLaplacian
// ----------------------
int vtkSVGeneralUtils::ComputeMeshLaplacian(vtkPolyData *pd,
                                            vtkEdgeTable *edgeTable,
                                            vtkFloatArray *edgeWeights,
                                            vtkIntArray *edgeNeighbors,
                                            vtkFloatArray *laplacian, int map)
{
  // Get number of points
  int numPts = pd->GetNumberOfPoints();

  // Loop through points computing laplacian at each point
  for (int i=0; i<numPts; i++)
  {
    double pointLaplacian[3];
    vtkSVGeneralUtils::ComputePointLaplacian(i, pd,
                                             edgeTable,
                                             edgeWeights, edgeNeighbors,
                                             pointLaplacian, map);
    laplacian->SetTuple(i, pointLaplacian);
  }

  return SV_OK;
}

// ----------------------
// ComputePointLaplacian
// ----------------------
int vtkSVGeneralUtils::ComputePointLaplacian(vtkIdType p0,
                                             vtkPolyData *pd,
                                             vtkEdgeTable *edgeTable,
                                             vtkFloatArray *edgeWeights,
                                             vtkIntArray *edgeNeighbors,
                                             double laplacian[],
                                             int map)
{
  // Get point neighbors
  vtkNew(vtkIdList, pointNeighbors);
  vtkSVGeneralUtils::GetPointNeighbors(p0, pd, pointNeighbors);

  // Loop through all edges of constaining point p0
  laplacian[0] = 0.0; laplacian[1] = 0.0; laplacian[2] = 0.0;
  for (int i=0; i<pointNeighbors->GetNumberOfIds(); i++)
  {
    // Get edge of p0 and p1
    vtkIdType p1 = pointNeighbors->GetId(i);
    vtkIdType edgeId = edgeTable->IsEdge(p0, p1);

    // Get weight of edge
    double weight = edgeWeights->GetValue(edgeId);
    //if (map == TUTTE)
    //{
    //  weight = 1.0;
    //}
    // if on boundary, no influence on laplacian
    int edgeNeighbor = edgeNeighbors->GetValue(edgeId);
    if (edgeNeighbor == -1)
      continue;

    // Get points, which are in this case the metric that we are ccomputing
    // the laplacian of.
    double p0Metric[3], p1Metric[3], data0[3], data1[3];
    pd->GetPoint(p0, p0Metric);
    pd->GetPoint(p1, p1Metric);

    // Update the laplacian of this point in the mesh.
    for (int j=0; j<3; j++)
      laplacian[j] += weight * (p0Metric[j] - p1Metric[j]);
  }

  return SV_OK;
}

// ----------------------
// ComputeDataLaplacian
// ----------------------
int vtkSVGeneralUtils::ComputeDataLaplacian(vtkIdType p0,
                                            vtkFloatArray *data,
                                            vtkPolyData *pd,
                                            vtkEdgeTable *edgeTable,
                                            vtkFloatArray *edgeWeights,
                                            vtkIntArray *edgeNeighbors,
                                            double laplacian[],
                                            int map)
{
  // Make sure three components to data array
  if (data->GetNumberOfComponents() != 3)
    return SV_ERROR;

  // Get point neighbors
  vtkNew(vtkIdList, pointNeighbors);
  vtkNew(vtkIdList, dummyList);
  vtkSVGeneralUtils::GetPointNeighbors(p0, pd, pointNeighbors);

  // Loop through all edges containing p0
  laplacian[0] = 0.0; laplacian[1] = 0.0; laplacian[2] = 0.0;
  for (int i=0; i<pointNeighbors->GetNumberOfIds(); i++)
  {
    // Get edge of p0 and p1
    vtkIdType p1 = pointNeighbors->GetId(i);
    vtkIdType edgeId = edgeTable->IsEdge(p0, p1);
    double weight = edgeWeights->GetValue(edgeId);
    //if (map == TUTTE)
    //{
    //  weight = 1.0;
    //}
    // If edge is boundary, this cell does not contribute to laplacian
    int edgeNeighbor = edgeNeighbors->GetValue(edgeId);
    if (edgeNeighbor == -1)
      continue;

    // Get values of data array
    double p0Metric[3], p1Metric[3], data0[3], data1[3];
    data->GetTuple(p0, p0Metric);
    data->GetTuple(p1, p1Metric);

    for (int j=0; j<3; j++)
    {
      laplacian[j] += weight * (p0Metric[j] - p1Metric[j]);
    }
  }

  return SV_OK;
}


// ----------------------
// ComputeDataArrayLaplacian
// ----------------------
int vtkSVGeneralUtils::ComputeDataArrayLaplacian(vtkFloatArray *data,
                                                 vtkPolyData *pd,
                                                 vtkEdgeTable *edgeTable,
                                                 vtkFloatArray *edgeWeights,
                                                 vtkIntArray *edgeNeighbors,
                                                 vtkFloatArray *laplacian, int map)
{
  // Get number of points
  int numPts = data->GetNumberOfTuples();

  // Loop through points, getting the laplacian of the data array at each point
  for (int i=0; i<numPts; i++)
  {
    double pointLaplacian[3];
    vtkSVGeneralUtils::ComputeDataLaplacian(i, data, pd, edgeTable, edgeWeights,
                                                        edgeNeighbors, pointLaplacian, map);
    laplacian->SetTuple(i, pointLaplacian);
  }

  return SV_OK;
}

// ----------------------
// RunLoopFind
// ----------------------
int vtkSVGeneralUtils::RunLoopFind(vtkPolyData *pd,
                                   vtkIdType startPt,
                                   vtkIdType nextCell,
                                   vtkPolyData *loop,
                                   vtkIdList *boundaryIds)
{
  // Set check ids in case booundaryIds is not NULL
  int checkIds = 0;
  int checkNum = 0;
  vtkNew(vtkIntArray, checkList);
  checkList->SetNumberOfTuples(pd->GetNumberOfPoints());
  checkList->FillComponent(0, 0);

  // If boundaryIds not NULL, we check the order of points given!
  if (boundaryIds != NULL)
  {
    checkIds = 1;
    for (int i=0; i<boundaryIds->GetNumberOfIds(); i++)
      checkList->SetTuple1(boundaryIds->GetId(i), 1);
    if (startPt != boundaryIds->GetId(0))
    {
      fprintf(stdout,"Start point does not match given\n");
      return SV_ERROR;
    }
    checkNum++;
  }

  // Initialize starting and ending loop points
  vtkIdType prevPt = startPt;
  vtkIdType nextPt = startPt;
  vtkNew(vtkIdList, pointIds);
  vtkNew(vtkIdList, cellIds);

  // Get Cell points
  pd->GetCellPoints(nextCell,pointIds);

  // Iterate the point
  if (pointIds->GetId(0) == nextPt)
    nextPt = pointIds->GetId(1);
  else
    nextPt = pointIds->GetId(0);
  vtkNew(vtkIdList, newline);
  newline->SetNumberOfIds(2);
  newline->SetId(0, prevPt);
  newline->SetId(1, nextPt);
  //newline.id = nextCell;
  loop->InsertNextCell(VTK_LINE, newline);

  // Loop through cells
  while(nextPt != startPt)
  {
    if (checkIds && checkList->GetTuple1(nextPt))
    {
      // Return error if boundary points are provided and out of order
      if (checkNum != boundaryIds->IsId(nextPt))
      {
        fprintf(stdout,"Boundary points are not in correct order\n");
        return SV_ERROR;
      }
      checkNum++;
    }

    // Get next cell
    pd->GetPointCells(nextPt,cellIds);
    if (cellIds->GetId(0) == nextCell)
      nextCell = cellIds->GetId(1);
    else
      nextCell = cellIds->GetId(0);

    // Get next point
    pd->GetCellPoints(nextCell,pointIds);
    prevPt = nextPt;
    if (pointIds->GetId(0) == nextPt)
      nextPt = pointIds->GetId(1);
    else
      nextPt = pointIds->GetId(0);

    // Insert new line
    vtkNew(vtkIdList, newestline);
    newestline->SetNumberOfIds(2);
    newestline->InsertId(0, prevPt);
    newestline->InsertId(1, nextPt);
    //newestline.id = nextCell;
    loop->InsertNextCell(VTK_LINE, newestline);
  }

  return SV_OK;
}

// ----------------------
// SeparateLoops
// ----------------------
/*
 * \details TODO:Function needs some work
 */
int vtkSVGeneralUtils::SeparateLoops(vtkPolyData *pd,
                                     vtkPolyData **loops,
                                     int numBoundaries,
                                     const double xvec[3],
                                     const double zvec[3],
                                     const int boundaryStart[2])
{
  vtkIdType nextCell;
  vtkNew(vtkIdList, cellIds);
  int numInterPts = pd->GetNumberOfPoints();
  int numInterLines = pd->GetNumberOfLines();
  pd->BuildLinks();

  int count = 0;
  for (int i=0;i<numBoundaries;i++)
  {
    vtkIdType startPt = boundaryStart[i];
    vtkPolyData *newloop = loops[count];
    newloop->Allocate(pd->GetNumberOfCells(), 1000);
    pd->GetPointCells(startPt,cellIds);

    nextCell = cellIds->GetId(0);
    vtkIdType npts, *pts;
    int testPt = -1;
    pd->GetCellPoints(nextCell, npts, pts);
    if (pts[0] == startPt)
      testPt = pts[1];
    else
      testPt = pts[0];

    double pt0[3], pt1[3], vec0[3], vec1[3];
    pd->GetPoint(startPt, pt0);
    pd->GetPoint(testPt, pt1);
    vtkMath::Subtract(pt1, pt0, vec0);
    vtkMath::Normalize(vec0);
    vtkMath::Cross(zvec, xvec, vec1);
    vtkMath::Normalize(vec1);
    if (vtkMath::Dot(vec0, vec1) < 0)
    {
      nextCell = cellIds->GetId(1);
    }
    //if (testPt != boundaryStart[i+2])
    //{
    //  nextCell = cellIds->GetId(1);
    //}

    //Run through intersection lines to get loops!
    vtkSVGeneralUtils::RunLoopFind(pd, startPt, nextCell, newloop, NULL);
    loops[count++] = newloop;
  }

  return SV_OK;
}

// ----------------------
// GetRotationMatrix
// ----------------------
/**
 * \details Uses quaternions to compute a rotation matrix from to align vec0 and vec1
 */
int vtkSVGeneralUtils::GetRotationMatrix(double vec0[3], double vec1[3], vtkMatrix4x4 *rotMatrix)
{
  double perpVec[3];
  vtkMath::Normalize(vec0);
  vtkMath::Normalize(vec1);
  vtkMath::Cross(vec0, vec1, perpVec);
  double costheta = vtkMath::Dot(vec0, vec1);
  double sintheta = vtkMath::Norm(perpVec);
  double theta = atan2(sintheta, costheta);
  if (sintheta != 0)
  {
    perpVec[0] /= sintheta;
    perpVec[1] /= sintheta;
    perpVec[2] /= sintheta;
  }
  costheta = cos(0.5*theta);
  sintheta = sin(0.5*theta);
  double quat[4];
  quat[0] = costheta;
  quat[1] = perpVec[0]*sintheta;
  quat[2] = perpVec[1]*sintheta;
  quat[3] = perpVec[2]*sintheta;

  double mat[3][3];
  vtkMath::QuaternionToMatrix3x3(quat, mat);

  // | R_0 R_1 R_2 0 |
  // | R_3 R_4 R_2 0 |
  // | R_6 R_7 R_8 0 |
  // |  0   0   0  1 |
  for (int i=0; i<3; i++)
  {
    for (int j=0; j<3; j++)
    {
      rotMatrix->SetElement(i, j, mat[i][j]);
    }
    rotMatrix->SetElement(i, 3, 0.0);
    rotMatrix->SetElement(3, i, 0.0);
  }
  rotMatrix->SetElement(3, 3, 1.0);

  return SV_OK;
}

// ----------------------
// GetRotationMatrix
// ----------------------
int vtkSVGeneralUtils::GetRotationMatrix(double vec0[3], double vec1[3], double rotMatrix[16])
{
  vtkNew(vtkMatrix4x4, rotMatrix4x4);
  vtkSVGeneralUtils::GetRotationMatrix(vec0, vec1, rotMatrix4x4);
  for (int i=0; i<4; i++)
  {
    for (int j=0; j<4; j++)
      rotMatrix[i*4+j] = rotMatrix4x4->GetElement(i, j);
  }

  return SV_OK;
}


// ----------------------
// ApplyRotationMatrix
// ----------------------
int vtkSVGeneralUtils::ApplyRotationMatrix(vtkPolyData *pd, vtkMatrix4x4 *rotMatrix)
{
  // Set up transformer
  vtkSmartPointer<vtkTransform> transformer =
    vtkSmartPointer<vtkTransform>::New();
  transformer->SetMatrix(rotMatrix);

  // Transform the polydata
  vtkSmartPointer<vtkTransformPolyDataFilter> pdTransformer =
    vtkSmartPointer<vtkTransformPolyDataFilter>::New();
  pdTransformer->SetInputData(pd);
  pdTransformer->SetTransform(transformer);
  pdTransformer->Update();

  // Copy output
  pd->DeepCopy(pdTransformer->GetOutput());
  pd->BuildLinks();

  return SV_OK;
}

// ----------------------
// ApplyRotationMatrix
// ----------------------
int vtkSVGeneralUtils::ApplyRotationMatrix(vtkUnstructuredGrid *ug, double rotMatrix[16])
{
  // Set up transformer
  vtkSmartPointer<vtkTransform> transformer =
    vtkSmartPointer<vtkTransform>::New();
  transformer->SetMatrix(rotMatrix);

  // Transform the polydata
  vtkSmartPointer<vtkTransformFilter> ugTransformer =
    vtkSmartPointer<vtkTransformFilter>::New();
  ugTransformer->SetInputData(ug);
  ugTransformer->SetTransform(transformer);
  ugTransformer->Update();

  // Copy output
  ug->DeepCopy(ugTransformer->GetOutput());
  ug->BuildLinks();
  return SV_OK;
}

// ----------------------
// ApplyRotationMatrix
// ----------------------
int vtkSVGeneralUtils::ApplyRotationMatrix(vtkUnstructuredGrid *ug, vtkMatrix4x4 *rotMatrix)
{
  // Set up transformer
  vtkSmartPointer<vtkTransform> transformer =
    vtkSmartPointer<vtkTransform>::New();
  transformer->SetMatrix(rotMatrix);

  // Transform the polydata
  vtkSmartPointer<vtkTransformFilter> ugTransformer =
    vtkSmartPointer<vtkTransformFilter>::New();
  ugTransformer->SetInputData(ug);
  ugTransformer->SetTransform(transformer);
  ugTransformer->Update();

  // Copy output
  ug->DeepCopy(ugTransformer->GetOutput());
  ug->BuildLinks();

  return SV_OK;
}

// ----------------------
// ApplyRotationMatrix
// ----------------------
int vtkSVGeneralUtils::ApplyRotationMatrix(vtkPolyData *pd, double rotMatrix[16])
{
  // Set up transformer
  vtkSmartPointer<vtkTransform> transformer =
    vtkSmartPointer<vtkTransform>::New();
  transformer->SetMatrix(rotMatrix);

  // Transform the polydata
  vtkSmartPointer<vtkTransformPolyDataFilter> pdTransformer =
    vtkSmartPointer<vtkTransformPolyDataFilter>::New();
  pdTransformer->SetInputData(pd);
  pdTransformer->SetTransform(transformer);
  pdTransformer->Update();

  // Copy output
  pd->DeepCopy(pdTransformer->GetOutput());
  pd->BuildLinks();
  return SV_OK;
}

// ----------------------
// TransformTriangleToXYPlane
// ----------------------
int vtkSVGeneralUtils::TransformTriangleToXYPlane(double pt0[3], double pt1[3], double pt2[3],
                                                  double outPt0[3], double outPt1[3], double outPt2[3])
{
  double xVec[3], tmpVec[3];
  vtkMath::Subtract(pt1, pt0, xVec);
  vtkMath::Normalize(xVec);
  vtkMath::Subtract(pt2, pt1, tmpVec);
  vtkMath::Normalize(tmpVec);

  double zVec[3];
  vtkMath::Cross(xVec, tmpVec, zVec);
  vtkMath::Normalize(zVec);

  double realX[3], realZ[3];
  realX[0] = 1.0; realX[1] = 0.0; realX[2] = 0.0;
  realZ[0] = 0.0; realZ[1] = 0.0; realZ[2] = 1.0;

  vtkNew(vtkMatrix4x4, rotMatrix0);
  vtkSVGeneralUtils::GetRotationMatrix(zVec, realZ, rotMatrix0);
  double inputXVec[4], newXVec[4];
  inputXVec[0] = 0.0; inputXVec[1] = 0.0; inputXVec[2] = 0.0; inputXVec[3] = 0.0;
  newXVec[0] = 0.0; newXVec[1] = 0.0; newXVec[2] = 0.0; newXVec[3] = 0.0;
  for (int i=0; i<3; i++)
    inputXVec[i] = xVec[i];
  inputXVec[3] = 0.0;
  rotMatrix0->MultiplyPoint(xVec, newXVec);

  vtkNew(vtkMatrix4x4, rotMatrix1);
  vtkSVGeneralUtils::GetRotationMatrix(newXVec, realX, rotMatrix1);

  double rot0Pts[3][4];
  double rot1Pts[3][4];
  double tmpOutPts[3][4];
  for (int j=0; j<3; j++)
  {
    rot0Pts[0][j] = pt0[j];
    rot0Pts[1][j] = pt1[j];
    rot0Pts[2][j] = pt2[j];
  }
  rot0Pts[0][3] = 0.0;
  rot0Pts[1][3] = 0.0;
  rot0Pts[2][3] = 0.0;

  for (int i=0; i<3; i++)
  {
    rotMatrix0->MultiplyPoint(rot0Pts[i], rot1Pts[i]);
  }

  for (int i=0; i<3; i++)
  {
    rotMatrix1->MultiplyPoint(rot1Pts[i], tmpOutPts[i]);
  }

  // move to 0.0, 0.0
  for (int j=0; j<3; j++)
  {
    outPt0[j] = tmpOutPts[0][j] - tmpOutPts[0][j];
    outPt1[j] = tmpOutPts[1][j] - tmpOutPts[0][j];
    outPt2[j] = tmpOutPts[2][j] - tmpOutPts[0][j];
  }

  return SV_OK;
}


// ----------------------
// GetPolyDataAngles
// ----------------------
int vtkSVGeneralUtils::GetPolyDataAngles(vtkPolyData *pd, vtkFloatArray *cellAngles)
{
  // Get number of cells
  int numCells = pd->GetNumberOfCells();
  pd->BuildLinks();

  // Set up the resultant array
  cellAngles->SetNumberOfComponents(3);
  cellAngles->Allocate(numCells, 10000);
  cellAngles->SetNumberOfTuples(numCells);

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    vtkIdType npts, *pts;
    pd->GetCellPoints(i, npts, pts);
    for (int j=0; j<3; j++)
    {
      // Get point permutation
      vtkIdType p0 = pts[j];
      vtkIdType p1 = pts[(j+1)%npts];
      vtkIdType p2 = pts[(j+2)%npts];

      double pt0[3], pt1[3], pt2[3];
      pd->GetPoint(p0, pt0);
      pd->GetPoint(p1, pt1);
      pd->GetPoint(p2, pt2);

      // Compute two vectors of triangle
      double vec0[3], vec1[3];
      for (int k=0; k<3; k++)
      {
        vec0[k] = pt0[k] - pt1[k];
        vec1[k] = pt2[k] - pt1[k];
      }

      // Compute angle between vectors
      double angleVec[3];
      vtkMath::Cross(vec0, vec1, angleVec);
      double radAngle = atan2(vtkMath::Norm(angleVec), vtkMath::Dot(vec0, vec1));

      // Set cell angles
      cellAngles->SetComponent(i, j, radAngle);
    }
  }

  return SV_OK;
}

// ----------------------
// GetRegions
// ----------------------
int vtkSVGeneralUtils::GetRegions(vtkPolyData *pd, std::string arrayName,
                                  std::vector<Region> &allRegions)
{
  int numCells = pd->GetNumberOfCells();
  int numPoints = pd->GetNumberOfPoints();

  std::vector<std::vector<int> > tempRegions(numCells);
  std::vector<std::vector<int> > directNeighbors(numCells);
  std::vector<int> numberOfDirectNeighbors(numCells);
  std::vector<int> pointOnOpenEdge(numPoints, 0);

  for (int i=0; i<numCells; i++)
  {
    int directNeiCount = 0;
    std::vector<int> neighborCells;
    vtkIdType npts, *pts;
    pd->GetCellPoints(i, npts, pts);
    for (int j=0; j<npts; j++)
    {
      int ptId0 = pts[j];
      int ptId1 = pts[(j+1)%npts];
      vtkNew(vtkIdList, cellEdgeNeighbors);
      pd->GetCellEdgeNeighbors(i, ptId0, ptId1, cellEdgeNeighbors);
      directNeiCount += cellEdgeNeighbors->GetNumberOfIds();
      for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
        neighborCells.push_back(cellEdgeNeighbors->GetId(k));

      if (cellEdgeNeighbors->GetNumberOfIds() == 0)
      {
        pointOnOpenEdge[ptId0] = 1;
        pointOnOpenEdge[ptId1] = 1;
      }
    }
    directNeighbors[i] = neighborCells;
    numberOfDirectNeighbors[i] = directNeiCount;
  }

  for (int i=0; i<numCells; i++)
  {
    int regionId = pd->GetCellData()->GetArray(
      arrayName.c_str())->GetTuple1(i);
    tempRegions[i].push_back(-1);
    tempRegions[i].push_back(regionId);
  }

  int region = 0;
  for (int i=0; i<numCells; i++)
  {
    if (tempRegions[i][0] == -1)
    {
      tempRegions[i][0] = region;

      int count=1;
      std::vector<int> tempIndex;
      tempIndex.push_back(i);

      for (int j=0; j<count; j++)
      {
        for (int k=0; k<numberOfDirectNeighbors[tempIndex[j]]; k++)
        {
          int cellId = directNeighbors[tempIndex[j]][k];
          if (tempRegions[cellId][0] == -1 && tempRegions[i][1] == tempRegions[cellId][1])
          {
            tempRegions[cellId][0] = region;
            tempIndex.push_back(cellId);
            count++;
          }
        }
      }
      region++;
    }
  }

  int numberOfRegions = region;

  allRegions.clear();
  allRegions.resize(numberOfRegions);

  for (int i=0; i<numberOfRegions; i++)
  {
    allRegions[i].Index = i;
    allRegions[i].IndexCluster = 0;
    allRegions[i].NumberOfCorners = 0;
    allRegions[i].NumberOfElements = 0;
    allRegions[i].Elements.clear();
    allRegions[i].CornerPoints.clear();
    for (int j=0; j<allRegions[i].BoundaryEdges.size(); j++)
      allRegions[i].BoundaryEdges[j].clear();
    allRegions[i].BoundaryEdges.clear();
  }

  for (int i=0; i<numCells; i++)
  {
    int regionId = tempRegions[i][0];
    allRegions[regionId].Elements.push_back(i);
    allRegions[regionId].NumberOfElements++;
  }

  for (int i=0; i<numberOfRegions; i++)
  {
    int cellId = allRegions[i].Elements[0];
    allRegions[i].IndexCluster = tempRegions[cellId][1];
  }

  std::vector<int> cornerPoints;
  std::vector<int> isCornerPoint(numPoints);
  std::vector<int> isBoundaryPoint(numPoints);
  for (int i=0; i<numPoints; i++)
  {
    vtkNew(vtkIdList, pointCellsValues);
    vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, i, pointCellsValues);
    if (pointOnOpenEdge[i] == 1)
      pointCellsValues->InsertNextId(-1);
    if (pointCellsValues->GetNumberOfIds() >= 3)
    {
      cornerPoints.push_back(i);
      isCornerPoint[i] = 1;
    }
    else
      isCornerPoint[i] = 0;

    if (pointCellsValues->GetNumberOfIds() == 2)
      isBoundaryPoint[i] = 1;
    else
      isBoundaryPoint[i] = 0;
  }

  int runCount = 0;
  int numberOfCornerPoints = cornerPoints.size();

  int firstCorner;

  for (int i=0; i<numberOfRegions; i++)
  {
    std::vector<int> tempCornerPoints;
    for (int j=0; j<allRegions[i].NumberOfElements; j++)
    {
      int cellId = allRegions[i].Elements[j];
      vtkIdType npts, *pts;
      pd->GetCellPoints(cellId, npts, pts);
      for (int k=0; k<npts; k++)
      {
        if (isCornerPoint[pts[k]])
        {
          bool kCount = true;
          for (int kk=0; kk<tempCornerPoints.size(); kk++)
          {
            if (pts[k] == tempCornerPoints[kk])
            {
              kCount = false;
            }
          }

          if (kCount == true)
          {
            tempCornerPoints.push_back(pts[k]);
          }
        }
      }
    }

    allRegions[i].NumberOfCorners = tempCornerPoints.size();

    vtkNew(vtkIdList, uniqueCornerPoints);
    if (allRegions[i].NumberOfCorners != 0)
    {
      firstCorner = tempCornerPoints[0];
      allRegions[i].CornerPoints.push_back(firstCorner);
      uniqueCornerPoints->InsertUniqueId(firstCorner);

      int count=1;
      std::vector<int> tempNodes;
      tempNodes.push_back(firstCorner);

      vtkNew(vtkIdList, overrideCells);
      for (int j=0; j<count; j++)
      {
        vtkNew(vtkIdList, pointCells);
        if (overrideCells->GetNumberOfIds() != 0)
        {
          pointCells->DeepCopy(overrideCells);
          overrideCells->Reset();
        }
        else
        {
          pd->GetPointCells(tempNodes[j], pointCells);
        }

        for (int k=0; k<pointCells->GetNumberOfIds(); k++)
        {
          int cellId =  pointCells->GetId(k);
          int pointCCWId = vtkSVGeneralUtils::GetCCWPoint(pd, tempNodes[j], cellId);
          int isBoundaryEdge = vtkSVGeneralUtils::CheckBoundaryEdge(pd, arrayName, cellId, tempNodes[j], pointCCWId);

          if (tempRegions[cellId][0] == allRegions[i].Index && isBoundaryPoint[pointCCWId] && isBoundaryEdge)
          {
            tempNodes.push_back(pointCCWId);
            count++;
            break;
          }
          else if (tempRegions[cellId][0] == allRegions[i].Index && isCornerPoint[pointCCWId] && isBoundaryEdge)
          {
            if (pointCCWId == firstCorner)
            {
              tempNodes.push_back(pointCCWId);
              allRegions[i].BoundaryEdges.push_back(tempNodes);

              tempNodes.clear();

              if (uniqueCornerPoints->GetNumberOfIds() == allRegions[i].NumberOfCorners)
              {
                count = -1;
                break;
              }
              else
              {
                for (int ii=0; ii<tempCornerPoints.size(); ii++)
                {
                  bool tempCount = false;
                  int tempIndex  = tempCornerPoints[ii];

                  for (int jj=0; jj<allRegions[i].CornerPoints.size(); jj++)
                  {
                    if (tempIndex == allRegions[i].CornerPoints[jj])
                      tempCount = true;
                  }
                  if (tempCount == false)
                  {
                    firstCorner = tempIndex;
                    break;
                  }
                }
                allRegions[i].CornerPoints.push_back(firstCorner);
                uniqueCornerPoints->InsertUniqueId(firstCorner);
                tempNodes.push_back(firstCorner);
                count = 1;
                j = -1;
                break;
              }
            }
            else
            {
              tempNodes.push_back(pointCCWId);
              allRegions[i].CornerPoints.push_back(pointCCWId);
              uniqueCornerPoints->InsertUniqueId(pointCCWId);
              allRegions[i].BoundaryEdges.push_back(tempNodes);

              tempNodes.clear();
              tempNodes.push_back(pointCCWId);
              count = 1;
              j = -1;

              // Need the cellId to be first in the odd case where the corner point is a two-time corner point
              vtkNew(vtkIdList, addCells);
              addCells->InsertNextId(cellId);
              vtkSVGeneralUtils::GetPointEdgeCells(pd, arrayName, cellId, pointCCWId, addCells);
              for (int ii=0; ii<addCells->GetNumberOfIds(); ii++)
              {
                overrideCells->InsertUniqueId(addCells->GetId(ii));
              }

              vtkNew(vtkIdList, tempCells);
              pd->GetPointCells(pointCCWId, tempCells);

              for (int ii=0; ii<tempCells->GetNumberOfIds(); ii++)
              {
                overrideCells->InsertUniqueId(tempCells->GetId(ii));
              }

              break;
            }
          }
        }
      }
    }
    if (uniqueCornerPoints->GetNumberOfIds() != allRegions[i].NumberOfCorners)
    {
      return SV_ERROR;
    }
    allRegions[i].NumberOfCorners = allRegions[i].CornerPoints.size();
  }

  return SV_OK;
}

// ----------------------
// GetSpecificRegions
// ----------------------
int vtkSVGeneralUtils::GetSpecificRegions(vtkPolyData *pd, std::string arrayName,
                                             std::vector<Region> &allRegions,
                                             vtkIdList *targetRegions)
{
  int numCells = pd->GetNumberOfCells();
  int numPoints = pd->GetNumberOfPoints();

  std::vector<std::vector<int> > tempRegions(numCells);
  std::vector<std::vector<int> > directNeighbors(numCells);
  std::vector<int> numberOfDirectNeighbors(numCells);
  std::vector<int> pointOnOpenEdge(numPoints, 0);

  for (int i=0; i<numCells; i++)
  {
    int directNeiCount = 0;
    std::vector<int> neighborCells;
    vtkIdType npts, *pts;
    pd->GetCellPoints(i, npts, pts);
    for (int j=0; j<npts; j++)
    {
      int ptId0 = pts[j];
      int ptId1 = pts[(j+1)%npts];
      vtkNew(vtkIdList, cellEdgeNeighbors);
      pd->GetCellEdgeNeighbors(i, ptId0, ptId1, cellEdgeNeighbors);
      directNeiCount += cellEdgeNeighbors->GetNumberOfIds();
      for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
        neighborCells.push_back(cellEdgeNeighbors->GetId(k));

      if (cellEdgeNeighbors->GetNumberOfIds() == 0)
      {
        pointOnOpenEdge[ptId0] = 1;
        pointOnOpenEdge[ptId1] = 1;
      }
    }
    directNeighbors[i] = neighborCells;
    numberOfDirectNeighbors[i] = directNeiCount;
  }

  for (int i=0; i<numCells; i++)
  {
    int regionId = pd->GetCellData()->GetArray(
      arrayName.c_str())->GetTuple1(i);
    tempRegions[i].push_back(-1);
    tempRegions[i].push_back(regionId);
  }

  int region = 0;
  for (int i=0; i<numCells; i++)
  {
    if (tempRegions[i][0] == -1 && targetRegions->IsId(tempRegions[i][1]) != -1)
    {
      tempRegions[i][0] = region;

      int count=1;
      std::vector<int> tempIndex;
      tempIndex.push_back(i);

      for (int j=0; j<count; j++)
      {
        for (int k=0; k<numberOfDirectNeighbors[tempIndex[j]]; k++)
        {
          int cellId = directNeighbors[tempIndex[j]][k];
          if (tempRegions[cellId][0] == -1 && tempRegions[i][1] == tempRegions[cellId][1])
          {
            tempRegions[cellId][0] = region;
            tempIndex.push_back(cellId);
            count++;
          }
        }
      }
      region++;
    }
  }

  int numberOfRegions = region;

  allRegions.clear();
  allRegions.resize(numberOfRegions);

  for (int i=0; i<numberOfRegions; i++)
  {
    allRegions[i].Index = i;
    allRegions[i].IndexCluster = 0;
    allRegions[i].NumberOfCorners = 0;
    allRegions[i].NumberOfElements = 0;
    allRegions[i].Elements.clear();
    allRegions[i].CornerPoints.clear();
    for (int j=0; j<allRegions[i].BoundaryEdges.size(); j++)
      allRegions[i].BoundaryEdges[j].clear();
    allRegions[i].BoundaryEdges.clear();
  }

  for (int i=0; i<numCells; i++)
  {
    int regionId = tempRegions[i][0];
    if (regionId != -1)
    {
      allRegions[regionId].Elements.push_back(i);
      allRegions[regionId].NumberOfElements++;
    }
  }

  for (int i=0; i<numberOfRegions; i++)
  {
    int cellId = allRegions[i].Elements[0];
    allRegions[i].IndexCluster = tempRegions[cellId][1];
  }

  std::vector<int> cornerPoints;
  std::vector<int> isCornerPoint(numPoints);
  std::vector<int> isBoundaryPoint(numPoints);
  std::vector<int> isNonTargetBoundaryPoint(numPoints);
  for (int i=0; i<numPoints; i++)
  {
    vtkNew(vtkIdList, pointCellsValues);
    vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, i, pointCellsValues);
    if (pointOnOpenEdge[i] == 1)
      pointCellsValues->InsertNextId(-1);
    if (pointCellsValues->GetNumberOfIds() >= 3)
    {
      cornerPoints.push_back(i);
      isCornerPoint[i] = 1;
    }
    else
      isCornerPoint[i] = 0;

    if (pointCellsValues->GetNumberOfIds() == 2)
    {
      if (targetRegions->IsId(pointCellsValues->GetId(0)) != -1 &&
          targetRegions->IsId(pointCellsValues->GetId(1)) != -1)
      {
        isBoundaryPoint[i] = 1;
        isNonTargetBoundaryPoint[i] = 0;
      }
      else
      {
        isBoundaryPoint[i] = 0;
        isNonTargetBoundaryPoint[i] = 1;
      }
    }
    else
    {
      isBoundaryPoint[i] = 0;
      isNonTargetBoundaryPoint[i] = 0;
    }
  }

  int runCount = 0;
  int numberOfCornerPoints = cornerPoints.size();

  int firstCorner;

  for (int i=0; i<numberOfRegions; i++)
  {
    std::vector<int> tempCornerPoints;
    for (int j=0; j<allRegions[i].NumberOfElements; j++)
    {
      int cellId = allRegions[i].Elements[j];
      vtkIdType npts, *pts;
      pd->GetCellPoints(cellId, npts, pts);
      for (int k=0; k<npts; k++)
      {
        if (isCornerPoint[pts[k]])
        {
          bool kCount = true;
          for (int kk=0; kk<tempCornerPoints.size(); kk++)
          {
            if (pts[k] == tempCornerPoints[kk])
            {
              kCount = false;
            }
          }

          if (kCount == true)
          {
            tempCornerPoints.push_back(pts[k]);
          }
        }
      }
    }


    allRegions[i].NumberOfCorners = tempCornerPoints.size();

    vtkNew(vtkIdList, uniqueCornerPoints);
    if (allRegions[i].NumberOfCorners != 0)
    {
      firstCorner = tempCornerPoints[0];
      allRegions[i].CornerPoints.push_back(firstCorner);
      uniqueCornerPoints->InsertUniqueId(firstCorner);

      int count=1;
      std::vector<int> tempNodes;
      tempNodes.push_back(firstCorner);
      std::vector<int> newNodes;
      newNodes.push_back(firstCorner);

      vtkNew(vtkIdList, overrideCells);
      for (int j=0; j<count; j++)
      {
        vtkNew(vtkIdList, pointCells);
        if (overrideCells->GetNumberOfIds() != 0)
        {
          pointCells->DeepCopy(overrideCells);
          overrideCells->Reset();
        }
        else
        {
          pd->GetPointCells(tempNodes[j], pointCells);
        }

        for (int k=0; k<pointCells->GetNumberOfIds(); k++)
        {
          int cellId =  pointCells->GetId(k);
          int pointCCWId = vtkSVGeneralUtils::GetCCWPoint(pd, tempNodes[j], cellId);
          int isBoundaryEdge = vtkSVGeneralUtils::CheckBoundaryEdge(pd, arrayName, cellId, tempNodes[j], pointCCWId);

          if (tempRegions[cellId][0] == allRegions[i].Index && isBoundaryPoint[pointCCWId] && isBoundaryEdge)
          {
            tempNodes.push_back(pointCCWId);
            newNodes.push_back(pointCCWId);
            count++;
            break;
          }
          else if (tempRegions[cellId][0] == allRegions[i].Index && isCornerPoint[pointCCWId] && isBoundaryEdge)
          {
            if (pointCCWId == firstCorner)
            {
              tempNodes.push_back(pointCCWId);
              newNodes.push_back(pointCCWId);
              if (newNodes.size() > 2)
                allRegions[i].BoundaryEdges.push_back(newNodes);


              tempNodes.clear();
              newNodes.clear();

              if (uniqueCornerPoints->GetNumberOfIds() == allRegions[i].NumberOfCorners)
              {
                count = -1;
                break;
              }
              else
              {
                for (int ii=0; ii<tempCornerPoints.size(); ii++)
                {
                  bool tempCount = false;
                  int tempIndex  = tempCornerPoints[ii];

                  for (int jj=0; jj<allRegions[i].CornerPoints.size(); jj++)
                  {
                    if (tempIndex == allRegions[i].CornerPoints[jj])
                      tempCount = true;
                  }
                  if (tempCount == false)
                  {
                    firstCorner = tempIndex;
                    break;
                  }
                }
                allRegions[i].CornerPoints.push_back(firstCorner);
                uniqueCornerPoints->InsertUniqueId(firstCorner);
                tempNodes.push_back(firstCorner);
                newNodes.push_back(firstCorner);
                count = 1;
                j = -1;
                break;
              }
            }
            else
            {
              tempNodes.push_back(pointCCWId);
              newNodes.push_back(pointCCWId);
              allRegions[i].CornerPoints.push_back(pointCCWId);
              uniqueCornerPoints->InsertUniqueId(pointCCWId);
              if (newNodes.size() > 2)
                allRegions[i].BoundaryEdges.push_back(newNodes);
              tempNodes.clear();
              tempNodes.push_back(pointCCWId);
              newNodes.clear();
              newNodes.push_back(pointCCWId);
              count = 1;
              j = -1;

              // Need to cellId to be first in the odd case where the corner point is a two-time corner point
              vtkNew(vtkIdList, addCells);
              addCells->InsertNextId(cellId);
              vtkSVGeneralUtils::GetPointEdgeCells(pd, arrayName, cellId, pointCCWId, addCells);
              for (int ii=0; ii<addCells->GetNumberOfIds(); ii++)
              {
                overrideCells->InsertUniqueId(addCells->GetId(ii));
              }

              vtkNew(vtkIdList, tempCells);
              pd->GetPointCells(pointCCWId, tempCells);

              for (int ii=0; ii<tempCells->GetNumberOfIds(); ii++)
              {
                overrideCells->InsertUniqueId(tempCells->GetId(ii));
              }

              break;
            }
          }
          else if (tempRegions[cellId][0] == allRegions[i].Index && isNonTargetBoundaryPoint[pointCCWId] && isBoundaryEdge)
          {
            tempNodes.push_back(pointCCWId);
            count++;
            break;
          }
        }
      }
    }
    if (uniqueCornerPoints->GetNumberOfIds() != allRegions[i].NumberOfCorners)
    {
      return SV_ERROR;
    }
    allRegions[i].NumberOfCorners = allRegions[i].CornerPoints.size();
  }
  return SV_OK;
}


// ----------------------
// GetCCWPoint
// ----------------------
int vtkSVGeneralUtils::GetCCWPoint(vtkPolyData *pd, const int pointId, const int cellId)
{
	int pointCCW;
	int position = 0;

  vtkIdType npts, *pts;
  pd->GetCellPoints(cellId, npts, pts);
	for (int i = 0; i < npts; i++)
	{
		if (pts[i] == pointId)
		{
			position = i;
			break;
		}
	}

  position = (position+1)%npts;
  return pts[position];
}

// ----------------------
// GetCWPoint
// ----------------------
int vtkSVGeneralUtils::GetCWPoint(vtkPolyData *pd, const int pointId, const int cellId)
{
	int pointCCW;
	int position = 0;

  vtkIdType npts, *pts;
  pd->GetCellPoints(cellId, npts, pts);
	for (int i = 0; i < npts; i++)
	{
		if (pts[i] == pointId)
		{
			position = i;
			break;
		}
	}

  position = (position+npts-1)%npts;
  return pts[position];
}

// ----------------------
// CheckCellValuesEdge
// ----------------------
int vtkSVGeneralUtils::CheckCellValuesEdge(vtkPolyData *pd, std::string arrayName, const int cellId, const int pointId0, const int pointId1)
{
  vtkNew(vtkIdList, cellEdgeNeighbors);
  pd->GetCellEdgeNeighbors(cellId, pointId0, pointId1, cellEdgeNeighbors);

  vtkNew(vtkIdList, uniqueVals);
  uniqueVals->InsertNextId(pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellId));
  for (int i=0; i<cellEdgeNeighbors->GetNumberOfIds(); i++)
  {
    uniqueVals->InsertUniqueId(pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellEdgeNeighbors->GetId(i)));
  }

  int isEdge = 0;

  if (uniqueVals->GetNumberOfIds() == 2)
    isEdge = 1;

  return isEdge;
}

// ----------------------
// CheckBoundaryEdge
// ----------------------
int vtkSVGeneralUtils::CheckBoundaryEdge(vtkPolyData *pd, std::string arrayName, const int cellId, const int pointId0, const int pointId1)
{
  vtkNew(vtkIdList, cellEdgeNeighbors);
  pd->GetCellEdgeNeighbors(cellId, pointId0, pointId1, cellEdgeNeighbors);

  vtkNew(vtkIdList, uniqueVals);
  uniqueVals->InsertNextId(pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellId));
  for (int i=0; i<cellEdgeNeighbors->GetNumberOfIds(); i++)
  {
    uniqueVals->InsertUniqueId(pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellEdgeNeighbors->GetId(i)));
  }

  if (cellEdgeNeighbors->GetNumberOfIds() == 0)
    uniqueVals->InsertUniqueId(-1);

  int isEdge = 0;

  if (uniqueVals->GetNumberOfIds() == 2)
    isEdge = 1;

  return isEdge;
}

// ----------------------
// SplineKnots
// ----------------------
void vtkSVGeneralUtils::SplineKnots(std::vector<int> &u, int n, int t)
{

	int j;

	for (j = 0; j <= n+t; j++)
	{

		if (j < t)
		{
			u[j] = 0;
		}
		else if (j <= n)
		{
			u[j] = j - t + 1;
		}
		else if (j > n)
		{
			u[j] = n - t + 2;
		}

	}

}

// ----------------------
// SplineCurve
// ----------------------
void vtkSVGeneralUtils::SplineCurve(const std::vector<XYZ> &inp, int n, const std::vector<int> &knots, int t, std::vector<XYZ> &outp, int res)
{

	int i;

	double interval, increment;

	interval = 0.f;
	increment = (n - t + 2) / (double)(res-1);

	for (i = 0; i < res-1; i++)
	{

		SplinePoint(knots, n, t, interval, inp, outp[i]);

		interval += increment;
	}

	outp[res-1] = inp[n];

}

void vtkSVGeneralUtils::SplinePoint(const std::vector<int> &u, int n, int t, double v, const std::vector<XYZ> &control, XYZ &output)
{

	int k;
	double b;

	output.x = 0.f;
	output.y = 0.f;
	output.z = 0.f;

	for (k = 0; k <= n; k++)
	{
		b = SplineBlend(k, t, u, v);

		output.x += control[k].x * b;
		output.y += control[k].y * b;
		output.z += control[k].z * b;
	}

}

double vtkSVGeneralUtils::SplineBlend(int k, int t, const std::vector<int> &u, double v)
{

	double value;

	if (t == 1)
	{
		if ((u[k] <= v) && (v < u[k+1]))
			value = 1;
		else
			value = 0;
	}
	else
	{
		if ((u[k+t-1] == u[k]) && (u[k+t] == u[k+1]))
			value = 0;
		else if (u[k+t-1] == u[k])
			value = (u[k+t] - v) / (u[k+t] - u[k+1]) * SplineBlend(k+1,t-1,u,v);
		else if (u[k+t] == u[k+1])
			value = (v - u[k]) / (u[k+t-1] - u[k]) * SplineBlend(k,t-1,u,v);
		else
			value = (v - u[k]) / (u[k+t-1] - u[k]) * SplineBlend(k,t-1,u,v) +
			(u[k+t] - v) / (u[k+t] - u[k+1]) * SplineBlend(k+1,t-1,u,v);
	}

	return(value);

}

// ----------------------
// GetMostOccuringVal
// ----------------------
void vtkSVGeneralUtils::GetMostOccuringVal(vtkIdList *idList, int &output,
                                             int &max_count)
{
  int numIds = idList->GetNumberOfIds();

  max_count = 0;
  int max_val = idList->GetId(0);
  for (int i=0; i<numIds; i++)
  {
    int count = 1;
    for (int j=0; j<numIds; j++)
    {
      if (idList->GetId(i) == idList->GetId(j))
        count++;
    }
    if (count > max_count)
    {
      max_count = count;
      max_val = idList->GetId(i);
    }
  }

  output = max_val;
}

// ----------------------
// SmoothBoundaries
// ----------------------
int vtkSVGeneralUtils::SmoothBoundaries(vtkPolyData *pd, std::string arrayName)
{
  int numPoints = pd->GetNumberOfPoints();
  std::vector<int> cornerPoints;
  std::vector<int> isCornerPoint(numPoints);
  std::vector<int> isBoundaryPoint(numPoints);
  for (int i=0; i<numPoints; i++)
  {
    vtkNew(vtkIdList, pointCellsValues);
    vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, i, pointCellsValues);
    if (pointCellsValues->GetNumberOfIds() >= 3)
    {
      cornerPoints.push_back(i);
      isCornerPoint[i] = 1;
    }
    else
      isCornerPoint[i] = 0;

    if (pointCellsValues->GetNumberOfIds() == 2)
      isBoundaryPoint[i] = 1;
    else
      isBoundaryPoint[i] = 0;
  }


  for (int i=0; i<numPoints; i++)
  {
    if (isBoundaryPoint[i])
    {
      vtkNew(vtkIdList, pointCellsValues);
      vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName,
                                             i, pointCellsValues);

      // boundary edge
      if (pointCellsValues->GetNumberOfIds() == 2)
      {
        vtkNew(vtkIdList, pointCells);
        pd->GetPointCells(i, pointCells);

        int count[2]; count[0] = 0; count[1] = 0;
        int cellIds[2][2];
        for (int j=0; j<pointCells->GetNumberOfIds(); j++)
        {
          for (int k=0; k<2; k++)
          {
            if (pd->GetCellData()->GetArray(
              arrayName.c_str())->GetTuple1(pointCells->GetId(j)) == pointCellsValues->GetId(k))
            {
              if (count[k] < 2)
                cellIds[k][count[k]] = pointCells->GetId(j);
              count[k]++;
            }
          }
        }

        if (count[0] == 2 || count[1] == 2)
        {
          vtkNew(vtkIdList, uniquePoints);
          vtkIdType npts, *pts;
          if (count[0] == 2 && count[1] == 2)
          {
            for (int j=0; j<2; j++)
            {
              for (int k=0; k<2; k++)
              {
                pd->GetCellPoints(cellIds[j][k], npts, pts);
                for (int p=0; p<npts; p++)
                  uniquePoints->InsertUniqueId(pts[p]);
              }
            }
          }
          else
          {
            if (count[0] == 2)
            {
              vtkNew(vtkIdList, avgPoints);
              double check0[3]; check0[0] = 0.0; check0[1] = 0.0; check0[2] = 0.0;
              for (int j=0; j<2; j++)
              {
                pd->GetCellPoints(cellIds[0][j], npts, pts);
                for (int p=0; p<npts; p++)
                {
                  int isId = avgPoints->IsId(pts[p]);
                  if (isId == -1)
                  {
                    avgPoints->InsertNextId(pts[p]);
                    double pt[3];
                    pd->GetPoint(pts[p], pt);
                    for (int r=0; r<3; r++)
                      check0[r] += pt[r];
                  }
                }
              }
              for (int j=0; j<3; j++)
                check0[j] = (1./avgPoints->GetNumberOfIds())*check0[j];

              vtkNew(vtkIdList, halfPoints);
              double check1[3]; check1[0] = 0.0; check1[1] = 0.0; check1[2] = 0.0;
              pd->GetCellPoints(cellIds[0][0], npts, pts);
              for (int j=0; j<npts; j++)
              {
                int ptId0 = pts[j];
                int ptId1 = pts[(j+1)%npts];
                vtkNew(vtkIdList, neighborCell);
                pd->GetCellEdgeNeighbors(cellIds[0][0], ptId0, ptId1, neighborCell);
                if (neighborCell->GetNumberOfIds() > 0)
                {
                  if (neighborCell->GetId(0) == cellIds[0][1])
                  {
                    halfPoints->InsertNextId(ptId0);
                    halfPoints->InsertNextId(ptId1);

                    double pt[3];
                    pd->GetPoint(ptId0, pt);
                    for (int r=0; r<3; r++)
                      check1[r] += pt[r];
                    pd->GetPoint(ptId1, pt);
                    for (int r=0; r<3; r++)
                      check1[r] += pt[r];
                    for (int r=0; r<3; r++)
                      check1[r] = (1./2)*check1[r];
                  }
                }
              }

              double startPt[3];
              pd->GetPoint(i, startPt);
              double dist0 = vtkSVMathUtils::Distance(check0, startPt);
              double dist1 = vtkSVMathUtils::Distance(check1, startPt);

              if (dist0 > dist1)
              {
                for (int r=0; r<halfPoints->GetNumberOfIds(); r++)
                  uniquePoints->InsertNextId(halfPoints->GetId(r));
              }
              else
              {
                for (int r=0; r<avgPoints->GetNumberOfIds(); r++)
                  uniquePoints->InsertNextId(avgPoints->GetId(r));
              }
            }
            else if (count[1] == 2)
            {
              vtkNew(vtkIdList, avgPoints);
              double check0[3]; check0[0] = 0.0; check0[1] = 0.0; check0[2] = 0.0;
              for (int j=0; j<2; j++)
              {
                pd->GetCellPoints(cellIds[1][j], npts, pts);
                for (int p=0; p<npts; p++)
                {
                  int isId = avgPoints->IsId(pts[p]);
                  if (isId == -1)
                  {
                    avgPoints->InsertNextId(pts[p]);
                    double pt[3];
                    pd->GetPoint(pts[p], pt);
                    for (int r=0; r<3; r++)
                      check0[r] += pt[r];
                  }
                }
              }
              for (int j=0; j<3; j++)
                check0[j] = (1./avgPoints->GetNumberOfIds())*check0[j];

              vtkNew(vtkIdList, halfPoints);
              double check1[3]; check1[0] = 0.0; check1[1] = 0.0; check1[2] = 0.0;
              pd->GetCellPoints(cellIds[1][0], npts, pts);
              for (int j=0; j<npts; j++)
              {
                int ptId0 = pts[j];
                int ptId1 = pts[(j+1)%npts];
                vtkNew(vtkIdList, neighborCell);
                pd->GetCellEdgeNeighbors(cellIds[1][0], ptId0, ptId1, neighborCell);
                if (neighborCell->GetNumberOfIds() > 0)
                {
                  if (neighborCell->GetId(0) == cellIds[1][1])
                  {
                    halfPoints->InsertNextId(ptId0);
                    halfPoints->InsertNextId(ptId1);

                    double pt[3];
                    pd->GetPoint(ptId0, pt);
                    for (int r=0; r<3; r++)
                      check1[r] += pt[r];
                    pd->GetPoint(ptId1, pt);
                    for (int r=0; r<3; r++)
                      check1[r] += pt[r];
                    for (int r=0; r<3; r++)
                      check1[r] = (1./2)*check1[r];
                  }
                }
              }

              double startPt[3];
              pd->GetPoint(i, startPt);
              double dist0 = vtkSVMathUtils::Distance(check0, startPt);
              double dist1 = vtkSVMathUtils::Distance(check1, startPt);

              if (dist0 > dist1)
              {
                for (int r=0; r<halfPoints->GetNumberOfIds(); r++)
                  uniquePoints->InsertNextId(halfPoints->GetId(r));
              }
              else
              {
                for (int r=0; r<avgPoints->GetNumberOfIds(); r++)
                  uniquePoints->InsertNextId(avgPoints->GetId(r));
              }
            }
          }
          int numIds = uniquePoints->GetNumberOfIds();
          double center[3];
          for (int j=0; j<3; j++)
            center[j] = 0.0;
          for (int k=0; k<numIds; k++)
          {
            double pt[3];
            pd->GetPoint(uniquePoints->GetId(k), pt);
            for (int j=0; j<3; j++)
              center[j] += pt[j];
          }
          for (int j=0; j<3; j++)
            center[j] = (1./numIds)*center[j];

          pd->GetPoints()->SetPoint(i, center);
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// SmoothSpecificBoundaries
// ----------------------
int vtkSVGeneralUtils::SmoothSpecificBoundaries(vtkPolyData *pd, std::string arrayName, vtkIdList *targetRegions)
{
  int numPoints = pd->GetNumberOfPoints();
  std::vector<int> cornerPoints;
  std::vector<int> isCornerPoint(numPoints);
  std::vector<int> isBoundaryPoint(numPoints);
  for (int i=0; i<numPoints; i++)
  {
    vtkNew(vtkIdList, pointCellsValues);
    vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, i, pointCellsValues);
    if (pointCellsValues->GetNumberOfIds() >= 3)
    {
      cornerPoints.push_back(i);
      isCornerPoint[i] = 1;
    }
    else
      isCornerPoint[i] = 0;

    if (pointCellsValues->GetNumberOfIds() == 2)
    {
      if (targetRegions->IsId(pointCellsValues->GetId(0)) != -1 &&
          targetRegions->IsId(pointCellsValues->GetId(1)) != -1)
        isBoundaryPoint[i] = 1;
      else
        isBoundaryPoint[i] = 0;
    }
    else
      isBoundaryPoint[i] = 0;
  }

  for (int i=0; i<numPoints; i++)
  {
    if (isBoundaryPoint[i])
    {
      vtkNew(vtkIdList, pointCellsValues);
      vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName,
                                             i, pointCellsValues);

      // boundary edge
      if (pointCellsValues->GetNumberOfIds() == 2)
      {
        vtkNew(vtkIdList, pointCells);
        pd->GetPointCells(i, pointCells);

        int count[2]; count[0] = 0; count[1] = 0;
        int cellIds[2][2];
        for (int j=0; j<pointCells->GetNumberOfIds(); j++)
        {
          for (int k=0; k<2; k++)
          {
            if (pd->GetCellData()->GetArray(
              arrayName.c_str())->GetTuple1(pointCells->GetId(j)) == pointCellsValues->GetId(k))
            {
              if (count[k] < 2)
                cellIds[k][count[k]] = pointCells->GetId(j);
              count[k]++;
            }
          }
        }

        if (count[0] == 2 || count[1] == 2)
        {
          vtkNew(vtkIdList, uniquePoints);
          vtkIdType npts, *pts;
          if (count[0] == 2 && count[1] == 2)
          {
            for (int j=0; j<2; j++)
            {
              for (int k=0; k<2; k++)
              {
                pd->GetCellPoints(cellIds[j][k], npts, pts);
                for (int p=0; p<npts; p++)
                  uniquePoints->InsertUniqueId(pts[p]);
              }
            }
          }
          else
          {
            if (count[0] == 2)
            {
              vtkNew(vtkIdList, avgPoints);
              double check0[3]; check0[0] = 0.0; check0[1] = 0.0; check0[2] = 0.0;
              for (int j=0; j<2; j++)
              {
                pd->GetCellPoints(cellIds[0][j], npts, pts);
                for (int p=0; p<npts; p++)
                {
                  int isId = avgPoints->IsId(pts[p]);
                  if (isId == -1)
                  {
                    avgPoints->InsertNextId(pts[p]);
                    double pt[3];
                    pd->GetPoint(pts[p], pt);
                    for (int r=0; r<3; r++)
                      check0[r] += pt[r];
                  }
                }
              }
              for (int j=0; j<3; j++)
                check0[j] = (1./avgPoints->GetNumberOfIds())*check0[j];

              vtkNew(vtkIdList, halfPoints);
              double check1[3]; check1[0] = 0.0; check1[1] = 0.0; check1[2] = 0.0;
              pd->GetCellPoints(cellIds[0][0], npts, pts);
              for (int j=0; j<npts; j++)
              {
                int ptId0 = pts[j];
                int ptId1 = pts[(j+1)%npts];
                vtkNew(vtkIdList, neighborCell);
                pd->GetCellEdgeNeighbors(cellIds[0][0], ptId0, ptId1, neighborCell);
                if (neighborCell->GetNumberOfIds() > 0)
                {
                  if (neighborCell->GetId(0) == cellIds[0][1])
                  {
                    halfPoints->InsertNextId(ptId0);
                    halfPoints->InsertNextId(ptId1);

                    double pt[3];
                    pd->GetPoint(ptId0, pt);
                    for (int r=0; r<3; r++)
                      check1[r] += pt[r];
                    pd->GetPoint(ptId1, pt);
                    for (int r=0; r<3; r++)
                      check1[r] += pt[r];
                    for (int r=0; r<3; r++)
                      check1[r] = (1./2)*check1[r];
                  }
                }
              }

              double startPt[3];
              pd->GetPoint(i, startPt);
              double dist0 = vtkSVMathUtils::Distance(check0, startPt);
              double dist1 = vtkSVMathUtils::Distance(check1, startPt);

              if (dist0 > dist1)
              {
                for (int r=0; r<halfPoints->GetNumberOfIds(); r++)
                  uniquePoints->InsertNextId(halfPoints->GetId(r));
              }
              else
              {
                for (int r=0; r<avgPoints->GetNumberOfIds(); r++)
                  uniquePoints->InsertNextId(avgPoints->GetId(r));
              }
            }
            else if (count[1] == 2)
            {
              vtkNew(vtkIdList, avgPoints);
              double check0[3]; check0[0] = 0.0; check0[1] = 0.0; check0[2] = 0.0;
              for (int j=0; j<2; j++)
              {
                pd->GetCellPoints(cellIds[1][j], npts, pts);
                for (int p=0; p<npts; p++)
                {
                  int isId = avgPoints->IsId(pts[p]);
                  if (isId == -1)
                  {
                    avgPoints->InsertNextId(pts[p]);
                    double pt[3];
                    pd->GetPoint(pts[p], pt);
                    for (int r=0; r<3; r++)
                      check0[r] += pt[r];
                  }
                }
              }
              for (int j=0; j<3; j++)
                check0[j] = (1./avgPoints->GetNumberOfIds())*check0[j];

              vtkNew(vtkIdList, halfPoints);
              double check1[3]; check1[0] = 0.0; check1[1] = 0.0; check1[2] = 0.0;
              pd->GetCellPoints(cellIds[1][0], npts, pts);
              for (int j=0; j<npts; j++)
              {
                int ptId0 = pts[j];
                int ptId1 = pts[(j+1)%npts];
                vtkNew(vtkIdList, neighborCell);
                pd->GetCellEdgeNeighbors(cellIds[1][0], ptId0, ptId1, neighborCell);
                if (neighborCell->GetNumberOfIds() > 0)
                {
                  if (neighborCell->GetId(0) == cellIds[1][1])
                  {
                    halfPoints->InsertNextId(ptId0);
                    halfPoints->InsertNextId(ptId1);

                    double pt[3];
                    pd->GetPoint(ptId0, pt);
                    for (int r=0; r<3; r++)
                      check1[r] += pt[r];
                    pd->GetPoint(ptId1, pt);
                    for (int r=0; r<3; r++)
                      check1[r] += pt[r];
                    for (int r=0; r<3; r++)
                      check1[r] = (1./2)*check1[r];
                  }
                }
              }

              double startPt[3];
              pd->GetPoint(i, startPt);
              double dist0 = vtkSVMathUtils::Distance(check0, startPt);
              double dist1 = vtkSVMathUtils::Distance(check1, startPt);

              if (dist0 > dist1)
              {
                for (int r=0; r<halfPoints->GetNumberOfIds(); r++)
                  uniquePoints->InsertNextId(halfPoints->GetId(r));
              }
              else
              {
                for (int r=0; r<avgPoints->GetNumberOfIds(); r++)
                  uniquePoints->InsertNextId(avgPoints->GetId(r));
              }
            }
          }
          int numIds = uniquePoints->GetNumberOfIds();
          double center[3];
          for (int j=0; j<3; j++)
            center[j] = 0.0;
          for (int k=0; k<numIds; k++)
          {
            double pt[3];
            pd->GetPoint(uniquePoints->GetId(k), pt);
            for (int j=0; j<3; j++)
              center[j] += pt[j];
          }
          for (int j=0; j<3; j++)
            center[j] = (1./numIds)*center[j];

          pd->GetPoints()->SetPoint(i, center);
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// GetPointEdgeCells
// ----------------------
int vtkSVGeneralUtils::GetPointEdgeCells(vtkPolyData *pd, std::string arrayName,
                                                     const int cellId, const int pointId,
                                                     vtkIdList *sameCells)
{
  int sameValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellId);

  vtkIdType npts, *pts;
  pd->GetCellPoints(cellId, npts, pts);

  for (int i=0; i<npts; i++)
  {
    int ptId0 = pts[i];
    int ptId1 = pts[(i+1)%npts];

    if (ptId0 == pointId || ptId1 == pointId)
    {
      vtkNew(vtkIdList, cellNeighbor);
      pd->GetCellEdgeNeighbors(cellId, ptId0, ptId1, cellNeighbor);

      for (int j=0; j<cellNeighbor->GetNumberOfIds(); j++)
      {
        int cellNeighborId = cellNeighbor->GetId(j);
        int cellNeighborValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellNeighborId);
        if (sameCells->IsId(cellNeighborId) == -1 && cellNeighborValue == sameValue)
        {
          sameCells->InsertUniqueId(cellNeighborId);
          vtkSVGeneralUtils::GetPointEdgeCells(pd, arrayName, cellNeighborId, pointId, sameCells);
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// CurveFitBoundaries
// ----------------------
int vtkSVGeneralUtils::CurveFitBoundaries(vtkPolyData *pd, std::string arrayName,
                                     std::vector<Region> allRegions)
{
  int numRegions = allRegions.size();

  std::vector<int> edgeValueCheck;
  for (int i=0; i<numRegions; i++)
  {
    for (int j=0; j<allRegions[i].BoundaryEdges.size(); j++)
    {
      int edgeSize = allRegions[i].BoundaryEdges[j].size();

      int edgeValue = 0;
      for (int k=0; k<edgeSize; k++)
        edgeValue += allRegions[i].BoundaryEdges[j][k];

      int usedEdge=0;
      for (int k=0; k<edgeValueCheck.size(); k++)
      {
        if (edgeValue == edgeValueCheck[k])
        {
          usedEdge = 1;
          break;
        }
      }
      if (usedEdge == 1)
        continue;
      else
      {
        edgeValueCheck.push_back(edgeValue);
      }

      int numPoints = edgeSize-1;
      if (numPoints > 4)
      {
        std::vector<double> lengthRatio(edgeSize, 0.0);

        std::vector<XYZ> inputNodes(edgeSize);
        std::vector<XYZ> outputNodes(edgeSize);

        const int sampleSize = 1000;
        std::vector<XYZ> outputRes(sampleSize);

        for (int k=0; k<edgeSize; k++)
        {
          int pointId = allRegions[i].BoundaryEdges[j][k];
          double pt[3];
          pd->GetPoint(pointId, pt);
          inputNodes[k].x = pt[0];
          inputNodes[k].y = pt[1];
          inputNodes[k].z = pt[2];
        }

        int deg = 4;
        std::vector<int> knots(numPoints+deg+1);

        vtkSVGeneralUtils::SplineKnots(knots, numPoints, deg);

        double totalLength = 0.0;

        for (int k = 1; k < edgeSize; k++)
        {

          int pointId = allRegions[i].BoundaryEdges[j][k];
          int prevPointId = allRegions[i].BoundaryEdges[j][k-1];

          double pt0[3], pt1[3];
          pd->GetPoint(pointId, pt0);
          pd->GetPoint(prevPointId, pt1);

          totalLength += vtkSVMathUtils::Distance(pt0, pt1);
        }

        double tempLength = 0.0;
        for (int k = 1; k < edgeSize; k++)
        {
          int pointId = allRegions[i].BoundaryEdges[j][k];
          int prevPointId = allRegions[i].BoundaryEdges[j][k-1];

          double pt0[3], pt1[3];
          pd->GetPoint(pointId, pt0);
          pd->GetPoint(prevPointId, pt1);

          tempLength += vtkSVMathUtils::Distance(pt0, pt1);

          lengthRatio[k] = tempLength / totalLength;
        }

        SplineCurve(inputNodes, numPoints, knots, deg, outputRes, sampleSize);

        double minDist = VTK_SV_LARGE_DOUBLE;
        int tempCount=0;
        for (int k = 0; k < edgeSize; k++)
        {
          minDist = VTK_SV_LARGE_DOUBLE;
          int pointId = allRegions[i].BoundaryEdges[j][k];
          double pt[3];
          pd->GetPoint(pointId, pt);
          for (int l = 0; l < sampleSize; l++)
          {
            double outputPt[3];
            outputPt[0] = outputRes[l].x;
            outputPt[1] = outputRes[l].y;
            outputPt[2] = outputRes[l].z;

            double dist = vtkSVMathUtils::Distance(pt, outputPt);

            if (dist < minDist)
            {
              minDist = dist;
              tempCount = l;
            }

          }

          double newPoint[3];
          newPoint[0] = outputRes[tempCount].x;
          newPoint[1] = outputRes[tempCount].y;
          newPoint[2] = outputRes[tempCount].z;

          pd->GetPoints()->SetPoint(pointId, newPoint);
        }
      }
    }
  }
  return SV_OK;
}

// ----------------------
// GetCellRingNeighbors
// ----------------------
int vtkSVGeneralUtils::GetCellRingNeighbors(vtkPolyData *pd, vtkIdList *cellIds,
                                            int ringNumber,
                                            int totNumberOfRings,
                                            std::vector<std::vector<int> > &neighbors)
{
  // Number of cells
  int numCells = cellIds->GetNumberOfIds();

  for (int i=0; i<numCells; i++)
  {
    // temporary node vec
    std::vector<int> tmpNodes;
    int iSize = neighbors[i].size();

    for (int j=0; j<iSize; j++)
    {
      // Get neighbor cell points
      int neiCellId = neighbors[i][j];
      vtkIdType *pts, npts;
      pd->GetCellPoints(neiCellId, npts, pts);

      // Loop around cell points
      for (int k=0; k<npts; k++)
      {
        int tmpNode = pts[k];
        int kSize   = tmpNodes.size();

        int kk = 0;
        for (kk=0; kk<kSize; kk++)
        {
          if (tmpNode == tmpNodes[kk])
          {
            break;
          }
        }
        if (kk == kSize)
        {
          tmpNodes.push_back(tmpNode);
        }
      }
    }

    // Now find neighbor elems
    iSize = tmpNodes.size();

    for (int j=0; j<iSize; j++)
    {
      int tmpNode = tmpNodes[j];

      vtkNew(vtkIdList, pointCellIds);
      pd->GetPointCells(tmpNode, pointCellIds);
      for (int k=0; k<pointCellIds->GetNumberOfIds(); k++)
      {
        int tmpCell = pointCellIds->GetId(k);
        int kSize =   neighbors[i].size();

        int kk=0;
        for (kk=0; kk<kSize; kk++)
        {
          if (tmpCell == neighbors[i][kk])
          {
            break;
          }
        }
        if (kk == kSize)
        {
          neighbors[i].push_back(tmpCell);
        }
      }
    }
  }

  if (ringNumber < totNumberOfRings)
  {
    ringNumber++;
    vtkSVGeneralUtils::GetCellRingNeighbors(pd, cellIds, ringNumber, totNumberOfRings, neighbors);
  }

  return SV_OK;
}

// ----------------------
// GetCellDirectNeighbors
// ----------------------
int vtkSVGeneralUtils::GetCellDirectNeighbors(vtkPolyData *pd,
                                              std::vector<std::vector<int> > &neighbors,
                                              std::vector<int> &numNeighbors)
{

  int numCells = pd->GetNumberOfCells();
  pd->BuildLinks();

  neighbors.clear();
  numNeighbors.clear();

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    // count number of edge neighbors
    int directNeiCount = 0;
    std::vector<int> neighborCells;

    // Get cell points
    vtkIdType *pts, npts;
    pd->GetCellPoints(i, npts, pts);

    // Get cell edge neighbors
    for (int j=0; j<npts; j++)
    {
      int ptId0 = pts[j];
      int ptId1 = pts[(j+1)%npts];

      // Get cell edge neighbors
      vtkNew(vtkIdList, cellEdgeNeighbors);
      pd->GetCellEdgeNeighbors(i, ptId0, ptId1, cellEdgeNeighbors);
      directNeiCount += cellEdgeNeighbors->GetNumberOfIds();
      for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
      {
        neighborCells.push_back(cellEdgeNeighbors->GetId(k));
      }
    }
    neighbors.push_back(neighborCells);
    numNeighbors.push_back(directNeiCount);
  }

  return SV_OK;
}



// ----------------------
// CorrectCellBoundaries
// ----------------------
int vtkSVGeneralUtils::CorrectCellBoundaries(vtkPolyData *pd, std::string cellArrayName )
{
  // Get current cell ids
  vtkDataArray *cellIds = pd->GetCellData()->GetArray(cellArrayName.c_str());

  // Num cells
  pd->BuildLinks();
  int numCells = pd->GetNumberOfCells();

  // Set up array to keep track of temp cell ids, will be different than
  // cellIds if disconnected regions of same value
  vtkNew(vtkIntArray, tmpIds);
  tmpIds->SetNumberOfTuples(numCells);
  tmpIds->FillComponent(0, -1);

  // Set count var
  int regionCount =0;

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    // If not set yet
    if (tmpIds->GetTuple1(i) == -1)
    {
      tmpIds->SetTuple1(i, regionCount);

      // Count cells in connected region
      int count = 1;
      vtkNew(vtkIdList, queue);
      queue->InsertId(0, i);

      // Loop through updating count
      for (int j=0; j<count; j++)
      {
        // Get Cell points
        vtkIdType npts, *pts;
        pd->GetCellPoints(queue->GetId(j), npts, pts);

        // Loop through cell points
        for (int k=0; k<npts; k++)
        {
          int ptId0 = pts[k];
          int ptId1 = pts[(k+1)%npts];

          // Get cell edge neighbors
          vtkNew(vtkIdList, cellEdgeNeighbors);
          pd->GetCellEdgeNeighbors(queue->GetId(j), ptId0, ptId1, cellEdgeNeighbors);

          // Check val of cell edge neighbors
          for (int l=0; l<cellEdgeNeighbors->GetNumberOfIds(); l++)
          {
            int cellEdgeNeighbor = cellEdgeNeighbors->GetId(l);
            if (tmpIds->GetTuple1(cellEdgeNeighbor) == -1 &&
                cellIds->GetTuple1(i) == cellIds->GetTuple1(cellEdgeNeighbor))
            {
              // Update cell val, count
              tmpIds->SetTuple1(cellEdgeNeighbor, regionCount);
              queue->InsertNextId(cellEdgeNeighbor);
              count++;
            }
          }
        }
      }
      regionCount++;
    }
  }

  int allGood = 0;
  int iter = 0;
  int maxIters = 100;

  while(!allGood && iter < maxIters)
  {
    allGood = 1;
    // Loop through cells again
    for (int i=0; i<numCells; i++)
    {

      // get direct neighbor value count
      vtkNew(vtkIdList, neiCellIds);
      vtkNew(vtkIdList, neiTmpIds);

      // Get cell points
      vtkIdType npts, *pts;
      pd->GetCellPoints(i, npts, pts);

      // Loop through cell points
      for (int j=0; j<npts; j++)
      {
        int ptId0 = pts[j];
        int ptId1 = pts[(j+1)%npts];

        // Get cell edge neighbors
        vtkNew(vtkIdList, cellEdgeNeighbors);
        pd->GetCellEdgeNeighbors(i, ptId0, ptId1, cellEdgeNeighbors);

        // loop through neighbors
        for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
        {
          int cellEdgeNeighbor = cellEdgeNeighbors->GetId(k);

          // Check to see if equal to region val
          // Important for these cases! Adding to make sure the value is not -1
          if (tmpIds->GetTuple1(cellEdgeNeighbor) != tmpIds->GetTuple1(i) &&
              cellIds->GetTuple1(cellEdgeNeighbor) != -1)
          {
            neiCellIds->InsertNextId(cellIds->GetTuple1(cellEdgeNeighbor));
            neiTmpIds->InsertNextId(tmpIds->GetTuple1(cellEdgeNeighbor));
          }
        }
      }

      // If we found a cell surrounded by cells of another val, we can update
      vtkSortDataArray::Sort(neiTmpIds);
      int neiSize = neiTmpIds->GetNumberOfIds();
      if (neiSize == 2)
      {
        if (neiTmpIds->GetId(0) == neiTmpIds->GetId(1))
        {
          allGood = 0;
          int maxVal, maxCount;
          vtkSVGeneralUtils::GetMostOccuringVal(neiCellIds, maxVal, maxCount);

          cellIds->SetTuple1(i, maxVal);
          tmpIds->SetTuple1(i, neiTmpIds->GetId(1));
        }
      }
      else if (neiSize >= 3)
      {
        if ((neiTmpIds->GetId(0) == neiTmpIds->GetId(1) ||
             neiTmpIds->GetId(1) == neiTmpIds->GetId(2)))
        {
          allGood = 0;
          int maxVal, maxCount;
          vtkSVGeneralUtils::GetMostOccuringVal(neiCellIds, maxVal, maxCount);

          cellIds->SetTuple1(i, maxVal);
          tmpIds->SetTuple1(i, neiTmpIds->GetId(1));
        }
      }
    }
    iter++;
  }

  return SV_OK;
}

// ----------------------
// CorrectSpecificCellBoundaries
// ----------------------
int vtkSVGeneralUtils::CorrectSpecificCellBoundaries(vtkPolyData *pd, std::string cellArrayName, vtkIdList *targetRegions)
{
  // Get current cell ids
  vtkDataArray *cellIds = pd->GetCellData()->GetArray(cellArrayName.c_str());

  // Num cells
  pd->BuildLinks();
  int numCells = pd->GetNumberOfCells();

  // Set up array to keep track of temp cell ids, will be different than
  // cellIds if disconnected regions of same value
  vtkNew(vtkIntArray, tmpIds);
  tmpIds->SetNumberOfTuples(numCells);
  tmpIds->FillComponent(0, -1);

  // Set count var
  int regionCount =0;

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    // If not set yet
    if (tmpIds->GetTuple1(i) == -1)
    {
      tmpIds->SetTuple1(i, regionCount);

      // Count cells in connected region
      int count = 1;
      vtkNew(vtkIdList, queue);
      queue->InsertId(0, i);

      // Loop through updating count
      for (int j=0; j<count; j++)
      {
        // Get Cell points
        vtkIdType npts, *pts;
        pd->GetCellPoints(queue->GetId(j), npts, pts);

        // Loop through cell points
        for (int k=0; k<npts; k++)
        {
          int ptId0 = pts[k];
          int ptId1 = pts[(k+1)%npts];

          // Get cell edge neighbors
          vtkNew(vtkIdList, cellEdgeNeighbors);
          pd->GetCellEdgeNeighbors(queue->GetId(j), ptId0, ptId1, cellEdgeNeighbors);

          // Check val of cell edge neighbors
          for (int l=0; l<cellEdgeNeighbors->GetNumberOfIds(); l++)
          {
            int cellEdgeNeighbor = cellEdgeNeighbors->GetId(l);
            if (tmpIds->GetTuple1(cellEdgeNeighbor) == -1 &&
                cellIds->GetTuple1(i) == cellIds->GetTuple1(cellEdgeNeighbor))
            {
              // Update cell val, count
              tmpIds->SetTuple1(cellEdgeNeighbor, regionCount);
              queue->InsertNextId(cellEdgeNeighbor);
              count++;
            }
          }
        }
      }
      regionCount++;
    }
  }

  int allGood = 0;
  int iter = 0;
  int maxIters = 100;

  while(!allGood && iter<maxIters)
  {
    allGood = 1;
    // Loop through cells again
    for (int i=0; i<numCells; i++)
    {

      // get direct neighbor value count
      vtkNew(vtkIdList, neiCellIds);
      vtkNew(vtkIdList, neiTmpIds);

      // Get cell points
      vtkIdType npts, *pts;
      pd->GetCellPoints(i, npts, pts);

      // Loop through cell points
      for (int j=0; j<npts; j++)
      {
        int ptId0 = pts[j];
        int ptId1 = pts[(j+1)%npts];

        // Get cell edge neighbors
        vtkNew(vtkIdList, cellEdgeNeighbors);
        pd->GetCellEdgeNeighbors(i, ptId0, ptId1, cellEdgeNeighbors);

        // loop through neighbors
        for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
        {
          int cellEdgeNeighbor = cellEdgeNeighbors->GetId(k);

          // Check to see if equal to region val
          // Important for these cases! Adding to make sure the value is not -1
          if (tmpIds->GetTuple1(cellEdgeNeighbor) != tmpIds->GetTuple1(i) &&
              cellIds->GetTuple1(cellEdgeNeighbor) != -1)
          {
            int cellValue = cellIds->GetTuple1(cellEdgeNeighbor);
            if (targetRegions->IsId(cellValue) != -1)
            {
              neiCellIds->InsertNextId(cellValue);
              neiTmpIds->InsertNextId(tmpIds->GetTuple1(cellEdgeNeighbor));
            }
          }
        }
      }

      // If we found a cell surrounded by cells of another val, we can update
      vtkSortDataArray::Sort(neiTmpIds);
      int neiSize = neiTmpIds->GetNumberOfIds();
      if (neiSize == 2)
      {
        if (neiTmpIds->GetId(0) == neiTmpIds->GetId(1))
        {
          allGood = 0;
          int maxVal, maxCount;
          vtkSVGeneralUtils::GetMostOccuringVal(neiCellIds, maxVal, maxCount);

          cellIds->SetTuple1(i, maxVal);
          tmpIds->SetTuple1(i, neiTmpIds->GetId(1));
        }
      }
      else if (neiSize >= 3)
      {
        if ((neiTmpIds->GetId(0) == neiTmpIds->GetId(1) ||
             neiTmpIds->GetId(1) == neiTmpIds->GetId(2)))
        {
          allGood = 0;
          int maxVal, maxCount;
          vtkSVGeneralUtils::GetMostOccuringVal(neiCellIds, maxVal, maxCount);

          cellIds->SetTuple1(i, maxVal);
          tmpIds->SetTuple1(i, neiTmpIds->GetId(1));
        }
      }
    }
    iter++;
  }

  return SV_OK;
}

// ----------------------
// ComputeRotationMatrix
// ----------------------
int vtkSVGeneralUtils::ComputeRotationMatrix(const double from_x[3],
                                             const double from_y[3],
                                             const double from_z[3],
                                             const double to_x[3],
                                             const double to_y[3],
                                             const double to_z[3],
                                             double rotMatrix[9])
{
  rotMatrix[0] = to_x[0]*from_x[0] +
                 to_x[1]*from_x[1] +
                 to_x[2]*from_x[2];
  rotMatrix[1] = to_x[0]*from_y[0] +
                 to_x[1]*from_y[1] +
                 to_x[2]*from_y[2];
  rotMatrix[2] = to_x[0]*from_z[0] +
                 to_x[1]*from_z[1] +
                 to_x[2]*from_z[2];

  rotMatrix[3] = to_y[0]*from_x[0] +
                 to_y[1]*from_x[1] +
                 to_y[2]*from_x[2];
  rotMatrix[4] = to_y[0]*from_y[0] +
                 to_y[1]*from_y[1] +
                 to_y[2]*from_y[2];
  rotMatrix[5] = to_y[0]*from_z[0] +
                 to_y[1]*from_z[1] +
                 to_y[2]*from_z[2];

  rotMatrix[6] = to_z[0]*from_x[0] +
                 to_z[1]*from_x[1] +
                 to_z[2]*from_x[2];
  rotMatrix[7] = to_z[0]*from_y[0] +
                 to_z[1]*from_y[1] +
                 to_z[2]*from_y[2];
  rotMatrix[8] = to_z[0]*from_z[0] +
                 to_z[1]*from_z[1] +
                 to_z[2]*from_z[2];

  return SV_OK;
}

// ----------------------
// FindPointMatchingValues
// ----------------------
int vtkSVGeneralUtils::FindPointMatchingValues(vtkPointSet *ps, std::string arrayName, vtkIdList *matchingVals, int &returnPtId)
{
  int closeMatch = -1;
  for (int i=0; i<ps->GetNumberOfPoints(); i++)
  {
    vtkNew(vtkIdList, pointCellValues);
    vtkSVGeneralUtils::GetPointCellsValues(ps, arrayName, i, pointCellValues);
    int prevNum = pointCellValues->GetNumberOfIds();
    pointCellValues->IntersectWith(matchingVals);

    if (pointCellValues->GetNumberOfIds() == matchingVals->GetNumberOfIds() &&
        prevNum == pointCellValues->GetNumberOfIds())
    {
      // We found it!
      returnPtId = i;
      return SV_OK;
    }
    else if (pointCellValues->GetNumberOfIds() == matchingVals->GetNumberOfIds())
      closeMatch = i;
    else if (prevNum == pointCellValues->GetNumberOfIds() && prevNum == 4)
      closeMatch = i;
  }
  // TODO CHECK IF WE WANT THIS
  if (closeMatch != -1)
    returnPtId = closeMatch;

  return SV_ERROR;
}

// ----------------------
// FindPointsMatchingValues
// ----------------------
int vtkSVGeneralUtils::FindPointsMatchingValues(vtkPointSet *ps, std::string arrayName, vtkIdList *matchingVals, vtkIdList *returnPtIds)
{
  int found = 0;
  for (int i=0; i<ps->GetNumberOfPoints(); i++)
  {
    vtkNew(vtkIdList, pointCellValues);
    vtkSVGeneralUtils::GetPointCellsValues(ps, arrayName, i, pointCellValues);
    int prevNum = pointCellValues->GetNumberOfIds();
    pointCellValues->IntersectWith(matchingVals);

    if (pointCellValues->GetNumberOfIds() == matchingVals->GetNumberOfIds() &&
        prevNum == pointCellValues->GetNumberOfIds())
    {
      // We found it!
      returnPtIds->InsertNextId(i);
      found = 1;
    }
  }

  if (!found)
    return SV_ERROR;

  return SV_OK;
}
