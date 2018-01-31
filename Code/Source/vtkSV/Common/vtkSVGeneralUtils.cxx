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
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"
#include "vtkThreshold.h"
#include "vtkTransform.h"
#include "vtkTransformPolyDataFilter.h"
#include "vtkTriangleFilter.h"
#include "vtkDataSet.h"
#include "vtkUnstructuredGrid.h"

#include <algorithm>

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
// CheckSurface
// ----------------------
/*
 * \details TODO: Want to make more checks
 */
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
      //vtkErrorMacro("Surface contains elements that aren't triangles");
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
        //vtkErrorMacro("Surface contains triangles with multiple neighbors, not manifold");
        return SV_ERROR;
      }
    }
  }
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
// GiveIds
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
int vtkSVGeneralUtils::GetPointCellsValues(vtkPolyData *pd, std::string arrayName,
                                           const int pointId, vtkIdList *valList)
{
  // Get data from pd
  vtkDataArray *valArray =
    pd->GetCellData()->GetArray(arrayName.c_str());
  valList->Reset();

  // Get point cells
  vtkNew(vtkIdList, cellIds);
  pd->GetPointCells(pointId, cellIds);

  // Loop through and check each point
  for (int i=0; i<cellIds->GetNumberOfIds(); i++)
  {
    int groupValue = valArray->GetTuple1(cellIds->GetId(i));

    // Only adding to list if value is not -1
    if (valList->IsId(groupValue) == -1)
      valList->InsertNextId(groupValue);
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
        edgeNeighbors->InsertValue(edgeId, neighborCellId);
        if (weight < 0)
        {
          //vtkWarningMacro("Negative weight on edge between cells " << i <<
          //  " and "<< neighborCellId << ": " << weight);
        }
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
// GetAllMapKeys
// ----------------------
int vtkSVGeneralUtils::GetAllMapKeys(std::multimap<int, int> &map,
                                     std::list<int> &list)
{
  std::multimap<int, int>::iterator it = map.begin();

  for (int i=0; it != map.end(); ++it)
  {
    list.push_back(it->first);
  }
  list.unique();

  return SV_OK;
}

// ----------------------
// GetAllMapValues
// ----------------------
int vtkSVGeneralUtils::GetAllMapValues(std::multimap<int, int> &map,
                                       std::list<int> &list)
{
  std::multimap<int, int>::iterator it = map.begin();

  for (int i=0; it != map.end(); ++it)
  {
    list.push_back(it->second);
  }
  list.unique();

  return SV_OK;
}

// ----------------------
// GetValuesFromMap
// ----------------------
int vtkSVGeneralUtils::GetValuesFromMap(std::multimap<int, int> &map,
                                        const int key,
                                        std::list<int> &list)
{
  std::multimap<int, int>::iterator it = map.begin();

  for (int i=0; it != map.end(); ++it)
  {
    if (it->first == key)
    {
      list.push_back(it->second);
    }
  }
  return SV_OK;
}

// ----------------------
// GetKeysFromMap
// ----------------------
int vtkSVGeneralUtils::GetKeysFromMap(std::multimap<int, int> &map,
                                                  const int value,
                                                  std::list<int> &list)
{
  std::multimap<int, int>::iterator it = map.begin();

  for (int i=0; it != map.end(); ++it)
  {
    if (it->second == value)
    {
      list.push_back(it->first);
    }
  }
  return SV_OK;
}

// ----------------------
// GetCommonValues
// ----------------------
int vtkSVGeneralUtils::GetCommonValues(std::multimap<int, int> &map,
                                                   const int keyA, const int keyB,
                                                   std::list<int> &returnList)
{
  std::list<int> listA, listB;
  vtkSVGeneralUtils::GetValuesFromMap(map, keyA, listA);
  vtkSVGeneralUtils::GetValuesFromMap(map, keyB, listB);
  vtkSVGeneralUtils::ListIntersection(listA, listB, returnList);

  return SV_OK;
}

// ----------------------
// GetUniqueNeighbors
// ----------------------
int vtkSVGeneralUtils::GetUniqueNeighbors(std::multimap<int, int> &map,
                                          const int key,
                                          std::list<int> &keyVals,
                                          std::list<int> &uniqueKeys)
{
  int numVals = keyVals.size();

  std::list<int>::iterator valit = keyVals.begin();
  for (int i=0; valit != keyVals.end(); ++valit)
  {
    std::list<int> valKeys;
    vtkSVGeneralUtils::GetKeysFromMap(map, *valit, valKeys);
    std::list<int>::iterator keyit = valKeys.begin();
    for (int j=0; keyit != valKeys.end(); ++keyit)
    {
      if (*keyit != key)
      {
        uniqueKeys.push_back(*keyit);
      }
    }
  }
  uniqueKeys.sort();
  uniqueKeys.unique();

  return SV_OK;
}

// ----------------------
// ListIntersection
// ----------------------
int vtkSVGeneralUtils::ListIntersection(std::list<int> &listA,
                                                    std::list<int> &listB,
                                                    std::list<int> &returnList)
{
  std::set_intersection(listA.begin(), listA.end(),
                        listB.begin(), listB.end(),
                        std::inserter(returnList,returnList.begin()));

  return SV_OK;
}
