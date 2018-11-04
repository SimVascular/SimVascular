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

#include "vtkSVFindGeodesicPath.h"

#include "vtkCellData.h"
#include "vtkConnectivityFilter.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkDijkstraGraphGeodesicPath.h"
#include "vtkDoubleArray.h"
#include "vtkErrorCode.h"
#include "vtkFeatureEdges.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"

#include <iostream>
#include <cmath>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVFindGeodesicPath);

// ----------------------
// Constructor
// ----------------------
vtkSVFindGeodesicPath::vtkSVFindGeodesicPath()
{
  this->SetNumberOfInputPorts(1);
  this->AddPathBooleanArray      = 0;
  this->RemoveInternalIds        = 1;
  this->RepelCloseBoundaryPoints = 0;

  this->StartPtId = -1;
  this->EndPtId   = -1;

  for (int i=0; i<3; i++)
    this->ClosePt[i] = 0.0;

  this->DijkstraArrayName = NULL;
  this->InternalIdsArrayName = NULL;
  this->PathBooleanArrayName = NULL;

  this->WorkPd       = vtkPolyData::New();
  this->BoundaryPd   = vtkPolyData::New();
  this->PathIds      = vtkIdList::New();
  this->PathBoolean  = vtkIntArray::New();
}

// ----------------------
// Destructor
// ----------------------
vtkSVFindGeodesicPath::~vtkSVFindGeodesicPath()
{
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }
  if (this->PathIds != NULL)
  {
    this->PathIds->Delete();
    this->PathIds = NULL;
  }
  if (this->BoundaryPd != NULL)
  {
    this->BoundaryPd->Delete();
    this->BoundaryPd = NULL;
  }
  if (this->PathBoolean != NULL)
  {
    this->PathBoolean->Delete();
    this->PathBoolean = NULL;
  }

  if (this->DijkstraArrayName)
  {
    delete [] this->DijkstraArrayName;
    this->DijkstraArrayName = NULL;
  }
  if (this->InternalIdsArrayName)
  {
    delete [] this->InternalIdsArrayName;
    this->InternalIdsArrayName = NULL;
  }
  if (this->PathBooleanArrayName)
  {
    delete [] this->PathBooleanArrayName;
    this->PathBooleanArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVFindGeodesicPath::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "Start Point Id: " <<
    this->StartPtId << "\n";
  os << indent << "End Point Id: " <<
    this->EndPtId << "\n";
  os << indent << "Add path Boolean array: " <<
    this->AddPathBooleanArray << "\n";
  os << indent << "Repel close boundary points: " <<
    this->RepelCloseBoundaryPoints << "\n";
  os << indent << "Boundary close point: " <<
    this->ClosePt[0] << " " << this->ClosePt[1] << " " << this->ClosePt[2] << "\n";
  if (this->InternalIdsArrayName != NULL)
    os << indent << "Internal Ids array name: " << this->InternalIdsArrayName << "\n";
  if (this->DijkstraArrayName != NULL)
    os << indent << "Dijkstra distance array name: " << this->DijkstraArrayName << "\n";
  if (this->PathBooleanArrayName != NULL)
    os << indent << "Path boolean array name: " << this->PathBooleanArrayName << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVFindGeodesicPath::RequestData(
                                 vtkInformation *vtkNotUsed(request),
                                 vtkInformationVector **inputVector,
                                 vtkInformationVector *outputVector)
{
  // Get the input and output
  vtkPolyData *input  = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  // Copy the input to operate on
  this->WorkPd->DeepCopy(input);

  // Prep work for filter
  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Prep of filter failed");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  // Run the filter
  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Filter failed");
    this->SetErrorCode(vtkErrorCode::UserError + 2);
    return SV_ERROR;
  }

  if (this->RemoveInternalIds)
  {
    this->WorkPd->GetPointData()->RemoveArray(this->InternalIdsArrayName);
    this->WorkPd->GetCellData()->RemoveArray(this->InternalIdsArrayName);
  }
  output->DeepCopy(this->WorkPd);
  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVFindGeodesicPath::PrepFilter()
{
  // Get number of cells and points
  vtkIdType numPolys  = this->WorkPd->GetNumberOfPolys();
  vtkIdType numPoints = this->WorkPd->GetNumberOfPoints();

  //Check the input to make sure it is there
  if (numPolys < 1)
  {
    vtkErrorMacro("No input!");
    return SV_ERROR;
  }

  // Check is start point id is given
  if (this->StartPtId == -1)
  {
    vtkErrorMacro("No input start id given");
    return SV_ERROR;
  }

  // Check start pt id
  if (this->StartPtId > numPoints)
  {
    vtkErrorMacro("Start id is greater than number of pts on pd");
    return SV_ERROR;
  }

  // Check end pt id
  if (this->EndPtId > numPoints)
  {
    vtkErrorMacro("End id is greater than number of pts on pd");
    return SV_ERROR;
  }

  // Check if dijkstra array name is given
  if (!this->DijkstraArrayName)
  {
    vtkDebugMacro("Dijkstra Array Name not given, setting to DijkstraDistance");
    this->DijkstraArrayName = new char[strlen("DijkstraDistance") + 1];
    strcpy(this->DijkstraArrayName, "DijkstraDistance");
  }

  // Check if array dijkstra is already on pd
  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 0, this->DijkstraArrayName))
  {
    this->WorkPd->GetPointData()->RemoveArray(this->DijkstraArrayName);
  }

  // Check if internal id array name is given
  if (!this->InternalIdsArrayName)
  {
    vtkDebugMacro("Internal Ids Array Name not given, setting to InternalIds");
    this->InternalIdsArrayName = new char[strlen("InternalIds") + 1];
    strcpy(this->InternalIdsArrayName, "InternalIds");
  }
  // Check if array internal ids is already on pd
  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 0, this->InternalIdsArrayName))
  {
    this->RemoveInternalIds = 0;
  }
  else
  {
    vtkSVGeneralUtils::GiveIds(this->WorkPd, this->InternalIdsArrayName);
  }

  // Check if path boolean array name is given
  if (!this->PathBooleanArrayName)
  {
    vtkDebugMacro("PathBoolean Array Name not given, setting to PathBoolean");
    this->PathBooleanArrayName = new char[strlen("PathBoolean") + 1];
    strcpy(this->PathBooleanArrayName, "PathBoolean");
  }
  // Check if array path booleana is already on pd
  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 0, this->PathBooleanArrayName))
  {
    this->WorkPd->GetPointData()->RemoveArray(this->PathBooleanArrayName);
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVFindGeodesicPath::RunFilter()
{
  // Check if we need to get do an intial run of dijkstra
  int runItChrisBrown = 0;
  if (this->EndPtId != -1 || this->AddPathBooleanArray)
    runItChrisBrown = 1;

  // If End point id not provided, we ned to get a point on our own!
  if (this->EndPtId == -1)
  {
    if (this->FindClosestBoundaryPoint() != SV_OK)
    {
      vtkErrorMacro("Error finding a point close on the boundary");
      return SV_ERROR;
    }
  }

  // If asked to repel close boundary points, we need to repel these points!
  vtkNew(vtkPoints, repelPoints);
  if (this->RepelCloseBoundaryPoints)
  {
    if (this->GetCloseBoundaryPoints(this->StartPtId, this->EndPtId, repelPoints) != SV_OK)
    {
      vtkErrorMacro("Error getting close points on the boundary to repel");
      return SV_ERROR;
    }
  }

  // We need to run it chris brown
  if (runItChrisBrown)
  {
    // Run the filter
    if (this->RunDijkstra(repelPoints) != SV_OK)
    {
      vtkErrorMacro("vtkDijkstraGraphGeodesicPath failed");
      return SV_ERROR;
    }

    // If we are asked to add an array, do this with the path ids
    if (this->AddPathBooleanArray)
    {
      // Set up data array
      int numPoints = this->WorkPd->GetNumberOfPoints();
      this->PathBoolean->SetNumberOfTuples(numPoints);
      this->PathBoolean->FillComponent(0, 0);

      // Loop through path ids and set value to one on data array
      for (int i=0; i<this->PathIds->GetNumberOfIds(); i++)
      {
        this->PathBoolean->SetValue(this->PathIds->GetId(i), 1);
      }

      // Attach array to surface
      this->PathBoolean->SetName(this->PathBooleanArrayName);
      this->WorkPd->GetPointData()->AddArray(this->PathBoolean);
    }
  }

  return SV_OK;
}

// ----------------------
// FindClosestBoundaryPoints
// ----------------------
int vtkSVFindGeodesicPath::FindClosestBoundaryPoint()
{
  // Get closest boundary point by runnning initial dijkstra
  if (this->RunDijkstra(NULL) != SV_OK)
  {
    vtkErrorMacro("vtkDijkstraGraphGeodesicPath failed");
    return SV_ERROR;
  }

  // Get the boundary edges
  vtkNew(vtkFeatureEdges, boundaries);
  boundaries->SetInputData(this->WorkPd);
  boundaries->BoundaryEdgesOn();
  boundaries->FeatureEdgesOff();
  boundaries->NonManifoldEdgesOff();
  boundaries->ManifoldEdgesOff();
  boundaries->Update();

  // Get the boundary edge closest to close point
  vtkSVGeneralUtils::GetClosestPointConnectedRegion(boundaries->GetOutput(),
                                                    this->ClosePt,
                                                    this->BoundaryPd);

  // Get all the weights from dijkstra on boundary
  vtkDataArray *passedWeights = this->BoundaryPd->GetPointData()->GetArray(this->DijkstraArrayName);
  vtkDataArray *internalIds   = this->BoundaryPd->GetPointData()->GetArray(this->InternalIdsArrayName);

  // Loop through all points on the boundary
  int numPoints = this->BoundaryPd->GetNumberOfPoints();
  double minVal = 1.0e10;
  int minId = -1;
  for (int i=0; i<numPoints; i++)
  {
    // Check the value of the point
    double val = passedWeights->GetTuple1(i);

    // We are looking for the closest point, so minimum value
    if (val < minVal)
    {
      minVal = val;
      minId  = internalIds->GetTuple1(i);
    }
  }

  // Set end pt id to the point with the minimum distance
  this->EndPtId = minId;
  std::cout << "What is closest: " << minId << endl;

  return SV_OK;
}

// ----------------------
// RunDijkstra
// ----------------------
int vtkSVFindGeodesicPath::RunDijkstra(vtkPoints *repelPoints)
{
  // Set up dijkstra filter
  vtkNew(vtkDijkstraGraphGeodesicPath, dijkstra);
  dijkstra->SetInputData(this->WorkPd);
  dijkstra->SetStartVertex(this->StartPtId);

  // Add repel points
  if (repelPoints != NULL)
  {
    if (repelPoints->GetNumberOfPoints() != 0)
    {
      dijkstra->RepelPathFromVerticesOn();
      dijkstra->SetRepelVertices(repelPoints);
    }
  }
  // Add end pt if not -1
  if (this->EndPtId != -1)
    dijkstra->SetEndVertex(this->EndPtId);

  // Tell dijkstra to keep going and calculate at all points
  dijkstra->StopWhenEndReachedOff();
  dijkstra->Update();

  // Get weights from points and add on the working surface
  vtkNew(vtkDoubleArray, tmpWeights);
  dijkstra->GetCumulativeWeights(tmpWeights);
  tmpWeights->SetName(this->DijkstraArrayName);
  this->WorkPd->GetPointData()->AddArray(tmpWeights);

  // Get path ids as well
  this->PathIds->DeepCopy(dijkstra->GetIdList());

  return SV_OK;
}

// ----------------------
// GetCloseBoundaryPoints
// ----------------------
int vtkSVFindGeodesicPath::GetCloseBoundaryPoints(const int startPtId,
                                                  const int endPtId,
                                                  vtkPoints *repelPoints)
{
  // Get boundary edges
  vtkNew(vtkFeatureEdges, boundaries);
  boundaries->SetInputData(this->WorkPd);
  boundaries->BoundaryEdgesOn();
  boundaries->FeatureEdgesOff();
  boundaries->NonManifoldEdgesOff();
  boundaries->ManifoldEdgesOff();
  boundaries->Update();

  // Get points of start and end pt id
  double startPt[3], endPt[3];
  this->WorkPd->GetPoint(startPtId, startPt);
  this->WorkPd->GetPoint(endPtId, endPt);

  // Get boundary closest to start point
  vtkNew(vtkPolyData, startPtBoundary);
  vtkSVGeneralUtils::GetClosestPointConnectedRegion(boundaries->GetOutput(),
                                                    startPt,
                                                    startPtBoundary);

  // Get the neighboring points on boundary and add to repelPoints
  if (this->GetNeighborBoundaryPoints(startPtId, startPtBoundary, repelPoints) != SV_OK)
  {
    vtkErrorMacro("Error getting neighbor boundary points");
    return SV_ERROR;
  }

  // Get boundary closest to end point
  vtkNew(vtkPolyData, endPtBoundary);
  vtkSVGeneralUtils::GetClosestPointConnectedRegion(boundaries->GetOutput(),
                                                    endPt,
                                                    endPtBoundary);

  // Get the neighboring points on boundary and add to repelPoints
  if (this->GetNeighborBoundaryPoints(endPtId, endPtBoundary, repelPoints) != SV_OK)
  {
    vtkErrorMacro("Error getting neighbor boundary points");
    return SV_ERROR;
  }

  return SV_OK;
}
// ----------------------
// GetNeighborBoundaryPoints
// ----------------------
int vtkSVFindGeodesicPath::GetNeighborBoundaryPoints(const int ptId,
                                                     vtkPolyData *pd,
                                                     vtkPoints *repelPoints)
{
  // Get internal ids data array
  vtkDataArray *internalIds = this->WorkPd->GetPointData()->
    GetArray(this->InternalIdsArrayName);
  vtkDataArray *eInternalIds = pd->GetPointData()->
    GetArray(this->InternalIdsArrayName);

  // Get point cells
  vtkNew(vtkIdList, cells);
  this->WorkPd->GetPointCells(ptId, cells);

  // This is kind of muy important. We only want to add the neighboring points
  // to the repel list if they aren't part of the same cell. If the other
  // two points share a triangle with ptId, then if we repel those points,
  // the path would have nowhere to go
  int offLimits[2]; offLimits[0] = -1; offLimits[1] = -1;
  int count = 0;
  if (cells->GetNumberOfIds() == 1)
  {
    // Get cell points
    vtkIdType npts, *pts;
    this->WorkPd->GetCellPoints(cells->GetId(0), npts, pts);

    // Loop through neighbor points
    for (int j=0; j<npts; j++)
    {
      // If the points are not ptId, add to offLimits
      if (pts[j] != ptId)
        offLimits[count++] = eInternalIds->LookupValue(int(internalIds->GetTuple1(pts[j])));
    }
  }

  // Get boundary pt id of the given ptId and make sure it exists
  int bId = eInternalIds->LookupValue(int(internalIds->GetTuple1(ptId)));
  if (bId != -1)
  {
    if (bId >= pd->GetNumberOfPoints())
    {
      vtkErrorMacro("Point id is not valid " << ptId);
      return SV_ERROR;
    }

    // Loop through all points in the full polydata
    for (int i=0; i<pd->GetNumberOfPoints(); i++)
    {
      // if this point is not our ptId and it has not been desgnated as
      // off limits, we can add to repel points
      if (i != bId && i != offLimits[0] && i != offLimits[1])
      {
        double pt[3];
        pd->GetPoint(i, pt);
        repelPoints->InsertNextPoint(pt);
      }
    }
  }

  return SV_OK;
}
