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

#include "vtkSVBoundaryMapper.h"

#include "vtkCell.h"
#include "vtkCellData.h"
#include "vtkConnectivityFilter.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkErrorCode.h"
#include "vtkFeatureEdges.h"
#include "vtkIdFilter.h"
#include "vtkIdList.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"

#include <sstream>
#include <map>

// ----------------------
// Constructor
// ----------------------
vtkSVBoundaryMapper::vtkSVBoundaryMapper()
{
  this->RemoveInternalIds = 1;

  this->InitialPd     = vtkPolyData::New();
  this->BoundaryPd    = vtkPolyData::New();
  this->EdgeTable     = NULL;

  this->IsBoundary    = NULL;
  this->BoundaryIds   = NULL;
  this->Boundaries    = vtkPolyData::New();
  this->BoundaryLoop  = vtkPolyData::New();

  this->InternalIdsArrayName = NULL;

  this->ObjectXAxis[0] = 1.0;
  this->ObjectXAxis[1] = 0.0;
  this->ObjectXAxis[2] = 0.0;

  this->ObjectZAxis[0] = 0.0;
  this->ObjectZAxis[1] = 0.0;
  this->ObjectZAxis[2] = 1.0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVBoundaryMapper::~vtkSVBoundaryMapper()
{
  if (this->InitialPd != NULL)
  {
    InitialPd->Delete();
  }
  if (this->BoundaryPd != NULL)
  {
    BoundaryPd->Delete();
  }
  if (this->EdgeTable != NULL)
  {
    EdgeTable->Delete();
  }
  if (this->Boundaries != NULL)
  {
    this->Boundaries->Delete();
  }
  if (this->BoundaryLoop != NULL)
  {
    this->BoundaryLoop->Delete();
  }
  if (this->InternalIdsArrayName)
  {
    delete [] this->InternalIdsArrayName;
    this->InternalIdsArrayName = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVBoundaryMapper::RequestData(vtkInformation *vtkNotUsed(request),
                                   vtkInformationVector **inputVector,
                                   vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  //Copy the input to operate on
  this->InitialPd->DeepCopy(input);

  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Error when mapping");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Error when mapping");
    this->SetErrorCode(vtkErrorCode::UserError + 2);
    return SV_ERROR;
  }

  if (this->RemoveInternalIds)
  {
    this->BoundaryPd->GetPointData()->RemoveArray(this->InternalIdsArrayName);
    this->BoundaryPd->GetCellData()->RemoveArray(this->InternalIdsArrayName);
  }
  output->DeepCopy(this->BoundaryPd);

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVBoundaryMapper::PrepFilter()
{
  vtkIdType numPolys = this->InitialPd->GetNumberOfPolys();
  vtkIdType numPoints = this->InitialPd->GetNumberOfPoints();
  //Check the input to make sure it is there
  if (numPolys < 1)
  {
    vtkErrorMacro("No input!");
    return SV_ERROR;
  }

  //Check the input to make sure it is manifold and a triangulated surface
  if (vtkSVGeneralUtils::CheckSurface(this->InitialPd) != SV_OK)
  {
    vtkErrorMacro("Error when checking input surface");
    return SV_ERROR;
  }

  // Check if internal id array name is given
  if (!this->InternalIdsArrayName)
  {
    vtkDebugMacro("Internal Ids Array Name not given, setting to InternalIds");
    this->InternalIdsArrayName = new char[strlen("InternalIds") + 1];
    strcpy(this->InternalIdsArrayName, "InternalIds");
  }
  // Check if array internal ids is already on pd
  if (vtkSVGeneralUtils::CheckArrayExists(this->InitialPd, 0, this->InternalIdsArrayName))
  {
    this->RemoveInternalIds = 0;
  }
  else
    vtkSVGeneralUtils::GiveIds(this->InitialPd, this->InternalIdsArrayName);

  //Create the edge table for the input surface
  this->InitialPd->BuildLinks();

  if (this->EdgeTable->GetNumberOfEdges() == 0)
  {
    vtkErrorMacro("No Edges! Use SetEdgeTable");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVBoundaryMapper::RunFilter()
{
  // Find all boundaries
  if (this->FindBoundaries() != SV_OK)
  {
    vtkErrorMacro("Could not find boundaries");
    return SV_ERROR;
  }

  // Get the boundary loop to set
  if (this->GetBoundaryLoop() != SV_OK)
  {
    vtkErrorMacro("Error orienting boundary loop");
    return SV_ERROR;
  }

  // Set the boundaries!
  if (this->SetBoundaries() != SV_OK)
  {
    vtkErrorMacro("Error in mapping");
    return SV_ERROR;
  }
  return SV_OK;
}

// ----------------------
// GetBoundaryLoop
// ----------------------
int vtkSVBoundaryMapper::GetBoundaryLoop()
{
  // Initialize cell and cell ids
  vtkIdType nextCell;
  vtkNew(vtkIdList, cellIds);

  // Get point ids of boundary loop
  vtkDataArray *pointIds = this->Boundaries->GetPointData()->GetArray(this->InternalIdsArrayName);
  vtkDataArray *oPointIds = this->InitialPd->GetPointData()->GetArray(this->InternalIdsArrayName);

  // Get number of points and cells on boundary
  int numInterPts = this->Boundaries->GetNumberOfPoints();
  int numInterLines = this->Boundaries->GetNumberOfLines();
  this->Boundaries->BuildLinks();

  // Get value of start point
  int count = 0;
  vtkIdType startPt = pointIds->LookupValue(
    oPointIds->GetTuple1(this->BoundaryIds->GetValue(0)));
  vtkDebugMacro("Start Point is!: " <<this->BoundaryIds->GetValue(0));

  // Set the boundary loop points and point data and allocate space for cells
  this->BoundaryLoop->SetPoints(this->Boundaries->GetPoints());
  this->BoundaryLoop->GetPointData()->PassData(this->Boundaries->GetPointData());
  this->BoundaryLoop->Allocate(this->Boundaries->GetNumberOfCells(), 1000);
  vtkDebugMacro("The value on this is!: " <<  startPt);
  this->Boundaries->GetPointCells(startPt,cellIds);

  // Get starting cell
  nextCell = cellIds->GetId(0);

  // Get the list of boundary Ids
  vtkNew(vtkIdList, boundaryIds);
  boundaryIds->SetNumberOfIds(this->BoundaryIds->GetNumberOfTuples());
  for (int i=0; i<this->BoundaryIds->GetNumberOfTuples(); i++)
    boundaryIds->SetId(i, pointIds->LookupValue(this->BoundaryIds->GetTuple1(i)));

  // Run loop find to get correct loop and see if actually correct
  if (vtkSVGeneralUtils::RunLoopFind(this->Boundaries, startPt, nextCell, this->BoundaryLoop, boundaryIds) != SV_OK)
  {
    nextCell = cellIds->GetId(1);
    this->BoundaryLoop->DeleteCells();
    // If it failed, then try the other way!
    if (vtkSVGeneralUtils::RunLoopFind(this->Boundaries, startPt, nextCell, this->BoundaryLoop, boundaryIds) != SV_OK)
    {
      vtkErrorMacro("Both directions didn't work!!");
      return SV_ERROR;
    }
  }
  vtkDebugMacro("COMPARE: " << this->Boundaries->GetNumberOfPoints() << " " << this->BoundaryLoop->GetNumberOfPoints());

  return SV_OK;
}

// ----------------------
// FindBoundaries
// ----------------------
int vtkSVBoundaryMapper::FindBoundaries()
{
  // Set up locators
  vtkIndent indenter;
  vtkNew(vtkPointLocator, locator);
  vtkNew(vtkFeatureEdges, finder);
  finder->SetInputData(this->InitialPd);
  finder->FeatureEdgesOff();
  //finder->SetLocator(locator);
  finder->NonManifoldEdgesOff();
  finder->BoundaryEdgesOn();
  finder->Update();

  // Get all connected regions
  vtkNew(vtkConnectivityFilter, connector);
  connector->SetInputData(finder->GetOutput());
  connector->SetExtractionMode(VTK_EXTRACT_ALL_REGIONS);
  connector->ColorRegionsOn();
  connector->Update();

  // Get polydata
  vtkNew(vtkDataSetSurfaceFilter, surfacer);
  surfacer->SetInputData(connector->GetOutput());
  surfacer->Update();

  // Copy to the boundaries data
  this->Boundaries->DeepCopy(surfacer->GetOutput());

  if (this->Boundaries->GetNumberOfCells() == 0)
  {
    vtkErrorMacro("No boundaries on polydata");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVBoundaryMapper::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  if (this->InternalIdsArrayName != NULL)
    os << indent << "Internal Ids array name: " << this->InternalIdsArrayName << "\n";
  os << indent << "Z axis: " <<
    this->ObjectZAxis[0] << " " << this->ObjectZAxis[1] << " " << this->ObjectZAxis[2] << "\n";
  os << indent << "X axis: " <<
    this->ObjectXAxis[0] << " " << this->ObjectXAxis[1] << " " << this->ObjectXAxis[2] << "\n";
}
