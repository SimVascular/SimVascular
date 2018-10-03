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

#include "vtkSVSmoothVolume.h"

#include "vtkCell.h"
#include "vtkCellData.h"
#include "vtkDoubleArray.h"
#include "vtkErrorCode.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
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
vtkStandardNewMacro(vtkSVSmoothVolume);

// ----------------------
// Constructor
// ----------------------
vtkSVSmoothVolume::vtkSVSmoothVolume()
{
  this->WorkUg = vtkUnstructuredGrid::New();
  this->NumberOfSmoothIterations = 5;
}

// ----------------------
// Destructor
// ----------------------
vtkSVSmoothVolume::~vtkSVSmoothVolume()
{
  if (this->WorkUg != NULL)
  {
    this->WorkUg->Delete();
    this->WorkUg = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVSmoothVolume::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
}

// ----------------------
// RequestData
// ----------------------
int vtkSVSmoothVolume::RequestData(
                                 vtkInformation *vtkNotUsed(request),
                                 vtkInformationVector **inputVector,
                                 vtkInformationVector *outputVector)
{
  // Get the input and output
  vtkUnstructuredGrid *input  = vtkUnstructuredGrid::GetData(inputVector[0]);
  vtkUnstructuredGrid *output = vtkUnstructuredGrid::GetData(outputVector);

  // Copy the input to operate on
  this->WorkUg->DeepCopy(input);

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

  output->DeepCopy(this->WorkUg);
  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVSmoothVolume::PrepFilter()
{
  // Get number of cells and points
  vtkIdType numCells  = this->WorkUg->GetNumberOfCells();
  vtkIdType numPoints = this->WorkUg->GetNumberOfPoints();

  //Check the input to make sure it is there
  if (numCells < 1)
  {
    vtkErrorMacro("No input!");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVSmoothVolume::RunFilter()
{
  this->WorkUg->BuildLinks();

  if (this->WorkUg->GetCellType(0) == VTK_HEXAHEDRON)
  {
    if (this->SmoothHexMesh() != SV_OK)
    {
      vtkErrorMacro("Error smoothing hex volume");
      return SV_ERROR;
    }
  }
  else if (this->WorkUg->GetCellType(0) == VTK_TETRA)
  {
    if (this->SmoothTetMesh() != SV_OK)
    {
      vtkErrorMacro("Error smoothing hex volume");
      return SV_ERROR;
    }
  }
  else
  {
    vtkErrorMacro("Smoothing is not implemented for this mesh type");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// SmoothHexMesh
// ----------------------
int vtkSVSmoothVolume::SmoothHexMesh()
{
  int numCells = this->WorkUg->GetNumberOfCells();
  int numPoints = this->WorkUg->GetNumberOfPoints();

  vtkNew(vtkIntArray, isInteriorPoint);
  isInteriorPoint->SetNumberOfTuples(numPoints);
  isInteriorPoint->SetName("IsInteriorPoint");

  for (int i=0; i<numCells; i++)
  {
    if (this->WorkUg->GetCellType(i) != VTK_HEXAHEDRON)
    {
      vtkErrorMacro("All cells must be hexes");
      return SV_ERROR;
    }
  }

  std::vector<std::vector<int> > ptEdgeNeighbors(numPoints);

  for (int i=0; i<numPoints; i++)
  {
    vtkNew(vtkIdList, ptCellIds);
    this->WorkUg->GetPointCells(i, ptCellIds);

    int interiorPoint = 1;
    vtkNew(vtkIdList, ptNeighbors);
    for (int j=0; j<ptCellIds->GetNumberOfIds(); j++)
    {
      vtkCell *cell = this->WorkUg->GetCell(ptCellIds->GetId(j));

      int numFaces = cell->GetNumberOfFaces();
      for (int k=0; k<numFaces; k++)
      {
        vtkCell *face = cell->GetFace(k);

        int checkable = 0;
        for (int l=0; l<4; l++)
        {
          if (face->PointIds->GetId(l) == i)
            checkable = 1;
        }

        if (checkable)
        {
          // Have to do this for special interior cells in which multiple boundaries
          // meeting as four points of one face may not actually correspond to
          // just one cell. Essentially, interior of anything > bifurcation.
          int neighCount = 0;
          for (int l=0; l<4; l++)
          {
            vtkNew(vtkIdList, threePtIds);
            threePtIds->InsertNextId(face->PointIds->GetId(l));
            threePtIds->InsertNextId(face->PointIds->GetId((l+1)%4));
            threePtIds->InsertNextId(face->PointIds->GetId((l+2)%4));

            vtkNew(vtkIdList, neighCellIds);
            this->WorkUg->GetCellNeighbors(ptCellIds->GetId(j), threePtIds, neighCellIds);
            if (neighCellIds->GetNumberOfIds() != 0)
              neighCount++;
          }
          if (neighCount == 0)
            interiorPoint = 0;
        }
      }

      for (int k=0; k<cell->GetNumberOfEdges(); k++)
      {
        vtkIdList *edge = cell->GetEdge(k)->GetPointIds();
        int isPtId = edge->IsId(i);
        if (isPtId != -1)
        {
          if (ptNeighbors->IsId(edge->GetId((isPtId+1)%2)) == -1)
           ptNeighbors->InsertNextId(edge->GetId((isPtId+1)%2));
        }
      }
    }

    isInteriorPoint->SetTuple1(i, interiorPoint);
    if (interiorPoint)
    {
      if (ptNeighbors->GetNumberOfIds() > 0)
      {
        for (int j=0; j<ptNeighbors->GetNumberOfIds(); j++)
          ptEdgeNeighbors[i].push_back(ptNeighbors->GetId(j));
      }
    }
  }

  for (int iter=0; iter<this->NumberOfSmoothIterations; iter++)
  {
    for (int i=0; i<numPoints; i++)
    {
      // If > 0 neighbors, that means this is interior son
      int numPtNeighbors = ptEdgeNeighbors[i].size();
      if (numPtNeighbors > 0)
      {
        double center[3]; center[0] = 0.0; center[1] = 0.0; center[2] = 0.0;

        for (int j=0; j<numPtNeighbors; j++)
        {
          int neighborPtId = ptEdgeNeighbors[i][j];
          double neighborPt[3];
          this->WorkUg->GetPoint(neighborPtId, neighborPt);

          for (int k=0; k<3; k++)
            center[k] += neighborPt[k];
        }

        double pt[3];
        this->WorkUg->GetPoint(i, pt);

        for (int j=0; j<3; j++)
          pt[j] += (center[j]/numPtNeighbors - pt[j]) * 0.02;

        this->WorkUg->GetPoints()->SetPoint(i, pt);
      }
    }
  }

  //this->WorkUg->GetPointData()->AddArray(isInteriorPoint);

  return SV_OK;
}

// ----------------------
// SmoothTetMesh
// ----------------------
int vtkSVSmoothVolume::SmoothTetMesh()
{
  int numCells = this->WorkUg->GetNumberOfCells();
  int numPoints = this->WorkUg->GetNumberOfPoints();

  vtkNew(vtkIntArray, isInteriorPoint);
  isInteriorPoint->SetNumberOfTuples(numPoints);
  isInteriorPoint->SetName("IsInteriorPoint");

  for (int i=0; i<numCells; i++)
  {
    if (this->WorkUg->GetCellType(i) != VTK_TETRA)
    {
      vtkErrorMacro("All cells must be hexes");
      return SV_ERROR;
    }
  }

  std::vector<std::vector<int> > ptEdgeNeighbors(numPoints);

  for (int i=0; i<numPoints; i++)
  {
    vtkNew(vtkIdList, ptCellIds);
    this->WorkUg->GetPointCells(i, ptCellIds);

    int interiorPoint = 1;
    vtkNew(vtkIdList, ptNeighbors);
    for (int j=0; j<ptCellIds->GetNumberOfIds(); j++)
    {
      vtkCell *cell = this->WorkUg->GetCell(ptCellIds->GetId(j));

      int numFaces = cell->GetNumberOfFaces();
      for (int k=0; k<numFaces; k++)
      {
        vtkCell *face = cell->GetFace(k);

        int checkable = 0;
        for (int l=0; l<3; l++)
        {
          if (face->PointIds->GetId(l) == i)
            checkable = 1;
        }

        if (checkable)
        {
          // Have to do this for special interior cells in which multiple boundaries
          // meeting as four points of one face may not actually correspond to
          // just one cell. Essentially, interior of anything > bifurcation.
          int neighCount = 0;
          vtkNew(vtkIdList, threePtIds);
          threePtIds->InsertNextId(face->PointIds->GetId(0));
          threePtIds->InsertNextId(face->PointIds->GetId(1));
          threePtIds->InsertNextId(face->PointIds->GetId(2));
          vtkNew(vtkIdList, neighCellIds);

          this->WorkUg->GetCellNeighbors(ptCellIds->GetId(j), threePtIds, neighCellIds);

          if (neighCellIds->GetNumberOfIds() == 0)
            interiorPoint = 0;

          for (int l=0; l<3; l++)
          {
            if (face->PointIds->GetId(l) != i)
            {
              if (ptNeighbors->IsId(face->PointIds->GetId(l)) == -1)
                ptNeighbors->InsertNextId(face->PointIds->GetId(l));
            }
          }

        }
      }
    }

    isInteriorPoint->SetTuple1(i, interiorPoint);
    if (interiorPoint)
    {
      if (ptNeighbors->GetNumberOfIds() > 0)
      {
        for (int j=0; j<ptNeighbors->GetNumberOfIds(); j++)
          ptEdgeNeighbors[i].push_back(ptNeighbors->GetId(j));
      }
    }
  }

  for (int iter=0; iter<this->NumberOfSmoothIterations; iter++)
  {
    for (int i=0; i<numPoints; i++)
    {
      // If > 0 neighbors, that means this is interior son
      int numPtNeighbors = ptEdgeNeighbors[i].size();
      if (numPtNeighbors > 0)
      {
        double center[3]; center[0] = 0.0; center[1] = 0.0; center[2] = 0.0;

        for (int j=0; j<numPtNeighbors; j++)
        {
          int neighborPtId = ptEdgeNeighbors[i][j];
          double neighborPt[3];
          this->WorkUg->GetPoint(neighborPtId, neighborPt);

          for (int k=0; k<3; k++)
            center[k] += neighborPt[k];
        }

        double pt[3];
        this->WorkUg->GetPoint(i, pt);

        for (int j=0; j<3; j++)
          pt[j] += (center[j]/numPtNeighbors - pt[j]) * 0.02;

        this->WorkUg->GetPoints()->SetPoint(i, pt);
      }
    }
  }

  this->WorkUg->GetPointData()->AddArray(isInteriorPoint);

  return SV_OK;
}
