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

/*=========================================================================

  Program:   Visualization Toolkit
  Module:    vtkSVSurfaceVectors.cxx

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "vtkSVSurfaceVectors.h"

#include "vtkCellType.h"
#include "vtkDataSet.h"
#include "vtkDoubleArray.h"
#include "vtkIdList.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolygon.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkTriangle.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVSurfaceVectors);

// ----------------------
// Constructor
// ----------------------
// Construct with feature angle=30, splitting and consistency turned on,
// flipNormals turned off, and non-manifold traversal turned on.
vtkSVSurfaceVectors::vtkSVSurfaceVectors()
{
  this->ConstraintMode = vtkSVSurfaceVectors::Parallel;

  // by default process active point vectors
  this->SetInputArrayToProcess(0,0,0,vtkDataObject::FIELD_ASSOCIATION_POINTS,
                               vtkDataSetAttributes::VECTORS);
}

// ----------------------
// Destructor
// ----------------------
vtkSVSurfaceVectors::~vtkSVSurfaceVectors()
{
}

// ----------------------
// RequestUpdateExtent
// ----------------------
int vtkSVSurfaceVectors::RequestUpdateExtent(
                                           vtkInformation * vtkNotUsed(request),
                                           vtkInformationVector **inputVector,
                                           vtkInformationVector *outputVector)
{
  // get the info objects
  vtkInformation* outInfo = outputVector->GetInformationObject(0);
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);

  inInfo->Set(vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_GHOST_LEVELS(),
              outInfo->Get(vtkStreamingDemandDrivenPipeline::
                           UPDATE_NUMBER_OF_GHOST_LEVELS()) + 1);

  return 1;
}

// ----------------------
// RequestData
// ----------------------
// Generate normals for polygon meshesPrint
//----------------------------------------------------------------------------
int vtkSVSurfaceVectors::RequestData(vtkInformation *vtkNotUsed(request),
                                   vtkInformationVector **inputVector,
                                   vtkInformationVector *outputVector)
{
  // get the info objects
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  // get the input and output
  vtkDataSet *input = vtkDataSet::SafeDownCast(
    inInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkDataSet *output = vtkDataSet::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));

  vtkIdType numPoints, pointId, i, cellId;
  numPoints = input->GetNumberOfPoints();
  vtkDataArray *inVectors = this->GetInputArrayToProcess(0,inputVector);

  if(!inVectors)
    {
    output->ShallowCopy(input);
    return 1;
    }

  vtkDataArray* newVectors = 0;
  vtkDoubleArray* newScalars = 0;
  vtkIdList* cellIds = vtkIdList::New();
  vtkIdList* ptIds = vtkIdList::New();
  double p1[3];
  double p2[3];
  double p3[3];
  double normal[3];
  double tmp[3];
  double v1[3], v2[3];
  int count;
  int cellType;

  // We could generate both ...
  if(numPoints)
    {
    if (this->ConstraintMode == vtkSVSurfaceVectors::PerpendicularScale)
      {
      newScalars = vtkDoubleArray::New();
      newScalars->SetNumberOfComponents(1);
      newScalars->SetNumberOfTuples(numPoints);
      newScalars->SetName("Perpendicular Scale");
      }
    else
      {
      newVectors = inVectors->NewInstance();
      newVectors->SetNumberOfComponents(3);
      newVectors->SetNumberOfTuples(numPoints);
      newVectors->SetName(inVectors->GetName());
      }
    }

  for (pointId = 0; pointId < numPoints; ++pointId)
    {
    input->GetPointCells(pointId, cellIds);
    // Compute the point normal.
    count = 0;
    normal[0] = normal[1] = normal [2] = 0.0;
    for (i = 0; i < cellIds->GetNumberOfIds(); ++i)
      {
      cellId = cellIds->GetId(i);
      cellType = input->GetCellType(cellId);
      if (cellType == VTK_VOXEL || cellType == VTK_POLYGON ||
          cellType == VTK_TRIANGLE || cellType == VTK_QUAD)
        {
        input->GetCellPoints(cellId, ptIds);
        input->GetPoint(ptIds->GetId(0), p1);
        input->GetPoint(ptIds->GetId(1), p2);
        input->GetPoint(ptIds->GetId(2), p3);
        v1[0] = p2[0] - p1[0];
        v1[1] = p2[1] - p1[1];
        v1[2] = p2[2] - p1[2];
        v2[0] = p3[0] - p1[0];
        v2[1] = p3[1] - p1[1];
        v2[2] = p3[2] - p1[2];
        vtkMath::Cross(v1, v2, tmp);
        ++count;
        normal[0] += tmp[0];
        normal[1] += tmp[1];
        normal[2] += tmp[2];
        }
      if (cellType == VTK_PIXEL)
        {
        input->GetCellPoints(cellId, ptIds);
        input->GetPoint(ptIds->GetId(0), p1);
        input->GetPoint(ptIds->GetId(1), p2);
        input->GetPoint(ptIds->GetId(2), p3);
        v1[0] = p2[0] - p1[0];
        v1[1] = p2[1] - p1[1];
        v1[2] = p2[2] - p1[2];
        v2[0] = p3[0] - p1[0];
        v2[1] = p3[1] - p1[1];
        v2[2] = p3[2] - p1[2];
        vtkMath::Cross(v2, v1, tmp);
        ++count;
        normal[0] += tmp[0];
        normal[1] += tmp[1];
        normal[2] += tmp[2];
        }
      }
    double inVector[3];
    inVectors->GetTuple(pointId, inVector);
    double k = 0.0;
    if (count > 0)
      {
      vtkMath::Normalize(normal);
      k = vtkMath::Dot(normal, inVector);
      if (this->ConstraintMode == vtkSVSurfaceVectors::Parallel)
        {
        // Remove non orthogonal component.
        inVector[0] = inVector[0] - (normal[0]*k);
        inVector[1] = inVector[1] - (normal[1]*k);
        inVector[2] = inVector[2] - (normal[2]*k);
        }
      else if (this->ConstraintMode == vtkSVSurfaceVectors::Perpendicular)
        { // Keep only the orthogonal component.
        inVector[0] = normal[0]*k;
        inVector[1] = normal[1]*k;
        inVector[2] = normal[2]*k;
        }
      }
    if (newScalars)
      {
      newScalars->InsertValue(pointId, k);
      }
    if (newVectors)
      {
      newVectors->InsertTuple(pointId, inVector);
      }
    }

  output->ShallowCopy(input);
  if (newVectors)
    {
    output->GetPointData()->SetVectors(newVectors);
    newVectors->Delete();
    }
  if (newScalars)
    {
    output->GetPointData()->SetScalars(newScalars);
    newScalars->Delete();
    }
  cellIds->Delete();
  ptIds->Delete();

  // Not implemented for data set.
  //output->RemoveGhostCells
  return 1;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVSurfaceVectors::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  if (this->ConstraintMode == vtkSVSurfaceVectors::Parallel)
    {
    os << indent << "ConstraintMode: Parallel\n";
    }
  else if (this->ConstraintMode == vtkSVSurfaceVectors::Perpendicular)
    {
    os << indent << "ConstraintMode: Perpendicular\n";
    }
  else if (this->ConstraintMode == vtkSVSurfaceVectors::PerpendicularScale)
    {
    os << indent << "ConstraintMode: PerpendicularScale\n";
    }
  else
    {
    os << indent << "ConstraintMode: Unknown\n";
    }
}
