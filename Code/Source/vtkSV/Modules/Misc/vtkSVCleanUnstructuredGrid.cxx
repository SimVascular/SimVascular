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

#include "vtkSVCleanUnstructuredGrid.h"

#include "vtkCell.h"
#include "vtkCellData.h"
#include "vtkCollection.h"
#include "vtkErrorCode.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkIntArray.h"
#include "vtkMergePoints.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGlobals.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVCleanUnstructuredGrid);

// ----------------------
// Constructor
// ----------------------
vtkSVCleanUnstructuredGrid::vtkSVCleanUnstructuredGrid()
{
  this->Locator = NULL;
  this->ToleranceIsAbsolute  = 0;
  this->Tolerance            = 0.0;
  this->AbsoluteTolerance    = 1.0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVCleanUnstructuredGrid::~vtkSVCleanUnstructuredGrid()
{
  this->SetLocator(NULL);
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVCleanUnstructuredGrid::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "ToleranceIsAbsolute: "
     << (this->ToleranceIsAbsolute ? "On\n" : "Off\n");
  os << indent << "Tolerance: "
     << (this->Tolerance ? "On\n" : "Off\n");
  os << indent << "AbsoluteTolerance: "
     << (this->AbsoluteTolerance ? "On\n" : "Off\n");
  if ( this->Locator )
  {
    os << indent << "Locator: " << this->Locator << "\n";
  }
  else
  {
    os << indent << "Locator: (none)\n";
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVCleanUnstructuredGrid::RequestData(vtkInformation* vtkNotUsed(request),
  vtkInformationVector** inputVector, vtkInformationVector* outputVector)
{
  vtkInformation* inInfo = inputVector[0]->GetInformationObject(0);
  vtkInformation* outInfo = outputVector->GetInformationObject(0);

  vtkDataSet* input = vtkDataSet::SafeDownCast(inInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkUnstructuredGrid* output =
    vtkUnstructuredGrid::SafeDownCast(outInfo->Get(vtkDataObject::DATA_OBJECT()));

  if (input->GetNumberOfCells() == 0)
  {
    // set up a ugrid with same data arrays as input, but
    // no points, cells or data.
    output->Allocate(1);
    output->GetPointData()->CopyAllocate(input->GetPointData(), VTK_CELL_SIZE);
    output->GetCellData()->CopyAllocate(input->GetCellData(), 1);
    vtkPoints* pts = vtkPoints::New();
    output->SetPoints(pts);
    pts->Delete();

    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  output->GetPointData()->CopyAllocate(input->GetPointData());
  output->GetCellData()->PassData(input->GetCellData());

  // First, create a new points array that eliminate duplicate points.
  // Also create a mapping from the old point id to the new.
  vtkPoints* newPts = vtkPoints::New();
  vtkIdType num = input->GetNumberOfPoints();
  vtkIdType id;
  vtkIdType newId;
  vtkIdType* ptMap = new vtkIdType[num];
  double pt[3];

  this->CreateDefaultLocator(input);
  if (this->ToleranceIsAbsolute)
  {
    this->Locator->SetTolerance(this->AbsoluteTolerance);
  }
  else
  {
    this->Locator->SetTolerance(this->Tolerance*input->GetLength());
  }
  this->Locator->InitPointInsertion(newPts, input->GetBounds(), num);

  vtkIdType progressStep = num / 100;
  if (progressStep == 0)
  {
    progressStep = 1;
  }
  for (id = 0; id < num; ++id)
  {
    if (id % progressStep == 0)
    {
      this->UpdateProgress(0.8 * ((float)id / num));
    }
    input->GetPoint(id, pt);
    if (this->Locator->InsertUniquePoint(pt, newId))
    {
      output->GetPointData()->CopyData(input->GetPointData(), id, newId);
    }
    ptMap[id] = newId;
  }
  output->SetPoints(newPts);
  newPts->Delete();

  // Now copy the cells.
  vtkIdList* cellPoints = vtkIdList::New();
  num = input->GetNumberOfCells();
  output->Allocate(num);
  for (id = 0; id < num; ++id)
  {
    if (id % progressStep == 0)
    {
      this->UpdateProgress(0.8 + 0.2 * ((float)id / num));
    }
    // special handling for polyhedron cells
    if (vtkUnstructuredGrid::SafeDownCast(input) && input->GetCellType(id) == VTK_POLYHEDRON)
    {
      vtkUnstructuredGrid::SafeDownCast(input)->GetFaceStream(id, cellPoints);
      vtkUnstructuredGrid::ConvertFaceStreamPointIds(cellPoints, ptMap);
    }
    else
    {
      input->GetCellPoints(id, cellPoints);
      for (int i = 0; i < cellPoints->GetNumberOfIds(); i++)
      {
        int cellPtId = cellPoints->GetId(i);
        newId = ptMap[cellPtId];
        cellPoints->SetId(i, newId);
      }
    }
    output->InsertNextCell(input->GetCellType(id), cellPoints);
  }

  delete[] ptMap;
  cellPoints->Delete();
  output->Squeeze();

  return SV_OK;
}

// ----------------------
// CreateDefaultLocator
// ----------------------
void vtkSVCleanUnstructuredGrid::CreateDefaultLocator(vtkDataSet *input)
{
  double tol;
  if (this->ToleranceIsAbsolute)
  {
    tol = this->AbsoluteTolerance;
  }
  else
  {
    if (input)
    {
      tol = this->Tolerance*input->GetLength();
    }
    else
    {
      tol = this->Tolerance;
    }
  }

  if ( this->Locator == NULL)
  {
    if (tol==0.0)
    {
      this->Locator = vtkMergePoints::New();
      this->Locator->Register(this);
      this->Locator->Delete();
    }
    else
    {
      this->Locator = vtkPointLocator::New();
      this->Locator->Register(this);
      this->Locator->Delete();
    }
  }
  else
  {
    // check that the tolerance wasn't changed from zero to non-zero
    if ((tol>0.0) && (this->GetLocator()->GetTolerance()==0.0))
    {
      this->SetLocator(NULL);
      this->Locator = vtkPointLocator::New();
      this->Locator->Register(this);
      this->Locator->Delete();
    }
  }
}

// ----------------------
// FillInputPortInformation
// ----------------------
int vtkSVCleanUnstructuredGrid::FillInputPortInformation(int vtkNotUsed(port), vtkInformation* info)
{
  info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkDataSet");
  return SV_OK;
}


