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

#include "vtkSVFindSeparateRegions.h"

#include "vtkCellData.h"
#include "vtkCellDataToPointData.h"
#include "vtkErrorCode.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkIntArray.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"

#include <iostream>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVFindSeparateRegions);

// ----------------------
// Constructor
// ----------------------
vtkSVFindSeparateRegions::vtkSVFindSeparateRegions()
{
  this->CellArrayName     = NULL;
  this->OutPointArrayName = NULL;

  this->WorkPd        = vtkPolyData::New();
  this->TargetCellIds = vtkIdList::New();
}

// ----------------------
// Destructor
// ----------------------
vtkSVFindSeparateRegions::~vtkSVFindSeparateRegions()
{
  if (this->WorkPd)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }
  if (this->TargetCellIds)
  {
    this->TargetCellIds->Delete();
    this->TargetCellIds = NULL;
  }

  if (this->CellArrayName != NULL)
  {
    delete [] this->CellArrayName;
    this->CellArrayName = NULL;
  }

  if (this->OutPointArrayName != NULL)
  {
    delete [] this->OutPointArrayName;
    this->OutPointArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVFindSeparateRegions::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  if (this->CellArrayName != NULL)
    os << indent << "Cell array name: " << this->CellArrayName << "\n";
  if (this->OutPointArrayName != NULL)
    os << indent << "Out point array name: " << this->OutPointArrayName << "\n";
  if (this->TargetCellIds->GetNumberOfIds() != 0)
  {
    os << indent << "Target values to separate: "<< "\n";
      os << indent;
    for (int i=0; i<this->TargetCellIds->GetNumberOfIds(); i++)
      os << this->TargetCellIds->GetId(i);
    os << "\n";
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVFindSeparateRegions::RequestData(
                                 vtkInformation *vtkNotUsed(request),
                                 vtkInformationVector **inputVector,
                                 vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  // Copy input to working polydata
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

  output->DeepCopy(this->WorkPd);

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVFindSeparateRegions::PrepFilter()
{
  //Get the number of Polys for scalar  allocation
  int numPolys = this->WorkPd->GetNumberOfPolys();
  int numPts = this->WorkPd->GetNumberOfPoints();

  //Check the input to make sure it is there
  if (numPolys < 1)
  {
    vtkDebugMacro("No input!");
    return SV_ERROR;
  }

  if (this->GetCellArray(this->WorkPd) != SV_OK)
  {
    std::cout<<"No Cell Array Named "<<this->CellArrayName<<" on surface"<<endl;
    return SV_ERROR;
  }
  if (this->OutPointArrayName == NULL)
  {
    std::cout<<"Need name for output point data information"<<endl;
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVFindSeparateRegions::RunFilter()
{
  //Get the number of Polys for scalar  allocation
  int numPolys = this->WorkPd->GetNumberOfPolys();
  int numPts = this->WorkPd->GetNumberOfPoints();

  this->WorkPd->BuildLinks();
  vtkNew(vtkIntArray, newPointArray);
  vtkNew(vtkIdList, pointCells);
  vtkNew(vtkIdList, checkList);

  // If no targets given, assume we are using all
  if (this->TargetCellIds->GetNumberOfIds() == 0)
    this->SetAllCellIds();

  // Loop through all points
  for (int pointId = 0;pointId < numPts;pointId++)
  {
    // Get point cells
    checkList->Reset();
    int boundaryPoint = 0;
    this->WorkPd->GetPointCells(pointId,pointCells);

    // Loop through all point cells
    for (int i=0;i<pointCells->GetNumberOfIds();i++)
    {
      // Check the values of point
      int cellId = pointCells->GetId(i);
      vtkIdType value = this->IntCellScalars->GetValue(cellId);
      if (this->TargetCellIds->IsId(value) != -1)
      {
        vtkIdType check = checkList->InsertUniqueId(value);

        // The value of check isnt zero, we found a point that touches
        // cells of multiple values
        if (check != 0)
          boundaryPoint = 1;
      }
    }

    // Add corrct array value
    if (boundaryPoint)
      newPointArray->InsertValue(pointId,1);
    else
      newPointArray->InsertValue(pointId,0);
  }

  // Add array to polydata
  newPointArray->SetName(this->OutPointArrayName);
  this->WorkPd->GetPointData()->AddArray(newPointArray);

  return SV_OK;
}


// ----------------------
// GetCellArray
// ----------------------
int vtkSVFindSeparateRegions::GetCellArray(vtkPolyData *object)
{
  int exists = vtkSVGeneralUtils::CheckArrayExists(object, 1, this->CellArrayName);

  if (exists)
  {
    this->IntCellScalars = vtkIntArray::SafeDownCast(
	object->GetCellData()->GetArray(this->CellArrayName));
  }

  return exists;
}

// ----------------------
// SetAllCellIds
// ----------------------
int vtkSVFindSeparateRegions::SetAllCellIds()
{
  double range[2];
  this->IntCellScalars->GetRange(range);

  int max = range[1];
  for (int i=0;i <= max;i++)
    this->TargetCellIds->InsertNextId(i);

  return SV_OK;
}
