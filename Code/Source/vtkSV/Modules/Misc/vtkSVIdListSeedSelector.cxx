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

#include "vtkSVIdListSeedSelector.h"
#include "vtkCellPicker.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkErrorCode.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkGlyph3D.h"
#include "vtkIntArray.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkPolyLine.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkPolyDataMapper.h"
#include "vtkProperty.h"
#include "vtkIdList.h"
#include "vtkCellData.h"
#include "vtkCleanPolyData.h"
#include "vtkDoubleArray.h"
#include "vtkLabeledDataMapper.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkSphereSource.h"
#include "vtkThreshold.h"
#include "vtkTriangleFilter.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVIOUtils.h"
#include "vtkSVGlobals.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVIdListSeedSelector);

// ----------------------
// Constructor
// ----------------------
vtkSVIdListSeedSelector::vtkSVIdListSeedSelector()
{
  this->SourceIds = vtkIdList::New();
  this->TargetIds = vtkIdList::New();
}

// ----------------------
// Destructor
// ----------------------
vtkSVIdListSeedSelector::~vtkSVIdListSeedSelector()
{
  if (this->SourceIds != NULL)
  {
    this->SourceIds->Delete();
    this->SourceIds = NULL;
  }
  if (this->TargetIds != NULL)
  {
    this->TargetIds->Delete();
    this->TargetIds = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVIdListSeedSelector::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  vtkPolyData *input = vtkPolyData::SafeDownCast(
    inInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkPolyData *output = vtkPolyData::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));

  this->SurfacePd->DeepCopy(input);

  if (this->SurfacePd->GetNumberOfPoints() == 0 ||
      this->SurfacePd->GetNumberOfCells() == 0)
  {
    vtkErrorMacro("Not a valid input surface, need cells and points");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  int maxId = this->SurfacePd->GetNumberOfPoints() - 1;

  for (int i=0; i<this->SourceIds->GetNumberOfIds(); i++)
  {
    if (this->SourceIds->GetId(i) > maxId)
    {
      vtkErrorMacro("Source seed is larger than number of points");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
    this->SourceSeedIds->InsertNextId(this->SourceIds->GetId(i));
  }
  for (int i=0; i<this->TargetIds->GetNumberOfIds(); i++)
  {
    if (this->TargetIds->GetId(i) > maxId)
    {
      vtkErrorMacro("Target seed is larger than number of points");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
    this->TargetSeedIds->InsertNextId(this->TargetIds->GetId(i));
  }

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVIdListSeedSelector::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}
