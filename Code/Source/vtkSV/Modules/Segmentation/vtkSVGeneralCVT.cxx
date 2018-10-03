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

#include "vtkSVGeneralCVT.h"

#include "vtkCellData.h"
#include "vtkErrorCode.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"

// ----------------------
// Constructor
// ----------------------
vtkSVGeneralCVT::vtkSVGeneralCVT()
{
  this->WorkPd =     vtkPolyData::New();
  this->Generators = NULL;
  this->WorkGenerators = vtkPolyData::New();

  this->CVTDataArray =    NULL;
  this->PatchIdsArray =   NULL;
  this->GeneratorsArray = NULL;
  this->FixedIdsList = NULL;

  this->CVTDataArrayName =    NULL;
  this->PatchIdsArrayName =   NULL;

  this->UsePointArray =      0;
  this->UseCellArray  =      1;
  this->UseGeneratorsArray = 0;
  this->Threshold =          2.0;

  this->MaximumNumberOfIterations = 1.0e2;
  this->UseTransferredPatchesAsThreshold = 1;
  this->NoInitialization = 0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVGeneralCVT::~vtkSVGeneralCVT()
{
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }
  if (this->WorkGenerators != NULL)
  {
    this->WorkGenerators->Delete();
    this->WorkGenerators = NULL;
  }
  if (this->Generators != NULL)
  {
    this->Generators->Delete();
    this->Generators = NULL;
  }
  if (this->NoInitialization == 0)
  {
    if (this->PatchIdsArray != NULL)
    {
      this->PatchIdsArray->Delete();
      this->PatchIdsArray = NULL;
    }
  }
  if (this->FixedIdsList != NULL)
  {
    this->FixedIdsList->Delete();
    this->FixedIdsList = NULL;
  }

  if (this->CVTDataArrayName != NULL)
  {
    delete [] this->CVTDataArrayName;
    this->CVTDataArrayName = NULL;
  }
  if (this->PatchIdsArrayName != NULL)
  {
    delete [] this->PatchIdsArrayName;
    this->PatchIdsArrayName = NULL;
  }

}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVGeneralCVT::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  if (this->CVTDataArrayName != NULL)
    os << indent << "CVT data array name: " << this->CVTDataArrayName << "\n";
  if (this->PatchIdsArrayName != NULL)
    os << indent << "Patch ids array name: " << this->PatchIdsArrayName << "\n";

  os << indent << "Use cell array: " << this->UseCellArray << "\n";
  os << indent << "Use point array: " << this->UsePointArray << "\n";
  os << indent << "Use generators array: " << this->UseGeneratorsArray << "\n";
  os << indent << "Use transferred patches as threshold: " << this->UseTransferredPatchesAsThreshold << "\n";
  os << indent << "Threshold: " << this->Threshold << "\n";
  os << indent << "Maximum number of iterations: " << this->MaximumNumberOfIterations << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVGeneralCVT::RequestData(vtkInformation *vtkNotUsed(request),
                                 vtkInformationVector **inputVector,
                                 vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input1 = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  // Copy the input to operate on
  this->WorkPd->DeepCopy(input1);

  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Error in preprocessing the polydata\n");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Error when running main operation\n");
    this->SetErrorCode(vtkErrorCode::UserError + 2);
    return SV_ERROR;
  }

  output->DeepCopy(this->WorkPd);
  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVGeneralCVT::PrepFilter()
{

  if (this->Generators == NULL)
  {
    vtkErrorMacro("Must set the generators!");
    return SV_ERROR;
  }
  this->WorkGenerators->DeepCopy(this->Generators);

  if (this->UseCellArray && this->UsePointArray)
  {
    vtkErrorMacro("Can only use points or cells, not both");
    return SV_ERROR;
  }

  if (this->UseCellArray)
  {
    if (this->PatchIdsArrayName == NULL)
    {
      vtkDebugMacro("Patch Ids Array Name not given, setting to PatchIds");
      this->PatchIdsArrayName = new char[strlen("PatchIds") + 1];
      strcpy(this->PatchIdsArrayName, "PatchIds");
    }

    if (this->NoInitialization == 1)
    {
      if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->PatchIdsArrayName) == SV_OK)
      {
        this->PatchIdsArray = vtkIntArray::SafeDownCast(this->WorkPd->GetCellData()->GetArray(this->PatchIdsArrayName));
      }
      else
      {
        vtkErrorMacro("No array named "<< this->PatchIdsArrayName << "on input.");
        return SV_ERROR;
      }
    }
    else
    {
      this->PatchIdsArray = vtkIntArray::New();
      this->PatchIdsArray->SetNumberOfTuples(this->WorkPd->GetNumberOfCells());
      this->PatchIdsArray->FillComponent(0, 0);
    }
    this->PatchIdsArray->SetName(this->PatchIdsArrayName);

    if (this->CVTDataArrayName == NULL)
    {
      vtkErrorMacro("Must provide data array name on input");
      return SV_ERROR;
    }

    if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->CVTDataArrayName) != SV_OK)
    {
      vtkErrorMacro("No array named "<< this->CVTDataArrayName << "on input.");
      return SV_ERROR;
    }

    this->CVTDataArray = this->WorkPd->GetCellData()->GetArray(this->CVTDataArrayName);
  }
  else if (this->UsePointArray)
  {
    if (this->PatchIdsArrayName == NULL)
    {
      vtkDebugMacro("Patch Ids Array Name not given, setting to PatchIds");
      this->PatchIdsArrayName = new char[strlen("PatchIds") + 1];
      strcpy(this->PatchIdsArrayName, "PatchIds");
    }

    if (this->NoInitialization == 1)
    {
      if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 0, this->PatchIdsArrayName) != SV_OK)
      {
        this->PatchIdsArray = vtkIntArray::SafeDownCast(this->WorkPd->GetPointData()->GetArray(this->PatchIdsArrayName));
      }
      else
      {
        vtkErrorMacro("No array named "<< this->PatchIdsArrayName << "on input.");
        return SV_ERROR;
      }
    }
    else
    {
      this->PatchIdsArray = vtkIntArray::New();
      this->PatchIdsArray->SetNumberOfTuples(this->WorkPd->GetNumberOfPoints());
      this->PatchIdsArray->FillComponent(0, 0);
    }
    this->PatchIdsArray->SetName(this->PatchIdsArrayName);

    if (this->CVTDataArrayName == NULL)
    {
      vtkErrorMacro("Must provide array name on input");
      return SV_ERROR;
    }

    if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 0, this->CVTDataArrayName) != SV_OK)
    {
      vtkErrorMacro("No array named "<< this->CVTDataArrayName << "on input.");
      return SV_ERROR;
    }

    this->CVTDataArray = this->WorkPd->GetPointData()->GetArray(this->CVTDataArrayName);
  }
  else
  {
    vtkErrorMacro("Must use either point or cell data");
    return SV_ERROR;
  }

  if (this->UseGeneratorsArray)
  {
    if (this->CVTDataArray->GetNumberOfComponents() !=
        this->GeneratorsArray->GetNumberOfComponents())
    {
      vtkErrorMacro("Array on input and generators should have the same number of components");
      return SV_ERROR;
    }
  }
  else
  {
    if (this->CVTDataArray->GetNumberOfComponents() != 3)
    {
      vtkErrorMacro("Array on input should have 3 components");
    }
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVGeneralCVT::RunFilter()
{
  if (this->NoInitialization == 0)
  {
    if (this->InitializeGenerators() != SV_OK)
    {
      vtkErrorMacro("Unable to initialize CVT connectivity");
      return SV_ERROR;
    }
  }

  if (this->InitializeConnectivity() != SV_OK)
  {
    vtkErrorMacro("Unable to initialize CVT connectivity");
    return SV_ERROR;
  }

  // Get number of cells or points
  int numDatas = 0;
  if (this->UseCellArray)
    numDatas = this->WorkPd->GetNumberOfCells();
  else if (this->UsePointArray)
    numDatas = this->WorkPd->GetNumberOfPoints();

  // Set fixed values
  this->FixedIds.resize(numDatas);
  for (int i=0; i<numDatas; i++)
    this->FixedIds[i] = 0;

  if (this->FixedIdsList != NULL)
  {
    for (int i=0; i<numDatas; i++)
    {
      if (this->FixedIdsList->IsId(i) != -1)
        this->FixedIds[i] = 1;
    }
  }

  int iter=0;
  int eval=this->Threshold + 1;

  vtkDebugMacro("GOING TO ITERATe\n");
  // iterate until threshold met
  while (eval >= this->Threshold && iter < this->MaximumNumberOfIterations)
  {

    vtkDebugMacro("SOMEHOW IN HERE\n");
    // If using transferred patches
    //if (this->UseTransferredPatchesAsThreshold)
    eval = 0;

    // Loop through cells
    for (int i=0; i<numDatas; i++)
    {
      if (this->UseCellArray && this->IsBoundaryCell(i) && !this->FixedIds[i])
      {
        // Set for check of new generator
        int oldGenerator = this->PatchIdsArray->GetTuple1(i);
        int newGenerator;
        // Get the closest generator
        if (this->GetClosestGenerator(i, newGenerator) != SV_OK)
        {
          vtkErrorMacro("Could not get closest generator");
          return SV_ERROR;
        }
        if (newGenerator != oldGenerator)
        {
          this->UpdateConnectivity(i, oldGenerator, newGenerator);
          //this->UpdateGenerators();
          this->PatchIdsArray->SetTuple1(i, newGenerator);
          //if (this->UseTransferredPatchesAsThreshold)
          eval++;
        }
      }
    }
    fprintf(stdout,"step %d: %d\n", iter, eval);
    iter++;
  }

  this->WorkPd->GetCellData()->AddArray(this->PatchIdsArray);

  return SV_OK;
}
