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

#include "vtkSVOpenProfilesSeedSelector.h"

#include "vtkCellData.h"
#include "vtkCellPicker.h"
#include "vtkCleanPolyData.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkDoubleArray.h"
#include "vtkErrorCode.h"
#include "vtkGlyph3D.h"
#include "vtkIdList.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkIntArray.h"
#include "vtkLabeledDataMapper.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkPolyDataMapper.h"
#include "vtkPolyLine.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkProperty.h"
#include "vtkSphereSource.h"
#include "vtkThreshold.h"
#include "vtkTriangleFilter.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVIOUtils.h"
#include "vtkSVGlobals.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVOpenProfilesSeedSelector);

// ----------------------
// Constructor
// ----------------------
vtkSVOpenProfilesSeedSelector::vtkSVOpenProfilesSeedSelector()
{
  this->SeedIds = vtkIdList::New();

  this->SVRenderer = vtkSVRenderer::New();
}

// ----------------------
// Destructor
// ----------------------
vtkSVOpenProfilesSeedSelector::~vtkSVOpenProfilesSeedSelector()
{
  if (this->SeedIds != NULL)
  {
    this->SeedIds->Delete();
    this->SeedIds = NULL;
  }
  if (this->SVRenderer != NULL)
  {
    this->SVRenderer->Delete();
    this->SVRenderer = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVOpenProfilesSeedSelector::RequestData(
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
    return SV_ERROR;
  }

  vtkNew(vtkPoints, seedPoints);
  for (int i=0; i<this->SeedIds->GetNumberOfIds(); i++)
  {
    seedPoints->InsertNextPoint(this->SurfacePd->GetPoint(this->SeedIds->GetId(i)));
  }

  vtkNew(vtkPolyData, seedPolyData);
  seedPolyData->SetPoints(seedPoints);
  vtkNew(vtkLabeledDataMapper, labelsMapper);
  labelsMapper->SetInputData(seedPolyData);
  labelsMapper->SetLabelModeToLabelIds();
  vtkNew(vtkActor2D, labelsActor);
  labelsActor->SetMapper(labelsMapper);

  this->SVRenderer->GetRenderer()->AddActor(labelsActor);

  vtkNew(vtkPolyDataMapper, surfaceMapper);
  surfaceMapper->SetInputData(this->SurfacePd);
  surfaceMapper->ScalarVisibilityOff();

  vtkNew(vtkActor, surfaceActor);
  surfaceActor->SetMapper(surfaceMapper);
  surfaceActor->GetProperty()->SetOpacity(0.25);

  this->SVRenderer->GetRenderer()->AddActor(surfaceActor);

  this->SVRenderer->Render();

  this->SVRenderer->SetTextInputQuery("Please input list of inlet profile ids: \n");
  this->SVRenderer->SetCurrentTextInput(NULL);
  this->SVRenderer->UpdateTextInput();
  this->SVRenderer->EnterTextInputMode();

  std::string currentText = this->SVRenderer->GetCurrentTextInput();
  if (currentText == "")
  {
    vtkErrorMacro("Must provide a source seed");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  else
  {
    std::string separator = " ";
    if (currentText.find(separator) == std::string::npos)
      separator = ",";

    size_t pos = 0;
    std::string textId;
    while ((pos = currentText.find(separator)) != std::string::npos)
    {
      textId = currentText.substr(0, pos);
      //this->SourceSeedIds->InsertNextId(this->SeedIds->GetId(std::stoi(textId)));
      this->SourceSeedIds->InsertNextId(std::stoi(textId));
      currentText.erase(0, pos + separator.length());
    }
    //this->SourceSeedIds->InsertNextId(this->SeedIds->GetId(std::stoi(currentText)));
    this->SourceSeedIds->InsertNextId(std::stoi(currentText));
  }
  // Process info

  this->SVRenderer->SetTextInputQuery("Please input list of outlet profile ids (leave empty for all available profiles): ");
  this->SVRenderer->SetCurrentTextInput(NULL);
  this->SVRenderer->UpdateTextInput();
  this->SVRenderer->EnterTextInputMode();

  // Process info

  currentText = this->SVRenderer->GetCurrentTextInput();
  if (currentText == "")
  {
    for (int i=0; i<seedPoints->GetNumberOfPoints(); i++)
    {
      //if (this->SourceSeedIds->IsId(this->SeedIds->GetId(i)) == -1)
      if (this->SourceSeedIds->IsId(i) == -1)
      {
        //this->TargetSeedIds->InsertNextId(this->SeedIds->GetId(i));
        this->TargetSeedIds->InsertNextId(i);
      }
    }
  }
  else
  {
    std::string separator = " ";
    if (currentText.find(separator) == std::string::npos)
      separator = ",";

    size_t pos = 0;
    std::string textId;
    while ((pos = currentText.find(separator)) != std::string::npos)
    {
      textId = currentText.substr(0, pos);
      //this->TargetSeedIds->InsertNextId(this->SeedIds->GetId(std::stoi(textId)));
      this->TargetSeedIds->InsertNextId(std::stoi(textId));
      currentText.erase(0, pos + separator.length());
    }
    //this->TargetSeedIds->InsertNextId(this->SeedIds->GetId(std::stoi(currentText)));
    this->TargetSeedIds->InsertNextId(std::stoi(currentText));
  }

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVOpenProfilesSeedSelector::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}
