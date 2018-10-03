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

#include "vtkSVPickPointSeedSelector.h"

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
vtkStandardNewMacro(vtkSVPickPointSeedSelector);

// ----------------------
// Constructor
// ----------------------
vtkSVPickPointSeedSelector::vtkSVPickPointSeedSelector()
{
  this->PickedSeeds = vtkPolyData::New();

  this->PickedSeedIds = vtkIdList::New();

  this->SVRenderer = vtkSVRenderer::New();

  this->UndoCallbackCommand = NULL;
  this->PickCallbackCommand = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVPickPointSeedSelector::~vtkSVPickPointSeedSelector()
{
  if (this->PickedSeeds != NULL)
  {
    this->PickedSeeds->Delete();
    this->PickedSeeds = NULL;
  }
  if (this->PickedSeedIds != NULL)
  {
    this->PickedSeedIds->Delete();
    this->PickedSeedIds = NULL;
  }
  if (this->SVRenderer != NULL)
  {
    this->SVRenderer->Delete();
    this->SVRenderer = NULL;
  }
  if (this->UndoCallbackCommand != NULL)
  {
    this->UndoCallbackCommand->Delete();
    this->UndoCallbackCommand = NULL;
  }
  if (this->PickCallbackCommand != NULL)
  {
    this->PickCallbackCommand->Delete();
    this->PickCallbackCommand = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVPickPointSeedSelector::RequestData(
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

  vtkNew(vtkGlyph3D, glyphs);
  vtkNew(vtkSphereSource, glyphSource);

  glyphs->SetInputData(this->PickedSeeds);
  glyphs->SetSourceConnection(glyphSource->GetOutputPort());
  glyphs->SetScaleModeToDataScalingOff();
  glyphs->SetScaleFactor(this->SurfacePd->GetLength()*0.01);

  vtkNew(vtkPolyDataMapper, glyphMapper);
  glyphMapper->SetInputConnection(glyphs->GetOutputPort());

  vtkNew(vtkActor, seedActor);
  seedActor->SetMapper(glyphMapper);
  seedActor->GetProperty()->SetColor(1.0,0.0,0.0);
  seedActor->PickableOff();
  this->SVRenderer->GetRenderer()->AddActor(seedActor);

  this->UndoCallbackCommand = vtkCallbackCommand::New();
  this->UndoCallbackCommand->SetCallback(vtkSVPickPointSeedSelector::UndoCallback);
  this->UndoCallbackCommand->SetClientData(this);
  this->SVRenderer->AddKeyBinding("u","Undo.", this->UndoCallbackCommand,"0");
  this->PickCallbackCommand = vtkCallbackCommand::New();
  this->PickCallbackCommand->SetCallback(vtkSVPickPointSeedSelector::PickCallback);
  this->PickCallbackCommand->SetClientData(this);
  this->SVRenderer->AddKeyBinding("space","Add points.",this->PickCallbackCommand,"0");

  vtkNew(vtkPolyDataMapper, surfaceMapper);
  surfaceMapper->SetInputData(this->SurfacePd);
  surfaceMapper->ScalarVisibilityOff();

  vtkNew(vtkActor, surfaceActor);
  surfaceActor->SetMapper(surfaceMapper);
  surfaceActor->GetProperty()->SetOpacity(1.0);

  this->SVRenderer->GetRenderer()->AddActor(surfaceActor);

  this->SVRenderer->SetTextInputQuery("Please position the mouse and press space to add source points, \'u\' to undo\n");
  this->SVRenderer->UpdateTextInput();

  int any = 0;
  while (!any)
  {
    this->InitializeSeeds();
    this->SVRenderer->Render();
    any = this->PickedSeedIds->GetNumberOfIds();
  }
  this->SourceSeedIds->DeepCopy(this->PickedSeedIds);

  this->SVRenderer->SetTextInputQuery("Please position the mouse and press space to add target points, \'u\' to undo\n");
  this->SVRenderer->UpdateTextInput();

  any = 0;
  while (!any)
  {
    this->InitializeSeeds();
    this->SVRenderer->Render();
    any = this->PickedSeedIds->GetNumberOfIds();
  }
  this->TargetSeedIds->DeepCopy(this->PickedSeedIds);

  return SV_OK;
}

// ----------------------
// InitializeSeeds
// ----------------------
void vtkSVPickPointSeedSelector::InitializeSeeds()
{
  this->PickedSeedIds->Initialize();
  this->PickedSeeds->Initialize();
  vtkNew(vtkPoints, seedPoints);
  this->PickedSeeds->SetPoints(seedPoints);
}

// ----------------------
// PickCallback
// ----------------------
void vtkSVPickPointSeedSelector::PickCallback( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* clientData, void* vtkNotUsed(callData) )
{
   vtkSVPickPointSeedSelector* parent =
     static_cast<vtkSVPickPointSeedSelector*>(clientData);

  vtkNew(vtkCellPicker, picker);
  picker->SetTolerance(1E-4 * parent->SurfacePd->GetLength());
  int eventPosition[2];
  parent->SVRenderer->GetRenderWindowInteractor()->GetEventPosition(eventPosition);
  int result = picker->Pick(float(eventPosition[0]),float(eventPosition[1]),0.0,parent->SVRenderer->GetRenderer());
  if (result == 0)
    return;

  double pickPosition[3];
  picker->GetPickPosition(pickPosition);
  vtkNew(vtkIdList, pickedCellPointIds);
  pickedCellPointIds = parent->SurfacePd->GetCell(picker->GetCellId())->GetPointIds();
  double minDistance = 1.0e10;
  int pickedSeedId = -1;
  for (int i=0; i<pickedCellPointIds->GetNumberOfIds(); i++)
  {
    double distance = vtkMath::Distance2BetweenPoints(pickPosition,parent->SurfacePd->GetPoint(pickedCellPointIds->GetId(i)));
    if (distance < minDistance)
    {
      minDistance = distance;
      pickedSeedId = pickedCellPointIds->GetId(i);
    }
  }
  if (pickedSeedId == -1)
    pickedSeedId = pickedCellPointIds->GetId(0);

  double pt[3];
  parent->PickedSeedIds->InsertNextId(pickedSeedId);
  parent->SurfacePd->GetPoint(pickedSeedId, pt);
  parent->PickedSeeds->GetPoints()->InsertNextPoint(pt);
  parent->PickedSeeds->Modified();
  parent->SVRenderer->GetRenderWindow()->Render();
}

void vtkSVPickPointSeedSelector::UndoCallback( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* clientData, void* vtkNotUsed(callData) )
{
   vtkSVPickPointSeedSelector* parent =
     static_cast<vtkSVPickPointSeedSelector*>(clientData);

  parent->InitializeSeeds();
  parent->PickedSeeds->Modified();
  parent->SVRenderer->GetRenderWindow()->Render();
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVPickPointSeedSelector::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}
