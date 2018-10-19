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

#include "vtkSVRenderer.h"

#include "vtkCallbackCommand.h"
#include "vtkRenderer.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkTextActor.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"

#include <algorithm>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVRenderer);

// ----------------------
// Constructor
// ----------------------
vtkSVRenderer::vtkSVRenderer()
{
  this->WindowSize[0] = 1600; this->WindowSize[1] = 1600;
  this->WindowPosition[0] = 100; this->WindowPosition[1] = 100;
  this->Background[0] = 0.1; this->Background[1] = 0.1; this->Background[2] = 0.2;
  this->Annotations = 1;
  this->PointSmoothing = 1;
  this->LineSmoothing = 1;
  this->PolygonSmoothing = 0;
  this->TextInputMode = 0;
  this->ExitAfterTextInputMode = 1;
  this->InputPosition[0] = 0.25; this->InputPosition[1] = 0.1;
  this->Position[0] = 0.001; this->Position[1] = 0.05;

  this->Renderer = vtkRenderer::New();
  this->Renderer->SetBackground(this->Background);

  this->RenderWindow = vtkRenderWindow::New();
  this->RenderWindow->AddRenderer(this->Renderer);
  this->RenderWindow->SetSize(this->WindowSize[0],this->WindowSize[1]);
  this->RenderWindow->SetPosition(this->WindowPosition[0],this->WindowPosition[1]);
  this->RenderWindow->SetPointSmoothing(this->PointSmoothing);
  this->RenderWindow->SetLineSmoothing(this->LineSmoothing);
  this->RenderWindow->SetPolygonSmoothing(this->PolygonSmoothing);

  this->RenderWindowInteractor = vtkRenderWindowInteractor::New();

  this->RenderWindow->SetInteractor(this->RenderWindowInteractor);

  this->TrackballCamera = vtkInteractorStyleTrackballCamera::New();
  this->RenderWindowInteractor->SetInteractorStyle(this->TrackballCamera);
  this->RenderWindowInteractor->GetInteractorStyle()->KeyPressActivationOff();
  vtkNew(vtkCallbackCommand, charCallback);
  charCallback->SetCallback(vtkSVRenderer::CharCallback);
  this->RenderWindowInteractor->GetInteractorStyle()->AddObserver("CharEvent", charCallback);
  vtkNew(vtkCallbackCommand, keyPressCallback);
  keyPressCallback->SetCallback(vtkSVRenderer::KeyPressCallback);
  keyPressCallback->SetClientData(this);
  this->RenderWindowInteractor->GetInteractorStyle()->AddObserver("KeyPressEvent",keyPressCallback);

  this->ResetCameraCallbackCommand = vtkCallbackCommand::New();
  this->ResetCameraCallbackCommand->SetCallback(vtkSVRenderer::ResetCameraCallback);
  this->ResetCameraCallbackCommand->SetClientData(this);
  this->AddKeyBinding("r","Reset camera.",this->ResetCameraCallbackCommand,"0");
  this->QuitRendererCallbackCommand = vtkCallbackCommand::New();
  this->QuitRendererCallbackCommand->SetCallback(vtkSVRenderer::QuitRendererCallback);
  this->QuitRendererCallbackCommand->SetClientData(this);
  this->AddKeyBinding("q","Quit renderer/proceed.",this->QuitRendererCallbackCommand,"0");

  this->TextActor = vtkTextActor::New();
  this->TextActor->GetPositionCoordinate()->SetCoordinateSystemToNormalizedViewport();
  this->TextActor->GetPosition2Coordinate()->SetCoordinateSystemToNormalizedViewport();
  this->TextActor->SetPosition(this->Position);
  this->Renderer->AddActor(this->TextActor);

  this->TextInputActor = vtkTextActor::New();
  this->TextInputActor->GetPositionCoordinate()->SetCoordinateSystemToNormalizedViewport();
  this->TextInputActor->GetPosition2Coordinate()->SetCoordinateSystemToNormalizedViewport();
  this->TextInputActor->SetPosition(this->InputPosition);

  this->ExitTextInputCallbackCommand = NULL;
  this->TextInputQuery = NULL;
  this->CurrentTextInput = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVRenderer::~vtkSVRenderer()
{
  if (this->Renderer != NULL)
  {
    this->Renderer->Delete();
    this->Renderer = NULL;
  }
  if (this->RenderWindow != NULL)
  {
    this->RenderWindow->Delete();
    this->RenderWindow = NULL;
  }
  if (this->Renderer != NULL)
  {
    this->RenderWindowInteractor->Delete();
    this->RenderWindowInteractor = NULL;
  }
  if (this->TextActor != NULL)
  {
    this->TextActor->Delete();
    this->TextActor = NULL;
  }
  if (this->TextInputActor != NULL)
  {
    this->TextInputActor->Delete();
    this->TextInputActor = NULL;
  }
  if (this->TrackballCamera != NULL)
  {
    this->TrackballCamera->Delete();
    this->TrackballCamera = NULL;
  }
  if (this->ResetCameraCallbackCommand != NULL)
  {
    this->ResetCameraCallbackCommand->Delete();
    this->ResetCameraCallbackCommand = NULL;
  }
  if (this->QuitRendererCallbackCommand != NULL)
  {
    this->QuitRendererCallbackCommand->Delete();
    this->QuitRendererCallbackCommand = NULL;
  }
  if (this->ExitTextInputCallbackCommand != NULL)
  {
    this->ExitTextInputCallbackCommand->Delete();
    this->ExitTextInputCallbackCommand = NULL;
  }
}

// ----------------------
// Render
// ----------------------
int vtkSVRenderer::Render(int interactive)
{
  if (interactive)
  {
    this->RenderWindowInteractor->Initialize();
  }
  this->RenderWindow->SetWindowName("vtkSV");

  std::string textActorInput;

  for (int i=0; i<this->KeyBindings.size(); i++)
    textActorInput = textActorInput + "\n" + this->KeyBindings[i].key + ": " + this->KeyBindings[i].text;

  this->TextActor->SetInput(textActorInput.c_str());
  this->Renderer->AddActor(this->TextActor);

  this->RenderWindow->Render();

  if (interactive)
  {
    this->RenderWindowInteractor->Start();
  }

  return SV_OK;
}

// ----------------------
// AddKeyBinding
// ----------------------
int vtkSVRenderer::AddKeyBinding(std::string key, std::string text,
                                 vtkCallbackCommand *callback, std::string group)
{
  Binding newKey;
  newKey.key = key;
  newKey.text = text;
  newKey.callback = callback;
  newKey.group = group;

  this->KeyBindings.push_back(newKey);

  return SV_OK;
}

// ----------------------
// RemovKeyBinding
// ----------------------
int vtkSVRenderer::RemoveKeyBinding(std::string key)
{
  std::vector<int> deleteList;
  for (int i=0; i<this->KeyBindings.size(); i++)
  {
    if (this->KeyBindings[i].key == key)
    {
      deleteList.push_back(i);
    }
  }

  for (int i=0; i<deleteList.size(); i++)
  {
      std::vector<Binding>::iterator delElem = this->KeyBindings.begin() + deleteList[i];
      this->KeyBindings.erase(delElem, this->KeyBindings.end());
  }

  return SV_OK;
}

// ----------------------
// PromptAsync
// ----------------------
int vtkSVRenderer::PromptAsync(std::string queryText, vtkCallbackCommand *callback)
{
  this->SetTextInputQuery(queryText.c_str());
  this->ExitTextInputCallbackCommand = callback;
  this->UpdateTextInput();
  this->EnterTextInputMode(0);

  return SV_OK;
}

// ----------------------
// EnterTextInputMode
// ----------------------
int vtkSVRenderer::EnterTextInputMode(int interactive)
{
  this->SetCurrentTextInput("");
  this->Renderer->AddActor(this->TextInputActor);
  this->Renderer->RemoveActor(this->TextActor);
  this->UpdateTextInput();
  this->TextInputMode = 1;
  this->Render(interactive);

  return SV_OK;
}

// ----------------------
// ExitTextInputMode
// ----------------------
int vtkSVRenderer::ExitTextInputMode()
{
  this->Renderer->RemoveActor(this->TextInputActor);
  this->Renderer->AddActor(this->TextActor);
  this->RenderWindow->Render();
  this->TextInputMode = 0;

  //if (this->ExitTextInputCallbackCommand != NULL)
  //  this->ExitTextInputCallbackCommand(this->CurrentTextInput);

  if (this->ExitAfterTextInputMode)
    this->RenderWindowInteractor->ExitCallback();

  return SV_OK;
}

// ----------------------
// Close
// ----------------------
int vtkSVRenderer::Close()
{
  this->RenderWindowInteractor->TerminateApp();
  return SV_OK;
}

// ----------------------
// ResetCameraCallback
// ----------------------
void vtkSVRenderer::ResetCameraCallback( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* clientData, void* vtkNotUsed(callData) )
{
   vtkSVRenderer* parent =
     static_cast<vtkSVRenderer*>(clientData);

  parent->Renderer->ResetCamera();
  parent->RenderWindow->Render();
}

// ----------------------
// QuitRendererCallback
// ----------------------
void vtkSVRenderer::QuitRendererCallback( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* clientData, void* vtkNotUsed(callData) )
{
   vtkSVRenderer* parent =
     static_cast<vtkSVRenderer*>(clientData);

  //parent->SetPrintLog("Quit renderer")
  parent->Renderer->RemoveActor(parent->TextActor);
  parent->RenderWindowInteractor->ExitCallback();
}

// ----------------------
// KeyPressCallback
// ----------------------
void vtkSVRenderer::KeyPressCallback( vtkObject* caller, long unsigned int eventId, void* clientData, void* callData )
{
  std::cout << "Keypress callback" << std::endl;

   vtkSVRenderer* parent =
     static_cast<vtkSVRenderer*>(clientData);

  std::cout << "Pressed: " << parent->GetRenderWindowInteractor()->GetKeySym() << std::endl;
  std::string key = parent->GetRenderWindowInteractor()->GetKeySym();

  if (key == "Escape")
  {
    if (parent->TextInputMode)
      parent->TextInputMode = 0;
    else
      parent->TextInputMode = 1;
  }
  if (parent->TextInputMode)
  {
    if (key == "Return" || key == "Enter")
    {
      std::cout << "Exit text input mode" << std::endl;
      parent->ExitTextInputMode();
      return;
    }

    if (!strncmp(key.c_str(), "KP_", 3))
      key = key.substr(3);

    if (key == "space")
      key = " ";
    else if (key == "minus" || key == "Subtract")
      key = "-";
    else if (key == "period" || key == "Decimal")
      key = ".";
    else if (key.length() > 1 && (key != "Backspace" || key != "BackSpace"))
      key = "";

    if (key == "Backspace" || key == "BackSpace")
    {
      std::string textInput = parent->CurrentTextInput;
      if (textInput.length() > 0)
        parent->SetCurrentTextInput(textInput.c_str());
    }
    else if (key != "")
    {
      std::string textInput = parent->CurrentTextInput + key;
      parent->SetCurrentTextInput(textInput.c_str());
    }

    parent->UpdateTextInput();
    return;
  }

  int isKey = -1;
  for (int i=0; i<parent->KeyBindings.size(); i++)
  {
    if (parent->KeyBindings[i].key == key)
      isKey = i;
  }

  if (isKey != -1 && parent->KeyBindings[isKey].callback != NULL)
  {
    parent->KeyBindings[isKey].callback->Execute(caller, eventId, callData);
  }
  else
  {
    std::cout << key << "is not a bound key" << std::endl;
    if (key == "plus")
      key = "+";
    if (key == "minus")
      key = "-";
    if (key == "equal")
      key = "=";
    isKey = -1;
    for (int i=0; i<parent->KeyBindings.size(); i++)
    {
      if (parent->KeyBindings[i].key == key)
        isKey = i;
    }
    if (isKey != -1 && parent->KeyBindings[isKey].callback != NULL)
    {
      parent->KeyBindings[isKey].callback->Execute(caller, eventId, callData);
    }
  }
}

// ----------------------
// UpdateTextInput
// ----------------------
void vtkSVRenderer::UpdateTextInput()
{
  if (this->TextInputQuery != NULL)
  {
    std::string inputText;

    if (this->CurrentTextInput != NULL)
    {
      std::string queryText   = this->TextInputQuery;
      std::string currentText = this->CurrentTextInput;
      inputText = queryText + currentText + "_";
    }
    else
      inputText = this->TextInputQuery;

    this->TextInputActor->SetInput(inputText.c_str());
    this->Renderer->AddActor(this->TextInputActor);
  }
  else
  {
    this->Renderer->RemoveActor(this->TextInputActor);
  }
  this->RenderWindow->Render();
}

// ----------------------
// CharCallback
// ----------------------
void vtkSVRenderer::CharCallback( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* vtkNotUsed(clientData), void* vtkNotUsed(callData) )
{
  return;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVRenderer::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}
