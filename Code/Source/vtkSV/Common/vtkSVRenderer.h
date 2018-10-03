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

/**
 *  \class  vtkSVRenderer
 *  \brief This sets up a render window to display objects and allow interaction
 *  with objects as well as input modes. A majority of this is very similar
 *  to vmtks renderer.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVRenderer_h
#define vtkSVRenderer_h

#include "vtkDataObject.h"
#include "vtkRenderer.h"
#include "vtkCallbackCommand.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkInteractorStyleTrackballCamera.h"
#include "vtkTextActor.h"
#include "vtkSVCommonModule.h" // For exports

#include <vector>

struct Binding
{
  std::string key;
  std::string text;
  vtkCallbackCommand *callback = NULL;
  std::string group;
};

class VTKSVCOMMON_EXPORT vtkSVRenderer : public vtkDataObject
{
public:
  static vtkSVRenderer* New();

  vtkTypeMacro(vtkSVRenderer,vtkDataObject);

  //@{
  /** \brief Get and set the renderer  */
  vtkSetObjectMacro(Renderer, vtkRenderer);
  vtkGetObjectMacro(Renderer, vtkRenderer);
  //@}

  //@{
  /** \brief Get and set the render window  */
  vtkSetObjectMacro(RenderWindow, vtkRenderWindow);
  vtkGetObjectMacro(RenderWindow, vtkRenderWindow);
  //@}

  //@{
  /** \brief Get and set the render window interactor */
  vtkSetObjectMacro(RenderWindowInteractor, vtkRenderWindowInteractor);
  vtkGetObjectMacro(RenderWindowInteractor, vtkRenderWindowInteractor);
  //@}

  //@{
  /** \brief Get and set the text to show up on the window that asks the user for
   *  input. Follow with UpdateTextInput() */
  vtkSetStringMacro(TextInputQuery);
  vtkGetStringMacro(TextInputQuery);
  //@}

  //@{
  /** \brief Get and set the text that was input */
  vtkSetStringMacro(CurrentTextInput);
  vtkGetStringMacro(CurrentTextInput);
  //@}

  //@{
  /** \brief Indicate whether window should be interactive */
  int Render(int interactive=1);
  //@}

  //@{
  /** \brief Add a binding to a key. Must provide the key, the text for what the binding does,
   *  the callback command to be performed upon keypress and an organization string group */
  int AddKeyBinding(std::string key, std::string text,
                    vtkCallbackCommand *callback, std::string group);
  //@}

  //@{
  /** \brief Remove a key binding*/
  int RemoveKeyBinding(std::string key);
  //@}

  //@{
  /** \brief Update and finish text input mode */
  int PromptAsync(std::string queryText, vtkCallbackCommand *callback);
  //@}

  //@{
  /** \brief Enter text input mode with or without interaction */
  int EnterTextInputMode(int interactive=1);
  //@}

  //@{
  /** \brief Exit text input */
  int ExitTextInputMode();
  //@}

  //@{
  /** \brief Exit renderer */
  int Close();
  //@}

  //@{
  /** \brief Update what the renderer says */
  void UpdateTextInput();
  //@}

  //@{
  /** \brief Reset the camera */
  static void ResetCameraCallback( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* clientData, void* vtkNotUsed(callData) );
  //@}

  //@{
  /** \brief Quit the renderer callback */
  static void QuitRendererCallback( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* clientData, void* vtkNotUsed(callData) );
  //@}

  //@{
  /** \brief Formulate the keypress callback */
  static void KeyPressCallback( vtkObject* caller, long unsigned int eventId, void* clientData, void* callData );
  //@}

  //@{
  /** \brief Formulate the char callback */
  static void CharCallback( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* vtkNotUsed(clientData), void* vtkNotUsed(callData) );
  //@}

  //@{
  /** \brief Print */
  void PrintSelf(ostream& os, vtkIndent indent) override;
  //@}

protected:
  vtkSVRenderer();
  ~vtkSVRenderer();

  // Render stuff
  vtkRenderer *Renderer;
  vtkRenderWindow *RenderWindow;
  vtkRenderWindowInteractor *RenderWindowInteractor;
  vtkInteractorStyleTrackballCamera *TrackballCamera;

  // Text stuff
  vtkTextActor *TextActor;
  vtkTextActor *TextInputActor;

  // Callbacks
  vtkCallbackCommand *ResetCameraCallbackCommand;
  vtkCallbackCommand *QuitRendererCallbackCommand;
  vtkCallbackCommand *ExitTextInputCallbackCommand;

  // Camera
  vtkCamera *Camera;

  // Int member data
  int WindowSize[2];
  int WindowPosition[2];
  int Annotations;
  int PointSmoothing;
  int LineSmoothing;
  int PolygonSmoothing;
  int TextInputMode;
  int ExitAfterTextInputMode;
  int Interactive;

  // Double member data
  double Background[3];
  double InputPosition[2];
  double Position[2];

  // Char member data
  char* TextInputQuery;
  char* CurrentTextInput;

  // Bindings
  std::vector<Binding> KeyBindings;

private:
  vtkSVRenderer(const vtkSVRenderer&);  // Not implemented.
  void operator=(const vtkSVRenderer&);  // Not implemented.
};

#endif
