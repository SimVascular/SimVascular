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

#include "sv4gui_ImageCenterlineInteractor.h"
#include "sv4gui_ImageLinesContainer.h"

#include "mitkInteractionPositionEvent.h"
#include "mitkInternalEvent.h"
#include "mitkBaseRenderer.h"
#include "mitkRenderingManager.h"

#include <usModuleRegistry.h>
#include <usGetModuleContext.h>
#include <usModule.h>
#include <usModuleContext.h>

//---------------------------
// sv4guiImageCenterlineInteractor
//---------------------------
//
sv4guiImageCenterlineInteractor::sv4guiImageCenterlineInteractor()
{
}

sv4guiImageCenterlineInteractor::~sv4guiImageCenterlineInteractor()
{
}

//----------------------------
// ConnectActionsAndFunctions
//----------------------------
//
void sv4guiImageCenterlineInteractor::ConnectActionsAndFunctions()
{
  CONNECT_CONDITION("is_over_centerline", IsOverCenterline);
  CONNECT_FUNCTION( "select_centerline" , SelectCenterline);
}

//------------------
// IsOverCenterline
//------------------
// Check if the mouse pointer is over a centerline.
//
bool sv4guiImageCenterlineInteractor::IsOverCenterline(const mitk::InteractionEvent* interactionEvent)
{
  #define ndbg_IsOverCenterline
  #ifdef dbg_IsOverCenterline
  std::cout << "========== sv4guiImageCenterlineInteractor::IsOverCenterline ========== " << std::endl;
  #endif
  const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
  if (positionEvent == nullptr) {
      return false;
  }

  auto linesContainer = static_cast<sv4guiImageLinesContainer*>(GetDataNode()->GetData());
  auto polydata = linesContainer->GetLines();

  if (polydata == nullptr) { 
      return false;
  }

  // Get the position of the mouse pointer.
  mitk::Point3D point3d = positionEvent->GetPositionInWorld();
  m_currentPickedPoint = point3d;
  return true;

  // Find the seed point within m_seedRadius tolerance..
  double tol = m_selectRadius;
  bool selected = false;
  //seeds->FindNearestCenterline(point3d[0], point3d[1], point3d[2], tol, startID, endID);

  bool found;
  //linesContainer->FindPointOnCenterline(point3d[0], point3d[1], point3d[2], tol, found);

/*
  if (updateGraphics) { 
     interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
  }
*/

  return selected;
}

//------------------
// SelectCenterline
//------------------
//
void sv4guiImageCenterlineInteractor::SelectCenterline(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
  #define dbg_SelectCenterline 
  #ifdef dbg_SelectCenterline 
  std::cout << "========== sv4guiImageCenterlineInteractor::SelectCenterline ========== " << std::endl;
  #endif

  const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );

  /*
  if (positionEvent == NULL) {
      std::cout << "[SelectCenterline] positionEvent == NULL" << std::endl;
      return;
  }
  */

  auto linesContainer = static_cast<sv4guiImageLinesContainer*>(GetDataNode()->GetData());
  auto centerlines = linesContainer->GetLines();

  if (centerlines == nullptr) {
      std::cout << "[SelectCenterline] polydata == nullptr " << std::endl;
      return;
  }

  // Get the position of the mouse pointer.
  /*
  mitk::Point3D point3d = positionEvent->GetPositionInWorld();
  m_currentPickedPoint = point3d;
  */

  // Find the seed point within m_seedRadius tolerance..
  double tol = m_selectRadius;
  vtkIdType cellID; 
  int subID;
  bool selected = false;
  bool found;
  double closestPoint[3];
  linesContainer->FindPointOnCenterline(m_currentPickedPoint[0], m_currentPickedPoint[1], m_currentPickedPoint[2], tol, found, 
      closestPoint, cellID, subID);

  auto pointSet = vtkSmartPointer<vtkPolyData>::New();
  pointSet->SetPoints(centerlines->GetPoints());
  int index = pointSet->FindPoint(closestPoint);
  std::cout << "[SelectCenterline] CellID: " << cellID << std::endl;
  std::cout << "[SelectCenterline] subID: " << subID << std::endl;
  std::cout << "[SelectCenterline] Index: " << index << std::endl;

}
