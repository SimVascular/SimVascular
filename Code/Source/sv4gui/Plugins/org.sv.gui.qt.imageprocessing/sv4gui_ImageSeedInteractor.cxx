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

#include "sv4gui_ImageSeedInteractor.h"
#include "sv4gui_ImageSeedContainer.h"

#include "mitkInteractionPositionEvent.h"
#include "mitkInternalEvent.h"
#include "mitkBaseRenderer.h"
#include "mitkRenderingManager.h"

#include <usModuleRegistry.h>
#include <usGetModuleContext.h>
#include <usModule.h>
#include <usModuleContext.h>

//---------------------------
// sv4guiImageSeedInteractor
//---------------------------
//
sv4guiImageSeedInteractor::sv4guiImageSeedInteractor()
{
  m_selectedSeed.push_back(-1);
  m_selectedSeed.push_back(-1);
}

sv4guiImageSeedInteractor::~sv4guiImageSeedInteractor()
{
}

//----------------------------
// ConnectActionsAndFunctions
//----------------------------
//
void sv4guiImageSeedInteractor::ConnectActionsAndFunctions()
{
  CONNECT_CONDITION("is_over_seed", IsOverSeed);
  //CONNECT_FUNCTION( "add_start_seed"    , AddSeed);
  //CONNECT_FUNCTION( "add_end_seed", AddEndSeed);
  CONNECT_FUNCTION( "remove_seed" , DeleteSeed);
  //CONNECT_FUNCTION( "select_centerline" , SelectCenterline);
  CONNECT_FUNCTION( "make_seed_current" , MakeSeedCurrent);
  //CONNECT_CONDITION("is_over_centerline", IsOverCenterline);
}

//------------
// IsOverSeed
//------------
// Check if the mouse pointer is over a seed.
//
// This searches the start and end seeds to determine if a seed is near
// the current mouse pointer.
//
bool sv4guiImageSeedInteractor::IsOverSeed(const mitk::InteractionEvent* interactionEvent)
{
  #define ndbg_IsOverSeed
  #ifdef dbg_IsOverSeed
  std::cout << "========== sv4guiImageSeedInteractor::IsOverSeed ========== " << std::endl;
  #endif
  const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );

  if (positionEvent == NULL) {
      return false;
  }

  auto seeds = static_cast< sv4guiImageSeedContainer*>(GetDataNode()->GetData());
  if (seeds == NULL) {
      std::cout << "[IsOverSeed] No seeds." << std::endl;
      return false;
  }

  int numStartSeeds = seeds->GetNumStartSeeds();
  if (numStartSeeds == 0) { 
      return false;
  }

  // Get the position of the mouse pointer.
  mitk::Point3D point3d = positionEvent->GetPositionInWorld();
  m_currentPickedPoint = point3d;
  seeds->hoverPoint[0] = point3d[0];
  seeds->hoverPoint[1] = point3d[1];
  seeds->hoverPoint[2] = point3d[2];

  // Find the seed point within m_seedRadius tolerance..
  double tol = m_seedRadius;
  int startID, endID;
  seeds->FindNearestSeed(point3d[0], point3d[1], point3d[2], tol, startID, endID);

  m_selectedSeed[0] = startID;
  m_selectedSeed[1] = endID;

  #ifdef dbg_IsOverSeed
  std::cout << std::endl;
  std::cout << "[IsOverSeed] startID: " << startID << std::endl;
  std::cout << "[IsOverSeed] endID: " << endID << std::endl;
  std::cout << "[IsOverSeed] Point: " << point3d[0] << "  " << point3d[1] << "  " << point3d[2] << std::endl;
  #endif

  // Check for a start seed found.
  bool updateGraphics = false;
  bool selected = false;

  if (startID == -1) {
      if (seeds->selectStartSeed) {
          updateGraphics = true;
      }
      seeds->selectStartSeed = false;
      seeds->selectStartSeedIndex = -1; 
  } else { 
      seeds->selectStartSeed = true;
      seeds->selectStartSeedIndex = startID; 
      updateGraphics = true;
      selected = true;
  } 

  // Check for an end seed found.
  if (endID == -1){
      bool updateGraphics = false;
      if (seeds->selectEndSeed) {
          updateGraphics = true;
      }
      seeds->selectEndSeed = false;
      seeds->selectEndSeedIndex = -1;
  } else {
      seeds->selectEndSeed = true;
      seeds->selectEndSeedIndex = endID;
      selected = true;
      updateGraphics = true;
  }

  if (updateGraphics) { 
     interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
  }

  return selected;
}

//------------------
// IsOverCenterline
//------------------
//
/*
bool sv4guiImageSeedInteractor::IsOverCenterline(const mitk::InteractionEvent* interactionEvent)
{
  #define dbg_IsOverCenterline 
  #ifdef dbg_IsOverCenterline 
  std::cout << "========== sv4guiImageSeedInteractor::IsOverCenterline ========== " << std::endl;
  #endif
  const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );

  if (positionEvent == NULL) {
      return false;
  }

}
*/

//---------
// AddSeed
//---------
// [TODO:DaveP] this is not currently used.
//
void sv4guiImageSeedInteractor::AddSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
  static int nvisit = 0;
  nvisit++;
  #define AddSeed
  #ifdef AddSeed
  std::cout << std::endl;
  std::cout << "========== sv4guiImageSeedInteractor::AddSeed " << nvisit << " ========== " << std::endl;
  #endif
  IsOverSeed(interactionEvent);
  sv4guiImageSeedContainer* seeds  = static_cast< sv4guiImageSeedContainer* >( GetDataNode()->GetData() );

  if (seeds == NULL) {
      return;
  }

  seeds->AddStartSeed((double)m_currentPickedPoint[0], (double)m_currentPickedPoint[1], (double)m_currentPickedPoint[2]);

  #ifdef AddSeed
  auto renderer = interactionEvent->GetSender();
 if (renderer->GetMapperID() == mitk::BaseRenderer::Standard2D) {
      std::cout << "[AddSeed] Is On 2D View " << std::endl; 
  }

  std::cout << "[AddSeed] Picked Point: " << m_currentPickedPoint[0] << "  " << m_currentPickedPoint[1] << "  " 
    << m_currentPickedPoint[2] << std::endl;
  #endif

  m_currentStartSeed += 1;

  //interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

//------------
// AddEndSeed
//------------
// [TODO:DaveP] this is not currently used.
//
void sv4guiImageSeedInteractor::AddEndSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
  IsOverSeed(interactionEvent);

  sv4guiImageSeedContainer* seeds = static_cast< sv4guiImageSeedContainer* >( GetDataNode()->GetData() );

  if(seeds==NULL || m_currentStartSeed < 0) {
      return;
  }

  seeds->AddEndSeed(m_currentPickedPoint[0], m_currentPickedPoint[1], m_currentPickedPoint[2]);

  // interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

//-----------------
// MakeSeedCurrent 
//-----------------
// Make the selected start seed the currently active start seed (i.e. can add end points to it).
//
void sv4guiImageSeedInteractor::MakeSeedCurrent(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
  std::cout << "========== sv4guiImageSeedInteractor::MakeSeedCurrent ========== " << std::endl;
  auto seeds = static_cast< sv4guiImageSeedContainer*>(GetDataNode()->GetData());
  //std::cout << "[DeleteSeed] seeds->selectStartSeed: " << seeds->selectStartSeed << std::endl;
  //std::cout << "[DeleteSeed] seeds->selectStartSeedIndex: " << seeds->selectStartSeedIndex << std::endl;

  seeds->SetActiveStartSeed(seeds->selectStartSeedIndex);

  interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

//------------
// DeleteSeed
//------------
// Delete a start or end seed.
//
void sv4guiImageSeedInteractor::DeleteSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
  #ifdef dbg_sv4guiImageSeedInteractor_DeleteSeed
  std::cout << "========== sv4guiImageSeedInteractor::DeleteSeed ========== " << std::endl;
  #endif
  auto seeds = static_cast< sv4guiImageSeedContainer*>(GetDataNode()->GetData());
  if (seeds == NULL) {
      return;
  }

  #ifdef dbg_sv4guiImageSeedInteractor_DeleteSeed
  std::cout << "[DeleteSeed] seeds->selectStartSeed: " << seeds->selectStartSeed << std::endl;
  std::cout << "[DeleteSeed] seeds->selectStartSeedIndex: " << seeds->selectStartSeedIndex << std::endl;
  std::cout << "[DeleteSeed] seeds->selectEndSeed: " << seeds->selectEndSeed << std::endl;
  std::cout << "[DeleteSeed] seeds->selectEndSeedIndex: " << seeds->selectEndSeedIndex<< std::endl;
  #endif

  // Set start and end seed IDs.
  int startIndex = -1;
  if (seeds->selectStartSeed) {
      startIndex = seeds->selectStartSeedIndex;
  }      

  int endIndex = -1;
  if (seeds->selectEndSeed) {
      endIndex = seeds->selectEndSeedIndex ;
  }

  // Delete the seed(s).
  seeds->DeleteSeed(startIndex, endIndex);

  // Reset selections.
  seeds->selectStartSeed = false;
  seeds->selectStartSeedIndex = -1; 
  seeds->selectEndSeed = false;
  seeds->selectEndSeedIndex = -1;

  interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

//------------------
// SelectCenterline
//------------------
//
/*
void sv4guiImageSeedInteractor::SelectCenterline(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
  #define dbg_SelectCenterline 
  #ifdef dbg_SelectCenterline 
  std::cout << "========== sv4guiImageSeedInteractor::SelectCenterline ========== " << std::endl;
  #endif
  const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );

  if (positionEvent == NULL) {
      return;
  }


}
*/
