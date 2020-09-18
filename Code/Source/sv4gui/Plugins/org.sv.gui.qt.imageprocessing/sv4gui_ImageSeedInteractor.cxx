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
}

//------------
// IsOverSeed
//------------
// Check if the mouse pointer is over a seed.
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

  int numStartSeeds = seeds->getNumStartSeeds();
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
  m_selectedSeed = seeds->findNearestSeed(point3d[0], point3d[1], point3d[2], tol);

  #ifdef dbg_IsOverSeed
  std::cout << "[IsOverSeed] m_selectedSeed[0]: " << m_selectedSeed[0] << std::endl;
  std::cout << "[IsOverSeed] m_selectedSeed[1]: " << m_selectedSeed[1] << std::endl;
  std::cout << "[IsOverSeed] Point: " << point3d[0] << "  " << point3d[1] << "  " << point3d[2] << std::endl;
  #endif

  // Check for a start seed found.
  if (m_selectedSeed[0] == -1){
      bool updateGraphics = false;
      if (seeds->selectStartSeed) {
          updateGraphics = true;
      }
      seeds->selectStartSeed = false;
      if (updateGraphics) { 
          interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
      }
  } else { 
    seeds->selectStartSeed = true;
    seeds->selectStartSeedIndex = m_selectedSeed[0];
    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
    return true;
  } 

  // Check for an end seed found.
  if (m_selectedSeed[1] == -1){
      bool updateGraphics = false;
      if (seeds->selectEndSeed) {
          updateGraphics = true;
      }
      seeds->selectEndSeed = false;
      if (updateGraphics) {
          interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
      }
  } else {
    seeds->selectEndSeed = true;
    seeds->selectEndSeedIndex = m_selectedSeed[1];
    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
    return true;
  }

  return false;
}

//---------
// AddSeed
//---------
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

  seeds->addStartSeed((double)m_currentPickedPoint[0], (double)m_currentPickedPoint[1], (double)m_currentPickedPoint[2]);

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
//
void sv4guiImageSeedInteractor::AddEndSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
  IsOverSeed(interactionEvent);

  sv4guiImageSeedContainer* seeds = static_cast< sv4guiImageSeedContainer* >( GetDataNode()->GetData() );

  if(seeds==NULL || m_currentStartSeed < 0) {
      return;
  }

  seeds->addEndSeed((double)m_currentPickedPoint[0], (double)m_currentPickedPoint[1], (double)m_currentPickedPoint[2], m_currentStartSeed);

  // interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

//------------
// DeleteSeed
//------------
// Delete a start or end seed.
//
void sv4guiImageSeedInteractor::DeleteSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
  std::cout << "========== sv4guiImageSeedInteractor::DeleteSeed ========== " << std::endl;
  auto seeds = static_cast< sv4guiImageSeedContainer*>(GetDataNode()->GetData());
  if (seeds == NULL) {
      return;
  }

  int numStartSeeds = seeds->getNumStartSeeds();
  std::cout << "[DeleteSeed] numStartSeeds: " << numStartSeeds << std::endl;

  std::cout << "[DeleteSeed] seeds->selectStartSeed: " << seeds->selectStartSeed << std::endl;
  std::cout << "[DeleteSeed] seeds->selectStartSeedIndex: " << seeds->selectStartSeedIndex<< std::endl;

  std::cout << "[DeleteSeed] seeds->selectEndSeed: " << seeds->selectEndSeed << std::endl;
  std::cout << "[DeleteSeed] seeds->selectEndSeedIndex: " << seeds->selectEndSeedIndex<< std::endl;

  // If a start seed is selected then delete it and all of its related end seeds.
  if (seeds->selectStartSeed) {
      std::cout << "[DeleteSeed] Delete start seed and all of its end seed." << std::endl;
      int startIndex = seeds->selectEndSeedIndex;
      int endIndex = -1;
      seeds->deleteSeed(startIndex, endIndex);
      int numStartSeeds = seeds->getNumStartSeeds();
      std::cout << "[DeleteSeed] After delete: numStartSeeds: " << numStartSeeds << std::endl;
  }

/*
  IsOverSeed(interactionEvent);

  if (m_selectedSeed[0] == -1) return;

  sv4guiImageSeedContainer* seeds = static_cast< sv4guiImageSeedContainer* >( GetDataNode()->GetData() );

  if(seeds==NULL)
      return;

  seeds->deleteSeed(m_selectedSeed[0], m_selectedSeed[1]);
  if (m_selectedSeed[1] == -1){
    m_currentStartSeed -= 1;
  }
*/
  interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}
