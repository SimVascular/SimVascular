#include "sv4gui_ImageSeedInteractor.h"
#include "mitkInteractionPositionEvent.h"
#include "mitkInternalEvent.h"
#include "sv4gui_ImageSeedContainer.h"
#include "mitkBaseRenderer.h"
#include "mitkRenderingManager.h"
#include <usModuleRegistry.h>
#include <usGetModuleContext.h>
#include <usModule.h>
#include <usModuleContext.h>

sv4guiImageSeedInteractor::sv4guiImageSeedInteractor(){
  m_selectedSeed.push_back(-1);
  m_selectedSeed.push_back(-1);
}

sv4guiImageSeedInteractor::~sv4guiImageSeedInteractor(){

}

void sv4guiImageSeedInteractor::ConnectActionsAndFunctions(){
  std::cout << "connect actions and functions\n";
  CONNECT_CONDITION("is_over_seed", IsOverSeed);

  CONNECT_FUNCTION( "add_seed"    , AddSeed);
  CONNECT_FUNCTION( "add_end_seed", AddEndSeed);
  CONNECT_FUNCTION( "delete_seed" , DeleteSeed);
}

bool sv4guiImageSeedInteractor::IsOverSeed(const mitk::InteractionEvent * interactionEvent){
  const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
  if ( positionEvent == NULL )
      return false;

  sv4guiImageSeedContainer* seeds  =
        static_cast< sv4guiImageSeedContainer* >( GetDataNode()->GetData() );

  if(seeds==NULL)
      return false;

  mitk::Point3D point3d = positionEvent->GetPositionInWorld();
  m_currentPickedPoint = point3d;

  seeds->hoverPoint[0] = (double)point3d[0];
  seeds->hoverPoint[1] = (double)point3d[1];
  seeds->hoverPoint[2] = (double)point3d[2];

  m_selectedSeed  = seeds->findNearestSeed((double)point3d[0], (double)point3d[1], (double)point3d[2], 3*m_seedRadius);
  std::cout << "selected seed " << m_selectedSeed[0] << ", " << m_selectedSeed[1] << "\n";
  interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();

  if (m_selectedSeed[0] == -1){
    return false;
  }
  else{
    return true;
  }
  return false;
}

void sv4guiImageSeedInteractor::AddSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent){
  IsOverSeed(interactionEvent);
  std::cout << "Start seed added\n";
  sv4guiImageSeedContainer* seeds  =
        static_cast< sv4guiImageSeedContainer* >( GetDataNode()->GetData() );

  if(seeds==NULL)
      return;

  seeds->addStartSeed((double)m_currentPickedPoint[0],
    (double)m_currentPickedPoint[1],
  (double)m_currentPickedPoint[2]);

  m_currentStartSeed += 1;
  std::cout << m_currentStartSeed << "\n";
  interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void sv4guiImageSeedInteractor::AddEndSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent){
  IsOverSeed(interactionEvent);
  std::cout << "end seed added\n";
  sv4guiImageSeedContainer* seeds  =
        static_cast< sv4guiImageSeedContainer* >( GetDataNode()->GetData() );

  if(seeds==NULL || m_currentStartSeed < 0)
      return;

  seeds->addEndSeed((double)m_currentPickedPoint[0],
    (double)m_currentPickedPoint[1],
  (double)m_currentPickedPoint[2],
  m_currentStartSeed);

  std::cout << m_currentStartSeed << "\n";
  interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void sv4guiImageSeedInteractor::DeleteSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent ){
  IsOverSeed(interactionEvent);

  std::cout << "seed deleted\n";
  if (m_selectedSeed[0] == -1) return;

  sv4guiImageSeedContainer* seeds  =
        static_cast< sv4guiImageSeedContainer* >( GetDataNode()->GetData() );

  if(seeds==NULL)
      return;

  seeds->deleteSeed(m_selectedSeed[0], m_selectedSeed[1]);
  if (m_selectedSeed[1] == -1){
    m_currentStartSeed -= 1;
  }
  std::cout << "deleted seed, current start seed " << m_currentStartSeed << "\n";
  interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}
