#ifndef sv4guiImageSEEDINTERACTOR_H
#define sv4guiImageSEEDINTERACTOR_H

#include <mitkDataInteractor.h>
class sv4guiImageSeedInteractor : public mitk::DataInteractor
{
public:
  mitkClassMacro(sv4guiImageSeedInteractor, mitk::DataInteractor)
  itkFactorylessNewMacro(Self)
  itkCloneMacro(Self)
  double m_seedRadius = 0.5;

protected:
  sv4guiImageSeedInteractor();
  ~sv4guiImageSeedInteractor();

  virtual void ConnectActionsAndFunctions() override;

  bool IsOverSeed( const mitk::InteractionEvent* interactionEvent );

  void AddSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

  void AddEndSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

  void DeleteSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

private:
  std::vector<int> m_selectedSeed;
  mitk::Point3D m_currentPickedPoint;
  int m_currentStartSeed = -1;
};

#endif // sv4guiImageSEEDINTERACTOR_H
