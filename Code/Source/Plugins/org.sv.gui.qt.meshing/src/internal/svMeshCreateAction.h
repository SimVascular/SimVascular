#ifndef SVMESHCREATEACTION_H
#define SVMESHCREATEACTION_H

#include <org_sv_gui_qt_meshing_Export.h>

#include "svMeshCreate.h"

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_MESHING svMeshCreateAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svMeshCreateAction();
  ~svMeshCreateAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svMeshCreateAction(const svMeshCreateAction &);
  svMeshCreateAction & operator=(const svMeshCreateAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

  svMeshCreate* m_MeshCreateWidget;
};

#endif
