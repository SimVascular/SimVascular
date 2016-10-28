#ifndef SVPROJECTSHOWMODELEDGESACTION_H
#define SVPROJECTSHOWMODELEDGESACTION_H

#include <org_sv_gui_qt_projectmanager_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PROJECTMANAGER svProjectShowModelEdgesAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svProjectShowModelEdgesAction();
  ~svProjectShowModelEdgesAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svProjectShowModelEdgesAction(const svProjectShowModelEdgesAction &);
  svProjectShowModelEdgesAction & operator=(const svProjectShowModelEdgesAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
