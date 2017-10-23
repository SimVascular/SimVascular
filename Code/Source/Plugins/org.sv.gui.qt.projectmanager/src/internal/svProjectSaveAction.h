#ifndef SVPROJECTSAVEACTION_H
#define SVPROJECTSAVEACTION_H

#include <org_sv_gui_qt_projectmanager_Export.h>

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PROJECTMANAGER svProjectSaveAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svProjectSaveAction();
  ~svProjectSaveAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svProjectSaveAction(const svProjectSaveAction &);
  svProjectSaveAction & operator=(const svProjectSaveAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
