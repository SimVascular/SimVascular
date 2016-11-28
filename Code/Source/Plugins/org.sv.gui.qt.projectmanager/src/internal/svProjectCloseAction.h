#ifndef SVPROJECTCLOSEACTION_H
#define SVPROJECTCLOSEACTION_H

#include <org_sv_gui_qt_projectmanager_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PROJECTMANAGER svProjectCloseAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svProjectCloseAction();
  ~svProjectCloseAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svProjectCloseAction(const svProjectCloseAction &);
  svProjectCloseAction & operator=(const svProjectCloseAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
