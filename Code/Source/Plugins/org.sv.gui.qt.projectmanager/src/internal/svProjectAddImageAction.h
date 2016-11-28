#ifndef SVPROJECTADDIMAGEACTION_H
#define SVPROJECTADDIMAGEACTION_H

#include <org_sv_gui_qt_projectmanager_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PROJECTMANAGER svProjectAddImageAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svProjectAddImageAction();
  ~svProjectAddImageAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svProjectAddImageAction(const svProjectAddImageAction &);
  svProjectAddImageAction & operator=(const svProjectAddImageAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
