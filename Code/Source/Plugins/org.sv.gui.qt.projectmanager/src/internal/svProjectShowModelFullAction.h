#ifndef SVPROJECTSHOWMODELFULLACTION_H
#define SVPROJECTSHOWMODELFULLACTION_H

#include <org_sv_gui_qt_projectmanager_Export.h>

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PROJECTMANAGER svProjectShowModelFullAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svProjectShowModelFullAction();
  ~svProjectShowModelFullAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svProjectShowModelFullAction(const svProjectShowModelFullAction &);
  svProjectShowModelFullAction & operator=(const svProjectShowModelFullAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
