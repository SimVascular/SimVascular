#ifndef SVPROJECTCREATEACTION_H
#define SVPROJECTCREATEACTION_H

#include <org_sv_gui_qt_projectmanager_Export.h>

// Parent classes
#include <QObject>
#include <mitkIContextMenuAction.h>

// Data members
#include <mitkDataNode.h>

//class QmitkStdMultiWidget;

class SV_QT_PROJECTMANAGER svProjectCreateAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svProjectCreateAction();
  ~svProjectCreateAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svProjectCreateAction(const svProjectCreateAction &);
  svProjectCreateAction & operator=(const svProjectCreateAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
