#ifndef SVPATHLEGACYLOADACTION_H
#define SVPATHLEGACYLOADACTION_H

#include <org_sv_gui_qt_pathplanning_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PATHPLANNING svPathLegacyLoadAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svPathLegacyLoadAction();
  ~svPathLegacyLoadAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svPathLegacyLoadAction(const svPathLegacyLoadAction &);
  svPathLegacyLoadAction & operator=(const svPathLegacyLoadAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
