#ifndef SVPATHLEGACYSAVEACTION_H
#define SVPATHLEGACYSAVEACTION_H

#include <org_sv_gui_qt_pathplanning_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PATHPLANNING svPathLegacySaveAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svPathLegacySaveAction();
  ~svPathLegacySaveAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svPathLegacySaveAction(const svPathLegacySaveAction &);
  svPathLegacySaveAction & operator=(const svPathLegacySaveAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
