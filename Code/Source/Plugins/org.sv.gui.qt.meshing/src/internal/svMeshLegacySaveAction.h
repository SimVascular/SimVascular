#ifndef SVMESHLEGACYSAVEACTION_H
#define SVMESHLEGACYSAVEACTION_H

#include <org_sv_gui_qt_meshing_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_MESHING svMeshLegacySaveAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svMeshLegacySaveAction();
  ~svMeshLegacySaveAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svMeshLegacySaveAction(const svMeshLegacySaveAction &);
  svMeshLegacySaveAction & operator=(const svMeshLegacySaveAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
