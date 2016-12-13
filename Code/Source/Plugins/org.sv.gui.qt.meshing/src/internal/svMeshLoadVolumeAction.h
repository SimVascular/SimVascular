#ifndef SVMESHLOADVOLUMEACTION_H
#define SVMESHLOADVOLUMEACTION_H

#include <org_sv_gui_qt_meshing_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_MESHING svMeshLoadVolumeAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svMeshLoadVolumeAction();
  ~svMeshLoadVolumeAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svMeshLoadVolumeAction(const svMeshLoadVolumeAction &);
  svMeshLoadVolumeAction & operator=(const svMeshLoadVolumeAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
