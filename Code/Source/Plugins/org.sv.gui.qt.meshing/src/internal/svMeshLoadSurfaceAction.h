#ifndef SVMESHLOADSURFACEACTION_H
#define SVMESHLOADSURFACEACTION_H

#include <org_sv_gui_qt_meshing_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_MESHING svMeshLoadSurfaceAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svMeshLoadSurfaceAction();
  ~svMeshLoadSurfaceAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svMeshLoadSurfaceAction(const svMeshLoadSurfaceAction &);
  svMeshLoadSurfaceAction & operator=(const svMeshLoadSurfaceAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
