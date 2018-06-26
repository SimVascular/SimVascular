#ifndef sv4guisvFSIJobCreateACTION_H
#define sv4guisvFSIJobCreateACTION_H

#include <org_sv_gui_qt_svfsi_Export.h>

#include "sv4gui_DataNodeOperationInterface.h"

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_SVFSI sv4guisvFSIJobCreateAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  sv4guisvFSIJobCreateAction();
  ~sv4guisvFSIJobCreateAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  sv4guisvFSIJobCreateAction(const sv4guisvFSIJobCreateAction &);
  sv4guisvFSIJobCreateAction & operator=(const sv4guisvFSIJobCreateAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

  sv4guiDataNodeOperationInterface* m_Interface;
};

#endif
