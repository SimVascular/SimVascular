#ifndef svSimJobCreateACTION_H
#define svSimJobCreateACTION_H

#include <org_sv_gui_qt_simulation_Export.h>

#include "svSimJobCreate.h"

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_SIMULATION svSimJobCreateAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svSimJobCreateAction();
  ~svSimJobCreateAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svSimJobCreateAction(const svSimJobCreateAction &);
  svSimJobCreateAction & operator=(const svSimJobCreateAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

  svSimJobCreate* m_SimJobCreateWidget;
};

#endif
