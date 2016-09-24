#ifndef SVPATHCREATEACTION_H
#define SVPATHCREATEACTION_H

#include <org_sv_gui_qt_pathplanning_Export.h>

#include "svPathCreate.h"

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PATHPLANNING svPathCreateAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svPathCreateAction();
  ~svPathCreateAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svPathCreateAction(const svPathCreateAction &);
  svPathCreateAction & operator=(const svPathCreateAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

  svPathCreate* m_PathCreateWidget;
};

#endif
