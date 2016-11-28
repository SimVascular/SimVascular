#ifndef SVMODELCREATEACTION_H
#define SVMODELCREATEACTION_H

#include <org_sv_gui_qt_modeling_Export.h>

#include "svModelCreate.h"

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_MODELING svModelCreateAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svModelCreateAction();
  ~svModelCreateAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svModelCreateAction(const svModelCreateAction &);
  svModelCreateAction & operator=(const svModelCreateAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

  svModelCreate* m_ModelCreateWidget;
};

#endif
