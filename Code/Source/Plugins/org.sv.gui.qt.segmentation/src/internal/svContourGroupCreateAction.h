#ifndef SVCONTOURGROUPCREATEACTION_H
#define SVCONTOURGROUPCREATEACTION_H

#include <org_sv_gui_qt_segmentation_Export.h>

#include "svContourGroupCreate.h"

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_SEGMENTATION svContourGroupCreateAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svContourGroupCreateAction();
  ~svContourGroupCreateAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svContourGroupCreateAction(const svContourGroupCreateAction &);
  svContourGroupCreateAction & operator=(const svContourGroupCreateAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

  svContourGroupCreate* m_ContourGroupCreateWidget;
};

#endif
