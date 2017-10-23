#ifndef SVSEG3DCREATEACTION_H
#define SVSEG3DCREATEACTION_H

#include <org_sv_gui_qt_segmentation_Export.h>

#include "svDataNodeOperationInterface.h"

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_SEGMENTATION svSeg3DCreateAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svSeg3DCreateAction();
  ~svSeg3DCreateAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svSeg3DCreateAction(const svSeg3DCreateAction &);
  svSeg3DCreateAction & operator=(const svSeg3DCreateAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

  svDataNodeOperationInterface* m_Interface;
};

#endif
