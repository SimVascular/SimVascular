#ifndef SVSEGMENTATIONLOADACTION_H
#define SVSEGMENTATIONLOADACTION_H

#include <org_sv_gui_qt_segmentation_Export.h>

#include <svPath.h>

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_SEGMENTATION svSegmentationLoadAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svSegmentationLoadAction();
  ~svSegmentationLoadAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svSegmentationLoadAction(const svSegmentationLoadAction &);
  svSegmentationLoadAction & operator=(const svSegmentationLoadAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
