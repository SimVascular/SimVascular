#ifndef SVSEGMENTATIONLEGACYLOADACTION_H
#define SVSEGMENTATIONLEGACYLOADACTION_H

#include <org_sv_gui_qt_segmentation_Export.h>

#include <svPath.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_SEGMENTATION svSegmentationLegacyLoadAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svSegmentationLegacyLoadAction();
  ~svSegmentationLegacyLoadAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

  svPath* GetPath(int groupPathID, mitk::DataNode::Pointer segFolderNode);

private:
  svSegmentationLegacyLoadAction(const svSegmentationLegacyLoadAction &);
  svSegmentationLegacyLoadAction & operator=(const svSegmentationLegacyLoadAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
