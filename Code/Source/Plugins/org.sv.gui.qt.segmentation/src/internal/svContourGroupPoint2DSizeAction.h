#ifndef SVCONTOURGROUPPOINT2DSIZEACTION_H
#define SVCONTOURGROUPPOINT2DSIZEACTION_H

#include <org_sv_gui_qt_segmentation_Export.h>

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_SEGMENTATION svContourGroupPoint2DSizeAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svContourGroupPoint2DSizeAction();
  ~svContourGroupPoint2DSizeAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svContourGroupPoint2DSizeAction(const svContourGroupPoint2DSizeAction &);
  svContourGroupPoint2DSizeAction & operator=(const svContourGroupPoint2DSizeAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

};

#endif
