#ifndef SVCONTOURGROUPPOINT3DSIZEACTION_H
#define SVCONTOURGROUPHPOINT3DSIZEACTION_H

#include <org_sv_gui_qt_segmentation_Export.h>

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_SEGMENTATION svContourGroupPoint3DSizeAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svContourGroupPoint3DSizeAction();
  ~svContourGroupPoint3DSizeAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svContourGroupPoint3DSizeAction(const svContourGroupPoint3DSizeAction &);
  svContourGroupPoint3DSizeAction & operator=(const svContourGroupPoint3DSizeAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

};

#endif
