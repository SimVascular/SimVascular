#ifndef SVPATHPOINT2DSIZEACTION_H
#define SVPATHPOINT2DSIZEACTION_H

#include <org_sv_gui_qt_pathplanning_Export.h>

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PATHPLANNING svPathPoint2DSizeAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svPathPoint2DSizeAction();
  ~svPathPoint2DSizeAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svPathPoint2DSizeAction(const svPathPoint2DSizeAction &);
  svPathPoint2DSizeAction & operator=(const svPathPoint2DSizeAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

};

#endif
