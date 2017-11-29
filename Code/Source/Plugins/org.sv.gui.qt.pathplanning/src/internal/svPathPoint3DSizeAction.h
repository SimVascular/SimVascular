#ifndef SVPATHPOINT3DSIZEACTION_H
#define SVPATHPOINT3DSIZEACTION_H

#include <org_sv_gui_qt_pathplanning_Export.h>

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PATHPLANNING svPathPoint3DSizeAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svPathPoint3DSizeAction();
  ~svPathPoint3DSizeAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svPathPoint3DSizeAction(const svPathPoint3DSizeAction &);
  svPathPoint3DSizeAction & operator=(const svPathPoint3DSizeAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

};

#endif
