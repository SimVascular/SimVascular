#ifndef SVPATHLOADACTION_H
#define SVPATHLOADACTION_H

#include <org_sv_gui_qt_pathplanning_Export.h>

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_PATHPLANNING svPathLoadAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svPathLoadAction();
  ~svPathLoadAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svPathLoadAction(const svPathLoadAction &);
  svPathLoadAction & operator=(const svPathLoadAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
