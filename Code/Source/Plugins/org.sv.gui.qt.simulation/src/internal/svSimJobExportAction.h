#ifndef SVSIMJOBEXPORTACTION_H
#define SVSIMJOBEXPORTACTION_H

#include <org_sv_gui_qt_simulation_Export.h>

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_SIMULATION svSimJobExportAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svSimJobExportAction();
  ~svSimJobExportAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  svSimJobExportAction(const svSimJobExportAction &);
  svSimJobExportAction & operator=(const svSimJobExportAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

};

#endif
