#ifndef SVMODELFACEINFOEXPORTACTION_H
#define SVMODELFACEINFOEXPORTACTION_H

#include <org_sv_gui_qt_modeling_Export.h>

#include <svmitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_MODELING svModelFaceInfoExportAction : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  svModelFaceInfoExportAction();
  ~svModelFaceInfoExportAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svModelFaceInfoExportAction(const svModelFaceInfoExportAction &);
  svModelFaceInfoExportAction & operator=(const svModelFaceInfoExportAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
