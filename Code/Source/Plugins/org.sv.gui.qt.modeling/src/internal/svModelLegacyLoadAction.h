#ifndef SVMODELLEGACYLOADACTION_H
#define SVMODELLEGACYLOADACTION_H

#include <org_sv_gui_qt_modeling_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_MODELING svModelLegacyLoadAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svModelLegacyLoadAction();
  ~svModelLegacyLoadAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svModelLegacyLoadAction(const svModelLegacyLoadAction &);
  svModelLegacyLoadAction & operator=(const svModelLegacyLoadAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
