#ifndef SVMODELLEGACYSAVEACTION_H
#define SVMODELLEGACYSAVEACTION_H

#include <org_sv_gui_qt_modeling_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>

class SV_QT_MODELING svModelLegacySaveAction : public QObject, public mitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(mitk::IContextMenuAction)

public:
  svModelLegacySaveAction();
  ~svModelLegacySaveAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override {}

private:
  svModelLegacySaveAction(const svModelLegacySaveAction &);
  svModelLegacySaveAction & operator=(const svModelLegacySaveAction &);

  mitk::DataStorage::Pointer m_DataStorage;

};

#endif
