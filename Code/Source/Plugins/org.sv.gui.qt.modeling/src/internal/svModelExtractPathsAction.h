#ifndef SVMODELEXTRACTPATHSACTION_H
#define SVMODELEXTRACTPATHSACTION_H

#include <org_sv_gui_qt_modeling_Export.h>

#include <mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>
#include <QThread>

class SV_QT_MODELING svModelExtractPathsAction : public QObject, public mitk::IContextMenuAction
{
    Q_OBJECT
    Q_INTERFACES(mitk::IContextMenuAction)

public:
    svModelExtractPathsAction();
    ~svModelExtractPathsAction();

    // IContextMenuAction
    void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
    void SetDataStorage(mitk::DataStorage *dataStorage) override;
    void SetSmoothed(bool smoothed) override {}
    void SetDecimated(bool decimated) override {}
    void SetFunctionality(berry::QtViewPart *functionality) override {}

public slots:
    void UpdateStatus();

private:
    svModelExtractPathsAction(const svModelExtractPathsAction &);
    svModelExtractPathsAction & operator=(const svModelExtractPathsAction &);

    mitk::DataStorage::Pointer m_DataStorage;

    mitk::DataNode::Pointer m_ProjFolderNode;

    class WorkThread : public QThread
    {
        //      Q_OBJECT
    public:
        WorkThread(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode);

        QString GetStatus(){return m_Status;}

        mitk::DataNode::Pointer GetPathFolderNode(){return m_PathFolderNode;}

        std::vector<mitk::DataNode::Pointer> GetPathNodes(){return m_PathNodes;}

    private:

        void run();

        mitk::DataNode::Pointer m_SelectedNode;

        mitk::DataStorage::Pointer mm_DataStorage;

        mitk::DataNode::Pointer m_PathFolderNode;

        QString m_Status;

        std::vector<mitk::DataNode::Pointer> m_PathNodes;

    };

    WorkThread* m_Thread;

};

#endif
