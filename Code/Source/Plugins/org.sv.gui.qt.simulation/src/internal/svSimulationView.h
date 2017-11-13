#ifndef SVSIMULATIONVIEW_H
#define SVSIMULATIONVIEW_H

#include "svMitkSimJob.h"
#include "svModel.h"
#include "svCapBCWidget.h"
#include "svSplitBCWidget.h"
#include "svQmitkFunctionality.h"

#include "svModelDataInteractor.h"

#include <berryIBerryPreferences.h>

#include <QWidget>
#include <QStandardItemModel>
#include <QProcess>
#include <QMessageBox>

namespace Ui {
class svSimulationView;
}

class svSimulationView : public svQmitkFunctionality
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svSimulationView();

    virtual ~svSimulationView();

public slots:

    void SaveToManager();

    void ClearAll();

    void AddObservers();

    void RemoveObservers();

    void UpdateFaceListSelection();

    void UpdateGUIBasic();

    void TableViewBasicDoubleClicked(const QModelIndex& index);

    void TableCapSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void TableViewCapDoubleClicked(const QModelIndex& index);

    void TableViewCapContextMenuRequested(const QPoint& pos);

    void ShowCapBCWidget( bool checked = false );

    void SetDistalPressure( bool checked = false );

    void SetCapBC();

    void ShowSplitBCWidget(QString splitTarget);
    void ShowSplitBCWidgetR( bool checked = false);
    void ShowSplitBCWidgetC( bool checked = false);

    void SplitCapBC();

    void UpdateGUICap();

    void WallTypeSelectionChanged(int index);

    void TableVarSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void TableViewVarContextMenuRequested(const QPoint& pos);

    void SetVarE( bool checked = false );

    void SetVarThickness( bool checked = false );

    void UpdateGUIWall();

    void UpdateGUISolver();

    void UpdateGUIJob();

    void UpdateGUIRunDir();

//    void ExportInputFiles();

//    void ExportAllFiles();

    void CreateAllFiles();

    void ImportFiles();//like rcrt.dat, cort.dat, Qhistor.dat, impt.dat,etc.

    void RunJob();

    void SetResultDir();

    void ExportResults();

    void UpdateJobStatus();

    void UpdateSimJob();

    void UdpateSimJobMeshName();

    void UpdateSimJobNumProcs();

    void SetupInternalSolverPaths();

    void ShowCalculateFowsWidget(bool checked = false);

    void ShowModel(bool checked = false);

public:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void OnSelectionChanged(std::vector<mitk::DataNode*> nodes) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

//    virtual void Activated() override;

//    virtual void Deactivated() override;

    virtual void Visible() override;

    virtual void Hidden() override;

    virtual void OnPreferencesChanged(const berry::IBerryPreferences* prefs) override;

    svSimJob* CreateJob(std::string& msg, bool checkValidity = true);

    bool CreateDataFiles(QString outputDir, bool outputAllFiles, bool updateJob, bool createFolder);

    bool IsDouble(std::string value);

    bool AreDouble(std::string values, int* count = NULL);

    bool IsInt(std::string value);

    void EnableTool(bool able);

    QString GetJobPath();

    void EnableConnection(bool able = true);

#if defined(Q_OS_WIN)
    QString FindLatestKey(QString key, QStringList keys);
    QString GetRegistryValue(QString category, QString key);
#endif

private:

    QWidget* m_Parent;

    Ui::svSimulationView *ui;

    svMitkSimJob* m_MitkJob;

    svModel* m_Model;

    mitk::DataNode::Pointer m_JobNode;

    mitk::DataNode::Pointer m_ModelNode;

    svModelDataInteractor::Pointer m_DataInteractor;

    long m_ModelSelectFaceObserverTag;

    QmitkStdMultiWidget* m_DisplayWidget;

    QStandardItemModel* m_TableModelBasic;

    QStandardItemModel* m_TableModelCap;
    QMenu* m_TableMenuCap;

    QStandardItemModel* m_TableModelVar;
    QMenu* m_TableMenuVar;

    svCapBCWidget* m_CapBCWidget;

    svSplitBCWidget* m_SplitBCWidget;

    QStandardItemModel* m_TableModelSolver;

    QString m_InternalPresolverPath;
    QString m_InternalFlowsolverPath;
    QString m_InternalFlowsolverNoMPIPath;
    QString m_InternalPostsolverPath;
    QString m_InternalMPIExecPath;

    QString m_ExternalPresolverPath;
    QString m_ExternalFlowsolverPath;
    QString m_ExternalFlowsolverNoMPIPath;
    bool m_UseMPI;
    QString m_MPIExecPath;
    bool m_UseCustom;
    QString m_SolverTemplatePath;
    QString m_ExternalPostsolverPath;
    QString m_ExternalMPIExecPath;

    bool m_ConnectionEnabled;

};

class svProcessHandler : public QObject
{
    Q_OBJECT

public:
    svProcessHandler(QProcess* process, mitk::DataNode::Pointer jobNode, bool multithreading=true, bool stoppable=true, QWidget* parent=NULL);
    virtual ~svProcessHandler();

    void Start();

    QString GetMessage(){return m_Message;}

public slots:

    void AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);

private:

    QProcess* m_Process;

    QWidget* m_Parent;

    QMessageBox* m_MessageBox;

    mitk::DataNode::Pointer m_JobNode;

    bool m_Stoppable;

    bool m_MultiThreading;

    QString m_Message;
};

class svSolverProcessHandler : public QObject
{
    Q_OBJECT

public:
    svSolverProcessHandler(QProcess* process, mitk::DataNode::Pointer jobNode, int startStep, int totalSteps, QString runDir, QWidget* parent=NULL);
    virtual ~svSolverProcessHandler();

    void Start();

    void KillProcess();

public slots:

    void AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);

    void UpdateStatus();

private:

    QProcess* m_Process;

    QWidget* m_Parent;

    QMessageBox* m_MessageBox;

    mitk::DataNode::Pointer m_JobNode;

    QTimer* m_Timer;

    int m_StartStep;

    int m_TotalSteps;

    QString m_RunDir;
};

#endif // SVSIMULATIONVIEW_H
