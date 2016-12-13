#ifndef SVSIMULATIONVIEW_H
#define SVSIMULATIONVIEW_H

#include "svMitkSimJob.h"
#include "svModel.h"
#include "svCapBCWidget.h"
#include "svModelDataInteractor.h"

#include <QmitkFunctionality.h>

#include <berryIBerryPreferences.h>

#include <QWidget>
#include <QStandardItemModel>

namespace Ui {
class svSimulationView;
}

class svSimulationView : public QmitkFunctionality
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

    void TableCapSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void TableViewCapDoubleClicked(const QModelIndex& index);

    void TableViewCapContextMenuRequested(const QPoint& pos);

    void ShowCapBCWidget( bool checked = false );

    void SetDistalPressure( bool checked = false );

    void SetCapBC();

    void UpdateGUICap();

    void WallTypeSelectionChanged(int index);

    void TableVarSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void TableViewVarContextMenuRequested(const QPoint& pos);

    void SetVarE( bool checked = false );

    void SetVarThickness( bool checked = false );

    void UpdateGUIWall();


    void UpdateGUISolver();


    void UpdateGUIJob();

    void ExportInputFiles();

    void ExportAllFiles();

    void CreateAllFiles();

    void ImportFiles();//like rcrt.dat, cort.dat, Qhistor.dat, impt.dat,etc.

    void RunJob();

    void SetResultDir();

    void ExportResults();

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

    svSimJob* CreateJob(std::string& msg);

    bool CreateDataFiles(QString outputDir, bool outputAllFiles, bool updateJob);

    bool IsDouble(std::string value);

    bool AreDouble(std::string values, int* count = NULL);

    bool IsInt(std::string value);


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

    QStandardItemModel* m_TableModelSolver;

    QString m_PresolverPath;
    QString m_FlowsolverPath;
    bool m_UseMPI;
    QString m_MPIExecPath;
    bool m_UseCustom;
    QString m_SolverTemplatePath;
    QString m_PostsolverPath;

};

#endif // SVSIMULATIONVIEW_H
