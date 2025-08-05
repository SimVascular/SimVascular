/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SV4GUI_SIMULATIONVIEW_H
#define SV4GUI_SIMULATIONVIEW_H

#include "sv4gui_MitkSimJob.h"
#include "sv4gui_Model.h"
#include "sv4gui_CapBCWidget.h"
#include "sv4gui_SplitBCWidget.h"
#include "sv4gui_QmitkFunctionality.h"

#include "sv4gui_ProcessHandler.h"
#include "sv4gui_SolverProcessHandler.h"
#include "sv4gui_SimulationPreferences.h"
#include "sv4gui_MPIPreferences.h"

#include "sv4gui_ModelDataInteractor.h"

#include <mitkIPreferences.h>

#include <QWidget>
#include <QButtonGroup>
#include <QRadioButton>
#include <QStandardItemModel>
#include <QProcess>
#include <QMessageBox>
#include <QItemSelection>
#include <QMenu>
#include <QmitkStdMultiWidget.h>

#include <map>

namespace Ui {
class sv4guiSimulationView;
}

class sv4guiSimulationView : public sv4guiQmitkFunctionality
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;
    static const QString MsgTitle;

    sv4guiSimulationView();

    virtual ~sv4guiSimulationView();

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

    // Wall properties.
    //
    void UpdateGUIWall();

    void WallTypeSelectionChanged(int index);

    void TableVarSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void TableViewVarContextMenuRequested(const QPoint& pos);

    void SetVarE( bool checked = false );

    void SetVarThickness( bool checked = false );

    // Coupled Momentum Method
    //
    void UpdateGUICmm();
    void CmmSimType_changed(bool checked);

    // Solver paramters 
    //
    void UpdateGUISolver();

    void UpdateGUIJob();

    // davep void UpdateGUIRunDir();

    void CreateAllFiles();

    // davep void ImportFiles();//like rcrt.dat, cort.dat, Qhistor.dat, impt.dat,etc.

    void RunJob();

    // davep void SetResultDir();

    void UpdateJobStatus();

    void UpdateSimJob();

    void UdpateSimJobMeshName();

    void UpdateSimJobNumProcs();

    // davep void ShowCalculateFowsWidget(bool checked = false);

    void ShowModel(bool checked = false);

    // davep void UseMpi(bool checked = false);

public:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void OnSelectionChanged(berry::IWorkbenchPart::Pointer /*part*/,
                                    const QList<mitk::DataNode::Pointer>& /*nodes*/) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

    virtual void Activated() override;

    virtual void Deactivated() override;

    virtual void Visible() override;

    virtual void Hidden() override;

    virtual void OnPreferencesChanged(const mitk::IPreferences* prefs) override;

    sv4guiSimJob* CreateJob(std::string& msg, bool checkValidity = true);

    bool CreateDataFiles(QString outputDir, bool outputAllFiles, bool updateJob, bool createFolder);

    bool IsDouble(std::string value);

    bool AreDouble(std::string values, int* count = nullptr);

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

    Ui::sv4guiSimulationView *ui;

    sv4guiMitkSimJob* m_MitkJob;

    sv4guiModel* m_Model;

    mitk::DataNode::Pointer m_JobNode;

    mitk::DataNode::Pointer m_ModelNode;

    sv4guiModelDataInteractor::Pointer m_DataInteractor;

    long m_ModelSelectFaceObserverTag;

    QmitkStdMultiWidget* m_DisplayWidget;

    QStandardItemModel* m_BasicParametersPage;

    QStandardItemModel*  m_InletOutletBCsPage;
    QMenu* m_InletOutletBCs_caps_table;

    QStandardItemModel* m_WallPropertiesPage;
    QMenu* m_WallPropertiesPage_variable_props;

    sv4guiCapBCWidget* m_CapBCWidget;

    sv4guiSplitBCWidget* m_SplitBCWidget;

    QButtonGroup* m_CmmSimType_group;

    QStandardItemModel* m_SolverParametersPage;
    std::map<int,std::string>  m_SolverParametersPageSections;

    sv4guiMPIPreferences::MpiImplementation m_MpiImplementation;

    std::string m_SolverPath;

    std::string m_SolverInputFileName = "solver.xml";

    std::string m_MPIExecPath;
    // davep bool m_UseMPI;
    // davep bool m_UseCustom;
    // davep QString m_SolverTemplatePath;

    bool m_ConnectionEnabled;

    sv4guiSimulationPreferences m_DefaultPrefs;
    sv4guiMPIPreferences m_DefaultMPIPrefs;

    void CheckSolver();
    void CheckMpi();
    // davep int GetStartTimeStep(const QString& runPath, const QString& jobPath, const int numProcs);

    std::string m_CmmSimulationType;

};

#endif // SV4GUI_SIMULATIONVIEW_H
