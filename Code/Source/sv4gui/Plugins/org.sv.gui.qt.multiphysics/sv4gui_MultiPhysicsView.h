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

#ifndef sv4guiMultiPhysicsVIEW_H
#define sv4guiMultiPhysicsVIEW_H

#include "sv4gui_MitkMultiPhysicsJob.h"
#include "sv4gui_MultiPhysicsBCWidget.h"
#include "sv4gui_QmitkFunctionality.h"
#include "sv4gui_MultiPhysicsUtil.h"
#include "sv4gui_MultiPhysicsPreferences.h"

#include <sv4gui_QmitkFunctionality.h>
#include <mitkIPreferences.h>

#include <QWidget>
#include <QProcess>
#include <QMessageBox>
#include <QListWidget>
#include <QDoubleValidator>
#include <QIntValidator>
#include <QSettings>

namespace Ui {
class sv4guiMultiPhysicsView;
}

class sv4guiMultiPhysicsView : public sv4guiQmitkFunctionality
{
    Q_OBJECT

public:
    static const QString EXTENSION_ID;\

    sv4guiMultiPhysicsView();

    virtual ~sv4guiMultiPhysicsView();

public slots:

    void loadMesh();

    void LoadJob();

    void LoadJob(std::string jobPath, std::string jobName);

    void SetNsd(const QString &text);

    void AddMeshComplete();

    void SelectDomain(const QString &name);

    void DeleteDomain();

    void ChangeDomainType(int type);

    void AddEquation();

    void ClearEquation();

    void SelectEquation();

    void SaveProps();

    void AddOutput();

    void ClearOutput();

    void SaveOutputs();

    void SaveAdvanced();

    void ResetEquation();

    void SaveLinearSolver();

    void ShowNSWidget();

    void AddBC();

    void ModifyBC();

    void RemoveBC();

    void ShowRemesher();

    void ShowEdgeSize();

    void SaveRemesher();

    void UpdatePhysicsPanel();
    void SetupPhysicsPanel();

    void SetupDomainsPanel();

    void SetupRunSimulationPanel();

    void SaveSimulationParameters();
    void SetupSimulationParametersPanel();
    void UpdateSimulationParametersPanel();

    void CreateInputFile();

    void RunSimulation();

    void CreateNewJob();

    void SaveJob();

    void StopSimulation();

    void Initialize();

public:

    virtual void CreateQtPartControl(QWidget *parent) override;

     virtual void OnSelectionChanged(berry::IWorkbenchPart::Pointer /*part*/, const QList<mitk::DataNode::Pointer>& /*nodes*/) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

    virtual void Activated() override;

    virtual void Deactivated() override;

    virtual void Visible() override;

    virtual void Hidden() override;

    virtual void OnPreferencesChanged(const mitk::IPreferences* prefs) override;

    void DataChanged();

    QString GetJobPath();

    void UpdateJobStatus();

//    void ClearAll();

//    void AddObservers();

//    void RemoveObservers();

#if defined(Q_OS_WIN)
    QString FindLatestKey(QString key, QStringList keys);
    QString GetRegistryValue(QString category, QString key);
#endif

private:
    QWidget* m_Parent;

    Ui::sv4guiMultiPhysicsView *ui;

    sv4guiMitkMultiPhysicsJob* m_MitkJob;
    sv4guiMultiPhysicsJob* m_Job;

    mitk::DataNode::Pointer m_JobNode;

    QString m_InternalSolverPath;
    QString m_ExternalSolverPath;
    QString m_InternalMPIExecPath;

    QList<QLabel *> propL;
    QList<QLineEdit *> propB;

    QDoubleValidator* m_RealVal;
    QIntValidator* m_IntVal;

    bool m_EnableSave;

    sv4guiMultiPhysicsUtil sv4guiMultiPhysicsUtil;

    sv4guiMultiPhysicsPreferences m_DefaultPrefs;

    void SetMpiExec();
};

class sv4guiMultiPhysicsSolverProcessHandler : public QObject
{
    Q_OBJECT

public:
    sv4guiMultiPhysicsSolverProcessHandler(QProcess* process, mitk::DataNode::Pointer jobNode, int startStep, int totalSteps, QString runDir, QWidget* parent=nullptr);
    virtual ~sv4guiMultiPhysicsSolverProcessHandler();

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

#endif // sv4guiMultiPhysicsVIEW_H
