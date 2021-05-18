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

#include "sv4gui_svFSIView.h"
#include "ui_sv4gui_svFSIView.h"
#include "sv4gui_ProjectManager.h"
#include "sv4gui_svFSIUtil.h"
#include "sv4gui_svFSIbcClass.h"
#include "sv4gui_svFSIeqClass.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_Mesh.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkStatusBar.h>
#include <mitkIOUtil.h>

#include <berryPlatform.h>

#include <QFileDialog>
#include <QErrorMessage>
#include <QMessageBox>
#include <QProcess>
#include <QTextStream>
#include <QThread>
#include <QInputDialog>

#include <iostream>

const QString sv4guisvFSIView::EXTENSION_ID = "org.sv.views.svfsi";

sv4guisvFSIView::sv4guisvFSIView() : ui(new Ui::sv4guisvFSIView)
{
    m_MitkJob=NULL;
    m_Job=NULL;
    m_JobNode=NULL;

    m_InternalSolverPath="";
    m_ExternalSolverPath="";

    m_RealVal=NULL;
    m_IntVal=NULL;

    m_EnableSave=true;
}

sv4guisvFSIView::~sv4guisvFSIView()
{
    if(m_RealVal)
        delete m_RealVal;

    if(m_IntVal)
        delete m_IntVal;

    delete ui;
}

void sv4guisvFSIView::Initialize(){
  sv4guisvFSIUtil.setDataStorage(GetDataStorage());
  sv4guisvFSIUtil.makeDir();
  sv4guisvFSIUtil.createDataFolder();


}

//---------------------
// CreateQtPartControl
//---------------------
//
void sv4guisvFSIView::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);

    // Initialize folder nodes etc
    Initialize();

    // Define validators to check for valid integer and double input values. 
    m_RealVal = new QDoubleValidator;
    m_RealVal->setBottom(0.0);
    m_IntVal = new QIntValidator;
    m_IntVal->setBottom(1);

    // Top section of the FSI panel.
    connect(ui->btnNewJob, SIGNAL(clicked()), this, SLOT(CreateNewJob()));
    connect(ui->btnSave, SIGNAL(clicked()), this, SLOT(SaveJob()));
    connect(ui->btnLoadJob, SIGNAL(clicked()), this, SLOT(LoadJob()));
    connect(ui->loadMeshButton, SIGNAL(clicked()), this, SLOT(loadMesh()));

    // Setup Domains panel.
    SetupDomainsPanel();

    // Setup Physics panel.
    SetupPhysicsPanel();

    // Reset
    connect(ui->btnResetEq, SIGNAL(clicked()), this, SLOT(ResetEquation()));

    //  Setup the Simulation Parameters panel.
    SetupSimulationParametersPanel();

    // Setup the Run simulation panel.
    SetupRunSimulationPanel();

    ui->Subpanel_Widget->setEnabled(false);
    ui->btnSave->setEnabled(false);

    // Get paths for the external solvers
    berry::IPreferences::Pointer prefs = this->GetPreferences();
    berry::IBerryPreferences* berryprefs = dynamic_cast<berry::IBerryPreferences*>(prefs.GetPointer());
    this->OnPreferencesChanged(berryprefs);

    ui->comboBoxRemesher->setEnabled(false);
}

//--------------------
// OnSelectionChanged
//--------------------
//
void sv4guisvFSIView::OnSelectionChanged(std::vector<mitk::DataNode*> nodes)
{
    if (!IsVisible()) {
        return;
    }

    if(nodes.size()==0) {
        ui->Subpanel_Widget->setEnabled(false);
        ui->btnSave->setEnabled(false);
        return;
    }

    mitk::DataNode::Pointer jobNode=nodes.front();
    sv4guiMitksvFSIJob* mitkJob=dynamic_cast<sv4guiMitksvFSIJob*>(jobNode->GetData());
    if (!mitkJob) {
        ui->Subpanel_Widget->setEnabled(false);
        ui->btnSave->setEnabled(false);
        return;
    }

    ui->Subpanel_Widget->setEnabled(true);
    ui->btnSave->setEnabled(true);

    m_JobNode=jobNode;
    m_MitkJob=mitkJob;
    m_Job=mitkJob->GetSimJob();

    // Setup the top of the FSI tool.
    ui->labelJobName->setText(QString::fromStdString(m_JobNode->GetName()));
    ui->labelJobStatus->setText(QString::fromStdString(m_MitkJob->GetStatus()));

    if (!m_Job) {
        return;
    }

    // Update the Domains panel.
    ui->comboBoxDomains->clear();
    for(auto& d: m_Job->m_Domains) {
        ui->comboBoxDomains->addItem(QString::fromStdString(d.first));
    }

    // Update the Physics panel.
    UpdatePhysicsPanel();

    // Update the Simulation Parameters panel.
    UpdateSimulationParametersPanel();

    // Run parameters
    //
    ui->sliderNumProcs->setValue(m_MitkJob->GetProcessNumber());

    UpdateJobStatus();
}

//-------------------------
// SetupRunSimulationPanel
//-------------------------
//
void sv4guisvFSIView::SetupRunSimulationPanel()
{
    SetupInternalSolverPaths();

    // Set the number of MPI processors.
    ui->sliderNumProcs->setDecimals(0);
    ui->sliderNumProcs->setMinimum(1);
    int coreNum = QThread::idealThreadCount();
    ui->sliderNumProcs->setMaximum(coreNum);

    connect(ui->btnCreateInputFile, SIGNAL(clicked()), this, SLOT(CreateInputFile()));
    connect(ui->btnRunSim, SIGNAL(clicked()), this, SLOT(RunSimulation()));
    connect(ui->btnStopSim, SIGNAL(clicked()), this, SLOT(StopSimulation()));
}

//-------------------
// SetupPhysicsPanel
//-------------------
//
void sv4guisvFSIView::SetupPhysicsPanel()
{
    connect(ui->btnAddEq, SIGNAL(clicked()), this, SLOT(AddEquation()));
    connect(ui->btnClearEq, SIGNAL(clicked()), this, SLOT(ClearEquation()));
    connect(ui->listEqs, SIGNAL(itemSelectionChanged()), this, SLOT(SelectEquation()));

    // Physics properties.
    //
    // propL[] stores the property label, propB[] its value (box?).
    //
    // The label and values are determined in sv4guisvFSIeqClass() and
    // set in sv4guisvFSIView::SelectEquation().
    //
    propL.append(ui->prop_1_txt); 
    propB.append(ui->prop_1_box);

    propL.append(ui->prop_2_txt);  
    propB.append(ui->prop_2_box);

    propL.append(ui->prop_3_txt); 
    propB.append(ui->prop_3_box);

    propL.append(ui->prop_4_txt); 
    propB.append(ui->prop_4_box);

    propL.append(ui->prop_5_txt); 
    propB.append(ui->prop_5_box);

    for (int i=0; i < propL.length(); i++) {
        propB.at(i)->setValidator(m_RealVal);
        connect(propB.at(i), SIGNAL(textEdited(const QString &)), this, SLOT(SaveProps()));
    }
    ui->phys_prop_group_box->setVisible(false);

    ui->widgetConstitutive->hide();
    connect(ui->comboBoxConstitutive, SIGNAL(currentTextChanged(const QString &)), this, SLOT(SaveProps()));

    // Output
    connect(ui->btnAddOutput, SIGNAL(clicked()), this, SLOT(AddOutput()));
    connect(ui->btnClearOutput, SIGNAL(clicked()), this, SLOT(ClearOutput()));

    // Advanced
    ui->lineEditTol->setValidator(m_RealVal);
    connect(ui->coupled, SIGNAL(clicked()), this, SLOT(SaveAdvanced()));
    connect(ui->lineEditTol, SIGNAL(textEdited(const QString &)), this, SLOT(SaveAdvanced()));
    connect(ui->dBr, SIGNAL(editingFinished()), this, SLOT(SaveAdvanced()));
    connect(ui->minItr, SIGNAL(editingFinished()), this, SLOT(SaveAdvanced()));
    connect(ui->maxItr, SIGNAL(editingFinished()), this, SLOT(SaveAdvanced()));

    // Linear solver
    ShowNSWidget();
    connect(ui->comboBoxLSType, SIGNAL(currentTextChanged(const QString &)), this, SLOT(SaveLinearSolver()));
    connect(ui->comboBoxLSType, SIGNAL(currentTextChanged(const QString &)), this, SLOT(ShowNSWidget()));

    ui->lineEditLSMaxItr->setValidator(m_IntVal);
    connect(ui->lineEditLSMaxItr, SIGNAL(textEdited(const QString &)), this, SLOT(SaveLinearSolver()));
    ui->lineEditLSTol->setValidator(m_RealVal);
    connect(ui->lineEditLSTol, SIGNAL(textEdited(const QString &)), this, SLOT(SaveLinearSolver()));

    ui->lineEditNSGMMaxItr->setValidator(m_IntVal);
    connect(ui->lineEditNSGMMaxItr, SIGNAL(textEdited(const QString &)), this, SLOT(SaveLinearSolver()));
    ui->lineEditNSGMTol->setValidator(m_RealVal);
    connect(ui->lineEditNSGMTol, SIGNAL(textEdited(const QString &)), this, SLOT(SaveLinearSolver()));

    ui->lineEditNSCGMaxItr->setValidator(m_IntVal);
    connect(ui->lineEditNSCGMaxItr, SIGNAL(textEdited(const QString &)), this, SLOT(SaveLinearSolver()));
    ui->lineEditNSCGTol->setValidator(m_RealVal);
    connect(ui->lineEditNSCGTol, SIGNAL(textEdited(const QString &)), this, SLOT(SaveLinearSolver()));

    ui->lineEditKrylovDim->setValidator(m_IntVal);
    connect(ui->lineEditKrylovDim, SIGNAL(textEdited(const QString &)), this, SLOT(SaveLinearSolver()));
    ui->lineEditLSAbsTol->setValidator(m_RealVal);
    connect(ui->lineEditLSAbsTol, SIGNAL(textEdited(const QString &)), this, SLOT(SaveLinearSolver()));

    connect(ui->comboBoxPreconditioner, SIGNAL(currentTextChanged(const QString &)), this, SLOT(SaveLinearSolver()));
    ui->comboBoxPreconditioner->clear();
    ui->comboBoxPreconditioner->addItem(QString::fromStdString("Default"));
    for (auto const& precond : sv4guisvFSILinearSolverPreconditioner::list) {
        ui->comboBoxPreconditioner->addItem(QString::fromStdString(precond));
    }

    // Boundary conditions 
    ui->bcList->horizontalHeader()->setVisible(true);
    connect(ui->bcList, SIGNAL(cellDoubleClicked(int,int)), ui->btnModifyBC, SLOT(click()));
    connect(ui->btnAddBC, SIGNAL(clicked()), this, SLOT(AddBC()));
    connect(ui->btnModifyBC, SIGNAL(clicked()), this, SLOT(ModifyBC()));
    connect(ui->btnRemoveBC, SIGNAL(clicked()), this, SLOT(RemoveBC()));

    // Remesher
    ui->widgetRemesher->hide();
    connect(ui->comboBoxRemesher, SIGNAL(currentTextChanged(const QString &)), this, SLOT(ShowRemesher()));
    connect(ui->comboBoxDomain2, SIGNAL(currentTextChanged(const QString &)), this, SLOT(ShowEdgeSize()));

    connect(ui->comboBoxRemesher, SIGNAL(currentTextChanged(const QString &)), this, SLOT(SaveRemesher()));
    connect(ui->dsbMinAngle, SIGNAL(editingFinished()), this, SLOT(SaveRemesher()));
    connect(ui->dsbRadiusRatio, SIGNAL(editingFinished()), this, SLOT(SaveRemesher()));
    connect(ui->sbRemeshFrequency, SIGNAL(editingFinished()), this, SLOT(SaveRemesher()));
    connect(ui->sbCopyFrequency, SIGNAL(editingFinished()), this, SLOT(SaveRemesher()));
    connect(ui->dsbEdgeSize, SIGNAL(editingFinished()), this, SLOT(SaveRemesher()));
}

//-------------------
// SetupDomainsPanel
//-------------------
//
void sv4guisvFSIView::SetupDomainsPanel()
{
    //connect(ui->comboBoxNsd, SIGNAL(currentTextChanged(const QString &)), this, SLOT(SetNsd(const QString &)));
    SetNsd(QString(3));
//    connect(ui->btnAddMesh, SIGNAL(clicked()), this, SLOT(AddMesh()));
    connect(ui->btnAddMeshComplete, SIGNAL(clicked()), this, SLOT(AddMeshComplete()));
    connect(ui->comboBoxDomains, SIGNAL(currentTextChanged(const QString &)), this, SLOT(SelectDomain(const QString &)));
    connect(ui->btnDeleteDomain, SIGNAL(clicked()), this, SLOT(DeleteDomain()));

    ui->buttonGroup->setId(ui->radioButtonFluid,0);
    ui->buttonGroup->setId(ui->radioButtonSolid,1);
    connect(ui->buttonGroup, SIGNAL(buttonClicked(int )), this, SLOT(ChangeDomainType(int )));
}

//--------------------------------
// SetupSimulationParametersPanel
//--------------------------------
// Setup the Simulation Parameters panel GUI.
//
void sv4guisvFSIView::SetupSimulationParametersPanel()
{
    // Time control.
    connect(ui->TimeStartFromPrev_checkBox, SIGNAL(clicked()), this, SLOT(SaveSimulationParameters()));
    connect(ui->TimeNumSteps_spinBox, SIGNAL(editingFinished()), this, SLOT(SaveSimulationParameters()));
    ui->TimeStepSize_lineEdit->setValidator(m_RealVal);
    connect(ui->TimeStepSize_lineEdit, SIGNAL(textEdited(const QString &)), this, SLOT(SaveSimulationParameters()));

    // Results output.
    connect(ui->RestartName_lineEdit, SIGNAL(editingFinished()), this, SLOT(SaveSimulationParameters()));
    connect(ui->RestartSaveIncr_spinBox, SIGNAL(editingFinished()), this, SLOT(SaveSimulationParameters()));
    connect(ui->RestartSaveStart_spinBox, SIGNAL(editingFinished()), this, SLOT(SaveSimulationParameters()));

    // VTK output.
    connect(ui->VtkSave_checkBox, SIGNAL(clicked()), this, SLOT(SaveSimulationParameters()));
    connect(ui->VtkName_lineEdit, SIGNAL(editingFinished()), this, SLOT(SaveSimulationParameters()));
    connect(ui->VtkSaveIncr_spinBox, SIGNAL(editingFinished()), this, SLOT(SaveSimulationParameters()));

    connect(ui->save_average, SIGNAL(clicked()), this, SLOT(SaveSimulationParameters()));

    // Advanced.
    connect(ui->checkBoxRemeshing, SIGNAL(clicked()), this, SLOT(SaveSimulationParameters()));
    connect(ui->rhoInf, SIGNAL(editingFinished()), this, SLOT(SaveSimulationParameters()));

    connect(ui->warn, SIGNAL(clicked()), this, SLOT(SaveSimulationParameters()));
    connect(ui->verb, SIGNAL(clicked()), this, SLOT(SaveSimulationParameters()));
    connect(ui->debug, SIGNAL(clicked()), this, SLOT(SaveSimulationParameters()));
}

//--------------------------
// SaveSimulationParameters
//--------------------------
// Save GUI Simulation Parameters to the m_Job object. 
//
void sv4guisvFSIView::SaveSimulationParameters()
{
    if (!m_Job) {
        return;
    }

    // Time control.
    m_Job->continuePrevious = ui->TimeStartFromPrev_checkBox->isChecked();
    m_Job->timeSteps = ui->TimeNumSteps_spinBox->value();
    m_Job->stepSize = ui->TimeStepSize_lineEdit->text().trimmed().toStdString();

    // Results output.
    m_Job->restartFileName = ui->RestartName_lineEdit->text().toStdString();
    m_Job->restartInc = ui->RestartSaveIncr_spinBox->value();
    m_Job->startSavingStep = ui->RestartSaveStart_spinBox->value();

    // vtk output.
    m_Job->vtkSaveResults = ui->VtkSave_checkBox->isChecked(); 
    m_Job->vtkFileName = ui->VtkName_lineEdit->text().toStdString();
    m_Job->vtkInc = ui->VtkSaveIncr_spinBox->value();                            

    // Advanded 
    m_Job->saveAvgResult = ui->save_average->isChecked();
    m_Job->rhoInf = ui->rhoInf->value();
    m_Job->verbose = ui->verb->isChecked();
    m_Job->warn = ui->warn->isChecked();
    m_Job->debug = ui->debug->isChecked();
    m_Job->remeshing = ui->checkBoxRemeshing->isChecked();

    DataChanged();
}

//--------------------
// UpdatePhysicsPanel
//--------------------
// Update the widgets in the Physics panel from the m_Job object.
//
void sv4guisvFSIView::UpdatePhysicsPanel()
{
    ui->listEqs->clear();
    for(int i=0;i<m_Job->m_Eqs.size();++i) {
        ui->listEqs->insertItem(i,new QListWidgetItem(m_Job->m_Eqs[i].fullName));
    }

    if(ui->listEqs->count()>0) {
        ui->listEqs->setCurrentRow(0);
        ui->eqAdvOptions->setEnabled(true);
    }

    bool includingFluid = false;
    for(sv4guisvFSIeqClass& eq : m_Job->m_Eqs) {
        if (eq.physName=="FSI" || eq.physName=="fluid") {
            includingFluid = true;
            break;
        }
    }

    if (includingFluid){
        ui->listAvailableEqs->item(0)->setFlags(Qt::NoItemFlags);
        ui->listAvailableEqs->item(2)->setFlags(Qt::NoItemFlags);
    } else {
        ui->listAvailableEqs->item(0)->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
        ui->listAvailableEqs->item(2)->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
    }
}

//---------------------------------
// UpdateSimulationParametersPanel
//---------------------------------
// Update GUI Simulation Parameters from the m_Job object. 
//
void sv4guisvFSIView::UpdateSimulationParametersPanel()
{
    // Time control. 
    ui->TimeStartFromPrev_checkBox->setChecked(m_Job->continuePrevious);          // Start from previous simulation
    ui->TimeNumSteps_spinBox->setValue(m_Job->timeSteps);                         // Number of time steps
    ui->TimeStepSize_lineEdit->setText(QString::fromStdString(m_Job->stepSize));  // Time step size

    // Simulation results output.
    ui->RestartName_lineEdit->setText(QString::fromStdString(m_Job->restartFileName));
    ui->RestartSaveIncr_spinBox->setValue(m_Job->restartInc);                     // Increment in saving restart
    ui->RestartSaveStart_spinBox->setValue(m_Job->startSavingStep);               // Save start time step

    // Save as VTK.
    ui->VtkSave_checkBox->setChecked(m_Job->vtkSaveResults);                      // Save results using VTK format
    ui->VtkName_lineEdit->setText(QString::fromStdString(m_Job->vtkFileName));    // VTK file prefix name 
    ui->VtkSaveIncr_spinBox->setValue(m_Job->vtkInc);                             // VTK save start time step

    ui->save_average->setChecked(m_Job->saveAvgResult);                           // Produce a time-averaged results

    // Advanced options.
    ui->rhoInf->setValue(m_Job->rhoInf);                   // Rho inf
    ui->checkBoxRemeshing->setChecked(m_Job->remeshing);   // Remeshing
    ui->verb->setChecked(m_Job->verbose);
    ui->warn->setChecked(m_Job->warn);
    ui->debug->setChecked(m_Job->debug);
}

//-------------
// NodeChanged
//-------------
//
void sv4guisvFSIView::NodeChanged(const mitk::DataNode* node)
{
    if(m_JobNode.IsNotNull() && m_JobNode==node) {
        ui->labelJobName->setText(QString::fromStdString(m_JobNode->GetName()));
        UpdateJobStatus();

//        bool updateRunDir=false;
//        m_JobNode->GetBoolProperty("update rundir",updateRunDir);
//        if(updateRunDir)
//        {
//            UpdateGUIRunDir();
//            m_JobNode->SetBoolProperty("update rundir",false);
//        }

    }
}

void sv4guisvFSIView::NodeAdded(const mitk::DataNode* node)
{

}

void sv4guisvFSIView::NodeRemoved(const mitk::DataNode* node)
{

}

//    virtual void Activated() {}

//    virtual void Deactivated() {}

void sv4guisvFSIView::Visible()
{
    OnSelectionChanged(GetDataManagerSelection());
}

void sv4guisvFSIView::Hidden()
{

}

void sv4guisvFSIView::OnPreferencesChanged(const berry::IBerryPreferences* prefs)
{
    if(prefs==NULL)
        return;

    m_ExternalSolverPath=prefs->Get("svFSI solver path","");
}

void sv4guisvFSIView::DataChanged()
{
    if(m_MitkJob)
        m_MitkJob->SetDataModified();
}

//------------
// GetJobPath
//------------
//
QString sv4guisvFSIView::GetJobPath()
{
    QString jobPath="";

    if(m_JobNode.IsNull())
        return jobPath;

    std::string path="";
    m_JobNode->GetStringProperty("path",path);
    jobPath=QString::fromStdString(path+"/"+m_JobNode->GetName());
    return jobPath;
}

//--------------------------
// SetupInternalSolverPaths
//--------------------------
//
void sv4guisvFSIView::SetupInternalSolverPaths()
{
    m_InternalMPIExecPath="mpiexec";

    //get path for the internal solvers
    QString solverPath="/usr/local/sv/svFSI";
    QStringList dirList=QDir(solverPath).entryList(QDir::Dirs|QDir::NoDotAndDotDot|QDir::NoSymLinks,QDir::Name);
    if(dirList.size()!=0)
        solverPath+="/"+dirList.back();

    QString solverPathBin=solverPath+"/bin";

    QString applicationPath=QCoreApplication::applicationDirPath();
    QString sv4guisvFSISolverName="/svFSI";

    QString filePath="";

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)
    //flowsolver with mpi, prefer to the script one which sets some lib paths for the mpi libs from svsolver
    //Those libs are needed in Ubuntu 16, intead of using the system ones
    if(QFile(filePath=solverPathBin+"/.."+sv4guisvFSISolverName).exists())
        m_InternalSolverPath=filePath;
    else if(QFile(filePath=solverPathBin+sv4guisvFSISolverName).exists())
        m_InternalSolverPath=filePath;
    else if(QFile(filePath=applicationPath+"/.."+sv4guisvFSISolverName).exists())
        m_InternalSolverPath=filePath;
    else if(QFile(filePath=applicationPath+sv4guisvFSISolverName).exists())
        m_InternalSolverPath=filePath;

#endif

#if defined(Q_OS_MAC)
    //mpiexec
    QString mpiexecName="/mpiexec";
    if(QFile(filePath=solverPathBin+mpiexecName).exists())
        m_InternalMPIExecPath=filePath;
    else if(QFile(filePath=applicationPath+"/.."+mpiexecName).exists())
        m_InternalMPIExecPath=filePath;
    else if(QFile(filePath=applicationPath+mpiexecName).exists())
        m_InternalMPIExecPath=filePath;
#endif

#if defined(Q_OS_WIN)
    m_InternalSolverPath=GetRegistryValue("SimVascular\\svSolver","svFSI_MSMPI_EXE");
    QString msmpiDir=GetRegistryValue("Microsoft\\MPI","InstallRoot");
    if(msmpiDir!="")
    {
        if(msmpiDir.endsWith("\\"))
            m_InternalMPIExecPath=msmpiDir+"Bin\\mpiexec";
        else
            m_InternalMPIExecPath=msmpiDir+"\\Bin\\mpiexec";
    }
#endif
}

void sv4guisvFSIView::SetNsd(const QString &text)
{
    int nsd=text.toInt();
    if(m_Job && m_Job->nsd!=nsd) {
        m_Job->nsd=nsd;
        DataChanged();
    }
}

//-----------------
// AddMeshComplete
//-----------------
//
void sv4guisvFSIView::AddMeshComplete()
{
    if(!m_Job) {
        return;
    }

    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer prefs;
    if (prefService) {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    } else {
        prefs = berry::IPreferences::Pointer(0);
    }

    // QString lastFileOpenPath=prefs->Get("LastFileOpenPath", "");
    // if(lastFileOpenPath=="")
    //     lastFileOpenPath=QDir::homePath();

    auto sv4guisvFSI_dir = sv4guisvFSIUtil.getsv4guisvFSIDir();

    sv4guisvFSI_dir.cdUp();
    sv4guisvFSI_dir.cd("Meshes");

    QString dirPath = QFileDialog::getExistingDirectory(m_Parent
                                                        , tr("Choose Mesh-Complete Directory")
                                                        , sv4guisvFSI_dir.absolutePath());

    dirPath = dirPath.trimmed();
    if(dirPath.isEmpty()) {
        return;
    }

    if(prefs.IsNotNull()) {
        prefs->Put("LastFileOpenPath", dirPath);
        prefs->Flush();
    }

    QDir meshDir(dirPath);
    QString domainName=meshDir.dirName();
    auto search=m_Job->m_Domains.find(domainName.toStdString());
    if(search!=m_Job->m_Domains.end()) {
        QMessageBox::warning(m_Parent,"Already exists", "A domain with the same name was already added.");
        return;
    }

    sv4guisvFSIDomain domain;
    QString faceFolderName=QString::fromStdString(domain.faceFolderName); //use the default name

    QFileInfoList meshList=meshDir.entryInfoList(QStringList("*.vtu"));
    if(meshList.size()==0) {
        QMessageBox::warning(m_Parent,"No Mesh Found", "A mesh (vtu) file is not found in the folder");
        return;
    }

    QFileInfoList vtpList=meshDir.entryInfoList(QStringList("*.vtp"));
    if(vtpList.size()==0) {
        QMessageBox::warning(m_Parent,"No Mesh Found", "A mesh (vtp) file is not found in the folder");
        return;
    }

    QDir faceDir(dirPath+"/"+faceFolderName);
    QFileInfoList faceList=faceDir.entryInfoList(QStringList("*.vtp"));
    if(faceList.size()==0) {
        QMessageBox::warning(m_Parent,"No Faces Found", "Face (vtp) files are not found in the mesh-surfaces folder");
        return;
    }

    domain.name=domainName.toStdString();
    domain.folderName=domainName.toStdString();
    domain.fileName=meshList[0].fileName().toStdString();
    domain.surfaceName=vtpList[0].fileName().toStdString();

    QString jobPath=GetJobPath();
    QDir newDir;
    newDir.mkpath(jobPath+"/"+QString::fromStdString(domain.folderName)+"/"+faceFolderName);

    QFile::copy(meshList[0].absoluteFilePath(),jobPath+"/"+QString::fromStdString(domain.folderName)+"/"+QString::fromStdString(domain.fileName));

    QFile::copy(vtpList[0].absoluteFilePath(),jobPath+"/"+QString::fromStdString(domain.folderName)+"/"+QString::fromStdString(domain.surfaceName));

    for(int i=0;i<faceList.size();++i)
    {
        QString faceFileName=faceList[i].fileName();
        QString faceName=faceList[i].baseName();
        domain.faceNames.push_back(faceName.toStdString());

        QFile::copy(faceList[i].absoluteFilePath(),jobPath+"/"+QString::fromStdString(domain.folderName)+"/"+faceFolderName+"/"+faceFileName);
    }

    m_Job->m_Domains[domainName.toStdString()]=domain;

    ui->comboBoxDomains->addItem(domainName);

    //update remesher domain list
    ui->comboBoxDomain2->clear();
    for(auto& d: m_Job->m_Domains)
    {
        ui->comboBoxDomain2->addItem(QString::fromStdString(d.first));
    }

    std::string outputString = std::string("Successfully loaded mesh complete ").append(
      domain.folderName);

    QMessageBox::information(m_Parent,"Mesh-complete Success", outputString.c_str());



    DataChanged();
    loadMesh();
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//----------
// loadMesh
//----------
//
void sv4guisvFSIView::loadMesh()
{
  auto children = GetDataStorage()->GetDerivations(m_JobNode);
  if (!children->empty()){
    GetDataStorage()->Remove(children);
  }

  QString jobPath = GetJobPath();
  auto domains = m_Job->m_Domains;

  for (auto itr = domains.begin(); itr != domains.end(); ++itr){
    auto dom = itr->second;
    auto folderName = dom.folderName;
    auto surfaceName = dom.surfaceName;
    auto path = (jobPath+"/"+QString::fromStdString(folderName)+ "/" + QString::fromStdString(surfaceName)).toStdString();

    std::cout << "Reading mesh file " << path << "\n";

    sv4guiMesh* mesh = new sv4guiMesh();
    mesh->ReadSurfaceFile(path);

    sv4guiMitkMesh::Pointer mitkMesh = sv4guiMitkMesh::New();
    mitkMesh->SetMesh(mesh);

    mitk::DataNode::Pointer node = mitk::DataNode::New();
    node->SetData(mitkMesh);
    node->SetName(surfaceName);

    GetDataStorage()->Add(node,m_JobNode);
  }
}

//--------------
// SelectDomain
//--------------
//
void sv4guisvFSIView::SelectDomain(const QString &name)
{
    if(!m_Job)
        return;

    ui->faceList->clear();

    if(name.isEmpty())
        return;

    auto search=m_Job->m_Domains.find(name.toStdString());
    if(search==m_Job->m_Domains.end())
    {
        QMessageBox::warning(m_Parent,"Domain Not Found", "The domain is not found in the job!");
        return;
    }

    sv4guisvFSIDomain& domain=m_Job->m_Domains[name.toStdString()];

    if(domain.type=="fluid")
        ui->radioButtonFluid->setChecked(true);
    else if(domain.type=="solid")
        ui->radioButtonSolid->setChecked(true);

    QStringList faceNames;
    for(int i=0;i<domain.faceNames.size();++i)
    {
        faceNames<<QString::fromStdString(domain.faceNames[i]);
    }
    faceNames.sort();

    ui->faceList->addItems(faceNames);
}

void sv4guisvFSIView::DeleteDomain()
{
    if(!m_Job)
        return;

    if (QMessageBox::question(m_Parent, "Delete Domain", "Are you sure to delete the domain?",
                              QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
    {
      return;
    }

    QString domainName=ui->comboBoxDomains->currentText();
    int indx=ui->comboBoxDomains->currentIndex();

    m_Job->m_Domains.erase(domainName.toStdString());
    ui->comboBoxDomains->removeItem(indx);

    //update remesher domain list
    ui->comboBoxDomain2->clear();
    for(auto& d: m_Job->m_Domains)
    {
        ui->comboBoxDomain2->addItem(QString::fromStdString(d.first));
    }

    DataChanged();
}

void sv4guisvFSIView::ChangeDomainType(int type)
{
    if(!m_Job)
        return;

    std::string domainName=ui->comboBoxDomains->currentText().toStdString();
    auto search=m_Job->m_Domains.find(domainName);
    if(search==m_Job->m_Domains.end())
        return;

    if(type==0)
        m_Job->m_Domains[domainName].type="fluid";
    else if(type==1)
        m_Job->m_Domains[domainName].type="solid";

    DataChanged();
}

void sv4guisvFSIView::AddEquation()
{
    if(!m_Job)
        return;

    QList<QListWidgetItem*> items = ui->listAvailableEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    QString eqName = items.first()->text();

    QListWidgetItem* item=NULL;
//    int row=0;
    bool includingFluid=false;

    if ( eqName == "Incomp. fluid")
    {
        sv4guisvFSIeqClass eq(eqName);
        m_Job->m_Eqs.insert(m_Job->m_Eqs.begin(),eq);
        item= new QListWidgetItem(eqName);
        ui->listEqs->insertItem(0,item);
//        row=0;
        includingFluid=true;
        ui->comboBoxRemesher->setEnabled(false);
    }
    else if ( eqName == "FSI" )
    {
        sv4guisvFSIeqClass eq(eqName);
        m_Job->m_Eqs.insert(m_Job->m_Eqs.begin(),eq);
        item= new QListWidgetItem(eqName);
        ui->listEqs->insertItem(0,item);

        sv4guisvFSIeqClass eq2("Mesh motion");
        m_Job->m_Eqs.insert(m_Job->m_Eqs.begin()+1,eq2);
        item= new QListWidgetItem("Mesh motion");
        ui->listEqs->insertItem(1,item);

        includingFluid=true;
        ui->comboBoxRemesher->setEnabled(true);

//        row
    }
    else
    {
        sv4guisvFSIeqClass eq(eqName);
        m_Job->m_Eqs.push_back(eq);
        item= new QListWidgetItem(eqName);
        ui->listEqs->addItem(item);
        ui->comboBoxRemesher->setEnabled(false);


    }

    if(item)
        item->setSelected(true);

    if(includingFluid)
    {
        ui->listAvailableEqs->item(0)->setFlags(Qt::NoItemFlags);
        ui->listAvailableEqs->item(2)->setFlags(Qt::NoItemFlags);
    }

    ui->eqAdvOptions->setEnabled(true);

    DataChanged();
}

void sv4guisvFSIView::ClearEquation()
{
    if(!m_Job)
        return;

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    if (QMessageBox::No == QMessageBox::question(0, "Removing equation", "All modifications will be lost.\nAre you sure?", QMessageBox::Yes|QMessageBox::No))
        return;

    int row=ui->listEqs->row(items.first());

    sv4guisvFSIeqClass eq=m_Job->m_Eqs[row];

    if ( (eq.getPhysName() == "FSI") || (eq.getPhysName() == "fluid") ) {
        ui->listAvailableEqs->item(0)->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
        ui->listAvailableEqs->item(2)->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
    }

    if ( eq.getPhysName() == "FSI" ) {

        m_Job->m_Eqs[row].faceBCs.clear();
        m_Job->m_Eqs[row+1].faceBCs.clear();
        m_Job->m_Eqs.erase(m_Job->m_Eqs.begin()+row);
        ui->listEqs->takeItem(row);
        m_Job->m_Eqs.erase(m_Job->m_Eqs.begin()+row);//remove mesh motion
        ui->listEqs->takeItem(row);
    }
    else
    {
        m_Job->m_Eqs.erase(m_Job->m_Eqs.begin()+row);
        ui->listEqs->takeItem(row);
    }

    if ( ui->listEqs->count() == 0 )
        ui->eqAdvOptions->setEnabled(false);

    DataChanged();
}

//----------------
// SelectEquation
//----------------
// Set the visibility of different GUI widgets based on equation type (e.g. fluid).
//
void sv4guisvFSIView::SelectEquation()
{
    if (!m_Job) {
        return;
    }

    m_EnableSave = false;

    for (int i = 0; i < propL.length(); i++) {
        propL.at(i)->setVisible(false);
        propB.at(i)->setVisible(false);
    }

    ui->output->clear();
    ui->outputList->clear();

    ui->bc_box->setEnabled(false);
    ui->bcList->clearContents();

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if (items.isEmpty()) {
        return;
    }

    int row = ui->listEqs->row(items.first());
    if (row < 0) {
        return;
    }

    // Get the equation object selected from the Physics panel.
    sv4guisvFSIeqClass& eq = m_Job->m_Eqs[row];

    // Set property names and values defined for the equation.
    ui->phys_prop_group_box->setVisible(true);
    for (int i = 0; i < eq.getPropCount(); i++) {
        propL.at(i)->setVisible(true);
        propB.at(i)->setVisible(true);
        propL.at(i)->setText(eq.getPropName(i));
        propB.at(i)->setText(QString::number(eq.getPropValue(i)));
    }

    if(eq.physName=="FSI" || eq.physName=="struct") {
        ui->widgetConstitutive->show();
        ui->comboBoxConstitutive->setCurrentText(eq.constitutiveModel);
    } else {
        ui->widgetConstitutive->hide();
    }

    //output
    ui->output->addItems(eq.getOutputCandidates());
    ui->outputList->addItems(eq.getOutputNames());

    //Advanced
    ui->coupled->setChecked(eq.getCoupled());
    ui->lineEditTol->setText(eq.getTol());
    ui->dBr->setValue(eq.getdBr());
    ui->minItr->setValue(eq.getMinItr());
    ui->maxItr->setValue(eq.getMaxItr());

    if(eq.physName=="fluid" || eq.physName=="FSI") {
        ui->labelBackflow->show();
        ui->dsbBackflow->show();
        ui->dsbBackflow->setValue(eq.backflowStab);
    } else {
        ui->labelBackflow->hide();
        ui->dsbBackflow->hide();
    }

    //linear solver
    ui->comboBoxLSType->setCurrentText(eq.lsType);
    ui->lineEditLSMaxItr->setText(QString::number(eq.lsMaxItr));
    ui->lineEditLSTol->setText(eq.lsTol);
    ui->lineEditNSGMMaxItr->setText(QString::number(eq.lsNSGMMaxItr));
    ui->lineEditNSGMTol->setText(eq.lsNSGMTol);
    ui->lineEditNSCGMaxItr->setText(QString::number(eq.lsNSCGMaxItr));
    ui->lineEditNSCGTol->setText(eq.lsNSCGTol);
    ui->lineEditKrylovDim->setText(QString::number(eq.lsKrylovDim));
    ui->lineEditLSAbsTol->setText(eq.lsAbsoluteTol);
    ui->comboBoxPreconditioner->setCurrentText(eq.lsPreconditioner);

    //remesher
    ui->comboBoxRemesher->setCurrentText(eq.remesher);
    ui->dsbMinAngle->setValue(eq.rmMinAngle);
    ui->dsbRadiusRatio->setValue(eq.rmMaxRadiusRatio);
    ui->sbRemeshFrequency->setValue(eq.rmFrequency);
    ui->sbCopyFrequency->setValue(eq.rmCopyFrequency);

    ui->comboBoxDomain2->clear();
    for(auto& d: m_Job->m_Domains)
    {
        ui->comboBoxDomain2->addItem(QString::fromStdString(d.first));
    }

    //BCs
    ui->bc_box->setEnabled(true);

    ui->bcList->setRowCount(eq.faceBCs.size());
    int i=0;
    for (auto& f : eq.faceBCs) {
        sv4guisvFSIbcClass& bc=f.second;
        QTableWidgetItem* name = new QTableWidgetItem(bc.faceName);
        QTableWidgetItem* bcT = new QTableWidgetItem(bc.bcGrp);
        ui->bcList->setItem(i,0,name);
        ui->bcList->setItem(i,1,bcT);
        i++;
    }

    // Special case of FSI and Incomp fluid
//    if ( (eq.getPhysName() == "FSI") || (eq.getPhysName() == "fluid") ) {
//        ui->listAvailableEqs->item(0)->setFlags(Qt::NoItemFlags);
//        ui->listAvailableEqs->item(2)->setFlags(Qt::NoItemFlags);
//    }

    if ( eq.getPhysName() == "mesh" ) {
        ui->btnClearEq->setEnabled(false);
    } else {
        ui->btnClearEq->setEnabled(true);
    }

    m_EnableSave=true;
}

//-----------
// SaveProps
//-----------
//
void sv4guisvFSIView::SaveProps()
{
    if(!m_Job)
        return;

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    int row=ui->listEqs->row(items.first());
    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[row];

    for ( int i=0 ; i < eq.getPropCount() ; i++ ) {
        eq.setPropValue(propB.at(i)->text().toDouble(),i);
    }

    eq.constitutiveModel=ui->comboBoxConstitutive->currentText();

    DataChanged();
}

void sv4guisvFSIView::AddOutput()
{
    QList<QListWidgetItem *> items = ui->output->selectedItems();
    if ( items.isEmpty() )
        return;

    int i = ui->output->row(items.first());
    QListWidgetItem* item = ui->output->takeItem(i);
    ui->outputList->addItem(item);

    SaveOutputs();
}

void sv4guisvFSIView::ClearOutput()
{
    QList<QListWidgetItem *> items = ui->outputList->selectedItems();
    if ( items.isEmpty() )
        return;

    int i = ui->outputList->row(items.first());
    QListWidgetItem* item = ui->outputList->takeItem(i);
    ui->output->addItem(item);

    SaveOutputs();
}

void sv4guisvFSIView::SaveOutputs()
{
    if(!m_Job)
        return;

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    int row=ui->listEqs->row(items.first());
    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[row];

    QStringList outputs;
    for ( int i=0 ; i < ui->outputList->count() ; i++ )
    {
        outputs.append(ui->outputList->item(i)->text());
    }
    eq.setOutputs(outputs);

    DataChanged();
}

void sv4guisvFSIView::SaveAdvanced()
{
    if(!m_Job)
        return;

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    int row=ui->listEqs->row(items.first());
    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[row];

    eq.setCoupled(ui->coupled->isChecked());
    eq.setMinItr(ui->minItr->value());
    eq.setMaxItr(ui->maxItr->value());
    eq.setdBr(ui->dBr->value());
    eq.setTol(ui->lineEditTol->text());
    eq.backflowStab=ui->dsbBackflow->value();

    DataChanged();
}

void sv4guisvFSIView::ResetEquation()
{
    if(!m_Job)
        return;

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    if (QMessageBox::No == QMessageBox::question(0, "Reseting equation", "All modifications will be lost.\nAre you sure?", QMessageBox::Yes|QMessageBox::No))
        return;

    int row=ui->listEqs->row(items.first());

    sv4guisvFSIeqClass newEq(items.first()->text());
    m_Job->m_Eqs[row]=newEq;

    SelectEquation();

    DataChanged();
}

void sv4guisvFSIView::SaveLinearSolver()
{
    if(!m_Job)
        return;

    if(!m_EnableSave)
        return;

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    int row=ui->listEqs->row(items.first());
    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[row];

    eq.lsType=ui->comboBoxLSType->currentText();
    eq.lsMaxItr=ui->lineEditLSMaxItr->text().toInt();
    eq.lsTol=ui->lineEditLSTol->text();
    eq.lsNSGMMaxItr=ui->lineEditNSGMMaxItr->text().toInt();
    eq.lsNSGMTol=ui->lineEditNSGMTol->text();
    eq.lsNSCGMaxItr=ui->lineEditNSCGMaxItr->text().toInt();
    eq.lsNSCGTol=ui->lineEditNSCGTol->text();
    eq.lsKrylovDim=ui->lineEditKrylovDim->text().toInt();
    eq.lsAbsoluteTol=ui->lineEditLSAbsTol->text();
    eq.lsPreconditioner=ui->comboBoxPreconditioner->currentText();

    DataChanged();
}

void sv4guisvFSIView::ShowNSWidget()
{
    if(ui->comboBoxLSType->currentText()=="NS")
        ui->widgetNS->show();
    else
        ui->widgetNS->hide();
}

void sv4guisvFSIView::AddBC()
{
    if(!m_Job)
        return;

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    int row=ui->listEqs->row(items.first());

    sv4guisvFSIBCWidget bcWidget;
    bcWidget.Setup(m_MitkJob,row,true,QStringList(), GetJobPath());
    if(bcWidget.exec()==QDialog::Rejected)
        return;

    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[row];

    ui->bcList->clearContents();
    ui->bcList->setRowCount(eq.faceBCs.size());
    int i=0;
    for (auto& f : eq.faceBCs) {
        sv4guisvFSIbcClass& bc=f.second;
        QTableWidgetItem* name = new QTableWidgetItem(bc.faceName);
        QTableWidgetItem* bcT = new QTableWidgetItem(bc.bcGrp);
        ui->bcList->setItem(i,0,name);
        ui->bcList->setItem(i,1,bcT);
        i++;
    }
}

void sv4guisvFSIView::ModifyBC()
{
    if(!m_Job)
        return;

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    int eqRow=ui->listEqs->row(items.first());
    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[eqRow];

    QModelIndexList indexesOfSelectedRows = ui->bcList->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
        return;

    QStringList faceList;
    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();
        faceList<<ui->bcList->item(row,0)->text();
    }

    sv4guisvFSIBCWidget bcWidget;
    bcWidget.Setup(m_MitkJob,eqRow,false,faceList, GetJobPath());
    if(bcWidget.exec()==QDialog::Rejected)
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();
        std::string name=ui->bcList->item(row,0)->text().toStdString();
        ui->bcList->item(row,1)->setText(eq.faceBCs[name].bcGrp);
    }
}

void sv4guisvFSIView::RemoveBC()
{
    if(!m_Job)
        return;

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    int eqRow=ui->listEqs->row(items.first());
    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[eqRow];

    QModelIndexList indexesOfSelectedRows = ui->bcList->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
        return;

    QList<int> rowList;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();
        std::string name=ui->bcList->item(row,0)->text().toStdString();
        eq.faceBCs.erase(name);
        rowList<<row;

    }

    for(int i=rowList.size()-1;i>-1;i--)
        ui->bcList->removeRow(rowList[i]);

    DataChanged();
}


void sv4guisvFSIView::ShowRemesher()
{
    if(ui->comboBoxRemesher->currentText()=="None")
        ui->widgetRemesher->hide();
    else
        ui->widgetRemesher->show();

    DataChanged();
}

void sv4guisvFSIView::ShowEdgeSize()
{
    if(!m_Job)
        return;

    std::string domainName=ui->comboBoxDomain2->currentText().toStdString();

    if(m_Job->m_Domains.find(domainName)!=m_Job->m_Domains.end())
        ui->dsbEdgeSize->setValue(m_Job->m_Domains[domainName].edgeSize);
}

void sv4guisvFSIView::SaveRemesher()
{
    if(!m_Job)
        return;

    if(!m_EnableSave)
        return;

    QList<QListWidgetItem*> items = ui->listEqs->selectedItems();
    if ( items.isEmpty() )
        return;

    int eqRow=ui->listEqs->row(items.first());
    sv4guisvFSIeqClass& eq=m_Job->m_Eqs[eqRow];

    eq.remesher=ui->comboBoxRemesher->currentText();
    eq.rmMinAngle=ui->dsbMinAngle->value();
    eq.rmMaxRadiusRatio=ui->dsbRadiusRatio->value();
    eq.rmFrequency=ui->sbRemeshFrequency->value();
    eq.rmCopyFrequency=ui->sbCopyFrequency->value();

    std::string domainName=ui->comboBoxDomain2->currentText().toStdString();

    if(m_Job->m_Domains.find(domainName)!=m_Job->m_Domains.end())
        m_Job->m_Domains[domainName].edgeSize=ui->dsbEdgeSize->value();

    DataChanged();
}


void sv4guisvFSIView::CreateInputFile()
{
    if(!m_MitkJob)
        return;

    QString jobPath=GetJobPath();
    if(jobPath=="" || !QDir(jobPath).exists())
    {
        QMessageBox::warning(m_Parent,"Unable to run","Please make sure mesh files have been added!");
        return;
    }

    std::string mfsFileName=m_JobNode->GetName()+".txt";
    std::string mfsFullFilePath=jobPath.toStdString()+"/"+mfsFileName;

    if(m_Job->WriteFile(mfsFullFilePath))
    {
        m_MitkJob->SetStatus("Input file created");
        m_JobNode->SetBoolProperty("dummy",true);//trigger NodeChanged to update job status
        mitk::StatusBar::GetInstance()->DisplayText("Input file (.msf) have been created.");
    }
}

void sv4guisvFSIView::RunSimulation()
{
    if (QMessageBox::question(m_Parent, "Run Job", "Are you sure to run the job? It may take a while to finish.",
                              QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
    {
      return;
    }

    if(!m_MitkJob)
        return;

    QString jobPath=GetJobPath();
    if(jobPath=="" || !QDir(jobPath).exists())
    {
        QMessageBox::warning(m_Parent,"Unable to run","Please make sure mesh files have been added!");
        return;
    }

    std::string mfsFileName=m_JobNode->GetName()+".txt";
    std::string mfsFullFilePath=jobPath.toStdString()+"/"+mfsFileName;

    int procNum=ui->sliderNumProcs->value();
    if(m_MitkJob->GetProcessNumber()!=procNum)
    {
        m_MitkJob->SetProcessNumber(procNum);
        DataChanged();
    }

//    m_Job->WriteFile(mfsFullFilePath);


    if(!QFile(QString::fromStdString(mfsFullFilePath)).exists())
    {
        QMessageBox::warning(m_Parent,"Unable to run","Please make sure input file exists!");
        return;
    }


    QString flowsolverPath=m_ExternalSolverPath;
    if(flowsolverPath=="")
        flowsolverPath=m_InternalSolverPath;

    if(flowsolverPath=="")
    {
        QMessageBox::warning(m_Parent,"Flowsolver Missing","Please make sure flowsolver exists!");
        return;
    }

    QString mpiExecPath=m_InternalMPIExecPath;

//        if(mpiExecPath=="")
//        {
//            QMessageBox::warning(m_Parent,"MPIExec Missing","Please make sure mpiexec exists!");
//            return;
//        }


    QString runPath=jobPath;
    int numProcs=ui->sliderNumProcs->value();
    runPath=jobPath+"/"+QString::number(numProcs)+"-procs";

    int totalSteps=100;//initial none zero value
    if(m_Job)
    {
//        job->SetRunProp("Number of Processes",QString::number(numProcs).toStdString());
//        QString tstr=QString::fromStdString(job->GetSolverProp("Number of Timesteps"));
        totalSteps=m_Job->timeSteps;
    }

    mitk::StatusBar::GetInstance()->DisplayText("Running simulation");

    QProcess *flowsolverProcess = new QProcess(m_Parent);
    flowsolverProcess->setWorkingDirectory(jobPath);

    if(numProcs>1)
    {
        QStringList arguments;
        arguments << "-n" << QString::number(numProcs)<< flowsolverPath<<QString::fromStdString(mfsFileName);
        flowsolverProcess->setProgram(mpiExecPath);
        flowsolverProcess->setArguments(arguments);
    }
    else
    {
        QStringList arguments;
        arguments <<QString::fromStdString(mfsFileName);
        flowsolverProcess->setProgram(flowsolverPath);
        flowsolverProcess->setArguments(arguments);
    }

    sv4guisvFSISolverProcessHandler* handler=new sv4guisvFSISolverProcessHandler(flowsolverProcess,m_JobNode,0,totalSteps,runPath,m_Parent);
    handler->Start();
}

void sv4guisvFSIView::StopSimulation()
{
    if(m_JobNode.IsNull())
        return;

    if (QMessageBox::question(NULL, "Stop Simulation", "Are you sure to stop simulation for this job?",
                              QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
    {
      return;
    }

      bool running=false;
      m_JobNode->GetBoolProperty("running",running);

      if(running)
      {
          sv4guisvFSISolverProcessHandler* handler=NULL;
          bool ok=m_JobNode->GetPropertyValue<sv4guisvFSISolverProcessHandler*>("process handler",handler);

          if(ok && handler)
          {
              handler->KillProcess();
          }

      }
      else
      {
          QMessageBox::information(NULL,"Info","The selected job is not running.");
          return;
      }

}

void sv4guisvFSIView::UpdateJobStatus()
{
    if(m_JobNode.IsNull())
        return;

    bool running=false;
    double runningProgress=0;
    m_JobNode->GetBoolProperty("running",running);
    m_JobNode->GetDoubleProperty("running progress",runningProgress);
    if(running)
    {
        ui->labelJobStatus->setText("Running: "+QString::number((int)(runningProgress*100))+"% completed");
        ui->widgetRun->setEnabled(false);
        ui->btnStopSim->setEnabled(true);
    }
    else
    {
        ui->labelJobStatus->setText(QString::fromStdString(m_MitkJob->GetStatus()));
        ui->widgetRun->setEnabled(true);
        ui->btnStopSim->setEnabled(false);
    }

}

void sv4guisvFSIView::CreateNewJob()
{
    bool ok;
    QString text = QInputDialog::getText(NULL, tr("Create svFSI Job"),
                                         tr("Job Name:"), QLineEdit::Normal,
                                         "", &ok);
    if(!ok)
        return;

    std::string jobName=text.trimmed().toStdString();
    if(jobName==""){
        QMessageBox::warning(NULL,"No name for job!","Please give a name for the job!");
        return;
    }

    mitk::DataNode::Pointer exitingNode=GetDataStorage()->GetNamedNode(jobName.c_str());
    if(exitingNode){
        QMessageBox::warning(NULL,"Job Already Created","Please use a different job name!");
        return;
    }

    auto dir = sv4guisvFSIUtil.getsv4guisvFSIDir().absolutePath();

    // QString dir = QFileDialog::getExistingDirectory(m_Parent
    //                                                 , tr("Choose directory to save the job")
    //                                                 , sv4guisvFSI_dir.absolutePath());

    dir=dir.trimmed();
    if(dir.isEmpty())
        return;

    sv4guiMitksvFSIJob::Pointer mitkJob = sv4guiMitksvFSIJob::New();
    sv4guisvFSIJob* job=new sv4guisvFSIJob();
    mitkJob->SetSimJob(job);
//    mitkJob->SetDataModified();

    mitk::DataNode::Pointer jobNode = mitk::DataNode::New();
    jobNode->SetData(mitkJob);
    jobNode->SetName(jobName);
    jobNode->SetStringProperty("path", dir.toStdString().c_str());

    mitk::DataNode::Pointer sv4guisvFSI_folder_node = GetDataStorage()->GetNamedNode(sv4guisvFSI_NODE_NAME);
    if (!sv4guisvFSI_folder_node){
      std::cout << "sv4guisvFSI folder node null\n";
    }else {
      GetDataStorage()->Add(jobNode, sv4guisvFSI_folder_node);
      jobNode->SetSelected(true);
      sv4guiMitksvFSIJob* mitkJob=dynamic_cast<sv4guiMitksvFSIJob*>(jobNode->GetData());

      m_JobNode = jobNode;
      m_MitkJob = mitkJob;
      DataChanged();
    }
}

void sv4guisvFSIView::SaveJob()
{
    if(m_JobNode.IsNull())
        return;

    std::string path="";
    m_JobNode->GetStringProperty("path",path);

    if(path=="")
        return;

    QDir dir(QString::fromStdString(path));
    QString	filePath=dir.absoluteFilePath(QString::fromStdString(m_JobNode->GetName())+".fsijob");
    mitk::IOUtil::Save(m_JobNode->GetData(),filePath.toStdString());
}

void sv4guisvFSIView::LoadJob()
{
    auto sv4guisvFSI_dir = sv4guisvFSIUtil.getsv4guisvFSIDir();
    QString dir = QFileDialog::getOpenFileName(NULL
                                                    , tr("Choose .fsijob file")
                                                    , sv4guisvFSI_dir.absolutePath()
                                                    , tr("Job file (*.fsijob)"));

    dir=dir.trimmed();
    if(dir.isEmpty())
        return;

    auto node = mitk::IOUtil::Load(dir.toStdString().c_str(),
      *GetDataStorage())->ElementAt(0);

    GetDataStorage()->Remove(node);

    mitk::DataNode::Pointer sv4guisvFSI_folder_node = GetDataStorage()->GetNamedNode(sv4guisvFSI_NODE_NAME);

    if (!sv4guisvFSI_folder_node){
      std::cout << "svFSI folder node doesnt exist\n";
      return;
    }
    GetDataStorage()->Add(node,sv4guisvFSI_folder_node);
}

void sv4guisvFSIView::LoadJob(std::string jobPath, std::string jobName)
{
    if (GetDataStorage()->Exists(GetDataStorage()->GetNamedNode(jobName))) return;

    auto node = mitk::IOUtil::Load(jobPath.c_str(),
      *GetDataStorage())->ElementAt(0);

    if (!node){
      std::cout << "svFSI job with name " << jobPath << " doesn't exist\n";
      return;
    }

    GetDataStorage()->Remove(node);

    mitk::DataNode::Pointer sv4guisvFSI_folder_node = GetDataStorage()->GetNamedNode(sv4guisvFSI_NODE_NAME);

    if (!sv4guisvFSI_folder_node){
      std::cout << "svFSI folder node doesnt exist\n";
      return;
    }
    GetDataStorage()->Add(node,sv4guisvFSI_folder_node);
}

#if defined(Q_OS_WIN)
QString sv4guisvFSIView::FindLatestKey(QString key, QStringList keys)
{
    keys.sort();

    QString latestKey="";
    for(int i=keys.size()-1;i>-1;i--)
    {
        if(keys[i].endsWith("/"+key))
        {
            latestKey=keys[i];
            break;
        }
    }

    return latestKey;
}

QString sv4guisvFSIView::GetRegistryValue(QString category, QString key)
{
    QString value="";

    QSettings settings1("HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\svSolver", QSettings::NativeFormat);
    value=settings1.value(key).toString().trimmed();
    if(value!="")
        return value;

    QStringList keys=settings1.allKeys();
    QString latestKey=FindLatestKey(key,keys);
    if(latestKey!="")
    {
        value=settings1.value(latestKey).toString().trimmed();
        if(value!="")
            return value;
    }

    QSettings settings2("HKEY_LOCAL_MACHINE\\SOFTWARE\\WOW6432Node\\SimVascular\\svSolver", QSettings::NativeFormat);
    value=settings2.value(key).toString().trimmed();
    if(value!="")
        return value;

    keys=settings2.allKeys();
    latestKey=FindLatestKey(key,keys);
    if(latestKey!="")
    {
        value=settings2.value(latestKey).toString().trimmed();
        if(value!="")
            return value;
    }

    return "";
}
#endif

sv4guisvFSISolverProcessHandler::sv4guisvFSISolverProcessHandler(QProcess* process, mitk::DataNode::Pointer jobNode, int startStep, int totalSteps, QString runDir, QWidget* parent)
    : m_Process(process)
    , m_JobNode(jobNode)
    , m_StartStep(startStep)
    , m_TotalSteps(totalSteps)
    , m_RunDir(runDir)
    , m_Parent(parent)
    , m_Timer(NULL)
{
}

sv4guisvFSISolverProcessHandler::~sv4guisvFSISolverProcessHandler()
{
    if(m_Process)
        delete m_Process;

    if(m_Timer)
        delete m_Timer;
}

void sv4guisvFSISolverProcessHandler::Start()
{
    if(m_Process==NULL)
        return;

    if(m_JobNode.IsNull())
        return;

    connect(m_Process,SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(AfterProcessFinished(int,QProcess::ExitStatus)));

    m_JobNode->SetBoolProperty("running", true);
    m_JobNode->SetDoubleProperty("running progress", 0);
    mitk::GenericProperty<sv4guisvFSISolverProcessHandler*>::Pointer solverProcessProp=mitk::GenericProperty<sv4guisvFSISolverProcessHandler*>::New(this);
    m_JobNode->SetProperty("process handler",solverProcessProp);

    m_Process->start();

    m_Timer = new QTimer(this);
    connect(m_Timer, SIGNAL(timeout()), this, SLOT(UpdateStatus()));
    m_Timer->start(3000);
}

void sv4guisvFSISolverProcessHandler::KillProcess()
{
    if(m_Process)
        m_Process->kill();
}

void sv4guisvFSISolverProcessHandler::AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
    if(m_JobNode.IsNull())
        return;

    QString title="";
    QString text="";
    QMessageBox::Icon icon=QMessageBox::NoIcon;
    QMessageBox mb(NULL); //svSimualtionView maybe doesn't exist.
    QString status="";

    if(exitStatus==QProcess::NormalExit)
    {
        title="Finished";
        text="Job "+QString::fromStdString(m_JobNode->GetName())+": Finished.";
        icon=QMessageBox::Information;
        status="Simulation done";
        m_JobNode->SetBoolProperty("update rundir",true);
    }
    else
    {
        title="Not finished";
        text="Job "+QString::fromStdString(m_JobNode->GetName())+": Failed to finish.";
        icon=QMessageBox::Warning;
        status="Simulation failed";
    }

    mb.setWindowTitle(title);
    mb.setText(text+"                                                                                         ");
    mb.setIcon(icon);

    if(m_Process)
        mb.setDetailedText(m_Process->readAllStandardOutput()+"\n"+m_Process->readAllStandardError());

    mb.exec();

    sv4guiMitksvFSIJob* mitkJob=dynamic_cast<sv4guiMitksvFSIJob*>(m_JobNode->GetData());
    if(mitkJob)
        mitkJob->SetStatus(status.toStdString());

    m_JobNode->SetBoolProperty("running",false);
    m_JobNode->SetDoubleProperty("running progress", 0);

    mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());

    deleteLater();
}

void sv4guisvFSISolverProcessHandler::UpdateStatus()
{
    int currentStep=0;
    QString info="";

    QFile historFile(m_RunDir+"/histor.dat");
    if (historFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&historFile);
        QString content=in.readAll();

        QStringList list=content.split(QRegExp("[\r\n]"),QString::SkipEmptyParts);
        info=list.last();

        std::cout<<info.toStdString()<<std::endl;

        list=info.split(QRegExp("[(),{}-\\s+]"),QString::SkipEmptyParts);

        if(list.size()>1)
        {
            QString stepStr=list[1];
            bool ok;
            int step=stepStr.toInt(&ok);
            if(ok)
                currentStep=step;
        }

        historFile.close();
    }

    double progress=0;
    if(currentStep>m_StartStep && m_TotalSteps>0)
        progress=(currentStep-m_StartStep)*1.0/m_TotalSteps;

    m_JobNode->SetDoubleProperty("running progress", progress);

    QString status=QString::fromStdString(m_JobNode->GetName())+": running, " +QString::number((int)(progress*100))+"% completed. Info: "+info;
    mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());
}
