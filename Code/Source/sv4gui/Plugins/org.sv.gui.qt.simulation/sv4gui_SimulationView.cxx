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

#include <sstream>
#include <tuple>

#include "sv4gui_SimulationView.h"
#include "ui_sv4gui_SimulationView.h"

#include "sv4gui_TableCapDelegate.h"
#include "sv4gui_TableSolverDelegate.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MeshLegacyIO.h"
#include "sv4gui_MPIPreferencePage.h"
#include "sv4gui_SimulationUtils.h"
#include "sv4gui_SimulationPreferences.h"
#include "sv4gui_SimulationPreferencePage.h"

#include <QmitkStdMultiWidgetEditor.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
#include <mitkGenericProperty.h>

#include <mitkIPreferencesService.h>
#include <mitkIPreferences.h>
#include <berryPlatform.h>

#include <usModuleRegistry.h>

#include <QTreeView>
#include <QInputDialog>
#include <QMessageBox>
#include <QDomDocument>
#include <QDomElement>
#include <QDir>
#include <QProcess>
#include <QFileDialog>
#include <QThread>
#include <QSettings>
#include <QScrollArea>
#include <QVBoxLayout>
#include <QApplication>
#include <QRegularExpression>

using namespace sv4guiSimulationPreferenceDBKey;

const QString sv4guiSimulationView::EXTENSION_ID = "org.sv.views.simulation";

// Set the title for QMessageBox warnings.
//
// Note: On MacOS the window title is ignored (as required by the Mac OS X Guidelines). 
const QString sv4guiSimulationView::MsgTitle = "SimVascular CFD Simulation";

//----------------------
// sv4guiSimulationView
//----------------------
//
sv4guiSimulationView::sv4guiSimulationView() :
    ui(new Ui::sv4guiSimulationView)
{
    m_MitkJob = nullptr;
    m_Model = nullptr;
    m_JobNode = nullptr;
    m_ModelNode = nullptr;

    m_DataInteractor = nullptr;
    m_ModelSelectFaceObserverTag = -1;

    m_BasicParametersPage = nullptr;

    m_InletOutletBCsPage = nullptr;
    m_InletOutletBCs_caps_table = nullptr;

    //m_WallPropsPage = nullptr;
    //m_WallPropsPage_variable_props = nullptr;

    m_CapBCWidget = nullptr;

    m_SolverParametersPage = nullptr;

    m_SolverPath = "";
    m_MPIExecPath = "";

    m_ConnectionEnabled = false;

    // Get the default solver binaries.
    //m_DefaultPrefs = sv4guiSimulationPreferences();

    m_CmmSimulationType = "inflate";
}

sv4guiSimulationView::~sv4guiSimulationView()
{
    delete ui;

    if(m_BasicParametersPage) {
        delete m_BasicParametersPage;
    }

    if(m_InletOutletBCsPage) {
        delete m_InletOutletBCsPage;
    }

    if(m_InletOutletBCs_caps_table) {
        delete m_InletOutletBCs_caps_table;
    }

    if(m_SolverParametersPage) {
        delete m_SolverParametersPage;
    }

    if(m_CapBCWidget) {
        delete m_CapBCWidget;
    }
}

//------------------
// EnableConnection
//------------------
// Set slots for changed parameter values to be written the the .sjb file.
//
// [TODO] It is not clear why this is needed, most other tools don't
// have this method.
//
void sv4guiSimulationView::EnableConnection(bool enable)
{
    #define n_debug_EnableConnection
    #ifdef debug_EnableConnection 
    std::string msg("[sv4guiSimulationView::EnableConnection] ");
    std::cout << msg << "========== EnableConnection ==========" << std::endl;
    std::cout << msg << "enable: " << enable << std::endl;
    std::cout << msg << "m_ConnectionEnabled: " << m_ConnectionEnabled << std::endl;
    #endif
    auto slot = SLOT(UpdateSimJob());

    if (enable && !m_ConnectionEnabled) {

        #ifdef debug_EnableConnection 
        std::cout << msg << "++++ connect " << std::endl;
        #endif

        connect(m_BasicParametersPage, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));

        connect(m_InletOutletBCsPage, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));

        connect(ui->WallProps_type,SIGNAL(currentIndexChanged(int )), this, SLOT(UpdateSimJob( )));
        connect(ui->WallProps_thickness, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        connect(ui->WallProps_elastic_modulus, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        connect(ui->WallProps_poisson_ratio, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        connect(ui->WallProps_density, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        //connect(m_WallPropsPage, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));

        // Solver paraemters 
        connect(m_SolverParametersPage, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));

        connect(ui->comboBoxMeshName, SIGNAL(currentIndexChanged(int )), this, SLOT(UdpateSimJobMeshName( )));

        connect(ui->sliderNumProcs, SIGNAL(valueChanged(double)), this, SLOT(UpdateSimJobNumProcs()));

        m_ConnectionEnabled = enable;
    }

    if (!enable && m_ConnectionEnabled) {

        #ifdef debug_EnableConnection 
        std::cout << msg << "---- disconnect " << std::endl;
        #endif

        disconnect(m_BasicParametersPage, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));

        disconnect(m_InletOutletBCsPage, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));

        disconnect(ui->WallProps_type,SIGNAL(currentIndexChanged(int )), this, SLOT(UpdateSimJob( )));
        disconnect(ui->WallProps_thickness, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        disconnect(ui->WallProps_elastic_modulus, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        disconnect(ui->WallProps_poisson_ratio, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        disconnect(ui->WallProps_density, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        //disconnect(m_WallPropsPage, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));

        // Solver parameters 
        disconnect(m_SolverParametersPage, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));

        disconnect(ui->comboBoxMeshName, SIGNAL(currentIndexChanged(int )), this, SLOT(UdpateSimJobMeshName( )));
        disconnect(ui->sliderNumProcs, SIGNAL(valueChanged(double)), this, SLOT(UpdateSimJobNumProcs()));
        m_ConnectionEnabled = enable;
    }

}

//---------------------
// CreateQtPartControl
//---------------------
// Set the GUI widgets event/callbacks.
//
void sv4guiSimulationView::CreateQtPartControl( QWidget *parent )
{
    m_Parent = parent;
    ui->setupUi(parent);
    ui->btnSave->hide();

    // Upper panel.
    connect(ui->checkBoxShowModel, SIGNAL(clicked(bool)), this, SLOT(ShowModel(bool)) );

    // Set toolbox page (Basic Parameters).
    ui->toolBox->setCurrentIndex(0);

    // Basic Parameters toolbox page 
    //
    m_BasicParametersPage = new QStandardItemModel(this);
    ui->BasicParameters_table->setModel(m_BasicParametersPage);
    connect( ui->BasicParameters_table, SIGNAL(doubleClicked(const QModelIndex&)), this, 
        SLOT(TableViewBasicDoubleClicked(const QModelIndex&)) );

    connect(ui->BasicParameters_pressure_ic_set_file_name, SIGNAL(clicked()), this, SLOT(SetPressureICFile()));
    connect(ui->BasicParameters_velocity_ic_set_file_name, SIGNAL(clicked()), this, SLOT(SetVelocityICFile()));

    // Inlet and outlet BCS toolbox page 
    //
    m_InletOutletBCsPage = new QStandardItemModel(this);
    ui->InletOutletBCs_page->setModel(m_InletOutletBCsPage);
    sv4guiTableCapDelegate* itemDelegate=new sv4guiTableCapDelegate(this);
    ui->InletOutletBCs_page->setItemDelegateForColumn(1,itemDelegate);

    connect( ui->InletOutletBCs_page->selectionModel(), 
        SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) ), this, 
        SLOT( TableCapSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    connect( ui->InletOutletBCs_page, SIGNAL(doubleClicked(const QModelIndex&)), this, 
        SLOT(TableViewCapDoubleClicked(const QModelIndex&)) );

    connect( ui->InletOutletBCs_page, SIGNAL(customContextMenuRequested(const QPoint&)), this, 
        SLOT(TableViewCapContextMenuRequested(const QPoint&)) );

    // Table listing mesh inlet/outlet faces (aka caps).
    //
    m_InletOutletBCs_caps_table = new QMenu(ui->InletOutletBCs_page);
    QAction* setBCAction = m_InletOutletBCs_caps_table->addAction("Set BC");
    QAction* setPressureAction = m_InletOutletBCs_caps_table->addAction("Set Distal Pressure");
    connect( setBCAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowCapBCWidget(bool) ) );
    connect( setPressureAction, SIGNAL( triggered(bool) ) , this, SLOT( SetDistalPressure(bool) ) );

    QAction* splitBCRAction = m_InletOutletBCs_caps_table->addAction("Split Resistance");
    QAction* splitBCCAction = m_InletOutletBCs_caps_table->addAction("Split Capacitance");
    connect( splitBCRAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowSplitBCWidgetR(bool) ) );
    connect( splitBCCAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowSplitBCWidgetC(bool) ) );

    m_CapBCWidget = new sv4guiCapBCWidget();
    m_CapBCWidget->move(400,400);
    m_CapBCWidget->hide();
    m_CapBCWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_CapBCWidget,SIGNAL(accepted()), this, SLOT(SetCapBC()));

    m_SplitBCWidget=new sv4guiSplitBCWidget();
    m_SplitBCWidget->move(400,400);
    m_SplitBCWidget->hide();
    m_SplitBCWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_SplitBCWidget,SIGNAL(accepted()), this, SLOT(SplitCapBC()));

    // Wall properties toolbox page
    //
    connect(ui->WallProps_type, SIGNAL(currentIndexChanged(int)), this, SLOT(WallTypeSelectionChanged(int)));
    ui->WallProps_density->setText("1.0");

    ui->WallProps_constant_props->hide();
    ui->WallProps_elastic_props->hide();
    ui->WallProps_variable_props->hide();

    connect(ui->WallsProps_set_variable_props_file, SIGNAL(clicked()), this, SLOT(SetVariableWallPropsFile()));
    ui->WallsProps_variable_props_file->setText("none");

    // Coupled Momentum Method toolbox page
    //
    connect(ui->CmmSim_enable_cmm_simulation, SIGNAL(toggled(bool)), this, 
        SLOT(CmmSim_enable_cmm_simulation_changed(bool)));

    connect(ui->CmmSimType_inflate, SIGNAL(toggled(bool)), this, SLOT(CmmSimType_changed(bool)));
    connect(ui->CmmSimType_prestress, SIGNAL(toggled(bool)), this, SLOT(CmmSimType_changed(bool)));

    connect(ui->CmmSim_Initialize, SIGNAL(toggled(bool)), this, SLOT(CmmSim_Initialize_changed()));
    connect(ui->CmmSim_WallFile_set_file_name, SIGNAL(clicked()), this, SLOT(SetCmmSimWallFile()));
    connect(ui->CmmSim_TractionFile_set_file_name, SIGNAL(clicked()), this, SLOT(SetCmmSimTractionFile()));

    // Solver Parameters toolbox page
    //
    m_SolverParametersPage = new QStandardItemModel(this);
    ui->SolverParameters_table->setModel(m_SolverParametersPage);
    sv4guiTableSolverDelegate* itemSolverDelegate = new sv4guiTableSolverDelegate(this);
    ui->SolverParameters_table->setItemDelegateForColumn(1,itemSolverDelegate);

    // Create Files and Run Simulation toolbox page //
    //
    connect(ui->btnCreateAllFiles, SIGNAL(clicked()), this, SLOT(CreateAllFiles()) );
    connect(ui->btnRunJob, SIGNAL(clicked()), this, SLOT(RunJob()) );
    ui->NumberProcessesLabel->setEnabled(true);
    ui->sliderNumProcs->setEnabled(true);

    // Set paths for the external solvers.
    mitk::IPreferences* prefs = this->GetPreferences();
    this->OnPreferencesChanged(prefs);

    //UpdateSimJob();
}

//----------------------
// OnPreferencesChanged 
//----------------------
// Set solver binaries, mpiexec binary and the MPI implementation 
// used to create and execute simulation jobs.
//
// This method is called when the CFD Simulation plugin is activated or when values 
// in the Preferences Page->SimVascular Simulation panel are changed. 
//
// If a SimVascular MITK database exists from previous SimVascular sessions then 
// the values for the binaries are obtained from there. If the database does not
// exist then set the values of the binaries to their default values, which
// are the same values set for the SimVascular Simulation Preferences page.
//
// The svMultiPhysics binary is svmultiphysics. The value for the binary
// contains the full path to the binary together with its name. For example, 
//
//     m_SolverPath = "/usr/local/sv/svMultiPhysics/DATE/bin/svmultiphysics"
//
// Note that the 'binary' may actually be a shell script that sets up environment
// variables and then executes the actual binary. In that case m_FlowsolverPath
// is set to (on Linux) 
//
//     m_FlowsolverPath = "/usr/local/sv/svMultiPhysics"
//
void sv4guiSimulationView::OnPreferencesChanged(const mitk::IPreferences* prefs)
{
    if (prefs == nullptr) {
        return;
    }

    // Set the solver binary.
    m_SolverPath = prefs->Get(SOLVER_PATH, m_DefaultPrefs.GetSolver().toStdString());

    // Set the mpiexec binary and mpi implementation.
    m_MPIExecPath = prefs->Get(sv4guiMPIPreferenceDBKey::MPI_EXEC_PATH, m_DefaultMPIPrefs.GetMpiExec().toStdString()); 
    auto mpiName = prefs->Get(sv4guiMPIPreferenceDBKey::MPI_IMPLEMENTATION, m_DefaultMPIPrefs.GetMpiImplementationName().toStdString());
    m_MpiImplementation = m_DefaultMPIPrefs.GetMpiImplementation(QString::fromStdString(mpiName));
}

//--------------------
// OnSelectionChanged
//--------------------
// Update the CFD Simulation plugin panel when an CFD Simulation plugin is selected from the
// SV Data Manager.
//
// This method is also called when SimVascular starts and there is a CFD Simulation plugin 
// panel left over from a previous session. In this case nodes.size()==0.
//
void sv4guiSimulationView::OnSelectionChanged(berry::IWorkbenchPart::Pointer /*part*/,
                                              const QList<mitk::DataNode::Pointer>& nodes)
{
    #define n_debug_OnSelectionChanged 
    #ifdef debug_OnSelectionChanged 
    std::string msg("[sv4guiSimulationView::OnSelectionChanged] ");
    std::cout << msg << "========== OnSelectionChanged ==========" << std::endl;
    std::cout << msg << "nodes.size(): " << nodes.size() << std::endl;
    #endif

    if (!m_isVisible) {
        return;
    }

    if (nodes.size() == 0) {
        RemoveObservers();
        EnableTool(false);
        return;
    }

    mitk::DataNode::Pointer jobNode = nodes.front();
    sv4guiMitkSimJob* mitkJob = dynamic_cast<sv4guiMitkSimJob*>(jobNode->GetData());

    if (!mitkJob) {
        RemoveObservers();
        EnableTool(false);
        return;
    }

    #ifdef debug_OnSelectionChanged 
    std::cout << msg << "Create sv4guiMitkSimJob " << std::endl;
    #endif

    std::string modelName = mitkJob->GetModelName();
    mitk::DataNode::Pointer modelNode = nullptr;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (jobNode,isProjFolder,false);

    if (rs->size() > 0) {
        mitk::DataNode::Pointer projFolderNode = rs->GetElement(0);
        rs = GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiModelFolder"));

        if (rs->size() > 0) {
            mitk::DataNode::Pointer modelFolderNode = rs->GetElement(0);
            modelNode = GetDataStorage()->GetNamedDerivedNode(modelName.c_str(),modelFolderNode);
        }
    }

    sv4guiModel* model = nullptr;

    if (modelNode.IsNotNull()) {
        model = dynamic_cast<sv4guiModel*>(modelNode->GetData());
    }

    if (m_JobNode.IsNotNull()) {
        RemoveObservers();
    }

    m_ModelNode = modelNode;
    m_Model = model;
    m_JobNode = jobNode;
    m_MitkJob = mitkJob;

    #ifdef debug_OnSelectionChanged 
    std::cout << msg << "Create m_MitkJob " << std::endl;
    std::cout << msg << "Create m_JobNode " << std::endl;
    #endif

    if (m_Model == nullptr) {
        EnableTool(false);
    } else {
        EnableTool(true);
        AddObservers();
    }

    // Update the GUI above the ToolBox tabs. 
    //
    ui->labelJobName->setText(QString::fromStdString(m_JobNode->GetName()));
    ui->labelJobStatus->setText(QString::fromStdString(m_MitkJob->GetStatus()));
    ui->checkBoxShowModel->setChecked(false);

    if(m_ModelNode.IsNotNull()) {
        ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
        if(m_ModelNode->IsVisible(nullptr))
            ui->checkBoxShowModel->setChecked(true);
    } else {
        ui->labelModelName->setText("No model found");
    }

    #ifdef debug_OnSelectionChanged 
    std::cout << msg << "EnableConnection(false) ... " << std::endl;
    std::cout << msg << "Update GUI Basic Solver ..." << std::endl;
    #endif

    EnableConnection(false);

    UpdateGUIBasic();

    UpdateGUICap();

    UpdateGUIWall();

    UpdateGUICmm();

    UpdateGUISolver();

    UpdateGUIJob();

    UpdateFaceListSelection();

    UpdateJobStatus();

    #ifdef debug_OnSelectionChanged 
    std::cout << msg << "EnableConnection(true) " << std::endl;
    #endif

    EnableConnection(true);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-------------
// NodeChanged
//-------------
//
void sv4guiSimulationView::NodeChanged(const mitk::DataNode* node)
{
    if(m_JobNode.IsNotNull() && m_JobNode==node)
    {
        ui->labelJobName->setText(QString::fromStdString(m_JobNode->GetName()));
        UpdateJobStatus();

        bool updateRunDir=false;
        m_JobNode->GetBoolProperty("update rundir",updateRunDir);
        if(updateRunDir)
        {
            //davep UpdateGUIRunDir();
            m_JobNode->SetBoolProperty("update rundir",false);
        }

    }
}

void sv4guiSimulationView::NodeAdded(const mitk::DataNode* node)
{

}

void sv4guiSimulationView::NodeRemoved(const mitk::DataNode* node)
{

}

void sv4guiSimulationView::Activated() 
{

}

void sv4guiSimulationView::Deactivated() 
{

}

void sv4guiSimulationView::Visible()
{
    m_isVisible = true;
    OnSelectionChanged(berry::IWorkbenchPart::Pointer(), 
                       GetDataManagerSelection());
}

void sv4guiSimulationView::Hidden()
{
    m_isVisible = false;
    RemoveObservers();
}

//--------------
// AddObservers
//--------------
//
void sv4guiSimulationView::AddObservers()
{
    if(m_ModelNode.IsNotNull())
    {
        if(m_ModelNode->GetDataInteractor().IsNull())
        {
            m_DataInteractor = sv4guiModelDataInteractor::New();
            m_DataInteractor->LoadStateMachine("sv4gui_ModelInteraction.xml", us::ModuleRegistry::GetModule("sv4guiModuleModel"));
            m_DataInteractor->SetEventConfig("sv4gui_ModelConfig.xml", us::ModuleRegistry::GetModule("sv4guiModuleModel"));
            m_DataInteractor->SetDataNode(m_ModelNode);
        }
        m_ModelNode->SetStringProperty("interactor user","simulation");
        sv4guiModelDataInteractor* interactor=dynamic_cast<sv4guiModelDataInteractor*>(m_ModelNode->GetDataInteractor().GetPointer());
        if(interactor)
            interactor->SetFaceSelectionOnly();
    }

    if(m_Model && m_ModelSelectFaceObserverTag==-1)
    {
        itk::SimpleMemberCommand<sv4guiSimulationView>::Pointer modelSelectFaceCommand = itk::SimpleMemberCommand<sv4guiSimulationView>::New();
        modelSelectFaceCommand->SetCallbackFunction(this, &sv4guiSimulationView::UpdateFaceListSelection);
        m_ModelSelectFaceObserverTag = m_Model->AddObserver( sv4guiModelSelectFaceEvent(), modelSelectFaceCommand);
    }
}

void sv4guiSimulationView::RemoveObservers()
{
    if(m_Model && m_ModelSelectFaceObserverTag!=-1)
    {
        m_Model->RemoveObserver(m_ModelSelectFaceObserverTag);
        m_ModelSelectFaceObserverTag=-1;
    }

    if(m_ModelNode)
    {
        std::string user="";
        m_ModelNode->GetStringProperty("interactor user", user);
        if(user=="simulation")
            m_ModelNode->SetDataInteractor(nullptr);
    }
    m_DataInteractor=nullptr;
}

void sv4guiSimulationView::ClearAll()
{
    m_Model=nullptr;
    m_JobNode=nullptr;
    m_MitkJob=nullptr;
    m_ModelNode=nullptr;

    ui->labelJobName->setText("");
    ui->labelJobStatus->setText("");
    ui->labelModelName->setText("");
}

//----------------
// UpdateGUIBasic
//----------------
// Update the GUI basic parameters with the values
// read in from a .sjb file.
//
// If there no sv4guiSimJob had been defined then it seems
// that default values are set here.
//
void sv4guiSimulationView::UpdateGUIBasic()
{
    if (!m_MitkJob) {
        return;
    }

    sv4guiSimJob* job = m_MitkJob->GetSimJob();

    if (job == nullptr) {
        job = new sv4guiSimJob();
    }

    //m_BasicParametersPage->clear();

    // Set the table headers.
    //
    QStringList basicHeaders;
    basicHeaders << "Parameter" << "Value";
    m_BasicParametersPage->setHorizontalHeaderLabels(basicHeaders);
    m_BasicParametersPage->setColumnCount(2);

    // Set the table parameter names.
    //
    // Note: the names are used as the parameter name in 
    // sv4guiSimJob->SetBasicProp() calls.
    //
    QList<QStandardItem*> parList;
    QList<QStandardItem*> valueList;
    QString value;

    parList << new QStandardItem("Fluid Density");
    value = QString::fromStdString(job->GetBasicProp("Fluid Density"));
    valueList<<new QStandardItem(value == "" ? QString("1.06"):value);

    parList << new QStandardItem("Fluid Viscosity");
    value = QString::fromStdString(job->GetBasicProp("Fluid Viscosity"));
    valueList<<new QStandardItem(value==""?QString("0.04"):value);

    parList<<new QStandardItem("Initial Pressure");
    value = QString::fromStdString(job->GetBasicProp("Initial Pressure"));
    valueList << new QStandardItem(value == "" ? QString("0"):value);

    parList << new QStandardItem("Initial Velocities");
    value = QString::fromStdString(job->GetBasicProp("Initial Velocities"));
    valueList << new QStandardItem(value == "" ? QString("0.0001 0.0001 0.0001"):value);

    for (int i = 0; i < parList.size(); i++) {
        parList[i]->setEditable(false);
        m_BasicParametersPage->setItem(i, 0, parList[i]);
        m_BasicParametersPage->setItem(i, 1, valueList[i]);
    }

    ui->BasicParameters_table->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->BasicParameters_table->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);

    ui->BasicParameters_pressure_ic_file_name->setText(QString::fromStdString(job->GetBasicProp("Pressure IC File")));
    ui->BasicParameters_velocity_ic_file_name->setText(QString::fromStdString(job->GetBasicProp("Velocity IC File")));
}

//-----------------------------
// TableViewBasicDoubleClicked
//-----------------------------
// Process input of basic parameter values.
//
// Parameters (even IC files) are displayed in a table.
//
void sv4guiSimulationView::TableViewBasicDoubleClicked(const QModelIndex& index)
{
    if (index.column() != 0) {
        return;
    }

    QModelIndexList indexesOfSelectedRows = ui->BasicParameters_table->selectionModel()->selectedRows();

    if (indexesOfSelectedRows.size() < 1) {
        return;
    }

    int row = indexesOfSelectedRows[0].row();
    QStandardItem* itemName = m_BasicParametersPage->item(row,0);

    if (itemName->text() != "IC File") {
        return;
    }

    QStandardItem* itemValue= m_BasicParametersPage->item(row,1);
    QString lastFileOpenPath=itemValue->text().trimmed();

    mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    mitk::IPreferences* prefs;

    if (prefService) {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    } else {
        prefs = nullptr;
    }

    if(lastFileOpenPath=="" || !QFile(lastFileOpenPath).exists()) {
        if(prefs != nullptr) {
            lastFileOpenPath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
        }
        if(lastFileOpenPath=="") {
            lastFileOpenPath=QDir::homePath();
        }
    }

    QString icFilePath = QFileDialog::getOpenFileName(m_Parent, tr("Select IC File (Restart)")
                                                            , lastFileOpenPath
                                                            , tr("All Files (*)"));

    icFilePath = icFilePath.trimmed();

    if (icFilePath.isEmpty()) {
        return;
    }

    if(prefs != nullptr) {
         prefs->Put("LastFileOpenPath", icFilePath.toStdString());
         prefs->Flush();
     }

    itemValue->setText(icFilePath);
}

//-------------------------
// UpdateFaceListSelection
//-------------------------
//
void sv4guiSimulationView::UpdateFaceListSelection()
{
    if(!m_Model) {
        return;
    }

    sv4guiModelElement* modelElement = m_Model->GetModelElement();

    if (modelElement == nullptr) {
        return;
    }

    //for InletOutletBCs_page
    disconnect( ui->InletOutletBCs_page->selectionModel()
                , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
                , this
                , SLOT( TableCapSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    ui->InletOutletBCs_page->clearSelection();

    int count=m_InletOutletBCsPage->rowCount();

    for(int i=0;i<count;i++) {
        QStandardItem* itemName= m_InletOutletBCsPage->item(i,0);
        std::string name=itemName->text().toStdString();

        if(modelElement->IsFaceSelected(name)) {
            QModelIndex mIndex=m_InletOutletBCsPage->index(i,1);
            ui->InletOutletBCs_page->selectionModel()->select(mIndex, QItemSelectionModel::Select|QItemSelectionModel::Rows);
        }
    }

    connect( ui->InletOutletBCs_page->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableCapSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

}

//--------------------------
// TableCapSelectionChanged
//--------------------------
//
void sv4guiSimulationView::TableCapSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    mitk::StatusBar::GetInstance()->DisplayText("");

    if (!m_Model) {
        return;
    }

    sv4guiModelElement* modelElement = m_Model->GetModelElement();

    if (modelElement == nullptr) {
        return;
    }

    QModelIndexList indexesOfSelectedRows = ui->InletOutletBCs_page->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin(); 
         it != indexesOfSelectedRows.end(); it++) {
        int row=(*it).row();
        std::string name= m_InletOutletBCsPage->item(row,0)->text().toStdString();
        modelElement->SelectFace(name);

        if (it==indexesOfSelectedRows.begin()) {
            double faceArea=modelElement->GetFaceArea(modelElement->GetFaceID(name));
            QString info="Face "+QString::fromStdString(name)+": Area="+QString::number(faceArea);
            mitk::StatusBar::GetInstance()->DisplayText(info.toStdString().c_str());
        }

    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiSimulationView::TableViewCapDoubleClicked(const QModelIndex& index)
{
    if(index.column()==0) {
        ShowCapBCWidget();
    }
}

void sv4guiSimulationView::TableViewCapContextMenuRequested( const QPoint & pos )
{
    m_InletOutletBCs_caps_table->popup(QCursor::pos());
}

//-----------------
// ShowCapBCWidget
//-----------------
//
void sv4guiSimulationView::ShowCapBCWidget(bool)
{
    QModelIndexList indexesOfSelectedRows = ui->InletOutletBCs_page->selectionModel()->selectedRows();

    if (indexesOfSelectedRows.size() < 1) {
        return;
    }

    std::map<std::string,std::string> props;
    std::string capName;
    int row = indexesOfSelectedRows[0].row();

    if(indexesOfSelectedRows.size() == 1) {
        capName=m_InletOutletBCsPage->item(row,0)->text().toStdString();
    } else {
        capName="multiple faces";
    }

    props["BC Type"]=m_InletOutletBCsPage->item(row,1)->text().toStdString();
    props["Values"]=m_InletOutletBCsPage->item(row,2)->text().toStdString();
    props["Pressure"]=m_InletOutletBCsPage->item(row,3)->text().toStdString();
    props["Analytic Shape"]=m_InletOutletBCsPage->item(row,4)->text().toStdString();
    props["Period"]=m_InletOutletBCsPage->item(row,5)->text().toStdString();
    props["Point Number"]=m_InletOutletBCsPage->item(row,6)->text().toStdString();
    props["Fourier Modes"]=m_InletOutletBCsPage->item(row,7)->text().toStdString();
    props["Flip Normal"]=m_InletOutletBCsPage->item(row,8)->text().toStdString();
    props["Flow Rate"]=m_InletOutletBCsPage->item(row,9)->text().toStdString();
    props["File"]=m_InletOutletBCsPage->item(row,10)->text().toStdString();
    props["Original File"]=m_InletOutletBCsPage->item(row,10)->text().toStdString();
    props["Timed Pressure"]=m_InletOutletBCsPage->item(row,11)->text().toStdString();
    props["Pressure Period"]=m_InletOutletBCsPage->item(row,12)->text().toStdString();
    props["Pressure Scaling"]=m_InletOutletBCsPage->item(row,13)->text().toStdString();
    props["R Values"]=m_InletOutletBCsPage->item(row,14)->text().toStdString();
    props["C Values"]=m_InletOutletBCsPage->item(row,15)->text().toStdString();

    m_CapBCWidget->UpdateGUI(capName,props);

    m_CapBCWidget->show();
}

//-------------------
// SetDistalPressure
//-------------------
//
void sv4guiSimulationView::SetDistalPressure(bool)
{
    QModelIndexList indexesOfSelectedRows = ui->InletOutletBCs_page->selectionModel()->selectedRows();

    if(indexesOfSelectedRows.size() < 1) {
        return;
    }

    bool ok=false;
    double pressure=QInputDialog::getDouble(m_Parent, "Set Distal Pressure", "Distal Pressure:", 0.0, 0, 1000000, 2, &ok);
    QString str=QString::number(pressure);

    if(!ok) {
        return;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin(); 
         it != indexesOfSelectedRows.end(); it++) {
        int row=(*it).row();

        QStandardItem* itemBCType= m_InletOutletBCsPage->item(row,1);
        if(itemBCType->text()!="" && itemBCType->text()!="Prescribed Velocities")
        {
            QStandardItem* itemPressure= m_InletOutletBCsPage->item(row,3);
            itemPressure->setText(str);
        }
    }
}

//----------
// SetCapBC
//----------
//
void  sv4guiSimulationView::SetCapBC()
{
    QModelIndexList indexesOfSelectedRows = ui->InletOutletBCs_page->selectionModel()->selectedRows();

    if(indexesOfSelectedRows.size() < 1) {
        return;
    }

    std::map<std::string, std::string> props=m_CapBCWidget->GetProps();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        m_InletOutletBCsPage->item(row,1)->setText(QString::fromStdString(props["BC Type"]));
        if(props["BC Type"]=="Resistance" || props["BC Type"]=="RCR" || props["BC Type"]=="Coronary")
        {
            m_InletOutletBCsPage->item(row,2)->setText(QString::fromStdString(props["Values"]));
        }
        else if(props["BC Type"]=="Prescribed Velocities")
        {
            if(props["Flow Rate"]!="")
                m_InletOutletBCsPage->item(row,2)->setText("Assigned");
        }
        else
        {
            m_InletOutletBCsPage->item(row,2)->setText("");
        }

        m_InletOutletBCsPage->item(row,3)->setText(QString::fromStdString(props["Pressure"]));
        m_InletOutletBCsPage->item(row,4)->setText(QString::fromStdString(props["Analytic Shape"]));
        m_InletOutletBCsPage->item(row,5)->setText(QString::fromStdString(props["Period"]));
        m_InletOutletBCsPage->item(row,6)->setText(QString::fromStdString(props["Point Number"]));
        m_InletOutletBCsPage->item(row,7)->setText(QString::fromStdString(props["Fourier Modes"]));
        m_InletOutletBCsPage->item(row,8)->setText(QString::fromStdString(props["Flip Normal"]));
        m_InletOutletBCsPage->item(row,9)->setText(QString::fromStdString(props["Flow Rate"]));
        m_InletOutletBCsPage->item(row,10)->setText(QString::fromStdString(props["Original File"]));

        m_InletOutletBCsPage->item(row,11)->setText(QString::fromStdString(props["Timed Pressure"]));
        m_InletOutletBCsPage->item(row,12)->setText(QString::fromStdString(props["Pressure Period"]));
        m_InletOutletBCsPage->item(row,13)->setText(QString::fromStdString(props["Pressure Scaling"]));
        m_InletOutletBCsPage->item(row,14)->setText(QString::fromStdString(props["R Values"]));
        m_InletOutletBCsPage->item(row,15)->setText(QString::fromStdString(props["C Values"]));
    }
}

//-------------------
// ShowSplitBCWidget
//-------------------
//
void sv4guiSimulationView::ShowSplitBCWidget(QString splitTarget)
{
    QModelIndexList indexesOfSelectedRows = ui->InletOutletBCs_page->selectionModel()->selectedRows();

    if(indexesOfSelectedRows.size() < 1) {
        return;
    }

    QString lastBCType="";
    for(int i=0;i<indexesOfSelectedRows.size();i++)
    {
        int row=indexesOfSelectedRows[i].row();
        QString BCType=m_InletOutletBCsPage->item(row,1)->text().trimmed();

        if(BCType=="")
        {
            QMessageBox::warning(m_Parent,"BC Type Missing","Please speficify BC type for the caps!");
            return;
        }
        else if(BCType!=lastBCType && lastBCType!="")
        {
            QMessageBox::warning(m_Parent,"BC Type Inconsistent","Please split BC for the caps of the same BC type!");
            return;
        }

        lastBCType=BCType;
    }

    if(lastBCType=="Resistance" && splitTarget=="Capacitance")
    {
        QMessageBox::warning(m_Parent,"Warning","Can't split capacitance for BC type Resistance!");
        return;
    }

    m_SplitBCWidget->UpdateGUI(lastBCType,splitTarget);

    m_SplitBCWidget->show();
}

void sv4guiSimulationView::ShowSplitBCWidgetR(bool)
{
    ShowSplitBCWidget("Resistance");
}

void sv4guiSimulationView::ShowSplitBCWidgetC(bool)
{
    ShowSplitBCWidget("Capacitance");
}

void  sv4guiSimulationView::SplitCapBC()
{
    if(!m_MitkJob) {
        return;
    }

    if(!m_Model) {
        return;
    }

    if(!m_SplitBCWidget) {
        return;
    }

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==nullptr) {
        return;
    }

    QModelIndexList indexesOfSelectedRows = ui->InletOutletBCs_page->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    QString bcType=m_SplitBCWidget->GetBCType();
    QString splitTarget=m_SplitBCWidget->GetSplitTarget();
    double totalValue=m_SplitBCWidget->GetTotalValue();
    double murrayCoefficient=m_SplitBCWidget->GetMurrayCoefficient();
    double percentage1=m_SplitBCWidget->GetPercentage1();
    double percentage2=m_SplitBCWidget->GetPercentage2();
    double percentage3=m_SplitBCWidget->GetPercentage3();

    double totalMurrayArea=0;
    std::vector<double> faceMurrayArea;
    for(int i=0;i<indexesOfSelectedRows.size();i++)
    {
        int row=indexesOfSelectedRows[i].row();
        std::string faceName=m_InletOutletBCsPage->item(row,0)->text().trimmed().toStdString();
        double murrayArea=pow(modelElement->GetFaceArea(modelElement->GetFaceID(faceName)),murrayCoefficient/2);
        totalMurrayArea+=murrayArea;
        faceMurrayArea.push_back(murrayArea);
    }

    for(int i=0;i<indexesOfSelectedRows.size();i++)
    {
        int row=indexesOfSelectedRows[i].row();

        if(splitTarget=="Resistance")
        {
            double murrayRatio=totalMurrayArea/faceMurrayArea[i];
            if(bcType=="Resistance")
            {
                m_InletOutletBCsPage->item(row,2)->setText(QString::number(murrayRatio*totalValue));
            }
            else if(bcType=="RCR")
            {
                QString Rp=QString::number(murrayRatio*totalValue*percentage1);
                QString CC="0";
                QString Rd=QString::number(murrayRatio*totalValue*percentage2);

                QStringList list = m_InletOutletBCsPage->item(row,15)->text().split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
                if(list.size()==1)
                    CC=list[0];

                m_InletOutletBCsPage->item(row,2)->setText(Rp+" "+CC+" "+Rd);
                m_InletOutletBCsPage->item(row,14)->setText(Rp+" "+Rd);
            }
            else if(bcType=="Coronary")
            {
                QString Ra=QString::number(murrayRatio*totalValue*percentage1);
                QString Ca="0";
                QString Ram=QString::number(murrayRatio*totalValue*percentage2);
                QString Cim="0";
                QString Rv=QString::number(murrayRatio*totalValue*percentage3);

                QStringList list = m_InletOutletBCsPage->item(row,15)->text().split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
                if(list.size()==2)
                {
                    Ca=list[0];
                    Cim=list[1];
                }

                m_InletOutletBCsPage->item(row,2)->setText(Ra+" "+Ca+" "+Ram+" "+Cim+" "+Rv);
                m_InletOutletBCsPage->item(row,14)->setText(Ra+" "+Ram+" "+Rv);
            }
        }
        else if(splitTarget=="Capacitance")
        {
            double murrayRatio=faceMurrayArea[i]/totalMurrayArea;
            if(bcType=="RCR")
            {
                QString Rp="0";
                QString CC=QString::number(murrayRatio*totalValue);
                QString Rd="0";

                QStringList list = m_InletOutletBCsPage->item(row,14)->text().split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
                if(list.size()==2)
                {
                    Rp=list[0];
                    Rd=list[1];
                }

                m_InletOutletBCsPage->item(row,2)->setText(Rp+" "+CC+" "+Rd);
                m_InletOutletBCsPage->item(row,15)->setText(CC);
            }
            else if(bcType=="Coronary")
            {
                QString Ra="0";
                QString Ca=QString::number(murrayRatio*totalValue*percentage1);
                QString Ram="0";
                QString Cim=QString::number(murrayRatio*totalValue*percentage2);
                QString Rv="0";

                QStringList list = m_InletOutletBCsPage->item(row,14)->text().split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
                if(list.size()==3)
                {
                    Ra=list[0];
                    Ram=list[1];
                    Rv=list[2];
                }

                m_InletOutletBCsPage->item(row,2)->setText(Ra+" "+Ca+" "+Ram+" "+Cim+" "+Rv);
                m_InletOutletBCsPage->item(row,15)->setText(Ca+" "+Cim);
            }
        }
    }
}

//--------------
// UpdateGUICap
//--------------
//
void sv4guiSimulationView::UpdateGUICap()
{
    if(!m_MitkJob) {
        return;
    }

    if(!m_Model) {
        return;
    }

    sv4guiModelElement* modelElement=m_Model->GetModelElement();

    if(modelElement == nullptr) {
        return;
    }

    sv4guiSimJob* job = m_MitkJob->GetSimJob();
    if(job==nullptr) {
        job=new sv4guiSimJob();
    }

    m_InletOutletBCsPage->clear();

    QStringList capHeaders;
    capHeaders << "Name" << "BC Type" << "Values" << "Pressure"
               << "Analytic Shape" << "Period" << "Point Number" << "Fourier Modes" << "Flip Normal" << "Flow Rate" << "Original File"
               << "Timed Pressure" << "Pressure Period" << "Pressure Scaling"
               << "R Values" << "C Values";
    m_InletOutletBCsPage->setHorizontalHeaderLabels(capHeaders);
    m_InletOutletBCsPage->setColumnCount(capHeaders.size());

    std::vector<int> ids=modelElement->GetCapFaceIDs();
    int rowIndex=-1;
    for(int i=0;i<ids.size();i++)
    {
        sv4guiModelElement::svFace* face=modelElement->GetFace(ids[i]);
        if(face==nullptr )
            continue;

        rowIndex++;
        m_InletOutletBCsPage->insertRow(rowIndex);

        QStandardItem* item;

        item= new QStandardItem(QString::fromStdString(face->name));
        item->setEditable(false);
        m_InletOutletBCsPage->setItem(rowIndex, 0, item);

        std::string bcType=job->GetCapProp(face->name,"BC Type");
        item= new QStandardItem(QString::fromStdString(bcType));
        m_InletOutletBCsPage->setItem(rowIndex, 1, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Values")));
        m_InletOutletBCsPage->setItem(rowIndex, 2, item);
        if(bcType=="Prescribed Velocities" && job->GetCapProp(face->name,"Flow Rate")!="")
        {
            item= new QStandardItem(QString::fromStdString("Assigned"));
            m_InletOutletBCsPage->setItem(rowIndex, 2, item);
        }

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Pressure")));
        m_InletOutletBCsPage->setItem(rowIndex, 3, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Analytic Shape")));
        m_InletOutletBCsPage->setItem(rowIndex, 4, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Period")));
        m_InletOutletBCsPage->setItem(rowIndex, 5, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Point Number")));
        m_InletOutletBCsPage->setItem(rowIndex, 6, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Fourier Modes")));
        m_InletOutletBCsPage->setItem(rowIndex, 7, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Flip Normal")));
        m_InletOutletBCsPage->setItem(rowIndex, 8, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Flow Rate")));
        m_InletOutletBCsPage->setItem(rowIndex, 9, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Original File")));
        m_InletOutletBCsPage->setItem(rowIndex, 10, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Timed Pressure")));
        m_InletOutletBCsPage->setItem(rowIndex, 11, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Pressure Period")));
        m_InletOutletBCsPage->setItem(rowIndex, 12, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Pressure Scaling")));
        m_InletOutletBCsPage->setItem(rowIndex, 13, item);

        QString RValues="";
        QString CValues="";
        QStringList list =QString::fromStdString(job->GetCapProp(face->name,"Values")).split(QRegularExpression("[(),{}\\s+]"), 
            Qt::SkipEmptyParts);

        if(bcType=="RCR")
        {
            if(list.size()==3)
            {
                RValues=list[0]+" "+list[2];
                CValues=list[1];
            }
        }
        else if(bcType=="Coronary")
        {
            if(list.size()==5)
            {
                RValues=list[0]+" "+list[2]+" "+list[4];
                CValues=list[1]+" "+list[3];
            }
        }

        item= new QStandardItem(RValues);
        m_InletOutletBCsPage->setItem(rowIndex, 14, item);

        item= new QStandardItem(CValues);
        m_InletOutletBCsPage->setItem(rowIndex, 15, item);
    }

    ui->InletOutletBCs_page->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->InletOutletBCs_page->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    ui->InletOutletBCs_page->horizontalHeader()->resizeSection(1,100);
    ui->InletOutletBCs_page->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);

    for (int i = 3; i < capHeaders.size(); i++) {
        ui->InletOutletBCs_page->setColumnHidden(i,true);
    }

}

//--------------------------
// SetVariableWallPropsFile
//--------------------------
// Set the name of the VTK file used for variable wall properties.
//
void sv4guiSimulationView::SetVariableWallPropsFile()
{
  #define n_debug_SetVariableWallPropsFile
  #ifdef debug_SetVariableWallPropsFile
  std::string msg("[sv4guiSimulationView::SetVariableWallPropsFile] ");
  std::cout << msg << "========== SetVariableWallPropsFile ==========" << std::endl;
  #endif

  mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
  mitk::IPreferences* prefs;

  if (prefService) {
    prefs = prefService->GetSystemPreferences()->Node("/General");
  } else {
    prefs = nullptr; 
  }

  QString lastFileOpenPath = "";

  if (prefs != nullptr) {
    lastFileOpenPath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
  }

  if (lastFileOpenPath == "") {
    lastFileOpenPath = QDir::homePath();
  }

  QString file_path = QFileDialog::getOpenFileName(ui->WallsProps_variable_props_file, 
      tr("Set variable wall properties file"), lastFileOpenPath, tr("VTK VTP Files (*.vtp)"));

  file_path = file_path.trimmed();

  if (file_path.isEmpty()) {
    return;
  }

  if (prefs != nullptr) {
    prefs->Put("LastFileOpenPath", file_path.toStdString());
    prefs->Flush();
  }

#if 0
  QFile inputFile(file_path);

  if (inputFile.open(QIODevice::ReadOnly)) {
        QTextStream in(&inputFile);

        QFileInfo fi(pressureFilePath);
        ui->labelLoadPressureFile->setText(fi.fileName());

        std::stringstream ss;
        QString line;
        while (1) {
            line=in.readLine();
            if(line.isNull())
                break;

            if(line.contains("#"))
                continue;

            QStringList list = line.split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
            if(list.size()!=2)
                continue;

            ss << list[0].toStdString() << " " << list[1].toStdString() <<"\n";

            pressurePeriod=list[0];
        }

        m_TimedPressureContent=ss.str();
        inputFile.close();
    }

#endif

  #ifdef debug_SetVariableWallPropsFile
  std::cout << msg << "file_path: " << file_path << std::endl;
  #endif

  ui->WallsProps_variable_props_file->setText(file_path);

  //UpdateSimJob();
}

//-------------------
// SetPressureICFile
//-------------------
//
void sv4guiSimulationView::SetPressureICFile()
{
  mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
  mitk::IPreferences* prefs;

  if (prefService) {
    prefs = prefService->GetSystemPreferences()->Node("/General");
  } else {
    prefs = nullptr; 
  }

  QString lastFileOpenPath = "";

  if (prefs != nullptr) {
    lastFileOpenPath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
  }

  if (lastFileOpenPath == "") {
    lastFileOpenPath = QDir::homePath();
  }

  QString file_path = QFileDialog::getOpenFileName(ui->BasicParameters_pressure_ic_set_file_name, 
      tr("Set pressure IC file"), lastFileOpenPath, tr("VTK VTU Files (*.vtu)"));

  file_path = file_path.trimmed();

  if (file_path.isEmpty()) {
    return;
  }

  if (prefs != nullptr) {
    prefs->Put("LastFileOpenPath", file_path.toStdString());
    prefs->Flush();
  }

  ui->BasicParameters_pressure_ic_file_name->setText(file_path);

  //UpdateSimJob();
}

//-------------------
// SetVelocityICFile
//-------------------
//
void sv4guiSimulationView::SetVelocityICFile()
{
  mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
  mitk::IPreferences* prefs;

  if (prefService) {
    prefs = prefService->GetSystemPreferences()->Node("/General");
  } else {
    prefs = nullptr; 
  }

  QString lastFileOpenPath = "";

  if (prefs != nullptr) {
    lastFileOpenPath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
  }

  if (lastFileOpenPath == "") {
    lastFileOpenPath = QDir::homePath();
  }

  QString file_path = QFileDialog::getOpenFileName(ui->BasicParameters_velocity_ic_set_file_name, 
      tr("Set velocity IC file"), lastFileOpenPath, tr("VTK VTU Files (*.vtu)"));

  file_path = file_path.trimmed();

  if (file_path.isEmpty()) {
    return;
  }

  if (prefs != nullptr) {
    prefs->Put("LastFileOpenPath", file_path.toStdString());
    prefs->Flush();
  }

  ui->BasicParameters_velocity_ic_file_name->setText(file_path);

  //UpdateSimJob();
}

//-------------------
// SetCmmSimWallFile 
//-------------------
// Set the name of the VTK file used for a shell wall.
//
void sv4guiSimulationView::SetCmmSimWallFile()
{
  #define n_debug_SetCmmSimWallFile
  #ifdef debug_SetCmmSimWallFile
  std::string msg("[sv4guiSimulationView::SetCmmSimWallFile] ");
  std::cout << msg << "========== SetCmmSimWallFile ==========" << std::endl;
  #endif

  mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
  mitk::IPreferences* prefs;

  if (prefService) {
    prefs = prefService->GetSystemPreferences()->Node("/General");
  } else {
    prefs = nullptr; 
  }

  QString lastFileOpenPath = "";

  if (prefs != nullptr) {
    lastFileOpenPath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
  }

  if (lastFileOpenPath == "") {
    lastFileOpenPath = QDir::homePath();
  }

  QString file_path = QFileDialog::getOpenFileName(ui->CmmSim_WallFile_set_file_name, 
      tr("Set wall shell file"), lastFileOpenPath, tr("VTK VTP Files (*.vtp)"));

  file_path = file_path.trimmed();

  if (file_path.isEmpty()) {
    return;
  }

  if (prefs != nullptr) {
    prefs->Put("LastFileOpenPath", file_path.toStdString());
    prefs->Flush();
  }

  ui->CmmSim_WallFile_file_name->setText(file_path);

  //UpdateSimJob();
}

//-----------------------
// SetCmmSimTractionFile 
//-----------------------
// Set the name of the VTK file used for traction values.
//
void sv4guiSimulationView::SetCmmSimTractionFile()
{
  #define n_debug_SetCmmSimTractionFile
  #ifdef debug_SetCmmSimTractionFile
  std::string msg("[sv4guiSimulationView::SetCmmSimTractionFile] ");
  std::cout << msg << "========== SetCmmSimTractionFile ==========" << std::endl;
  #endif

  mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
  mitk::IPreferences* prefs;

  if (prefService) {
    prefs = prefService->GetSystemPreferences()->Node("/General");
  } else {
    prefs = nullptr; 
  }

  QString lastFileOpenPath = "";

  if (prefs != nullptr) {
    lastFileOpenPath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
  }

  if (lastFileOpenPath == "") {
    lastFileOpenPath = QDir::homePath();
  }

  QString file_path = QFileDialog::getOpenFileName(ui->CmmSim_TractionFile_set_file_name, 
      tr("Set traction values file"), lastFileOpenPath, tr("VTK VTP Files (*.vtp)"));

  file_path = file_path.trimmed();

  if (file_path.isEmpty()) {
    return;
  }

  if (prefs != nullptr) {
    prefs->Put("LastFileOpenPath", file_path.toStdString());
    prefs->Flush();
  }

  ui->CmmSim_TractionFile_file_name->setText(file_path);

  //UpdateSimJob();
}

//--------------------------
// WallTypeSelectionChanged
//--------------------------
//
void sv4guiSimulationView::WallTypeSelectionChanged(int index)
{
    switch(index) {

      // Rigid
      case 0:
        ui->WallProps_constant_props->hide();
        ui->WallProps_elastic_props->hide();
        ui->WallProps_variable_props->hide();
      break;

      // Deformable with constant properties 
      case 1:
        ui->WallProps_constant_props->show();
        ui->WallProps_elastic_props->show();
        ui->WallProps_variable_props->hide();
      break;

      // Deformable with variable thickness and elastic properties 
      case 2:
        ui->WallProps_constant_props->show();
        ui->WallProps_elastic_props->hide();
        ui->WallProps_variable_props->show();
      break;

      default:
      break;
    }
}

//---------------
// UpdateGUIWall
//---------------
// Update the GUI controls for the Wall Properties tab from a .sjb file.
//
void sv4guiSimulationView::UpdateGUIWall()
{
    if (!m_MitkJob) {
        return;
    }

    sv4guiSimJob* job = m_MitkJob->GetSimJob();

    // [TODO] Memory leak ?
    if (job == nullptr) {
        job = new sv4guiSimJob();
    }

    if (job->GetWallProp("Type") == "rigid") {
        ui->WallProps_type->setCurrentIndex(0);

    } else if(job->GetWallProp("Type") == "constant") {
        ui->WallProps_type->setCurrentIndex(1);

    } else if(job->GetWallProp("Type")=="variable") {
        ui->WallProps_type->setCurrentIndex(2);

    } else {
        ui->WallProps_type->setCurrentIndex(0);
    }

    ui->WallProps_thickness->setText(QString::fromStdString(job->GetWallProp("Thickness")));
    ui->WallProps_elastic_modulus->setText(QString::fromStdString(job->GetWallProp("Elastic Modulus")));

    QString pratio = QString::fromStdString(job->GetWallProp("Poisson Ratio"));
    if (pratio == "") {
        pratio = "0.5";
    }
    ui->WallProps_poisson_ratio->setText(pratio);

    ui->WallProps_density->setText(QString::fromStdString(job->GetWallProp("Density")));

    ui->WallsProps_variable_props_file->setText(QString::fromStdString(job->GetWallProp("Wall Properities File")));

    if (!m_Model) {
        return;
    }

    sv4guiModelElement* modelElement = m_Model->GetModelElement();
    if (modelElement == nullptr) {
        return;
    }
}

//--------------
// UpdateGUICmm
//--------------
// Update the GUI controls for the Coupled Momentum Method tab from a .sjb file.
//
void sv4guiSimulationView::UpdateGUICmm()
{
    #define n_debug_UpdateGUICmm
    #ifdef debug_UpdateGUICmm 
    std::string msg("[sv4guiSimulationView::UpdateGUICmm] ");
    std::cout << msg << "========== UpdateGUICmm ==========" << std::endl;
    #endif

    if (!m_MitkJob) {
        return;
    }

    sv4guiSimJob* job = m_MitkJob->GetSimJob();

    #ifdef debug_UpdateGUICmm 
    std::cout << msg << "job: " << job << std::endl;
    #endif

    if (job == nullptr) {
        job = new sv4guiSimJob();
    }

    #ifdef debug_UpdateGUICmm 
    std::cout << msg << "Set type checked ... " << std::endl;
    #endif

    if (job->GetCmmProp("Simulation Type") == "inflate") {
        ui->CmmSimType_inflate->setChecked(true);
    } else if (job->GetCmmProp("Simulation Type") == "prestress") {
        ui->CmmSimType_prestress->setChecked(true);
    }

    #ifdef debug_UpdateGUICmm 
    std::cout << msg << "Set enable checked ... " << std::endl;
    #endif

    ui->CmmSim_enable_cmm_simulation->setChecked(job->GetCmmProp("Enable cmm simulation") == "true");

    #ifdef debug_UpdateGUICmm 
    std::cout << msg << "Set init checked ... " << std::endl;
    #endif

    ui->CmmSim_Initialize->setChecked(job->GetCmmProp("Initialize simulation") == "true");

    ui->CmmSim_WallFile_file_name->setText(QString::fromStdString(job->GetCmmProp("Wall file")));
    ui->CmmSim_TractionFile_file_name->setText(QString::fromStdString(job->GetCmmProp("Traction file")));

    #ifdef debug_UpdateGUICmm 
    std::cout << msg << "Done " << std::endl;
    #endif
}

//---------------------------
// CmmSim_Initialize_changed
//---------------------------
//
void sv4guiSimulationView::CmmSim_Initialize_changed(bool checked)
{
}

//--------------------------------------
// CmmSim_enable_cmm_simulation_changed 
//--------------------------------------
//
void sv4guiSimulationView::CmmSim_enable_cmm_simulation_changed(bool checked)
{
  #define n_debug_CmmSim_enable_cmm_simulation_changed
  #ifdef debug_CmmSim_enable_cmm_simulation_changed
  std::string msg("[sv4guiSimulationView::CmmSim_enable_cmm_simulation_changed] ");
  std::cout << msg << "========== CmmSim_enable_cmm_simulation_changed ==========" << std::endl;
  std::cout << msg << "checked: " << checked << std::endl;
  #endif

  //UpdateSimJob();
}

//--------------------
// CmmSimType_changed
//--------------------
// Process selecting a CmmSimType radio button.
//
// [TODO] Could not get QButtonGroup to work to pass a button index.
//
// Need to call UpdateSimJob() method to store new values in sv4guiSimJob ?
//
void sv4guiSimulationView::CmmSimType_changed(bool checked)
{
  #define n_debug_CmmSimType_changed 
  #ifdef debug_CmmSimType_changed 
  std::string msg("[sv4guiSimulationView::CmmSimType_changed] ");
  std::cout << msg << "========== CmmSimType_changed ==========" << std::endl;
  #endif

  auto sender = qobject_cast<QRadioButton*>(QObject::sender());
  int page_index = 0;
  
  if (sender->text() == ui->CmmSimType_inflate->text()) {
    page_index = 1;

  } else if (sender->text() == ui->CmmSimType_prestress->text()) {
    page_index = 2;
  }

  #ifdef debug_CmmSimType_changed 
  std::cout << msg << "page_index: " << page_index << std::endl;
  #endif

  m_CmmSimulationType = sender->text().toStdString();

  #ifdef debug_CmmSimType_changed 
  std::cout << msg << "m_CmmSimulationType: " << m_CmmSimulationType << std::endl;
  #endif

  //ui->CmmSimulation_pages->setCurrentIndex(page_index);

  #ifdef debug_CmmSimType_changed 
  std::cout << msg << "Done " << std::endl;
  std::cout << msg << std::endl;
  #endif
}

//-----------------
// UpdateGUISolver
//-----------------
// Update the GUI solver parameters with the values read in from a .sjb file.
//
// The rows of the 'Solver Parameters' table are taken from the resources/solvertemplate.xml file.
// Note that these are used as parameter names in the sv4guiSimJob object.
//
void sv4guiSimulationView::UpdateGUISolver()
{
  #define n_debug_UpdateGUISolver
  #ifdef debug_UpdateGUISolver 
  std::string msg("[sv4guiSimulationView::UpdateGUISolver] ");
  std::cout << msg << std::endl;
  std::cout << msg << "######################################" << std::endl;
  std::cout << msg << "==========  UpdateGUISolver ==========" << std::endl;
  #endif

  if (!m_MitkJob) {
    return;
  }

  sv4guiSimJob* job = m_MitkJob->GetSimJob();
  #ifdef debug_UpdateGUISolver 
  std::cout << msg << "sv4guiSimJob* job: " << job << std::endl;
  #endif

  if (job == nullptr) {
    job = new sv4guiSimJob();
  }

  m_SolverParametersPage->clear();

  QStringList solverHeaders;
  solverHeaders << "Parameter" << "Value" << "Type" << "Value List";
  m_SolverParametersPage->setHorizontalHeaderLabels(solverHeaders);
  int colCount = solverHeaders.size();
  m_SolverParametersPage->setColumnCount(colCount);

  QString templateFilePath = ":solvertemplate.xml";
  QFile xmlFile(templateFilePath);

  if (!xmlFile.open(QIODevice::ReadOnly)) {
    QMessageBox::warning(m_Parent,"Info Missing","Solver Parameter Table template file not found");
    return;
  }

  QDomDocument doc("solvertemplate");
  if (!doc.setContent(&xmlFile)) {
    QMessageBox::warning(m_Parent,"File Template Error","Format Error.");
    return;
  }
  xmlFile.close();

  QDomElement templateElement = doc.firstChildElement("template");
  QDomNodeList sectionList = templateElement.elementsByTagName("section");
  int rowIndex = -1;

  #ifdef debug_UpdateGUISolver 
  std::cout << msg << "sectionList.size(): " << sectionList.size() << std::endl;
  #endif

  for (int i = 0; i < sectionList.size(); i++) {
    #ifdef debug_UpdateGUISolver 
    std::cout << msg << "----------- i " << i << " ---------- " << std::endl;
    #endif
    QDomNode sectionNode = sectionList.item(i);

    if (sectionNode.isNull()) {
      continue;
    }

    QDomElement sectionElement = sectionNode.toElement();
    if (sectionElement.isNull()) {
      continue;
    }
    
    QString section_name = sectionElement.attribute("name");
    QStandardItem* item = new QStandardItem(section_name);
    item->setEditable(false);
    QBrush brushGray(Qt::lightGray);
    item->setBackground(brushGray);

    #ifdef debug_UpdateGUISolver 
    std::cout << msg << "section_name: " << section_name << std::endl;
    #endif

    rowIndex += 1;
    m_SolverParametersPage->setItem(rowIndex, 0, item);
    ui->SolverParameters_table->setSpan(rowIndex, 0, 1, colCount);
    QDomNodeList parList = sectionElement.elementsByTagName("param");

    for (int j = 0; j < parList.size(); j++) {
      #ifdef debug_UpdateGUISolver 
      std::cout << msg << "------ j " << j << " -----" << std::endl;
      #endif
      QDomNode parNode = parList.item(j);

      if(parNode.isNull()) {
        continue;
      }

      QDomElement parElement = parNode.toElement();

      if (parElement.isNull()) {
        continue;
      }

      QString name = parElement.attribute("name");

      QStandardItem* item = new QStandardItem(name);
      item->setEditable(false);
      item->setToolTip(parElement.attribute("name"));

      rowIndex += 1;
      m_SolverParametersPage->setItem(rowIndex, 0, item);

      // Save the section that this paramter is under, needed later to set parameter props
      // based on section so duplicate parameter names can be used.
      m_SolverParametersPageSections[rowIndex] = section_name.toStdString();

      QString qvalue;
      auto att_name = parElement.attribute("name").toStdString();
      std::string value = job->GetSolverProp(att_name);
      auto par_value = parElement.attribute("value");
      if (value == "") {
        qvalue = par_value;
      } else {
        qvalue = QString::fromStdString(value);
      }

      item = new QStandardItem(qvalue);
      //item = new QStandardItem(value == "" ? parElement.attribute("value"):QString::fromStdString(value));
      m_SolverParametersPage->setItem(rowIndex, 1, item);

      #ifdef debug_UpdateGUISolver 
      std::cout << msg << "name: '" << name << "'" << std::endl;
      std::cout << msg << "att_name: '" << att_name << "'" << std::endl;
      std::cout << msg << "value: " << value << std::endl;
      std::cout << msg << "qvalue: " << qvalue << std::endl;
      #endif

      item = new QStandardItem(parElement.attribute("type"));
      item->setEditable(false);
      m_SolverParametersPage->setItem(rowIndex, 2, item);

      item = new QStandardItem(parElement.attribute("enum_list"));
      item->setEditable(false);
      m_SolverParametersPage->setItem(rowIndex, 3, item);
    }
  }

  ui->SolverParameters_table->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
  ui->SolverParameters_table->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
  ui->SolverParameters_table->horizontalHeader()->resizeSection(1,120);

  ui->SolverParameters_table->setColumnHidden(2,true);
  ui->SolverParameters_table->setColumnHidden(3,true);
}

void sv4guiSimulationView::UpdateGUIJob()
{
    if(!m_MitkJob)
        return;

    std::string modelName=m_MitkJob->GetModelName();
    std::vector<std::string> meshNames;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiMeshFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);
            rs=GetDataStorage()->GetDerivations(meshFolderNode);

            for(int i=0;i<rs->size();i++)
            {
                sv4guiMitkMesh* mitkMesh=dynamic_cast<sv4guiMitkMesh*>(rs->GetElement(i)->GetData());
                if(mitkMesh&&mitkMesh->GetModelName()==modelName)
                {
                    meshNames.push_back(rs->GetElement(i)->GetName());
                }
            }
        }
    }

    ui->comboBoxMeshName->clear();
    for(int i=0;i<meshNames.size();i++)
        ui->comboBoxMeshName->addItem(QString::fromStdString(meshNames[i]));

    int foundIndex=ui->comboBoxMeshName->findText(QString::fromStdString(m_MitkJob->GetMeshName()));
    ui->comboBoxMeshName->setCurrentIndex(foundIndex);

    int coreNum=QThread::idealThreadCount();
    ui->sliderNumProcs->setMaximum(coreNum);

    sv4guiSimJob* job=m_MitkJob->GetSimJob();
    if(job==nullptr)
        return;

    std::string pNum=job->GetRunProp("Number of Processes");
    ui->sliderNumProcs->setValue(pNum==""?1:QString::fromStdString(pNum).toInt());
}

QString sv4guiSimulationView::GetJobPath()
{
    QString jobPath="";

    if(m_JobNode.IsNull())
        return jobPath;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);

    std::string projPath="";
    std::string simFolderName="";

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);
        projFolderNode->GetStringProperty("project path", projPath);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiSimulationFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer simFolderNode=rs->GetElement(0);
            simFolderName=simFolderNode->GetName();
            jobPath=QString::fromStdString(projPath+"/"+simFolderName+"/"+m_JobNode->GetName());
        }
    }

    return jobPath;
}

void sv4guiSimulationView::CreateAllFiles()
{
    if(!m_MitkJob)
        return;

    CreateDataFiles(GetJobPath(), true, true, false);
}

//--------
// RunJob
//--------
// Run a simulation job.
//
void sv4guiSimulationView::RunJob()
{
  #define n_debug_RunJob
  #ifdef debug_RunJob
  std::string msg("[sv4guiSimulationView::RunJob] ");
  std::cout << msg << "========== RunJob ==========" << std::endl;
  #endif

  if (!m_MitkJob) {
    return;
  }

  // Check that the directory for a job has been created.
  //
  QString jobPath = GetJobPath();

  if ((jobPath == "") || !QDir(jobPath).exists()) {
    QString msg1 = "The simulation job cannot be run.\n\n";
    QString msg2 = "Please make sure that data files have been created for the simulation.";
    QMessageBox::warning(m_Parent, MsgTitle, msg1+msg2);
    return;
  }

  try {

    CheckSolver();
    CheckMpi();
    auto job = m_MitkJob->GetSimJob();

    if (!job) {
      QMessageBox::warning(m_Parent, MsgTitle, "Cannot start job, simulation job does not exist.");
      throw std::string("Job does not exist"); 
    }
	
    // Set the solver output directory.
    QString runPath = jobPath;
    int numProcs = ui->sliderNumProcs->value();
    auto results_dir = QString::fromStdString(job->GetSolverProp("Save results in folder"));

    // The 'Save results in folder' defualt value is 'N-procs' which means
    // results will be saved in the numProcs//"-procs' directory. 
    //
    if (results_dir == "N-procs") {
      runPath = jobPath + "/" + QString::number(numProcs)+"-procs";
    } else {
      runPath = jobPath + "/" + results_dir;
    }
    #ifdef debug_RunJob
    std::cout << msg << "results_dir: " << results_dir << std::endl;
    std::cout << msg << "runPath: " << runPath << std::endl;
    #endif

    // Execute the job.
    //
    int totalSteps = 100;


    job->SetRunProp("Number of Processes",QString::number(numProcs).toStdString());
    totalSteps = QString::fromStdString(job->GetSolverProp("Number of Timesteps")).toInt();
    mitk::StatusBar::GetInstance()->DisplayText("Running a CFD simulation");

    QProcess* solverProcess = new QProcess(m_Parent);
    solverProcess->setWorkingDirectory(jobPath);

    QStringList arguments;
    arguments << "-n" << QString::number(numProcs) 
        << QString::fromStdString(m_SolverPath) 
        << QString::fromStdString(m_SolverInputFileName);
    solverProcess->setProgram(QString::fromStdString(m_MPIExecPath));
    solverProcess->setArguments(arguments);

    sv4guiSolverProcessHandler* handler = new sv4guiSolverProcessHandler(solverProcess, m_JobNode, 
            0, totalSteps, runPath, m_Parent);

    handler->Start();

  } catch (std::string exception) {
      std::cout << "Run job failed with: " <<  exception << std::endl; 
  }
}

//------------------
// GetStartTimeStep 
//------------------
// Get the simulation start step number.
//
// The simulation start step number is obtained from the GUI 'Starting Step Number'
// if it is given. If it is not then it is read from a numstart.dat file.  The numstart.dat 
// file is written by the solver at the end of each simulation time step for which results 
// were computed. For a numProcs=1 numstart.dat is written to PROJECT/Simulations/JOB_NAME/,
// for numProcs=N is is written to PROJECT/Simulations/JOB_NAME/N-procs_case.
//
// Arguments:
//
//   numProcs: The number of processors used to run the simulation job.
//
//   runPath: The path to the PROJECT/Simulations/JOB_NAME/ directory that
//            simulation results are written to. For a N-processor simulation
//            it is runPath/'N-procs_case'.
//
//   jobPath: The path to the PROJECT/Simulations/JOB_NAME/ directory that 
//            contains solver files (e.g. solver.inp).
//
// Returns:
//
//   startStepNumber: The simulation starting time step. 
//   
// 
#ifdef old_svsolver
int sv4guiSimulationView::GetStartTimeStep(const QString& runPath, const QString& jobPath, const int numProcs)
{
    auto badValue = false;
    std::string exception("Write numstart file");

    // Read / write the numstart.dat file to runPath.
    auto fileName = runPath + "/numstart.dat";
    QFile numStartFile(fileName);

    // A starting step has been given so check that a restart file
    // exists for the given starting step.
    //
    if (startStep != "") { 
        QString runRestart = runPath+"/restart."+startStep+".1";
        QString jobRestart = jobPath+"/restart."+startStep+".1";

        if ( (QDir(runPath).exists() && !QFile(runRestart).exists()) || 
             ((numProcs > 1) && !QDir(runPath).exists() && !QFile(jobRestart).exists()) ) {
            QString msg1 = "No restart file found in " + runPath + " for the starting step number " + startStep + 
                " and the number of processors " + QString::number(numProcs) + ".\n";
            QMessageBox::warning(m_Parent, MsgTitle, msg1);
            throw exception; 
        }

        // Write the start step to the numstart.dat file.
        if(numStartFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
            QTextStream out(&numStartFile);
            out << startStep + "\n";
            numStartFile.close();
        }

    // No start step has been given so read it from the
    // numstart.dat file.
    } else if (numStartFile.open(QIODevice::ReadOnly)) {   
        QTextStream in(&numStartFile);
        QString stepStr = in.readLine();
        bool ok;
        int step = stepStr.toInt(&ok);
        if (ok) {
            startStepNumber = step;
        }
        numStartFile.close();
    }

    return startStepNumber;
}
#endif

//-------------
// CheckSolver
//-------------
// Check for valid solver binaries.
//
void sv4guiSimulationView::CheckSolver()
{
    std::string exception("Check solver");

    // Set the name and path to check for the solver binaries.
    typedef std::tuple<QString,QString> binaryNamePath;
    std::vector<binaryNamePath> binariesToCheck = { 
        std::make_tuple("Solver", QString::fromStdString(m_SolverPath)),
    };

    // Check the name and path for the solver binaries.
    //
    for (auto const& namePath : binariesToCheck) {
        auto name = std::get<0>(namePath);
        auto path = std::get<1>(namePath);

        if ((path == "") || (path == m_DefaultPrefs.UnknownBinary)) {
            auto msg1 = "The " + name + " executable cannot be found. \n";
            auto msg2 = "Please install " + name + " and set its location in the Preferences->SimVascular Simulation page.";
            QMessageBox::warning(m_Parent, MsgTitle, msg1+msg2);
            throw exception; 
        }

        QFileInfo check_file(path);
        if (!check_file.exists()) {
            auto msg1 = "The " + name + " executable '" + path + "' cannot be found. \n\n";
            auto msg2 = "Please set the " + name + " executable in the Preferences->SimVascular Simulation page.";
            QMessageBox::warning(m_Parent, MsgTitle, msg1+msg2);
            throw exception; 
        }

        if (!check_file.isFile()) {
            auto msg1 = "The " + name + " executable '" + path + "' does not name a file. \n";
            auto msg2 = "Please set the " + name + " executable in the Preferences->SimVascular Simulation page.";
            QMessageBox::warning(m_Parent, MsgTitle, msg1+msg2);
            throw exception; 
        }
    }
}

//----------
// CheckMpi
//----------
// Check for valid mpiexec binary and MPI implementation.
//
// svSolver needs the OpenMPI implementation.
//
void sv4guiSimulationView::CheckMpi()
{
    std::string exception("Check MPI");

    // Check for valid mpiexec.
    //
    QString name = "mpiexec";
    QString path = QString::fromStdString(m_MPIExecPath); 

    if ((path == "") || (path == m_DefaultPrefs.UnknownBinary)) {
        auto msg1 = "The " + name + " executable cannot be found. \n\n";
        auto msg2 = "Please install MPI and set its location in the Preferences->SimVascular Simulation page.";
        QMessageBox::warning(m_Parent, MsgTitle, msg1+msg2);
        throw exception; 
     }

    QFileInfo check_file(path);

    if (!check_file.exists()) {
        auto msg1 = "The " + name + " executable '" + path + "' cannot be found. \n\n";
        auto msg2 = "Please set the " + name + " executable in the Preferences->SimVascular Simulation page.";
        QMessageBox::warning(m_Parent, MsgTitle, msg1+msg2);
        throw exception; 
    }

    // only msmpi allowed on win32
    #ifndef WIN32
    // Check the MPI implementation.
    auto mpiName = m_DefaultMPIPrefs.GetMpiImplementationName();
    if (m_MpiImplementation != sv4guiMPIPreferences::MpiImplementation::OpenMPI) {
       QString msg1 = "svSolver requires OpenMPI but an OpenMPI MPI implementation was not found.\n";
       QString msg2 = "Please install OpenMPI MPI or set the location of an OpenMPI mpiexec in the Preferences->SimVascular Simulation page.";
       QMessageBox::warning(m_Parent, MsgTitle, msg1+msg2);
       throw exception; 
    }
    #endif
}


//-----------------
// CreateDataFiles
//-----------------
// Create the files needed to run a simulation.
//
bool sv4guiSimulationView::CreateDataFiles(QString outputDir, bool outputAllFiles, bool updateJob, bool createFolder)
{
  #define n_debug_CreateDataFiles
  #ifdef debug_CreateDataFiles
  std::string msg("[sv4guiSimulationView::CreateDataFiles] ");
  std::cout << msg << "========== CreateDataFiles ==========" << std::endl;
  #endif

  if(!m_MitkJob) {
    return false;
  }

  if(outputDir == "") {
    return false;
  }
  #ifdef debug_CreateDataFiles
  std::cout << msg << "outputDir: " << outputDir.toStdString() << std::endl;
  #endif

  sv4guiModelElement* modelElement = nullptr;

  if (m_Model) {
    modelElement=m_Model->GetModelElement();
  }

  if (modelElement == nullptr) {
    QMessageBox::warning(m_Parent,"A model is not navailable","Please make sure that a model exists and is valid.");
    return false;
  }

  mitk::StatusBar::GetInstance()->DisplayText("Creating svMultiPhysics simulation files");
  std::string job_msg;

  sv4guiSimJob* job = CreateSimJob(job_msg);

  if (job == nullptr) {
    QMessageBox::warning(m_Parent,"Parameter Values Error",QString::fromStdString(job_msg));
    return false;
  }

  if (createFolder) {
    outputDir=outputDir+"/"+QString::fromStdString(m_JobNode->GetName())+"-files";
  }

  QDir dir(outputDir);
  dir.mkpath(outputDir);

  // Set face names and type.
  std::map<std::string,std::string> faces_name_type;
  auto faces = modelElement->GetFaces();
  for (auto face : faces ) { 
    faces_name_type[face->name] = face->type;
  }

  // Create solver XML file.
  //
  mitk::StatusBar::GetInstance()->DisplayText("Creating solver.xml");
  std::string file_name = outputDir.toStdString() + "/" + m_SolverInputFileName;
  sv4guiSimulationUtils::CreateSolverInputFile(job, faces_name_type, outputDir.toStdString(), file_name);

  // Create mesh files.
  //
  std::string meshName = "";

  if (outputAllFiles) {
    meshName = ui->comboBoxMeshName->currentText().toStdString();

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs = GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);

    sv4guiMesh* mesh = nullptr;
    mitk::DataNode::Pointer projFolderNode = nullptr;
    mitk::DataNode::Pointer meshNode = nullptr;

    if (rs->size()>0) {
      projFolderNode=rs->GetElement(0);
      rs = GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiMeshFolder"));

      if (rs->size()>0) {
        mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);
        meshNode=GetDataStorage()->GetNamedDerivedNode(meshName.c_str(),meshFolderNode);

        if (meshNode.IsNotNull()) {
          sv4guiMitkMesh* mitkMesh=dynamic_cast<sv4guiMitkMesh*>(meshNode->GetData());
          if(mitkMesh) {
            mesh=mitkMesh->GetMesh();
          }
        }
      }
    }

    if (mesh == nullptr) {
      QMessageBox::warning(m_Parent,"Mesh Unavailable","Please make sure the mesh exists and is valid.");
      return false;
    }

    mitk::StatusBar::GetInstance()->DisplayText("Creating mesh-complete files");
    QString meshCompletePath = outputDir+"/mesh-complete";
    dir.mkpath(meshCompletePath);
    WaitCursorOn();
    bool ok = sv4guiMeshLegacyIO::WriteFiles(meshNode, modelElement, meshCompletePath);
    WaitCursorOff();

    if (!ok) {
      QMessageBox::warning(m_Parent,"Mesh info missing","Please make sure the mesh exists and is valid.");
      return false;
    }
  }

  m_MitkJob->SetSimJob(job);
  m_MitkJob->SetMeshName(meshName);
  m_MitkJob->SetDataModified();

  QMessageBox::information(m_Parent, "Information", "The svMultiPhysics mesh files and the solver.xml input file have been created in " + 
      outputDir + ".");

  return true;
}

//--------------
// CreateSimJob
//--------------
// Create a sv4guiSimJob object and set its parameter values from the GUI.
//
// The sv4guiSimJob object is used to write GUI values to the .sjb XML file
// and create a solver.xml file.
//
sv4guiSimJob* sv4guiSimulationView::CreateSimJob(std::string& msg, bool checkValidity)
{
  #define n_debug_CreateSimJob
  #ifdef debug_CreateSimJob 
  std::string pmsg("[sv4guiSimulationView::CreateSimJob] ");
  std::cout << pmsg << "========== CreateSimJob ==========" << std::endl;
  std::cout << pmsg << "checkValidity: " << checkValidity << std::endl;
  #endif

  sv4guiSimJob* job = new sv4guiSimJob();
  checkValidity = false;

  if (!SetJobBasicProps(job, msg, checkValidity)) {
    delete job;
    return nullptr;
  }

  if (!SetJobCapProps(job, msg, checkValidity)) {
    delete job;
    return nullptr;
  }

  SetJobWallProps(job, msg, checkValidity);

  SetJobCmmProps(job, msg, checkValidity);

  if (!SetJobSolverProps(job, msg, checkValidity)) {
    delete job;
    return nullptr;
  }

  return job;
}

//-------------------
// SetJobSolverProps
//-------------------
//
bool sv4guiSimulationView::SetJobSolverProps(sv4guiSimJob* job, std::string& msg, bool checkValidity)
{
  #define n_debug_SetJobSolverProps 
  #ifdef debug_SetJobSolverProps 
  std::string pmsg("[sv4guiSimulationView::SetJobSolverProps] ");
  std::cout << pmsg << std::endl;
  std::cout << pmsg << "========== SetJobSolverProps ========== " << std::endl;
  std::cout << pmsg << "m_JobNode: " << m_JobNode->GetName() << std::endl;
  std::cout << pmsg << "m_SolverParametersPage->rowCount(): " << m_SolverParametersPage->rowCount() << std::endl;
  #endif

  #ifdef debug_SetJobSolverProps 
  std::cout << pmsg << "Process parameters ... " << std::endl;
  #endif

  for (int i = 0; i < m_SolverParametersPage->rowCount(); i++) {
    std::string parName = m_SolverParametersPage->item(i,0)->text().trimmed().toStdString();
    QStandardItem* valueItem = m_SolverParametersPage->item(i,1);

    #ifdef debug_SetJobSolverProps 
    std::cout << pmsg << "----- i " << i << " -----" << std::endl;
    std::cout << pmsg << "parName: " << parName << std::endl;
    #endif

    if (valueItem == nullptr) {
        #ifdef debug_SetJobSolverProps 
        std::cout << pmsg << "parName item is nullptr " << std::endl;
        #endif
      continue;
    }

    std::string value = valueItem->text().trimmed().toStdString();
    std::string type = m_SolverParametersPage->item(i,2)->text().trimmed().toStdString();

    #ifdef debug_SetJobSolverProps 
    std::cout << pmsg << "value: " << value << std::endl;
    std::cout << pmsg << "type: " << type << std::endl;
    #endif

    if (checkValidity ) {
      if (value == "") {
        msg = parName + " missing value";
        return false;

      } else if (type == "int" && !IsInt(value)) {
        msg = parName + " value error: " + value;
        return false;

      } else if (type == "double" && !IsDouble(value)) {
        msg = parName + " value error: " + value;
        return false;
      }
    }

    #ifdef debug_SetJobSolverProps 
    std::cout << pmsg << "Set prop " << parName << " to section " << m_SolverParametersPageSections[i] << std::endl;
    #endif

    job->SetSolverProp(parName, value, m_SolverParametersPageSections[i]);
  }

  return true;
}

//----------------
// SetJobCmmProps
//----------------
// Set Coupled Momentum Method parameters.
//
void sv4guiSimulationView::SetJobCmmProps(sv4guiSimJob* job, std::string& msg, bool checkValidity)
{
  #define n_debug_SetJobCmmProps 
  #ifdef debug_SetJobCmmProps 
  std::string pmsg("[sv4guiSimulationView::SetJobCmmProps] ");
  std::cout << pmsg << "========== SetJobCmmProps ========== " << std::endl;
  std::cout << pmsg << "m_CmmSimulationType: " << m_CmmSimulationType << std::endl;
  std::cout << pmsg << "CmmSim_enable_cmm_simulation: " << ui->CmmSim_enable_cmm_simulation->isChecked() << std::endl;
  #endif

  job->SetCmmProp("Simulation Type", m_CmmSimulationType);

  std::map<bool,std::string> bool_to_str{{true,"true"}, {false,"false"}};

  job->SetCmmProp("Enable cmm simulation", bool_to_str[ui->CmmSim_enable_cmm_simulation->isChecked()]);
  job->SetCmmProp("Initialize simulation", bool_to_str[ui->CmmSim_Initialize->isChecked()]);

  job->SetCmmProp("Wall file", ui->CmmSim_WallFile_file_name->text().trimmed().toStdString());
  job->SetCmmProp("Traction file",ui->CmmSim_TractionFile_file_name->text().trimmed().toStdString());
}

//------------------
// SetJobBasicProps
//------------------
// Set basic properties values.
//
bool sv4guiSimulationView::SetJobBasicProps(sv4guiSimJob* job, std::string& msg, bool checkValidity)
{
  #ifdef debug_SetJobBasicProps
  std::string pmsg("[sv4guiSimulationView::SetJobBasicProps] ");
  std::cout << pmsg << "Set basic properties values ... " << std::endl;
  #endif

  // Get values from the basic parameters table.
  //
  for (int i = 0; i < m_BasicParametersPage->rowCount(); i++) {
    std::string par = m_BasicParametersPage->item(i,0)->text().toStdString();
    std::string values = m_BasicParametersPage->item(i,1)->text().trimmed().toStdString();

    if (checkValidity) {
      if (par == "Fluid Density" || par == "Fluid Viscosity" || par == "Initial Pressure") {
        if(!IsDouble(values)) {
          msg = par + " value error: " + values;
          return false;
        }

      } else if (par == "Initial Velocities") {
        int count = 0;

        QStringList list = QString(values.c_str()).split(QRegularExpression("[(),{}\\s+]"), Qt::SkipEmptyParts);
        values=list.join(" ").toStdString();

        if(!AreDouble(values,&count) || count!=3) {
          msg = par + " value error: " + values;
          return false;
        }
      }
    }

  job->SetBasicProp(par, values);
  }

  job->SetBasicProp("Pressuee IC File", ui->BasicParameters_pressure_ic_file_name->text().toStdString());
  job->SetBasicProp("Velocity IC File", ui->BasicParameters_velocity_ic_file_name->text().toStdString());

  return true;
}

//----------------
// SetJobCapProps
//----------------
//
bool sv4guiSimulationView::SetJobCapProps(sv4guiSimJob* job, std::string& msg, bool checkValidity)
{
  #ifdef debug_SetJobCapProps
  std::string pmsg("[sv4guiSimulationView::SetJobCapProps] ");
  std::cout << pmsg << "Set cap properties values ... " << std::endl;
  #endif

  for(int i=0;i<m_InletOutletBCsPage->rowCount();i++) {
    std::string capName=m_InletOutletBCsPage->item(i,0)->text().toStdString();
    std::string bcType=m_InletOutletBCsPage->item(i,1)->text().trimmed().toStdString();

    if(bcType=="Prescribed Velocities") {
      std::string flowrateContent=m_InletOutletBCsPage->item(i,9)->text().trimmed().toStdString();
      std::string period=m_InletOutletBCsPage->item(i,5)->text().trimmed().toStdString();

      if(checkValidity) {
        if(flowrateContent=="") {
          msg = capName + ": no flowrate data";
          return false;
        }

        if(period=="") {
          msg=capName + ": no period for flowrate data";
          return false;
        }
      }

      std::string shape=m_InletOutletBCsPage->item(i,4)->text().trimmed().toStdString();
      std::string pointNum=m_InletOutletBCsPage->item(i,6)->text().trimmed().toStdString();
      std::string modeNum=m_InletOutletBCsPage->item(i,7)->text().trimmed().toStdString();
      std::string flip=m_InletOutletBCsPage->item(i,8)->text().trimmed().toStdString();
      std::string originalFile=m_InletOutletBCsPage->item(i,10)->text().trimmed().toStdString();

      job->SetCapProp(capName,"BC Type", bcType);
      job->SetCapProp(capName,"Analytic Shape", shape);
      job->SetCapProp(capName,"Period", period);
      job->SetCapProp(capName,"Point Number", pointNum);
      job->SetCapProp(capName,"Fourier Modes", modeNum);
      job->SetCapProp(capName,"Flip Normal", flip);
      job->SetCapProp(capName,"Flow Rate", flowrateContent);
      job->SetCapProp(capName,"Original File", originalFile);

    } else if(bcType != "") {
      std::string values=m_InletOutletBCsPage->item(i,2)->text().trimmed().toStdString();
      std::string pressure=m_InletOutletBCsPage->item(i,3)->text().trimmed().toStdString();
      std::string originalFile=m_InletOutletBCsPage->item(i,10)->text().trimmed().toStdString();
      std::string timedPressure=m_InletOutletBCsPage->item(i,11)->text().trimmed().toStdString();
      std::string pressurePeriod=m_InletOutletBCsPage->item(i,12)->text().trimmed().toStdString();
      std::string pressureScaling=m_InletOutletBCsPage->item(i,13)->text().trimmed().toStdString();
      std::string RValues=m_InletOutletBCsPage->item(i,14)->text().trimmed().toStdString();
      std::string CValues=m_InletOutletBCsPage->item(i,15)->text().trimmed().toStdString();

      if(checkValidity) {
        if(bcType=="Resistance") {
          if(!IsDouble(values)) {
            msg=capName + " R value error: " + values;
            return false;
          }

        } else if(bcType=="RCR") {
          int count=0;
          QStringList list = QString(values.c_str()).split(QRegularExpression("[(),{}\\s+]"), Qt::SkipEmptyParts);
          values=list.join(" ").toStdString();

          if(!AreDouble(values,&count)||count!=3) {
            msg=capName + " RCR values error: " + values;
            return false;
          }

        } else if(bcType=="Coronary") {
          int count=0;
          QStringList list = QString(values.c_str()).split(QRegularExpression("[(),{}\\s+]"), Qt::SkipEmptyParts);
          values=list.join(" ").toStdString();

          if(!AreDouble(values,&count)||count!=5) {
            msg=capName + " Coronary values error: " + values;
            return false;
          }

          if(timedPressure=="") {
            msg=capName + ": no Pim data";
            return false;
          }

          if(pressurePeriod=="" || !IsDouble(pressurePeriod)) {
            msg=capName + " coronary period error: " + pressurePeriod;
            return false;
          }

          if(pressureScaling=="" || !IsDouble(pressureScaling)) {
            msg=capName + " coronary pressure scaling error: " + pressureScaling;
            return false;
          }
        }

        if(pressure!="") {
          if(!IsDouble(pressure)) {
            msg=capName + " pressure error: " + pressure;
            return false;
          }
        } else {
          pressure="0";
        }
      }

      job->SetCapProp(capName,"BC Type", bcType);
      job->SetCapProp(capName,"Values", values);
      job->SetCapProp(capName,"Pressure",pressure);

      if(bcType=="Coronary") {
        job->SetCapProp(capName,"Timed Pressure", timedPressure);
        job->SetCapProp(capName,"Pressure Period", pressurePeriod);
        job->SetCapProp(capName,"Pressure Scaling",pressureScaling);
        job->SetCapProp(capName,"Original File", originalFile);
      }

      if(bcType=="RCR" || bcType=="Coronary") {
        job->SetCapProp(capName,"R Values", RValues);
        job->SetCapProp(capName,"C Values", CValues);
      }
    }
  }

  return true;
}

//-----------------
// SetJobWallProps
//-----------------
//
void sv4guiSimulationView::SetJobWallProps(sv4guiSimJob* job, std::string& msg, bool checkValidity)
{
  #ifdef debug_SetJobWallProps
  std::string pmsg("[sv4guiSimulationView::SetJobWallProps] ");
  std::cout << pmsg << "Set wall properties values ... " << std::endl;
  std::cout << pmsg << "wallTypeIndex: " << wallTypeIndex << std::endl;
  #endif

  int wallTypeIndex = ui->WallProps_type->currentIndex();

  if (wallTypeIndex == 0) {
    job->SetWallProp("Type", "rigid");

  } else if(wallTypeIndex == 1) {
    std::string thickness = ui->WallProps_thickness->text().trimmed().toStdString();
    std::string modulus = ui->WallProps_elastic_modulus->text().trimmed().toStdString();
    std::string nu = ui->WallProps_poisson_ratio->text().trimmed().toStdString();
    std::string wallDensity = ui->WallProps_density->text().trimmed().toStdString();

    job->SetWallProp("Type", "constant");
    job->SetWallProp("Thickness", thickness);
    job->SetWallProp("Elastic Modulus", modulus);
    job->SetWallProp("Poisson Ratio", nu);
    job->SetWallProp("Density", wallDensity);

  } else if (wallTypeIndex == 2) {
    std::string nu = ui->WallProps_poisson_ratio->text().trimmed().toStdString();
    std::string wallDensity = ui->WallProps_density->text().trimmed().toStdString();
    std::string wallPropsFile = ui->WallsProps_variable_props_file->text().trimmed().toStdString();

    job->SetWallProp("Type", "variable");
    job->SetWallProp("Poisson Ratio", nu);
    job->SetWallProp("Density", wallDensity);
    job->SetWallProp("Wall Properities File", wallPropsFile);
    #ifdef debug_SetJobWallProps
    std::cout << pmsg << "wallPropsFile: " << wallPropsFile << std::endl;
    #endif
  }

}

//---------------
// SaveToManager
//---------------
// Save the simulation GUI data to .sjb files.
//
void sv4guiSimulationView::SaveToManager()
{
    #define n_debug_SaveToManager
    #ifdef debug_SaveToManager 
    std::string pmsg("[sv4guiSimulationView::SaveToManager] ");
    std::cout << pmsg << "========== SaveToManager ==========" << std::endl;
    #endif

    if (!m_MitkJob) {
        return;
    }

    std::string msg;

    sv4guiSimJob* job = CreateSimJob(msg);

    if (job == nullptr) {
        QMessageBox::warning(m_Parent,"Parameter Values Error",QString::fromStdString(msg));
        return;
    }

    m_MitkJob->SetSimJob(job);
    m_MitkJob->SetDataModified();
}

bool sv4guiSimulationView::IsInt(std::string value)
{
    bool ok;
    QString(value.c_str()).toInt(&ok);
    return ok;
}

bool sv4guiSimulationView::IsDouble(std::string value)
{
    bool ok;
    QString(value.c_str()).toDouble(&ok);
    return ok;
}

bool sv4guiSimulationView::AreDouble(std::string values, int* count)
{
    QStringList list = QString(values.c_str()).split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
    bool ok;
    for(int i=0;i<list.size();i++)
    {
        list[i].toDouble(&ok);
        if(!ok) return false;
    }

    if(count!=nullptr)
        (*count)=list.size();

    return true;
}

//------------
// EnableTool
//------------
// Enable the ToolBox pages.
//
void sv4guiSimulationView::EnableTool(bool able)
{
    ui->widgetTop->setEnabled(able);
    ui->Toolbox_basic_params_page->setEnabled(able);
    ui->Toolbox_bcs_page->setEnabled(able);
    ui->Toolbox_wall_props_page->setEnabled(able);
    ui->Toolbox_cmm_page->setEnabled(able);
    ui->Toolbox_solver_params_page->setEnabled(able);
    ui->Toolbox_create_files_page->setEnabled(able);
}

//--------------
// UpdateSimJob
//--------------
// Update the m_MitkJob sv4guiSimJob with GUI data.
//
// Note: It is not clear when this should be called. 
// Calling it from a GUI event seems to mess up
// the 'SolverParameters_table' table.
//
void sv4guiSimulationView::UpdateSimJob()
{
    #define n_debug_UpdateSimJob 
    #ifdef debug_UpdateSimJob  
    std::string pmsg("[sv4guiSimulationView::UpdateSimJob] ");
    std::cout << pmsg << "========== UpdateSimJob ==========" << std::endl;
    #endif

    if (m_JobNode == nullptr) {
        #ifdef debug_UpdateSimJob  
        std::cout << pmsg << "m_JobNode is nullptr " << std::endl;
        #endif
        return;
    }

    if (!m_MitkJob) {
        #ifdef debug_UpdateSimJob  
        std::cout << pmsg << "m_MitkJob is nullptr " << std::endl;
        #endif
        return;
    }

    #ifdef debug_UpdateSimJob  
    std::cout << pmsg << "m_JobNode: " << m_JobNode->GetName() << std::endl;
    #endif

    sv4guiSimJob* job = m_MitkJob->GetSimJob();
    std::string numProcsStr = "";

    if (job) {
      numProcsStr = job->GetRunProp("Number of Processes");
    }

    // Set the sv4guiSimJob values (key/value pairs) from
    // the GUI controls.
    //
    std::string error_msg = "";

    sv4guiSimJob* newJob = CreateSimJob(error_msg, false);

    #ifdef debug_UpdateSimJob  
    std::cout << pmsg << "CreateSimJob ... " << std::endl;
    std::cout << pmsg << "newJob: " << newJob << std::endl;
    #endif

    if (newJob == nullptr) {
        #ifdef debug_UpdateSimJob  
        std::cout << pmsg << "##############################" << std::endl;
        std::cout << pmsg << "ERROR: Update failed: " << error_msg << std::endl;
        std::cout << pmsg << "##############################" << std::endl;
        #endif
        return;
    }

    newJob->SetRunProp("Number of Processes",numProcsStr);
    m_MitkJob->SetSimJob(newJob);
    m_MitkJob->SetDataModified();
}

void sv4guiSimulationView::UdpateSimJobMeshName()
{
    if(!m_MitkJob)
        return;

    std::string meshName=ui->comboBoxMeshName->currentText().toStdString();
    m_MitkJob->SetMeshName(meshName);
    m_MitkJob->SetDataModified();
}

//----------------------
// UpdateSimJobNumProcs
//----------------------
//
void sv4guiSimulationView::UpdateSimJobNumProcs()
{
  if(!m_MitkJob) {
    return;
  }

  sv4guiSimJob* job=m_MitkJob->GetSimJob();

  if (job) {
    std::string numProcsStr=QString::number((int)(ui->sliderNumProcs->value())).toStdString();
    job->SetRunProp("Number of Processes",numProcsStr);
    m_MitkJob->SetDataModified();
  }
}

#if defined(Q_OS_WIN)
QString sv4guiSimulationView::FindLatestKey(QString key, QStringList keys)
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

QString sv4guiSimulationView::GetRegistryValue(QString category, QString key)
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

//-----------------
// UpdateJobStatus
//-----------------
// Update the the current simulation time step information in the 
// SV window bottom frame.
//
void sv4guiSimulationView::UpdateJobStatus()
{
    if (m_JobNode.IsNull()) {
        return;
    }

    bool running=false;
    double runningProgress=0;
    m_JobNode->GetBoolProperty("running",running);
    m_JobNode->GetDoubleProperty("running progress",runningProgress);

    if (running) {
        ui->labelJobStatus->setText("Running: "+QString::number((int)(runningProgress*100))+"% completed");
        ui->widgetRun->setEnabled(false);

    } else {
        ui->labelJobStatus->setText(QString::fromStdString(m_MitkJob->GetStatus()));
        ui->widgetRun->setEnabled(true);
    }
}

void sv4guiSimulationView::ShowModel(bool checked)
{
    if(m_ModelNode.IsNotNull())
    {
        m_ModelNode->SetVisibility(checked);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}


