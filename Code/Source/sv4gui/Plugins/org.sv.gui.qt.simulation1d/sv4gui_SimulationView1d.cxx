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

// The sv4guiSimulationView1d class methods defined here provide an interface to
// the 1D Simulation Tool Qt GUI widgets used to create the input files needed to 
// run a 1D simulation. The 1D solver (sv1dsolver) can be run from the GUI or from 
// the command line.
//
// The 1D Simulation Tool uses a surface model created by a Modeling Tool. The surface 
// model is used to create geometry identifying vessel centerlines, branches and
// bifurcations. The centerlines geometry is used to create a 1D finite element mesh.
//
// The input files neeed to run a simulation are created using the following steps
//
//   1) Select an inlet face from a model surface.
//
//      GUI widgets used:
//        selectModelFacesPushButton
//        m_ModelFaceSelectionWidget
//
//      Methods called:
//        SelectModelInletFaces() - Displays a table (m_ModelFaceSelectionWidget) used to select 
//                                  the inlet face name.
//        SetModelInletFaces() - Set the names and face IDs for the inlet faces selected from 
//                               the m_ModelFaceSelectionWidget.
//      Data set:
//        m_ModelInletFaceNames
//        m_ModelInletFaceIds
//        m_ModelInletFaceSelected
//        m_ModelOutletFaceNames
//        job->SetModelProp("Inlet Face Name", ...)
//
//   2) Calculate centerlines using model surface and inlet face name 
//
//      GUI widgets used:
//        CalculateCenterlinesPushButton
//
//      Methods called:
//        CalculateCenterlines()
//        SetCenterlinesGeometry()
//        UpdateCenterlines()
//        ---- 
//        sv4guiSimulationExtractCenterlines1d::Run()
//
//      Data set:
//        m_CenterlinesCalculated
//        m_CenterlinesContainer
//        m_CenterlinesFileName
//        m_CenterlinesMapper
//        m_CenterlinesNode
//
//   3) Set inlet and outlet boundary conditions 
//
//      This uses the sv4guiCapBCWidget1d class and m_TableModelCap.
//
//      GUI widgets used: 
//        m_TableModelCap - QStandardItemModel which provides a generic model for storing custom data.
//        tableViewCap
//        m_CapBCWidget - sv4guiCapBCWidget1d
//
//      Methods called:
//        CreateJob()
//        SetCapBC() - modifies m_TableModelCap
//        SetCapBcs() - job->SetCapProp()
//        ShowCapBCWidget()
//        UpdateSimJob() - called when any parameter changed
//
//      Data set:
//        m_TableModelCap->item()
//        job->SetCapProp()
//
//   4) Set solver parameters 
//
//      GUI widgets used: 
//        m_TableModelSolver - QStandardItemModel which provides a generic model for storing custom data.
//
//      Methods called:
//        CreateJob()
//        SetBasicParameters()
//        SetCapBcs() - job->SetCapProp()
//        SetSolverParameters() - job->SetSolverProp()
//        SetWallProperites()
//        UpdateGUISolver() - modifies m_TableModelSolver
//        UpdateSimJob() - called when any parameter changed
//
//      Data set:
//        job 
//        m_TableModelSolver
//
//   5) Create simulation files 
//
//      GUI widgets used: 
//        CreateSimulationFilesButton
//
//      Methods called:
//        CreateDataFiles()
//        CreateJob()
//        CreateSimulationFiles()
//        WriteOutletFaceNames()
//        WriteRcrFile()
//
//      Data set:
//        None - writes simulation files.
//

#include "sv4gui_SimulationView1d.h"
#include "ui_sv4gui_SimulationView1d.h"
#include "sv4gui_SimulationPythonConvert1d.h"

#include "sv4gui_TableCapDelegate1d.h"
#include "sv4gui_TableSolverDelegate1d.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MeshLegacyIO.h"
#include "sv4gui_SimulationUtils1d.h"

#include "sv4gui_SimulationExtractCenterlines1d.h"
#include "sv_polydatasolid_utils.h"

#include <QmitkStdMultiWidgetEditor.h>
#include <mitkNodePredicateDataType.h>
#include "mitkVtkRepresentationProperty.h"
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
#include <mitkGenericProperty.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
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

// Redefine MITK_INFO to deactivate all of the debugging statements.
#define MITK_INFO MITK_DEBUG

const QString sv4guiSimulationView1d::EXTENSION_ID = "org.sv.views.simulation1d";

// Set the title for QMessageBox warnings.
//
// Note: On MacOS the window title is ignored (as required by the Mac OS X Guidelines). 
const QString sv4guiSimulationView1d::MsgTitle = "SimVascular SV 1D Simulation";

// Set solver default name and install location.
const QString sv4guiSimulationView1d::SOLVER_EXECUTABLE_NAME = "OneDSolver";
const QString sv4guiSimulationView1d::SOLVER_INSTALL_DIRECTORY = "/usr/local/sv/oneDSolver";
const QString sv4guiSimulationView1d::SOLVER_INSTALL_SUB_DIRECTORY = "/bin";
const QString sv4guiSimulationView1d::SOLVER_LOG_FILE_NAME = "sv1dsolver.log";

// Set the names of the files written as output.
const QString sv4guiSimulationView1d::INLET_FACE_NAMES_FILE_NAME = "inlet_face_names.dat";
const QString sv4guiSimulationView1d::MESH_FILE_NAME = "mesh1d.vtp";
const QString sv4guiSimulationView1d::MODEL_SURFACE_FILE_NAME = "model_surface.vtp";
const QString sv4guiSimulationView1d::OUTLET_FACE_NAMES_FILE_NAME = "outlet_face_names.dat";
const QString sv4guiSimulationView1d::RCR_BC_FILE_NAME = "rcrt.dat";
const QString sv4guiSimulationView1d::RESISTANCE_BC_FILE_NAME = "resistance.dat";
const QString sv4guiSimulationView1d::SOLVER_FILE_NAME = "solver.in";

// Set the values of the Surface Model Origin types.
const QString sv4guiSimulationView1d::SurfaceModelSource::MESH_PLUGIN = "Mesh Plugin";
const QString sv4guiSimulationView1d::SurfaceModelSource::MODEL_PLUGIN = "Model Plugin";
const QString sv4guiSimulationView1d::SurfaceModelSource::READ_FROM_FILE = "Read from File";
const std::vector<QString> sv4guiSimulationView1d::SurfaceModelSource::types = 
{
    //sv4guiSimulationView1d::SurfaceModelSource::MESH_PLUGIN,
    sv4guiSimulationView1d::SurfaceModelSource::MODEL_PLUGIN
    //sv4guiSimulationView1d::SurfaceModelSource::READ_FROM_FILE
};

// Set the values of the Centerlines Source types.
//
// There are three sources of centerlines:
//   1) Calculate - centerlines are calculated using vmtk.
//   2) Model Plugin - centerlines are calculated using vmtk by the Model Plugin.
//   3) Read from a file - centerlines are read from a VTK .vtp file.
//
const QString sv4guiSimulationView1d::CenterlinesSource::CALCULATE = "Calculate";
const QString sv4guiSimulationView1d::CenterlinesSource::MODEL_PLUGIN = "Model Plugin";
const QString sv4guiSimulationView1d::CenterlinesSource::READ_FROM_FILE = "Read from File";
const std::vector<QString> sv4guiSimulationView1d::CenterlinesSource::types = 
{
    sv4guiSimulationView1d::CenterlinesSource::CALCULATE
    //sv4guiSimulationView1d::CenterlinesSource::MODEL_PLUGIN,
    //sv4guiSimulationView1d::CenterlinesSource::READ_FROM_FILE
};

const QString sv4guiSimulationView1d::DataInputStateName::INLET_FACE = "Inlet face name";
const QString sv4guiSimulationView1d::DataInputStateName::CENTERLINES = "Centerlines";
const QString sv4guiSimulationView1d::DataInputStateName::BOUNDRY_CONDITIONS = "Boundary conditions";
const QString sv4guiSimulationView1d::DataInputStateName::SOLVER_PARAMETERS = "Solver parameters";
const QString sv4guiSimulationView1d::DataInputStateName::SIMULATION_FILES = "Simulation files";

// Set default material model parameters.
//
const QString sv4guiSimulationView1d::MaterialModel::LinearParameters::Ehr = "1.0e7";
const QString sv4guiSimulationView1d::MaterialModel::LinearParameters::referencePressure = "0.0";
//
const QString sv4guiSimulationView1d::MaterialModel::OlufsenParameters::k1 = "0.0";
const QString sv4guiSimulationView1d::MaterialModel::OlufsenParameters::k2 = "-22.5267"; 
const QString sv4guiSimulationView1d::MaterialModel::OlufsenParameters::k3 = "1.0e7";
const QString sv4guiSimulationView1d::MaterialModel::OlufsenParameters::exponent = "1.0";
const QString sv4guiSimulationView1d::MaterialModel::OlufsenParameters::referencePressure = "0.0";

// Set material model names.
//
const QString sv4guiSimulationView1d::MaterialModel::LINEAR = "LINEAR";
const QString sv4guiSimulationView1d::MaterialModel::OLUFSEN = "OLUFSEN";
const std::vector<QString> sv4guiSimulationView1d::MaterialModel::names = 
{
   sv4guiSimulationView1d::MaterialModel::LINEAR, 
   sv4guiSimulationView1d::MaterialModel::OLUFSEN
};

// Set segment export types.
//
const QString sv4guiSimulationView1d::SegmentExportType::ALL = "All";
const QString sv4guiSimulationView1d::SegmentExportType::OUTLET = "Outlet";
const std::vector<QString> sv4guiSimulationView1d::SegmentExportType::types =
{
   sv4guiSimulationView1d::SegmentExportType::ALL,
   sv4guiSimulationView1d::SegmentExportType::OUTLET
};

// Set export data names.
//
const QString sv4guiSimulationView1d::DataExportName::AREA = "area";
const QString sv4guiSimulationView1d::DataExportName::FLOW = "flow";
const QString sv4guiSimulationView1d::DataExportName::PRESSURE = "pressure";
const QString sv4guiSimulationView1d::DataExportName::WSS = "wss";
const QString sv4guiSimulationView1d::DataExportName::RE = "Re";
const std::vector<QString> sv4guiSimulationView1d::DataExportName::names =
{
   sv4guiSimulationView1d::DataExportName::AREA,
   sv4guiSimulationView1d::DataExportName::FLOW,
   sv4guiSimulationView1d::DataExportName::PRESSURE,
   sv4guiSimulationView1d::DataExportName::RE,
   sv4guiSimulationView1d::DataExportName::WSS
};


//------------------------
// sv4guiSimulationView1d
//------------------------
//
sv4guiSimulationView1d::sv4guiSimulationView1d() : ui(new Ui::sv4guiSimulationView1d)
{
    m_MitkJob = NULL;

    m_Model = NULL;
    m_ModelFolderNode = nullptr;
    m_ModelNode = NULL;
    m_ModelInletFaceSelected = false;

    m_Mesh = nullptr;
    m_MeshFolderNode = nullptr;
    m_MeshNode = nullptr;

    m_1DMeshContainer = nullptr;
    m_1DMeshMapper = nullptr;
    m_1DMeshElementSize = 0.1;

    m_CenterlinesCalculated = false;
    m_CenterlinesContainer = nullptr;
    m_CenterlinesMapper = nullptr;
    m_CenterlinesNode = nullptr;

    m_JobNode = NULL;
    m_DataStorage = nullptr;
    m_DataInteractor=NULL;

    m_ModelSelectFaceObserverTag = -1;
    m_TableModelBasic = NULL;
    m_TableModelCap = NULL;
    m_TableMenuCap = NULL;

    m_TableModelVar = NULL;
    m_TableMenuVar = NULL;

    m_CapBCWidget = NULL;

    m_TableModelSolver=NULL;

    m_InternalPresolverPath="";
    m_InternalFlowsolverPath="";
    m_InternalFlowsolverNoMPIPath="";
    m_InternalPostsolverPath="";
    m_InternalMPIExecPath="";

    m_ExternalPresolverPath="";
    m_ExternalFlowsolverPath="";
    m_ExternalFlowsolverNoMPIPath="";
    m_UseMPI=true;
    m_UseCustom=false;
    m_SolverTemplatePath="";
    m_ExternalPostsolverPath="";
    m_ExternalMPIExecPath="";

    m_ConnectionEnabled=false;

    m_SimulationFilesCreated = false;

    m_CenterlinesSource = CenterlinesSource::CALCULATE;
}

//-------------------------
// ~sv4guiSimulationView1d
//-------------------------
//
sv4guiSimulationView1d::~sv4guiSimulationView1d()
{
    delete ui;

    if(m_TableModelBasic)
        delete m_TableModelBasic;

    if(m_TableModelCap)
        delete m_TableModelCap;

    if(m_TableMenuCap)
        delete m_TableMenuCap;

    if(m_TableModelVar)
        delete m_TableModelVar;

    if(m_TableMenuVar)
        delete m_TableMenuVar;

    if(m_TableModelSolver)
        delete m_TableModelSolver;

    if(m_CapBCWidget)
        delete m_CapBCWidget;
}

//------------------
// EnableConnection
//------------------
// [DaveP] Not sure why this is needed.
//
void sv4guiSimulationView1d::EnableConnection(bool able)
{
    auto slot = SLOT(UpdateSimJob());

    if (able && !m_ConnectionEnabled) {
        connect(m_TableModelBasic, SIGNAL(itemChanged(QStandardItem*)), this, slot);
        connect(m_TableModelCap, SIGNAL(itemChanged(QStandardItem*)), this, slot);
        connect(ui->MaterialModelComboBox,SIGNAL(currentIndexChanged(int )), this, slot);
        connect(m_TableModelSolver, SIGNAL(itemChanged(QStandardItem*)), this, slot);
        m_ConnectionEnabled = able;
    }

    if (!able && m_ConnectionEnabled) {
        disconnect(m_TableModelBasic, SIGNAL(itemChanged(QStandardItem*)), this, slot);
        disconnect(m_TableModelCap, SIGNAL(itemChanged(QStandardItem*)), this, slot);
        disconnect(ui->MaterialModelComboBox,SIGNAL(currentIndexChanged(int )), this, slot);
        disconnect(m_TableModelSolver, SIGNAL(itemChanged(QStandardItem*)), this, slot);
        m_ConnectionEnabled = able;
    }
}

//---------------------
// CreateQtPartControl
//---------------------
// Create connections between GUI events (signals) and callbacks (slots). 
//
// The m_TableModelCap widget is a QStandardItemModel object that provides a generic model 
// for storing custom data.
//
// Modifies: 
//   m_TableModelCap 
//
void sv4guiSimulationView1d::CreateQtPartControl( QWidget *parent )
{
    auto msg = "[sv4guiSimulationView1d::CreateQtPartControl] ";
    //MITK_INFO << msg << "--------- CreateQtPartControl ----------"; 
    m_Parent=parent;
    ui->setupUi(parent);

    // Hide Job Status for now, can't get it to work. 
    ui->JobStatusNameLabel->hide();
    ui->JobStatusValueLabel->hide();

    // Set the toolbox to display the first ('1D Mesh') tab.
    ui->toolBox->setCurrentIndex(0);
    connect(ui->toolBox, SIGNAL(currentChanged(const int)), this, SLOT(ToolBoxChanged(const int)) );

    // Create 1D Mesh page controls.
    Create1DMeshControls(parent);

    // For basic table.
    m_TableModelBasic = new QStandardItemModel(this);
    ui->tableViewBasic->setModel(m_TableModelBasic);

    connect(ui->tableViewBasic, SIGNAL(doubleClicked(const QModelIndex&)), this, 
      SLOT(TableViewBasicDoubleClicked(const QModelIndex&)) );

    // Inlet and Outlet BCs.
    //
    // The QStandardItemModel object provides a generic model for storing custom data.
    //
    m_TableModelCap = new QStandardItemModel(this);
    ui->tableViewCap->setModel(m_TableModelCap);
    sv4guiTableCapDelegate1d* itemDelegate = new sv4guiTableCapDelegate1d(this);
    ui->tableViewCap->setItemDelegateForColumn(1,itemDelegate);

    connect(ui->tableViewCap->selectionModel(), SIGNAL(selectionChanged(const QItemSelection&, const QItemSelection&)), 
      this, SLOT(TableCapSelectionChanged(const QItemSelection&, const QItemSelection&)));
    connect(ui->tableViewCap, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(TableViewCapDoubleClicked(const QModelIndex&)));
    connect(ui->tableViewCap, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(TableViewCapContextMenuRequested(const QPoint&)));

    m_TableMenuCap = new QMenu(ui->tableViewCap);
    QAction* setBCAction = m_TableMenuCap->addAction("Set BC");
    connect(setBCAction, SIGNAL(triggered(bool)) , this, SLOT(ShowCapBCWidget(bool)));

    /* [DaveP] I don't think we needs these.
    QAction* setPressureAction = m_TableMenuCap->addAction("Set Distal Pressure");
    connect( setPressureAction, SIGNAL( triggered(bool) ) , this, SLOT( SetDistalPressure(bool) ) );

    QAction* splitBCRAction = m_TableMenuCap->addAction("Split Resistance");
    QAction* splitBCCAction = m_TableMenuCap->addAction("Split Capacitance");
    connect( splitBCRAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowSplitBCWidgetR(bool) ) );
    connect( splitBCCAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowSplitBCWidgetC(bool) ) );
    */

    m_CapBCWidget = new sv4guiCapBCWidget1d();
    m_CapBCWidget->move(400,400);
    m_CapBCWidget->hide();
    m_CapBCWidget->setWindowFlags(Qt::WindowStaysOnTopHint);
    connect(m_CapBCWidget,SIGNAL(accepted()), this, SLOT(SetCapBC()));

    // Split Resistance BCs.
    m_SplitBCWidget = new sv4guiSplitBCWidget1d();
    m_SplitBCWidget->move(400,400);
    m_SplitBCWidget->hide();
    m_SplitBCWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_SplitBCWidget,SIGNAL(accepted()), this, SLOT(SplitCapBC()));

    // Wall properties panel. 
    CreateWallPropertiesControls(parent);

    /*
    m_TableModelVar = new QStandardItemModel(this);
    ui->tableViewVar->setModel(m_TableModelVar);

    connect(ui->tableViewVar->selectionModel(), SIGNAL(selectionChanged(const QItemSelection &, const QItemSelection&)), 
      this, SLOT(TableVarSelectionChanged(const QItemSelection&, const QItemSelection&)));

    connect( ui->tableViewVar, SIGNAL(customContextMenuRequested(const QPoint&))
             , this, SLOT(TableViewVarContextMenuRequested(const QPoint&)) );

    m_TableMenuVar=new QMenu(ui->tableViewVar);
    QAction* setVarThicknessAction=m_TableMenuVar->addAction("Set Thickness");
    QAction* setVarEAction=m_TableMenuVar->addAction("Set Elastic Modulus ");
    connect( setVarThicknessAction, SIGNAL( triggered(bool) ) , this, SLOT( SetVarThickness(bool) ) );
    connect( setVarEAction, SIGNAL( triggered(bool) ) , this, SLOT( SetVarE(bool) ) );
    */

    // Solver parameters table.
    //
    // The table format is read from resources/solvertemplate1d.xml. 
    m_TableModelSolver = new QStandardItemModel(this);
    ui->tableViewSolver->setModel(m_TableModelSolver);
    sv4guiTableSolverDelegate1d* itemSolverDelegate = new sv4guiTableSolverDelegate1d(this);
    ui->tableViewSolver->setItemDelegateForColumn(1,itemSolverDelegate);

    // Create files and Run Simulation toolbox tab. 
    connect(ui->CreateSimulationFilesButton, SIGNAL(clicked()), this, SLOT(CreateSimulationFiles()) );
    connect(ui->RunSimulationPushButton, SIGNAL(clicked()), this, SLOT(RunJob()) );
    // Disable these until the prerequisite data has be defined.
    ui->CreateSimulationFilesButton->setEnabled(false);
    ui->RunSimulationPushButton->setEnabled(false);

    // Convert Results toolbox tab.
    //
    // Create a validator that allows only valid time input.
    QDoubleValidator *validTime = new QDoubleValidator(this);
    validTime->setNotation(QDoubleValidator::ScientificNotation);
    validTime->setBottom(0.0);
    ui->lineEditStart->setValidator(validTime);
    ui->lineEditStop->setValidator(validTime);

    // Set segement export types.
    connect(ui->SegmentExportComboBox, SIGNAL(currentIndexChanged(int)), this, SLOT(SelectSegmentExportType(int)));
    for (auto const& type : SegmentExportType::types) {
        ui->SegmentExportComboBox->addItem(type);
    }
    ui->SegmentExportComboBox->setCurrentIndex(1);

    // Set data names.
    for (auto const& name : DataExportName::names) {
        ui->DataExportListWidget->addItem(name);
        //QListWidgetItem *listItem = new QListWidgetItem(name, listWidget);
        //listItem->setCheckState(Qt::Unchecked);
        //ui->DataExportListWidget->addItem(listItem);
    }
    ui->DataExportListWidget->setSelectionMode(QListWidget::MultiSelection);

    connect(ui->toolButtonResultDir, SIGNAL(clicked()), this, SLOT(SetResultDir()));
    connect(ui->toolButtonConvertDir, SIGNAL(clicked()), this, SLOT(SetConvertDir()));
    connect(ui->btnExportResults, SIGNAL(clicked()), this, SLOT(ExportResults()));

    SetupInternalSolverPaths();

    //get paths for the external solvers
    berry::IPreferences::Pointer prefs = this->GetPreferences();
    berry::IBerryPreferences* berryprefs = dynamic_cast<berry::IBerryPreferences*>(prefs.GetPointer());
    //    InitializePreferences(berryprefs);
    this->OnPreferencesChanged(berryprefs);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//----------------
// ToolBoxChanged
//----------------
// Process a toolbox tab change.
//
void sv4guiSimulationView1d::ToolBoxChanged(int index)
{
    // If changed to Convert Results then try to display convert time range.
    //
    if (index == 6) {
        auto numTimeStepsStr = m_TableModelSolver->item(TableModelSolverRow::NumberofTimesteps,1)->text().trimmed();
        auto timeStepStr = m_TableModelSolver->item(TableModelSolverRow::TimeStepSize,1)->text().trimmed();

        if (!(numTimeStepsStr.isEmpty() and timeStepStr.isEmpty())) { 
            auto numTimeSteps = std::stoi(numTimeStepsStr.toStdString());
            auto timeStep = std::stod(timeStepStr.toStdString());
            auto startTimeStr = ui->lineEditStart->text();
            auto stopTimeStr = ui->lineEditStop->text();

            // Set time range if values have not been set.
            //
            if (startTimeStr.isEmpty()) {  
                ui->lineEditStart->setText("0.0");
            }
            if (stopTimeStr.isEmpty()) { 
                auto maxTime = numTimeSteps * timeStep;
                ui->lineEditStop->setText(QString::number(maxTime));
            }
        }
    }
}

//--------------
// Update1DMesh
//--------------
// Create container and mapper used to display the 1D mesh.
//
// The mesh geometry is created by calling the 1D mesh generation
// Python script and written as VTK (.vtp) file.
//
void sv4guiSimulationView1d::Update1DMesh()
{
    auto msg = "[sv4guiSimulationView1d::Update1DMesh] ";
    //MITK_INFO << msg << "--------- Update1DMesh ----------"; 

    if ((m_1DMeshMapper == nullptr) || (m_1DMeshContainer == nullptr)) {
        m_1DMeshContainer = sv4guiSimulationLinesContainer::New();
        //MITK_INFO << msg << "Create m_1DMeshContainer";

        // Create 1D Mesh node under 'Simulations1d' node.
        auto m_1DMeshNode = mitk::DataNode::New();
        m_1DMeshNode->SetData(m_1DMeshContainer);
        m_1DMeshNode->SetVisibility(true);
        m_1DMeshNode->SetName("1D-Mesh");
        auto parentNode = GetDataStorage()->GetNamedNode("Simulations1d");
        if (parentNode) {
          GetDataStorage()->Add(m_1DMeshNode, parentNode);
        }
      
        // Create mapper to display the 1d mesh.
        m_1DMeshMapper = sv4guiSimulationLinesMapper::New();
        m_1DMeshMapper->SetDataNode(m_1DMeshNode);
        m_1DMeshMapper->m_box = false;
        m_1DMeshMapper->SetColor(1.0, 0.0, 0.0);
        m_1DMeshNode->SetMapper(mitk::BaseRenderer::Standard3D, m_1DMeshMapper);
    }

    auto meshGeometry = Read1DMesh(m_1DMeshFileName.toStdString());

    if (meshGeometry != nullptr) { 
        m_1DMeshContainer->SetMesh(meshGeometry);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

//----------------------
// Create1DMeshControls
//----------------------
// Create connections between GUI events (signals) and callbacks (slots)
// for the '1D Mesh' toolbox tab.
//
void sv4guiSimulationView1d::Create1DMeshControls(QWidget *parent)
{
    auto msg = "[sv4guiSimulationView1d::Create1DMeshControls] ";
    //MITK_INFO << msg << "--------- Create1DMeshControls ----------"; 

    // Add surface model widgets.
    //
    //connect(ui->surfaceModelComboBox, SIGNAL(currentIndexChanged(int )), this, SLOT(UpdateSurfaceModelSource( )));
    //connect(ui->ReadModelPushButton, SIGNAL(clicked()), this, SLOT(SelectModelFile()) );
    //connect(ui->comboBoxMeshName, SIGNAL(currentIndexChanged(int)), this, SLOT(UpdateSurfaceMeshName()));
    //connect(ui->showModelCheckBox, SIGNAL(clicked(bool)), this, SLOT(ShowModel(bool)) );
    ui->InletFaceNameLabel->setText("");

    for (auto const& type : SurfaceModelSource::types) {
        //ui->surfaceModelComboBox->addItem(type);
    }

    m_ModelSource = SurfaceModelSource::MODEL_PLUGIN; 
    //ui->surfaceModelComboBox->setCurrentText(m_ModelSource);
    ui->ModelNameLabel->setText("");
    // [TODO:DaveP] Maybe add read model later.
    //ui->ReadModelPushButton->setVisible(false);
    //ui->modelFileNameLabel->setVisible(false);
    //ui->modelFileNameLineEdit->setVisible(false);
    //ui->meshNameLabel->setVisible(false);
    //ui->comboBoxMeshName->setVisible(false);

    // Add centerlines widgets.
    //
    //connect(ui->centerlinesComboBox, SIGNAL(currentIndexChanged(int )), this, SLOT(UpdateCenterlinesSource()));
    //connect(ui->readCenterlinesPushButton, SIGNAL(clicked()), this, SLOT(SelectCenterlinesFile()));
    connect(ui->CalculateCenterlinesPushButton, SIGNAL(clicked()), this, SLOT(CalculateCenterlines()) );
    connect(ui->selectModelFacesPushButton, SIGNAL(clicked()), this, SLOT(SelectModelInletFaces()));
    //connect(ui->showCenterLinesCheckBox, SIGNAL(clicked(bool)), this, SLOT(ShowCenterlines(bool)) );
    for (auto const& type : CenterlinesSource::types) {
        //ui->centerlinesComboBox->addItem(type);
    }
    m_CenterlinesSource = CenterlinesSource::CALCULATE; 
    //ui->centerlinesComboBox->setCurrentText(m_CenterlinesSource);
    //ui->readCenterlinesPushButton->setVisible(false);
    //ui->centerlinesFileNameLabel->setVisible(false);
    //ui->centerlinesFileNameLineEdit->setVisible(false);
    ui->CalculateCenterlinesPushButton->setVisible(true);
    ui->CalculateCenterlinesPushButton->setEnabled(false);
    //ui->showCenterLinesCheckBox->setChecked(true);

    // Add model face selection widget.
    //
    m_ModelFaceSelectionWidget = new sv4guiCapSelectionWidget();
    m_ModelFaceSelectionWidget->move(400,400);
    m_ModelFaceSelectionWidget->hide();
    m_ModelFaceSelectionWidget->setWindowFlags(Qt::WindowStaysOnTopHint);
    // Set callback when 'OK' button is selected. 
    connect(m_ModelFaceSelectionWidget, SIGNAL(accepted()), this, SLOT(SetModelInletFaces()));

    // Generate Mesh.
    //
    //connect(ui->generateMeshPushButton, SIGNAL(clicked()), this, SLOT(Generate1DMesh()));
    //connect(ui->showMeshCheckBox, SIGNAL(clicked(bool)), this, SLOT(Show1DMesh(bool)) );
    connect(ui->ElementSizeLineEdit, SIGNAL(textChanged(QString)), this, SLOT(SetElementSize(QString)));
    // [DaveP] Hide these for now.
    //ui->generateMeshPushButton->hide();
    //ui->showMeshCheckBox->hide();

    // By default disable push buttons used to calculate centerlines, 
    // create simulation files and run a simulation.
    //
    ui->CalculateCenterlinesPushButton->setEnabled(false);
    ui->CreateSimulationFilesButton->setEnabled(false);
    ui->RunSimulationPushButton->setEnabled(false);
}

//------------------------------
// CreateWallPropertiesControls
//------------------------------
// Create connections between GUI events (signals) and callbacks (slots)
// for the 'Wall Properties' toolbox tab.
//
// [TODO:Davep] Currently only the OLUFSEN is supported.
//
void sv4guiSimulationView1d::CreateWallPropertiesControls(QWidget *parent)
{
    // Setup the material model combination box.
    //
    connect(ui->MaterialModelComboBox, SIGNAL(currentIndexChanged(int)), this, SLOT(SelectMaterialModel(int)));
    for (auto const& name : MaterialModel::names) {
        ui->MaterialModelComboBox->addItem(name);
    }
    ui->MaterialModelComboBox->setCurrentIndex(0);

    // Create a validator that allows only valid float input.
    QDoubleValidator *validDouble = new QDoubleValidator(this);
    validDouble->setNotation(QDoubleValidator::ScientificNotation);

    // Setup Linear material parameters.
    //
    auto signal = SIGNAL(returnPressed());
    auto slot = SLOT(UpdateSimJob());
    connect(ui->LinearMatProp_Ehr_LineEdit, signal, this, slot); 
    ui->LinearMatProp_Ehr_LineEdit->setText(MaterialModel::LinearParameters::Ehr);
    ui->LinearMatProp_Ehr_LineEdit->setValidator(validDouble);

    connect(ui->LinearMatProp_Pressure_LineEdit, signal, this, slot); 
    ui->LinearMatProp_Pressure_LineEdit->setText(MaterialModel::LinearParameters::referencePressure);
    ui->LinearMatProp_Pressure_LineEdit->setValidator(validDouble);

    // Setup Olufsen material parameters.
    //
    connect(ui->OlufsenMatProp_K1_LineEdit, signal, this, slot);
    ui->OlufsenMatProp_K1_LineEdit->setText(MaterialModel::OlufsenParameters::k1);
    ui->OlufsenMatProp_K1_LineEdit->setValidator(validDouble);
      
    connect(ui->OlufsenMatProp_K2_LineEdit, signal, this, slot); 
    ui->OlufsenMatProp_K2_LineEdit->setText(MaterialModel::OlufsenParameters::k2);
    ui->OlufsenMatProp_K2_LineEdit->setValidator(validDouble);
    
    connect(ui->OlufsenMatProp_K3_LineEdit, signal, this, slot);
    ui->OlufsenMatProp_K3_LineEdit->setText(MaterialModel::OlufsenParameters::k3);
    ui->OlufsenMatProp_K3_LineEdit->setValidator(validDouble);

    connect(ui->OlufsenMatProp_Exponent_LineEdit, signal, this, slot);
    ui->OlufsenMatProp_Exponent_LineEdit->setText(MaterialModel::OlufsenParameters::exponent);
    ui->OlufsenMatProp_Exponent_LineEdit->setValidator(validDouble);

    connect(ui->OlufsenMatProp_Pressure_LineEdit, signal, this, slot);
    ui->OlufsenMatProp_Pressure_LineEdit->setText(MaterialModel::OlufsenParameters::referencePressure);
    ui->OlufsenMatProp_Pressure_LineEdit->setValidator(validDouble);
}

//--------------------------
// UpdateSurfaceModelSource 
//--------------------------
// Set the source of a surface model.
//
// Modifies:
//   m_ModelSource 
//
void sv4guiSimulationView1d::UpdateSurfaceModelSource()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::UpdateSurfaceModelSource] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "---------- UpdateSurfaceModelSource ----------";

    //auto sourceType = ui->surfaceModelComboBox->currentText();
    //std::string sourceType = ui->surfaceModelComboBox->currentText().toStdString();
    //MITK_INFO << msg << "sourceType: " << sourceType;

    //auto showModel = (sourceType == SurfaceModelSource::MODEL_PLUGIN);
    //ui->ModelNameLabel->setVisible(showModel);

    //auto showRead = (sourceType == SurfaceModelSource::READ_FROM_FILE);
    // [TODO:DaveP] Maybe add read model later.
    //ui->ReadModelPushButton->setVisible(showRead);
    //ui->modelFileNameLabel->setVisible(showRead);
    //ui->modelFileNameLineEdit->setVisible(showRead);

    //auto showMesh = (sourceType == SurfaceModelSource::MESH_PLUGIN);
    //ui->meshNameLabel->setVisible(showMesh);
    //ui->comboBoxMeshName->setVisible(showMesh);

    //m_ModelSource = sourceType; 
}

//-----------------
// SelectModelFile
//-----------------
// Select a model surface file.
//
// The model surface file is in the VTK .vtp format.
//
// Sets:
//    m_ModelFileName
//
void sv4guiSimulationView1d::SelectModelFile()
{
    auto msg = "[sv4guiSimulationView1d::SelectModelFile] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "---------- SelectModelFile ----------";

    try {
        berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
        berry::IPreferences::Pointer prefs;

        if (prefService) {   
            prefs = prefService->GetSystemPreferences()->Node("/General");
        } else {
            prefs = berry::IPreferences::Pointer(0);
        }

        QString lastFilePath = "";
  
        if (prefs.IsNotNull()) {
            lastFilePath = prefs->Get("LastFileOpenPath", "");
        }
  
        if (lastFilePath == "") {
            lastFilePath=QDir::homePath();
        }
  
        m_ModelFileName = QFileDialog::getOpenFileName(NULL, tr("Import Surface Model (Choose File)"), lastFilePath,
          tr("Surface model file (*.vtp)"));
  
        m_ModelFileName = m_ModelFileName.trimmed();
  
        if (m_ModelFileName.isEmpty()) {
            return;
        }
  
        // [TODO:DaveP] Maybe add read model later.
        /*
        MITK_INFO << msg << "Read surface model: " << m_ModelFileName.toStdString();
        QFile file(m_ModelFileName);
        QFileInfo fileInfo(file);
        ui->modelFileNameLineEdit->setText(fileInfo.fileName());
        */
  
  /*
        m_SurfaceNetworkMesh = new sv4guiMesh();
        m_SurfaceNetworkMesh->ReadSurfaceFile(m_MeshFileName.toStdString());
        auto polyMesh = m_SurfaceNetworkMesh->GetSurfaceMesh();
  
        auto points = polyMesh->GetPoints();
        auto numPoints = points->GetNumberOfPoints();
        MITK_INFO << "[sv4guiPurkinjeNetworkEdit::LoadMesh] Number of points " << numPoints; 
  
        auto polygons = polyMesh->GetPolys();
        auto numPolys = polygons->GetNumberOfCells();
        MITK_INFO << "[sv4guiPurkinjeNetworkEdit::LoadMesh] Number of triangles " << numPolys; 
  
        m_MeshContainer->SetSurfaceMesh(m_SurfaceNetworkMesh);
  
        // Write mesh to project.
        QFileInfo fileInfo(m_MeshFileName);
        QString outFileName(fileInfo.fileName());
        mitk::DataNode::Pointer projFolderNode = getProjectNode();
        std::string projPath = "";
        projFolderNode->GetStringProperty("project path", projPath);
        QString QprojPath = QString(projPath.c_str());
        m_MeshOutputFileName = QprojPath + "/" + m_StoreDir + "/" + outFileName;
        MITK_INFO << "[sv4guiPurkinjeNetworkEdit::LoadMesh] m_MeshOutputFileName " <<m_MeshOutputFileName.toStdString();
        m_SurfaceNetworkMesh->WriteSurfaceFile(m_MeshOutputFileName.toStdString());
  */
}

  catch(...) {
      MITK_ERROR << "Error loading Purkinje surface mesh.!";
  }
}

//------------
// WriteModel
//------------
// Write the surface model to a VTK .vtp file.
//
void sv4guiSimulationView1d::WriteModel()
{
    auto msg = "[sv4guiSimulationView1d::WriteModel] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "---------- WriteModel ----------";

    if ((m_ModelNode == nullptr) || (m_Model == nullptr)) {
        return;
    }

    auto modelElement = m_Model->GetModelElement();
    //vtkSmartPointer<vtkPolyData> polyData = modelElement->GetWholeVtkPolyData();
    auto fileName = m_PluginOutputDirectory.toStdString() + "/" + MODEL_SURFACE_FILE_NAME.toStdString(); 
    modelElement->WriteFile(fileName);
}

//------------------------
// SelectModelInletFaces
//------------------------
// Select the inlet faces of a surface model.
//
// The model cap faces are displayed as checkable
// rows in a GUI popup table.
//
// The inlet faces are used to compute centerlines from
// the surface model.
//
void sv4guiSimulationView1d::SelectModelInletFaces(bool show)
{
    if (!m_Model) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::SelectModelFaces] ";
    //MITK_INFO << msg << "--------- SelectModelFaces ----------"; 

/*
    if (m_ModelType != "PolyData") {
      QMessageBox::warning(m_Parent,"Error","Cannot currently extract centerlines of anyting other than a PolyData model");
      return;
    }

    int timeStep=GetTimeStep();
*/

    // Create a vector of caps names from the model faces.
    //
    int timeStep = 0;
    sv4guiModelElement* modelElement = m_Model->GetModelElement(timeStep);
    std::vector<sv4guiModelElement::svFace*> faces = modelElement->GetFaces();
    std::vector<std::string> caps;
    //MITK_INFO << msg << "Number of model faces: " << faces.size();

    for (auto const& face : faces) {
        if ((face == nullptr) || (face->type != "cap")) {
            continue;
        }
        caps.push_back(face->name);
    }

    m_ModelFaceSelectionWidget->SetTableView(caps, modelElement, "PolyData");
    //m_ModelFaceSelectionWidget->SetTableView(caps, modelElement, m_ModelType);

    if (show) {
        m_ModelFaceSelectionWidget->show();
    }
}

//--------------------
// SetModelInletFaces
//--------------------
// Set the names and face IDs for the inlet faces selected from the m_ModelFaceSelectionWidget.
//
// Currently only a single inlet face may be selected.
//
// This also sets the outlet faces.
//
// Modifies:
//   m_ModelInletFaceIds
//   m_ModelInletFaceNames
//   m_ModelInletFaceSelected
//   m_ModelOutletFaceNames
//
void sv4guiSimulationView1d::SetModelInletFaces()
{
    if (!m_Model) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::SetModelInletFaces] ";
    //MITK_INFO << msg << "--------- SetModelInletFaces ----------"; 

    int timeStep = 0;
    std::vector<std::string> inletFaceNames = m_ModelFaceSelectionWidget->GetUsedCapNames();
    //MITK_INFO << msg << "Number of inlet faces selected: " << inletFaceNames.size(); 
    if (inletFaceNames.size() == 0) {
        return;
    }

    m_ModelInletFaceSelected = false;
    m_ModelInletFaceNames.clear();
    m_ModelInletFaceIds.clear();
    m_ModelOutletFaceNames.clear();
    sv4guiModelElement* modelElement = m_Model->GetModelElement(timeStep);

    for (const auto& name : inletFaceNames) {
        //MITK_INFO << msg << "Inlet face: " << name; 
        m_ModelInletFaceNames.push_back(name);
        m_ModelInletFaceIds.push_back(modelElement->GetFaceID(name));
    }

    if ( m_ModelInletFaceNames.size() > 1) {
        auto msg = "Only one inlet face may be selected.\n";
        MITK_WARN << msg; 
        QMessageBox::warning(NULL, MsgTitle, msg);
        m_ModelInletFaceNames.clear();
        m_ModelInletFaceIds.clear();
        return;
    } 
    ui->InletFaceNameLabel->setText(QString(m_ModelInletFaceNames[0].c_str()));
    m_ModelInletFaceSelected = true;
    //MITK_INFO << msg << "####### m_ModelInletFaceSelected: " << m_ModelInletFaceSelected; 

    // Set the unselected outlet faces.
    //
    std::vector<std::string> outletFaceNames = m_ModelFaceSelectionWidget->GetUnselectedCapNames();
    //MITK_INFO << msg << "Number of outlet faces not selected: " << outletFaceNames.size();
    for (const auto& name : outletFaceNames) {
        //MITK_INFO << msg << "Outlet face: " << name;
        m_ModelOutletFaceNames.push_back(name);
    }

    // Set the inlet face name in the job.
    //
    // [DaveP] This does not work because the job it not
    // written until I don't know when.
    //
    /*
    auto job = m_MitkJob->GetSimJob();
    if (job != nullptr) {
        job->SetModelProp("Inlet Face Name", m_ModelInletFaceNames[0]);
        job->Write();
    }
    */

    // Write the inlet face names to a file.
    WriteInletFaceNames(m_PluginOutputDirectory);

    // Enable execute centerlines button.
    if (!m_CenterlinesCalculated) { 
        ui->JobStatusValueLabel->setText("Centerlines have not been calculated");
    } else {
        ui->JobStatusValueLabel->setText("Simulation files have not been created");
        //MITK_INFO << msg << "Job status: " << "Simulation files have not been created"; 
    }

    // Once an inlet face has been selected then centerlines may be calculated.
    ui->CalculateCenterlinesPushButton->setEnabled(true);
}

//-----------
// ShowModel
//-----------
// Set the visibility of the surface model.
//
void sv4guiSimulationView1d::ShowModel(bool checked)
{
    if (m_ModelNode.IsNotNull()) {
        m_ModelNode->SetVisibility(checked);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

//------------
// ResetModel
//------------
// Reset data and GUI widgets when the model has changed. 
//
// The model surface has changed so centerlines must be recalculated.
//
// The surface model geometry must also be written; it can't be
// read from the Models Tool directory because that is not updated
// unless the project is manually saved.
//
void sv4guiSimulationView1d::ResetModel()
{
    auto msg = "[sv4guiSimulationView1d::ResetModel] ";
    //MITK_INFO << msg << "---------- ResetModel ---------"; 
   // MITK_INFO << msg << "m_1DMeshContainer: " << m_1DMeshContainer;

    // If the model is remeshed before centerlines are created then 
    // there will not be any m_1DMeshContainer or m_CenterlinesNode defined.
    if ((m_1DMeshContainer == nullptr) || (m_CenterlinesNode == nullptr)) {
        return;
    }

    // [DaveP] Can't figure out how to remove centerlines geometry 
    // so just hide it for now.
    //
    //m_CenterlinesContainer->DeleteMesh();
    m_CenterlinesNode->SetVisibility(false);
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    // Disable GUI buttons.
    //
    ui->CreateSimulationFilesButton->setEnabled(false);
    ui->RunSimulationPushButton->setEnabled(false);

    // Set empty centerlines file name to 
    m_CenterlinesFileName = "";
    m_CenterlinesCalculated = false;

    // Write the model surface.
    WriteModel(); 

    // Reset data state flags.
    m_CenterlinesCalculated = false;
    m_SimulationFilesCreated = false;

    QMessageBox::warning(NULL, MsgTitle, "The model has changed. Centerlines and simulation files need to be regenerated.");
}

//-------------------------
// UpdateCenterlinesSource 
//-------------------------
// Update GUI depending on centerlines source.
//
// Sets:
//   m_CenterlinesSource 
//
void sv4guiSimulationView1d::UpdateCenterlinesSource()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::UpdateCenterlinesSource] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "---------- UpdateCenterlinesSource ----------";

    //auto sourceType = ui->centerlinesComboBox->currentText();
    //MITK_INFO << msg << "sourceType: " << sourceType;
    //m_CenterlinesSource = sourceType;

    // Show or hide widgets depending on centerline source.
    //
    //auto showRead = (sourceType == CenterlinesSource::READ_FROM_FILE);
    //ui->readCenterlinesPushButton->setVisible(showRead);
    //ui->centerlinesFileNameLabel->setVisible(showRead);
    //ui->centerlinesFileNameLineEdit->setVisible(showRead);

    //auto showCalculate = (sourceType == CenterlinesSource::CALCULATE);
    //ui->selectModelFacesPushButton->setVisible(showCalculate);
    //ui->CalculateCenterlinesPushButton->setVisible(showCalculate);
    //ui->CalculateCenterlinesPushButton->setEnabled(false);
}

//------------------------
// SetCenterlinesGeometry
//------------------------
// Set the centerline geometry if it exists.
//
// If the centerline geometry file exists in the project directory 
// then read and display it.
//
// Modifies:
//   m_CenterlinesFileName 
//   m_CenterlinesCalculated 
//
void sv4guiSimulationView1d::SetCenterlinesGeometry()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::SetCenterlinesGeometry] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "---------- SetCenterlinesGeometry ----------";
    auto fileName = m_PluginOutputDirectory + "/centerlines.vtp";
    //MITK_INFO << msg << "File name: " << fileName;

    QFileInfo check_file(fileName);

    if (check_file.exists() && check_file.isFile()) {
        //MITK_INFO << msg << "Centerlines file exists.";
        m_CenterlinesFileName = fileName;
        m_CenterlinesCalculated = true;
        UpdateCenterlines();
    }
}

//----------------------
// CalculateCenterlines
//----------------------
//
void sv4guiSimulationView1d::CalculateCenterlines()
{
    auto msg = "[sv4guiSimulationView1d::CalculateCenterlines]";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "---------- CalculateCenterlines ----------";
    //MITK_INFO << msg << "Number of inlet faces: " <<  m_ModelInletFaceIds.size();

    if (!m_ModelInletFaceSelected) {
        MITK_WARN << "No inlet faces selected.";
        QMessageBox::warning(NULL, MsgTitle, "No inlet faces selected.");
        return;
    }

    // Set the name of the file to write centerlines to.
    m_CenterlinesFileName = m_PluginOutputDirectory + "/centerlines.vtp";

    // Create an object to execute the centerline extraction.
    //
    sv4guiSimulationExtractCenterlines1d* extractCenterlines = new sv4guiSimulationExtractCenterlines1d();
    extractCenterlines->SetDataStorage(this->GetDataStorage());
    extractCenterlines->SetFunctionality(this);
    extractCenterlines->SetSourceCapIds(m_ModelInletFaceIds);
    extractCenterlines->m_JobNode = m_JobNode;
    extractCenterlines->m_CenterlinesFileName = m_CenterlinesFileName; 

    // Set the Modeling data node as the source of the surface mesh.
    QList<mitk::DataNode::Pointer> selectedNode;
    selectedNode.push_back(m_ModelNode);

    // Execute the centerlines extraction.
    extractCenterlines->Run(selectedNode);
}

//-------------------
// UpdateCenterlines
//-------------------
// Read and display the centerlines geometry. 
//
// If no Centerlines Data Node is defined then create one. The centerlines geometry 
// is read from a file, defined by m_CenterlinesFileName, and used to define the 
// 1D mesh for m_CenterlinesContainer.
// 
void sv4guiSimulationView1d::UpdateCenterlines()
{
    auto msg = "[sv4guiSimulationView1d::UpdateCenterlines] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "---------- UpdateCenterlines ----------";
    //MITK_INFO << msg << "Centerlines file: " << m_CenterlinesFileName;

    if (m_JobNode == nullptr) {
        MITK_WARN << msg << "m_JobNode is null";
        return;
    }

    // Create the container and mapper used to display the centerlines.
    //
    if (m_CenterlinesMapper == nullptr) {
        m_CenterlinesContainer = sv4guiSimulationLinesContainer::New();

        // Create 'Centerlines' node under 'Simularions1d/JOB_NAME' node.
        m_CenterlinesNode = mitk::DataNode::New();
        m_CenterlinesNode->SetData(m_CenterlinesContainer);
        m_CenterlinesNode->SetVisibility(true);
        m_CenterlinesNode->SetName("Centerlines");
        GetDataStorage()->Add(m_CenterlinesNode, m_JobNode);

        // Create mapper to display the centerlines.
        m_CenterlinesMapper = sv4guiSimulationLinesMapper::New();
        m_CenterlinesMapper->SetDataNode(m_CenterlinesNode);
        m_CenterlinesMapper->m_box = false;
        m_CenterlinesMapper->SetColor(0.0, 1.0, 0.0);
        m_CenterlinesNode->SetMapper(mitk::BaseRenderer::Standard3D, m_CenterlinesMapper);
    }

    m_CenterlinesNode->SetVisibility(true);
    auto centerlinesGeometry = ReadCenterlines(m_CenterlinesFileName.toStdString());

    if (centerlinesGeometry != nullptr) { 
        m_CenterlinesContainer->SetMesh(centerlinesGeometry);
        m_CenterlinesCalculated = true;
    } else {
        m_CenterlinesCalculated = false;
    }

    // Check input data state to update GUI.
    CheckInputState();

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-----------------
// ShowCenterlines
//-----------------
// Set centerlines geometry visibility.
//
// This is the Qt widget showCenterLinesCheckBox callback.
//
void sv4guiSimulationView1d::ShowCenterlines(bool checked)
{
    if (m_CenterlinesNode == nullptr) {
        return;
    }

    m_CenterlinesNode->SetVisibility(checked);
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-----------------------
// SelectCenterlinesFile 
//-----------------------
// Select a centerlines file using a file browser.
//
// Modifies:
//   m_CenterlinesFileName 
//
void sv4guiSimulationView1d::SelectCenterlinesFile()
{
    auto msg = "[sv4guiSimulationView1d::SelectCenterlinesFile]";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "---------- SelectCenterlinesFile ----------";

    try {
        berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
        berry::IPreferences::Pointer prefs;

        if (prefService) {   
            prefs = prefService->GetSystemPreferences()->Node("/General");
        } else {
            prefs = berry::IPreferences::Pointer(0);
        }

        QString lastFilePath = "";
  
        if (prefs.IsNotNull()) {
            lastFilePath = prefs->Get("LastFileOpenPath", "");
        }
  
        if (lastFilePath == "") {
            lastFilePath=QDir::homePath();
        }
  
        m_CenterlinesFileName = QFileDialog::getOpenFileName(NULL, tr("Import Centerlines (Choose File)"), lastFilePath,
          tr("Centerlines file (*.vtp)"));
  
        m_CenterlinesFileName = m_CenterlinesFileName.trimmed();
  
        if (m_CenterlinesFileName.isEmpty()) {
            return;
        }
  
        QFile file(m_CenterlinesFileName);
        QFileInfo fileInfo(file);
        //ui->centerlinesFileNameLineEdit->setText(m_CenterlinesFileName);
        //ui->centerlinesFileNameLineEdit->setText(fileInfo.fileName());

    } catch(...) {
        MITK_ERROR << "Error loading centerlines geometry.";
        return;
    }

    // Update Centerlines Data Node with centerlines geometry.
    UpdateCenterlines();
}

//-----------------
// ReadCenterlines
//-----------------
// Read centerline geometry from a vtp file.
//
vtkSmartPointer<vtkPolyData> sv4guiSimulationView1d::ReadCenterlines(const std::string fileName)
{
    auto msg = "[sv4guiSimulationView1d::ReadCenterlines] ";
    //MITK_INFO << msg << "---------- ReadCenterlines ----------";
    //MITK_INFO << msg << "Read centerlines file: " << fileName;

    // Read centerlines.
    vtkSmartPointer<vtkPolyData> geom = vtkPolyData::New();
    //auto geom = vtkPolyData::New();

    if (PlyDtaUtils_ReadNative(const_cast<char*>(fileName.c_str()), geom) != SV_OK) {
        MITK_WARN << msg << "Unable to read centerlines from " << m_CenterlinesFileName;
        return nullptr;
    }

    return geom;
}

//----------------
// getProjectNode
//----------------
//
mitk::DataNode::Pointer sv4guiSimulationView1d::getProjectNode()
{
  if (m_DataStorage == nullptr) {
    return nullptr;
  }
  mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
  mitk::DataNode::Pointer projFolderNode = m_DataStorage->GetNode(isProjFolder);
  return projFolderNode;
}

// -----------------------
//  GetModelFolderDataNode 
// -----------------------
// Get the pointer to the model folder data node.

mitk::DataNode::Pointer sv4guiSimulationView1d::GetModelFolderDataNode()
{
  auto projFolderNode = getProjectNode();
  mitk::DataNode::Pointer modelFolderNode;
  auto modelFolderNodes = m_DataStorage->GetDerivations(projFolderNode, mitk::NodePredicateDataType::New("sv4guiModelFolder"));
  if (modelFolderNodes->size() > 0) {
    modelFolderNode = modelFolderNodes->GetElement(0);
  }

  return modelFolderNode;
}

//------------------
// GetModelFileName
//------------------
// Get the name of the file containing the model surface mesh.
//
// The model surface mesh is written into this projects job
// directory instead of being read from the Models Tool directory
// beacause it is not automatically updated if the model surface
// changes.
//
QString sv4guiSimulationView1d::GetModelFileName()
{
    auto msg = "[sv4guiSimulationView1d::GetModelFileName] ";
    //MITK_INFO << msg << "Model source '" << m_ModelSource << "'";
    QString modelFileName;

    if (m_ModelSource == SurfaceModelSource::MODEL_PLUGIN) { 
        auto modelFileName = m_PluginOutputDirectory + "/" + MODEL_SURFACE_FILE_NAME; 
        QFileInfo check_file(modelFileName );
        if (!check_file.exists() || !check_file.isFile()) {
            MITK_INFO << msg << "**** ERROR: Model file '" << modelFileName << "'does not exists.";
            return modelFileName;
        }
    }

    return modelFileName;
}

// -----------------------
//  GetMeshFolderDataNode 
// -----------------------
// Get the pointer to the mesh folder data node.

mitk::DataNode::Pointer sv4guiSimulationView1d::GetMeshFolderDataNode()
{
  auto projFolderNode = getProjectNode();
  mitk::DataNode::Pointer meshFolderNode;
  auto meshFolderNodes = m_DataStorage->GetDerivations(projFolderNode, mitk::NodePredicateDataType::New("sv4guiMeshFolder"));
  if (meshFolderNodes->size() > 0) {
    meshFolderNode = meshFolderNodes->GetElement(0);
  }
  return meshFolderNode;
}

// -----------------
//  GetDataNodeMesh
// -----------------
//
// Get the mesh from the Meshes data node.

sv4guiMesh* sv4guiSimulationView1d::GetDataNodeMesh()
{
  auto projFolderNode = getProjectNode();
  auto meshFolderNodes = m_DataStorage->GetDerivations(projFolderNode, mitk::NodePredicateDataType::New("sv4guiMeshFolder"));
  sv4guiMesh* mesh = nullptr;

  if (meshFolderNodes->size() > 0) {
    m_MeshFolderNode = meshFolderNodes->GetElement(0);
    auto meshNodes = GetDataStorage()->GetDerivations(m_MeshFolderNode);
    for (auto it = meshNodes->begin(); it != meshNodes->end(); ++it) {
      sv4guiMitkMesh* mitkMesh = dynamic_cast<sv4guiMitkMesh*>((*it)->GetData());
      if (mitkMesh) {
        mesh = mitkMesh->GetMesh();
        break;
      }
    }
  }

  return mesh;
}

//----------------
// Generate1DMesh
//----------------
//
// python generate-1d-mesh.py 
//     --output-directory <dir>  
//     --centerlines-input-file <clFile.vtp> 
//     --compute-mesh 
//     --write-mesh-file 
//     --mesh-output-file <meshFile>.vtp
//
void sv4guiSimulationView1d::Generate1DMesh()
{
    auto msg = "[sv4guiSimulationView1d::Generate1DMesh] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "---------- Generate1DMesh ----------";

    if (m_Model == nullptr) { 
        return;
    }

    //MITK_INFO << msg << "Output directory: " << m_PluginOutputDirectory;

    // Get the file name of the surface model.
    auto modelFileName = m_ModelFileName;
    //MITK_INFO << msg << "Model file: " << modelFileName;

    if (!m_CenterlinesCalculated) {
        QMessageBox::warning(NULL, MsgTitle, "No centerlines have been calculated or centerlines source file set.");
        MITK_ERROR << "No centerlines file is defined.";
        return;
    }
    //MITK_INFO << msg << "Centerlines file: " << m_CenterlinesFileName;

    auto outputDirectory = m_PluginOutputDirectory.toStdString();
    auto inputCenterlinesFile = m_CenterlinesFileName.toStdString();
    auto meshFileName = MESH_FILE_NAME.toStdString();

    // Execute the generate-1d-mesh.py script.
    auto pythonInterface = sv4guiSimulationPython1d();
    pythonInterface.GenerateMesh(outputDirectory, inputCenterlinesFile, meshFileName);

    auto fileName = outputDirectory + "/" + meshFileName;
    m_1DMeshFileName = m_PluginOutputDirectory + "/" + MESH_FILE_NAME; 

    // Read and display the 1D mesh.
    Update1DMesh();

/*
    MITK_INFO << msg << "Mesh file: " << fileName;
    auto meshGeometry = Read1DMesh(fileName);
    if (meshGeometry != nullptr) { 
        m_1DMeshContainer->SetMesh(meshGeometry);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();

        //if (ui->networkCheckBox->isChecked()) {
          //showNetwork(true);
        //} else {
          //showNetwork(false);
        //}
    }
*/

}

//------------
// Show1DMesh
//------------
//
void sv4guiSimulationView1d::Show1DMesh()
{
    auto msg = "[sv4guiSimulationView1d::Show1DMesh] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "---------- Show1DMesh ----------";

    if (m_Model == nullptr) { 
        return;
    }
}

//----------------
// SetElementSize
//----------------
//
void sv4guiSimulationView1d::SetElementSize(QString valueArg)
{
    std::string value = valueArg.toStdString();
    if (!IsDouble(value)) {
        QMessageBox::warning(m_Parent, MsgTitle, "The element size is not a float value."); 
        return;
    }
    m_1DMeshElementSize = std::stod(value);
}

//------------
// Read1DMesh
//------------
// Read a 1D mesh into a vtkPolyData object and
// display it.
//
vtkSmartPointer<vtkPolyData> sv4guiSimulationView1d::Read1DMesh(const std::string fileName)
{
    auto msg = "[sv4guiSimulationView1d::Read1DMesh] ";
    //MITK_INFO << msg; 
    //MITK_INFO << msg << "---------- Read1DMesh ---------- "; 
    //MITK_INFO << msg << "Read mesh file name: " << fileName;

    // Read 1D mesh into vtkPolyData object.
    //
    vtkSmartPointer<vtkPolyData> geom = vtkPolyData::New();

    if (PlyDtaUtils_ReadNative(const_cast<char *>(fileName.c_str()), geom) != SV_OK) {
        MITK_WARN << msg << "Unable to read 1D mesh " << fileName;
        return nullptr;
    }
    //MITK_INFO << msg << "Done! ";
    return geom;
}

//----------
// ReadMesh
//-----------
//
void sv4guiSimulationView1d::ReadMesh()
{   
    auto msg = "[sv4guiSimulationView1d::ReadMesh] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "--------- ReadMesh ----------";
}


//--------------------------
// SetupInternalSolverPaths
//--------------------------
// Set the path to the 1D solver.
//
void sv4guiSimulationView1d::SetupInternalSolverPaths()
{
    //get path for the internal solvers
    QString solverPath="/usr/local/sv/svsolver";
    QStringList dirList=QDir(solverPath).entryList(QDir::Dirs|QDir::NoDotAndDotDot|QDir::NoSymLinks,QDir::Name);
    if(dirList.size()!=0)
        solverPath+="/"+dirList.back();

    QString solverPathBin=solverPath+"/bin";

    QString applicationPath=QCoreApplication::applicationDirPath();
    QString svpreName="/svpre";
    QString svsolverName="/svsolver";
    QString svsolverNoMPIName="/svsolver-nompi";
    QString svpostName="/svpost";

    m_InternalMPIExecPath="mpiexec";

    QString filePath="";

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)
    //flowsolver with mpi, prefer to the script one which sets some lib paths for the mpi libs from svsolver
    //Those libs are needed in Ubuntu 16, intead of using the system ones
    if(QFile(filePath=solverPathBin+"/.."+svsolverName).exists())
        m_InternalFlowsolverPath=filePath;
    else if(QFile(filePath=solverPathBin+svsolverName).exists())
        m_InternalFlowsolverPath=filePath;
    else if(QFile(filePath=applicationPath+"/.."+svsolverName).exists())
        m_InternalFlowsolverPath=filePath;
    else if(QFile(filePath=applicationPath+svsolverName).exists())
        m_InternalFlowsolverPath=filePath;

    //svpost
    if(QFile(filePath=solverPathBin+svpostName).exists())
        m_InternalPostsolverPath=filePath;
    else if(QFile(filePath=solverPathBin+"/.."+svpostName).exists())
        m_InternalPostsolverPath=filePath;
    else if(QFile(filePath=applicationPath+svpostName).exists())
        m_InternalPostsolverPath=filePath;
    else if(QFile(filePath=applicationPath+"/.."+svpostName).exists())
        m_InternalPostsolverPath=filePath;
#endif

#if defined(Q_OS_LINUX)
    //svpre
    if(QFile(filePath=solverPathBin+svpreName).exists())
        m_InternalPresolverPath=filePath;
    else if(QFile(filePath=solverPathBin+"/.."+svpreName).exists())
        m_InternalPresolverPath=filePath;
    else if(QFile(filePath=applicationPath+svpreName).exists())
        m_InternalPresolverPath=filePath;
    else if(QFile(filePath=applicationPath+"/.."+svpreName).exists())
        m_InternalPresolverPath=filePath;

    //flowsolver with no mpi
    if(QFile(filePath=solverPathBin+svsolverNoMPIName).exists())
        m_InternalFlowsolverNoMPIPath=filePath;
    else if(QFile(filePath=solverPathBin+"/.."+svsolverNoMPIName).exists())
        m_InternalFlowsolverNoMPIPath=filePath;
    else if(QFile(filePath=applicationPath+svsolverNoMPIName).exists())
        m_InternalFlowsolverNoMPIPath=filePath;
    else if(QFile(filePath=applicationPath+"/.."+svsolverNoMPIName).exists())
        m_InternalFlowsolverNoMPIPath=filePath;

    //mpiexec
    //user the system one; issue happens if use the one from svsolver or application in Ubuntu 16
#endif

#if defined(Q_OS_MAC)
    //svpre
    if(QFile(filePath=solverPathBin+"/.."+svpreName).exists())
        m_InternalPresolverPath=filePath;
    else if(QFile(filePath=solverPathBin+svpreName).exists())
        m_InternalPresolverPath=filePath;
    else if(QFile(filePath=applicationPath+"/.."+svpreName).exists())
        m_InternalPresolverPath=filePath;
    else if(QFile(filePath=applicationPath+svpreName).exists())
        m_InternalPresolverPath=filePath;

    //flowsolver with no mpi
    if(QFile(filePath=solverPathBin+"/.."+svsolverNoMPIName).exists())
        m_InternalFlowsolverNoMPIPath=filePath;
    else if(QFile(filePath=solverPathBin+svsolverNoMPIName).exists())
        m_InternalFlowsolverNoMPIPath=filePath;
    else if(QFile(filePath=applicationPath+"/.."+svsolverNoMPIName).exists())
        m_InternalFlowsolverNoMPIPath=filePath;
    else if(QFile(filePath=applicationPath+svsolverNoMPIName).exists())
        m_InternalFlowsolverNoMPIPath=filePath;

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
    m_InternalPresolverPath=GetRegistryValue("SimVascular\\svSolver","SVPRE_EXE");
    m_InternalFlowsolverPath=GetRegistryValue("SimVascular\\svSolver","SVSOLVER_MSMPI_EXE");
    m_InternalFlowsolverNoMPIPath=GetRegistryValue("SimVascular\\svSolver","SVSOLVER_NOMPI_EXE");
    m_InternalPostsolverPath=GetRegistryValue("SimVascular\\svSolver","SVPOST_EXE");
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

//----------------------
// OnPreferencesChanged
//----------------------
//
void sv4guiSimulationView1d::OnPreferencesChanged(const berry::IBerryPreferences* prefs)
{
    if(prefs==NULL)
        return;

    m_ExternalPresolverPath=prefs->Get("presolver path","");
    m_ExternalFlowsolverPath=prefs->Get("flowsolver path","");
    m_UseMPI=prefs->GetBool("use mpi", true);
    m_ExternalMPIExecPath=prefs->Get("mpiexec path","");
    m_UseCustom=prefs->GetBool("use custom", false);
    m_SolverTemplatePath=prefs->Get("solver template path","");
    m_ExternalPostsolverPath=prefs->Get("postsolver path","");
}

//--------------------
// OnSelectionChanged
//--------------------
// Process a change in the Data Manager nodes.
//
// Sets:  
//    m_ModelFileName if it is not already set.
//    m_ModelNode 
//    m_Model 
//    m_JobNode 
//    m_MitkJob 
//
void sv4guiSimulationView1d::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
    auto msg = "[sv4guiSimulationView1d::OnSelectionChanged] ";
    MITK_INFO << msg;
    MITK_INFO << msg << "--------- OnSelectionChanged ----------";
    MITK_INFO << msg << "nodes.size() " << nodes.size();

    if (!IsVisible()) {
        return;
    }

    if (nodes.size() == 0) {
        RemoveObservers();
        EnableTool(false);
        m_Parent->setEnabled(false);
        return;
    }

    m_DataStorage = GetDataStorage();
    if (m_DataStorage == nullptr) {
        MITK_INFO << msg << " m_DataStorage == nullptr";
        return;
    }

    // Check that a job nodes exists.
    mitk::DataNode::Pointer jobNode = nodes.front();
    sv4guiMitkSimJob1d* mitkJob = dynamic_cast<sv4guiMitkSimJob1d*>(jobNode->GetData());
    if (!mitkJob) {
        RemoveObservers();
        EnableTool(false);
        m_Parent->setEnabled(false);
        MITK_INFO << msg << " mitkJob == nullptr";
        return;
    }

    if (m_JobNode.IsNotNull()) {
        RemoveObservers();
    }

    m_JobNode = jobNode;
    m_MitkJob = mitkJob;
    m_Parent->setEnabled(true);

    // Set the plugin output directory.
    m_PluginOutputDirectory = GetJobPath();
    MITK_INFO << msg << "Set m_PluginOutputDirectory to: " << m_PluginOutputDirectory;
    if (!QDir(m_PluginOutputDirectory).exists()) {
        QDir().mkdir(m_PluginOutputDirectory);
    }
    
    // Get the model and mesh folder data nodes.
    m_ModelFolderNode = GetModelFolderDataNode();
    m_MeshFolderNode = GetMeshFolderDataNode();

    // Get the model name (set when we create a 1d simulation).
    //
    std::string modelName = mitkJob->GetModelName();
    MITK_INFO << msg << "Model name '" << modelName << "'";

    // Set the model node. 
    //
    auto modelNode = m_DataStorage->GetNamedDerivedNode(modelName.c_str(), m_ModelFolderNode);
    auto model = dynamic_cast<sv4guiModel*>(modelNode->GetData());
    auto resetModel = false;

    if (model != nullptr) {
        //modelNode->SetVisibility(false);

        // [DaveP] I assume that m_ModelNode cannot change but 
        // not sure about this
        //
        if (m_ModelNode == nullptr) {
            m_ModelNode = modelNode;
            m_Model = model;
            WriteModel();
        }
        MITK_INFO << msg << "Model node: " << modelNode; 

        // [DaveP] can't get time the model node's data was last modified.
        //auto lastTimeModified = m_ModelNode->GetDataReferenceChangedTime();
        //auto lastTimeModified = m_ModelNode->GetMTime();
        // auto modeified = m_Model->GetDataModified();  // does not exist.
        //MITK_INFO << msg << "#### The last time the model has been modified: " << lastTimeModified;
        std::string timeModified;
        m_ModelNode->GetStringProperty("time modified", timeModified);
        MITK_INFO << msg;
        MITK_INFO << msg << "#############################";
        MITK_INFO << msg << "Model time modified: " << timeModified; 
        MITK_INFO << msg << "m_ModelNodeTimeModified: " << m_ModelNodeTimeModified; 
        MITK_INFO << msg << "#############################";

        // The model has changed so reset the data the depends on the surface model. 
        if ((timeModified != "") && (timeModified != m_ModelNodeTimeModified)) {
            resetModel = true;
        }
        m_ModelNodeTimeModified = timeModified;

        if (m_ModelFileName.isEmpty()) {
            m_ModelFileName = GetModelFileName();
        }
        MITK_INFO << msg << "The model has been set.";
        // Check for centerlines created for the model.
        auto rs = GetDataStorage()->GetDerivations(modelNode);
        if (rs->size() > 0) {
            MITK_INFO << msg << "Have centerlines:";
            m_ModelCenterlineNodes.clear();
            for (auto const& node : *rs) { 
                auto name = node->GetName();
                MITK_INFO << msg << name;
                m_ModelCenterlineNodes.emplace_back(name, node);
            }
        } else { 
            MITK_INFO << msg << "Don't have centerlines from Model Tool.";
        }
    } else {
        MITK_WARN << msg << "No model has been created!";
    }

    // Set the mesh node. 
    //
    auto meshNodes = m_DataStorage->GetDerivations(m_MeshFolderNode,mitk::NodePredicateDataType::New("sv4guiMitkMesh"));
    if (meshNodes->size() != 0) {
        m_MeshNodes.clear();
        MITK_INFO << msg << "Mesh names: ";
        for (auto const& node : *meshNodes) {
            auto name = node->GetName();
            MITK_INFO << msg << name; 
            m_MeshNodes.emplace_back(name, node);
        }
    } else {
      MITK_WARN << msg << "Mesh data node not found!";
    }

    // Enable the toolbox pages ('1D Mesh', 'Basic Parameters', etc.) to allow input.
    //
    if (m_Model == NULL) {
        EnableTool(false);
    } else {
        EnableTool(true);
        AddObservers();
    }

    SetCenterlinesGeometry();

    // Update main GUI panel upper section.
    //
    ui->labelJobName->setText(QString::fromStdString(m_JobNode->GetName()));
    ui->JobStatusValueLabel->setText(QString::fromStdString(m_MitkJob->GetStatus()));
    //ui->showModelCheckBox->setChecked(true);
    if(m_ModelNode.IsNotNull()) {
        m_ModelNode->SetProperty("material.representation", mitk::VtkRepresentationProperty::New(VTK_WIREFRAME));
        ui->ModelNameLabel->setText(QString::fromStdString(m_ModelNode->GetName()));
        if (m_ModelNode->IsVisible(NULL)) {
            //ui->showModelCheckBox->setChecked(true);
        }
    } else {
        ui->ModelNameLabel->setText("No model found");
    }

    EnableConnection(false);

    UpdateModelGUI();

    UpdateGUIBasic();

    UpdateGUICap();

    UpdateGUIWall();

    UpdateGUISolver();

    UpdateGUIJob();

    UpdateGUIRunDir();

    UpdateFaceListSelection();

    UpdateJobStatus();

    EnableConnection(true);

    if (resetModel) { 
        ResetModel();
    }

    // Check the state of the input data.
    CheckInputState();

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-------------
// NodeChanged
//-------------
//
void sv4guiSimulationView1d::NodeChanged(const mitk::DataNode* node)
{
    if(m_JobNode.IsNotNull() && m_JobNode==node)
    {
        ui->labelJobName->setText(QString::fromStdString(m_JobNode->GetName()));
        UpdateJobStatus();

        bool updateRunDir=false;
        m_JobNode->GetBoolProperty("update rundir",updateRunDir);
        if(updateRunDir) {
            UpdateGUIRunDir();
            m_JobNode->SetBoolProperty("update rundir",false);
        }

    }
}

void sv4guiSimulationView1d::NodeAdded(const mitk::DataNode* node)
{

}

void sv4guiSimulationView1d::NodeRemoved(const mitk::DataNode* node)
{

}

void sv4guiSimulationView1d::Visible()
{
    OnSelectionChanged(GetDataManagerSelection());
}

void sv4guiSimulationView1d::Hidden()
{
    RemoveObservers();
}

//--------------
// AddObservers
//--------------
//
// Enable notification of Model changes.
//
void sv4guiSimulationView1d::AddObservers()
{
    auto msg = "[sv4guiSimulationView1d::AddObservers] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "--------- AddObservers ----------";

    if(m_ModelNode.IsNotNull()) {
        if(m_ModelNode->GetDataInteractor().IsNull()) {
            m_DataInteractor = sv4guiModelDataInteractor::New();
            m_DataInteractor->LoadStateMachine("sv4gui_ModelInteraction.xml", us::ModuleRegistry::GetModule("sv4guiModuleModel"));
            m_DataInteractor->SetEventConfig("sv4gui_ModelConfig.xml", us::ModuleRegistry::GetModule("sv4guiModuleModel"));
            m_DataInteractor->SetDataNode(m_ModelNode);
        }

        m_ModelNode->SetStringProperty("interactor user", "simulation1d");
        sv4guiModelDataInteractor* interactor = dynamic_cast<sv4guiModelDataInteractor*>(m_ModelNode->GetDataInteractor().GetPointer());
        if (interactor) {
            interactor->SetFaceSelectionOnly();
        }
    }

    if (m_Model && m_ModelSelectFaceObserverTag == -1) {
        itk::SimpleMemberCommand<sv4guiSimulationView1d>::Pointer modelSelectFaceCommand = 
          itk::SimpleMemberCommand<sv4guiSimulationView1d>::New();
        modelSelectFaceCommand->SetCallbackFunction(this, &sv4guiSimulationView1d::UpdateFaceListSelection);
        m_ModelSelectFaceObserverTag = m_Model->AddObserver( sv4guiModelSelectFaceEvent(), modelSelectFaceCommand);
    }
}

//-----------------
// RemoveObservers
//-----------------
//
void sv4guiSimulationView1d::RemoveObservers()
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
        if(user=="simulation1d")
            m_ModelNode->SetDataInteractor(NULL);
    }
    m_DataInteractor=NULL;
}

void sv4guiSimulationView1d::ClearAll()
{
    m_Model=NULL;
    m_JobNode=NULL;
    m_MitkJob=NULL;
    m_ModelNode=NULL;

    ui->labelJobName->setText("");
    ui->JobStatusValueLabel->setText("");
    ui->ModelNameLabel->setText("");
}

//----------------
// UpdateModelGUI
//----------------
// Update the surface model GUI.
//
// If an inlet face has been previously set and stored for the job
// (in the .s1djb file) then set that name in the checkable
// rows of the GUI popup table m_ModelFaceSelectionWidget.
//
void sv4guiSimulationView1d::UpdateModelGUI()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::UpdateModelGUI] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "--------- UpdateModelGUI ----------";

    /* [DaveP] This does not work well.
    sv4guiSimJob1d* job = m_MitkJob->GetSimJob();
    if (job == NULL) {
        job = new sv4guiSimJob1d();
    }
    auto inletFaceName = job->GetModelProp("Inlet Face Name");
    MITK_INFO << msg << "inletFaceName: " << inletFaceName;
    */

    auto inletFaceNames = ReadInletFaceNames(m_PluginOutputDirectory);

    if (inletFaceNames.size() == 0) { 
        return;
    }

    // Set cap face names for the m_ModelFaceSelectionWidget.
    auto show = false;
    SelectModelInletFaces(show);

    // Select the inlet face programmatically. 
    std::set<std::string> capNames = { inletFaceNames[0] }; 
    m_ModelFaceSelectionWidget->SetUsedCapNames(capNames);

    // Set the face name for this object from name set in m_ModelFaceSelectionWidget.
    SetModelInletFaces();
}

//----------------
// UpdateGUIBasic
//----------------
// Update the 'Basic Paramaters' GUI page.
//
void sv4guiSimulationView1d::UpdateGUIBasic()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::UpdateGUIBasic] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "--------- UpdateGUIBasic ----------";

    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();

    if (job == NULL) {
        job = new sv4guiSimJob1d();
    }

    m_TableModelBasic->clear();

    QStringList basicHeaders;
    basicHeaders << "Parameter" << "Value";
    m_TableModelBasic->setHorizontalHeaderLabels(basicHeaders);
    m_TableModelBasic->setColumnCount(2);

    QList<QStandardItem*> parList;
    QList<QStandardItem*> valueList;
    QString value;

    parList<<new QStandardItem("Fluid Density");
    value=QString::fromStdString(job->GetBasicProp("Fluid Density"));
    valueList<<new QStandardItem(value==""?QString("1.06"):value);

    parList<<new QStandardItem("Fluid Viscosity");
    value=QString::fromStdString(job->GetBasicProp("Fluid Viscosity"));
    valueList<<new QStandardItem(value==""?QString("0.04"):value);

//    parList<<new QStandardItem("Period");
//    value=QString::fromStdString(job->GetBasicProp("Period"));
//    valueList<<new QStandardItem(value==""?QString("1.0"):value);

    /* [TODO:DaveP] Maybe add these later.
    parList<<new QStandardItem("IC File");
    value=QString::fromStdString(job->GetBasicProp("IC File"));
    valueList<<new QStandardItem(value);

    parList<<new QStandardItem("Initial Pressure");
    value=QString::fromStdString(job->GetBasicProp("Initial Pressure"));
    valueList<<new QStandardItem(value==""?QString("0"):value);

    parList<<new QStandardItem("Initial Velocities");
    value=QString::fromStdString(job->GetBasicProp("Initial Velocities"));
    valueList<<new QStandardItem(value==""?QString("0.0001 0.0001 0.0001"):value);
    */

    for(int i=0;i<parList.size();i++) {
        parList[i]->setEditable(false);
        m_TableModelBasic->setItem(i, 0, parList[i]);
        m_TableModelBasic->setItem(i, 1, valueList[i]);
    }

    ui->tableViewBasic->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->tableViewBasic->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
}

//-----------------------------
// TableViewBasicDoubleClicked
//-----------------------------
//
void sv4guiSimulationView1d::TableViewBasicDoubleClicked(const QModelIndex& index)
{
    if(index.column()!=0)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewBasic->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1) {
        return;
    }

    int row=indexesOfSelectedRows[0].row();
    QStandardItem* itemName= m_TableModelBasic->item(row,0);
    if(itemName->text()!="IC File")
        return;

    QStandardItem* itemValue= m_TableModelBasic->item(row,1);
    QString lastFileOpenPath=itemValue->text().trimmed();

    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer prefs;
    if (prefService)
    {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    }
    else
    {
        prefs = berry::IPreferences::Pointer(0);
    }

    if(lastFileOpenPath=="" || !QFile(lastFileOpenPath).exists())
    {
        if(prefs.IsNotNull())
        {
            lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
        }
        if(lastFileOpenPath=="")
            lastFileOpenPath=QDir::homePath();
    }

    QString icFilePath = QFileDialog::getOpenFileName(m_Parent, tr("Select IC File (Restart)")
                                                            , lastFileOpenPath
                                                            , tr("All Files (*)"));

    icFilePath=icFilePath.trimmed();
    if (icFilePath.isEmpty())
        return;

    if(prefs.IsNotNull())
     {
         prefs->Put("LastFileOpenPath", icFilePath);
         prefs->Flush();
     }

    itemValue->setText(icFilePath);
}

//-------------------------
// UpdateFaceListSelection
//-------------------------
//
// Update the 'Inlet and Outlet BCs' face table when the faces for m_Model have 
// been modified in the Model plugin. 
//
void sv4guiSimulationView1d::UpdateFaceListSelection()
{
    auto msg = "[sv4guiSimulationView1d::UpdateFaceListSelection] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "--------- UpdateFaceListSelection ----------";

    if (!m_Model) {
        return;
    }

    sv4guiModelElement* modelElement = m_Model->GetModelElement();

    if (modelElement == NULL) {
        return;
    }

    // Update the tableViewCap GUI object for inlet/outlet BCs.
    //
    disconnect(ui->tableViewCap->selectionModel(), SIGNAL(selectionChanged(const QItemSelection&, const QItemSelection&)), this, 
      SLOT(TableCapSelectionChanged(const QItemSelection &, const QItemSelection &)));

    ui->tableViewCap->clearSelection();

    int count = m_TableModelCap->rowCount();

    for (int i = 0; i < count; i++) {
        QStandardItem* itemName = m_TableModelCap->item(i,0);
        std::string name = itemName->text().toStdString();

        if(modelElement->IsFaceSelected(name)) {
            //MITK_INFO << msg << "Face is selected " << name;
            QModelIndex mIndex=m_TableModelCap->index(i,1);
            ui->tableViewCap->selectionModel()->select(mIndex, QItemSelectionModel::Select|QItemSelectionModel::Rows);
        }
    }

    connect( ui->tableViewCap->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableCapSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );


    // Update tableViewVar, GUI Wall Properties / Variable Properties.
    //
/*
    disconnect( ui->tableViewVar->selectionModel()
                , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
                , this
                , SLOT( TableVarSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    ui->tableViewVar->clearSelection();
    count=m_TableModelVar->rowCount();

    for(int i=0;i<count;i++) {
        QStandardItem* itemName= m_TableModelVar->item(i,0);
        std::string name=itemName->text().toStdString();

        if(modelElement->IsFaceSelected(name)) {
            QModelIndex mIndex=m_TableModelVar->index(i,1);
            ui->tableViewVar->selectionModel()->select(mIndex, QItemSelectionModel::Select|QItemSelectionModel::Rows);
        }
    }

    connect( ui->tableViewVar->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableVarSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );
*/

}

//--------------------------
// TableCapSelectionChanged
//--------------------------
//
void sv4guiSimulationView1d::TableCapSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    auto msg = "[sv4guiSimulationView1d::TableCapSelectionChanged] ";
    //MITK_INFO << msg << "------------------- TableCapSelectionChanged ----------";

    mitk::StatusBar::GetInstance()->DisplayText("");

    if (!m_Model) {
        return;
    }

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin(); it != indexesOfSelectedRows.end(); it++) {
        int row=(*it).row();
        std::string name= m_TableModelCap->item(row,0)->text().toStdString();
        modelElement->SelectFace(name);

        if(it==indexesOfSelectedRows.begin()){
            double faceArea=modelElement->GetFaceArea(modelElement->GetFaceID(name));
            QString info="Face "+QString::fromStdString(name)+": Area="+QString::number(faceArea);
            mitk::StatusBar::GetInstance()->DisplayText(info.toStdString().c_str());
        }

    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//---------------------------
// TableViewCapDoubleClicked
//---------------------------
//
void sv4guiSimulationView1d::TableViewCapDoubleClicked(const QModelIndex& index)
{
    auto msg = "[sv4guiSimulationView1d::TableViewCapDoubleClicked] ";
    //MITK_INFO << msg << "--------- TableViewCapDoubleClicked ----------";

    if (index.column()==0) {
        ShowCapBCWidget();
    }
}

void sv4guiSimulationView1d::TableViewCapContextMenuRequested( const QPoint & pos )
{
    m_TableMenuCap->popup(QCursor::pos());
}

//-----------------
// ShowCapBCWidget
//-----------------
// Show the m_CapBCWidget (sv4guiCapBCWidget1d) popup.
//
// Sets values stored in m_TableModelCap.
//
void sv4guiSimulationView1d::ShowCapBCWidget(bool)
{
    auto msg = "[sv4guiSimulationView1d::ShowCapBCWidget] ";
    //MITK_INFO << msg << "--------- ShowCapBCWidget ----------";
    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();

    if(indexesOfSelectedRows.size() < 1) {
        return;
    }

    std::map<std::string,std::string> props;
    std::string capName;
    int row = indexesOfSelectedRows[0].row();

    if(indexesOfSelectedRows.size() == 1) {
        capName=m_TableModelCap->item(row,0)->text().toStdString();
    } else {
        capName="multiple faces";
    }

    props["BC Type"] = m_TableModelCap->item(row,1)->text().toStdString();
    props["Values"] = m_TableModelCap->item(row,2)->text().toStdString();
    props["Pressure"] = m_TableModelCap->item(row,3)->text().toStdString();
    props["Analytic Shape"] = m_TableModelCap->item(row,4)->text().toStdString();
    props["Period"] = m_TableModelCap->item(row,5)->text().toStdString();
    props["Point Number"] = m_TableModelCap->item(row,6)->text().toStdString();
    props["Fourier Modes"] = m_TableModelCap->item(row,7)->text().toStdString();
    props["Flip Normal"] = m_TableModelCap->item(row,8)->text().toStdString();
    props["Flow Rate"] = m_TableModelCap->item(row,9)->text().toStdString();
    props["File"] = m_TableModelCap->item(row,10)->text().toStdString();
    props["Original File"] = m_TableModelCap->item(row,10)->text().toStdString();
    props["Timed Pressure"] = m_TableModelCap->item(row,11)->text().toStdString();
    props["Pressure Period"] = m_TableModelCap->item(row,12)->text().toStdString();
    props["Pressure Scaling"] = m_TableModelCap->item(row,13)->text().toStdString();
    props["R Values"] = m_TableModelCap->item(row,14)->text().toStdString();
    props["C Values"] = m_TableModelCap->item(row,15)->text().toStdString();

    //MITK_INFO << msg << "capName: " << capName;
    //MITK_INFO << msg << "props[Flow Rate]: " << props["Flow Rate"];

    m_CapBCWidget->UpdateGUI(capName,props);

    m_CapBCWidget->show();
}

void sv4guiSimulationView1d::SetDistalPressure(bool)
{
    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    bool ok=false;
    double pressure=QInputDialog::getDouble(m_Parent, "Set Distal Pressure", "Distal Pressure:", 0.0, 0, 1000000, 2, &ok);
    QString str=QString::number(pressure);

    if(!ok)
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemBCType= m_TableModelCap->item(row,1);
        if(itemBCType->text()!="" && itemBCType->text()!="Prescribed Velocities")
        {
            QStandardItem* itemPressure= m_TableModelCap->item(row,3);
            itemPressure->setText(str);
        }
    }
}

//----------
// SetCapBC
//----------
// Set the GUI values for inlet / outler (cap) boundary conditions
// table in the 'Inlet and Outlet BCs' toolbox tab.
//
// The GUI values are updated from m_CapBCWidget properties.
//
// This is called when values in the 'Set Inlet/Outlet BCs' popup values 
// are changed (sv4guiCapBCWidget1d class).
//
// Modifies:
//   m_TableModelCap
//
void  sv4guiSimulationView1d::SetCapBC()
{
    auto msg = "[sv4guiSimulationView1d::SetCapBC] ";
    //MITK_INFO << msg << "--------- SetCapBC ----------"; 
    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();

    if (indexesOfSelectedRows.size() < 1) {
        return;
    }

    std::map<std::string, std::string> props = m_CapBCWidget->GetProps();
    //MITK_INFO << msg << "size of props: " << props.size();
    //MITK_INFO << msg << "props[BC Type]: " << props["BC Type"];
    //MITK_INFO << msg << "################# flow rate: " << QString::fromStdString(props["Flow Rate"]);

    for (auto const& item : indexesOfSelectedRows) {
        auto row = item.row();
        //MITK_INFO << msg << "row: " << row; 
        m_TableModelCap->item(row,1)->setText(QString::fromStdString(props["BC Type"]));

        if (props["BC Type"] == "Resistance" || props["BC Type"] == "RCR" || props["BC Type"] == "Coronary") {
            m_TableModelCap->item(row,2)->setText(QString::fromStdString(props["Values"]));
        } else if (props["BC Type"] == "Prescribed Velocities") {
            if (props["Flow Rate"] != "") {
                m_TableModelCap->item(row,2)->setText("Assigned");
            }
        } else {
            m_TableModelCap->item(row,2)->setText("");
        }

        m_TableModelCap->item(row,3)->setText(QString::fromStdString(props["Pressure"]));
        m_TableModelCap->item(row,4)->setText(QString::fromStdString(props["Analytic Shape"]));
        m_TableModelCap->item(row,5)->setText(QString::fromStdString(props["Period"]));
        m_TableModelCap->item(row,6)->setText(QString::fromStdString(props["Point Number"]));
        m_TableModelCap->item(row,7)->setText(QString::fromStdString(props["Fourier Modes"]));
        m_TableModelCap->item(row,8)->setText(QString::fromStdString(props["Flip Normal"]));
        m_TableModelCap->item(row,9)->setText(QString::fromStdString(props["Flow Rate"]));
        m_TableModelCap->item(row,10)->setText(QString::fromStdString(props["Original File"]));

        std::string flowrateContent  = m_TableModelCap->item(row,9)->text().trimmed().toStdString();
        //MITK_INFO << msg << "set flow rate for row: " << row; 
        //MITK_INFO << msg << "flowrateContent: " << flowrateContent;

        m_TableModelCap->item(row,11)->setText(QString::fromStdString(props["Timed Pressure"]));
        m_TableModelCap->item(row,12)->setText(QString::fromStdString(props["Pressure Period"]));
        m_TableModelCap->item(row,13)->setText(QString::fromStdString(props["Pressure Scaling"]));
        m_TableModelCap->item(row,14)->setText(QString::fromStdString(props["R Values"]));
        m_TableModelCap->item(row,15)->setText(QString::fromStdString(props["C Values"]));
    }

  //CheckBCsInputState();
}

//-------------------
// ShowSplitBCWidget
//-------------------
// Show popup to set data for split resistance and capacitance. 
//
// This is show when right clicking on BCs in the Inlet / Outlet toolbox page. 
//
void sv4guiSimulationView1d::ShowSplitBCWidget(QString splitTarget)
{
    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1) {
        return;
    }

    QString lastBCType="";
    for (auto const& item : indexesOfSelectedRows) {
        auto row = item.row();
        QString BCType=m_TableModelCap->item(row,1)->text().trimmed();

        if(BCType=="") {
            QMessageBox::warning(m_Parent,MsgTitle,"BC Type Missing\nPlease speficify BC type for the caps.");
            return;
        } else if(BCType!=lastBCType && lastBCType!="") {
            QMessageBox::warning(m_Parent, MsgTitle, "BC Type Inconsistent\nPlease split BC for the caps of the same BC type!");
            return;
        }

        lastBCType=BCType;
    }

    if(lastBCType=="Resistance" && splitTarget=="Capacitance") {
        QMessageBox::warning(m_Parent, MsgTitle,"Can't split capacitance for BC type Resistance!");
        return;
    }

    m_SplitBCWidget->UpdateGUI(lastBCType,splitTarget);

    m_SplitBCWidget->show();
}

void sv4guiSimulationView1d::ShowSplitBCWidgetR(bool)
{
    ShowSplitBCWidget("Resistance");
}

void sv4guiSimulationView1d::ShowSplitBCWidgetC(bool)
{
    ShowSplitBCWidget("Capacitance");
}


//------------
// SplitCapBC
//------------
//
void  sv4guiSimulationView1d::SplitCapBC()
{
    if(!m_MitkJob || !m_Model || !m_SplitBCWidget) {
        return;
    }

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) {
        return;
    }

    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1) {
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
    for (auto const& item : indexesOfSelectedRows) {
        auto row = item.row();
        std::string faceName=m_TableModelCap->item(row,0)->text().trimmed().toStdString();
        double murrayArea=pow(modelElement->GetFaceArea(modelElement->GetFaceID(faceName)),murrayCoefficient/2);
        totalMurrayArea+=murrayArea;
        faceMurrayArea.push_back(murrayArea);
    }

    for(int i=0;i<indexesOfSelectedRows.size();i++) {
        int row=indexesOfSelectedRows[i].row();

        if(splitTarget=="Resistance") {
            double murrayRatio=totalMurrayArea/faceMurrayArea[i];
            if(bcType=="Resistance") {
                m_TableModelCap->item(row,2)->setText(QString::number(murrayRatio*totalValue));
            } else if(bcType=="RCR") {
                QString Rp=QString::number(murrayRatio*totalValue*percentage1);
                QString CC="0";
                QString Rd=QString::number(murrayRatio*totalValue*percentage2);
                QStringList list = m_TableModelCap->item(row,15)->text().split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);

                if(list.size()==1) {
                    CC=list[0];
                }

                m_TableModelCap->item(row,2)->setText(Rp+" "+CC+" "+Rd);
                m_TableModelCap->item(row,14)->setText(Rp+" "+Rd);
            } else if(bcType=="Coronary") {
                QString Ra=QString::number(murrayRatio*totalValue*percentage1);
                QString Ca="0";
                QString Ram=QString::number(murrayRatio*totalValue*percentage2);
                QString Cim="0";
                QString Rv=QString::number(murrayRatio*totalValue*percentage3);

                QStringList list = m_TableModelCap->item(row,15)->text().split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);
                if(list.size()==2) {
                    Ca=list[0];
                    Cim=list[1];
                }

                m_TableModelCap->item(row,2)->setText(Ra+" "+Ca+" "+Ram+" "+Cim+" "+Rv);
                m_TableModelCap->item(row,14)->setText(Ra+" "+Ram+" "+Rv);
            }
        } else if(splitTarget=="Capacitance") {
            double murrayRatio=faceMurrayArea[i]/totalMurrayArea;
            if(bcType=="RCR") {
                QString Rp="0";
                QString CC=QString::number(murrayRatio*totalValue);
                QString Rd="0";

                QStringList list = m_TableModelCap->item(row,14)->text().split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);
                if(list.size()==2) {
                    Rp=list[0];
                    Rd=list[1];
                }

                m_TableModelCap->item(row,2)->setText(Rp+" "+CC+" "+Rd);
                m_TableModelCap->item(row,15)->setText(CC);
            } else if(bcType=="Coronary") {
                QString Ra="0";
                QString Ca=QString::number(murrayRatio*totalValue*percentage1);
                QString Ram="0";
                QString Cim=QString::number(murrayRatio*totalValue*percentage2);
                QString Rv="0";

                QStringList list = m_TableModelCap->item(row,14)->text().split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);
                if(list.size()==3) {
                    Ra=list[0];
                    Ram=list[1];
                    Rv=list[2];
                }

                m_TableModelCap->item(row,2)->setText(Ra+" "+Ca+" "+Ram+" "+Cim+" "+Rv);
                m_TableModelCap->item(row,15)->setText(Ca+" "+Cim);
            }
        }
    }
}

//--------------
// UpdateGUICap
//--------------
// Set the GUI values for inlet / outler (cap) boundary conditions
// table in the 'Inlet and Outlet BCs' toolbox tab.
//
// The GUI values are updated from sv4guiSimJob1d properties?
//
// Modifies:
//   m_TableModelCap
//
//
void sv4guiSimulationView1d::UpdateGUICap()
{
    if(!m_MitkJob || !m_Model) {
        return;
    }

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) {
        return;
    }

    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();
    if(job==NULL) {
        job=new sv4guiSimJob1d();
    }

    m_TableModelCap->clear();

    QStringList capHeaders;
    capHeaders << "Name" << "BC Type" << "Values" << "Pressure"
               << "Analytic Shape" << "Period" << "Point Number" << "Fourier Modes" << "Flip Normal" << "Flow Rate" << "Original File"
               << "Timed Pressure" << "Pressure Period" << "Pressure Scaling"
               << "R Values" << "C Values";
    m_TableModelCap->setHorizontalHeaderLabels(capHeaders);
    m_TableModelCap->setColumnCount(capHeaders.size());

    std::vector<int> ids=modelElement->GetCapFaceIDs();
    int rowIndex=-1;

    for (auto const& id : ids) {
        sv4guiModelElement::svFace* face=modelElement->GetFace(id);
        if(face==NULL ) {
            continue;
        }

        rowIndex++;
        m_TableModelCap->insertRow(rowIndex);

        QStandardItem* item;

        item= new QStandardItem(QString::fromStdString(face->name));
        item->setEditable(false);
        m_TableModelCap->setItem(rowIndex, 0, item);

        std::string bcType=job->GetCapProp(face->name,"BC Type");
        item= new QStandardItem(QString::fromStdString(bcType));
        m_TableModelCap->setItem(rowIndex, 1, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Values")));
        m_TableModelCap->setItem(rowIndex, 2, item);

        if(bcType=="Prescribed Velocities" && job->GetCapProp(face->name,"Flow Rate")!="") {
            item= new QStandardItem(QString::fromStdString("Assigned"));
            m_TableModelCap->setItem(rowIndex, 2, item);
        }

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Pressure")));
        m_TableModelCap->setItem(rowIndex, 3, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Analytic Shape")));
        m_TableModelCap->setItem(rowIndex, 4, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Period")));
        m_TableModelCap->setItem(rowIndex, 5, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Point Number")));
        m_TableModelCap->setItem(rowIndex, 6, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Fourier Modes")));
        m_TableModelCap->setItem(rowIndex, 7, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Flip Normal")));
        m_TableModelCap->setItem(rowIndex, 8, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Flow Rate")));
        m_TableModelCap->setItem(rowIndex, 9, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Original File")));
        m_TableModelCap->setItem(rowIndex, 10, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Timed Pressure")));
        m_TableModelCap->setItem(rowIndex, 11, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Pressure Period")));
        m_TableModelCap->setItem(rowIndex, 12, item);

        item= new QStandardItem(QString::fromStdString(job->GetCapProp(face->name,"Pressure Scaling")));
        m_TableModelCap->setItem(rowIndex, 13, item);

        QString RValues="";
        QString CValues="";
        QStringList list =QString::fromStdString(job->GetCapProp(face->name,"Values")).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
        if(bcType=="RCR") {
            if(list.size()==3) {
                RValues=list[0]+" "+list[2];
                CValues=list[1];
            }
        } else if(bcType=="Coronary") {
            if(list.size()==5) {
                RValues=list[0]+" "+list[2]+" "+list[4];
                CValues=list[1]+" "+list[3];
            }
        }

        item= new QStandardItem(RValues);
        m_TableModelCap->setItem(rowIndex, 14, item);

        item= new QStandardItem(CValues);
        m_TableModelCap->setItem(rowIndex, 15, item);
    }

    ui->tableViewCap->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->tableViewCap->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    ui->tableViewCap->horizontalHeader()->resizeSection(1,100);
    ui->tableViewCap->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);

    for(int i=3;i<capHeaders.size();i++) {
        ui->tableViewCap->setColumnHidden(i,true);
    }
}

//---------------------
// SelectMaterialModel
//---------------------
//
void sv4guiSimulationView1d::SelectMaterialModel(int index)
{
    auto msg = "[sv4guiSimulationView1d::SelectMaterialModel] ";
    MITK_INFO << msg << "---------- SelectMaterialModel ----------  ";
    auto matModelName = MaterialModel::names[index]; 
    //MITK_INFO << msg << "matModelName: " << matModelName;

    // Show widgets for the selected material model.
    ui->MaterialModel_StackedWidget->setCurrentIndex(index);

    // [DaveP] 09/17/2019 why are we calling this?
    //UpdateSimJob();
}

//--------------------------
// TableVarSelectionChanged
//--------------------------
//
void sv4guiSimulationView1d::TableVarSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
/*
    if(!m_Model) {
        return;
    }

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewVar->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    for (auto const& item : indexesOfSelectedRows) {
        auto row = item.row();
        std::string name= m_TableModelVar->item(row,0)->text().toStdString();
        modelElement->SelectFace(name);
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
*/
}

void sv4guiSimulationView1d::TableViewVarContextMenuRequested( const QPoint & pos )
{
    m_TableMenuVar->popup(QCursor::pos());
}

void sv4guiSimulationView1d::SetVarThickness(bool)
{
/*
    QModelIndexList indexesOfSelectedRows = ui->tableViewVar->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    bool ok=false;
    QString thickness=QInputDialog::getText(m_Parent, "Set Thickness", "Thickness:", QLineEdit::Normal, "", &ok);

    if(!ok)
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();
        m_TableModelVar->item(row,2)->setText(thickness);
    }
*/
}

void sv4guiSimulationView1d::SetVarE(bool)
{
/*
    QModelIndexList indexesOfSelectedRows = ui->tableViewVar->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    bool ok=false;
    QString modulus=QInputDialog::getText(m_Parent, "Set Elastic Modulus", "Elastic Modulus:", QLineEdit::Normal, "", &ok);

    if(!ok)
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();
        m_TableModelVar->item(row,3)->setText(modulus);
    }
*/
}

//---------------
// UpdateGUIWall
//---------------
//
void sv4guiSimulationView1d::UpdateGUIWall()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::UpdateGUIWall] ";
    MITK_INFO << msg << "--------- UpdateGUIWall ----------"; 

    sv4guiSimJob1d* job = m_MitkJob->GetSimJob();

    if (job == NULL) {
        job = new sv4guiSimJob1d();
        MITK_INFO << msg << " job is null, create new one."; 
    } else {
        MITK_INFO << msg << " job is not null."; 
    }

    // Update material model.
    //
    auto materialModel = QString::fromStdString(job->GetWallProp("Material Model"));
    MITK_INFO << msg << "materialModel: " << materialModel; 
    auto it = std::find(MaterialModel::names.begin(), MaterialModel::names.end(), materialModel);
    if (it != MaterialModel::names.end()) {
        auto index = std::distance(MaterialModel::names.begin(), it);
        ui->MaterialModelComboBox->setCurrentIndex(index);
    } else {
        return;
    }

    if (materialModel == MaterialModel::OLUFSEN) { 
        auto k1 = QString::fromStdString(job->GetWallProp("Olufsen Material K1")); 
        auto k2 = QString::fromStdString(job->GetWallProp("Olufsen Material K2")); 
        auto k3 = QString::fromStdString(job->GetWallProp("Olufsen Material K3")); 
        auto exponent = QString::fromStdString(job->GetWallProp("Olufsen Material Exponent")); 
        auto pressure = QString::fromStdString(job->GetWallProp("Olufsen Material Pressure")); 
        ui->OlufsenMatProp_K1_LineEdit->setText(k1);
        ui->OlufsenMatProp_K2_LineEdit->setText(k2);
        ui->OlufsenMatProp_K3_LineEdit->setText(k3);
        ui->OlufsenMatProp_Exponent_LineEdit->setText(exponent);
        ui->OlufsenMatProp_Pressure_LineEdit->setText(pressure);
    }

    else if (materialModel == MaterialModel::LINEAR) { 
        auto Ehr = QString::fromStdString(job->GetWallProp("Linear Material Ehr")); 
        auto pressure = QString::fromStdString(job->GetWallProp("Linear Material Pressure")); 
        ui->LinearMatProp_Ehr_LineEdit->setText(Ehr);
        ui->LinearMatProp_Pressure_LineEdit->setText(pressure);
    }
}

//-----------------
// UpdateGUISolver
//-----------------
//
void sv4guiSimulationView1d::UpdateGUISolver()
{
    auto msg = "[sv4guiSimulationView1d::UpdateGUISolver]";
    MITK_INFO << msg;
    MITK_INFO << msg << "--------- UpdateGUISolver ----------"; 
    if (!m_MitkJob) {
        return;
    }

    sv4guiSimJob1d* job = m_MitkJob->GetSimJob();
    if (job == NULL) {
        job = new sv4guiSimJob1d();
        MITK_INFO << msg << " job is null, create new one."; 
    } else {
        MITK_INFO << msg << " job is not null."; 
    }

    m_TableModelSolver->clear();

    // Set table column headers.
    //
    QStringList solverHeaders;
    solverHeaders << "Parameter" << "Value" << "Type" << "Value List";
    m_TableModelSolver->setHorizontalHeaderLabels(solverHeaders);
    int colCount = solverHeaders.size();
    m_TableModelSolver->setColumnCount(colCount);

    // Read solver parameter names.
    //
    // This sets up parameter names displayed in the table.
    //
    QString templateFilePath=":solvertemplate1d.xml";
    if (m_UseCustom) {
        templateFilePath = m_SolverTemplatePath;
    }

    QFile xmlFile(templateFilePath);
    if(!xmlFile.open(QIODevice::ReadOnly)) {
        QMessageBox::warning(m_Parent, MsgTitle,"Solver Parameter Table template file not found");
        return;
    }

    QDomDocument doc("solvertemplate1d");
    if(!doc.setContent(&xmlFile)) {
        QMessageBox::warning(m_Parent,"File Template Error","Format Error.");
        return;
    }
    xmlFile.close();

    // Setup parameter table.
    //
    QDomElement templateElement = doc.firstChildElement("template");
    QDomNodeList sectionList = templateElement.elementsByTagName("section");
    int rowIndex = -1;
    MITK_INFO << msg << "sectionList.size(): " << sectionList.size();

    auto numTimeSteps = QString::fromStdString(job->GetSolverProp("Number of Timesteps"));
    MITK_INFO << msg << "numTimeSteps: " << numTimeSteps;

    for (int i = 0; i < sectionList.size(); i++) {
        QDomNode sectionNode = sectionList.item(i);
        if (sectionNode.isNull()) {
            continue;
        }

        QDomElement sectionElement = sectionNode.toElement();

        if (sectionElement.isNull()) {
            continue;
        }

        // Add a row.
        QString name = sectionElement.attribute("name");
        MITK_INFO << msg << "name: " << name;
        rowIndex++;
        QStandardItem* item = new QStandardItem(name);
        item->setEditable(false);
        QBrush brushGray(Qt::lightGray);
        item->setBackground(brushGray);
        m_TableModelSolver->setItem(rowIndex, 0, item);
        ui->tableViewSolver->setSpan(rowIndex,0,1,colCount);

        QDomNodeList parList = sectionElement.elementsByTagName("param");
        MITK_INFO << msg << "parList.size(): " << parList.size();
        for(int j = 0; j < parList.size(); j++) {
            QDomNode parNode = parList.item(j);
            if (parNode.isNull()) {
                MITK_INFO << msg << "parNode.isNull";
                continue;
            }

            QDomElement parElement=parNode.toElement();
            if (parElement.isNull()) {
                MITK_INFO << msg << "parElement.isNull()";
                continue;
            }

            rowIndex++;
            QStandardItem* item = new QStandardItem(parElement.attribute("name"));
            item->setEditable(false);
            item->setToolTip(parElement.attribute("name"));
            m_TableModelSolver->setItem(rowIndex, 0, item);
            MITK_INFO << msg << "rowIndex: " << rowIndex;

            auto attName = parElement.attribute("name").toStdString();
            MITK_INFO << msg << "attName: " << attName;

            std::string value = job->GetSolverProp(parElement.attribute("name").toStdString());
            MITK_INFO << msg << "value: " << value;
            item= new QStandardItem(value==""?parElement.attribute("value"):QString::fromStdString(value));
            m_TableModelSolver->setItem(rowIndex, 1, item);

            item= new QStandardItem(parElement.attribute("type"));
            item->setEditable(false);
            m_TableModelSolver->setItem(rowIndex, 2, item);

            item= new QStandardItem(parElement.attribute("enum_list"));
            item->setEditable(false);
            m_TableModelSolver->setItem(rowIndex, 3, item);
        }
    }

    ui->tableViewSolver->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
    ui->tableViewSolver->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    ui->tableViewSolver->horizontalHeader()->resizeSection(1,120);

    ui->tableViewSolver->setColumnHidden(2,true);
    ui->tableViewSolver->setColumnHidden(3,true);

}

//--------------
// UpdateGUIJob
//--------------
//
void sv4guiSimulationView1d::UpdateGUIJob()
{
    //MITK_INFO << "--------- UpdateGUIJob ----------"; 
    if (!m_MitkJob) {
        return;
    }

    /* [TODO:Davep] May add using mesh later. 
    auto meshNames = GetMeshNames();
    ui->comboBoxMeshName->clear();
    ui->comboBoxMeshName->addItem(" ");

    for (auto const& meshName : meshNames) {
        ui->comboBoxMeshName->addItem(QString::fromStdString(meshName));
    }

    int foundIndex = ui->comboBoxMeshName->findText(QString::fromStdString(m_MitkJob->GetMeshName()));
    ui->comboBoxMeshName->setCurrentIndex(foundIndex);
    */

    //int coreNum=QThread::idealThreadCount();
    //ui->sliderNumProcs->setMaximum(coreNum);

    sv4guiSimJob1d* job = m_MitkJob->GetSimJob();

    if (job == NULL) {
        return;
    }

    //std::string pNum = job->GetRunProp("Number of Processes");
    //ui->sliderNumProcs->setValue(pNum== "" ? 1 : QString::fromStdString(pNum).toInt());
}

//-----------------------
// UpdateSurfaceMeshName
//-----------------------
//
void sv4guiSimulationView1d::UpdateSurfaceMeshName()
{
    /* [TODO:Davep] May add using mesh later. 
    auto msg = "[sv4guiSimulationView1d::UpdateSurfaceMeshName] ";
    MITK_INFO << msg << "--------- UpdateSurfaceMeshName ----------"; 
    auto meshName = ui->comboBoxMeshName->currentText().toStdString();

    if (meshName != "") {
        auto mesh = GetSurfaceMesh(meshName);
    }
    MITK_INFO << msg << "Mesh name: " << meshName;
    */
}

//--------------
// GetMeshNames
//--------------
//
std::vector<std::string> sv4guiSimulationView1d::GetMeshNames()
{
    std::string modelName = m_MitkJob->GetModelName();
    std::vector<std::string> meshNames;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);

    if (rs->size() > 0) {
        mitk::DataNode::Pointer projFolderNode = rs->GetElement(0);
        rs = GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiMeshFolder"));

        if (rs->size() > 0) {
            mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);
            rs=GetDataStorage()->GetDerivations(meshFolderNode);

            for (int i = 0; i < rs->size(); i++) {
                sv4guiMitkMesh* mitkMesh = dynamic_cast<sv4guiMitkMesh*>(rs->GetElement(i)->GetData());
                if (mitkMesh && mitkMesh->GetModelName() == modelName) {
                    meshNames.push_back(rs->GetElement(i)->GetName());
                }
            }
        }
    }

    return meshNames;
}

//-----------------
// UpdateGUIRunDir
//-----------------
//
void sv4guiSimulationView1d::UpdateGUIRunDir()
{
    //MITK_INFO << "--------- UpdateGUIRunDir ----------";
    ui->lineEditResultDir->clear();
    ui->lineEditConvertDir->clear();

    if(m_JobNode.IsNull()) {
        return;
    }

    QString jobPath=GetJobPath();
    if(jobPath=="") {
        return;
    }

    if(!m_MitkJob) {
        return;
    }

    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();
    if(job==NULL)
        return;

    std::string pNum=job->GetRunProp("Number of Processes");
    if(pNum=="")
        return;

    QString runDir =pNum=="1"?jobPath:jobPath+"/"+QString::fromStdString(pNum)+"-procs_case";
    ui->lineEditResultDir->setText(runDir);
}

//------------
// GetJobPath
//------------
//
QString sv4guiSimulationView1d::GetJobPath()
{
    QString jobPath = "";

    if (m_JobNode.IsNull()) {
        return jobPath;
    }

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);

    std::string projPath = "";
    std::string simFolderName = "";

    if (rs->size() > 0) {
        mitk::DataNode::Pointer projFolderNode = rs->GetElement(0);
        projFolderNode->GetStringProperty("project path", projPath);
        rs = GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiSimulation1dFolder"));

        if (rs->size() > 0) {
            mitk::DataNode::Pointer simFolderNode = rs->GetElement(0);
            simFolderName = simFolderNode->GetName();
            jobPath = QString::fromStdString(projPath+"/"+simFolderName+"/"+m_JobNode->GetName());
        }
    }

    return jobPath;
}

//-----------------------
// CreateSimulationFiles
//-----------------------
// Create files for a simulation.
//
// [Davep] Not sure why we have this function.
//
void sv4guiSimulationView1d::CreateSimulationFiles()
{
    if (!m_MitkJob) {
        return;
    }

    auto outputAllFiles = true;
    auto updateJob = true; 
    auto createFolder = false;

    CreateDataFiles(GetJobPath(), outputAllFiles, updateJob, createFolder);
}

//--------
// RunJob
//--------
// Execute a 1D simulation job.
//
void sv4guiSimulationView1d::RunJob()
{
    auto msg = "[sv4guiSimulationView1d::RunJob] ";
    //MITK_INFO << msg << "--------- RunJob ----------"; 

    if (!m_MitkJob) {
        return;
    }

    QString jobPath = GetJobPath();
    if ((jobPath == "") || !QDir(jobPath).exists()) {
        QMessageBox::warning(m_Parent,MsgTitle, "Unable to run.\nPlease make sure data files have been created!");
        return;
    }
    MITK_INFO << msg << "Job path: " << jobPath;

    // Get the solver executable.
    auto solverExecutable = GetSolverExecutable();
    if (solverExecutable == nullptr) {
        return; 
    } 

    // Set job properties used to write solver log.
    //
    m_JobNode->SetStringProperty("output directory", GetJobPath().toStdString().c_str());
    m_JobNode->SetStringProperty("solver log file", sv4guiSimulationView1d::SOLVER_LOG_FILE_NAME.toStdString().c_str());

    sv4guiSimJob1d* job = m_MitkJob->GetSimJob();

/*
    if (job) {
        QString tstr = QString::fromStdString(job->GetSolverProp("Number of Timesteps"));
        totalSteps = tstr.toInt();
    }
*/

    mitk::StatusBar::GetInstance()->DisplayText("Running simulation ...");

    QProcess* solverProcess = new QProcess(m_Parent);
    solverProcess->setWorkingDirectory(jobPath);
    solverProcess->setProgram(solverExecutable);

    QStringList arguments;
    arguments << m_SolverInputFile;
    solverProcess->setArguments(arguments);

    int startStep = 0;
    int totalSteps = 2000;

    sv4guiSolverProcessHandler1d* handler = new sv4guiSolverProcessHandler1d(solverProcess, m_JobNode, startStep, totalSteps, 
        jobPath, m_Parent);
    handler->Start();
}

//---------------------
// GetSolverExecutable
//---------------------
//
QString sv4guiSimulationView1d::GetSolverExecutable()
{
    auto msg = "[sv4guiSimulationView1d::GetSolverExecutable] ";
    MITK_INFO << msg << "--------- GetSolverExecutable ----------";
#ifdef WIN32
    auto solverExecutable = "svOneDSolver.exe";
#else
    auto solverExecutable = SOLVER_INSTALL_DIRECTORY + "/" + SOLVER_EXECUTABLE_NAME;
    auto solverInstallPath = SOLVER_INSTALL_DIRECTORY;

    if (!QDir(solverInstallPath).exists()) {
        auto msg1 = "The 1D solver was not found.\n"; 
        auto msg2 = "Please install the 1D solver in '" + solverInstallPath + "'.\n"; 
        QMessageBox::warning(m_Parent, MsgTitle, msg1+msg2); 
        return nullptr;
    }

    // Set the install path.
    QStringList dirList = QDir(solverInstallPath).entryList(QDir::Dirs|QDir::NoDotAndDotDot|QDir::NoSymLinks,QDir::Name);
    if (dirList.size() != 0) {
      solverInstallPath += "/" + dirList.back();
    } else {
        auto msg1 = "The 1D solver was not found.\n"; 
        auto msg2 = "Please install the 1D solver in '" + solverInstallPath + "'.\n"; 
        QMessageBox::warning(m_Parent, MsgTitle, msg1+msg2); 
        return nullptr;
    }
    MITK_INFO << msg << "solverInstallPath: " << solverInstallPath;

    // Set the solver executable.
    solverExecutable = solverInstallPath + "/" + SOLVER_INSTALL_SUB_DIRECTORY + "/" + SOLVER_EXECUTABLE_NAME; 
    if (!QFile::exists(solverExecutable)) {
        auto msg1 = "The 1D solver was not found.\n"; 
        auto msg2 = "Please install the 1D solver in '" + solverInstallPath + "'.\n"; 
        QMessageBox::warning(m_Parent, MsgTitle, msg1+msg2); 
        return nullptr;
    }
#endif

    MITK_INFO << msg << "solverExecutable: " << solverExecutable;

    return solverExecutable;
}

//-----------------
// CreateDataFiles
//-----------------
// Create files for a simulation.
//
// A sv4guiSimulationPython1d object is used to execute a Python script to generate 
// a 1D mesh and create a input file for the 1D solver.
//
// Files are written to the PROJECT/Simulations1d/JOB_NAME directory.
//
// Arguments:
//   outputDir: The directory to write the files to. 
//
// Files written:
//   OUTLET_FACE_NAMES_FILE_NAME
//   SOLVER_FILE_NAME
//   RCR_BC_FILE_NAME or RESISTANCE_BC_FILE_NAME 
//
// Modifies:
//   m_SolverInputFile = solverInputFile;
//   m_SimulationFilesCreated = true;
//
bool sv4guiSimulationView1d::CreateDataFiles(QString outputDir, bool outputAllFiles, bool updateJob, bool createFolder)
{
    auto msg = "[sv4guiSimulationView1d::CreateDataFiles] ";
    MITK_INFO << msg << "--------- CreateDataFiles ----------"; 
    MITK_INFO << msg << "Output directory: " << outputDir;
    MITK_INFO << msg << "Output pulgin directory: " << m_PluginOutputDirectory;

    if (!m_MitkJob) {
        return false;
    }

    if (outputDir == "") {
        return false;
    }

    /*
    if (!CheckInputState(DataInputStateType::SIMULATION_FILES)) {
        return false;
    }
    */

    // Create a job object storing all the parameters needed for a simulation.
    //
    mitk::StatusBar::GetInstance()->DisplayText("Creating Job");
    std::string jobMsg;
    sv4guiSimJob1d* job = CreateJob(jobMsg);

    if (job == nullptr) {
        QMessageBox::warning(m_Parent, MsgTitle, "Error in parameter values.\n"+QString::fromStdString(jobMsg));
        return false;
    }

    QDir dir(outputDir);
    dir.mkpath(outputDir);

    m_MitkJob->SetSimJob(job);
    //m_MitkJob->SetMeshName(meshName);
    m_MitkJob->SetDataModified();

    // Check that centerlines have been generated.
    if (!m_CenterlinesCalculated) {
        QMessageBox::warning(NULL, MsgTitle, "No centerlines have been calculated or centerlines source file set.");
        MITK_ERROR << "No centerlines file is defined.";
        return false;
    }
    MITK_INFO << msg << "Centerlines file: " << m_CenterlinesFileName;

    // Check that inlet and outlet faces have been identified.
    if (!m_ModelInletFaceSelected || (m_ModelOutletFaceNames.size() == 0)) {
        QMessageBox::warning(NULL, MsgTitle, "No inlet face has been selected.");
        MITK_ERROR << "No inlet face has been defined.";
        return false;
    }
    auto inletFaceName = m_ModelInletFaceNames[0];

    // Check that an inlet flow rate file has been specified.
    if (job->GetCapProp(inletFaceName, "Original File") == "") {
        auto msg = "A flow rate file for the inlet face '" + inletFaceName + "' has not been defined.";
        MITK_WARN << msg; 
        QMessageBox::warning(NULL, MsgTitle, QString(msg.c_str())); 
        return false;
    }

    m_SimulationFilesCreated = false;
    m_SolverInputFile = ""; 

   // Create a sv4guiSimulationPython1d used to execute a Python script
   // and get the parameter names used by that script.
    //
    auto pythonInterface = sv4guiSimulationPython1d();
    auto params = pythonInterface.m_ParameterNames;

    // Set the parameters used by the Python script.
    //
    auto modelName = m_ModelNode->GetName();
    pythonInterface.AddParameter(params.MODEL_NAME, modelName);

    auto outDir = outputDir.toStdString();
    pythonInterface.AddParameter(params.OUTPUT_DIRECTORY, outDir);

    pythonInterface.AddParameter(params.UNITS, "cm");
    pythonInterface.AddParameter(params.ELEMENT_SIZE, std::to_string(m_1DMeshElementSize));
    pythonInterface.AddParameter(params.CENTERLINES_INPUT_FILE, m_CenterlinesFileName.toStdString()); 

    // Set parameter and write outlet face names to a file.
    WriteOutletFaceNames(outputDir, job, pythonInterface);

    // Set bc paramaters and write the bc data files.
    pythonInterface.AddParameter(params.UNIFORM_BC, "false"); 
    WriteBCFiles(outputDir, job, pythonInterface);

    auto solverFileName = SOLVER_FILE_NAME.toStdString(); 
    pythonInterface.AddParameter(params.WRITE_SOLVER_FILE, "true"); 
    pythonInterface.AddParameter(params.SOLVER_OUTPUT_FILE, solverFileName); 

    // Add basic physical parameters.
    auto density = m_TableModelBasic->item(TableModelBasicRow::Density,1)->text().trimmed().toStdString();
    pythonInterface.AddParameter(params.DENSITY, density);
    auto viscosity = m_TableModelBasic->item(TableModelBasicRow::Viscosity,1)->text().trimmed().toStdString();
    pythonInterface.AddParameter(params.VISCOSITY, viscosity);

    // Add wall properties. 
    AddWallPropertiesParameters(job, pythonInterface);

    // Add solver parameters.
    auto numTimeSteps = m_TableModelSolver->item(TableModelSolverRow::NumberofTimesteps,1)->text().trimmed().toStdString();
    pythonInterface.AddParameter(params.NUM_TIME_STEPS, numTimeSteps);
    auto timeStep = m_TableModelSolver->item(TableModelSolverRow::TimeStepSize,1)->text().trimmed().toStdString();
    pythonInterface.AddParameter(params.TIME_STEP, timeStep);
    auto saveFreq = m_TableModelSolver->item(TableModelSolverRow::NumberofTimeStepsSavingData,1)->text().trimmed().toStdString();
    pythonInterface.AddParameter(params.SAVE_DATA_FREQUENCY, saveFreq);

    // Execute the Python script to generate the 1D solver input file.
    auto statusMsg = "Generating simulation files ..."; 
    ui->JobStatusValueLabel->setText(statusMsg);
    mitk::StatusBar::GetInstance()->DisplayText(statusMsg);
    auto status = pythonInterface.GenerateSolverInput(outDir, job);

    if (!status) {
        QMessageBox::warning(NULL, MsgTitle, "Creating the 1D solver input file has failed.");
        return false;
    }

    m_SolverInputFile = outputDir + "/" + SOLVER_FILE_NAME;
    ui->RunSimulationPushButton->setEnabled(true);
    MITK_INFO << msg << "Solver input file: " << m_SolverInputFile;

    statusMsg = "Simulation files have been created."; 
    ui->JobStatusValueLabel->setText(statusMsg);
    mitk::StatusBar::GetInstance()->DisplayText(statusMsg);
    m_SimulationFilesCreated = true;

    return true;
}

//-----------------------------
// AddWallPropertiesParameters
//-----------------------------
// Add wall properties parameters for the Python script.
//
void sv4guiSimulationView1d::AddWallPropertiesParameters(sv4guiSimJob1d* job, sv4guiSimulationPython1d& pythonInterface)
{
    auto params = pythonInterface.m_ParameterNames;
    auto materialModel = job->GetWallProp("Material Model");
    pythonInterface.AddParameter(params.MATERIAL_MODEL, materialModel);

    if (QString::fromStdString(materialModel) == MaterialModel::OLUFSEN) { 
        auto k1 = job->GetWallProp("Olufsen Material K1"); 
        auto k2 = job->GetWallProp("Olufsen Material K2"); 
        auto k3 = job->GetWallProp("Olufsen Material K3"); 
        auto exponent = job->GetWallProp("Olufsen Material Exponent"); 
        auto pressure = job->GetWallProp("Olufsen Material Pressure"); 
        pythonInterface.AddParameter(params.OLUFSEN_MATERIAL_K1, k1);
        pythonInterface.AddParameter(params.OLUFSEN_MATERIAL_K2, k2);
        pythonInterface.AddParameter(params.OLUFSEN_MATERIAL_K3, k3);
        pythonInterface.AddParameter(params.OLUFSEN_MATERIAL_EXP, exponent);
        pythonInterface.AddParameter(params.OLUFSEN_MATERIAL_PRESSURE, pressure);

    } else if (QString::fromStdString(materialModel) == MaterialModel::LINEAR) { 
        auto Ehr = job->GetWallProp("Linear Material Ehr"); 
        auto pressure = job->GetWallProp("Linear Material Pressure"); 
        pythonInterface.AddParameter(params.LINEAR_MATERIAL_EHR, Ehr);
        pythonInterface.AddParameter(params.LINEAR_MATERIAL_PRESSURE, pressure);
    }
}

//--------------
// WriteBCFiles
//--------------
// Write boundary condition files and set the parameters used 
// by the Python script.
//
// Files written:
//   inlet flow file    
//   rcr or resistance boundary conditions
//
void sv4guiSimulationView1d::WriteBCFiles(const QString outputDir, sv4guiSimJob1d* job, 
  sv4guiSimulationPython1d& pythonInterface)
{
    auto msg = "[sv4guiSimulationView1d::WriteBCFiles] ";
    MITK_INFO << msg << "--------- WriteBCFiles ----------";
    std::string bcType;

    // Write the inflow BC data.
    WriteFlowFile(outputDir, job, pythonInterface);

    // Find the BC type.
    //
    // [DaveP] Can only have one BC type, resistance or rcr?
    //
    for (int i = 0; i < m_TableModelCap->rowCount(); i++) {
        bcType = m_TableModelCap->item(i,1)->text().trimmed().toStdString();
        if (bcType != "Prescribed Velocities") {
            break;
        }
    }
    MITK_INFO << msg << "bcType: " << bcType;
    auto params = pythonInterface.m_ParameterNames;

    if (bcType == "RCR") {
        WriteRcrFile(outputDir, job, pythonInterface);
    } else if (bcType == "Resistance") {
        WriteResistanceFile(outputDir, job, pythonInterface);
    }
}

//---------------
// WriteFlowFile
//---------------
// Write the inlet face flow rate file and set the parameter used 
// by the Python script.
//
// The flow rate file is set by the user using the 'Set Inlet/Outlet BCs' popup 
// called up from the 'Inlet and Outlet BCs' toolbox tab and processed using the
// sv4guiCapBCWidget1d object. The sv4guiSimJob1d object stores the flow rate
// file name (without path) and its contents.
//
void sv4guiSimulationView1d::WriteFlowFile(const QString outputDir, sv4guiSimJob1d* job, 
    sv4guiSimulationPython1d& pythonInterface)
{
    auto msg = "[sv4guiSimulationView1d::WriteFlowFile] ";
    MITK_INFO << msg << "--------- WriteFlowFile ----------";
    MITK_INFO << msg << "Output directory: " << outputDir;

    // Write flow file.
    //
    auto inletFaceName = m_ModelInletFaceNames[0];
    auto flowFileName = job->GetCapProp(inletFaceName, "Original File");
    auto flowFileContent = QString::fromStdString(job->GetCapProp(inletFaceName, "Flow Rate"));
    auto flowFile = outputDir + "/" + QString(flowFileName.c_str());
    QFile flowFileWriter(flowFile);
    MITK_INFO << msg << "inletFaceName: " << inletFaceName;

    if (flowFileWriter.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QTextStream output(&flowFileWriter);
        output << flowFileContent;
        flowFileWriter.close();
    } else {
        auto msg = "Unable to write flow rate file '" + flowFile + "'";
        MITK_ERROR << msg; 
        QMessageBox::critical(NULL, MsgTitle, msg);
    }

    // Add script parameter.
    auto params = pythonInterface.m_ParameterNames;
    pythonInterface.AddParameter(params.INFLOW_INPUT_FILE, flowFile.toStdString()); 
}

//---------------------
// WriteResistanceFile
//---------------------
// Write the resistance boundary conditions file and set the parameters used 
// by the Python script.
//
void sv4guiSimulationView1d::WriteResistanceFile(const QString outputDir, sv4guiSimJob1d* job,
  sv4guiSimulationPython1d& pythonInterface)
{
    auto msg = "[sv4guiSimulationView1d::WriteResistanceFile] ";
    MITK_INFO << msg << "--------- WriteResistanceFile ----------";
    MITK_INFO << msg << "Output directory: " << outputDir;

    // Set the Python script parameters.
    std::string bcType = "Resistance";
    auto outflowBcFileName = outputDir + "/" + RESISTANCE_BC_FILE_NAME;
    auto params = pythonInterface.m_ParameterNames;
    pythonInterface.AddParameter(params.OUTFLOW_BC_TYPE, "resistance");
    pythonInterface.AddParameter(params.OUTFLOW_BC_INPUT_FILE, outflowBcFileName.toStdString());

    // Write the resistance bc data to a file.
    //
    QFile resFile(outflowBcFileName);

    if (resFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QTextStream out(&resFile);

        for (int i = 0; i < m_TableModelCap->rowCount(); i++) {
            auto capName = m_TableModelCap->item(i,0)->text();
            if (m_TableModelCap->item(i,1)->text().trimmed().toStdString() == bcType) { 
                auto values = m_TableModelCap->item(i,2)->text().trimmed();
                out << capName << " " << values << "\n"; 
            }
        }
        resFile.close();
    }
}

//--------------
// WriteRcrFile
//--------------
// Write the RCR boundary conditions file and set the parameters used 
// by the Python script.
//
void sv4guiSimulationView1d::WriteRcrFile(const QString outputDir, sv4guiSimJob1d* job, 
  sv4guiSimulationPython1d& pythonInterface)
{
    auto msg = "[sv4guiSimulationView1d::WriteRcrFile] ";
    MITK_INFO << msg << "--------- WriteRcrFile ----------"; 
    MITK_INFO << msg << "Output directory: " << outputDir;
    QString rcrtFielContent = QString::fromStdString(sv4guiSimulationUtils1d::CreateRCRTFileContent(job));
    //MITK_INFO << msg << "rcrtFielContent: " << rcrtFielContent;

    if (rcrtFielContent == "") {
        return;
    }

    auto outflowBcFileName = outputDir + "/" + RCR_BC_FILE_NAME; 
    auto params = pythonInterface.m_ParameterNames;
    pythonInterface.AddParameter(params.OUTFLOW_BC_TYPE, "rcr"); 
    pythonInterface.AddParameter(params.OUTFLOW_BC_INPUT_FILE, outflowBcFileName.toStdString()); 

    // Write rcr data.
    QFile rcrtFile(outflowBcFileName);
    if (rcrtFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QTextStream out(&rcrtFile);
        out << rcrtFielContent;
        rcrtFile.close();
    }
    return;
}

//--------------------
// ReadInletFaceNames
//--------------------
// Read the inlet face names.
//
std::vector<std::string> sv4guiSimulationView1d::ReadInletFaceNames(const QString outputDir)
{
    auto msg = "[sv4guiSimulationView1d::ReadInletFaceNames] ";
    MITK_INFO << msg << "--------- ReadInletFaceNames ----------";

    std::vector<std::string> inletFaceNames;
    QFile inletNamesFile(outputDir + "/" + INLET_FACE_NAMES_FILE_NAME);

    if (inletNamesFile.open(QIODevice::ReadOnly | QIODevice::Text)) {
        QTextStream in(&inletNamesFile);
        while (!in.atEnd()) {
            QString line = in.readLine();
            MITK_INFO << msg << "line: '" << line << "'";
            inletFaceNames.emplace_back(line.toStdString());
        }
        inletNamesFile.close();
    }
    return inletFaceNames;
}

//---------------------
// WriteInletFaceNames
//---------------------
// Write the inlet face names.
//
void sv4guiSimulationView1d::WriteInletFaceNames(const QString outputDir)
{
    auto msg = "[sv4guiSimulationView1d::WriteInletFaceNames] ";
    MITK_INFO << msg << "--------- WriteInletFaceNames ----------";

    QFile inletNamesFile(outputDir + "/" + INLET_FACE_NAMES_FILE_NAME);

    if (inletNamesFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QTextStream out(&inletNamesFile);

        for (auto const& name : m_ModelInletFaceNames) {
            out << QString::fromStdString(name + "\n");
        }
        inletNamesFile.close();
    }
}

//----------------------
// WriteOutletFaceNames
//----------------------
// Write the outlet face names and set the parameters used 
// by the Python script.
//
void sv4guiSimulationView1d::WriteOutletFaceNames(const QString outputDir, sv4guiSimJob1d* job, 
  sv4guiSimulationPython1d& pythonInterface)
{
    auto msg = "[sv4guiSimulationView1d::WriteRcrFileWriteOutletFaceNames] ";
    MITK_INFO << msg << "--------- WriteOutletFaceNames ----------";

    // Write the outlet names to a file.
    auto outletFacesFileName = outputDir + "/" + OUTLET_FACE_NAMES_FILE_NAME;
    QFile outletNamesFile(outletFacesFileName);
    if (outletNamesFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QTextStream out(&outletNamesFile);
        for (auto const& name : m_ModelOutletFaceNames) {
            out << QString::fromStdString(name + "\n");
        }
        outletNamesFile.close();
    }

    // Set the script parameter.
    auto params = pythonInterface.m_ParameterNames;
    pythonInterface.AddParameter(params.OUTLET_FACE_NAMES_INPUT_FILE, outletFacesFileName.toStdString()); 
}

//----------------
// GetSurfaceMesh
//----------------
// Get a surface mesh from a Mesh Folder.
//
sv4guiMesh* sv4guiSimulationView1d::GetSurfaceMesh(const std::string meshName)
{
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs = GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);

    sv4guiMesh* mesh = NULL;
    mitk::DataNode::Pointer projFolderNode = NULL;
    mitk::DataNode::Pointer meshNode = NULL;

    if (rs->size()>0) {
        projFolderNode = rs->GetElement(0);
        rs = GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiMeshFolder"));

        if (rs->size()>0) {
            mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);

            meshNode=GetDataStorage()->GetNamedDerivedNode(meshName.c_str(),meshFolderNode);
            if(meshNode.IsNotNull()) {
                sv4guiMitkMesh* mitkMesh=dynamic_cast<sv4guiMitkMesh*>(meshNode->GetData());
                if(mitkMesh) {
                    mesh=mitkMesh->GetMesh();
                }
            }
        }
    }
    return mesh;
}


void sv4guiSimulationView1d::ImportFiles()
{
    QString jobPath=GetJobPath();

    if(jobPath=="")
        return;

    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer prefs;
    if (prefService)
    {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    }
    else
    {
        prefs = berry::IPreferences::Pointer(0);
    }

    QString lastFilePath="";
    if(prefs.IsNotNull())
    {
        lastFilePath = prefs->Get("LastFileOpenPath", "");
    }
    if(lastFilePath=="")
        lastFilePath=QDir::homePath();

    QStringList filePaths = QFileDialog::getOpenFileNames(m_Parent, "Choose Files", lastFilePath, tr("All Files (*)"));

    if(filePaths.size()>0)
        if(prefs.IsNotNull())
         {
             prefs->Put("LastFileOpenPath", filePaths.first());
             prefs->Flush();
         }

    for(int i=0;i<filePaths.size();i++)
    {
        QString filePath=filePaths[i];
        QFileInfo fi(filePath);
        QString fileName=fi.fileName();
        QString newFilePath=jobPath+"/"+fileName;
        if (QFile::exists(newFilePath))
        {
            if (QMessageBox::question(m_Parent, MsgTitle, "Overwrite File?\nDo you want to overwrite the file (" +fileName +") in the job?",
                                      QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
            {
                continue;
            }

            QFile::remove(newFilePath);
        }

        QFile::copy(filePath, newFilePath);
    }
}

//-----------
// CreateJob
//-----------
// Create a simulation job.
//
// Creates a sv4guiSimJob1d object and sets all the parameters needed 
// for a simulation in that object.
//
// This is called when any parameter is changed: basic, BCs, solver, ...
//
sv4guiSimJob1d* sv4guiSimulationView1d::CreateJob(std::string& msg, bool checkValidity)
{
    sv4guiSimJob1d* job = new sv4guiSimJob1d();

    if (!SetBasicParameters(job, msg, checkValidity)) {
        delete job;
        return nullptr;
    }

    if (!SetCapBcs(job, msg, checkValidity)) {
        delete job;
        return nullptr;
    }

    if (!SetWallProperites(job, msg, checkValidity)) {
        delete job;
        return nullptr;
    }

    if (!SetSolverParameters(job, msg, checkValidity)) {
        delete job;
        return nullptr;
    }

    return job;
}

//--------------------
// SetBasicParameters
//--------------------
// Set the basic physical constants and velocity initial conditions
// for the simulation.
//
// This sets properies in the sv4guiSimJob1d object.
//
// Modifies: 
//   job
//
bool sv4guiSimulationView1d::SetBasicParameters(sv4guiSimJob1d* job, std::string& msg, bool checkValidity)
{
    for(int i=0;i<m_TableModelBasic->rowCount();i++) {
        std::string par = m_TableModelBasic->item(i,0)->text().toStdString();
        std::string values = m_TableModelBasic->item(i,1)->text().trimmed().toStdString();

        if ((par == "Fluid Density") || (par=="Fluid Viscosity") || (par=="Initial Pressure")) {
            if(!IsDouble(values)) {
                msg=par + " value error: " + values;
                return false;
            }
        } else if(par=="Initial Velocities") {
            int count=0;
            QStringList list = QString(values.c_str()).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            values=list.join(" ").toStdString();

            if(!AreDouble(values,&count) || count!=3) {
                msg=par + " value error: " + values;
                return false;
             }
        }

        job->SetBasicProp(par,values);
    }

    return true;
}

//-----------
// SetCapBcs
//-----------
// Set the values for inlet / outler (cap) boundary conditions.
//
// These values are taken from the GUI values for inlet / outlet 
// (cap) boundary conditions table in the 'Inlet and Outlet BCs' 
// toolbox tab (m_TableModelCap).
//
// Modifies:
//   job.props[]
//
bool sv4guiSimulationView1d::SetCapBcs(sv4guiSimJob1d* job, std::string& msg, bool checkValidity)
{
    auto imsg = "[sv4guiSimulationView1d::SetCapBcs] ";
    MITK_INFO << imsg << "--------- SetCapBcs ----------"; 
    //auto checkValidity = true;
    //auto checkValidity = false;
    MITK_INFO << imsg << "checkValidity: " << checkValidity;

    for (int i = 0; i < m_TableModelCap->rowCount(); i++) {
        std::string capName = m_TableModelCap->item(i,0)->text().toStdString();
        std::string bcType = m_TableModelCap->item(i,1)->text().trimmed().toStdString();
        MITK_INFO << imsg << "capName: " << capName;
        MITK_INFO << imsg << "bcType: " << bcType;

        if (bcType == "Prescribed Velocities") {
            std::string flowrateContent  = m_TableModelCap->item(i,9)->text().trimmed().toStdString();
            std::string period = m_TableModelCap->item(i,5)->text().trimmed().toStdString();
            //MITK_INFO << "check flow rate for row: " << i; 
            //MITK_INFO << imsg << "flowrateContent: " << flowrateContent;

            if (checkValidity) {
                if(flowrateContent == "") {
                    msg = capName + ": no flowrate data";
                    return false;
                }

                if(period == "") {
                    msg = capName + ": no period for flowrate data";
                    return false;
                }
            }

            std::string shape = m_TableModelCap->item(i,4)->text().trimmed().toStdString();
            std::string pointNum = m_TableModelCap->item(i,6)->text().trimmed().toStdString();
            std::string modeNum = m_TableModelCap->item(i,7)->text().trimmed().toStdString();
            std::string flip = m_TableModelCap->item(i,8)->text().trimmed().toStdString();
            std::string originalFile = m_TableModelCap->item(i,10)->text().trimmed().toStdString();

            job->SetCapProp(capName,"BC Type", bcType);
            job->SetCapProp(capName,"Analytic Shape", shape);
            job->SetCapProp(capName,"Period", period);
            job->SetCapProp(capName,"Point Number", pointNum);
            job->SetCapProp(capName,"Fourier Modes", modeNum);
            job->SetCapProp(capName,"Flip Normal", flip);
            job->SetCapProp(capName,"Flow Rate", flowrateContent);
            job->SetCapProp(capName,"Original File", originalFile);

        } else if (bcType != "") {
            std::string values = m_TableModelCap->item(i,2)->text().trimmed().toStdString();
            std::string pressure = m_TableModelCap->item(i,3)->text().trimmed().toStdString();
            std::string originalFile = m_TableModelCap->item(i,10)->text().trimmed().toStdString();
            std::string timedPressure = m_TableModelCap->item(i,11)->text().trimmed().toStdString();
            std::string pressurePeriod = m_TableModelCap->item(i,12)->text().trimmed().toStdString();
            std::string pressureScaling = m_TableModelCap->item(i,13)->text().trimmed().toStdString();
            std::string RValues = m_TableModelCap->item(i,14)->text().trimmed().toStdString();
            std::string CValues = m_TableModelCap->item(i,15)->text().trimmed().toStdString();

            if (checkValidity) {
                if (bcType == "Resistance") {
                    if(!IsDouble(values)) {
                        msg = capName + " R value error: " + values;
                        return false;
                    }
                } else if (bcType == "RCR") {
                    int count = 0;
                    QStringList list = QString(values.c_str()).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                    values=list.join(" ").toStdString();

                    if(!AreDouble(values,&count)||count!=3) {
                        msg=capName + " RCR values error: " + values;
                        return false;
                    }
                } else if(bcType == "Coronary") {
                    int count=0;
                    QStringList list = QString(values.c_str()).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
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

//-------------------
// SetWallProperites
//-------------------
// Set the job wall properties data.
//
bool sv4guiSimulationView1d::SetWallProperites(sv4guiSimJob1d* job, std::string& errorMsg, bool checkValidity)
{
    auto msg = "[sv4guiSimulationView1d::SetWallProperites] ";
    MITK_INFO << msg << "--------- SetWallProperites ----------"; 
    int materialModelIndex = ui->MaterialModelComboBox->currentIndex();
    MITK_INFO << msg << "materialModelIndex: " << materialModelIndex;
    auto materialModel = MaterialModel::names[materialModelIndex]; 

    if (materialModel == MaterialModel::OLUFSEN) { 
        auto k1 = ui->OlufsenMatProp_K1_LineEdit->text().trimmed().toStdString();
        auto k2 = ui->OlufsenMatProp_K2_LineEdit->text().trimmed().toStdString();
        auto k3 = ui->OlufsenMatProp_K3_LineEdit->text().trimmed().toStdString();
        auto exponent = ui->OlufsenMatProp_Exponent_LineEdit->text().trimmed().toStdString();
        auto pressure = ui->OlufsenMatProp_Pressure_LineEdit->text().trimmed().toStdString();
        job->SetWallProp("Material Model", MaterialModel::OLUFSEN.toStdString());
        job->SetWallProp("Olufsen Material K1", k1); 
        job->SetWallProp("Olufsen Material K2", k2); 
        job->SetWallProp("Olufsen Material K3", k3); 
        job->SetWallProp("Olufsen Material Exponent", exponent); 
        job->SetWallProp("Olufsen Material Pressure", pressure); 
    }

    else if (materialModel == MaterialModel::LINEAR) { 
        auto Ehr = ui->LinearMatProp_Ehr_LineEdit->text().trimmed().toStdString();
        auto pressure = ui->LinearMatProp_Pressure_LineEdit->text().trimmed().toStdString();
        job->SetWallProp("Material Model", MaterialModel::LINEAR.toStdString());
        job->SetWallProp("Linear Material Ehr", Ehr); 
        job->SetWallProp("Linear Material Pressure", pressure); 
    }

    return true;
}

//-----------------
// CheckInputState
//-----------------
// Check the state of the input data.
//
// Enable GUI push buttons depending on the state of the tool input data. 
// For example, if all inlet / outlet BC and solver paramater data have
// been input then the 'CreateSimulationFilesButton' push button is enabled.
//
bool sv4guiSimulationView1d::CheckInputState(DataInputStateType checkType)
{
    auto msg = "[sv4guiSimulationView1d::CheckInputState] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "#####################################################################";
    //MITK_INFO << msg << "---------- CheckInputState ---------"; 

    ui->CalculateCenterlinesPushButton->setEnabled(false);
    ui->CreateSimulationFilesButton->setEnabled(false);
    ui->RunSimulationPushButton->setEnabled(false);

    //MITK_INFO << msg << "m_ModelInletFaceSelected: " << m_ModelInletFaceSelected;
    //MITK_INFO << msg << "m_CenterlinesCalculated: " << m_CenterlinesCalculated;

    // Check if an inlet face has been selected. 
    if (!m_ModelInletFaceSelected) {
        return false;
    }
    ui->CalculateCenterlinesPushButton->setEnabled(true);

    // Check if centerlines have been generated.
    if (!m_CenterlinesCalculated) { 
        return false;
    }

    // If all the BCs and solver parameters have been set then 
    // allow creating simulation files.
    //
    if (!CheckSolverInputState()) {
        //MITK_INFO << msg << "CheckSolverInputState: false";
        return false;
    } 

    if (!CheckBCsInputState()) { 
        //MITK_INFO << msg << "CheckBCsInputState: false";
        return false;
    }

    ui->CreateSimulationFilesButton->setEnabled(true);

    if (!m_SimulationFilesCreated) { 
        //MITK_INFO << msg << "Simulation files have not been created.";
        return false;
    }

    ui->RunSimulationPushButton->setEnabled(true);

    return true;
}

//--------------------
// CheckBCsInputState
//--------------------
// Check the state of the input boundary conditions data.
//
// The primary check is to make sure all of the data has been set.
//
// The BC data is checked in SetCapBcs() when it is input to the popup
// but input to the Inlet / Outlet BCs table is not so we need to 
// check for valid data here too. 
//  
// There is a problem with the interaction between job props and
// m_TableModelCap, seems to take two passes of creating a job
// before the values are passed to m_TableModelCap. 
//
//
bool sv4guiSimulationView1d::CheckBCsInputState(bool validate)
{
    auto msg = "[sv4guiSimulationView1d::CheckBCsInputState] ";
    //MITK_INFO << msg << "---------- CheckBCsInputState ---------"; 
    //MITK_INFO << msg << "validate: " << validate;
    bool passed = true;
    std::string errorMsg = "";

    // Create map to check that there is one PRESCRIBED_VELOCITIES bc
    // and other bc's of the same type.
    std::map<std::string,int> bcTypeCount;
    for (auto const& bcType : sv4guiCapBCWidget1d::BCType::types) {
        bcTypeCount[bcType] = 0;
    }

    for (int row = 0; row < m_TableModelCap->rowCount(); row ++) {
        QStandardItem* itemName = m_TableModelCap->item(row, TableModelCapType::Name);
        std::string capName = itemName->text().toStdString();
        auto bcType = m_TableModelCap->item(row,TableModelCapType::BCType)->text().toStdString(); 
        auto values = m_TableModelCap->item(row,TableModelCapType::Values)->text().trimmed().toStdString();
        //MITK_INFO << msg << "cap name: " << capName << "   bc type: " << bcType << "  values: " << values; 
        if (!sv4guiCapBCWidget1d::BCType::isValid(bcType) || (values == "")) { 
            //MITK_INFO << msg << "BC type not set";
            passed = false;
            break;
        }
        bcTypeCount[bcType] += 1;

        if (!validate) {
            continue;
        }

        if (bcType == sv4guiCapBCWidget1d::BCType::PRESCRIBED_VELOCITIES) {
            auto flowFile = m_TableModelCap->item(row,TableModelCapType::OriginalFile)->text().toStdString(); 
            auto flowrateContent = m_TableModelCap->item(row,TableModelCapType::FlowRate)->text().trimmed().toStdString();
            auto period = m_TableModelCap->item(row,TableModelCapType::Period)->text().trimmed().toStdString();
            if ((flowrateContent == "") || (period == "")) {
                passed = false;
                break;
            }

        } else if (bcType == sv4guiCapBCWidget1d::BCType::RCR) {
            QStringList list = QString(values.c_str()).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            values = list.join(" ").toStdString();
            int count = 0;
            if (!AreDouble(values,&count) || (count != 3)) {
                //MITK_INFO << msg << "   count: " << count;
                if (count == 3) {
                    errorMsg = capName + ": RCR values are not floats. Values: " + values;
                } else {
                    errorMsg = capName + ": Need three RCR values but " + std::to_string(count) + " were given. Values: " + values; 
                }
                m_TableModelCap->item(row,TableModelCapType::Values)->setText("");
                passed = false;
                break;
            }

        } else if (bcType == sv4guiCapBCWidget1d::BCType::RESISTANCE) {
            /*
            if (!IsDouble(values)) { 
                errorMsg = capName + ": Need one float R value. Values: '" + values + "'"; 
                passed = false;
                break;
            }
            */
        }
    }

    if (bcTypeCount[sv4guiCapBCWidget1d::BCType::PRESCRIBED_VELOCITIES] != 1) {
        errorMsg = "There must be one prescribed velocity boundary condition.";
        passed = false;
    }

    if ((bcTypeCount[sv4guiCapBCWidget1d::BCType::RESISTANCE] != 0) && 
        (bcTypeCount[sv4guiCapBCWidget1d::BCType::RCR] != 0)) {
        errorMsg = "Outlet boundary conditions can not be of mixed type. They must all be of type rcr or resistance.";
        passed = false;
    }

    if (errorMsg != "") {
        QMessageBox::warning(m_Parent, MsgTitle, "Inlet / Outlet BC parameter values error.\n" + QString::fromStdString(errorMsg));
    }

    return passed;
}

//-----------------------
// CheckSolverInputState 
//-----------------------
// Check the state of the input solver parameters data.
//
// The primary check is to make sure all of the data has been set.
//
// We don't need to check the paramaeter values here, this is 
// done in SetSolverParameters().
//
bool sv4guiSimulationView1d::CheckSolverInputState(bool validate)
{
    auto msg = "[sv4guiSimulationView1d::CheckSolverInputState] ";
    //MITK_INFO << msg << "---------- CheckSolverInputState ---------";
    //MITK_INFO << msg << "validate: " << validate;
    bool passed = true;
    std::string errorMsg = "";

    for (int i = 0; i < m_TableModelSolver->rowCount();i++) {
        std::string parName = m_TableModelSolver->item(i,0)->text().trimmed().toStdString();
        QStandardItem* valueItem = m_TableModelSolver->item(i,1);
        //MITK_INFO << msg << "parName: : " << parName << "  value: " << valueItem;
        // Check for section header (e.g. "Time Step Parameters").
        if (valueItem == NULL) {
            continue;
        }
        std::string type = m_TableModelSolver->item(i,2)->text().trimmed().toStdString();
        std::string value = valueItem->text().trimmed().toStdString();
        if (value == "") {
            passed = false;
            break;
        }
    }

    return passed;
}

//---------------
// SetInputState
//---------------
//
void sv4guiSimulationView1d::SetInputState(DataInputStateType checkType, bool value)
{
    for (auto& state : dataInputState) {
        auto stype = std::get<0>(state);
        if (stype == checkType) {
            std::get<2>(state) = value;
            break;
        }
    }
}

//---------------------
// SetSolverParameters
//---------------------
// Set solver parameters in an sv4guiSimJob1d object.
//
// This is called when a parameter value is changed.
//
bool sv4guiSimulationView1d::SetSolverParameters(sv4guiSimJob1d* job, std::string& emsg, bool checkValidity)
{
    auto msg = "[sv4guiSimulationView1d::SetSolverParameters] ";
    //MITK_INFO << msg << "---------- SetSolverParameters ---------"; 

    for (int i = 0; i < m_TableModelSolver->rowCount();i++) {
        std::string parName = m_TableModelSolver->item(i,0)->text().trimmed().toStdString();
        QStandardItem* valueItem = m_TableModelSolver->item(i,1);
        if (valueItem == NULL) {
            continue;
        }

        std::string value = valueItem->text().trimmed().toStdString();
        std::string type = m_TableModelSolver->item(i,2)->text().trimmed().toStdString();
        //MITK_INFO << msg << "type: " << type << "  value: " << value;

        if(checkValidity ) {
            if(value == "") {
                emsg = parName+ " missing value";
                return false;
            } else if(type=="int"&&!IsInt(value)) {
                emsg=parName+ " value error: " + value;
                return false;
            } else if(type=="double"&&!IsDouble(value)) {
                emsg=parName+ " value error: " + value;
                return false;
            }
        }

        job->SetSolverProp(parName, value);
    }

    return true;
}

//---------------
// SaveToManager
//---------------
//
void sv4guiSimulationView1d::SaveToManager()
{
    if (!m_MitkJob) {
        return;
    }

    std::string msg;

    sv4guiSimJob1d* job = CreateJob(msg);

    if (job == NULL) {
        QMessageBox::warning(m_Parent, MsgTitle, "Parameter Values Error.\n"+QString::fromStdString(msg));
        return;
    }

    m_MitkJob->SetSimJob(job);
    m_MitkJob->SetDataModified();
}

//--------------
// SetResultDir
//--------------
// Set the directory containing the 1D simulation results.
//
void sv4guiSimulationView1d::SetResultDir()
{
    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer prefs;

    if (prefService) {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    } else {
        prefs = berry::IPreferences::Pointer(0);
    }

    QString lastFileOpenPath = "";
    QString currentPath=ui->lineEditResultDir->text().trimmed();

    if (currentPath != "" && QDir(currentPath).exists()) {
        lastFileOpenPath=currentPath;
    } else if(prefs.IsNotNull()) {
        lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
    }

    if (lastFileOpenPath == "") {
        lastFileOpenPath=QDir::homePath();
    }

    QString dir = QFileDialog::getExistingDirectory(m_Parent, tr("Choose Result Directory") , lastFileOpenPath);
    dir = dir.trimmed();

    if (dir.isEmpty()) {
        return;
    }

    if (prefs.IsNotNull()) {
        prefs->Put("LastFileOpenPath", dir);
        prefs->Flush();
    }

    QDir rdir(dir);
    if (!rdir.exists()) {
        QMessageBox::warning(m_Parent, "1D Simultation", "The results directory does not exist.");
        return;
    }

    ui->lineEditResultDir->setText(dir);
}

//---------------
// SetConvertDir
//---------------
// Set the directory for writing converted 1D simulation results.
//
// [TODO:DaveP] The code in this function is duplicated all over SV.
// Make this a utility function.
// 
void sv4guiSimulationView1d::SetConvertDir()
{
    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer prefs;

    if (prefService) {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    } else {
        prefs = berry::IPreferences::Pointer(0);
    }

    QString lastFileOpenPath = "";
    QString currentPath = ui->lineEditConvertDir->text().trimmed();

    if (currentPath != "" && QDir(currentPath).exists()) {
        lastFileOpenPath=currentPath;
    } else if(prefs.IsNotNull()) {
        lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
    }

    if (lastFileOpenPath == "") {
        lastFileOpenPath=QDir::homePath();
    }

    QString dir = QFileDialog::getExistingDirectory(m_Parent, tr("Choose directory to write converted files.") , lastFileOpenPath);

    dir = dir.trimmed();

    if (dir.isEmpty()) {
        return;
    }

    if(prefs.IsNotNull()) {
        prefs->Put("LastFileOpenPath", dir);
        prefs->Flush();
    }

    QDir rdir(dir);
    if (!rdir.exists()) {
        QMessageBox::warning(m_Parent, "1D Simultation", "The convert directory does not exist.");
        return;
    }

    ui->lineEditConvertDir->setText(dir);
}

//-------------------------
// SelectSegmentExportType
//-------------------------
//
void sv4guiSimulationView1d::SelectSegmentExportType(int index)
{
    auto msg = "sv4guiSimulationView1d::SelectSegmentExportType";
    MITK_INFO << msg << "--------- SelectSegmentExportType ----------"; 
    //auto type = SegmentExportType::types[index]; 
    auto type = ui->SegmentExportComboBox->currentText();
    MITK_INFO << msg << "Export type: " << type; 
}

//---------------
// ExportResults
//---------------
//
void sv4guiSimulationView1d::ExportResults()
{
    auto msg = "sv4guiSimulationView1d::ExportResults";
    MITK_INFO << msg << "--------- ExportResults ----------"; 

    QString resultDir = ui->lineEditResultDir->text();
    if (resultDir.isEmpty()) { 
        QMessageBox::warning(m_Parent, "1D Simultation", "No results directory has been set.");
        return;
    }

    QString convertDir = ui->lineEditConvertDir->text();
    if (convertDir.isEmpty()) { 
        QMessageBox::warning(m_Parent, "1D Simultation", "No convert directory has been set.");
        return;
    }

    // Get the start/end time for exporting results.
    QString startTimeStr = ui->lineEditStart->text().trimmed();
    auto startTime = std::stod(startTimeStr.toStdString());
    QString stopTimeStr = ui->lineEditStop->text().trimmed();
    auto stopTime = std::stod(stopTimeStr.toStdString());

    if (stopTime < stopTime) { 
        QMessageBox::warning(m_Parent,"1D Simulation", "The stop time must be larger than the start time.");
        return;
    }

   // Create a sv4guiSimulationPythonConvert1d object used to execute a Python 
   // script and get the parameter names used by that script.
   //
   auto pythonInterface = sv4guiSimulationPythonConvert1d();
   auto params = pythonInterface.m_ParameterNames;

   pythonInterface.AddParameter(params.RESULTS_DIRECTORY, resultDir.toStdString());
   pythonInterface.AddParameter(params.SOLVER_FILE_NAME, SOLVER_FILE_NAME.toStdString());

   // Set the data names to convert.
   //
   std::string dataNames;
   auto selectedItems = ui->DataExportListWidget->selectedItems();
   if (selectedItems.size() == 0) { 
        QMessageBox::warning(m_Parent,"1D Simulation", "No data names are selected to convert.");
        return;
   }

   for (auto const& item : selectedItems) {
       auto dataName = item->text().toStdString();
       MITK_INFO << msg << "Selected data name: " << dataName; 
       dataNames += dataName + ",";
   }
   dataNames.pop_back();
   pythonInterface.AddParameter(params.DATA_NAMES, dataNames); 

   // Set time range of data to export.
   auto timeRange = startTimeStr + "," + stopTimeStr;
   pythonInterface.AddParameter(params.TIME_RANGE, timeRange.toStdString()); 

   // Set convert resuls for all or only outlet segments.
   auto segmentExportType = ui->SegmentExportComboBox->currentText();
   if (segmentExportType == sv4guiSimulationView1d::SegmentExportType::ALL) {
       pythonInterface.AddParameter(params.ALL_SEGMENTS, "true"); 
   } else {
     pythonInterface.AddParameter(params.OUTLET_SEGMENTS, "true"); 
   }

   QString jobName("");
   if (m_JobNode.IsNotNull()) {
       jobName = QString::fromStdString(m_JobNode->GetName());
   }
   MITK_INFO << msg << "jobName: " << jobName; 

   convertDir = convertDir + "/" + jobName + "-converted-results";
   QDir exdir(convertDir);
   exdir.mkpath(convertDir);

   pythonInterface.AddParameter(params.OUTPUT_DIRECTORY, convertDir.toStdString());
   pythonInterface.AddParameter(params.OUTPUT_FILE_NAME, jobName.toStdString());

   // Execute the Python script to generate the 1D solver input file.
   //
   // The script writes a log file to the convert directory.
   //
   auto statusMsg = "Converting simulation files ..."; 
   ui->JobStatusValueLabel->setText(statusMsg);
   mitk::StatusBar::GetInstance()->DisplayText(statusMsg);
   auto status = pythonInterface.ConvertResults(convertDir.toStdString());

   if (!status) {
       QMessageBox::warning(NULL, MsgTitle, "Converting 1D solver results has failed.");
       return;
   }

   statusMsg = "1D simulation files have been converted.";
   ui->JobStatusValueLabel->setText(statusMsg);
   mitk::StatusBar::GetInstance()->DisplayText(statusMsg);
}

//---------------------
// GetExportResultsDir
//---------------------
//
QString sv4guiSimulationView1d::GetExportResultsDir()
{
    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer prefs;

    if (prefService) {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    } else {
        prefs = berry::IPreferences::Pointer(0);
    }

    QString lastFileSavePath = "";
    if (prefs.IsNotNull()) {
        lastFileSavePath = prefs->Get("LastFileSavePath", "");
    }

    if (lastFileSavePath == "") {
        lastFileSavePath=QDir::homePath();
    }

    QString exportDir = QFileDialog::getExistingDirectory(m_Parent , tr("Choose Export Directory"), lastFileSavePath);
    exportDir = exportDir.trimmed();

    if (exportDir.isEmpty()) {
        return exportDir;
    }

    if (prefs.IsNotNull()) {
         prefs->Put("LastFileSavePath", exportDir);
         prefs->Flush();
     }

    return exportDir;
}

bool sv4guiSimulationView1d::IsInt(std::string value)
{
    bool ok;
    QString(value.c_str()).toInt(&ok);
    return ok;
}

bool sv4guiSimulationView1d::IsDouble(std::string value)
{
    bool ok;
    QString(value.c_str()).toDouble(&ok);
    return ok;
}

bool sv4guiSimulationView1d::AreDouble(std::string values, int* count)
{
    QStringList list = QString(values.c_str()).split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);
    bool ok;
    for(int i=0;i<list.size();i++)
    {
        list[i].toDouble(&ok);
        if(!ok) return false;
    }

    if(count!=NULL)
        (*count)=list.size();

    return true;
}

//------------
// EnableTool
//------------
// Enable the toolbox pages ('1D Mesh', 'Basic Parameters', etc.)
// to allow input.
//
void sv4guiSimulationView1d::EnableTool(bool able)
{
    ui->widgetTop->setEnabled(able);
    ui->page->setEnabled(able);
    ui->page_2->setEnabled(able);
    ui->page_3->setEnabled(able);
    ui->page_4->setEnabled(able);
    ui->page_5->setEnabled(able);
}

//--------------
// UpdateSimJob
//--------------
// Update a job when a parameter has changed.
//
// This is called when any BC, basic, and solver parameters are changed.
//
void sv4guiSimulationView1d::UpdateSimJob()
{
    auto msg = "[sv4guiSimulationView1d::UpdateSimJob] ";
    MITK_INFO << msg << "---------- UpdateSimJob ---------"; 

    if (!m_MitkJob) {
        return;
    }

    sv4guiSimJob1d* job = m_MitkJob->GetSimJob();

    // Create a new job.
    //
    std::string emsg = "";
    auto validate = false;
    sv4guiSimJob1d* newJob = CreateJob(emsg, validate);

    if (newJob == NULL) {
        //QMessageBox::warning(m_Parent, MsgTitle, "Parameter Values Error.\n"+QString::fromStdString(emsg));
        return;
    }

    m_MitkJob->SetSimJob(newJob);
    m_MitkJob->SetDataModified();

    // Check input state of all data. 
    CheckInputState();
}

#if defined(Q_OS_WIN)
QString sv4guiSimulationView1d::FindLatestKey(QString key, QStringList keys)
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

QString sv4guiSimulationView1d::GetRegistryValue(QString category, QString key)
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
//
void sv4guiSimulationView1d::UpdateJobStatus()
{
    MITK_INFO << "---------- UpdateJobStatus ---------"; 
    if(m_JobNode.IsNull()) {
        return;
    }

    bool running=false;
    double runningProgress=0;
    m_JobNode->GetBoolProperty("running",running);
    m_JobNode->GetDoubleProperty("running progress",runningProgress);

    if(running) {
        ui->JobStatusValueLabel->setText("Simulation running");
        //ui->JobStatusLabel->setText("Simulatin Running: "+QString::number((int)(runningProgress*100))+"% completed");
        ui->widgetRun->setEnabled(false);
    } else {
        ui->JobStatusValueLabel->setText(QString::fromStdString(m_MitkJob->GetStatus()));
        ui->widgetRun->setEnabled(true);
    }

}

