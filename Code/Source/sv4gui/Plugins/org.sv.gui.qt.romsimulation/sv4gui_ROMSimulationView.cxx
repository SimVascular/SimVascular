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

// The sv4guiROMSimulationView class methods defined here provide an interface to
// the 1D Simulation Tool Qt GUI widgets used to create the input files needed to 
// run a 1D simulation. The 1D solver (sv1dsolver) can be run from the GUI or from 
// the command line.
//
// The 1D Simulation Tool uses a surface model created by a Modeling Tool. The surface 
// model is used to create geometry identifying vessel centerlines, branches and
// bifurcations. The centerlines geometry is used to create a 1D finite element mesh.
//
// A Qt connect() must be set for each widget in sv4guiROMSimulationView::EnableConnection(bool able)
// so that its value is saved for the job and written to the .romsimjob file.
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
//        sv4guiROMSimulationExtractCenterlines::Run()
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
//      This uses the sv4guiCapBCWidgetROM class and m_TableModelCap.
//
//      GUI widgets used: 
//        m_TableModelCap - QStandardItemModel which provides a generic model for storing custom data.
//        tableViewCap
//        m_CapBCWidget - sv4guiCapBCWidgetROM
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
//        SetConvertResultsParameters()
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

#include "sv4gui_ROMSimulationView.h"
#include "ui_sv4gui_ROMSimulationView.h"
#include "sv4gui_ROMSimulationPythonConvert.h"

#include "sv4gui_TableCapDelegateROM.h"
#include "sv4gui_TableSolverDelegateROM.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MitkSimJob.h"
#include "sv4gui_MeshLegacyIO.h"
#include "sv4gui_ROMSimulationUtils.h"

#include "sv4gui_ROMSimulationExtractCenterlines.h"
#include "sv_polydatasolid_utils.h"
#include "sv4gui_StringUtils.h"

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

const QString sv4guiROMSimulationView::EXTENSION_ID = "org.sv.views.romsimulation";

// Set the title for QMessageBox warnings.
//
// Note: On MacOS the window title is ignored (as required by the Mac OS X Guidelines). 
const QString sv4guiROMSimulationView::MsgTitle = "SimVascular SV ROM Simulation";

// Set solver default name and install location.
const QString sv4guiROMSimulationView::SOLVER_EXECUTABLE_NAME = "OneDSolver";
const QString sv4guiROMSimulationView::SOLVER_INSTALL_DIRECTORY = "/usr/local/sv/oneDSolver";
const QString sv4guiROMSimulationView::SOLVER_INSTALL_SUB_DIRECTORY = "/bin";
const QString sv4guiROMSimulationView::SOLVER_LOG_FILE_NAME = "svromsolver.log";

// Set the names of the files written as output.
const QString sv4guiROMSimulationView::CORONARY_BC_FILE_NAME = "cort.dat";
const QString sv4guiROMSimulationView::INLET_FACE_NAMES_FILE_NAME = "inlet_face_names.dat";
const QString sv4guiROMSimulationView::MESH_FILE_NAME = "rommesh.vtp";
const QString sv4guiROMSimulationView::MODEL_SURFACE_FILE_NAME = "model_surface.vtp";
const QString sv4guiROMSimulationView::OUTLET_FACE_NAMES_FILE_NAME = "outlet_face_names.dat";
const QString sv4guiROMSimulationView::RCR_BC_FILE_NAME = "rcrt.dat";
const QString sv4guiROMSimulationView::RESISTANCE_BC_FILE_NAME = "resistance.dat";
const QString sv4guiROMSimulationView::SOLVER_0D_FILE_NAME = "solver_0d.in";
const QString sv4guiROMSimulationView::SOLVER_1D_FILE_NAME = "solver_1d.in";

// Set the values of the Surface Model Origin types.
const QString sv4guiROMSimulationView::SurfaceModelSource::MESH_PLUGIN = "Mesh Plugin";
const QString sv4guiROMSimulationView::SurfaceModelSource::MODEL_PLUGIN = "Model Plugin";
const QString sv4guiROMSimulationView::SurfaceModelSource::READ_FROM_FILE = "Read from File";
const std::vector<QString> sv4guiROMSimulationView::SurfaceModelSource::types = 
{
    //sv4guiROMSimulationView::SurfaceModelSource::MESH_PLUGIN,
    sv4guiROMSimulationView::SurfaceModelSource::MODEL_PLUGIN
    //sv4guiROMSimulationView::SurfaceModelSource::READ_FROM_FILE
};

// Set the values of the Centerlines Source types.
//
// There are three sources of centerlines:
//   1) Calculate - centerlines are calculated using vmtk.
//   2) Model Plugin - centerlines are calculated using vmtk by the Model Plugin.
//   3) Read from a file - centerlines are read from a VTK .vtp file.
//
const QString sv4guiROMSimulationView::CenterlinesSource::CALCULATE = "Calculate";
const QString sv4guiROMSimulationView::CenterlinesSource::MODEL_PLUGIN = "Model Plugin";
const QString sv4guiROMSimulationView::CenterlinesSource::READ_FROM_FILE = "Read from File";
const std::vector<QString> sv4guiROMSimulationView::CenterlinesSource::types = 
{
    sv4guiROMSimulationView::CenterlinesSource::CALCULATE
    //sv4guiROMSimulationView::CenterlinesSource::MODEL_PLUGIN,
    //sv4guiROMSimulationView::CenterlinesSource::READ_FROM_FILE
};

const QString sv4guiROMSimulationView::DataInputStateName::INLET_FACE = "Inlet face name";
const QString sv4guiROMSimulationView::DataInputStateName::CENTERLINES = "Centerlines";
const QString sv4guiROMSimulationView::DataInputStateName::BOUNDRY_CONDITIONS = "Boundary conditions";
const QString sv4guiROMSimulationView::DataInputStateName::SOLVER_PARAMETERS = "Solver parameters";
const QString sv4guiROMSimulationView::DataInputStateName::SIMULATION_FILES = "Simulation files";

// Set default material model parameters.
//
const QString sv4guiROMSimulationView::MaterialModel::LinearParameters::Ehr = "1.0e7";
const QString sv4guiROMSimulationView::MaterialModel::LinearParameters::referencePressure = "0.0";
//
const QString sv4guiROMSimulationView::MaterialModel::OlufsenParameters::k1 = "0.0";
const QString sv4guiROMSimulationView::MaterialModel::OlufsenParameters::k2 = "-22.5267"; 
const QString sv4guiROMSimulationView::MaterialModel::OlufsenParameters::k3 = "1.0e7";
const QString sv4guiROMSimulationView::MaterialModel::OlufsenParameters::exponent = "1.0";
const QString sv4guiROMSimulationView::MaterialModel::OlufsenParameters::referencePressure = "0.0";

// Set material model names.
//
const QString sv4guiROMSimulationView::MaterialModel::LINEAR = "LINEAR";
const QString sv4guiROMSimulationView::MaterialModel::OLUFSEN = "OLUFSEN";
const std::vector<QString> sv4guiROMSimulationView::MaterialModel::names = 
{
   sv4guiROMSimulationView::MaterialModel::LINEAR, 
   sv4guiROMSimulationView::MaterialModel::OLUFSEN
};

// Set segment export types.
//
const QString sv4guiROMSimulationView::SegmentExportType::ALL = "All";
const QString sv4guiROMSimulationView::SegmentExportType::OUTLET = "Outlet";
const std::vector<QString> sv4guiROMSimulationView::SegmentExportType::types =
{
   sv4guiROMSimulationView::SegmentExportType::ALL,
   sv4guiROMSimulationView::SegmentExportType::OUTLET
};

// Set export data names.
//
const QString sv4guiROMSimulationView::DataExportName::AREA = "area";
const QString sv4guiROMSimulationView::DataExportName::FLOW = "flow";
const QString sv4guiROMSimulationView::DataExportName::PRESSURE = "pressure";
const QString sv4guiROMSimulationView::DataExportName::WSS = "wss";
const QString sv4guiROMSimulationView::DataExportName::RE = "Re";
const std::vector<QString> sv4guiROMSimulationView::DataExportName::names =
{
   sv4guiROMSimulationView::DataExportName::AREA,
   sv4guiROMSimulationView::DataExportName::FLOW,
   sv4guiROMSimulationView::DataExportName::PRESSURE,
   sv4guiROMSimulationView::DataExportName::RE,
   sv4guiROMSimulationView::DataExportName::WSS
};


//------------------------
// sv4guiROMSimulationView
//------------------------
//
sv4guiROMSimulationView::sv4guiROMSimulationView() : ui(new Ui::sv4guiROMSimulationView)
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

    m_ConvertWorker = nullptr;

}

//-------------------------
// ~sv4guiROMSimulationView
//-------------------------
//
sv4guiROMSimulationView::~sv4guiROMSimulationView()
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
// Set slots for changed parameter values to be written the the .romsimjob file. 
//
void sv4guiROMSimulationView::EnableConnection(bool able)
{
    auto slot = SLOT(UpdateSimJob());

    if (able && !m_ConnectionEnabled) {
        connect(ui->ModelOrderOne_RadioButton, SIGNAL(clicked()), this, slot);
        connect(ui->ModelOrderZero_RadioButton, SIGNAL(clicked()), this, slot);

        connect(m_TableModelBasic, SIGNAL(itemChanged(QStandardItem*)), this, slot);
        connect(m_TableModelCap, SIGNAL(itemChanged(QStandardItem*)), this, slot);
        connect(ui->MaterialModelComboBox,SIGNAL(currentIndexChanged(int )), this, slot);
        connect(m_TableModelSolver, SIGNAL(itemChanged(QStandardItem*)), this, slot);
        connect(ui->NumSegmentsLineEdit, SIGNAL(textChanged(QString)), this, slot);
        connect(ui->AdaptiveMeshingCheckBox, SIGNAL(stateChanged(int)), this, slot);

        // Convert results.
        connect(ui->ProjectCenterlines_CheckBox, SIGNAL(stateChanged(int)), this, slot);
        connect(ui->ExportNumpy_CheckBox, SIGNAL(stateChanged(int)), this, slot);
        connect(ui->ProjectTo3DMesh_CheckBox, SIGNAL(stateChanged(int)), this, slot);
        connect(ui->SimName_ComboBox, SIGNAL(currentTextChanged(QString)), this, slot);

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
void sv4guiROMSimulationView::CreateQtPartControl( QWidget *parent )
{
    auto msg = "[sv4guiROMSimulationView::CreateQtPartControl] ";
    //MITK_INFO << msg << "--------- CreateQtPartControl ----------"; 
    m_Parent=parent;
    ui->setupUi(parent);

    // Hide Job Status for now, can't get it to work. 
    //ui->JobStatusNameLabel->hide();
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
    sv4guiTableCapDelegateROM* itemDelegate = new sv4guiTableCapDelegateROM(this);
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

    m_CapBCWidget = new sv4guiCapBCWidgetROM();
    m_CapBCWidget->move(400,400);
    m_CapBCWidget->hide();
    m_CapBCWidget->setWindowFlags(Qt::WindowStaysOnTopHint);
    connect(m_CapBCWidget,SIGNAL(accepted()), this, SLOT(SetCapBC()));

    // Split Resistance BCs.
    m_SplitBCWidget = new sv4guiSplitBCWidgetROM();
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
    sv4guiTableSolverDelegateROM* itemSolverDelegate = new sv4guiTableSolverDelegateROM(this);
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

    ui->ExportNumpy_CheckBox->setChecked(0);
    ui->ProjectCenterlines_CheckBox->setChecked(0);

    connect(ui->btnExportResults, SIGNAL(clicked()), this, SLOT(ExportResults()));

    SetupInternalSolverPaths();

    //get paths for the external solvers
    berry::IPreferences::Pointer prefs = this->GetPreferences();
    berry::IBerryPreferences* berryprefs = dynamic_cast<berry::IBerryPreferences*>(prefs.GetPointer());
    //    InitializePreferences(berryprefs);
    this->OnPreferencesChanged(berryprefs);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//--------------------------
// ShowConvertWorkerMessage
//--------------------------
// The callback executed by an sv4guiConvertWorkerROM object's 'showMessage' event.
//
void sv4guiROMSimulationView::ShowConvertWorkerMessage(const bool errorMsg, const QString& msg)
{
  QMessageBox mb(nullptr);
  mb.setWindowTitle(sv4guiROMSimulationView::MsgTitle);
  mb.setDetailedText(msg);
  mb.setDefaultButton(QMessageBox::Ok);

  if (errorMsg) { 
    mb.setIcon(QMessageBox::Critical);
    mb.setText("Converting reduced-order results files has failed.");
  } else {
    mb.setIcon(QMessageBox::Information);
    mb.setText("Converting reduced-order results files has completed.");
  }

  mb.exec();
}

//--------------------
// ConvertWorkerError
//--------------------
// The callback executed by an sv4guiConvertWorkerROM object's 'error' event.
//
void sv4guiROMSimulationView::ConvertWorkerError(const QString& msg)
{
  ShowConvertWorkerMessage(true, msg);
  ConvertWorkerFinished();
}

//-----------------------
// ConvertWorkerFinished
//-----------------------
// The callback executed by an sv4guiConvertWorkerROM object's 'finish' event.
//
void sv4guiROMSimulationView::ConvertWorkerFinished()
{
  if (m_ConvertWorker != nullptr) {
      delete m_ConvertWorker;
      m_ConvertWorker = nullptr;
  }
}

//----------------
// ToolBoxChanged
//----------------
// Process a toolbox tab change.
//
void sv4guiROMSimulationView::ToolBoxChanged(int index)
{
    // If changed to Convert Results then try to display convert time range.
    //
    if (index == 6) {
        auto numTimeStepsStr = m_TableModelSolver->item(TableModelSolverRow::NumberofTimesteps,1)->text().trimmed();
        auto timeStepStr = m_TableModelSolver->item(TableModelSolverRow::TimeStepSize,1)->text().trimmed();

        if (!(numTimeStepsStr.isEmpty() && timeStepStr.isEmpty())) { 
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
void sv4guiROMSimulationView::Update1DMesh()
{
    auto msg = "[sv4guiROMSimulationView::Update1DMesh] ";
    //MITK_INFO << msg << "--------- Update1DMesh ----------"; 

    if ((m_1DMeshMapper == nullptr) || (m_1DMeshContainer == nullptr)) {
        m_1DMeshContainer = sv4guiSimulationLinesContainer::New();
        //MITK_INFO << msg << "Create m_1DMeshContainer";

        // Create 1D Mesh node under 'Simulations1d' node.
        auto m_1DMeshNode = mitk::DataNode::New();
        m_1DMeshNode->SetData(m_1DMeshContainer);
        m_1DMeshNode->SetVisibility(true);
        m_1DMeshNode->SetName("1D-Mesh");
        auto parentNode = GetDataStorage()->GetNamedNode("ROM Simulations");
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
void sv4guiROMSimulationView::Create1DMeshControls(QWidget *parent)
{
    auto msg = "[sv4guiROMSimulationView::Create1DMeshControls] ";
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

    // Mesh segment parameters. 
    auto numSegsValidator = new QIntValidator(this);
    numSegsValidator->setBottom(1);
    ui->NumSegmentsLineEdit->setValidator(numSegsValidator);

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
void sv4guiROMSimulationView::CreateWallPropertiesControls(QWidget *parent)
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
void sv4guiROMSimulationView::UpdateSurfaceModelSource()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiROMSimulationView::UpdateSurfaceModelSource] ";
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
void sv4guiROMSimulationView::SelectModelFile()
{
    auto msg = "[sv4guiROMSimulationView::SelectModelFile] ";
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
void sv4guiROMSimulationView::WriteModel()
{
    auto msg = "[sv4guiROMSimulationView::WriteModel] ";
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
void sv4guiROMSimulationView::SelectModelInletFaces(bool show)
{
    if (!m_Model) {
        return;
    }

    auto msg = "[sv4guiROMSimulationView::SelectModelFaces] ";
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
void sv4guiROMSimulationView::SetModelInletFaces()
{
    if (!m_Model) {
        return;
    }

    auto msg = "[sv4guiROMSimulationView::SetModelInletFaces] ";
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
void sv4guiROMSimulationView::ShowModel(bool checked)
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
void sv4guiROMSimulationView::ResetModel()
{
    auto msg = "[sv4guiROMSimulationView::ResetModel] ";
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
void sv4guiROMSimulationView::UpdateCenterlinesSource()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiROMSimulationView::UpdateCenterlinesSource] ";
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
void sv4guiROMSimulationView::SetCenterlinesGeometry()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiROMSimulationView::SetCenterlinesGeometry] ";
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
void sv4guiROMSimulationView::CalculateCenterlines()
{
    auto msg = "[sv4guiROMSimulationView::CalculateCenterlines]";
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
    sv4guiROMSimulationExtractCenterlines* extractCenterlines = new sv4guiROMSimulationExtractCenterlines();
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
void sv4guiROMSimulationView::UpdateCenterlines()
{
    auto msg = "[sv4guiROMSimulationView::UpdateCenterlines] ";
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
void sv4guiROMSimulationView::ShowCenterlines(bool checked)
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
void sv4guiROMSimulationView::SelectCenterlinesFile()
{
    auto msg = "[sv4guiROMSimulationView::SelectCenterlinesFile]";
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
vtkSmartPointer<vtkPolyData> sv4guiROMSimulationView::ReadCenterlines(const std::string fileName)
{
    auto msg = "[sv4guiROMSimulationView::ReadCenterlines] ";
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
mitk::DataNode::Pointer sv4guiROMSimulationView::getProjectNode()
{
  //MITK_INFO << "---------- getProjectNode ---------"; 
  //MITK_INFO << "[getProjectNode] m_DataStorage: " << m_DataStorage; 
  if (m_DataStorage == nullptr) {
    return nullptr;
  }
  mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
  //MITK_INFO << "[getProjectNode] isProjFolder: " << isProjFolder; 

  mitk::DataNode::Pointer projFolderNode = m_DataStorage->GetNode(isProjFolder);
  return projFolderNode;
}

// -----------------------
//  GetModelFolderDataNode 
// -----------------------
// Get the pointer to the model folder data node.

mitk::DataNode::Pointer sv4guiROMSimulationView::GetModelFolderDataNode()
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
QString sv4guiROMSimulationView::GetModelFileName()
{
    auto msg = "[sv4guiROMSimulationView::GetModelFileName] ";
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

mitk::DataNode::Pointer sv4guiROMSimulationView::GetMeshFolderDataNode()
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

sv4guiMesh* sv4guiROMSimulationView::GetDataNodeMesh()
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
void sv4guiROMSimulationView::Generate1DMesh()
{
    auto msg = "[sv4guiROMSimulationView::Generate1DMesh] ";
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
    auto pythonInterface = sv4guiROMSimulationPython();
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
void sv4guiROMSimulationView::Show1DMesh()
{
    auto msg = "[sv4guiROMSimulationView::Show1DMesh] ";
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
void sv4guiROMSimulationView::SetElementSize(QString valueArg)
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
vtkSmartPointer<vtkPolyData> sv4guiROMSimulationView::Read1DMesh(const std::string fileName)
{
    auto msg = "[sv4guiROMSimulationView::Read1DMesh] ";
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
void sv4guiROMSimulationView::ReadMesh()
{   
    auto msg = "[sv4guiROMSimulationView::ReadMesh] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "--------- ReadMesh ----------";
}


//--------------------------
// SetupInternalSolverPaths
//--------------------------
// Set the path to the 1D solver.
//
void sv4guiROMSimulationView::SetupInternalSolverPaths()
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
void sv4guiROMSimulationView::OnPreferencesChanged(const berry::IBerryPreferences* prefs)
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
void sv4guiROMSimulationView::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
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
        return;
    }

    // Check that a job nodes exists.
    mitk::DataNode::Pointer jobNode = nodes.front();
    sv4guiMitkROMSimJob* mitkJob = dynamic_cast<sv4guiMitkROMSimJob*>(jobNode->GetData());
    if (!mitkJob) {
        RemoveObservers();
        EnableTool(false);
        m_Parent->setEnabled(false);
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
    if (!QDir(m_PluginOutputDirectory).exists()) {
        QDir().mkdir(m_PluginOutputDirectory);
    }
    
    // Get the model and mesh folder data nodes.
    m_ModelFolderNode = GetModelFolderDataNode();
    m_MeshFolderNode = GetMeshFolderDataNode();

    // Get the model name (set when we create a ROM simulation).
    //
    std::string modelName = mitkJob->GetModelName();

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

        // [DaveP] can't get time the model node's data was last modified.
        //auto lastTimeModified = m_ModelNode->GetDataReferenceChangedTime();
        //auto lastTimeModified = m_ModelNode->GetMTime();
        // auto modeified = m_Model->GetDataModified();  // does not exist.
        //MITK_INFO << msg << "#### The last time the model has been modified: " << lastTimeModified;
        std::string timeModified;
        m_ModelNode->GetStringProperty("time modified", timeModified);

        // The model has changed so reset the data the depends on the surface model. 
        if ((timeModified != "") && (timeModified != m_ModelNodeTimeModified)) {
            resetModel = true;
        }
        m_ModelNodeTimeModified = timeModified;

        if (m_ModelFileName.isEmpty()) {
            m_ModelFileName = GetModelFileName();
        }
        // Check for centerlines created for the model.
        auto rs = GetDataStorage()->GetDerivations(modelNode);
        if (rs->size() > 0) {
            m_ModelCenterlineNodes.clear();
            for (auto const& node : *rs) { 
                auto name = node->GetName();
                m_ModelCenterlineNodes.emplace_back(name, node);
            }
        }
    }

    // Set the mesh node. 
    //
    auto meshNodes = m_DataStorage->GetDerivations(m_MeshFolderNode,mitk::NodePredicateDataType::New("sv4guiMitkMesh"));
    if (meshNodes->size() != 0) {
        m_MeshNodes.clear();
        for (auto const& node : *meshNodes) {
            auto name = node->GetName();
            m_MeshNodes.emplace_back(name, node);
        }
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

    // Set the model order.
    auto model_order = m_MitkJob->GetModelOrder();
    if (model_order == "0") { 
        ui->ModelOrderZero_RadioButton->setChecked(true); 
    } else if (model_order == "1") { 
       ui->ModelOrderOne_RadioButton->setChecked(true);
    }

    EnableConnection(false);

    UpdateModelGUI();

    UpdateGUIBasic();

    UpdateGUIMesh();

    UpdateGUICap();

    UpdateGUIWall();

    UpdateGUISolver();

    UpdateGUIJob();

    UpdateGUIRunDir();

    UpdateGUIConvertResults();

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
void sv4guiROMSimulationView::NodeChanged(const mitk::DataNode* node)
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

void sv4guiROMSimulationView::NodeAdded(const mitk::DataNode* node)
{

}

void sv4guiROMSimulationView::NodeRemoved(const mitk::DataNode* node)
{

}

void sv4guiROMSimulationView::Visible()
{
    OnSelectionChanged(GetDataManagerSelection());
}

void sv4guiROMSimulationView::Hidden()
{
    RemoveObservers();
}

//--------------
// AddObservers
//--------------
//
// Enable notification of Model changes.
//
void sv4guiROMSimulationView::AddObservers()
{
    auto msg = "[sv4guiROMSimulationView::AddObservers] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "--------- AddObservers ----------";

    if(m_ModelNode.IsNotNull()) {
        if(m_ModelNode->GetDataInteractor().IsNull()) {
            m_DataInteractor = sv4guiModelDataInteractor::New();
            m_DataInteractor->LoadStateMachine("sv4gui_ModelInteraction.xml", us::ModuleRegistry::GetModule("sv4guiModuleModel"));
            m_DataInteractor->SetEventConfig("sv4gui_ModelConfig.xml", us::ModuleRegistry::GetModule("sv4guiModuleModel"));
            m_DataInteractor->SetDataNode(m_ModelNode);
        }

        m_ModelNode->SetStringProperty("interactor user", "romsimulation");
        sv4guiModelDataInteractor* interactor = dynamic_cast<sv4guiModelDataInteractor*>(m_ModelNode->GetDataInteractor().GetPointer());
        if (interactor) {
            interactor->SetFaceSelectionOnly();
        }
    }

    if (m_Model && m_ModelSelectFaceObserverTag == -1) {
        itk::SimpleMemberCommand<sv4guiROMSimulationView>::Pointer modelSelectFaceCommand = 
          itk::SimpleMemberCommand<sv4guiROMSimulationView>::New();
        modelSelectFaceCommand->SetCallbackFunction(this, &sv4guiROMSimulationView::UpdateFaceListSelection);
        m_ModelSelectFaceObserverTag = m_Model->AddObserver( sv4guiModelSelectFaceEvent(), modelSelectFaceCommand);
    }
}

//-----------------
// RemoveObservers
//-----------------
//
void sv4guiROMSimulationView::RemoveObservers()
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
        if(user=="romsimulation")
            m_ModelNode->SetDataInteractor(NULL);
    }
    m_DataInteractor=NULL;
}

void sv4guiROMSimulationView::ClearAll()
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
// (in the .romsimjob file) then set that name in the checkable
// rows of the GUI popup table m_ModelFaceSelectionWidget.
//
void sv4guiROMSimulationView::UpdateModelGUI()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiROMSimulationView::UpdateModelGUI] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "--------- UpdateModelGUI ----------";

    /* [DaveP] This does not work well.
    sv4guiROMSimJob* job = m_MitkJob->GetSimJob();
    if (job == NULL) {
        job = new sv4guiROMSimJob();
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
void sv4guiROMSimulationView::UpdateGUIBasic()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiROMSimulationView::UpdateGUIBasic] ";
    //MITK_INFO << msg;
    //MITK_INFO << msg << "--------- UpdateGUIBasic ----------";

    sv4guiROMSimJob* job = m_MitkJob->GetSimJob();

    if (job == NULL) {
        job = new sv4guiROMSimJob();
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
void sv4guiROMSimulationView::TableViewBasicDoubleClicked(const QModelIndex& index)
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
void sv4guiROMSimulationView::UpdateFaceListSelection()
{
    auto msg = "[sv4guiROMSimulationView::UpdateFaceListSelection] ";
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
void sv4guiROMSimulationView::TableCapSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    auto msg = "[sv4guiROMSimulationView::TableCapSelectionChanged] ";
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
void sv4guiROMSimulationView::TableViewCapDoubleClicked(const QModelIndex& index)
{
    auto msg = "[sv4guiROMSimulationView::TableViewCapDoubleClicked] ";
    //MITK_INFO << msg << "--------- TableViewCapDoubleClicked ----------";

    if (index.column()==0) {
        ShowCapBCWidget();
    }
}

void sv4guiROMSimulationView::TableViewCapContextMenuRequested( const QPoint & pos )
{
    m_TableMenuCap->popup(QCursor::pos());
}

//-----------------
// ShowCapBCWidget
//-----------------
// Show the m_CapBCWidget (sv4guiCapBCWidgetROM) popup.
//
// Sets values stored in m_TableModelCap.
//
void sv4guiROMSimulationView::ShowCapBCWidget(bool)
{
    auto msg = "[sv4guiROMSimulationView::ShowCapBCWidget] ";
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

void sv4guiROMSimulationView::SetDistalPressure(bool)
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
// are changed (sv4guiCapBCWidgetROM class).
//
// Modifies:
//   m_TableModelCap
//
void  sv4guiROMSimulationView::SetCapBC()
{
    auto msg = "[sv4guiROMSimulationView::SetCapBC] ";
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
void sv4guiROMSimulationView::ShowSplitBCWidget(QString splitTarget)
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

void sv4guiROMSimulationView::ShowSplitBCWidgetR(bool)
{
    ShowSplitBCWidget("Resistance");
}

void sv4guiROMSimulationView::ShowSplitBCWidgetC(bool)
{
    ShowSplitBCWidget("Capacitance");
}


//------------
// SplitCapBC
//------------
//
void  sv4guiROMSimulationView::SplitCapBC()
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
// The GUI values are updated from sv4guiROMSimJob properties?
//
// Modifies:
//   m_TableModelCap
//
//
void sv4guiROMSimulationView::UpdateGUICap()
{
    if(!m_MitkJob || !m_Model) {
        return;
    }

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) {
        return;
    }

    sv4guiROMSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL) {
        job=new sv4guiROMSimJob();
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
void sv4guiROMSimulationView::SelectMaterialModel(int index)
{
    auto msg = "[sv4guiROMSimulationView::SelectMaterialModel] ";
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
void sv4guiROMSimulationView::TableVarSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
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

void sv4guiROMSimulationView::TableViewVarContextMenuRequested( const QPoint & pos )
{
    m_TableMenuVar->popup(QCursor::pos());
}

void sv4guiROMSimulationView::SetVarThickness(bool)
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

void sv4guiROMSimulationView::SetVarE(bool)
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

//-------------------------
// UpdateGUIConvertResults 
//-------------------------
// Update the convert results panel GUI with values from the .romsimjob file.
//
void sv4guiROMSimulationView::UpdateGUIConvertResults()
{
    auto msg = "[sv4guiROMSimulationView::UpdateGUIConvertResults]";
    MITK_INFO << msg;
    MITK_INFO << msg << "--------- UpdateGUIConvertResults ----------";

    if (!m_MitkJob) {
        return;
    }

    sv4guiROMSimJob* job = m_MitkJob->GetSimJob();
    if (job == nullptr) {
        job = new sv4guiROMSimJob();
    }

    // Set the check boxes.
    //
    auto projCenterlines = job->GetConvertResultsProp("Project Centerlines");
    MITK_INFO << msg << "projCenterlines: " << projCenterlines;
    if (projCenterlines != "") {
        ui->ProjectCenterlines_CheckBox->setChecked(std::stoi(projCenterlines));
    }

    auto exportNumpy = job->GetConvertResultsProp("Export NumPy");
    if (exportNumpy != "") {
        ui->ExportNumpy_CheckBox->setChecked(std::stoi(exportNumpy));
    }

    auto projMesh = job->GetConvertResultsProp("Project To 3D Mesh");
    if (projMesh != "") {
        ui->ProjectTo3DMesh_CheckBox->setChecked(std::stoi(projMesh));
    }

    // Set simulation names for projecting results to a 3D simulation volume mesh.
    //
    auto simNames = GetSimulationNames();
    ui->SimName_ComboBox->clear();
    if (simNames.size() > 0) {
        for (auto const& simName : simNames) {
            ui->SimName_ComboBox->addItem(QString::fromStdString(simName));
        }
        int foundIndex = ui->SimName_ComboBox->findText(QString(simNames[0].c_str()));
        ui->SimName_ComboBox->setCurrentIndex(foundIndex);
    }
}

//---------------
// UpdateGUIMesh
//---------------
// Update the mesh GUI with values from the .romsimjob file.
//
// These parameters were added later so we must check to
// see if they exist to maintain compatibility with older
// files.
//
void sv4guiROMSimulationView::UpdateGUIMesh()
{
    if (!m_MitkJob) {
        return;
    }

    sv4guiROMSimJob* job = m_MitkJob->GetSimJob();

    if (job == nullptr) {
        job = new sv4guiROMSimJob();
    }

    auto numSegements = job->GetMeshProp("Number of segments per branch");
    if (numSegements == "") { 
        numSegements = "1";
    }
    ui->NumSegmentsLineEdit->setText(QString::fromStdString(numSegements));

    auto adaptMeshing = job->GetMeshProp("Adaptive Meshing"); 
    if (adaptMeshing == "1") { 
        ui->AdaptiveMeshingCheckBox->setChecked(1);
    } else {
        ui->AdaptiveMeshingCheckBox->setChecked(0);
    } 

}

//---------------
// UpdateGUIWall
//---------------
//
void sv4guiROMSimulationView::UpdateGUIWall()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiROMSimulationView::UpdateGUIWall] ";
    MITK_INFO << msg << "--------- UpdateGUIWall ----------"; 

    sv4guiROMSimJob* job = m_MitkJob->GetSimJob();

    if (job == NULL) {
        job = new sv4guiROMSimJob();
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
void sv4guiROMSimulationView::UpdateGUISolver()
{
    auto msg = "[sv4guiROMSimulationView::UpdateGUISolver]";
    MITK_INFO << msg;
    MITK_INFO << msg << "--------- UpdateGUISolver ----------"; 
    if (!m_MitkJob) {
        return;
    }

    sv4guiROMSimJob* job = m_MitkJob->GetSimJob();
    if (job == NULL) {
        job = new sv4guiROMSimJob();
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
void sv4guiROMSimulationView::UpdateGUIJob()
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

    sv4guiROMSimJob* job = m_MitkJob->GetSimJob();

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
void sv4guiROMSimulationView::UpdateSurfaceMeshName()
{
    /* [TODO:Davep] May add using mesh later. 
    auto msg = "[sv4guiROMSimulationView::UpdateSurfaceMeshName] ";
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
std::vector<std::string> sv4guiROMSimulationView::GetMeshNames()
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
void sv4guiROMSimulationView::UpdateGUIRunDir()
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

    sv4guiROMSimJob* job=m_MitkJob->GetSimJob();
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
QString sv4guiROMSimulationView::GetJobPath()
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
        rs = GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiROMSimulationFolder"));

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
void sv4guiROMSimulationView::CreateSimulationFiles()
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
void sv4guiROMSimulationView::RunJob()
{
    auto msg = "[sv4guiROMSimulationView::RunJob] ";

    if (!m_MitkJob) {
        return;
    }

    QString jobPath = GetJobPath();
    if ((jobPath == "") || !QDir(jobPath).exists()) {
        QMessageBox::warning(m_Parent,MsgTitle, "Unable to run.\nPlease make sure data files have been created!");
        return;
    }
    MITK_INFO << msg << "Job path: " << jobPath;

    if (ui->ModelOrderZero_RadioButton->isChecked()) { 
        RunZeroDSimulationJob(jobPath);
    } else {
        RunOneDSimulationJob(jobPath);
    }

}

//-----------------------
// RunZeroDSimulationJob
//-----------------------
// Execute a 0D simulation job.
//
// Arguments:
//   jobPath: The simulation results output directory.
//
void sv4guiROMSimulationView::RunZeroDSimulationJob(const QString& jobPath)
{
    // Set job properties used to write solver log.
    //
    m_JobNode->SetStringProperty("output directory", GetJobPath().toStdString().c_str());
    m_JobNode->SetStringProperty("solver log file", sv4guiROMSimulationView::SOLVER_LOG_FILE_NAME.toStdString().c_str());

    sv4guiROMSimJob* job = m_MitkJob->GetSimJob();

    QDir dir(jobPath);
    dir.mkpath(jobPath);

    // Execute the 0D solver.
    auto pythonInterface = sv4guiROMSimulationPython();
    auto statusMsg = "Executing a 0D simulation ..."; 
    ui->JobStatusValueLabel->setText(statusMsg);
    mitk::StatusBar::GetInstance()->DisplayText(statusMsg);
    auto status = pythonInterface.ExecuteZeroDSimulation(jobPath.toStdString(), job);

    statusMsg = "The 0D simulation has completed."; 
    ui->JobStatusValueLabel->setText(statusMsg);
    mitk::StatusBar::GetInstance()->DisplayText(statusMsg);
}

//----------------------
// RunOneDSimulationJob 
//----------------------
// Execute a 1D simulation job.
//
void sv4guiROMSimulationView::RunOneDSimulationJob(const QString& jobPath)
{
    auto msg = "[sv4guiROMSimulationView::RunJob] ";
    //MITK_INFO << msg << "--------- RunJob ----------"; 

    // Get the solver executable.
    auto solverExecutable = GetSolverExecutable();
    if (solverExecutable == nullptr) {
        return; 
    } 

    // Set job properties used to write solver log.
    //
    m_JobNode->SetStringProperty("output directory", GetJobPath().toStdString().c_str());
    m_JobNode->SetStringProperty("solver log file", sv4guiROMSimulationView::SOLVER_LOG_FILE_NAME.toStdString().c_str());

    sv4guiROMSimJob* job = m_MitkJob->GetSimJob();

/*
    if (job) {
        QString tstr = QString::fromStdString(job->GetSolverProp("Number of Timesteps"));
        totalSteps = tstr.toInt();
    }
*/

    mitk::StatusBar::GetInstance()->DisplayText("Running 1D simulation ...");

    QProcess* solverProcess = new QProcess(m_Parent);
    solverProcess->setWorkingDirectory(jobPath);
    solverProcess->setProgram(solverExecutable);

    QStringList arguments;
    arguments << m_SolverInputFile;
    solverProcess->setArguments(arguments);

    int startStep = 0;
    int totalSteps = 2000;

    sv4guiSolverProcessHandlerROM* handler = new sv4guiSolverProcessHandlerROM(solverProcess, m_JobNode, startStep, totalSteps, 
        jobPath, m_Parent);
    handler->Start();
}

//---------------------
// GetSolverExecutable
//---------------------
//
QString sv4guiROMSimulationView::GetSolverExecutable()
{
    auto msg = "[sv4guiROMSimulationView::GetSolverExecutable] ";
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
// A sv4guiROMSimulationPython object is used to execute a Python script to generate 
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
//   CORONARY_BC_FILE_NAME, RCR_BC_FILE_NAME, or RESISTANCE_BC_FILE_NAME 
//
// Modifies:
//   m_SolverInputFile = solverInputFile;
//   m_SimulationFilesCreated = true;
//
bool sv4guiROMSimulationView::CreateDataFiles(QString outputDir, bool outputAllFiles, bool updateJob, bool createFolder)
{
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
    sv4guiROMSimJob* job = CreateJob(jobMsg);

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

   // Create a sv4guiROMSimulationPython used to execute a Python script
   // and get the parameter names used by that script.
    //
    auto pythonInterface = sv4guiROMSimulationPython();
    auto params = pythonInterface.m_ParameterNames;

    // Set the parameters used by the Python script.
    //
    auto modelName = m_ModelNode->GetName();
    pythonInterface.AddParameter(params.MODEL_NAME, modelName);

    auto modelOrder =  m_MitkJob->GetModelOrder();
    pythonInterface.AddParameter(params.MODEL_ORDER, modelOrder);

    auto outDir = outputDir.toStdString();
    pythonInterface.AddParameter(params.OUTPUT_DIRECTORY, outDir);

    pythonInterface.AddParameter(params.UNITS, "cm");
    pythonInterface.AddParameter(params.ELEMENT_SIZE, std::to_string(m_1DMeshElementSize));
    pythonInterface.AddParameter(params.CENTERLINES_INPUT_FILE, m_CenterlinesFileName.toStdString()); 

    // Mesh parameters.
    AddMeshParameters(job, pythonInterface);

    // Set parameter and write outlet face names to a file.
    WriteOutletFaceNames(outputDir, job, pythonInterface);

    // Set bc paramaters and write the bc data files.
    pythonInterface.AddParameter(params.UNIFORM_BC, "false"); 
    pythonInterface.AddParameter(params.OUTFLOW_BC_INPUT_FILE, outputDir.toStdString());
    WriteBCFiles(outputDir, job, pythonInterface);

	QString solverFileName;
	if (modelOrder == "0")
		solverFileName = SOLVER_0D_FILE_NAME;
	if (modelOrder == "1")
		solverFileName = SOLVER_1D_FILE_NAME;
	pythonInterface.AddParameter(params.SOLVER_OUTPUT_FILE,
			solverFileName.toStdString());

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

	m_SolverInputFile = outputDir + "/" + solverFileName;
    ui->RunSimulationPushButton->setEnabled(true);

    statusMsg = "Simulation files have been created."; 
    ui->JobStatusValueLabel->setText(statusMsg);
    mitk::StatusBar::GetInstance()->DisplayText(statusMsg);
    m_SimulationFilesCreated = true;

    return true;
}

//-----------------------------
// AddConvertResultsParameters 
//-----------------------------
// Add parameters used to convert results.
//
void sv4guiROMSimulationView::AddConvertResultsParameters(sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface)
{
    auto params = pythonInterface.m_ParameterNames;

    auto projCenterlines = job->GetConvertResultsProp("Project Centerlines");
    //pythonInterface.AddParameter(params.SEG_MIN_NUM, numSegements);

    auto exportNumpy = job->GetConvertResultsProp("Export NumPy");
    //pythonInterface.AddParameter(params.SEG_MIN_NUM, numSegements);

    auto projMesh = job->GetConvertResultsProp("Project To 3D Mesh");
    //pythonInterface.AddParameter(params.SEG_MIN_NUM, numSegements);

    auto simName = ui->SimName_ComboBox->currentText();
}

//-------------------
// AddMeshParameters
//-------------------
// Add parameters used to generate the 1D mesh.
//
void sv4guiROMSimulationView::AddMeshParameters(sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface)
{
    auto params = pythonInterface.m_ParameterNames;

    // Number of segments per branch.
    auto numSegements = job->GetMeshProp("Number of segments per branch");
    pythonInterface.AddParameter(params.SEG_MIN_NUM, numSegements); 

    // Enable adaptive meshing (true/false).
    auto adaptMeshing = job->GetMeshProp("Adaptive Meshing"); 
    pythonInterface.AddParameter(params.SEG_SIZE_ADAPTIVE, adaptMeshing); 
}

//-----------------------------
// AddWallPropertiesParameters
//-----------------------------
// Add wall properties parameters for the Python script.
//
void sv4guiROMSimulationView::AddWallPropertiesParameters(sv4guiROMSimJob* job, sv4guiROMSimulationPython& pythonInterface)
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
//   coronary, rcr and resistance boundary conditions
//
void sv4guiROMSimulationView::WriteBCFiles(const QString outputDir, sv4guiROMSimJob* job, 
  sv4guiROMSimulationPython& pythonInterface)
{
    // Get the list of BC types files.
    auto resFileName = RESISTANCE_BC_FILE_NAME.toStdString();
    auto rcrFileName = RCR_BC_FILE_NAME.toStdString();
    auto corFileName = CORONARY_BC_FILE_NAME.toStdString();

    std::set<std::string> bcTypes;
    for (int i = 0; i < m_TableModelCap->rowCount(); i++) {
        auto bcType = m_TableModelCap->item(i,1)->text().trimmed().toStdString();
        if (bcType == "RCR") {
            bcTypes.insert(rcrFileName);
        } else if (bcType == "Resistance") {
            bcTypes.insert(resFileName);
        } else if (bcType == "Coronary") {
            bcTypes.insert(corFileName);
        }
    }

    // Write the inflow BC data.
    WriteFlowFile(outputDir, job, pythonInterface);

    // Write resistance BC data.
    if (bcTypes.count(resFileName) != 0) {
        WriteResistanceFile(outputDir, job, pythonInterface);
    }

    // Write RCR BC data.
    if (bcTypes.count(rcrFileName) != 0) {
        WriteRcrFile(outputDir, job, pythonInterface);
    }

    // Write coronary BC data.
    if (bcTypes.count(corFileName) != 0) {
        WriteCoronaryFile(outputDir, job, pythonInterface);
    }

    // Set the list of BC files used to identify different BC types.
    auto params = pythonInterface.m_ParameterNames;
    std::vector<std::string> values;
    for (auto bcType : bcTypes) { 
        values.push_back(bcType);
    }
    pythonInterface.AddParameterList(params.OUTFLOW_BC_TYPE, values); 
}

//---------------
// WriteFlowFile
//---------------
// Write the inlet face flow rate file and set the parameter used 
// by the Python script.
//
// The flow rate file is set by the user using the 'Set Inlet/Outlet BCs' popup 
// called up from the 'Inlet and Outlet BCs' toolbox tab and processed using the
// sv4guiCapBCWidgetROM object. The sv4guiROMSimJob object stores the flow rate
// file name (without path) and its contents.
//
void sv4guiROMSimulationView::WriteFlowFile(const QString outputDir, sv4guiROMSimJob* job, 
    sv4guiROMSimulationPython& pythonInterface)
{
    auto inletFaceName = m_ModelInletFaceNames[0];
    auto flowFileName = job->GetCapProp(inletFaceName, "Original File");
    auto flowFileContent = QString::fromStdString(job->GetCapProp(inletFaceName, "Flow Rate"));
    auto flowFile = outputDir + "/" + QString(flowFileName.c_str());
    QFile flowFileWriter(flowFile);

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

//-------------------
// WriteCoronaryFile
//-------------------
// Write the coronary boundary conditions file.
//
void sv4guiROMSimulationView::WriteCoronaryFile(const QString outputDir, sv4guiROMSimJob* job,
  sv4guiROMSimulationPython& pythonInterface)
{
    // Get the data for the coronary BC.
    auto fileContents = sv4guiROMSimulationUtils::CreateCORTFileContent(job);
    if (fileContents == "") { 
        return;
    }

    // Write the data.
    auto cortBcFileName = outputDir + "/" + CORONARY_BC_FILE_NAME;
    QFile cortrFile(cortBcFileName);
    if (cortrFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QTextStream out(&cortrFile);
        out << QString::fromStdString(fileContents);
        cortrFile.close();
    }
}

//---------------------
// WriteResistanceFile
//---------------------
// Write the resistance boundary conditions file.
//
void sv4guiROMSimulationView::WriteResistanceFile(const QString outputDir, sv4guiROMSimJob* job,
  sv4guiROMSimulationPython& pythonInterface)
{
    std::string bcType = "Resistance";
    auto resBcFileName = outputDir + "/" + RESISTANCE_BC_FILE_NAME;
    QFile resFile(resBcFileName);

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
// Write the RCR boundary conditions file.
//
void sv4guiROMSimulationView::WriteRcrFile(const QString outputDir, sv4guiROMSimJob* job, 
  sv4guiROMSimulationPython& pythonInterface)
{
    QString rcrtFielContent = QString::fromStdString(sv4guiROMSimulationUtils::CreateRCRTFileContent(job));
    if (rcrtFielContent == "") {
        return;
    }

    // Write rcr data.
    auto outflowBcFileName = outputDir + "/" + RCR_BC_FILE_NAME; 
    QFile rcrtFile(outflowBcFileName);
    if (rcrtFile.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QTextStream out(&rcrtFile);
        out << rcrtFielContent;
        rcrtFile.close();
    }
}

//--------------------
// ReadInletFaceNames
//--------------------
// Read the inlet face names.
//
std::vector<std::string> sv4guiROMSimulationView::ReadInletFaceNames(const QString outputDir)
{
    auto msg = "[sv4guiROMSimulationView::ReadInletFaceNames] ";
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
void sv4guiROMSimulationView::WriteInletFaceNames(const QString outputDir)
{
    auto msg = "[sv4guiROMSimulationView::WriteInletFaceNames] ";
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
void sv4guiROMSimulationView::WriteOutletFaceNames(const QString outputDir, sv4guiROMSimJob* job, 
  sv4guiROMSimulationPython& pythonInterface)
{
    auto msg = "[sv4guiROMSimulationView::WriteRcrFileWriteOutletFaceNames] ";
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
sv4guiMesh* sv4guiROMSimulationView::GetSurfaceMesh(const std::string meshName)
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


void sv4guiROMSimulationView::ImportFiles()
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
// Creates a sv4guiROMSimJob object and sets all the parameters needed 
// for a simulation in that object.
//
// This is called when any parameter is changed: basic, BCs, solver, ...
//
sv4guiROMSimJob* sv4guiROMSimulationView::CreateJob(std::string& msg, bool checkValidity)
{
    sv4guiROMSimJob* job = new sv4guiROMSimJob();

    // Set the model order.
    if (ui->ModelOrderZero_RadioButton->isChecked()) { 
        m_MitkJob->SetModelOrder("0");
    } else if (ui->ModelOrderOne_RadioButton->isChecked()) { 
        m_MitkJob->SetModelOrder("1");
    }

    if (!SetBasicParameters(job, msg, checkValidity)) {
        delete job;
        return nullptr;
    }

    if (!SetConvertResultsParameters(job, msg, checkValidity)) {
        delete job;
        return nullptr;
    }

    if (!SetMeshParameters(job, msg, checkValidity)) {
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

//-------------------
// SetMeshParameters
//-------------------
// Set the parameters used to generate the FE mesh from centerlines.
//
bool sv4guiROMSimulationView::SetMeshParameters(sv4guiROMSimJob* job, std::string& msg, bool checkValidity)
{
    auto numSegements = ui->NumSegmentsLineEdit->text().toStdString();
    job->SetMeshProp("Number of segments per branch", numSegements); 

    if (ui->AdaptiveMeshingCheckBox->isChecked()) {
        job->SetMeshProp("Adaptive Meshing", "1"); 
    } else {
        job->SetMeshProp("Adaptive Meshing", "0"); 
    }

    return true;
}

//-----------------------------
// SetConvertResultsParameters
//-----------------------------
// Set the parameters used to convert results. 
//
bool sv4guiROMSimulationView::SetConvertResultsParameters(sv4guiROMSimJob* job, std::string& msg, bool checkValidity)
{
    if (ui->ProjectCenterlines_CheckBox->isChecked()) {
        job->SetConvertResultsProp("Project Centerlines", "1");
    } else {
        job->SetConvertResultsProp("Project Centerlines", "0");
    }

    if (ui->ExportNumpy_CheckBox->isChecked()) {
        job->SetConvertResultsProp("Export NumPy", "1");
    } else {
        job->SetConvertResultsProp("Export NumPy", "0");
    }

    if (ui->ProjectTo3DMesh_CheckBox->isChecked()) {
        job->SetConvertResultsProp("Project To 3D Mesh", "1");
    } else {
        job->SetConvertResultsProp("Project To 3D Mesh", "0");
    }

    auto simName = ui->SimName_ComboBox->currentText();
    job->SetConvertResultsProp("Simulation Name", simName.toStdString());

    return true;
}

//--------------------
// SetBasicParameters
//--------------------
// Set the basic physical constants and velocity initial conditions
// for the simulation.
//
// This sets properies in the sv4guiROMSimJob object.
//
// Modifies: 
//   job
//
bool sv4guiROMSimulationView::SetBasicParameters(sv4guiROMSimJob* job, std::string& msg, bool checkValidity)
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
bool sv4guiROMSimulationView::SetCapBcs(sv4guiROMSimJob* job, std::string& msg, bool checkValidity)
{
    auto imsg = "[sv4guiROMSimulationView::SetCapBcs] ";
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
bool sv4guiROMSimulationView::SetWallProperites(sv4guiROMSimJob* job, std::string& errorMsg, bool checkValidity)
{
    auto msg = "[sv4guiROMSimulationView::SetWallProperites] ";
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
bool sv4guiROMSimulationView::CheckInputState(DataInputStateType checkType)
{
    auto msg = "[sv4guiROMSimulationView::CheckInputState] ";
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
bool sv4guiROMSimulationView::CheckBCsInputState(bool validate)
{
    auto msg = "[sv4guiROMSimulationView::CheckBCsInputState] ";
    //MITK_INFO << msg << "---------- CheckBCsInputState ---------"; 
    //MITK_INFO << msg << "validate: " << validate;
    bool passed = true;
    std::string errorMsg = "";

    // Create map to check that there is one PRESCRIBED_VELOCITIES bc
    // and other bc's of the same type.
    std::map<std::string,int> bcTypeCount;
    for (auto const& bcType : sv4guiCapBCWidgetROM::BCType::types) {
        bcTypeCount[bcType] = 0;
    }

    for (int row = 0; row < m_TableModelCap->rowCount(); row ++) {
        QStandardItem* itemName = m_TableModelCap->item(row, TableModelCapType::Name);
        std::string capName = itemName->text().toStdString();
        auto bcType = m_TableModelCap->item(row,TableModelCapType::BCType)->text().toStdString(); 
        auto values = m_TableModelCap->item(row,TableModelCapType::Values)->text().trimmed().toStdString();
        //MITK_INFO << msg << "cap name: " << capName << "   bc type: " << bcType << "  values: " << values; 
        if (!sv4guiCapBCWidgetROM::BCType::isValid(bcType) || (values == "")) { 
            //MITK_INFO << msg << "BC type not set";
            passed = false;
            break;
        }
        bcTypeCount[bcType] += 1;

        if (!validate) {
            continue;
        }

        if (bcType == sv4guiCapBCWidgetROM::BCType::PRESCRIBED_VELOCITIES) {
            auto flowFile = m_TableModelCap->item(row,TableModelCapType::OriginalFile)->text().toStdString(); 
            auto flowrateContent = m_TableModelCap->item(row,TableModelCapType::FlowRate)->text().trimmed().toStdString();
            auto period = m_TableModelCap->item(row,TableModelCapType::Period)->text().trimmed().toStdString();
            if ((flowrateContent == "") || (period == "")) {
                passed = false;
                break;
            }

        } else if (bcType == sv4guiCapBCWidgetROM::BCType::RCR) {
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

        } else if (bcType == sv4guiCapBCWidgetROM::BCType::RESISTANCE) {
            /*
            if (!IsDouble(values)) { 
                errorMsg = capName + ": Need one float R value. Values: '" + values + "'"; 
                passed = false;
                break;
            }
            */
        }
    }

    if (bcTypeCount[sv4guiCapBCWidgetROM::BCType::PRESCRIBED_VELOCITIES] != 1) {
        errorMsg = "There must be one prescribed velocity boundary condition.";
        passed = false;
    }

    // [TODO:DaveP] why was this here?
    /*
    if ((bcTypeCount[sv4guiCapBCWidgetROM::BCType::RESISTANCE] != 0) && 
        (bcTypeCount[sv4guiCapBCWidgetROM::BCType::RCR] != 0)) {
        errorMsg = "Outlet boundary conditions can not be of mixed type. They must all be of type rcr or resistance.";
        passed = false;
    }
    */

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
bool sv4guiROMSimulationView::CheckSolverInputState(bool validate)
{
    auto msg = "[sv4guiROMSimulationView::CheckSolverInputState] ";
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
void sv4guiROMSimulationView::SetInputState(DataInputStateType checkType, bool value)
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
// Set solver parameters in an sv4guiROMSimJob object.
//
// This is called when a parameter value is changed.
//
bool sv4guiROMSimulationView::SetSolverParameters(sv4guiROMSimJob* job, std::string& emsg, bool checkValidity)
{
    auto msg = "[sv4guiROMSimulationView::SetSolverParameters] ";
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
void sv4guiROMSimulationView::SaveToManager()
{
    if (!m_MitkJob) {
        return;
    }

    std::string msg;

    sv4guiROMSimJob* job = CreateJob(msg);

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
void sv4guiROMSimulationView::SetResultDir()
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
void sv4guiROMSimulationView::SetConvertDir()
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
void sv4guiROMSimulationView::SelectSegmentExportType(int index)
{
    auto msg = "sv4guiROMSimulationView::SelectSegmentExportType";
    MITK_INFO << msg << "--------- SelectSegmentExportType ----------"; 
    //auto type = SegmentExportType::types[index]; 
    auto type = ui->SegmentExportComboBox->currentText();
    MITK_INFO << msg << "Export type: " << type; 
}

//---------------
// ExportResults
//---------------
// Process the Convert Results button press.
//
// The 'run_from_c()' function in the 'extract_results.py' script located in the 
// 'sv_rom_extract_results' module is executed to extract solution resulsts and write them
// to files of various formats.
//
void sv4guiROMSimulationView::ExportResults()
{
    auto msg = "sv4guiROMSimulationView::ExportResults";
    MITK_INFO << msg; 
    MITK_INFO << msg << "--------- ExportResults ----------"; 

    QString resultDir = ui->lineEditResultDir->text();
    if (resultDir.isEmpty()) { 
        QMessageBox::warning(m_Parent, "ROM Simultation", "No results directory has been set.");
        return;
    }

    QString convertDir = ui->lineEditConvertDir->text();
    if (convertDir.isEmpty()) { 
        QMessageBox::warning(m_Parent, "ROM Simultation", "No convert directory has been set.");
        return;
    }

    // Get the start/end time for exporting results.
    QString startTimeStr = ui->lineEditStart->text().trimmed();
    auto startTime = std::stod(startTimeStr.toStdString());
    QString stopTimeStr = ui->lineEditStop->text().trimmed();
    auto stopTime = std::stod(stopTimeStr.toStdString());
    if (stopTime < stopTime) { 
        QMessageBox::warning(m_Parent,"ROM Simulation", "The stop time must be larger than the start time.");
        return;
    }

   // Create a sv4guiROMSimulationPythonConvert object used to execute a Python 
   // script and get the parameter names used by that script.
   //
   auto modelOrder =  m_MitkJob->GetModelOrder();
   auto pythonInterface = sv4guiROMSimulationPythonConvert();
   auto params = pythonInterface.m_ParameterNames;

   QString solverFileName;
   if (modelOrder == "0") {
       solverFileName = SOLVER_0D_FILE_NAME;
   }

   if (modelOrder == "1") {
       solverFileName = SOLVER_1D_FILE_NAME;
   }

   pythonInterface.AddParameter(params.MODEL_ORDER, modelOrder);
   pythonInterface.AddParameter(params.RESULTS_DIRECTORY, resultDir.toStdString());
   pythonInterface.AddParameter(params.SOLVER_FILE_NAME, solverFileName.toStdString());

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
   if (segmentExportType == sv4guiROMSimulationView::SegmentExportType::ALL) {
       pythonInterface.AddParameter(params.ALL_SEGMENTS, "true"); 
   } else {
       pythonInterface.AddParameter(params.OUTLET_SEGMENTS, "true"); 
   }

   // Export results as numpy arrays.
   if (ui->ExportNumpy_CheckBox->isChecked()) {
       MITK_INFO << msg << "exportNumpy "; 
   }

   // Set parameters to project results to a 3D simulation mesh.
   //
   if (ui->ProjectTo3DMesh_CheckBox->isChecked()) {
       auto simName = ui->SimName_ComboBox->currentText().toStdString();
       if (simName != "") { 
           std::string volumeMeshPath; 
           std::string wallsMeshPath; 
           GetSimulationMeshPaths(simName, volumeMeshPath, wallsMeshPath); 
           MITK_INFO << msg << "volumeMeshPath: " << volumeMeshPath; 
           MITK_INFO << msg << "wallsMeshPath: " << wallsMeshPath; 
           pythonInterface.AddParameter(params.VOLUME_MESH_FILE, volumeMeshPath); 
           pythonInterface.AddParameter(params.WALLS_MESH_FILE, wallsMeshPath); 

       } else {
           auto modelName = QString(m_MitkJob->GetModelName().c_str());
           QMessageBox::warning(NULL, "", "No simulation job was found for the model '" + modelName + 
               "' used by the 1D simulation.");
           return;
       }
   }

   // Set the name for the centerlines geometry file needed to project results to a mesh. 
   //
   if (ui->ProjectCenterlines_CheckBox->isChecked() || ui->ProjectTo3DMesh_CheckBox->isChecked()) {
       auto inputCenterlinesFile = m_CenterlinesFileName.toStdString();
       MITK_INFO << msg << "inputCenterlinesFile: " << inputCenterlinesFile; 
       pythonInterface.AddParameter(params.CENTERLINES_FILE, inputCenterlinesFile); 
   }

   QString jobName("");
   if (m_JobNode.IsNotNull()) {
       jobName = QString::fromStdString(m_JobNode->GetName());
   }
   MITK_INFO << msg << "jobName: " << jobName; 

   QString modelOrderStr = QString::fromStdString(modelOrder);

   convertDir = convertDir + "/" + jobName + "-converted-results_" + modelOrderStr + "d";
   QDir exdir(convertDir);
   exdir.mkpath(convertDir);

   pythonInterface.AddParameter(params.OUTPUT_DIRECTORY, convertDir.toStdString());
   pythonInterface.AddParameter(params.OUTPUT_FILE_NAME, jobName.toStdString());

   // Create a sv4guiConvertWorkerROM used to convert ROM solver results from a QThread
   // so as not to freeze the SV GUI while executing.
   //
   // The object is created here so the worker, which is executed in a separate thread, 
   // can display messages using QMessage.
   //
   // Note: This may cause SV to hang when importing scipy. 
   //
   #ifdef sv4guiROMSimulationView_ExportResults_use_threads
   m_ConvertWorker = new sv4guiConvertWorkerROM();
   connect(m_ConvertWorker, &sv4guiConvertWorkerROM::showMessage, this, &sv4guiROMSimulationView::ShowConvertWorkerMessage);
   connect(m_ConvertWorker, &sv4guiConvertWorkerROM::finished, this, &sv4guiROMSimulationView::ConvertWorkerFinished);
   connect(m_ConvertWorker, &sv4guiConvertWorkerROM::error, this, &sv4guiROMSimulationView::ConvertWorkerError);
   auto status = pythonInterface.ConvertResultsWorker(m_ConvertWorker, convertDir.toStdString());

   // Run a script as a separate process calling the SV Python interpreter.
   //
   // Note: This fails when importing vtk.
   //
   #elif sv4guiROMSimulationView_ExportResults_use_process

   auto status = pythonInterface.ConvertResultsProcess(convertDir.toStdString());

   // Run the convert within this process, will block GUI.
   //
   #else

   auto status = pythonInterface.ConvertResults(convertDir.toStdString());

   #endif
}

//------------------------
// GetSimulationMeshPaths
//------------------------
//
void sv4guiROMSimulationView::GetSimulationMeshPaths(const std::string& simName, std::string& volumeMeshPath, std::string& wallsMeshPath)
{
    mitk::DataNode::Pointer projFolderNode = getProjectNode();
    std::string projPath = "";
    projFolderNode->GetStringProperty("project path", projPath);

    std::string simMeshPath = projPath + "/Simulations/" + simName + "/mesh-complete/"; 
    volumeMeshPath = simMeshPath + "mesh-complete.mesh.vtu";
    wallsMeshPath = simMeshPath + "walls_combined.vtp";
}

//---------------------
// GetExportResultsDir
//---------------------
//
QString sv4guiROMSimulationView::GetExportResultsDir()
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

bool sv4guiROMSimulationView::IsInt(std::string value)
{
    bool ok;
    QString(value.c_str()).toInt(&ok);
    return ok;
}

bool sv4guiROMSimulationView::IsDouble(std::string value)
{
    bool ok;
    QString(value.c_str()).toDouble(&ok);
    return ok;
}

bool sv4guiROMSimulationView::AreDouble(std::string values, int* count)
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
void sv4guiROMSimulationView::EnableTool(bool able)
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
void sv4guiROMSimulationView::UpdateSimJob()
{
    auto msg = "[sv4guiROMSimulationView::UpdateSimJob] ";
    MITK_INFO << msg << "---------- UpdateSimJob ---------"; 
    if (!m_MitkJob) {
        return;
    }

    sv4guiROMSimJob* job = m_MitkJob->GetSimJob();

    // Create a new job.
    //
    std::string emsg = "";
    auto validate = false;
    sv4guiROMSimJob* newJob = CreateJob(emsg, validate);

    if (newJob == NULL) {
        //QMessageBox::warning(m_Parent, MsgTitle, "Parameter Values Error.\n"+QString::fromStdString(emsg));
        return;
    }

    // Check that there is a simulation for the model used by the 1D simultion.
    //
    if (m_ConnectionEnabled && ui->ProjectTo3DMesh_CheckBox->isChecked()) {
       auto simName = ui->SimName_ComboBox->currentText().toStdString();
       if (simName == "") { 
           auto modelName = QString(m_MitkJob->GetModelName().c_str());
           QMessageBox::warning(NULL, "", "No simulation job was found for the model '" + modelName + 
               "' used by the 1D simulation.");
           ui->ProjectTo3DMesh_CheckBox->setChecked(0);
       }
    }

    m_MitkJob->SetSimJob(newJob);
    m_MitkJob->SetDataModified();

    // Check input state of all data. 
    CheckInputState();
}

#if defined(Q_OS_WIN)
QString sv4guiROMSimulationView::FindLatestKey(QString key, QStringList keys)
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

QString sv4guiROMSimulationView::GetRegistryValue(QString category, QString key)
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
void sv4guiROMSimulationView::UpdateJobStatus()
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

//--------------------
// GetSimulationNames
//--------------------
//
std::vector<std::string> sv4guiROMSimulationView::GetSimulationNames()
{
    MITK_INFO << "--------- GetSimulationNames ----------"; 
    std::string modelName = m_MitkJob->GetModelName();
    std::vector<std::string> simNames;

    auto isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    auto rs = GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);
    if (rs->size() == 0) {
        return simNames;
    }

    auto projFolderNode = rs->GetElement(0);
    rs = GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiSimulationFolder"));
    if (rs->size() == 0) {
        return simNames;
    }

    mitk::DataNode::Pointer simFolderNode = rs->GetElement(0);
    rs = GetDataStorage()->GetDerivations(simFolderNode);
    MITK_INFO << "[GetSimulationNames] num sims: " << rs->size(); 
    auto sim1DModelName = m_ModelNode->GetName();
    MITK_INFO << "[GetSimulationNames] sim1DModelName: " << sim1DModelName; 

    for (int i = 0; i < rs->size(); i++) {
        sv4guiMitkSimJob* simJob = dynamic_cast<sv4guiMitkSimJob*>(rs->GetElement(i)->GetData());
        if (simJob == nullptr) {
            continue; 
        } 
        auto simName = rs->GetElement(i)->GetName();
        MITK_INFO << "[GetSimulationNames] ----- simulation: " << simName << " -----"; 
        auto modelName = simJob->GetModelName();
        auto meshName = simJob->GetMeshName();
        MITK_INFO << "[GetSimulationNames] modelName: " << modelName; 
        MITK_INFO << "[GetSimulationNames] meshName: " << meshName; 
        if (modelName == sim1DModelName) {
            simNames.push_back(simName);
        }
    }

    return simNames;
}


