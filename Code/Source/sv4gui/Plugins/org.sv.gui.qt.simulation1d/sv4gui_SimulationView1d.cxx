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

#include <Python.h>

#include "sv4gui_SimulationView1d.h"
#include "ui_sv4gui_SimulationView1d.h"

#include "sv4gui_TableCapDelegate1d.h"
#include "sv4gui_TableSolverDelegate1d.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MeshLegacyIO.h"
#include "sv4gui_SimulationUtils1d.h"
#include "sv4gui_SimulationPython1d.h"

#include "sv4gui_SimulationExtractCenterlines1d.h"
#include "sv_polydatasolid_utils.h"

#include <QmitkStdMultiWidgetEditor.h>
#include <mitkNodePredicateDataType.h>
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

const QString sv4guiSimulationView1d::EXTENSION_ID = "org.sv.views.simulation1d";
const QString sv4guiSimulationView1d::MESH_FILE_NAME = "mesh1d.vtp";
const QString sv4guiSimulationView1d::SOLVER_FILE_NAME = "solver.in";

// Set the values of the Surface Model Origin types.
//
const QString sv4guiSimulationView1d::SurfaceModelSource::MESH_PLUGIN = "Mesh Plugin";
const QString sv4guiSimulationView1d::SurfaceModelSource::MODEL_PLUGIN = "Model Plugin";
const QString sv4guiSimulationView1d::SurfaceModelSource::READ_FROM_FILE = "Read from File";
const std::vector<QString> sv4guiSimulationView1d::SurfaceModelSource::types = 
{
    sv4guiSimulationView1d::SurfaceModelSource::MESH_PLUGIN,
    sv4guiSimulationView1d::SurfaceModelSource::MODEL_PLUGIN,
    sv4guiSimulationView1d::SurfaceModelSource::READ_FROM_FILE
};

// Set the values of the Centerlines Origin types.
//
const QString sv4guiSimulationView1d::CenterlinesSource::CALCULATE = "Calculate";
const QString sv4guiSimulationView1d::CenterlinesSource::MODEL_PLUGIN = "Model Plugin";
const QString sv4guiSimulationView1d::CenterlinesSource::READ_FROM_FILE = "Read from File";
const std::vector<QString> sv4guiSimulationView1d::CenterlinesSource::types = 
{
    sv4guiSimulationView1d::CenterlinesSource::CALCULATE,
    sv4guiSimulationView1d::CenterlinesSource::MODEL_PLUGIN,
    sv4guiSimulationView1d::CenterlinesSource::READ_FROM_FILE
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

    m_Mesh = nullptr;
    m_MeshFolderNode = nullptr;
    m_MeshNode = nullptr;

    m_1DMeshContainer = nullptr;
    m_1DMeshMapper = nullptr;

    m_CenterlinesContainer = nullptr;
    m_CenterlinesMapper = nullptr;

    m_JobNode = NULL;
    m_DataStorage = nullptr;
    m_DataInteractor=NULL;

    m_ModelSelectFaceObserverTag = -1;
    m_TableModelBasic = NULL;
    m_TableModelCap = NULL;
    m_TableMenuCap = NULL;

    m_TableModelVar=NULL;
    m_TableMenuVar=NULL;

    m_CapBCWidget=NULL;

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
}

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
    if(able && !m_ConnectionEnabled)
    {
        connect(m_TableModelBasic, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));
        connect(m_TableModelCap, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));
        connect(ui->comboBoxWallType,SIGNAL(currentIndexChanged(int )), this, SLOT(UpdateSimJob( )));
        connect(ui->lineEditThickness, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        connect(ui->lineEditE, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        connect(ui->lineEditNu, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        connect(ui->lineEditKcons, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        connect(ui->lineEditWallDensity, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        connect(ui->lineEditPressure, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        connect(m_TableModelVar, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));
        connect(m_TableModelSolver, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));
        //connect(ui->comboBoxMeshName, SIGNAL(currentIndexChanged(int )), this, SLOT(UdpateSimJobMeshName( )));
        connect(ui->sliderNumProcs, SIGNAL(valueChanged(double)), this, SLOT(UpdateSimJobNumProcs()));

        m_ConnectionEnabled=able;
    }

    if(!able && m_ConnectionEnabled)
    {
        disconnect(m_TableModelBasic, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));
        disconnect(m_TableModelCap, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));
        disconnect(ui->comboBoxWallType,SIGNAL(currentIndexChanged(int )), this, SLOT(UpdateSimJob( )));
        disconnect(ui->lineEditThickness, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        disconnect(ui->lineEditE, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        disconnect(ui->lineEditNu, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        disconnect(ui->lineEditKcons, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        disconnect(ui->lineEditWallDensity, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        disconnect(ui->lineEditPressure, SIGNAL(textChanged(QString)), this, SLOT(UpdateSimJob()));
        disconnect(m_TableModelVar, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));
        disconnect(m_TableModelSolver, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateSimJob()));
        //disconnect(ui->comboBoxMeshName, SIGNAL(currentIndexChanged(int )), this, SLOT(UdpateSimJobMeshName( )));
        disconnect(ui->sliderNumProcs, SIGNAL(valueChanged(double)), this, SLOT(UpdateSimJobNumProcs()));

        m_ConnectionEnabled=able;
    }
}

//---------------------
// CreateQtPartControl
//---------------------
// Create connections between GUI events (signals) and callbacks (slots). 
//
void sv4guiSimulationView1d::CreateQtPartControl( QWidget *parent )
{
    auto msg = "[sv4guiSimulationView1d::CreateQtPartControl] ";
    MITK_INFO << msg << "--------- CreateQtPartControl ----------"; 
    m_Parent=parent;
    ui->setupUi(parent);
    ui->btnSave->hide();

    // Show Model checkbox.
    connect(ui->checkBoxShowModel, SIGNAL(clicked(bool)), this, SLOT(ShowModel(bool)) );

    // Set the toolbox ('1D Mesh', 'Basic Parameters', etc. pages) to display the first page.
    ui->toolBox->setCurrentIndex(0);

    // Create 1D Mesh page controls.
    Create1DMeshControls(parent);

    // For basic table.
    m_TableModelBasic = new QStandardItemModel(this);
    ui->tableViewBasic->setModel(m_TableModelBasic);

    connect( ui->tableViewBasic, SIGNAL(doubleClicked(const QModelIndex&))
             , this, SLOT(TableViewBasicDoubleClicked(const QModelIndex&)) );

    // Inlet and Outlet BCs.
    //
    m_TableModelCap = new QStandardItemModel(this);
    ui->tableViewCap->setModel(m_TableModelCap);
    sv4guiTableCapDelegate1d* itemDelegate=new sv4guiTableCapDelegate1d(this);
    ui->tableViewCap->setItemDelegateForColumn(1,itemDelegate);

    connect( ui->tableViewCap->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableCapSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    connect( ui->tableViewCap, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(TableViewCapDoubleClicked(const QModelIndex&)) );

    connect( ui->tableViewCap, SIGNAL(customContextMenuRequested(const QPoint&))
             , this, SLOT(TableViewCapContextMenuRequested(const QPoint&)) );

    m_TableMenuCap=new QMenu(ui->tableViewCap);
    QAction* setBCAction=m_TableMenuCap->addAction("Set BC");
    QAction* setPressureAction=m_TableMenuCap->addAction("Set Distal Pressure");
    connect( setBCAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowCapBCWidget(bool) ) );
    connect( setPressureAction, SIGNAL( triggered(bool) ) , this, SLOT( SetDistalPressure(bool) ) );

    QAction* splitBCRAction=m_TableMenuCap->addAction("Split Resistance");
    QAction* splitBCCAction=m_TableMenuCap->addAction("Split Capacitance");
    connect( splitBCRAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowSplitBCWidgetR(bool) ) );
    connect( splitBCCAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowSplitBCWidgetC(bool) ) );

    m_CapBCWidget=new sv4guiCapBCWidget1d();
    m_CapBCWidget->move(400,400);
    m_CapBCWidget->hide();
    m_CapBCWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_CapBCWidget,SIGNAL(accepted()), this, SLOT(SetCapBC()));

    // Split Resistance BCs.
    m_SplitBCWidget=new sv4guiSplitBCWidget1d();
    m_SplitBCWidget->move(400,400);
    m_SplitBCWidget->hide();
    m_SplitBCWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_SplitBCWidget,SIGNAL(accepted()), this, SLOT(SplitCapBC()));

    //for wall panel and var table
    connect(ui->comboBoxWallType,SIGNAL(currentIndexChanged(int )), this, SLOT(WallTypeSelectionChanged(int )));

    ui->widgetConstant->hide();
    ui->widgetVariable->hide();

    m_TableModelVar = new QStandardItemModel(this);
    ui->tableViewVar->setModel(m_TableModelVar);

    connect( ui->tableViewVar->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableVarSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    connect( ui->tableViewVar, SIGNAL(customContextMenuRequested(const QPoint&))
             , this, SLOT(TableViewVarContextMenuRequested(const QPoint&)) );

    m_TableMenuVar=new QMenu(ui->tableViewVar);
    QAction* setVarThicknessAction=m_TableMenuVar->addAction("Set Thickness");
    QAction* setVarEAction=m_TableMenuVar->addAction("Set Elastic Modulus ");
    connect( setVarThicknessAction, SIGNAL( triggered(bool) ) , this, SLOT( SetVarThickness(bool) ) );
    connect( setVarEAction, SIGNAL( triggered(bool) ) , this, SLOT( SetVarE(bool) ) );

    //for solver table
    m_TableModelSolver = new QStandardItemModel(this);
    ui->tableViewSolver->setModel(m_TableModelSolver);
    sv4guiTableSolverDelegate1d* itemSolverDelegate=new sv4guiTableSolverDelegate1d(this);
    ui->tableViewSolver->setItemDelegateForColumn(1,itemSolverDelegate);

    //for data file and run
//    connect(ui->btnExportInputFiles, SIGNAL(clicked()), this, SLOT(ExportInputFiles()) );
//    connect(ui->btnExportAllFiles, SIGNAL(clicked()), this, SLOT(ExportAllFiles()) );
    connect(ui->btnCreateAllFiles, SIGNAL(clicked()), this, SLOT(CreateAllFiles()) );
    connect(ui->btnRunJob, SIGNAL(clicked()), this, SLOT(RunJob()) );

    //for export results
    connect(ui->toolButtonResultDir, SIGNAL(clicked()), this, SLOT(SetResultDir()) );
    connect(ui->btnExportResults, SIGNAL(clicked()), this, SLOT(ExportResults()) );

//    ui->widgetCalculateFlows->hide();
    connect(ui->checkBoxCalculateFlows, SIGNAL(clicked(bool)), this, SLOT(ShowCalculateFowsWidget(bool)) );

    SetupInternalSolverPaths();

    //get paths for the external solvers
    berry::IPreferences::Pointer prefs = this->GetPreferences();
    berry::IBerryPreferences* berryprefs = dynamic_cast<berry::IBerryPreferences*>(prefs.GetPointer());
    //    InitializePreferences(berryprefs);
    this->OnPreferencesChanged(berryprefs);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//--------------
// Update1DMesh
//--------------
// Create container and mapper used to display the 1D mesh.
//
void sv4guiSimulationView1d::Update1DMesh()
{
    if (m_1DMeshMapper == nullptr) {
        m_1DMeshContainer = sv4guiSimulationLinesContainer::New();

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
// for the '1D Mesh' page.
//
void sv4guiSimulationView1d::Create1DMeshControls(QWidget *parent)
{
    auto msg = "[sv4guiSimulationView1d::Create1DMeshControls] ";
    MITK_INFO << msg << "--------- Create1DMeshControls ----------"; 
    connect(ui->generateMeshPushButton, SIGNAL(clicked()), this, SLOT(Generate1DMesh()));
    //connect(ui->readMeshPushButton, SIGNAL(clicked()), this, SLOT(ReadMesh()));

    // Add surface model widgets.
    //
    connect(ui->surfaceModelComboBox, SIGNAL(currentIndexChanged(int )), this, SLOT(UdpateSurfaceModelSource( )));
    connect(ui->readModelPushButton, SIGNAL(clicked()), this, SLOT(ReadModel()) );
    connect(ui->comboBoxMeshName, SIGNAL(currentIndexChanged(int)), this, SLOT(UdpateSurfaceMeshName()));

    for (auto const& type : SurfaceModelSource::types) {
        ui->surfaceModelComboBox->addItem(type);
    }

    m_ModelSource = SurfaceModelSource::MODEL_PLUGIN; 
    ui->surfaceModelComboBox->setCurrentText(m_ModelSource);
    ui->readModelPushButton->setVisible(false);
    ui->modelFileNameLabel->setVisible(false);
    ui->modelFileNameLineEdit->setVisible(false);
    ui->meshNameLabel->setVisible(false);
    ui->comboBoxMeshName->setVisible(false);

    // Add centerlines widgets.
    //
    connect(ui->centerlinesComboBox, SIGNAL(currentIndexChanged(int )), this, SLOT(UdpateCenterlinesSource()));
    connect(ui->readCenterlinesPushButton, SIGNAL(clicked()), this, SLOT(SelectCenterlinesFile()));
    connect(ui->calculateCenterlinesPushButton, SIGNAL(clicked()), this, SLOT(CalculateCenterlines()) );
    connect(ui->selectModelFacesPushButton, SIGNAL(clicked()), this, SLOT(SelectModelFaces()));
    for (auto const& type : CenterlinesSource::types) {
        ui->centerlinesComboBox->addItem(type);
    }
    m_CenterlinesSource = CenterlinesSource::CALCULATE; 
    ui->centerlinesComboBox->setCurrentText(m_CenterlinesSource);
    ui->readCenterlinesPushButton->setVisible(false);
    ui->centerlinesFileNameLabel->setVisible(false);
    ui->centerlinesFileNameLineEdit->setVisible(false);
    ui->calculateCenterlinesPushButton->setVisible(true);
    ui->calculateCenterlinesPushButton->setEnabled(false);

    // Add model face selection widget.
    m_ModelFaceSelectionWidget = new sv4guiCapSelectionWidget();
    m_ModelFaceSelectionWidget->move(400,400);
    m_ModelFaceSelectionWidget->hide();
    m_ModelFaceSelectionWidget->setWindowFlags(Qt::WindowStaysOnTopHint);
    // Set callback when 'OK' button is selected. 
    connect(m_ModelFaceSelectionWidget, SIGNAL(accepted()), this, SLOT(AddModelFaces()));
}

//--------------------------
// UdpateSurfaceModelSource 
//--------------------------
// Set the source of a surface model.
//
// Modifies:
//     m_ModelSource 
//
void sv4guiSimulationView1d::UdpateSurfaceModelSource()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::UdpateSurfaceModelSource] ";
    MITK_INFO << msg;
    MITK_INFO << msg << "---------- UdpateSurfaceModelSource ----------";

    auto sourceType = ui->surfaceModelComboBox->currentText();
    //std::string sourceType = ui->surfaceModelComboBox->currentText().toStdString();
    MITK_INFO << msg << "sourceType: " << sourceType;

    auto showModel = (sourceType == SurfaceModelSource::MODEL_PLUGIN);

    auto showRead = (sourceType == SurfaceModelSource::READ_FROM_FILE);
    ui->readModelPushButton->setVisible(showRead);
    ui->modelFileNameLabel->setVisible(showRead);
    ui->modelFileNameLineEdit->setVisible(showRead);

    auto showMesh = (sourceType == SurfaceModelSource::MESH_PLUGIN);
    ui->meshNameLabel->setVisible(showMesh);
    ui->comboBoxMeshName->setVisible(showMesh);

    m_ModelSource = sourceType; 
}

//-----------
// ReadModel
//-----------
//
// Sets:
//    m_ModelFileName
//
void sv4guiSimulationView1d::ReadModel()
{
    auto msg = "[sv4guiSimulationView1d::ReadModel] ";
    MITK_INFO << msg;
    MITK_INFO << msg << "---------- ReadModel ----------";

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
  
        MITK_INFO << msg << "Read surface model: " << m_ModelFileName.toStdString();
        QFile file(m_ModelFileName);
        QFileInfo fileInfo(file);
        ui->modelFileNameLineEdit->setText(fileInfo.fileName());
  
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

//------------------
// SelectModelFaces
//------------------
//
void sv4guiSimulationView1d::SelectModelFaces()
{
    if (!m_Model) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::SelectModelFaces] ";
    MITK_INFO << msg << "--------- SelectModelFaces ----------"; 

/*
    if (m_ModelType != "PolyData") {
      QMessageBox::warning(m_Parent,"Error","Cannot currently extract centerlines of anyting other than a PolyData model");
      return;
    }

    int timeStep=GetTimeStep();
*/
    int timeStep = 0;

    sv4guiModelElement* modelElement = m_Model->GetModelElement(timeStep);
    std::vector<sv4guiModelElement::svFace*> faces = modelElement->GetFaces();
    std::vector<std::string> caps;
    MITK_INFO << msg << "Number of model faces: " << faces.size();

    int rowIndex=-1;

    for(int i = 0; i < faces.size(); i++) {
        if (faces[i] == NULL) {
            continue;
        }

        if (faces[i]->type == "cap") {
            caps.push_back(faces[i]->name);
        }
    }

    m_ModelFaceSelectionWidget->SetTableView(caps, modelElement, "PolyData");
    //m_ModelFaceSelectionWidget->SetTableView(caps, modelElement, m_ModelType);
    m_ModelFaceSelectionWidget->show();
}

//---------------
// AddModelFaces
//---------------
// Save the names and face IDs for the inlet faces selected 
// from the m_ModelFaceSelectionWidget.
//
// Modifies:
//   m_ModelInletFaceNames
//   m_ModelInletFaceIds
//
void sv4guiSimulationView1d::AddModelFaces()
{
    if (!m_Model) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::AddModelFaces] ";
    MITK_INFO << msg << "--------- AddModelFaces ----------"; 
    int timeStep = 0;
    sv4guiModelElement* modelElement = m_Model->GetModelElement(timeStep);
    std::vector<std::string> capNames = m_ModelFaceSelectionWidget->GetUsedCapNames();
    MITK_INFO << msg << "Number of inlet faces selected: " << capNames.size(); 

    for (const auto& name : capNames) {
        MITK_INFO << msg << "Inlet face: " << name; 
        m_ModelInletFaceNames.push_back(name);
        m_ModelInletFaceIds.push_back(modelElement->GetFaceID(name));
    }

    // Enable execute centerlines button.
    ui->calculateCenterlinesPushButton->setEnabled(true);
}

//-------------------------
// UdpateCenterlinesSource 
//-------------------------
// Update GUI depending on centerlines source.
//
// Sets:
//   m_CenterlinesSource 
//
void sv4guiSimulationView1d::UdpateCenterlinesSource()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::UdpateCenterlinesSource] ";
    MITK_INFO << msg;
    MITK_INFO << msg << "---------- UdpateCenterlinesSource ----------";

    auto sourceType = ui->centerlinesComboBox->currentText();
    MITK_INFO << msg << "sourceType: " << sourceType;
    m_CenterlinesSource = sourceType;

    // Show or hide widgets depending on centerline source.
    //
    auto showRead = (sourceType == CenterlinesSource::READ_FROM_FILE);
    ui->readCenterlinesPushButton->setVisible(showRead);
    ui->centerlinesFileNameLabel->setVisible(showRead);
    ui->centerlinesFileNameLineEdit->setVisible(showRead);

    auto showCalculate = (sourceType == CenterlinesSource::CALCULATE);
    ui->selectModelFacesPushButton->setVisible(showCalculate);
    ui->calculateCenterlinesPushButton->setVisible(showCalculate);
    ui->calculateCenterlinesPushButton->setEnabled(false);
}

//----------------------
// CalculateCenterlines
//----------------------
//
void sv4guiSimulationView1d::CalculateCenterlines()
{
    auto msg = "[sv4guiSimulationView1d::CalculateCenterlines]";
    MITK_INFO << msg;
    MITK_INFO << msg << "---------- CalculateCenterlines ----------";
    MITK_INFO << msg << "Number of inlet faces: " <<  m_ModelInletFaceIds.size();

    if (m_ModelInletFaceIds.size() == 0) {
        MITK_WARN << "No inlet faces selected.";
        QMessageBox::warning(NULL, "1D Simulation", "No inlet faces selected.");
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
    auto msg = "[sv4guiSimulationView1d::UpdateCenterlines]";
    MITK_INFO << msg;
    MITK_INFO << msg << "---------- Update ----------";
    MITK_INFO << msg << "Centerlines file: " << m_CenterlinesFileName;

    // Create the container and mapper used to display the centerlines.
    //
    if (m_CenterlinesMapper == nullptr) {
        m_CenterlinesContainer = sv4guiSimulationLinesContainer::New();

        // Create 'Centerlines' node under 'Simularions1d' node.
        auto m_CenterlinesNode = mitk::DataNode::New();
        m_CenterlinesNode->SetData(m_CenterlinesContainer);
        m_CenterlinesNode->SetVisibility(true);
        m_CenterlinesNode->SetName("Centerlines");
        auto parentNode = GetDataStorage()->GetNamedNode("Simulations1d");
        if (parentNode) {
          GetDataStorage()->Add(m_CenterlinesNode, parentNode);
        }

        // Create mapper to display the centerlines.
        m_CenterlinesMapper = sv4guiSimulationLinesMapper::New();
        m_CenterlinesMapper->SetDataNode(m_CenterlinesNode);
        m_CenterlinesMapper->m_box = false;
        m_CenterlinesMapper->SetColor(0.0, 1.0, 0.0);
        m_CenterlinesNode->SetMapper(mitk::BaseRenderer::Standard3D, m_CenterlinesMapper);
    }

    auto centerlinesGeometry = ReadCenterlines(m_CenterlinesFileName.toStdString());

    if (centerlinesGeometry != nullptr) { 
        m_CenterlinesContainer->SetMesh(centerlinesGeometry);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

//-----------------------
// SelectCenterlinesFile 
//-----------------------
// Select a centerlines file using a file browser.
//
// Modifies:
//    m_CenterlinesFileName 
//
void sv4guiSimulationView1d::SelectCenterlinesFile()
{
    auto msg = "[sv4guiSimulationView1d::SelectCenterlinesFile]";
    MITK_INFO << msg;
    MITK_INFO << msg << "---------- SelectCenterlinesFile ----------";

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
        ui->centerlinesFileNameLineEdit->setText(m_CenterlinesFileName);
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
    MITK_INFO << msg << "---------- ReadCenterlines ----------";
    MITK_INFO << msg << "Read centerlines file: " << fileName;

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
//
// Get the name of the file containing the model surface mesh.
//
QString sv4guiSimulationView1d::GetModelFileName()
{
    auto msg = "[sv4guiSimulationView1d::GetModelFileName] ";
    MITK_INFO << msg << "Model source '" << m_ModelSource << "'";
    QString modelFileName;

    if (m_ModelSource == SurfaceModelSource::MODEL_PLUGIN) { 
        auto modelName = QString::fromStdString(m_ModelNode->GetName());
        MITK_INFO << msg << "Model name '" << modelName << "'";
        auto projFolderNode = getProjectNode();

        std::string modelPath = "";
        m_ModelNode->GetStringProperty("path", modelPath);
        MITK_INFO << msg << "modelPath " << modelPath;

        // Set surface mesh name.
        modelFileName = QString::fromStdString(modelPath) + QDir::separator() + modelName + ".vtp";
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
    MITK_INFO << msg;
    MITK_INFO << msg << "---------- Generate1DMesh ----------";

    if (m_Model == nullptr) { 
        return;
    }

    MITK_INFO << msg << "Output directory: " << m_PluginOutputDirectory;

    // Get the file name of the surface model.
    auto modelFileName = m_ModelFileName;
    MITK_INFO << msg << "Model file: " << modelFileName;

    if (m_CenterlinesFileName.isEmpty()) {
        QMessageBox::warning(NULL, "1D Simulation", "No centerlines have been calculated or centerlines source file set.");
        MITK_ERROR << "No centerlines file is defined.";
        return;
    }
    MITK_INFO << msg << "Centerlines file: " << m_CenterlinesFileName;

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
// Read1DMesh
//------------
// Read a 1D mesh into a vtkPolyData object and
// display it.
//
vtkSmartPointer<vtkPolyData> sv4guiSimulationView1d::Read1DMesh(const std::string fileName)
{
    auto msg = "[sv4guiSimulationView1d::Read1DMesh] ";
    MITK_INFO << msg; 
    MITK_INFO << msg << "---------- Read1DMesh ---------- "; 
    MITK_INFO << msg << "Read mesh file name: " << fileName;

    // Read 1D mesh into vtkPolyData object.
    //
    vtkSmartPointer<vtkPolyData> geom = vtkPolyData::New();

    if (PlyDtaUtils_ReadNative(const_cast<char *>(fileName.c_str()), geom) != SV_OK) {
        MITK_WARN << msg << "Unable to read 1D mesh " << fileName;
        return nullptr;
    }
    MITK_INFO << msg << "Done! ";
    return geom;
}

//----------
// ReadMesh
//-----------
//
void sv4guiSimulationView1d::ReadMesh()
{   
    auto msg = "[sv4guiSimulationView1d::ReadMesh] ";
    MITK_INFO << msg;
    MITK_INFO << msg << "--------- ReadMesh ----------";
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

    if(!IsVisible()) {
        return;
    }

    if (nodes.size() == 0) {
        RemoveObservers();
        EnableTool(false);
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
        return;
    }
    
    std::string jobPath = "";
    jobNode->GetStringProperty("path", jobPath);
    MITK_INFO << msg << "jobPath " << jobPath;
    //m_PluginOutputDirectory = GetJobPath();
    m_PluginOutputDirectory = QString(jobPath.c_str());

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

    if (model != nullptr) {
        modelNode->SetVisibility(false);
        m_Model = model;
        m_ModelNode = modelNode;
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
            MITK_INFO << msg << "Don't have centerlines.";
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

    if (m_JobNode.IsNotNull()) {
        RemoveObservers();
    }

    m_JobNode = jobNode;
    m_MitkJob = mitkJob;

    // Enable the toolbox pages ('1D Mesh', 'Basic Parameters', etc.)
    // to allow input.
    if(m_Model == NULL) {
        EnableTool(false);
    } else {
        EnableTool(true);
        AddObservers();
    }

    // Update main GUI panel upper section.
    //
    ui->labelJobName->setText(QString::fromStdString(m_JobNode->GetName()));
    ui->labelJobStatus->setText(QString::fromStdString(m_MitkJob->GetStatus()));
    ui->checkBoxShowModel->setChecked(false);
    if(m_ModelNode.IsNotNull()) {
        ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
        if(m_ModelNode->IsVisible(NULL)) {
            ui->checkBoxShowModel->setChecked(true);
        }
    } else {
        ui->labelModelName->setText("No model found");
    }

    EnableConnection(false);

    UpdateGUIBasic();

    UpdateGUICap();

    UpdateGUIWall();

    UpdateGUISolver();

    UpdateGUIJob();

    UpdateGUIRunDir();

    UpdateFaceListSelection();

    UpdateJobStatus();

    EnableConnection(true);

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
        if(updateRunDir)
        {
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
    MITK_INFO << msg;
    MITK_INFO << msg << "--------- AddObservers ----------";

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
    ui->labelJobStatus->setText("");
    ui->labelModelName->setText("");
}

//----------------
// UpdateGUIBasic
//----------------
//
// Update the 'Basic Paramaters' GUI page.
//
void sv4guiSimulationView1d::UpdateGUIBasic()
{
    if (!m_MitkJob) {
        return;
    }

    auto msg = "[sv4guiSimulationView1d::UpdateGUIBasic] ";
    MITK_INFO << msg;
    MITK_INFO << msg << "--------- UpdateGUIBasic ----------";

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

    parList<<new QStandardItem("IC File");
    value=QString::fromStdString(job->GetBasicProp("IC File"));
    valueList<<new QStandardItem(value);

    parList<<new QStandardItem("Initial Pressure");
    value=QString::fromStdString(job->GetBasicProp("Initial Pressure"));
    valueList<<new QStandardItem(value==""?QString("0"):value);

    parList<<new QStandardItem("Initial Velocities");
    value=QString::fromStdString(job->GetBasicProp("Initial Velocities"));
//    valueList<<new QStandardItem(value==""?QString("0 0 0"):value);
    valueList<<new QStandardItem(value==""?QString("0.0001 0.0001 0.0001"):value);

    for(int i=0;i<parList.size();i++)
    {
        parList[i]->setEditable(false);
        m_TableModelBasic->setItem(i, 0, parList[i]);
        m_TableModelBasic->setItem(i, 1, valueList[i]);
    }

    ui->tableViewBasic->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->tableViewBasic->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
}

void sv4guiSimulationView1d::TableViewBasicDoubleClicked(const QModelIndex& index)
{
    if(index.column()!=0)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewBasic->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
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
// been modified in the its Model plugin. 
//
void sv4guiSimulationView1d::UpdateFaceListSelection()
{
    auto msg = "[sv4guiSimulationView1d::UpdateFaceListSelection] ";
    MITK_INFO << msg;
    MITK_INFO << msg << "--------- UpdateFaceListSelection ----------";

    if (!m_Model) {
        return;
    }

    sv4guiModelElement* modelElement = m_Model->GetModelElement();

    if (modelElement == NULL) {
        return;
    }

    // Update the tableViewCap GUI object for inlet/outlet BCs.
    //
    disconnect( ui->tableViewCap->selectionModel()
                , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
                , this
                , SLOT( TableCapSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    ui->tableViewCap->clearSelection();

    int count=m_TableModelCap->rowCount();

    for(int i=0; i<count; i++) {
        QStandardItem* itemName = m_TableModelCap->item(i,0);
        std::string name = itemName->text().toStdString();

        if(modelElement->IsFaceSelected(name)) {
            MITK_INFO << msg << "Face is selected " << name;
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

}

//--------------------------
// TableCapSelectionChanged
//--------------------------
//
void sv4guiSimulationView1d::TableCapSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    auto msg = "[sv4guiSimulationView1d::TableCapSelectionChanged] ";
    MITK_INFO << msg << "------------------- TableCapSelectionChanged ----------";

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
    MITK_INFO << msg << "--------- TableViewCapDoubleClicked ----------";

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
//
void sv4guiSimulationView1d::ShowCapBCWidget(bool)
{
    auto msg = "[sv4guiSimulationView1d::ShowCapBCWidget] ";
    MITK_INFO << msg << "--------- ShowCapBCWidget ----------";
    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();

    if(indexesOfSelectedRows.size() < 1) {
        return;
    }

    std::map<std::string,std::string> props;
    std::string capName;
    int row=indexesOfSelectedRows[0].row();

    if(indexesOfSelectedRows.size() == 1)
        capName=m_TableModelCap->item(row,0)->text().toStdString();
    else
        capName="multiple faces";

    props["BC Type"]=m_TableModelCap->item(row,1)->text().toStdString();
    props["Values"]=m_TableModelCap->item(row,2)->text().toStdString();
    props["Pressure"]=m_TableModelCap->item(row,3)->text().toStdString();
    props["Analytic Shape"]=m_TableModelCap->item(row,4)->text().toStdString();
    props["Period"]=m_TableModelCap->item(row,5)->text().toStdString();
    props["Point Number"]=m_TableModelCap->item(row,6)->text().toStdString();
    props["Fourier Modes"]=m_TableModelCap->item(row,7)->text().toStdString();
    props["Flip Normal"]=m_TableModelCap->item(row,8)->text().toStdString();
    props["Flow Rate"]=m_TableModelCap->item(row,9)->text().toStdString();
    props["File"]=m_TableModelCap->item(row,10)->text().toStdString();
    props["Original File"]=m_TableModelCap->item(row,10)->text().toStdString();
    props["Timed Pressure"]=m_TableModelCap->item(row,11)->text().toStdString();
    props["Pressure Period"]=m_TableModelCap->item(row,12)->text().toStdString();
    props["Pressure Scaling"]=m_TableModelCap->item(row,13)->text().toStdString();
    props["R Values"]=m_TableModelCap->item(row,14)->text().toStdString();
    props["C Values"]=m_TableModelCap->item(row,15)->text().toStdString();

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

void  sv4guiSimulationView1d::SetCapBC()
{
    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    std::map<std::string, std::string> props=m_CapBCWidget->GetProps();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        m_TableModelCap->item(row,1)->setText(QString::fromStdString(props["BC Type"]));
        if(props["BC Type"]=="Resistance" || props["BC Type"]=="RCR" || props["BC Type"]=="Coronary")
        {
            m_TableModelCap->item(row,2)->setText(QString::fromStdString(props["Values"]));
        }
        else if(props["BC Type"]=="Prescribed Velocities")
        {
            if(props["Flow Rate"]!="")
                m_TableModelCap->item(row,2)->setText("Assigned");
        }
        else
        {
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

        m_TableModelCap->item(row,11)->setText(QString::fromStdString(props["Timed Pressure"]));
        m_TableModelCap->item(row,12)->setText(QString::fromStdString(props["Pressure Period"]));
        m_TableModelCap->item(row,13)->setText(QString::fromStdString(props["Pressure Scaling"]));
        m_TableModelCap->item(row,14)->setText(QString::fromStdString(props["R Values"]));
        m_TableModelCap->item(row,15)->setText(QString::fromStdString(props["C Values"]));
    }
}

void sv4guiSimulationView1d::ShowSplitBCWidget(QString splitTarget)
{
    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    QString lastBCType="";
    for(int i=0;i<indexesOfSelectedRows.size();i++)
    {
        int row=indexesOfSelectedRows[i].row();
        QString BCType=m_TableModelCap->item(row,1)->text().trimmed();

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
    if(!m_MitkJob)
        return;

    if(!m_Model)
        return;

    if(!m_SplitBCWidget)
        return;

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();
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
        std::string faceName=m_TableModelCap->item(row,0)->text().trimmed().toStdString();
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
                m_TableModelCap->item(row,2)->setText(QString::number(murrayRatio*totalValue));
            }
            else if(bcType=="RCR")
            {
                QString Rp=QString::number(murrayRatio*totalValue*percentage1);
                QString CC="0";
                QString Rd=QString::number(murrayRatio*totalValue*percentage2);

                QStringList list = m_TableModelCap->item(row,15)->text().split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);
                if(list.size()==1)
                    CC=list[0];

                m_TableModelCap->item(row,2)->setText(Rp+" "+CC+" "+Rd);
                m_TableModelCap->item(row,14)->setText(Rp+" "+Rd);
            }
            else if(bcType=="Coronary")
            {
                QString Ra=QString::number(murrayRatio*totalValue*percentage1);
                QString Ca="0";
                QString Ram=QString::number(murrayRatio*totalValue*percentage2);
                QString Cim="0";
                QString Rv=QString::number(murrayRatio*totalValue*percentage3);

                QStringList list = m_TableModelCap->item(row,15)->text().split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);
                if(list.size()==2)
                {
                    Ca=list[0];
                    Cim=list[1];
                }

                m_TableModelCap->item(row,2)->setText(Ra+" "+Ca+" "+Ram+" "+Cim+" "+Rv);
                m_TableModelCap->item(row,14)->setText(Ra+" "+Ram+" "+Rv);
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

                QStringList list = m_TableModelCap->item(row,14)->text().split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);
                if(list.size()==2)
                {
                    Rp=list[0];
                    Rd=list[1];
                }

                m_TableModelCap->item(row,2)->setText(Rp+" "+CC+" "+Rd);
                m_TableModelCap->item(row,15)->setText(CC);
            }
            else if(bcType=="Coronary")
            {
                QString Ra="0";
                QString Ca=QString::number(murrayRatio*totalValue*percentage1);
                QString Ram="0";
                QString Cim=QString::number(murrayRatio*totalValue*percentage2);
                QString Rv="0";

                QStringList list = m_TableModelCap->item(row,14)->text().split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);
                if(list.size()==3)
                {
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
//
void sv4guiSimulationView1d::UpdateGUICap()
{
    if(!m_MitkJob)
        return;

    if(!m_Model)
        return;

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) return;

    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
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
    for(int i=0;i<ids.size();i++)
    {
        sv4guiModelElement::svFace* face=modelElement->GetFace(ids[i]);
        if(face==NULL )
            continue;

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
        if(bcType=="Prescribed Velocities" && job->GetCapProp(face->name,"Flow Rate")!="")
        {
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
        m_TableModelCap->setItem(rowIndex, 14, item);

        item= new QStandardItem(CValues);
        m_TableModelCap->setItem(rowIndex, 15, item);
    }

    ui->tableViewCap->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->tableViewCap->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    ui->tableViewCap->horizontalHeader()->resizeSection(1,100);
    ui->tableViewCap->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);

    for(int i=3;i<capHeaders.size();i++)
        ui->tableViewCap->setColumnHidden(i,true);

}

void sv4guiSimulationView1d::WallTypeSelectionChanged(int index)
{
    switch(index)
    {
    case 0:
        ui->widgetConstant->hide();
        ui->widgetVariable->hide();
        break;
    case 1:
        ui->widgetThicknessE->show();
        ui->widgetConstant->show();
        ui->widgetVariable->hide();
        break;
    case 2:
        ui->widgetThicknessE->hide();
        ui->widgetConstant->show();
        ui->widgetVariable->show();
        break;
    default:
        break;
    }
}

void sv4guiSimulationView1d::TableVarSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewVar->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();
        std::string name= m_TableModelVar->item(row,0)->text().toStdString();
        modelElement->SelectFace(name);
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiSimulationView1d::TableViewVarContextMenuRequested( const QPoint & pos )
{
    m_TableMenuVar->popup(QCursor::pos());
}

void sv4guiSimulationView1d::SetVarThickness(bool)
{
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
}

void sv4guiSimulationView1d::SetVarE(bool)
{
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
}

void sv4guiSimulationView1d::UpdateGUIWall()
{
    if(!m_MitkJob)
        return;

    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
        job=new sv4guiSimJob1d();
    }

    if(job->GetWallProp("Type")=="rigid")
        ui->comboBoxWallType->setCurrentIndex(0);
    else if(job->GetWallProp("Type")=="deformable")
        ui->comboBoxWallType->setCurrentIndex(1);
    else if(job->GetWallProp("Type")=="variable")
        ui->comboBoxWallType->setCurrentIndex(2);
    else
        ui->comboBoxWallType->setCurrentIndex(0);

    ui->lineEditThickness->setText(QString::fromStdString(job->GetWallProp("Thickness")));
    ui->lineEditE->setText(QString::fromStdString(job->GetWallProp("Elastic Modulus")));

    QString pratio=QString::fromStdString(job->GetWallProp("Poisson Ratio"));
    if(pratio=="")
        pratio="0.5";
    QString kconst=QString::fromStdString(job->GetWallProp("Shear Constant"));
    if(kconst=="")
        kconst="0.833333";

    ui->lineEditNu->setText(pratio);
    ui->lineEditKcons->setText(kconst);

    ui->lineEditWallDensity->setText(QString::fromStdString(job->GetWallProp("Density")));
    ui->lineEditPressure->setText(QString::fromStdString(job->GetWallProp("Pressure")));

    if(!m_Model)
        return;

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) return;

    m_TableModelVar->clear();

    QStringList varHeaders;
    varHeaders << "Name" << "Type" << "Thickness" << "E. Modulus";
    m_TableModelVar->setHorizontalHeaderLabels(varHeaders);
    m_TableModelVar->setColumnCount(4);

    std::vector<sv4guiModelElement::svFace*> faces=modelElement->GetFaces();
    int rowIndex=-1;
    for(int i=0;i<faces.size();i++)
    {
        sv4guiModelElement::svFace* face=faces[i];
        if(face==NULL )
            continue;

        rowIndex++;
        m_TableModelVar->insertRow(rowIndex);

        QStandardItem* item;

        item= new QStandardItem(QString::fromStdString(face->name));
        item->setEditable(false);
        m_TableModelVar->setItem(rowIndex, 0, item);

        item= new QStandardItem(QString::fromStdString(face->type));
        item->setEditable(false);
        m_TableModelVar->setItem(rowIndex, 1, item);

        item= new QStandardItem(QString::fromStdString(job->GetVarProp(face->name,"Thickness")));
        m_TableModelVar->setItem(rowIndex, 2, item);

        item= new QStandardItem(QString::fromStdString(job->GetVarProp(face->name,"Elastic Modulus")));
        m_TableModelVar->setItem(rowIndex, 3, item);
    }

    ui->tableViewVar->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->tableViewVar->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    ui->tableViewVar->horizontalHeader()->resizeSection(1,60);
    ui->tableViewVar->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Fixed);
    ui->tableViewVar->horizontalHeader()->resizeSection(2,80);
    ui->tableViewVar->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Stretch);
}

void sv4guiSimulationView1d::UpdateGUISolver()
{
    if(!m_MitkJob)
        return;

    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
        job=new sv4guiSimJob1d();
    }

    m_TableModelSolver->clear();

    QStringList solverHeaders;
    solverHeaders << "Parameter" << "Value" << "Type" << "Value List";
    m_TableModelSolver->setHorizontalHeaderLabels(solverHeaders);
    int colCount=solverHeaders.size();
    m_TableModelSolver->setColumnCount(colCount);

    QString templateFilePath=":solvertemplate.xml";
    if(m_UseCustom)
        templateFilePath=m_SolverTemplatePath;

    QFile xmlFile(templateFilePath);
    if(!xmlFile.open(QIODevice::ReadOnly))
    {
        QMessageBox::warning(m_Parent,"Info Missing","Solver Parameter Table template file not found");
        return;
    }

    QDomDocument doc("solvertemplate");
    //    QString *em=NULL;
    if(!doc.setContent(&xmlFile))
    {
        QMessageBox::warning(m_Parent,"File Template Error","Format Error.");
        return;
    }
    xmlFile.close();

    QDomElement templateElement = doc.firstChildElement("template");
    QDomNodeList sectionList=templateElement.elementsByTagName("section");
    int rowIndex=-1;
    for(int i=0;i<sectionList.size();i++)
    {
        QDomNode sectionNode=sectionList.item(i);
        if(sectionNode.isNull()) continue;

        QDomElement sectionElement=sectionNode.toElement();
        if(sectionElement.isNull()) continue;

        QString name=sectionElement.attribute("name");
        rowIndex++;
        QStandardItem* item= new QStandardItem(name);
        item->setEditable(false);
        QBrush brushGray(Qt::lightGray);
        item->setBackground(brushGray);
        m_TableModelSolver->setItem(rowIndex, 0, item);
        ui->tableViewSolver->setSpan(rowIndex,0,1,colCount);

        QDomNodeList parList=sectionElement.elementsByTagName("param");
        for(int j=0;j<parList.size();j++)
        {
            QDomNode parNode=parList.item(j);
            if(parNode.isNull()) continue;

            QDomElement parElement=parNode.toElement();
            if(parElement.isNull()) continue;

            rowIndex++;
            QStandardItem* item= new QStandardItem(parElement.attribute("name"));
            item->setEditable(false);
            item->setToolTip(parElement.attribute("name"));
            m_TableModelSolver->setItem(rowIndex, 0, item);

            std::string value=job->GetSolverProp(parElement.attribute("name").toStdString());
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
    if (!m_MitkJob) {
        return;
    }

    auto meshNames = GetMeshNames();
    ui->comboBoxMeshName->clear();
    ui->comboBoxMeshName->addItem(" ");

    for (auto const& meshName : meshNames) {
        ui->comboBoxMeshName->addItem(QString::fromStdString(meshName));
    }

    int foundIndex = ui->comboBoxMeshName->findText(QString::fromStdString(m_MitkJob->GetMeshName()));
    ui->comboBoxMeshName->setCurrentIndex(foundIndex);

    int coreNum=QThread::idealThreadCount();
    ui->sliderNumProcs->setMaximum(coreNum);

    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();

    if (job == NULL) {
        return;
    }

    std::string pNum = job->GetRunProp("Number of Processes");
    ui->sliderNumProcs->setValue(pNum== "" ? 1 : QString::fromStdString(pNum).toInt());
}

//-----------------------
// UdpateSurfaceMeshName
//-----------------------
//
void sv4guiSimulationView1d::UdpateSurfaceMeshName()
{
    auto msg = "[sv4guiSimulationView1d::UdpateSurfaceMeshName] ";
    MITK_INFO << msg << "--------- UdpateSurfaceMeshName ----------"; 
    auto meshName = ui->comboBoxMeshName->currentText().toStdString();

    if (meshName != "") {
        auto mesh = GetSurfaceMesh(meshName);
    }
    MITK_INFO << msg << "Mesh name: " << meshName;
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


void sv4guiSimulationView1d::UpdateGUIRunDir()
{
    ui->lineEditResultDir->clear();

    if(m_JobNode.IsNull())
        return;

    QString jobPath=GetJobPath();
    if(jobPath=="")
        return;

    if(!m_MitkJob)
        return;

    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();
    if(job==NULL)
        return;

    std::string pNum=job->GetRunProp("Number of Processes");
    if(pNum=="")
        return;

    QString runDir=pNum=="1"?jobPath:jobPath+"/"+QString::fromStdString(pNum)+"-procs_case";
    ui->lineEditResultDir->setText(runDir);
}

QString sv4guiSimulationView1d::GetJobPath()
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

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiSimulation1dFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer simFolderNode=rs->GetElement(0);
            simFolderName=simFolderNode->GetName();
            jobPath=QString::fromStdString(projPath+"/"+simFolderName+"/"+m_JobNode->GetName());
        }
    }

    return jobPath;
}

//----------------
// CreateAllFiles
//----------------
//
// Create files for a simulation.
//
// [Davep] Not sure why we have this function.
//
void sv4guiSimulationView1d::CreateAllFiles()
{
    if(!m_MitkJob)
        return;

    auto outputAllFiles = true;
    auto updateJob = true; 
    auto createFolder = false;

    CreateDataFiles(GetJobPath(), outputAllFiles, updateJob, createFolder);
}

//--------
// RunJob
//--------
//
void sv4guiSimulationView1d::RunJob()
{
/*
    if (QMessageBox::question(m_Parent, "Run Job", "Are you sure to run the job? It may take a while to finish.",
                              QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
    {
      return;
    }
*/

    if (!m_MitkJob) {
        return;
    }

    QString jobPath=GetJobPath();
    if(jobPath=="" || !QDir(jobPath).exists())
    {
        QMessageBox::warning(m_Parent,"Unable to run","Please make sure data files have been created!");
        return;
    }

    QString flowsolverPath=m_ExternalFlowsolverPath;
    if(flowsolverPath=="")
    {
        if(m_UseMPI)
            flowsolverPath=m_InternalFlowsolverPath;
        else
            flowsolverPath=m_InternalFlowsolverNoMPIPath;
    }

    if(flowsolverPath=="")
    {
        QMessageBox::warning(m_Parent,"Flowsolver Missing","Please make sure flowsolver exists!");
        return;
    }

    QString mpiExecPath="";
    if(m_UseMPI)
    {
        mpiExecPath=m_ExternalMPIExecPath;
        if(mpiExecPath=="")
            mpiExecPath=m_InternalMPIExecPath;

        if(mpiExecPath=="")
        {
            QMessageBox::warning(m_Parent,"MPIExec Missing","Please make sure mpiexec exists!");
            return;
        }
    }

    QString runPath=jobPath;
    int numProcs=ui->sliderNumProcs->value();
    if(m_UseMPI && numProcs>1)
    {
        runPath=jobPath+"/"+QString::number(numProcs)+"-procs_case";
    }

    std::string startingNumber=ui->lineEditStartStepNum->text().trimmed().toStdString();
    if(startingNumber!="")
    {
        if(!IsInt(startingNumber))
        {
            QMessageBox::warning(m_Parent,"Parameter Error","Please provide starting step number in correct format.");
            return;
        }

        QString runRestart=runPath+"/restart."+QString::fromStdString(startingNumber)+".1";
        QString jobRestart=jobPath+"/restart."+QString::fromStdString(startingNumber)+".1";

        if( (QDir(runPath).exists() && !QFile(runRestart).exists())
                || (numProcs>1 && !QDir(runPath).exists() && !QFile(jobRestart).exists()) )
        {
            QMessageBox::warning(m_Parent,"Unable to run","Please make sure starting step number is right");
            return;
        }

        QFile numStartFile(runPath+"/numstart.dat");
        if(numStartFile.open(QIODevice::WriteOnly | QIODevice::Text))
        {
            QTextStream out(&numStartFile);
            out<<QString::fromStdString(startingNumber+"\n");
            numStartFile.close();
        }

    }

    int startStep=0;
    QFile numFile(runPath+"/numstart.dat");
    if (numFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&numFile);
        QString stepStr=in.readLine();
        bool ok;
        int step=stepStr.toInt(&ok);
        if(ok)
            startStep=step;

        numFile.close();
    }

    int totalSteps=100;//initial none zero value
    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();
    if(job)
    {
        job->SetRunProp("Number of Processes",QString::number(numProcs).toStdString());
        QString tstr=QString::fromStdString(job->GetSolverProp("Number of Timesteps"));
        totalSteps=tstr.toInt();
    }

    mitk::StatusBar::GetInstance()->DisplayText("Running simulation");

    QProcess *flowsolverProcess = new QProcess(m_Parent);
    flowsolverProcess->setWorkingDirectory(jobPath);

    if(m_UseMPI)
    {
        QStringList arguments;
        arguments << "-n" << QString::number(numProcs)<< flowsolverPath;
        flowsolverProcess->setProgram(mpiExecPath);
        flowsolverProcess->setArguments(arguments);
    }
    else
    {
        flowsolverProcess->setProgram(flowsolverPath);
        flowsolverProcess->setArguments(QStringList());
    }

    sv4guiSolverProcessHandler1d* handler=new sv4guiSolverProcessHandler1d(flowsolverProcess,m_JobNode,startStep,totalSteps,runPath,m_Parent);
    handler->Start();
}

//-----------------
// CreateDataFiles
//-----------------
//
// Create files for a simulation.
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

    // Create a job object storing all the parameters needed for a simulation.
    //
    mitk::StatusBar::GetInstance()->DisplayText("Creating Job");
    std::string jobMsg;
    sv4guiSimJob1d* job = CreateJob(jobMsg);

    if (job == nullptr) {
        QMessageBox::warning(m_Parent, "Parameter Values Error", QString::fromStdString(msg));
        return false;
    }

    QDir dir(outputDir);
    dir.mkpath(outputDir);

    m_MitkJob->SetSimJob(job);
    //m_MitkJob->SetMeshName(meshName);
    m_MitkJob->SetDataModified();

    // Check centerlines have been generated.
    if (m_CenterlinesFileName.isEmpty()) {
        QMessageBox::warning(NULL, "1D Simulation", "No centerlines have been calculated or centerlines source file set.");
        MITK_ERROR << "No centerlines file is defined.";
        return;
    }
    MITK_INFO << msg << "Centerlines file: " << m_CenterlinesFileName;

    auto outputDirectory = outputDir.toStdString();
    auto inputCenterlinesFile = m_CenterlinesFileName.toStdString();
    auto solverFileName = SOLVER_FILE_NAME.toStdString();

    // Execute the generate-1d-mesh.py script.
    auto pythonInterface = sv4guiSimulationPython1d();
    pythonInterface.GenerateSolverInput(outputDirectory, inputCenterlinesFile, solverFileName, job);

    return true;
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
            if (QMessageBox::question(m_Parent, "Overwrite File?", "Do you want to overwrite the file (" +fileName +") in the job?",
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
sv4guiSimJob1d* sv4guiSimulationView1d::CreateJob(std::string& msg, bool checkValidity)
{
    sv4guiSimJob1d* job = new sv4guiSimJob1d();

    if (!SetBasicParameters(job, msg)) {
        delete job;
        return nullptr;
    }

    if (!SetCapBcs(job, msg)) {
        delete job;
        return nullptr;
    }

    if (!SetWallProperites(job, msg)) {
        delete job;
        return nullptr;
    }

    if (!SetSolverParameters(job, msg)) {
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
bool sv4guiSimulationView1d::SetBasicParameters(sv4guiSimJob1d* job, std::string& msg)
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
//
bool sv4guiSimulationView1d::SetCapBcs(sv4guiSimJob1d* job, std::string& msg)
{
    auto checkValidity = false;

    for(int i=0;i<m_TableModelCap->rowCount();i++) {
        std::string capName=m_TableModelCap->item(i,0)->text().toStdString();
        std::string bcType=m_TableModelCap->item(i,1)->text().trimmed().toStdString();

        if(bcType=="Prescribed Velocities") {
            std::string flowrateContent=m_TableModelCap->item(i,9)->text().trimmed().toStdString();
            std::string period=m_TableModelCap->item(i,5)->text().trimmed().toStdString();

            if(checkValidity) {
                if(flowrateContent=="") {
                    msg=capName + ": no flowrate data";
                    return false;
                }

                if(period=="") {
                    msg=capName + ": no period for flowrate data";
                    return false;
                }
            }

            std::string shape=m_TableModelCap->item(i,4)->text().trimmed().toStdString();
            std::string pointNum=m_TableModelCap->item(i,6)->text().trimmed().toStdString();
            std::string modeNum=m_TableModelCap->item(i,7)->text().trimmed().toStdString();
            std::string flip=m_TableModelCap->item(i,8)->text().trimmed().toStdString();
            std::string originalFile=m_TableModelCap->item(i,10)->text().trimmed().toStdString();

            job->SetCapProp(capName,"BC Type", bcType);
            job->SetCapProp(capName,"Analytic Shape", shape);
            job->SetCapProp(capName,"Period", period);
            job->SetCapProp(capName,"Point Number", pointNum);
            job->SetCapProp(capName,"Fourier Modes", modeNum);
            job->SetCapProp(capName,"Flip Normal", flip);
            job->SetCapProp(capName,"Flow Rate", flowrateContent);
            job->SetCapProp(capName,"Original File", originalFile);

        } else if(bcType!="") {
            std::string values=m_TableModelCap->item(i,2)->text().trimmed().toStdString();
            std::string pressure=m_TableModelCap->item(i,3)->text().trimmed().toStdString();
            std::string originalFile=m_TableModelCap->item(i,10)->text().trimmed().toStdString();
            std::string timedPressure=m_TableModelCap->item(i,11)->text().trimmed().toStdString();
            std::string pressurePeriod=m_TableModelCap->item(i,12)->text().trimmed().toStdString();
            std::string pressureScaling=m_TableModelCap->item(i,13)->text().trimmed().toStdString();
            std::string RValues=m_TableModelCap->item(i,14)->text().trimmed().toStdString();
            std::string CValues=m_TableModelCap->item(i,15)->text().trimmed().toStdString();

            if(checkValidity) {
                if(bcType=="Resistance") {
                    if(!IsDouble(values)) {
                        msg=capName + " R value error: " + values;
                        return false;
                    }
                } else if(bcType=="RCR") {
                    int count=0;
                    QStringList list = QString(values.c_str()).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                    values=list.join(" ").toStdString();

                    if(!AreDouble(values,&count)||count!=3) {
                        msg=capName + " RCR values error: " + values;
                        return false;
                    }
                } else if(bcType=="Coronary") {
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
//
bool sv4guiSimulationView1d::SetWallProperites(sv4guiSimJob1d* job, std::string& msg)
{
    auto checkValidity = false;
    int wallTypeIndex = ui->comboBoxWallType->currentIndex();

    if(wallTypeIndex==0) {
        job->SetWallProp("Type","rigid");
    } else if(wallTypeIndex==1) {
        std::string thickness=ui->lineEditThickness->text().trimmed().toStdString();
        std::string modulus=ui->lineEditE->text().trimmed().toStdString();
        std::string nu=ui->lineEditNu->text().trimmed().toStdString();
        std::string kcons=ui->lineEditKcons->text().trimmed().toStdString();
        std::string wallDensity=ui->lineEditWallDensity->text().trimmed().toStdString();
        std::string pressure=ui->lineEditPressure->text().trimmed().toStdString();

        if(checkValidity) {
            if(!IsDouble(thickness)) {
                msg="wall thickness error: " + thickness;
                return false;
            }

            if(!IsDouble(modulus)) {
                msg="wall elastic modulus error: " + modulus;
                return false;
            }

            if(!IsDouble(nu)) {
                msg="wall Poisson ratio error: " + nu;
                return false;
            }

            if(!IsDouble(kcons)) {
                msg="wall shear constant error: " + kcons;
                return false;
            }

            if(wallDensity!="") {
                if(!IsDouble(wallDensity)) {
                    msg="wall density error: " + wallDensity;
                    return false;
                }
            } else {
                wallDensity=job->GetBasicProp("Fluid Density");
            }

            if(!IsDouble(pressure)) {
                msg="wall pressure error: " + pressure;
                return false;
            }
        }

        job->SetWallProp("Type","deformable");
        job->SetWallProp("Thickness",thickness);
        job->SetWallProp("Elastic Modulus",modulus);
        job->SetWallProp("Poisson Ratio",nu);
        job->SetWallProp("Shear Constant",kcons);
        job->SetWallProp("Density",wallDensity);
        job->SetWallProp("Pressure",pressure);

    } else if(wallTypeIndex==2) {
        std::string nu=ui->lineEditNu->text().trimmed().toStdString();
        std::string kcons=ui->lineEditKcons->text().trimmed().toStdString();
        std::string wallDensity=ui->lineEditWallDensity->text().trimmed().toStdString();
        std::string pressure=ui->lineEditPressure->text().trimmed().toStdString();

        if(checkValidity) {
            if(!IsDouble(nu)) {
                msg="wall Poisson ratio error: " + nu;
                return false;
            }

            if(!IsDouble(kcons)) {
                msg="wall shear constant error: " + kcons;
                return false;
            }

            if(wallDensity!="") {
                if(!IsDouble(wallDensity)) {
                    msg="wall density error: " + wallDensity;
                    return false;
                }
            } else {
                wallDensity=job->GetBasicProp("Fluid Density");
            }

            if(!IsDouble(pressure)) {
                msg="wall pressure error: " + pressure;
                return false;
            }
        }

        job->SetWallProp("Type","variable");
        job->SetWallProp("Poisson Ratio",nu);
        job->SetWallProp("Shear Constant",kcons);
        job->SetWallProp("Density",wallDensity);
        job->SetWallProp("Pressure",pressure);

        for(int i=0;i<m_TableModelVar->rowCount();i++) {
            std::string faceName = m_TableModelVar->item(i,0)->text().toStdString();
            std::string thickness = m_TableModelVar->item(i,2)->text().trimmed().toStdString();
            std::string modulus = m_TableModelVar->item(i,3)->text().trimmed().toStdString();

            if(checkValidity) {
                if(thickness!="" && !IsDouble(thickness)) {
                    msg="wall thickness error: " + thickness;
                    return false;
                }

                if(modulus!="" && !IsDouble(modulus)) {
                    msg="wall elastic modulus error: " + modulus;
                    return false;
                }
            }

            job->SetVarProp(faceName,"Thickness", thickness);
            job->SetVarProp(faceName,"Elastic Modulus", modulus);
        }
    }

    return true;
}

//---------------------
// SetSolverParameters
//---------------------
//
bool sv4guiSimulationView1d::SetSolverParameters(sv4guiSimJob1d* job, std::string& msg)
{
    auto checkValidity = false;

    for(int i=0;i<m_TableModelSolver->rowCount();i++) {
        std::string parName=m_TableModelSolver->item(i,0)->text().trimmed().toStdString();
        QStandardItem* valueItem=m_TableModelSolver->item(i,1);
        if(valueItem==NULL) {
            continue;
        }

        std::string value=valueItem->text().trimmed().toStdString();
        std::string type=m_TableModelSolver->item(i,2)->text().trimmed().toStdString();

        if(checkValidity ) {
            if(value == "") {
                msg = parName+ " missing value";
                return false;
            } else if(type=="int"&&!IsInt(value)) {
                msg=parName+ " value error: " + value;
                return false;
            } else if(type=="double"&&!IsDouble(value)) {
                msg=parName+ " value error: " + value;
                return false;
            }
        }

        job->SetSolverProp(parName, value);
    }

    return true;
}



void sv4guiSimulationView1d::SaveToManager()
{
    if(!m_MitkJob)
        return;

    std::string msg;

    sv4guiSimJob1d* job=CreateJob(msg);

    if(job==NULL)
    {
        QMessageBox::warning(m_Parent,"Parameter Values Error",QString::fromStdString(msg));
        return;
    }

    m_MitkJob->SetSimJob(job);
    m_MitkJob->SetDataModified();
}

//--------------
// SetResultDir
//--------------
//
void sv4guiSimulationView1d::SetResultDir()
{
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

    QString lastFileOpenPath="";
    QString currentPath=ui->lineEditResultDir->text().trimmed();
    if(currentPath!="" && QDir(currentPath).exists())
        lastFileOpenPath=currentPath;
    else if(prefs.IsNotNull())
    {
        lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
    }
    if(lastFileOpenPath=="")
        lastFileOpenPath=QDir::homePath();

    QString dir = QFileDialog::getExistingDirectory(m_Parent
                                                    , tr("Choose Result Directory")
                                                    , lastFileOpenPath);

    dir=dir.trimmed();
    if(dir.isEmpty()) return;

    if(prefs.IsNotNull())
    {
        prefs->Put("LastFileOpenPath", dir);
        prefs->Flush();
    }

    ui->lineEditResultDir->setText(dir);
}

void sv4guiSimulationView1d::ExportResults()
{
    QString postsolverPath=m_ExternalPostsolverPath;
    if(postsolverPath=="")
        postsolverPath=m_InternalPostsolverPath;

    if(postsolverPath=="" || !QFile(postsolverPath).exists())
    {
        QMessageBox::warning(m_Parent,"Postsolver Missing","Please make sure postsolver exists!");
        return;
    }

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

    QString lastFileSavePath="";
    if(prefs.IsNotNull())
    {
        lastFileSavePath = prefs->Get("LastFileSavePath", "");
    }
    if(lastFileSavePath=="")
        lastFileSavePath=QDir::homePath();

    QString exportDir = QFileDialog::getExistingDirectory(m_Parent
                                                    , tr("Choose Export Directory")
                                                    , lastFileSavePath);

    exportDir=exportDir.trimmed();
    if(exportDir.isEmpty())
        return;

    if(prefs.IsNotNull())
     {
         prefs->Put("LastFileSavePath", exportDir);
         prefs->Flush();
     }

    QString jobName("");
    if(m_JobNode.IsNotNull())
        jobName=QString::fromStdString(m_JobNode->GetName())+"-";

    exportDir=exportDir+"/"+jobName+"converted-results";
    QDir exdir(exportDir);
    exdir.mkpath(exportDir);

    QString resultDir=ui->lineEditResultDir->text();
    QDir rdir(resultDir);
    if(!rdir.exists())
    {
        QMessageBox::warning(m_Parent,"Result dir not exists","Please provide valid result dir");
        return;
    }

    QString startNo=ui->lineEditStart->text().trimmed();
    if(!IsInt(startNo.toStdString()))
    {
        QMessageBox::warning(m_Parent,"Start Step Error","Please provide start step number in correct format.");
        return;
    }

    QString stopNo=ui->lineEditStop->text().trimmed();
    if(!IsInt(stopNo.toStdString()))
    {
        QMessageBox::warning(m_Parent,"Stop Step Error","Please provide stop step number in correct format.");
        return;
    }

    QString increment=ui->lineEditIncrement->text().trimmed();
    if(!IsInt(increment.toStdString()))
    {
        QMessageBox::warning(m_Parent,"Increment Error","Please provide increment in correct format.");
        return;
    }

    QStringList arguments;
    arguments << "-all";
    arguments << "-indir" << resultDir;
    arguments << "-outdir" << exportDir;
    arguments << "-start" << startNo;
    arguments << "-stop" << stopNo;
    arguments << "-incr" << increment;
    if(ui->checkBoxSingleFile->isChecked())
        arguments << "-vtkcombo";

    if(ui->checkBoxVolume->isChecked())
    {
       if(ui->checkBoxSingleFile->isChecked())
           arguments << "-vtu" << "all_results.vtu";
       else
           arguments << "-vtu" << "all_results";
    }

    if(ui->checkBoxSurface->isChecked())
    {
       if(ui->checkBoxSingleFile->isChecked())
           arguments << "-vtp" << "all_results.vtp";
       else
           arguments << "-vtp" << "all_results";
    }

    if(ui->checkBoxToRestart->isChecked())
        arguments << "-ph" << "-laststep";

    mitk::StatusBar::GetInstance()->DisplayText("Exporting results.");

    QProcess *postsolverProcess = new QProcess(m_Parent);
    postsolverProcess->setWorkingDirectory(exportDir);
    postsolverProcess->setProgram(postsolverPath);
    postsolverProcess->setArguments(arguments);

    sv4guiProcessHandler1d* handler=new sv4guiProcessHandler1d(postsolverProcess,m_JobNode,false,false,m_Parent);
    handler->Start();

    QString detailedInfo=handler->GetMessage();
    delete handler;

    bool convertedFilesExit=true;
    bool meshFaceDirExits=true;
    bool meshFaceFilesExist=true;
    bool calculateFlows=true;

    if(ui->checkBoxCalculateFlows->isChecked())
    {
        convertedFilesExit=false;
        meshFaceDirExits=false;
        meshFaceFilesExist=false;
        calculateFlows=false;

        QString meshFaceDir=GetJobPath()+"/mesh-complete/mesh-surfaces";
        meshFaceDirExits=QDir(meshFaceDir).exists();
        std::vector<std::string> meshFaceFileNames;
        if(meshFaceDirExits)
        {
            QStringList filters;
            filters<<"*.vtp";
            QStringList fileList=QDir(meshFaceDir).entryList(filters, QDir::Files);
            meshFaceFilesExist=(fileList.size()>0);
            for(int i=0;i<fileList.size();i++)
                meshFaceFileNames.push_back(fileList[i].toStdString());
        }

        std::vector<std::string> vtxFilePaths;

        if(ui->checkBoxSingleFile->isChecked())
        {
            QString vtpResultFilePath=exportDir+"/all_results.vtp";
            QString vtuResultFilePath=exportDir+"/all_results.vtu";

            if(QFile(vtpResultFilePath).exists())
                vtxFilePaths.push_back(vtpResultFilePath.toStdString());
            else if(QFile(vtuResultFilePath).exists())
                vtxFilePaths.push_back(vtuResultFilePath.toStdString());
        }
        else
        {
            QStringList filters;
            filters<<"all_results_*.vtp";
            QStringList fileList=QDir(exportDir).entryList(filters, QDir::Files, QDir::Name);

            if(fileList.size()==0)
            {
                filters.clear();
                filters<<"all_results_*.vtu";
                fileList=QDir(exportDir).entryList(filters, QDir::Files, QDir::Name);
            }

            for(int i=0;i<fileList.size();i++)
                vtxFilePaths.push_back((exportDir+"/"+fileList[i]).toStdString());

        }

        convertedFilesExit=(vtxFilePaths.size()>0);

        if( convertedFilesExit && meshFaceDirExits && meshFaceFilesExist )
        {

            QString outPressureFlePath=exportDir+"/all_results-pressures.txt";
            QString outFlowFilePath=exportDir+"/all_results-flows.txt";
            QString outAverageFilePath=exportDir+"/all_results-averages.txt";
            QString outAverageUnitsFilePath=exportDir+"/all_results-averages-from_cm-to-mmHg-L_per_min.txt";
            QString unit=ui->comboBoxSimUnits->currentText();
            bool skipWalls=ui->checkBoxSkipWalls->isChecked();

            calculateFlows=sv4guiSimulationUtils1d::CreateFlowFiles(outFlowFilePath.toStdString(), outPressureFlePath.toStdString()
                                                              , outAverageFilePath.toStdString(), outAverageUnitsFilePath.toStdString()
                                                              , vtxFilePaths,ui->checkBoxSingleFile->isChecked()
                                                              , meshFaceDir.toStdString(), meshFaceFileNames
                                                              , unit.toStdString(), skipWalls);
        }
    }

    QString msg="";
    if(convertedFilesExit)
    {
        msg="Results have been converted.";
        if(!meshFaceDirExits)
            msg=msg+"\nNo mesh face dir exits.";
        else if(!meshFaceFilesExist)
            msg=msg+"\nNo mesh face files exit.";
        else if(!calculateFlows)
            msg=msg+"\nFail to calculate flows.";
    }
    else
        msg="Results not converted.";

    msg=msg+"                                                                                        ";

    QMessageBox mb(m_Parent);
    mb.setWindowTitle("Finished");
    mb.setText(msg);
    mb.setIcon(QMessageBox::Information);
    mb.setDetailedText(detailedInfo);
    mb.setDefaultButton(QMessageBox::Ok);
    mb.exec();

    mitk::StatusBar::GetInstance()->DisplayText("Results converting finished.");
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
//
void sv4guiSimulationView1d::UpdateSimJob()
{
    if(!m_MitkJob)
        return;

    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();
    std::string numProcsStr="";
    if(job)
    {
        numProcsStr=job->GetRunProp("Number of Processes");
    }

    std::string msg="";
    sv4guiSimJob1d* newJob=CreateJob(msg,false);
    if(newJob==NULL)
        return;

    newJob->SetRunProp("Number of Processes",numProcsStr);
    m_MitkJob->SetSimJob(newJob);
    m_MitkJob->SetDataModified();
}

void sv4guiSimulationView1d::UpdateSimJobNumProcs()
{
    if(!m_MitkJob)
        return;

    sv4guiSimJob1d* job=m_MitkJob->GetSimJob();
    if(job)
    {
        std::string numProcsStr=QString::number((int)(ui->sliderNumProcs->value())).toStdString();
        job->SetRunProp("Number of Processes",numProcsStr);
        m_MitkJob->SetDataModified();
    }
}

void sv4guiSimulationView1d::ShowCalculateFowsWidget(bool checked)
{
    ui->widgetCalculateFlows->setVisible(checked);
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

void sv4guiSimulationView1d::UpdateJobStatus()
{
    if(m_JobNode.IsNull())
        return;

    bool running=false;
    double runningProgress=0;
    m_JobNode->GetBoolProperty("running",running);
    m_JobNode->GetDoubleProperty("running progress",runningProgress);

    if(running) {
        ui->labelJobStatus->setText("Running: "+QString::number((int)(runningProgress*100))+"% completed");
        ui->widgetRun->setEnabled(false);
    } else {
        ui->labelJobStatus->setText(QString::fromStdString(m_MitkJob->GetStatus()));
        ui->widgetRun->setEnabled(true);
    }

}

//-----------
// ShowModel
//-----------
//
void sv4guiSimulationView1d::ShowModel(bool checked)
{
    if (m_ModelNode.IsNotNull()) {
        m_ModelNode->SetVisibility(checked);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}
