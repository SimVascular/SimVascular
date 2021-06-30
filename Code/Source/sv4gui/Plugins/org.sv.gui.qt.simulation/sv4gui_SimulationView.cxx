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

using namespace sv4guiSimulationPreferenceDBKey;

const QString sv4guiSimulationView::EXTENSION_ID = "org.sv.views.simulation";

// Set the title for QMessageBox warnings.
//
// Note: On MacOS the window title is ignored (as required by the Mac OS X Guidelines). 
const QString sv4guiSimulationView::MsgTitle = "SimVascular SV Simulation";

//----------------------
// sv4guiSimulationView
//----------------------
//
sv4guiSimulationView::sv4guiSimulationView() :
    ui(new Ui::sv4guiSimulationView)
{
    m_MitkJob=NULL;
    m_Model=NULL;
    m_JobNode=NULL;
    m_ModelNode=NULL;

    m_DataInteractor=NULL;
    m_ModelSelectFaceObserverTag=-1;

    m_TableModelBasic=NULL;

    m_TableModelCap=NULL;
    m_TableMenuCap=NULL;

    m_TableModelVar=NULL;
    m_TableMenuVar=NULL;

    m_CapBCWidget=NULL;

    m_TableModelSolver=NULL;

    m_PresolverPath="";
    m_FlowsolverPath="";
    m_FlowsolverNOMPIPath="";
    m_PostsolverPath="";
    m_MPIExecPath="";

    m_UseMPI=false;
    m_UseCustom=false;
    m_SolverTemplatePath="";

    m_ConnectionEnabled=false;

    // Get the default solver binaries.
    //m_DefaultPrefs = sv4guiSimulationPreferences();
}

sv4guiSimulationView::~sv4guiSimulationView()
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

void sv4guiSimulationView::EnableConnection(bool able)
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
        connect(ui->comboBoxMeshName, SIGNAL(currentIndexChanged(int )), this, SLOT(UdpateSimJobMeshName( )));
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
        disconnect(ui->comboBoxMeshName, SIGNAL(currentIndexChanged(int )), this, SLOT(UdpateSimJobMeshName( )));
        disconnect(ui->sliderNumProcs, SIGNAL(valueChanged(double)), this, SLOT(UpdateSimJobNumProcs()));
        m_ConnectionEnabled=able;
    }
}

//---------------------
// CreateQtPartControl
//---------------------
// Set the GUI widgets event/callbacks.
//
void sv4guiSimulationView::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);
    ui->btnSave->hide();

    // Upper panel.
    connect(ui->checkBoxShowModel, SIGNAL(clicked(bool)), this, SLOT(ShowModel(bool)) );

    // Set toolbox page (Basic Parameters).
    ui->toolBox->setCurrentIndex(0);

    // for basic table
    m_TableModelBasic = new QStandardItemModel(this);
    ui->tableViewBasic->setModel(m_TableModelBasic);

    connect( ui->tableViewBasic, SIGNAL(doubleClicked(const QModelIndex&))
             , this, SLOT(TableViewBasicDoubleClicked(const QModelIndex&)) );

    //for cap table
    m_TableModelCap = new QStandardItemModel(this);
    ui->tableViewCap->setModel(m_TableModelCap);
    sv4guiTableCapDelegate* itemDelegate=new sv4guiTableCapDelegate(this);
    ui->tableViewCap->setItemDelegateForColumn(1,itemDelegate);

    connect( ui->tableViewCap->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableCapSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    connect( ui->tableViewCap, SIGNAL(doubleClicked(const QModelIndex&))
             , this, SLOT(TableViewCapDoubleClicked(const QModelIndex&)) );

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

    m_CapBCWidget=new sv4guiCapBCWidget();
    m_CapBCWidget->move(400,400);
    m_CapBCWidget->hide();
    m_CapBCWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_CapBCWidget,SIGNAL(accepted()), this, SLOT(SetCapBC()));

    m_SplitBCWidget=new sv4guiSplitBCWidget();
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
    sv4guiTableSolverDelegate* itemSolverDelegate=new sv4guiTableSolverDelegate(this);
    ui->tableViewSolver->setItemDelegateForColumn(1,itemSolverDelegate);

    ///////////////////////////////////////////////////
    // Create Files and Run Simulation toolbox tab  //
    /////////////////////////////////////////////////
    //
    // Don't use MPI by default so disable sliderNumProcs, etc.
    //
    connect(ui->btnCreateAllFiles, SIGNAL(clicked()), this, SLOT(CreateAllFiles()) );
    connect(ui->btnImportFiles, SIGNAL(clicked()), this, SLOT(ImportFiles()) );
    connect(ui->btnRunJob, SIGNAL(clicked()), this, SLOT(RunJob()) );
    connect(ui->UseMpiCheckBox, SIGNAL(clicked(bool)), this, SLOT(UseMpi(bool)) );
    ui->NumberProcessesLabel->setEnabled(false);
    ui->sliderNumProcs->setEnabled(false);

    ///////////////////////////////////////////////////
    //           Convert Results toolbox tab        //
    /////////////////////////////////////////////////
    // Widgets for exporting results.
    //
    connect(ui->toolButtonResultDir, SIGNAL(clicked()), this, SLOT(SetResultDir()) );
    connect(ui->btnExportResults, SIGNAL(clicked()), this, SLOT(ExportResults()) );

//    ui->widgetCalculateFlows->hide();
    connect(ui->checkBoxCalculateFlows, SIGNAL(clicked(bool)), this, SLOT(ShowCalculateFowsWidget(bool)) );

    // Set paths for the external solvers.
    berry::IPreferences::Pointer prefs = this->GetPreferences();
    berry::IBerryPreferences* berryprefs = dynamic_cast<berry::IBerryPreferences*>(prefs.GetPointer());
    this->OnPreferencesChanged(berryprefs);
}

//----------------------
// OnPreferencesChanged 
//----------------------
// Set solver binaries, mpiexec binary and the MPI implementation 
// used to create and execute simulation jobs.
//
// This method is called when the SV Simulation plugin is activated or when values 
// in the Preferences Page->SimVascular Simulation panel are changed. 
//
// If a SimVascular MITK database exists from previous SimVascular sessions then 
// the values for the binaries are obtained from there. If the database does not
// exist then set the values of the binaries to their default values, which
// are the same values set for the SimVascular Simulation Preferences page.
//
// The solver binaries are: svpre, svsolver and svpost. The value for each binary
// contains the full path to the binary together with its name. For example, 
//
//     m_FlowsolverPath = "/usr/local/sv/bin/svsolver"
//
// Note that the 'binary' may actually be a shell script that sets up environment
// variables and then executes the actual binary. In that case m_FlowsolverPath
// is set to (on Linux) 
//
//     m_FlowsolverPath = "/usr/local/sv/svsolver"
//
// The 'prefs' Get() argument names (e.g. "presolver path") are set by the 
// sv4guiSimulationPreferencePage object.
//
void sv4guiSimulationView::OnPreferencesChanged(const berry::IBerryPreferences* prefs)
{
    if (prefs == NULL) {
        return;
    }

    // Set the solver binaries.
    m_PresolverPath = prefs->Get(PRE_SOLVER_PATH, m_DefaultPrefs.GetPreSolver());
    m_FlowsolverPath = prefs->Get(FLOW_SOLVER_PATH, m_DefaultPrefs.GetSolver());
    m_FlowsolverNOMPIPath = prefs->Get(FLOW_SOLVER_NO_MPI_PATH, m_DefaultPrefs.GetSolverNOMPI());
    m_PostsolverPath = prefs->Get(POST_SOLVER_PATH, m_DefaultPrefs.GetPostSolver());

    // Set the mpiexec binary and mpi implementation.
    m_MPIExecPath = prefs->Get(sv4guiMPIPreferenceDBKey::MPI_EXEC_PATH, m_DefaultMPIPrefs.GetMpiExec()); 
    auto mpiName = prefs->Get(sv4guiMPIPreferenceDBKey::MPI_IMPLEMENTATION, m_DefaultMPIPrefs.GetMpiImplementationName());
    m_MpiImplementation = m_DefaultMPIPrefs.GetMpiImplementation(mpiName);
    // [DaveP] m_UseMPI = prefs->GetBool("use mpi",false);
}

//--------------------
// OnSelectionChanged
//--------------------
// Update the SV Simulation plugin panel when an SV Simulation plugin is selected from the
// SV Data Manager.
//
// This method is also called when SimVascular starts and there is a SV Simulation plugin 
// panel left over from a previous session. In this case nodes.size()==0.
//
void sv4guiSimulationView::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
    if (!IsVisible()) {
        return;
    }

    if(nodes.size()==0) {
        RemoveObservers();
        EnableTool(false);
        return;
    }

    mitk::DataNode::Pointer jobNode=nodes.front();
    sv4guiMitkSimJob* mitkJob=dynamic_cast<sv4guiMitkSimJob*>(jobNode->GetData());

    if(!mitkJob)
    {
        RemoveObservers();
        EnableTool(false);
        return;
    }

    std::string modelName=mitkJob->GetModelName();
    mitk::DataNode::Pointer modelNode=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (jobNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiModelFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer modelFolderNode=rs->GetElement(0);
            modelNode=GetDataStorage()->GetNamedDerivedNode(modelName.c_str(),modelFolderNode);
        }
    }

    sv4guiModel* model=NULL;
    if(modelNode.IsNotNull())
    {
        model=dynamic_cast<sv4guiModel*>(modelNode->GetData());
    }

    if(m_JobNode.IsNotNull())
        RemoveObservers();

    m_ModelNode=modelNode;
    m_Model=model;
    m_JobNode=jobNode;
    m_MitkJob=mitkJob;

    if(m_Model==NULL)
    {
        EnableTool(false);
    }
    else
    {
        EnableTool(true);
        AddObservers();
    }

    //update top part
    //======================================================================
    ui->labelJobName->setText(QString::fromStdString(m_JobNode->GetName()));
    ui->labelJobStatus->setText(QString::fromStdString(m_MitkJob->GetStatus()));
    ui->checkBoxShowModel->setChecked(false);
    if(m_ModelNode.IsNotNull())
    {
        ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
        if(m_ModelNode->IsVisible(NULL))
            ui->checkBoxShowModel->setChecked(true);
    }
    else
        ui->labelModelName->setText("No model found");

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
            UpdateGUIRunDir();
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

void sv4guiSimulationView::Visible()
{
    OnSelectionChanged(GetDataManagerSelection());
}

void sv4guiSimulationView::Hidden()
{
    RemoveObservers();
}

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
            m_ModelNode->SetDataInteractor(NULL);
    }
    m_DataInteractor=NULL;
}

void sv4guiSimulationView::ClearAll()
{
    m_Model=NULL;
    m_JobNode=NULL;
    m_MitkJob=NULL;
    m_ModelNode=NULL;

    ui->labelJobName->setText("");
    ui->labelJobStatus->setText("");
    ui->labelModelName->setText("");
}

void sv4guiSimulationView::UpdateGUIBasic()
{
    if(!m_MitkJob)
        return;

    sv4guiSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
        job=new sv4guiSimJob();
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

void sv4guiSimulationView::TableViewBasicDoubleClicked(const QModelIndex& index)
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

void sv4guiSimulationView::UpdateFaceListSelection()
{
    if(!m_Model)
        return;

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) return;


    //for tableViewCap
    disconnect( ui->tableViewCap->selectionModel()
                , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
                , this
                , SLOT( TableCapSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    ui->tableViewCap->clearSelection();

    int count=m_TableModelCap->rowCount();

    for(int i=0;i<count;i++)
    {
        QStandardItem* itemName= m_TableModelCap->item(i,0);
        std::string name=itemName->text().toStdString();

        if(modelElement->IsFaceSelected(name))
        {
            QModelIndex mIndex=m_TableModelCap->index(i,1);
            ui->tableViewCap->selectionModel()->select(mIndex, QItemSelectionModel::Select|QItemSelectionModel::Rows);
        }
    }

    connect( ui->tableViewCap->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableCapSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );


    //for tableViewVar
    disconnect( ui->tableViewVar->selectionModel()
                , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
                , this
                , SLOT( TableVarSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    ui->tableViewVar->clearSelection();

    count=m_TableModelVar->rowCount();

    for(int i=0;i<count;i++)
    {
        QStandardItem* itemName= m_TableModelVar->item(i,0);
        std::string name=itemName->text().toStdString();

        if(modelElement->IsFaceSelected(name))
        {
            QModelIndex mIndex=m_TableModelVar->index(i,1);
            ui->tableViewVar->selectionModel()->select(mIndex, QItemSelectionModel::Select|QItemSelectionModel::Rows);
        }
    }

    connect( ui->tableViewVar->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableVarSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

}

void sv4guiSimulationView::TableCapSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    mitk::StatusBar::GetInstance()->DisplayText("");

    if(!m_Model)
        return;

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
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

void sv4guiSimulationView::TableViewCapDoubleClicked(const QModelIndex& index)
{
    if(index.column()==0)
        ShowCapBCWidget();
}

void sv4guiSimulationView::TableViewCapContextMenuRequested( const QPoint & pos )
{
    m_TableMenuCap->popup(QCursor::pos());
}

void sv4guiSimulationView::ShowCapBCWidget(bool)
{
    QModelIndexList indexesOfSelectedRows = ui->tableViewCap->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
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

void sv4guiSimulationView::SetDistalPressure(bool)
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

void  sv4guiSimulationView::SetCapBC()
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

void sv4guiSimulationView::ShowSplitBCWidget(QString splitTarget)
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

void sv4guiSimulationView::UpdateGUICap()
{
    if(!m_MitkJob)
        return;

    if(!m_Model)
        return;

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) return;

    sv4guiSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
        job=new sv4guiSimJob();
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

void sv4guiSimulationView::WallTypeSelectionChanged(int index)
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

void sv4guiSimulationView::TableVarSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
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

void sv4guiSimulationView::TableViewVarContextMenuRequested( const QPoint & pos )
{
    m_TableMenuVar->popup(QCursor::pos());
}

void sv4guiSimulationView::SetVarThickness(bool)
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

void sv4guiSimulationView::SetVarE(bool)
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

void sv4guiSimulationView::UpdateGUIWall()
{
    if(!m_MitkJob)
        return;

    sv4guiSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
        job=new sv4guiSimJob();
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

void sv4guiSimulationView::UpdateGUISolver()
{
    if(!m_MitkJob)
        return;

    sv4guiSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
        job=new sv4guiSimJob();
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
    if(job==NULL)
        return;

    std::string pNum=job->GetRunProp("Number of Processes");
    ui->sliderNumProcs->setValue(pNum==""?1:QString::fromStdString(pNum).toInt());
}

void sv4guiSimulationView::UpdateGUIRunDir()
{
    ui->lineEditResultDir->clear();

    if(m_JobNode.IsNull())
        return;

    QString jobPath=GetJobPath();
    if(jobPath=="")
        return;

    if(!m_MitkJob)
        return;

    sv4guiSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
        return;

    std::string pNum=job->GetRunProp("Number of Processes");
    if(pNum=="")
        return;

    QString runDir=pNum=="1"?jobPath:jobPath+"/"+QString::fromStdString(pNum)+"-procs_case";
    ui->lineEditResultDir->setText(runDir);
}

//void sv4guiSimulationView::ExportInputFiles()
//{
//    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
//    berry::IPreferences::Pointer prefs;
//    if (prefService)
//    {
//        prefs = prefService->GetSystemPreferences()->Node("/General");
//    }
//    else
//    {
//        prefs = berry::IPreferences::Pointer(0);
//    }

//    QString lastFileSavePath=QString();
//    if(prefs.IsNotNull())
//    {
//        lastFileSavePath = prefs->Get("LastFileSavePath", "");
//    }

//    QString dir = QFileDialog::getExistingDirectory(m_Parent
//                                                    , tr("Choose Directory")
//                                                    , lastFileSavePath);

//    if(dir.isEmpty()) return;

//    CreateDataFiles(dir, false, true, true);
//}

//void sv4guiSimulationView::ExportAllFiles()
//{
//    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
//    berry::IPreferences::Pointer prefs;
//    if (prefService)
//    {
//        prefs = prefService->GetSystemPreferences()->Node("/General");
//    }
//    else
//    {
//        prefs = berry::IPreferences::Pointer(0);
//    }

//    QString lastFileSavePath=QString();
//    if(prefs.IsNotNull())
//    {
//        lastFileSavePath = prefs->Get("LastFileSavePath", "");
//    }

//    QString dir = QFileDialog::getExistingDirectory(m_Parent
//                                                    , tr("Choose Directory")
//                                                    , lastFileSavePath);

//    if(dir.isEmpty()) return;

//    CreateDataFiles(dir, true, true, true);
//}

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

    // Checks throw exceptions if they fail.

    try {

        if(m_UseMPI) {
          // Check that the solver binaries are valid.
          CheckSolver();
          // Check that mpi is installed and that the implementation is OpenMPI or MSMPI.
          CheckMpi();
        } else {
	  CheckSolverNOMPI();
        }
	
        // Set the solver output directory.
        QString runPath = jobPath;
        int numProcs = ui->sliderNumProcs->value();

        /* [DaveP] sort of useless check.
	if(!m_UseMPI && (numProcs > 1)) {
            QMessageBox::warning(m_Parent, MsgTitle, "Cannot specify > 1 procs when not using MPI!");
            throw std::string("Cannot specify > 1 procs when not using MPI");
        }
        */

	if (!m_UseMPI) {
            numProcs = 1;
        }
	
        if (numProcs > 1) {
            runPath = jobPath+"/"+QString::number(numProcs)+"-procs_case";
        }

        // Get the simulation start time step and for numProcs=1
        // write the numstart.dat file. 
        auto startStep = GetStartTimeStep(runPath, jobPath, numProcs);

        // Execute the job.
        //
        int totalSteps=100;
        sv4guiSimJob* job = m_MitkJob->GetSimJob();

        if (!job) {
            QMessageBox::warning(m_Parent, MsgTitle, "Cannot start job, simulation job does not exist.");
            throw std::string("Job does not exist"); 
        }

        job->SetRunProp("Number of Processes",QString::number(numProcs).toStdString());
        totalSteps = QString::fromStdString(job->GetSolverProp("Number of Timesteps")).toInt();
        mitk::StatusBar::GetInstance()->DisplayText("Running simulation");

        QProcess* flowsolverProcess = new QProcess(m_Parent);
        flowsolverProcess->setWorkingDirectory(jobPath);

        if (m_UseMPI) {
            QStringList arguments;
            arguments << "-n" << QString::number(numProcs) << m_FlowsolverPath;
            flowsolverProcess->setProgram(m_MPIExecPath);
            flowsolverProcess->setArguments(arguments);
        } else {
            flowsolverProcess->setProgram(m_FlowsolverNOMPIPath);
            flowsolverProcess->setArguments(QStringList());
        }

        sv4guiSolverProcessHandler* handler = new sv4guiSolverProcessHandler(flowsolverProcess, m_JobNode, 
            startStep, totalSteps, runPath, m_Parent);

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
int sv4guiSimulationView::GetStartTimeStep(const QString& runPath, const QString& jobPath, const int numProcs)
{
    auto badValue = false;
    std::string exception("Write numstart file");

    // Process start time step from the GUI.
    //
    auto startStep = ui->lineEditStartStepNum->text().trimmed();
    auto startStepNumber = startStep.toInt();

    if (startStep == "") { 
        startStepNumber = 0;
    } else if ((startStepNumber < 0) || !IsInt(startStep.toStdString())) {
        QMessageBox::warning(m_Parent, MsgTitle, "The starting step number must be a positive integer.");
        throw exception; 
    }

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
        std::make_tuple("FlowSolver", m_FlowsolverPath),
        std::make_tuple("PreSolver", m_PresolverPath),
        std::make_tuple("PostSolver", m_PostsolverPath)
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

//-----------------
// CheckSolverNOMPI
//-----------------
// Check for valid solver binaries.
//
void sv4guiSimulationView::CheckSolverNOMPI()
{
    std::string exception("Check nompi solver");

    // Set the name and path to check for the solver binaries.
    typedef std::tuple<QString,QString> binaryNamePath;
    std::vector<binaryNamePath> binariesToCheck = { 
        std::make_tuple("FlowSolverNOMPI", m_FlowsolverNOMPIPath),
        std::make_tuple("PreSolver", m_PresolverPath),
        std::make_tuple("PostSolver", m_PostsolverPath)
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
    QString path = m_MPIExecPath; 

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

bool sv4guiSimulationView::CreateDataFiles(QString outputDir, bool outputAllFiles, bool updateJob, bool createFolder)
{
    if(!m_MitkJob)
        return false;

    if(outputDir=="")
        return false;

    sv4guiModelElement* modelElement=NULL;

    if(m_Model)
        modelElement=m_Model->GetModelElement();

    if(modelElement==NULL)
    {
        QMessageBox::warning(m_Parent,"Model Unavailable","Please make sure the model exists ans is valid.");
        return false;
    }

    mitk::StatusBar::GetInstance()->DisplayText("Creating Job");
    std::string msg;

    sv4guiSimJob* job=CreateJob(msg);

    if(job==NULL)
    {
        QMessageBox::warning(m_Parent,"Parameter Values Error",QString::fromStdString(msg));
        return false;
    }

    if(createFolder)
        outputDir=outputDir+"/"+QString::fromStdString(m_JobNode->GetName())+"-files";

    QDir dir(outputDir);
    dir.mkpath(outputDir);

    mitk::StatusBar::GetInstance()->DisplayText("Creating svpre file...");
    QString svpreFielContent=QString::fromStdString(sv4guiSimulationUtils::CreatePreSolverFileContent(job));
    QFile svpreFile(outputDir+"/"+QString::fromStdString(m_JobNode->GetName())+".svpre");
    if(svpreFile.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        QTextStream out(&svpreFile);
        out<<svpreFielContent;
        svpreFile.close();
    }

    auto capProps=job->GetCapProps();
    auto it = capProps.begin();
    while(it != capProps.end())
    {
        if(it->first!=""&&it->second["BC Type"]=="Prescribed Velocities")
        {
            auto props=it->second;
            std::ofstream out(outputDir.toStdString()+"/"+it->first+".flow");
            out << props["Flow Rate"];
            out.close();
        }
        it++;
    }

    mitk::StatusBar::GetInstance()->DisplayText("Creating solver.inp");
    QString solverFileContent=QString::fromStdString(sv4guiSimulationUtils::CreateFlowSolverFileContent(job));
    QFile solverFile(outputDir+"/solver.inp");
    if(solverFile.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        QTextStream out(&solverFile);
        out<<solverFileContent;
        solverFile.close();
    }

    QFile numStartFile(outputDir+"/numstart.dat");
    if(numStartFile.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        QTextStream out(&numStartFile);
        out<<"0\n";
        numStartFile.close();
    }

    QString rcrtFielContent=QString::fromStdString(sv4guiSimulationUtils::CreateRCRTFileContent(job));
    if(rcrtFielContent!="")
    {
        mitk::StatusBar::GetInstance()->DisplayText("Creating rcrt.dat");
        QFile rcrtFile(outputDir+"/rcrt.dat");
        if(rcrtFile.open(QIODevice::WriteOnly | QIODevice::Text))
        {
            QTextStream out(&rcrtFile);
            out<<rcrtFielContent;
            rcrtFile.close();
        }
    }

    QString cortFielContent=QString::fromStdString(sv4guiSimulationUtils::CreateCORTFileContent(job));
    if(cortFielContent!="")
    {
        mitk::StatusBar::GetInstance()->DisplayText("Creating cort.dat");
        QFile cortFile(outputDir+"/cort.dat");
        if(cortFile.open(QIODevice::WriteOnly | QIODevice::Text))
        {
            QTextStream out(&cortFile);
            out<<cortFielContent;
            cortFile.close();
        }
    }

    std::string meshName="";
    if(outputAllFiles)
    {
        meshName=ui->comboBoxMeshName->currentText().toStdString();

        mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
        mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);

        sv4guiMesh* mesh=NULL;
        mitk::DataNode::Pointer projFolderNode=NULL;
        mitk::DataNode::Pointer meshNode=NULL;

        if(rs->size()>0)
        {
            projFolderNode=rs->GetElement(0);

            rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiMeshFolder"));
            if (rs->size()>0)
            {
                mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);

                meshNode=GetDataStorage()->GetNamedDerivedNode(meshName.c_str(),meshFolderNode);
                if(meshNode.IsNotNull())
                {
                    sv4guiMitkMesh* mitkMesh=dynamic_cast<sv4guiMitkMesh*>(meshNode->GetData());
                    if(mitkMesh)
                    {
                        mesh=mitkMesh->GetMesh();
                    }
                }
            }
        }

        if(mesh==NULL)
        {
            QMessageBox::warning(m_Parent,"Mesh Unavailable","Please make sure the mesh exists and is valid.");
            return false;
        }

        mitk::StatusBar::GetInstance()->DisplayText("Creating mesh-complete files");
        QString meshCompletePath=outputDir+"/mesh-complete";
        dir.mkpath(meshCompletePath);
        WaitCursorOn();
        bool ok=sv4guiMeshLegacyIO::WriteFiles(meshNode,modelElement, meshCompletePath);
        WaitCursorOff();
        if(!ok)
        {
            QMessageBox::warning(m_Parent,"Mesh info missing","Please make sure the mesh exists and is valid.");
            return false;
        }

        QString presolverPath=m_PresolverPath;
        if(presolverPath=="")
            presolverPath=m_PresolverPath;

//        if(presolverPath=="" || !QFile(presolverPath).exists())
        if(presolverPath=="")
        {
            QMessageBox::warning(m_Parent,"Presolver Missing","Please make sure presolver exists!");
        }
        else
        {
            QString icFile=(QString::fromStdString(job->GetBasicProp("IC File"))).trimmed();
            if(icFile!="" && QFile(icFile).exists())
            {
                QString newFilePath=outputDir+"/restart.0.1";
                QFile::copy(icFile, newFilePath);
            }

            mitk::StatusBar::GetInstance()->DisplayText("Creating Data files: bct, restart, geombc,etc.");
            QProcess *presolverProcess = new QProcess(m_Parent);
            presolverProcess->setWorkingDirectory(outputDir);
            presolverProcess->setProgram(presolverPath);
            QStringList arguments;
            arguments << QString::fromStdString(m_JobNode->GetName()+".svpre");
            presolverProcess->setArguments(arguments);
#if defined(Q_OS_MAC)
            sv4guiProcessHandler* handler=new sv4guiProcessHandler(presolverProcess,m_JobNode,true,false,m_Parent);
#else
            sv4guiProcessHandler* handler=new sv4guiProcessHandler(presolverProcess,m_JobNode,true,true,m_Parent);
#endif
            handler->Start();
        }
    }

    m_MitkJob->SetSimJob(job);
    m_MitkJob->SetMeshName(meshName);
    m_MitkJob->SetDataModified();

    return true;
}

void sv4guiSimulationView::ImportFiles()
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

sv4guiSimJob* sv4guiSimulationView::CreateJob(std::string& msg, bool checkValidity)
{
    sv4guiSimJob* job=new sv4guiSimJob();

    //for basic
    for(int i=0;i<m_TableModelBasic->rowCount();i++)
    {
        std::string par=m_TableModelBasic->item(i,0)->text().toStdString();
        std::string values=m_TableModelBasic->item(i,1)->text().trimmed().toStdString();

        if(checkValidity)
        {
//            if(par=="Fluid Density" || par=="Fluid Viscosity" || par=="Period" || par=="Initial Pressure")
            if(par=="Fluid Density" || par=="Fluid Viscosity" || par=="Initial Pressure")
            {
                if(!IsDouble(values))
                {
                    msg=par + " value error: " + values;
                    delete job;
                    return NULL;
                }
            }
            else if(par=="Initial Velocities")
            {
                int count=0;

                QStringList list = QString(values.c_str()).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                values=list.join(" ").toStdString();

                if(!AreDouble(values,&count) || count!=3)
                {
                    msg=par + " value error: " + values;
                    delete job;
                    return NULL;
                }
            }
        }

        job->SetBasicProp(par,values);
    }

    //for cap bc
    for(int i=0;i<m_TableModelCap->rowCount();i++)
    {
        std::string capName=m_TableModelCap->item(i,0)->text().toStdString();
        std::string bcType=m_TableModelCap->item(i,1)->text().trimmed().toStdString();

        if(bcType=="Prescribed Velocities")
        {
            std::string flowrateContent=m_TableModelCap->item(i,9)->text().trimmed().toStdString();
            std::string period=m_TableModelCap->item(i,5)->text().trimmed().toStdString();

            if(checkValidity)
            {
                if(flowrateContent=="")
                {
                    msg=capName + ": no flowrate data";
                    delete job;
                    return NULL;
                }

                if(period=="")
                {
                    msg=capName + ": no period for flowrate data";
                    delete job;
                    return NULL;
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
        }
        else if(bcType!="")
        {
            std::string values=m_TableModelCap->item(i,2)->text().trimmed().toStdString();
            std::string pressure=m_TableModelCap->item(i,3)->text().trimmed().toStdString();
            std::string originalFile=m_TableModelCap->item(i,10)->text().trimmed().toStdString();
            std::string timedPressure=m_TableModelCap->item(i,11)->text().trimmed().toStdString();
            std::string pressurePeriod=m_TableModelCap->item(i,12)->text().trimmed().toStdString();
            std::string pressureScaling=m_TableModelCap->item(i,13)->text().trimmed().toStdString();
            std::string RValues=m_TableModelCap->item(i,14)->text().trimmed().toStdString();
            std::string CValues=m_TableModelCap->item(i,15)->text().trimmed().toStdString();

            if(checkValidity)
            {
                if(bcType=="Resistance")
                {
                    if(!IsDouble(values))
                    {
                        msg=capName + " R value error: " + values;
                        delete job;
                        return NULL;
                    }
                }
                else if(bcType=="RCR")
                {
                    int count=0;

                    QStringList list = QString(values.c_str()).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                    values=list.join(" ").toStdString();

                    if(!AreDouble(values,&count)||count!=3)
                    {
                        msg=capName + " RCR values error: " + values;
                        delete job;
                        return NULL;
                    }
                }
                else if(bcType=="Coronary")
                {
                    int count=0;

                    QStringList list = QString(values.c_str()).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                    values=list.join(" ").toStdString();

                    if(!AreDouble(values,&count)||count!=5)
                    {
                        msg=capName + " Coronary values error: " + values;
                        delete job;
                        return NULL;
                    }

                    if(timedPressure=="")
                    {
                        msg=capName + ": no Pim data";
                        delete job;
                        return NULL;
                    }

                    if(pressurePeriod=="" || !IsDouble(pressurePeriod))
                    {
                        msg=capName + " coronary period error: " + pressurePeriod;
                        delete job;
                        return NULL;
                    }

                    if(pressureScaling=="" || !IsDouble(pressureScaling))
                    {
                        msg=capName + " coronary pressure scaling error: " + pressureScaling;
                        delete job;
                        return NULL;
                    }
                }

                if(pressure!="")
                {
                    if(!IsDouble(pressure))
                    {
                        msg=capName + " pressure error: " + pressure;
                        delete job;
                        return NULL;
                    }
                }
                else
                {
                    pressure="0";
                }
            }

            job->SetCapProp(capName,"BC Type", bcType);
            job->SetCapProp(capName,"Values", values);
            job->SetCapProp(capName,"Pressure",pressure);

            if(bcType=="Coronary")
            {
                job->SetCapProp(capName,"Timed Pressure", timedPressure);
                job->SetCapProp(capName,"Pressure Period", pressurePeriod);
                job->SetCapProp(capName,"Pressure Scaling",pressureScaling);
                job->SetCapProp(capName,"Original File", originalFile);
            }

            if(bcType=="RCR" || bcType=="Coronary")
            {
                job->SetCapProp(capName,"R Values", RValues);
                job->SetCapProp(capName,"C Values", CValues);
            }
        }
    }

    //for wall and var
    int wallTypeIndex=ui->comboBoxWallType->currentIndex();
    if(wallTypeIndex==0)
    {
        job->SetWallProp("Type","rigid");
    }
    else if(wallTypeIndex==1)
    {
        std::string thickness=ui->lineEditThickness->text().trimmed().toStdString();
        std::string modulus=ui->lineEditE->text().trimmed().toStdString();
        std::string nu=ui->lineEditNu->text().trimmed().toStdString();
        std::string kcons=ui->lineEditKcons->text().trimmed().toStdString();
        std::string wallDensity=ui->lineEditWallDensity->text().trimmed().toStdString();
        std::string pressure=ui->lineEditPressure->text().trimmed().toStdString();

        if(checkValidity)
        {
            if(!IsDouble(thickness))
            {
                msg="wall thickness error: " + thickness;
                delete job;
                return NULL;
            }

            if(!IsDouble(modulus))
            {
                msg="wall elastic modulus error: " + modulus;
                delete job;
                return NULL;
            }

            if(!IsDouble(nu))
            {
                msg="wall Poisson ratio error: " + nu;
                delete job;
                return NULL;
            }

            if(!IsDouble(kcons))
            {
                msg="wall shear constant error: " + kcons;
                delete job;
                return NULL;
            }

            if(wallDensity!="")
            {
                if(!IsDouble(wallDensity))
                {
                    msg="wall density error: " + wallDensity;
                    delete job;
                    return NULL;
                }
            }
            else
            {
                wallDensity=job->GetBasicProp("Fluid Density");
            }

            if(!IsDouble(pressure))
            {
                msg="wall pressure error: " + pressure;
                delete job;
                return NULL;
            }
        }

        job->SetWallProp("Type","deformable");
        job->SetWallProp("Thickness",thickness);
        job->SetWallProp("Elastic Modulus",modulus);
        job->SetWallProp("Poisson Ratio",nu);
        job->SetWallProp("Shear Constant",kcons);
        job->SetWallProp("Density",wallDensity);
        job->SetWallProp("Pressure",pressure);
    }
    else if(wallTypeIndex==2)
    {
        std::string nu=ui->lineEditNu->text().trimmed().toStdString();
        std::string kcons=ui->lineEditKcons->text().trimmed().toStdString();
        std::string wallDensity=ui->lineEditWallDensity->text().trimmed().toStdString();
        std::string pressure=ui->lineEditPressure->text().trimmed().toStdString();

        if(checkValidity)
        {
            if(!IsDouble(nu))
            {
                msg="wall Poisson ratio error: " + nu;
                delete job;
                return NULL;
            }

            if(!IsDouble(kcons))
            {
                msg="wall shear constant error: " + kcons;
                delete job;
                return NULL;
            }

            if(wallDensity!="")
            {
                if(!IsDouble(wallDensity))
                {
                    msg="wall density error: " + wallDensity;
                    delete job;
                    return NULL;
                }
            }
            else
            {
                wallDensity=job->GetBasicProp("Fluid Density");
            }

            if(!IsDouble(pressure))
            {
                msg="wall pressure error: " + pressure;
                delete job;
                return NULL;
            }
        }

        job->SetWallProp("Type","variable");
        job->SetWallProp("Poisson Ratio",nu);
        job->SetWallProp("Shear Constant",kcons);
        job->SetWallProp("Density",wallDensity);
        job->SetWallProp("Pressure",pressure);

        for(int i=0;i<m_TableModelVar->rowCount();i++)
        {
            std::string faceName=m_TableModelVar->item(i,0)->text().toStdString();
            std::string thickness=m_TableModelVar->item(i,2)->text().trimmed().toStdString();
            std::string modulus=m_TableModelVar->item(i,3)->text().trimmed().toStdString();

            if(checkValidity)
            {
                if(thickness!="" && !IsDouble(thickness))
                {
                    msg="wall thickness error: " + thickness;
                    delete job;
                    return NULL;
                }

                if(modulus!="" && !IsDouble(modulus))
                {
                    msg="wall elastic modulus error: " + modulus;
                    delete job;
                    return NULL;
                }
            }

            job->SetVarProp(faceName,"Thickness", thickness);
            job->SetVarProp(faceName,"Elastic Modulus", modulus);
        }
    }

    for(int i=0;i<m_TableModelSolver->rowCount();i++)
    {
        std::string parName=m_TableModelSolver->item(i,0)->text().trimmed().toStdString();
        QStandardItem* valueItem=m_TableModelSolver->item(i,1);
        if(valueItem==NULL)
            continue;

        std::string value=valueItem->text().trimmed().toStdString();
        std::string type=m_TableModelSolver->item(i,2)->text().trimmed().toStdString();

        if(checkValidity )
        {
            if(value=="")
            {
                msg=parName+ " missing value";
                delete job;
                return NULL;
            }
            else if(type=="int"&&!IsInt(value))
            {
                msg=parName+ " value error: " + value;
                delete job;
                return NULL;
            }
            else if(type=="double"&&!IsDouble(value))
            {
                msg=parName+ " value error: " + value;
                delete job;
                return NULL;
            }
        }

        job->SetSolverProp(parName, value);
    }


    return job;
}

void sv4guiSimulationView::SaveToManager()
{
    if(!m_MitkJob)
        return;

    std::string msg;

    sv4guiSimJob* job=CreateJob(msg);

    if(job==NULL)
    {
        QMessageBox::warning(m_Parent,"Parameter Values Error",QString::fromStdString(msg));
        return;
    }

    m_MitkJob->SetSimJob(job);
    m_MitkJob->SetDataModified();
}

void sv4guiSimulationView::SetResultDir()
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

//---------------
// ExportResults
//---------------
//
void sv4guiSimulationView::ExportResults()
{
    QString postsolverPath=m_PostsolverPath;
    if(postsolverPath=="")
        postsolverPath=m_PostsolverPath;

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

    sv4guiProcessHandler* handler=new sv4guiProcessHandler(postsolverProcess,m_JobNode,false,false,m_Parent);
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

            calculateFlows=sv4guiSimulationUtils::CreateFlowFiles(outFlowFilePath.toStdString(), outPressureFlePath.toStdString()
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

void sv4guiSimulationView::EnableTool(bool able)
{
    ui->widgetTop->setEnabled(able);
    ui->page->setEnabled(able);
    ui->page_2->setEnabled(able);
    ui->page_3->setEnabled(able);
    ui->page_4->setEnabled(able);
    ui->page_5->setEnabled(able);
}

void sv4guiSimulationView::UpdateSimJob()
{
    if(!m_MitkJob)
        return;

    sv4guiSimJob* job=m_MitkJob->GetSimJob();
    std::string numProcsStr="";
    if(job)
    {
        numProcsStr=job->GetRunProp("Number of Processes");
    }

    std::string msg="";
    sv4guiSimJob* newJob=CreateJob(msg,false);
    if(newJob==NULL)
        return;

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

void sv4guiSimulationView::UpdateSimJobNumProcs()
{
    if(!m_MitkJob)
        return;

    sv4guiSimJob* job=m_MitkJob->GetSimJob();
    if(job)
    {
        std::string numProcsStr=QString::number((int)(ui->sliderNumProcs->value())).toStdString();
        job->SetRunProp("Number of Processes",numProcsStr);
        m_MitkJob->SetDataModified();
    }
}

void sv4guiSimulationView::ShowCalculateFowsWidget(bool checked)
{
    ui->widgetCalculateFlows->setVisible(checked);
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

void sv4guiSimulationView::UpdateJobStatus()
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
    }
    else
    {
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

//--------
// UseMpi
//--------
// Process 'Use MPI' check box selection.
//
// Disable number of processors if MPI is not selected. Use disable rather 
// than show/hide, which does not work for labels.
//
void sv4guiSimulationView::UseMpi(bool checked)
{
    ui->sliderNumProcs->setEnabled(checked);
    ui->NumberProcessesLabel->setEnabled(checked);
    m_UseMPI = checked;
}


