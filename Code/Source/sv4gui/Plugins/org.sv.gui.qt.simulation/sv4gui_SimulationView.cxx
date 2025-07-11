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
    m_MitkJob=nullptr;
    m_Model=nullptr;
    m_JobNode=nullptr;
    m_ModelNode=nullptr;

    m_DataInteractor=nullptr;
    m_ModelSelectFaceObserverTag=-1;

    m_TableModelBasic=nullptr;

    m_TableModelCap=nullptr;
    m_TableMenuCap=nullptr;

    m_TableModelVar=nullptr;
    m_TableMenuVar=nullptr;

    m_CapBCWidget=nullptr;

    m_TableModelSolver=nullptr;

    m_SolverPath="";
    m_MPIExecPath="";

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

//------------------
// EnableConnection
//------------------
//
void sv4guiSimulationView::EnableConnection(bool able)
{
    if(able && !m_ConnectionEnabled) {
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

    if(!able && m_ConnectionEnabled) {
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
    connect(ui->btnCreateAllFiles, SIGNAL(clicked()), this, SLOT(CreateAllFiles()) );
    connect(ui->btnRunJob, SIGNAL(clicked()), this, SLOT(RunJob()) );
    ui->NumberProcessesLabel->setEnabled(true);
    ui->sliderNumProcs->setEnabled(true);

    // Set paths for the external solvers.
    mitk::IPreferences* prefs = this->GetPreferences();
    this->OnPreferencesChanged(prefs);
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
    if (!m_isVisible) return;

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
    mitk::DataNode::Pointer modelNode=nullptr;
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

    sv4guiModel* model=nullptr;
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

    if(m_Model==nullptr)
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
        if(m_ModelNode->IsVisible(nullptr))
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

void sv4guiSimulationView::UpdateGUIBasic()
{
    if(!m_MitkJob)
        return;

    sv4guiSimJob* job=m_MitkJob->GetSimJob();
    if(job==nullptr)
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

    parList<<new QStandardItem("Initial Pressure");
    value=QString::fromStdString(job->GetBasicProp("Initial Pressure"));
    valueList<<new QStandardItem(value==""?QString("0"):value);

    parList<<new QStandardItem("Initial Velocities");
    value=QString::fromStdString(job->GetBasicProp("Initial Velocities"));
    valueList<<new QStandardItem(value==""?QString("0.0001 0.0001 0.0001"):value);

    for(int i=0;i<parList.size();i++) {
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

    mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    mitk::IPreferences* prefs;

    if (prefService)
    {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    }
    else
    {
        prefs = nullptr;
    }

    if(lastFileOpenPath=="" || !QFile(lastFileOpenPath).exists())
    {
        if(prefs != nullptr) 
        {
            lastFileOpenPath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
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

    if(prefs != nullptr) 
     {
         prefs->Put("LastFileOpenPath", icFilePath.toStdString());
         prefs->Flush();
     }

    itemValue->setText(icFilePath);
}

void sv4guiSimulationView::UpdateFaceListSelection()
{
    if(!m_Model)
        return;

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==nullptr) return;


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
    if(modelElement==nullptr) return;

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
    if(modelElement==nullptr)
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

                QStringList list = m_TableModelCap->item(row,15)->text().split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
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

                QStringList list = m_TableModelCap->item(row,15)->text().split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
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

                QStringList list = m_TableModelCap->item(row,14)->text().split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
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

                QStringList list = m_TableModelCap->item(row,14)->text().split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
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
    if(modelElement==nullptr) return;

    sv4guiSimJob* job=m_MitkJob->GetSimJob();
    if(job==nullptr)
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
        if(face==nullptr )
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
    if(modelElement==nullptr) return;

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
    if(job==nullptr)
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
    if(modelElement==nullptr) return;

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
        if(face==nullptr )
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

//-----------------
// UpdateGUISolver
//-----------------
//
void sv4guiSimulationView::UpdateGUISolver()
{
  if(!m_MitkJob) {
    return;
  }

  sv4guiSimJob* job = m_MitkJob->GetSimJob();

  if(job == nullptr) {
    job = new sv4guiSimJob();
  }

  m_TableModelSolver->clear();

  QStringList solverHeaders;
  solverHeaders << "Parameter" << "Value" << "Type" << "Value List";
  m_TableModelSolver->setHorizontalHeaderLabels(solverHeaders);
  int colCount=solverHeaders.size();
  m_TableModelSolver->setColumnCount(colCount);

  QString templateFilePath=":solvertemplate.xml";

  // davep 
  /*if(m_UseCustom) {
    // davep templateFilePath=m_SolverTemplatePath;
  }
  */

  QFile xmlFile(templateFilePath);

  if(!xmlFile.open(QIODevice::ReadOnly)) {
    QMessageBox::warning(m_Parent,"Info Missing","Solver Parameter Table template file not found");
    return;
  }

  QDomDocument doc("solvertemplate");
  //    QString *em=nullptr;

  if(!doc.setContent(&xmlFile)) {
    QMessageBox::warning(m_Parent,"File Template Error","Format Error.");
    return;
  }
  xmlFile.close();

  QDomElement templateElement = doc.firstChildElement("template");
  QDomNodeList sectionList = templateElement.elementsByTagName("section");
  int rowIndex = -1;

  for (int i = 0; i < sectionList.size(); i++) {
    QDomNode sectionNode = sectionList.item(i);

    if(sectionNode.isNull()) {
      continue;
    }

    QDomElement sectionElement=sectionNode.toElement();
    if(sectionElement.isNull()) {
      continue;
    }
    
    QString section_name = sectionElement.attribute("name");
    QStandardItem* item = new QStandardItem(section_name);
    item->setEditable(false);
    QBrush brushGray(Qt::lightGray);
    item->setBackground(brushGray);

    rowIndex += 1;
    m_TableModelSolver->setItem(rowIndex, 0, item);
    ui->tableViewSolver->setSpan(rowIndex, 0, 1, colCount);
    QDomNodeList parList = sectionElement.elementsByTagName("param");

    for (int j = 0; j < parList.size(); j++) {
      QDomNode parNode = parList.item(j);

      if(parNode.isNull()) {
        continue;
      }

      QDomElement parElement = parNode.toElement();
      if(parElement.isNull()) continue;
      QString name = parElement.attribute("name");

      QStandardItem* item = new QStandardItem(name);
      item->setEditable(false);
      item->setToolTip(parElement.attribute("name"));

      rowIndex += 1;
      m_TableModelSolver->setItem(rowIndex, 0, item);

      // Save the section that this paramter is under, needed later to set parameter props
      // based on section so duplicate parameter names can be used.
      m_TableModelSolverSections[rowIndex] = section_name.toStdString();

      std::string value = job->GetSolverProp(parElement.attribute("name").toStdString());
      item = new QStandardItem(value == "" ? parElement.attribute("value"):QString::fromStdString(value));
      m_TableModelSolver->setItem(rowIndex, 1, item);

      item = new QStandardItem(parElement.attribute("type"));
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
	
    // Set the solver output directory.
    QString runPath = jobPath;
    int numProcs = ui->sliderNumProcs->value();

    if (numProcs > 1) {
      runPath = jobPath+"/"+QString::number(numProcs)+"-procs_case";
    }

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
  #define debug_CreateDataFiles
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

  sv4guiSimJob* job = CreateJob(job_msg);

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

  return true;
}

//-----------
// CreateJob
//-----------
//
sv4guiSimJob* sv4guiSimulationView::CreateJob(std::string& msg, bool checkValidity)
{
  sv4guiSimJob* job = new sv4guiSimJob();

  // Set basic properties values.
  //
  for(int i=0;i<m_TableModelBasic->rowCount();i++) {
    std::string par=m_TableModelBasic->item(i,0)->text().toStdString();
    std::string values=m_TableModelBasic->item(i,1)->text().trimmed().toStdString();

    if(checkValidity) {

      if(par=="Fluid Density" || par=="Fluid Viscosity" || par=="Initial Pressure") {
        if(!IsDouble(values)) {
          msg=par + " value error: " + values;
          delete job;
          return nullptr;
        }

      } else if(par=="Initial Velocities") {
        int count=0;

        QStringList list = QString(values.c_str()).split(QRegularExpression("[(),{}\\s+]"), Qt::SkipEmptyParts);
        values=list.join(" ").toStdString();

        if(!AreDouble(values,&count) || count!=3) {
          msg=par + " value error: " + values;
          delete job;
          return nullptr;
        }
      }
    }

  job->SetBasicProp(par,values);
  }

  // Set cap properties values.
  //
  for(int i=0;i<m_TableModelCap->rowCount();i++) {
    std::string capName=m_TableModelCap->item(i,0)->text().toStdString();
    std::string bcType=m_TableModelCap->item(i,1)->text().trimmed().toStdString();

    if(bcType=="Prescribed Velocities") {
      std::string flowrateContent=m_TableModelCap->item(i,9)->text().trimmed().toStdString();
      std::string period=m_TableModelCap->item(i,5)->text().trimmed().toStdString();

      if(checkValidity) {
        if(flowrateContent=="") {
          msg=capName + ": no flowrate data";
          delete job;
          return nullptr;
        }

        if(period=="") {
          msg=capName + ": no period for flowrate data";
          delete job;
          return nullptr;
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

    } else if(bcType != "") {
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
            delete job;
            return nullptr;
          }

        } else if(bcType=="RCR") {
          int count=0;
          QStringList list = QString(values.c_str()).split(QRegularExpression("[(),{}\\s+]"), Qt::SkipEmptyParts);
          values=list.join(" ").toStdString();

          if(!AreDouble(values,&count)||count!=3) {
            msg=capName + " RCR values error: " + values;
            delete job;
            return nullptr;
          }

        } else if(bcType=="Coronary") {
          int count=0;
          QStringList list = QString(values.c_str()).split(QRegularExpression("[(),{}\\s+]"), Qt::SkipEmptyParts);
          values=list.join(" ").toStdString();

          if(!AreDouble(values,&count)||count!=5) {
            msg=capName + " Coronary values error: " + values;
            delete job;
            return nullptr;
          }

          if(timedPressure=="") {
            msg=capName + ": no Pim data";
            delete job;
            return nullptr;
          }

          if(pressurePeriod=="" || !IsDouble(pressurePeriod)) {
            msg=capName + " coronary period error: " + pressurePeriod;
            delete job;
            return nullptr;
          }

          if(pressureScaling=="" || !IsDouble(pressureScaling)) {
            msg=capName + " coronary pressure scaling error: " + pressureScaling;
            delete job;
            return nullptr;
          }
        }

        if(pressure!="") {
          if(!IsDouble(pressure)) {
            msg=capName + " pressure error: " + pressure;
            delete job;
            return nullptr;
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

  // Set wall properties values.
  int wallTypeIndex=ui->comboBoxWallType->currentIndex();

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
        delete job;
        return nullptr;
      }

      if(!IsDouble(modulus)) {
        msg="wall elastic modulus error: " + modulus;
        delete job;
        return nullptr;
      }

      if(!IsDouble(nu)) {
        msg="wall Poisson ratio error: " + nu;
        delete job;
        return nullptr;
      }

      if(!IsDouble(kcons)) {
        msg="wall shear constant error: " + kcons;
        delete job;
        return nullptr;
      }

      if(wallDensity!="") {
        if(!IsDouble(wallDensity)) {
          msg="wall density error: " + wallDensity;
          delete job;
          return nullptr;
        }
      } else {
        wallDensity=job->GetBasicProp("Fluid Density");
      }

      if(!IsDouble(pressure)) {
        msg="wall pressure error: " + pressure;
        delete job;
        return nullptr;
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
        delete job;
        return nullptr;
      }

      if(!IsDouble(kcons)) {
        msg="wall shear constant error: " + kcons;
        delete job;
        return nullptr;
      }

      if(wallDensity!="") {
        if(!IsDouble(wallDensity)) {
          msg="wall density error: " + wallDensity;
          delete job;
          return nullptr;
        }
      } else {
        wallDensity=job->GetBasicProp("Fluid Density");
      }

      if(!IsDouble(pressure)) {
        msg="wall pressure error: " + pressure;
        delete job;
        return nullptr;
      }
    }

    job->SetWallProp("Type","variable");
    job->SetWallProp("Poisson Ratio",nu);
    job->SetWallProp("Shear Constant",kcons);
    job->SetWallProp("Density",wallDensity);
    job->SetWallProp("Pressure",pressure);

    for(int i=0;i<m_TableModelVar->rowCount();i++) {
      std::string faceName=m_TableModelVar->item(i,0)->text().toStdString();
      std::string thickness=m_TableModelVar->item(i,2)->text().trimmed().toStdString();
      std::string modulus=m_TableModelVar->item(i,3)->text().trimmed().toStdString();

      if(checkValidity) {
        if(thickness!="" && !IsDouble(thickness)) {
          msg="wall thickness error: " + thickness;
          delete job;
          return nullptr;
        }

        if(modulus!="" && !IsDouble(modulus)) {
          msg="wall elastic modulus error: " + modulus;
          delete job;
          return nullptr;
        }
      }

      job->SetVarProp(faceName,"Thickness", thickness);
      job->SetVarProp(faceName,"Elastic Modulus", modulus);
    }
  }

  for (int i = 0; i < m_TableModelSolver->rowCount(); i++) {
    std::string parName=m_TableModelSolver->item(i,0)->text().trimmed().toStdString();
    QStandardItem* valueItem=m_TableModelSolver->item(i,1);

    if(valueItem==nullptr) {
      continue;
    }

    std::string value=valueItem->text().trimmed().toStdString();
    std::string type=m_TableModelSolver->item(i,2)->text().trimmed().toStdString();

    if(checkValidity ) {
      if(value=="") {
        msg=parName+ " missing value";
        delete job;
        return nullptr;

      } else if(type=="int"&&!IsInt(value)) {
        msg=parName+ " value error: " + value;
        delete job;
        return nullptr;

      } else if(type=="double"&&!IsDouble(value)) {
        msg=parName+ " value error: " + value;
        delete job;
        return nullptr;
      }
    }

    job->SetSolverProp(parName, value, m_TableModelSolverSections[i]);
  }

  return job;
}

//---------------
// SaveToManager
//---------------
//
void sv4guiSimulationView::SaveToManager()
{
    if(!m_MitkJob)
        return;

    std::string msg;

    sv4guiSimJob* job=CreateJob(msg);

    if(job==nullptr)
    {
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

void sv4guiSimulationView::EnableTool(bool able)
{
    ui->widgetTop->setEnabled(able);
    ui->page->setEnabled(able);
    ui->page_2->setEnabled(able);
    ui->page_3->setEnabled(able);
    ui->page_4->setEnabled(able);
    ui->page_5->setEnabled(able);
}

//---------------
// UpdateSimJob
//---------------
//
void sv4guiSimulationView::UpdateSimJob()
{
    if (!m_MitkJob) {
        return;
    }

    sv4guiSimJob* job = m_MitkJob->GetSimJob();
    std::string numProcsStr = "";

    if (job) {
      numProcsStr = job->GetRunProp("Number of Processes");
    }

    std::string msg="";
    sv4guiSimJob* newJob = CreateJob(msg,false);

    if (newJob==nullptr) {
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


