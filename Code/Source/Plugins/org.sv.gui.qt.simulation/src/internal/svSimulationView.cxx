#include "svSimulationView.h"
#include "ui_svSimulationView.h"

#include "svTableCapDelegate.h"
#include "svTableSolverDelegate.h"
#include "svMitkMesh.h"
#include "svMeshLegacyIO.h"
#include "svSimulationUtils.h"

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

const QString svSimulationView::EXTENSION_ID = "org.sv.views.simulation";

svSimulationView::svSimulationView() :
    ui(new Ui::svSimulationView)
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

svSimulationView::~svSimulationView()
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

void svSimulationView::EnableConnection(bool able)
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

void svSimulationView::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);

    //    m_DisplayWidget=GetActiveStdMultiWidget();

    //    if(m_DisplayWidget==NULL)
    //    {
    //        parent->setEnabled(false);
    //        MITK_ERROR << "Plugin Simulation Init Error: No QmitkStdMultiWidget Available!";
    //        return;
    //    }

    ui->btnSave->hide();
//    connect(ui->btnSave, SIGNAL(clicked()), this, SLOT(SaveToManager()) );

    connect(ui->checkBoxShowModel, SIGNAL(clicked(bool)), this, SLOT(ShowModel(bool)) );

    ui->toolBox->setCurrentIndex(0);

    //for basic table
    m_TableModelBasic = new QStandardItemModel(this);
    ui->tableViewBasic->setModel(m_TableModelBasic);

    connect( ui->tableViewBasic, SIGNAL(doubleClicked(const QModelIndex&))
             , this, SLOT(TableViewBasicDoubleClicked(const QModelIndex&)) );

    //for cap table
    m_TableModelCap = new QStandardItemModel(this);
    ui->tableViewCap->setModel(m_TableModelCap);
    svTableCapDelegate* itemDelegate=new svTableCapDelegate(this);
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

    m_CapBCWidget=new svCapBCWidget();
    m_CapBCWidget->move(400,400);
    m_CapBCWidget->hide();
    m_CapBCWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_CapBCWidget,SIGNAL(accepted()), this, SLOT(SetCapBC()));

    m_SplitBCWidget=new svSplitBCWidget();
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
    svTableSolverDelegate* itemSolverDelegate=new svTableSolverDelegate(this);
    ui->tableViewSolver->setItemDelegateForColumn(1,itemSolverDelegate);

    //for data file and run
//    connect(ui->btnExportInputFiles, SIGNAL(clicked()), this, SLOT(ExportInputFiles()) );
//    connect(ui->btnExportAllFiles, SIGNAL(clicked()), this, SLOT(ExportAllFiles()) );
    connect(ui->btnCreateAllFiles, SIGNAL(clicked()), this, SLOT(CreateAllFiles()) );
    connect(ui->btnImportFiles, SIGNAL(clicked()), this, SLOT(ImportFiles()) );
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
}

void svSimulationView::SetupInternalSolverPaths()
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

void svSimulationView::OnPreferencesChanged(const berry::IBerryPreferences* prefs)
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

void svSimulationView::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
    //    if(!IsActivated())
    if(!IsVisible())
    {
        return;
    }

    if(nodes.size()==0)
    {
        RemoveObservers();
        EnableTool(false);
        return;
    }

    mitk::DataNode::Pointer jobNode=nodes.front();
    svMitkSimJob* mitkJob=dynamic_cast<svMitkSimJob*>(jobNode->GetData());

    if(!mitkJob)
    {
        RemoveObservers();
        EnableTool(false);
        return;
    }

    //comment this section to always update
//    if(m_JobNode==jobNode)
//    {
//        AddObservers();
//        EnableTool(true);
//        return;
//    }

    std::string modelName=mitkJob->GetModelName();

    mitk::DataNode::Pointer modelNode=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (jobNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svModelFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer modelFolderNode=rs->GetElement(0);
            modelNode=GetDataStorage()->GetNamedDerivedNode(modelName.c_str(),modelFolderNode);
        }
    }

    svModel* model=NULL;
    if(modelNode.IsNotNull())
    {
        model=dynamic_cast<svModel*>(modelNode->GetData());
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

void svSimulationView::NodeChanged(const mitk::DataNode* node)
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

void svSimulationView::NodeAdded(const mitk::DataNode* node)
{

}

void svSimulationView::NodeRemoved(const mitk::DataNode* node)
{

}

void svSimulationView::Visible()
{
    OnSelectionChanged(GetDataManagerSelection());
}

void svSimulationView::Hidden()
{
    RemoveObservers();
}

void svSimulationView::AddObservers()
{
    if(m_ModelNode.IsNotNull())
    {
        if(m_ModelNode->GetDataInteractor().IsNull())
        {
            m_DataInteractor = svModelDataInteractor::New();
            m_DataInteractor->LoadStateMachine("svModelInteraction.xml", us::ModuleRegistry::GetModule("svModel"));
            m_DataInteractor->SetEventConfig("svModelConfig.xml", us::ModuleRegistry::GetModule("svModel"));
            m_DataInteractor->SetDataNode(m_ModelNode);
        }
        m_ModelNode->SetStringProperty("interactor user","simulation");
        svModelDataInteractor* interactor=dynamic_cast<svModelDataInteractor*>(m_ModelNode->GetDataInteractor().GetPointer());
        if(interactor)
            interactor->SetFaceSelectionOnly();
    }

    if(m_Model && m_ModelSelectFaceObserverTag==-1)
    {
        itk::SimpleMemberCommand<svSimulationView>::Pointer modelSelectFaceCommand = itk::SimpleMemberCommand<svSimulationView>::New();
        modelSelectFaceCommand->SetCallbackFunction(this, &svSimulationView::UpdateFaceListSelection);
        m_ModelSelectFaceObserverTag = m_Model->AddObserver( svModelSelectFaceEvent(), modelSelectFaceCommand);
    }
}

void svSimulationView::RemoveObservers()
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

void svSimulationView::ClearAll()
{
    m_Model=NULL;
    m_JobNode=NULL;
    m_MitkJob=NULL;
    m_ModelNode=NULL;

    ui->labelJobName->setText("");
    ui->labelJobStatus->setText("");
    ui->labelModelName->setText("");
}

void svSimulationView::UpdateGUIBasic()
{
    if(!m_MitkJob)
        return;

    svSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
        job=new svSimJob();
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

void svSimulationView::TableViewBasicDoubleClicked(const QModelIndex& index)
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

void svSimulationView::UpdateFaceListSelection()
{
    if(!m_Model)
        return;

    svModelElement* modelElement=m_Model->GetModelElement();
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

void svSimulationView::TableCapSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    mitk::StatusBar::GetInstance()->DisplayText("");

    if(!m_Model)
        return;

    svModelElement* modelElement=m_Model->GetModelElement();
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

void svSimulationView::TableViewCapDoubleClicked(const QModelIndex& index)
{
    if(index.column()==0)
        ShowCapBCWidget();
}

void svSimulationView::TableViewCapContextMenuRequested( const QPoint & pos )
{
    m_TableMenuCap->popup(QCursor::pos());
}

void svSimulationView::ShowCapBCWidget(bool)
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

void svSimulationView::SetDistalPressure(bool)
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

void  svSimulationView::SetCapBC()
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

void svSimulationView::ShowSplitBCWidget(QString splitTarget)
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

void svSimulationView::ShowSplitBCWidgetR(bool)
{
    ShowSplitBCWidget("Resistance");
}

void svSimulationView::ShowSplitBCWidgetC(bool)
{
    ShowSplitBCWidget("Capacitance");
}

void  svSimulationView::SplitCapBC()
{
    if(!m_MitkJob)
        return;

    if(!m_Model)
        return;

    if(!m_SplitBCWidget)
        return;

    svModelElement* modelElement=m_Model->GetModelElement();
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

void svSimulationView::UpdateGUICap()
{
    if(!m_MitkJob)
        return;

    if(!m_Model)
        return;

    svModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) return;

    svSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
        job=new svSimJob();
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
        svModelElement::svFace* face=modelElement->GetFace(ids[i]);
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

void svSimulationView::WallTypeSelectionChanged(int index)
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

void svSimulationView::TableVarSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    svModelElement* modelElement=m_Model->GetModelElement();
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

void svSimulationView::TableViewVarContextMenuRequested( const QPoint & pos )
{
    m_TableMenuVar->popup(QCursor::pos());
}

void svSimulationView::SetVarThickness(bool)
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

void svSimulationView::SetVarE(bool)
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

void svSimulationView::UpdateGUIWall()
{
    if(!m_MitkJob)
        return;

    svSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
        job=new svSimJob();
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

    svModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL) return;

    m_TableModelVar->clear();

    QStringList varHeaders;
    varHeaders << "Name" << "Type" << "Thickness" << "E. Modulus";
    m_TableModelVar->setHorizontalHeaderLabels(varHeaders);
    m_TableModelVar->setColumnCount(4);

    std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();
    int rowIndex=-1;
    for(int i=0;i<faces.size();i++)
    {
        svModelElement::svFace* face=faces[i];
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

void svSimulationView::UpdateGUISolver()
{
    if(!m_MitkJob)
        return;

    svSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
    {
        job=new svSimJob();
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

void svSimulationView::UpdateGUIJob()
{
    if(!m_MitkJob)
        return;

    std::string modelName=m_MitkJob->GetModelName();
    std::vector<std::string> meshNames;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svMeshFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);
            rs=GetDataStorage()->GetDerivations(meshFolderNode);

            for(int i=0;i<rs->size();i++)
            {
                svMitkMesh* mitkMesh=dynamic_cast<svMitkMesh*>(rs->GetElement(i)->GetData());
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

    svSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
        return;

    std::string pNum=job->GetRunProp("Number of Processes");
    ui->sliderNumProcs->setValue(pNum==""?1:QString::fromStdString(pNum).toInt());
}

void svSimulationView::UpdateGUIRunDir()
{
    ui->lineEditResultDir->clear();

    if(m_JobNode.IsNull())
        return;

    QString jobPath=GetJobPath();
    if(jobPath=="")
        return;

    if(!m_MitkJob)
        return;

    svSimJob* job=m_MitkJob->GetSimJob();
    if(job==NULL)
        return;

    std::string pNum=job->GetRunProp("Number of Processes");
    if(pNum=="")
        return;

    QString runDir=pNum=="1"?jobPath:jobPath+"/"+QString::fromStdString(pNum)+"-procs_case";
    ui->lineEditResultDir->setText(runDir);
}

//void svSimulationView::ExportInputFiles()
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

//void svSimulationView::ExportAllFiles()
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

QString svSimulationView::GetJobPath()
{
    QString jobPath="";

    if(m_JobNode.IsNull())
        return jobPath;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);

    std::string projPath="";
    std::string simFolderName="";

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);
        projFolderNode->GetStringProperty("project path", projPath);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svSimulationFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer simFolderNode=rs->GetElement(0);
            simFolderName=simFolderNode->GetName();
            jobPath=QString::fromStdString(projPath+"/"+simFolderName+"/"+m_JobNode->GetName());
        }
    }

    return jobPath;
}

void svSimulationView::CreateAllFiles()
{
    if(!m_MitkJob)
        return;

    CreateDataFiles(GetJobPath(), true, true, false);
}

void svSimulationView::RunJob()
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
    svSimJob* job=m_MitkJob->GetSimJob();
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

    svSolverProcessHandler* handler=new svSolverProcessHandler(flowsolverProcess,m_JobNode,startStep,totalSteps,runPath,m_Parent);
    handler->Start();
}

bool svSimulationView::CreateDataFiles(QString outputDir, bool outputAllFiles, bool updateJob, bool createFolder)
{
    if(!m_MitkJob)
        return false;

    if(outputDir=="")
        return false;

    svModelElement* modelElement=NULL;

    if(m_Model)
        modelElement=m_Model->GetModelElement();

    if(modelElement==NULL)
    {
        QMessageBox::warning(m_Parent,"Model Unavailable","Please make sure the model exists ans is valid.");
        return false;
    }

    mitk::StatusBar::GetInstance()->DisplayText("Creating Job");
    std::string msg;

    svSimJob* job=CreateJob(msg);

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
    QString svpreFielContent=QString::fromStdString(svSimulationUtils::CreatePreSolverFileContent(job));
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
    QString solverFileContent=QString::fromStdString(svSimulationUtils::CreateFlowSolverFileContent(job));
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

    QString rcrtFielContent=QString::fromStdString(svSimulationUtils::CreateRCRTFileContent(job));
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

    QString cortFielContent=QString::fromStdString(svSimulationUtils::CreateCORTFileContent(job));
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

        mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
        mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_JobNode,isProjFolder,false);

        svMesh* mesh=NULL;
        mitk::DataNode::Pointer projFolderNode=NULL;
        mitk::DataNode::Pointer meshNode=NULL;

        if(rs->size()>0)
        {
            projFolderNode=rs->GetElement(0);

            rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svMeshFolder"));
            if (rs->size()>0)
            {
                mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);

                meshNode=GetDataStorage()->GetNamedDerivedNode(meshName.c_str(),meshFolderNode);
                if(meshNode.IsNotNull())
                {
                    svMitkMesh* mitkMesh=dynamic_cast<svMitkMesh*>(meshNode->GetData());
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
        bool ok=svMeshLegacyIO::WriteFiles(meshNode,modelElement, meshCompletePath);
        WaitCursorOff();
        if(!ok)
        {
            QMessageBox::warning(m_Parent,"Mesh info missing","Please make sure the mesh exists and is valid.");
            return false;
        }

        QString presolverPath=m_ExternalPresolverPath;
        if(presolverPath=="")
            presolverPath=m_InternalPresolverPath;

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
            svProcessHandler* handler=new svProcessHandler(presolverProcess,m_JobNode,true,false,m_Parent);
#else
            svProcessHandler* handler=new svProcessHandler(presolverProcess,m_JobNode,true,true,m_Parent);
#endif
            handler->Start();
        }
    }

    m_MitkJob->SetSimJob(job);
    m_MitkJob->SetMeshName(meshName);
    m_MitkJob->SetDataModified();

    return true;
}

void svSimulationView::ImportFiles()
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

svSimJob* svSimulationView::CreateJob(std::string& msg, bool checkValidity)
{
    svSimJob* job=new svSimJob();

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

void svSimulationView::SaveToManager()
{
    if(!m_MitkJob)
        return;

    std::string msg;

    svSimJob* job=CreateJob(msg);

    if(job==NULL)
    {
        QMessageBox::warning(m_Parent,"Parameter Values Error",QString::fromStdString(msg));
        return;
    }

    m_MitkJob->SetSimJob(job);
    m_MitkJob->SetDataModified();
}

void svSimulationView::SetResultDir()
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

void svSimulationView::ExportResults()
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

    svProcessHandler* handler=new svProcessHandler(postsolverProcess,m_JobNode,false,false,m_Parent);
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

            calculateFlows=svSimulationUtils::CreateFlowFiles(outFlowFilePath.toStdString(), outPressureFlePath.toStdString()
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

bool svSimulationView::IsInt(std::string value)
{
    bool ok;
    QString(value.c_str()).toInt(&ok);
    return ok;
}

bool svSimulationView::IsDouble(std::string value)
{
    bool ok;
    QString(value.c_str()).toDouble(&ok);
    return ok;
}

bool svSimulationView::AreDouble(std::string values, int* count)
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

void svSimulationView::EnableTool(bool able)
{
    ui->widgetTop->setEnabled(able);
    ui->page->setEnabled(able);
    ui->page_2->setEnabled(able);
    ui->page_3->setEnabled(able);
    ui->page_4->setEnabled(able);
    ui->page_5->setEnabled(able);
}

void svSimulationView::UpdateSimJob()
{
    if(!m_MitkJob)
        return;

    svSimJob* job=m_MitkJob->GetSimJob();
    std::string numProcsStr="";
    if(job)
    {
        numProcsStr=job->GetRunProp("Number of Processes");
    }

    std::string msg="";
    svSimJob* newJob=CreateJob(msg,false);
    if(newJob==NULL)
        return;

    newJob->SetRunProp("Number of Processes",numProcsStr);
    m_MitkJob->SetSimJob(newJob);
    m_MitkJob->SetDataModified();
}

void svSimulationView::UdpateSimJobMeshName()
{
    if(!m_MitkJob)
        return;

    std::string meshName=ui->comboBoxMeshName->currentText().toStdString();
    m_MitkJob->SetMeshName(meshName);
    m_MitkJob->SetDataModified();
}

void svSimulationView::UpdateSimJobNumProcs()
{
    if(!m_MitkJob)
        return;

    svSimJob* job=m_MitkJob->GetSimJob();
    if(job)
    {
        std::string numProcsStr=QString::number((int)(ui->sliderNumProcs->value())).toStdString();
        job->SetRunProp("Number of Processes",numProcsStr);
        m_MitkJob->SetDataModified();
    }
}

void svSimulationView::ShowCalculateFowsWidget(bool checked)
{
    ui->widgetCalculateFlows->setVisible(checked);
}

#if defined(Q_OS_WIN)
QString svSimulationView::FindLatestKey(QString key, QStringList keys)
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

QString svSimulationView::GetRegistryValue(QString category, QString key)
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

void svSimulationView::UpdateJobStatus()
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

void svSimulationView::ShowModel(bool checked)
{
    if(m_ModelNode.IsNotNull())
    {
        m_ModelNode->SetVisibility(checked);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

svProcessHandler::svProcessHandler(QProcess* process, mitk::DataNode::Pointer jobNode, bool multithreading, bool stoppable, QWidget* parent)
    : m_Process(process)
    , m_JobNode(jobNode)
    , m_Parent(parent)
    , m_MessageBox(NULL)
    , m_Stoppable(stoppable)
    , m_MultiThreading(multithreading)
{
}

svProcessHandler::~svProcessHandler()
{
    if(m_Process)
        delete m_Process;

    if(m_MessageBox)
        delete m_MessageBox;
}

void svProcessHandler::Start()
{
    if(m_Process==NULL)
        return;

    if(m_MultiThreading)
        connect(m_Process,SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(AfterProcessFinished(int,QProcess::ExitStatus)));

    m_Process->start();

    if(!m_MultiThreading)
    {
        m_Process->waitForFinished(-1);
        m_Message=m_Process->readAllStandardOutput()+"\n"+m_Process->readAllStandardError();
    }

    if(m_MultiThreading && m_Stoppable)
    {
        m_MessageBox= new QMessageBox(m_Parent);
        m_MessageBox->setWindowTitle("Processing");
        m_MessageBox->setText("Processing data and creating files...                                 ");
        m_MessageBox->setInformativeText("Click \"OK\" to continue in background.\nClick \"Abort\" to terminate.");
        m_MessageBox->setStandardButtons(QMessageBox::Ok | QMessageBox::Abort);
        m_MessageBox->setDefaultButton(QMessageBox::Ok);

        int ret = m_MessageBox->exec();
        if(ret==QMessageBox::Abort && m_Process)
            m_Process->kill();
    }
}

void svProcessHandler::AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
    if(m_MessageBox)
    {
        delete m_MessageBox;
        m_MessageBox=NULL;
    }

    QString title="";
    QString text="";
    QMessageBox::Icon icon=QMessageBox::NoIcon;
    QMessageBox mb(m_Parent);

    if(exitStatus==QProcess::NormalExit)
    {
        title="Finished";
        text="Data files have been created.                                                                                         ";
        icon=QMessageBox::Information;
        if(m_JobNode.IsNotNull())
        {
            svMitkSimJob* mitkJob=dynamic_cast<svMitkSimJob*>(m_JobNode->GetData());
            if(mitkJob)
            {
                mitkJob->SetStatus("Input/Data files created");
                m_JobNode->SetBoolProperty("dummy",true);//trigger NodeChanged to update job status
                mitk::StatusBar::GetInstance()->DisplayText("Data files have been created: restart, geombc, etc.");
            }
        }
    }
    else
    {
        title="Not finished";
        text="Failed to finish creating data files.                                                                                 ";
        icon=QMessageBox::Warning;
    }

    mb.setWindowTitle(title);
    mb.setText(text);
    mb.setIcon(icon);

    if(m_Process)
        mb.setDetailedText(m_Process->readAllStandardOutput()+"\n"+m_Process->readAllStandardError());

    mb.setDefaultButton(QMessageBox::Ok);

    mb.exec();

    deleteLater();
}

svSolverProcessHandler::svSolverProcessHandler(QProcess* process, mitk::DataNode::Pointer jobNode, int startStep, int totalSteps, QString runDir, QWidget* parent)
    : m_Process(process)
    , m_JobNode(jobNode)
    , m_StartStep(startStep)
    , m_TotalSteps(totalSteps)
    , m_RunDir(runDir)
    , m_Parent(parent)
    , m_Timer(NULL)
{
}

svSolverProcessHandler::~svSolverProcessHandler()
{
    if(m_Process)
        delete m_Process;

    if(m_Timer)
        delete m_Timer;
}

void svSolverProcessHandler::Start()
{
    if(m_Process==NULL)
        return;

    if(m_JobNode.IsNull())
        return;

    connect(m_Process,SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(AfterProcessFinished(int,QProcess::ExitStatus)));

    m_JobNode->SetBoolProperty("running", true);
    m_JobNode->SetDoubleProperty("running progress", 0);
    mitk::GenericProperty<svSolverProcessHandler*>::Pointer solverProcessProp=mitk::GenericProperty<svSolverProcessHandler*>::New(this);
    m_JobNode->SetProperty("process handler",solverProcessProp);

    m_Process->start();

    m_Timer = new QTimer(this);
    connect(m_Timer, SIGNAL(timeout()), this, SLOT(UpdateStatus()));
    m_Timer->start(3000);
}

void svSolverProcessHandler::KillProcess()
{
    if(m_Process)
        m_Process->kill();
}

void svSolverProcessHandler::AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
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

    svMitkSimJob* mitkJob=dynamic_cast<svMitkSimJob*>(m_JobNode->GetData());
    if(mitkJob)
        mitkJob->SetStatus(status.toStdString());

    m_JobNode->SetBoolProperty("running",false);
    m_JobNode->SetDoubleProperty("running progress", 0);

    mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());

    deleteLater();
}

void svSolverProcessHandler::UpdateStatus()
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

        list=info.split(QRegExp("\\s+"),QString::SkipEmptyParts);
        QString stepStr=list.first();
        bool ok;
        int step=stepStr.toInt(&ok);
        if(ok)
            currentStep=step;

        historFile.close();
    }

    double progress=0;
    if(currentStep>m_StartStep && m_TotalSteps>0)
        progress=(currentStep-m_StartStep)*1.0/m_TotalSteps;

    m_JobNode->SetDoubleProperty("running progress", progress);

    QString status=QString::fromStdString(m_JobNode->GetName())+": running, " +QString::number((int)(progress*100))+"% completed. Info: "+info;
    mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());
}
