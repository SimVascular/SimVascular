#include "svSimulationView.h"
#include "ui_svSimulationView.h"

#include "svTableCapDelegate.h"
#include "svTableSolverDelegate.h"

#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <usModuleRegistry.h>

#include <QTreeView>
#include <QInputDialog>
#include <QMessageBox>
#include <QDomDocument>
#include <QDomElement>

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

    connect(ui->btnSave, SIGNAL(clicked()), this, SLOT(SaveToManager()) );


    //for basic table
    m_TableModelBasic = new QStandardItemModel(this);
    ui->tableViewBasic->setModel(m_TableModelBasic);

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

    m_CapBCWidget=new svCapBCWidget();
    m_CapBCWidget->move(400,400);
    m_CapBCWidget->hide();
    m_CapBCWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_CapBCWidget,SIGNAL(accepted()), this, SLOT(SetCapBC()));

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
//    connect(ui->btnCreateDataFiles, SIGNAL(clicked()), this, SLOT(CreateDataFiles()) );

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
        m_Parent->setEnabled(false);
        return;
    }

    mitk::DataNode::Pointer jobNode=nodes.front();
    svMitkSimJob* mitkJob=dynamic_cast<svMitkSimJob*>(jobNode->GetData());

    if(!mitkJob)
    {
        RemoveObservers();
        m_Parent->setEnabled(false);
        return;
    }

    if(m_JobNode==jobNode)
    {
        AddObservers();
        m_Parent->setEnabled(true);
        return;
    }

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
        m_Parent->setEnabled(false);
    }
    else
    {
        m_Parent->setEnabled(true);
        AddObservers();
    }

    //update top part
    //======================================================================
    ui->labelJobName->setText(QString::fromStdString(m_JobNode->GetName()));
    ui->labelJobStatus->setText(QString::fromStdString(m_MitkJob->GetStatus()));
    ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));

    UpdateGUIBasic();

    UpdateGUICap();

    UpdateGUIWall();

    UpdateGUISolver();

    //        UpdateGUIRun();

    UpdateFaceListSelection();

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svSimulationView::NodeChanged(const mitk::DataNode* node)
{
    if(m_JobNode==node)
        ui->labelJobName->setText(QString::fromStdString(m_JobNode->GetName()));
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

    if(m_ModelSelectFaceObserverTag==-1)
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

    parList<<new QStandardItem("Period");
    value=QString::fromStdString(job->GetBasicProp("Period"));
    valueList<<new QStandardItem(value==""?QString("1.0"):value);

    parList<<new QStandardItem("Initial Pressure");
    value=QString::fromStdString(job->GetBasicProp("Initial Pressure"));
    valueList<<new QStandardItem(value==""?QString("0"):value);

    parList<<new QStandardItem("Initial Velocities");
    value=QString::fromStdString(job->GetBasicProp("Initial Velocities"));
    valueList<<new QStandardItem(value==""?QString("0 0 0"):value);


    for(int i=0;i<parList.size();i++)
    {
        m_TableModelBasic->setItem(i, 0, parList[i]);
        m_TableModelBasic->setItem(i, 1, valueList[i]);
    }

    ui->tableViewBasic->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->tableViewBasic->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
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
    props["Original File"]=m_TableModelCap->item(row,10)->text().toStdString();

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
        if(props["BC Type"]=="Resistance" || props["BC Type"]=="RCR")
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
    }

}

void svSimulationView::UpdateGUICap()
{
    if(!m_MitkJob)
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
    capHeaders << "Name" << "BC Type" << "Values" << "Pressure" << "Analytic Shape" << "Period" << "Point Number" << "Fourier Modes" << "Flip Normal" << "Flow Rate" << "Original File";
    m_TableModelCap->setHorizontalHeaderLabels(capHeaders);
    m_TableModelCap->setColumnCount(11);

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
    }

    ui->tableViewCap->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui->tableViewCap->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    ui->tableViewCap->horizontalHeader()->resizeSection(1,100);
    ui->tableViewCap->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);

    for(int i=3;i<11;i++)
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

    QStringList solverHeaders;
    solverHeaders << "Parameter" << "Value" << "Type" << "Value List";
    m_TableModelSolver->setHorizontalHeaderLabels(solverHeaders);
    int colCount=solverHeaders.size();
    m_TableModelSolver->setColumnCount(colCount);

    QFile xmlFile(":solvertemplate.xml");
    if(!xmlFile.open(QIODevice::ReadOnly))
    {
        QMessageBox::warning(NULL,"Info Missing","Solver Parameter Table template file not found");
        return;
    }

    QDomDocument doc("solvertemplate");
//    QString *em=NULL;
    if(!doc.setContent(&xmlFile))
    {
        QMessageBox::warning(NULL,"File Template Error","Format Error.");
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

svSimJob* svSimulationView::CreateJob(std::string& msg)
{
    svSimJob* job=new svSimJob();

    //for basic
    for(int i=0;i<m_TableModelBasic->rowCount();i++)
    {
        std::string par=m_TableModelBasic->item(i,0)->text().toStdString();
        std::string values=m_TableModelBasic->item(i,1)->text().trimmed().toStdString();

        if(par=="Fluid Density" || par=="Fluid Viscosity" || par=="Period" || par=="Initial Pressure")
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
            if(!AreDouble(values,&count) || count!=3)
            {
                msg=par + " value error: " + values;
                delete job;
                return NULL;
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
            if(flowrateContent=="")
            {
                msg=capName + ": no flowrate data";
                delete job;
                return NULL;
            }

            std::string shape=m_TableModelCap->item(i,4)->text().trimmed().toStdString();
            std::string period=m_TableModelCap->item(i,5)->text().trimmed().toStdString();
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
            job->SetCapProp(capName,"BC Type", bcType);

            std::string values=m_TableModelCap->item(i,2)->text().trimmed().toStdString();
            if(bcType=="Resistance")
            {
                if(!IsDouble(values))
                {
                    msg=capName + " R value error: " + values;
                    delete job;
                    return NULL;
                }
                job->SetCapProp(capName,"Values", values);
            }
            else if(bcType=="RCR")
            {
                int count=0;
                if(!AreDouble(values,&count)||count!=3)
                {
                    msg=capName + " RCR values error: " + values;
                    delete job;
                    return NULL;
                }
                job->SetCapProp(capName,"Values", values);
            }

            std::string pressure=m_TableModelCap->item(i,3)->text().trimmed().toStdString();
            if(pressure!="")
            {
                if(!IsDouble(pressure))
                {
                    msg=capName + " pressure error: " + pressure;
                    delete job;
                    return NULL;
                }
                job->SetCapProp(capName,"Pressure",pressure);
            }
            else
            {
                job->SetCapProp(capName,"Pressure","0");
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
        job->SetWallProp("Type","deformable");

        std::string thickness=ui->lineEditThinkness->text().trimmed().toStdString();
        if(!IsDouble(thickness))
        {
            msg="wall thickness error: " + thickness;
            delete job;
            return NULL;
        }
        job->SetWallProp("Thickness",thickness);

        std::string modulus=ui->lineEditE->text().trimmed().toStdString();
        if(!IsDouble(modulus))
        {
            msg="wall elastic modulus error: " + modulus;
            delete job;
            return NULL;
        }
        job->SetWallProp("Elastic Modulus",modulus);

        std::string nu=ui->lineEditNu->text().trimmed().toStdString();
        if(!IsDouble(nu))
        {
            msg="wall Poisson ratio error: " + nu;
            delete job;
            return NULL;
        }
        job->SetWallProp("Poisson Ratio",nu);

        std::string kcons=ui->lineEditKcons->text().trimmed().toStdString();
        if(!IsDouble(kcons))
        {
            msg="wall shear constant error: " + kcons;
            delete job;
            return NULL;
        }
        job->SetWallProp("Shear Constant",kcons);

        std::string pressure=ui->lineEditPressure->text().trimmed().toStdString();
        if(!IsDouble(pressure))
        {
            msg="wall pressure error: " + pressure;
            delete job;
            return NULL;
        }
        job->SetWallProp("Pressure",pressure);

    }
    else if(wallTypeIndex==2)
    {
        job->SetWallProp("Type","variable");

        std::string nu=ui->lineEditNu->text().trimmed().toStdString();
        if(!IsDouble(nu))
        {
            msg="wall Poisson ratio error: " + nu;
            delete job;
            return NULL;
        }
        job->SetWallProp("Poisson Ratio",nu);

        std::string kcons=ui->lineEditKcons->text().trimmed().toStdString();
        if(!IsDouble(kcons))
        {
            msg="wall shear constant error: " + kcons;
            delete job;
            return NULL;
        }
        job->SetWallProp("Shear Constant",kcons);

        std::string pressure=ui->lineEditPressure->text().trimmed().toStdString();
        if(!IsDouble(pressure))
        {
            msg="wall pressure error: " + pressure;
            delete job;
            return NULL;
        }
        job->SetWallProp("Pressure",pressure);

        for(int i=0;i<m_TableModelVar->rowCount();i++)
        {
            std::string faceName=m_TableModelVar->item(i,0)->text().toStdString();
            std::string thickness=m_TableModelVar->item(i,2)->text().trimmed().toStdString();
            std::string modulus=m_TableModelVar->item(i,3)->text().trimmed().toStdString();

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

            job->SetVarProp(faceName,"Thickness", thickness);
            job->SetVarProp(faceName,"Elastic Modulus", modulus);

        }
    }

    for(int i=0;i<m_TableModelSolver->rowCount();i++)
    {
        std::string parName=m_TableModelSolver->item(i,0)->text().toStdString();
        QStandardItem* valueItem=m_TableModelSolver->item(i,1);
        if(valueItem==NULL)
            continue;

        std::string value=valueItem->text().trimmed().toStdString();
        std::string type=m_TableModelSolver->item(i,2)->text().toStdString();

        if(type=="int"&&!IsInt(value))
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
        QMessageBox::warning(NULL,"Parameter Values Error",QString::fromStdString(msg));
        return;
    }

    m_MitkJob->SetSimJob(job);
    m_MitkJob->SetDataModified();
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
    QStringList list = QString(values.c_str()).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
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



