#include "svMeshEdit.h"
#include "ui_svMeshEdit.h"

#include "svModel.h"
#include "svModelElementPolyData.h"

#include "svMeshTetGen.h"
#include "svMesh.h"
#include "svMitkMesh.h"

#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <usModuleRegistry.h>

#include <vtkProperty.h>

#include <QTreeView>
#include <QInputDialog>
#include <QColorDialog>
#include <QSignalMapper>
#include <QMessageBox>

#include <iostream>
using namespace std;

const QString svMeshEdit::EXTENSION_ID = "org.sv.views.meshedit";

svMeshEdit::svMeshEdit() :
    ui(new Ui::svMeshEdit)
{
    m_MitkMesh=NULL;
    m_Model=NULL;
    m_MeshNode=NULL;

    m_ModelSelectFaceObserverTag=0;
    m_SphereObserverTag=0;

    m_TableMenuLocalT=NULL;
    m_TableModelLocalT=NULL;

    m_TableMenuRegionT=NULL;
    m_TableModelRegionT=NULL;

    m_SphereWidget=NULL;

    m_SelectedRegionIndex=-1;
}

svMeshEdit::~svMeshEdit()
{
    delete ui;
}

void svMeshEdit::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);

//    parent->setMaximumWidth(450);

    m_DisplayWidget=GetActiveStdMultiWidget();

    if(m_DisplayWidget==NULL)
    {
        parent->setEnabled(false);
        MITK_ERROR << "Plugin MeshEdit Init Error: No QmitkStdMultiWidget!";
        return;
    }

    SetupTetGenGUI(parent);

    if(m_SphereWidget==NULL)
    {
        m_SphereWidget = vtkSmartPointer<vtkSphereWidget>::New();
        m_SphereWidget->SetInteractor(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow()->GetInteractor());
    //    m_SphereWidget->SetRepresentationToSurface();
    }
}

void svMeshEdit::SetupTetGenGUI(QWidget *parent )
{
    connect(ui->btnRunMesherT, SIGNAL(clicked()), this, SLOT(RunMesher()) );
    connect(ui->btnEstimateT, SIGNAL(clicked()), this, SLOT(EstimateEdgeSize()) );

    //for local table
    m_TableModelLocalT = new QStandardItemModel(this);
    ui->tableViewLocalT->setModel(m_TableModelLocalT);

    connect( ui->tableViewLocalT->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableFaceListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    m_TableMenuLocalT=new QMenu(ui->tableViewLocalT);
    QAction* setLocalTAction=m_TableMenuLocalT->addAction("Set Local Size");
    QAction* clearLocalTAction=m_TableMenuLocalT->addAction("Clear Local Size");
    connect( setLocalTAction, SIGNAL( triggered(bool) ) , this, SLOT( SetLocal(bool) ) );
    connect( clearLocalTAction, SIGNAL( triggered(bool) ) , this, SLOT( ClearLocal(bool) ) );

    connect( ui->tableViewLocalT, SIGNAL(customContextMenuRequested(const QPoint&))
      , this, SLOT(TableViewLocalContextMenuRequested(const QPoint&)) );

    //for sphere table
    connect(ui->checkBoxSphereT, SIGNAL(toggled(bool)), this, SLOT(ShowSphereInteractor(bool)));
    connect(ui->btnAddSphereT, SIGNAL(clicked()), this, SLOT(AddSphere()) );

    m_TableModelRegionT = new QStandardItemModel(this);
    ui->tableViewRegionT->setModel(m_TableModelRegionT);

    connect( ui->tableViewRegionT->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableRegionListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    m_TableMenuRegionT=new QMenu(ui->tableViewRegionT);
    QAction* deleteRegionTAction=m_TableMenuRegionT->addAction("Delete");
    connect( deleteRegionTAction, SIGNAL( triggered(bool) ) , this, SLOT( DeleteSelectedRegions(bool) ) );

    connect( ui->tableViewRegionT, SIGNAL(customContextMenuRequested(const QPoint&))
      , this, SLOT(TableViewRegionContextMenuRequested(const QPoint&)) );

    //for command history
    connect(ui->btnRunHistoryT, SIGNAL(clicked()), this, SLOT(RunHistory()) );
}

void svMeshEdit::TableFaceListSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    QStandardItemModel* tableModel=NULL;
    QTableView* tableView=NULL;

    if(m_MeshType=="TetGen")
    {
        tableModel=m_TableModelLocalT;
        tableView=ui->tableViewLocalT;
    }
    else if(m_MeshType=="MeshSim")
    {
//        tableModel=m_TableModelLocalM;
//        tableView=ui->tableViewLocalM;
    }

    if(tableModel==NULL || tableView==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= tableModel->item(row,0);
        int id=itemID->text().toInt();

        modelElement->SelectFace(id);
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svMeshEdit::SetLocal(bool)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    QStandardItemModel* tableModel=NULL;
    QTableView* tableView=NULL;

    if(m_MeshType=="TetGen")
    {
        tableModel=m_TableModelLocalT;
        tableView=ui->tableViewLocalT;
    }
    else if(m_MeshType=="MeshSim")
    {
//        tableModel=m_TableModelLocalM;
//        tableView=ui->tableViewLocalM;
    }

    if(tableModel==NULL || tableView==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    bool ok=false;
    QString localInfo="";

    if(m_MeshType=="TetGen")
    {
        double localSize=QInputDialog::getDouble(m_Parent, "Set Local Size", "Local Size:", 0.0, 0, 100, 4, &ok);
        localInfo=QString::number(localSize);
    }
    else if(m_MeshType=="MeshSim")
    {

    }

    if(!ok)
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
       ; it != indexesOfSelectedRows.end(); it++)
     {
       int row=(*it).row();

       QStandardItem* item= tableModel->item(row,3);
       item->setText(localInfo);
     }
}

void svMeshEdit::ClearLocal(bool)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    QStandardItemModel* tableModel=NULL;
    QTableView* tableView=NULL;

    if(m_MeshType=="TetGen")
    {
        tableModel=m_TableModelLocalT;
        tableView=ui->tableViewLocalT;
    }
    else if(m_MeshType=="MeshSim")
    {
//        tableModel=m_TableModelLocalM;
//        tableView=ui->tableViewLocalM;
    }

    if(tableModel==NULL || tableView==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
       ; it != indexesOfSelectedRows.end(); it++)
     {
       int row=(*it).row();

       QStandardItem* item= tableModel->item(row,3);
       item->setText("");
     }
}

void svMeshEdit::TableViewLocalContextMenuRequested( const QPoint & pos )
{
    if(m_MeshType=="TetGen")
    {
        m_TableMenuLocalT->popup(QCursor::pos());
    }
    else if(m_MeshType=="MeshSim")
    {
//        m_TableMenuLocalM->popup(QCursor::pos());
    }
}

void svMeshEdit::TableRegionListSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    QStandardItemModel* tableModel=NULL;
    QTableView* tableView=NULL;

    if(m_MeshType=="TetGen")
    {
        tableModel=m_TableModelRegionT;
        tableView=ui->tableViewRegionT;
    }
    else if(m_MeshType=="MeshSim")
    {
//        tableModel=m_TableModelRegionM;
//        tableView=ui->tableViewRegionM;
    }

    if(tableModel==NULL || tableView==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();

    m_SelectedRegionIndex=-1;

    if(m_SphereWidget&&m_SphereWidget->GetEnabled())
    {
        m_SphereWidget->GetSphereProperty()->SetColor(1.0,1.0,1.0);
    }

    if(indexesOfSelectedRows.size()==0)
    {
        return;
    }

    int row=indexesOfSelectedRows[0].row();
    m_SelectedRegionIndex=row;

    QStandardItem* itemShape= tableModel->item(row,0);
    QString shape=itemShape->text();

    QStandardItem* itemLocal= tableModel->item(row,1);
    QString localSize=itemLocal->text();

    QStandardItem* itemParams= tableModel->item(row,2);
    QString params=itemParams->text();

    if(m_SphereWidget&&m_SphereWidget->GetEnabled())
    {
        m_SphereWidget->GetSphereProperty()->SetColor(1.0,0.0,0.0);
        QStringList plist = params.split(QRegExp("\\s+"));
        m_SphereWidget->SetRadius(plist[0].toDouble());
        m_SphereWidget->SetCenter(plist[1].toDouble(),plist[2].toDouble(),plist[3].toDouble());
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svMeshEdit::DeleteSelectedRegions(bool)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    QStandardItemModel* tableModel=NULL;
    QTableView* tableView=NULL;

    if(m_MeshType=="TetGen")
    {
        tableModel=m_TableModelRegionT;
        tableView=ui->tableViewRegionT;
    }
    else if(m_MeshType=="MeshSim")
    {
//        tableModel=m_TableModelRegionM;
//        tableView=ui->tableViewRegionM;
    }

    if(tableModel==NULL || tableView==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
       ; it != indexesOfSelectedRows.end(); it++)
     {
       int row=(*it).row();
       tableModel->removeRow(row);
     }
}

void svMeshEdit::TableViewRegionContextMenuRequested( const QPoint & pos )
{
    if(m_MeshType=="TetGen")
    {
        m_TableMenuRegionT->popup(QCursor::pos());
    }
    else if(m_MeshType=="MeshSim")
    {
//        m_TableMenuRegionM->popup(QCursor::pos());
    }
}

void svMeshEdit::EstimateEdgeSize()
{
    if(!m_MitkMesh) return;

    if(!m_Model) return;
    svModelElement* modelElement=dynamic_cast<svModelElement*>(m_Model->GetModelElement());
    if(!modelElement) return;

    double minArea=0;

    std::vector<int> faceIDs=modelElement->GetAllFaceIDs();
    for(int i=0;i<faceIDs.size();i++)
    {
        if(i==0)
            minArea=modelElement->GetFaceArea(faceIDs[i]);
        else
        {
            double area=modelElement->GetFaceArea(faceIDs[i]);
            if(area<minArea)
                minArea=area;
        }
    }

    double edgeSize= sqrt(minArea/3.1415)/2.5;
    edgeSize=round(10000*edgeSize)/10000;

    ui->lineEditGlobalEdgeSizeT->setText(QString::number(edgeSize));
}

void svMeshEdit::RunMesher()
{
    RunCommands(true);
}

void svMeshEdit::RunHistory()
{
    RunCommands(false);
}

void svMeshEdit::RunCommands(bool fromGUI)
{
    if(!m_MitkMesh) return;

    if(!m_Model) return;
    svModelElementPolyData* modelElement=dynamic_cast<svModelElementPolyData*>(m_Model->GetModelElement());
    if(!modelElement) return;

    svMesh* mesh=NULL;

    if(fromGUI)
    {
        mesh=new svMeshTetGen();
    }
    else
    {
        mesh=m_MitkMesh->GetMesh(GetTimeStep());
        if(mesh==NULL)
        {
            QMessageBox::warning(NULL,"Mesh Not Been Created!","The function is for an existing mesh!");
            return;
        }
    }

    mesh->InitNewMesher();
    mesh->SetModelElement(modelElement);

    if(fromGUI)
    {
        std::vector<std::string> cmds;

//        QString text=ui->plainTextEdit->toPlainText();
//        QStringList list=text.split("\n");

//        for(int i=0;i<list.size();i++)
//        {
//            cmds.push_back(list[i].toStdString());
//        }

        std::string msg;
        if(!mesh->ExecuteCommands(cmds, msg))
        {
            QMessageBox::warning(NULL,"Error during executing",QString::fromStdString(msg));
            return;
        }
        mesh->SetCommandHistory(cmds);

        m_MitkMesh->SetMesh(mesh);
    }
    else
    {
        std::string msg;
        if(!mesh->ExecuteCommandHistory(msg))
        {
            QMessageBox::warning(NULL,"Error during executing",QString::fromStdString(msg));
            return;
        }
    }
}

void svMeshEdit::Visible()
{
    OnSelectionChanged(GetDataManagerSelection());
}

void svMeshEdit::Hidden()
{
    ClearAll();
}

int svMeshEdit::GetTimeStep()
{
    mitk::SliceNavigationController* timeNavigationController = NULL;
    if(m_DisplayWidget)
    {
        timeNavigationController=m_DisplayWidget->GetTimeNavigationController();
    }

    if(timeNavigationController)
        return timeNavigationController->GetTime()->GetPos();
    else
        return 0;
}

void svMeshEdit::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
//    if(!IsActivated())
    if(!IsVisible())
    {
        return;
    }

    if(nodes.size()==0)
    {
        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    mitk::DataNode::Pointer meshNode=nodes.front();

//    if(m_ModelNode==modelNode)
//    {
////        return;
//    }

    ClearAll();

    m_MeshNode=meshNode;
    m_MitkMesh=dynamic_cast<svMitkMesh*>(meshNode->GetData());
    if(!m_MitkMesh)
    {
        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    std::string modelName=m_MitkMesh->GetModelName();

    mitk::DataNode::Pointer modelNode=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_MeshNode,isProjFolder,false);

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

    if(modelNode.IsNotNull())
    {
        m_Model=dynamic_cast<svModel*>(modelNode->GetData());
        m_ModelNode=modelNode;
    }

    if(m_Model==NULL)
    {
        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    m_Parent->setEnabled(true);

    m_MeshType=m_MitkMesh->GetType();

    UpdateGUI();

    m_DataInteractor = svModelDataInteractor::New();
    m_DataInteractor->SetFaceSelectionOnly();
    m_DataInteractor->LoadStateMachine("svModelInteraction.xml", us::ModuleRegistry::GetModule("svModel"));
    m_DataInteractor->SetEventConfig("svModelConfig.xml", us::ModuleRegistry::GetModule("svModel"));
    m_DataInteractor->SetDataNode(m_ModelNode);

    //Add Observers
    itk::SimpleMemberCommand<svMeshEdit>::Pointer modelSelectFaceCommand = itk::SimpleMemberCommand<svMeshEdit>::New();
    modelSelectFaceCommand->SetCallbackFunction(this, &svMeshEdit::UpdateFaceListSelection);
    m_ModelSelectFaceObserverTag = m_Model->AddObserver( svModelSelectFaceEvent(), modelSelectFaceCommand);

    vtkSmartPointer<vtkCallbackCommand> sphereChangedCommand = vtkSmartPointer<vtkCallbackCommand>::New();
    sphereChangedCommand->SetCallback(&svMeshEdit::UpdateSphereData);
    m_SphereObserverTag = m_SphereWidget->AddObserver(vtkCommand::InteractionEvent, sphereChangedCommand);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svMeshEdit::UpdateGUI()
{
    //update top part
    //------------------------------------------------------------------------
    ui->labelMeshName->setText(QString::fromStdString(m_MeshNode->GetName()));
    ui->labelMeshType->setText(QString::fromStdString(m_MeshType));

    if(!m_MitkMesh)
        return;

    if(!m_Model)
        return;

    if(m_MeshType=="TetGen")
    {
        UpdateTetGenGUI();
    }
    else if(m_MeshType=="MeshSim")
    {
//        UpdateMeshSimGUI();
    }
}

void svMeshEdit::UpdateTetGenGUI()
{
    //put default values
    //==========================================

    //global size
    ui->lineEditGlobalEdgeSizeT->clear();

    //advanced options
    ui->checkBoxBoundaryLayerT->setChecked(false);
    ui->dsbPortionT->setValue(0.5);
    ui->sbLayersT->setValue(2);
    ui->dsbRatioT->setValue(0.8);

    ui->checkBoxRadiusBasedT->setChecked(false);

    ui->checkBoxSurfaceT->setChecked(false);
    ui->checkBoxVolumeT->setChecked(false);

    //local size
    m_TableModelLocalT->clear();

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();

    QStringList faceListHeaders;
    faceListHeaders << "ID" << "Name" << "Type" << "Local Size";
    m_TableModelLocalT->setHorizontalHeaderLabels(faceListHeaders);
    m_TableModelLocalT->setColumnCount(4);

    int rowIndex=-1;

    for(int i=0;i<faces.size();i++)
    {
        if(faces[i]==NULL )
            continue;

        rowIndex++;
        m_TableModelLocalT->insertRow(rowIndex);

        QStandardItem* item;

        item= new QStandardItem(QString::number(faces[i]->id));
        item->setEditable(false);
        m_TableModelLocalT->setItem(rowIndex, 0, item);

        item= new QStandardItem(QString::fromStdString(faces[i]->name));
        item->setEditable(false);
        m_TableModelLocalT->setItem(rowIndex, 1, item);

        item= new QStandardItem(QString::fromStdString(faces[i]->type));
        item->setEditable(false);
        m_TableModelLocalT->setItem(rowIndex, 2, item);

        item= new QStandardItem("");
        m_TableModelLocalT->setItem(rowIndex, 3, item);
    }

    ui->tableViewLocalT->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Fixed);
    ui->tableViewLocalT->horizontalHeader()->resizeSection(0,20);
    ui->tableViewLocalT->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Interactive);
    ui->tableViewLocalT->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Fixed);
    ui->tableViewLocalT->horizontalHeader()->resizeSection(2,60);
    ui->tableViewLocalT->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Fixed);
    ui->tableViewLocalT->horizontalHeader()->resizeSection(3,80);

    ui->tableViewLocalT->setColumnHidden(0,true);

    //regional refinement
    m_TableModelRegionT->clear();

    QStringList regionListHeaders;
    regionListHeaders << "Type" << "Local Size" << "Parameters";
    m_TableModelRegionT->setHorizontalHeaderLabels(faceListHeaders);
    m_TableModelRegionT->setColumnCount(3);

    ui->tableViewRegionT->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Fixed);
    ui->tableViewRegionT->horizontalHeader()->resizeSection(0,80);
    ui->tableViewRegionT->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    ui->tableViewRegionT->horizontalHeader()->resizeSection(1,80);
    ui->tableViewRegionT->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Interactive);

    //advanced flags
    ui->checkBoxFlagO->setChecked(true);
    ui->sliderFlagO->setValue(3);

    ui->checkBoxFlagQ->setChecked(true);
    ui->sliderFlagQ->setValue(1.4);

    ui->checkBoxFlagT->setChecked(false);
    ui->lineEditFlagT->setText("1e-8");

    ui->checkBoxFlagY->setChecked(true);
    ui->checkBoxFlagM->setChecked(false);
    ui->checkBoxFlagD->setChecked(false);
    ui->checkBoxFlagC->setChecked(false);
    ui->checkBoxFlagQ2->setChecked(false);
    ui->checkBoxFlagV->setChecked(false);

    UpdateFaceListSelection();

    //then udpate with command history
    //========================================
    svMeshTetGen* mesh=dynamic_cast<svMeshTetGen*>(m_MitkMesh->GetMesh(GetTimeStep()));
    if(mesh==NULL)
        return;

    std::vector<std::string> cmdHistory=mesh->GetCommandHistory();
    std::string flag="";
    double values[20]={0};
    std::string strValues[5]={""};
    bool option=false;
    std::string msg="";
    int regionRowIndex=-1;

    for(int i=0;i<cmdHistory.size();i++)
    {
        if(cmdHistory[i]=="")
            continue;

        if(!svMeshTetGen::ParseCommand(cmdHistory[i],flag,values,strValues,option,msg))
        {
            QMessageBox::warning(NULL,"Parsing Error","Error in parsing command history!");
            return;
        }

        if(flag=="GlobalEdgeSize")
        {
            ui->lineEditGlobalEdgeSizeT->setText(QString::number(values[0]));
        }
        else if(flag=="boundaryLayer")
        {
            ui->checkBoxBoundaryLayerT->setChecked(true);
            ui->sbLayersT->setValue(values[0]);
            ui->dsbPortionT->setValue(values[1]);
            ui->dsbRatioT->setValue(values[2]);
        }
        else if(flag=="useCenterlineRadius")
        {
            ui->checkBoxRadiusBasedT->setChecked(true);
        }
        else if(flag=="SurfaceMeshFlag")
        {
            if(values[0]==0.0)
                ui->checkBoxSurfaceT->setChecked(false);
            else
                ui->checkBoxSurfaceT->setChecked(true);
        }
        else if(flag=="VolumeMeshFlag")
        {
            if(values[0]==0.0)
                ui->checkBoxVolumeT->setChecked(false);
            else
                ui->checkBoxVolumeT->setChecked(true);
        }
        else if(flag=="Optimization")
        {
            ui->checkBoxFlagO->setChecked(true);
            ui->sliderFlagO->setValue(values[0]);
        }
        else if(flag=="QualityRatio")
        {
            ui->checkBoxFlagQ->setChecked(true);
            ui->sliderFlagQ->setValue(values[0]);
        }
        else if(flag=="Epsilon")
        {
            ui->checkBoxFlagT->setChecked(true);
            ui->lineEditFlagT->setText(QString::number(values[0]));
        }
        else if(flag=="NoBisect")
        {
            ui->checkBoxFlagY->setChecked(true);
        }
        else if(flag=="NoMerge")
        {
            ui->checkBoxFlagM->setChecked(true);
        }
        else if(flag=="Diagnose")
        {
            ui->checkBoxFlagD->setChecked(true);
        }
        else if(flag=="Check")
        {
            ui->checkBoxFlagC->setChecked(true);
        }
        else if(flag=="Quiet")
        {
            ui->checkBoxFlagQ2->setChecked(true);
        }
        else if(flag=="Verbose")
        {
            ui->checkBoxFlagV->setChecked(true);
        }
        else if(flag=="LocalEdgeSize")
        {
            int faceID=modelElement->GetFaceID(strValues[0]);

            for(int j=0;i<m_TableModelLocalT->rowCount(); j++)
            {
                QStandardItem* itemID= m_TableModelLocalT->item(j,0);
                int id=itemID->text().toInt();

                if(faceID==id)
                {
                    QStandardItem* item= m_TableModelLocalT->item(j,3);
                    item->setText(QString::number(values[0]));
                }
            }
        }
        else if(flag=="sphereRefinement")
        {
            regionRowIndex++;
            m_TableModelRegionT->insertRow(regionRowIndex);

            QStandardItem* item;

            item= new QStandardItem("Sphere");
            item->setEditable(false);
            m_TableModelRegionT->setItem(regionRowIndex, 0, item);

            item= new QStandardItem(QString::number(values[0]));
            m_TableModelRegionT->setItem(regionRowIndex, 1, item);

            item= new QStandardItem(QString::number(values[1])+" "+QString::number(values[2])+" "+QString::number(values[3])+" "+QString::number(values[4]));
            m_TableModelRegionT->setItem(regionRowIndex, 2, item);
        }
        else
        {
            //do nothing
        }

    }

}

void svMeshEdit::AddSphere()
{
    if(m_SphereWidget==NULL || !m_SphereWidget->GetEnabled() || m_SphereWidget->GetRadius()==0)
        return;

    int regionRowIndex=m_TableModelRegionT->rowCount();

    QStandardItem* item;

    item= new QStandardItem("Sphere");
    item->setEditable(false);
    m_TableModelRegionT->setItem(regionRowIndex, 0, item);

    item= new QStandardItem("");
    m_TableModelRegionT->setItem(regionRowIndex, 1, item);

    double center[3];
    m_SphereWidget->GetCenter(center);
    item= new QStandardItem(QString::number(m_SphereWidget->GetRadius())+" "+QString::number(center[0])+" "+QString::number(center[1])+" "+QString::number(center[2]));
    m_TableModelRegionT->setItem(regionRowIndex, 2, item);
}

void svMeshEdit::ShowSphereInteractor(bool checked)
{
    if(!checked)
    {
        if(m_SphereWidget!=NULL)
        {
            m_SphereWidget->Off();
        }

        return;
    }

    if(m_Model==NULL) return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=dynamic_cast<svModelElement*>(m_Model->GetModelElement(timeStep));

    if(modelElement==NULL) return;

    if(modelElement->GetWholeVtkPolyData()==NULL) return;
    if(m_SphereWidget==NULL)
    {
        m_SphereWidget = vtkSmartPointer<vtkSphereWidget>::New();
        m_SphereWidget->SetInteractor(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow()->GetInteractor());
    //    m_SphereWidget->SetRepresentationToSurface();
    }

    m_SphereWidget->SetInputData(modelElement->GetWholeVtkPolyData());
    m_SphereWidget->PlaceWidget();

    m_SphereWidget->On();
}

void svMeshEdit::UpdateSphereData( vtkObject* caller, unsigned long vtkNotUsed(eventId), void* vtkNotUsed(clientData), void* vtkNotUsed(callData) )
{
    if(m_SelectedRegionIndex>-1 && m_SphereWidget->GetRadius()>0)
    {
        QStandardItem* item= m_TableModelRegionT->item(m_SelectedRegionIndex,2);
        double center[3];
        m_SphereWidget->GetCenter(center);
        item->setText(QString::number(m_SphereWidget->GetRadius())+" "+QString::number(center[0])+" "+QString::number(center[1])+" "+QString::number(center[2]));
    }
}

void svMeshEdit::UpdateFaceListSelection()
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    QStandardItemModel* tableModel=NULL;
    QTableView* tableView=NULL;

    if(m_MeshType=="TetGen")
    {
        tableModel=m_TableModelLocalT;
        tableView=ui->tableViewLocalT;
    }
    else if(m_MeshType=="MeshSim")
    {
//        tableModel=m_TableModelLocalM;
//        tableView=ui->tableViewLocalM;
    }

    disconnect( tableView->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableFaceListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );


    tableView->clearSelection();

    int count=tableModel->rowCount();

    for(int i=0;i<count;i++)
    {
        QStandardItem* itemID= tableModel->item(i,0);
        int id=itemID->text().toInt();

        if(modelElement->IsFaceSelected(id))
        {
            QModelIndex mIndex=tableModel->index(i,1);
            tableView->selectionModel()->select(mIndex, QItemSelectionModel::Select|QItemSelectionModel::Rows);
        }
    }

    connect( tableView->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableFaceListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

}

void svMeshEdit::NodeChanged(const mitk::DataNode* node)
{
    if(m_MeshNode==node)
        ui->labelMeshName->setText(QString::fromStdString(m_MeshNode->GetName()));
}

void svMeshEdit::NodeAdded(const mitk::DataNode* node)
{
}

void svMeshEdit::NodeRemoved(const mitk::DataNode* node)
{
}

void svMeshEdit::ClearAll()
{
    if(m_Model && m_ModelSelectFaceObserverTag)
    {
        m_Model->RemoveObserver(m_ModelSelectFaceObserverTag);
    }

    if(m_SphereWidget && m_SphereObserverTag)
    {
        m_SphereWidget->RemoveObserver(m_SphereObserverTag);
    }

    if(m_ModelNode)
    {
        m_ModelNode->SetDataInteractor(NULL);
        m_DataInteractor=NULL;
    }

    m_Model=NULL;
    m_MeshNode=NULL;
    m_MitkMesh=NULL;
    m_ModelNode=NULL;

    ui->labelMeshName->setText("");
    ui->labelMeshType->setText("");

}


