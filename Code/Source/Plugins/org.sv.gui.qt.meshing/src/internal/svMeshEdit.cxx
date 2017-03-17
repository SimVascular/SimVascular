#include "svMeshEdit.h"
#include "ui_svMeshEdit.h"

#include "svVtkMeshSphereWidget.h"

#include "svModel.h"
#include "svMeshTetGen.h"
#include "svMesh.h"
#include "svMitkMesh.h"
#include "svMitkMeshOperation.h"
#include "svMitkMeshIO.h"
#include "svProjectManager.h"

#include "svModelElementPolyData.h"

#ifdef SV_USE_OpenCASCADE_QT_GUI
#include "svModelElementOCCT.h"
#endif

#ifdef SV_USE_PARASOLID_QT_GUI
#include "svModelElementParasolid.h"
#endif

#include "svMeshTetGenAdaptor.h"
#include "svDataNodeOperation.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <usModuleRegistry.h>

#include <vtkProperty.h>
#include <vtkXMLUnstructuredGridReader.h>

#include <QTreeView>
#include <QInputDialog>
#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

const QString svMeshEdit::EXTENSION_ID = "org.sv.views.meshing";

svMeshEdit::svMeshEdit() :
    ui(new Ui::svMeshEdit)
{
    m_MitkMesh=NULL;
    m_Model=NULL;
    m_MeshNode=NULL;
    m_ModelNode=NULL;

    m_DataInteractor=NULL;
    m_ModelSelectFaceObserverTag=-1;

    m_TableModelLocalT=NULL;
    m_TableMenuLocalT=NULL;

    m_TableModelRegionT=NULL;
    m_TableMenuRegionT=NULL;

    m_SphereWidget=NULL;

    m_SelectedRegionIndex=-1;

    m_UndoAble=false;

    m_Interface=new svDataNodeOperationInterface;
}

svMeshEdit::~svMeshEdit()
{
    delete ui;

    if(m_TableModelLocalT)
        delete m_TableModelLocalT;

    if(m_TableMenuLocalT)
        delete m_TableMenuLocalT;

    if(m_TableModelRegionT)
        delete m_TableModelRegionT;

    if(m_TableMenuRegionT)
        delete m_TableMenuRegionT;
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
        MITK_ERROR << "Plugin MeshEdit Init Error: No QmitkStdMultiWidget Available!";
        return;
    }

    connect(ui->btnRunMesher, SIGNAL(clicked()), this, SLOT(RunMesher()) );

    SetupTetGenGUI(parent);

//    SetUpMeshSimGUI(parent);

    if(m_SphereWidget==NULL)
    {
        m_SphereWidget = vtkSmartPointer<svVtkMeshSphereWidget>::New();
        m_SphereWidget->SetInteractor(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow()->GetInteractor());
    //    m_SphereWidget->SetRepresentationToSurface();
        svVtkMeshSphereWidget* sphereWidget=dynamic_cast<svVtkMeshSphereWidget*>(m_SphereWidget.GetPointer());
        sphereWidget->SetMeshEdit(this);
    }

    connect(ui->btnMeshInfo, SIGNAL(clicked()), this, SLOT(DisplayMeshInfo()) );
}

void svMeshEdit::SetupTetGenGUI(QWidget *parent )
{
    connect(ui->btnEstimateT, SIGNAL(clicked()), this, SLOT(SetEstimatedEdgeSize()) );

    ui->toolBox->setCurrentIndex(0);

    //for local table
    m_TableModelLocalT = new QStandardItemModel(this);
    ui->tableViewLocalT->setModel(m_TableModelLocalT);

    connect( ui->tableViewLocalT->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableFaceListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    m_TableMenuLocalT=new QMenu(ui->tableViewLocalT);
    QAction* setLocalTAction=m_TableMenuLocalT->addAction("Set Edge Size");
    QAction* clearLocalTAction=m_TableMenuLocalT->addAction("Clear Edge Size");
    connect( setLocalTAction, SIGNAL( triggered(bool) ) , this, SLOT( SetLocal(bool) ) );
    connect( clearLocalTAction, SIGNAL( triggered(bool) ) , this, SLOT( ClearLocal(bool) ) );

    connect( ui->tableViewLocalT, SIGNAL(customContextMenuRequested(const QPoint&))
      , this, SLOT(TableViewLocalContextMenuRequested(const QPoint&)) );

    //for regional table
    connect(ui->checkBoxSphereT, SIGNAL(toggled(bool)), this, SLOT(ShowSphereInteractor(bool)));
    connect(ui->btnAddSphereT, SIGNAL(clicked()), this, SLOT(AddSphere()) );

    m_TableModelRegionT = new QStandardItemModel(this);
    ui->tableViewRegionT->setModel(m_TableModelRegionT);

    connect( ui->tableViewRegionT->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableRegionListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    m_TableMenuRegionT=new QMenu(ui->tableViewRegionT);
    QAction* setRegionTAction=m_TableMenuRegionT->addAction("Set Edge Size");
    QAction* deleteRegionTAction=m_TableMenuRegionT->addAction("Delete");
    connect( setRegionTAction, SIGNAL( triggered(bool) ) , this, SLOT( SetRegion(bool) ) );
    connect( deleteRegionTAction, SIGNAL( triggered(bool) ) , this, SLOT( DeleteSelectedRegions(bool) ) );

    connect( ui->tableViewRegionT, SIGNAL(customContextMenuRequested(const QPoint&))
      , this, SLOT(TableViewRegionContextMenuRequested(const QPoint&)) );

    //for adaptor
    connect(ui->comboBoxOption, SIGNAL(currentIndexChanged(int)), this, SLOT(UpdateAdaptGUI(int)));
    ui->comboBoxOption->setCurrentIndex(0);
    UpdateAdaptGUI(0);

    connect(ui->toolButtonResultFile, SIGNAL(clicked()), this, SLOT(SetResultFile()));

    connect(ui->btnAdapt, SIGNAL(clicked()), this, SLOT(Adapt()));
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
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
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

void svMeshEdit::SetRegion(bool)
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

    bool ok=false;
    QString localInfo="";

    if(m_MeshType=="TetGen")
    {
        double localSize=QInputDialog::getDouble(m_Parent, "Set Edge Size", "Edge Size:", 0.0, 0, 100, 4, &ok);
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

       QStandardItem* item= tableModel->item(row,1);
       item->setText(localInfo);
     }
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

//    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
//       ; it != indexesOfSelectedRows.end(); it++)
//     {
//       int row=(*it).row();
//       tableModel->removeRow(row);
//     }

    std::vector<int> rows;
    for(int i=0;i<indexesOfSelectedRows.size();i++)
        rows.push_back(indexesOfSelectedRows[i].row());

    std::sort(rows.begin(), rows.end(), std::greater<int>());

    for(int i=0;i<rows.size();i++)
        tableModel->removeRow(rows[i]);

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

void svMeshEdit::SetEstimatedEdgeSize()
{
    double edgeSize=EstimateEdgeSize();

    if(m_MeshType=="TetGen")
        ui->lineEditGlobalEdgeSizeT->setText(QString::number(edgeSize));
//    else if(m_MeshType="MeshSim")
//        ui->lineEditGlobalEdgeSizeM->setText(QString::number(edgeSize));
}

double svMeshEdit::EstimateEdgeSize()
{
    if(!m_MitkMesh) return 0;

    if(!m_Model) return 0;
    svModelElement* modelElement=dynamic_cast<svModelElement*>(m_Model->GetModelElement());
    if(!modelElement) return 0;

    double edgeSize= sqrt(modelElement->GetMinFaceArea()/3.1415)/2.5;
    edgeSize=round(10000*edgeSize)/10000;

    return edgeSize;
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
    int timeStep=GetTimeStep();
    svMesh* originalMesh=m_MitkMesh->GetMesh(timeStep);
    if(originalMesh&&originalMesh->GetSurfaceMesh()==NULL)
    {
        std::string path="";
        m_MeshNode->GetStringProperty("path",path);
        if(path!="")
        {
            std::string surfaceFileName = path+"/"+m_MeshNode->GetName()+".vtp";
            std::ifstream surfaceFile(surfaceFileName);
            if(surfaceFile)
            {
                if (QMessageBox::question(m_Parent, "Previous Mesh exists", "Previous mesh created already, but not loaded from file. Do you want to create new mesh?",
                                          QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
                {
                  return;
                }
            }
        }

    }

    if (QMessageBox::question(m_Parent, "Meshing", "The meshing may take a while. Do you want to continue?",
                              QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
    {
      return;
    }

    if(!m_MitkMesh) return;

    if(!m_Model) return;

    svModelElementPolyData* modelElement=NULL;
    modelElement=dynamic_cast<svModelElementPolyData*>(m_Model->GetModelElement());

#ifdef SV_USE_OpenCASCADE_QT_GUI
    if(modelElement==NULL)
    {
        svModelElementOCCT* me=dynamic_cast<svModelElementOCCT*>(m_Model->GetModelElement());
        if(me)
        {
            mitk::StatusBar::GetInstance()->DisplayText("converting OpenCASCADE to PolyData...");
            WaitCursorOn();
            modelElement=me->ConverToPolyDataModel();
            WaitCursorOff();
        }
    }
#endif

#ifdef SV_USE_PARASOLID_QT_GUI
    if(modelElement==NULL)
    {
        svModelElementParasolid* me=dynamic_cast<svModelElementParasolid*>(m_Model->GetModelElement());
        if(me)
        {
            mitk::StatusBar::GetInstance()->DisplayText("converting Parasolid to PolyData...");
            WaitCursorOn();
            modelElement=me->ConverToPolyDataModel();
            WaitCursorOff();
        }
    }
#endif

    if(!modelElement) return;

    svMesh* newMesh=NULL;

    if(m_MeshType=="TetGen")
    {
        QString ges=ui->lineEditGlobalEdgeSizeT->text().trimmed();
        bool ok=false;
        ges.toDouble(&ok);
        if(!ok)
        {
            QMessageBox::warning(m_Parent,"Warning","Error in Global Egde Size!");
            return;
        }

        newMesh=new svMeshTetGen();
    }
//    else if(m_MeshType=="MeshSim")
//        mesh=new svMeshMeshSim();

    if(newMesh==NULL)
        return;

    //add fake progress
    mitk::ProgressBar::GetInstance()->AddStepsToDo(3);

    mitk::StatusBar::GetInstance()->DisplayText("Creating mesh...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    newMesh->InitNewMesher();
    newMesh->SetModelElement(modelElement);

    std::vector<std::string> cmds;
    if(fromGUI)
    {
        if(m_MeshType=="TetGen")
            cmds=CreateCmdsT();
//        else if(m_MeshType=="MeshSim")
//            cmds=CreateCmdM();
    }
    else
    {
        cmds=originalMesh->GetCommandHistory();
    }

    std::string msg;
    if(!newMesh->ExecuteCommands(cmds, msg))
    {
        WaitCursorOff();
        mitk::ProgressBar::GetInstance()->Progress(2);
        QMessageBox::warning(m_Parent,"Error during executing",QString::fromStdString(msg));
        delete newMesh;
        return;
    }

    newMesh->SetCommandHistory(cmds);

    if(m_UndoAble)
    {
        mitk::OperationEvent::IncCurrObjectEventId();
    }

    svMitkMeshOperation* doOp = new svMitkMeshOperation(svMitkMeshOperation::OpSETMESH,timeStep,newMesh);

    if(m_UndoAble)
    {
        svMitkMeshOperation* undoOp = new svMitkMeshOperation(svMitkMeshOperation::OpSETMESH,timeStep,originalMesh);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_MitkMesh, doOp, undoOp, "Set Mesh");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }

    m_MitkMesh->ExecuteOperation(doOp);

    if(!m_UndoAble)
    {
        delete originalMesh;
    }

    WaitCursorOff();
    mitk::ProgressBar::GetInstance()->Progress(2);
    mitk::StatusBar::GetInstance()->DisplayText("Meshing done.");

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    DisplayMeshInfo();

}

std::vector<std::string> svMeshEdit::CreateCmdsT()
{
    std::vector<std::string> cmds;

    if(ui->checkBoxSurfaceT->isChecked())
        cmds.push_back("option surface 1");
    else
        cmds.push_back("option surface 0");

    if(ui->checkBoxVolumeT->isChecked())
        cmds.push_back("option volume 1");
    else
        cmds.push_back("option volume 0");

    if(ui->checkBoxRadiusBasedT->isChecked() || ui->checkBoxBoundaryLayerT->isChecked())
    {
        cmds.push_back("option UseMMG 0");
        ui->checkBoxFastMeshing->setChecked(false);
    }
    else
    {
        cmds.push_back("option UseMMG 1");
    }

    cmds.push_back("option GlobalEdgeSize "+ui->lineEditGlobalEdgeSizeT->text().trimmed().toStdString());

    if(!ui->checkBoxFastMeshing->isChecked())
        cmds.push_back("setWalls");

    if(ui->checkBoxRadiusBasedT->isChecked())
    {
        cmds.push_back("useCenterlineRadius");
        cmds.push_back("functionBasedMeshing "+ ui->lineEditGlobalEdgeSizeT->text().trimmed().toStdString() +" DistanceToCenterlines");
    }

    if(ui->checkBoxBoundaryLayerT->isChecked())
        cmds.push_back("boundaryLayer "+QString::number(ui->sbLayersT->value()).toStdString()
                       +" "+QString::number(ui->dsbPortionT->value()).toStdString()+" "+QString::number(ui->dsbRatioT->value()).toStdString());

    for(int i=0;i<m_TableModelLocalT->rowCount();i++)
    {
        QStandardItem* itemName= m_TableModelLocalT->item(i,1);
        QString name=itemName->text();

        QStandardItem* itemLocal= m_TableModelLocalT->item(i,3);
        QString localSize=itemLocal->text().trimmed();

        if(!localSize.isEmpty())
            cmds.push_back("localSize " + name.toStdString() + " " + localSize.toStdString());
    }

    for(int i=0;i<m_TableModelRegionT->rowCount();i++)
    {
        QStandardItem* itemShape= m_TableModelRegionT->item(i,0);
        QString shape=itemShape->text();

        QStandardItem* itemLocal= m_TableModelRegionT->item(i,1);
        QString localSize=itemLocal->text().trimmed();

        QStandardItem* itemParams= m_TableModelRegionT->item(i,2);
        QString params=itemParams->text();
        QStringList plist = params.split(QRegExp("\\s+"));

        if(!localSize.isEmpty())
            cmds.push_back("sphereRefinement " + localSize.toStdString() + " " + plist[0].toStdString()
                    + " " + plist[1].toStdString() + " " + plist[2].toStdString() + " " + plist[3].toStdString());
    }

    if(ui->checkBoxFlagO->isChecked())
        cmds.push_back("option Optimization "+QString::number(ui->sliderFlagO->value()).toStdString());

    if(ui->checkBoxFlagT->isChecked())
        cmds.push_back("option Epsilon "+ui->lineEditFlagT->text().toStdString());

    if(ui->checkBoxFlagQ->isChecked())
        cmds.push_back("option QualityRatio "+QString::number(ui->sliderFlagQ->value()).toStdString());

    if(ui->checkBoxFlagY->isChecked())
        cmds.push_back("option NoBisect");

    if(ui->checkBoxFlagM->isChecked())
        cmds.push_back("option NoMerge");

    if(ui->checkBoxFlagD->isChecked())
        cmds.push_back("option Diagnose");

    if(ui->checkBoxFlagC->isChecked())
        cmds.push_back("option Check");

    if(ui->checkBoxFlagQ2->isChecked())
        cmds.push_back("option Quiet");

    if(ui->checkBoxFlagV->isChecked())
        cmds.push_back("option Verbose");

    cmds.push_back("generateMesh");

    if(ui->checkBoxBoundaryLayerT->isChecked())
        cmds.push_back("getBoundaries");

    cmds.push_back("writeMesh");

    return cmds;
}

void svMeshEdit::Visible()
{
    OnSelectionChanged(GetDataManagerSelection());
}

void svMeshEdit::Hidden()
{
//    ClearAll();
    RemoveObservers();
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
        RemoveObservers();
        m_Parent->setEnabled(false);
        return;
    }

    mitk::DataNode::Pointer meshNode=nodes.front();
    svMitkMesh* mitkMesh=dynamic_cast<svMitkMesh*>(meshNode->GetData());

    if(!mitkMesh)
    {
        RemoveObservers();
        m_Parent->setEnabled(false);
        return;
    }

    if(m_MeshNode==meshNode)
    {
        AddObservers();
        m_Parent->setEnabled(true);
        return;
    }

    std::string modelName=mitkMesh->GetModelName();

    mitk::DataNode::Pointer modelNode=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (meshNode,isProjFolder,false);

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

    if(m_MeshNode.IsNotNull())
        RemoveObservers();

    m_ModelNode=modelNode;
    m_Model=model;
    m_MeshNode=meshNode;
    m_MitkMesh=mitkMesh;
    m_MeshType=m_MitkMesh->GetType();

    if(m_Model==NULL)
    {
        m_Parent->setEnabled(false);
        QMessageBox::warning(m_Parent,"No Model Found","No model found for this mesh!");
    }
    else
    {
        m_Parent->setEnabled(true);
        AddObservers();
    }

    UpdateGUI();

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svMeshEdit::UpdateGUI()
{
    //update top part
    //======================================================================
    ui->labelMeshName->setText(QString::fromStdString(m_MeshNode->GetName()));
    ui->labelMeshType->setText(QString::fromStdString(m_MeshType));
    if(m_ModelNode.IsNotNull())
        ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
    else
        ui->labelModelName->setText("No model found");

    if(!m_MitkMesh)
        return;

    if(!m_Model)
        return;

    if(m_MeshType=="TetGen")
    {
        ui->stackedWidget->setCurrentIndex(0);
        UpdateTetGenGUI();
    }
    else if(m_MeshType=="MeshSim")
    {
//        ui->stackedWidget->setCurrentIndex(1);
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

    ui->checkBoxSurfaceT->setChecked(true);
    ui->checkBoxVolumeT->setChecked(true);

    ui->checkBoxFastMeshing->setChecked(false);

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

    UpdateFaceListSelection();

    //regional refinement
    m_TableModelRegionT->clear();

    QStringList regionListHeaders;
    regionListHeaders << "Type" << "Local Size" << "Radius x y z";
    m_TableModelRegionT->setHorizontalHeaderLabels(regionListHeaders);
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

    //then udpate with command history
    //========================================
//    svMeshTetGen* mesh=dynamic_cast<svMeshTetGen*>(m_MitkMesh->GetMesh(GetTimeStep()));
    svMesh* mesh=m_MitkMesh->GetMesh(GetTimeStep());
    if(mesh==NULL)
        return;

    std::vector<std::string> cmdHistory=mesh->GetCommandHistory();
    std::string flag="";
    double values[20]={0};
    std::string strValues[5]={""};
    bool option=false;
    std::string msg="";
    int regionRowIndex=-1;

    if(cmdHistory.size()>0)
        ui->checkBoxFastMeshing->setChecked(true);

    for(int i=0;i<cmdHistory.size();i++)
    {
        if(cmdHistory[i]=="")
            continue;

        if(!svMeshTetGen::ParseCommand(cmdHistory[i],flag,values,strValues,option,msg))
        {
            QMessageBox::warning(m_Parent,"Parsing Error","Error in parsing command history!");
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
        else if(flag=="setWalls")
        {
            ui->checkBoxFastMeshing->setChecked(false);
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

            for(int j=0;j<m_TableModelLocalT->rowCount(); j++)
            {
                QStandardItem* itemID= m_TableModelLocalT->item(j,0);
                int id=itemID->text().toInt();

                if(faceID==id)
                {
                    QStandardItem* item= m_TableModelLocalT->item(j,3);
                    item->setText(QString::number(values[0]));
                    break;
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
            item->setEditable(false);
            m_TableModelRegionT->setItem(regionRowIndex, 2, item);
        }
        else
        {
            //do nothing
        }

    }

    //adaptor options
    ui->lineEditMaxEdgeSize->setText(ui->lineEditGlobalEdgeSizeT->text());

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

    m_SphereWidget->SetInputData(modelElement->GetWholeVtkPolyData());
    m_SphereWidget->PlaceWidget();

    m_SphereWidget->On();

    TableRegionListSelectionChanged(QItemSelection(),QItemSelection());
}

void svMeshEdit::UpdateSphereData()
{
    if(m_SelectedRegionIndex>-1 && m_SphereWidget->GetRadius()>0)
    {
        QStandardItem* item= m_TableModelRegionT->item(m_SelectedRegionIndex,2);
        if(item)
        {
            double center[3];
            m_SphereWidget->GetCenter(center);
            item->setText(QString::number(m_SphereWidget->GetRadius())+" "+QString::number(center[0])+" "+QString::number(center[1])+" "+QString::number(center[2]));
        }
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
    {
        ui->labelMeshName->setText(QString::fromStdString(m_MeshNode->GetName()));
    }
}

void svMeshEdit::NodeAdded(const mitk::DataNode* node)
{
}

void svMeshEdit::NodeRemoved(const mitk::DataNode* node)
{
}

void svMeshEdit::AddObservers()
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
        m_ModelNode->SetStringProperty("interactor user","meshing");
        svModelDataInteractor* interactor=dynamic_cast<svModelDataInteractor*>(m_ModelNode->GetDataInteractor().GetPointer());
        if(interactor)
            interactor->SetFaceSelectionOnly();
    }

    if(m_Model && m_ModelSelectFaceObserverTag==-1)
    {
        itk::SimpleMemberCommand<svMeshEdit>::Pointer modelSelectFaceCommand = itk::SimpleMemberCommand<svMeshEdit>::New();
        modelSelectFaceCommand->SetCallbackFunction(this, &svMeshEdit::UpdateFaceListSelection);
        m_ModelSelectFaceObserverTag = m_Model->AddObserver( svModelSelectFaceEvent(), modelSelectFaceCommand);
    }

    svVtkMeshSphereWidget* sphereWidget=dynamic_cast<svVtkMeshSphereWidget*>(m_SphereWidget.GetPointer());
    if(sphereWidget)
        sphereWidget->AddMyObserver();
}

void svMeshEdit::RemoveObservers()
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
        if(user=="meshing")
            m_ModelNode->SetDataInteractor(NULL);
    }
    m_DataInteractor=NULL;

    svVtkMeshSphereWidget* sphereWidget=dynamic_cast<svVtkMeshSphereWidget*>(m_SphereWidget.GetPointer());
    if(sphereWidget)
    {
        sphereWidget->RemoveMyObserver();
    }
}

void svMeshEdit::ClearAll()
{
    m_Model=NULL;
    m_MeshNode=NULL;
    m_MitkMesh=NULL;
    m_ModelNode=NULL;

    ui->labelMeshName->setText("");
    ui->labelMeshType->setText("");
    ui->labelModelName->setText("");
}

void svMeshEdit::DisplayMeshInfo()
{
    if(m_MeshNode.IsNull())
        return;

    svMitkMesh* mitkMesh=dynamic_cast<svMitkMesh*>(m_MeshNode->GetData());
    if(!mitkMesh)
        return;

    svMesh* mesh=mitkMesh->GetMesh();
    if(!mesh)
        return;

    std::string path="";
    m_MeshNode->GetStringProperty("path",path);
    std::string meshFileName = path+"/"+m_MeshNode->GetName()+".msh";

    vtkSmartPointer<vtkPolyData> surfaceMesh=mesh->GetSurfaceMesh();
    if(surfaceMesh==NULL && path!="")
    {
        surfaceMesh=svMitkMeshIO::GetSurfaceMesh(meshFileName);
    }

    vtkSmartPointer<vtkUnstructuredGrid> volumeMesh=mesh->GetVolumeMesh();
    if(volumeMesh==NULL && path!="")
    {
        volumeMesh=svMitkMeshIO::GetVolumeMesh(meshFileName);
    }

    int num_nodes = 0;
    int nMeshFaces = 0;
    int num_elems = 0;
    int nMeshEdges = 0;

    if(surfaceMesh)
    {
      nMeshFaces = surfaceMesh->GetNumberOfCells();
      nMeshEdges=nMeshFaces*3/2;
    }

    if(volumeMesh)
    {
      num_nodes = volumeMesh->GetNumberOfPoints();
      num_elems = volumeMesh->GetNumberOfCells();
    }

    QString stat="Number of Nodes: " + QString::number(num_nodes)
            + "\n" + "Number of Elems: " + QString::number(num_elems)
            + "\n" + "Number of Edges: " + QString::number(nMeshEdges)
            + "\n" + "Number of Faces: " + QString::number(nMeshFaces);


    QMessageBox::information(m_Parent,"Mesh Statistics","Mesh done. Statistics:           \n\n"+stat);
}

void svMeshEdit::UpdateAdaptGUI(int selected)
{
    switch(selected)
    {
    case 0:
        ui->widgetStartStep->hide();
        ui->widgetStepIncrement->hide();
        ui->labelEndStep->setText("Step Number:");
        break;
    case 1:
        ui->widgetStartStep->show();
        ui->widgetStepIncrement->show();
        ui->labelEndStep->setText("End Step Number:");
        break;
    }
}

void svMeshEdit::SetResultFile()
{
    QString lastFileOpenPath="";

    QString previousFilePath=ui->lineEditResultFile->text().trimmed();
    if(QFile(previousFilePath).exists())
        lastFileOpenPath=previousFilePath;

    if(lastFileOpenPath=="")
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

        if(prefs.IsNotNull())
        {
            lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
        }
    }

    QString resultVtuFile = QFileDialog::getOpenFileName(m_Parent, tr("Select Result VTU File")
                                                            , lastFileOpenPath
                                                            , tr("All Files (*.*)")
                                                            , NULL
                                                            , QFileDialog::DontUseNativeDialog);

    if (resultVtuFile.isEmpty())
        return;

    ui->lineEditResultFile->setText(resultVtuFile);
}

void svMeshEdit::Adapt()
{
    if(m_MeshNode.IsNull())
        return;

    if(m_ModelNode.IsNull())
        return;

    if(m_Model==NULL)
        return;

    svModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL)
        return;

    bool ok;
    QString adaptedMeshName = QInputDialog::getText(m_Parent, tr("Name for Adapted Mesh"),
                                                    tr("Mesh Name:"), QLineEdit::Normal,
                                                    QString("adapted-")+QString::fromStdString(m_MeshNode->GetName()), &ok);
    if (!ok || adaptedMeshName.isEmpty())
        return;

    mitk::DataNode::Pointer meshFolderNode=NULL;
    std::string meshFolderPath="";
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources(m_MeshNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);
        std::string projPath="";
        projFolderNode->GetStringProperty("project path", projPath);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svMeshFolder"));
        if (rs->size()>0)
        {
            meshFolderNode=rs->GetElement(0);
            meshFolderPath=projPath+"/"+meshFolderNode->GetName();
        }
    }
    if(meshFolderPath=="")
        return;

    int currentIndex=ui->comboBoxOption->currentIndex();

    //check if user inputs are valid
    QString resultFile=ui->lineEditResultFile->text().trimmed();
    if(!QFile(resultFile).exists())
    {
        QMessageBox::warning(m_Parent,"File not existing","Make sure the result vtu file exists!");
        return;
    }

    QString startStep="0";
    QString stepInc="1";
    if(currentIndex==1)
    {
        startStep =ui->lineEditStartStep->text().trimmed();
        if(!IsInt(startStep))
        {
            QMessageBox::warning(m_Parent,"Format error","Please provide start step in a correct format!");
            return;
        }
        stepInc =ui->lineEditStepIncrement->text().trimmed();
        if(!IsInt(stepInc))
        {
            QMessageBox::warning(m_Parent,"Format error","Please provide step increment in a correct format!");
            return;
        }
    }

    QString endStep=ui->lineEditEndStep->text().trimmed();
    if(!IsInt(endStep))
    {
        QMessageBox::warning(m_Parent,"Format error","Please provide step number in a correct format!");
        return;
    }

    QString errorFactor=ui->lineEditErrorFactor->text().trimmed();
    if(!IsDouble(errorFactor))
    {
        QMessageBox::warning(m_Parent,"Format error","Please provide erro factor in a correct format!");
        return;
    }

    QString minSize=ui->lineEditMinEdgeSize->text().trimmed();
    if(!IsDouble(minSize))
    {
        QMessageBox::warning(m_Parent,"Format error","Please provide min edge size in a correct format!");
        return;
    }

    QString maxSize=ui->lineEditMaxEdgeSize->text().trimmed();
    if(!IsDouble(maxSize))
    {
        QMessageBox::warning(m_Parent,"Format error","Please provide max edge size in a correct format!");
        return;
    }

    svMeshTetGenAdaptor* adaptor=new svMeshTetGenAdaptor();

    if(!adaptor->SetModelElement(modelElement))
    {
        QMessageBox::warning(m_Parent,"Error","Failed in loading model.");
        delete adaptor;
        return;
    }

    if(!adaptor->LoadMeshFromResultVTUFile(resultFile.toStdString()))
    {
        QMessageBox::warning(m_Parent,"Error","Failed in loading result mesh.");
        delete adaptor;
        return;
    }

    adaptor->SetAdaptOptions("strategy",1);
    adaptor->SetAdaptOptions("metric_option",currentIndex+2);
    adaptor->SetAdaptOptions("outstep",endStep.toInt());
    adaptor->SetAdaptOptions("ratio",errorFactor.toDouble());
    adaptor->SetAdaptOptions("hmin",minSize.toDouble());
    adaptor->SetAdaptOptions("hmax",maxSize.toDouble());
    if(currentIndex==1)
    {
        adaptor->SetAdaptOptions("instep",startStep.toInt());
        adaptor->SetAdaptOptions("step_incr",stepInc.toInt());
    }

    if(!adaptor->Adapt())
    {
        QMessageBox::warning(m_Parent,"Error","Failed in adapting the mesh.");
        delete adaptor;
        return;
    }

    QString solutionFilePath=QString::fromStdString(meshFolderPath)+"/adapted-restart."+endStep+".1";
    solutionFilePath=QDir::toNativeSeparators(solutionFilePath);

    if(!adaptor->WriteAdaptedSolution(solutionFilePath.toStdString()))
    {
        QMessageBox::warning(m_Parent,"Error","Failed in writing adapted solution (restart).");
        delete adaptor;
        return;
    }

    svMeshTetGen* adaptedMesh=adaptor->GetAdaptedMesh();
    delete adaptor;
    if(adaptedMesh==NULL)
    {
        QMessageBox::warning(m_Parent,"Error","Failed in getting adapted mesh.");
        return;
    }

    std::vector<std::string> cmds;
    cmds.push_back("option GlobalEdgeSize "+maxSize.toStdString());
    adaptedMesh->SetCommandHistory(cmds);

    svMitkMesh::Pointer mitkMesh = svMitkMesh::New();
    mitkMesh->SetModelName(m_ModelNode->GetName());
    mitkMesh->SetType(adaptedMesh->GetType());
    mitkMesh->SetMesh(adaptedMesh);
    mitkMesh->SetDataModified();

    mitk::DataNode::Pointer meshNode = mitk::DataNode::New();
    meshNode->SetData(mitkMesh);
    meshNode->SetName(adaptedMeshName.toStdString());

    bool undoEnabled=true;
    if(undoEnabled)
        mitk::OperationEvent::IncCurrObjectEventId();

    svDataNodeOperation* doOp = new svDataNodeOperation(svDataNodeOperation::OpADDDATANODE,GetDataStorage(),meshNode,meshFolderNode);
    if(undoEnabled)
    {
        svDataNodeOperation* undoOp = new svDataNodeOperation(svDataNodeOperation::OpREMOVEDATANODE,GetDataStorage(),meshNode,meshFolderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    m_Interface->ExecuteOperation(doOp);
}

bool svMeshEdit::IsInt(QString value)
{
    bool ok;
    value.toInt(&ok);
    return ok;
}

bool svMeshEdit::IsDouble(QString value)
{
    bool ok;
    value.toDouble(&ok);
    return ok;
}
