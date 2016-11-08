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
    m_MeshUpdateObserverTag=0;

    m_TableMenuLocalT=NULL;
    m_TableModelLocalT=NULL;

    m_SphereTableMenu=NULL;
    m_SphereTableModel=NULL;

    m_SphereWidget=NULL;
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
    QAction* setLocalSizeAction=m_TableMenuLocalT->addAction("Set Local Size");
    QAction* clearLocalSizeAction=m_TableMenuLocalT->addAction("Clear Local Size");



    connect(ui->btnRunHistoryT, SIGNAL(clicked()), this, SLOT(RunHistory()) );
}

void svMeshEdit::TableFaceListSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    if(m_TableModelLocalT==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewLocalT->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= m_TableModelLocalT->item(row,0);
        int id=itemID->text().toInt();

        modelElement->SelectFace(id);
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
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

//    m_DataInteractor = svMeshDataInteractor::New();
//    m_DataInteractor->LoadStateMachine("svMeshInteraction.xml", us::ModuleRegistry::GetModule("svMesh"));
//    m_DataInteractor->SetEventConfig("svMeshConfig.xml", us::ModuleRegistry::GetModule("svMesh"));
//    m_DataInteractor->SetDataNode(m_ModelNode);

//    //Add Observers
//    itk::SimpleMemberCommand<svMeshEdit>::Pointer modelSelectFaceCommand = itk::SimpleMemberCommand<svMeshEdit>::New();
//    modelSelectFaceCommand->SetCallbackFunction(this, &svMeshEdit::UpdateFaceListSelection);
//    m_ModelSelectFaceObserverTag = m_Model->AddObserver( svMeshSelectFaceEvent(), modelSelectFaceCommand);

//    itk::SimpleMemberCommand<svMeshEdit>::Pointer modelUpdateCommand = itk::SimpleMemberCommand<svMeshEdit>::New();
//    modelUpdateCommand->SetCallbackFunction(this, &svMeshEdit::UpdateGUI);
//    m_ModelUpdateObserverTag = m_Model->AddObserver( svMeshSetEvent(), modelUpdateCommand);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svMeshEdit::UpdateGUI()
{
    //update top part
    //------------------------------------------------------------------------
    ui->labelMeshName->setText(QString::fromStdString(m_MeshNode->GetName()));
    ui->labelMeshType->setText(QString::fromStdString(m_MeshType));

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
//    if(m_Model && m_ModelSelectFaceObserverTag)
//    {
//        m_Model->RemoveObserver(m_ModelSelectFaceObserverTag);
//    }

//    if(m_Model && m_ModelUpdateObserverTag)
//    {
//        m_Model->RemoveObserver(m_ModelUpdateObserverTag);
//    }

//    if(m_ModelNode)
//    {
//        m_ModelNode->SetDataInteractor(NULL);
//        m_DataInteractor=NULL;
//    }

    m_Model=NULL;
    m_MeshNode=NULL;
    m_MitkMesh=NULL;

    ui->labelMeshName->setText("");
    ui->labelMeshType->setText("");

}


