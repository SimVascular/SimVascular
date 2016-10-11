#include "svModelEdit.h"
#include "ui_svModelEdit.h"
#include "ui_svSegSelectionWidget.h"

#include "svModel.h"
#include "svModelUtils.h"
#include "svModelElementPolyData.h"

#include "cv_polydatasolid_utils.h"

#include <mitkNodePredicateDataType.h>
#include <mitkSurface.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>

#include <usModuleRegistry.h>

#include <QTreeView>
#include <QStandardItemModel>

#include <iostream>
using namespace std;

const QString svModelEdit::EXTENSION_ID = "org.sv.views.modeledit";

svModelEdit::svModelEdit() :
    ui(new Ui::svModelEdit)
{
    m_Model=NULL;
    m_ModelNode=NULL;

    m_SegSelectionWidget=NULL;

//    m_RemovingNode=false;

    m_ModelSelectFaceObserverTag=0;
    m_ModelUpdateObserverTag=0;
}

svModelEdit::~svModelEdit()
{
    delete ui;

    if(m_SegSelectionWidget) delete m_SegSelectionWidget;
}

void svModelEdit::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);

//    parent->setMaximumWidth(450);

    m_DisplayWidget=GetActiveStdMultiWidget();

    if(m_DisplayWidget==NULL)
    {
        parent->setEnabled(false);
        MITK_ERROR << "Plugin ModelEdit Init Error: No QmitkStdMultiWidget!";
        return;
    }

    //for top part
    connect(ui->btnUpdateModel, SIGNAL(clicked()), this, SLOT(ShowSegSelectionWidget()) );

    m_SegSelectionWidget=new svSegSelectionWidget();
    m_SegSelectionWidget->move(400,400);
    m_SegSelectionWidget->hide();
    m_SegSelectionWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_SegSelectionWidget->ui->buttonBox,SIGNAL(accepted()), this, SLOT(CreateModel()));
    connect(m_SegSelectionWidget->ui->buttonBox,SIGNAL(rejected()), this, SLOT(HideSegSelectionWidget()));

    //for tab Face List
    connect(ui->listWidget,SIGNAL(clicked(const QModelIndex&)), this, SLOT(SelectItem(const QModelIndex&)) );

   //for tab Blend
    connect(ui->btnBlend, SIGNAL(clicked()), this, SLOT(BlendModel()) );
    connect(ui->tabWidget,SIGNAL(currentChanged(int)), this, SLOT(UpdateBlendFaceList(int)) );
}

void svModelEdit::Visible()
{
    ui->tabWidget->setCurrentIndex(0);
    OnSelectionChanged(GetDataManagerSelection());
}

void svModelEdit::Hidden()
{
    ClearAll();
}

int svModelEdit::GetTimeStep()
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

void svModelEdit::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
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

    mitk::DataNode::Pointer modelNode=nodes.front();

//    if(m_ModelNode==modelNode)
//    {
////        return;
//    }

    ClearAll();

    m_ModelNode=modelNode;
    m_Model=dynamic_cast<svModel*>(modelNode->GetData());
    if(!m_Model)
    {
        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    m_Parent->setEnabled(true);
    ui->tabWidget->setCurrentIndex(0);

    m_ModelType=m_Model->GetType();

    UpdateGUI();

    m_DataInteractor = svModelDataInteractor::New();
    m_DataInteractor->LoadStateMachine("svModelInteraction.xml", us::ModuleRegistry::GetModule("svModel"));
    m_DataInteractor->SetEventConfig("svModelConfig.xml", us::ModuleRegistry::GetModule("svModel"));
    m_DataInteractor->SetDataNode(m_ModelNode);

    //Add Observers
    itk::SimpleMemberCommand<svModelEdit>::Pointer modelSelectFaceCommand = itk::SimpleMemberCommand<svModelEdit>::New();
    modelSelectFaceCommand->SetCallbackFunction(this, &svModelEdit::UpdateFaceListSelection);
    m_ModelSelectFaceObserverTag = m_Model->AddObserver( svModelSelectFaceEvent(), modelSelectFaceCommand);

    itk::SimpleMemberCommand<svModelEdit>::Pointer modelUpdateCommand = itk::SimpleMemberCommand<svModelEdit>::New();
    modelUpdateCommand->SetCallbackFunction(this, &svModelEdit::UpdateGUI);
    m_ModelUpdateObserverTag = m_Model->AddObserver( svModelSetEvent(), modelUpdateCommand);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svModelEdit::UpdateGUI()
{
    //update top part
    //------------------------------------------------------------------------
    ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
    ui->labelModelType->setText(QString::fromStdString(m_ModelType));

    if(m_ModelType=="Parasolid" || m_ModelType=="OpenCASCADE")
        ui->btnConvert->show();
    else
        ui->btnConvert->hide();

    //update tab face list
    //--------------------------------------------------------------------
    ui->listWidget->clear();
    if(!m_Model) return;

    svModelElement* modelElement=m_Model->GetModelElement();

    if(modelElement)
    {
        std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();

        for(int i=0;i<faces.size();i++)
        {
            svModelElement::svFace* face=faces[i];
            if(face)
            {
                QString item=QString::fromStdString(face->name);
                ui->listWidget->addItem(item);
            }
        }
    }

    UpdateFaceListSelection();

    if(m_ModelType=="PolyData")
        ui->toolBoxPolyData->show();
    else
        ui->toolBoxPolyData->hide();

    if(m_ModelType=="OpenCASCADE")
        ui->widgetOCC->show();
    else
        ui->widgetOCC->hide();


    //update tab Blend
    //------------------------------------------------------
    if(m_ModelType=="Discrete")
        ui->tabWidget->setTabEnabled(1,false);
    else
        ui->tabWidget->setTabEnabled(1,true);

    if(m_ModelType=="PolyData")
    {
        ui->widgetBlendDecimation->show();
        ui->groupBoxBlendIters->show();
    }else{
        ui->widgetBlendDecimation->hide();
        ui->groupBoxBlendIters->hide();
    }
}

void svModelEdit::UpdateFaceListSelection()
{
    if(!m_Model) return;
    svModelElement* modelElement=m_Model->GetModelElement();
    if(!modelElement) return;

    ui->listWidget->selectionModel()->clearSelection();

    int count=ui->listWidget->count();
    for(int i=0;i<count;i++)
    {
        std::string name=ui->listWidget->item(i)->text().toStdString();

        if(modelElement->IsFaceSelected(name))
        {
            QModelIndex mIndex=ui->listWidget->model()->index(i,0);
            //                    ui->listWidget->selectionModel()->select(mIndex, QItemSelectionModel::ClearAndSelect);
            ui->listWidget->selectionModel()->select(mIndex, QItemSelectionModel::Select);
        }
    }

}

void svModelEdit::SelectItem(const QModelIndex & idx)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    int index=idx.row();
    QListWidgetItem* item=ui->listWidget->item(index);
    if(!item) return;

    std::string selectedName=item->text().toStdString();

    modelElement->ClearFaceSelection();
    modelElement->SetSelectedFace(selectedName);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svModelEdit::UpdateBlendFaceList(int index)
{
    if(index!=1)
        return;

    ui->plainTextEditBlend->clear();
    //updata face list for blending
}

void svModelEdit::NodeChanged(const mitk::DataNode* node)
{
    if(m_ModelNode==node)
        ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
}

void svModelEdit::NodeAdded(const mitk::DataNode* node)
{
}

void svModelEdit::NodeRemoved(const mitk::DataNode* node)
{
//    OnSelectionChanged(GetDataManagerSelection());
}

void svModelEdit::ClearAll()
{
    if(m_Model && m_ModelSelectFaceObserverTag)
    {
        m_Model->RemoveObserver(m_ModelSelectFaceObserverTag);
    }

    if(m_Model && m_ModelUpdateObserverTag)
    {
        m_Model->RemoveObserver(m_ModelUpdateObserverTag);
    }

    if(m_ModelNode)
    {
        m_ModelNode->SetDataInteractor(NULL);
        m_DataInteractor=NULL;
    }

    m_Model=NULL;
    m_ModelNode=NULL;

    ui->labelModelName->setText("");
    ui->labelModelType->setText("");
    ui->listWidget->clear();
//    ui->plainTextEditBlend->clear();
}

void svModelEdit::ShowSegSelectionWidget()
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
//    if(modelElement==NULL) return;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(isProjFolder);

    if(rs->size()<1) return;

    mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

    rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svSegmentationFolder"));
    if(rs->size()<1) return;

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);
    rs=GetDataStorage()->GetDerivations(segFolderNode);
    if(rs->size()<1) return;

    std::vector<mitk::DataNode::Pointer> segNodes;
    for(int i=0;i<rs->size();i++)
        segNodes.push_back(rs->GetElement(i));

    int segNum=segNodes.size();

    QStandardItemModel *itemModel;
    itemModel = new QStandardItemModel(segNum,2,this);

    for(int row = 0; row < segNum; row++)
    {
        for(int col = 0; col < 2; col++)
        {
//            QModelIndex index
//                    = itemModel->index(row,col,QModelIndex());
            //            if(col==0)
            //            {
            //                model->setData(index,QString::fromStdString(segNodes[row]->GetName()));
            //            }
            //            else if(col==1)
            //            {
            //                if(modelElement&&modelElement->HasSeg(segNodes[row]->GetName()))
            //                    model->setData(index,true);
            //                else
            //                    model->setData(index,false);
            //            }
            if(col==0)
            {
                QStandardItem* item= new QStandardItem(QString::fromStdString(segNodes[row]->GetName()));
                item->setEditable(false);
                itemModel->setItem(row,col,item);
            }
            else if(col==1)
            {
                if(modelElement&&modelElement->HasSeg(segNodes[row]->GetName()))
                {
                    QStandardItem* item= new QStandardItem(true);
                    item->setCheckable(true);
                    item->setCheckState(Qt::Checked);
                    itemModel->setItem(row,col,item);
                }
                else
                {
                    QStandardItem* item= new QStandardItem(false);
                    item->setCheckable(true);
                    item->setCheckState(Qt::Unchecked);
                    itemModel->setItem(row,col,item);
                }

            }
        }
    }

    QStringList headers;
    headers << "Segmentation" << "Use";
    itemModel->setHorizontalHeaderLabels(headers);

    m_SegSelectionWidget->ui->tableView->setModel(itemModel);
    //    m_SegSelectionWidget->ui->tableView->setColumnWidth(0,150);
    m_SegSelectionWidget->ui->tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
    m_SegSelectionWidget->show();
}

void svModelEdit::HideSegSelectionWidget()
{
    m_SegSelectionWidget->hide();
}

void svModelEdit::CreateModel()
{
    std::vector<std::string> segNames;
    int rowCount=m_SegSelectionWidget->ui->tableView->model()->rowCount(QModelIndex());
    for(int i=0;i<rowCount;i++)
    {
        QModelIndex index= m_SegSelectionWidget->ui->tableView->model()->index(i,1, QModelIndex());
        if(index.data(Qt::CheckStateRole) == Qt::Checked){
            QModelIndex idx= m_SegSelectionWidget->ui->tableView->model()->index(i,0, QModelIndex());
            segNames.push_back(idx.data().toString().toStdString());
        }
    }

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(isProjFolder);

    if(rs->size()<1) return;

    mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

    rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svSegmentationFolder"));
    if(rs->size()<1) return;

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);
    //    rs=GetDataStorage()->GetDerivations(segFolderNode);
    //    if(rs->size()<1) return;

    std::vector<mitk::DataNode::Pointer> segNodes;
    //    for(int i=0;i<rs->size();i++)
    //        segNodes.push_back(rs->GetElement(i));

    for(int i=0;i<segNames.size();i++)
    {
        mitk::DataNode::Pointer node=GetDataStorage()->GetNamedDerivedNode(segNames[i].c_str(),segFolderNode);
        if(node.IsNotNull())
            segNodes.push_back(node);
    }

    svModelElement* newModelElement=NULL;
    svModelElement* modelElement=m_Model->GetModelElement();

    if(m_ModelType=="PolyData"){
        newModelElement=svModelUtils::CreateModelElementPolyData(segNodes);
    }
    else if(m_ModelType=="Parasolid")
    {

    }

    int timeStep=GetTimeStep();

    mitk::OperationEvent::IncCurrObjectEventId();
    svModelOperation* doOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
    svModelOperation* undoOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

    HideSegSelectionWidget();

//    OnSelectionChanged(GetDataManagerSelection());
    UpdateGUI();
}

std::vector<svModelElement::svBlendParamRadius*> svModelEdit::GetBlendRadii()
{
    std::vector<svModelElement::svBlendParamRadius*> blendRadii;

    QString content=ui->plainTextEditBlend->toPlainText();
    QStringList list = content.split("\n");

    for(int i=0;i<list.size();i++)
    {
        QStringList list2 = list[i].split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
        if(list2.size()!=3) continue;

        int faceID1=list2[0].trimmed().toInt();
        int faceID2=list2[1].trimmed().toInt();
        double radius=list2[2].trimmed().toDouble();

        cout<<faceID1<<"...."<<faceID2<<"....."<<radius<<endl;

        blendRadii.push_back(new svModelElement::svBlendParamRadius(faceID1,faceID2,radius));

    }

    return blendRadii;
}

void svModelEdit::BlendModel()
{
    if(m_Model==NULL) return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);

    if(modelElement==NULL) return;

    svModelElement* newModelElement=NULL;

    std::vector<svModelElement::svBlendParamRadius*> blendRadii=GetBlendRadii();

    if(m_ModelType=="PolyData"){

        svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(modelElement);
        if(!mepd) return;

        svModelElementPolyData::svBlendParam* param=new svModelElementPolyData::svBlendParam();

        param->numblenditers=ui->sbBlendIters->value();
        param->numsubblenditers=ui->sbSubBlendIters->value();
        param->numcgsmoothiters=ui->sbCstrSmoothIters->value();
        param->numlapsmoothiters=ui->sbLapSmoothIters->value();
        param->numsubdivisioniters=ui->sbSubdivisionIters->value();
        param->targetdecimation=ui->dsbDecimation->value();

        newModelElement=svModelUtils::CreateModelElementPolyDataByBlend(mepd, blendRadii, param);
    }
    else if(m_ModelType=="Parasolid")
    {

    }

    if(newModelElement==NULL) return;

    mitk::OperationEvent::IncCurrObjectEventId();

    svModelOperation* doOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
    svModelOperation* undoOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

