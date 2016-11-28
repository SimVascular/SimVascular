#include "svPathCreate.h"
#include "ui_svPathCreate.h"

#include "svPath.h"
#include "svPathOperation.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkImage.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

svPathCreate::svPathCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep)
    : ui(new Ui::svPathCreate)
    , m_DataStorage(dataStorage)
    , m_SelecteNode(selectedNode)
    , m_TimeStep(timeStep)
    , m_CreatePath(true)
    , m_PathFolderNode(NULL)
{
//    m_Parent=parent;
    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreatePath()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditPathName, SIGNAL(returnPressed()), this, SLOT(CreatePath()));
    connect(ui->comboBoxSubdivisionType, SIGNAL(currentIndexChanged(int)), this, SLOT(ResetLineEditNumber(int )));
    connect(ui->lineEditNumber, SIGNAL(returnPressed()), this, SLOT(CreatePath()));
    move(400,400);
}

svPathCreate::~svPathCreate()
{
    delete ui;
}

void svPathCreate::SetFocus( )
{
    ui->lineEditPathName->setFocus();
}

void svPathCreate::ResetLineEditNumber(int index)
{
    if(index==2)
    {
        ui->lineEditNumber->setEnabled(false);
        ui->lineEditNumber->setText("");
    }
    else
    {
        ui->lineEditNumber->setEnabled(true);
        if(index==0)
            ui->lineEditNumber->setText("100");
        else
            ui->lineEditNumber->setText("10");
    }
}

void svPathCreate::SetCreatePath(bool create)
{
    m_CreatePath=create;
    if(create){
        setWindowTitle("Create New Path");
        ui->lineEditPathName->setEnabled(true);
    }else{
        ui->lineEditPathName->setEnabled(false);
        setWindowTitle("Change Current Path");
    }
}

void svPathCreate::CreatePath()
{
    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");
    mitk::NodePredicateDataType::Pointer isPathNode = mitk::NodePredicateDataType::New("svPath");
    mitk::DataNode::Pointer pathNode=NULL;

    if(m_SelecteNode.IsNull())
    {
        return;
    }

    mitk::DataNode::Pointer node=m_SelecteNode;

    if(isPathFolder->CheckNode(node)){
        m_PathFolderNode=node;
    }
    else if(isPathNode->CheckNode(node)){
        pathNode=node;
        mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(node);
        if(rs->size()>0){
            m_PathFolderNode=rs->GetElement(0);
        }else{
            return;
        }
    }
    else{
        return;
    }

    std::string pathName=ui->lineEditPathName->text().trimmed().toStdString();

    if(m_CreatePath)
    {
        if(pathName==""){
            QMessageBox::warning(NULL,"Path Empty","Please give a path name!");
            return;
        }

        mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(pathName.c_str(),m_PathFolderNode);
        if(exitingNode){
            QMessageBox::warning(NULL,"Path Already Created","Please use a different path name!");
            return;
        }
    }

    int currentIndex=ui->comboBoxSubdivisionType->currentIndex();

    int subdivisionNum=ui->lineEditNumber->text().trimmed().toInt();
    if(currentIndex==0&&subdivisionNum<2){
        QMessageBox::warning(NULL,"Total Point Number Too Small","Please give a number >= 2 at least!");
        return;
    }
    if(currentIndex==1&&subdivisionNum<1){
        QMessageBox::warning(NULL,"Subdivision Number Too Small","Please give a number >= 1 at least!");
        return;
    }

    int maxPathID=svPath::GetMaxPathID(m_DataStorage->GetDerivations(m_PathFolderNode));

    if(m_CreatePath)
    {
        svPath::Pointer path = svPath::New();
        path->SetPathID(maxPathID+1);
        int timeStep=m_TimeStep;

        switch(currentIndex)
        {
        case 0:
            path->SetMethod(svPathElement::CONSTANT_TOTAL_NUMBER);
            path->SetCalculationNumber(subdivisionNum);
            path->SetSpacing(0);
            break;
        case 1:
            path->SetMethod(svPathElement::CONSTANT_SUBDIVISION_NUMBER);
            path->SetCalculationNumber(subdivisionNum);
            path->SetSpacing(0);
            break;
        case 2:
            path->SetMethod(svPathElement::CONSTANT_SPACING);
            path->SetSpacing(GetVolumeImageSpacing());
            path->SetCalculationNumber(0);
            break;
        default:
            return;
        }

        svPathElement* pathElement=new svPathElement();
        pathElement->SetMethod(path->GetMethod());
        pathElement->SetCalculationNumber(path->GetCalculationNumber());
        pathElement->SetSpacing(path->GetSpacing());

        path->SetPathElement(pathElement,timeStep);
        path->SetDataModified();

        mitk::DataNode::Pointer pathNode = mitk::DataNode::New();
        pathNode->SetData(path);
        pathNode->SetName(pathName);

        m_DataStorage->Add(pathNode,m_PathFolderNode);
    }
    else if(!pathNode.IsNull())
    {
        svPath* path=dynamic_cast<svPath*>(pathNode->GetData());
        int timeStep=m_TimeStep;
        svPathElement* pathElement=path->GetPathElement(timeStep);
        svPathElement* changedPathElement=pathElement->Clone();

        switch(currentIndex)

        {
        case 0:
            changedPathElement->SetMethod(svPathElement::CONSTANT_TOTAL_NUMBER);
            changedPathElement->SetCalculationNumber(subdivisionNum);
            changedPathElement->SetSpacing(0);
            break;
        case 1:
            changedPathElement->SetMethod(svPathElement::CONSTANT_SUBDIVISION_NUMBER);
            changedPathElement->SetCalculationNumber(subdivisionNum);
            changedPathElement->SetSpacing(0);
            break;
        case 2:
            changedPathElement->SetMethod(svPathElement::CONSTANT_SPACING);
            changedPathElement->SetSpacing(GetVolumeImageSpacing());
            changedPathElement->SetCalculationNumber(0);
            break;
        default:
            return;
        }
        changedPathElement->CreatePathPoints();//update

        svPathOperation* doOp = new svPathOperation(svPathOperation::OpSETPATHELEMENT,timeStep,changedPathElement);
        svPathOperation* undoOp = new svPathOperation(svPathOperation::OpSETPATHELEMENT,timeStep,pathElement);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(path, doOp, undoOp, "Set PathElement");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        path->ExecuteOperation(doOp);
    }

    hide();
    SetCreatePath(true);
}

double svPathCreate::GetVolumeImageSpacing()
{
    double minSpacing=0.1;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (m_PathFolderNode,isProjFolder,false);
    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=m_DataStorage->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svImageFolder"));
        if(rs->size()>0)
        {
            mitk::DataNode::Pointer imageFolderNode=rs->GetElement(0);
            rs=m_DataStorage->GetDerivations(imageFolderNode);
            if(rs->size()>0)
            {
                mitk::Image* image=dynamic_cast<mitk::Image*>(rs->GetElement(0)->GetData());
                if(image)
                {
                    mitk::Vector3D spacing=image->GetGeometry()->GetSpacing();
                    minSpacing=std::min(spacing[0],std::min(spacing[1],spacing[2]));
                }

            }
        }
    }

    return minSpacing;
}

void svPathCreate::Cancel()
{
    hide();
    SetCreatePath(true);
}

void svPathCreate::SetPathName(QString pathName)
{
    ui->lineEditPathName->setText(pathName);
}

void svPathCreate::SetSubdivisionType(int index)
{
    ui->comboBoxSubdivisionType->setCurrentIndex(index);
}

void svPathCreate::SetNumber(int number)
{
     ui->lineEditNumber->setText(QString::number(number));
}
