#include "svPathCreate.h"
#include "ui_svPathCreate.h"

#include "svPath.h"
#include "svPathLegacyIO.h"
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

const QString svPathCreate::EXTENSION_ID = "sv.pathcreate";

svPathCreate::svPathCreate()
    : ui(new Ui::svPathCreate)
    , m_CreatePath(true)
    , m_PathFolderNode(NULL)
{
}

svPathCreate::~svPathCreate()
{
    for (std::vector< std::pair< QmitkNodeDescriptor*, QAction* > >::iterator it = mDescriptorActionList.begin();it != mDescriptorActionList.end(); it++)
    {
        // first== the NodeDescriptor; second== the registered QAction
        (it->first)->RemoveAction(it->second);
    }

    delete ui;
}

void svPathCreate::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreatePath()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditPathName, SIGNAL(returnPressed()), this, SLOT(CreatePath()));
    connect(ui->comboBoxSubdivisionType, SIGNAL(currentIndexChanged(int)), this, SLOT(ResetLineEditNumber(int )));
    connect(ui->lineEditNumber, SIGNAL(returnPressed()), this, SLOT(CreatePath()));
    parent->move(400,400);

    QmitkNodeDescriptor* dataNodeDescriptor = getNodeDescriptorManager()->GetDescriptor("svPath");
    QAction* action = new QAction(QIcon(":pathcreate.png"), "Create Path", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( ShowPathCreate() ) );
    dataNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(dataNodeDescriptor, action));

    //    action = new QAction(QIcon(":pathcreate.png"), "Create Path", this);
    //    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( ShowPathCreate() ) );
    //    dataNodeDescriptor->AddAction(action,false);
    //    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(dataNodeDescriptor, action));

    QmitkNodeDescriptor* folderNodeDescriptor = getNodeDescriptorManager()->GetDescriptor("svPathFolder");
    action = new QAction(QIcon(":pathloadlegacy.png"), "Load Legacy Paths", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( LoadLegacyPaths() ) );
    folderNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));

    action = new QAction(QIcon(":pathsavelegacy.png"), "Save as Legacy Paths", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( SaveLegacyPaths() ) );
    folderNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));

    action = new QAction(QIcon(":pathcreate.png"), "Create Path", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( ShowPathCreate() ) );
    folderNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));
}

void svPathCreate::SetFocus( )
{
    ui->lineEditPathName->setFocus();
}

void svPathCreate::ShowPathCreate()
{
    useExtension(EXTENSION_ID);
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
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");
    mitk::NodePredicateDataType::Pointer isPathNode = mitk::NodePredicateDataType::New("svPath");
    mitk::DataNode::Pointer pathNode=NULL;

    if(nodes.size()==0)
    {
        return;
    }

    mitk::DataNode::Pointer node=nodes.front();

    if(isPathFolder->CheckNode(node)){
        m_PathFolderNode=node;
    }else if(isPathNode->CheckNode(node)){
        pathNode=node;
        mitk::DataStorage::SetOfObjects::ConstPointer rs = this->GetDataStorage()->GetSources(node);
        if(rs->size()>0){
            m_PathFolderNode=rs->GetElement(0);
        }else{
            return;
        }
    }else{
        return;
    }

    std::string pathName=ui->lineEditPathName->text().trimmed().toStdString();

    if(m_CreatePath)
    {

        if(pathName==""){
            QMessageBox::warning(NULL,"Path Empty","Please give a path name!");
            return;
        }

        mitk::DataNode::Pointer exitingNode=this->GetDataStorage()->GetNamedDerivedNode(pathName.c_str(),m_PathFolderNode);
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

    int maxPathID=svPath::GetMaxPathID(this->GetDataStorage()->GetDerivations(m_PathFolderNode));

    if(m_CreatePath)
    {
        svPath::Pointer path = svPath::New();
        path->SetPathID(maxPathID+10);
        int timeStep=GetTimeStep(path);

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
        mitk::DataNode::Pointer pathNode = mitk::DataNode::New();
        pathNode->SetData(path);
        pathNode->SetName(pathName);

        this->GetDataStorage()->Add(pathNode,m_PathFolderNode);
    }
    else if(!pathNode.IsNull())
    {
        svPath* path=dynamic_cast<svPath*>(pathNode->GetData());
        int timeStep=GetTimeStep(path);
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

    mitk::DataNode::Pointer imageNode=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_PathFolderNode,isProjFolder,false);
    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svImageFolder"));
        if(rs->size()>0)
        {
            mitk::DataNode::Pointer imageFolderNode=rs->GetElement(0);
            rs=GetDataStorage()->GetDerivations(imageFolderNode);
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

void svPathCreate::LoadLegacyPaths()
{
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");

    if(nodes.size()>0 && isPathFolder->CheckNode(nodes.front()))
    {
        mitk::DataNode::Pointer folderNode=nodes.front();
        QString fileName = QFileDialog::getOpenFileName(this, tr("Open File"),
                                                        QDir::homePath(),
                                                        tr("SV Legacy Path (*.paths)"),NULL,QFileDialog::DontUseNativeDialog);

        if(!fileName.isEmpty()){

            std::vector<mitk::DataNode::Pointer> newNodes=svPathLegacyIO::ReadFile(fileName);
            for(int i=0;i<newNodes.size();i++)
            {
                this->GetDataStorage()->Add(newNodes[i],folderNode);
            }

        }
    }

}

void svPathCreate::SaveLegacyPaths()
{
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");

    if(nodes.size()>0 && isPathFolder->CheckNode(nodes.front()))
    {
        mitk::DataNode::Pointer folderNode=nodes.front();

        QString fileName = QFileDialog::getSaveFileName(this,tr("Save File"),
                                                        QDir::homePath(),
                                                        tr("SV Legacy Path (*.paths)"),NULL,QFileDialog::DontUseNativeDialog);

        if(!fileName.isEmpty()){
            if ( fileName.right(6) != ".paths" ) fileName += ".paths";

            svPathLegacyIO::WriteFile(GetDataStorage()->GetDerivations (folderNode),fileName);
        }
    }
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
