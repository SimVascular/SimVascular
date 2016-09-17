#include "svModelCreate.h"
#include "ui_svModelCreate.h"

#include "svModel.h"
#include "svModelLegacyIO.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkImage.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

const QString svModelCreate::EXTENSION_ID = "sv.modelcreate";

svModelCreate::svModelCreate()
    : ui(new Ui::svModelCreate)
    , m_CreateModel(true)
    , m_ModelFolderNode(NULL)
{
}

svModelCreate::~svModelCreate()
{
    for (std::vector< std::pair< QmitkNodeDescriptor*, QAction* > >::iterator it = mDescriptorActionList.begin();it != mDescriptorActionList.end(); it++)
    {
        // first== the NodeDescriptor; second== the registered QAction
        (it->first)->RemoveAction(it->second);
    }

    delete ui;
}

void svModelCreate::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateModel()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    parent->move(400,400);

    QmitkNodeDescriptor* dataNodeDescriptor = getNodeDescriptorManager()->GetDescriptor("svModel");
    QAction* action = new QAction(QIcon(":ModelCreate.png"), "Create Model", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( ShowModelCreate() ) );
    dataNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(dataNodeDescriptor, action));

    QmitkNodeDescriptor* folderNodeDescriptor = getNodeDescriptorManager()->GetDescriptor("svModelFolder");
    action = new QAction(QIcon(":Modelloadlegacy.png"), "Load Legacy Models", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( LoadLegacyModels() ) );
    folderNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));

    action = new QAction(QIcon(":Modelsavelegacy.png"), "Save as Legacy Models", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( SaveLegacyModels() ) );
    folderNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));

    action = new QAction(QIcon(":ModelCreate.png"), "Create Model", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( ShowModelCreate() ) );
    folderNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));
}

void svModelCreate::SetFocus( )
{
    //ui->lineEditModelName->setFocus();
}

void svModelCreate::ShowModelCreate()
{
    useExtension(EXTENSION_ID);
}

//void svModelCreate::ResetLineEditNumber(int index)
//{
//    if(index==2)
//    {
//        ui->lineEditNumber->setEnabled(false);
//        ui->lineEditNumber->setText("");
//    }
//    else
//    {
//        ui->lineEditNumber->setEnabled(true);
//        if(index==0)
//            ui->lineEditNumber->setText("100");
//        else
//            ui->lineEditNumber->setText("10");
//    }
//}

//void svModelCreate::SetCreateModel(bool create)
//{
//    m_CreateModel=create;
//    if(create){
//        setWindowTitle("Create New Model");
//        ui->lineEditModelName->setEnabled(true);
//    }else{
//        ui->lineEditModelName->setEnabled(false);
//        setWindowTitle("Change Current Model");
//    }
//}

void svModelCreate::CreateModel()
{
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("svModelFolder");
    mitk::NodePredicateDataType::Pointer isModelNode = mitk::NodePredicateDataType::New("svModel");
    mitk::DataNode::Pointer modelNode=NULL;

    if(nodes.size()==0)
    {
        return;
    }

    mitk::DataNode::Pointer node=nodes.front();

    if(isModelFolder->CheckNode(node)){
        m_ModelFolderNode=node;
    }else if(isModelNode->CheckNode(node)){
        modelNode=node;
        mitk::DataStorage::SetOfObjects::ConstPointer rs = this->GetDataStorage()->GetSources(node);
        if(rs->size()>0){
            m_ModelFolderNode=rs->GetElement(0);
        }else{
            return;
        }
    }else{
        return;
    }

    std::string modelName=ui->lineEditModelName->text().trimmed().toStdString();

    if(m_CreateModel)
    {
        if(modelName==""){
            QMessageBox::warning(NULL,"Model Empty","Please give a model name!");
            return;
        }

        mitk::DataNode::Pointer exitingNode=this->GetDataStorage()->GetNamedDerivedNode(modelName.c_str(),m_ModelFolderNode);
        if(exitingNode){
            QMessageBox::warning(NULL,"Model Already Created","Please use a different model name!");
            return;
        }
    }

    if(m_CreateModel)
    {
        svModel::Pointer solidModel=svModel::New();

        mitk::DataNode::Pointer solidModelNode = mitk::DataNode::New();
        solidModelNode->SetData(solidModel);
        solidModelNode->SetName(modelName);

        this->GetDataStorage()->Add(solidModelNode,m_ModelFolderNode);
    }
    else if(!modelNode.IsNull())
    {

    }

    hide();
}

void svModelCreate::Cancel()
{
    hide();
}

void svModelCreate::LoadLegacyModels()
{
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("svModelFolder");

    if(nodes.size()>0 && isModelFolder->CheckNode(nodes.front()))
    {
        QString modelDir = QFileDialog::getExistingDirectory(this, tr("Choose Directory"),
                                                             QDir::homePath(),
                                                             QFileDialog::ShowDirsOnly
                                                             | QFileDialog::DontResolveSymlinks
                                                             | QFileDialog::DontUseNativeDialog
                                                             );

        if(modelDir.trimmed().isEmpty()) return;

        mitk::DataNode::Pointer modelFolderNode=nodes.front();

        std::vector<mitk::DataNode::Pointer> modelNodes=svModelLegacyIO::ReadFiles(modelDir);

        for(int i=0;i<modelNodes.size();i++)
            GetDataStorage()->Add(modelNodes[i],modelFolderNode);

    }
}

void svModelCreate::SaveLegacyModels()
{
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("svModelFolder");

    if(nodes.size()>0 && isModelFolder->CheckNode(nodes.front()))
    {
        QString modelDir = QFileDialog::getExistingDirectory(this, tr("Choose Directory"),
                                                             QDir::homePath(),
                                                             QFileDialog::ShowDirsOnly
                                                             | QFileDialog::DontResolveSymlinks
                                                             | QFileDialog::DontUseNativeDialog
                                                             );

        if(modelDir.trimmed().isEmpty()) return;

        mitk::DataNode::Pointer modelFolderNode=nodes.front();

        mitk::DataStorage::SetOfObjects::ConstPointer rsModel=GetDataStorage()->GetDerivations(modelFolderNode,mitk::NodePredicateDataType::New("svModel"));

        svModelLegacyIO::WriteFiles(rsModel, modelDir);

    }

}
