#include "svContourGroupCreate.h"
#include "ui_svContourGroupCreate.h"

#include "svContourGroup.h"
#include "svPath.h"
#include "svSegmentationLegacyIO.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

const QString svContourGroupCreate::EXTENSION_ID = "sv.contourgroupcreate";

svContourGroupCreate::svContourGroupCreate() :
    ui(new Ui::svContourGroupCreate),
    m_SegFolderNode(NULL),
    m_PathFolderNode(NULL)
{
}

svContourGroupCreate::~svContourGroupCreate()
{
    for (std::vector< std::pair< QmitkNodeDescriptor*, QAction* > >::iterator it = mDescriptorActionList.begin();it != mDescriptorActionList.end(); it++)
    {
        // first== the NodeDescriptor; second== the registered QAction
        (it->first)->RemoveAction(it->second);
    }

    delete ui;
}

void svContourGroupCreate::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateGroup()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditGroupName, SIGNAL(returnPressed()), this, SLOT(CreateGroup()));
    parent->move(400,400);

    QmitkNodeDescriptor* dataNodeDescriptor = getNodeDescriptorManager()->GetDescriptor("svContourGroup");
    QAction* action = new QAction(QIcon(":contourgroupcreate.png"), "Create Contour Group", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( ShowGroupCreate() ) );
    dataNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(dataNodeDescriptor, action));

    QmitkNodeDescriptor* folderNodeDescriptor = getNodeDescriptorManager()->GetDescriptor("svSegmentationFolder");
    action = new QAction(QIcon(":contourgrouploadlegacy.png"), "Load Legacy Segmentations", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( LoadLegacySegmentations() ) );
    folderNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));

    action = new QAction(QIcon(":contourgroupsavelegacy.png"), "Save as Legacy Segmentations", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( SaveLegacySegmentations() ) );
    folderNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));

    action = new QAction(QIcon(":contourgroupcreate.png"), "Create Contour Group", this);
    QObject::connect( action, SIGNAL( triggered() ) , this, SLOT( ShowGroupCreate() ) );
    folderNodeDescriptor->AddAction(action,false);
    mDescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));
}

void svContourGroupCreate::Activated( )
{
    ui->comboBox->clear();

    m_PathFolderNode=NULL;
    m_SegFolderNode=NULL;

    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();

    if(nodes.size()>0)
    {
        mitk::DataNode::Pointer selectedNode=nodes.front();

        mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
        mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (selectedNode,isProjFolder,false);

        if(rs->size()>0)
        {
            mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

            mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");
            mitk::NodePredicateDataType::Pointer isGroupNode = mitk::NodePredicateDataType::New("svContourGroup");

            if(isSegFolder->CheckNode(selectedNode)){
                m_SegFolderNode=selectedNode;
            }else if(isGroupNode->CheckNode(selectedNode)){
                mitk::DataStorage::SetOfObjects::ConstPointer rs = GetDataStorage()->GetSources(selectedNode);
                if(rs->size()>0){
                    m_SegFolderNode=rs->GetElement(0);
                }
            }

            rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svPathFolder"));
            if (rs->size()>0)
            {
                m_PathFolderNode=rs->GetElement(0);

                rs=GetDataStorage()->GetDerivations(m_PathFolderNode,mitk::NodePredicateDataType::New("svPath"));

                for(int i=0;i<rs->size();i++)
                {
                    ui->comboBox->addItem(QString::fromStdString(rs->GetElement(i)->GetName()));
                }
            }

        }

    }

    ui->lineEditGroupName->clear();
}

void svContourGroupCreate::SetFocus( )
{
    ui->comboBox->setFocus();
}

void svContourGroupCreate::ShowGroupCreate()
{
    useExtension(EXTENSION_ID);
}

void svContourGroupCreate::CreateGroup()
{

    QString selectedPathName=ui->comboBox->currentText();
    if(selectedPathName=="")
    {
        QMessageBox::warning(NULL,"No Path Selected","Please select a path!");
        return;
    }

    mitk::DataNode::Pointer selectedPathNode=GetDataStorage()->GetNamedDerivedNode(selectedPathName.toStdString().c_str(),m_PathFolderNode);

    if(selectedPathNode.IsNull())
    {
        QMessageBox::warning(NULL,"No Path Found!","Please select a existing path!");
        return;
    }

    std::string groupName=ui->lineEditGroupName->text().trimmed().toStdString();

    if(groupName==""){
        groupName=selectedPathNode->GetName();
    }

    mitk::DataNode::Pointer exitingNode=this->GetDataStorage()->GetNamedDerivedNode(groupName.c_str(),m_SegFolderNode);
    if(exitingNode){
        QMessageBox::warning(NULL,"Contour Group Already Created","Please use a different group name!");
        return;
    }

    svContourGroup::Pointer group = svContourGroup::New();
    group->SetPathName(selectedPathNode->GetName());

    svPath* selectedPath=dynamic_cast<svPath*>(selectedPathNode->GetData());
    if(selectedPath)
    {
        group->SetPathID(selectedPath->GetPathID());
    }

    mitk::DataNode::Pointer groupNode = mitk::DataNode::New();
    groupNode->SetData(group);
    groupNode->SetName(groupName);

    this->GetDataStorage()->Add(groupNode,m_SegFolderNode);

    hide();
}

void svContourGroupCreate::Cancel()
{
    hide();
}

void svContourGroupCreate::LoadLegacySegmentations()
{
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");

    if(nodes.size()>0 && isSegFolder->CheckNode(nodes.front()))
    {
        QString segDir = QFileDialog::getExistingDirectory(this, tr("Choose Directory"),
                                                             QDir::homePath(),
                                                             QFileDialog::ShowDirsOnly
                                                             | QFileDialog::DontResolveSymlinks
                                                             | QFileDialog::DontUseNativeDialog
                                                             );

        if(segDir.trimmed().isEmpty()) return;

        mitk::DataNode::Pointer segFolderNode=nodes.front();

        std::vector<mitk::DataNode::Pointer> segNodes=svSegmentationLegacyIO::ReadFiles(segDir);

        for(int i=0;i<segNodes.size();i++)
            GetDataStorage()->Add(segNodes[i],segFolderNode);

    }
}

void svContourGroupCreate::SaveLegacySegmentations()
{
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");

    if(nodes.size()>0 && isSegFolder->CheckNode(nodes.front()))
    {
        QString segDir = QFileDialog::getExistingDirectory(this, tr("Choose Directory"),
                                                             QDir::homePath(),
                                                             QFileDialog::ShowDirsOnly
                                                             | QFileDialog::DontResolveSymlinks
                                                             | QFileDialog::DontUseNativeDialog
                                                             );

        if(segDir.trimmed().isEmpty()) return;

        mitk::DataNode::Pointer segFolderNode=nodes.front();

        mitk::DataStorage::SetOfObjects::ConstPointer rsContourGroup=GetDataStorage()->GetDerivations(segFolderNode,mitk::NodePredicateDataType::New("svContourGroup"));

        mitk::DataStorage::SetOfObjects::ConstPointer rsSeg3D=GetDataStorage()->GetDerivations(segFolderNode,mitk::NodePredicateDataType::New("svSeg3D"));

        svSegmentationLegacyIO::WriteFiles(rsContourGroup, rsSeg3D, segDir);

    }

}

