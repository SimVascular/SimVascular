#include "svContourGroupCreate.h"
#include "ui_svContourGroupCreate.h"

#include "svContourGroup.h"
#include "svPath.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

svContourGroupCreate::svContourGroupCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep)
    : ui(new Ui::svContourGroupCreate)
    , m_DataStorage(dataStorage)
    , m_SelecteNode(selectedNode)
    , m_TimeStep(timeStep)
    , m_SegFolderNode(NULL)
    , m_PathFolderNode(NULL)
{
    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateGroup()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditGroupName, SIGNAL(returnPressed()), this, SLOT(CreateGroup()));
    move(400,400);

    Activated();
}

svContourGroupCreate::~svContourGroupCreate()
{
    delete ui;
}

void svContourGroupCreate::Activated()
{
    ui->comboBox->clear();

    m_PathFolderNode=NULL;
    m_SegFolderNode=NULL;

    if(m_SelecteNode.IsNull())
        return;

    mitk::DataNode::Pointer selectedNode=m_SelecteNode;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (selectedNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");
        mitk::NodePredicateDataType::Pointer isGroupNode = mitk::NodePredicateDataType::New("svContourGroup");

        if(isSegFolder->CheckNode(selectedNode)){
            m_SegFolderNode=selectedNode;
        }else if(isGroupNode->CheckNode(selectedNode)){
            mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(selectedNode);
            if(rs->size()>0){
                m_SegFolderNode=rs->GetElement(0);
            }
        }

        rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svPathFolder"));
        if (rs->size()>0)
        {
            m_PathFolderNode=rs->GetElement(0);

            rs=m_DataStorage->GetDerivations(m_PathFolderNode,mitk::NodePredicateDataType::New("svPath"));

            for(int i=0;i<rs->size();i++)
            {
                ui->comboBox->addItem(QString::fromStdString(rs->GetElement(i)->GetName()));
            }
        }

    }

    ui->lineEditGroupName->clear();
}

void svContourGroupCreate::SetFocus( )
{
    ui->comboBox->setFocus();
}

void svContourGroupCreate::CreateGroup()
{
    QString selectedPathName=ui->comboBox->currentText();
    if(selectedPathName=="")
    {
        QMessageBox::warning(NULL,"No Path Selected","Please select a path!");
        return;
    }

    mitk::DataNode::Pointer selectedPathNode=m_DataStorage->GetNamedDerivedNode(selectedPathName.toStdString().c_str(),m_PathFolderNode);

    if(selectedPathNode.IsNull())
    {
        QMessageBox::warning(NULL,"No Path Found!","Please select a existing path!");
        return;
    }

    std::string groupName=ui->lineEditGroupName->text().trimmed().toStdString();

    if(groupName==""){
        groupName=selectedPathNode->GetName();
    }

    mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(groupName.c_str(),m_SegFolderNode);
    if(exitingNode){
        QMessageBox::warning(NULL,"Contour Group Already Created","Please use a different group name!");
        return;
    }

    svContourGroup::Pointer group = svContourGroup::New();
    group->SetPathName(selectedPathNode->GetName());
    group->SetDataModified();

    svPath* selectedPath=dynamic_cast<svPath*>(selectedPathNode->GetData());
    if(selectedPath)
    {
        group->SetPathID(selectedPath->GetPathID());
    }

    mitk::DataNode::Pointer groupNode = mitk::DataNode::New();
    groupNode->SetData(group);
    groupNode->SetName(groupName);

    m_DataStorage->Add(groupNode,m_SegFolderNode);

    hide();
}

void svContourGroupCreate::Cancel()
{
    hide();
}
