#include "svSimJobCreate.h"
#include "ui_svSimJobCreate.h"

#include "svMitkSimJob.h"
#include "svModel.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

svSimJobCreate::svSimJobCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep)
    : ui(new Ui::svSimJobCreate)
    , m_DataStorage(dataStorage)
    , m_SelecteNode(selectedNode)
    , m_TimeStep(timeStep)
    , m_SimulationFolderNode(NULL)
    , m_ModelFolderNode(NULL)
{
    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateJob()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditJobName, SIGNAL(returnPressed()), this, SLOT(CreateJob()));
    move(400,400);

    Activated();
}

svSimJobCreate::~svSimJobCreate()
{
    delete ui;
}

void svSimJobCreate::Activated()
{
    ui->comboBox->clear();

    m_ModelFolderNode=NULL;
    m_SimulationFolderNode=NULL;

    if(m_SelecteNode.IsNull())
        return;

    mitk::DataNode::Pointer selectedNode=m_SelecteNode;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (selectedNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        mitk::NodePredicateDataType::Pointer isSimFolder = mitk::NodePredicateDataType::New("svSimulationFolder");
        mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("svMitkSimJob");

        if(isSimFolder->CheckNode(selectedNode)){
            m_SimulationFolderNode=selectedNode;
        }else if(isSimJob->CheckNode(selectedNode)){
            mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(selectedNode);
            if(rs->size()>0){
                m_SimulationFolderNode=rs->GetElement(0);
            }
        }

        rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svModelFolder"));
        if (rs->size()>0)
        {
            m_ModelFolderNode=rs->GetElement(0);

            rs=m_DataStorage->GetDerivations(m_ModelFolderNode,mitk::NodePredicateDataType::New("svModel"));

            for(int i=0;i<rs->size();i++)
            {
                ui->comboBox->addItem(QString::fromStdString(rs->GetElement(i)->GetName()));
            }
        }

    }

    ui->lineEditJobName->clear();
}

void svSimJobCreate::SetFocus( )
{
    ui->comboBox->setFocus();
}

void svSimJobCreate::CreateJob()
{
    QString selectedModelName=ui->comboBox->currentText();
    if(selectedModelName=="")
    {
        QMessageBox::warning(NULL,"No Model Selected","Please select a model!");
        return;
    }

    mitk::DataNode::Pointer selectedModelNode=m_DataStorage->GetNamedDerivedNode(selectedModelName.toStdString().c_str(),m_ModelFolderNode);

    if(selectedModelNode.IsNull())
    {
        QMessageBox::warning(NULL,"No Model Found!","Please select a existing model!");
        return;
    }

    svModel* model=dynamic_cast<svModel*>(selectedModelNode->GetData());
    if(model==NULL || model->GetModelElement()==NULL)
    {
        QMessageBox::warning(NULL,"Model is invalid or empty!","Please make sure the model has valid data!");
        return;
    }

    std::string jobName=ui->lineEditJobName->text().trimmed().toStdString();

    if(jobName==""){
        QMessageBox::warning(NULL,"No name for job!","Please give a name for the job!");
        return;
    }

    mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(jobName.c_str(),m_SimulationFolderNode);
    if(exitingNode){
        QMessageBox::warning(NULL,"Job Already Created","Please use a different job name!");
        return;
    }

    svMitkSimJob::Pointer mitkJob = svMitkSimJob::New();
    mitkJob->SetModelName(selectedModelNode->GetName());
    mitkJob->SetDataModified();

    mitk::DataNode::Pointer jobNode = mitk::DataNode::New();
    jobNode->SetData(mitkJob);
    jobNode->SetName(jobName);

    m_DataStorage->Add(jobNode,m_SimulationFolderNode);

    hide();
}

void svSimJobCreate::Cancel()
{
    hide();
}
