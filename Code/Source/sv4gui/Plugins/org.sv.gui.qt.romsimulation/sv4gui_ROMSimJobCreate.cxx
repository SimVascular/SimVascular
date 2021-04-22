/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "sv4gui_ROMSimJobCreate.h"
#include "ui_sv4gui_ROMSimJobCreate.h"

#include "sv4gui_MitkROMSimJob.h"
#include "sv4gui_Model.h"
#include "sv4gui_DataNodeOperation.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkOperationEvent.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

sv4guiROMSimJobCreate::sv4guiROMSimJobCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep)
    : ui(new Ui::sv4guiROMSimJobCreate)
    , m_DataStorage(dataStorage)
    , m_SelecteNode(selectedNode)
    , m_TimeStep(timeStep)
    , m_SimulationFolderNode(NULL)
    , m_ModelFolderNode(NULL)
{
    m_Interface=new sv4guiDataNodeOperationInterface;

    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateJob()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditJobName, SIGNAL(returnPressed()), this, SLOT(CreateJob()));
    move(400,400);

    Activated();
}

sv4guiROMSimJobCreate::~sv4guiROMSimJobCreate()
{
    delete ui;
}

void sv4guiROMSimJobCreate::Activated()
{
    ui->comboBox->clear();

    m_ModelFolderNode=NULL;
    m_SimulationFolderNode=NULL;

    if(m_SelecteNode.IsNull())
        return;

    mitk::DataNode::Pointer selectedNode=m_SelecteNode;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (selectedNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        mitk::NodePredicateDataType::Pointer isSimFolder = mitk::NodePredicateDataType::New("sv4guiROMSimulationFolder");
        mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("sv4guiMitkROMSimJob");

        if(isSimFolder->CheckNode(selectedNode)){
            m_SimulationFolderNode=selectedNode;
        }else if(isSimJob->CheckNode(selectedNode)){
            mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(selectedNode);
            if(rs->size()>0){
                m_SimulationFolderNode=rs->GetElement(0);
            }
        }

        rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiModelFolder"));
        if (rs->size()>0)
        {
            m_ModelFolderNode=rs->GetElement(0);

            rs=m_DataStorage->GetDerivations(m_ModelFolderNode,mitk::NodePredicateDataType::New("sv4guiModel"));

            for(int i=0;i<rs->size();i++)
            {
                ui->comboBox->addItem(QString::fromStdString(rs->GetElement(i)->GetName()));
            }
        }

    }

    ui->lineEditJobName->clear();
}

void sv4guiROMSimJobCreate::SetFocus( )
{
    ui->comboBox->setFocus();
}

void sv4guiROMSimJobCreate::CreateJob()
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

    sv4guiModel* model=dynamic_cast<sv4guiModel*>(selectedModelNode->GetData());
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

    if (!sv4guiDataNodeOperationInterface::IsValidDataNodeName(jobName)) {
        auto validName = QString::fromStdString(sv4guiDataNodeOperationInterface::ValidDataNodeNameMsg);
        auto jobName = ui->lineEditJobName->text().trimmed();
        QString msg = "The name '" + jobName + "' is not valid.\n" +
                      "Job names " + validName + ".\n";
        QMessageBox::warning(NULL, "Simulation 1D", msg);
        return;
    }


    mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(jobName.c_str(),m_SimulationFolderNode);
    if(exitingNode){
        QMessageBox::warning(NULL,"Job Already Created","Please use a different job name!");
        return;
    }

    sv4guiMitkROMSimJob::Pointer mitkJob = sv4guiMitkROMSimJob::New();
    mitkJob->SetModelName(selectedModelNode->GetName());
    mitkJob->SetModelOrder("1");
    mitkJob->SetDataModified();

    mitk::DataNode::Pointer jobNode = mitk::DataNode::New();
    jobNode->SetData(mitkJob);
    jobNode->SetName(jobName);

//    m_DataStorage->Add(jobNode,m_SimulationFolderNode);
    mitk::OperationEvent::IncCurrObjectEventId();

    bool undoEnabled=true;
    sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,m_DataStorage,jobNode,m_SimulationFolderNode);
    if(undoEnabled)
    {
        sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,jobNode,m_SimulationFolderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    m_Interface->ExecuteOperation(doOp);

    hide();
}

void sv4guiROMSimJobCreate::Cancel()
{
    hide();
}
