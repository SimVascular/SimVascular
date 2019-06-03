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

#include "sv4gui_ModelCreate.h"
#include "ui_sv4gui_ModelCreate.h"

#include "sv4gui_Model.h"
#include "sv4gui_DataNodeOperation.h"
#include "sv4gui_ModelElementFactory.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkImage.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

sv4guiModelCreate::sv4guiModelCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep)
    : ui(new Ui::sv4guiModelCreate)
    , m_DataStorage(dataStorage)
    , m_SelecteNode(selectedNode)
    , m_TimeStep(timeStep)
    , m_CreateModel(true)
    , m_ModelFolderNode(NULL)
{
    m_Interface=new sv4guiDataNodeOperationInterface;

    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateModel()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditModelName, SIGNAL(returnPressed()), this, SLOT(CreateModel()));

    ui->comboBoxType->clear();

    std::vector<std::string> types=sv4guiModelElementFactory::GetAvailableTypes();
    for(int i=0;i<types.size();i++)
        ui->comboBoxType->addItem(QString::fromStdString(types[i]));

    ui->comboBoxType->setCurrentText("PolyData");

    move(400,400);
}

sv4guiModelCreate::~sv4guiModelCreate()
{
    delete ui;
}

void sv4guiModelCreate::SetFocus( )
{
    ui->lineEditModelName->setFocus();
}

//-------------
// CreateModel
//-------------
//
void sv4guiModelCreate::CreateModel()
{
    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("sv4guiModelFolder");
    mitk::NodePredicateDataType::Pointer isModelNode = mitk::NodePredicateDataType::New("sv4guiModel");
    mitk::DataNode::Pointer modelNode=NULL;

    if(m_SelecteNode.IsNull())
        return;

    mitk::DataNode::Pointer node=m_SelecteNode;

    if(isModelFolder->CheckNode(node)){
        m_ModelFolderNode=node;
    }else if(isModelNode->CheckNode(node)){
        modelNode=node;
        mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(node);
        if(rs->size()>0){
            m_ModelFolderNode=rs->GetElement(0);
        }else{
            return;
        }
    }else{
        return;
    }

    std::string modelName=ui->lineEditModelName->text().trimmed().toStdString();


    if(modelName==""){
        QMessageBox::warning(NULL,"Model Empty","Please give a model name!");
        return;
    }

    if (!sv4guiDataNodeOperationInterface::IsValidDataNodeName(modelName)) {
        auto validName = QString::fromStdString(sv4guiDataNodeOperationInterface::ValidDataNodeNameMsg);
        auto modelName = ui->lineEditModelName->text().trimmed();
        QString msg = "The name '" + modelName + "' is not valid.\n" +
                      "Model names " + validName + ".\n";
        QMessageBox::warning(NULL, "Model", msg);
        return;
    }



    mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(modelName.c_str(),m_ModelFolderNode);
    if(exitingNode){
        QMessageBox::warning(NULL,"Model Already Created","Please use a different model name!");
        return;
    }

    QString currentType=ui->comboBoxType->currentText().trimmed();
    int currentIndex=ui->comboBoxType->currentIndex();

    sv4guiModel::Pointer solidModel=sv4guiModel::New();
    solidModel->SetDataModified();
//    int timeStep=m_TimeStep;

    if(currentIndex<0 || currentType=="")
        return;

    solidModel->SetType(currentType.toStdString());

    mitk::DataNode::Pointer solidModelNode = mitk::DataNode::New();
    solidModelNode->SetData(solidModel);
    solidModelNode->SetName(modelName);

//    m_DataStorage->Add(solidModelNode,m_ModelFolderNode);
    mitk::OperationEvent::IncCurrObjectEventId();

    bool undoEnabled=true;
    sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,m_DataStorage,solidModelNode,m_ModelFolderNode);
    if(undoEnabled)
    {
        sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,solidModelNode,m_ModelFolderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    m_Interface->ExecuteOperation(doOp);

    hide();
}

void sv4guiModelCreate::Cancel()
{
    hide();
}

