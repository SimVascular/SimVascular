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

#include "svMeshCreate.h"
#include "ui_svMeshCreate.h"

#include "simvascular_options.h"

#include "svMitkMesh.h"
#include "svModel.h"
#include "svDataNodeOperation.h"
#include "svMeshFactory.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkOperationEvent.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

svMeshCreate::svMeshCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep)
    : ui(new Ui::svMeshCreate)
    , m_DataStorage(dataStorage)
    , m_SelecteNode(selectedNode)
    , m_TimeStep(timeStep)
    , m_MeshFolderNode(NULL)
    , m_ModelFolderNode(NULL)
{
    m_Interface=new svDataNodeOperationInterface;

    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateMesh()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditMeshName, SIGNAL(returnPressed()), this, SLOT(CreateMesh()));
    move(400,400);

    Activated();
}

svMeshCreate::~svMeshCreate()
{
    delete ui;
}

void svMeshCreate::Activated()
{
    ui->comboBox->clear();

    m_ModelFolderNode=NULL;
    m_MeshFolderNode=NULL;

    if(m_SelecteNode.IsNull())
        return;

    mitk::DataNode::Pointer selectedNode=m_SelecteNode;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (selectedNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        mitk::NodePredicateDataType::Pointer isMeshFolder = mitk::NodePredicateDataType::New("svMeshFolder");
        mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("svMitkMesh");

        if(isMeshFolder->CheckNode(selectedNode)){
            m_MeshFolderNode=selectedNode;
        }else if(isMesh->CheckNode(selectedNode)){
            mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(selectedNode);
            if(rs->size()>0){
                m_MeshFolderNode=rs->GetElement(0);
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

    ui->lineEditMeshName->clear();

    connect(ui->comboBox, SIGNAL(currentIndexChanged(int)), this, SLOT(SetupMeshType(int )));
    ui->comboBox->setCurrentIndex(-1);
    if(ui->comboBox->count()>0)
        ui->comboBox->setCurrentIndex(0);

}

void svMeshCreate::SetupMeshType(int idx)
{
    ui->comboBoxMeshType->clear();

    if(ui->comboBox->currentIndex()!=-1)
    {
        std::string modelName=ui->comboBox->currentText().toStdString();
        mitk::DataNode::Pointer modelNode=m_DataStorage->GetNamedDerivedNode(modelName.c_str(),m_ModelFolderNode);
        if(modelNode.IsNull())
        {
            ui->comboBoxMeshType->clear();
            return;
        }

        svModel* model=dynamic_cast<svModel*>(modelNode->GetData());
        if(model==NULL)
        {
            ui->comboBoxMeshType->clear();
            return;
        }

        std::string modelType=model->GetType();

        std::vector<std::string> types=svMeshFactory::GetAvailableTypes();
        for(int i=0;i<types.size();i++)
            ui->comboBoxMeshType->addItem(QString::fromStdString(types[i]));

            if(modelType=="Parasolid")
                ui->comboBoxMeshType->setCurrentText("MeshSim");
            else
                ui->comboBoxMeshType->setCurrentText("TetGen");

    }
}

void svMeshCreate::SetFocus( )
{
    ui->comboBox->setFocus();
}

void svMeshCreate::CreateMesh()
{
    QString selectedModelName=ui->comboBox->currentText();
    if(selectedModelName=="")
    {
        QMessageBox::warning(NULL,"No Model Selected","Please select a model!");
        return;
    }

    if(ui->comboBoxMeshType->currentIndex()==-1)
    {
        QMessageBox::warning(NULL,"No Type Selected","Please select a mesh type!");
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

    std::string modelType=model->GetType();
    std::string meshType=ui->comboBoxMeshType->currentText().toStdString();
    if( meshType=="MeshSim" && (modelType=="PolyData" || modelType=="OpenCASCADE") )
    {
        QMessageBox::warning(NULL,"Not Compatible!", QString::fromStdString(meshType)+ " doesn't work with " +QString::fromStdString(modelType) + " model.");
        return;
    }

    if( meshType=="TetGen" && modelType!="PolyData")
    {
        QMessageBox::warning(NULL,"Not Compatible!", QString::fromStdString(meshType)+ " only works with PolyData model.");
        return;
    }

    std::string meshName=ui->lineEditMeshName->text().trimmed().toStdString();

    if(meshName==""){
        meshName=selectedModelNode->GetName();
    }

    mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(meshName.c_str(),m_MeshFolderNode);
    if(exitingNode){
        QMessageBox::warning(NULL,"Mesh Already Created","Please use a different mesh name!");
        return;
    }

    svMitkMesh::Pointer mitkMesh = svMitkMesh::New();
    mitkMesh->SetModelName(selectedModelNode->GetName());
    mitkMesh->SetType(ui->comboBoxMeshType->currentText().toStdString());
    mitkMesh->SetDataModified();

    mitk::DataNode::Pointer meshNode = mitk::DataNode::New();
    meshNode->SetData(mitkMesh);
    meshNode->SetName(meshName);

//    m_DataStorage->Add(meshNode,m_MeshFolderNode);
    mitk::OperationEvent::IncCurrObjectEventId();

    bool undoEnabled=true;
    svDataNodeOperation* doOp = new svDataNodeOperation(svDataNodeOperation::OpADDDATANODE,m_DataStorage,meshNode,m_MeshFolderNode);
    if(undoEnabled)
    {
        svDataNodeOperation* undoOp = new svDataNodeOperation(svDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,meshNode,m_MeshFolderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    m_Interface->ExecuteOperation(doOp);

    hide();
}

void svMeshCreate::Cancel()
{
    hide();
}
