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

#include "sv4gui_ContourGroupCreate.h"
#include "ui_sv4gui_ContourGroupCreate.h"

#include "sv4gui_Path.h"
#include "sv4gui_DataNodeOperation.h"
#include "sv4gui_LoftingUtils.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkOperationEvent.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

sv4guiContourGroupCreate::sv4guiContourGroupCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep)
    : ui(new Ui::sv4guiContourGroupCreate)
    , m_DataStorage(dataStorage)
    , m_SelecteNode(selectedNode)
    , m_TimeStep(timeStep)
    , m_SegFolderNode(NULL)
    , m_PathFolderNode(NULL)
{
    m_Interface=new sv4guiDataNodeOperationInterface;

    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateGroup()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditGroupName, SIGNAL(returnPressed()), this, SLOT(CreateGroup()));
    move(400,400);

    Activated();
}

sv4guiContourGroupCreate::~sv4guiContourGroupCreate()
{
    delete ui;
}

void sv4guiContourGroupCreate::Activated()
{
    ui->comboBox->clear();

    m_PathFolderNode=NULL;
    m_SegFolderNode=NULL;

    if(m_SelecteNode.IsNull())
        return;

    mitk::DataNode::Pointer selectedNode=m_SelecteNode;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (selectedNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("sv4guiSegmentationFolder");
        mitk::NodePredicateDataType::Pointer isGroupNode = mitk::NodePredicateDataType::New("sv4guiContourGroup");

        if(isSegFolder->CheckNode(selectedNode)){
            m_SegFolderNode=selectedNode;
        }else if(isGroupNode->CheckNode(selectedNode)){
            mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(selectedNode);
            if(rs->size()>0){
                m_SegFolderNode=rs->GetElement(0);
            }
        }

        rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiPathFolder"));
        if (rs->size()>0)
        {
            m_PathFolderNode=rs->GetElement(0);

            rs=m_DataStorage->GetDerivations(m_PathFolderNode,mitk::NodePredicateDataType::New("sv4guiPath"));

            for(int i=0;i<rs->size();i++)
            {
                ui->comboBox->addItem(QString::fromStdString(rs->GetElement(i)->GetName()));
            }
        }

    }

    ui->lineEditGroupName->clear();
}

void sv4guiContourGroupCreate::SetFocus( )
{
    ui->comboBox->setFocus();
}

//-------------
// CreateGroup
//-------------
//
void sv4guiContourGroupCreate::CreateGroup()
{
    QString selectedPathName=ui->comboBox->currentText();
    if(selectedPathName=="") {
        QMessageBox::warning(NULL,"No Path Selected","Please select a path!");
        return;
    }

    mitk::DataNode::Pointer selectedPathNode=m_DataStorage->GetNamedDerivedNode(selectedPathName.toStdString().c_str(),m_PathFolderNode);

    if(selectedPathNode.IsNull()) {
        QMessageBox::warning(NULL,"No Path Found!","Please select a existing path!");
        return;
    }

    std::string groupName = ui->lineEditGroupName->text().trimmed().toStdString();

    if(groupName==""){
        groupName=selectedPathNode->GetName();
    }

    mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(groupName.c_str(),m_SegFolderNode);
    if(exitingNode){
        QMessageBox::warning(NULL,"Contour Group Already Created","Please use a different group name!");
        return;
    }

    if (!sv4guiDataNodeOperationInterface::IsValidDataNodeName(groupName)) {
        auto validName = QString::fromStdString(sv4guiDataNodeOperationInterface::ValidDataNodeNameMsg);
        auto groupName = ui->lineEditGroupName->text().trimmed();
        QString msg = "The name '" + groupName + "' is not valid.\n" +
                      "Contour group names " + validName + ".\n";
        QMessageBox::warning(NULL, "Contour group", msg);
        return;
    }

    sv4guiContourGroup::Pointer group = sv4guiContourGroup::New();

    sv4guiLoftingUtils::SetPreferencedValues(group->GetLoftingParam());

    group->SetPathName(selectedPathNode->GetName());
    group->SetDataModified();

    sv4guiPath* selectedPath=dynamic_cast<sv4guiPath*>(selectedPathNode->GetData());
    if(selectedPath)
    {
        group->SetPathID(selectedPath->GetPathID());
    }

    mitk::DataNode::Pointer groupNode = mitk::DataNode::New();
    groupNode->SetData(group);
    groupNode->SetName(groupName);

    float point2DSize=0;
    float pointSize=0;
    float resliceSize=0;
    if(m_SelecteNode.IsNotNull())
    {
        m_SelecteNode->GetFloatProperty("point.displaysize",point2DSize);
        m_SelecteNode->GetFloatProperty("point.3dsize",pointSize);
        m_SelecteNode->GetFloatProperty("reslice size",resliceSize);
        if(resliceSize==0)
        {
            sv4guiContourGroup* originalGroup=dynamic_cast<sv4guiContourGroup*>(m_SelecteNode->GetData());
            if(originalGroup)
                resliceSize=originalGroup->GetResliceSize();
        }
    }

    if(point2DSize!=0)
    {
        groupNode->SetFloatProperty("point.displaysize",point2DSize);
        group->SetProp("point 2D display size",QString::number(point2DSize).toStdString());
    }
    if(pointSize!=0)
    {
        groupNode->SetFloatProperty("point.3dsize",pointSize);
        group->SetProp("point size",QString::number(pointSize).toStdString());
    }
    if(resliceSize!=0)
        group->SetResliceSize(resliceSize);

//    m_DataStorage->Add(groupNode,m_SegFolderNode);
    mitk::OperationEvent::IncCurrObjectEventId();

    bool undoEnabled=true;
    sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,m_DataStorage,groupNode,m_SegFolderNode);
    if(undoEnabled)
    {
        sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,groupNode,m_SegFolderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    m_Interface->ExecuteOperation(doOp);

    hide();
}

void sv4guiContourGroupCreate::Cancel()
{
    hide();
}

