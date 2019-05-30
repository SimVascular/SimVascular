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

#include "sv4gui_PathCreate.h"
#include "ui_sv4gui_PathCreate.h"

#include "sv4gui_Path.h"
#include "sv4gui_PathOperation.h"
#include "sv4gui_DataNodeOperation.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkImage.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

sv4guiPathCreate::sv4guiPathCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep)
    : ui(new Ui::sv4guiPathCreate)
    , m_DataStorage(dataStorage)
    , m_SelecteNode(selectedNode)
    , m_TimeStep(timeStep)
    , m_CreatePath(true)
    , m_PathFolderNode(NULL)
    , m_UpdateNumberSpacing(true)
{
    m_Interface=new sv4guiDataNodeOperationInterface;

    //    m_Parent=parent;
    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreatePath()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditPathName, SIGNAL(returnPressed()), this, SLOT(CreatePath()));
    connect(ui->comboBoxSubdivisionType, SIGNAL(currentIndexChanged(int)), this, SLOT(ResetNumberSpacing(int )));
    connect(ui->lineEditNumber, SIGNAL(returnPressed()), this, SLOT(CreatePath()));

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("sv4guiPathFolder");
    mitk::NodePredicateDataType::Pointer isPathNode = mitk::NodePredicateDataType::New("sv4guiPath");
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
        }
    }

    move(400,400);
}

sv4guiPathCreate::~sv4guiPathCreate()
{
    delete ui;
}

void sv4guiPathCreate::SetFocus( )
{
    ui->lineEditPathName->setFocus();
}

void sv4guiPathCreate::ResetNumberSpacing(int index)
{
    if(index==2)
    {
        ui->labelNumberSpacing->setText("Spacing:");
        if(!m_UpdateNumberSpacing)
            return;

        double spacing=GetVolumeImageSpacing();
        if(spacing==-1.0)
        {
            ui->lineEditNumber->setText("");
            QMessageBox::warning(this,"No image found","Please provide a value for spacing!");
        }
        else
        {
            ui->lineEditNumber->setText(QString::number(spacing));
            QMessageBox::information(this,"Image found","The spacing is filled with the minimum image spacing.");
        }
    }
    else
    {
        ui->labelNumberSpacing->setText("Number:");
        if(!m_UpdateNumberSpacing)
            return;

        if(index==0)
            ui->lineEditNumber->setText("100");
        else
            ui->lineEditNumber->setText("10");
    }
}

//---------------
// SetCreatePath
//---------------
//
void sv4guiPathCreate::SetCreatePath(bool create)
{
    m_CreatePath = create;

    if (create) {
        setWindowTitle("Create New Path");
        ui->lineEditPathName->setEnabled(true);
    } else {
        ui->lineEditPathName->setEnabled(false);
        setWindowTitle("Change Current Path");
    }
}

//------------
// CreatePath
//------------
//
void sv4guiPathCreate::CreatePath()
{
    auto pathName = ui->lineEditPathName->text().trimmed().toStdString();

    if (!sv4guiDataNodeOperationInterface::IsValidDataNodeName(pathName)) { 
        auto validName = QString::fromStdString(sv4guiDataNodeOperationInterface::ValidDataNodeNameMsg);
        auto pname = ui->lineEditPathName->text().trimmed();
        QString msg = "The name '" + pname + "' is not valid.\n" + 
                      "Path names " + validName + ".\n"; 
        QMessageBox::warning(NULL, "Path", msg); 
        return;
    }

    if(m_CreatePath) {
        if(pathName==""){
            QMessageBox::warning(NULL,"Path Empty","Please give a path name!");
            return;
        }

        mitk::DataNode::Pointer exitingNode=NULL;
        if(m_PathFolderNode.IsNull())
            exitingNode=m_DataStorage->GetNamedNode(pathName);
        else
            exitingNode=m_DataStorage->GetNamedDerivedNode(pathName.c_str(),m_PathFolderNode);

        if(exitingNode){
            QMessageBox::warning(NULL,"Path Already Created","Please use a different path name!");
            return;
        }
    }

    int currentIndex=ui->comboBoxSubdivisionType->currentIndex();

    bool ok=false;

    int subdivisionNum=ui->lineEditNumber->text().trimmed().toInt(&ok);
    if(currentIndex==0 && (!ok || subdivisionNum<2)){
        QMessageBox::warning(NULL,"Total Point Number Not Valid","Please give a valid number >= 2!");
        return;
    }
    if(currentIndex==1 && (!ok || subdivisionNum<1)){
        QMessageBox::warning(NULL,"Subdivision Number Not Valid","Please give a valid number >= 1!");
        return;
    }

    ok=false;
    double spacing=ui->lineEditNumber->text().trimmed().toDouble(&ok);
    if(currentIndex==2 && (!ok || spacing<=0)){
        QMessageBox::warning(NULL,"Spacing Not Valid","Please give a valid value > 0!");
        return;
    }

    int maxPathID=sv4guiPath::GetMaxPathID(m_DataStorage->GetDerivations(m_PathFolderNode));

    if(m_CreatePath)
    {
        sv4guiPath::Pointer path = sv4guiPath::New();
        path->SetPathID(maxPathID+1);
        int timeStep=m_TimeStep;

        switch(currentIndex)
        {
        case 0:
            path->SetMethod(sv3::PathElement::CONSTANT_TOTAL_NUMBER);
            path->SetCalculationNumber(subdivisionNum);
            path->SetSpacing(0);
            break;
        case 1:
            path->SetMethod(sv3::PathElement::CONSTANT_SUBDIVISION_NUMBER);
            path->SetCalculationNumber(subdivisionNum);
            path->SetSpacing(0);
            break;
        case 2:
            path->SetMethod(sv3::PathElement::CONSTANT_SPACING);
            path->SetSpacing(spacing);
            path->SetCalculationNumber(0);
            break;
        default:
            return;
        }

        sv4guiPathElement* pathElement=new sv4guiPathElement();
        pathElement->SetMethod(path->GetMethod());
        pathElement->SetCalculationNumber(path->GetCalculationNumber());
        pathElement->SetSpacing(path->GetSpacing());

        path->SetPathElement(pathElement,timeStep);
        path->SetDataModified();

        mitk::DataNode::Pointer pathNode = mitk::DataNode::New();
        pathNode->SetData(path);
        pathNode->SetName(pathName);

        float point2DSize=0;
        float pointSize=0;
        float resliceSize=0;

        if(m_SelecteNode.IsNotNull())
        {
            m_SelecteNode->GetFloatProperty("point 2D display size",point2DSize);
            m_SelecteNode->GetFloatProperty("point size",pointSize);
            m_SelecteNode->GetFloatProperty("reslice size",resliceSize);
            if(resliceSize==0)
            {
                sv4guiPath* originalPath=dynamic_cast<sv4guiPath*>(m_SelecteNode->GetData());
                if(originalPath)
                    resliceSize=originalPath->GetResliceSize();
            }
        }

        if(point2DSize!=0)
        {
            pathNode->SetFloatProperty("point 2D display size",point2DSize);
            path->SetProp("point 2D display size",QString::number(point2DSize).toStdString());
        }
        if(pointSize!=0)
        {
            pathNode->SetFloatProperty("point size",pointSize);
            path->SetProp("point size",QString::number(pointSize).toStdString());
        }
        if(resliceSize!=0)
            path->SetResliceSize(resliceSize);

//        m_DataStorage->Add(pathNode,m_PathFolderNode);
        mitk::OperationEvent::IncCurrObjectEventId();

        bool undoEnabled=true;
        sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,m_DataStorage,pathNode,m_PathFolderNode);
        if(undoEnabled)
        {
            sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,pathNode,m_PathFolderNode);
            mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
            mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
        }
        m_Interface->ExecuteOperation(doOp);
    }
    else if(m_SelecteNode.IsNotNull())
    {
        sv4guiPath* path=dynamic_cast<sv4guiPath*>(m_SelecteNode->GetData());
        int timeStep=m_TimeStep;
        sv4guiPathElement* pathElement=path->GetPathElement(timeStep);
        sv4guiPathElement* changedPathElement=pathElement->Clone();

        switch(currentIndex)

        {
        case 0:
            changedPathElement->SetMethod(sv3::PathElement::CONSTANT_TOTAL_NUMBER);
            changedPathElement->SetCalculationNumber(subdivisionNum);
            changedPathElement->SetSpacing(0);
            break;
        case 1:
            changedPathElement->SetMethod(sv3::PathElement::CONSTANT_SUBDIVISION_NUMBER);
            changedPathElement->SetCalculationNumber(subdivisionNum);
            changedPathElement->SetSpacing(0);
            break;
        case 2:
            changedPathElement->SetMethod(sv3::PathElement::CONSTANT_SPACING);
            changedPathElement->SetSpacing(spacing);
            changedPathElement->SetCalculationNumber(0);

            break;
        default:
            return;
        }
        changedPathElement->CreatePathPoints();//update

        mitk::OperationEvent::IncCurrObjectEventId();

        sv4guiPathOperation* doOp = new sv4guiPathOperation(sv4guiPathOperation::OpSETPATHELEMENT,timeStep,changedPathElement);
        sv4guiPathOperation* undoOp = new sv4guiPathOperation(sv4guiPathOperation::OpSETPATHELEMENT,timeStep,pathElement);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(path, doOp, undoOp, "Set PathElement");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        path->ExecuteOperation(doOp);
    }

    hide();
    SetCreatePath(true);
}

double sv4guiPathCreate::GetVolumeImageSpacing()
{
    double minSpacing=-1.0;

    if(m_PathFolderNode.IsNull())
        return minSpacing;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (m_PathFolderNode,isProjFolder,false);
    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=m_DataStorage->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("sv4guiImageFolder"));
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

void sv4guiPathCreate::Cancel()
{
    hide();
    SetCreatePath(true);
}

void sv4guiPathCreate::SetPathName(QString pathName)
{
    ui->lineEditPathName->setText(pathName);
}

void sv4guiPathCreate::SetSubdivisionType(int index)
{
    m_UpdateNumberSpacing=false;
    ui->comboBoxSubdivisionType->setCurrentIndex(index);
    m_UpdateNumberSpacing=true;
}

void sv4guiPathCreate::SetNumber(QString number)
{
     ui->lineEditNumber->setText(number);
}
