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

#include "sv4gui_PathSmooth.h"
#include "ui_sv4gui_PathSmooth.h"

#include "sv4gui_Path.h"
#include "sv4gui_PathOperation.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkRenderingManager.h>

#include <QMessageBox>

sv4guiPathSmooth::sv4guiPathSmooth()
    : ui(new Ui::sv4guiPathSmooth)
{
    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(SmoothPath()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));

    move(400,400);
}

sv4guiPathSmooth::~sv4guiPathSmooth()
{
    delete ui;
}

void sv4guiPathSmooth::SetDataStorage(mitk::DataStorage::Pointer dataStorage)
{
    m_DataStorage=dataStorage;
}

void sv4guiPathSmooth::SetSelectedNode(mitk::DataNode::Pointer selectedNode)
{
    m_SelecteNode=selectedNode;
}

void sv4guiPathSmooth::SetTimeStep(int timeStep)
{
    m_TimeStep=timeStep;
}

void sv4guiPathSmooth::SetFocus( )
{
    ui->lineEditSubsample->setFocus();
}

void sv4guiPathSmooth::SmoothPath()
{
    if(m_SelecteNode.IsNull())
    {
        return;
    }

    sv4guiPath* path=dynamic_cast<sv4guiPath*>(m_SelecteNode->GetData());
    if(path==NULL) return;

    int timeStep=m_TimeStep;
    sv4guiPathElement* pathElement=path->GetPathElement(timeStep);
    if(pathElement==NULL)
    {
        QMessageBox::information(NULL,"No Path Selected","Please select a path in data manager!");
        return;
    }

    int numModes=ui->lineEditNumber->text().trimmed().toInt();
    if(numModes<2){
        QMessageBox::warning(NULL,"Not Enough Modes","Number of fourier mode must be greater than 1.");
        return;
    }

    int sampleRate=ui->lineEditSubsample->text().trimmed().toInt();

    int currentIndex=ui->comboBoxPointType->currentIndex();
    bool controlPointsBased=currentIndex==0?true:false;

    sv4guiPathElement* smoothedPathElement=pathElement->CreateSmoothedPathElement(sampleRate,numModes,controlPointsBased);

    mitk::OperationEvent::IncCurrObjectEventId();

    sv4guiPathOperation* doOp = new sv4guiPathOperation(sv4guiPathOperation::OpSETPATHELEMENT,timeStep,smoothedPathElement);
    sv4guiPathOperation* undoOp = new sv4guiPathOperation(sv4guiPathOperation::OpSETPATHELEMENT,timeStep,pathElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(path, doOp, undoOp, "Set PathElement");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    path->ExecuteOperation(doOp);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    hide();
}

void sv4guiPathSmooth::Cancel()
{
    hide();
}
