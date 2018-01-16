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

#include "svSeg3DCreateAction.h"
#include "svMitkSeg3D.h"
#include "svDataNodeOperation.h"

#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>

#include <QInputDialog>
#include <QMessageBox>

svSeg3DCreateAction::svSeg3DCreateAction()
    : m_Functionality(NULL)
{
    m_Interface=new svDataNodeOperationInterface;
}

svSeg3DCreateAction::~svSeg3DCreateAction()
{
}

void svSeg3DCreateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer segFolderNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");

    if(!isSegFolder->CheckNode(segFolderNode))
    {
        return;
    }

    try
    {
//        if(!m_Functionality)
//            return;

//        QmitkDataManagerView* dmView=dynamic_cast<QmitkDataManagerView*>(m_Functionality);

//        if(!dmView)
//            return;

//        mitk::IRenderWindowPart* renderWindowPart = dmView->GetRenderWindowPart();

//        if(!renderWindowPart)
//            return;

//        mitk::SliceNavigationController* timeNavigationController=renderWindowPart->GetTimeNavigationController();
//        int timeStep=0;
//        if(timeNavigationController)
//        {
//            timeStep=timeNavigationController->GetTime()->GetPos();
//        }

        bool ok;
        QString text = QInputDialog::getText(NULL, tr("Create 3D Segmentation"),
                                             tr("Name:"), QLineEdit::Normal,
                                             "", &ok);
        if (!ok || text.trimmed().isEmpty())
            return;

        std::string segName=text.trimmed().toStdString();

        mitk::DataNode::Pointer existingNode=m_DataStorage->GetNamedDerivedNode(segName.c_str(),segFolderNode);
        if(existingNode){
            QMessageBox::warning(NULL,"Name Already Exists","Please use a different name!");
            return;
        }

        svMitkSeg3D::Pointer mitkSeg3D = svMitkSeg3D::New();
        mitkSeg3D->SetDataModified();

        svSeg3D* seg3D=new svSeg3D();
        mitkSeg3D->SetSeg3D(seg3D);

        mitk::DataNode::Pointer segNode = mitk::DataNode::New();
        segNode->SetData(mitkSeg3D);
        segNode->SetName(segName);

    //    m_DataStorage->Add(groupNode,m_SegFolderNode);
        mitk::OperationEvent::IncCurrObjectEventId();

        bool undoEnabled=true;
        svDataNodeOperation* doOp = new svDataNodeOperation(svDataNodeOperation::OpADDDATANODE,m_DataStorage,segNode,segFolderNode);
        if(undoEnabled)
        {
            svDataNodeOperation* undoOp = new svDataNodeOperation(svDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,segNode,segFolderNode);
            mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
            mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
        }
        m_Interface->ExecuteOperation(doOp);

    }
    catch(...)
    {
        MITK_ERROR << "3D Segmentation Data Node Creation Error!";
    }
}


void svSeg3DCreateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svSeg3DCreateAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

