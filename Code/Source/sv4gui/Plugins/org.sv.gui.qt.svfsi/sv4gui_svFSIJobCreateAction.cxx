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

#include "sv4gui_svFSIJobCreateAction.h"

#include "sv4gui_MitksvFSIJob.h"
#include "sv4gui_DataNodeOperation.h"

#include <mitkDataStorage.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkOperationEvent.h>

#include <QInputDialog>
#include <QMessageBox>

sv4guisvFSIJobCreateAction::sv4guisvFSIJobCreateAction()
    : m_svFSIJobCreateWidget(NULL), m_Functionality(NULL)
{
    //m_Interface=new sv4guiDataNodeOperationInterface;
}

sv4guisvFSIJobCreateAction::~sv4guisvFSIJobCreateAction()
{
    if(m_svFSIJobCreateWidget) {
        delete m_svFSIJobCreateWidget;
    }
}

void sv4guisvFSIJobCreateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isSimulationFolder = mitk::NodePredicateDataType::New("sv4guisvFSIFolder");

    if(!isSimulationFolder->CheckNode(selectedNode))
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


        /* [DaveP]
        bool ok;
        QString text = QInputDialog::getText(NULL, tr("Create sv4guisvFSI Job"),
                                             tr("Job Name:"), QLineEdit::Normal,
                                             "", &ok);
        if(!ok)
            return;

        std::string jobName=text.trimmed().toStdString();
        if(jobName==""){
            QMessageBox::warning(NULL,"No name for job!","Please give a name for the job!");
            return;
        }

        mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(jobName.c_str(),selectedNode);
        if(exitingNode){
            QMessageBox::warning(NULL,"Job Already Created","Please use a different job name!");
            return;
        }

        sv4guiMitksvFSIJob::Pointer mitkJob = sv4guiMitksvFSIJob::New();
        sv4guisvFSIJob* job=new sv4guisvFSIJob();
        mitkJob->SetSimJob(job);
        mitkJob->SetDataModified();

        mitk::DataNode::Pointer jobNode = mitk::DataNode::New();
        jobNode->SetData(mitkJob);
        jobNode->SetName(jobName);

        mitk::OperationEvent::IncCurrObjectEventId();

        bool undoEnabled=true;
        sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,m_DataStorage,jobNode,selectedNode);
        if(undoEnabled)
        {
            sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,jobNode,selectedNode);
            mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
            mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
        }
        m_Interface->ExecuteOperation(doOp);
*/

        if(m_svFSIJobCreateWidget) {
            delete m_svFSIJobCreateWidget;
        }   
        
        m_svFSIJobCreateWidget = new sv4guisvFSIJobCreate(m_DataStorage, selectedNode);
        m_svFSIJobCreateWidget->show();
        m_svFSIJobCreateWidget->SetFocus();

    } catch(...) {
        MITK_ERROR << "svFSI Job Creation Error!";
    }
}


void sv4guisvFSIJobCreateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void sv4guisvFSIJobCreateAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}
