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

#include "sv4gui_MeshCreateAction.h"

#include <mitkNodePredicateDataType.h>

//#include <QmitkDataManagerView.h>

sv4guiMeshCreateAction::sv4guiMeshCreateAction()
    : m_MeshCreateWidget(nullptr)
    , m_Functionality(nullptr)
{
}

sv4guiMeshCreateAction::~sv4guiMeshCreateAction()
{
    if(m_MeshCreateWidget)
        delete m_MeshCreateWidget;
}

void sv4guiMeshCreateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isMeshFolder = mitk::NodePredicateDataType::New("sv4guiMeshFolder");

    if(!isMeshFolder->CheckNode(selectedNode))
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
        int timeStep=0;
//        if(timeNavigationController)
//        {
//            timeStep=timeNavigationController->GetTime()->GetPos();
//        }

        if(m_MeshCreateWidget)
        {
            delete m_MeshCreateWidget;
        }

        m_MeshCreateWidget=new sv4guiMeshCreate(m_DataStorage, selectedNode, timeStep);
        m_MeshCreateWidget->show();
        m_MeshCreateWidget->SetFocus();
    }
    catch(...)
    {
        MITK_ERROR << "Mesh Creation Error!";
    }
}


void sv4guiMeshCreateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void sv4guiMeshCreateAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

