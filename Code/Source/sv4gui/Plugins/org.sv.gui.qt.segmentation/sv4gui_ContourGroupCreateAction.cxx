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

#include "sv4gui_ContourGroupCreateAction.h"

#include <mitkNodePredicateDataType.h>

//#include <QmitkDataManagerView.h>

sv4guiContourGroupCreateAction::sv4guiContourGroupCreateAction()
    : m_ContourGroupCreateWidget(nullptr)
    , m_Functionality(nullptr)
{
}

sv4guiContourGroupCreateAction::~sv4guiContourGroupCreateAction()
{
    if(m_ContourGroupCreateWidget)
        delete m_ContourGroupCreateWidget;
}

void sv4guiContourGroupCreateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("sv4guiSegmentationFolder");

    if(!isSegFolder->CheckNode(selectedNode))
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

        if(m_ContourGroupCreateWidget)
        {
            delete m_ContourGroupCreateWidget;
        }

        m_ContourGroupCreateWidget=new sv4guiContourGroupCreate(m_DataStorage, selectedNode, timeStep);
        m_ContourGroupCreateWidget->show();
        m_ContourGroupCreateWidget->SetFocus();


    }
    catch(...)
    {
        MITK_ERROR << "Contour Group Creation Error!";
    }
}


void sv4guiContourGroupCreateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void sv4guiContourGroupCreateAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

