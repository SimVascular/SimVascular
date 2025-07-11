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

#include "sv4gui_ContourGroupPoint3DSizeAction.h"

#include "sv4gui_ContourGroup.h"

#include <mitkNodePredicateDataType.h>

#include <QInputDialog>

sv4guiContourGroupPoint3DSizeAction::sv4guiContourGroupPoint3DSizeAction()
    : m_Functionality(nullptr)
{
}

sv4guiContourGroupPoint3DSizeAction::~sv4guiContourGroupPoint3DSizeAction()
{
}

void sv4guiContourGroupPoint3DSizeAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isGroup = mitk::NodePredicateDataType::New("sv4guiContourGroup");

    float initialValue=0.1f;
    if(isGroup->CheckNode(selectedNode))
        selectedNode->GetFloatProperty ("point.3dsize", initialValue);

    bool ok;
    float size=QInputDialog::getDouble(nullptr, "Point 3D Size", "Size:", initialValue, 0.01, 50.0, 2, &ok);

    if(!ok)
        return;

    mitk::DataNode::Pointer segFolderNode=nullptr;
    if(selectedNode.IsNotNull())
    {
        mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(selectedNode);
        if(rs->size()>0){
            segFolderNode=rs->GetElement(0);
            if(segFolderNode.IsNotNull())
                segFolderNode->SetFloatProperty("point.3dsize",size);
        }
    }

    for(int i=0;i<selectedNodes.size();i++)
    {
        mitk::DataNode::Pointer node = selectedNodes[i];
        if(!isGroup->CheckNode(node))
            continue;

        node->SetFloatProperty("point.3dsize",size);
        sv4guiContourGroup* group=dynamic_cast<sv4guiContourGroup*>(node->GetData());
        if(group)
        {
            group->SetProp("point size",QString::number(size).toStdString());
            group->SetDataModified();
        }
    }

}

void sv4guiContourGroupPoint3DSizeAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void sv4guiContourGroupPoint3DSizeAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

