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

#include "svSegmentationLegacyLoadAction.h"

#include "svSegmentationLegacyIO.h"
#include "svContourGroup.h"

#include <mitkNodePredicateDataType.h>
#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <QFileDialog>

svSegmentationLegacyLoadAction::svSegmentationLegacyLoadAction()
{
}

svSegmentationLegacyLoadAction::~svSegmentationLegacyLoadAction()
{
}

svPath* svSegmentationLegacyLoadAction::GetPath(int groupPathID, std::string groupPathName, mitk::DataNode::Pointer segFolderNode)
{
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (segFolderNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svPathFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer pathFolderNode=rs->GetElement(0);
            rs=m_DataStorage->GetDerivations(pathFolderNode);

            if(groupPathID!=-1)
            {
                for(int i=0;i<rs->size();i++)
                {
                    svPath* path=dynamic_cast<svPath*>(rs->GetElement(i)->GetData());

                    if(path&&groupPathID==path->GetPathID())
                    {
                        return path;
                    }
                }
            }

            if(groupPathName!="")
            {
                for(int i=0;i<rs->size();i++)
                {
                    mitk::DataNode::Pointer pathNode=rs->GetElement(i);
                    svPath* path=dynamic_cast<svPath*>(pathNode->GetData());

                    if(path&&groupPathName==pathNode->GetName())
                    {
                        return path;
                    }
                }
            }

        }
    }

    return NULL;
}

void svSegmentationLegacyLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");

    if(!isSegFolder->CheckNode(selectedNode))
    {
        return;
    }

    try
    {
        berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
        berry::IPreferences::Pointer prefs;
        if (prefService)
        {
            prefs = prefService->GetSystemPreferences()->Node("/General");
        }
        else
        {
            prefs = berry::IPreferences::Pointer(0);
        }

        QString lastFilePath="";
        if(prefs.IsNotNull())
        {
            lastFilePath = prefs->Get("LastFileOpenPath", "");
        }
        if(lastFilePath=="")
            lastFilePath=QDir::homePath();

        QString segDir = QFileDialog::getExistingDirectory(NULL, tr("Import Legacy Segmentations (Choose Directory)"),
                                                             lastFilePath);

        segDir=segDir.trimmed();
        if(segDir.isEmpty()) return;

        std::vector<mitk::DataNode::Pointer> segNodes=svSegmentationLegacyIO::ReadFiles(segDir);

        if(prefs.IsNotNull())
         {
             prefs->Put("LastFileOpenPath", segDir);
             prefs->Flush();
         }

        for(int i=0;i<segNodes.size();i++)
        {

            //update path points using posID, since path point data is wrong sometimes form contour group file
            svContourGroup* contourGroup=dynamic_cast<svContourGroup*>(segNodes[i]->GetData());
            if(contourGroup)
            {
                svPath* path=GetPath(contourGroup->GetPathID(), contourGroup->GetPathName(), selectedNode);
                svPathElement* pe=NULL;
                if(path)
                {
                    pe=path->GetPathElement();
                    contourGroup->SetPathID(path->GetPathID());
                }

                if(pe)
                {
                    for(int i=0;i<contourGroup->GetSize();i++)
                    {
                        svContour* contour=contourGroup->GetContour(i);
                        if(contour)
                        {
                            int posID=contour->GetPathPosID();
                            contour->SetPathPoint(pe->GetPathPoint(posID));
                            //recalculate center and scaling points, since path point changed
                            contour->ContourPointsChanged();
                        }
                    }
                }
            }
            m_DataStorage->Add(segNodes[i],selectedNode);
        }

    }
    catch(...)
    {
        MITK_ERROR << "Legacy Segmentations Loading Error!";
    }
}

void svSegmentationLegacyLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

