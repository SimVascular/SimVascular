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

#include "sv4gui_SegmentationLegacyLoadAction.h"

#include "sv4gui_SegmentationLegacyIO.h"
#include "sv4gui_ContourGroup.h"

#include <mitkNodePredicateDataType.h>
#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <QFileDialog>

sv4guiSegmentationLegacyLoadAction::sv4guiSegmentationLegacyLoadAction()
{
}

sv4guiSegmentationLegacyLoadAction::~sv4guiSegmentationLegacyLoadAction()
{
}

sv4guiPath* sv4guiSegmentationLegacyLoadAction::GetPath(int groupPathID, std::string groupPathName, mitk::DataNode::Pointer segFolderNode)
{
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (segFolderNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiPathFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer pathFolderNode=rs->GetElement(0);
            rs=m_DataStorage->GetDerivations(pathFolderNode);

            if(groupPathID!=-1)
            {
                for(int i=0;i<rs->size();i++)
                {
                    sv4guiPath* path=dynamic_cast<sv4guiPath*>(rs->GetElement(i)->GetData());

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
                    sv4guiPath* path=dynamic_cast<sv4guiPath*>(pathNode->GetData());

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

void sv4guiSegmentationLegacyLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("sv4guiSegmentationFolder");

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

        std::vector<mitk::DataNode::Pointer> segNodes=sv4guiSegmentationLegacyIO::ReadFiles(segDir);

        if(prefs.IsNotNull())
         {
             prefs->Put("LastFileOpenPath", segDir);
             prefs->Flush();
         }

        for(int i=0;i<segNodes.size();i++)
        {

            //update path points using posID, since path point data is wrong sometimes form contour group file
            sv4guiContourGroup* contourGroup=dynamic_cast<sv4guiContourGroup*>(segNodes[i]->GetData());
            if(contourGroup)
            {
                sv4guiPath* path=GetPath(contourGroup->GetPathID(), contourGroup->GetPathName(), selectedNode);
                sv4guiPathElement* pe=NULL;
                if(path)
                {
                    pe=path->GetPathElement();
                    contourGroup->SetPathID(path->GetPathID());
                }

                if(pe)
                {
                    for(int i=0;i<contourGroup->GetSize();i++)
                    {
                        sv4guiContour* contour=contourGroup->GetContour(i);
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
            mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(segNodes[i]->GetName().c_str(),selectedNode);
            if(exitingNode){
                MITK_WARN << "Segmentation "<< segNodes[i]->GetName() << " Already Created","Please use a different name!";
            }
            m_DataStorage->Add(segNodes[i],selectedNode);
        }

    }
    catch(...)
    {
        MITK_ERROR << "Legacy Segmentations Loading Error!";
    }
}

void sv4guiSegmentationLegacyLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

