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

#include "sv4gui_SegmentationLegacySaveAction.h"

#include "sv4gui_SegmentationLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <QFileDialog>

sv4guiSegmentationLegacySaveAction::sv4guiSegmentationLegacySaveAction()
{
}

sv4guiSegmentationLegacySaveAction::~sv4guiSegmentationLegacySaveAction()
{
}

void sv4guiSegmentationLegacySaveAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
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
            lastFilePath = prefs->Get("LastFileSavePath", "");
        }
        if(lastFilePath=="")
            lastFilePath=QDir::homePath();


        QString segDir = QFileDialog::getExistingDirectory(NULL, tr("Export as Legacy Segmentations (Choose Directory)"),
                                                             lastFilePath);

        segDir=segDir.trimmed();
        if(segDir.isEmpty()) return;

        mitk::DataStorage::SetOfObjects::ConstPointer rsContourGroup=m_DataStorage->GetDerivations(selectedNode,mitk::NodePredicateDataType::New("sv4guiContourGroup"));

        mitk::DataStorage::SetOfObjects::ConstPointer rsSeg3D=m_DataStorage->GetDerivations(selectedNode,mitk::NodePredicateDataType::New("sv4guiMitkSeg3D"));

        sv4guiSegmentationLegacyIO::WriteFiles(rsContourGroup, rsSeg3D, segDir);

        if(prefs.IsNotNull())
         {
             prefs->Put("LastFileSavePath", segDir);
             prefs->Flush();
         }
    }
    catch(...)
    {
        MITK_ERROR << "Legacy Segmentations Saving Error!";
    }
}

void sv4guiSegmentationLegacySaveAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
