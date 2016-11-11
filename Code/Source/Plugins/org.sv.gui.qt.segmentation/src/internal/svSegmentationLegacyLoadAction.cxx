#include "svSegmentationLegacyLoadAction.h"

#include "svSegmentationLegacyIO.h"
#include "svContourGroup.h"

#include <mitkNodePredicateDataType.h>

#include <QFileDialog>

svSegmentationLegacyLoadAction::svSegmentationLegacyLoadAction()
{
}

svSegmentationLegacyLoadAction::~svSegmentationLegacyLoadAction()
{
}

svPath* svSegmentationLegacyLoadAction::GetPath(int groupPathID, mitk::DataNode::Pointer segFolderNode)
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

            for(int i=0;i<rs->size();i++)
            {
                svPath* path=dynamic_cast<svPath*>(rs->GetElement(i)->GetData());

                if(path&&groupPathID==path->GetPathID())
                {
                    return path;
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
        QString segDir = QFileDialog::getExistingDirectory(NULL, tr("Choose Directory"),
                                                             QDir::homePath(),
                                                             QFileDialog::ShowDirsOnly
                                                             | QFileDialog::DontResolveSymlinks
                                                             | QFileDialog::DontUseNativeDialog
                                                             );

        if(segDir.trimmed().isEmpty()) return;

        std::vector<mitk::DataNode::Pointer> segNodes=svSegmentationLegacyIO::ReadFiles(segDir);

        for(int i=0;i<segNodes.size();i++)
        {

            //update path points using posID, since path point data is wrong sometimes form contour group file
            svContourGroup* contourGroup=dynamic_cast<svContourGroup*>(segNodes[i]->GetData());
            if(contourGroup)
            {
                svPath* path=GetPath(contourGroup->GetPathID(), selectedNode);
                svPathElement* pe=NULL;
                if(path)
                    pe=path->GetPathElement();

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

