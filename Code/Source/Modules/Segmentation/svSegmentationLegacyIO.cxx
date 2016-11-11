#include "svSegmentationLegacyIO.h"
#include "svContourGroup.h"
#include "svSegmentationUtils.h"
#include <mitkNodePredicateDataType.h>

#include <QString>
#include <QStringList>
#include <QList>
#include <QHash>
#include <QFile>
#include <QTextStream>
#include <QFileInfo>
#include <QDir>

//#include <algorithm>

mitk::DataNode::Pointer svSegmentationLegacyIO::ReadContourGroupFile(QString filePath)
{
    QFileInfo fi(filePath);
    QString groupName = fi.baseName();

    svContourGroup::Pointer contourGroup=svContourGroup::New();
//    contourGroup->SetPathName(groupName.toStdString());

    QFile inputFile(filePath);
    if (inputFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&inputFile);
        while (!in.atEnd())
        {
            QString line = in.readLine();

            if(line.contains("/group/"))
            {
                QStringList list = line.split("/");
                contourGroup->SetPathName(list[1].toStdString());

                svContour* contour= new svContour();
                contourGroup->InsertContour(-1,contour);

                svPathElement::svPathPoint pathPoint;
                line = in.readLine();
                pathPoint.id=line.toInt();

                line = in.readLine();
                list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);

                int index;

                index=list.indexOf("pathId");
                if(index!=-1)
                {
                    contourGroup->SetPathID(list[index+1].toInt());
                }

                //index=list.indexOf("posId");

                index=list.indexOf("pos");
                if(index!=-1)
                {
                    for(int i=0;i<3;i++)
                        pathPoint.pos[i]=list[index+i+1].toDouble();
                }

                index=list.indexOf("nrm");
                if(index!=-1)
                {
                    for(int i=0;i<3;i++)
                        pathPoint.tangent[i]=list[index+i+1].toDouble();
                }

                index=list.indexOf("xhat");
                if(index!=-1)
                {
                    for(int i=0;i<3;i++)
                        pathPoint.rotation[i]=list[index+i+1].toDouble();
                }

                contour->SetPathPoint(pathPoint);
                contour->SetMethod("Legacy");
                contour->SetPlaced();

                std::vector<mitk::Point3D> contourPoints;
                while((line=in.readLine().trimmed())!=""){
                    list = line.split(QRegExp("\\s+"));
                    mitk::Point3D point;
                    for(int i=0;i<3;i++)
                    {
                        point[i]=list[i].toDouble();
                    }
                    contourPoints.push_back(point);
                }

                contour->SetContourPoints(contourPoints);

            }

        }
    }

    mitk::DataNode::Pointer node = mitk::DataNode::New();
    node->SetData(contourGroup);
    node->SetName(groupName.toStdString());

    return node;
}

std::vector<mitk::DataNode::Pointer> svSegmentationLegacyIO::ReadFiles(QString segDir)
{
    QDir dirSeg(segDir);
    QFileInfoList fileInfoList=dirSeg.entryInfoList(QStringList("*"), QDir::Files, QDir::Name);
    std::vector<mitk::DataNode::Pointer> nodes;
    for(int i=0;i<fileInfoList.size();i++)
    {
        QString filePath=fileInfoList[i].absoluteFilePath();
        if(filePath.endsWith(".tcl")||filePath.endsWith(".vtp")||filePath.endsWith(".svsurf"))
        {
            continue;
        }

        mitk::DataNode::Pointer node=ReadContourGroupFile(filePath);
        nodes.push_back(node);
    }
    return nodes;
}


void svSegmentationLegacyIO::WriteContourGroupFile(mitk::DataNode::Pointer node, QString filePath)
{
    if(node.IsNull()) return;

    svContourGroup* contourGroup=dynamic_cast<svContourGroup*>(node->GetData());
    if(!contourGroup) return;

    QFile outputFile(filePath);
    if(outputFile.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        QTextStream out(&outputFile);
         out.setRealNumberPrecision(17);

        std::string groupName=node->GetName();
        std::string pathName=contourGroup->GetPathName();
        int pathID=contourGroup->GetPathID();

        for(int i=0;i<contourGroup->GetSize();i++)
        {
            svContour* contour=contourGroup->GetContour(i);
            if(!contour) continue;

            svPathElement::svPathPoint pathPoint=contour->GetPathPoint();

            out<<"/group/"<<groupName.c_str()<<"/"<<pathPoint.id<<endl;
            out<<pathPoint.id<<endl;
            out<<"pathId "<<pathID<<" posId "<<pathPoint.id
                <<" pos {"<<pathPoint.pos[0]<<" "<<pathPoint.pos[1]<<" "<<pathPoint.pos[2]<<"}"
                <<" nrm {"<<pathPoint.tangent[0]<<" "<<pathPoint.tangent[1]<<" "<<pathPoint.tangent[2]<<"}"
                <<" xhat {"<<pathPoint.rotation[0]<<" "<<pathPoint.rotation[1]<<" "<<pathPoint.rotation[2]<<"}"
                <<endl;

            for(int j=0;j<contour->GetContourPointNumber();j++)
            {
                mitk::Point3D point=contour->GetContourPoint(j);
                out<<point[0]<<" "<<point[1]<<" "<<point[2]<<endl;
            }

            out<<endl;

        }

        outputFile.close();
    }

}

void svSegmentationLegacyIO::WriteSeg3DFile(mitk::DataNode::Pointer node, QString filePath)
{

}

void svSegmentationLegacyIO::WriteTclFile(mitk::DataStorage::SetOfObjects::ConstPointer rsContourGroup, mitk::DataStorage::SetOfObjects::ConstPointer rsSeg3D, QString filePath)
{
    QFile outputFile(filePath);
    if(outputFile.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        QTextStream out(&outputFile);

        out<<"# geodesic_groups_file 2.3"<<endl;
        out<<endl;
        out<<"#"<<endl;

        out<<"proc group_autoload {} {"<<endl;
        out<<"  global gFilenames"<<endl;
        out<<"  set grpdir $gFilenames(groups_dir)"<<endl;
        out<<"  # Group Stuff"<<endl;
        for(int i=0;i<rsContourGroup->size();i++)
        {
            std::string name=rsContourGroup->GetElement(i)->GetName();
            out<<"  group_readProfiles {"<<name.c_str()<<"} [file join $grpdir {"<<name.c_str()<<"}]"<<endl;
        }
        out<<"}"<<endl;

        out<<"proc seg3d_autoload {} {"<<endl;
        out<<"  global gFilenames"<<endl;
        out<<"  set grpdir $gFilenames(groups_dir)"<<endl;
        out<<"  # Seg Stuff"<<endl;
        for(int i=0;i<rsSeg3D->size();i++)
        {
            std::string name=rsSeg3D->GetElement(i)->GetName();
            out<<"  seg3d_readSurf {"<<name.c_str()<<"} [file join $grpdir {"<<name.c_str()<<".svsurf"<<"}]"<<endl;
        }
        out<<"}"<<endl;

        outputFile.close();
    }
}

void svSegmentationLegacyIO::WriteFiles(mitk::DataStorage::SetOfObjects::ConstPointer contoruGroupNodes, mitk::DataStorage::SetOfObjects::ConstPointer seg3DNodes, QString segDir)
{
    QDir dirSeg(segDir);

    for(int i=0;i<contoruGroupNodes->size();i++){
        mitk::DataNode::Pointer node=contoruGroupNodes->GetElement(i);
        QString	filePath=dirSeg.absoluteFilePath(QString::fromStdString(node->GetName()));
        WriteContourGroupFile(node, filePath);
    }

    for(int i=0;i<seg3DNodes->size();i++){
        mitk::DataNode::Pointer node=seg3DNodes->GetElement(i);
        QString	filePath=dirSeg.absoluteFilePath(QString::fromStdString(node->GetName())+".vtp");
        WriteSeg3DFile(node, filePath);
    }

    QString	filePath=dirSeg.absoluteFilePath("group_contents.tcl");
    WriteTclFile(contoruGroupNodes,seg3DNodes,filePath);
}


