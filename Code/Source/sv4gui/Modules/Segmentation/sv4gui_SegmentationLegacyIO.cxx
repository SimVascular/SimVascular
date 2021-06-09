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

#include "sv4gui_SegmentationLegacyIO.h"
#include "sv4gui_ContourGroup.h"
#include "sv4gui_MitkSeg3D.h"
#include "sv4gui_SegmentationUtils.h"
#include <mitkNodePredicateDataType.h>

#include <QString>
#include <QStringList>
#include <QList>
#include <QHash>
#include <QFile>
#include <QTextStream>
#include <QFileInfo>
#include <QDir>

#include <vtkPolyData.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkErrorCode.h>

//---------------------
// CreateGroupFromFile
//---------------------
// Create a sv4guiContourGroup from contour file.
//
sv4guiContourGroup::Pointer
sv4guiSegmentationLegacyIO::CreateGroupFromFile(const std::string& fileName)
{
    QString qfileName = QString::fromStdString(fileName);
    QFileInfo file(qfileName);
    QString groupName = file.baseName();

    sv4guiContourGroup::Pointer contourGroup = sv4guiContourGroup::New();

    QFile inputFile(qfileName);
    if (!inputFile.open(QIODevice::ReadOnly)) {
        return contourGroup;
    }

    QTextStream in(&inputFile);

    while (!in.atEnd()) {
        QString line = in.readLine();
        if (!line.contains("/group/")) {
            continue;
        }

        QStringList list = line.split("/",QString::SkipEmptyParts);
        contourGroup->SetPathName(list[1].toStdString());

        sv4guiContour* contour = new sv4guiContour();
        contourGroup->InsertContour(-1,contour);

        sv4guiPathElement::sv4guiPathPoint pathPoint;
        line = in.readLine();
        pathPoint.id=line.toInt();

        line = in.readLine();
        list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);

        int index;

        index=list.indexOf("pathId");
        if(index!=-1) {
            contourGroup->SetPathID(list[index+1].toInt());
        }

        index=list.indexOf("pos");
        if(index!=-1) {
            for(int i=0;i<3;i++)
                pathPoint.pos[i]=list[index+i+1].toDouble();
        }

        index=list.indexOf("nrm");
        if(index!=-1) {
            for(int i=0;i<3;i++)
                pathPoint.tangent[i]=list[index+i+1].toDouble();
        }

        index=list.indexOf("xhat");
        if(index!=-1) {
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
            for(int i=0;i<3;i++) {
                point[i]=list[i].toDouble();
            }
            contourPoints.push_back(point);
        }

        contour->SetContourPoints(contourPoints,false);
    }

    inputFile.close();

    return contourGroup;
}

//----------------------
// ReadContourGroupFile
//----------------------
// Create a DataNode from a contour file.
//
mitk::DataNode::Pointer sv4guiSegmentationLegacyIO::ReadContourGroupFile(QString filePath)
{
    QFileInfo file(filePath);
    QString groupName = file.baseName();

    sv4guiContourGroup::Pointer contourGroup = CreateGroupFromFile(filePath.toStdString());

    mitk::DataNode::Pointer node = mitk::DataNode::New();
    node->SetData(contourGroup);
    node->SetName(groupName.toStdString());

    return node;
}

//-----------
// ReadFiles
//-----------
// Create DataNodes from the contour files in a directory.
//
std::vector<mitk::DataNode::Pointer> sv4guiSegmentationLegacyIO::ReadFiles(QString segDir)
{
    QStringList groupList;
    QStringList seg3DList;
    QFile inputFile(segDir+"/group_contents.tcl");
    if (inputFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&inputFile);
        while (!in.atEnd())
        {
            QString line = in.readLine();
            QStringList list=line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            if(list.size()>1 && list[0]=="group_readProfiles")
                groupList<<list[1];

            if(list.size()>1 && list[0]=="seg3d_readSurf")
                seg3DList<<list[1];
        }

        inputFile.close();
    }

    std::vector<mitk::DataNode::Pointer> nodes;
    for(int i=0;i<groupList.size();i++)
    {
        QString filePath=segDir+"/"+groupList[i];
        if(!QFile(filePath).exists())
            continue;

        mitk::DataNode::Pointer node=ReadContourGroupFile(filePath);
        nodes.push_back(node);
    }

    for(int i=0;i<seg3DList.size();i++)
    {
        QString filePath=segDir+"/"+seg3DList[i]+".vtp";
        if(!QFile(filePath).exists())
            continue;

        vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
        reader->SetFileName(filePath.toStdString().c_str());
        reader->Update();
        vtkSmartPointer<vtkPolyData> vpd=reader->GetOutput();
        if(vpd!=NULL)
        {
            sv4guiSeg3D* seg3D=new sv4guiSeg3D();
            seg3D->SetVtkPolyData(vpd);

            sv4guiMitkSeg3D::Pointer mitkSeg3D=sv4guiMitkSeg3D::New();
            mitkSeg3D->SetSeg3D(seg3D);

            mitk::DataNode::Pointer node = mitk::DataNode::New();
            node->SetData(mitkSeg3D);
            node->SetName(seg3DList[i].toStdString());

            nodes.push_back(node);
        }
    }

    return nodes;
}

void sv4guiSegmentationLegacyIO::WriteContourGroupFile(mitk::DataNode::Pointer node, QString filePath)
{
    if(node.IsNull()) return;

    sv4guiContourGroup* contourGroup=dynamic_cast<sv4guiContourGroup*>(node->GetData());
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
            sv4guiContour* contour=contourGroup->GetContour(i);
            if(!contour) continue;

            sv4guiPathElement::sv4guiPathPoint pathPoint=contour->GetPathPoint();

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

void sv4guiSegmentationLegacyIO::WriteSeg3DFile(mitk::DataNode::Pointer node, QString filePath)
{
    if(node.IsNull()) return;

    sv4guiMitkSeg3D* seg3D=dynamic_cast<sv4guiMitkSeg3D*>(node->GetData());
    if(!seg3D) return;

    vtkPolyData* vpd=seg3D->GetVtkPolyData();
    if(!vpd) return;

    QFile outputFile(filePath+".svsurf");
    if(outputFile.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        QTextStream out(&outputFile);
         out.setRealNumberPrecision(17);

        std::string seg3DName=node->GetName();

        out<<"name:"<<seg3DName.c_str()<<endl;
        out<<"metadata:opacity .8 color steelblue"<<endl;
        out<<"vtp_filename:"<<(seg3DName+".vtp").c_str()<<endl;
        out<<"vtp_filename:476556BCC9C6082371E7EEF6A27F05BE"<<endl;//dummy entry

        outputFile.close();
    }

    if(vpd)
    {
        vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
        writer->SetFileName((filePath+".vtp").toStdString().c_str());
        writer->SetInputData(vpd);
        if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
        {
            std::cerr << "vtkXMLPolyDataWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode())<<std::endl;
        }
    }


}

void sv4guiSegmentationLegacyIO::WriteTclFile(mitk::DataStorage::SetOfObjects::ConstPointer rsContourGroup, mitk::DataStorage::SetOfObjects::ConstPointer rsSeg3D, QString filePath)
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

void sv4guiSegmentationLegacyIO::WriteFiles(mitk::DataStorage::SetOfObjects::ConstPointer contoruGroupNodes, mitk::DataStorage::SetOfObjects::ConstPointer seg3DNodes, QString segDir)
{
    QDir dirSeg(segDir);

    for(int i=0;i<contoruGroupNodes->size();i++){
        mitk::DataNode::Pointer node=contoruGroupNodes->GetElement(i);
        QString	filePath=dirSeg.absoluteFilePath(QString::fromStdString(node->GetName()));
        WriteContourGroupFile(node, filePath);
    }

    for(int i=0;i<seg3DNodes->size();i++){
        mitk::DataNode::Pointer node=seg3DNodes->GetElement(i);
        QString	filePath=dirSeg.absoluteFilePath(QString::fromStdString(node->GetName()));
        WriteSeg3DFile(node, filePath);
    }

    QString	filePath=dirSeg.absoluteFilePath("group_contents.tcl");
    WriteTclFile(contoruGroupNodes,seg3DNodes,filePath);
}


