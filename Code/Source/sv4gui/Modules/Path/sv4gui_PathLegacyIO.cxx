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

#include "sv4gui_PathLegacyIO.h"

#include "sv4gui_Path.h"

#include <QString>
#include <QStringList>
#include <QList>
#include <QHash>
#include <QFile>
#include <QTextStream>

//------------------
// ReadDataFromFile
//------------------
// Read path data from a file.
//
static void ReadDataFromFile(QString filePath, QList<int>& IDList, QList<QHash<int,mitk::Point3D>>& dataList,
      QHash<int,QString>& IDNames, QHash<int,int>& IDSplineNum, 
      QHash<int,std::vector<sv4guiPathElement::sv4guiPathPoint>>& dataHash)
    {

    QFile inputFile(filePath);

    if (!inputFile.open(QIODevice::ReadOnly)) {
        return;
    }

    QTextStream in(&inputFile);

    while (!in.atEnd()) {
        QString line = in.readLine();

        if(line.contains("set",Qt::CaseInsensitive) && 
           line.contains("gPathPoints",Qt::CaseInsensitive) && 
           line.contains("name",Qt::CaseInsensitive)) {
            QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            IDNames[list[2].toInt()]=list[4];
        }

        if(line.contains("set",Qt::CaseInsensitive) && 
           line.contains("gPathPoints",Qt::CaseInsensitive) && 
           line.contains("numSplinePts",Qt::CaseInsensitive)) {
            QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            IDSplineNum[list[2].toInt()]=list[4].toInt();
        }

        if(line.contains("set",Qt::CaseInsensitive) && 
           line.contains("gPathPoints",Qt::CaseInsensitive) && 
           !line.contains("name",Qt::CaseInsensitive) && 
           !line.contains("numSplinePts",Qt::CaseInsensitive) && 
           !line.contains("splinePts",Qt::CaseInsensitive) ) {
            QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            int id=list[2].toInt();
            int index=list[3].toInt();
            mitk::Point3D point;
            point[0]=list[4].toDouble();
            point[1]=list[5].toDouble();
            point[2]=list[6].toDouble();

            if(!IDList.contains(id)) {
                IDList<<id;
                QHash<int,mitk::Point3D> data;
                dataList<<data;
            }

            int loc=IDList.lastIndexOf(id);
            dataList[loc][index]=point;

        }

        if(line.contains("set",Qt::CaseInsensitive) && 
           line.contains("gPathPoints",Qt::CaseInsensitive) && 
           line.contains("splinePts",Qt::CaseInsensitive) && 
           line.contains("list",Qt::CaseInsensitive) ) {
            QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            int id=list[2].toInt();

            std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints;

            line = in.readLine();
            int posID=-1;
            while (!line.contains("]")) {
                posID++;
                QStringList listx = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                sv4guiPathElement::sv4guiPathPoint pathPoint;
                pathPoint.id=posID;
                pathPoint.pos[0]=listx[1].toDouble();
                pathPoint.pos[1]=listx[2].toDouble();
                pathPoint.pos[2]=listx[3].toDouble();
                pathPoint.tangent[0]=listx[5].toDouble();
                pathPoint.tangent[1]=listx[6].toDouble();
                pathPoint.tangent[2]=listx[7].toDouble();
                pathPoint.rotation[0]=listx[9].toDouble();
                pathPoint.rotation[1]=listx[10].toDouble();
                pathPoint.rotation[2]=listx[11].toDouble();
                pathPoints.push_back(pathPoint);

                line = in.readLine();
            }

            dataHash[id]=pathPoints;
        }

    }
    inputFile.close();
}

//---------------------
// CreateGroupFromFile
//---------------------
// Create a PathGroup from a file.
//
// This used by the Python API to read in legacy paths.
//
// A legacy path file may contain more than one path so a list
// of PathGroup pointers is returned..
//
std::vector<sv3::PathGroup*> 
sv4guiPathLegacyIO::CreateGroupFromFile(const std::string& filePath)
{
    using sv3::PathGroup;
    using sv3::PathElement;

    QList<int> IDList;
    QList<QHash<int,mitk::Point3D>> dataList;
    QHash<int,QString> IDNames;
    QHash<int,int> IDSplineNum;
    QHash<int,std::vector<sv4guiPathElement::sv4guiPathPoint>> dataHash;

    // Read path data from a file.
    ReadDataFromFile(QString::fromStdString(filePath), IDList, dataList, IDNames, IDSplineNum, dataHash);

    // Create a PathGroup* list from the data.
    //
    std::vector<sv3::PathGroup*> pathGroups;

    for (int i = 0; i < IDList.size(); i++) {
        auto pathGroup = new PathGroup();
        pathGroup->SetPathID(IDList[i]);
        pathGroup->SetName(IDNames[IDList[i]].toStdString());
        pathGroup->SetMethod(sv3::PathElement::CONSTANT_TOTAL_NUMBER);
        pathGroup->SetCalculationNumber(IDSplineNum[IDList[i]]);

        auto pe = new PathElement();
        pe->SetMethod(sv3::PathElement::CONSTANT_TOTAL_NUMBER);
        pe->SetCalculationNumber(IDSplineNum[IDList[i]]);

        std::vector<std::array<double, 3>> controlPoints;
        for(int j = 0; j < dataList[i].size(); j++) {
            auto mitkPt = dataList[i][j];
            controlPoints.push_back({mitkPt[0], mitkPt[1], mitkPt[2]});
        }
        pe->SetControlPoints(controlPoints, false);

        std::vector<PathElement::PathPoint> pathPoints;
        auto points = dataHash[IDList[i]];

        for (auto const& point : points) {
            PathElement::PathPoint pathPoint;
            pathPoint.id = point.id;
            for (int j = 0; j < 3; j++) {
                pathPoint.pos[j] = point.pos[j];
                pathPoint.tangent[j] = point.tangent[j];
                pathPoint.rotation[j] = point.rotation[j];
            }
            pathPoints.push_back(pathPoint);
        }

        pe->SetPathPoints(pathPoints);
        pathGroup->SetPathElement(pe);
        pathGroups.push_back(pathGroup);
    }

    return pathGroups;
}

//----------
// ReadFile
//----------
// Create an MITK Path Data Node from the data in a file.
//
std::vector<mitk::DataNode::Pointer> sv4guiPathLegacyIO::ReadFile(QString filePath)
{
    QList<int> IDList;
    QList<QHash<int,mitk::Point3D>> dataList;
    QHash<int,QString> IDNames;
    QHash<int,int> IDSplineNum;
    QHash<int,std::vector<sv4guiPathElement::sv4guiPathPoint>> dataHash;

    // Read the path data.
    ReadDataFromFile(filePath, IDList, dataList, IDNames, IDSplineNum, dataHash);

    // Create Data Nodes.
    //
    std::vector<mitk::DataNode::Pointer> nodes;

    for (int i=0; i<IDList.size();i++) {
        sv4guiPath::Pointer path = sv4guiPath::New();
        path->SetMethod(sv3::PathElement::CONSTANT_TOTAL_NUMBER);
        path->SetCalculationNumber(IDSplineNum[IDList[i]]);
        path->SetPathID(IDList[i]);

        sv4guiPathElement* pe = new sv4guiPathElement();
        pe->SetMethod(sv3::PathElement::CONSTANT_TOTAL_NUMBER);
        pe->SetCalculationNumber(IDSplineNum[IDList[i]]);

        std::vector<mitk::Point3D> controlPoints;
        for(int j=0;j<dataList[i].size();j++) {
            controlPoints.push_back(dataList[i][j]);
        }
        pe->SetControlPoints(controlPoints,false);
        pe->SetPathPoints(dataHash[IDList[i]]);

        path->SetPathElement(pe);

        mitk::DataNode::Pointer pathNode = mitk::DataNode::New();
        pathNode->SetData(path);
        pathNode->SetName(IDNames[IDList[i]].toStdString());

        nodes.push_back(pathNode);
    }

    return nodes;
}

void sv4guiPathLegacyIO::WriteFile(mitk::DataStorage::SetOfObjects::ConstPointer rs, QString filePath)
{
    if(rs)
    {
        QFile outputFile(filePath);
        if(outputFile.open(QIODevice::WriteOnly | QIODevice::Text))
        {
            QTextStream out(&outputFile);
            out.setRealNumberPrecision(17);

            out<<"#  Geodesic path file version 2.0 (from guiPP)"<<endl;
            out<<"global gPathPoints"<<endl;

            for(int i=0;i<rs->size();i++){

                mitk::DataNode::Pointer node=rs->GetElement(i);
                sv4guiPath* path=dynamic_cast<sv4guiPath*>(node->GetData());

                if(path){

                    out<<"set gPathPoints("<<path->GetPathID()<<",name) {"<<node->GetName().c_str()<<"}"<<endl;

                    sv4guiPathElement* pe=path->GetPathElement();
                    if(pe==NULL) continue;

                    for(int i=0;i<pe->GetControlPointNumber();i++)
                    {
                        mitk::Point3D point=pe->GetControlPoint(i);
                        out<<"set gPathPoints("<<path->GetPathID()<<","<<i<<") { "<<point[0]<<" "<<point[1]<<" "<<point[2]<<"}"<<endl;
                    }

                    out<<"set gPathPoints("<<path->GetPathID()<<",numSplinePts) {"<<pe->GetPathPointNumber()<<"}"<<endl;

                    if(pe->GetPathPointNumber()>0)
                    {
                        out<<"set gPathPoints("<<path->GetPathID()<<",splinePts) [list \\"<<endl;


                        for(int i=0;i<pe->GetPathPointNumber();i++)
                        {
                            sv4guiPathElement::sv4guiPathPoint pathPoint=pe->GetPathPoint(i);
                            out<<"  {p ("<<pathPoint.pos[0]<<","<<pathPoint.pos[1]<<","<<pathPoint.pos[2]<<")"
                              <<" t ("<<pathPoint.tangent[0]<<","<<pathPoint.tangent[1] <<","<<pathPoint.tangent[2]<<")"
                             <<" tx ("<<pathPoint.rotation[0]<<","<<pathPoint.rotation[1]<<","<<pathPoint.rotation[2]<<")} \\"<<endl;

                        }

                        out<<"  ]"<<endl;
                    }
                }
            }

            outputFile.close();
        }

    }

}


