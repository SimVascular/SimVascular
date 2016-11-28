#include "svPathLegacyIO.h"

#include "svPath.h"

#include <QString>
#include <QStringList>
#include <QList>
#include <QHash>
#include <QFile>
#include <QTextStream>

std::vector<mitk::DataNode::Pointer> svPathLegacyIO::ReadFile(QString filePath)
{
    std::vector<mitk::DataNode::Pointer> nodes;

    QList<int> IDList;
    QList<QHash<int,mitk::Point3D>> dataList;
    QHash<int,QString> IDNames;
    QHash<int,int> IDSplineNum;

    QHash<int,std::vector<svPathElement::svPathPoint>> dataHash;

    QFile inputFile(filePath);
    if (inputFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&inputFile);
        while (!in.atEnd())
        {
//            QString line = in.readLine().toLower();
            QString line = in.readLine();

            if(line.contains("set",Qt::CaseInsensitive) && line.contains("gPathPoints",Qt::CaseInsensitive) && line.contains("name",Qt::CaseInsensitive))
            {
                QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                IDNames[list[2].toInt()]=list[4];
            }

            if(line.contains("set",Qt::CaseInsensitive) && line.contains("gPathPoints",Qt::CaseInsensitive) && line.contains("numSplinePts",Qt::CaseInsensitive))
            {
                QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                IDSplineNum[list[2].toInt()]=list[4].toInt();
            }

            if(line.contains("set",Qt::CaseInsensitive) && line.contains("gPathPoints",Qt::CaseInsensitive) && !line.contains("name",Qt::CaseInsensitive)
                    && !line.contains("numSplinePts",Qt::CaseInsensitive) && !line.contains("splinePts",Qt::CaseInsensitive) )
            {
                QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                int id=list[2].toInt();
                int index=list[3].toInt();
                mitk::Point3D point;
                point[0]=list[4].toDouble();
                point[1]=list[5].toDouble();
                point[2]=list[6].toDouble();

                if(!IDList.contains(id))
                {
                    IDList<<id;
                    QHash<int,mitk::Point3D> data;
                    dataList<<data;
                }

                int loc=IDList.lastIndexOf(id);
                dataList[loc][index]=point;

            }

            if(line.contains("set",Qt::CaseInsensitive) && line.contains("gPathPoints",Qt::CaseInsensitive) && line.contains("splinePts",Qt::CaseInsensitive)
                    && line.contains("list",Qt::CaseInsensitive) )
            {
                QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                int id=list[2].toInt();

                std::vector<svPathElement::svPathPoint> pathPoints;

                line = in.readLine();
                int posID=-1;
                while (!line.contains("]"))
                {
                    posID++;
                    QStringList listx = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                    svPathElement::svPathPoint pathPoint;
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

        for(int i=0; i<IDList.size();i++)
        {
            svPath::Pointer path = svPath::New();
            path->SetMethod(svPathElement::CONSTANT_TOTAL_NUMBER);
            path->SetCalculationNumber(IDSplineNum[IDList[i]]);
            path->SetPathID(IDList[i]);

            svPathElement* pe = new svPathElement();
            pe->SetMethod(svPathElement::CONSTANT_TOTAL_NUMBER);
            pe->SetCalculationNumber(IDSplineNum[IDList[i]]);

            std::vector<mitk::Point3D> controlPoints;
            for(int j=0;j<dataList[i].size();j++)
            {
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
    }

    return nodes;
}

void svPathLegacyIO::WriteFile(mitk::DataStorage::SetOfObjects::ConstPointer rs, QString filePath)
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
                svPath* path=dynamic_cast<svPath*>(node->GetData());

                if(path){

                    out<<"set gPathPoints("<<path->GetPathID()<<",name) {"<<node->GetName().c_str()<<"}"<<endl;

                    svPathElement* pe=path->GetPathElement();
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
                            svPathElement::svPathPoint pathPoint=pe->GetPathPoint(i);
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


