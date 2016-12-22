#include "svModelLegacyIO.h"
#include "simvascular_options.h"

#include "svModel.h"
#include "svModelElementPolyData.h"

#ifdef SV_USE_OpenCASCADE
#include "svModelElementOCCT.h"
#endif

#include <QString>
#include <QStringList>
#include <QList>
#include <QFile>
#include <QFileInfo>
#include <QTextStream>
#include <QDir>

#include <vtkXMLPolyDataReader.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkErrorCode.h>

mitk::DataNode::Pointer svModelLegacyIO::ReadFile(QString filePath)
{
    mitk::DataNode::Pointer modelNode=NULL;

    QFileInfo fileInfo(filePath);
    QString baseName = fileInfo.baseName();
    QString suffix=fileInfo.suffix();
    QString filePath2=filePath+".facenames";

    QString handler="";
    if(suffix=="vtp")
        handler="set gPolyDataFaceNames(";
    else if(suffix=="brep" || suffix=="step" || suffix=="stl" || suffix=="iges")
        handler="set gOCCTFaceNames(";
    else
        return modelNode;


    QFile inputFile(filePath2);
    if (inputFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&inputFile);

        std::vector<svModelElement::svFace*> faces;

        while (!in.atEnd())
        {
            QString line = in.readLine();

            if(line.contains(handler))
            {
                QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                svModelElement::svFace* face=new svModelElement::svFace;
                face->id=list[2].toInt();
                face->name=list[3].toStdString();
                faces.push_back(face);
                if(face->name.substr(0,4)=="wall")
                    face->type="wall";
                else if(face->name.substr(0,3)=="cap")
                    face->type="cap";

            }
        }
        inputFile.close();

        if(suffix=="vtp")
        {
            vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
            reader->SetFileName(filePath.toStdString().c_str());
            reader->Update();
            vtkSmartPointer<vtkPolyData> pd=reader->GetOutput();
            if(pd!=NULL)
            {
                svModelElementPolyData* mepd=new svModelElementPolyData();
                mepd->SetWholeVtkPolyData(pd);

                for(int i=0;i<faces.size();i++)
                {
                    vtkSmartPointer<vtkPolyData> facepd=mepd->CreateFaceVtkPolyData(faces[i]->id);
                    faces[i]->vpd=facepd;
                }

                mepd->SetFaces(faces);

                svModel::Pointer model=svModel::New();
                model->SetType(mepd->GetType());
                model->SetModelElement(mepd);

                modelNode = mitk::DataNode::New();
                modelNode->SetData(model);
                modelNode->SetName(baseName.toStdString());
            }
        }
#ifdef SV_USE_OpenCASCADE
        else if(suffix=="brep" || suffix=="step" || suffix=="stl" || suffix=="iges")
        {
            cvOCCTSolidModel* occtSolid=new cvOCCTSolidModel();
            char* fpath=const_cast<char*>(filePath.toStdString().c_str());
            occtSolid->ReadNative(fpath);
            if(occtSolid)
            {
                svModelElementOCCT* meocct=new svModelElementOCCT();
                meocct->SetOCCTSolid(occtSolid);
                meocct->SetWholeVtkPolyData(meocct->CreateWholeVtkPolyData());

                for(int i=0;i<faces.size();i++)
                {
                    vtkSmartPointer<vtkPolyData> facepd=meocct->CreateFaceVtkPolyData(faces[i]->id);
                    faces[i]->vpd=facepd;
                }

                meocct->SetFaces(faces);

                svModel::Pointer model=svModel::New();
                model->SetType(meocct->GetType());
                model->SetModelElement(meocct);

                modelNode = mitk::DataNode::New();
                modelNode->SetData(model);
                modelNode->SetName(baseName.toStdString());
            }
        }
#endif

    }

    return modelNode;
}

std::vector<mitk::DataNode::Pointer> svModelLegacyIO::ReadFiles(QString modelDir)
{
    QDir dirModel(modelDir);
    QFileInfoList fileInfoList=dirModel.entryInfoList(QStringList("*"), QDir::Files, QDir::Name);
    std::vector<mitk::DataNode::Pointer> nodes;
    for(int i=0;i<fileInfoList.size();i++)
    {
        QString filePath=fileInfoList[i].absoluteFilePath();
        if(filePath.endsWith(".vtp"))
        {
            mitk::DataNode::Pointer node=ReadFile(filePath);
            if(node.IsNotNull())
                nodes.push_back(node);
        }
    }
    return nodes;
}

void svModelLegacyIO::WriteFile(mitk::DataNode::Pointer node, QString filePath)
{
    if(!node) return;

    svModel* model=dynamic_cast<svModel*>(node->GetData());
    if(!model) return;

    svModelElement* modelElement=model->GetModelElement();
    if(!modelElement) return;

    std::string type=modelElement->GetType();
    QString filePath2=filePath+".facenames";


    QString handler="";
    if(type=="PolyData")
        handler="gPolyDataFaceNames";
    else if(type=="OpenCASCADE")
        handler="gOCCTFaceNames";
    else
    {
        mitkThrow() << "Model type not support ";
        return;
    }

    QFile outputFile(filePath2);
    if(outputFile.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        QTextStream out(&outputFile);
        //out.setRealNumberPrecision(17);

        QFileInfo fileInfo(filePath);
        QString fileName = fileInfo.fileName();

        out<<"global "<<handler<<endl;
        out<<"global "<<handler<<"Info"<<endl;

        out<<"set "<<handler<<"Info(timestamp) {1500000000}"<<endl;
        //out<<"set "<<handler<<"Info(model_file_md5) {FCCF69239480A681C5579A153F2D552A}"<<endl;
        out<<"set "<<handler<<"Info(model_file_name) {"<<fileName<<"}"<<endl;

        std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();
        for(int i=0;i<faces.size();i++){
            svModelElement::svFace* face=faces[i];
            if(face)
            {
                out<<"set gPolyDataFaceNames("<<face->id<<") {"<<QString::fromStdString(face->name)<<"}"<<endl;
            }
        }

        outputFile.close();
    }
    else
    {
        return;
    }

    if(type=="PolyData")
    {
        svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(modelElement);
        if(!mepd) return;

        if(mepd->GetWholeVtkPolyData())
        {
            vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
            writer->SetFileName(filePath.toStdString().c_str());
            writer->SetInputData(mepd->GetWholeVtkPolyData());
            if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
            {
                mitkThrow() << "vtkXMLPolyDataWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode());
            }
        }
    }
#ifdef SV_USE_OpenCASCADE
    else if(type=="OpenCASCADE")
    {
        svModelElementOCCT* meocct=dynamic_cast<svModelElementOCCT*>(modelElement);
        if(!meocct) return;

        if(meocct->GetOCCTSolid())
        {
            char* fpath=const_cast<char*>(filePath.toStdString().c_str());
            if (meocct->GetOCCTSolid()->WriteNative(0,fpath) != CV_OK )
             {
                 mitkThrow() << "OpenCASCADE model writing error: ";
             }
        }
    }
#endif

}

void svModelLegacyIO::WriteFiles(mitk::DataStorage::SetOfObjects::ConstPointer modelNodes, QString modelDir)
{
    QDir dirModel(modelDir);

    for(int i=0;i<modelNodes->size();i++){
        mitk::DataNode::Pointer node=modelNodes->GetElement(i);

        if(!node) continue;

        svModel* model=dynamic_cast<svModel*>(node->GetData());
        if(!model) continue;

        svModelElement* modelElement=model->GetModelElement();
        if(!modelElement) continue;

        std::string type=modelElement->GetType();
        QString suffix;

        if(type=="PolyData")
            suffix="vtp";
        else if(type=="ParaSolid")
            suffix="xmt_txt";
        else if(type=="OpenCASCADE")
            suffix="brep";

        QString	filePath=dirModel.absoluteFilePath(QString::fromStdString(node->GetName())+"."+suffix);
        WriteFile(node, filePath);
    }

}
