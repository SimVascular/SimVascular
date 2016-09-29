#include "svModelLegacyIO.h"

#include "svModel.h"
#include "svModelElementPolyData.h"

//#include "cv_polydatasolid_utils.h"

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

    if(suffix=="vtp")
    {
        QString filePath2=filePath+".facenames";

        QFile inputFile(filePath2);

        if (inputFile.open(QIODevice::ReadOnly))
        {
            QTextStream in(&inputFile);

            std::vector<svModelElement::svFace*> faces;

            while (!in.atEnd())
            {
                QString line = in.readLine();

                if(line.contains("set gPolyDataFaceNames("))
                {
                    QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
                    svModelElement::svFace* face=new svModelElement::svFace;
                    face->id=list[2].toInt();
                    face->name=list[3].toStdString();
                    faces.push_back(face);
                }
            }
            inputFile.close();

            vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
            reader->SetFileName(filePath.toStdString().c_str());
            reader->Update();
            vtkPolyData* pd=reader->GetOutput();
            if(pd!=NULL)
            {
                vtkPolyData* vpdModel=vtkPolyData::New();
                vpdModel->DeepCopy(pd);
                svModelElementPolyData* mepd=new svModelElementPolyData();
                mepd->SetSolidModel(vpdModel);

                for(int i=0;i<faces.size();i++)
                {
//                    vtkPolyData *facepd = vtkPolyData::New();
//                    int id=faces[i]->id;
//                    PlyDtaUtils_GetFacePolyData(vpdModel, &id, facepd);
                    vtkPolyData *facepd=mepd->CreateFaceVtkPolyData(faces[i]->id);
                    faces[i]->vpd=facepd;
                }

                mepd->SetFaces(faces);

                svModel::Pointer model=svModel::New();
                model->SetModelElement(mepd);

                modelNode = mitk::DataNode::New();
                modelNode->SetData(model);
                modelNode->SetName(baseName.toStdString());

            }

        }
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
    QString suffix;

    if(type=="PolyData")
    {
        svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>modelElement;
        if(!mepd) return;

        QString filePath2=filePath+".facenames";

        QFile outputFile(filePath2);
        if(outputFile.open(QIODevice::WriteOnly | QIODevice::Text))
        {
            QTextStream out(&outputFile);
            //out.setRealNumberPrecision(17);

            QFileInfo fileInfo(filePath);
            QString fileName = fileInfo.fileName();

            out<<"global gPolyDataFaceNames"<<endl;
            out<<"global gPolyDataFaceNamesInfo"<<endl;

            out<<"set gPolyDataFaceNamesInfo(timestamp) {1500000000}"<<endl;
            //out<<"set gPolyDataFaceNamesInfo(model_file_md5) {FCCF69239480A681C5579A153F2D552A}"<<endl;
            out<<"set gPolyDataFaceNamesInfo(model_file_name) {"<<fileName<<"}"<<endl;


                std::vector<svModelElement::svFace*> faces=mepd->GetFaces();
                for(int i=0;i<faces.size();i++){
                    svModelElement::svFace* face=faces[i];
                    if(face)
                    {
                        out<<"set gPolyDataFaceNames("<<face->id<<") {"<<QString::fromStdString(face->name)<<"}"<<endl;
                    }
                }

                if(mepd->GetSolidModel())
                {
                    vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
                    writer->SetFileName(filePath.toStdString().c_str());
                    writer->SetInputData(mepd->GetSolidModel());
                    if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
                    {
                        mitkThrow() << "vtkXMLPolyDataWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode());
                    }
                }



            outputFile.close();

        }
    }
    else if(type=="ParaSolid")
    {

    }
    else if(type=="OpenCASCADE")
    {

    }


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
            suffix="occt";

        QString	filePath=dirModel.absoluteFilePath(QString::fromStdString(node->GetName())+"."+suffix);
        WriteFile(node, filePath);
    }

}
