#include "svProjectManager.h"

#include "svProjectFolder.h"
#include "svImageFolder.h"
#include "svPathFolder.h"
#include "svSegmentationFolder.h"
#include "svModelFolder.h"
#include "svMeshFolder.h"
#include "svSimulationFolder.h"

#include "svPath.h"
#include "svContourGroup.h"
#include "svModel.h"
#include "svMitkMesh.h"
#include "svMitkSimJob.h"
#include "svMitkMeshIO.h"
#include "svVtkUtils.h"

#include <mitkNodePredicateDataType.h>
#include <mitkIOUtil.h>
#include <mitkRenderingManager.h>
#include <mitkCoreServices.h>
#include <mitkIMimeTypeProvider.h>
#include <mitkMimeType.h>
#include <mitkCustomMimeType.h>

#include <vtkImageData.h>
#include <vtkXMLImageDataWriter.h>

#include <QDir>
#include <QDomDocument>
#include <QDomElement>
#include <QFile>
#include <QTextStream>

template <typename TDataFolder>
mitk::DataNode::Pointer svProjectManager::CreateDataFolder(mitk::DataStorage::Pointer dataStorage, QString folderName, mitk::DataNode::Pointer projFolderNode)
{
    mitk::NodePredicateDataType::Pointer isDataFolder = mitk::NodePredicateDataType::New(TDataFolder::GetStaticNameOfClass());

    mitk::DataStorage::SetOfObjects::ConstPointer rs;
    if(projFolderNode.IsNull())
    {
        rs=dataStorage->GetSubset(isDataFolder);
    }else
    {
        rs=dataStorage->GetDerivations (projFolderNode,isDataFolder);
    }

    bool exists=false;
    mitk::DataNode::Pointer dataFolderNode=NULL;
    std::string fdName=folderName.toStdString();

    for(int i=0;i<rs->size();i++)
    {
        if(rs->GetElement(i)->GetName()==fdName)
        {
            exists=true;
            dataFolderNode=rs->GetElement(i);
            break;
        }
    }

    if(!exists)
    {
        dataFolderNode=mitk::DataNode::New();
        dataFolderNode->SetName(fdName);
        dataFolderNode->SetVisibility(true);
        typename TDataFolder::Pointer dataFolder=TDataFolder::New();
        dataFolderNode->SetData(dataFolder);
        if(projFolderNode.IsNull())
        {
            dataStorage->Add(dataFolderNode);
        }else
        {
            dataStorage->Add(dataFolderNode,projFolderNode);
        }

    }

    return dataFolderNode;
}

void svProjectManager::AddProject(mitk::DataStorage::Pointer dataStorage, QString projName, QString projParentDir,bool newProject)
{
    QString projectConfigFileName=".svproj";
    QString imageFolderName="Images";
    QString pathFolderName="Paths";
    QString segFolderName="Segmentations";
    QString modelFolderName="Models";
    QString meshFolderName="Meshes";
    QString simFolderName="Simulations";

    QDir dir(projParentDir);
    if(newProject)
    {
        dir.mkdir(projName);
    }
    dir.cd(projName);

    QString projPath=dir.absolutePath();
    QString projectConfigFilePath=dir.absoluteFilePath(projectConfigFileName);

    QStringList imageFilePathList;
    if(newProject)
    {
        WriteEmptyConfigFile(projectConfigFilePath);
        dir.mkdir(imageFolderName);
        dir.mkdir(pathFolderName);
        dir.mkdir(segFolderName);
        dir.mkdir(modelFolderName);
        dir.mkdir(meshFolderName);
        dir.mkdir(simFolderName);
    }else{

        QFile xmlFile(projectConfigFilePath);
        xmlFile.open(QIODevice::ReadOnly);
        QDomDocument doc("svproj");
        QString *em=NULL;
        doc.setContent(&xmlFile,em);
        xmlFile.close();

        QDomElement projDesc = doc.firstChildElement("projectDescription");
        QDomElement imagesElement=projDesc.firstChildElement("images");
        imageFolderName=imagesElement.attribute("folder_name");
        QDomNodeList imageList=imagesElement.elementsByTagName("image");
        for(int i=0;i<imageList.size();i++)
        {
            QDomNode imageNode=imageList.item(i);
            if(imageNode.isNull()) continue;

            QDomElement imageElement=imageNode.toElement();
            if(imageElement.isNull()) continue;

            QString inProj=imageElement.attribute("in_project");

            if(inProj=="yes")
                imageFilePathList<<(projPath+"/"+imageFolderName+"/"+imageElement.attribute("name"));
            else
                imageFilePathList<<imageElement.attribute("path");
        }

        pathFolderName=projDesc.firstChildElement("paths").attribute("folder_name");
        segFolderName=projDesc.firstChildElement("segmentations").attribute("folder_name");
        modelFolderName=projDesc.firstChildElement("models").attribute("folder_name");
        meshFolderName=projDesc.firstChildElement("meshes").attribute("folder_name");
        simFolderName=projDesc.firstChildElement("simulations").attribute("folder_name");

    }

    mitk::DataNode::Pointer projectFolderNode= CreateDataFolder<svProjectFolder>(dataStorage,projName);
    projectFolderNode->AddProperty( "project path",mitk::StringProperty::New(projPath.toStdString().c_str()));

    mitk::DataNode::Pointer imageFolderNode=CreateDataFolder<svImageFolder>(dataStorage, imageFolderName, projectFolderNode);
    mitk::DataNode::Pointer pathFolderNode=CreateDataFolder<svPathFolder>(dataStorage, pathFolderName, projectFolderNode);
    mitk::DataNode::Pointer segFolderNode=CreateDataFolder<svSegmentationFolder>(dataStorage,segFolderName, projectFolderNode);
    mitk::DataNode::Pointer modelFolderNode=CreateDataFolder<svModelFolder>(dataStorage, modelFolderName, projectFolderNode);
    mitk::DataNode::Pointer meshFolderNode=CreateDataFolder<svMeshFolder>(dataStorage, meshFolderName, projectFolderNode);
    mitk::DataNode::Pointer simFolderNode=CreateDataFolder<svSimulationFolder>(dataStorage, simFolderName, projectFolderNode);

    imageFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    pathFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    segFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    modelFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    meshFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    simFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );

    if(!newProject)
    {
        imageFolderNode->SetVisibility(false);
        for(int i=0;i<imageFilePathList.size();i++)
        {
            mitk::DataNode::Pointer imageNode=mitk::IOUtil::LoadDataNode(imageFilePathList[i].toStdString());
//            imageNode->SetVisibility(false);
            dataStorage->Add(imageNode,imageFolderNode);
        }

        pathFolderNode->SetVisibility(false);
        QDir dir1(projPath);
        dir1.cd(pathFolderName);
        QFileInfoList fileInfoList=dir1.entryInfoList(QStringList("*.pth"), QDir::Files, QDir::Name);
        for(int i=0;i<fileInfoList.size();i++)
        {
            mitk::DataNode::Pointer pathNode=mitk::IOUtil::LoadDataNode(fileInfoList[i].absoluteFilePath().toStdString());
            pathNode->SetVisibility(false);
            dataStorage->Add(pathNode,pathFolderNode);
        }

        segFolderNode->SetVisibility(false);
        QDir dirSeg(projPath);
        dirSeg.cd(segFolderName);
        fileInfoList=dirSeg.entryInfoList(QStringList("*.ctgr"), QDir::Files, QDir::Name);
        for(int i=0;i<fileInfoList.size();i++)
        {
            mitk::DataNode::Pointer contourGroupNode=mitk::IOUtil::LoadDataNode(fileInfoList[i].absoluteFilePath().toStdString());
            contourGroupNode->SetVisibility(false);
            dataStorage->Add(contourGroupNode,segFolderNode);
        }

        modelFolderNode->SetVisibility(false);
        QDir dirModel(projPath);
        dirModel.cd(modelFolderName);
        fileInfoList=dirModel.entryInfoList(QStringList("*.mdl"), QDir::Files, QDir::Name);
        for(int i=0;i<fileInfoList.size();i++)
        {
            mitk::DataNode::Pointer modelNode=mitk::IOUtil::LoadDataNode(fileInfoList[i].absoluteFilePath().toStdString());
            modelNode->SetVisibility(false);
            dataStorage->Add(modelNode,modelFolderNode);
        }

        meshFolderNode->SetVisibility(false);
        QDir dirMesh(projPath);
        dirMesh.cd(meshFolderName);
        fileInfoList=dirMesh.entryInfoList(QStringList("*.msh"), QDir::Files, QDir::Name);
        for(int i=0;i<fileInfoList.size();i++)
        {
            mitk::DataNode::Pointer meshNode=mitk::IOUtil::LoadDataNode(fileInfoList[i].absoluteFilePath().toStdString());
            meshNode->SetVisibility(false);
            dataStorage->Add(meshNode,meshFolderNode);
        }

        simFolderNode->SetVisibility(false);
        QDir dirSim(projPath);
        dirSim.cd(simFolderName);
        fileInfoList=dirSim.entryInfoList(QStringList("*.sjb"), QDir::Files, QDir::Name);
        for(int i=0;i<fileInfoList.size();i++)
        {
            mitk::DataNode::Pointer jobNode=mitk::IOUtil::LoadDataNode(fileInfoList[i].absoluteFilePath().toStdString());
            jobNode->SetVisibility(false);
            dataStorage->Add(jobNode,simFolderNode);
        }
    }

    mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(dataStorage);

}

void svProjectManager::WriteEmptyConfigFile(QString projConfigFilePath)
{
    QDomDocument doc;
    //    doc.createProcessingInstruction("xml", "version=\"1.0\" encoding=\"UTF-8\"");

    QDomNode xmlNode = doc.createProcessingInstruction("xml", "version=\"1.0\" encoding=\"UTF-8\"");
    doc.appendChild(xmlNode);

    QDomElement root = doc.createElement("projectDescription");
    root.setAttribute("version", "1.0");
    doc.appendChild(root);

    QDomElement tag = doc.createElement("images");
    tag.setAttribute("folder_name","Images");
    root.appendChild(tag);

    tag = doc.createElement("paths");
    tag.setAttribute("folder_name","Paths");
    root.appendChild(tag);

    tag = doc.createElement("segmentations");
    tag.setAttribute("folder_name","Segmentations");
    root.appendChild(tag);

    tag = doc.createElement("models");
    tag.setAttribute("folder_name","Models");
    root.appendChild(tag);

    tag = doc.createElement("meshes");
    tag.setAttribute("folder_name","Meshes");
    root.appendChild(tag);

    tag = doc.createElement("simulations");
    tag.setAttribute("folder_name","Simulations");
    root.appendChild(tag);

    QString xml = doc.toString(4);

    QFile file( projConfigFilePath );

    if (file.open(QFile::WriteOnly | QFile::Truncate)) {
        QTextStream out(&file);
        out << xml <<endl;
    }


}

// so far, no copy into project
void svProjectManager::AddImage(mitk::DataStorage::Pointer dataStorage, QString imageFilePath, mitk::DataNode::Pointer imageFolderNode, bool copyIntoProject)
{
    mitk::DataNode::Pointer imageNode=mitk::IOUtil::LoadDataNode(imageFilePath.toStdString());

    mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(dataStorage);

    //add image to config

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources (imageFolderNode,isProjFolder);

    std::string projPath;
    mitk::DataNode::Pointer projectFolderNode=rs->GetElement(0);

    projectFolderNode->GetStringProperty("project path",projPath);

    QDir projDir(QString::fromStdString(projPath));
    QString	configFilePath=projDir.absoluteFilePath(".svproj");

    QFile xmlFile(configFilePath);
    xmlFile.open(QIODevice::ReadOnly);
    QDomDocument doc("svproj");
    QString *em=NULL;
    doc.setContent(&xmlFile,em);
    xmlFile.close();

    QDomElement projDesc = doc.firstChildElement("projectDescription");
    QDomElement imagesElement=projDesc.firstChildElement("images");

    QDomElement previousImgElement=imagesElement.firstChildElement("image");
    if(!previousImgElement.isNull())
    {
        QString inProj=previousImgElement.attribute("in_project");

        if(inProj=="yes")
        {
            QString piPath=QString::fromStdString(projPath+"/"+imageFolderNode->GetName()+"/"+previousImgElement.attribute("name").toStdString());
            QFile piFile(piPath);
            piFile.remove();
        }

        mitk::DataStorage::SetOfObjects::ConstPointer nodesToRemove=dataStorage->GetDerivations(imageFolderNode,nullptr,false);
        if( !nodesToRemove->empty())
        {
            dataStorage->Remove(nodesToRemove);
        }

        imagesElement.removeChild(previousImgElement);
    }

    QDomElement imgElement = doc.createElement("image");
    if(copyIntoProject)
    {
        imgElement.setAttribute("in_project","yes");
        QString imageName=QString::fromStdString(imageNode->GetName());
        if(imageName.size()>15)
            imageName=imageName.mid(0,15);

        imageName=imageName+".vti";
        imgElement.setAttribute("name",imageName);
        std::string ipath=projPath+"/"+imageFolderNode->GetName();
        std::string ifilePath=ipath+"/"+imageName.toStdString();
        imageNode->SetStringProperty("path",ipath.c_str());
        mitk::Image* image=dynamic_cast<mitk::Image*>(imageNode->GetData());
        if(image)
        {
            vtkImageData* vtkImg=svVtkUtils::MitkImage2VtkImage(image);
            if(vtkImg)
            {
                vtkSmartPointer<vtkXMLImageDataWriter> writer = vtkSmartPointer<vtkXMLImageDataWriter>::New();
                writer->SetFileName(ifilePath.c_str());
                writer->SetInputData(vtkImg);
                writer->Write();
            }
        }

//        mitk::IOUtil::Save(imageNode->GetData(),ifilePath);
    }
    else
    {
        imgElement.setAttribute("in_project","no");
        imgElement.setAttribute("path",imageFilePath);
    }

    dataStorage->Add(imageNode,imageFolderNode);

    imagesElement.appendChild(imgElement);

    QString xml = doc.toString(4);

    QFile file( configFilePath );

    if (file.open(QFile::WriteOnly | QFile::Truncate)) {
        QTextStream out(&file);
        out << xml <<endl;
    }

}

void svProjectManager::SaveProject(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode)
{
    std::string projPath;
    projFolderNode->GetStringProperty("project path",projPath);

    //save paths
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svPathFolder"));

    mitk::DataNode::Pointer pathFolderNode=rs->GetElement(0);
    std::string pathFolderName=pathFolderNode->GetName();

    rs=dataStorage->GetDerivations(pathFolderNode,mitk::NodePredicateDataType::New("svPath"));

    QDir dir(QString::fromStdString(projPath));
    dir.cd(QString::fromStdString(pathFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);
        svPath *path=dynamic_cast<svPath*>(node->GetData());
        if(path==NULL || !path->IsDataModified())
            continue;

        QString	filePath=dir.absoluteFilePath(QString::fromStdString(node->GetName())+".pth");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        path->SetDataModified(false);
    }

    //save contour groups
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svSegmentationFolder"));

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);
    std::string segFolderName=segFolderNode->GetName();

    rs=dataStorage->GetDerivations(segFolderNode,mitk::NodePredicateDataType::New("svContourGroup"));

    QDir dirSeg(QString::fromStdString(projPath));
    dirSeg.cd(QString::fromStdString(segFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);
        svContourGroup *contourGroup=dynamic_cast<svContourGroup*>(node->GetData());
        if(contourGroup==NULL || !contourGroup->IsDataModified())
            continue;

        QString	filePath=dirSeg.absoluteFilePath(QString::fromStdString(node->GetName())+".ctgr");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        contourGroup->SetDataModified(false);
    }

    //save models
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svModelFolder"));

    mitk::DataNode::Pointer modelFolderNode=rs->GetElement(0);
    std::string modelFolderName=modelFolderNode->GetName();

    rs=dataStorage->GetDerivations(modelFolderNode,mitk::NodePredicateDataType::New("svModel"));

    QDir dirModel(QString::fromStdString(projPath));
    dirModel.cd(QString::fromStdString(modelFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);
        svModel *model=dynamic_cast<svModel*>(node->GetData());
        if(model==NULL || !model->IsDataModified())
            continue;

        QString	filePath=dirModel.absoluteFilePath(QString::fromStdString(node->GetName())+".mdl");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        model->SetDataModified(false);
    }

    //save mesh
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svMeshFolder"));

    mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);
    std::string meshFolderName=meshFolderNode->GetName();

    rs=dataStorage->GetDerivations(meshFolderNode,mitk::NodePredicateDataType::New("svMitkMesh"));

    QDir dirMesh(QString::fromStdString(projPath));
    dirMesh.cd(QString::fromStdString(meshFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);

        svMitkMesh *mitkMesh=dynamic_cast<svMitkMesh*>(node->GetData());
        if(mitkMesh==NULL || !mitkMesh->IsDataModified())
            continue;

        QString	filePath=dirMesh.absoluteFilePath(QString::fromStdString(node->GetName())+".msh");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        mitkMesh->SetDataModified(false);
    }

    //sava simjobs
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svSimulationFolder"));

    mitk::DataNode::Pointer simFolderNode=rs->GetElement(0);
    std::string simFolderName=simFolderNode->GetName();

    rs=dataStorage->GetDerivations(simFolderNode,mitk::NodePredicateDataType::New("svMitkSimJob"));

    QDir dirSim(QString::fromStdString(projPath));
    dirSim.cd(QString::fromStdString(simFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);
        svMitkSimJob *mitkJob=dynamic_cast<svMitkSimJob*>(node->GetData());
        if(mitkJob==NULL || !mitkJob->IsDataModified())
            continue;

        QString	filePath=dirSim.absoluteFilePath(QString::fromStdString(node->GetName())+".sjb");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        mitkJob->SetDataModified(false);
    }
}

void svProjectManager::SaveAllProjects(mitk::DataStorage::Pointer dataStorage)
{
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSubset(mitk::NodePredicateDataType::New("svProjectFolder"));
    for(int i=0;i<rs->size();i++)
    {
        SaveProject(dataStorage, rs->GetElement(i));
    }
}

void svProjectManager::LoadData(mitk::DataNode::Pointer dataNode)
{
    std::string path="";
    dataNode->GetStringProperty("path",path);
    if(path=="")
        return;

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");
    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("svContourGroup");
    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");
    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("svMitkMesh");
    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("svMitkSimJob");
    std::string extension="";
    if(isPath->CheckNode(dataNode))
    {
        extension="pth";
    }
    else if(isContourGroup->CheckNode(dataNode))
    {
        extension="ctgr";
    }
    else if(isModel->CheckNode(dataNode))
    {
        extension="mdl";
    }
    else if(isMesh->CheckNode(dataNode))
    {
        extension="msh";
    }
    else if(isSimJob->CheckNode(dataNode))
    {
        extension="sjb";
    }

    std::vector<mitk::BaseData::Pointer> vdata = mitk::IOUtil::Load(path+"/"+dataNode->GetName()+"."+extension);
    if(vdata.size()>0)
    {
        dataNode->SetData(vdata[0]);
    }
}

mitk::DataNode::Pointer svProjectManager::GetProjectFolderNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode)
{
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources (dataNode,isProjFolder,false);

    mitk::DataNode::Pointer projFolderNode=NULL;
    if(rs->size()>0)
        projFolderNode=rs->GetElement(0);

    return projFolderNode;
}
