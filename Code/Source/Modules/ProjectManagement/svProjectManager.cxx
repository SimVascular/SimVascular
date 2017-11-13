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
#include "svMitkSeg3D.h"
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
#include <mitkNodePredicateOr.h>

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
    QStringList imageNameList;

    bool rewriteProjectConfigFile=false;
    QDomDocument doc("svproj");

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
//        QDomDocument doc("svproj");
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
            QString imageName=imageElement.attribute("name");
            QString imagePath=imageElement.attribute("path");

            if(inProj=="yes")
            {
                if(imagePath!="")
                {
                    imageFilePathList<<(projPath+"/"+imageFolderName+"/"+imagePath);
                }
                else if(imageName!="")
                {
                    imageFilePathList<<(projPath+"/"+imageFolderName+"/"+imageName);

                    imageElement.setAttribute("path",imageName);
                    imageElement.setAttribute("name",imageName.remove(".vti"));
                    rewriteProjectConfigFile=true;
                }
                 imageNameList<<imageName;
            }
            else
            {
                imageFilePathList<<imageElement.attribute("path");

                if(imageName=="")
                {
                    imageName="image";
                    imageElement.setAttribute("name",imageName);
                    rewriteProjectConfigFile=true;
                }
                imageNameList<<imageName;

            }
        }

        pathFolderName=projDesc.firstChildElement("paths").attribute("folder_name");
        segFolderName=projDesc.firstChildElement("segmentations").attribute("folder_name");
        modelFolderName=projDesc.firstChildElement("models").attribute("folder_name");
        meshFolderName=projDesc.firstChildElement("meshes").attribute("folder_name");
        simFolderName=projDesc.firstChildElement("simulations").attribute("folder_name");

    }

    if(rewriteProjectConfigFile)
    {
        QString xml = doc.toString(4);

        QFile file( projectConfigFilePath );

        if (file.open(QFile::WriteOnly | QFile::Truncate)) {
            QTextStream out(&file);
            out << xml <<endl;
        }
    }


    mitk::DataNode::Pointer projectFolderNode= CreateDataFolder<svProjectFolder>(dataStorage,projName);
    projectFolderNode->AddProperty("project path",mitk::StringProperty::New(projPath.toStdString().c_str()));

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
            std::string imageFilePath=imageFilePathList[i].toStdString();

            try{
                mitk::DataNode::Pointer imageNode=mitk::IOUtil::LoadDataNode(imageFilePath);
                //            imageNode->SetVisibility(false);
                imageNode->SetName(imageNameList[i].toStdString());
                dataStorage->Add(imageNode,imageFolderNode);
            }
            catch(...)
            {
                MITK_ERROR << "Failed to load image (maybe non-existing or unsupported data type): " << imageFilePath;
            }
        }

        pathFolderNode->SetVisibility(false);
        QDir dir1(projPath);
        dir1.cd(pathFolderName);
        QFileInfoList fileInfoList=dir1.entryInfoList(QStringList("*.pth"), QDir::Files, QDir::Name);
        for(int i=0;i<fileInfoList.size();i++)
        {
            mitk::DataNode::Pointer pathNode=mitk::IOUtil::LoadDataNode(fileInfoList[i].absoluteFilePath().toStdString());
            pathNode->SetVisibility(false);

            svPath* path=dynamic_cast<svPath*>(pathNode->GetData());
            if(path)
            {
                auto props=path->GetProps();
                auto it = props.begin();
                while(it != props.end())
                {
                    if(it->first=="point 2D display size"
                    || it->first=="point size")
                    {
                        if(it->second!="")
                        {
                            float value=(float)(std::stod(it->second));
                            pathNode->SetFloatProperty(it->first.c_str(),value);
                        }
                    }
                    it++;
                }
            }

            dataStorage->Add(pathNode,pathFolderNode);
        }

        segFolderNode->SetVisibility(false);
        QDir dirSeg(projPath);
        dirSeg.cd(segFolderName);
        QStringList segNameFilters;
        segNameFilters<<"*.ctgr"<<"*.s3d";
        fileInfoList=dirSeg.entryInfoList(segNameFilters, QDir::Files, QDir::Name);
        for(int i=0;i<fileInfoList.size();i++)
        {
            std::string filePath=fileInfoList[i].absoluteFilePath().toStdString();

            try
            {
                mitk::DataNode::Pointer segNode=mitk::IOUtil::LoadDataNode(filePath);
                segNode->SetVisibility(false);

                svContourGroup* group=dynamic_cast<svContourGroup*>(segNode->GetData());
                if(group)
                {
                    auto props=group->GetProps();
                    auto it = props.begin();
                    while(it != props.end())
                    {
                        if(it->second!="")
                        {
                            if(it->first=="point 2D display size")
                            {
                                float value=(float)(std::stod(it->second));
                                segNode->SetFloatProperty("point.displaysize",value);
                            }
                            else if(it->first=="point size")
                            {
                                float value=(float)(std::stod(it->second));
                                segNode->SetFloatProperty("point.3dsize",value);
                            }
                        }

                        it++;
                    }
                }

                dataStorage->Add(segNode,segFolderNode);
            }
            catch(...)
            {
                MITK_ERROR << "Failed to load file (maybe unsupported data type): " << filePath;
            }
        }

        modelFolderNode->SetVisibility(false);
        QDir dirModel(projPath);
        dirModel.cd(modelFolderName);
        fileInfoList=dirModel.entryInfoList(QStringList("*.mdl"), QDir::Files, QDir::Name);
        bool firstModel=true;
        for(int i=0;i<fileInfoList.size();i++)
        {
            std::string filePath=fileInfoList[i].absoluteFilePath().toStdString();

            try
            {
                mitk::DataNode::Pointer modelNode=mitk::IOUtil::LoadDataNode(filePath);
                if(firstModel)
                {
                    modelNode->SetVisibility(true);
                    firstModel=false;
                }
                else
                    modelNode->SetVisibility(false);

                dataStorage->Add(modelNode,modelFolderNode);
            }
            catch(...)
            {
                MITK_ERROR << "Failed to load file (maybe unsupported data type): " << filePath;
            }
        }

        meshFolderNode->SetVisibility(false);
        QDir dirMesh(projPath);
        dirMesh.cd(meshFolderName);
        fileInfoList=dirMesh.entryInfoList(QStringList("*.msh"), QDir::Files, QDir::Name);
        for(int i=0;i<fileInfoList.size();i++)
        {
            std::string filePath=fileInfoList[i].absoluteFilePath().toStdString();
            try
            {
            mitk::DataNode::Pointer meshNode=mitk::IOUtil::LoadDataNode(fileInfoList[i].absoluteFilePath().toStdString());
            meshNode->SetVisibility(false);
            dataStorage->Add(meshNode,meshFolderNode);
            }
            catch(...)
            {
                MITK_ERROR << "Failed to load file (maybe unsupported data type): " << filePath;
            }
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

void svProjectManager::AddImage(mitk::DataStorage::Pointer dataStorage, QString imageFilePath, mitk::DataNode::Pointer imageNode, mitk::DataNode::Pointer imageFolderNode
                                , bool copyIntoProject, double scaleFactor, QString newImageName)
{
    mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(dataStorage);

    //add image to config

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources (imageFolderNode,isProjFolder);

    std::string projPath;
    mitk::DataNode::Pointer projectFolderNode=rs->GetElement(0);

    projectFolderNode->GetStringProperty("project path",projPath);

    std::string imageName=newImageName.toStdString();
    if(imageName=="")
    {
        imageName="image";
        if(projectFolderNode.IsNotNull())
            imageName=projectFolderNode->GetName();
    }

    imageNode->SetName(imageName);

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
            QString imagePath=previousImgElement.attribute("path");
            if(imagePath=="")
                imagePath=previousImgElement.attribute("name");

            if(imagePath!="")
            {
                QString fullPath=QString::fromStdString(projPath+"/"+imageFolderNode->GetName()+"/"+imagePath.toStdString());
                QFile piFile(fullPath);
                piFile.remove();
            }
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
        QString imagePath=imageName+".vti";

        imgElement.setAttribute("name",imageName);
        imgElement.setAttribute("path",imagePath);

        std::string imageParentPath=projPath+"/"+imageFolderNode->GetName();
        std::string imagFullPath=imageParentPath+"/"+imagePath.toStdString();
        imageNode->SetStringProperty("path",imageParentPath.c_str());
        mitk::Image* image=dynamic_cast<mitk::Image*>(imageNode->GetData());
        if(image)
        {
            if(scaleFactor>0 && scaleFactor!=1)
            {
                mitk::Point3D org = image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetOrigin();
                mitk::Vector3D spacing=image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetSpacing();
                org[0]*=scaleFactor;
                org[1]*=scaleFactor;
                org[2]*=scaleFactor;
                image->SetOrigin(org);
                image->SetSpacing(scaleFactor*spacing);
            }

            vtkImageData* vtkImg=svVtkUtils::MitkImage2VtkImage(image);
            if(vtkImg)
            {
                vtkSmartPointer<vtkXMLImageDataWriter> writer = vtkSmartPointer<vtkXMLImageDataWriter>::New();
                writer->SetFileName(imagFullPath.c_str());
                writer->SetInputData(vtkImg);
                writer->Write();
            }
        }

        //        mitk::IOUtil::Save(imageNode->GetData(),ifilePath);
    }
    else
    {
        imgElement.setAttribute("in_project","no");
        imgElement.setAttribute("name",QString::fromStdString(imageName));
        imgElement.setAttribute("path",imageFilePath);
    }

    dataStorage->Add(imageNode,imageFolderNode);

    mitk::RenderingManager::GetInstance()->InitializeViews(imageNode->GetData()->GetTimeGeometry(), mitk::RenderingManager::REQUEST_UPDATE_ALL, true );

    imagesElement.appendChild(imgElement);

    QString xml = doc.toString(4);

    QFile file( configFilePath );

    if (file.open(QFile::WriteOnly | QFile::Truncate)) {
        QTextStream out(&file);
        out << xml <<endl;
    }

}

void svProjectManager::SaveProjectAs(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode, QString saveFilePath)
{
    SaveProject(dataStorage,projFolderNode);

    std::string projPath;
    projFolderNode->GetStringProperty("project path",projPath);

    QString oldPath=QString::fromStdString(projPath);

    DuplicateDirRecursively(oldPath, saveFilePath);
}

void svProjectManager::SaveProject(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode)
{
    std::vector<std::string> removeList;

    std::string projPath;
    projFolderNode->GetStringProperty("project path",projPath);

    //save paths
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svPathFolder"));

    mitk::DataNode::Pointer pathFolderNode=rs->GetElement(0);
    std::string pathFolderName=pathFolderNode->GetName();
    svPathFolder* pathFolder=dynamic_cast<svPathFolder*>(pathFolderNode->GetData());
    removeList.clear();
    if(pathFolder)
        removeList=pathFolder->GetNodeNamesToRemove();

    rs=dataStorage->GetDerivations(pathFolderNode,mitk::NodePredicateDataType::New("svPath"));

    QDir dir(QString::fromStdString(projPath));
    dir.cd(QString::fromStdString(pathFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);

        for(int j=removeList.size()-1;j>-1;j--)
        {
            if(removeList[j]==node->GetName())
                removeList.erase(removeList.begin()+j);
        }

        svPath *path=dynamic_cast<svPath*>(node->GetData());
        if(path==NULL || (!path->IsDataModified() && dir.exists(QString::fromStdString(node->GetName())+".pth")) )
            continue;

        QString	filePath=dir.absoluteFilePath(QString::fromStdString(node->GetName())+".pth");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        node->SetStringProperty("path",dir.absolutePath().toStdString().c_str());

        path->SetDataModified(false);
    }

    //delete files using removeList
    for(int i=0;i<removeList.size();i++)
    {
        dir.remove(QString::fromStdString(removeList[i])+".pth");
    }
    pathFolder->ClearRemoveList();

    //save contour groups
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svSegmentationFolder"));

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);
    std::string segFolderName=segFolderNode->GetName();
    svSegmentationFolder* segFolder=dynamic_cast<svSegmentationFolder*>(segFolderNode->GetData());
    removeList.clear();
    if(segFolder)
        removeList=segFolder->GetNodeNamesToRemove();

    mitk::NodePredicateOr::Pointer segTypes = mitk::NodePredicateOr::New();
    segTypes->AddPredicate(mitk::NodePredicateDataType::New("svContourGroup"));
    segTypes->AddPredicate(mitk::NodePredicateDataType::New("svMitkSeg3D"));

    rs=dataStorage->GetDerivations(segFolderNode,segTypes);

    QDir dirSeg(QString::fromStdString(projPath));
    dirSeg.cd(QString::fromStdString(segFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);

        for(int j=removeList.size()-1;j>-1;j--)
        {
            if(removeList[j]==node->GetName())
                removeList.erase(removeList.begin()+j);
        }

        svContourGroup *contourGroup=dynamic_cast<svContourGroup*>(node->GetData());
        svMitkSeg3D *mitkSeg3D=dynamic_cast<svMitkSeg3D*>(node->GetData());

        if(contourGroup)
        {
            if(!contourGroup->IsDataModified() && dirSeg.exists(QString::fromStdString(node->GetName())+".ctgr") )
                continue;

            QString	filePath=dirSeg.absoluteFilePath(QString::fromStdString(node->GetName())+".ctgr");
            mitk::IOUtil::Save(node->GetData(),filePath.toStdString());
            node->SetStringProperty("path",dirSeg.absolutePath().toStdString().c_str());
            contourGroup->SetDataModified(false);
        }

        if(mitkSeg3D)
        {
            if(!mitkSeg3D->IsDataModified() && dirSeg.exists(QString::fromStdString(node->GetName())+".s3d") )
                continue;

            QString	filePath=dirSeg.absoluteFilePath(QString::fromStdString(node->GetName())+".s3d");
            mitk::IOUtil::Save(node->GetData(),filePath.toStdString());
            node->SetStringProperty("path",dirSeg.absolutePath().toStdString().c_str());
            mitkSeg3D->SetDataModified(false);
        }
    }

    for(int i=0;i<removeList.size();i++)
    {
        dirSeg.remove(QString::fromStdString(removeList[i])+".ctgr");
        dirSeg.remove(QString::fromStdString(removeList[i])+".s3d");
        dirSeg.remove(QString::fromStdString(removeList[i])+".vtp");
    }
    segFolder->ClearRemoveList();

    //save models
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svModelFolder"));

    mitk::DataNode::Pointer modelFolderNode=rs->GetElement(0);
    std::string modelFolderName=modelFolderNode->GetName();
    svModelFolder* modelFolder=dynamic_cast<svModelFolder*>(modelFolderNode->GetData());
    removeList.clear();
    if(modelFolder)
        removeList=modelFolder->GetNodeNamesToRemove();

    rs=dataStorage->GetDerivations(modelFolderNode,mitk::NodePredicateDataType::New("svModel"));

    QDir dirModel(QString::fromStdString(projPath));
    dirModel.cd(QString::fromStdString(modelFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);

        for(int j=removeList.size()-1;j>-1;j--)
        {
            if(removeList[j]==node->GetName())
                removeList.erase(removeList.begin()+j);
        }

        svModel *model=dynamic_cast<svModel*>(node->GetData());
        if(model==NULL || (!model->IsDataModified() && dirModel.exists(QString::fromStdString(node->GetName())+".mdl")) )
            continue;

        QString	filePath=dirModel.absoluteFilePath(QString::fromStdString(node->GetName())+".mdl");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        node->SetStringProperty("path",dirModel.absolutePath().toStdString().c_str());

        model->SetDataModified(false);
    }

    for(int i=0;i<removeList.size();i++)
    {
        dirModel.remove(QString::fromStdString(removeList[i])+".mdl");
        dirModel.remove(QString::fromStdString(removeList[i])+".vtp");
        dirModel.remove(QString::fromStdString(removeList[i])+".brep");
        dirModel.remove(QString::fromStdString(removeList[i])+".xmt_txt");
    }
    modelFolder->ClearRemoveList();

    //save mesh
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svMeshFolder"));

    mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);
    std::string meshFolderName=meshFolderNode->GetName();
    svMeshFolder* meshFolder=dynamic_cast<svMeshFolder*>(meshFolderNode->GetData());
    removeList.clear();
    if(meshFolder)
        removeList=meshFolder->GetNodeNamesToRemove();

    rs=dataStorage->GetDerivations(meshFolderNode,mitk::NodePredicateDataType::New("svMitkMesh"));

    QDir dirMesh(QString::fromStdString(projPath));
    dirMesh.cd(QString::fromStdString(meshFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);

        for(int j=removeList.size()-1;j>-1;j--)
        {
            if(removeList[j]==node->GetName())
                removeList.erase(removeList.begin()+j);
        }

        svMitkMesh *mitkMesh=dynamic_cast<svMitkMesh*>(node->GetData());
        if(mitkMesh==NULL || (!mitkMesh->IsDataModified() && dirMesh.exists(QString::fromStdString(node->GetName())+".msh")) )
            continue;

        QString	filePath=dirMesh.absoluteFilePath(QString::fromStdString(node->GetName())+".msh");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        node->SetStringProperty("path",dirMesh.absolutePath().toStdString().c_str());

        mitkMesh->SetDataModified(false);
    }

    for(int i=0;i<removeList.size();i++)
    {
        dirMesh.remove(QString::fromStdString(removeList[i])+".msh");
        dirMesh.remove(QString::fromStdString(removeList[i])+".vtp");
        dirMesh.remove(QString::fromStdString(removeList[i])+".vtu");
        dirMesh.remove(QString::fromStdString(removeList[i])+".sms");
    }
    meshFolder->ClearRemoveList();

    //sava simjobs
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svSimulationFolder"));

    mitk::DataNode::Pointer simFolderNode=rs->GetElement(0);
    std::string simFolderName=simFolderNode->GetName();
    svSimulationFolder* simFolder=dynamic_cast<svSimulationFolder*>(simFolderNode->GetData());
    removeList.clear();
    if(simFolder)
        removeList=simFolder->GetNodeNamesToRemove();

    rs=dataStorage->GetDerivations(simFolderNode,mitk::NodePredicateDataType::New("svMitkSimJob"));

    QDir dirSim(QString::fromStdString(projPath));
    dirSim.cd(QString::fromStdString(simFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);

        for(int j=removeList.size()-1;j>-1;j--)
        {
            if(removeList[j]==node->GetName())
                removeList.erase(removeList.begin()+j);
        }

        svMitkSimJob *mitkJob=dynamic_cast<svMitkSimJob*>(node->GetData());
        if(mitkJob==NULL || (!mitkJob->IsDataModified() && dirSim.exists(QString::fromStdString(node->GetName())+".sjb")) )
            continue;

        QString	filePath=dirSim.absoluteFilePath(QString::fromStdString(node->GetName())+".sjb");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        node->SetStringProperty("path",dirSim.absolutePath().toStdString().c_str());

        mitkJob->SetDataModified(false);
    }

    for(int i=0;i<removeList.size();i++)
    {
        dirSim.remove(QString::fromStdString(removeList[i])+".sjb");
    }
    simFolder->ClearRemoveList();
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
    mitk::NodePredicateDataType::Pointer isSeg3D = mitk::NodePredicateDataType::New("svMitkSeg3D");
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
    else if(isSeg3D->CheckNode(dataNode))
    {
        extension="s3d";
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

void svProjectManager::AddDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, mitk::DataNode::Pointer parentNode)
{
    if(parentNode.IsNull())
        dataStorage->Add(dataNode);
    else
        dataStorage->Add(dataNode,parentNode);
}

void svProjectManager::RemoveDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, mitk::DataNode::Pointer parentNode)
{
    dataStorage->Remove(dataNode);

    if(parentNode.IsNull())
        return;

    mitk::TNodePredicateDataType<svDataFolder>::Pointer isDataFolder= mitk::TNodePredicateDataType<svDataFolder>::New();

    if(!isDataFolder->CheckNode(parentNode))
        return;

    svDataFolder* folder=dynamic_cast<svDataFolder*>(parentNode->GetData());
    folder->AddToRemoveList(dataNode->GetName());

}

void svProjectManager::RenameDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, std::string newName)
{
    std::string name=dataNode->GetName();

    std::string path="";
    dataNode->GetStringProperty("path", path);

    dataNode->SetName(newName);

    if(path=="")
        return;

    //rename the corresponding files and folder if applicable, for svPath, svContourGroup, svModel, svMitkMesh,svMitkSimJob
    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");
    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("svContourGroup");
    mitk::NodePredicateDataType::Pointer isSeg3D = mitk::NodePredicateDataType::New("svMitkSeg3D");
    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");
    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("svMitkMesh");
    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("svMitkSimJob");

    std::vector<std::string> extensions;
    if(isPath->CheckNode(dataNode))
    {
        extensions.push_back(".pth");
    }
    else if(isContourGroup->CheckNode(dataNode))
    {
        extensions.push_back(".ctgr");
    }
    else if(isSeg3D->CheckNode(dataNode))
    {
        extensions.push_back(".s3d");
        extensions.push_back(".vtp");
    }
    else if(isModel->CheckNode(dataNode))
    {
        extensions.push_back(".mdl");
        extensions.push_back(".vtp");
        extensions.push_back(".brep");
        extensions.push_back(".xmt_txt");
    }
    else if(isMesh->CheckNode(dataNode))
    {
        extensions.push_back(".msh");
        extensions.push_back(".vtp");
        extensions.push_back(".vtu");
        extensions.push_back(".sms");
    }
    else if(isSimJob->CheckNode(dataNode))
    {
        extensions.push_back(".sjb");
        extensions.push_back("");//for folder
    }
    else
        return;

    QDir dir(QString::fromStdString(path));
    for(int i=0;i<extensions.size();i++)
    {
        dir.rename(QString::fromStdString(name+extensions[i]),QString::fromStdString(newName+extensions[i]));
    }
}

void svProjectManager::DuplicateProject(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode, QString newName)
{
    SaveProject(dataStorage,projFolderNode);

    std::string projPath;
    projFolderNode->GetStringProperty("project path",projPath);

    QString oldPath=QString::fromStdString(projPath);
    QDir dir(oldPath);
    dir.cdUp();
    QString projParentDir=dir.absolutePath();
    QString newPath=projParentDir+"/"+newName;

    if(DuplicateDirRecursively(oldPath, newPath))
        svProjectManager::AddProject(dataStorage,newName,projParentDir,false);
}

bool svProjectManager::DuplicateDirRecursively(const QString &srcFilePath, const QString &tgtFilePath)
{
    QFileInfo srcFileInfo(srcFilePath);
    if (srcFileInfo.isDir()) {
        QDir targetDir(tgtFilePath);
        if (targetDir.exists())
          targetDir.cdUp();
        else
        {
          targetDir.cdUp();
          if (!targetDir.mkdir(QFileInfo(tgtFilePath).fileName()))
          {
              return false;
          }
        }
        QDir sourceDir(srcFilePath);
        QStringList fileNames = sourceDir.entryList(QDir::Files | QDir::Dirs | QDir::NoDotAndDotDot | QDir::Hidden | QDir::System);
        foreach (const QString &fileName, fileNames) {
            const QString newSrcFilePath
                    = srcFilePath + QLatin1Char('/') + fileName;
            const QString newTgtFilePath
                    = tgtFilePath + QLatin1Char('/') + fileName;
            if (!DuplicateDirRecursively(newSrcFilePath, newTgtFilePath))
            {
                return false;
            }
        }
    } else {

        QFile targetFile(tgtFilePath);
        if (targetFile.exists())
          targetFile.remove();

        if (!QFile::copy(srcFilePath, tgtFilePath))
        {
            return false;
        }
    }
    return true;
}

