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

#include "sv4gui_ProjectManager.h"

#include "sv4gui_ProjectFolder.h"
#include "sv4gui_ImageFolder.h"
#include "sv4gui_PathFolder.h"
#include "sv4gui_SegmentationFolder.h"
#include "sv4gui_ModelFolder.h"
#include "sv4gui_MeshFolder.h"
#include "sv4gui_SimulationFolder.h"
#include "sv4gui_svFSIFolder.h"
#include "sv4gui_Simulation1dFolder.h"
#include "sv4gui_RepositoryFolder.h"
#include "sv4gui_svFSIFolder.h"

#include "sv4gui_Path.h"
#include "sv4gui_ContourGroup.h"
#include "sv4gui_MitkSeg3D.h"
#include "sv4gui_Model.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MitkSimJob.h"
#include "sv4gui_MitksvFSIJob.h"
#include "sv4gui_MitkSimJob1d.h"
#include "sv4gui_MitkMeshIO.h"
#include "sv4gui_VtkUtils.h"

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

#include <itksys/SystemTools.hxx>

#include <QDir>
#include <QDomDocument>
#include <QDomElement>
#include <QFile>
#include <QTextStream>

#include <iostream>
#include <fstream>

// The sv4guiProjectManager class is used to manage SimVascular projects. 
//
// The SV Data Manager contains a hierarchy of Data Nodes comprising
// top level plugin nodes (e.g. Images, Paths, Segmentations, etc.) under 
// under which instances of that plugin type are created and stored. 
//
// For example:
//
//     Project Node
//
//        Images              - Top Level Images Plugin Data Node
//           Images-1         - Images Plugin Data Node 1
//           Images-2         - Images Plugin Data Node 2
//
//        Paths               - Top Level Paths Plugin Data Node
//           Paths-1          - Instance of Paths Plugin Data Node 
//
//
// Each plugin (Images, Paths, etc.) has a unique file extension used to
// identify a plugin type. Each plugin directory contains files for each plugin 
// instance (name chosen by the user) with its extenstion. This stores the plugin
// state, i.e. the values of the GUI widgets used as parameters used to perform
// plugin operations.
// 
//     Images: .vti
//     Meshes: .msh
//     Models: .mdl 
//     Paths:  .pth
//     Segmentations: .ctgr
//     Simulations: .sjb
//     1D Simulations: .s1djb
//     svFSI: .fsijob

// Define the name of the files used to identify the image data location.
//
// SVPROJ_CONFIG_FILE_NAME is deprecated.
//
const QString sv4guiProjectManager::SVPROJ_CONFIG_FILE_NAME = ".svproj";
const QString sv4guiProjectManager::IMAGE_LOCATION_FILE_NAME = "image_location.xml";

//------------
// AddProject
//------------
// Create a new project or read in an existing one.
//
void sv4guiProjectManager::AddProject(mitk::DataStorage::Pointer dataStorage, QString projName, QString projParentDir, bool newProject)
{
    std::cout << std::endl;
    std::cout << "========== sv4guiProjectManager::AddProject ========== " << std::endl;
    std::cout << "[AddProject] newProject: " << newProject << std::endl;
    QString projectConfigFileName = ".svproj";

    // Set plugin directory names. 
    QString imageFolderName = "Images";
    QString pathFolderName = "Paths";
    QString segFolderName = "Segmentations";
    QString modelFolderName = "Models";
    QString meshFolderName = "Meshes";
    QString simFolderName = "Simulations";
    QString sim1dFolderName = "Simulations1d";
    QString svFSIFolderName = "svFSI";
    QString reposFolderName = "Repository";

    GetImageDataPath(projName, projParentDir, imageFolderName);

    QDir dir(projParentDir);
    if (newProject) {
        dir.mkdir(projName);
    }
    dir.cd(projName);

    QString projPath = dir.absolutePath();
    QString projectConfigFilePath = dir.absoluteFilePath(projectConfigFileName);

    QStringList imageFilePathList;
    QStringList imageNameList;

    bool rewriteProjectConfigFile = false;
    QDomDocument doc("svproj");

    if (newProject) {
        WriteEmptyConfigFile(projectConfigFilePath);
        dir.mkdir(imageFolderName);
        dir.mkdir(pathFolderName);
        dir.mkdir(segFolderName);
        dir.mkdir(modelFolderName);
        dir.mkdir(meshFolderName);
        dir.mkdir(simFolderName);
        dir.mkdir(sim1dFolderName);
        dir.mkdir(svFSIFolderName);
        dir.mkdir(reposFolderName);

    } else {

        QFile xmlFile(projectConfigFilePath);
        xmlFile.open(QIODevice::ReadOnly);
//        QDomDocument doc("svproj");
        QString *em=NULL;
        doc.setContent(&xmlFile,em);
        xmlFile.close();

        QDomElement projDesc = doc.firstChildElement("projectDescription");
        QDomElement imagesElement = projDesc.firstChildElement("images");
        imageFolderName = imagesElement.attribute("folder_name");
        QDomNodeList imageList = imagesElement.elementsByTagName("image");

        // Create a list of image names to read.
        //
        // [TODO:DaveP] This loop does not make sense because only one 
        // image is allowed in SV.
        //
        std::cout << "[AddProject] imageList.size(): " << imageList.size() << std::endl;
        for (int i = 0; i < imageList.size(); i++) {
            QDomNode imageNode = imageList.item(i);
            if (imageNode.isNull()) {
                continue;
            }

            QDomElement imageElement=imageNode.toElement();
            if(imageElement.isNull()) {
                continue;
            }

            QString inProj = imageElement.attribute("in_project");
            QString imageName = imageElement.attribute("name");
            QString imagePath = imageElement.attribute("path");

            // Set the image plugin attributes.
            //
            // [TODO:DaveP] How can imagePath = "" ?
            //
            if (inProj == "yes") {
                if (imagePath != "") {
                    imageFilePathList << (projPath+"/"+imageFolderName+"/"+imagePath);
                } else if(imageName != "") {
                    imageFilePathList << (projPath+"/"+imageFolderName+"/"+imageName);
                    imageElement.setAttribute("path", imageName);
                    imageElement.setAttribute("name", imageName.remove(".vti"));
                    rewriteProjectConfigFile = true;
                }
                imageNameList << imageName;

            } else {
                imageFilePathList << imageElement.attribute("path");

                if (imageName=="") {
                    imageName = "image";
                    imageElement.setAttribute("name",imageName);
                    rewriteProjectConfigFile = true;
                }
                imageNameList<<imageName;
            }
        }
    }

    // Overwrite the .svproj file.

    if (rewriteProjectConfigFile) {
        QString xml = doc.toString(4);
        QFile file( projectConfigFilePath );
        if (file.open(QFile::WriteOnly | QFile::Truncate)) {
            QTextStream out(&file);
            out << xml <<endl;
        }
    }

    // Create the SV Data Manager top level plugin data nodes.
    //
    mitk::DataNode::Pointer projectFolderNode = CreateDataFolder<sv4guiProjectFolder>(dataStorage,projName);
    projectFolderNode->AddProperty("project path",mitk::StringProperty::New(projPath.toStdString().c_str()));

    mitk::DataNode::Pointer imageFolderNode = CreateDataFolder<sv4guiImageFolder>(dataStorage, imageFolderName, projectFolderNode);
    mitk::DataNode::Pointer pathFolderNode = CreateDataFolder<sv4guiPathFolder>(dataStorage, pathFolderName, projectFolderNode);
    mitk::DataNode::Pointer segFolderNode = CreateDataFolder<sv4guiSegmentationFolder>(dataStorage,segFolderName, projectFolderNode);
    mitk::DataNode::Pointer modelFolderNode = CreateDataFolder<sv4guiModelFolder>(dataStorage, modelFolderName, projectFolderNode);
    mitk::DataNode::Pointer meshFolderNode = CreateDataFolder<sv4guiMeshFolder>(dataStorage, meshFolderName, projectFolderNode);
    mitk::DataNode::Pointer simFolderNode = CreateDataFolder<sv4guiSimulationFolder>(dataStorage, simFolderName, projectFolderNode);
    mitk::DataNode::Pointer svFSIFolderNode = CreateDataFolder<sv4guisvFSIFolder>(dataStorage, svFSIFolderName, projectFolderNode);
    mitk::DataNode::Pointer sim1dFolderNode = CreateDataFolder<sv4guiSimulation1dFolder>(dataStorage, sim1dFolderName, projectFolderNode);
    mitk::DataNode::Pointer reposFolderNode = CreateDataFolder<sv4guiRepositoryFolder>(dataStorage, reposFolderName, projectFolderNode);

    imageFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    pathFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    segFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    modelFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    meshFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    simFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    svFSIFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    sim1dFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );
    reposFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );

    if (newProject) {
        mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(dataStorage);
        return;
    }

    // Create Images plugin data nodes and read image data.
    auto imageNode = CreateImagesPlugin(dataStorage, projPath, imageFolderNode, imageFilePathList, imageNameList);

    // Create Paths plugin data nodes.
    CreatePathsPlugin(dataStorage, projPath, pathFolderNode, pathFolderName);

    // Create Segmentations plugin data nodes.
    CreateSegmentationsPlugin(dataStorage, projPath, segFolderNode, segFolderName);

    // Create MITK segmentation data nodes
    CreateMitkSegmentationsPlugin(dataStorage, projPath, imageNode);

    // Create Model plugin data nodes.
    CreateModelPlugin(dataStorage, projPath, modelFolderNode, modelFolderName);

    // Create Meshes plugin data nodes.
    CreatePlugin(dataStorage, projPath, meshFolderNode, meshFolderName, "*.msh");

    // Create Simulations plugin data nodes.
    CreatePlugin(dataStorage, projPath, simFolderNode, simFolderName, "*.sjb");

    // Create svFSI plugin data nodes.
    CreatePlugin(dataStorage, projPath, svFSIFolderNode, svFSIFolderName, "*.fsijob");

    // Create Simulations1d plugin data nodes.
    CreatePlugin(dataStorage, projPath, sim1dFolderNode, sim1dFolderName, "*.s1djb");

    mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(dataStorage);
}

//------------------
// GetImageDataPath
//------------------
//
void sv4guiProjectManager::GetImageDataPath(QString projName, QString projParentDir, QString imageFolderName)
{
  std::cout << std::endl;
  std::cout << "========== sv4guiProjectManager::GetImageDataPath ========== " << std::endl;
  QDir dir(projParentDir);
  dir.cd(projName);

  std::cout << "[GetImageDataPath] projName: " << projName.toStdString() << std::endl;
  std::cout << "[GetImageDataPath] projParentDir: " << projParentDir.toStdString() << std::endl;

  auto projPath = dir.absolutePath();
  auto imageLocFilePath = dir.absoluteFilePath(imageFolderName + "/" + sv4guiProjectManager::IMAGE_LOCATION_FILE_NAME);
  std::cout << "[GetImageDataPath] imageLocFilePath: " << imageLocFilePath.toStdString() << std::endl;

  QString imageFilePath;
  QString imageName;
  bool noimageLocFile; 

  // If there is no image location file then read the old .svproj file.
  //
  QFileInfo checkFile(imageLocFilePath);
  if (!checkFile.exists()) { 
    std::cout << "[GetImageDataPath] **** No imageLocFilePath: " << imageLocFilePath.toStdString() << std::endl;
    QStringList imageFilePathList;
    QStringList imageNameList;
    GetImageDataPathFromSvproj(projName, projParentDir, imageFolderName, imageFilePathList, imageNameList);
    imageFilePath = imageFilePathList[0];
    imageName = imageNameList[0];
    noimageLocFile = true; 
  } else {
    noimageLocFile = false; 
    GetImageDataPathFromImageLoc(imageLocFilePath, imageFolderName, imageFilePath, imageName);
  }

  std::cout << "[GetImageDataPath] imageName: " << imageName.toStdString() << std::endl;
  std::cout << "[GetImageDataPath] imageFilePath: " << imageFilePath.toStdString() << std::endl;

  std::cout << std::endl;

}

//------------------------------
// GetImageDataPathFromImageLoc
//------------------------------
//
void sv4guiProjectManager::GetImageDataPathFromImageLoc(QString imageLocFilePath, QString imageFolderName, QString& imageFilePath, 
      QString& imageName)
{
  // Read IMAGE_LOCATION_FILE_NAME xml file.
  //
  QDomDocument doc("image_location");
  QFile xmlFile(imageLocFilePath);
  xmlFile.open(QIODevice::ReadOnly);
  QString *em = NULL;
  doc.setContent(&xmlFile, em);
  xmlFile.close();
  
  QDomElement imageLoc = doc.firstChildElement("ImageLocation");
  imageName = imageLoc.attribute("name");
  imageFilePath = imageLoc.attribute("path");
}

//----------------------------
// GetImageDataPathFromSvproj
//----------------------------
//
void sv4guiProjectManager::GetImageDataPathFromSvproj(QString projName, QString projParentDir, QString imageFolderName, 
    QStringList& imageFilePathList, QStringList& imageNameList)
{
  std::cout << std::endl;
  std::cout << "========== sv4guiProjectManager::GetImageDataPathFromSvproj ========== " << std::endl;
  QDir dir(projParentDir);
  dir.cd(projName);
  auto projPath = dir.absolutePath();

  QString svprojFilePath = dir.absoluteFilePath(sv4guiProjectManager::SVPROJ_CONFIG_FILE_NAME);
  std::cout << "[GetImageDataPathFromSvproj] svprojFilePath: " << svprojFilePath.toStdString() << std::endl;
  QFileInfo checkFile(svprojFilePath);

  if (!checkFile.exists()) { 
    std::cout << "[GetImageDataPathFromSvproj] **** No svproj file; " << svprojFilePath.toStdString() << std::endl;
    MITK_ERROR << "No .svproj file '" << svprojFilePath.toStdString() << "' found for the project.";
  }
  
  // Read .svproj xml file.
  //
  QDomDocument doc("svproj");
  QFile xmlFile(svprojFilePath);
  xmlFile.open(QIODevice::ReadOnly);
  QString *em = NULL;
  doc.setContent(&xmlFile, em);
  xmlFile.close();

  QDomElement projDesc = doc.firstChildElement("projectDescription");
  QDomElement imagesElement = projDesc.firstChildElement("images");
  imageFolderName = imagesElement.attribute("folder_name");
  QDomNodeList imageList = imagesElement.elementsByTagName("image");

  // Get image paths and names.
  //
  // [DaveP] There can only be a single image?
  //
  for (int i = 0; i < imageList.size(); i++) {
    QDomNode imageNode = imageList.item(i);
    if (imageNode.isNull()) {
      continue;
    }

    QDomElement imageElement = imageNode.toElement();
    if (imageElement.isNull()) {
      continue;
    }

    QString inProj = imageElement.attribute("in_project");
    QString imageName = imageElement.attribute("name");
    QString imagePath = imageElement.attribute("path");
    bool rewriteProjectConfigFile = false;

    if (inProj == "yes") {
      if (imagePath != "") {
        imageFilePathList << (projPath+"/"+imageFolderName+"/"+imagePath);
      } else if(imageName != "") {
        imageFilePathList << (projPath+"/"+imageFolderName+"/"+imageName);
        imageElement.setAttribute("path", imageName);
        imageElement.setAttribute("name", imageName.remove(".vti"));
        rewriteProjectConfigFile = true;
      }
      imageNameList << imageName;

    } else {
      imageFilePathList << imageElement.attribute("path");

      if (imageName == "") {
        imageName = "image";
        imageElement.setAttribute("name",imageName);
        rewriteProjectConfigFile = true;
      }
      imageNameList << imageName;
    }
  }

  if (imageFilePathList.size() != 1) {
      MITK_ERROR << "Failed to load .svproj file '" << svprojFilePath.toStdString() << "'; no image path information.";
  }
}

//----------------------
// WriteEmptyConfigFile
//----------------------
//
void sv4guiProjectManager::WriteEmptyConfigFile(QString projConfigFilePath)
{
    QDomDocument doc;
    //    doc.createProcessingInstruction("xml", "version=\"1.0\" encoding=\"UTF-8\"");

    QDomNode xmlNode = doc.createProcessingInstruction("xml", "version=\"1.0\" encoding=\"UTF-8\"");
    doc.appendChild(xmlNode);

    QDomElement root = doc.createElement("projectDescription");
//    root.setAttribute("version", "1.0");
    root.setAttribute("version", "1.1");
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

    tag = doc.createElement("simulations1d");
    tag.setAttribute("folder_name","Simulations1d");
    root.appendChild(tag);

    tag = doc.createElement("svFSI");
    tag.setAttribute("folder_name","svFSI");
    root.appendChild(tag);

    tag = doc.createElement("repository");
    tag.setAttribute("folder_name","Repository");
    root.appendChild(tag);

    tag = doc.createElement("svFSI");
    tag.setAttribute("folder_name","svFSI");
    root.appendChild(tag);

    QString xml = doc.toString(4);

    QFile file( projConfigFilePath );

    if (file.open(QFile::WriteOnly | QFile::Truncate)) {
        QTextStream out(&file);
        out << xml <<endl;
    }
}

//----------
// AddImage
//----------
//
void sv4guiProjectManager::AddImage(mitk::DataStorage::Pointer dataStorage, QString imageFilePath, mitk::DataNode::Pointer imageNode, mitk::DataNode::Pointer imageFolderNode
                                , bool copyIntoProject, double scaleFactor, QString newImageName)
{
    mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(dataStorage);

    //add image to config

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
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

    //continue image adding

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

            //if converting from dicom to vti we need to record the original
            //image transform so we can load it back in
            //otherwise the vti will use an incorrect transform when reloaded
            writeTransformFile(image, imageParentPath, imageNode->GetName());

            vtkImageData* vtkImg=sv4guiVtkUtils::MitkImage2VtkImage(image);
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

    return;
}

/**
Due to differing conventions in the way DICOM and .vti files treat image axes
if we convert a dicom to a vti and then load it back in it will have an incorrect
transform.

To counteract this we write the original transform when loading in the dicom,
then when the converted .vti is read back in, we reset the transform.
**/
void sv4guiProjectManager::writeTransformFile(mitk::Image* image, std::string imageParentPath, std::string imageName)
{
  QDir dir(QString::fromStdString(imageParentPath));
  QString FilePath=dir.absoluteFilePath(QString::fromStdString(imageName+".transform.xml"));

  auto transform_fn = FilePath.toStdString();
  auto transform = image->GetGeometry()->GetVtkMatrix();

  QDomDocument doc;
  QDomNode xmlNode = doc.createProcessingInstruction("xml", "version=\"1.0\" encoding=\"UTF-8\"");
  doc.appendChild(xmlNode);

  QDomElement root = doc.createElement("Transform");
  root.setAttribute("transform_version", 1.0);
  root.setAttribute("description", "original dicom transform to reset the .vti transform "
    "when reloading a .vti after converting from dicom. This is needed because mitk "
    "loads DICOMs and vtis differently.");
  doc.appendChild(root);

  QDomElement tag = doc.createElement("transform");

  for (int j = 0; j < 3; j++){
    for (int i = 0; i < 3; i++){
      auto v = transform->GetElement(i,j);
      auto label = "t"+std::to_string(i)+std::to_string(j);
      tag.setAttribute(QString::fromStdString(label),v);
    }
  }

  root.appendChild(tag);
  QString xml = doc.toString(4);

  QFile file( QString::fromStdString(transform_fn ) );

  if (file.open(QFile::WriteOnly | QFile::Truncate)) {
      QTextStream out(&file);
      out << xml <<endl;
  }
}

//--------------
// setTransform
//--------------
// Set the image transformation from the SV project Images/.transform.xml file.
//
// This sets the upper 3x3 elements in the vtkMatrix4x4 object.
//
void sv4guiProjectManager::setTransform(mitk::Image* image, std::string proj_path, std::string imageName)
{
  std::cout << "========== sv4guiProjectManager::setTransform ==========" << std::endl;

  QDir dir(QString::fromStdString(proj_path));
  dir.cd(QString::fromStdString("Images"));
  QString FilePath = dir.absoluteFilePath(QString::fromStdString(imageName+".transform.xml"));
  auto transform_fn = FilePath.toStdString();

  QDomDocument doc("transform");

  QFile xmlFile(QString::fromStdString(transform_fn));
  if (xmlFile.open(QIODevice::ReadOnly)){
    //std::cout << "loading xml\n";
    QString *em=NULL;
    doc.setContent(&xmlFile,em);
    xmlFile.close();

    auto root = doc.firstChildElement("Transform").firstChildElement("transform");
    //std::cout << doc.toString().toStdString() << "\n";
    //std::cout << root.text().toStdString() << "\n";
    auto transform = image->GetGeometry()->GetVtkMatrix();

    std::cout << "[setTransform] ---------- transform ----------" << std::endl;
    vtkIndent indent;
    transform->PrintSelf(std::cout, indent);
    std::cout << "[setTransform] ----------------------------" << std::endl;

    std::cout << "[setTransform] Read 'transform' element ..." << std::endl;
    for (int j = 0; j < 3; j++){
      for (int i = 0; i < 3; i++){
        auto label = "t"+std::to_string(i)+std::to_string(j);
        std::cout << "[setTransform] Label: " << label << std::endl;
        auto line = root.attribute(QString::fromStdString(label));
        std::cout << "[setTransform] Value: " << line.toStdString() << std::endl;
        auto v = std::stod(line.toStdString());
        transform->SetElement(i,j,v);
      }
    }


    auto spacing = image->GetGeometry()->GetSpacing();
    std::cout << "[setTransform] Spacing: " << spacing[0] << " " << spacing[1] << " " << spacing[2] << std::endl;
    auto origin = image->GetGeometry()->GetOrigin();
    std::cout << "[setTransform] Origin: " << origin[0] << " " << origin[1] << " " << origin[2] << std::endl;

    image->GetGeometry()->SetIndexToWorldTransformByVtkMatrix(transform);
    image->UpdateOutputInformation();

    transform = image->GetGeometry()->GetVtkMatrix();
    std::cout << "[setTransform] ---------- new transform ----------" << std::endl;
    transform->PrintSelf(std::cout, indent);
    std::cout << "[setTransform] ----------------------------" << std::endl;

    spacing = image->GetGeometry()->GetSpacing();
    std::cout << "[setTransform] New Spacing: "<<spacing[0]<<" "<<spacing[1]<<" "<<spacing[2]<<std::endl;

    auto norigin = image->GetGeometry()->GetOrigin();
    std::cout << "[setTransform] New Origin: " << norigin[0] << " " << norigin[1] << " " << norigin[2] << std::endl;

  }
}

//---------------
// SaveProjectAs
//---------------
//
void sv4guiProjectManager::SaveProjectAs(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode, 
      QString saveFilePath)
{
    SaveProject(dataStorage,projFolderNode);

    std::string projPath;
    projFolderNode->GetStringProperty("project path",projPath);

    QString oldPath=QString::fromStdString(projPath);

    DuplicateDirRecursively(oldPath, saveFilePath);
}

//-------------
// SaveProject
//-------------
//
void sv4guiProjectManager::SaveProject(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode)
{
    std::vector<std::string> removeList;

    std::string projPath;
    projFolderNode->GetStringProperty("project path",projPath);

    //save paths
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiPathFolder"));

    mitk::DataNode::Pointer pathFolderNode=rs->GetElement(0);
    std::string pathFolderName=pathFolderNode->GetName();
    sv4guiPathFolder* pathFolder=dynamic_cast<sv4guiPathFolder*>(pathFolderNode->GetData());
    removeList.clear();
    if(pathFolder)
        removeList=pathFolder->GetNodeNamesToRemove();

    rs=dataStorage->GetDerivations(pathFolderNode,mitk::NodePredicateDataType::New("sv4guiPath"));

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

        sv4guiPath *path=dynamic_cast<sv4guiPath*>(node->GetData());
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
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiSegmentationFolder"));

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);
    std::string segFolderName=segFolderNode->GetName();
    sv4guiSegmentationFolder* segFolder=dynamic_cast<sv4guiSegmentationFolder*>(segFolderNode->GetData());
    removeList.clear();
    if(segFolder)
        removeList=segFolder->GetNodeNamesToRemove();

    mitk::NodePredicateOr::Pointer segTypes = mitk::NodePredicateOr::New();
    segTypes->AddPredicate(mitk::NodePredicateDataType::New("sv4guiContourGroup"));
    segTypes->AddPredicate(mitk::NodePredicateDataType::New("sv4guiMitkSeg3D"));

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

        sv4guiContourGroup *contourGroup=dynamic_cast<sv4guiContourGroup*>(node->GetData());
        sv4guiMitkSeg3D *mitkSeg3D=dynamic_cast<sv4guiMitkSeg3D*>(node->GetData());

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
    
    //save 3d mitk segmentation
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiImageFolder"));
    mitk::DataNode::Pointer imageFolderNode=rs->GetElement(0);
    
    rs=dataStorage->GetDerivations(imageFolderNode);

    if (rs->size() != 0) {
        mitk::DataNode::Pointer imageNode=rs->GetElement(0);
        rs=dataStorage->GetDerivations(imageNode);
    
        for (int i=0;i<rs->size();i++) {
            mitk::DataNode::Pointer node=rs->GetElement(i);
            mitk::Image::Pointer segmentation=dynamic_cast<mitk::Image*>(node->GetData());
            if(segmentation.IsNull()) {
                continue;
            }
            QString filePath=dirSeg.absoluteFilePath(QString::fromStdString(node->GetName())+".vti");
            //mitk::IOUtil::Save(node->GetData(),filePath.toStdString());
            vtkImageData* vtkImg=sv4guiVtkUtils::MitkImage2VtkImage(segmentation);
            if(vtkImg) {
                vtkSmartPointer<vtkXMLImageDataWriter> writer = vtkSmartPointer<vtkXMLImageDataWriter>::New();
                writer->SetFileName(filePath.toStdString().c_str());
                writer->SetInputData(vtkImg);
                writer->Write();
            }

            node->SetStringProperty("3dseg",dirSeg.absolutePath().toStdString().c_str());
        }
    }

    //save models
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiModelFolder"));

    mitk::DataNode::Pointer modelFolderNode=rs->GetElement(0);
    std::string modelFolderName=modelFolderNode->GetName();
    sv4guiModelFolder* modelFolder=dynamic_cast<sv4guiModelFolder*>(modelFolderNode->GetData());
    removeList.clear();
    if(modelFolder)
        removeList=modelFolder->GetNodeNamesToRemove();

    rs=dataStorage->GetDerivations(modelFolderNode,mitk::NodePredicateDataType::New("sv4guiModel"));

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

        sv4guiModel *model=dynamic_cast<sv4guiModel*>(node->GetData());
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
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiMeshFolder"));

    mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);
    std::string meshFolderName=meshFolderNode->GetName();
    sv4guiMeshFolder* meshFolder=dynamic_cast<sv4guiMeshFolder*>(meshFolderNode->GetData());
    removeList.clear();
    if(meshFolder)
        removeList=meshFolder->GetNodeNamesToRemove();

    rs=dataStorage->GetDerivations(meshFolderNode,mitk::NodePredicateDataType::New("sv4guiMitkMesh"));

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

        sv4guiMitkMesh *mitkMesh=dynamic_cast<sv4guiMitkMesh*>(node->GetData());
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
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiSimulationFolder"));

    mitk::DataNode::Pointer simFolderNode=rs->GetElement(0);
    std::string simFolderName=simFolderNode->GetName();
    sv4guiSimulationFolder* simFolder=dynamic_cast<sv4guiSimulationFolder*>(simFolderNode->GetData());
    removeList.clear();
    if(simFolder)
        removeList=simFolder->GetNodeNamesToRemove();

    rs=dataStorage->GetDerivations(simFolderNode,mitk::NodePredicateDataType::New("sv4guiMitkSimJob"));

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

        sv4guiMitkSimJob *mitkJob=dynamic_cast<sv4guiMitkSimJob*>(node->GetData());
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

    // Save simulations 1d jobs
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiSimulation1dFolder"));
    mitk::DataNode::Pointer sim1dFolderNode=rs->GetElement(0);
    std::string sim1dFolderName=sim1dFolderNode->GetName();
    sv4guiSimulation1dFolder* sim1dFolder=dynamic_cast<sv4guiSimulation1dFolder*>(sim1dFolderNode->GetData());
    removeList.clear();
    if(sim1dFolder)
        removeList=sim1dFolder->GetNodeNamesToRemove();

    rs=dataStorage->GetDerivations(sim1dFolderNode,mitk::NodePredicateDataType::New("sv4guiMitkSimJob1d"));

    QDir dir1dSim(QString::fromStdString(projPath));
    dir1dSim.cd(QString::fromStdString(sim1dFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);

        for(int j=removeList.size()-1;j>-1;j--)
        {
            if(removeList[j]==node->GetName())
                removeList.erase(removeList.begin()+j);
        }

        sv4guiMitkSimJob1d *mitkJob=dynamic_cast<sv4guiMitkSimJob1d*>(node->GetData());
        if(mitkJob==NULL || (!mitkJob->IsDataModified() && dir1dSim.exists(QString::fromStdString(node->GetName())+".s1djb")) )
            continue;

        QString	filePath=dir1dSim.absoluteFilePath(QString::fromStdString(node->GetName())+".s1djb");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        node->SetStringProperty("path",dir1dSim.absolutePath().toStdString().c_str());

        mitkJob->SetDataModified(false);
    }

    for(int i=0;i<removeList.size();i++)
    {
        dirSim.remove(QString::fromStdString(removeList[i])+".s1djb");
    }
    sim1dFolder->ClearRemoveList();


    //svFSI Jobs
    rs=dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guisvFSIFolder"));

    mitk::DataNode::Pointer svFSIFolderNode=rs->GetElement(0);
    std::string svFSIFolderName=svFSIFolderNode->GetName();
    sv4guisvFSIFolder* svFSIFolder=dynamic_cast<sv4guisvFSIFolder*>(svFSIFolderNode->GetData());
    removeList.clear();
    if(svFSIFolder)
        removeList=svFSIFolder->GetNodeNamesToRemove();

    rs=dataStorage->GetDerivations(svFSIFolderNode,mitk::NodePredicateDataType::New("sv4guiMitksvFSIJob"));

    QDir dirFSI(QString::fromStdString(projPath));
    dirFSI.cd(QString::fromStdString(svFSIFolderName));

    for(int i=0;i<rs->size();i++)
    {
        mitk::DataNode::Pointer node=rs->GetElement(i);

        for(int j=removeList.size()-1;j>-1;j--)
        {
            if(removeList[j]==node->GetName())
                removeList.erase(removeList.begin()+j);
        }

        sv4guiMitksvFSIJob *mitkJob=dynamic_cast<sv4guiMitksvFSIJob*>(node->GetData());
        if(mitkJob==NULL || (!mitkJob->IsDataModified() && dirFSI.exists(QString::fromStdString(node->GetName())+".fsijob")) )
            continue;

        QString	filePath=dirFSI.absoluteFilePath(QString::fromStdString(node->GetName())+".fsijob");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        node->SetStringProperty("path",dirFSI.absolutePath().toStdString().c_str());

        mitkJob->SetDataModified(false);
    }

    for(int i=0;i<removeList.size();i++)
    {
        dirFSI.remove(QString::fromStdString(removeList[i])+".fsijob");
    }
    svFSIFolder->ClearRemoveList();
}

//-----------------
// SaveAllProjects
//-----------------
//
void sv4guiProjectManager::SaveAllProjects(mitk::DataStorage::Pointer dataStorage)
{
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSubset(mitk::NodePredicateDataType::New("sv4guiProjectFolder"));
    for(int i=0;i<rs->size();i++)
    {
        SaveProject(dataStorage, rs->GetElement(i));
    }
}

//----------
// LoadData
//----------
//
void sv4guiProjectManager::LoadData(mitk::DataNode::Pointer dataNode)
{
    // Get the path to the project data node (e.g. Paths data node)..
    std::string path = "";
    dataNode->GetStringProperty("path",path);
    if (path == "") {
        return;
    }

    // Evaluate if the given DataNodes data objects are of the specific data type.
    //
    // The data type is specified a string
    //
    // [TODO:DaveP] This is hideous! Need to have a global class that stores
    // all of these string names.
    //
    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("sv4guiPath");
    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("sv4guiContourGroup");
    mitk::NodePredicateDataType::Pointer isSeg3D = mitk::NodePredicateDataType::New("sv4guiMitkSeg3D");
    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("sv4guiModel");
    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("sv4guiMitkMesh");
    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob");
    mitk::NodePredicateDataType::Pointer issvFSIJob = mitk::NodePredicateDataType::New("sv4guiMitksvFSIJob");
    mitk::NodePredicateDataType::Pointer isSim1dJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob1d");

    // Determine data node file extension.
    //
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
    else if(isSim1dJob->CheckNode(dataNode))
    {
        extension="s1djb";
    }
    else if(issvFSIJob->CheckNode(dataNode))
    {
        extension="fsijob";
    }


    std::vector<mitk::BaseData::Pointer> vdata = mitk::IOUtil::Load(path+"/"+dataNode->GetName()+"."+extension);
    if(vdata.size()>0)
    {
        dataNode->SetData(vdata[0]);
    }
}

//--------------
// LoadDataNode
//--------------
// Create a data node with the given file path and load data into it.
//
// This loads data for all SV tools from their specific files types: .vti, .ctgr, .mdl, etc. 
//
// Note: MITK knows how to load the different data files by defining a Read() method inherited 
// from mitk::AbstractFileIO.
//
mitk::DataNode::Pointer sv4guiProjectManager::LoadDataNode(std::string filePath)
{
 /*
 std::cout << std::endl;
 std::cout << "========== sv4guiProjectManager::LoadData(string) ========== " << std::endl;
 std::cout << "[LoadData] filePath: " << filePath << std::endl;
 */

 // Load data from a file.
 std::vector<mitk::BaseData::Pointer> baseDataList = mitk::IOUtil::Load(filePath);
 if (baseDataList.empty()) {
   MITK_ERROR << "Object not added to Data Storage! Please make sure object is valid: " << filePath;
   return NULL;
 }

 // Create a data node for the data.
 mitk::BaseData::Pointer baseData = baseDataList.front();
 mitk::DataNode::Pointer node = mitk::DataNode::New();
 node->SetData(baseData);

 // Set the data node PATH property. 
 mitk::StringProperty::Pointer pathProp = mitk::StringProperty::New(itksys::SystemTools::GetFilenamePath(filePath));
 node->SetProperty(mitk::StringProperty::PATH, pathProp);

 // Check if the name is already defined.
 mitk::StringProperty::Pointer nameProp = dynamic_cast<mitk::StringProperty *>(node->GetProperty("name"));
 if (nameProp.IsNull() || (strcmp(nameProp->GetValue(), "No Name!") == 0)) {
   mitk::StringProperty::Pointer baseDataNameProp =
       dynamic_cast<mitk::StringProperty*>(node->GetData()->GetProperty("name").GetPointer());
   if (baseDataNameProp.IsNull() || (strcmp(baseDataNameProp->GetValue(), "No Name!") == 0)) {
     nameProp = mitk::StringProperty::New(itksys::SystemTools::GetFilenameWithoutExtension(filePath));
     node->SetProperty("name", nameProp);
   } else {
     nameProp = mitk::StringProperty::New(baseDataNameProp->GetValue());
     node->SetProperty("name", nameProp);
   }
 }

 // Set visibility.
 if (!node->GetProperty("visible")) {
   node->SetVisibility(true);
 }

 return node;
}

//----------------------
// GetProjectFolderNode
//----------------------
//
mitk::DataNode::Pointer 
sv4guiProjectManager::GetProjectFolderNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode)
{
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources (dataNode,isProjFolder,false);

    mitk::DataNode::Pointer projFolderNode=NULL;
    if(rs->size()>0)
        projFolderNode=rs->GetElement(0);

    return projFolderNode;
}

//-------------
// AddDataNode
//-------------
//
void sv4guiProjectManager::AddDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, mitk::DataNode::Pointer parentNode)
{
    if(parentNode.IsNull())
        dataStorage->Add(dataNode);
    else
        dataStorage->Add(dataNode,parentNode);
}

//----------------
// RemoveDataNode
//----------------
//
void sv4guiProjectManager::RemoveDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, mitk::DataNode::Pointer parentNode)
{
    dataStorage->Remove(dataNode);

    if(parentNode.IsNull())
        return;

    mitk::TNodePredicateDataType<sv4guiDataFolder>::Pointer isDataFolder= mitk::TNodePredicateDataType<sv4guiDataFolder>::New();

    if(!isDataFolder->CheckNode(parentNode))
        return;

    sv4guiDataFolder* folder=dynamic_cast<sv4guiDataFolder*>(parentNode->GetData());
    folder->AddToRemoveList(dataNode->GetName());

}

//----------------
// RenameDataNode
//----------------
//
void sv4guiProjectManager::RenameDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, std::string newName)
{
    std::string name=dataNode->GetName();

    std::string path="";
    dataNode->GetStringProperty("path", path);

    dataNode->SetName(newName);

    if(path=="")
        return;

    //rename the corresponding files and folder if applicable, for sv4guiPath, sv4guiContourGroup, sv4guiModel, sv4guiMitkMesh,sv4guiMitkSimJob
    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("sv4guiPath");
    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("sv4guiContourGroup");
    mitk::NodePredicateDataType::Pointer isSeg3D = mitk::NodePredicateDataType::New("sv4guiMitkSeg3D");
    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("sv4guiModel");
    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("sv4guiMitkMesh");
    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob");
    mitk::NodePredicateDataType::Pointer issvFSIJob = mitk::NodePredicateDataType::New("sv4guiMitksvFSIJob");
    mitk::NodePredicateDataType::Pointer isSim1dJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob1d");
    // fix???    mitk::NodePredicateDataType::Pointer issvFSIJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob");

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
    else if(isSim1dJob->CheckNode(dataNode))
    {
        extensions.push_back(".s1djb");
        extensions.push_back("");//for folder
    }
    else if(issvFSIJob->CheckNode(dataNode))
    {
        extensions.push_back(".fsijob");
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

//------------------
// DuplicateProject
//------------------
//
void sv4guiProjectManager::DuplicateProject(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode, QString newName)
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
        sv4guiProjectManager::AddProject(dataStorage,newName,projParentDir,false);
}

//-------------------------
// DuplicateDirRecursively
//-------------------------
//
bool sv4guiProjectManager::DuplicateDirRecursively(const QString &srcFilePath, const QString &tgtFilePath)
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

//--------------------
// CreateImagesPlugin
//--------------------
// Create Images plugin data nodes and read image data.
//
mitk::DataNode::Pointer 
sv4guiProjectManager::CreateImagesPlugin(mitk::DataStorage::Pointer dataStorage, QString projPath, 
    mitk::DataNode::Pointer imageFolderNode, QStringList imageFilePathList, QStringList imageNameList)
{
  imageFolderNode->SetVisibility(false);
  mitk::DataNode::Pointer imageNode;

  for (int i = 0; i < imageFilePathList.size(); i++) {
    auto imageFilePath = imageFilePathList[i].toStdString();
    std::cout << "[AddProject] imageFilePath: " << imageFilePath << std::endl;

    try {
      imageNode = sv4guiProjectManager::LoadDataNode(imageFilePath);
      imageNode->SetName(imageNameList[i].toStdString());

      // Transform the image from the values given in the .transform.xml file. 
      auto image = dynamic_cast<mitk::Image*>(imageNode->GetData());
      setTransform(image, projPath.toStdString(), imageNode->GetName());
      dataStorage->Add(imageNode,imageFolderNode);

    } catch(...) {
      MITK_ERROR << "Failed to load image (maybe non-existing or unsupported data type): " << imageFilePath;
    }
  }

  return imageNode;
}

//-------------------
// CreatePathsPlugin
//-------------------
// Create Paths plugin data nodes.
//
void sv4guiProjectManager::CreatePathsPlugin(mitk::DataStorage::Pointer dataStorage, QString projPath, 
    mitk::DataNode::Pointer pathFolderNode, QString pathFolderName)
{
  pathFolderNode->SetVisibility(false);
  QDir dir(projPath);
  dir.cd(pathFolderName);
  auto fileInfoList = dir.entryInfoList(QStringList("*.pth"), QDir::Files, QDir::Name);

  for (int i = 0; i < fileInfoList.size(); i++) {
    auto pathNode = sv4guiProjectManager::LoadDataNode(fileInfoList[i].absoluteFilePath().toStdString());
    pathNode->SetVisibility(false);
    auto path = dynamic_cast<sv4guiPath*>(pathNode->GetData());

    if (path) {
      auto props = path->GetProps();
      auto it = props.begin();
      while (it != props.end()) {
        if (it->first=="point 2D display size" || it->first=="point size") {
          if (it->second != "") {
            float value = (float)(std::stod(it->second));
            pathNode->SetFloatProperty(it->first.c_str(),value);
          }
        }
        it++;
      }
    }

    dataStorage->Add(pathNode, pathFolderNode);
  }
}

//---------------------------
// CreateSegmentationsPlugin
//---------------------------
// Create Segmentations plugin data nodes.
//
void sv4guiProjectManager::CreateSegmentationsPlugin(mitk::DataStorage::Pointer dataStorage, QString projPath, 
    mitk::DataNode::Pointer segFolderNode, QString segFolderName)
{
  segFolderNode->SetVisibility(false);
  QDir dirSeg(projPath);
  dirSeg.cd(segFolderName);
  QStringList segNameFilters;
  segNameFilters << "*.ctgr" <<"*.s3d";
  auto fileInfoList = dirSeg.entryInfoList(segNameFilters, QDir::Files, QDir::Name);

  for (int i = 0; i < fileInfoList.size(); i++) {
    auto filePath = fileInfoList[i].absoluteFilePath().toStdString();

    try {
      auto segNode = sv4guiProjectManager::LoadDataNode(filePath);
      segNode->SetVisibility(false);
      auto group = dynamic_cast<sv4guiContourGroup*>(segNode->GetData());

      if (group) {
        auto props = group->GetProps();
        auto it = props.begin();
        while(it != props.end()) {
          if (it->second!="") {
            if (it->first=="point 2D display size") {
              float value=(float)(std::stod(it->second));
              segNode->SetFloatProperty("point.displaysize",value);
            } else if (it->first=="point size") {
              float value=(float)(std::stod(it->second));
              segNode->SetFloatProperty("point.3dsize",value);
            }
          }

          it++;
        }
      }

      dataStorage->Add(segNode,segFolderNode);

    } catch(...) {
      MITK_ERROR << "Failed to load file (maybe unsupported data type): " << filePath;
    }
  }

}

//-------------------------------
// CreateMitkSegmentationsPlugin
//-------------------------------
// Create MITK segmentation data nodes
//
void sv4guiProjectManager::CreateMitkSegmentationsPlugin(mitk::DataStorage::Pointer dataStorage, QString projPath, 
    mitk::DataNode::Pointer imageNode)
{
  QDir dirSeg(projPath);
  auto fileInfoList = dirSeg.entryInfoList(QStringList("*.vti"), QDir::Files, QDir::Name);

  for (int i = 0; i < fileInfoList.size(); i++) {
    auto filePath = fileInfoList[i].absoluteFilePath().toStdString();

    try {
      auto mitkSegNode = sv4guiProjectManager::LoadDataNode(filePath);
      mitkSegNode->SetVisibility(false);
      auto image = dynamic_cast<mitk::Image*>(mitkSegNode->GetData());
      setTransform(image, projPath.toStdString(), imageNode->GetName());
      dataStorage->Add(mitkSegNode,imageNode);
    } catch(...) {
      MITK_ERROR << "Failed to load segmentation image (maybe non-existing or unsupported data type): " << filePath;
    }
  }

}

//-------------------
// CreateModelPlugin
//-------------------
// Create Model plugin data nodes.
//
void sv4guiProjectManager::CreateModelPlugin(mitk::DataStorage::Pointer dataStorage, QString projPath, 
    mitk::DataNode::Pointer modelFolderNode, QString modelFolderName)
{
  modelFolderNode->SetVisibility(false);
  QDir dirModel(projPath);
  dirModel.cd(modelFolderName);
  auto fileInfoList = dirModel.entryInfoList(QStringList("*.mdl"), QDir::Files, QDir::Name);
  bool firstModel = true;

  for (int i = 0; i < fileInfoList.size(); i++) {
    auto filePath = fileInfoList[i].absoluteFilePath().toStdString();
    try {
      auto modelNode = sv4guiProjectManager::LoadDataNode(filePath);
      if (firstModel) {
        modelNode->SetVisibility(true);
        firstModel = false;
      } else {
        modelNode->SetVisibility(false);
      }
      dataStorage->Add(modelNode,modelFolderNode);

    } catch(...) {
      MITK_ERROR << "Failed to load file (maybe unsupported data type): " << filePath;
    }
  }
}

//--------------
// CreatePlugin
//--------------
// Create generic plugin data nodes.
//
void sv4guiProjectManager::CreatePlugin(mitk::DataStorage::Pointer dataStorage, QString projPath, 
    mitk::DataNode::Pointer folderNode, QString folderName, QString fileExt)
{
  folderNode->SetVisibility(false);
  QDir dir(projPath);
  dir.cd(folderName);
  auto fileInfoList = dir.entryInfoList(QStringList(fileExt), QDir::Files, QDir::Name);

  for (int i = 0; i < fileInfoList.size();i++) {
    auto filePath = fileInfoList[i].absoluteFilePath().toStdString();

    try {
      auto node = sv4guiProjectManager::LoadDataNode(fileInfoList[i].absoluteFilePath().toStdString());
      node->SetVisibility(false);
      dataStorage->Add(node, folderNode);
    } catch(...) {
      MITK_ERROR << "Failed to load file (maybe unsupported data type): " << filePath;
    }
  }
}
