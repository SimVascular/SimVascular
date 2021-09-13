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
//     ROMSimulations: .romsimjob
//     svFSI: .fsijob

#include "sv4gui_ProjectManager.h"

#include "sv4gui_ProjectFolder.h"
#include "sv4gui_ImageFolder.h"
#include "sv4gui_PathFolder.h"
#include "sv4gui_SegmentationFolder.h"
#include "sv4gui_ModelFolder.h"
#include "sv4gui_MeshFolder.h"
#include "sv4gui_SimulationFolder.h"
#include "sv4gui_svFSIFolder.h"
#include "sv4gui_ROMSimulationFolder.h"
#include "sv4gui_RepositoryFolder.h"
#include "sv4gui_svFSIFolder.h"

#include "sv4gui_Path.h"
#include "sv4gui_ContourGroup.h"
#include "sv4gui_MitkSeg3D.h"
#include "sv4gui_Model.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MitkSimJob.h"
#include "sv4gui_MitksvFSIJob.h"
#include "sv4gui_MitkROMSimJob.h"
#include "sv4gui_MitkMeshIO.h"
#include "sv4gui_VtkUtils.h"

#include "sv4gui_ImageProcessingUtils.h"
#include <mitkImageCast.h>

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
#include <QMessageBox>
#include <QTextStream>

#include <ctime>

#include <fstream>
#include <iostream>
#include <map>
#include <regex>

// Define the name of the files used to identify the image data location.
//
// SVPROJ_CONFIG_FILE_NAME is deprecated.
//
const QString sv4guiProjectManager::SVPROJ_CONFIG_FILE_NAME = ".svproj";
const QString sv4guiProjectManager::IMAGE_OBJECT_INFORMATION_FILE_NAME = "image_information.xml";
const QString sv4guiProjectManager::SV_PROJECT_FILE_NAME = "simvascular.proj";

namespace sv4gui_project_manager {

  // Set plugin names. 
  const QString PluginNames::IMAGES = "Images";
  const QString PluginNames::MESHES = "Meshes";
  const QString PluginNames::MODELS = "Models";
  const QString PluginNames::PATHS = "Paths";
  const QString PluginNames::SEGMENTATIONS = "Segmentations";
  const QString PluginNames::SIMULATIONS = "Simulations";
  const QString PluginNames::ROMSIMULATIONS = "ROMSimulations";
  const QString PluginNames::SVFSI = "svFSI";
  const QStringList PluginNames::NAMES_LIST = {
    PluginNames::IMAGES,
    PluginNames::MESHES,
    PluginNames::MODELS,
    PluginNames::PATHS,
    PluginNames::SEGMENTATIONS,
    PluginNames::SIMULATIONS,
    PluginNames::ROMSIMULATIONS,
    PluginNames::SVFSI 
  };

  // Set plugin file extensions.
  const QString FileExtension::MESHES = ".msh";
  const QString FileExtension::MODELS = ".mdl";
  const QString FileExtension::PATHS = ".pth";
  const QString FileExtension::SEGMENTATIONS = ".ctgr";
  const QString FileExtension::SEGMENTATIONS3D = ".s3d";
  const QString FileExtension::SIMULATIONS = ".sjb";
  const QString FileExtension::ROMSIMULATIONS = ".romsimjob";
  const QString FileExtension::SVFSI = ".fsijob";

  // image file extensions
  const QString FileExtension::IMAGE_VTI = ".vti";
  const QString FileExtension::IMAGE_HEADER = ".hdr";

  // Set the element names used in the image location xml file.
  const QString XmlImageInformationElementNames::ROOT = "ImageObjectInformation";
  const QString XmlImageInformationElementNames::TIMESTEP = "timestep";
  const QString XmlImageInformationElementNames::CREATED_WITH_SIMVASCULAR_VERSION = "created_with_simvascular_version";
  const QString XmlImageInformationElementNames::IMAGE_FILE_NAME = "image_file_name";
  const QString XmlImageInformationElementNames::IMAGE_HEADER_FILE_NAME = "image_header_file_name";
  const QString XmlImageInformationElementNames::IMAGE_NAME = "image_name";
  const QString XmlImageInformationElementNames::DATA_IS_LOCAL_COPY = "data_is_local_copy";
  const QString XmlImageInformationElementNames::PATH = "path";
  const QString XmlImageInformationElementNames::SCALE_FACTOR = "scale_factor";
  const std::set<QString> XmlImageInformationElementNames::valid_names = {
    XmlImageInformationElementNames::TIMESTEP,
    XmlImageInformationElementNames::CREATED_WITH_SIMVASCULAR_VERSION,
    XmlImageInformationElementNames::IMAGE_FILE_NAME,
    XmlImageInformationElementNames::IMAGE_HEADER_FILE_NAME,
    XmlImageInformationElementNames::IMAGE_NAME,
    XmlImageInformationElementNames::PATH,
    XmlImageInformationElementNames::DATA_IS_LOCAL_COPY,
    XmlImageInformationElementNames::SCALE_FACTOR
  };

  // Set the element names used in the image header file.
  const QString XmlImageHeaderElementNames::ROOT = "ImageHeaderInformation";
  const QString XmlImageHeaderElementNames::CREATED_WITH_SIMVASCULAR_VERSION = "created_with_simvascular_version";
  const QString XmlImageHeaderElementNames::MODALITY = "modality";
  const QString XmlImageHeaderElementNames::AGE = "age";
  const QString XmlImageHeaderElementNames::GENDER = "gender";
  const QString XmlImageHeaderElementNames::ETHNICITY = "ethnicity";
  const QString XmlImageHeaderElementNames::IMAGE_IS_SCALED = "image_is_scaled";
  const QString XmlImageHeaderElementNames::SCALE_FACTOR = "scale_factor";
  const QString XmlImageHeaderElementNames::ORIGINAL_UNITS = "original_units";
  const QString XmlImageHeaderElementNames::CURRENT_UNITS = "current_units";
  const QString XmlImageHeaderElementNames::TRANSFORM_LPS = "transform_lps";
  const QString XmlImageHeaderElementNames::TRANSFORM = "transform";

  const std::set<QString> XmlImageHeaderElementNames::valid_names = {
    XmlImageHeaderElementNames::CREATED_WITH_SIMVASCULAR_VERSION,
    XmlImageHeaderElementNames::MODALITY,
    XmlImageHeaderElementNames::AGE,
    XmlImageHeaderElementNames::GENDER,
    XmlImageHeaderElementNames::ETHNICITY,
    XmlImageHeaderElementNames::IMAGE_IS_SCALED,
    XmlImageHeaderElementNames::SCALE_FACTOR,
    XmlImageHeaderElementNames::ORIGINAL_UNITS,
    XmlImageHeaderElementNames::CURRENT_UNITS,
    XmlImageHeaderElementNames::TRANSFORM_LPS,
    XmlImageHeaderElementNames::TRANSFORM,
  };

  const QString XmlProjectElementNames::ROOT = "simvascular_project";
  const QString XmlProjectElementNames::VERSION = "version";
  const std::set<QString> XmlProjectElementNames::valid_names = {
  };

}

// [TODO:DaveP] How should the version be set?
QString sv4guiProjectManager::simvascularVersion_ = "1.0";

//----------------------
// sv4guiProjectManager
//----------------------
//
sv4guiProjectManager::sv4guiProjectManager()
{
    // simvascularVersion_ = "1.0";
}

//------------
// AddProject
//------------
// Create a new project or read in an existing one.
//
void sv4guiProjectManager::AddProject(mitk::DataStorage::Pointer dataStorage, QString projName, QString projParentDir, bool newProject)
{
    using namespace sv4gui_project_manager;


    QString projectConfigFileName = sv4guiProjectManager::SVPROJ_CONFIG_FILE_NAME; 

    // Create a new project directory and cd to it.
    QDir project_dir(projParentDir);
    if (newProject) {
        project_dir.mkdir(projName);
    }
    project_dir.cd(projName);

    // Get the image information.
    //
    // imageFilePath: where the image data resides.
    // imageFileName: the name of the image file (e.g. image.vti or image001.dcm)
    // imageName: the name of SV Data Manager Images node.
    //
    QString projPath = project_dir.absolutePath();
    QString imageFilePath;
    QString imageFileName;
    QString imageHeaderFileName;
    QString imageName;
    QString simvascularVersion;

    // If a new project then create plugin directories. 
    if (newProject) {
        for (auto const& name : PluginNames::NAMES_LIST) {
            project_dir.mkdir(name);
        }
        WriteImageInfo(projPath, imageFilePath, imageFileName, imageHeaderFileName, imageName);
        WriteProjectFile(projPath);

    } else {
        ReadImageInfo(projPath, imageFilePath, imageFileName, imageHeaderFileName, imageName);
        ReadProjectFile(projPath, simvascularVersion);
    }

    // Create the SV Data Manager top level plugin mitk data nodes.
    //
    auto projectFolderNode = CreateDataFolder<sv4guiProjectFolder>(dataStorage, projName);
    projectFolderNode->AddProperty("project path",mitk::StringProperty::New(projPath.toStdString().c_str()));

    QString imageFolderName = PluginNames::IMAGES; 
    auto imageFolderNode = sv4guiProjectManager::CreateDataFolder<sv4guiImageFolder>(dataStorage, imageFolderName, projectFolderNode); 
    imageFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );

    QString pathFolderName =  PluginNames::PATHS; 
    auto pathFolderNode = CreateDataFolder<sv4guiPathFolder>(dataStorage, pathFolderName, projectFolderNode);
    pathFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );

    QString segFolderName = PluginNames::SEGMENTATIONS;  
    auto segFolderNode = CreateDataFolder<sv4guiSegmentationFolder>(dataStorage,segFolderName, projectFolderNode);
    segFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );

    QString modelFolderName = PluginNames::MODELS;
    auto modelFolderNode = CreateDataFolder<sv4guiModelFolder>(dataStorage, modelFolderName, projectFolderNode);
    modelFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );

    QString meshFolderName = PluginNames::MESHES; 
    auto meshFolderNode = CreateDataFolder<sv4guiMeshFolder>(dataStorage, meshFolderName, projectFolderNode);
    meshFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );

    QString simFolderName = PluginNames::SIMULATIONS;
    auto simFolderNode = CreateDataFolder<sv4guiSimulationFolder>(dataStorage, simFolderName, projectFolderNode);
    simFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );

    QString svFSIFolderName = PluginNames::SVFSI;
    auto svFSIFolderNode = CreateDataFolder<sv4guisvFSIFolder>(dataStorage, svFSIFolderName, projectFolderNode);
    svFSIFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );

    QString romSimFolderName = PluginNames::ROMSIMULATIONS;
    auto romSimFolderNode = CreateDataFolder<sv4guiROMSimulationFolder>(dataStorage, romSimFolderName, projectFolderNode);
    romSimFolderNode->AddProperty("previous visibility",mitk::BoolProperty::New(false) );

    if (newProject) {
        mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(dataStorage);
        return;
    }

    // Create Images plugin data nodes and read image data.
    //
    // Note: imageFileName == "" if the project was created without adding an image.
    //
    if (imageFileName != "") {
      auto imageNode = CreateImagesPlugin(dataStorage, projPath, imageFolderNode, imageFilePath, imageFileName, imageHeaderFileName, imageName);
      CreateMitkSegmentationsPlugin(dataStorage, projPath, imageNode);
    }

    // Create Paths plugin data nodes.
    CreatePathsPlugin(dataStorage, projPath, pathFolderNode, pathFolderName);

    // Create Segmentations plugin data nodes.
    CreateSegmentationsPlugin(dataStorage, projPath, segFolderNode, segFolderName);

    // Create Model plugin data nodes.
    CreateModelPlugin(dataStorage, projPath, modelFolderNode, modelFolderName);

    // Create Meshes plugin data nodes.
    CreatePlugin(dataStorage, projPath, meshFolderNode, meshFolderName, FileExtension::MESHES);

    // Create Simulations plugin data nodes.
    CreatePlugin(dataStorage, projPath, simFolderNode, simFolderName, FileExtension::SIMULATIONS);

    // Create svFSI plugin data nodes.
    CreatePlugin(dataStorage, projPath, svFSIFolderNode, svFSIFolderName, FileExtension::SVFSI);

    // Create ROMSimulations plugin data nodes.
    UpdateSimulations1dFolder(projPath, romSimFolderName);
    CreatePlugin(dataStorage, projPath, romSimFolderNode, romSimFolderName, FileExtension::ROMSIMULATIONS);

    mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(dataStorage);
}

//---------------
// ReadImageInfo
//---------------
// Read the path to image data, the image file name, and the image name from a file.
//
// The information is read from the IMAGE_OBJECT_INFORMATION_FILE_NAME file if it exists. 
// If not then it is read from the SVPROJ_CONFIG_FILE_NAME file. 
//
// Arguments
//   projPath: The path to the SV project.
//
// Returns:
//   imageFilePath: The path to the image file. If empty then the image file is in the project's Images directory.
//   imageFileName: The name of the image file.
//   imageHeaderFileName: The name of the image transform file.
//   imageName: The name of the image.
//
void sv4guiProjectManager::ReadImageInfo(const QString& projPath, QString& imageFilePath, QString& imageFileName, 
     QString& imageHeaderFileName, QString& imageName)
{
  using namespace sv4gui_project_manager;
  auto imageFolderName = PluginNames::IMAGES;
  QDir projec_dir(projPath);
  auto imageLocFilePath = GetImageInfoFilePath(projec_dir);

  // If there is no image location file then read the old SVPROJ_CONFIG_FILE_NAME file.
  //
  QString imagePath;
  QFileInfo checkFile(imageLocFilePath);
  if (!checkFile.exists()) { 
    std::cout << "[ReadImageInfo] Read .svproj file." << std::endl;
    QStringList imageFilePathList;
    QStringList imageNameList;
    bool localFile; 
    ReadImageInfoFromSvproj(projPath, imageFilePathList, imageNameList, localFile);
    // There may be not image information in the file.
    if ((imageFilePathList.size() == 0) || (imageNameList.size() == 0)) {
      imageFilePath = "";
      imageFileName = "";
      imageHeaderFileName = "";
      imageName = "";
      return;
    }
    imageFilePath = imageFilePathList[0];
    imageName = imageNameList[0];
    // Get separate file path and file name.
    QFileInfo fileInfo(imageFilePath);
    imageFilePath = fileInfo.path();
    imageFileName = fileInfo.fileName();
    imageHeaderFileName = imageName+".transform.xml" ;
  } else {
    ReadImageInfoFromImageLoc(imageLocFilePath, imageFilePath, imageFileName, imageHeaderFileName, imageName);
  }
}

//---------------------------
// ReadImageInfoFromImageLoc
//---------------------------
// Read the path to image data and the image name from a IMAGE_OBJECT_INFORMATION_FILE_NAME file.
//
void sv4guiProjectManager::ReadImageInfoFromImageLoc(QString imageLocFilePath, QString& imageFilePath, QString& imageFileName, 
       QString& imageHeaderFileName, QString& imageName)
{
  using namespace sv4gui_project_manager;

  // Read IMAGE_OBJECT_INFORMATION_FILE_NAME xml file.
  QDomDocument doc("image_location");
  QFile xmlFile(imageLocFilePath);
  xmlFile.open(QIODevice::ReadOnly);
  QString *em = NULL;
  doc.setContent(&xmlFile, em);
  xmlFile.close();

  // Parse xml file data.
  QDomElement docElem = doc.documentElement();

  // hardcoding here to expect only one timestep
  QDomNode imageobjectnode = docElem.firstChild();
  QDomNode node = imageobjectnode.firstChild();

  while (!node.isNull()) {
    QDomElement element = node.toElement(); 
    if (element.isNull()) {
      continue;
    }
    auto tagName = element.tagName();

    if (XmlImageInformationElementNames::valid_names.count(tagName) == 0) { 
      auto msg = "An unknown element '" + tagName + "' was found in the image location file '" + imageLocFilePath + "'.";
      QMessageBox::warning(NULL, "", msg);
    }

    if (tagName == XmlImageInformationElementNames::IMAGE_NAME) {
      imageName = element.text(); 

    } else if (tagName == XmlImageInformationElementNames::IMAGE_FILE_NAME) {
      imageFileName = element.text(); 

    } else if (tagName == XmlImageInformationElementNames::IMAGE_HEADER_FILE_NAME) {
      imageHeaderFileName = element.text(); 

    } else if (tagName == XmlImageInformationElementNames::PATH) {
      imageFilePath = element.text(); 
    }

    node = node.nextSibling();
  }
}

//-------------------------
// ReadImageInfoFromSvproj
//-------------------------
// Read the path to image data and the image name from a SVPROJ_CONFIG_FILE_NAME file.
//
void sv4guiProjectManager::ReadImageInfoFromSvproj(const QString& projPath, QStringList& imageFilePathList, QStringList& imageNameList,
    bool& localFile)
{

  using namespace sv4gui_project_manager;
  
  QDir projec_dir(projPath);
  QString svprojFilePath = projec_dir.absoluteFilePath(sv4guiProjectManager::SVPROJ_CONFIG_FILE_NAME);
  QFileInfo checkFile(svprojFilePath);

  if (!checkFile.exists()) { 
    MITK_ERROR << "No .svproj file '" << svprojFilePath.toStdString() << "' found for the project.";
    return;
  }
  
  // Read SVPROJ_CONFIG_FILE_NAME xml file.
  //
  QDomDocument doc("svproj");
  QFile xmlFile(svprojFilePath);
  xmlFile.open(QIODevice::ReadOnly);
  QString *em = NULL;
  doc.setContent(&xmlFile, em);
  xmlFile.close();

  QDomElement projDesc = doc.firstChildElement("projectDescription");
  QDomElement imagesElement = projDesc.firstChildElement("images");
  auto imageFolderName = imagesElement.attribute("folder_name");
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
      localFile = true;
      if (imagePath != "") {
        imageFilePathList << (projPath+"/"+imageFolderName+"/"+imagePath);
      } else if(imageName != "") {
        imageFilePathList << (projPath+"/"+imageFolderName+"/"+imageName);
        imageElement.setAttribute("path", imageName);
        imageElement.setAttribute("name", imageName.remove(FileExtension::IMAGE_VTI));
        rewriteProjectConfigFile = true;
      }
      imageNameList << imageName;

    } else {
      imageFilePathList << imageElement.attribute("path");
      localFile = false;

      if (imageName == "") {
        imageName = "image";
        imageElement.setAttribute("name",imageName);
        rewriteProjectConfigFile = true;
      }
      imageNameList << imageName;
    }
  }
}

//----------------
// WriteImageInfo
//----------------
// Write the location of image data and the image name to the IMAGE_OBJECT_INFORMATION_FILE_NAME file.
//
// Arguments
//   projPath: The path to the SV project.
//   imageFilePath: The path to the image file. If empty then the image file is in the project's Images directory.
//   imageFileName: The name of the image file. 
//   imageName: The name of the image. 
//
void sv4guiProjectManager::WriteImageInfo(const QString& projPath, const QString& imageFilePath, const QString& imageFileName,
					  const QString& imageHeaderFileName, const QString& imageName, bool copyIntoProject, double scaleFactor)
{
    using namespace sv4gui_project_manager;

    // Create XML doc.
    QDomDocument doc;
    QDomNode xmlNode = doc.createProcessingInstruction("xml", "version=\"1.0\" encoding=\"UTF-8\"");
    doc.appendChild(xmlNode);

    std::time_t modtime = std::time(nullptr);

    // Add data.
    QDomElement root = doc.createElement(XmlImageInformationElementNames::ROOT);
    root.setAttribute("version", "1.0");
    root.setAttribute("creation_time", QString::fromStdString(std::to_string(modtime))); 
    root.setAttribute("modification_time", QString::fromStdString(std::to_string(modtime)));
    doc.appendChild(root);

    QDomElement timestep = doc.createElement(XmlImageInformationElementNames::TIMESTEP);
    timestep.setAttribute("id","0");
    root.appendChild(timestep);

    auto generatedWithElem = doc.createElement(XmlImageInformationElementNames::CREATED_WITH_SIMVASCULAR_VERSION);
#ifndef SV_FULL_VER_NO
    auto generatedWithText = doc.createTextNode("0");
#else
    auto generatedWithText = doc.createTextNode(SV_FULL_VER_NO);
#endif
    generatedWithElem.appendChild(generatedWithText);
    timestep.appendChild(generatedWithElem);

    auto pathElem = doc.createElement(XmlImageInformationElementNames::PATH);
    auto pathText = doc.createTextNode(imageFilePath);
    pathElem.appendChild(pathText);
    timestep.appendChild(pathElem);

    auto fileNameElem = doc.createElement(XmlImageInformationElementNames::IMAGE_FILE_NAME);
    auto fileNameText = doc.createTextNode(imageFileName);
    fileNameElem.appendChild(fileNameText);
    timestep.appendChild(fileNameElem);

    auto transFileNameElem = doc.createElement(XmlImageInformationElementNames::IMAGE_HEADER_FILE_NAME);
    auto transFileNameText = doc.createTextNode(imageHeaderFileName);
    transFileNameElem.appendChild(transFileNameText);
    timestep.appendChild(transFileNameElem);
  
    auto nameElem = doc.createElement(XmlImageInformationElementNames::IMAGE_NAME);
    auto nameText = doc.createTextNode(imageName);
    nameElem.appendChild(nameText);
    timestep.appendChild(nameElem);

    QString boolText = copyIntoProject ? "true" : "false";
    auto copiedWithElem = doc.createElement(XmlImageInformationElementNames::DATA_IS_LOCAL_COPY);
    auto copiedWithText = doc.createTextNode(boolText);
    copiedWithElem.appendChild(copiedWithText);
    timestep.appendChild(copiedWithElem);
   
    auto scaleElem = doc.createElement(XmlImageInformationElementNames::SCALE_FACTOR);
    auto scaleText = doc.createTextNode(QString::number(scaleFactor));
    scaleElem.appendChild(scaleText);
    timestep.appendChild(scaleElem);

    // Write the data to a file.
    QDir project_dir(projPath);
    QString xml = doc.toString(4);
    QString filePath = GetImageInfoFilePath(project_dir);
    QFile file(filePath);

    if (file.open(QFile::WriteOnly | QFile::Truncate)) {
        QTextStream out(&file);
        out << xml <<endl;
    }
}

//-----------------
// ReadProjectFile
//-----------------
// Read the SimVascular project file.
//
void sv4guiProjectManager::ReadProjectFile(const QString& projPath, QString& version)
{
  using namespace sv4gui_project_manager;
  QString projFilePath = projPath + "/" + sv4guiProjectManager::SV_PROJECT_FILE_NAME;

  QFileInfo checkFile(projFilePath);
  if (!checkFile.exists()) { 
      return;
  }

  QDomDocument doc(XmlProjectElementNames::ROOT);
  QFile xmlFile(projFilePath);
  xmlFile.open(QIODevice::ReadOnly);
  QString *em = NULL;
  doc.setContent(&xmlFile, em);
  xmlFile.close();

  // Get ROOT attributes.
  QDomNode root = doc.namedItem(XmlProjectElementNames::ROOT);
  version = root.attributes().namedItem(XmlProjectElementNames::VERSION).nodeValue();

  // Parse xml file data.
  QDomElement docElem = doc.documentElement();
  QDomNode projObjectNode = docElem.firstChild();
  QDomNode node = projObjectNode.firstChild();

  while (!node.isNull()) {
    QDomElement element = node.toElement(); 
    if (element.isNull()) {
      continue;
    }
    auto tagName = element.tagName();

    if (XmlProjectElementNames::valid_names.count(tagName) == 0) { 
      auto msg = "An unknown element '" + tagName + "' was found in the project file '" + projFilePath + "'.";
      QMessageBox::warning(NULL, "", msg);
    }

    node = node.nextSibling();
  }
}

//------------------
// WriteProjectFile
//------------------
// Writte the SimVascular project file.
//
void sv4guiProjectManager::WriteProjectFile(const QString& projPath)
{
    using namespace sv4gui_project_manager;

    // Create XML doc.
    QDomDocument doc;
    QDomNode xmlNode = doc.createProcessingInstruction("xml", "version=\"1.0\" encoding=\"UTF-8\"");
    doc.appendChild(xmlNode);

    std::time_t modtime = std::time(nullptr);

    // Add data.
    QDomElement root = doc.createElement(XmlProjectElementNames::ROOT);
    root.setAttribute(XmlProjectElementNames::VERSION, sv4guiProjectManager::simvascularVersion_);
    doc.appendChild(root);

    // Write the data to a file.
    QDir project_dir(projPath);
    QString xml = doc.toString(4);
    QString filePath = projPath + "/" + sv4guiProjectManager::SV_PROJECT_FILE_NAME;
    QFile file(filePath);

    if (file.open(QFile::WriteOnly | QFile::Truncate)) {
        QTextStream out(&file);
        out << xml <<endl;
    }
}

//----------------------
// GetImageInfoFilePath
//----------------------
// Get the path to the image location file IMAGE_OBJECT_INFORMATION_FILE_NAME.
//
// The IMAGE_OBJECT_INFORMATION_FILE_NAME file is currently store in the 
// project's 'Images' directory.
//
QString sv4guiProjectManager::GetImageInfoFilePath(QDir project_dir)
{
  using namespace sv4gui_project_manager;
  return project_dir.absoluteFilePath(PluginNames::IMAGES + "/" + sv4guiProjectManager::IMAGE_OBJECT_INFORMATION_FILE_NAME);
}

//----------
// AddImage
//----------
// Add image data to the SV Data Manager Images node.
//
// If 'copyIntoProject' is true then the image data is copied into the project as a .vti file.
//
void sv4guiProjectManager::AddImage(mitk::DataStorage::Pointer dataStorage, QString imageFilePath, mitk::DataNode::Pointer imageNode, 
      mitk::DataNode::Pointer imageFolderNode, bool copyIntoProject, double scaleFactor, QString addImageName)
{
  using namespace sv4gui_project_manager;
  mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(dataStorage);

  // Get the project path. 
  auto isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
  auto rs = dataStorage->GetSources(imageFolderNode, isProjFolder);
  auto projectFolderNode = rs->GetElement(0);
  std::string projPath;
  projectFolderNode->GetStringProperty("project path", projPath);

  // Set the new image name.
  auto newImageName = addImageName.toStdString();
  if (newImageName == "") {
    newImageName = "image";
    if (projectFolderNode.IsNotNull()) {
      newImageName = projectFolderNode->GetName();
    }
  }
  imageNode->SetName(newImageName);

  // Read image path and name information from a file.
  QString imageFolderName = PluginNames::IMAGES; 
  QString oldImageFilePath; 
  QString oldImageFileName;
  QString oldTransformFileName;
  QString oldImageName;

  ReadImageInfo(QString(projPath.c_str()), oldImageFilePath, oldImageFileName, oldTransformFileName, oldImageName);
  auto imagePath = QString(projPath.c_str()) + "/" + PluginNames::IMAGES + "/" + oldImageName;

  // If the old image is local then remove it and the transform file if it exists.
  if (oldImageFilePath == "") {
    QString oldImageAbsoluteFileName = QString::fromStdString(projPath+"/"+imageFolderNode->GetName()+"/"+oldImageFileName.toStdString());
    QFile oldImageAbsoluteFile(oldImageAbsoluteFileName);
    oldImageAbsoluteFile.remove();
    // Remove transform file if it exists.
    QString oldTransformAbsoluteFileName = oldImageAbsoluteFileName+FileExtension::IMAGE_HEADER;
    QFile oldTransformAbsoluteFile(oldTransformAbsoluteFileName);
    oldTransformAbsoluteFile.remove();
  }

  // Remove current Image data node if it exists.
  auto nodesToRemove = dataStorage->GetDerivations(imageFolderNode, nullptr, false);
  if (!nodesToRemove->empty()) {
    dataStorage->Remove(nodesToRemove);
  }

  // Copy image data to the project as a VTK VTI file and optionally scale it.
  //
  QString newImageFileName;
  QString newImageFilePath;
  QString newTransformFileName;
  if (copyIntoProject) {
    CopyImageToProject(projPath, imageNode, imageFolderNode, scaleFactor, newImageFileName, newTransformFileName);
    newImageFilePath = "";
  } else {
    QFileInfo fileInfo(imageFilePath);
    newImageFilePath = fileInfo.path();
    newImageFileName = fileInfo.fileName();
    newTransformFileName = fileInfo.fileName()+FileExtension::IMAGE_HEADER;
  }

  // Write the image location file.
  WriteImageInfo(QString(projPath.c_str()), newImageFilePath, newImageFileName, newTransformFileName, QString(newImageName.c_str()), copyIntoProject, scaleFactor);

  dataStorage->Add(imageNode, imageFolderNode);
  mitk::RenderingManager::GetInstance()->InitializeViews(imageNode->GetData()->GetTimeGeometry(), mitk::RenderingManager::REQUEST_UPDATE_ALL, true );

  // Set the imageNode's path to the image data; used in sv4guiSeg2DEdit::initialize().
  QString imageAbsoluteFileName;
  if (newImageFilePath == "") {
    imageAbsoluteFileName = QString(projPath.c_str()) + "/" + PluginNames::IMAGES + "/" + newImageFileName;
  } else {
    imageAbsoluteFileName = newImageFilePath + "/" + newImageFileName;
  }

  imageNode->SetStringProperty("image_absolute_file_name", imageAbsoluteFileName.toStdString().c_str());
}

//--------------------
// CopyImageToProject
//--------------------
// Copy image data into the project's 'Images' directory as a VTK VTI file.
//
// This will optionally scale the image data.
//
void sv4guiProjectManager::CopyImageToProject(const std::string& projPath, mitk::DataNode::Pointer imageNode, 
					      mitk::DataNode::Pointer imageFolderNode, double scaleFactor, QString& imageFileName,
					      QString& imageHeaderFileName)
{
  using namespace sv4gui_project_manager;
  
  // image data is hardcoded to be image name + .vti here
  QString imageName = QString::fromStdString(imageNode->GetName());
  imageFileName = imageName + FileExtension::IMAGE_VTI;
  std::string imageParentPath = projPath + "/" + imageFolderNode->GetName();
  std::string imageAbsoluteFileName = imageParentPath + "/" + imageFileName.toStdString();
  imageHeaderFileName = imageFileName + FileExtension::IMAGE_HEADER;
 
  // storing the path to the image dir in the image node
  imageNode->SetStringProperty("path", imageParentPath.c_str());

  imageNode->SetStringProperty("image_file_name", (imageFileName.toStdString()).c_str());
  imageNode->SetStringProperty("image_absolute_file_name", imageAbsoluteFileName.c_str());

  // need to use full path when reading / writing transform file
  std::string imageHeaderAbsoluteFileName = imageParentPath + "/" + imageHeaderFileName.toStdString();

  // storing the path to the image dir in the image node
  imageNode->SetStringProperty("transform_file_name", (imageHeaderFileName.toStdString()).c_str());
  imageNode->SetStringProperty("transform_absolute_file_name", imageHeaderAbsoluteFileName.c_str());

  mitk::Image* image = dynamic_cast<mitk::Image*>(imageNode->GetData());

  if (!image) {
    return;
  }

  if ((scaleFactor > 0.0) && (scaleFactor != 1.0)) {
    mitk::Point3D org = image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetOrigin();
    mitk::Vector3D spacing = image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetSpacing();
    org[0] *= scaleFactor;
    org[1] *= scaleFactor;
    org[2] *= scaleFactor;
    image->SetOrigin(org);
    image->SetSpacing(scaleFactor*spacing);
  }

  // Write the original DICOM image header
  std::string modality = "unknown";
  int age = 0;
  std::string gender = "unknown";
  std::string ethnicity = "unknown";
  std::string original_units = "mm";
  bool scale_volume;
  std::string scaled_units;
  if ((scaleFactor > 0.0) && (scaleFactor != 1.0)) {
    scale_volume = true;
    scaled_units = "cm";
  } else {
    scale_volume = true;
    scaled_units = "mm";
  }
  
  writeImageHeaderFile(image, modality, age, gender, ethnicity,
		       scale_volume, scaleFactor, original_units, scaled_units, 
		       imageHeaderAbsoluteFileName);

  // Write the .vti file.
  vtkImageData* vtkImg = sv4guiVtkUtils::MitkImage2VtkImage(image);
  if (vtkImg) {
    vtkSmartPointer<vtkXMLImageDataWriter> writer = vtkSmartPointer<vtkXMLImageDataWriter>::New();
    writer->SetFileName(imageAbsoluteFileName.c_str());
    writer->SetInputData(vtkImg);
    writer->Write();
  }
}

//--------------------
// writeTransformFile
//--------------------
// Write a file containing the DICOM image axes.
//
void sv4guiProjectManager::writeTransformFile(mitk::Image* image, std::string imageHeaderAbsoluteFileName)
{

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

  std::cout << std::endl << "Writing to transform file (" << imageHeaderAbsoluteFileName << ")..." << std::endl << std::endl;
  
  QFile file( QString::fromStdString(imageHeaderAbsoluteFileName ) );

  if (file.open(QFile::WriteOnly | QFile::Truncate)) {
      QTextStream out(&file);
      out << xml <<endl;
  }
}

//---------------------
// setTransformFromFile
//---------------------
// Set the image transformation from a file.
//
// This sets the upper 3x3 elements in the vtkMatrix4x4 object.
//
void sv4guiProjectManager::setTransformFromFile(mitk::Image* image, std::string imageHeaderAbsoluteFileName)
{

  QDomDocument doc("transform");

  QFile xmlFile(QString::fromStdString(imageHeaderAbsoluteFileName));
  if (xmlFile.open(QIODevice::ReadOnly)){
    QString *em=NULL;
    doc.setContent(&xmlFile,em);
    xmlFile.close();

    auto root = doc.firstChildElement("Transform").firstChildElement("transform");
    auto transform = image->GetGeometry()->GetVtkMatrix();

    for (int j = 0; j < 3; j++){
      for (int i = 0; i < 3; i++){
        auto label = "t"+std::to_string(i)+std::to_string(j);
        auto line = root.attribute(QString::fromStdString(label));
        auto v = std::stod(line.toStdString());
        transform->SetElement(i,j,v);
      }
    }

    image->GetGeometry()->SetIndexToWorldTransformByVtkMatrix(transform);
    image->UpdateOutputInformation();

  } else {
    
    // note: no transform file found! Warn user as this is the norm!
    std::cout << "NOTE: no transform file found (" << imageHeaderAbsoluteFileName <<")" << std::endl;

  }

}


//---------------------
// writeImageHeaderFile
//---------------------
// Write a file containing the DICOM header info and transformation matrix.
//
void sv4guiProjectManager::writeImageHeaderFile(mitk::Image* image,
				     std::string modality, int age, std::string gender, std::string ethnicity,
				     bool scale_volume, double scaleFactor, std::string original_units, std::string scaled_units, 
				     std::string imageHeaderAbsoluteFileName)
{

    using namespace sv4gui_project_manager;

    auto transform = image->GetGeometry()->GetVtkMatrix();
   
    // Create XML doc.
    QDomDocument doc;
    QDomNode xmlNode = doc.createProcessingInstruction("xml", "version=\"1.0\" encoding=\"UTF-8\"");
    doc.appendChild(xmlNode);

    std::time_t modtime = std::time(nullptr);

    // Add data.
    QDomElement root = doc.createElement(XmlImageHeaderElementNames::ROOT);
    root.setAttribute("version", "1.0");
    root.setAttribute("creation_time", QString::fromStdString(std::to_string(modtime))); 
    root.setAttribute("modification_time", QString::fromStdString(std::to_string(modtime)));
    doc.appendChild(root);

    auto generatedWithElem = doc.createElement(XmlImageHeaderElementNames::CREATED_WITH_SIMVASCULAR_VERSION);
#ifndef SV_FULL_VER_NO
    auto generatedWithText = doc.createTextNode("0");
#else
    auto generatedWithText = doc.createTextNode(SV_FULL_VER_NO);
#endif
    generatedWithElem.appendChild(generatedWithText);
    root.appendChild(generatedWithElem);

    QString q_modality = QString::fromStdString(modality);
    QString q_age = QString::fromStdString(std::to_string(age));
    QString q_gender = QString::fromStdString(gender);
    QString q_ethnicity = QString::fromStdString(ethnicity);
    QString q_image_is_scaled = scale_volume ? "true" : "false";
    QString q_scale_factor = QString::fromStdString(std::to_string(scaleFactor));
    QString q_original_units = QString::fromStdString(original_units);
    QString q_current_units = QString::fromStdString(scaled_units);
  
    auto modalityElem = doc.createElement(XmlImageHeaderElementNames::MODALITY);
    auto modalityText = doc.createTextNode(q_modality);
    modalityElem.appendChild(modalityText);
    root.appendChild(modalityElem);

    auto ageElem = doc.createElement(XmlImageHeaderElementNames::AGE);
    auto ageText = doc.createTextNode(q_age);
    ageElem.appendChild(ageText);
    root.appendChild(ageElem);

    auto genderElem = doc.createElement(XmlImageHeaderElementNames::GENDER);
    auto genderText = doc.createTextNode(q_gender);
    genderElem.appendChild(genderText);
    root.appendChild(genderElem);

    auto ethnicityElem = doc.createElement(XmlImageHeaderElementNames::ETHNICITY);
    auto ethnicityText = doc.createTextNode(q_ethnicity);
    ethnicityElem.appendChild(ethnicityText);
    root.appendChild(ethnicityElem);

    auto image_is_scaledElem = doc.createElement(XmlImageHeaderElementNames::IMAGE_IS_SCALED);
    auto image_is_scaledText = doc.createTextNode(q_image_is_scaled);
    image_is_scaledElem.appendChild(image_is_scaledText);
    root.appendChild(image_is_scaledElem);

    auto scale_factorElem = doc.createElement(XmlImageHeaderElementNames::SCALE_FACTOR);
    auto scale_factorText = doc.createTextNode(q_scale_factor);
    scale_factorElem.appendChild(scale_factorText);
    root.appendChild(scale_factorElem);

    auto original_unitsElem = doc.createElement(XmlImageHeaderElementNames::ORIGINAL_UNITS);
    auto original_unitsText = doc.createTextNode(q_original_units);
    original_unitsElem.appendChild(original_unitsText);
    root.appendChild(original_unitsElem);

    auto current_unitsElem = doc.createElement(XmlImageHeaderElementNames::CURRENT_UNITS);
    auto current_unitsText = doc.createTextNode(q_current_units);
    current_unitsElem.appendChild(current_unitsText);
    root.appendChild(current_unitsElem);
    
    auto transform_lpsElem = doc.createElement(XmlImageHeaderElementNames::TRANSFORM_LPS);
    //auto transform_lpsText = doc.createTextNode();
    //transform_lpsElem.appendChild(transform_lpsText);
 
    for (int j = 0; j < 3; j++){
      for (int i = 0; i < 3; i++){
        auto v = transform->GetElement(i,j);
        auto label = "t"+std::to_string(i)+std::to_string(j);
        transform_lpsElem.setAttribute(QString::fromStdString(label),v);
      }
    }

    root.appendChild(transform_lpsElem);

    QString xml = doc.toString(4);

    std::cout << std::endl << "Writing to transform file (" << imageHeaderAbsoluteFileName << ")..." << std::endl << std::endl;
  
    QFile file( QString::fromStdString(imageHeaderAbsoluteFileName ) );

    if (file.open(QFile::WriteOnly | QFile::Truncate)) {
      QTextStream out(&file);
      out << xml <<endl;
    }
}


//--------------------------------
// setTransformFromImageHeaderFile
//--------------------------------
// Set the image transformation from a file.
//
// This sets the upper 3x3 elements in the vtkMatrix4x4 object.
//
void sv4guiProjectManager::setTransformFromImageHeaderFile(mitk::Image* image, std::string imageHeaderAbsoluteFileName)
{
  //std::cout << "============================ sv4guiProjectManager::setTransformFromImageHeaderFile =====================" << std::endl;
  using namespace sv4gui_project_manager;

  std::cout << std::endl << "Reading from transform file (" << imageHeaderAbsoluteFileName << ")..." << std::endl << std::endl;
  
  bool boolFoundTransform = false;
  QString q_imageHeaderAbsoluteFileName = QString::fromStdString(imageHeaderAbsoluteFileName);
   
  // Read IMAGE_HEADER_FILE_NAME xml file.
  QDomDocument doc("image_header");
  QFile xmlFile( q_imageHeaderAbsoluteFileName);
  xmlFile.open(QIODevice::ReadOnly);
  QString *em = NULL;
  doc.setContent(&xmlFile, em);
  xmlFile.close();

  // Parse xml file data.
  QDomElement docElem = doc.documentElement();

  // hardcoding here to only interact with first child
  
  //  QDomNode imageobjectnode = docElem.firstChild();
  QDomNode node = docElem.firstChild();

  while (!node.isNull()) {
    QDomElement element = node.toElement(); 
    if (element.isNull()) {
      continue;
    }
    auto tagName = element.tagName();

    if (XmlImageHeaderElementNames::valid_names.count(tagName) == 0) { 
      auto msg = "An unknown element '" + tagName + "' was found in the image header file '" + q_imageHeaderAbsoluteFileName + "'.";
      QMessageBox::warning(NULL, "", msg);
    }

    if (tagName == XmlImageHeaderElementNames::TRANSFORM_LPS) {

       boolFoundTransform = true;
       auto transform = image->GetGeometry()->GetVtkMatrix();
	
       for (int j = 0; j < 3; j++){
        for (int i = 0; i < 3; i++){
          auto label = "t"+std::to_string(i)+std::to_string(j);
          auto line = element.attribute(QString::fromStdString(label));
          auto v = std::stod(line.toStdString());
          transform->SetElement(i,j,v);
        }
      }

      // Setting the ITK transform (m_IndexToWorldTransform) using a vtkMatrix4x4.
      // The spacing of the new transform is copied to m_spacing.
      image->GetGeometry()->SetIndexToWorldTransformByVtkMatrix(transform);
      image->UpdateOutputInformation();

    } else if (tagName == XmlImageHeaderElementNames::TRANSFORM) {

      // legacy read  -- deprecated
      boolFoundTransform = true;
      
      auto transform = image->GetGeometry()->GetVtkMatrix();

       for (int j = 0; j < 3; j++){
        for (int i = 0; i < 3; i++){
          auto label = "t"+std::to_string(i)+std::to_string(j);
          auto line = element.attribute(QString::fromStdString(label));
          auto v = std::stod(line.toStdString());
          transform->SetElement(i,j,v);
        }
      }

      image->GetGeometry()->SetIndexToWorldTransformByVtkMatrix(transform);
      image->UpdateOutputInformation();      
    }

    node = node.nextSibling();
  }

  /* [DaveP] Leave this here for now, might need it later.
  auto origin = image->GetGeometry()->GetOrigin();
  std::cout << "[setTransformFromImageHeaderFile] image origin: "  << origin[0] << " " << origin[1] << " " << origin[2] << std::endl; 

  auto spacing = image->GetGeometry()->GetSpacing();
  std::cout << "[setTransformFromImageHeaderFile] image spacing: "  << spacing[0] << " " << spacing[1] << " " << 
          spacing[2] << std::endl; 

  auto dims = image->GetDimensions();
  std::cout << "[setTransformFromImageHeaderFile] image dims: "  << dims[0] << " " << dims[1] << " " << dims[2] << std::endl; 

  auto mat = image->GetGeometry()->GetIndexToWorldTransform()->GetMatrix();
  std::cout << "[setTransformFromImageHeaderFile] GetIndexToWorldTransform matrix: " << std::endl;
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++) {
      std::cout << mat[i][j] << " ";
    }
    std::cout << std::endl;
  }

  sv4guiImageProcessingUtils::itkImPoint itkImage = sv4guiImageProcessingUtils::itkImageType::New();
  mitk::CastToItkImage(image, itkImage);

  auto direction_cos = itkImage->GetDirection();

  std::cout << "[setTransformFromImageHeaderFile] itkImage direction_cos: " << std::endl;
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
      std::cout << direction_cos[i][j] << " ";
    }
    std::cout << std::endl;
  }

  auto itk_origin = itkImage->GetOrigin();
  std::cout << "[setTransformFromImageHeaderFile] itkImage origin: "  << itk_origin[0] << " " << itk_origin[1] << " " << itk_origin[2] << std::endl;
  */

  return;

  if (!boolFoundTransform) {
    // note: no transform file found! Warn user as this is the norm!
    std::cout << "NOTE: no transform file found (" << imageHeaderAbsoluteFileName <<")" << std::endl;

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
    using namespace sv4gui_project_manager;
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
        if(path==NULL || (!path->IsDataModified() && dir.exists(QString::fromStdString(node->GetName())+FileExtension::PATHS)) )
            continue;

        QString	filePath=dir.absoluteFilePath(QString::fromStdString(node->GetName())+FileExtension::PATHS);
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        node->SetStringProperty("path",dir.absolutePath().toStdString().c_str());

        path->SetDataModified(false);
    }

    //delete files using removeList
    for(int i=0;i<removeList.size();i++)
    {
        dir.remove(QString::fromStdString(removeList[i])+FileExtension::PATHS);
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
            QString filePath=dirSeg.absoluteFilePath(QString::fromStdString(node->GetName())+FileExtension::IMAGE_VTI);
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

    // Save ROMsimulations jobs.
    rs = dataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiROMSimulationFolder"));
    mitk::DataNode::Pointer romSimFolderNode = rs->GetElement(0);
    std::string romSimFolderName = romSimFolderNode->GetName();
    sv4guiROMSimulationFolder* romSimFolder = dynamic_cast<sv4guiROMSimulationFolder*>(romSimFolderNode->GetData());
    removeList.clear();
    if (romSimFolder) {
        removeList = romSimFolder->GetNodeNamesToRemove();
    }
    rs = dataStorage->GetDerivations(romSimFolderNode,mitk::NodePredicateDataType::New("sv4guiMitkROMSimJob"));

    QDir dirROMSim(QString::fromStdString(projPath));
    dirROMSim.cd(QString::fromStdString(romSimFolderName));

    for(int i=0;i<rs->size();i++) {
        mitk::DataNode::Pointer node=rs->GetElement(i);

        for(int j=removeList.size()-1;j>-1;j--) {
            if(removeList[j]==node->GetName())
                removeList.erase(removeList.begin()+j);
        }

        sv4guiMitkROMSimJob *mitkJob=dynamic_cast<sv4guiMitkROMSimJob*>(node->GetData());
        if(mitkJob==NULL || (!mitkJob->IsDataModified() && dirROMSim.exists(QString::fromStdString(node->GetName())+".romsimjob")) )
            continue;

        QString	filePath=dirROMSim.absoluteFilePath(QString::fromStdString(node->GetName())+".romsimjob");
        mitk::IOUtil::Save(node->GetData(),filePath.toStdString());

        node->SetStringProperty("path",dirROMSim.absolutePath().toStdString().c_str());

        mitkJob->SetDataModified(false);
    }

    for(int i=0;i<removeList.size();i++)
    {
        dirSim.remove(QString::fromStdString(removeList[i])+".romsimjob");
    }
    romSimFolder->ClearRemoveList();


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
    using namespace sv4gui_project_manager;
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
    mitk::NodePredicateDataType::Pointer isROMSimJob = mitk::NodePredicateDataType::New("sv4guiMitkROMSimJob");

    // Determine data node file extension.
    //
    QString extension = "";
    if (isPath->CheckNode(dataNode)) {
        extension = FileExtension::PATHS;
    } else if (isContourGroup->CheckNode(dataNode)) {
        extension = FileExtension::SEGMENTATIONS;
    } else if(isSeg3D->CheckNode(dataNode)) {
        extension = FileExtension::SEGMENTATIONS3D;
    } else if(isModel->CheckNode(dataNode)) {
        extension = FileExtension::MODELS;
    } else if(isMesh->CheckNode(dataNode)) {
        extension = FileExtension::MESHES;
    } else if(isSimJob->CheckNode(dataNode)) {
        extension = FileExtension::SIMULATIONS;
    } else if(isROMSimJob->CheckNode(dataNode)) {
        extension = FileExtension::ROMSIMULATIONS;
    } else if(issvFSIJob->CheckNode(dataNode)) {
        extension = FileExtension::SVFSI;
    }

    std::vector<mitk::BaseData::Pointer> vdata = mitk::IOUtil::Load(path+"/"+dataNode->GetName()+ extension.toStdString());
    if(vdata.size()>0) {
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
    using namespace sv4gui_project_manager;
    std::string name=dataNode->GetName();
    std::string path="";
    dataNode->GetStringProperty("path", path);
    dataNode->SetName(newName);

    if (path == "") {
        return;
    }

    //rename the corresponding files and folder if applicable, for sv4guiPath, sv4guiContourGroup, sv4guiModel, sv4guiMitkMesh,sv4guiMitkSimJob
    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("sv4guiPath");
    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("sv4guiContourGroup");
    mitk::NodePredicateDataType::Pointer isSeg3D = mitk::NodePredicateDataType::New("sv4guiMitkSeg3D");
    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("sv4guiModel");
    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("sv4guiMitkMesh");
    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob");
    mitk::NodePredicateDataType::Pointer issvFSIJob = mitk::NodePredicateDataType::New("sv4guiMitksvFSIJob");
    mitk::NodePredicateDataType::Pointer isROMSimJob = mitk::NodePredicateDataType::New("sv4guiMitkROMSimJob");
    // fix???    mitk::NodePredicateDataType::Pointer issvFSIJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob");

    std::vector<QString> extensions;

    if(isPath->CheckNode(dataNode)) { 
        extensions.push_back( FileExtension::PATHS);
    } else if(isContourGroup->CheckNode(dataNode)) {
        extensions.push_back(FileExtension::SEGMENTATIONS);
    } else if(isSeg3D->CheckNode(dataNode)) {
        extensions.push_back(FileExtension::SEGMENTATIONS3D);
        extensions.push_back(".vtp");
    } else if(isModel->CheckNode(dataNode)) {
        extensions.push_back(FileExtension::MODELS);
        extensions.push_back(".vtp");
        extensions.push_back(".brep");
        extensions.push_back(".xmt_txt");
    } else if(isMesh->CheckNode(dataNode)) {
        extensions.push_back(FileExtension::MESHES);
        extensions.push_back(".vtp");
        extensions.push_back(".vtu");
        extensions.push_back(".sms");
    } else if(isSimJob->CheckNode(dataNode)) {
        extensions.push_back(FileExtension::SIMULATIONS);
        extensions.push_back("");//for folder
    } else if(isROMSimJob->CheckNode(dataNode)) {
        extensions.push_back(FileExtension::ROMSIMULATIONS);
        extensions.push_back("");//for folder
    } else if(issvFSIJob->CheckNode(dataNode)) {
        extensions.push_back(FileExtension::SVFSI);
        extensions.push_back("");//for folder
    } else {
        return;
    }

    QDir dir(QString::fromStdString(path));
    for (const auto& extension : extensions) {
        dir.rename(QString::fromStdString(name)+extension, QString::fromStdString(newName)+extension);
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
					 mitk::DataNode::Pointer imageFolderNode, const QString& imageFilePath,
					 const QString& imageFileName, const QString& imageHeaderFileName, const QString& imageName)
{
  using namespace sv4gui_project_manager;
  imageFolderNode->SetVisibility(false);
  mitk::DataNode::Pointer imageNode;
  
  QString imageAbsoluteFileName;
  QString imageHeaderAbsoluteFileName;

  // assume that if path is blank, image is a local copy
  if (imageFilePath == "") {
    imageAbsoluteFileName = projPath + "/" + PluginNames::IMAGES + "/" + imageFileName; 
  } else {
    imageAbsoluteFileName = imageFilePath + "/" + imageFileName; 
  }

  // assume that if path is blank, it is a local copy
  // note: this code forces the transform file to be in the same directory as the image vol
  if (imageFilePath == "") {
    imageHeaderAbsoluteFileName = projPath + "/" + PluginNames::IMAGES + "/" + imageHeaderFileName; 
  } else {
    imageHeaderAbsoluteFileName = imageFilePath + "/" + imageHeaderFileName; 
  }

  try {
    
    imageNode = sv4guiProjectManager::LoadDataNode(imageAbsoluteFileName.toStdString());
    imageNode->SetName(imageName.toStdString());

    // Transform the image from the values given in the transform file. 
    auto image = dynamic_cast<mitk::Image*>(imageNode->GetData());
    if (imageHeaderFileName.toStdString().empty()) {
      std::cout << "NOTE: no transform file specified (" << imageHeaderFileName.toStdString() <<")" << std::endl;
    } else {  
      setTransformFromImageHeaderFile(image, imageHeaderAbsoluteFileName.toStdString());
    }
    dataStorage->Add(imageNode,imageFolderNode);

    // storing the path to the image dir in the image node
    imageNode->SetStringProperty("path", (imageFilePath.toStdString()).c_str());
    imageNode->SetStringProperty("image_file_name", (imageFileName.toStdString()).c_str());
    imageNode->SetStringProperty("image_absolute_file_name", (imageAbsoluteFileName.toStdString()).c_str());

    // storing the path to the image dir in the image node
    imageNode->SetStringProperty("image_header_file_name", (imageHeaderFileName.toStdString()).c_str());
    imageNode->SetStringProperty("image_header_absolute_file_name", (imageHeaderAbsoluteFileName.toStdString()).c_str());
  
  } catch(...) {
    MITK_ERROR << "Failed to load image (maybe non-existing or unsupported data type): " << imageFilePath.toStdString();
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
      std::string imageHeaderFileName = "";
      std::string imageHeaderAbsoluteFileName = "";
      if (imageNode->GetStringProperty("image_header_file_name",imageHeaderFileName)) {
	std::cout << "NOTE: NO tranform file name found in mitksegmentationsplugin!" << std::endl;
      } else {
        std::cout << "NOTE: found transform file name (" << imageHeaderFileName << ")..." << std::endl;
      }
      if (imageNode->GetStringProperty("image_header_absolute_file_name",imageHeaderAbsoluteFileName)) {
	std::cout << "no absolute tranform file name found in mitksegmentationsplugin!" << std::endl;
      }	else {
	std::cout << "NOTE: found transform absolute file name (" << imageHeaderAbsoluteFileName << ")..." << std::endl;
      }
      setTransformFromImageHeaderFile(image, imageHeaderAbsoluteFileName);
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
  auto fileInfoList = dir.entryInfoList(QStringList("*"+fileExt), QDir::Files, QDir::Name);

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

//---------------------------
// UpdateSimulations1dFolder
//---------------------------
// Update an old SV project 'Simulations1d' directory to the new 'ROMSimulations'. 
//
// The '1D Simulation' tool was changed to the 'Reduced-Order Model Simualtion' tool 
// that includes both 1D and 0D simulations. Update older SV projects by 
//
//    1) Changing file extensions from .s1djb to .romsimjob
//
//    2) Rename 'Simulations1d' directory to 'ROMSimulations'
//
void sv4guiProjectManager::UpdateSimulations1dFolder(const QString& projPath, const QString& romSimFolderName)
{
  // If there is no 'Simulations1d' directory then just 
  // create a 'ROMSimulations' directory.
  //
  QDir project_dir(projPath);
  auto sim1dFolder = project_dir.absoluteFilePath("Simulations1d");
  QFileInfo checkFile(sim1dFolder);
  if (!checkFile.exists()) { 
    project_dir.mkdir("ROMSimulations"); 
    return; 
  }

  // Change file extensions from .s1djb to .romsimjob.
  //
  project_dir.cd(sim1dFolder);
  auto fileInfoList = project_dir.entryInfoList(QStringList("*.s1djb"), QDir::Files, QDir::Name);

  for (int i = 0; i < fileInfoList.size();i++) {
    auto filePath = fileInfoList[i].absoluteFilePath().toStdString();
    auto fileName = fileInfoList[i].fileName().toStdString();
    auto baseName = fileInfoList[i].baseName().toStdString();
    auto renamedFilePath = std::regex_replace(filePath, std::regex(fileName), baseName+".romsimjob"); 
    QFile oldFile(fileInfoList[i].absoluteFilePath());
    oldFile.rename(QString(renamedFilePath.c_str()));
  }

  // Rename 'Simulations1d' directory to 'ROMSimulations'.
  project_dir.cdUp();
  project_dir.rename("Simulations1d", "ROMSimulations"); 

  QString msg = "To maintain compatibility the `Simulations1d` folder has been renamed to `ROMSimulations` and files with the .s1djb extension have been changed to use the .romsimjob extension."; 
  QMessageBox::information(NULL, "", msg);
}


