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

#ifndef SV4GUI_PROJECTMANAGER_H
#define SV4GUI_PROJECTMANAGER_H

#include "SimVascular.h"

#include <sv4guiModuleProjectManagementExports.h>

#include <mitkDataNode.h>
#include <mitkDataStorage.h>
#include <mitkNodePredicateDataType.h>

#include <QDir>
#include <QString>
#include <set>

namespace sv4gui_project_manager {

//-------------
// PluginNames
//-------------
// This class is used to stores the names of SV plugins.
//
class PluginNames 
{
  public:
    static const QString IMAGES;
    static const QString MESHES;
    static const QString MODELS;
    static const QString PATHS;
    static const QString SEGMENTATIONS;
    static const QString SEGMENTATIONS3D;
    static const QString SIMULATIONS;
    static const QString ROMSIMULATIONS;
    static const QString SVFSI;
    static const QStringList NAMES_LIST;
};

//-----------------
// XmlImageInformationElementNames
//-----------------
// This class is used to store the element names for the 
// image location xml file. 
//
class XmlImageInformationElementNames 
{
  public:
    static const QString ROOT;
    static const QString TIMESTEP;
    static const QString CREATED_WITH_SIMVASCULAR_VERSION;
    static const QString PATH;
    static const QString IMAGE_FILE_NAME;
    static const QString IMAGE_HEADER_FILE_NAME;
    static const QString IMAGE_NAME;
    static const QString DATA_IS_LOCAL_COPY;
    static const QString SCALE_FACTOR;
    static const std::set<QString> valid_names;
};

class  XmlImageHeaderElementNames
{
  public:
    static const QString ROOT;
    static const QString CREATED_WITH_SIMVASCULAR_VERSION;
    static const QString MODALITY;
    static const QString AGE;
    static const QString GENDER;
    static const QString ETHNICITY;
    static const QString IMAGE_IS_SCALED;
    static const QString SCALE_FACTOR;
    static const QString ORIGINAL_UNITS;
    static const QString CURRENT_UNITS;
    static const QString TRANSFORM_LPS;
    // transform is for legacy transform files and is deprecated
    static const QString TRANSFORM;
    static const std::set<QString> valid_names;
  };
  
//---------------
// FileExtension
//---------------
// This class stores the names of plugin file extensions
// used to store plugin state.
//
class FileExtension
{
  public:
    static const QString MESHES;
    static const QString MODELS;
    static const QString PATHS;
    static const QString SEGMENTATIONS;
    static const QString SEGMENTATIONS3D;
    static const QString SIMULATIONS;
    static const QString ROMSIMULATIONS;
    static const QString SVFSI;
    static const QString IMAGE_VTI;
    static const QString IMAGE_HEADER;  
};

class  XmlProjectElementNames
{
  public:
    static const QString ROOT;
    static const QString VERSION;
    static const std::set<QString> valid_names;
};

}

//----------------------
// sv4guiProjectManager
//----------------------
// The sv4guiProjectManager class primarially stores methods for project
// operations.
//
// [TODO:DaveP] This should really just be a namespace.
//
class SV4GUIMODULEPROJECTMANAGEMENT_EXPORT sv4guiProjectManager
{

  public:
    sv4guiProjectManager();
    static const QString SVPROJ_CONFIG_FILE_NAME;
    static const QString IMAGE_OBJECT_INFORMATION_FILE_NAME;
    static const QString SV_PROJECT_FILE_NAME;

    static QString simvascularVersion_;

    // project methods
    static void AddProject(mitk::DataStorage::Pointer dataStorage, QString projectName, QString projParentDir, bool newProject);
    static void ReadProjectFile(const QString& projPath, QString& version);
    static void SaveProject(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode);
    static void SaveProjectAs(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode, QString saveFilePath);
    static void SaveAllProjects(mitk::DataStorage::Pointer dataStorage);
    static void WriteProjectFile(const QString& projPath);

    // data node methods
    static void LoadData(mitk::DataNode::Pointer dataNode);
    static mitk::DataNode::Pointer LoadDataNode(std::string filePath);
    static mitk::DataNode::Pointer GetProjectFolderNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode);

    static void AddDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, mitk::DataNode::Pointer parentNode);
    static void RemoveDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, mitk::DataNode::Pointer parentNode);
    static void RenameDataNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, std::string newName);

    static void DuplicateProject(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode, QString newName);
    static bool DuplicateDirRecursively(const QString &srcFilePath, const QString &tgtFilePath);

    // public image data methods
    static void WriteImageInfo(const QString& projPath, const QString& imageFilePath, const QString& imageFileName,
			       const QString& imageHeaderFileName, const QString& imageName, bool copyIntoProject=false, double scaleFactor=1.0); 
    static void AddImage(mitk::DataStorage::Pointer dataStorage, QString imageFilePath, mitk::DataNode::Pointer imageNode,
			 mitk::DataNode::Pointer imageFolderNode, bool copyIntoProject, double scaleFactor, QString newImageName);
    static void writeTransformFile(mitk::Image* image, std::string transformAbsoluteFileName);   // deprecated
    static void setTransformFromFile(mitk::Image* image, std::string transformAbsoluteFileName);  // deprecated
    static void writeImageHeaderFile(mitk::Image* image,
				     std::string modality, int age, std::string gender, std::string race,
				     bool scale_volume, double scaleFactor, std::string orginal_units, std::string scaled_units, 
				     std::string imageHeaderAbsoluteFileName);
    static void setTransformFromImageHeaderFile(mitk::Image* image, std::string imageHeaderAbsoluteFileName);
    static QString GetImageInfoFilePath(QDir project_dir); 

    // A function template defining functions used to create a SV Data Manager node. 
    //
    template <typename TDataFolder>
    mitk::DataNode::Pointer static CreateDataFolder(mitk::DataStorage::Pointer dataStorage, QString folderName, 
          mitk::DataNode::Pointer projFolderNode=NULL)
    {
        mitk::NodePredicateDataType::Pointer isDataFolder = mitk::NodePredicateDataType::New(TDataFolder::GetStaticNameOfClass());
        mitk::DataStorage::SetOfObjects::ConstPointer rs;

        if (projFolderNode.IsNull()) {
            rs=dataStorage->GetSubset(isDataFolder);
        } else {
            rs = dataStorage->GetDerivations(projFolderNode, isDataFolder);
        }

        bool exists = false;
        mitk::DataNode::Pointer dataFolderNode = NULL;
        std::string fdName=folderName.toStdString();

        for (int i = 0; i < rs->size(); i++) {
            if (rs->GetElement(i)->GetName() == fdName) {
                exists = true;
                dataFolderNode=rs->GetElement(i);
                break;
            }
        }

        if (!exists) {
            dataFolderNode = mitk::DataNode::New();
            dataFolderNode->SetName(fdName);
            dataFolderNode->SetVisibility(true);
            typename TDataFolder::Pointer dataFolder = TDataFolder::New();
            dataFolderNode->SetData(dataFolder);
            if(projFolderNode.IsNull()) {
                dataStorage->Add(dataFolderNode);
            } else {
                dataStorage->Add(dataFolderNode, projFolderNode);
            }

        }

        return dataFolderNode;
    }

  private:

    static mitk::DataNode::Pointer CreateImagesPlugin(mitk::DataStorage::Pointer dataStorage, QString projPath,
						      mitk::DataNode::Pointer imageFolderNode, const QString& imageFilePath,
						      const QString& imageFileName, const QString& imageHeaderFileName, const QString& imageName);

    static void CreatePathsPlugin(mitk::DataStorage::Pointer dataStorage, QString projPath,
        mitk::DataNode::Pointer pathFolderNode, QString pathFolderName);

    static void CreateSegmentationsPlugin(mitk::DataStorage::Pointer dataStorage, QString projPath,
        mitk::DataNode::Pointer segFolderNode, QString segFolderName);

    static void CreateMitkSegmentationsPlugin(mitk::DataStorage::Pointer dataStorage, QString projPath,
        mitk::DataNode::Pointer imageNode);

    static void CreateModelPlugin(mitk::DataStorage::Pointer dataStorage, QString projPath,             
        mitk::DataNode::Pointer modelNode, QString modelFolderName);

    static void CreatePlugin(mitk::DataStorage::Pointer dataStorage, QString projPath,
        mitk::DataNode::Pointer folderNode, QString folderName, QString fileExt);

    // private image data methods
    static void CopyImageToProject(const std::string& projPath, mitk::DataNode::Pointer imageNode, mitk::DataNode::Pointer imageFolderNode,
				   double scaleFactor, QString& imageFileName, QString& imageHeaderFileName);

    static void ReadImageInfo(const QString& projPath, QString& imageFilePath, QString& imageFileName, QString& imageHeaderFileName,
			      QString& imageName);

    static void ReadImageInfoFromImageLoc(QString imageLocFilePath, QString& imageFilePath, QString& imageFileName, QString& imageHeaderFileName,
					  QString& imageName);

    static void ReadImageInfoFromSvproj(const QString& projPath, QStringList& imageFilePathList, QStringList& imageNameList, bool& localFile);
    
    // private update methods
    static void UpdateSimulations1dFolder(const QString& projPath, const QString& romSimFolderName);
};

#endif // SV4GUI_PROJECTMANAGER_H
