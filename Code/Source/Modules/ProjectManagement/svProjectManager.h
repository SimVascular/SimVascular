#ifndef SVPROJECTMANAGER_H
#define SVPROJECTMANAGER_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include <mitkDataNode.h>
#include <mitkDataStorage.h>

#include <QString>

class SVPROJECTMANAGEMENT_EXPORT svProjectManager
{

public:

    static void AddProject(mitk::DataStorage::Pointer dataStorage, QString projectName, QString projParentDir, bool newProject);
    static void WriteEmptyConfigFile(QString projConfigFilePath);
    static void AddImage(mitk::DataStorage::Pointer dataStorage, QString imageFilePath, mitk::DataNode::Pointer imageFolderNode, bool copyIntoProject);

    static void SaveProject(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer projFolderNode);
    static void SaveAllProjects(mitk::DataStorage::Pointer dataStorage);

    static void LoadData(mitk::DataNode::Pointer dataNode);
    static mitk::DataNode::Pointer GetProjectFolderNode(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode);

    template <typename TDataFolder> static mitk::DataNode::Pointer CreateDataFolder(mitk::DataStorage::Pointer dataStorage, QString folderName, mitk::DataNode::Pointer projFolderNode=NULL);

};

#endif // SVPROJECTMANAGER_H
