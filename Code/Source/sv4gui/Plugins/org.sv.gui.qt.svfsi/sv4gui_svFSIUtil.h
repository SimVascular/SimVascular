#ifndef sv4guisvFSIUTIL_H
#define sv4guisvFSIUTIL_H

#include "sv4gui_DataNodeOperation.h"
#include "sv4gui_ProjectManager.h"
#include "sv4gui_svFSIFolder.h"

#include <mitkNodePredicateDataType.h>
#include <mitkDataStorage.h>
#include <mitkIOUtil.h>

#include <QDir>
#include <QString>

#include <vtkStructuredPoints.h>
#include <vtkStructuredPointsWriter.h>
#include <vtkPolyDataReader.h>
#include <vtkSmartPointer.h>

#include <sstream>

static const std::string sv4guisvFSI_DIR_PATH = "svFSI";
static const std::string sv4guisvFSI_NODE_NAME = "svFSI";
class sv4guisvFSIUtil {
  public:

    void setDataStorage(mitk::DataStorage* dataStorage){
      m_DS = dataStorage;
    }

    mitk::DataNode::Pointer getProjectNode(){
      mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
      mitk::DataNode::Pointer projFolderNode=m_DS->GetNode (isProjFolder);
      return projFolderNode;
    }

    QDir getDir(){
      mitk::DataNode::Pointer projFolderNode = getProjectNode();

      std::string projPath="";
      if (!projFolderNode){
        MITK_ERROR << "project folder node null, cannot get project path\n";
        return QDir("not_found");
      }
      else{
          projFolderNode->GetStringProperty("project path", projPath);

          QString QprojPath = QString::QString(projPath.c_str());

          QDir dir(QprojPath);

          return dir;
      }
    }

    QDir getsv4guisvFSIDir(){
      QDir projDir = getDir();
      projDir.cd(sv4guisvFSI_DIR_PATH.c_str());
      return projDir;
    }

    void makeDir(){
      QDir dir = getDir();
      if(dir.exists()){
        QString Qstore_dir = QString::QString(sv4guisvFSI_DIR_PATH.c_str());

        m_StoreDir = Qstore_dir;

        if(!dir.exists(Qstore_dir))
        {
            dir.mkdir(Qstore_dir);
        }else {
          std::cout <<"svFSI directory already exists\n";
        }
      }

    }

    void createDataFolder(){
      mitk::DataNode::Pointer projFolderNode = getProjectNode();
      mitk::DataNode::Pointer sv4guisvFSINode = m_DS->GetNamedNode(sv4guisvFSI_NODE_NAME);
      if (!projFolderNode){ return; }
      if (sv4guisvFSINode){ return; }

      QString folderName = QString::QString(sv4guisvFSI_NODE_NAME.c_str());
      sv4guisvFSINode = svProj.CreateDataFolder<sv4guisvFSIFolder>(m_DS, folderName, projFolderNode);
    }

  private:

    mitk::DataStorage::Pointer m_DS;
    QString m_StoreDir;
    sv4guiProjectManager svProj;
};

#endif // sv4guisvFSIUTIL_H
