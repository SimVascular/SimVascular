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

          QString QprojPath = QString(projPath.c_str());

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
        QString Qstore_dir = QString(sv4guisvFSI_DIR_PATH.c_str());

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

      QString folderName = QString(sv4guisvFSI_NODE_NAME.c_str());
      sv4guisvFSINode = svProj.CreateDataFolder<sv4guisvFSIFolder>(m_DS, folderName, projFolderNode);
    }

  private:

    mitk::DataStorage::Pointer m_DS;
    QString m_StoreDir;
    sv4guiProjectManager svProj;
};

#endif // sv4guisvFSIUTIL_H
