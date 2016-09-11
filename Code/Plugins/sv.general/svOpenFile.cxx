#include "svOpenFile.h"

#include <QmitkIOUtil.h>

#include <mitkIOUtil.h>

#include <QDir>
#include <QFileDialog>



const QString svOpenFile::EXTENSION_ID = "sv.openfile";

svOpenFile::svOpenFile()
{
}

svOpenFile::~svOpenFile()
{
}

void svOpenFile::Exec()
{
      QStringList fileNames = QFileDialog::getOpenFileNames(NULL, "Open File",
                                                            QDir::homePath(),
                                                            QmitkIOUtil::GetFileOpenFilterString(),NULL,QFileDialog::DontUseNativeDialog);
      if(fileNames.size()==0) return;

      std::vector<std::string> fileList;
      for(int i=0;i<fileNames.size();i++)
      {
          fileList.push_back(fileNames[i].toStdString());
      }
      mitk::IOUtil::LoadFiles(fileList,*GetDataStorage());
      mitk::RenderingManager::GetInstance()->InitializeViewsByBoundingObjects(GetDataStorage());
}
