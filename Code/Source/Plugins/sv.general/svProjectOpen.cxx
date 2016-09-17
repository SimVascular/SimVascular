#include "svProjectOpen.h"
#include "svProjectManager.h"

#include <QDir>
#include <QFileDialog>
#include <QMessageBox>


const QString svProjectOpen::EXTENSION_ID = "sv.projectopen";

svProjectOpen::svProjectOpen()
{
}

svProjectOpen::~svProjectOpen()
{
}

void svProjectOpen::Exec()
{
    QString projPath = QFileDialog::getExistingDirectory(this, tr("Choose Project"),
                                                    QDir::homePath(),
                                                    QFileDialog::ShowDirsOnly
                                                    | QFileDialog::DontResolveSymlinks
                                                    | QFileDialog::DontUseNativeDialog
                                                    );

    if(projPath.trimmed().isEmpty()) return;
    QDir dir(projPath);
    if(dir.exists(".svproj"))
    {
        QString projName=dir.dirName();
        dir.cdUp();
        QString projParentDir=dir.absolutePath();
        svProjectManager::AddProject(GetDataStorage(), projName,projParentDir,false);
    }else{
        QMessageBox::warning(NULL,"Invalid Project","No project config file found!");
    }


}
