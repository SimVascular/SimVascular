#include "svProjectCreate.h"
#include "ui_svProjectCreate.h"

#include "svProjectManager.h"

#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <QMessageBox>
#include <QFile>
#include <QFileDialog>
#include <QDir>

svProjectCreate::svProjectCreate(mitk::DataStorage::Pointer dataStorage)
    : ui(new Ui::svProjectCreate)
    , m_DataStorage(dataStorage)
    , m_LastPath("")
{
    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateNewProject()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->btnBrowse, SIGNAL(clicked()), this, SLOT(ChoosePath()));

    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer prefs;
   if (prefService)
   {
       prefs = prefService->GetSystemPreferences()->Node("/General");
   }
   else
   {
       prefs = berry::IPreferences::Pointer(0);
   }

   if(prefs.IsNotNull())
   {
       m_LastPath = prefs->Get("LastSVProjCreatParentPath", "");
   }

   if(m_LastPath=="")
       m_LastPath=QDir::homePath();

   ui->lineEditDir->setText(m_LastPath);
}



svProjectCreate::~svProjectCreate()
{
    delete ui;
}

void svProjectCreate::SetFocus()
{
    ui->lineEditProjectName->setFocus();
}

void svProjectCreate::CreateNewProject()
{
    if(m_DataStorage.IsNull()) return;

    QString projName=ui->lineEditProjectName->text().trimmed();
    QString projParentDir=ui->lineEditDir->text().trimmed();

    if(projName.isEmpty()){
        QMessageBox::warning(NULL,"No Name","Please give a project name!");
        return;
    }

    if(projParentDir.isEmpty()){
        QMessageBox::warning(NULL,"No Dir","Please give a directory!");
        return;
    }else if(!QFile::exists(projParentDir)){
        QMessageBox::warning(NULL,"No Dir Exists","Please give a existing directory!");
        return;
    }

    QDir dir(projParentDir);
    if(dir.exists(projName))
    {
        QMessageBox::warning(NULL,"Project Exists","Please give a new project!");
        return;
    }

    svProjectManager::AddProject(m_DataStorage, projName,projParentDir,true);

    m_LastPath=projParentDir;
    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer prefs;
    if (prefService)
    {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    }
    else
    {
        prefs = berry::IPreferences::Pointer(0);
    }
    if(prefs.IsNotNull())
    {
        prefs->Put("LastSVProjCreatParentPath", m_LastPath);
        prefs->Flush();
    }

    hide();
    deleteLater();
}

void svProjectCreate::Cancel()
{
    hide();
    deleteLater();
}

void svProjectCreate::ChoosePath()
{
    QString path=ui->lineEditDir->text().trimmed();
    if(path=="" || !QDir(path).exists())
        path=m_LastPath;

    QString dir = QFileDialog::getExistingDirectory(this, tr("Choose Directory"),
                                                    path,
                                                    QFileDialog::DontResolveSymlinks
                                                    | QFileDialog::DontUseNativeDialog
                                                    );
    dir=dir.trimmed();
    if(dir!="")
        ui->lineEditDir->setText(dir);
}
