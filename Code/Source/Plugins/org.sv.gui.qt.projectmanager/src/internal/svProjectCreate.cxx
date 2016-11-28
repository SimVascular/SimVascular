#include "svProjectCreate.h"
#include "ui_svProjectCreate.h"

#include "svProjectManager.h"

#include <QMessageBox>
#include <QFile>
#include <QFileDialog>
#include <QDir>

svProjectCreate::svProjectCreate(mitk::DataStorage::Pointer dataStorage)
    : ui(new Ui::svProjectCreate)
    , m_DataStorage(dataStorage)
//    , m_SelectedDataNode(NULL)
{
    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateNewProject()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->btnBrowse, SIGNAL(clicked()), this, SLOT(ChoosePath()));
}

svProjectCreate::~svProjectCreate()
{
    delete ui;
}

void svProjectCreate::SetFocus()
{
    ui->lineEditProjectName->setFocus();
    ui->lineEditDir->setText(QDir::homePath());
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
        QMessageBox::warning(NULL,"No Dir","Please give a  directory!");
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

    hide();
}

void svProjectCreate::Cancel()
{
    hide();
}

void svProjectCreate::ChoosePath()
{
    QString dir = QFileDialog::getExistingDirectory(this, tr("Choose Directory"),
                                                    QDir::homePath(), //to do: get last path from mitk
                                                    QFileDialog::ShowDirsOnly
                                                    | QFileDialog::DontResolveSymlinks
                                                    | QFileDialog::DontUseNativeDialog
                                                    );
    ui->lineEditDir->setText(dir);
}
