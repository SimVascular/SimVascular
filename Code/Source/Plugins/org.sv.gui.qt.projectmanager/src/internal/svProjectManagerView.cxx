#include "svProjectManagerView.h"
#include "svProjectCreate.h"
#include "svProjectManager.h"

// Qt
#include <QMessageBox>
#include <QShortcut>
#include <QDir>
#include <QFileDialog>
#include <QMessageBox>
#include <QVBoxLayout>
#include <QPushButton>

const std::string svProjectManagerView::VIEW_ID = "org.sv.views.projectmanager";

svProjectManagerView::svProjectManagerView()
//    : m_SelectedDataNode(NULL)
    :m_Parent(NULL)
{
}

svProjectManagerView::~svProjectManagerView()
{
}

void svProjectManagerView::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;

    QVBoxLayout* vlayout = new QVBoxLayout(parent);
    vlayout->setContentsMargins(5,5,5,5);
    vlayout->setSpacing(10);

    parent->setLayout(vlayout);

    QPushButton* btnCreateProject=new QPushButton("Create Project");
    QPushButton* btnOpeneProject=new QPushButton("Open Project");
    QPushButton* btnSaveProjects=new QPushButton("Save All Projects");

    vlayout->addWidget(btnCreateProject);
    vlayout->addWidget(btnOpeneProject);
    vlayout->addWidget(btnSaveProjects);
    vlayout->addStretch(1);

    connect(btnCreateProject, SIGNAL(clicked()), this, SLOT(CreateProject()));
    connect(btnOpeneProject, SIGNAL(clicked()), this, SLOT(OpenProject()));
    connect(btnSaveProjects, SIGNAL(clicked()), this, SLOT(SaveAllProjects()));

}

void svProjectManagerView::CreateProject()
{
    svProjectCreate* pc=new svProjectCreate(GetDataStorage());
    pc->move(400,400);
    pc->show();
    pc->SetFocus();
}

void svProjectManagerView::OpenProject()
{
    QString projPath = QFileDialog::getExistingDirectory(m_Parent, tr("Choose Project"),
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

void svProjectManagerView::SaveAllProjects()
{
    svProjectManager::SaveAllProjects(GetDataStorage());
}

