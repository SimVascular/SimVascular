#include "svProjectManagerView.h"
#include "svProjectCreate.h"
#include "svProjectManager.h"

// Blueberry
//#include <berryISelectionService.h>
//#include <berryIWorkbenchWindow.h>

// mitk
#include <mitkIRenderingManager.h>
//#include <mitkDataStorage.h>
//#include <mitkDataNode.h>
//#include "mitkProperties.h"
//#include <mitkNodePredicateBase.h>
#include <mitkNodePredicateProperty.h>

// Qt
#include <QMessageBox>
#include <QShortcut>
#include <QDir>
#include <QFileDialog>
#include <QMessageBox>

const std::string svProjectManagerView::VIEW_ID = "sv.views.projectmanager";

svProjectManagerView::svProjectManagerView()
//    : m_SelectedDataNode(NULL)
{

}

svProjectManagerView::~svProjectManagerView()
{

}

void svProjectManagerView::CreateQtPartControl( QWidget *parent )
{
    QVBoxLayout* vlayout = new QVBoxLayout(parent);
    vlayout->setContentsMargins(0,0,0,0);
    vlayout->setSpacing(0);

    parent->setLayout(vlayout);

    QPushButton btnCreateProject=new QPushButton("Create Project");
    QPushButton btnOpeneProject=new QPushButton("Open Project");
    QPushButton btnSaveProjects=new QPushButton("Save All Projects");

    connect(btnCreateProject, SIGNAL(clicked()), this, SLOT(CreateProject()));
    connect(btnOpeneProject, SIGNAL(clicked()), this, SLOT(OpenProject()));
    connect(btnSaveProjects, SIGNAL(clicked()), this, SLOT(SaveAllProjects()));

}

void svProjectManagerView::CreateProject()
{
    svProjectCreate* pc=new svProjectCreate(GetDataStorage());
    pc->move(400,400);
    pc->show();
}

void svProjectManagerView::OpenProject()
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

void svProjectManagerView::SaveAllProjects()
{
    svProjectManager::SaveAllProjects(GetDataStorage());
}

//void svProjectManagerView::SetFocus()
//{
//}

//void svProjectManagerView::OnSelectionChanged( berry::IWorkbenchPart::Pointer /*source*/,
//        const QList<mitk::DataNode::Pointer>& nodes )
//{

//    foreach( mitk::DataNode::Pointer node, nodes)
//    {
//        if(!node) continue;

//        m_SelectedDataNode=node;

//   }

//}


//void svProjectManagerView::NodeChanged(const mitk::DataNode* node){

//}

//void svProjectManagerView::NodeAdded(const mitk::DataNode* node){

//}


//void svProjectManagerView::NodeRemoved(const mitk::DataNode* node){

//}



