#include "svProjectDuplicateAction.h"

#include "svProjectManager.h"

#include <mitkNodePredicateDataType.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <QMessageBox>
#include <QInputDialog>
#include <QDir>
#include <QApplication>

svProjectDuplicateAction::svProjectDuplicateAction()
{
}

svProjectDuplicateAction::~svProjectDuplicateAction()
{
}

void svProjectDuplicateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("svProjectFolder");

    if(!isProjectFolder->CheckNode(selectedNode))
        return;

    bool ok;
    QString text = QInputDialog::getText(NULL, tr("Duplicate Project"),
                                         tr("New Project Name:"), QLineEdit::Normal,
                                         QString::fromStdString(selectedNode->GetName())+"_Copy", &ok);
    QString newName=text.trimmed();
    if (!ok || newName.isEmpty())
        return;

    std::string projPath;
    selectedNode->GetStringProperty("project path",projPath);

    QDir dir(QString::fromStdString(projPath));
    dir.cdUp();
    QString newPath=dir.absolutePath()+"/"+newName;
    if(QDir(newPath).exists())
    {
        QMessageBox::warning(NULL, "Warning", "A project with the same name already exists!");
        return;
    }

    mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
    mitk::StatusBar::GetInstance()->DisplayText("Duplicating SV project...");
    QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );

    try
    {
        svProjectManager::DuplicateProject(m_DataStorage, selectedNode, newName);
    }
    catch(std::exception& e)
    {
        MITK_ERROR << "Project duplicating failed!";
        QMessageBox::warning(NULL, "Error", QString("An error occurred during duplicating project: %1").arg(e.what()));
    }

    mitk::ProgressBar::GetInstance()->Progress(2);
    mitk::StatusBar::GetInstance()->DisplayText("SV project duplicated.");
    QApplication::restoreOverrideCursor();
}

void svProjectDuplicateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

