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

#include "sv4gui_ProjectDuplicateAction.h"

#include "sv4gui_ProjectManager.h"

#include <mitkNodePredicateDataType.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <QMessageBox>
#include <QInputDialog>
#include <QDir>
#include <QApplication>

sv4guiProjectDuplicateAction::sv4guiProjectDuplicateAction()
{
}

sv4guiProjectDuplicateAction::~sv4guiProjectDuplicateAction()
{
}

void sv4guiProjectDuplicateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");

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
        sv4guiProjectManager::DuplicateProject(m_DataStorage, selectedNode, newName);
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

void sv4guiProjectDuplicateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

