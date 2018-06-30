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

#include "sv4gui_AboutDialog.h"

#include "sv4gui_AppWorkbenchAdvisor.h"
#include "simvascular_version.h"

#include <QmitkModulesDialog.h>
#include <mitkVersion.h>
#include <itkConfigure.h>
#include <vtkConfigure.h>
#include <QPushButton>
#include <vtkVersionMacros.h>

sv4guiAboutDialog::sv4guiAboutDialog(QWidget* parent, Qt::WindowFlags f)
  : QDialog(parent, f)
{
  m_GUI.setupUi(this);

  QString svVersion = QString("%1.%2.%3").arg(SV_MAJOR_VERSION).arg(SV_MINOR_VERSION).arg(SV_PATCH_VERSION);
  m_GUI.m_VersionLabel->setText("Version: "+svVersion);

  QString mitkVersion(MITK_VERSION_STRING);
  QString itkVersion = QString("%1.%2.%3").arg(ITK_VERSION_MAJOR).arg(ITK_VERSION_MINOR).arg(ITK_VERSION_PATCH);
  QString vtkVersion = QString("%1.%2.%3").arg(VTK_MAJOR_VERSION).arg(VTK_MINOR_VERSION).arg(VTK_BUILD_VERSION);
  m_GUI.m_ToolkitVersionsLabel->setText(QString("Major third-party packages: MITK %1, VTK %2, ITK %3, Qt %4").arg(mitkVersion, vtkVersion, itkVersion, QT_VERSION_STR));

  QPushButton* btnModules = new QPushButton(QIcon(":/QtWidgetsExt/ModuleView.png"), "Modules");
  m_GUI.m_ButtonBox->addButton(btnModules, QDialogButtonBox::ActionRole);

  connect(btnModules, SIGNAL(clicked()), this, SLOT(ShowModules()));
  connect(m_GUI.m_ButtonBox, SIGNAL(rejected()), this, SLOT(reject()));
}

sv4guiAboutDialog::~sv4guiAboutDialog()
{
}

void sv4guiAboutDialog::ShowModules()
{
  QmitkModulesDialog dialog(this);
  dialog.setWindowTitle("SimVascular and MITK Modules");
  dialog.exec();
}

QString sv4guiAboutDialog::GetAboutText() const
{
  return m_GUI.m_AboutLabel->text();
}

QString sv4guiAboutDialog::GetCaptionText() const
{
    return NULL;
//  return m_GUI.m_CaptionLabel->text();
}

QString sv4guiAboutDialog::GetVersionText() const
{
  return m_GUI.m_VersionLabel->text();
}

void sv4guiAboutDialog::SetAboutText(const QString &text)
{
  m_GUI.m_AboutLabel->setText(text);
}

void sv4guiAboutDialog::SetCaptionText(const QString &text)
{
//  m_GUI.m_CaptionLabel->setText(text);
}

void sv4guiAboutDialog::SetVersionText(const QString &text)
{
  m_GUI.m_VersionLabel->setText(text);
}

void sv4guiAboutDialog::UpdateStyle()
{
  sv4guiAppWorkbenchAdvisor::UpdateStyle();
}
