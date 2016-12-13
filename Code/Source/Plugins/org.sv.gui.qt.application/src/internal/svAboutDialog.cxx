#include "svAboutDialog.h"
#include "simvascular_version.h"

#include <QmitkModulesDialog.h>
#include <mitkVersion.h>
#include <itkConfigure.h>
#include <vtkConfigure.h>
#include <QPushButton>
#include <vtkVersionMacros.h>

svAboutDialog::svAboutDialog(QWidget* parent, Qt::WindowFlags f)
  : QDialog(parent, f)
{
  m_GUI.setupUi(this);

//  QString svVersion(SV_FULL_VER_NO);
  QString svVersion("3.0");
  QString mitkVersion(MITK_VERSION_STRING);
  QString itkVersion = QString("%1.%2.%3").arg(ITK_VERSION_MAJOR).arg(ITK_VERSION_MINOR).arg(ITK_VERSION_PATCH);
  QString vtkVersion = QString("%1.%2.%3").arg(VTK_MAJOR_VERSION).arg(VTK_MINOR_VERSION).arg(VTK_BUILD_VERSION);

//  QString revisionText = QString("Revision: %1").arg(SV_REVISION);
//  if (!QString(SV_REVISION_DESC).isEmpty())
//    revisionText += QString("\nDescription: %1").arg(SV_REVISION_DESC);

//  m_GUI.m_RevisionLabel->setText(revisionText);

  m_GUI.m_ToolkitVersionsLabel->setText(QString("MITK %1, VTK %2, ITK %3, Qt %4").arg(mitkVersion, vtkVersion, itkVersion, QT_VERSION_STR));

  QPushButton* btnModules = new QPushButton(QIcon(":/QtWidgetsExt/ModuleView.png"), "Modules");
  m_GUI.m_ButtonBox->addButton(btnModules, QDialogButtonBox::ActionRole);

  connect(btnModules, SIGNAL(clicked()), this, SLOT(ShowModules()));
  connect(m_GUI.m_ButtonBox, SIGNAL(rejected()), this, SLOT(reject()));
}

svAboutDialog::~svAboutDialog()
{
}

void svAboutDialog::ShowModules()
{
  QmitkModulesDialog dialog(this);
  dialog.setWindowTitle("SimVascular and MITK Modules");
  dialog.exec();
}

QString svAboutDialog::GetAboutText() const
{
  return m_GUI.m_AboutLabel->text();
}

QString svAboutDialog::GetCaptionText() const
{
    return NULL;
//  return m_GUI.m_CaptionLabel->text();
}

QString svAboutDialog::GetRevisionText() const
{
  return m_GUI.m_RevisionLabel->text();
}

void svAboutDialog::SetAboutText(const QString &text)
{
  m_GUI.m_AboutLabel->setText(text);
}

void svAboutDialog::SetCaptionText(const QString &text)
{
//  m_GUI.m_CaptionLabel->setText(text);
}

void svAboutDialog::SetRevisionText(const QString &text)
{
  m_GUI.m_RevisionLabel->setText(text);
}

