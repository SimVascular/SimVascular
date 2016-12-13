
#ifndef SVABOUTDIALOG_H
#define SVABOUTDIALOG_H

#include <org_sv_gui_qt_application_Export.h>

#include "ui_svAboutDialog.h"

class SV_QT_APPLICATION svAboutDialog : public QDialog
{
  Q_OBJECT

public:
  svAboutDialog(QWidget* parent = nullptr, Qt::WindowFlags f = Qt::CustomizeWindowHint | Qt::WindowCloseButtonHint);
  virtual ~svAboutDialog();

  QString GetAboutText() const;
  QString GetCaptionText() const;
  QString GetRevisionText() const;

  void SetAboutText(const QString &text);
  void SetCaptionText(const QString &text);
  void SetRevisionText(const QString &text);

protected slots:
  void ShowModules();

private:
  Ui::svAboutDialog m_GUI;
};

#endif
