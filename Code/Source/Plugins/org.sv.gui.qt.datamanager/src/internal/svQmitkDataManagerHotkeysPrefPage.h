/*===================================================================

The Medical Imaging Interaction Toolkit (MITK)

Copyright (c) German Cancer Research Center,
Division of Medical and Biological Informatics.
All rights reserved.

This software is distributed WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.

See LICENSE.txt or http://www.mitk.org for details.

===================================================================*/


#ifndef SVQMITKDATAMANAGERHOTKEYSPREFPAGE_H_
#define SVQMITKDATAMANAGERHOTKEYSPREFPAGE_H_

#include "berryIQtPreferencePage.h"
#include <org_sv_gui_qt_datamanager_Export.h>

#include <map>
#include <QWidget>

class QmitkHotkeyLineEdit;

struct SV_QT_DATAMANAGER svQmitkDataManagerHotkeysPrefPage : public QObject, public berry::IQtPreferencePage
{
  Q_OBJECT
  Q_INTERFACES(berry::IPreferencePage)

public:
  svQmitkDataManagerHotkeysPrefPage();

  void Init(berry::IWorkbench::Pointer workbench) override;

  void CreateQtControl(QWidget* parent) override;

  QWidget* GetQtControl() const override;

  ///
  /// \see IPreferencePage::PerformOk()
  ///
  virtual bool PerformOk() override;

  ///
  /// \see IPreferencePage::PerformCancel()
  ///
  virtual void PerformCancel() override;

  ///
  /// \see IPreferencePage::Update()
  ///
  virtual void Update() override;

protected:
  ///
  /// The node from which the properties are taken (will be catched from the preferences service in ctor)
  ///
  berry::IPreferences::WeakPtr m_DataManagerHotkeysPreferencesNode;

  ///
  /// Maps a label to hotkey lineedit, e.g. "Toggle Visibility of selected nodes" => QmitkHotkeyLineEdit
  ///
  std::map<QString, QmitkHotkeyLineEdit*> m_HotkeyEditors;

  QWidget* m_MainControl;
};

#endif /* SVQMITKDATAMANAGERHOTKEYSPREFPAGE_H_ */
