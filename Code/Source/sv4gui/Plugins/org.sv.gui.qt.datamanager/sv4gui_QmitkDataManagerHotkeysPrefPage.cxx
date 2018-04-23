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

#include "sv4gui_QmitkDataManagerHotkeysPrefPage.h"
#include <QmitkHotkeyLineEdit.h>

#include "berryIPreferencesService.h"
#include "berryPlatform.h"

#include <QLabel>
#include <QPushButton>
#include <QLineEdit>
#include <QGridLayout>
#include <QMessageBox>
#include <QApplication>

#include <map>

using namespace berry;

sv4guiQmitkDataManagerHotkeysPrefPage::sv4guiQmitkDataManagerHotkeysPrefPage()
: m_MainControl(nullptr)
{

}

void sv4guiQmitkDataManagerHotkeysPrefPage::Init(berry::IWorkbench::Pointer )
{

}

void sv4guiQmitkDataManagerHotkeysPrefPage::CreateQtControl(QWidget* parent)
{
  IPreferencesService* prefService = Platform::GetPreferencesService();
  berry::IPreferences::Pointer _DataManagerHotkeysPreferencesNode = prefService->GetSystemPreferences()->Node("/DataManager/Hotkeys");
  m_DataManagerHotkeysPreferencesNode = _DataManagerHotkeysPreferencesNode;

  m_HotkeyEditors["Make all nodes invisible"] = new QmitkHotkeyLineEdit("Ctrl+, V");

  m_HotkeyEditors["Toggle visibility of selected nodes"] = new QmitkHotkeyLineEdit("V");

  m_HotkeyEditors["Delete selected nodes"] = new QmitkHotkeyLineEdit("Del");

  m_HotkeyEditors["Reinit selected nodes"] = new QmitkHotkeyLineEdit("R");

  m_HotkeyEditors["Global Reinit"] = new QmitkHotkeyLineEdit("Ctrl+, R");

  m_HotkeyEditors["Show Node Information"] = new QmitkHotkeyLineEdit("Ctrl+, I");

  m_MainControl = new QWidget(parent);

  auto   layout = new QGridLayout;
  int i = 0;
  for (auto it = m_HotkeyEditors.begin()
    ; it != m_HotkeyEditors.end(); ++it)
  {
    layout->addWidget(new QLabel(it->first), i,0);
    layout->addWidget(it->second, i,1);
    layout->setRowStretch(i,0);
    ++i;
  }
  layout->setRowStretch(i+1,10);

  m_MainControl->setLayout(layout);
  this->Update();
}

QWidget* sv4guiQmitkDataManagerHotkeysPrefPage::GetQtControl() const
{
  return m_MainControl;
}

bool sv4guiQmitkDataManagerHotkeysPrefPage::PerformOk()
{
  IPreferences::Pointer _DataManagerHotkeysPreferencesNode = m_DataManagerHotkeysPreferencesNode.Lock();
  if(_DataManagerHotkeysPreferencesNode.IsNotNull())
  {
    bool duplicate = false;
    QString keyString;
    QString errString;
    for (auto it = m_HotkeyEditors.begin()
      ; it != m_HotkeyEditors.end(); ++it)
    {
      keyString = it->second->GetKeySequenceAsString();

      if(keyString.isEmpty())
        errString = QString("No valid key sequence for \"%1\"").arg(it->first);

      if(errString.isEmpty())
      {
        std::map<QString, QmitkHotkeyLineEdit*>::iterator it2;
        // search for duplicated key
        for (it2 = m_HotkeyEditors.begin(); it2 != m_HotkeyEditors.end(); ++it2)
        {
           if(it->first != it2->first && keyString == it2->second->GetKeySequenceAsString())
           {
             duplicate = true;
             break;
           }
        }
        if(duplicate == true)
          errString = QString("Duplicate hot key for \"%1\" and \"%2\"").arg(it->first).arg(it2->first);
      }

      if(!errString.isEmpty())
      {
        QMessageBox::critical(QApplication::activeWindow(), "Error", errString);
        return false;
      }
    }

  //# no errors -> save all values and flush to file
    for (auto it = m_HotkeyEditors.begin()
      ; it != m_HotkeyEditors.end(); ++it)
      _DataManagerHotkeysPreferencesNode->Put(it->first
        , it->second->GetKeySequenceAsString());

    _DataManagerHotkeysPreferencesNode->Flush();

    return true;
  }
  return false;
}

void sv4guiQmitkDataManagerHotkeysPrefPage::PerformCancel()
{

}

void sv4guiQmitkDataManagerHotkeysPrefPage::Update()
{
  IPreferences::Pointer _DataManagerHotkeysPreferencesNode = m_DataManagerHotkeysPreferencesNode.Lock();
  if(_DataManagerHotkeysPreferencesNode.IsNotNull())
  {
    for (auto it = m_HotkeyEditors.begin()
      ; it != m_HotkeyEditors.end(); ++it)
    {
      it->second->setText(_DataManagerHotkeysPreferencesNode->Get(it->first, it->second->text()));
    }
  }
}
