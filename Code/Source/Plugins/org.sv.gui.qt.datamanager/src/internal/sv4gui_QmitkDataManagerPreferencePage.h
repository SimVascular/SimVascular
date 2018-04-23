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


#ifndef SV4GUI_QMITKDATAMANAGERPREFERENCEPAGE_H_
#define SV4GUI_QMITKDATAMANAGERPREFERENCEPAGE_H_

#include "berryIQtPreferencePage.h"
#include <org_sv_gui_qt_datamanager_Export.h>
#include <berryIPreferences.h>

class QWidget;
class QCheckBox;

struct SV_QT_DATAMANAGER sv4guiQmitkDataManagerPreferencePage : public QObject, public berry::IQtPreferencePage
{
  Q_OBJECT
  Q_INTERFACES(berry::IPreferencePage)

public:
  sv4guiQmitkDataManagerPreferencePage();

  void Init(berry::IWorkbench::Pointer workbench) override;

  void CreateQtControl(QWidget* widget) override;

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
  QWidget* m_MainControl;
  QCheckBox* m_EnableSingleEditing;
  QCheckBox* m_PlaceNewNodesOnTop;
  QCheckBox* m_ShowHelperObjects;
  QCheckBox* m_ShowNodesContainingNoData;
  QCheckBox* m_GlobalReinitOnNodeDelete;
  QCheckBox* m_GlobalReinitOnNodeAdded;
  QCheckBox* m_UseSurfaceDecimation;
  berry::IPreferences::Pointer m_DataManagerPreferencesNode;
};

#endif /* SV4GUI_QMITKDATAMANAGERPREFERENCEPAGE_H_ */
