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

#include "sv4gui_QmitkDataManagerPreferencePage.h"
#include "sv4gui_QmitkDataManagerView.h"

#include <QLabel>
#include <QPushButton>
#include <QFormLayout>
#include <QCheckBox>

#include <berryIPreferencesService.h>
#include <berryPlatform.h>

sv4guiQmitkDataManagerPreferencePage::sv4guiQmitkDataManagerPreferencePage()
: m_MainControl(nullptr)
{

}

void sv4guiQmitkDataManagerPreferencePage::Init(berry::IWorkbench::Pointer )
{

}

void sv4guiQmitkDataManagerPreferencePage::CreateQtControl(QWidget* parent)
{
  berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();

  m_DataManagerPreferencesNode = prefService->GetSystemPreferences()->Node(sv4guiQmitkDataManagerView::VIEW_ID);

  m_MainControl = new QWidget(parent);
  m_EnableSingleEditing = new QCheckBox;
  m_PlaceNewNodesOnTop = new QCheckBox;
  m_ShowHelperObjects = new QCheckBox;
  m_ShowNodesContainingNoData = new QCheckBox;
  m_GlobalReinitOnNodeDelete = new QCheckBox;
  m_GlobalReinitOnNodeAdded = new QCheckBox;
  m_UseSurfaceDecimation = new QCheckBox;

  auto  formLayout = new QFormLayout;
  formLayout->addRow("&Single click property editing:", m_EnableSingleEditing);
  formLayout->addRow("&Place new nodes on top:", m_PlaceNewNodesOnTop);
  formLayout->addRow("&Show helper objects:", m_ShowHelperObjects);
  formLayout->addRow("&Show nodes containing no data", m_ShowNodesContainingNoData);
  formLayout->addRow("&Call global reinit if node is deleted", m_GlobalReinitOnNodeDelete);
  formLayout->addRow("&Call global reinit if node is added", m_GlobalReinitOnNodeAdded);
  formLayout->addRow("&Use surface decimation:", m_UseSurfaceDecimation);

  m_MainControl->setLayout(formLayout);
  this->Update();
}

QWidget* sv4guiQmitkDataManagerPreferencePage::GetQtControl() const
{
  return m_MainControl;
}

bool sv4guiQmitkDataManagerPreferencePage::PerformOk()
{
  m_DataManagerPreferencesNode->PutBool("Single click property editing"
                                        , m_EnableSingleEditing->isChecked());
  m_DataManagerPreferencesNode->PutBool("Place new nodes on top"
                                        , m_PlaceNewNodesOnTop->isChecked());
  m_DataManagerPreferencesNode->PutBool("Show helper objects"
                                        , m_ShowHelperObjects->isChecked());
  m_DataManagerPreferencesNode->PutBool("Show nodes containing no data"
                                        , m_ShowNodesContainingNoData->isChecked());
  m_DataManagerPreferencesNode->PutBool("Call global reinit if node is deleted"
                                        , m_GlobalReinitOnNodeDelete->isChecked());
  m_DataManagerPreferencesNode->PutBool("Call global reinit if node is added"
                                        , m_GlobalReinitOnNodeAdded->isChecked());
  m_DataManagerPreferencesNode->PutBool("Use surface decimation"
                                        , m_UseSurfaceDecimation->isChecked());
  return true;
}

void sv4guiQmitkDataManagerPreferencePage::PerformCancel()
{

}

void sv4guiQmitkDataManagerPreferencePage::Update()
{
  m_EnableSingleEditing->setChecked(m_DataManagerPreferencesNode->GetBool("Single click property editing", true));
  m_PlaceNewNodesOnTop->setChecked(m_DataManagerPreferencesNode->GetBool("Place new nodes on top", false));
  m_ShowHelperObjects->setChecked(m_DataManagerPreferencesNode->GetBool("Show helper objects", false));
  m_ShowNodesContainingNoData->setChecked(m_DataManagerPreferencesNode->GetBool("Show nodes containing no data", false));
  m_UseSurfaceDecimation->setChecked(m_DataManagerPreferencesNode->GetBool("Use surface decimation", true));
  m_GlobalReinitOnNodeDelete->setChecked(m_DataManagerPreferencesNode->GetBool("Call global reinit if node is deleted", true));
  m_GlobalReinitOnNodeAdded->setChecked(m_DataManagerPreferencesNode->GetBool("Call global reinit if node is added", true));
}
