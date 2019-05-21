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

#include "sv4gui_QmitkMorphologicalOperationsWidget.h"
#include <mitkProgressBar.h>
#include <mitkSliceNavigationController.h>
#include <QCheckBox>

static const char* const HelpText = "Select a segmentation above";

QmitkMorphologicalOperationsWidget::QmitkMorphologicalOperationsWidget(mitk::SliceNavigationController* timeNavigationController, QWidget* parent)
  : QmitkSegmentationUtilityWidget(timeNavigationController, parent)
{
  m_Controls.setupUi(this);

  m_Controls.dataSelectionWidget->AddDataStorageComboBox(QmitkDataSelectionWidget::SegmentationPredicate);
  m_Controls.dataSelectionWidget->SetHelpText(HelpText);

  connect(m_Controls.btnClosing, SIGNAL(clicked()), this, SLOT(OnClosingButtonClicked()));
  connect(m_Controls.btnOpening, SIGNAL(clicked()), this, SLOT(OnOpeningButtonClicked()));
  connect(m_Controls.btnDilatation, SIGNAL(clicked()), this, SLOT(OnDilatationButtonClicked()));
  connect(m_Controls.btnErosion, SIGNAL(clicked()), this, SLOT(OnErosionButtonClicked()));
  connect(m_Controls.btnFillHoles, SIGNAL(clicked()), this, SLOT(OnFillHolesButtonClicked()));
  connect(m_Controls.radioButtonMorphoCross, SIGNAL(clicked()), this, SLOT(OnRadioButtonsClicked()));
  connect(m_Controls.radioButtonMorphoBall, SIGNAL(clicked()), this, SLOT(OnRadioButtonsClicked()));
  connect(m_Controls.dataSelectionWidget, SIGNAL(SelectionChanged(unsigned int, const mitk::DataNode*)), this, SLOT(OnSelectionChanged(unsigned int, const mitk::DataNode*)));

  if (m_Controls.dataSelectionWidget->GetSelection(0).IsNotNull())
    this->OnSelectionChanged(0, m_Controls.dataSelectionWidget->GetSelection(0));
}

QmitkMorphologicalOperationsWidget::~QmitkMorphologicalOperationsWidget()
{
}

void QmitkMorphologicalOperationsWidget::OnSelectionChanged(unsigned int, const mitk::DataNode*)
{
  QmitkDataSelectionWidget* dataSelectionWidget = m_Controls.dataSelectionWidget;
  mitk::DataNode::Pointer node = dataSelectionWidget->GetSelection(0);

  if (node.IsNotNull())
  {
    m_Controls.dataSelectionWidget->SetHelpText("");
    this->EnableButtons(true);
  }
  else
  {
    m_Controls.dataSelectionWidget->SetHelpText(HelpText);
    this->EnableButtons(false);
  }
}

void QmitkMorphologicalOperationsWidget::EnableButtons(bool enable)
{
  m_Controls.btnClosing->setEnabled(enable);
  m_Controls.btnDilatation->setEnabled(enable);
  m_Controls.btnErosion->setEnabled(enable);
  m_Controls.btnFillHoles->setEnabled(enable);
  m_Controls.btnOpening->setEnabled(enable);
}

void QmitkMorphologicalOperationsWidget::OnRadioButtonsClicked()
{
  bool enable = m_Controls.radioButtonMorphoBall->isChecked();

  m_Controls.sliderMorphFactor->setEnabled(enable);
  m_Controls.spinBoxMorphFactor->setEnabled(enable);
}

void QmitkMorphologicalOperationsWidget::OnClosingButtonClicked()
{
  QApplication::setOverrideCursor(QCursor(Qt::BusyCursor));
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();

  QmitkDataSelectionWidget* dataSelectionWidget = m_Controls.dataSelectionWidget;
  mitk::DataNode::Pointer node = dataSelectionWidget->GetSelection(0);
  mitk::Image::Pointer image = static_cast<mitk::Image*>(node->GetData());
  mitk::MorphologicalOperations::StructuralElementType structuralElement = CreateStructerElement_UI();
  try
  {
    int factor = m_Controls.spinBoxMorphFactor->isEnabled()
      ? m_Controls.spinBoxMorphFactor->value()
      : 1;

    mitk::MorphologicalOperations::Closing(image, factor, structuralElement);
  }
  catch (const itk::ExceptionObject& exception)
  {
    MITK_WARN << "Exception caught: " << exception.GetDescription();

    QApplication::restoreOverrideCursor();
    return;
  }

  node->SetData(image);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
  QApplication::restoreOverrideCursor();
}

void QmitkMorphologicalOperationsWidget::OnOpeningButtonClicked()
{
  QApplication::setOverrideCursor(QCursor(Qt::BusyCursor));
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();

  QmitkDataSelectionWidget* dataSelectionWidget = m_Controls.dataSelectionWidget;
  mitk::DataNode::Pointer node = dataSelectionWidget->GetSelection(0);
  mitk::Image::Pointer image = static_cast<mitk::Image*>(node->GetData());

  mitk::MorphologicalOperations::StructuralElementType structuralElement = CreateStructerElement_UI();

  try
  {
    int factor = m_Controls.spinBoxMorphFactor->isEnabled()
      ? m_Controls.spinBoxMorphFactor->value()
      : 1;

     mitk::MorphologicalOperations::Opening(image, factor, structuralElement);
  }
  catch (const itk::ExceptionObject& exception)
  {
     MITK_WARN << "Exception caught: " << exception.GetDescription();

     QApplication::restoreOverrideCursor();
     return;
  }

  node->SetData(image);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
  QApplication::restoreOverrideCursor();
}

void QmitkMorphologicalOperationsWidget::OnDilatationButtonClicked()
{
  QApplication::setOverrideCursor(QCursor(Qt::BusyCursor));
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();

  QmitkDataSelectionWidget* dataSelectionWidget = m_Controls.dataSelectionWidget;
  mitk::DataNode::Pointer node = dataSelectionWidget->GetSelection(0);
  mitk::Image::Pointer image = static_cast<mitk::Image*>(node->GetData());
  mitk::MorphologicalOperations::StructuralElementType structuralElement = this->CreateStructerElement_UI();

  try
  {
    int factor = m_Controls.spinBoxMorphFactor->isEnabled()
      ? m_Controls.spinBoxMorphFactor->value()
      : 1;

    mitk::MorphologicalOperations::Dilate(image, factor, structuralElement);
  }
  catch (const itk::ExceptionObject& exception)
  {
    MITK_WARN << "Exception caught: " << exception.GetDescription();

    QApplication::restoreOverrideCursor();
    return;
  }

  node->SetData(image);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
  QApplication::restoreOverrideCursor();
}

void QmitkMorphologicalOperationsWidget::OnErosionButtonClicked()
{
  QApplication::setOverrideCursor(QCursor(Qt::BusyCursor));
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();

  QmitkDataSelectionWidget* dataSelectionWidget = m_Controls.dataSelectionWidget;
  mitk::DataNode::Pointer node = dataSelectionWidget->GetSelection(0);
  mitk::Image::Pointer image = static_cast<mitk::Image*>(node->GetData());
mitk::MorphologicalOperations::StructuralElementType structuralElement = CreateStructerElement_UI();

  try
  {
    int factor = m_Controls.spinBoxMorphFactor->isEnabled()
      ? m_Controls.spinBoxMorphFactor->value()
      : 1;

    mitk::MorphologicalOperations::Erode(image, factor, structuralElement);
  }
  catch (const itk::ExceptionObject& exception)
  {
    MITK_WARN << "Exception caught: " << exception.GetDescription();

    QApplication::restoreOverrideCursor();
    return;
 }

  node->SetData(image);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
  QApplication::restoreOverrideCursor();
}

void QmitkMorphologicalOperationsWidget::OnFillHolesButtonClicked()
{
  QApplication::setOverrideCursor(QCursor(Qt::BusyCursor));
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();

  QmitkDataSelectionWidget* dataSelectionWidget = m_Controls.dataSelectionWidget;
  mitk::DataNode::Pointer node = dataSelectionWidget->GetSelection(0);
  mitk::Image::Pointer image = static_cast<mitk::Image*>(node->GetData());

  try
  {
    mitk::MorphologicalOperations::FillHoles(image);
  }
  catch (const itk::ExceptionObject& exception)
  {
    MITK_WARN << "Exception caught: " << exception.GetDescription();

    QApplication::restoreOverrideCursor();
    return;
  }

  node->SetData(image);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
  QApplication::restoreOverrideCursor();
}


mitk::MorphologicalOperations::StructuralElementType QmitkMorphologicalOperationsWidget::CreateStructerElement_UI()
{
  bool ball = m_Controls.radioButtonMorphoBall->isChecked();
  int accum_flag = 0;
  if(ball){
   if(m_Controls.planeSelectionComboBox->currentIndex() == 0) accum_flag = mitk::MorphologicalOperations::Ball; // 3D Operation
   if(m_Controls.planeSelectionComboBox->currentIndex() == 1) accum_flag = mitk::MorphologicalOperations::Ball_Axial; // 2D Operation - Axial plane
   if(m_Controls.planeSelectionComboBox->currentIndex() == 2) accum_flag = mitk::MorphologicalOperations::Ball_Sagital; // 2D Operation - Sagital plane
   if(m_Controls.planeSelectionComboBox->currentIndex() == 3) accum_flag = mitk::MorphologicalOperations::Ball_Coronal; // 2D Operation - Coronal plane
  }else{
    if(m_Controls.planeSelectionComboBox->currentIndex() == 0) accum_flag = mitk::MorphologicalOperations::Cross;
    if(m_Controls.planeSelectionComboBox->currentIndex() == 1) accum_flag = mitk::MorphologicalOperations::Cross_Axial;
    if(m_Controls.planeSelectionComboBox->currentIndex() == 2) accum_flag = mitk::MorphologicalOperations::Cross_Sagital;
    if(m_Controls.planeSelectionComboBox->currentIndex() == 3) accum_flag = mitk::MorphologicalOperations::Cross_Coronal;
  }
  return (mitk::MorphologicalOperations::StructuralElementType)accum_flag;
}
