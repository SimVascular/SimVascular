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

#include "sv4gui_QmitkDataSelectionWidget.h"
#include "sv4gui_mitkPluginActivator.h"

#include <berryIWorkbench.h>

#include <mitkContourModel.h>
#include <mitkContourModelSet.h>
#include <mitkIDataStorageService.h>
#include <mitkLabelSetImage.h>
#include <mitkNodePredicateAnd.h>
#include <mitkNodePredicateDataType.h>
#include <mitkNodePredicateNot.h>
#include <mitkNodePredicateOr.h>
#include <mitkNodePredicateProperty.h>
#include <mitkProperties.h>
#include <mitkSurface.h>
#include <QmitkDataStorageComboBox.h>
#include <QLabel>
#include <algorithm>
#include <cassert>
#include <iterator>

static mitk::NodePredicateBase::Pointer CreatePredicate(QmitkDataSelectionWidget::Predicate predicate)
{
  auto imageType = mitk::TNodePredicateDataType<mitk::Image>::New();
  auto labelSetImageType = mitk::TNodePredicateDataType<mitk::LabelSetImage>::New();
  auto surfaceType = mitk::TNodePredicateDataType<mitk::Surface>::New();
  auto contourModelType = mitk::TNodePredicateDataType<mitk::ContourModel>::New();
  auto contourModelSetType = mitk::TNodePredicateDataType<mitk::ContourModelSet>::New();
  auto nonLabelSetImageType = mitk::NodePredicateAnd::New(imageType, mitk::NodePredicateNot::New(labelSetImageType));
  auto nonHelperObject = mitk::NodePredicateNot::New(mitk::NodePredicateProperty::New("helper object"));
  auto isBinary = mitk::NodePredicateProperty::New("binary", mitk::BoolProperty::New(true));
  auto isSegmentation = mitk::NodePredicateProperty::New("segmentation", mitk::BoolProperty::New(true));
  auto isBinaryOrSegmentation = mitk::NodePredicateOr::New(isBinary, isSegmentation);

  mitk::NodePredicateBase::Pointer returnValue;

  switch(predicate)
  {
    case QmitkDataSelectionWidget::ImagePredicate:
      returnValue = mitk::NodePredicateAnd::New(
        mitk::NodePredicateNot::New(isBinaryOrSegmentation),
        nonLabelSetImageType).GetPointer();
      break;

    case QmitkDataSelectionWidget::SegmentationPredicate:
      returnValue = mitk::NodePredicateOr::New(
        mitk::NodePredicateAnd::New(imageType, isBinaryOrSegmentation),
        labelSetImageType).GetPointer();
      break;

    case QmitkDataSelectionWidget::SurfacePredicate:
      returnValue = surfaceType.GetPointer();
      break;

    case QmitkDataSelectionWidget::ImageAndSegmentationPredicate:
      returnValue = imageType.GetPointer();
      break;

    case QmitkDataSelectionWidget::ContourModelPredicate:
      returnValue = mitk::NodePredicateOr::New(
        contourModelSetType,
        contourModelSetType).GetPointer();
      break;

    default:
      assert(false && "Unknown predefined predicate!");
      return nullptr;
  }

  return mitk::NodePredicateAnd::New(returnValue, nonHelperObject).GetPointer();
}

QmitkDataSelectionWidget::QmitkDataSelectionWidget(QWidget* parent)
  : QWidget(parent)
{
  m_Controls.setupUi(this);
  m_Controls.helpLabel->hide();
}

QmitkDataSelectionWidget::~QmitkDataSelectionWidget()
{
}

unsigned int QmitkDataSelectionWidget::AddDataStorageComboBox(QmitkDataSelectionWidget::Predicate predicate)
{
  return this->AddDataStorageComboBox("", predicate);
}

unsigned int QmitkDataSelectionWidget::AddDataStorageComboBox(mitk::NodePredicateBase* predicate)
{
  return this->AddDataStorageComboBox("", predicate);
}

unsigned int QmitkDataSelectionWidget::AddDataStorageComboBox(const QString &labelText, QmitkDataSelectionWidget::Predicate predicate)
{
  return this->AddDataStorageComboBox(labelText, CreatePredicate(predicate));
}

unsigned int QmitkDataSelectionWidget::AddDataStorageComboBox(const QString &labelText, mitk::NodePredicateBase* predicate)
{
  int row = m_Controls.gridLayout->rowCount();

  if (!labelText.isEmpty())
  {
    QLabel* label = new QLabel(labelText, m_Controls.dataSelectionWidget);
    label->setSizePolicy(QSizePolicy::Maximum, QSizePolicy::Preferred);
    m_Controls.gridLayout->addWidget(label, row, 0);
  }

  QmitkDataStorageComboBox* comboBox = new QmitkDataStorageComboBox(this->GetDataStorage(), predicate, m_Controls.dataSelectionWidget);
  connect(comboBox, SIGNAL(OnSelectionChanged(const mitk::DataNode *)), this, SLOT(OnSelectionChanged(const mitk::DataNode *)));
  m_Controls.gridLayout->addWidget(comboBox, row, 1);

  m_DataStorageComboBoxes.push_back(comboBox);
  return static_cast<unsigned int>(m_DataStorageComboBoxes.size() - 1);
}

mitk::DataStorage::Pointer QmitkDataSelectionWidget::GetDataStorage() const
{
  ctkServiceReference ref = mitk::PluginActivator::getContext()->getServiceReference<mitk::IDataStorageService>();
  assert(ref == true);

  mitk::IDataStorageService* service = mitk::PluginActivator::getContext()->getService<mitk::IDataStorageService>(ref);

  assert(service);

  return service->GetDefaultDataStorage()->GetDataStorage();
}

mitk::DataNode::Pointer QmitkDataSelectionWidget::GetSelection(unsigned int index)
{
  assert(index < m_DataStorageComboBoxes.size());
  return m_DataStorageComboBoxes[index]->GetSelectedNode();
}

void QmitkDataSelectionWidget::SetPredicate(unsigned int index, Predicate predicate)
{
  this->SetPredicate(index, CreatePredicate(predicate));
}

void QmitkDataSelectionWidget::SetPredicate(unsigned int index, mitk::NodePredicateBase* predicate)
{
  assert(index < m_DataStorageComboBoxes.size());
  m_DataStorageComboBoxes[index]->SetPredicate(predicate);
}

void QmitkDataSelectionWidget::SetHelpText(const QString& text)
{
  if (!text.isEmpty())
  {
    m_Controls.helpLabel->setText(text);

    if (!m_Controls.helpLabel->isVisible())
      m_Controls.helpLabel->show();
  }
  else
  {
    m_Controls.helpLabel->hide();
  }
}

void QmitkDataSelectionWidget::OnSelectionChanged(const mitk::DataNode* selection)
{
  std::vector<QmitkDataStorageComboBox*>::iterator it = std::find(m_DataStorageComboBoxes.begin(), m_DataStorageComboBoxes.end(), sender());
  assert(it != m_DataStorageComboBoxes.end());

  emit SelectionChanged(std::distance(m_DataStorageComboBoxes.begin(), it), selection);
}
