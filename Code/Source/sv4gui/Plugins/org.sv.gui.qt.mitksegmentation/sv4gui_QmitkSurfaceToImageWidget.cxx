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

#include "sv4gui_QmitkSurfaceToImageWidget.h"

#include <mitkException.h>
#include <mitkExceptionMacro.h>
#include <mitkProgressBar.h>
#include <mitkProperties.h>
#include <mitkSurfaceToImageFilter.h>
#include <mitkSurface.h>
#include <mitkImage.h>
#include <mitkLabelSetImage.h>

#include <qmessagebox.h>

static const char* const HelpText = "Select an image and a surface above";

QmitkSurfaceToImageWidget::QmitkSurfaceToImageWidget(mitk::SliceNavigationController* timeNavigationController, QWidget* parent)
  : QmitkSegmentationUtilityWidget(timeNavigationController, parent)
{
  m_Controls.setupUi(this);

  m_Controls.dataSelectionWidget->AddDataStorageComboBox(QmitkDataSelectionWidget::ImageAndSegmentationPredicate);
  m_Controls.dataSelectionWidget->AddDataStorageComboBox(QmitkDataSelectionWidget::SurfacePredicate);
  m_Controls.dataSelectionWidget->SetHelpText(HelpText);

  this->EnableButtons(false);

  connect (m_Controls.btnSurface2Image, SIGNAL(pressed()), this, SLOT(OnSurface2ImagePressed()));
  connect(m_Controls.dataSelectionWidget, SIGNAL(SelectionChanged(unsigned int, const mitk::DataNode*)),
    this, SLOT(OnSelectionChanged(unsigned int, const mitk::DataNode*)));

  if( m_Controls.dataSelectionWidget->GetSelection(0).IsNotNull() &&
    m_Controls.dataSelectionWidget->GetSelection(1).IsNotNull() )
  {
    this->OnSelectionChanged(0, m_Controls.dataSelectionWidget->GetSelection(0));
  }
}

QmitkSurfaceToImageWidget::~QmitkSurfaceToImageWidget()
{
}

void QmitkSurfaceToImageWidget::EnableButtons(bool enable)
{
  m_Controls.btnSurface2Image->setEnabled(enable);
}

void QmitkSurfaceToImageWidget::OnSelectionChanged(unsigned int, const mitk::DataNode*)
{
  QmitkDataSelectionWidget* dataSelectionWidget = m_Controls.dataSelectionWidget;
  mitk::DataNode::Pointer imageNode = dataSelectionWidget->GetSelection(0);
  mitk::DataNode::Pointer surfaceNode = dataSelectionWidget->GetSelection(1);

  if (imageNode.IsNull() || surfaceNode.IsNull() )
  {
    dataSelectionWidget->SetHelpText(HelpText);
    this->EnableButtons(false);
  }
  else
  {
    mitk::Image::Pointer image = dynamic_cast<mitk::Image*>( dataSelectionWidget->GetSelection(0)->GetData() );
    mitk::Surface::Pointer surface = dynamic_cast<mitk::Surface*>( dataSelectionWidget->GetSelection(1)->GetData() );
    if( image->GetTimeSteps() != surface->GetTimeSteps() )
    {
      dataSelectionWidget->SetHelpText("Image and surface are of different size");
      this->EnableButtons(false);
    }
    else
    {
      dataSelectionWidget->SetHelpText("");
      this->EnableButtons();
    }
  }
}

void QmitkSurfaceToImageWidget::OnSurface2ImagePressed()
{
  this->EnableButtons(false);

  QmitkDataSelectionWidget* dataSelectionWidget = m_Controls.dataSelectionWidget;
  mitk::Image::Pointer image = dynamic_cast<mitk::Image*>( dataSelectionWidget->GetSelection(0)->GetData() );
  mitk::Surface::Pointer surface = dynamic_cast<mitk::Surface*>( dataSelectionWidget->GetSelection(1)->GetData() );

  if( image.IsNull() || surface.IsNull())
  {
    MITK_ERROR << "Selection does not contain an image and/or a surface";
    QMessageBox::information( this, "Surface To Image", "Selection does not contain an image and/or a surface", QMessageBox::Ok );
    this->EnableButtons();
    return;
  }

  mitk::Image::Pointer resultImage(nullptr);
  resultImage = this->ConvertSurfaceToImage( image, surface );

  if( resultImage.IsNull() )
  {
    MITK_ERROR << "Convert Surface to binary image failed";
    QMessageBox::information( this, "Surface To Image", "Convert Surface to binary image failed", QMessageBox::Ok );
    this->EnableButtons();
    return;
  }

  //create name for result node
  std::string nameOfResultImage = dataSelectionWidget->GetSelection(0)->GetName();
  nameOfResultImage.append("_");
  nameOfResultImage.append(dataSelectionWidget->GetSelection(1)->GetName());

  //create data node and add to data storage
  mitk::DataNode::Pointer resultNode = mitk::DataNode::New();
  resultNode->SetData( resultImage );
  resultNode->SetProperty("name", mitk::StringProperty::New(nameOfResultImage) );
//  resultNode->SetProperty("binary", mitk::BoolProperty::New(true) );

  dataSelectionWidget->GetDataStorage()->Add(resultNode, dataSelectionWidget->GetSelection(0));

  this->EnableButtons();
}

mitk::LabelSetImage::Pointer QmitkSurfaceToImageWidget::ConvertSurfaceToImage( mitk::Image::Pointer image, mitk::Surface::Pointer surface )
{
  mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
  mitk::ProgressBar::GetInstance()->Progress();

  mitk::SurfaceToImageFilter::Pointer surfaceToImageFilter = mitk::SurfaceToImageFilter::New();
  surfaceToImageFilter->MakeOutputBinaryOn();
  surfaceToImageFilter->SetInput(surface);
  surfaceToImageFilter->SetImage(image);
  try
  {
    surfaceToImageFilter->Update();
  }
  catch(itk::ExceptionObject& excpt)
  {
    MITK_ERROR << excpt.GetDescription();
    return nullptr;
  }

  mitk::ProgressBar::GetInstance()->Progress();

  mitk::Image::Pointer resultImage = surfaceToImageFilter->GetOutput();
  mitk::LabelSetImage::Pointer multilabelImage = mitk::LabelSetImage::New();
  multilabelImage->InitializeByLabeledImage(resultImage);

  return multilabelImage;
}

