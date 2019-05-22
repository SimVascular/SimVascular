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

#ifndef sv4gui_QmitkSurfaceToImageWidget_h
#define sv4gui_QmitkSurfaceToImageWidget_h

#include "sv4gui_QmitkSegmentationUtilityWidget.h"
#include <ui_sv4gui_QmitkSurfaceToImageWidgetControls.h>

namespace mitk {
  class Surface;
  class Image;
  class LabelSetImage;
}

/*!
  \brief QmitkSurfaceToImageWidget

  The Tool converts a surface to a binary image. The Method requires
  a surface and an image, which header information defines the output
  image. The resulting binary image has the same dimension, size, and
  Geometry3D as the input image.
*/
class QmitkSurfaceToImageWidget : public QmitkSegmentationUtilityWidget
{
  Q_OBJECT

public:

  /** @brief Default constructor, including creation of GUI elements and signals/slots connections. */
  explicit QmitkSurfaceToImageWidget(mitk::SliceNavigationController* timeNavigationController, QWidget* parent = nullptr);

  /** @brief Defaul destructor. */
  ~QmitkSurfaceToImageWidget();

private slots:

  /** @brief This slot is called if the selection in the workbench is changed. */
  void OnSelectionChanged(unsigned int index, const mitk::DataNode* selection);

  /** @brief This slot is called if user activates the button to convert a surface into a binary image. */
  void OnSurface2ImagePressed();

private:

  /** @brief Enable buttons if data selction is valid. */
  void EnableButtons(bool enable = true);

  /** @brief Convert a surface into an binary image. */
  itk::SmartPointer<mitk::LabelSetImage> ConvertSurfaceToImage( itk::SmartPointer<mitk::Image> image, itk::SmartPointer<mitk::Surface> surface );

  Ui::QmitkSurfaceToImageWidgetControls m_Controls;
};

#endif
