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

#ifndef sv4gui_QmitkSegmentationUtilitiesView_h
#define sv4gui_QmitkSegmentationUtilitiesView_h

#include <ui_sv4gui_QmitkSegmentationUtilitiesViewControls.h>
#include <mitkIRenderWindowPartListener.h>
#include <QmitkAbstractView.h>

class QmitkBooleanOperationsWidget;
class QmitkContourModelToImageWidget;
class QmitkImageMaskingWidget;
class QmitkMorphologicalOperationsWidget;
class QmitkSurfaceToImageWidget;

class QmitkSegmentationUtilitiesView : public QmitkAbstractView, public mitk::IRenderWindowPartListener
{
  Q_OBJECT

public:
  QmitkSegmentationUtilitiesView();
  ~QmitkSegmentationUtilitiesView();

  void CreateQtPartControl(QWidget* parent) override;
  void SetFocus() override;

  void RenderWindowPartActivated(mitk::IRenderWindowPart* renderWindowPart) override;
  void RenderWindowPartDeactivated(mitk::IRenderWindowPart* renderWindowPart) override;

private:
  void AddUtilityWidget(QWidget* widget, const QIcon& icon, const QString& text);

  Ui::QmitkSegmentationUtilitiesViewControls m_Controls;
  QmitkBooleanOperationsWidget* m_BooleanOperationsWidget;
  QmitkContourModelToImageWidget* m_ContourModelToImageWidget;
  QmitkImageMaskingWidget* m_ImageMaskingWidget;
  QmitkMorphologicalOperationsWidget* m_MorphologicalOperationsWidget;
  QmitkSurfaceToImageWidget* m_SurfaceToImageWidget;
};

#endif
