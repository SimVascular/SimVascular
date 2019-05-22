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

#ifndef sv4gui_QmitkSegmentationUtilityWidget_h
#define sv4gui_QmitkSegmentationUtilityWidget_h

#include <QWidget>

namespace mitk
{
  class SliceNavigationController;
}

/** \brief Base class for segmentation utility widgets that need access to the time navigation controller.
 *
 * Call GetTimeNavigationController() in your derived class to gain access to the time navigation controller.
 * The time navigation controller is not not available at all times and hence this method can return nullptr.
 */
class QmitkSegmentationUtilityWidget : public QWidget
{
  Q_OBJECT

public:
  explicit QmitkSegmentationUtilityWidget(mitk::SliceNavigationController* timeNavigationController, QWidget* parent = nullptr);
  virtual ~QmitkSegmentationUtilityWidget();

  /** \brief Usually called only from QmitkSegmentationUtilitiesView::RenderWindowPartActivated() and QmitkSegmentationUtilitiesView::RenderWindowPartDeactivated().
   */
  void SetTimeNavigationController(mitk::SliceNavigationController* timeNavigationController);

protected:
  /** \brief Call this method to access the time navigation controller.
   *
   * \return Pointer to the time navigation controller or nullptr, if it is not available.
   */
  mitk::SliceNavigationController* GetTimeNavigationController() const;

private:
  mitk::SliceNavigationController* m_TimeNavigationController;
};

#endif
