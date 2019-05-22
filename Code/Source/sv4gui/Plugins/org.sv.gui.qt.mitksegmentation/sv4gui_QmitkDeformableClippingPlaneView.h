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

#ifndef _QMITKDEFORMABLECLIPPINGPLANEVIEW_H_INCLUDED
#define _QMITKDEFORMABLECLIPPINGPLANEVIEW_H_INCLUDED


#include <ui_sv4gui_QmitkDeformableClippingPlaneViewControls.h>
#include <mitkImage.h>
#include <QmitkAbstractView.h>

typedef itk::RGBPixel< float > Color;

/*!
* \ingroup org_mitk_gui_qt_deformableSurface
*
* \brief QmitkDeformableClippingPlaneView
*
* Document your class here.
*/
class QmitkDeformableClippingPlaneView : public QmitkAbstractView
{
  Q_OBJECT

public:

  static const std::string VIEW_ID;

  QmitkDeformableClippingPlaneView();
  virtual ~QmitkDeformableClippingPlaneView();

  virtual void CreateQtPartControl(QWidget *parent) override;

  /// \brief Creation of the connections of main and control widget
  virtual void CreateConnections();

  ///
  /// Sets the focus to an internal widget.
  ///
  virtual void SetFocus() override;

protected slots:

    void OnComboBoxSelectionChanged(const mitk::DataNode* node);
    void OnCreateNewClippingPlane();
    void OnCalculateClippingVolume();

    void OnTranslationMode(bool check);
    void OnRotationMode(bool check);
    void OnDeformationMode(bool check);

protected:

  virtual void OnSelectionChanged(mitk::DataNode* node);
  virtual void OnSelectionChanged(berry::IWorkbenchPart::Pointer part, const QList<mitk::DataNode::Pointer>& nodes) override;
  virtual void NodeRemoved(const mitk::DataNode* node) override;
  virtual void NodeChanged(const mitk::DataNode* node) override;

  void UpdateView();

  Ui::QmitkDeformableClippingPlaneViewControls m_Controls;

private:
  mitk::DataStorage::SetOfObjects::ConstPointer GetAllClippingPlanes();
  mitk::Color GetLabelColor(int label);
  void DeactivateInteractionButtons();

  mitk::DataNode::Pointer m_ReferenceNode;
  mitk::DataNode::Pointer m_WorkingNode;
};

#endif
