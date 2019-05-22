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

#ifndef QMITKSEGMENTATIONVIEW_H
#define QMITKSEGMENTATIONVIEW_H

#include <QmitkAbstractView.h>
#include <mitkILifecycleAwarePart.h>
#include <mitkIRenderWindowPartListener.h>

#include <berryIBerryPreferences.h>

#include "ui_sv4gui_QmitkSegmentationControls.h"

class QmitkRenderWindow;

/**
* \ingroup ToolManagerEtAl
* \ingroup org_sv_gui_qt_mitksegmentation_internal
* \warning Implementation of this class is split up into two .cpp files to make things more compact. Check both this file and QmitkSegmentationOrganNamesHandling.cpp
*/
class QmitkSegmentationView : public QmitkAbstractView, public mitk::ILifecycleAwarePart, public mitk::IRenderWindowPartListener
{
  Q_OBJECT

public:

  QmitkSegmentationView();

  virtual ~QmitkSegmentationView();

  typedef std::map<mitk::DataNode*, unsigned long> NodeTagMapType;

  /*!
  \brief Invoked when the DataManager selection changed
  */
  virtual void OnSelectionChanged(mitk::DataNode* node);
  virtual void OnSelectionChanged(berry::IWorkbenchPart::Pointer part, const QList<mitk::DataNode::Pointer>& nodes) override;

  // reaction to new segmentations being created by segmentation tools
  void NewNodesGenerated();
  void NewNodeObjectsGenerated(mitk::ToolManager::DataVectorType*);

  virtual void Activated() override;
  virtual void Deactivated() override;
  virtual void Visible() override;
  virtual void Hidden() override;

  ///
  /// Sets the focus to an internal widget.
  ///
  virtual void SetFocus() override;

  virtual void RenderWindowPartActivated(mitk::IRenderWindowPart* renderWindowPart) override;

  virtual void RenderWindowPartDeactivated(mitk::IRenderWindowPart* renderWindowPart) override;

  // BlueBerry's notification about preference changes (e.g. from a dialog)
  virtual void OnPreferencesChanged(const berry::IBerryPreferences* prefs) override;

  // observer to mitk::RenderingManager's RenderingManagerViewsInitializedEvent event
  void RenderingManagerReinitialized();

  // observer to mitk::SliceController's SliceRotation event
  void SliceRotation(const itk::EventObject&);

  static const std::string VIEW_ID;

  protected slots:

  void OnPatientComboBoxSelectionChanged(const mitk::DataNode* node);
  void OnSegmentationComboBoxSelectionChanged(const mitk::DataNode* node);

  // reaction to the button "New segmentation"
  void CreateNewSegmentation();

  void OnManualTool2DSelected(int id);

  void OnVisiblePropertyChanged();

  void OnBinaryPropertyChanged();

  void OnShowMarkerNodes(bool);

  void OnTabWidgetChanged(int);

protected:

  // a type for handling lists of DataNodes
  typedef std::vector<mitk::DataNode*> NodeList;

  // GUI setup
  virtual void CreateQtPartControl(QWidget* parent) override;

  // reactions to selection events from data manager (and potential other senders)
  //void BlueBerrySelectionChanged(berry::IWorkbenchPart::Pointer sourcepart, berry::ISelection::ConstPointer selection);
  mitk::DataNode::Pointer FindFirstRegularImage(std::vector<mitk::DataNode*> nodes);
  mitk::DataNode::Pointer FindFirstSegmentation(std::vector<mitk::DataNode*> nodes);

  // initially set the tool manager selection from the combo boxes
  void InitToolManagerSelection(const mitk::DataNode* referenceData, const mitk::DataNode* workingData);

  // propagate BlueBerry selection to ToolManager for manual segmentation
  void SetToolManagerSelection(const mitk::DataNode* referenceData, const mitk::DataNode* workingData);

  // checks if given render window aligns with the slices of given image
  bool IsRenderWindowAligned(QmitkRenderWindow* renderWindow, mitk::Image* image);

  // make sure all images/segmentations look as selected by the users in this view's preferences
  void ForceDisplayPreferencesUponAllImages();

  // decorates a DataNode according to the user preference settings
  void ApplyDisplayOptions(mitk::DataNode* node);

  void ResetMouseCursor();

  void SetMouseCursor(const us::ModuleResource&, int hotspotX, int hotspotY);

  void SetToolSelectionBoxesEnabled(bool);

  // If a contourmarker is selected, the plane in the related widget will be reoriented according to the marker`s geometry
  void OnContourMarkerSelected(const mitk::DataNode* node);

  void NodeRemoved(const mitk::DataNode* node) override;

  void NodeAdded(const mitk::DataNode *node) override;

  bool CheckForSameGeometry(const mitk::DataNode*, const mitk::DataNode*) const;

  void UpdateWarningLabel(QString text/*, bool overwriteExistingText = true*/);

  // the Qt parent of our GUI (NOT of this object)
  QWidget* m_Parent;

  // our GUI
  Ui::QmitkSegmentationControls* m_Controls;

  mitk::IRenderWindowPart* m_RenderWindowPart;

  unsigned long m_VisibilityChangedObserverTag;

  bool m_MouseCursorSet;

  bool m_DataSelectionChanged;

  NodeTagMapType  m_WorkingDataObserverTags;

  NodeTagMapType  m_BinaryPropertyObserverTags;

  unsigned int m_RenderingManagerObserverTag;

  bool m_AutoSelectionEnabled;

  mitk::NodePredicateNot::Pointer m_IsNotAHelperObject;
  mitk::NodePredicateAnd::Pointer m_IsOfTypeImagePredicate;
  mitk::NodePredicateOr::Pointer m_IsASegmentationImagePredicate;
  mitk::NodePredicateAnd::Pointer m_IsAPatientImagePredicate;
};

#endif // QMITKSEGMENTATIONVIEW_H
