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

#ifndef QMITKVOLUMEVISUALIZATIONVIEW_H_
#define QMITKVOLUMEVISUALIZATIONVIEW_H_

#include "svAbstractView.h"

#include <mitkWeakPointer.h>

#include <mitkImage.h>

#include <mitkDataStorage.h>

//#include <QmitkNodeDescriptor.h>
#include <QmitkDataStorageListModel.h>
#include <QmitkDataStorageComboBox.h>
#include <QmitkTransferFunctionWidget.h>

#include "ui_QmitkVolumeVisualizationViewControls.h"

class QmitkVolumeVisualizationView : public svAbstractView
{
  Q_OBJECT

public:

  static const QString EXTENSION_ID;

  void SetFocus() override;

  QmitkVolumeVisualizationView();

  virtual ~QmitkVolumeVisualizationView();

  virtual void CreateQtPartControl(QWidget *parent) override;

  ///
  /// Invoked when the DataManager selection changed
  ///
  virtual void OnSelectionChanged(const QList<mitk::DataNode::Pointer>& nodes) override;

  virtual void Activated() override;

  virtual void Deactivated() override;

  bool Is3DImage(QList<mitk::DataNode::Pointer> nodes);

public slots:

  void ShowVolumeRenderingPane();
  void ShowVolumeRenderingPaneForImage();

protected slots:

  void OnMitkInternalPreset( int mode );

  void OnEnableRendering( bool state );
  void OnEnableLOD( bool state );
  void OnRenderMode( int mode );


protected:

  Ui::QmitkVolumeVisualizationViewControls* m_Controls;

  std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > mDescriptorActionList;

private:

  mitk::WeakPointer<mitk::DataNode> m_SelectedNode;

  void UpdateInterface();

  void NodeRemoved(const mitk::DataNode* node) override;

};

#endif /*QMITKVOLUMEVISUALIZATIONVIEW_H_*/
