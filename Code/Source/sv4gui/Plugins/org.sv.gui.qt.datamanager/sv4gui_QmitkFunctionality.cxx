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

#include "sv4gui_QmitkFunctionality.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>
#include <berryIWorkbenchWindow.h>
#include <berryISelectionService.h>
#include <berryISelectionProvider.h>

// #include <internal/QmitkFunctionalityUtil.h>
// #include <internal/QmitkCommonLegacyActivator.h>
#include <QmitkStdMultiWidgetEditor.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <usModuleRegistry.h>

#include <vtkProperty.h>

#include <QMessageBox>
#include <QShortcut>
#include <QInputDialog>
#include <QWheelEvent>
#include <QScrollArea>
#include <QVBoxLayout>
#include <QApplication>
#include <QFileDialog>

#include <iostream>
using namespace std;

sv4guiQmitkFunctionality::sv4guiQmitkFunctionality()
{
}

sv4guiQmitkFunctionality::~sv4guiQmitkFunctionality()
{
  this->Register();
  this->ClosePartProxy();

  this->UnRegister(false);
}

// --------- FOLLOWING FROM QmitkFunctionality ------------------------------
QList<mitk::DataNode::Pointer> sv4guiQmitkFunctionality::GetDataManagerSelection() const
{
  // taken from same method implementation in QmitkAbstractView
  berry::ISelection::ConstPointer selection( this->GetSite()->GetWorkbenchWindow()->GetSelectionService()->GetSelection("org.sv.views.datamanager"));
  mitk::DataNodeSelection::ConstPointer currentSelection = selection.Cast<const mitk::DataNodeSelection>();

  return DataNodeSelectionToQList(currentSelection);
}

void sv4guiQmitkFunctionality::CreatePartControl(QWidget* parent)
{
  // scrollArea
  QScrollArea* scrollArea = new QScrollArea;
  // QVBoxLayout* scrollAreaLayout = new QVBoxLayout(scrollArea);
  scrollArea->setFrameShadow(QFrame::Plain);
  scrollArea->setFrameShape(QFrame::NoFrame);
  scrollArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
  scrollArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);

  // m_Parent
  m_Parent = new QWidget;
  m_Parent->setObjectName("svPluginEditWindow");
  // m_Parent->setSizePolicy(QSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding));
  this->CreateQtPartControl(m_Parent);

  //scrollAreaLayout->addWidget(m_Parent);
  //scrollArea->setLayout(scrollAreaLayout);

  // set the widget now
  scrollArea->setWidgetResizable(true);
  scrollArea->setWidget(m_Parent);

  // add the scroll area to the real parent (the view tabbar)
  QWidget* parentQWidget = static_cast<QWidget*>(parent);
  QVBoxLayout* parentLayout = new QVBoxLayout(parentQWidget);
  parentLayout->setMargin(0);
  parentLayout->setSpacing(0);
  parentLayout->addWidget(scrollArea);

  // finally set the layout containing the scroll area to the parent widget (= show it)
  parentQWidget->setLayout(parentLayout);

  this->AfterCreateQtPartControl();
}

// this function is also empty in QmitkAbstractView
void sv4guiQmitkFunctionality::NodeAdded( const mitk::DataNode*  /*node*/ )
{

}

// this function is also empty in QmitkAbstractView
void sv4guiQmitkFunctionality::NodeRemoved( const mitk::DataNode*  /*node*/ )
{

}

// this function is also empty in QmitkAbstractView
void sv4guiQmitkFunctionality::NodeChanged( const mitk::DataNode* /*node*/ )
{

}

// this function is also empty in QmitkAbstractView
void sv4guiQmitkFunctionality::OnSelectionChanged(berry::IWorkbenchPart::Pointer /*part*/,
                        const QList<mitk::DataNode::Pointer>& /*nodes*/)
{

}

// this function is also empty in QmitkAbstractView
void sv4guiQmitkFunctionality::OnNullSelection(berry::IWorkbenchPart::Pointer /*part*/)
{
}


void sv4guiQmitkFunctionality::AfterCreateQtPartControl()
{
  this->GetDataStorage()->AddNodeEvent.AddListener( mitk::MessageDelegate1<sv4guiQmitkFunctionality, const mitk::DataNode*>
    ( this, &sv4guiQmitkFunctionality::NodeAdded ) );
  this->GetDataStorage()->ChangedNodeEvent.AddListener( mitk::MessageDelegate1<sv4guiQmitkFunctionality, const mitk::DataNode*>
    ( this, &sv4guiQmitkFunctionality::NodeChanged ) );
  this->GetDataStorage()->RemoveNodeEvent.AddListener( mitk::MessageDelegate1<sv4guiQmitkFunctionality, const mitk::DataNode*>
    ( this, &sv4guiQmitkFunctionality::NodeRemoved ) );

  // REGISTER PREFERENCES LISTENER
  berry::IBerryPreferences::Pointer prefs = this->GetPreferences().Cast<berry::IBerryPreferences>();
  // OnPreferencesChanged has become private in QmitkAbstractView
  if(prefs.IsNotNull())
    prefs->OnChanged.AddListener(berry::MessageDelegate1<sv4guiQmitkFunctionality
    , const berry::IBerryPreferences*>(this, &sv4guiQmitkFunctionality::OnPreferencesChanged));

  // REGISTER FOR WORKBENCH SELECTION EVENTS
  m_BlueBerrySelectionListener.reset(new berry::SelectionChangedAdapter<sv4guiQmitkFunctionality>(
                                       this,
                                       &sv4guiQmitkFunctionality::BlueBerrySelectionChanged)
                                     );
  this->GetSite()->GetWorkbenchWindow()->GetSelectionService()->AddPostSelectionListener(
        /*"org.sv.views.datamanager",*/ m_BlueBerrySelectionListener.data());

  // EMULATE INITIAL SELECTION EVENTS

  // send datamanager selection
  this->OnSelectionChanged(berry::IWorkbenchPart::Pointer(),this->GetDataManagerSelection());

  // send preferences changed event
  this->OnPreferencesChanged(this->GetPreferences().Cast<berry::IBerryPreferences>().GetPointer());
}

void sv4guiQmitkFunctionality::OnPreferencesChanged( const berry::IBerryPreferences* )
{
}

void sv4guiQmitkFunctionality::BlueBerrySelectionChanged(const berry::IWorkbenchPart::Pointer& sourcepart, const berry::ISelection::ConstPointer& selection)
{
  if(sourcepart.IsNull() || sourcepart.GetPointer() == static_cast<berry::IWorkbenchPart*>(this))
      return;

  if(selection.IsNull())
  {
    this->OnNullSelection(sourcepart);
    return;
  }

  mitk::DataNodeSelection::ConstPointer _DataNodeSelection
      = selection.Cast<const mitk::DataNodeSelection>();
  this->OnSelectionChanged(sourcepart, this->DataNodeSelectionToQList(_DataNodeSelection));
}

QList<mitk::DataNode::Pointer> sv4guiQmitkFunctionality::DataNodeSelectionToQList(mitk::DataNodeSelection::ConstPointer currentSelection) const
{
  if (currentSelection.IsNull()) return QList<mitk::DataNode::Pointer>();
  return QList<mitk::DataNode::Pointer>::fromStdList(currentSelection->GetSelectedDataNodes());
}

void sv4guiQmitkFunctionality::ClosePartProxy()
{
  this->GetDataStorage()->AddNodeEvent.RemoveListener( mitk::MessageDelegate1<sv4guiQmitkFunctionality, const mitk::DataNode*>
    ( this, &sv4guiQmitkFunctionality::NodeAdded ) );
  this->GetDataStorage()->RemoveNodeEvent.RemoveListener( mitk::MessageDelegate1<sv4guiQmitkFunctionality, const mitk::DataNode*>
    ( this, &sv4guiQmitkFunctionality::NodeRemoved) );
  this->GetDataStorage()->ChangedNodeEvent.RemoveListener( mitk::MessageDelegate1<sv4guiQmitkFunctionality, const mitk::DataNode*>
    ( this, &sv4guiQmitkFunctionality::NodeChanged ) );

  berry::IBerryPreferences::Pointer prefs = this->GetPreferences().Cast<berry::IBerryPreferences>();
  if(prefs.IsNotNull())
  {
    prefs->OnChanged.RemoveListener(berry::MessageDelegate1<sv4guiQmitkFunctionality
    , const berry::IBerryPreferences*>(this, &sv4guiQmitkFunctionality::OnPreferencesChanged));
  }

  // // REMOVE SELECTION PROVIDER
  this->GetSite()->SetSelectionProvider(berry::ISelectionProvider::Pointer(nullptr));

  berry::ISelectionService* s = GetSite()->GetWorkbenchWindow()->GetSelectionService();
  if(s)
  {
    s->RemovePostSelectionListener(m_BlueBerrySelectionListener.data());
  }
}

// --------------------------------------------------------------------------
