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

#include <internal/QmitkFunctionalityUtil.h>
#include <internal/QmitkCommonLegacyActivator.h>
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
std::vector<mitk::DataNode*> sv4guiQmitkFunctionality::GetDataManagerSelection() const
{
  berry::ISelection::ConstPointer selection( this->GetSite()->GetWorkbenchWindow()->GetSelectionService()->GetSelection("org.sv.views.datamanager"));
    // buffer for the data manager selection
  mitk::DataNodeSelection::ConstPointer currentSelection = selection.Cast<const mitk::DataNodeSelection>();
  return this->DataNodeSelectionToVector(currentSelection);
}

void sv4guiQmitkFunctionality::CreatePartControl(QWidget* parent)
{
  // scrollArea
  QScrollArea* scrollArea = new QScrollArea;
  //QVBoxLayout* scrollAreaLayout = new QVBoxLayout(scrollArea);
  scrollArea->setFrameShadow(QFrame::Plain);
  scrollArea->setFrameShape(QFrame::NoFrame);
  scrollArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
  scrollArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);

  // m_Parent
  m_Parent = new QWidget;
  m_Parent->setObjectName("svPluginEditWindow");
  //m_Parent->setSizePolicy(QSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding));
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

void sv4guiQmitkFunctionality::AfterCreateQtPartControl()
{
  // REGISTER DATASTORAGE LISTENER
  this->GetDefaultDataStorage()->AddNodeEvent.AddListener( mitk::MessageDelegate1<QmitkFunctionality, const mitk::DataNode*>
    ( this, &QmitkFunctionality::NodeAddedProxy ) );
  this->GetDefaultDataStorage()->ChangedNodeEvent.AddListener( mitk::MessageDelegate1<QmitkFunctionality, const mitk::DataNode*>
    ( this, &QmitkFunctionality::NodeChangedProxy ) );
  this->GetDefaultDataStorage()->RemoveNodeEvent.AddListener( mitk::MessageDelegate1<QmitkFunctionality, const mitk::DataNode*>
    ( this, &QmitkFunctionality::NodeRemovedProxy ) );

  // REGISTER PREFERENCES LISTENER
  berry::IBerryPreferences::Pointer prefs = this->GetPreferences().Cast<berry::IBerryPreferences>();
  if(prefs.IsNotNull())
    prefs->OnChanged.AddListener(berry::MessageDelegate1<QmitkFunctionality
    , const berry::IBerryPreferences*>(this, &QmitkFunctionality::OnPreferencesChanged));

  // REGISTER FOR WORKBENCH SELECTION EVENTS
  m_BlueBerrySelectionListener.reset(new berry::SelectionChangedAdapter<sv4guiQmitkFunctionality>(
                                       this,
                                       &sv4guiQmitkFunctionality::BlueBerrySelectionChanged)
                                     );
  this->GetSite()->GetWorkbenchWindow()->GetSelectionService()->AddPostSelectionListener(
        /*"org.sv.views.datamanager",*/ m_BlueBerrySelectionListener.data());

  // REGISTER A SELECTION PROVIDER
  //QmitkFunctionalitySelectionProvider::Pointer _SelectionProvider(
  //      new QmitkFunctionalitySelectionProvider(this));
  //m_SelectionProvider = _SelectionProvider.GetPointer();
  //this->GetSite()->SetSelectionProvider(berry::ISelectionProvider::Pointer(m_SelectionProvider));

  // EMULATE INITIAL SELECTION EVENTS

  // by default a a multi widget is always available
  this->StdMultiWidgetAvailable(*this->GetActiveStdMultiWidget());

  // send datamanager selection
  this->OnSelectionChanged(this->GetDataManagerSelection());

  // send preferences changed event
  this->OnPreferencesChanged(this->GetPreferences().Cast<berry::IBerryPreferences>().GetPointer());
}

void sv4guiQmitkFunctionality::BlueBerrySelectionChanged(const berry::IWorkbenchPart::Pointer& sourcepart,
                                                   const berry::ISelection::ConstPointer& selection)
{
  if(sourcepart.IsNull() || sourcepart->GetSite()->GetId() != "org.sv.views.datamanager")
    return;

  mitk::DataNodeSelection::ConstPointer _DataNodeSelection
    = selection.Cast<const mitk::DataNodeSelection>();
  this->OnSelectionChanged(this->DataNodeSelectionToVector(_DataNodeSelection));
}

void sv4guiQmitkFunctionality::ClosePartProxy()
{
  this->GetDefaultDataStorage()->AddNodeEvent.RemoveListener( mitk::MessageDelegate1<QmitkFunctionality, const mitk::DataNode*>
    ( this, &QmitkFunctionality::NodeAddedProxy ) );
  this->GetDefaultDataStorage()->RemoveNodeEvent.RemoveListener( mitk::MessageDelegate1<QmitkFunctionality, const mitk::DataNode*>
    ( this, &QmitkFunctionality::NodeRemovedProxy) );
  this->GetDefaultDataStorage()->ChangedNodeEvent.RemoveListener( mitk::MessageDelegate1<QmitkFunctionality, const mitk::DataNode*>
    ( this, &QmitkFunctionality::NodeChangedProxy ) );

  berry::IBerryPreferences::Pointer prefs = this->GetPreferences().Cast<berry::IBerryPreferences>();
  if(prefs.IsNotNull())
  {
    prefs->OnChanged.RemoveListener(berry::MessageDelegate1<QmitkFunctionality
    , const berry::IBerryPreferences*>(this, &QmitkFunctionality::OnPreferencesChanged));
    // flush the preferences here (disabled, everyone should flush them by themselves at the right moment)
    // prefs->Flush();
  }

  // REMOVE SELECTION PROVIDER
  this->GetSite()->SetSelectionProvider(berry::ISelectionProvider::Pointer(NULL));

  berry::ISelectionService* s = GetSite()->GetWorkbenchWindow()->GetSelectionService();
  if(s)
  {
    s->RemovePostSelectionListener(m_BlueBerrySelectionListener.data());
  }

  this->ClosePart();
}

// --------------------------------------------------------------------------
