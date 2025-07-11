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

#ifndef SV4GUI_QMITKFUNCTIONALITY_H
#define SV4GUI_QMITKFUNCTIONALITY_H

#include "org_sv_gui_qt_datamanager_Export.h"
// #include <org_mitk_gui_qt_common_legacy_Export.h>
#include <org_mitk_gui_qt_common_Export.h>

#include <QmitkAbstractView.h>
#include <mitkILifecycleAwarePart.h>

#include <QWidget>
#include <QStandardItemModel>
#include <QItemDelegate>

// [DaveP] This class previously inherited from QmitkFunctionality. 
// We transition to QmitkAbstractView following 
// https://www.mitk.org/wiki/Views_Without_Multi_Widget.

class SV_QT_DATAMANAGER sv4guiQmitkFunctionality : public QmitkAbstractView, public mitk::ILifecycleAwarePart
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    sv4guiQmitkFunctionality();

    virtual ~sv4guiQmitkFunctionality();

public:

    // Some function need to be hidden so we can use org.sv.views.datamanager 
    // instead of org.mitk.views.datamanager.
    QList<mitk::DataNode::Pointer> GetDataManagerSelection() const;

    /// Called immediately after CreateQtPartControl().
    /// Here standard event listeners for a QmitkFunctionality are registered
    void AfterCreateQtPartControl();

    /// reactions to selection events from data manager (and potential other senders)
    void BlueBerrySelectionChanged(const berry::IWorkbenchPart::Pointer& sourcepart, const berry::ISelection::ConstPointer& selection);

    /// Called, when the WorkbenchPart gets closed for removing event listeners
    /// Internally this method calls ClosePart after it removed the listeners registered
    /// by QmitkFunctionality. By having this proxy method the user does not have to
    /// call QmitkFunctionality::ClosePart() when overwriting ClosePart()
    void ClosePartProxy();

    /// Creates a scroll area for this view and calls CreateQtPartControl then
    void CreatePartControl(QWidget* parent) override;

     void SetFocus() {
        // Fore more info, see https://www.mitk.org/wiki/Views_Without_Multi_Widget
    }

    // implementation copied from QmitkAbstractViewPrivate
    QList<mitk::DataNode::Pointer> DataNodeSelectionToQList(mitk::DataNodeSelection::ConstPointer currentSelection) const;

protected:

    // [DaveP] This was originally included in QmitkFunctionality. 
    // Adding it now for compatibility during transition to QmitkAbstractView
    QWidget* m_Parent;

    bool m_isVisible;

private:

    // [DaveP] We need to reimplement these functions from QmitkAbstractView 
    // because they became private

    void NodeAdded( const mitk::DataNode*  /*node*/ ) override;

    void NodeRemoved( const mitk::DataNode*  /*node*/ ) override;

    void NodeChanged( const mitk::DataNode* /*node*/ ) override;

    void OnNullSelection(berry::IWorkbenchPart::Pointer /*part*/) override;

    // [DaveP] 
    void OnPreferencesChanged(const mitk::IPreferences* /*preferences*/) override;
    // void OnPreferencesChanged(const berry::IBerryPreferences* /*preferences*/) override;

    void OnSelectionChanged(berry::IWorkbenchPart::Pointer /*part*/,
                            const QList<mitk::DataNode::Pointer>& /*nodes*/) override;

    QmitkAbstractViewSelectionProvider* m_SelectionProvider;

    QScopedPointer<berry::ISelectionListener> m_BlueBerrySelectionListener;

};

#endif // SV4GUI_QMITKFUNCTIONALITY_H
