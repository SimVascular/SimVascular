#ifndef SVQMITKFUNCTIONALITY_H
#define SVQMITKFUNCTIONALITY_H

#include "org_sv_gui_qt_datamanager_Export.h"
#include <org_mitk_gui_qt_common_legacy_Export.h>
#include <org_mitk_gui_qt_common_Export.h>

#include <QmitkFunctionality.h>

#include <QWidget>
#include <QStandardItemModel>
#include <QItemDelegate>

class SV_QT_DATAMANAGER svQmitkFunctionality : public QmitkFunctionality
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svQmitkFunctionality();

    virtual ~svQmitkFunctionality();

public:

    //{
    // FUNCTIONS THAT NEED TO BE HIDDEN FROM QmitkFunctionality SO THAT
    // WE CAN USE org.sv.views.datamanager INSTEAD OF org.mitk.views.datamanager
    // HIDING Function from mitk data manager
    std::vector<mitk::DataNode*> GetDataManagerSelection() const;
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
    //}

private:

    //{
    /// PRIVATE OBJECTS FROM QmitkFunctionality
    /// object to observe BlueBerry selections
    QmitkFunctionalitySelectionProvider* m_SelectionProvider;
    QScopedPointer<berry::ISelectionListener> m_BlueBerrySelectionListener;
    //}


};

#endif // SVQMITKFUNCTIONALITY_H
