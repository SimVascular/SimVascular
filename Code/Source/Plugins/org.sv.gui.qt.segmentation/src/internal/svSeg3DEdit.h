#ifndef svSeg3DUtils_H
#define svSeg3DUtils_H

#include "svMitkSeg3D.h"
#include "svMitkSeg3DDataInteractor.h"

#include <QmitkFunctionality.h>

#include <vtkImageData.h>

#include <ctkRangeWidget.h>

namespace Ui {
class svSeg3DEdit;
}

class svSeg3DEdit : public QmitkFunctionality
{
    Q_OBJECT

public:

    enum SegmentationMethod {LEVELSET_METHOD, THRESHOLD_METHOD, REGION_GROWING_METHOD};

    static const QString EXTENSION_ID;

    svSeg3DEdit();

    virtual ~svSeg3DEdit();

public slots:

    void CreateByCollidingFronts();

    void SetSeedVisibility( bool checked = false );

    void ClearAll();

public:

//    int GetTimeStep();

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void OnSelectionChanged(std::vector<mitk::DataNode*> nodes) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

//    virtual void Activated() override;

//    virtual void Deactivated() override;

    virtual void Visible() override;

    virtual void Hidden() override;

//    bool IsExclusiveFunctionality() const override;

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

protected:

    QWidget* m_Parent;

    Ui::svSeg3DEdit *ui;

    vtkImageData* m_VtkImage;

    svMitkSeg3D* m_MitkSeg3D;

    mitk::DataNode::Pointer m_MitkSeg3DNode;

    svMitkSeg3DDataInteractor::Pointer m_DataInteractor;

    QmitkStdMultiWidget* m_DisplayWidget;

private:

    //{
    /// PRIVATE OBJECTS FROM QmitkFunctionality
    /// object to observe BlueBerry selections
    QmitkFunctionalitySelectionProvider* m_SelectionProvider;
    QScopedPointer<berry::ISelectionListener> m_BlueBerrySelectionListener;
    //}

};

#endif // svSeg3DUtils_H
