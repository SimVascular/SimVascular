#ifndef SVPATHEDIT_H
#define SVPATHEDIT_H

#include "svPathSmooth.h"
#include "svPathCreate.h"
#include "svPath.h"
#include "svPathDataInteractor.h"
#include "svPathCreate.h"

#include <QmitkFunctionality.h>
#include <QmitkStdMultiWidget.h>

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkDataInteractor.h>
#include <mitkImage.h>

namespace Ui {
  class svPathEdit;
}

class svPathEdit : public QmitkFunctionality
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svPathEdit();

    virtual ~svPathEdit();

public slots:

    void ChangePath();

    void AddPoint(mitk::Point3D);

    void SmoothCurrentPath();

    void ClearAll();

    void SelectPoint();

    void SelectPoint(const QModelIndex & idx);

    void DeleteSelected();

    void SmartAdd();

    void ManuallyAdd();

    void UpdateGUI();

    void SetupResliceSlider();

    void UpdateSlice();

    void UpdatePathResliceSize(double newSize);

    void UpdateAddingMode(int mode);

    void NewPath();

public:

    void SelectPoint(int index);

    int GetTimeStep();

    double GetVolumeImageSpacing();

    void GetImageRealBounds(double realBounds[6]);

    virtual void CreateQtPartControl(QWidget *parent) override;

    //    virtual void OnSelectionChanged(const QList<mitk::DataNode::Pointer>& nodes ) override;
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

    bool eventFilter(QObject *obj, QEvent *ev);

    long m_PathChangeObserverTag;

    long m_PointMoveObserverTag;

    mitk::DataNode::Pointer m_PathNode;

    mitk::DataNode::Pointer m_PathFolderNode;

    svPath* m_Path;

    Ui::svPathEdit *ui;

    QWidget* m_Parent;

    //    QmitkPointListView* m_PointListView;

    svPathDataInteractor::Pointer m_DataInteractor;

    svPathSmooth* m_SmoothWidget;

    svPathCreate* m_PathCreateWidget;
    svPathCreate* m_PathCreateWidget2;

    mitk::Image* m_Image;

    mitk::DataNode::Pointer m_ImageNode;

    QmitkStdMultiWidget* m_DisplayWidget;

    bool m_UpdatingGUI;

private:

    //{
    /// PRIVATE OBJECTS FROM QmitkFunctionality
    /// object to observe BlueBerry selections
    QmitkFunctionalitySelectionProvider* m_SelectionProvider;
    QScopedPointer<berry::ISelectionListener> m_BlueBerrySelectionListener;
    //}

};

#endif // SVPATHEDIT_H

