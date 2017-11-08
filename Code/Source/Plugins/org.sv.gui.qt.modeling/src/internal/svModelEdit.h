#ifndef SVMODELEDIT_H
#define SVMODELEDIT_H

#include "svModel.h"
#include "svSegSelectionWidget.h"
#include "svModelDataInteractor.h"
#include "svModelElementPolyData.h"

#include <QmitkFunctionality.h>

#include <vtkSphereWidget.h>
#include <vtkPlaneWidget.h>
#include <vtkBoxWidget.h>

#include <QWidget>

namespace Ui {
class svModelEdit;
}

class svModelEdit : public QmitkFunctionality
{
    Q_OBJECT

public:

    enum OperationType {DELETE_FACES, FILL_HOLES_WITH_IDS, COMBINE_FACES, REMESH_FACES, EXTRACT_FACES
                        , FILL_HOLES, SELECT_LARGEST_CONNECTED, REMESH_GLOBAL, DECIMATE_GLOBAL, LAPLACIAN_SMOOTH_GLOBAL, BUTTERFLY_SUBDIVIDE_GLOBAL, WINDOWSINC_SMOOTH_GLOBAL, DENSIFY_GLOBAL
                        , DECIMATE_LOCAL, LAPLACIAN_SMOOTH_LOCAL, CONSTRAIN_SMOOTH_LOCAL, LINEAR_SUBDIVIDE_LOCAL, LOOP_SUBDIVIDE_LOCAL
                        , CUT_ABOVE, CUT_BELOW, CUT_BOX};

    static const QString EXTENSION_ID;

    svModelEdit();

    virtual ~svModelEdit();

public slots:

    void CreateModel();

    void ClearAll();

    void AddObservers();

    void RemoveObservers();

    void ShowSegSelectionWidget();

    void UpdateGUI();

    void BlendModel();

    void UpdateFaceListSelection();

    void UpdateBlendTable(int index);

    void TableViewBlendContextMenuRequested( const QPoint & index );

    void SetRadius( bool checked = false );

    void ClearRadius( bool checked = false );

    void UseSelectedBlend( bool checked = false );

    void NotUseSelectedBlend( bool checked = false );

    void SetupBlendTable();

    void UpdatePolyDataBlendParam();

    void TableBlendSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void SetupFaceListTable();

    void UpdateFaceData(QStandardItem* item);

    void TableFaceListSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void ToggleVisibility(const QModelIndex &index);

    void ChangeColor(const QModelIndex &index);

    void TableViewFaceListContextMenuRequested( const QPoint & index );

    void ShowSelected( bool checked = false );

    void HideSelected( bool checked = false );

    void ChangeOpacitySelected( bool checked = false );

    void ChangeColorSelected( bool checked = false );

    void ChangeTypeSelected( bool checked = false );

    void ModelOperate(int operationType);

    void ShowSphereInteractor(bool checked);

    void ShowPlaneInteractor(bool checked);

    void ShowBoxInteractor(bool checked);

    void UpdatePathListForTrim();

    void SetupSliderPathPlane(int idx);

    void UpdatePlaneWidget(double idx);

    void SetupSliderPathBox(int idx);

    void UpdateBoxWidget(double idx);

    void ChangeFacetSize();

    void ConvertToPolyDataModel();

    void SetEstimatedEdgeSize();

    double EstimateEdgeSize();

public:

    int GetTimeStep();

    std::vector<svModelElement::svBlendParamRadius*> GetBlendRadii();

    std::vector<int> GetSelectedFaceIDs();

    bool MarkCells(svModelElementPolyData* modelElement);

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void OnSelectionChanged(std::vector<mitk::DataNode*> nodes) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

//    virtual void Activated() override;

//    virtual void Deactivated() override;

    virtual void Visible() override;

    virtual void Hidden() override;

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

    Ui::svModelEdit *ui;

    svModel* m_Model;

    std::string m_ModelType;

    mitk::DataNode::Pointer m_ModelNode;

    svSegSelectionWidget* m_SegSelectionWidget;

    svModelDataInteractor::Pointer m_DataInteractor;

    long m_ModelSelectFaceObserverTag;
    long m_ModelUpdateObserverTag;

    QmitkStdMultiWidget* m_DisplayWidget;

    QMenu* m_BlendTableMenu;
    QStandardItemModel* m_BlendTableModel;

    QMenu* m_FaceListTableMenu;
    QStandardItemModel* m_FaceListTableModel;

    vtkSmartPointer<vtkSphereWidget> m_SphereWidget;
    vtkSmartPointer<vtkPlaneWidget> m_PlaneWidget;
    vtkSmartPointer<vtkBoxWidget> m_BoxWidget;

    mitk::DataNode::Pointer m_PathFolderNode;

    bool m_OperatingWholeTableModel;

    bool m_LocalOperationforBlendRegion;

private:

    //{
    /// PRIVATE OBJECTS FROM QmitkFunctionality
    /// object to observe BlueBerry selections
    QmitkFunctionalitySelectionProvider* m_SelectionProvider;
    QScopedPointer<berry::ISelectionListener> m_BlueBerrySelectionListener;
    //}

};

#endif // SVMODELEDIT_H
