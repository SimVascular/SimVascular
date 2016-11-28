#ifndef SVMESHEDIT_H
#define SVMESHEDIT_H

#include "svMitkMesh.h"
#include "svModel.h"

#include "svModelDataInteractor.h"

#include <QmitkFunctionality.h>

#include <vtkSphereWidget.h>

#include <QWidget>
#include <QStandardItemModel>

namespace Ui {
class svMeshEdit;
}

class svMeshEdit : public QmitkFunctionality
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svMeshEdit();

    virtual ~svMeshEdit();

public slots:

    void RunMesher();

    void RunHistory();

    void ClearAll();

    void AddObservers();

    void RemoveObservers();

    void UpdateGUI();

    void SetEstimatedEdgeSize();

    void TableFaceListSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void SetLocal( bool checked = false );

    void ClearLocal( bool checked = false );

    void TableViewLocalContextMenuRequested( const QPoint & pos );

    void TableRegionListSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void SetRegion( bool checked = false );

    void DeleteSelectedRegions( bool checked = false );

    void TableViewRegionContextMenuRequested( const QPoint & pos );

    void UpdateFaceListSelection();

    void UpdateTetGenGUI();

    void AddSphere();

    void ShowSphereInteractor(bool checked);

public:

    int GetTimeStep();

    void SetupTetGenGUI(QWidget *parent );

    void RunCommands(bool fromGUI = true);

    double EstimateEdgeSize();

    std::vector<std::string> CreateCmdsT();

//    static void UpdateSphereData( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* vtkNotUsed(clientData), void* vtkNotUsed(callData) );

    void UpdateSphereData();

//    std::vector<int> GetSelectedFaceIDs();

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void OnSelectionChanged(std::vector<mitk::DataNode*> nodes) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

//    virtual void Activated() override;

//    virtual void Deactivated() override;

    virtual void Visible() override;

    virtual void Hidden() override;

protected:

    QWidget* m_Parent;

    Ui::svMeshEdit *ui;

    svMitkMesh* m_MitkMesh;

    svModel* m_Model;

    std::string m_MeshType;

    mitk::DataNode::Pointer m_MeshNode;

    mitk::DataNode::Pointer m_ModelNode;

    svModelDataInteractor::Pointer m_DataInteractor;

    long m_ModelSelectFaceObserverTag;
//    long m_SphereObserverTag;

    QmitkStdMultiWidget* m_DisplayWidget;

    QMenu* m_TableMenuLocalT;
    QStandardItemModel* m_TableModelLocalT;

    QMenu* m_TableMenuRegionT;
    QStandardItemModel* m_TableModelRegionT;

    int m_SelectedRegionIndex;

    vtkSmartPointer<vtkSphereWidget> m_SphereWidget;

    bool m_UndoAble;

};

#endif // SVMESHEDIT_H
