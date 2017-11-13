#ifndef SVMESHEDIT_H
#define SVMESHEDIT_H

#include "svMitkMesh.h"
#include "svModel.h"

#include "svModelDataInteractor.h"
#include "svDataNodeOperationInterface.h"
#include "svLocalTableDelegate.h"
#include "svQmitkFunctionality.h"

#include <vtkSphereWidget.h>

#include <QWidget>
#include <QStandardItemModel>
#include <QItemDelegate>

namespace Ui {
class svMeshEdit;
}

class svMeshEdit : public svQmitkFunctionality
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

    void TableDomainsListSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void DeleteSelectedDomains( bool checked = false );

    void SetSubDomainSize( bool checked = false );

    void SetRegion( bool checked = false );

    void DeleteSelectedRegions( bool checked = false );

    void TableViewRegionContextMenuRequested( const QPoint & pos );

    void TableViewDomainsContextMenuRequested( const QPoint & pos );

    void UpdateFaceListSelection();

    void UpdateTetGenGUI();

    void UpdateMeshSimGUI();

    void UpdateAdaptGUI(int selected);

    void AddSphere();

    void AddHole();

    void AddSubDomain();

    void ShowSphereInteractor(bool checked);

    void DisplayMeshInfo();

    void SetResultFile();

    void Adapt();

    void ShowModel(bool checked = false);

public:

    int GetTimeStep();

    void SetupGUI(QWidget *parent );

    void RunCommands(bool fromGUI = true);

    double EstimateEdgeSize();

    std::vector<std::string> CreateCmdsT();

    std::vector<std::string> CreateCmdsM();

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

    bool IsDouble(QString value);

    bool IsInt(QString value);

    QString GetMeshFolderPath();

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

    QMenu* m_TableMenuLocal;
    QStandardItemModel* m_TableModelLocal;

    QMenu* m_TableMenuRegion;
    QStandardItemModel* m_TableModelRegion;

    QMenu* m_TableMenuDomains;
    QStandardItemModel* m_TableModelDomains;

    int m_SelectedRegionIndex;
    int m_SelectedDomainsIndex;

    vtkSmartPointer<vtkSphereWidget> m_SphereWidget;

    bool m_UndoAble;

    svDataNodeOperationInterface* m_Interface;

    svLocalTableDelegate* m_CustomDelegate;

    QItemDelegate* m_DefaultDelegate;

};

#endif // SVMESHEDIT_H
