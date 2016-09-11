#ifndef SVPATHEDIT_H
#define SVPATHEDIT_H

#include "svAbstractView.h"
#include "ui_svPathEdit.h"

#include "svPath.h"

#include <QmitkPointListView.h>

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkSurface.h>
#include <mitkPointSet.h>
#include <mitkDataInteractor.h>
#include <mitkImage.h>

#include <vtkPoints.h>
#include <vtkSmartPointer.h>
#include <vtkActor.h>

class svPathEdit : public svAbstractView
{  
  Q_OBJECT
  
  public:  

    static const QString EXTENSION_ID;

    svPathEdit();

    virtual ~svPathEdit();

  public slots:

    void ChangePath();

    void AddToEnd();

    void AddToTop();

    void AddPoint(int index, mitk::Point3D, int timeStep);

    void SmoothCurrentPath();

    void ClearAll();

    void OpenPathCreateDialog();

    void ShowPathEditPane();

    void ShowPathEditPaneForPath();

    bool IsPath(QList<mitk::DataNode::Pointer> nodes);

    void SelectItem(const QModelIndex & idx);

    void InsertPointAbove();

    void DeleteSelected();

    void SmartAdd();

    void UpdateGUI();

    void SetupResliceSlider();

    void UpdateSlice();

    double GetVolumeImageSpacing();

    void GetImageRealBounds(double realBounds[6]);

  protected:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void OnSelectionChanged(const QList<mitk::DataNode::Pointer>& nodes ) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

    virtual void Activated() override;

    virtual void Deactivated() override;

    long m_PathChangeObserverTag;

    long m_PointMoveObserverTag;

    mitk::DataNode::Pointer m_PathNode;

    svPath* m_Path;

    bool m_ParentNodeOriginalVisible;

    Ui::svPathEdit *ui;

    QWidget* m_Parent;

    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > mDescriptorActionList;

    QmitkPointListView* m_PointListView;

    mitk::DataInteractor::Pointer m_DataInteractor;

    svAbstractExtension* m_SmoothWidget;

    mitk::Image* m_Image;


};

#endif // SVPATHEDIT_H

