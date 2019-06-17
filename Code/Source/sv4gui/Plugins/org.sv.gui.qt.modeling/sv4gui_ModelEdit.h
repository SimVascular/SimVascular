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

#ifndef SV4GUI_MODELEDIT_H
#define SV4GUI_MODELEDIT_H

#include "sv4gui_Model.h"
#include "sv4gui_SegSelectionWidget.h"
#include "sv4gui_CapSelectionWidget.h"
#include "sv4gui_ModelDataInteractor.h"
#include "sv4gui_ModelElementPolyData.h"
#include "sv4gui_QmitkFunctionality.h"

#include <vtkSphereWidget.h>
#include <vtkPlaneWidget.h>
#include <vtkBoxWidget.h>

#include <QWidget>
#include <QItemSelection>

namespace Ui {
class sv4guiModelEdit;
}

class sv4guiModelEdit : public sv4guiQmitkFunctionality
{
    Q_OBJECT

public:

    enum OperationType {DELETE_FACES, FILL_HOLES_WITH_IDS, COMBINE_FACES, REMESH_FACES, EXTRACT_FACES
                        , FILL_HOLES, SELECT_LARGEST_CONNECTED, REMESH_GLOBAL, DECIMATE_GLOBAL, LAPLACIAN_SMOOTH_GLOBAL, BUTTERFLY_SUBDIVIDE_GLOBAL, WINDOWSINC_SMOOTH_GLOBAL, DENSIFY_GLOBAL
                        , DECIMATE_LOCAL, LAPLACIAN_SMOOTH_LOCAL, CONSTRAIN_SMOOTH_LOCAL, LINEAR_SUBDIVIDE_LOCAL, LOOP_SUBDIVIDE_LOCAL
                        , CUT_ABOVE, CUT_BELOW, CUT_BOX};

    static const QString EXTENSION_ID;

    sv4guiModelEdit();

    virtual ~sv4guiModelEdit();

public slots:

    void CreateModel();

    void ClearAll();

    void AddObservers();

    void RemoveObservers();

    void ShowSegSelectionWidget();

    void ShowCapSelectionWidget();

    void UpdateGUI();

    void BlendModel();

    void ExtractCenterlines();

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

    std::vector<sv4guiModelElement::svBlendParamRadius*> GetBlendRadii();

    std::vector<int> GetSelectedFaceIDs();

    bool MarkCells(sv4guiModelElementPolyData* modelElement);

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

    Ui::sv4guiModelEdit *ui;

    sv4guiModel* m_Model;

    std::string m_ModelType;

    mitk::DataNode::Pointer m_ModelNode;

    sv4guiSegSelectionWidget* m_SegSelectionWidget;

    sv4guiCapSelectionWidget* m_CapSelectionWidget;

    sv4guiModelDataInteractor::Pointer m_DataInteractor;

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
    void SetTimeModified();



};

#endif // SV4GUI_MODELEDIT_H
