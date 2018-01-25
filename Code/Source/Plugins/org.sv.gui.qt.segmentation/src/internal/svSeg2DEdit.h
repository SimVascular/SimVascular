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

#ifndef SVSEG2DEDIT_H
#define SVSEG2DEDIT_H

#include "svPath.h"
#include "svSegmentationUtils.h"
#include "svContourGroup.h"
#include "svContourModel.h"
#include "svContourGroupDataInteractor.h"
#include "svContourGroupCreate.h"
#include "svContourModelThresholdInteractor.h"
#include "svQmitkFunctionality.h"

#include "svResliceSlider.h"
#include "svLevelSet2DWidget.h"
#include "svLoftParamWidget.h"

#include <QmitkSliceWidget.h>
#include <QmitkSliderNavigatorWidget.h>
#include <QmitkStepperAdapter.h>

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkSurface.h>
#include <mitkPointSet.h>
#include <mitkDataInteractor.h>
#include <mitkImage.h>

#include <vtkPoints.h>
#include <vtkSmartPointer.h>
#include <vtkActor.h>
#include <vtkTransform.h>

#include <ctkSliderWidget.h>

#include <QSlider>
#include <QPushButton>
#include <QWidget>

namespace Ui {
class svSeg2DEdit;
}

class svSeg2DEdit : public svQmitkFunctionality
{
    Q_OBJECT

public:

    enum SegmentationMethod {LEVELSET_METHOD, THRESHOLD_METHOD, REGION_GROWING_METHOD};

    static const QString EXTENSION_ID;

    svSeg2DEdit();

    virtual ~svSeg2DEdit();

public slots:

    void CreateLSContour();

    void CreateThresholdContour();

    void CreateCircle();

    void CreateEllipse();

    void CreateSplinePoly();

    void CreatePolygon();

    void UpdateContourList();

    void InsertContour(svContour* contour, int contourIndex);

    void InsertContourByPathPosPoint(svContour* contour);

    void SetContour(int contourIndex, svContour* newContour);

    void RemoveContour(int contourIndex);

    void DeleteSelected();

    void SelectContour();

    void SelectContour(const QModelIndex & idx);

    void ClearAll();

    void UpdatePreview();

    void FinishPreview();

    void SmoothSelected();

    void CreateContours(SegmentationMethod method);

    void SetSecondaryWidgetsVisible(bool visible);

    std::vector<int> GetBatchList();

    svContour* PostprocessContour(svContour* contour);

    double GetVolumeImageSpacing();

    void LoftContourGroup();

    void ShowLoftWidget();

    void UpdateContourGroupLoftingParam();

    void OKLofting();

    void ApplyLofting();

    void HideLoftWidget();

    void ContourChangingOn();

    void ContourChangingOff();

    void SelectContour3D();

    void ResetGUI();

    void UpdatePathResliceSize(double newSize);

    void ManualContextMenuRequested();

    void ManualCircleContextMenuRequested(const QPoint &pos);
    void ManualEllipseContextMenuRequested(const QPoint &pos);
    void ManualSplinePolyContextMenuRequested(const QPoint &pos);
    void ManualPolygonContextMenuRequested(const QPoint &pos);

    void CreateManualCircle( bool checked = false );

    void CreateManualEllipse( bool checked = false );

    void CreateManualSplinePoly( bool checked = false );

    void CreateManualPolygon( bool checked = false );

    void CreateManualPolygonType(bool spline);

    void CopyContour();

    void PasteContour();

    void NewGroup();

    void ShowPath(bool checked = false);

    void UpdatePathPoint(int pos);

public:

    void SelectContour(int index);

    int GetTimeStep();

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

    void PreparePreviewInteraction(QString method);

    void QuitPreviewInteraction();

protected:

    bool eventFilter(QObject *obj, QEvent *ev);

    QWidget* m_Parent;

    QWidget* m_CurrentParamWidget;

    QWidget* m_CurrentSegButton;

    svLevelSet2DWidget* m_LSParamWidget;

    svLoftParamWidget* m_LoftWidget;

    mitk::Image* m_Image;

    cvStrPts* m_cvImage;

    Ui::svSeg2DEdit *ui;

    svContourGroup* m_ContourGroup;

    svPath* m_Path;

    mitk::DataNode::Pointer m_PathNode;

    mitk::DataNode::Pointer m_ContourGroupNode;

    mitk::DataNode::Pointer m_GroupFolderNode;

    svContourGroupDataInteractor::Pointer m_DataInteractor;

    long m_ContourGroupChangeObserverTag;

    mitk::DataNode::Pointer m_LoftSurfaceNode;

    mitk::Surface::Pointer m_LoftSurface;

    bool groupCreated=false;

    svContourModelThresholdInteractor::Pointer m_PreviewDataNodeInteractor;

    mitk::DataNode::Pointer m_PreviewDataNode;

    long m_PreviewContourModelObserverFinishTag;
    long m_PreviewContourModelObserverUpdateTag;

    svContourModel::Pointer m_PreviewContourModel;

    long m_StartLoftContourGroupObserverTag;
    long m_StartLoftContourGroupObserverTag2;
    long m_StartChangingContourObserverTag;
    long m_EndChangingContourObserverTag;
    long m_SelectContourObserverTag;


    bool m_ContourChanging;

    QmitkStdMultiWidget* m_DisplayWidget;

    std::vector<svPathElement::svPathPoint> m_PathPoints;

    QMenu* m_ManualMenu;

    svContour* m_CopyContour;

    svContourGroupCreate* m_ContourGroupCreateWidget;

    bool m_UpdatingGUI;

};

#endif // SVSEG2DEDIT_H
