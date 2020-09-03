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

#ifndef SV4GUI_SEG2DEDIT_H
#define SV4GUI_SEG2DEDIT_H

#include "sv4gui_Path.h"
#include "sv4gui_SegmentationUtils.h"
#include "sv4gui_ContourGroup.h"
#include "sv4gui_ContourModel.h"
#include "sv4gui_ContourGroupDataInteractor.h"
#include "sv4gui_ContourGroupCreate.h"
#include "sv4gui_ContourModelThresholdInteractor.h"
#include "sv4gui_QmitkFunctionality.h"

#include "sv4gui_ResliceSlider.h"
#include "sv4gui_LevelSet2DWidget.h"
#include "sv4gui_LoftParamWidget.h"

#include "sv4gui_MachineLearningUtils.h"

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
#include <QItemSelection>

namespace Ui {
class sv4guiSeg2DEdit;
}

class sv4guiSeg2DEdit : public sv4guiQmitkFunctionality
{
    Q_OBJECT

public:

    enum SegmentationMethod {LEVELSET_METHOD, THRESHOLD_METHOD, REGION_GROWING_METHOD, ML_METHOD};

    static const QString EXTENSION_ID;

    sv4guiSeg2DEdit();

    virtual ~sv4guiSeg2DEdit();

public slots:

    void CreateLSContour();

    void CreateThresholdContour();

    void CreateCircle();

    void CreateEllipse();

    void CreateSplinePoly();

    void CreatePolygon();

    void UpdateContourList();

    void InsertContour(sv4guiContour* contour, int contourIndex);

    void InsertContourByPathPosPoint(sv4guiContour* contour);

    void SetContour(int contourIndex, sv4guiContour* newContour);

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

    sv4guiContour* PostprocessContour(sv4guiContour* contour);

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

    // ml stuff
    void selectAllPaths();
    void segmentPaths();
    //void sampleNetwork();
    void segTabSelected();
    void CreateMLContour();

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

    //ml additions
    void setupMLui();
    void initialize();
    void updatePaths();
    void createContourGroup(std::string path_name, std::string seg_name, std::vector<sv4guiContour*>& contours);
    std::vector<sv4guiContour*> segmentPath(sv4guiPath* path);
    sv4guiContour* doSegmentation(sv4guiPathElement::sv4guiPathPoint path_point, int index, int n);
    sv4guiContour* doMLContour(sv4guiPathElement::sv4guiPathPoint path_point);

protected:

    bool eventFilter(QObject *obj, QEvent *ev);

    QWidget* m_Parent;

    QWidget* m_CurrentParamWidget;

    QWidget* m_CurrentSegButton;

    sv4guiLevelSet2DWidget* m_LSParamWidget;

    sv4guiLoftParamWidget* m_LoftWidget;

    mitk::Image* m_Image;

    cvStrPts* m_cvImage;

    Ui::sv4guiSeg2DEdit *ui;

    sv4guiContourGroup* m_ContourGroup;

    sv4guiPath* m_Path;

    mitk::DataNode::Pointer m_PathNode;

    mitk::DataNode::Pointer m_ContourGroupNode;

    mitk::DataNode::Pointer m_GroupFolderNode;

    sv4guiContourGroupDataInteractor::Pointer m_DataInteractor;

    long m_ContourGroupChangeObserverTag;

    mitk::DataNode::Pointer m_LoftSurfaceNode;

    mitk::Surface::Pointer m_LoftSurface;

    bool groupCreated=false;

    sv4guiContourModelThresholdInteractor::Pointer m_PreviewDataNodeInteractor;

    mitk::DataNode::Pointer m_PreviewDataNode;

    long m_PreviewContourModelObserverFinishTag;
    long m_PreviewContourModelObserverUpdateTag;

    sv4guiContourModel::Pointer m_PreviewContourModel;

    long m_StartLoftContourGroupObserverTag;
    long m_StartLoftContourGroupObserverTag2;
    long m_StartChangingContourObserverTag;
    long m_EndChangingContourObserverTag;
    long m_SelectContourObserverTag;


    bool m_ContourChanging;

    QmitkStdMultiWidget* m_DisplayWidget;

    std::vector<sv4guiPathElement::sv4guiPathPoint> m_PathPoints;

    QMenu* m_ManualMenu;

    sv4guiContour* m_CopyContour;

    sv4guiContourGroupCreate* m_ContourGroupCreateWidget;

    bool m_UpdatingGUI;

    // ml additions
    std::string m_imageFilePath;
    sv4gui_MachineLearningUtils* ml_utils;

    int m_interval = 50;

    int m_numFourierModes = 7;

    mitk::DataNode::Pointer m_current_path_node;

    std::vector<std::string> m_selected_paths;

    sv4guiContourGroup::Pointer m_current_group;

    bool m_MachineLearninginitialized = false;

};

#endif // SV4GUI_SEG2DEDIT_H
