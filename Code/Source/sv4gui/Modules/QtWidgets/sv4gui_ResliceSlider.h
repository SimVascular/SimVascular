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

#ifndef SV4GUI_RESLICESLIDER_H
#define SV4GUI_RESLICESLIDER_H

#include <sv4guiModuleQtWidgetsExports.h>

#include "sv4gui_PathElement.h"

#include <mitkDataNode.h>
#include <mitkSlicedGeometry3D.h>
#include <mitkExtractSliceFilter.h>

#include <QWidget>
#include <QCheckBox>

#include <QmitkStdMultiWidget.h>
#include <QmitkRenderWindow.h>
#include <QmitkStepperAdapter.h>


class SV4GUIMODULEQTWIDGETS_EXPORT sv4guiResliceSlider : public QWidget
{
    Q_OBJECT
public:
//    sv4guiResliceSlider(QmitkStdMultiWidget* widget,QWidget *parent = 0);
    sv4guiResliceSlider(QWidget *parent = 0);

    virtual ~sv4guiResliceSlider();

    QmitkStepperAdapter* GetIntensityStepper() {return intensityStepper;}

public slots:

    bool isResliceOn();

    bool isStepperSynchronized();

    void SetDisplayWidget(QmitkStdMultiWidget* widget);

    void setDataNode(mitk::DataNode::Pointer imageNode);

    void setPathPoints(std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints);

    sv4guiPathElement::sv4guiPathPoint getPathPoint(int index);

    sv4guiPathElement::sv4guiPathPoint getCurrentPathPoint();

    void setResliceSize( double resliceSize);

    double getResliceSize();

    void updateReslice();

    int getCurrentSliceIndex();

    void intensityOnRefetch();

    void potentialOnRefetch();

    void restoreDisplayWidget();

    void changeDisplayWidget(bool checked);

    void turnOnReslice(bool on);

    void setCheckBoxVisible(bool visible);

    void setSlicePos(int pos);

    int GetSliceNumber();

    void moveToPathPosPoint(mitk::Point3D posPoint);

    void moveToClosestPathPosPoint(mitk::Point3D posPoint);

    void updateResliceSize();

    void SetUseGeometrySize(bool use);

    void SetUseGeometrySpacing(bool use);

    void SetUseMinimumSpacing(bool use);

    void SetResliceMode(mitk::ExtractSliceFilter::ResliceInterpolation mode);

    void SetStartingSlicePos(int pos);

signals:
    void resliceSizeChanged(double newSize);

    void reslicePositionChanged(int slice);

protected:

    bool stepperSynchronized;

    std::vector<sv4guiPathElement::sv4guiPathPoint> m_PathPoints;

    mitk::DataNode::Pointer currentDataNode;

    double resliceSize;

    QmitkStdMultiWidget* displayWidget=NULL;

    mitk::SlicedGeometry3D::Pointer currentSlicedGeometry;

    QCheckBox* resliceCheckBox;
    QmitkRenderWindow* intensityWindow;
    QmitkRenderWindow* potentialWindow;
    QmitkRenderWindow* coronalWindow;
    QWidget* sliderContainer;
//    QmitkSliderNavigatorWidget* intensitySlider;
//    QmitkSliderNavigatorWidget* potentialSlider;
    QmitkStepperAdapter* intensityStepper;
    QmitkStepperAdapter* potentialStepper;

    QPushButton* btnResliceSize;

    bool m_UseGeometrySize;

    bool m_UseGeometrySpacing;

    bool m_UseMinimumSpacing;

    mitk::ExtractSliceFilter::ResliceInterpolation m_ResliceMode;

    int m_StartingSlicePos;

};

#endif // SV4GUI_RESLICESLIDER_H
