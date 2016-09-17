#ifndef SVRESLICESLIDER_H
#define SVRESLICESLIDER_H

#include <svQtWidgetsExports.h>

#include "svPathElement.h"

#include <mitkDataNode.h>
#include <mitkSlicedGeometry3D.h>
#include <mitkExtractSliceFilter.h>

#include <QWidget>
#include <QCheckBox>

#include <QmitkStdMultiWidget.h>
#include <QmitkRenderWindow.h>
#include <QmitkStepperAdapter.h>


class SVQTWIDGETS_EXPORT svResliceSlider : public QWidget
{
    Q_OBJECT
public:
//    svResliceSlider(QmitkStdMultiWidget* widget,QWidget *parent = 0);
    svResliceSlider(QWidget *parent = 0);

    virtual ~svResliceSlider();

public slots:

    bool isResliceOn();

    bool isStepperSynchronized();

    void SetDisplayWidget(QmitkStdMultiWidget* widget);

    void setImageNode(mitk::DataNode::Pointer imageNode);

    void setPathPoints(std::vector<svPathElement::svPathPoint> pathPoints);

    svPathElement::svPathPoint getPathPoint(int index);

    svPathElement::svPathPoint getCurrentPathPoint();

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

protected:

    bool stepperSynchronized;

    std::vector<svPathElement::svPathPoint> m_PathPoints;

    mitk::DataNode::Pointer currentImageNode;

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

#endif // SVRESLICESLIDER_H
