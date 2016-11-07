#ifndef SVCONTOUR_H
#define SVCONTOUR_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svPathElement.h"

#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkImageData.h"

#include "mitkBaseData.h"
#include "mitkPoint.h"
#include "mitkPlaneGeometry.h"

// somehow GetClassName is getting set to GetClassNameA on Windows
#ifdef GetClassName
#undef GetClassName
#endif

class SVSEGMENTATION_EXPORT svContour
{

public:

    enum SubdivisionType {CONSTANT_TOTAL_NUMBER, CONSTANT_SUBDIVISION_NUMBER,CONSTANT_SPACING};

    static const int INVALID_INDEX=-2;

    enum ShapeType {ONLY_CONTOUR, CIRCLE, ELLIPSE,POLYGON,CURVE_POLYGON};

    svContour();

    svContour(const svContour &other);

    virtual ~svContour();

    virtual svContour* Clone();

    virtual std::string GetClassName();

    std::string GetType();

    void SetType(std::string type);

    std::string GetMethod();

    void SetMethod(std::string method);

    svPathElement::svPathPoint GetPathPoint();

    void SetPathPoint(svPathElement::svPathPoint pathPoint);

    int GetPathPosID();

    mitk::Point3D GetPathPosPoint();

    int GetContourID();

    void SetContourID(int contourID);

    bool IsSelected();

    void SetSelected(bool selected=true);

    bool IsHovering();

    void SetHovering(bool hovering=true);

    bool IsPlaced();

    void SetPlaced(bool placed=true);

    bool IsExtendable();

    void SetExtendable(bool extendable=true);

    bool IsInitiallyPlaced(){return m_InitiallyPlaced;}

    void SetInitiallyPlaced(bool placed){m_InitiallyPlaced=placed;}

    bool IsClosed();

    void SetClosed(bool closed=true);

    bool IsFinished();

    void SetFinished(bool finished=true);

    ShapeType GetShape();

    int GetSubdivisionNumber();

    void SetSubdivisionNumber(int number);

    SubdivisionType GetSubdivisionType();

    void SetSubdivisionType(SubdivisionType subdivType);

    double GetSubdivisionSpacing();

    void SetSubdivisionSpacing(double spacing);

    void SetPlaneGeometry(mitk::PlaneGeometry* planeGeometry);

    mitk::PlaneGeometry* GetPlaneGeometry();

    int GetControlPointNumber();

    int GetMinControlPointNumber();
    int GetMaxControlPointNumber();

    void SetMinControlPointNumber(int number);
    void SetMaxControlPointNumber(int number);

    mitk::Point3D GetControlPoint(int index);

    void InsertControlPoint(int index, mitk::Point3D point);

    void RemoveControlPoint(int index);

    virtual void SetControlPoint(int index, mitk::Point3D point);

//    virtual void SetActualControlPoint(int index, mitk::Point3D point);

    void SetControlPointSelectedIndex(int index);

    void DeselectControlPoint();

    int GetControlPointSelectedIndex();

    void ClearControlPoints();

    void PlaceContour(mitk::Point3D point);

    virtual void PlaceControlPoints(mitk::Point3D point);

    void SetControlPoints(std::vector<mitk::Point3D> controlPoints, bool updateContour = true);

    bool IsControlPointRemovable(int index);

    void SetPreviewControlPoint(mitk::Point3D point );

    void HidePreviewControlPoint();

    bool IsPreviewControlPointVisible();

    mitk::Point3D GetPreviewControlPoint();

    //for contour points
    //=================================

    virtual void CreateContourPoints(){}
    void CreateContour();

    void SetContourPoints(std::vector<mitk::Point3D> contourPoints, bool update = true);

    void ControlPointsChanged();

    int GetContourPointNumber();

    mitk::Point3D GetContourPoint(int index);

    void ClearContourPoints();

    vtkSmartPointer<vtkPolyData> CreateVtkPolyDataFromContour(bool includingAllLines = true);

    void ContourPointsChanged();

    void CreateCenterScalingPoints();
    virtual void AssignCenterScalingPoints();

    mitk::Point3D GetCenterPoint();

    virtual svContour* CreateSmoothedContour(int fourierNumber = 12 );

    //for all data
    //===================================

    virtual int SearchControlPointByContourPoint( int contourPointIndex );

    void Shift(mitk::Vector3D dirVec);

    void Scale(double factor, mitk::Point3D referencePoint);

    void Scale(double factor);

    void Scale(mitk::Point3D referencePoint, mitk::Point3D oldPoint, mitk::Point3D newPoint);

    void CalculateBoundingBox(double *bounds);

    bool IsOnPlane(const mitk::PlaneGeometry* planeGeometry, double precisionFactor=0.1);

    vtkImageData* GetVtkImageSlice();

    void SetVtkImageSlice(vtkImageData* slice);

  protected:

    bool m_Selected;

    bool m_Hovering;

    bool m_Placed;

    int m_ContourID;

    svPathElement::svPathPoint m_PathPoint;

    bool m_Extendable;

    ShapeType m_Shape;

    std::string m_Method;

    std::string m_Type;

    bool m_Closed;

    bool m_Finished;

    mitk::PlaneGeometry::Pointer m_PlaneGeometry;

    mitk::Point3D m_CenterPoint;

    mitk::Point3D m_ScalingPoint;

    int m_ControlPointSelectedIndex;

    int m_MinControlPointNumber;
    int m_MaxControlPointNumber;

    std::vector<mitk::Point3D> m_ControlPoints;

    std::vector<mitk::Point3D> m_ContourPoints;

	int m_ControlPointNonRemovableIndices[5];

    bool m_InitiallyPlaced;

    mitk::Point3D m_PreviewControlPoint;

    bool m_PreviewControlPointVisible;

    vtkImageData* m_VtkImageSlice;

    int m_SubdivisionNumber;

    SubdivisionType m_SubdivisionType;

    double m_SubdivisionSpacing;

  };


#endif // SVCONTOUR_H
