#ifndef SVPATHELEMENT_H
#define SVPATHELEMENT_H

#include "SimVascular.h"

#include <svPathExports.h>

#include "svSpline.h"

#include "mitkPoint.h"

class SVPATH_EXPORT svPathElement
{
public:

    enum CalculationMethod {CONSTANT_TOTAL_NUMBER, CONSTANT_SUBDIVISION_NUMBER, CONSTANT_SPACING};

    struct svControlPoint
    {
        int id=-1;
        bool selected=false;
        mitk::Point3D point;
    };

    typedef svSpline::svSplinePoint svPathPoint;

    svPathElement();

    svPathElement(const svPathElement &other);

    virtual ~svPathElement();

    svPathElement* Clone();

    int GetControlPointNumber();

    std::vector<mitk::Point3D> GetControlPoints();

    svControlPoint GetsvControlPoint(int index) ;

    mitk::Point3D GetControlPoint(int index);

    void InsertControlPoint(int index, mitk::Point3D point);

    int GetInsertintIndexByDistance( mitk::Point3D point);

    void RemoveControlPoint(int index);

    void SetControlPoint(int index, mitk::Point3D point);

    void SetControlPoints(std::vector<mitk::Point3D> points, bool update = true);

    void ControlPointsChanged();

    bool IsControlPointSelected(int index) ;

    void SetControlPointSelected( int index, bool selected);

    void DeselectControlPoint();

    int GetControlPointSelectedIndex();

    int SearchControlPoint( mitk::Point3D point, mitk::ScalarType distance);

    svPathElement* CreateSmoothedPathElement(int sampleRate, int numModes, bool controlPointsBased = true ); //otherwise pathPointsBased

    int GetPathPointNumber();

    void SetSpacing(double spacing);

    double GetSpacing();

    void SetMethod(CalculationMethod method = CONSTANT_TOTAL_NUMBER );

    CalculationMethod GetMethod();

    void SetCalculationNumber(int number);

    int GetCalculationNumber();

    std::vector<svPathPoint>  GetPathPoints();

    std::vector<mitk::Point3D> GetPathPosPoints();

    svPathPoint GetPathPoint(int index) ;

    mitk::Point3D GetPathPosPoint(int index) ;

    void SetPathPoints(std::vector<svPathElement::svPathPoint> pathPoints);

    void CreatePathPoints() ;

    void CalculateBoundingBox(double *bounds);

    std::vector<svPathPoint> GetExtendedPathPoints(double realBounds[6], double minSpacing, int& startingIndex);

protected:

    std::vector<svControlPoint> m_ControlPoints;

    std::vector<svPathPoint> m_PathPoints;

    double m_Spacing;

    CalculationMethod m_Method;

    int m_CalculationNumber;
};

#endif // SVPATHELEMENT_H
