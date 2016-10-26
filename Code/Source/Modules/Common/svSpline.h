#ifndef SVSPLINE_H
#define SVSPLINE_H

#include "SimVascular.h"

#include "svCommonExports.h"

#include <mitkPoint.h>
#include <mitkVector.h>

class svVtkParametricSpline;

class SVCOMMON_EXPORT svSpline
{
public:

    enum CalculationMethod {CONSTANT_TOTAL_NUMBER, CONSTANT_SUBDIVISION_NUMBER, CONSTANT_SPACING};

    struct svSplinePoint
    {
        int id;
        mitk::Point3D pos;
        mitk::Vector3D tangent;
        mitk::Vector3D rotation;
    };

    svSpline();

    svSpline(bool closed, CalculationMethod method, int  furtherSubdivionNumber = 10);

    virtual ~svSpline();

    void SetClosed(bool closed = true);

    bool IsClosed();

    void SetSpacing(double spacing);

    double GetSpacing();

    void SetMethod(CalculationMethod method = CONSTANT_TOTAL_NUMBER );

    CalculationMethod GetMethod();

    void SetCalculationNumber(int number);

    int GetCalculationNumber();

    void SetFurtherSubdivisionNumber(int number);

    int GetFurtherSubdivsionNumber();

    void SetInputPoints(std::vector<mitk::Point3D> inputPonits);

    std::vector<mitk::Point3D>  GetInputPoints();

    std::vector<svSplinePoint> GetSplinePoints();

    std::vector<mitk::Point3D> GetSplinePosPoints();

    void Update();

    double GetLength(svVtkParametricSpline* svpp, double idx1, double idx2);

    mitk::Point3D GetPoint(svVtkParametricSpline* svpp, double idx);

protected:

    bool m_Closed;

    double m_Spacing;

    CalculationMethod m_Method;

    int m_CalculationNumber;

    int m_FurtherSubdivisionNumber; //for tangent calculation;

    std::vector<mitk::Point3D> m_InputPoints;

    std::vector<svSplinePoint> m_SplinePoints;

};

#endif // SVSPLINE_H
