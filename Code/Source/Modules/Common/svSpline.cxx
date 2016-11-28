#include "svSpline.h"
#include "svVtkParametricSpline.h"
#include "svMath3.h"

#include "vtkParametricSpline.h"
#include "vtkSmartPointer.h"
#include "vtkSpline.h"
#include "vtkPoints.h"

svSpline::svSpline()
    : m_FurtherSubdivisionNumber(10)
{
}

svSpline::svSpline(bool closed, CalculationMethod method, int furtherSubdivisionNumber)
    : m_Closed(closed)
    , m_Method(method)
    , m_FurtherSubdivisionNumber(furtherSubdivisionNumber)
{
}

svSpline::~svSpline()
{
}

void svSpline::SetClosed(bool closed)
{
    m_Closed=closed;
}

bool svSpline::IsClosed()
{
    return m_Closed;
}

void svSpline::SetSpacing(double spacing)
{
    m_Spacing=spacing;
}

double svSpline::GetSpacing()
{
    return m_Spacing;
}

void svSpline::SetMethod(CalculationMethod method)
{
    m_Method=method;
}

svSpline::CalculationMethod svSpline::GetMethod()
{
    return m_Method;
}

void svSpline::SetCalculationNumber(int number)
{
    m_CalculationNumber=number;
}

int svSpline::GetCalculationNumber()
{
    return m_CalculationNumber;
}

void svSpline::SetFurtherSubdivisionNumber(int number)
{
    m_FurtherSubdivisionNumber=number;
}

int svSpline::GetFurtherSubdivsionNumber()
{
    return m_FurtherSubdivisionNumber;
}

void svSpline::SetInputPoints(std::vector<mitk::Point3D> inputPoints)
{
    m_InputPoints=inputPoints;
}

std::vector<mitk::Point3D>  svSpline::GetInputPoints()
{
    return m_InputPoints;
}

std::vector<svSpline::svSplinePoint> svSpline::GetSplinePoints()
{
    return m_SplinePoints;
}

std::vector<mitk::Point3D> svSpline::GetSplinePosPoints()
{
    std::vector<mitk::Point3D> posPoints;
    for(int i=0;i<m_SplinePoints.size();i++)
        posPoints.push_back(m_SplinePoints[i].pos);

    return posPoints;
}

mitk::Point3D svSpline::GetPoint(svVtkParametricSpline* svpp, double t)
{
    double pt[3];
    mitk::Point3D point;
    svpp->Evaluate(t, pt);

    point[0]=pt[0];
    point[1]=pt[1];
    point[2]=pt[2];

    return point;
}

double svSpline::GetLength(svVtkParametricSpline* svpp, double t1, double t2)
{
    int subdivisionNumber=10;
    double interval=(t2-t1)/subdivisionNumber;

    mitk::Point3D point1, point2;

    double totalLength=0.0;
    for(int i=0;i<subdivisionNumber;i++)
    {
        double tt1=t1+interval*i;
        double tt2=t1+interval*(i+1);
        if(i==subdivisionNumber-1)
        {
            tt2=t2;//make sure equal to t2, considering floating error
        }

        point1=GetPoint(svpp,tt1);
        point2=GetPoint(svpp,tt2);

        double length=point2.EuclideanDistanceTo(point1);
        totalLength+=length;
    }

    return totalLength;
}

void svSpline::Update()
{
    m_SplinePoints.clear();

    svVtkParametricSpline* svpp= new svVtkParametricSpline();
    svpp->ParameterizeByLengthOff();

    if(m_Closed)
        svpp->ClosedOn();
    else
        svpp->ClosedOff();

    int inputPointNumber=m_InputPoints.size();
    svpp->SetNumberOfPoints(inputPointNumber);

    for(int i=0;i<inputPointNumber;i++)
        svpp->SetPoint(i,m_InputPoints[i][0],m_InputPoints[i][1],m_InputPoints[i][2]);


    svSplinePoint splinePoint;
    mitk::Point3D pt1,ptx;
    int interNumber;

    switch(m_Method)
    {
    case CONSTANT_TOTAL_NUMBER:
        if(m_Closed)
            interNumber=std::ceil((m_CalculationNumber*1.0)/inputPointNumber);
        else
            interNumber=std::ceil((m_CalculationNumber-1.0)/(inputPointNumber-1.0));
        break;
    case CONSTANT_SUBDIVISION_NUMBER:
        interNumber=m_CalculationNumber;
        break;
    default:
        break;
    }

    int splinePointID=0;

    for(int i=0;i<inputPointNumber;i++)
    {
        pt1=m_InputPoints[i];

        if(m_Method==CONSTANT_SPACING)
        {
            if(i<inputPointNumber-1||m_Closed)
            {
                interNumber=std::ceil(GetLength(svpp,i,i+1)/m_Spacing);
                if(interNumber<5) interNumber=5;//make sure not too small
            }//otherwise interNumber not changes.It means using the previous value
        }

        splinePoint.pos=pt1;

        if(i==inputPointNumber-1 &&!m_Closed)
        {
            double tx=i-1.0/interNumber/m_FurtherSubdivisionNumber;
            ptx=GetPoint(svpp,tx);

            splinePoint.id=splinePointID;
            splinePointID++;
            splinePoint.tangent=pt1-ptx;
            splinePoint.tangent.Normalize();
            splinePoint.rotation=svMath3::GetPerpendicularNormalVector(splinePoint.tangent);
            m_SplinePoints.push_back(splinePoint);
            break;
        }

        double txx=i+1.0/interNumber/m_FurtherSubdivisionNumber;
        ptx=GetPoint(svpp,txx);

        splinePoint.id=splinePointID;
        splinePointID++;
        splinePoint.tangent=ptx-pt1;
        splinePoint.tangent.Normalize();
        splinePoint.rotation=svMath3::GetPerpendicularNormalVector(splinePoint.tangent);
        m_SplinePoints.push_back(splinePoint);

        for(int j=1;j<interNumber;j++)
        {
            double tnew=i+j*1.0/interNumber;
            double tx=tnew+1.0/interNumber/m_FurtherSubdivisionNumber;

            pt1=GetPoint(svpp,tnew);
            ptx=GetPoint(svpp,tx);

            splinePoint.id=splinePointID;
            splinePointID++;
            splinePoint.pos=pt1;
            splinePoint.tangent=ptx-pt1;
            splinePoint.tangent.Normalize();
            splinePoint.rotation=svMath3::GetPerpendicularNormalVector(splinePoint.tangent);
            m_SplinePoints.push_back(splinePoint);
        }

    }

}
