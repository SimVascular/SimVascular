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

#include "sv4gui_Spline.h"
#include "sv4gui_VtkParametricSpline.h"
#include "sv4gui_Math3.h"

#include "vtkParametricSpline.h"
#include "vtkSmartPointer.h"
#include "vtkSpline.h"
#include "vtkPoints.h"

sv4guiSpline::sv4guiSpline()
    : m_FurtherSubdivisionNumber(10)
{
}

sv4guiSpline::sv4guiSpline(bool closed, CalculationMethod method, int furtherSubdivisionNumber)
    : m_Closed(closed)
    , m_Method(method)
    , m_FurtherSubdivisionNumber(furtherSubdivisionNumber)
{
}

sv4guiSpline::~sv4guiSpline()
{
}

void sv4guiSpline::SetClosed(bool closed)
{
    m_Closed=closed;
}

bool sv4guiSpline::IsClosed()
{
    return m_Closed;
}

void sv4guiSpline::SetSpacing(double spacing)
{
    m_Spacing=spacing;
}

double sv4guiSpline::GetSpacing()
{
    return m_Spacing;
}

void sv4guiSpline::SetMethod(CalculationMethod method)
{
    m_Method=method;
}

sv4guiSpline::CalculationMethod sv4guiSpline::GetMethod()
{
    return m_Method;
}

void sv4guiSpline::SetCalculationNumber(int number)
{
    m_CalculationNumber=number;
}

int sv4guiSpline::GetCalculationNumber()
{
    return m_CalculationNumber;
}

void sv4guiSpline::SetFurtherSubdivisionNumber(int number)
{
    m_FurtherSubdivisionNumber=number;
}

int sv4guiSpline::GetFurtherSubdivsionNumber()
{
    return m_FurtherSubdivisionNumber;
}

void sv4guiSpline::SetInputPoints(std::vector<mitk::Point3D> inputPoints)
{
    m_InputPoints=inputPoints;
}

std::vector<mitk::Point3D>  sv4guiSpline::GetInputPoints()
{
    return m_InputPoints;
}

std::vector<sv4guiSpline::sv4guiSplinePoint> sv4guiSpline::GetSplinePoints()
{
    return m_SplinePoints;
}

std::vector<mitk::Point3D> sv4guiSpline::GetSplinePosPoints()
{
    std::vector<mitk::Point3D> posPoints;
    for(int i=0;i<m_SplinePoints.size();i++)
        posPoints.push_back(m_SplinePoints[i].pos);

    return posPoints;
}

mitk::Point3D sv4guiSpline::GetPoint(sv4guiVtkParametricSpline* svpp, double t)
{
    double pt[3];
    mitk::Point3D point;
    svpp->Evaluate(t, pt);

    point[0]=pt[0];
    point[1]=pt[1];
    point[2]=pt[2];

    return point;
}

double sv4guiSpline::GetLength(sv4guiVtkParametricSpline* svpp, double t1, double t2)
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

void sv4guiSpline::Update()
{
    m_SplinePoints.clear();

    sv4guiVtkParametricSpline* svpp= new sv4guiVtkParametricSpline();
    svpp->ParameterizeByLengthOff();

    if(m_Closed)
        svpp->ClosedOn();
    else
        svpp->ClosedOff();

    int inputPointNumber=m_InputPoints.size();
    svpp->SetNumberOfPoints(inputPointNumber);

    for(int i=0;i<inputPointNumber;i++)
        svpp->SetPoint(i,m_InputPoints[i][0],m_InputPoints[i][1],m_InputPoints[i][2]);


    sv4guiSplinePoint splinePoint;
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
            splinePoint.rotation=sv4guiMath3::GetPerpendicularNormalVector(splinePoint.tangent);
            m_SplinePoints.push_back(splinePoint);
            break;
        }

        double txx=i+1.0/interNumber/m_FurtherSubdivisionNumber;
        ptx=GetPoint(svpp,txx);

        splinePoint.id=splinePointID;
        splinePointID++;
        splinePoint.tangent=ptx-pt1;
        splinePoint.tangent.Normalize();
        splinePoint.rotation=sv4guiMath3::GetPerpendicularNormalVector(splinePoint.tangent);
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
            splinePoint.rotation=sv4guiMath3::GetPerpendicularNormalVector(splinePoint.tangent);
            m_SplinePoints.push_back(splinePoint);
        }

    }

}
