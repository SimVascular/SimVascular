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

#include "sv3_PolygonContour.h"
#include "sv_Math.h"
#include  <cmath>

using sv3::ContourPolygon;
ContourPolygon::ContourPolygon() : Contour()
{
    m_Method="Manual";
    m_Type="Polygon";

    m_MinControlPointNumber=4;
    m_MaxControlPointNumber=200;

    m_ControlPointNonRemovableIndices[0]=0;
    m_ControlPointNonRemovableIndices[1]=1;
    //m_Extendable=true;
}

ContourPolygon::ContourPolygon(const ContourPolygon &other)
    : Contour(other)
{
}

ContourPolygon::~ContourPolygon()
{
}

ContourPolygon* ContourPolygon::Clone()
{
    return new ContourPolygon(*this);
}

std::string ContourPolygon::GetClassName()
{
    return "ContourPolygon";
}


std::vector<std::array<double,3> > CreateInterpolationPoints(std::array<double,3>  pt1, std::array<double,3>  pt2, int interNumber)
{
    std::vector<std::array<double,3> > points;

    double dx,dy,dz;
    dx=(pt2[0]-pt1[0])/interNumber;
    dy=(pt2[1]-pt1[1])/interNumber;
    dz=(pt2[2]-pt1[2])/interNumber;

    std::array<double,3>  pt;
    for(int i=1;i<interNumber;i++)
    {
        pt[0]=pt1[0]+i*dx;
        pt[1]=pt1[1]+i*dy;
        pt[2]=pt1[2]+i*dz;

        points.push_back(pt);
    }

    return points;
}

void ContourPolygon::SetControlPoint(int index, std::array<double,3>  point)
{
    double tmp[3], projPt[3];
    for (int i = 0; i<3; i++)
        tmp[i] = point[i];
    m_vtkPlaneGeometry->ProjectPoint(tmp, projPt);
    if (index>=m_ControlPoints.size())
    {
        fprintf(stderr, "Unable to set control point\n");
        return;
    }
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index<0||index>m_ControlPoints.size()-1) return;

    if(index==0)
    {
        std::array<double,3> dirVec;
        for (int i = 0; i<3; i++)
            dirVec[i]=projPt[i]-m_ControlPoints[index][i];
        Shift(dirVec);
    }
    else if(index==1)
    {
        Scale(m_ControlPoints[0], m_ControlPoints[index], std::array<double,3>{projPt[0],projPt[1],projPt[2]});
    }
    else if(index<m_ControlPoints.size())
    {
        m_ControlPoints[index]=std::array<double,3>{projPt[0],projPt[1],projPt[2]};
        ControlPointsChanged();
    }
}

void ContourPolygon::CreateContourPoints()
{
    //exclude the first two points
    
    if (m_ContourPoints.size()!=0)
        m_ContourPoints.clear();

    int controlNumber=GetControlPointNumber();

    if(controlNumber<=2)
    {
        return;
    }
    else if(controlNumber==3)
    {
        m_ContourPoints.push_back(GetControlPoint(2));
        return;
    }

    std::vector<std::array<double,3> > tempControlPoints=m_ControlPoints;
    tempControlPoints.push_back(m_ControlPoints[2]);

    int interNumber;

    switch(m_SubdivisionType)
    {
    case CONSTANT_TOTAL_NUMBER:
        if(m_Closed)
            interNumber=std::ceil(m_SubdivisionNumber*1.0/(controlNumber-2));
        else
            interNumber=std::ceil((m_SubdivisionNumber-1.0)/(controlNumber-3));
        break;
    case CONSTANT_SUBDIVISION_NUMBER:
            interNumber=m_SubdivisionNumber;
        break;
    default:
        break;
    }

    int controlBeginIndex=2;
    for(int i=controlBeginIndex;i<controlNumber;i++)
    {
        std::array<double,3>  pt1,pt2;
        pt1=tempControlPoints[i];
        pt2=tempControlPoints[i+1];

        m_ContourPoints.push_back(pt1);

        if(i==controlNumber-1 &&!m_Closed) break;

        if(m_SubdivisionType==CONSTANT_SPACING)
        {
            double dist = sqrt(pow(pt2[0]-pt1[0],2)+pow(pt2[1]-pt1[1],2)+pow(pt2[2]-pt1[2],2));
            interNumber=std::ceil(dist/m_SubdivisionSpacing);
        }

        std::vector<std::array<double,3> > interPoints=CreateInterpolationPoints(pt1,pt2,interNumber);

         m_ContourPoints.insert(m_ContourPoints.end(),interPoints.begin(),interPoints.end());
    }

}

int ContourPolygon::SearchControlPointByContourPoint( int contourPointIndex )
{
    if(contourPointIndex<-1 || contourPointIndex>=m_ContourPoints.size()) return -2;

    if(contourPointIndex==-1) return m_ControlPoints.size();

    int controlBeginIndex=2;//exclude the first two points

    for(int i=contourPointIndex;i<m_ContourPoints.size();i++)
    {
        for(int j=controlBeginIndex;j<m_ControlPoints.size();j++)
        {
            if(m_ContourPoints[i][0]==m_ControlPoints[j][0]
                    &&m_ContourPoints[i][1]==m_ControlPoints[j][1]
                    &&m_ContourPoints[i][2]==m_ControlPoints[j][2])
            {
                return j;
            }
        }
    }

    return m_ControlPoints.size();
}

void ContourPolygon::AssignCenterScalingPoints()
{
    if(m_ControlPoints.size()>1)
    {
        m_ControlPoints[0]=m_CenterPoint;
        m_ControlPoints[1]=m_ScalingPoint;
    }
}

void ContourPolygon::PlaceControlPoints(std::array<double,3>  point)
{
    Contour::PlaceControlPoints(point);
    m_ControlPointSelectedIndex = 3;
}

ContourPolygon* ContourPolygon::CreateSmoothedContour(int fourierNumber)
{
    if(m_ContourPoints.size()<3)
        return this->Clone();

    ContourPolygon* contour=new ContourPolygon();
    contour->SetPathPoint(m_PathPoint);
    std::string method=m_Method;
    int idx=method.find("Smoothed");
    if(idx<0)
        method=method+" + Smoothed";

    contour->SetMethod(method);
    contour->SetClosed(m_Closed);

    int pointNumber=m_ContourPoints.size();

    int smoothedPointNumber;

    if((2*pointNumber)<fourierNumber)
        smoothedPointNumber=3*fourierNumber;
    else
        smoothedPointNumber=pointNumber;

    cvMath *cMath = new cvMath();
    std::vector<std::array<double, 3> > smoothedContourPoints=cMath->CreateSmoothedCurve(m_ContourPoints,m_Closed,fourierNumber,0,smoothedPointNumber);
    delete cMath;
    contour->SetContourPoints(smoothedContourPoints);

    return contour;
}
