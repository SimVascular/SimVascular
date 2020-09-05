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

#include "sv4gui_ContourPolygon.h"
#include "sv3_PolygonContour.h"

using sv3::ContourPolygon;

sv4guiContourPolygon::sv4guiContourPolygon()
{
    
    m_Method="Manual";
    m_Type="Polygon";

    m_MinControlPointNumber=4;
    m_MaxControlPointNumber=200;

    m_ControlPointNonRemovableIndices[0]=0;
    m_ControlPointNonRemovableIndices[1]=1;
    m_Extendable=true;
    SetKernel(cKernelType::cKERNEL_POLYGON);
}

sv4guiContourPolygon::sv4guiContourPolygon(const sv4guiContourPolygon &other)
    : sv4guiContour(other)
{
}

sv4guiContourPolygon::~sv4guiContourPolygon()
{
}

sv4guiContourPolygon* sv4guiContourPolygon::Clone()
{
    return new sv4guiContourPolygon(*this);
}

std::string sv4guiContourPolygon::GetClassName()
{
    return "sv4guiContourPolygon";
}

std::vector<mitk::Point3D> CreateInterpolationPoints(mitk::Point3D pt1, mitk::Point3D pt2, int interNumber)
{
    std::vector<mitk::Point3D> points;

    double dx,dy,dz;
    dx=(pt2[0]-pt1[0])/interNumber;
    dy=(pt2[1]-pt1[1])/interNumber;
    dz=(pt2[2]-pt1[2])/interNumber;

    mitk::Point3D pt;
    for(int i=1;i<interNumber;i++)
    {
        pt[0]=pt1[0]+i*dx;
        pt[1]=pt1[1]+i*dy;
        pt[2]=pt1[2]+i*dz;

        points.push_back(pt);
    }

    return points;
}

void sv4guiContourPolygon::SetControlPoint(int index, mitk::Point3D point)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index<0||index>m_ControlPoints.size()-1) return;

    
    if(index==0)
    {
        mitk::Vector3D dirVec=point-GetControlPoint(index);
        Shift(dirVec);
    }
    else if(index==1)
    {
        mitk::Point3D tmpPt;
        Scale(GetControlPoint(0), GetControlPoint(index), point);
    }
    else if(index<m_ControlPoints.size())
    {
        for(int i=0; i<3;i++)
            m_ControlPoints[index][i] = point[i];       
        ControlPointsChanged();
    }
}

void sv4guiContourPolygon::CreateContourPoints()
{
    //exclude the first two points

    int controlNumber=GetControlPointNumber();

    if(controlNumber<=2)
    {
        return;
    }
    else if(controlNumber==3)
    {
        std::array<double,3> stdpt;
        mitk::Point3D mitkpt = GetControlPoint(2);
        for(int i=0; i<3; i++)
            stdpt[i]=mitkpt[i];
        m_ContourPoints.push_back(stdpt);
        return;
    }


    std::vector<mitk::Point3D> tempControlPoints(m_ControlPoints.size());
    for(int i=0; i<m_ControlPoints.size(); i++)
    {    
    
        for (int j=0; j<3; j++)
            tempControlPoints[i][j] = m_ControlPoints[i][j];
    }
    mitk::Point3D tmpPt;
    for(int i=0; i<3; i++)
        tmpPt[i] = m_ControlPoints[2][i];
    tempControlPoints.push_back(tmpPt);

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
        mitk::Point3D pt1,pt2;
        pt1=tempControlPoints[i];
        pt2=tempControlPoints[i+1];

        m_ContourPoints.push_back(std::array<double,3>{pt1[0],pt1[1],pt1[2]});

        if(i==controlNumber-1 &&!m_Closed) break;

        if(m_SubdivisionType==CONSTANT_SPACING)
        {
            interNumber=std::ceil(pt2.EuclideanDistanceTo(pt1)/m_SubdivisionSpacing);
        }

        std::vector<mitk::Point3D> interPoints=CreateInterpolationPoints(pt1,pt2,interNumber);
        std::vector<std::array<double,3> > interPtsStd(interPoints.size()); 
        for(int p=0; p<interPoints.size();p++)
            for(int q=0; q<3;q++)
                interPtsStd[p][q] = interPoints[p][q];
         m_ContourPoints.insert(m_ContourPoints.end(),interPtsStd.begin(),interPtsStd.end());
    }

}

int sv4guiContourPolygon::SearchControlPointByContourPoint( int contourPointIndex )
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

void sv4guiContourPolygon::AssignCenterScalingPoints()
{
    if(m_ControlPoints.size()>1)
    {
        m_ControlPoints[0]=m_CenterPoint;
        m_ControlPoints[1]=m_ScalingPoint;
    }
}
void sv4guiContourPolygon::PlaceControlPoints(mitk::Point3D point)
{
    sv4guiContour::PlaceControlPoints(point);
    m_ControlPointSelectedIndex = 3;
}
