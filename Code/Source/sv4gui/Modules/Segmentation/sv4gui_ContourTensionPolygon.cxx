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

#include "sv4gui_ContourTensionPolygon.h"
#include "sv4gui_Contour.h"
#include <deque>

sv4guiContourTensionPolygon::sv4guiContourTensionPolygon()
{
    m_Method="Manual";
    m_Type="TensionPolygon";

    m_SubdivisionRounds=5;
    m_TensionParameter=0.0625;
}

sv4guiContourTensionPolygon::sv4guiContourTensionPolygon(const sv4guiContourTensionPolygon &other)
    : sv4guiContourPolygon(other)
    , m_SubdivisionRounds(other.m_SubdivisionRounds)
    , m_TensionParameter(other.m_TensionParameter)
{

}

sv4guiContourTensionPolygon::~sv4guiContourTensionPolygon()
{
}

sv4guiContourTensionPolygon* sv4guiContourTensionPolygon::Clone()
{
    return new sv4guiContourTensionPolygon(*this);
}

std::string sv4guiContourTensionPolygon::GetClassName()
{
    return "sv4guiContourTensionPolygon";
}

int sv4guiContourTensionPolygon::GetSubdivisionRounds()
{
    return m_SubdivisionRounds;
}

void sv4guiContourTensionPolygon::SetSubdivisionRounds(int rounds)
{
    m_SubdivisionRounds=rounds;
}

double sv4guiContourTensionPolygon::GetTensionParameter()
{
    return m_TensionParameter;
}

void sv4guiContourTensionPolygon::SetTensionParameter(double parameter)
{
    m_TensionParameter=parameter;
}

void sv4guiContourTensionPolygon::CreateContourPoints()
{
    std::deque<mitk::Point2D> subdivisionPoints;
    std::deque<mitk::Point2D> newSubdivisionPoints;

    subdivisionPoints.clear();

    int controlBeginIndex=2;

    for(int i=controlBeginIndex;i<m_ControlPoints.size();i++)
    {
        mitk::Point2D pt2d;
        m_PlaneGeometry->Map(GetControlPoint(i), pt2d );
        subdivisionPoints.push_back(pt2d);
    }

    if( m_ControlPoints.size() >= GetMinControlPointNumber() )
    {

        for( unsigned int i=0; i < m_SubdivisionRounds; i++ )
        {
            // Indices
            unsigned int index, indexPrev, indexNext, indexNextNext;

            unsigned int numberOfPoints = subdivisionPoints.size();

            mitk::Point2D newPoint;

            // Keep cycling our array indices forward until they wrap around at the end
            for ( index = 0; index < numberOfPoints; ++index )
            {
                // Create new subdivision point according to formula
                // p_new = (0.5 + tension) * (p_here + p_next) - tension * (p_prev + p_nextnext)
                indexPrev = (numberOfPoints + index - 1) % numberOfPoints;
                indexNext = (index + 1) % numberOfPoints;
                indexNextNext = (index + 2) % numberOfPoints;

                newPoint[0] = (0.5 + m_TensionParameter) * (double)( subdivisionPoints[index][0] + subdivisionPoints[indexNext][0] )
                        - m_TensionParameter * (double)( subdivisionPoints[indexPrev][0] + subdivisionPoints[indexNextNext][0]);
                newPoint[1] = (0.5 + m_TensionParameter) * (double)( subdivisionPoints[index][1] + subdivisionPoints[indexNext][1] )
                        - m_TensionParameter * (double)( subdivisionPoints[indexPrev][1] + subdivisionPoints[indexNextNext][1]);

                newSubdivisionPoints.push_back( newPoint );
            }

            std::deque<mitk::Point2D> mergedSubdivisionPoints;
            std::deque<mitk::Point2D>::iterator it, itNew;


            for ( it = subdivisionPoints.begin() , itNew = newSubdivisionPoints.begin();
                  it != subdivisionPoints.end();
                  ++it, ++itNew )
            {
                mergedSubdivisionPoints.push_back( *it );
                mergedSubdivisionPoints.push_back( *itNew );
            }

            subdivisionPoints = mergedSubdivisionPoints;

            newSubdivisionPoints.clear();
        }
    }

//    bool isInitiallyPlaced = true;

    unsigned int i;
    std::deque<mitk::Point2D>::iterator it;
    for ( it = subdivisionPoints.begin(), i = 0;
          it != subdivisionPoints.end();
          ++it, ++i )
    {
//        unsigned int nextIndex;
//        if ( i == 0 )
//        {
//            nextIndex = m_ControlPoints.size() - 1;
//        }
//        else
//        {
//            nextIndex = (((i - 1) >> m_SubdivisionRounds) + 1) % m_ControlPoints.size();
//            if(!isInitiallyPlaced && nextIndex > m_ControlPoints.size()-2)
//            {
//                m_ContourPoints.push_back(m_ControlPoints[m_ControlPoints.size()-1]);
//                break;
//            }
//        }

        mitk::Point3D pt3d;

        m_PlaneGeometry->Map(*it, pt3d);
        std::array<double,3> stdPt3d;
        for (int i=0; i<3; i++)
            stdPt3d[i] = pt3d[i];
        m_ContourPoints.push_back(stdPt3d);

    }

    subdivisionPoints.clear();

    //Replace contour points with control points at the same location;
    //The previous contour points are converted from control points by twice mapping ,so they are not exactly equal.
    for(int i=controlBeginIndex;i<m_ControlPoints.size();i++){
        int contourPointIndex=(i-controlBeginIndex)*(1<<m_SubdivisionRounds);
        m_ContourPoints[contourPointIndex]=m_ControlPoints[controlBeginIndex];
    }

}



