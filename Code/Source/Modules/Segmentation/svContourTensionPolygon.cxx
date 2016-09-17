#include "svContourTensionPolygon.h"
#include <deque>

svContourTensionPolygon::svContourTensionPolygon()
{
    m_Method="Manual";
    m_Type="TensionPolygon";

    m_SubdivisionRounds=5;
    m_TensionParameter=0.0625;
}

svContourTensionPolygon::svContourTensionPolygon(const svContourTensionPolygon &other)
    : svContourPolygon(other)
    , m_SubdivisionRounds(other.m_SubdivisionRounds)
    , m_TensionParameter(other.m_TensionParameter)
{

}

svContourTensionPolygon::~svContourTensionPolygon()
{
}

svContourTensionPolygon* svContourTensionPolygon::Clone()
{
    return new svContourTensionPolygon(*this);
}

std::string svContourTensionPolygon::GetClassName()
{
    return "svContourTensionPolygon";
}

int svContourTensionPolygon::GetSubdivisionRounds()
{
    return m_SubdivisionRounds;
}

void svContourTensionPolygon::SetSubdivisionRounds(int rounds)
{
    m_SubdivisionRounds=rounds;
}

double svContourTensionPolygon::GetTensionParameter()
{
    return m_TensionParameter;
}

void svContourTensionPolygon::SetTensionParameter(double parameter)
{
    m_TensionParameter=parameter;
}

void svContourTensionPolygon::CreateContourPoints()
{
    std::deque<mitk::Point2D> subdivisionPoints;
    std::deque<mitk::Point2D> newSubdivisionPoints;

    subdivisionPoints.clear();

    int controlBeginIndex=2;

    for(int i=controlBeginIndex;i<m_ControlPoints.size();i++)
    {
        mitk::Point2D pt2d;
        m_PlaneGeometry->Map(m_ControlPoints[i], pt2d );
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
        m_ContourPoints.push_back(pt3d);

    }

    subdivisionPoints.clear();

    //Replace contour points with control points at the same location;
    //The previous contour points are converted from control points by twice mapping ,so they are not exactly equal.
    for(int i=controlBeginIndex;i<m_ControlPoints.size();i++){
        int contourPointIndex=(i-controlBeginIndex)*(1<<m_SubdivisionRounds);
        m_ContourPoints[contourPointIndex]=m_ControlPoints[controlBeginIndex];
    }

}



