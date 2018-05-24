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

#include "sv4gui_Math3.h"

#include "sv_Math.h"

#include <mitkNumericConstants.h>
#include <mitkVector.h>
#include <mitkLine.h>

std::vector<mitk::Point3D> sv4guiMath3::CreateSmoothedCurve(std::vector<mitk::Point3D> points, bool closed, int numModes, int sampleRate, int outputNumPts)
{
    std::vector<mitk::Point3D> outputPoints;
    int numPts=points.size();

    std::vector<mitk::Point3D> actualPoints;

    if(sampleRate>0)
    {
        for(int i=0;i<numPts;i+=sampleRate){
            actualPoints.push_back(points[i]);
        }
        if(sampleRate>1){
            actualPoints.push_back(points[numPts-1]);
        }
        outputNumPts=actualPoints.size();
    }
    else if(outputNumPts>1)
    {
        actualPoints=points;
    }
    else
    {
        return outputPoints;
    }

    if(!closed)
    {
        for(int i=outputNumPts-1;i>=0;i--){
            actualPoints.push_back(actualPoints[i]);
        }
    }

    int actualNumPts=actualPoints.size();

    cvMath *cMath = new cvMath();

    double **pts = cMath->createArray(actualNumPts,3);
    for(int i=0;i<actualNumPts;i++)
    {
        pts[i][0] = actualPoints[i][0];
        pts[i][1] = actualPoints[i][1];
        pts[i][2] = actualPoints[i][2];
    }
    double **outPts = NULL;
    int isClosed=closed?1:0;
    int rslt;
    if(closed)
    {
        rslt=cMath->smoothCurve(pts, actualNumPts, 1, numModes, outputNumPts, &outPts);
    }
    else
    {
        rslt=cMath->smoothCurve(pts, actualNumPts, 0, numModes, 2*outputNumPts, &outPts);
    }
    delete cMath;
    if (rslt == SV_ERROR) {
        return outputPoints;
    }

    for(int i=0;i<outputNumPts;i++){
        mitk::Point3D point;
        point[0]=outPts[i][0];
        point[1]=outPts[i][1];
        point[2]=outPts[i][2];

        outputPoints.push_back(point);
    }

    return outputPoints;
}

int sv4guiMath3::GetInsertintIndexByDistance( std::vector<mitk::Point3D> points, mitk::Point3D point, bool insertOnlyIfDifferent, bool useDistanceSum)
{
    if(useDistanceSum)
        return GetInsertintIndexByDistanceSum(points, point,insertOnlyIfDifferent);
    else
        return GetInsertintIndexByProjectedDistance(points, point,insertOnlyIfDifferent);
}

int sv4guiMath3::GetInsertintIndexByDistanceSum( std::vector<mitk::Point3D> points, mitk::Point3D point, bool insertOnlyIfDifferent)
{
    int idx=-2;

    if(points.size()<2){
        idx=points.size();
    }else{
        bool firstTime=true;
        int markingIndex;
        int insertingIndex=-2;
        double minDist;

        for(int i=0;i<points.size()-1;i++)
        {
            mitk::Point3D startPoint=points[i];
            mitk::Point3D endPoint=points[i+1];

            double dist1 = startPoint.EuclideanDistanceTo(point);
            double dist2 = endPoint.EuclideanDistanceTo(point);

            if(dist1==0)//check if a same point already exist
            {
                if(insertOnlyIfDifferent)
                    return -2;
                else
                    return i;
            }

            if(dist2==0)//check if a same point already exist
            {
                if(insertOnlyIfDifferent)
                    return -2;
                else
                    return i+1;
            }

            double distSum=dist1+dist2;
            if(firstTime){
                markingIndex=i;
                minDist=distSum;
                firstTime=false;
            }
            else if(distSum<minDist)
            {
                markingIndex=i;
                minDist=distSum;
            }

        }

        mitk::Point3D startPoint=points[markingIndex];
        mitk::Point3D endPoint=points[markingIndex+1];

        mitk::Vector3D n1 = endPoint - startPoint;
        n1.Normalize();

        double l1 = n1 * (point - startPoint);
        double l2 = -n1 * (point - endPoint);

        if(l1<=0.0)
        {
            insertingIndex=markingIndex;
        }else if(l2<=0.0){
            insertingIndex=markingIndex+2;
        }else{
            insertingIndex=markingIndex+1;
        }

        if(insertingIndex!=-2)
        {
            idx=insertingIndex;
        }
    }

    return idx;
}

int sv4guiMath3::GetInsertintIndexByProjectedDistance( std::vector<mitk::Point3D> points, mitk::Point3D point, bool insertOnlyIfDifferent)
{
    int idx=-2;

    if(points.size()<2){
        idx=points.size();
    }else{
        bool firstTime=true;
        int insertingIndex=-2;
        double minDist;

        for(int i=0;i<points.size()-1;i++)
        {
            mitk::Point3D startPoint=points[i];
            mitk::Point3D endPoint=points[i+1];

            mitk::Vector3D n1 = endPoint - startPoint;
            n1.Normalize();

            double l1 = n1 * (point - startPoint);
            double l2 = -n1 * (point - endPoint);

            mitk::Point3D crossPoint = startPoint + n1 * l1;

            double dist1 = startPoint.EuclideanDistanceTo(point);
            double dist2 = endPoint.EuclideanDistanceTo(point);
            double dist3 = crossPoint.EuclideanDistanceTo(point);

            if(dist1==0)//check if a same point already exist
            {
                if(insertOnlyIfDifferent)
                    return -2;
                else
                    return i;
            }

            if(dist2==0)//check if a same point already exist
            {
                if(insertOnlyIfDifferent)
                    return -2;
                else
                    return i+1;
            }

            if(l1>=0.0&&l2>=0.0)
            {
                if(firstTime || dist3<minDist)
                {
                    insertingIndex=i+1;
                    minDist=dist3;
                }
            }
            else if(l1<0)
            {
                if(firstTime || dist1<minDist)
                {
                    insertingIndex=i;
                    minDist=dist1;
                }
            }
            else
            {
                if(firstTime || dist2<minDist)
                {
                    insertingIndex=i+2;
                    minDist=dist2;
                }
            }

            firstTime=false;
        }

        idx=insertingIndex;
    }

    return idx;
}

bool sv4guiMath3::InsideBounds(mitk::Point3D point, double bounds[6])
{
    if(point[0]-bounds[0]>-mitk::eps && point[0]-bounds[1]<mitk::eps
            &&point[1]-bounds[2]>-mitk::eps && point[1]-bounds[3]<mitk::eps
            &&point[2]-bounds[4]>-mitk::eps && point[2]-bounds[5]<mitk::eps)
        return true;
    else
        return false;
}

bool sv4guiMath3::GetIntersectionPoint(mitk::PlaneGeometry* plane, mitk::Point3D point, mitk::Vector3D direction,mitk::Point3D& interPoint)
{
    mitk::Line3D line( point, direction );
    mitk::Point3D point1;
    bool found=plane->IntersectionPoint(line, point1);
    if(found)
    {
       double dotProduct= direction*(point1-point);
       if(dotProduct>0)
       {
           interPoint=point1;
           return true;
       }
    }

    return false;
}

double sv4guiMath3::GetMachineEpsilon()
{
    double num = 1.0;
    double test = 1.0;

    while ( num + test > num ) {
      test /= 10.0;
    }
    return (test * 10.0);
}

mitk::Vector3D sv4guiMath3::GetPerpendicularNormalVector(mitk::Vector3D vec)
{
    mitk::Vector3D pvec;

    pvec.Fill(0);

    if(vec[0]==0&&vec[1]==0&&vec[2]==0)
    {
        //        pvec[2]=1;
        return pvec;
    }

    int replaceIdx;

    double dotProduct=0;

    if(std::abs(vec[2])>0.0001)
    {
        pvec[1]=1;
        replaceIdx=2;
        dotProduct=vec[0]*pvec[0]+vec[1]*pvec[1];
    }
    else if(std::abs(vec[1])>0.0001)
    {
        pvec[0]=1;
        replaceIdx=1;
        dotProduct=vec[0]*pvec[0]+vec[2]*pvec[2];
    }
    else
    {
        pvec[2]=1;
        replaceIdx=0;
        dotProduct=vec[1]*pvec[1]+vec[2]*pvec[2];
    }

    pvec[replaceIdx]=-dotProduct/vec[replaceIdx];

    pvec.Normalize();

    return pvec;
}
