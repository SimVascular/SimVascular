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

#ifndef SV4GUI_PATHELEMENT_H
#define SV4GUI_PATHELEMENT_H

#include "SimVascular.h"
#include "sv3_PathElement.h"

#include <sv4guiModulePathExports.h>

#include "sv4gui_Spline.h"

#include "mitkPoint.h"

class SV4GUIMODULEPATH_EXPORT sv4guiPathElement : public sv3::PathElement
{
public:

    struct svControlPoint
    {
        int id=-1;
        bool selected=false;
        mitk::Point3D point;
    };

    typedef sv4guiSpline::sv4guiSplinePoint sv4guiPathPoint;

    sv4guiPathElement();

    sv4guiPathElement(const sv4guiPathElement &other);

    virtual ~sv4guiPathElement();

    sv4guiPathElement* Clone();

    std::vector<mitk::Point3D> GetControlPoints();

    svControlPoint GetsvControlPoint(int index) ;

    mitk::Point3D GetControlPoint(int index);

    void InsertControlPoint(int index, mitk::Point3D point);

    int GetInsertintIndexByDistance( mitk::Point3D point);

    void SetControlPoint(int index, mitk::Point3D point);

    void SetControlPoints(std::vector<mitk::Point3D> points, bool update = true);

    int SearchControlPoint( mitk::Point3D point, mitk::ScalarType distance);

    sv4guiPathElement* CreateSmoothedPathElement(int sampleRate, int numModes, bool controlPointsBased = true ); //otherwise pathPointsBased

    std::vector<sv4guiPathPoint>  GetPathPoints();

    std::vector<mitk::Point3D> GetPathPosPoints();

    sv4guiPathPoint GetPathPoint(int index) ;

    mitk::Point3D GetPathPosPoint(int index) ;

    void SetPathPoints(std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints);
    
    std::vector<sv4guiPathPoint> GetExtendedPathPoints(double realBounds[6], double minSpacing, int& startingIndex);

protected:

};

#endif // SV4GUI_PATHELEMENT_H
