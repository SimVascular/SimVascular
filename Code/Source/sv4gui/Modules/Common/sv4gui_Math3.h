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

#ifndef SV4GUI_MATH3_H
#define SV4GUI_MATH3_H

#include "SimVascular.h"

#include "sv4guiModuleCommonExports.h"

#include <mitkPoint.h>
#include <mitkVector.h>
#include <mitkPlaneGeometry.h>

class SV4GUIMODULECOMMON_EXPORT sv4guiMath3
{
public:

    static std::vector<mitk::Point3D> CreateSmoothedCurve(std::vector<mitk::Point3D> points, bool closed, int numModes = 12, int sampleRate = 1, int outputNumPts = 0);

    static int GetInsertintIndexByDistance( std::vector<mitk::Point3D> points, mitk::Point3D point, bool insertOnlyIfDifferent = true, bool useDistanceSum = true);

    static int GetInsertintIndexByDistanceSum( std::vector<mitk::Point3D> points, mitk::Point3D point, bool insertOnlyIfDifferent = true);

    static int GetInsertintIndexByProjectedDistance( std::vector<mitk::Point3D> points, mitk::Point3D point, bool insertOnlyIfDifferent = true);

    static bool InsideBounds(mitk::Point3D point, double bounds[6]);

    static bool GetIntersectionPoint(mitk::PlaneGeometry* plane, mitk::Point3D point, mitk::Vector3D direction,mitk::Point3D& interPoint);

    static double GetMachineEpsilon();

    static mitk::Vector3D GetPerpendicularNormalVector(mitk::Vector3D vec);
};

#endif // SV4GUI_MATH3_H
