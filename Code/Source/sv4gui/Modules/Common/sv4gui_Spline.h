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

#ifndef SV4GUI_SPLINE_H
#define SV4GUI_SPLINE_H

#include "SimVascular.h"

#include "sv4guiModuleCommonExports.h"

#include <mitkPoint.h>
#include <mitkVector.h>

class sv4guiVtkParametricSpline;

class SV4GUIMODULECOMMON_EXPORT sv4guiSpline
{
public:

    enum CalculationMethod {CONSTANT_TOTAL_NUMBER, CONSTANT_SUBDIVISION_NUMBER, CONSTANT_SPACING};

    struct sv4guiSplinePoint
    {
        int id;
        mitk::Point3D pos;
        mitk::Vector3D tangent;
        mitk::Vector3D rotation;
    };

    sv4guiSpline();

    sv4guiSpline(bool closed, CalculationMethod method, int  furtherSubdivionNumber = 10);

    virtual ~sv4guiSpline();

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

    std::vector<sv4guiSplinePoint> GetSplinePoints();

    std::vector<mitk::Point3D> GetSplinePosPoints();

    void Update();

    double GetLength(sv4guiVtkParametricSpline* svpp, double idx1, double idx2);

    mitk::Point3D GetPoint(sv4guiVtkParametricSpline* svpp, double idx);

protected:

    bool m_Closed;

    double m_Spacing;

    CalculationMethod m_Method;

    int m_CalculationNumber;

    int m_FurtherSubdivisionNumber; //for tangent calculation;

    std::vector<mitk::Point3D> m_InputPoints;

    std::vector<sv4guiSplinePoint> m_SplinePoints;

};

#endif // SV4GUI_SPLINE_H
