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

#ifndef SV3_SPLINE_H
#define SV3_SPLINE_H

#include "SimVascular.h"

#include "sv3PathExports.h"
#include "sv3_VtkParametricSpline.h"
#include <array>
#include <vector>
namespace sv3 {

class SV_EXPORT_PATH Spline
{
public:

    enum CalculationMethod {CONSTANT_TOTAL_NUMBER, CONSTANT_SUBDIVISION_NUMBER, CONSTANT_SPACING};

    struct SplinePoint
    {
        int id;
        std::array<double,3>  pos;
        std::array<double,3>  tangent;
        std::array<double,3>  rotation;
    };

    Spline();

    Spline(bool closed, CalculationMethod method, int  furtherSubdivionNumber = 10);

    virtual ~Spline();

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

    void SetInputPoints(std::vector<std::array<double,3> > inputPonits);

    std::vector<std::array<double,3> >  GetInputPoints();

    std::vector<SplinePoint> GetSplinePoints();

    std::vector<std::array<double,3> > GetSplinePosPoints();

    void Update();
    
    double GetLength(VtkParametricSpline* svpp, double idx1, double idx2);

    std::array<double,3> GetPoint(VtkParametricSpline* svpp, double idx);

protected:

    bool m_Closed;

    double m_Spacing;

    CalculationMethod m_Method;

    int m_CalculationNumber;

    int m_FurtherSubdivisionNumber; //for tangent calculation;

    std::vector<std::array<double,3> > m_InputPoints;

    std::vector<SplinePoint> m_SplinePoints;

};
}
#endif // SV3_SPLINE_H
