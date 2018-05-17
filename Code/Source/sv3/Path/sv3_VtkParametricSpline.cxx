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

#include "sv3_VtkParametricSpline.h"
#include "vtkSmartPointer.h"
#include "vtkSpline.h"
#include "vtkPoints.h"

using sv3::VtkParametricSpline;
VtkParametricSpline::VtkParametricSpline()
{
}

VtkParametricSpline::~VtkParametricSpline()
{

}

void VtkParametricSpline::Evaluate(double t, double Pt[3])
{
    if ( this->InitializeTime < this->GetMTime () )
    {
        if ( ! this->Initialize() )
        {
            return;
        }
    }

    if(t<0) t=0;

    if ( this->Closed )
    {
        if(t > Points->GetNumberOfPoints()) t=Points->GetNumberOfPoints();
    }
    else
    {
        if(t > Points->GetNumberOfPoints()-1) t=Points->GetNumberOfPoints()-1;
    }

    if (this->Length == 0 && this->Points && this->Points->GetNumberOfPoints() > 0)
    {
        this->Points->GetPoint(0, Pt);
        return;
    }

    Pt[0] = this->XSpline->Evaluate(t);
    Pt[1] = this->YSpline->Evaluate(t);
    Pt[2] = this->ZSpline->Evaluate(t);
}

void VtkParametricSpline::EvaluateByLengthFactor(double t, double Pt[3])
{
    double U[3]={0};
    U[0]=t;

    vtkParametricSpline::Evaluate(U, Pt, NULL);
}
