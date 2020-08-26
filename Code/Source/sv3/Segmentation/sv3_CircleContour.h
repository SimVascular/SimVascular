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

#ifndef SV3_CIRCLECONTOUR_H
#define SV3_CIRCLECONTOUR_H

#include "SimVascular.h"

#include <sv3SegmentationExports.h>

#include "sv3_Contour.h"
#include "sv_RepositoryData.h"
#include "sv3_PathElement.h"

#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkImageData.h"
//#include "vtkPlane.h"

// somehow GetClassName is getting set to GetClassNameA on Windows
#ifdef GetClassName
#undef GetClassName
#endif

namespace sv3{
class SV_EXPORT_SEGMENTATION circleContour : public Contour
{

public:

    
    circleContour();
    
    circleContour(const circleContour &other);
    
    ~circleContour();
    
    virtual circleContour* Clone() override;
    
    virtual std::string GetClassName() override;

    virtual void SetControlPoint(int index, std::array<double,3> point) override;

    void SetControlPointByRadius(double radius, double* point);

    double GetRadius();

    void SetRadius(double radius);
    
    //virtual void CreateContourPoints() override;

    virtual void AssignCenterScalingPoints() override;

    static circleContour* CreateByFitting(Contour* contour);
    
    circleContour* CreateSmoothedContour(int fourierNumber);
    
    virtual void CreateContourPoints() override;

  protected:

  };

}
#endif // SV3_CIRCLECONTOUR_H
