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
 
#ifndef SV3_SEGMENTATIONUTILS_H
#define SV3_SEGMENTATIONUTILS_H


#include "SimVascular.h"

#include <deque>
#include <sv3SegmentationExports.h>
#include "sv_StrPts.h"
#include "sv3_PathElement.h"
#include <vtkImageData.h>
#include "vtkSmartPointer.h"
#include <vtkPlane.h>

namespace sv3 {

class SV_EXPORT_SEGMENTATION SegmentationUtils
{
  public:
    static cvStrPts* vtkImageData2cvStrPts(vtkImageData* vtkImg);
    
    static std::deque<int> GetOrderedPtIDs(vtkCellArray* lines, bool& ifClosed);
    
    static vtkTransform* GetvtkTransform(sv3::PathElement::PathPoint pathPoint);
    
    static vtkImageData* GetSlicevtkImage(sv3::PathElement::PathPoint pathPoint, vtkImageData* volumeimage, double size);
    
    static vtkPlane* CreatePlaneGeometry(PathElement::PathPoint pathPoint, std::array<double,3> spacing, double size);
    
    static void getOrthogonalVector(double normal[3], double vec[3]);

    static double math_angleBtw3DVectors(double vecA[3], double vecB[3]);

    static double math_radToDeg(double rad);

    static double math_dot(double vecA[3], double vecB[3]);

    static void math_cross(double cross[3], double vecA[3], double vecB[3]);

    static double math_magnitude(double vecA[3]);

};

}
#endif // SV3_SEGMENTATIONUTILS_H
