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

#include "sv4gui_VtkUtils.h"

#include "sv4gui_Math3.h"

#include <vtkCleanPolyData.h>

#include <iostream>
using namespace std;

vtkSmartPointer<vtkPolyData> sv4guiVtkUtils::MergePoints(vtkSmartPointer<vtkPolyData> inpd, double tol)
{
    vtkSmartPointer<vtkCleanPolyData> merge = vtkSmartPointer<vtkCleanPolyData>::New();
    merge->SetTolerance(tol );
    merge->SetInputDataObject(inpd);
    merge->Update();

    return merge->GetOutput();
}

vtkSmartPointer<vtkPolyData> sv4guiVtkUtils::MergePoints(vtkSmartPointer<vtkPolyData> inpd)
{
    return MergePoints(inpd, sv4guiMath3::GetMachineEpsilon());
}

void sv4guiVtkUtils::ResetMitkImage(mitk::Image* image)
{
    vtkImageData* vtkImg=image->GetVtkImageData();
    mitk::Point3D org = image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetOrigin();
    mitk::BaseGeometry::BoundsArrayType extent=image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetBounds();

    int whole[6];
    double *spacing, origin[3];

    whole[0]=extent[0];
    whole[1]=extent[1]-1;
    whole[2]=extent[2];
    whole[3]=extent[3]-1;
    whole[4]=extent[4];
    whole[5]=extent[5]-1;

    spacing = vtkImg->GetSpacing();

    origin[0] = spacing[0] * whole[0] +org[0];
    origin[1] = spacing[1] * whole[2] +org[1];
    whole[1] -= whole[0];
    whole[3] -= whole[2];
    whole[0] = 0;
    whole[2] = 0;
    origin[2] = spacing[2] * whole[4]+org[2];
    whole[5] -= whole[4];
    whole[4] = 0;

    vtkImg->SetExtent(whole);
    vtkImg->SetOrigin(origin);
}

vtkImageData* sv4guiVtkUtils::MitkImage2VtkImage(mitk::Image* image)
{
    vtkImageData* vtkImg=image->GetVtkImageData();
    mitk::Point3D org = image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetOrigin();
    mitk::BaseGeometry::BoundsArrayType extent=image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetBounds();

    vtkImageData* newVtkImg = vtkImageData::New();
    newVtkImg->ShallowCopy(vtkImg);

    int whole[6];
    double *spacing, origin[3];

    whole[0]=extent[0];
    whole[1]=extent[1]-1;
    whole[2]=extent[2];
    whole[3]=extent[3]-1;
    whole[4]=extent[4];
    whole[5]=extent[5]-1;

    spacing = vtkImg->GetSpacing();

    origin[0] = spacing[0] * whole[0] +org[0];
    origin[1] = spacing[1] * whole[2] +org[1];
    whole[1] -= whole[0];
    whole[3] -= whole[2];
    whole[0] = 0;
    whole[2] = 0;
    origin[2] = spacing[2] * whole[4]+org[2];
    whole[5] -= whole[4];
    whole[4] = 0;

    newVtkImg->SetExtent(whole);
    newVtkImg->SetOrigin(origin);
    newVtkImg->SetSpacing(spacing);

    return newVtkImg;
}
