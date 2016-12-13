#include "svVtkUtils.h"

#include "svMath3.h"

#include <vtkCleanPolyData.h>

#include <iostream>
using namespace std;

vtkSmartPointer<vtkPolyData> svVtkUtils::MergePoints(vtkSmartPointer<vtkPolyData> inpd, double tol)
{
    vtkSmartPointer<vtkCleanPolyData> merge = vtkSmartPointer<vtkCleanPolyData>::New();
    merge->SetTolerance(tol );
    merge->SetInputDataObject(inpd);
    merge->Update();

    return merge->GetOutput();
}

vtkSmartPointer<vtkPolyData> svVtkUtils::MergePoints(vtkSmartPointer<vtkPolyData> inpd)
{
    return MergePoints(inpd, svMath3::GetMachineEpsilon());
}

void svVtkUtils::ResetMitkImage(mitk::Image* image)
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

vtkImageData* svVtkUtils::MitkImage2VtkImage(mitk::Image* image)
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
