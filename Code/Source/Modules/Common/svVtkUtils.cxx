#include "svVtkUtils.h"

#include "svMath3.h"

#include <vtkCleanPolyData.h>

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
