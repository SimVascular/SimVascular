#ifndef SVVTKUTILS_H
#define SVVTKUTILS_H

#include "SimVascular.h"

#include "svCommonExports.h"

#include <vtkSmartPointer.h>
#include <vtkPolyData.h>
#include <vtkImageData.h>
#include <mitkImage.h>

class SVCOMMON_EXPORT svVtkUtils
{
public:

    static vtkSmartPointer<vtkPolyData> MergePoints(vtkSmartPointer<vtkPolyData> inpd);

    static vtkSmartPointer<vtkPolyData> MergePoints(vtkSmartPointer<vtkPolyData> inpd, double tol);

    static vtkImageData* MitkImage2VtkImage(mitk::Image* image);

    static void ResetMitkImage(mitk::Image* image);

};

#endif // SVVTKUTILS_H
