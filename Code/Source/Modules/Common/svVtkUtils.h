#ifndef SVVTKUTILS_H
#define SVVTKUTILS_H

#include "SimVascular.h"

#include "svCommonExports.h"

#include <vtkSmartPointer.h>
#include <vtkPolyData.h>

class SVCOMMON_EXPORT svVtkUtils
{
public:

    static vtkSmartPointer<vtkPolyData> MergePoints(vtkSmartPointer<vtkPolyData> inpd);

    static vtkSmartPointer<vtkPolyData> MergePoints(vtkSmartPointer<vtkPolyData> inpd, double tol);

};

#endif // SVVTKUTILS_H
