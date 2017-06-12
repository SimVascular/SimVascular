#ifndef SVSEGMENTATION3D_H
#define SVSEGMENTATION3D_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include <vtkTransform.h>
#include <vtkPolyData.h>
#include <vtkImageData.h>
#include <vtkSmartPointer.h>
class SVSEGMENTATION_EXPORT svSegmentation3D
{

public:

    svSegmentation3D();

    virtual ~svSegmentation3D();


    static vtkSmartPointer<vtkPolyData> collidingFronts(vtkImageData* volumeImage,
  std::vector<std::vector<int>>& seeds1,
  std::vector<std::vector<int>>& seeds2,
  int lowerThreshold =0, int upperThreshold=0);

};


#endif /* SVSEGMENTATION3D_H */
