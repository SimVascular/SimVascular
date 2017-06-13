#ifndef SVSEG3DUTILS_H
#define SVSEG3DUTILS_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include <vtkTransform.h>
#include <vtkPolyData.h>
#include <vtkImageData.h>
#include <vtkSmartPointer.h>

class SVSEGMENTATION_EXPORT svSeg3DUtils
{

public:

    svSeg3DUtils();

    virtual ~svSeg3DUtils();


    static vtkSmartPointer<vtkPolyData> collidingFronts(vtkImageData* volumeImage,
                                                        std::vector<std::vector<int>>& seeds1,
                                                        std::vector<std::vector<int>>& seeds2,
                                                        int lowerThreshold =0, int upperThreshold=0);

};


#endif /* SVSEG3DUTILS_H */
