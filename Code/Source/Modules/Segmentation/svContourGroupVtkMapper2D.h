#ifndef SVCONTOURGROUPVTKMAPPER2D_H
#define SVCONTOURGROUPVTKMAPPER2D_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContourModelVtkMapper2D.h"

class SVSEGMENTATION_EXPORT svContourGroupVtkMapper2D : public svContourModelVtkMapper2D
{
public:

    mitkClassMacro(svContourGroupVtkMapper2D, svContourModelVtkMapper2D);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

protected:

    svContourGroupVtkMapper2D();

    virtual ~svContourGroupVtkMapper2D();

    virtual void FindContourOnCurrentSlice(mitk::BaseRenderer* renderer, unsigned int t = 0) override;
};

#endif // SVCONTOURGROUPVTKMAPPER2D_H
