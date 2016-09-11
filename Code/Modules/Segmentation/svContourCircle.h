#ifndef SVCONTOURCIRCLE_H
#define SVCONTOURCIRCLE_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContour.h"

class SVSEGMENTATION_EXPORT svContourCircle : public svContour
{

public:

    svContourCircle();

    svContourCircle(const svContourCircle &other);

    virtual ~svContourCircle();

    virtual svContourCircle* Clone() override;

    virtual std::string GetClassName() override;

    virtual void SetControlPoint(int index, mitk::Point3D point) override;

    virtual void CreateContourPoints() override;

    void AssignCenterScalingPoints() override;

  protected:

  };


#endif // SVCONTOURCIRCLE_H
