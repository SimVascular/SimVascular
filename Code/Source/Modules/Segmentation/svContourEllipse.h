#ifndef SVCONTOURELLIPSE_H
#define SVCONTOURELLIPSE_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContour.h"

class SVSEGMENTATION_EXPORT svContourEllipse : public svContour
{

public:

    svContourEllipse();

    svContourEllipse(const svContourEllipse &other);

    virtual ~svContourEllipse();

    virtual svContourEllipse* Clone() override;

    virtual std::string GetClassName() override;

    virtual void SetControlPoint(int index, mitk::Point3D point) override;

    virtual void CreateContourPoints() override;

    bool AsCircle();

    void SetAsCircle(bool asCircle);

    void AssignCenterScalingPoints() override;

    void PlaceControlPoints(mitk::Point3D point) override;

  protected:

    bool m_TreatAsCircle;

  };


#endif // SVCONTOURELLIPSE_H
