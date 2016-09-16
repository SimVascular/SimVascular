#ifndef SVCONTOURPOLYGON_H
#define SVCONTOURPOLYGON_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContour.h"

class SVSEGMENTATION_EXPORT svContourPolygon : public svContour
{

public:


    svContourPolygon();

    svContourPolygon(const svContourPolygon &other);

    virtual ~svContourPolygon();

    virtual svContourPolygon* Clone() override;

    virtual std::string GetClassName() override;

    virtual void SetControlPoint(int index, mitk::Point3D point) override;

    virtual void CreateContourPoints() override;

    virtual int SearchControlPointByContourPoint( int contourPointIndex ) override;

    void AssignCenterScalingPoints() override;

    void PlaceControlPoints(mitk::Point3D point) override;

  protected:

  };


#endif // SVCONTOURPOLYGON_H
