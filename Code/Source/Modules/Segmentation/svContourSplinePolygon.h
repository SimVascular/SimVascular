#ifndef SVCONTOURSPLINEPOLYGON_H
#define SVCONTOURSPLINEPOLYGON_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContourPolygon.h"

class SVSEGMENTATION_EXPORT svContourSplinePolygon : public svContourPolygon
{

public:

    svContourSplinePolygon();

    svContourSplinePolygon(const svContourSplinePolygon &other);

    virtual ~svContourSplinePolygon();

    virtual svContourSplinePolygon* Clone() override;

    virtual std::string GetClassName() override;

    virtual void CreateContourPoints() override;

    static svContour* CreateByFitting(svContour* contour, int divisionNumber = 20);

  protected:

  };


#endif // SVCONTOURSPLINEPOLYGON_H
