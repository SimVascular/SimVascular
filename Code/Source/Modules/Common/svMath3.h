#ifndef SVMATH3_H
#define SVMATH3_H

#include "SimVascular.h"

#include "svCommonExports.h"

#include <mitkPoint.h>
#include <mitkVector.h>
#include <mitkPlaneGeometry.h>

class SVCOMMON_EXPORT svMath3
{
public:

    static std::vector<mitk::Point3D> CreateSmoothedCurve(std::vector<mitk::Point3D> points, bool closed, int numModes = 12, int sampleRate = 1, int outputNumPts = 0);

    static int GetInsertintIndexByDistance( std::vector<mitk::Point3D> points, mitk::Point3D point, bool insertOnlyIfDifferent = true, bool useDistanceSum = true);

    static int GetInsertintIndexByDistanceSum( std::vector<mitk::Point3D> points, mitk::Point3D point, bool insertOnlyIfDifferent = true);

    static int GetInsertintIndexByProjectedDistance( std::vector<mitk::Point3D> points, mitk::Point3D point, bool insertOnlyIfDifferent = true);

    static bool InsideBounds(mitk::Point3D point, double bounds[6]);

    static bool GetIntersectionPoint(mitk::PlaneGeometry* plane, mitk::Point3D point, mitk::Vector3D direction,mitk::Point3D& interPoint);

    static double GetMachineEpsilon();

    static mitk::Vector3D GetPerpendicularNormalVector(mitk::Vector3D vec);
};

#endif // SVMATH3_H
