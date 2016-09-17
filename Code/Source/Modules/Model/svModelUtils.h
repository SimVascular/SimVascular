#ifndef SVMODELUTILS_H
#define SVMODELUTILS_H

#include "SimVascular.h"

#include <svModelExports.h>

#include "svContour.h"
#include "svContourGroup.h"
#include "svModelElement.h"

#include <mitkDataStorage.h>

class SVMODEL_EXPORT svModelUtils
{

public:

    static vtkPolyData* CreateSolidModelPolyData(std::vector<svContourGroup*> groups, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6);

    static svModelElement* CreateSolidModelElement(std::vector<mitk::DataNode::Pointer> segNodes, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6);

    static vtkPolyData* CreateLoftSurface(svContourGroup* contourGroup, int addCaps, unsigned int t = 0,  svContourGroup::svLoftingParam* param = NULL);

    static vtkPolyData* CreateLoftSurface(std::vector<svContour*> contourSet, svContourGroup::svLoftingParam* param, int addCaps);

    static vtkPolyData* CreateOrientOpenPolySolidVessel(vtkPolyData* inpd);

    static vtkPolyData* FillHoles(vtkPolyData* inpd);

    static vtkPolyData* Orient(vtkPolyData* inpd);

    static vtkPolyData* CreateOrientClosedPolySolidVessel(vtkPolyData* inpd);

    static vtkPolyData* FillHolesWithIDs(vtkPolyData* inpd, int fillID, int fillType);

};


#endif /* SVMODELUTILS_H */
