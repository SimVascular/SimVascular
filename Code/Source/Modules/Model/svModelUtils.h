#ifndef SVMODELUTILS_H
#define SVMODELUTILS_H

#include <svModelExports.h>

#include "svContour.h"
#include "svContourGroup.h"
#include "svModelElement.h"

#include <mitkDataStorage.h>

class SVMODEL_EXPORT svModelUtils
{

public:

    struct svBlendParam
    {
        int numblenditers;
        int numsubblenditers;
        int numsubdivisioniters;
        int numcgsmoothiters;
        int numlapsmoothiters;
        double targetdecimation;

        svBlendParam()
            : numblenditers(2)
            , numsubblenditers(3)
            , numsubdivisioniters(1)
            , numcgsmoothiters(2)
            , numlapsmoothiters(50)
            , targetdecimation(0.01)
        {
        }

    };

    static vtkPolyData* CreateSolidModelPolyData(std::vector<svContourGroup*> groups, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6);

    static svModelElement* CreateSolidModelElement(std::vector<mitk::DataNode::Pointer> segNodes, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6);

    static vtkPolyData* CreateSolidModelPolyDataByBlend(vtkPolyData* vpdsrc, int faceID1, int faceID2, double radius, svBlendParam* param);

    static vtkPolyData* CreateLoftSurface(svContourGroup* contourGroup, int addCaps, unsigned int t = 0,  svContourGroup::svLoftingParam* param = NULL);

    static vtkPolyData* CreateLoftSurface(std::vector<svContour*> contourSet, svContourGroup::svLoftingParam* param, int addCaps);

    static vtkPolyData* CreateOrientOpenPolySolidVessel(vtkPolyData* inpd);

    static vtkPolyData* FillHoles(vtkPolyData* inpd);

    static vtkPolyData* Orient(vtkPolyData* inpd);

    static vtkPolyData* CreateOrientClosedPolySolidVessel(vtkPolyData* inpd);

    static vtkPolyData* FillHolesWithIDs(vtkPolyData* inpd, int fillID, int fillType);

};


#endif /* SVMODELUTILS_H */
