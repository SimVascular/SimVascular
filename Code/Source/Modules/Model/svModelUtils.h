#ifndef SVMODELUTILS_H
#define SVMODELUTILS_H

#include <svModelExports.h>

#include "svContour.h"
#include "svContourGroup.h"
#include "svModelElement.h"
#include "svModelElementPolyData.h"

#include <mitkDataStorage.h>

class SVMODEL_EXPORT svModelUtils
{

public:

    static vtkPolyData* CreatePolyData(std::vector<svContourGroup*> groups, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6);

    static svModelElementPolyData* CreateModelElementPolyData(std::vector<mitk::DataNode::Pointer> segNodes, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6);

    static vtkPolyData* CreatePolyDataByBlend(vtkPolyData* vpdsrc, int faceID1, int faceID2, double radius, svModelElementPolyData::svBlendParam* param);

    static svModelElementPolyData* CreateModelElementPolyDataByBlend(svModelElementPolyData* mepdsrc, std::vector<svModelElement::svBlendParamRadius*> blendRadii, svModelElementPolyData::svBlendParam* param);

    static vtkPolyData* CreateLoftSurface(svContourGroup* contourGroup, int addCaps, unsigned int t = 0,  svContourGroup::svLoftingParam* param = NULL);

    static vtkPolyData* CreateLoftSurface(std::vector<svContour*> contourSet, svContourGroup::svLoftingParam* param, int addCaps);

    static vtkPolyData* CreateOrientOpenPolySolidVessel(vtkPolyData* inpd);

    static vtkPolyData* FillHoles(vtkPolyData* inpd);

    static vtkPolyData* Orient(vtkPolyData* inpd);

    static vtkPolyData* CreateOrientClosedPolySolidVessel(vtkPolyData* inpd);

    static vtkPolyData* FillHolesWithIDs(vtkPolyData* inpd, int fillID, int fillType);

    static bool CheckArrayName(vtkDataSet *object,int datatype,std::string arrayname );

};


#endif /* SVMODELUTILS_H */
