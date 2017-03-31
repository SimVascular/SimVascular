#ifndef SVMODELUTILS_H
#define SVMODELUTILS_H

#include <svModelExports.h>

#include "simvascular_options.h"

#include "svContour.h"
#include "svContourGroup.h"
#include "svModelElement.h"
#include "svModelElementPolyData.h"

#ifdef SV_USE_OpenCASCADE_QT_GUI
   #include "svModelElementOCCT.h"
#endif

#ifdef SV_USE_PARASOLID_QT_GUI
   #include "svModelElementParasolid.h"
#endif

class SVMODEL_EXPORT svModelUtils
{

public:

    static vtkPolyData* CreatePolyData(std::vector<svContourGroup*> groups, int numSamplingPts, svModelElement::svNURBSLoftParam *nurbsParam, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6);

    static svModelElementPolyData* CreateModelElementPolyData(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, int stats[], svModelElement::svNURBSLoftParam *nurbsParam, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6);

    static vtkPolyData* CreatePolyDataByBlend(vtkPolyData* vpdsrc, int faceID1, int faceID2, double radius, svModelElementPolyData::svBlendParam* param);

    static svModelElementPolyData* CreateModelElementPolyDataByBlend(svModelElementPolyData* mepdsrc, std::vector<svModelElement::svBlendParamRadius*> blendRadii, svModelElementPolyData::svBlendParam* param);

    static vtkPolyData* CreateLoftSurface(svContourGroup* contourGroup, int numSamplingPts,svModelElement::svNURBSLoftParam *nurbsParam, int addCaps, unsigned int t = 0,  svContourGroup::svLoftingParam* param = NULL);

    static vtkPolyData* CreateLoftSurface(std::vector<svContour*> contourSet, int numSamplingPts,svModelElement::svNURBSLoftParam *nurbsParam, svContourGroup::svLoftingParam* param, int addCaps);

    static vtkPolyData* CreateOrientOpenPolySolidVessel(vtkPolyData* inpd);

    static vtkPolyData* FillHoles(vtkPolyData* inpd);

    static vtkPolyData* Orient(vtkPolyData* inpd);

    static vtkPolyData* CreateOrientClosedPolySolidVessel(vtkPolyData* inpd);

    static vtkPolyData* FillHolesWithIDs(vtkPolyData* inpd, int fillID, int fillType);

    static bool CheckArrayName(vtkDataSet *object,int datatype,std::string arrayname );

    static vtkSmartPointer<vtkPolyData> OrientVtkPolyData(vtkSmartPointer<vtkPolyData> inpd);

    static vtkSmartPointer<vtkPolyData> MarkCells(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> cellIDse);

    static vtkSmartPointer<vtkPolyData> MarkCellsBySphere(vtkSmartPointer<vtkPolyData> inpd, double radius, double center[3]);

    static vtkSmartPointer<vtkPolyData> MarkCellsByFaces(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> faceIDs);

    static vtkSmartPointer<vtkPolyData> DecimateLocal(vtkSmartPointer<vtkPolyData> inpd, double targetRate);

    static vtkSmartPointer<vtkPolyData> LaplacianSmoothLocal(vtkSmartPointer<vtkPolyData> inpd, int numIters, double relaxFactor);

    static vtkSmartPointer<vtkPolyData> ConstrainSmoothLocal(vtkSmartPointer<vtkPolyData> inpd, int numIters, double constrainFactor, int numCGSolves = 30);

    static vtkSmartPointer<vtkPolyData> LinearSubdivideLocal(vtkSmartPointer<vtkPolyData> inpd, int numDivs);

    static vtkSmartPointer<vtkPolyData> LoopSubdivideLocal(vtkSmartPointer<vtkPolyData> inpd, int numDivs);

    static vtkSmartPointer<vtkPolyData> CutByPlane(vtkSmartPointer<vtkPolyData> inpd, double origin[3], double point1[3], double point2[3], bool above );

    static vtkSmartPointer<vtkPolyData> CutByBox(vtkSmartPointer<vtkPolyData> inpd, vtkSmartPointer<vtkPlanes> boxPlanes, bool inside);

    static bool DeleteRegions(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> regionIDs);

    static vtkPolyData* CreateCenterlines(svModelElement* modelElement);

    static vtkPolyData* CreateCenterlines(vtkPolyData* vpd);

    static vtkPolyData* CalculateDistanceToCenterlines(vtkPolyData* centerlines, vtkPolyData* original);

    static std::vector<svPathElement::svPathPoint> ConvertToPathPoints(std::vector<mitk::Point3D> posPoints);

    static vtkSmartPointer<vtkPolyData> GetThresholdRegion(vtkSmartPointer<vtkPolyData> pd, vtkDataObject::FieldAssociations dataType, std::string arrayName, double minValue, double maxValue );

    static std::vector<svPathElement*> CreatePathElements(svModelElement* modelElement);

    static double CalculateVpdArea(vtkPolyData* vpd);

#ifdef SV_USE_OpenCASCADE_QT_GUI

    static cvOCCTSolidModel* CreateLoftSurfaceOCCT(std::vector<svContour*> contourSet, std::string groupName, int numSamplingPts, svModelElement::svNURBSLoftParam *nurbsParam, int vecFlag, int addCaps);

    static svModelElementOCCT* CreateModelElementOCCT(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, svModelElement::svNURBSLoftParam *nurbsParam, double maxDist = 20.0, unsigned int t = 0);

    static svModelElementOCCT* CreateModelElementOCCTByBlend(svModelElementOCCT* meocctsrc, std::vector<svModelElement::svBlendParamRadius*> blendRadii);

#endif

#ifdef SV_USE_PARASOLID_QT_GUI

    static cvParasolidSolidModel* CreateLoftSurfaceParasolid(std::vector<svContour*> contourSet, std::string groupName, int numSamplingPts, int vecFlag, int addCaps);

    static svModelElementParasolid* CreateModelElementParasolid(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, double maxDist = 1.0, unsigned int t = 0);

    static svModelElementParasolid* CreateModelElementParasolidByBlend(svModelElementParasolid* mepssrc, std::vector<svModelElement::svBlendParamRadius*> blendRadii);

#endif

};

#endif /* SVMODELUTILS_H */
