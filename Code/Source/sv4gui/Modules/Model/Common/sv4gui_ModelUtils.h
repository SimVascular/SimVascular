/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SV4GUI_MODELUTILS_H
#define SV4GUI_MODELUTILS_H

#include <sv4guiModuleModelExports.h>

#include "sv4gui_Contour.h"
#include "sv4gui_ContourGroup.h"
#include "sv4gui_ModelElement.h"
#include "sv4gui_ModelElementPolyData.h"

class SV4GUIMODULEMODEL_EXPORT sv4guiModelUtils
{

public:

    static vtkPolyData* CreatePolyData(std::vector<sv4guiContourGroup*> groups, std::vector<vtkPolyData*> vtps, int numSamplingPts, svLoftingParam *param, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6);

    static sv4guiModelElementPolyData* CreateModelElementPolyData(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, int stats[], svLoftingParam *param, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6);

    static vtkPolyData* CreatePolyDataByBlend(vtkPolyData* vpdsrc, int faceID1, int faceID2, double radius, sv4guiModelElement::svBlendParam* param);

    static sv4guiModelElementPolyData* CreateModelElementPolyDataByBlend(sv4guiModelElementPolyData* mepdsrc, std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii, sv4guiModelElement::svBlendParam* param);

    static vtkPolyData* CreateLoftSurface(sv4guiContourGroup* contourGroup, int numSamplingPts, int addCaps, svLoftingParam* param = NULL, unsigned int t = 0);

    static vtkPolyData* CreateLoftSurface(std::vector<sv4guiContour*> contourSet, int numSamplingPts, svLoftingParam* param, int addCaps);

    static vtkPolyData* CreateOrientOpenPolySolidVessel(vtkPolyData* inpd);

    static vtkPolyData* FillHoles(vtkPolyData* inpd);

    static vtkPolyData* Orient(vtkPolyData* inpd);

    static vtkPolyData* CreateOrientClosedPolySolidVessel(vtkPolyData* inpd);

    static vtkPolyData* FillHolesWithIDs(vtkPolyData* inpd, int fillID, int fillType);

    static bool CheckArrayName(vtkDataSet *object,int datatype,std::string arrayname );

    static vtkSmartPointer<vtkPolyData> OrientVtkPolyData(vtkSmartPointer<vtkPolyData> inpd);

    static vtkSmartPointer<vtkPolyData> MarkCells(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> cellIDs);

    static vtkSmartPointer<vtkPolyData> MarkCellsBySphere(vtkSmartPointer<vtkPolyData> inpd, double radius, double center[3]);

    static vtkSmartPointer<vtkPolyData> MarkCellsByFaces(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> faceIDs);

    static vtkSmartPointer<vtkPolyData> MarkCellsByFaceJunctions(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> faceIDs, double radius);

    static vtkSmartPointer<vtkPolyData> DecimateLocal(vtkSmartPointer<vtkPolyData> inpd, double targetRate);

    static vtkSmartPointer<vtkPolyData> LaplacianSmoothLocal(vtkSmartPointer<vtkPolyData> inpd, int numIters, double relaxFactor);

    static vtkSmartPointer<vtkPolyData> ConstrainSmoothLocal(vtkSmartPointer<vtkPolyData> inpd, int numIters, double constrainFactor, int numCGSolves = 30);

    static vtkSmartPointer<vtkPolyData> LinearSubdivideLocal(vtkSmartPointer<vtkPolyData> inpd, int numDivs);

    static vtkSmartPointer<vtkPolyData> LoopSubdivideLocal(vtkSmartPointer<vtkPolyData> inpd, int numDivs);

    static vtkSmartPointer<vtkPolyData> CutByPlane(vtkSmartPointer<vtkPolyData> inpd, double origin[3], double point1[3], double point2[3], bool above );

    static vtkSmartPointer<vtkPolyData> CutByBox(vtkSmartPointer<vtkPolyData> inpd, vtkSmartPointer<vtkPlanes> boxPlanes, bool inside);

    static bool DeleteRegions(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> regionIDs);

    static vtkPolyData* CreateCenterlines(sv4guiModelElement* modelElement, vtkIdList *sourceCapIds = NULL, bool getSections = false);

    static vtkPolyData* CreateCenterlines(vtkPolyData* inpd);

    static vtkPolyData* CreateCenterlines(vtkPolyData* inpd,
                                          vtkIdList *sourcePtIds,
                                          vtkIdList *targetPtIds);

    static vtkPolyData* CreateCenterlineSections(vtkPolyData* inpd,
                                                 vtkIdList *sourcePtIds,
                                                 vtkIdList *targetPtIds);

    static vtkPolyData* MergeCenterlines(vtkPolyData* centerlinesPD);

    static vtkPolyData* CalculateDistanceToCenterlines(vtkPolyData* centerlines, vtkPolyData* original);

    static std::vector<sv4guiPathElement::sv4guiPathPoint> ConvertToPathPoints(std::vector<mitk::Point3D> posPoints);

    static vtkSmartPointer<vtkPolyData> GetThresholdRegion(vtkSmartPointer<vtkPolyData> pd, vtkDataObject::FieldAssociations dataType, std::string arrayName, double minValue, double maxValue );

    static std::vector<sv4guiPathElement*> CreatePathElements(sv4guiModelElement* modelElement,
                                                          vtkSmartPointer<vtkPolyData> centerlinesPD);

    static double CalculateVpdArea(vtkPolyData* vpd);

    static bool CheckPolyDataSurface(vtkPolyData* pd, std::string &msg);

    static bool TriangulateSurface(vtkPolyData* pd);
};

#endif /* SV4GUI_MODELUTILS_H */
