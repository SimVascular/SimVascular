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

#ifndef SV4GUI_MODELELEMENTPOLYDATA_H
#define SV4GUI_MODELELEMENTPOLYDATA_H

#include <sv4guiModuleModelExports.h>

#include "sv4gui_ModelElement.h"

#include <mitkDataNode.h>

#include <vtkSmartPointer.h>
#include <vtkPlanes.h>

class SV4GUIMODULEMODEL_EXPORT sv4guiModelElementPolyData : public sv4guiModelElement
{
public:

    sv4guiModelElementPolyData();

    sv4guiModelElementPolyData(const sv4guiModelElementPolyData &other);

    virtual ~sv4guiModelElementPolyData();

    virtual sv4guiModelElementPolyData* Clone() override;

    virtual vtkSmartPointer<vtkPolyData> CreateFaceVtkPolyData(int id) override;

    virtual vtkSmartPointer<vtkPolyData> CreateWholeVtkPolyData() override;

    virtual std::vector<int> GetFaceIDsFromInnerSolid() override;

    bool DeleteFaces(std::vector<int> faceIDs);

    bool CombineFaces(std::vector<int> faceIDs);

    bool RemeshFaces(std::vector<int> faceIDs, double size);

    bool FillHolesWithIDs();

    bool ExtractFaces(double angle);

    bool FillHoles();

    bool SelectLargestConnectedRegion();

    bool RemeshG(double hmax, double hmin);

    bool Decimate(double targetRate);

    bool LaplacianSmooth(int numIters, double relaxFactor);

    bool ButterflySubdivide(int numDivs);

    bool WindowSincSmooth(int numIters, double band);

    bool Densify(int numDivs);

    std::vector<int> GetSelectedCellIDs();

    void ClearCellSelection();

    bool SelectCell(int cellID, bool select=true);

    bool DeleteCells(std::vector<int> cellIDs);

    bool MarkCells(std::vector<int> cellIDs);

    bool MarkCellsBySphere(double radius, double center[3]);

    bool MarkCellsByFaces(std::vector<int> faceIDs);

    bool MarkCellsByFaceJunctions(std::vector<int> faceIDs, double radius);

    bool DecimateLocal(double targetRate);//mark cells befor calling it

    bool LaplacianSmoothLocal(int numIters, double relaxFactor);//mark cells befor calling it

    bool ConstrainSmoothLocal(int numIters, double constrainFactor, int numCGSolves = 30);//mark cells befor calling it

    bool LinearSubdivideLocal(int numDivs);//mark cells befor calling it

    bool LoopSubdivideLocal(int numDivs);//mark cells befor calling it

    bool CutByPlane(double origin[3], double point1[3], double point2[3], bool above );

    bool CutByBox(vtkSmartPointer<vtkPlanes> boxPlanes, bool inside);

    void RemoveActiveCells();

    static sv4guiModelElement* CreateModelElement();

    virtual sv4guiModelElement* CreateModelElement(std::vector<mitk::DataNode::Pointer> segNodes
                                    , int numSamplingPts
                                    , svLoftingParam *param
                                    , int* stats = NULL
                                    , double maxDist = 1.0
                                    , int noInterOut = 1
                                    , double tol = 1e-6
                                    , unsigned int t = 0) override;

    virtual sv4guiModelElement* CreateModelElementByBlend(std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii
                                                      , sv4guiModelElement::svBlendParam* param) override;

    virtual bool ReadFile(std::string filePath) override;

    virtual bool WriteFile(std::string filePath) override;

  protected:

    std::vector<int> m_SelectedCellIDs;
  };


#endif // SV4GUI_MODELELEMENTPOLYDATA_H
