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

/**
  * \class vtkSVIntegrateAttributes - Integrates lines, surfaces and volume.
  * \section
  * Integrates all point and cell data attributes while computing
  * length, area or volume.  Works for 1D, 2D or 3D.  Only one dimensionality
  * at a time.  For volume, this filter ignores all but 3D cells.  It
  * will not compute the volume contained in a closed surface.
  * The output of this filter is a single point and vertex.  The attributes
  * for this point and cell will contain the integration results
  * for the corresponding input attributes.
  */

#ifndef vtkSVIntegrateAttributes_h
#define vtkSVIntegrateAttributes_h

#include "vtkSVMiscModule.h" // for exports

#include "vtkDataSet.h"
#include "vtkIdList.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkDataSetAttributes.h"
#include "vtkMultiProcessController.h"
#include "vtkUnstructuredGridAlgorithm.h"

class VTKSVMISC_EXPORT vtkSVIntegrateAttributes : public vtkUnstructuredGridAlgorithm
{
public:
  vtkTypeMacro(vtkSVIntegrateAttributes,vtkUnstructuredGridAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;
  static vtkSVIntegrateAttributes *New();

  void SetController(vtkMultiProcessController *controller);

//BTX
protected:
  vtkSVIntegrateAttributes();
  ~vtkSVIntegrateAttributes();

  vtkMultiProcessController* Controller;

  virtual int RequestData(vtkInformation* request,
                          vtkInformationVector** inputVector,
                          vtkInformationVector* outputVector) override;

  // Create a default executive.
  virtual vtkExecutive* CreateDefaultExecutive() override;

  virtual int FillInputPortInformation(int, vtkInformation*) override;


  int CompareIntegrationDimension(vtkDataSet* output, int dim);
  int IntegrationDimension;

  // The length, area or volume of the data set.  Computed by Execute;
  double Sum;
  // ToCompute the location of the output point.
  double SumCenter[3];

  void IntegratePolyLine(vtkDataSet* input,
                         vtkUnstructuredGrid* output,
                         vtkIdType cellId, vtkIdList* cellPtIds);
  void IntegratePolygon(vtkDataSet* input,
                         vtkUnstructuredGrid* output,
                         vtkIdType cellId, vtkIdList* cellPtIds);
  void IntegrateTriangleStrip(vtkDataSet* input,
                         vtkUnstructuredGrid* output,
                         vtkIdType cellId, vtkIdList* cellPtIds);
  void IntegrateTriangle(vtkDataSet* input,
                         vtkUnstructuredGrid* output,
                         vtkIdType cellId, vtkIdType pt1Id,
                         vtkIdType pt2Id, vtkIdType pt3Id);
  void IntegrateTetrahedron(vtkDataSet* input,
                            vtkUnstructuredGrid* output,
                            vtkIdType cellId, vtkIdType pt1Id,
                            vtkIdType pt2Id, vtkIdType pt3Id,
                            vtkIdType pt4Id);
  void IntegratePixel(vtkDataSet* input,
                      vtkUnstructuredGrid* output,
                      vtkIdType cellId, vtkIdList* cellPtIds);
  void IntegrateVoxel(vtkDataSet* input,
                      vtkUnstructuredGrid* output,
                      vtkIdType cellId, vtkIdList* cellPtIds);
  void IntegrateGeneral1DCell(vtkDataSet* input,
                              vtkUnstructuredGrid* output,
                              vtkIdType cellId,
                              vtkIdList* cellPtIds);
  void IntegrateGeneral2DCell(vtkDataSet* input,
                              vtkUnstructuredGrid* output,
                              vtkIdType cellId,
                              vtkIdList* cellPtIds);
  void IntegrateGeneral3DCell(vtkDataSet* input,
                              vtkUnstructuredGrid* output,
                              vtkIdType cellId,
                              vtkIdList* cellPtIds);
  void IntegrateSatelliteData(vtkDataSetAttributes* inda,
                              vtkDataSetAttributes* outda);
  void ZeroAttributes(vtkDataSetAttributes* outda);

private:
  vtkSVIntegrateAttributes(const vtkSVIntegrateAttributes&);  // Not implemented.
  void operator=(const vtkSVIntegrateAttributes&);  // Not implemented.

  class vtkFieldList;
  vtkFieldList* CellFieldList;
  vtkFieldList* PointFieldList;
  int FieldListIndex;

  void AllocateAttributes(
    vtkFieldList& fieldList, vtkDataSetAttributes* outda);
  void ExecuteBlock(vtkDataSet* input, vtkUnstructuredGrid* output,
    int fieldset_index, vtkFieldList& pdList, vtkFieldList& cdList);

  void IntegrateData1(vtkDataSetAttributes* inda,
                      vtkDataSetAttributes* outda,
                      vtkIdType pt1Id, double k,
                      vtkFieldList& fieldlist,
                      int fieldlist_index);
  void IntegrateData2(vtkDataSetAttributes* inda,
                      vtkDataSetAttributes* outda,
                      vtkIdType pt1Id, vtkIdType pt2Id, double k,
                      vtkFieldList& fieldlist,
                      int fieldlist_index);
  void IntegrateData3(vtkDataSetAttributes* inda,
                      vtkDataSetAttributes* outda, vtkIdType pt1Id,
                      vtkIdType pt2Id, vtkIdType pt3Id, double k,
                      vtkFieldList& fieldlist,
                      int fieldlist_index);
  void IntegrateData4(vtkDataSetAttributes* inda,
                      vtkDataSetAttributes* outda, vtkIdType pt1Id,
                      vtkIdType pt2Id, vtkIdType pt3Id, vtkIdType pt4Id,
                      double k,
                      vtkFieldList& fieldlist,
                      int fieldlist_index);
public:
  enum CommunicationIds
   {
     IntegrateAttrInfo=2000,
     IntegrateAttrData
   };
//ETX
};

#endif
