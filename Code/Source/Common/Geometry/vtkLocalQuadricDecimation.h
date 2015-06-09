/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
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
 *
 *=========================================================================*/

/** @file vtkLocalQuadricDecimation.cxx
 *  @brief This decimates a mesh based on given desired points or cells
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#ifndef vtkLocalQuadricDecimation_h
#define vtkLocalQuadricDecimation_h

#include "vtkFiltersCoreModule.h" // For export macro
#include "vtkPolyDataAlgorithm.h"

class vtkEdgeTable;
class vtkIdList;
class vtkPointData;
class vtkPriorityQueue;
class vtkDoubleArray;

class VTKFILTERSCORE_EXPORT vtkLocalQuadricDecimation : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkLocalQuadricDecimation, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);
  static vtkLocalQuadricDecimation *New();

  // Description:
  // Set/Get the desired reduction (expressed as a fraction of the original
  // number of triangles). The actual reduction may be less depending on
  // triangulation and topological constraints.
  vtkSetClampMacro(TargetReduction, double, 0.0, 1.0);
  vtkGetMacro(TargetReduction, double);

  // Description:
  // Decide whether to include data attributes in the error metric. If off,
  // then only geometric error is used to control the decimation. By default
  // the attribute errors are off.
  vtkSetMacro(AttributeErrorMetric, int);
  vtkGetMacro(AttributeErrorMetric, int);
  vtkBooleanMacro(AttributeErrorMetric, int);

  // Description:
  // If attribute errors are to be included in the metric (i.e.,
  // AttributeErrorMetric is on), then the following flags control which
  // attributes are to be included in the error calculation. By default all
  // of these are on.
  vtkSetMacro(ScalarsAttribute, int);
  vtkGetMacro(ScalarsAttribute, int);
  vtkBooleanMacro(ScalarsAttribute, int);
  vtkSetMacro(VectorsAttribute, int);
  vtkGetMacro(VectorsAttribute, int);
  vtkBooleanMacro(VectorsAttribute, int);
  vtkSetMacro(NormalsAttribute, int);
  vtkGetMacro(NormalsAttribute, int);
  vtkBooleanMacro(NormalsAttribute, int);
  vtkSetMacro(TCoordsAttribute, int);
  vtkGetMacro(TCoordsAttribute, int);
  vtkBooleanMacro(TCoordsAttribute, int);
  vtkSetMacro(TensorsAttribute, int);
  vtkGetMacro(TensorsAttribute, int);
  vtkBooleanMacro(TensorsAttribute, int);

  // Description:
  // Set/Get the scaling weight contribution of the attribute. These
  // values are used to weight the contribution of the attributes
  // towards the error metric.
  vtkSetMacro(ScalarsWeight, double);
  vtkSetMacro(VectorsWeight, double);
  vtkSetMacro(NormalsWeight, double);
  vtkSetMacro(TCoordsWeight, double);
  vtkSetMacro(TensorsWeight, double);
  vtkGetMacro(ScalarsWeight, double);
  vtkGetMacro(VectorsWeight, double);
  vtkGetMacro(NormalsWeight, double);
  vtkGetMacro(TCoordsWeight, double);
  vtkGetMacro(TensorsWeight, double);

  vtkSetStringMacro(DecimateCellArrayName);
  vtkGetStringMacro(DecimateCellArrayName);
  vtkSetStringMacro(DecimatePointArrayName);
  vtkGetStringMacro(DecimatePointArrayName);

  // Description:
  // Turn on/off the use of point array for constraint local operation.
  // If value in array equals 1, nodes will be decimated
  vtkSetMacro(UsePointArray,int);
  vtkGetMacro(UsePointArray,int);
  vtkBooleanMacro(UsePointArray,int);

  // Description:
  // Turn on/off the use of cell array for constraint on local operation.
  // If value in array equals 1, nodes of cell will be decimated
  vtkSetMacro(UseCellArray,int);
  vtkGetMacro(UseCellArray,int);
  vtkBooleanMacro(UseCellArray,int);

  // Description:
  // Get the actual reduction. This value is only valid after the
  // filter has executed.
  vtkGetMacro(ActualReduction, double);

protected:
  vtkLocalQuadricDecimation();
  ~vtkLocalQuadricDecimation();

  int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *);

  // Description:
  // Do the dirty work of eliminating the edge; return the number of
  // triangles deleted.
  int CollapseEdge(vtkIdType pt0Id, vtkIdType pt1Id);

  // Description:
  // Compute quadric for all vertices
  void InitializeQuadrics(vtkIdType numPts);

  // Description:
  // Free boundary edges are weighted
  void AddBoundaryConstraints(void);

  // Description:
  // Compute quadric for this vertex.
  void ComputeQuadric(vtkIdType pointId);

  // Description:
  // Add the quadrics for these 2 points since the edge between them has
  // been collapsed.
  void AddQuadric(vtkIdType oldPtId, vtkIdType newPtId);

  // Description:
  // Compute cost for contracting this edge and the point that gives us this
  // cost.
  double ComputeCost(vtkIdType edgeId, double *x);
  double ComputeCost2(vtkIdType edgeId, double *x);

  // Description:
  // Find all edges that will have an endpoint change ids because of an edge
  // collapse.  p1Id and p2Id are the endpoints of the edge.  p2Id is the
  // pointId being removed.
  void FindAffectedEdges(vtkIdType p1Id, vtkIdType p2Id, vtkIdList *edges);

  // Description:
  // Find a cell that uses this edge.
  vtkIdType GetEdgeCellId(vtkIdType p1Id, vtkIdType p2Id);

  int IsGoodPlacement(vtkIdType pt0Id, vtkIdType pt1Id, const double *x);
  int TrianglePlaneCheck(const double t0[3], const double t1[3],
                         const double t2[3],  const double *x);
  void ComputeNumberOfComponents(void);
  void UpdateEdgeData(vtkIdType ptoId, vtkIdType pt1Id);

  // Description:
  // Helper function to set and get the point and it's attributes as an array
  void SetPointAttributeArray(vtkIdType ptId, const double *x);
  void GetPointAttributeArray(vtkIdType ptId, double *x);

  // Description:
  // Find out how many components there are for each attribute for this
  // poly data.
  void GetAttributeComponents();

  int GetDecimateArrays(vtkPolyData *object,int type);
  int SetFixedPoints(vtkPolyData *object,int numTris);
  void CorrectPointData(vtkPolyData *object);

  double TargetReduction;
  double ActualReduction;
  int   AttributeErrorMetric;

  int ScalarsAttribute;
  int VectorsAttribute;
  int NormalsAttribute;
  int TCoordsAttribute;
  int TensorsAttribute;

  double ScalarsWeight;
  double VectorsWeight;
  double NormalsWeight;
  double TCoordsWeight;
  double TensorsWeight;

  char* DecimateCellArrayName;
  char* DecimatePointArrayName;

  int               NumberOfEdgeCollapses;
  vtkEdgeTable     *Edges;
  vtkIdList        *EndPoint1List;
  vtkIdList        *EndPoint2List;
  vtkPriorityQueue *EdgeCosts;
  vtkDoubleArray   *TargetPoints;
  int               NumberOfComponents;
  vtkPolyData      *Mesh;
  vtkIntArray 	   *DecimateCellArray;
  vtkIntArray 	   *DecimatePointArray;
  int UseCellArray;
  int UsePointArray;

  //BTX
  struct ErrorQuadric
  {
    double *Quadric;
  };
  //ETX

  ErrorQuadric *ErrorQuadrics;
  int           AttributeComponents[6];
  double        AttributeScale[6];

  // Temporary variables for performance
  vtkIdList *CollapseCellIds;
  double *TempX;
  double *TempQuad;
  double *TempB;
  double **TempA;
  double *TempData;

  int *changedPoint;
  int *fixedPoint;

private:
  vtkLocalQuadricDecimation(const vtkLocalQuadricDecimation&);  // Not implemented.
  void operator=(const vtkLocalQuadricDecimation&);  // Not implemented.
};

#endif
