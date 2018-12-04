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

#ifndef vtkSVCenterlines_h
#define vtkSVCenterlines_h

#include "vtkSVSegmentationModule.h" // For exports

#include "vtkIdList.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkPolyDataAlgorithm.h"
#include "vtkUnstructuredGrid.h"

class VTKSVSEGMENTATION_EXPORT vtkSVCenterlines : public vtkPolyDataAlgorithm
{
  public:
  vtkTypeMacro(vtkSVCenterlines,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  static vtkSVCenterlines *New();

  virtual void SetSourceSeedIds(vtkIdList*);
  vtkGetObjectMacro(SourceSeedIds,vtkIdList);

  virtual void SetTargetSeedIds(vtkIdList*);
  vtkGetObjectMacro(TargetSeedIds,vtkIdList);

  virtual void SetCapCenterIds(vtkIdList*);
  vtkGetObjectMacro(CapCenterIds,vtkIdList);

  vtkSetObjectMacro(DelaunayTessellation,vtkUnstructuredGrid);
  vtkGetObjectMacro(DelaunayTessellation,vtkUnstructuredGrid);

  vtkGetObjectMacro(VoronoiDiagram,vtkPolyData);

  vtkGetObjectMacro(PoleIds,vtkIdList);

  vtkGetObjectMacro(RawCenterlines, vtkPolyData);
  vtkSetObjectMacro(RawCenterlines, vtkPolyData);

  vtkSetStringMacro(RadiusArrayName);
  vtkGetStringMacro(RadiusArrayName);

  vtkSetStringMacro(CostFunction);
  vtkGetStringMacro(CostFunction);

  vtkSetStringMacro(EikonalSolutionArrayName);
  vtkGetStringMacro(EikonalSolutionArrayName);

  vtkSetStringMacro(EdgeArrayName);
  vtkGetStringMacro(EdgeArrayName);

  vtkSetStringMacro(EdgePCoordArrayName);
  vtkGetStringMacro(EdgePCoordArrayName);

  vtkSetStringMacro(CostFunctionArrayName);
  vtkGetStringMacro(CostFunctionArrayName);

  vtkSetMacro(FlipNormals,int);
  vtkGetMacro(FlipNormals,int);
  vtkBooleanMacro(FlipNormals,int);

  vtkSetMacro(SimplifyVoronoi,int);
  vtkGetMacro(SimplifyVoronoi,int);
  vtkBooleanMacro(SimplifyVoronoi,int);

  vtkSetMacro(CenterlineResampling,int);
  vtkGetMacro(CenterlineResampling,int);
  vtkBooleanMacro(CenterlineResampling,int);

  vtkSetMacro(ResamplingStepLength,double);
  vtkGetMacro(ResamplingStepLength,double);

  vtkSetMacro(AppendEndPointsToCenterlines,int);
  vtkGetMacro(AppendEndPointsToCenterlines,int);
  vtkBooleanMacro(AppendEndPointsToCenterlines,int);

  vtkSetMacro(GenerateDelaunayTessellation,int);
  vtkGetMacro(GenerateDelaunayTessellation,int);
  vtkBooleanMacro(GenerateDelaunayTessellation,int);

  vtkSetMacro(MedialEdgeThreshold, int);
  vtkGetMacro(MedialEdgeThreshold, int);

  vtkSetMacro(AbsoluteThreshold, int);
  vtkGetMacro(AbsoluteThreshold, int);

  vtkSetMacro(ProcessCenterlinesIntoTree, int);
  vtkGetMacro(ProcessCenterlinesIntoTree, int);

  vtkSetMacro(RelativeThreshold, double);
  vtkGetMacro(RelativeThreshold, double);

  vtkSetMacro(DelaunayTolerance,double);
  vtkGetMacro(DelaunayTolerance,double);

  protected:
  vtkSVCenterlines();
  ~vtkSVCenterlines();

  virtual int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *) override;

  int FindVoronoiSeeds(vtkUnstructuredGrid *delaunay, vtkIdList *boundaryBaricenterIds, vtkDataArray *normals, vtkIdList *seedIds);
  void AppendEndPoints(vtkPoints* endPointPairs);
  void ResampleCenterlines();
  void ReverseCenterlines();
  int PruneVoronoiDiagram(vtkPolyData *inTriPd,
                          vtkPolyData *inEdgePd,
                          vtkPolyData *outTriPd,
                          vtkPolyData *outEdgePd,
                          std::string medialEdgeArrayName);
  int RecursiveGetPolylines(vtkPolyData *pd,
                            std::vector<std::vector<int> > connectedEdgePts,
                            int startVertex, std::vector<int> &pointUsed,
                            std::vector<std::vector<int> > &allEdges,
                            std::vector<int> &thisEdge);
  int RecursiveGetFullCenterlines(std::vector<std::vector<int> > allEdges,
                                  std::vector<std::vector<int> > &fullCenterlineEdges,
                                  int thisEdge, int front, int back);
  int RemoveMarkedCells(vtkPolyData *pd,
                        std::vector<std::vector<int> > allEdges,
                        std::vector<int> needToDelete,
                        std::vector<int> &isDeleted,
                        vtkIdList *allEndIds,
                        std::vector<int> &nodeCount);
  int LoopRemoveMarkedCells(vtkPolyData *pd,
                            std::vector<std::vector<int> > allEdges,
                            std::vector<int> needToDelete,
                            std::vector<int> &isDeleted,
                            vtkIdList *allEndIds,
                            std::vector<int> &nodeCount);
  int GetLinesEndPoints(vtkPolyData *pd,
                        vtkIdList *endPointIds,
                        vtkPoints *endPoints,
                        std::vector<std::vector<int> > &connectedEdgePts,
                        int &firstVertex);

  vtkIdList* SourceSeedIds;
  vtkIdList* TargetSeedIds;

  vtkIdList* CapCenterIds;

  vtkUnstructuredGrid* DelaunayTessellation;

  vtkPolyData* VoronoiDiagram;
  vtkPolyData* RawCenterlines;

  vtkIdList* PoleIds;

  char* RadiusArrayName;
  char* CostFunction;
  char* EikonalSolutionArrayName;
  char* EdgeArrayName;
  char* EdgePCoordArrayName;
  char* CostFunctionArrayName;

  int FlipNormals;
  int SimplifyVoronoi;
  int AppendEndPointsToCenterlines;
  int CenterlineResampling;
  int MedialEdgeThreshold;
  int AbsoluteThreshold;
  int ProcessCenterlinesIntoTree;

  double ResamplingStepLength;
  double RelativeThreshold;

  int GenerateDelaunayTessellation;
  double DelaunayTolerance;

  private:
  vtkSVCenterlines(const vtkSVCenterlines&);  // Not implemented.
  void operator=(const vtkSVCenterlines&);  // Not implemented.
};

#endif
