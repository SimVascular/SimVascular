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
 *  \class vtkSVSurfaceCenterlineGrouper
 *  \brief Using a polydata centerlines, separate the polydata into regions
 *  based on the centerlines
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVSurfaceCenterlineGrouper_h
#define vtkSVSurfaceCenterlineGrouper_h

#include "vtkSVSegmentationModule.h" // For export

#include "vtkIdList.h"
#include "vtkMatrix4x4.h"
#include "vtkPolyDataAlgorithm.h"
#include "vtkPolyData.h"
#include "vtkStructuredGrid.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVPolyBallLine.h"
#include "vtkSVGlobals.h"

class VTKSVSEGMENTATION_EXPORT vtkSVSurfaceCenterlineGrouper : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkSVSurfaceCenterlineGrouper,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  static vtkSVSurfaceCenterlineGrouper *New();

  //@{
  /// \brief Get/Set macro for merged centerlines
  vtkSetObjectMacro(MergedCenterlines,vtkPolyData);
  vtkGetObjectMacro(MergedCenterlines,vtkPolyData);
  //@}

  //@{
  /// \brief Get/Set macro for surface polycube
  vtkSetObjectMacro(PolycubePd,vtkPolyData);
  vtkGetObjectMacro(PolycubePd,vtkPolyData);
  //@}

  //@{
  /// \brief Get/Set macro for array name used by the filter. Must
  //  be present on the centerlines.
  vtkSetStringMacro(CenterlineGroupIdsArrayName);
  vtkGetStringMacro(CenterlineGroupIdsArrayName);
  vtkSetStringMacro(CenterlineRadiusArrayName);
  vtkGetStringMacro(CenterlineRadiusArrayName);
  vtkSetStringMacro(GroupIdsArrayName);
  vtkGetStringMacro(GroupIdsArrayName);
  vtkSetStringMacro(BlankingArrayName);
  vtkGetStringMacro(BlankingArrayName);
  vtkSetStringMacro(CenterlineIdsArrayName);
  vtkGetStringMacro(CenterlineIdsArrayName);
  vtkSetStringMacro(TractIdsArrayName);
  vtkGetStringMacro(TractIdsArrayName);
  vtkSetStringMacro(PatchIdsArrayName);
  vtkGetStringMacro(PatchIdsArrayName);
  vtkSetStringMacro(SlicePointsArrayName);
  vtkGetStringMacro(SlicePointsArrayName);
  //@}

  //@{
  /// \brief Get/Set the radius information
  vtkSetMacro(UseRadiusInformation,int);
  vtkGetMacro(UseRadiusInformation,int);
  vtkBooleanMacro(UseRadiusInformation,int);
  //@}

  //@{
  /// \brief Get/Set the radius information
  vtkSetMacro(EnforcePolycubeConnectivity, int);
  vtkGetMacro(EnforcePolycubeConnectivity, int);
  vtkBooleanMacro(EnforcePolycubeConnectivity, int);
  //@}

  //@{
  /// \brief Get/Set the radius information
  vtkSetMacro(EnforceCenterlinesConnectivity, int);
  vtkGetMacro(EnforceCenterlinesConnectivity, int);
  vtkBooleanMacro(EnforceCenterlinesConnectivity, int);
  //@}

  //@{
  /// \brief Get/Set the radius information
  vtkSetMacro(GroupSurface, int);
  vtkGetMacro(GroupSurface, int);
  vtkBooleanMacro(GroupSurface, int);
  //@}

  static int SplitBoundary(vtkPolyData *pd, std::vector<int> boundary,
                    int numDivs, int groupId, std::vector<int> &newSlicePoints, std::string slicePointsArrayName);

protected:
  vtkSVSurfaceCenterlineGrouper();
  ~vtkSVSurfaceCenterlineGrouper();

  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  int MergeCenterlines();
  int CheckPolycubeEnforcePossible();
  int MatchSurfaceToPolycube();
  int CheckSlicePoints();
  int SplitCellsAroundPoint(vtkPolyData *pd, int ptId);

  int SplitEdge(vtkPolyData *pd, int cellId, int ptId0, int ptId1,
                vtkCellArray *newCells, std::vector<std::vector<int> >  &splitCells);
  // TODO Reduce FixMultiple and RemoveMultiple
  int FixMultipleGroups(vtkPolyData *pd, vtkPolyData *polycubePd,
                        std::vector<Region> surfaceGroups,
                        std::vector<Region> polycubeGroups);

  int CheckGroups(vtkPolyData *pd);
  int FixEdges(vtkPolyData *pd, vtkPolyData *origPd, std::string arrayName,
               const Region region, std::vector<int> allEdges,
               std::vector<int> fixEdges, vtkIdList *critPts);
  int FixPlanarTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, std::string arrayName,
                            const Region region, std::vector<int> allEdges,
                            std::vector<int> badEdges, vtkIdList *critPts);
  int FixPerpenTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, std::string arrayName,
                            const Region region, std::vector<int> allEdges,
                            std::vector<int> badEdges, vtkIdList *critPts);
  int FixCornerTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, std::string arrayName,
                            const Region region, std::vector<int> allEdges,
                            std::vector<int> badEdges, vtkIdList *critPts);
  int FixOffsetTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, vtkPolyData *polyPd,
                            std::string arrayName,
                            const Region region, const Region polyRegion,
                            std::vector<int> allEdges,
                            std::vector<int> badEdges, vtkIdList *critPts);
  int FixFilledTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, vtkPolyData *polyPd,
                            std::string arrayName,
                            const Region region, const Region polyRegion,
                            std::vector<int> allEdges,
                            std::vector<int> badEdges, vtkIdList *critPts);
  int FixSplitsTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, vtkPolyData *polyPd,
                            std::string arrayName,
                            const Region region, const Region polyRegion,
                            std::vector<int> allEdges,
                            std::vector<int> badEdges, vtkIdList *critPts);
  int FixCloseGroup(vtkPolyData *pd, vtkPolyData *origPd, vtkPolyData *polyPd,
                    std::string arrayName,
                    const Region region, const Region polyRegion,
                    std::vector<int> allEdges,
                    std::vector<int> badEdges, vtkIdList *critPts);
  int FixGroupsWithPolycube();
  int FixGroupsWithCenterlines(int fixIters);

  int GetConnectedEdges(std::vector<std::vector<int> > inputEdges,
                        std::vector<std::vector<int> > &connectedCornerPts);

  int RemoveNegativeGroups(vtkPolyData *pd, std::string arrayName);
  int RemoveDuplicateGroups(vtkPolyData *pd, std::string arrayName);
  int FixRegions(vtkPolyData *pd, std::string arrayName,
                        std::vector<Region> &allRegions,
                        std::vector<int> badRegions,
                        const int currentValue,
                        const int onlyFixIslands = 0);

  char *CenterlineGroupIdsArrayName;
  char *CenterlineRadiusArrayName;
  char *CenterlineIdsArrayName;
  char *GroupIdsArrayName;
  char *BlankingArrayName;
  char *TractIdsArrayName;
  char *PatchIdsArrayName;
  char *SlicePointsArrayName;

  vtkPolyData *WorkPd;
  vtkPolyData *MergedCenterlines;
  vtkPolyData *PolycubePd;

  int EnforcePolycubeConnectivity;
  int EnforceCenterlinesConnectivity;
  int GroupSurface;
  int UseRadiusInformation;

private:
  vtkSVSurfaceCenterlineGrouper(const vtkSVSurfaceCenterlineGrouper&);  // Not implemented.
  void operator=(const vtkSVSurfaceCenterlineGrouper&);  // Not implemented.
};

#endif
