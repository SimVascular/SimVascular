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
 *  \class vtkSVSurfaceCenterlineAttributesPasser
 *  \brief Using a polydata centerlines, separate the polydata into regions
 *  based on the centerlines
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVSurfaceCenterlineAttributesPasser_h
#define vtkSVSurfaceCenterlineAttributesPasser_h

#include "vtkSVSegmentationModule.h" // For export

#include "vtkIdList.h"
#include "vtkMatrix4x4.h"
#include "vtkPolyData.h"
#include "vtkPolyDataAlgorithm.h"
#include "vtkStructuredGrid.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGlobals.h"
#include "vtkSVPolyBallLine.h"

class VTKSVSEGMENTATION_EXPORT vtkSVSurfaceCenterlineAttributesPasser : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkSVSurfaceCenterlineAttributesPasser,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  static vtkSVSurfaceCenterlineAttributesPasser *New();

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
  vtkSetStringMacro(TransformedNormalsArrayName);
  vtkGetStringMacro(TransformedNormalsArrayName);
  vtkSetStringMacro(ParallelTransportVectorArrayName);
  vtkGetStringMacro(ParallelTransportVectorArrayName);
  //@}

  //@{
  /// \brief Get/Set the factor for enforcing of the boundary directions. Approximately represents the number of centerline points to enforce per branch. Default is 1, and typically a fairly low value works well. The larger the value, the larger the portion of the vessel is set explicitly, and sometimes this can cause large problems.
  vtkSetMacro(BoundaryEnforceFactor,int);
  vtkGetMacro(BoundaryEnforceFactor,int);
  //@}

  //@{
  /// \brief Get/Set the radius information
  vtkSetMacro(UseRadiusInformation,int);
  vtkGetMacro(UseRadiusInformation,int);
  vtkBooleanMacro(UseRadiusInformation,int);
  //@}

  //@{
  /// \brief Get/Set whether the boundary at separating patches should be more
  //  strictly enforced.
  vtkSetMacro(EnforceBoundaryDirections,int);
  vtkGetMacro(EnforceBoundaryDirections,int);
  vtkBooleanMacro(EnforceBoundaryDirections,int);
  //@}

  //@{
  /// \brief Get/Set the scalar determing how much influence to put on the normal
  // of the cell and how much influence to put on the position of the cell for
  // the cube patch clustering.
  vtkSetMacro(NormalsWeighting,double);
  vtkGetMacro(NormalsWeighting,double);
  //@}

  //@{
  /// \brief Get/Set whether the model is a vascular model with artificial truncated
  //  boundaries
  vtkSetMacro(IsVasculature,int);
  vtkGetMacro(IsVasculature,int);
  vtkBooleanMacro(IsVasculature,int);
  //@}

protected:
  vtkSVSurfaceCenterlineAttributesPasser();
  ~vtkSVSurfaceCenterlineAttributesPasser();

  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  int CheckGroupsWithPolycube();
  int GetOpenBoundaryEdges(vtkPolyData *branchPd,
                           std::vector<int> &openCornerPoints,
                           std::vector<std::vector<int> > &openEdges);
  int ShiftEdgeList(vtkPolyData *branchPd, std::vector<std::vector<int> > &openEdges,
                    std::vector<std::vector<int> > &shiftedOpenEdges);
  int SplitEdgeList(vtkPolyData *branchPd, std::vector<int> &openEdges,
                    std::vector<std::vector<int> > &shiftedOpenEdges);
  int GetCellDirectNeighbors(vtkPolyData *pd,
                             std::vector<std::vector<int> > &neighbors,
                             std::vector<int> &numNeighbors);


  char *CenterlineGroupIdsArrayName;
  char *CenterlineRadiusArrayName;
  char *CenterlineIdsArrayName;
  char *GroupIdsArrayName;
  char *BlankingArrayName;
  char *TractIdsArrayName;
  char *PatchIdsArrayName;
  char *SlicePointsArrayName;
  char *TransformedNormalsArrayName;
  char *ParallelTransportVectorArrayName;

  vtkPolyData *WorkPd;
  vtkPolyData *MergedCenterlines;
  vtkPolyData *PolycubePd;

  int EnforceBoundaryDirections;
  int IsVasculature;
  int BoundaryEnforceFactor;
  int UseRadiusInformation;

  double NormalsWeighting;

private:
  vtkSVSurfaceCenterlineAttributesPasser(const vtkSVSurfaceCenterlineAttributesPasser&);  // Not implemented.
  void operator=(const vtkSVSurfaceCenterlineAttributesPasser&);  // Not implemented.
};

#endif
