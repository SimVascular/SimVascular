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

/** @file vtkSVCenterlineGraph.h
 *  @brief a binary tree for creating a graph structure of 3D surfaces
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#ifndef vtkSVCenterlineGraph_h
#define vtkSVCenterlineGraph_h

#include "vtkSVSegmentationModule.h" // For exports

#include "vtkPolyData.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVCenterlineGCell.h"

#include <map>
#include <list>

class VTKSVSEGMENTATION_EXPORT vtkSVCenterlineGraph : public vtkObject
{
public:
  static vtkSVCenterlineGraph* New();
  vtkTypeMacro(vtkSVCenterlineGraph, vtkObject);

  //Constructors
  vtkSVCenterlineGraph();

  //Destructor
  ~vtkSVCenterlineGraph();

  //@{
  /// \brief Get/Set macro for merged centerlines
  vtkSetObjectMacro(Lines,vtkPolyData);
  vtkGetObjectMacro(Lines,vtkPolyData);
  //@}

  //Macros
  //@{
  /// \brief Get/Set number of cells, points
  vtkSetMacro(NumberOfCells,int);
  vtkGetMacro(NumberOfCells,int);
  vtkSetMacro(NumberOfNodes,int);
  vtkGetMacro(NumberOfNodes,int);
  //@}

  //@{
  /// \brief Get/Set cube size
  vtkSetMacro(CubeSize,double);
  vtkGetMacro(CubeSize,double);
  //@}

  //@{
  /// \brief Get/Set macro for array name used by the filter. Must
  //  be present on the centerlines.
  vtkSetStringMacro(GroupIdsArrayName);
  vtkGetStringMacro(GroupIdsArrayName);
  //@}

  //Member functions
  vtkSVCenterlineGCell* GetCell(const int findId);
  vtkSVCenterlineGCell* GetCellByGroupId(const int findId);
  vtkSVCenterlineGCell* FindId(vtkSVCenterlineGCell *lookCell, const int findId);
  vtkSVCenterlineGCell* FindGroupId(vtkSVCenterlineGCell *lookCell, const int findId);
  int BuildGraph();
  int PrintGraph();
  int GrowGraph(vtkSVCenterlineGCell *parent);
  int GetGraphPoints();
  int ComputeGlobalReferenceVectors(vtkSVCenterlineGCell *parent);
  int ComputeBranchReferenceVectors(vtkSVCenterlineGCell *parent);
  int GetInitialBranchDirections(vtkSVCenterlineGCell *parent);
  int UpdateBranchDirs(vtkSVCenterlineGCell *parent, const int updateDir);
  int UpdateBranchReferenceDirections();
  int GetGraphPolyData(vtkPolyData *pd);
  int GetConnectingLineGroups(const int groupId, std::vector<int> &connectingGroups);

  //Static Member functions
  static int Recurse(vtkSVCenterlineGCell *rootvtkSVCenterlineGCell,
         int(*function)(vtkSVCenterlineGCell *currentvtkSVCenterlineGCell, void *arg0, void *arg1, void *arg2),
         void *rec_arg0, void *rec_arg1, void *rec_arg2);
  static int PrintGCell(vtkSVCenterlineGCell *gCell, void *arg0, void *arg1, void *arg2);
  static int InsertGCellPoints(vtkSVCenterlineGCell *gCell, void *arg0, void *arg1, void *arg2);


  int ComputeLocalCoordinateSystem(const double prev_vz[3], const double vz[3],
                                   const double prev_vx[3],
                                   double vx[3], double vy[3]);

  int RotateVecAroundLine(const double inVec[3],
                          const double angle,
                          const double axis[3],
                          double outVec[3]);

  int FlipLinePoints(vtkPolyData *pd, const int cellId);
  int ComputeMinimumLength(vtkSVCenterlineGCell *gCell, double &minLength);

  //Member data
  vtkSVCenterlineGCell *Root;
  int NumberOfCells;
  int NumberOfNodes;
  double CubeSize;

  //Member data needed to build
  vtkPolyData *Lines;
  char* GroupIdsArrayName;
  double ReferenceVecs[3][3];

private:
  vtkSVCenterlineGraph(const vtkSVCenterlineGraph&); // Not implemented.
  void operator=(const vtkSVCenterlineGraph&); // Not implemented.
};

#endif
