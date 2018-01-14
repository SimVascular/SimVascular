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

/** @file svGraph.h
 *  @brief a binary tree for creating a graph structure of 3D surfaces
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#ifndef svGraph_h
#define svGraph_h

#include "vtkPolyData.h"
#include "svGCell.h"
#include "vtkSVParameterizationModule.h" // For exports

#include <map>
#include <list>

class VTKSVPARAMETERIZATION_EXPORT svGraph
{
public:
  //Constructors
  svGraph();
  svGraph(int rootId,
          vtkPolyData *linesPd,
          std::string groupIdsArrayName,
          std::multimap<int, int> criticalPointMap);


  //Destructor
  ~svGraph();

  //Member functions
  svGCell* NewCell(int a_Id, svGCell *a_Parent);
  svGCell* NewCell(int a_Id, int a_Dir, double a_StartPt[3], double a_EndPt[3]);
  svGCell* GetCell(const int findId);
  svGCell* LookUp(svGCell *lookCell, const int findId);
  int BuildGraph();
  int PrintGraph();
  int GrowGraph(svGCell *parent);
  int ComputeReferenceVectors(svGCell *parent);
  int GetNewBranchDirections(svGCell *parent);
  int GetGraphPolyData(vtkPolyData *pd);

  //Static Member functions
  static int GetDirectionVector(const int dir, double dirVector[3]);
  static int Recurse(svGCell *rootsvGCell,
         int(*function)(svGCell *currentsvGCell, void *arg0, void *arg1, void *arg2),
         void *rec_arg0, void *rec_arg1, void *rec_arg2);
  static int UpdateCellDirection(svGCell *gCell, void *arg0, void *arg1, void *arg2);
  static int PrintGCell(svGCell *gCell, void *arg0, void *arg1, void *arg2);
  static int InsertGCellPoints(svGCell *gCell, void *arg0, void *arg1, void *arg2);

  //Member data
  svGCell *Root;
  int NumberOfCells;
  int NumberOfNodes;

  //Member data needed to build
  vtkPolyData *Lines;
  std::string GroupIdsArrayName;
  std::multimap<int, int> CriticalPointMap;
  double ReferenceVecs[3][3];

};

#endif
