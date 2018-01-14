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
 *  \class vtkSVPullApartPolyData
 *  \brief Given a list of points or an array defined on surface, pull
 *  apart the polydata creating a seam along the geometry.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVPullApartPolyData_h
#define vtkSVPullApartPolyData_h

#include "vtkPolyDataAlgorithm.h"
#include "vtkSVFiltersModule.h" // For export

#include "vtkEdgeTable.h"
#include "vtkPolyData.h"
#include "vtkIdList.h"

class VTKSVFILTERS_EXPORT vtkSVPullApartPolyData : public vtkPolyDataAlgorithm
{
public:
  static vtkSVPullApartPolyData* New();
  //vtkTypeRevisionMacro(vtkSVPullApartPolyData, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);

  //@{
  /// \brief String to separate the polydata at. 1 Indicates the points that are along
  /// separation line, everything else should be 0
  vtkGetStringMacro(CutPointsArrayName);
  vtkSetStringMacro(CutPointsArrayName);
  //@}

  //@{
  /// \brief The list of points that are along seam
  vtkSetObjectMacro(SeamPointIds, vtkIntArray);
  vtkGetObjectMacro(SeamPointIds, vtkIntArray);
  //@}

  //@{
  /// \brief The list of points that are to be replaced
  vtkGetObjectMacro(ReplacePointList, vtkIdList);
  //@}

  //@{
  /// \brief The list that will be the same length as replacepoint list with the ids
  /// corresponding to the new points
  vtkGetObjectMacro(NewPointList, vtkIdList);
  //@}

  //@{
  /// \brief Axis of the object to use on orientation with sphee map
  vtkSetVector3Macro(ObjectXAxis, double);
  vtkSetVector3Macro(ObjectZAxis, double);
  //@}

  //@{
  // /\brief If start point is provided, it helps the algorithm go quicker because
  // a start point does not need to be found
  vtkGetMacro(StartPtId, int);
  vtkSetMacro(StartPtId, int);
  //@}

protected:
  vtkSVPullApartPolyData();
  ~vtkSVPullApartPolyData();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector);

  int PrepFilter(); // Prep work
  int RunFilter(); // Run filter operations.
  int FindEdgeCells(); // Find edge cells
  int PullApartCutEdges();  // Pull edges apart!

  /** \brief find the edge to rule them all.
   *  \param p0 Empty point to return first edge point.
   *  \param p1 Empty point to return second edge point.
   *  \param p2 Empty point to return third edge point.
   *  \param cellId Empty cell to return the id of starting cell. */
  int FindStartingEdge(int &p0, int &p1, int &p2, int &cellId);

  /** \brief find next edge in sequence. Recursive function
   *  \note adss to cellList. */
  int FindNextEdge(int p0, int p1, int p2, int cellId, std::vector<int> &cellList, int first);

  /** \brief If a bad start cell is detected with give start point, fix the
   *  cell. A bad cell is defined as a cell with threepoints on the boundary! */
  int FixTheBadStartCell(vtkPolyData *pd, const int pointId, const int cellId);

private:
  vtkSVPullApartPolyData(const vtkSVPullApartPolyData&);  // Not implemented.
  void operator=(const vtkSVPullApartPolyData&);  // Not implemented.

  char *CutPointsArrayName;

  // TODO: Add start and end point ids

  int StartPtId;
  vtkPolyData  *WorkPd;
  vtkEdgeTable *EdgeTable;
  vtkIntArray  *SeamPointIds;
  vtkIdList    *ReplacePointList;
  vtkIdList    *NewPointList;

  double ObjectXAxis[3];
  double ObjectZAxis[3];

  std::vector<int> ReplacePointVector;
  std::vector<std::vector<int> > ReplaceCellVector;

};

#endif
