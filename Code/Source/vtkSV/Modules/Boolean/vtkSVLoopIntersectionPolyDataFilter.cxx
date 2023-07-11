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

#define OLD 0
#define TRIANGLE 1
#define CURRENT 2
#include "vtkSVLoopIntersectionPolyDataFilter.h"
#include "delaunay_options.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCleanPolyData.h"
#if VTKSV_DELAUNAY_TYPE == OLD
#include "vtkDelaunay2D_60.h"
#elif VTKSV_DELAUNAY_TYPE == TRIANGLE
#include "triangle.h"
#include "vtkTriangleDelaunay2D.h"
#else
#include "vtkDelaunay2D.h"
#endif
#include "vtkDoubleArray.h"
#include "vtkIdList.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkLine.h"
#include "vtkOBBTree.h"
#include "vtkObjectFactory.h"
#include "vtkPlane.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkPoints.h"
#include "vtkPolyDataNormals.h"
#include "vtkPolygon.h"
#include "vtkSortDataArray.h"
#include "vtkSmartPointer.h"
#include "vtkSVGlobals.h"
#include "vtkTriangle.h"
#include "vtkTriangleFilter.h"
#include "vtkTransform.h"
#include "vtkTransformPolyDataFilter.h"
#include "vtkUnstructuredGrid.h"

#include <list>
#include <map>

//----------------------------------------------------------------------------
// Helper typedefs and data structures.
namespace {

// ----------------------
// simPoint
// ----------------------
struct simPoint
{
  vtkIdType id; // id of point
  double pt[3]; // location
};

// ----------------------
// simPolygon
// ----------------------
struct simPolygon
{
  std::list<simPoint> points; // list of points
  int orientation; // their orientation
};

}

// ----------------------
// IntersectionMapType
// ----------------------
typedef std::multimap< vtkIdType, vtkIdType >    IntersectionMapType;
typedef IntersectionMapType::iterator            IntersectionMapIteratorType;

// ----------------------
// CellEdgeLine
// ----------------------
typedef struct _CellEdgeLine {
  vtkIdType CellId;
  vtkIdType EdgeId;
  vtkIdType LineId;
} CellEdgeLineType;

// ----------------------
// PointEdgeMapType
// ----------------------
typedef std::multimap< vtkIdType, CellEdgeLineType > PointEdgeMapType;
typedef PointEdgeMapType::iterator                   PointEdgeMapIteratorType;


//----------------------------------------------------------------------------
// Private implementation to hide STL.
//----------------------------------------------------------------------------
class vtkSVLoopIntersectionPolyDataFilter::Impl
{
public:
  Impl();
  virtual ~Impl();

  /// \brief Finds all triangle triangle intersections between two input OOBTrees
  static int FindTriangleIntersections(vtkOBBNode *node0, vtkOBBNode *node1,
                                       vtkMatrix4x4 *transform, void *arg);

  /// \brief Temporarily moving here to try some stuff out
  static int IntersectPlaneWithLine(double p1[3], double p2[3], double n[3],
                                    double p0[3], double& t, double x[3]);

  /// \brief Runs the split mesh for the designated input surface
  int SplitMesh(int inputIndex, vtkPolyData *output,
                vtkPolyData *intersectionLines);

protected:

  /// \brief Split cells into polygons created by intersection lines
  vtkCellArray* SplitCell(vtkPolyData *input, vtkIdType cellId,
                          vtkIdType *cellPts,
                          IntersectionMapType *map,
                          vtkPolyData *interLines, int inputIndex,
                          int numCurrCells);

  /// \brief Function to add point to check edge list for remeshing step
  int AddToPointEdgeMap(int index, vtkIdType ptId, double x[3],
                        vtkPolyData *mesh, vtkIdType cellId,
                        vtkIdType edgeId, vtkIdType lineId,
                        vtkIdType triPts[3]);

  /// \brief Function to add information about the new cell data
  void AddToNewCellMap(int inputIndex, int interPtCount, int interPts[3],
                       vtkPolyData *interLines,int numCurrCells);

  /// \brief Function inside SplitCell to get the smaller triangle loops
  int GetLoops(vtkPolyData *pd, std::vector<simPolygon> *loops);

  /// \brief Get individual polygon loop of splitting cell
  int GetSingleLoop(vtkPolyData *pd,simPolygon *loop, vtkIdType nextCell,
                    bool *interPtBool, bool *lineBool);

  /// \brief Follow a loop orienation to iterate around a split polygon
  int FollowLoopOrientation(vtkPolyData *pd, simPolygon *loop,
                            vtkIdType *nextCell,
                            vtkIdType nextPt, vtkIdType prevPt,
                            vtkIdList *pointCells);

  /// \brief Set the loop orientation based on CW CCW geometric test
  void SetLoopOrientation(vtkPolyData *pd, simPolygon *loop,
                          vtkIdType *nextCell, vtkIdType nextPt,
                          vtkIdType prevPt, vtkIdList *pointCells);

  /// \brief Get the loop orienation is already given
  int GetLoopOrientation(vtkPolyData *pd, vtkIdType cell, vtkIdType ptId1,
                         vtkIdType ptId2);

  /// \brief Orient the triangle based on the transform for remeshing
  void Orient(vtkPolyData *pd, vtkTransform *transform, vtkPolyData *boundary,
              vtkPolygon *boundarypoly);

  /// \brief Checks to make sure multiple lines are not added to the same triangle
  /// that needs to re-triangulated
  int CheckLine(vtkPolyData *pd, vtkIdType ptId1, vtkIdType ptId2);

  /// \brief Gets a transform to the XY plane for three points comprising a triangle
  int GetTransform(vtkTransform *transform, vtkPoints *points);


public:
  vtkPolyData         *Mesh[2];
  vtkOBBTree          *OBBTree1;

  // Stores the intersection lines.
  vtkCellArray        *IntersectionLines;

  vtkIdTypeArray      *SurfaceId;
  vtkIdTypeArray      *NewCellIds[2];

  /// \brief  Cell data that indicates in which cell each intersection
  /// lies. One array for each output surface.
  vtkIdTypeArray      *CellIds[2];

  /// \brief Cell data that indicates on which surface the intersection point lies.

  /// Map from points to the cells that contain them. Used for point
  /// data interpolation. For points on the edge between two cells, it
  /// does not matter which cell is recorded bcause the interpolation
  /// will be the same.  One array for each output surface.
  vtkIdTypeArray      *PointCellIds[2];
  vtkIntArray         *BoundaryPoints[2];

  /// \brief Merging filter used to convert intersection lines from "line
  /// soup" to connected polylines.
  vtkPointLocator     *PointMerger;

  /// \brief Map from cell ID to intersection line.
  IntersectionMapType *IntersectionMap[2];
  IntersectionMapType *IntersectionPtsMap[2];
  IntersectionMapType *PointMapper;

  /// \brief Map from point to an edge on which it resides, the ID of the
  /// cell, and the ID of the line.
  PointEdgeMapType    *PointEdgeMap[2];

  /// \brief vtkPolyData to hold current splitting cell. Used to double check area
  /// of small area cells
  vtkPolyData *SplittingPD;
  int         TransformSign;
  double      Tolerance;

  /// \brief Pointer to overarching filter
  vtkSVLoopIntersectionPolyDataFilter *ParentFilter;

protected:
  Impl(const Impl&);
  void operator=(const Impl&);

};

// ----------------------
// Impl Constructor
// ----------------------
vtkSVLoopIntersectionPolyDataFilter::Impl::Impl() :
  OBBTree1(0), IntersectionLines(0), SurfaceId(0), PointMerger(0)
{
  for (int i = 0; i < 2; i++)
    {
    this->Mesh[i]                 = nullptr;
    this->CellIds[i]              = nullptr;
    this->IntersectionMap[i]      = new IntersectionMapType();
    this->IntersectionPtsMap[i]   = new IntersectionMapType();
    this->PointEdgeMap[i]         = new PointEdgeMapType();
    }
  this->PointMapper               = new IntersectionMapType();
  this->SplittingPD               = vtkPolyData::New();
  this->TransformSign = 0;
  this->Tolerance = 1e-6;
}

// ----------------------
// Impl Destructor
// ----------------------
vtkSVLoopIntersectionPolyDataFilter::Impl::~Impl()
{
  for (int i = 0; i < 2; i++)
    {
    delete this->IntersectionMap[i];
    delete this->IntersectionPtsMap[i];
    delete this->PointEdgeMap[i];
    }
  delete this->PointMapper;
  this->SplittingPD->Delete();
}

// ----------------------
// Impl::FindTriangleIntersections
// ----------------------
int vtkSVLoopIntersectionPolyDataFilter::Impl
::FindTriangleIntersections(vtkOBBNode *node0, vtkOBBNode *node1,
                            vtkMatrix4x4 *transform, void *arg)
{
  vtkSVLoopIntersectionPolyDataFilter::Impl *info =
    reinterpret_cast<vtkSVLoopIntersectionPolyDataFilter::Impl*>(arg);

  //Set up local structures to hold Impl array information
  vtkPolyData     *mesh0                 = info->Mesh[0];
  vtkPolyData     *mesh1                 = info->Mesh[1];
  vtkOBBTree      *obbTree1              = info->OBBTree1;
  vtkCellArray    *intersectionLines     = info->IntersectionLines;
  vtkIdTypeArray  *intersectionSurfaceId = info->SurfaceId;
  vtkIdTypeArray  *intersectionCellIds0  = info->CellIds[0];
  vtkIdTypeArray  *intersectionCellIds1  = info->CellIds[1];
  vtkPointLocator *pointMerger           = info->PointMerger;
  double tolerance                       = info->Tolerance;

  //The number of cells in OBBTree
  int numCells0 = node0->Cells->GetNumberOfIds();

  for (vtkIdType id0 = 0; id0 < numCells0; id0++)
    {
    vtkIdType cellId0 = node0->Cells->GetId(id0);
    int type0 = mesh0->GetCellType(cellId0);

    //Make sure the cell is a triangle
    if (type0 == VTK_TRIANGLE)
      {
      vtkIdType npts0;
      const vtkIdType *triPtIds0;
      mesh0->GetCellPoints(cellId0, npts0, triPtIds0);
      double triPts0[3][3];
      for (vtkIdType id = 0; id < npts0; id++)
        {
        mesh0->GetPoint(triPtIds0[id], triPts0[id]);
        }

      if (obbTree1->TriangleIntersectsNode
          (node1, triPts0[0], triPts0[1], triPts0[2], transform))
        {
        int numCells1 = node1->Cells->GetNumberOfIds();
        for (vtkIdType id1 = 0; id1 < numCells1; id1++)
          {
          vtkIdType cellId1 = node1->Cells->GetId(id1);
          int type1 = mesh1->GetCellType(cellId1);
          if (type1 == VTK_TRIANGLE)
            {
            // See if the two cells actually intersect. If they do,
            // add an entry into the intersection maps and add an
            // intersection line.
            vtkIdType npts1;
            const vtkIdType *triPtIds1;
            mesh1->GetCellPoints(cellId1, npts1, triPtIds1);

            double triPts1[3][3];
            for (vtkIdType id = 0; id < npts1; id++)
              {
              mesh1->GetPoint(triPtIds1[id], triPts1[id]);
              }

            int coplanar = 0;
            double outpt0[3], outpt1[3];
            double surfaceid[2];
            int intersects =
              vtkSVLoopIntersectionPolyDataFilter::TriangleTriangleIntersection
              (triPts0[0], triPts0[1], triPts0[2],
               triPts1[0], triPts1[1], triPts1[2],
               coplanar, outpt0, outpt1, surfaceid, tolerance);

            if (coplanar)
              {
              // Coplanar triangle intersection is not handled.
              // This intersection will not be included in the output. TODO
              //vtkDebugMacro(<<"Coplanar");
              intersects = 0;
              continue;
              }

            //If actual intersection, add point and cell to edge, line,
            //and surface maps!
            if (intersects)
              {
              vtkIdType lineId = intersectionLines->GetNumberOfCells();

              vtkIdType ptId0, ptId1;
              int unique[2];
              unique[0] = pointMerger->InsertUniquePoint(outpt0, ptId0);
              unique[1] = pointMerger->InsertUniquePoint(outpt1, ptId1);

              int addline = 1;
              if (ptId0 == ptId1)
                {
                addline = 0;
                }

              if (ptId0 == ptId1 && surfaceid[0] != surfaceid[1])
                {
                intersectionSurfaceId->InsertValue(ptId0, 3);
                }
              else
                {
                if (unique[0])
                  {
                  intersectionSurfaceId->InsertValue(ptId0, surfaceid[0]);
                  }
                else
                  {
                  if (intersectionSurfaceId->GetValue(ptId0) != 3)
                    {
                    intersectionSurfaceId->InsertValue(ptId0, surfaceid[0]);
                    }
                  }
                if (unique[1])
                  {
                  intersectionSurfaceId->InsertValue(ptId1, surfaceid[1]);
                  }
                else
                  {
                  if (intersectionSurfaceId->GetValue(ptId1) != 3)
                    {
                    intersectionSurfaceId->InsertValue(ptId1, surfaceid[1]);
                    }
                  }
                }

              info->IntersectionPtsMap[0]->
                insert(std::make_pair(ptId0, cellId0));
              info->IntersectionPtsMap[1]->
                insert(std::make_pair(ptId0, cellId1));
              info->IntersectionPtsMap[0]->
                insert(std::make_pair(ptId1, cellId0));
              info->IntersectionPtsMap[1]->
                insert(std::make_pair(ptId1, cellId1));

              //Check to see if duplicate line. Line can only be a duplicate
              //line if both points are not unique and they don't
              //equal eachother
              if (!unique[0] && !unique[1] && ptId0 != ptId1)
                {
                vtkNew(vtkPolyData, lineTest);
                lineTest->SetPoints(pointMerger->GetPoints());
                lineTest->SetLines(intersectionLines);
                lineTest->BuildLinks();
                int newLine = info->CheckLine(lineTest, ptId0, ptId1);
                if (newLine == 0)
                  {
                  addline = 0;
                  }
                }
              if (addline)
                {
                //If the line is new and does not consist of two identical
                //points, add the line to the intersection and update
                //mapping information
                intersectionLines->InsertNextCell(2);
                intersectionLines->InsertCellPoint(ptId0);
                intersectionLines->InsertCellPoint(ptId1);

                intersectionCellIds0->InsertNextValue(cellId0);
                intersectionCellIds1->InsertNextValue(cellId1);

                info->PointCellIds[0]->InsertValue(ptId0, cellId0);
                info->PointCellIds[0]->InsertValue(ptId1, cellId0);
                info->PointCellIds[1]->InsertValue(ptId0, cellId1);
                info->PointCellIds[1]->InsertValue(ptId1, cellId1);

                info->IntersectionMap[0]->
                  insert(std::make_pair(cellId0, lineId));
                info->IntersectionMap[1]->
                  insert(std::make_pair(cellId1, lineId));

                // Check which edges of cellId0 and cellId1 outpt0 and
                // outpt1 are on, if any.
                int isOnEdge=0;
                int m0p0=0, m0p1=0, m1p0=0, m1p1=0;
                for (vtkIdType edgeId = 0; edgeId < 3; edgeId++)
                  {
                  isOnEdge = info->AddToPointEdgeMap(0, ptId0, outpt0,
                      mesh0, cellId0, edgeId, lineId, triPtIds0);
                  if (isOnEdge != -1)
                    {
                    m0p0++;
                    }
                  isOnEdge = info->AddToPointEdgeMap(0, ptId1, outpt1,
                      mesh0, cellId0, edgeId, lineId, triPtIds0);
                  if (isOnEdge != -1)
                    {
                    m0p1++;
                    }
                  isOnEdge = info->AddToPointEdgeMap(1, ptId0, outpt0,
                      mesh1, cellId1, edgeId, lineId, triPtIds1);
                  if (isOnEdge != -1)
                    {
                    m1p0++;
                    }
                  isOnEdge = info->AddToPointEdgeMap(1, ptId1, outpt1,
                      mesh1, cellId1, edgeId, lineId, triPtIds1);
                  if (isOnEdge != -1)
                    {
                    m1p1++;
                    }
                  }
                //Special cases caught by tolerance and not from the Point
                //Merger
                if (m0p0 > 0 && m1p0 > 0)
                  {
                  intersectionSurfaceId->InsertValue(ptId0, 3);
                  }
                if (m0p1 > 0 && m1p1 > 0)
                  {
                  intersectionSurfaceId->InsertValue(ptId1, 3);
                  }
                }
              //Add information about origin surface to std::maps for
              //checks later
              if (intersectionSurfaceId->GetValue(ptId0) == 1)
                {
                info->IntersectionPtsMap[0]->
                  insert(std::make_pair(ptId0, cellId0));
                }
              else if (intersectionSurfaceId->GetValue(ptId0) == 2)
                {
                info->IntersectionPtsMap[1]->
                  insert(std::make_pair(ptId0, cellId1));
                }
              else
                {
                info->IntersectionPtsMap[0]->
                  insert(std::make_pair(ptId0, cellId0));
                info->IntersectionPtsMap[1]->
                  insert(std::make_pair(ptId0, cellId1));
                }
              if (intersectionSurfaceId->GetValue(ptId1) == 1)
                {
                info->IntersectionPtsMap[0]->
                  insert(std::make_pair(ptId1, cellId0));
                }
              else if (intersectionSurfaceId->GetValue(ptId1) == 2)
                {
                info->IntersectionPtsMap[1]->
                  insert(std::make_pair(ptId1, cellId1));
                }
              else
                {
                info->IntersectionPtsMap[0]->
                  insert(std::make_pair(ptId1, cellId0));
                info->IntersectionPtsMap[1]->
                  insert(std::make_pair(ptId1, cellId1));
                }
              }
            }
          }
        }
      }
    }

  return SV_OK;
}

// ----------------------
// Impl::SplitMesh
// ----------------------
int vtkSVLoopIntersectionPolyDataFilter::Impl
::SplitMesh(int inputIndex, vtkPolyData *output, vtkPolyData *intersectionLines)
{
  vtkPolyData *input = this->Mesh[inputIndex];
  IntersectionMapType *intersectionMap = this->IntersectionMap[inputIndex];
  vtkCellData *inCD  = input->GetCellData();
  vtkCellData *outCD = output->GetCellData();
  vtkIdType numCells = input->GetNumberOfCells();
  vtkIdType cellIdX = 0;

  //
  // Process points
  //
  vtkIdType inputNumPoints = input->GetPoints()->GetNumberOfPoints();
  vtkNew( vtkPoints , points);
  points->Allocate(100);
  output->SetPoints(points);

  //
  // Split intersection lines. The lines structure is constructed
  // using a vtkPointLocator. However, some lines may have an endpoint
  // on a cell edge that has no neighbor. We need to duplicate a line
  // point in such a case and update the point ID in the line cell.
  //
  vtkNew( vtkPolyData , splitLines);
  splitLines->DeepCopy(intersectionLines);

  vtkPointData *inPD  = input->GetPointData();
  vtkPointData *outPD = output->GetPointData();
  outPD->CopyAllocate(inPD, input->GetNumberOfPoints());

  // Copy over the point data from the input
  for (vtkIdType ptId = 0; ptId < inputNumPoints; ptId++)
    {
    double pt[3];
    input->GetPoints()->GetPoint(ptId, pt);
    output->GetPoints()->InsertNextPoint(pt);
    outPD->CopyData(inPD, ptId, ptId);
    this->BoundaryPoints[inputIndex]->InsertValue(ptId, 0);
    }

  // Copy the points from splitLines to the output, interpolating the
  // data as we go.
  for (vtkIdType id = 0; id < splitLines->GetNumberOfPoints(); id++)
    {
    double pt[3];
    splitLines->GetPoint(id, pt);
    vtkIdType newPtId = output->GetPoints()->InsertNextPoint(pt);

    // Retrieve the cell ID from splitLines
    vtkIdType cellId = this->PointCellIds[inputIndex]->GetValue(id);

    double closestPt[3], pcoords[3], dist2, weights[3];
    int subId;
    vtkCell *cell = input->GetCell(cellId);
    cell->EvaluatePosition(pt, closestPt, subId, pcoords, dist2, weights);
    outPD->InterpolatePoint(input->GetPointData(), newPtId, cell->PointIds,
                            weights);
    this->BoundaryPoints[inputIndex]->InsertValue(newPtId, 0);
    }

  //
  // Process cells
  //
  outCD->CopyAllocate(inCD, numCells);

  if (input->GetPolys()->GetNumberOfCells() > 0)
    {
    vtkCellArray *cells = input->GetPolys();
    vtkIdType newId = output->GetNumberOfCells();

    vtkNew( vtkCellArray , newPolys);

    newPolys->EstimateSize(cells->GetNumberOfCells(), 3);
    output->SetPolys(newPolys);

    vtkNew( vtkIdList , edgeNeighbors);
    vtkIdType nptsX = 0;
    const vtkIdType *pts;
    vtkNew( vtkIdList , cellsToCheck);
    for (cells->InitTraversal(); cells->GetNextCell(nptsX, pts); cellIdX++)
      {
      if (nptsX != 3)
        {
        vtkGenericWarningMacro(<< "vtkSVLoopIntersectionPolyDataFilter only works"
                                << " with triangle meshes.");
        continue;
        }

      cellsToCheck->Reset();
      cellsToCheck->Allocate(nptsX+1);
      cellsToCheck->InsertNextId(cellIdX);

      // Collect the cells relevant for splitting this cell.  If the
      // cell is in the intersection map, split. If not, one of its
      // edges may be split by an intersection line that splits a
      // neighbor cell. Mark the cell as needing a split if this is
      // the case.
      bool needsSplit = intersectionMap->find(cellIdX)
        != intersectionMap->end();
      for (vtkIdType ptId = 0; ptId < nptsX; ptId++)
        {
        vtkIdType pt0Id = pts[ptId];
        vtkIdType pt1Id = pts[(ptId+1) % nptsX];
        edgeNeighbors->Reset();
        input->GetCellEdgeNeighbors(cellIdX, pt0Id, pt1Id, edgeNeighbors);
        for (vtkIdType nbr = 0; nbr < edgeNeighbors->GetNumberOfIds(); nbr++)
          {
          vtkIdType nbrId = edgeNeighbors->GetId(nbr);
          cellsToCheck->InsertNextId(nbrId);

          if (intersectionMap->find(nbrId) != intersectionMap->end())
            {
            needsSplit = true;
            }
          } // for (vtkIdType nbr = 0; ...
        } // for (vtkIdType pt = 0; ...

      // Splitting occurs here
      if (!needsSplit)
        {
        // Just insert the cell and copy the cell data
        newId = newPolys->InsertNextCell(3, pts);
        outCD->CopyData(inCD, cellIdX, newId);
        }
      else
        {
        //Total number of cells so that we know the id numbers of the new
        //cells added and we can add it to the new cell id mapping
        int numCurrCells = newPolys->GetNumberOfCells();
        vtkCellArray *splitCells = this->SplitCell
          (input, cellIdX, pts, intersectionMap, splitLines,
           inputIndex,numCurrCells);
        if (splitCells != nullptr)
          {
          double pt0[3], pt1[3], pt2[3], normal[3];
          points->GetPoint(pts[0], pt0);
          points->GetPoint(pts[1], pt1);
          points->GetPoint(pts[2], pt2);
          vtkTriangle::ComputeNormal(pt0, pt1, pt2, normal);
          vtkMath::Normalize(normal);

          vtkIdType npts, subCellId;
          const vtkIdType *ptIds;
          splitCells->InitTraversal();
          for (subCellId = 0; splitCells->GetNextCell(npts, ptIds); subCellId++)
            {
            // Check for reversed cells. I'm not sure why, but in some
            // cases, cells are reversed.
            double subCellNormal[3];
            points->GetPoint(ptIds[0], pt0);
            points->GetPoint(ptIds[1], pt1);
            points->GetPoint(ptIds[2], pt2);
            vtkTriangle::ComputeNormal(pt0, pt1, pt2, subCellNormal);
            vtkMath::Normalize(subCellNormal);

            if (vtkMath::Dot(normal, subCellNormal) > 0)
              {
              newId = newPolys->InsertNextCell(npts, ptIds);
              }
            else
              {
              newId = newPolys->InsertNextCell(npts);
              for (int i = 0; i < npts; i++)
                {
                newPolys->InsertCellPoint(ptIds[ npts-i-1 ]);
                }
              }

            outCD->CopyData(inCD, cellIdX, newId); // Duplicate cell data
            }
          splitCells->Delete();
          }
        else
          {
          vtkDebugWithObjectMacro(this->ParentFilter, <<"Error in splitting cell!");
          }
        }
      } // for (cells->InitTraversal(); ...
    } //if inputGetPolys()->GetNumberOfCells() > 1 ...

  return SV_OK;
}

// ----------------------
// Impl::SplitCell
// ----------------------
vtkCellArray* vtkSVLoopIntersectionPolyDataFilter::Impl
::SplitCell(vtkPolyData *input, vtkIdType cellId, vtkIdType *cellPts,
            IntersectionMapType *map,
            vtkPolyData *interLines, int inputIndex,
            int numCurrCells)
{
  // Copy down the SurfaceID array that tells which surface the point belongs
  // to
  vtkIdTypeArray *surfaceMapper;
  surfaceMapper = vtkIdTypeArray::SafeDownCast(
    interLines->GetPointData()->GetArray("SurfaceID"));

  //Array to keep track of which points are on the boundary of the cell
  vtkNew(vtkIdTypeArray, cellBoundaryPt);
  //Array to tell whether the original cell points lie on the intersecting
  //line
  int CellPointOnInterLine[3] = {0,0,0};

  // Gather points from the cell
  vtkNew( vtkPoints, points);
  vtkNew( vtkPointLocator , merger);
  merger->SetTolerance(this->Tolerance);
  merger->InitPointInsertion(points, input->GetBounds());

  double xyz[3];
  for (int i = 0; i < 3; i++)
    {
    if (cellPts[i] >= input->GetNumberOfPoints())
      {
      vtkGenericWarningMacro(<< "invalid point read 1");
      }
    input->GetPoint(cellPts[i], xyz);
    merger->InsertNextPoint(xyz);
    cellBoundaryPt->InsertNextValue(1);
    }

  // Set up line cells and array to track the just the intersecting lines
  // on the cell.
  vtkNew( vtkCellArray , lines);
  vtkNew( vtkCellArray , interceptlines);

  double p0[3], p1[3], p2[3];
  input->GetPoint(cellPts[0], p0);
  input->GetPoint(cellPts[1], p1);
  input->GetPoint(cellPts[2], p2);

  // This maps the point IDs for the vtkPolyData passed to
  // vtkDelaunay2D back to the original IDs in interLines. NOTE: The
  // point IDs from the cell are not stored here.
  std::map< vtkIdType, vtkIdType > ptIdMap;

  IntersectionMapIteratorType iterLower = map->lower_bound(cellId);
  IntersectionMapIteratorType iterUpper = map->upper_bound(cellId);
  //Get all the lines associated with the original cell
  while (iterLower != iterUpper)
    {
    vtkIdType lineId = iterLower->second;
    vtkIdType nLinePts;
    const vtkIdType *linePtIds;
    interLines->GetLines()->GetCell(3*lineId, nLinePts, linePtIds);

    interceptlines->InsertNextCell(2);
    lines->InsertNextCell(2);
    //Loop through the points of each line
    for (vtkIdType i = 0; i < nLinePts; i++)
      {
      std::map< vtkIdType, vtkIdType >::iterator location =
        ptIdMap.find(linePtIds[i]);
      //If point already isn't in list
      if (location == ptIdMap.end())
        {
        interLines->GetPoint(linePtIds[i], xyz);
        if (linePtIds[i] >= interLines->GetNumberOfPoints())
          {
          vtkGenericWarningMacro(<< "invalid point read 2");
          }
        //Check to see if point is unique
        int unique = merger->InsertUniquePoint(xyz, ptIdMap[ linePtIds[i] ]);
        if (unique)
          {
          //If point is unique, check to see if it is actually a
          //point originating from this input surface or on both surfaces
          //Don't mark as boundary point if it originates from other surface
          if (surfaceMapper->GetValue(linePtIds[i]) == inputIndex + 1 ||
              surfaceMapper->GetValue(linePtIds[i]) == 3)
            {
            cellBoundaryPt->InsertValue(ptIdMap[linePtIds[i]], 1);
            }
          else
            {
            cellBoundaryPt->InsertValue(ptIdMap[linePtIds[i]], 0);
            }
          }
        else
          {
          //Obviously if the pointid is less than three, it is one of the
          //original cell points and can be added to the inter cell point arr
          if (ptIdMap[linePtIds[i] ] < 3)
            {
            CellPointOnInterLine[ptIdMap[linePtIds[i]]] = 1;
            }
          }
        interceptlines->InsertCellPoint(ptIdMap[ linePtIds[i] ]);
        lines->InsertCellPoint(ptIdMap[ linePtIds[i] ]);
        }
      //Point is already in list, so run through checks with its value
      else
        {
        interceptlines->InsertCellPoint(location->second);
        lines->InsertCellPoint(location->second);
        if (location->second < 3)
          {
          CellPointOnInterLine[location->second] = 1;
          }
        }
      }
    ++iterLower;
    }

  // Now check the neighbors of the cell
  IntersectionMapIteratorType ptIterLower;
  IntersectionMapIteratorType ptIterUpper;
  IntersectionMapIteratorType cellIterLower;
  IntersectionMapIteratorType cellIterUpper;
  vtkNew( vtkIdList , nbrCellIds);
  for (vtkIdType i = 0; i < 3; i++)
    {
    //Get Points belonging to each edge of this cell
    vtkIdType edgePtId0 = cellPts[i];
    vtkIdType edgePtId1 = cellPts[(i+1) % 3];

    double edgePt0[3], edgePt1[3];
    if (edgePtId0 >= input->GetNumberOfPoints())
      {
      vtkGenericWarningMacro(<< "invalid point read 3");
      }
    if (edgePtId1 >= input->GetNumberOfPoints())
      {
      vtkGenericWarningMacro(<< "invalid point read 4");
      }
    input->GetPoint(edgePtId0, edgePt0);
    input->GetPoint(edgePtId1, edgePt1);

    nbrCellIds->Reset();
    input->GetCellEdgeNeighbors(cellId, edgePtId0, edgePtId1, nbrCellIds);
    //Loop through attached neighbor cells and check for split edges
    for (vtkIdType j = 0; j < nbrCellIds->GetNumberOfIds(); j++)
      {
      vtkIdType nbrCellId = nbrCellIds->GetId(j);
      iterLower = map->lower_bound(nbrCellId);
      iterUpper = map->upper_bound(nbrCellId);
      while (iterLower != iterUpper)
        {
        vtkIdType lineId = iterLower->second;
        vtkIdType nLinePts;
        const vtkIdType *linePtIds;
        interLines->GetLines()->GetCell(3*lineId, nLinePts, linePtIds);
        for (vtkIdType k = 0; k < nLinePts; k++)
          {
          if (linePtIds[k] >= interLines->GetNumberOfPoints())
            {
            vtkGenericWarningMacro(<< "invalid point read 5");
            }
          interLines->GetPoint(linePtIds[k], xyz);
          ptIterLower = this->PointMapper->lower_bound(linePtIds[k]);
          ptIterUpper = this->PointMapper->upper_bound(linePtIds[k]);

          //Find all points within this neighbor cell
          while (ptIterLower != ptIterUpper)
            {
            vtkIdType mappedPtId = ptIterLower->second;
            cellIterLower = this->IntersectionPtsMap[inputIndex]->
              lower_bound(mappedPtId);
            cellIterUpper = this->IntersectionPtsMap[inputIndex]->
              upper_bound(mappedPtId);
            //Check all cell values associated with this point
            while (cellIterLower != cellIterUpper)
              {
              vtkIdType checkCellId = cellIterLower->second;

              //If this cell id is the same as the current cell id, this
              //means the point is a split edge, need to add to list!!
              if (checkCellId == cellId)
                {
                int unique=0;
                if (ptIdMap.find(linePtIds[k]) == ptIdMap.end())
                  {
                  unique = merger->InsertUniquePoint(xyz,
                      ptIdMap[ linePtIds[k] ]);
                  }

                else
                  {
                  //Point is less than 3, original cell point
                  if (ptIdMap[ linePtIds[k] ] < 3)
                    {
                    CellPointOnInterLine[ptIdMap[ linePtIds[k] ]] = 1;
                    }

                  }

                if (unique)
                  {
                  //Check to see what surface point originates from. Don't
                  //mark if point is from other surface
                  if (surfaceMapper->GetValue(linePtIds[k]) == inputIndex + 1
                      || surfaceMapper->GetValue(linePtIds[k]) == 3)
                    {
                    cellBoundaryPt->InsertValue(ptIdMap[linePtIds[k]], 1);
                    }

                  else
                    {
                    cellBoundaryPt->InsertValue(ptIdMap[linePtIds[k]], 0);
                    }

                  }
                else
                  {
                  if (ptIdMap[ linePtIds[k] ] < 3)
                    {
                    CellPointOnInterLine[ptIdMap[ linePtIds[k] ]] = 1;
                    }
                  }
                }
              ++cellIterLower;
              }
            ++ptIterLower;
            }
          }
        ++iterLower;
        }
      }
    }

  // Set up reverse ID map
  std::map< vtkIdType, vtkIdType > reverseIdMap;
  std::map< vtkIdType, vtkIdType > reverseLineIdMap;
  std::map< vtkIdType, vtkIdType >::iterator iter = ptIdMap.begin();
  while (iter != ptIdMap.end())
    {
    // If we have more than one point mapping back to the same point
    // in the input mesh, just use the first one. This will give a
    // preference for using cell points when an intersection line shares
    // a point with a a cell and prevent introducing accidental holes
    // in the mesh.
    if (reverseIdMap.find(iter->second) == reverseIdMap.end())
      {
      reverseIdMap[ iter->second ] = iter->first + input->GetNumberOfPoints();
      }
    if (reverseLineIdMap.find(iter->second) == reverseLineIdMap.end())
      {
      reverseLineIdMap[ iter->second ] = iter->first;
      }
    ++iter;
    }

  double v0[3], v1[3], n[3], c[3];
  vtkTriangle::TriangleCenter(p0, p1, p2, c);
  vtkTriangle::ComputeNormal(p0, p1, p2, n);
  vtkMath::Perpendiculars(n, v0, v1, 0.0);

  // For each point on an edge, compute it's relative angle about n.
  vtkNew( vtkIdTypeArray , edgePtIdList);
  vtkNew( vtkIdTypeArray , interPtIdList);
  edgePtIdList->Allocate(points->GetNumberOfPoints());
  vtkNew( vtkDoubleArray,  angleList);
  angleList->Allocate(points->GetNumberOfPoints());
  bool *interPtBool = new bool[points->GetNumberOfPoints()];

  for (vtkIdType ptId = 0; ptId < points->GetNumberOfPoints(); ptId++)
    {
    double x[3];
    points->GetPoint(ptId, x);

    interPtBool[ptId] = false;
    if (cellBoundaryPt->GetValue(ptId))
      {
      // Point is on line. Add its id to id list and add its angle to
      // angle list.
      edgePtIdList->InsertNextValue(ptId);
      double d[3];
      vtkMath::Subtract(x, c, d);
      angleList->InsertNextValue(atan2(vtkMath::Dot(d, v0),
                                       vtkMath::Dot(d, v1)));
      if (ptId > 2)
        {
        //Intersection Point!
        interPtIdList->InsertNextValue(ptId);
        interPtBool[ptId] = true;
        }
      }
    //Setting the boundary points
    if (ptId > 2)
      {
      this->BoundaryPoints[inputIndex]->InsertValue(reverseIdMap[ptId], 1);
      }
    else if (CellPointOnInterLine[ptId])
      {
      this->BoundaryPoints[inputIndex]->InsertValue(cellPts[ptId], 1);
      }
    else
      {
      this->BoundaryPoints[inputIndex]->InsertValue(cellPts[ptId], 0);
      }

    }
  // Sort the edgePtIdList according to the angle list. The starting
  // point doesn't matter. We just need to generate boundary lines in
  // a consistent order.
  vtkSortDataArray::Sort(angleList, edgePtIdList);

  vtkNew(vtkPolyData, checkPD);
  checkPD->SetPoints(points);
  checkPD->SetLines(lines);
  checkPD->BuildLinks();
  vtkIdType id;
  //Check to see if the lines are unique
  for (id = 0; id < edgePtIdList->GetNumberOfTuples()-1; id++)
    {
    int unique = this->CheckLine(checkPD, edgePtIdList->GetValue(id),
        edgePtIdList->GetValue(id+1));
    if (unique)
      {
      lines->InsertNextCell(2);
      lines->InsertCellPoint(edgePtIdList->GetValue(id));
      lines->InsertCellPoint(edgePtIdList->GetValue(id + 1));
      }
    }
  int unique = this->CheckLine(checkPD,
      edgePtIdList->GetValue(edgePtIdList->GetNumberOfTuples()-1),
      edgePtIdList->GetValue(0));
  if (unique)
    {
    lines->InsertNextCell(2);
    lines->InsertCellPoint
      (edgePtIdList->GetValue(edgePtIdList->GetNumberOfTuples()-1));
    lines->InsertCellPoint(edgePtIdList->GetValue(0));
    }

  // Set up a transform that will rotate the points to the
  // XY-plane (normal aligned with z-axis).
  vtkNew( vtkTransform , transform);
  this->TransformSign = this->GetTransform(transform, points);

  vtkCellArray *splitCells = vtkCellArray::New();
  vtkNew(vtkPolyData, interpd);
  interpd->SetPoints(points);
  interpd->SetLines(interceptlines);
  interpd->BuildLinks();

  vtkNew(vtkPolyData, fullpd);
  fullpd->SetPoints(points);
  fullpd->SetLines(lines);
  SplittingPD->DeepCopy(fullpd);

  vtkNew(vtkTransformPolyDataFilter, transformer);
  vtkNew(vtkPolyData, transformedpd);
  transformer->SetInputData(fullpd);
  transformer->SetTransform(transform);
  transformer->Update();
  transformedpd = transformer->GetOutput();
  transformedpd->BuildLinks();
#ifdef VTKSV_PREDELAUNAY_SPLIT
  //If the triangle has intersecting lines and new points
  if (interPtIdList->GetNumberOfTuples() > 0 &&
      interceptlines->GetNumberOfCells() > 0)
    {
    //Get polygon loops of intersected triangle
    std::vector<simPolygon> loops;
    if (this->GetLoops(transformedpd, &loops) != SV_OK)
      {
      splitCells->Delete();
      splitCells = nullptr;
      delete [] interPtBool;
      return splitCells;
      }
    //For each loop, orient and triangulate
    for (int k = 0; k < (int) loops.size(); k++)
      {
      vtkCellArray *polys;
      vtkNew(vtkPolyData, newpd);
      vtkNew(vtkPoints, newPoints);
      vtkNew(vtkCellArray, newLines);
      std::list<simPoint>::iterator it;
      int ptiter=0;
      int *pointMapper = new int[loops[k].points.size()];
      for (it = loops[k].points.begin(); it != loops[k].points.end(); ++it)
        {
        if (ptiter < (int) loops[k].points.size()-1)
          {
          newPoints->InsertNextPoint(points->GetPoint((it)->id));
          pointMapper[ptiter] = (it)->id;
          }
        if (ptiter < (int) loops[k].points.size()-2)
          {
          newLines->InsertNextCell(2);
          newLines->InsertCellPoint(ptiter);
          newLines->InsertCellPoint(ptiter+1);
          }
        ptiter++;
        }
      newLines->InsertNextCell(2);
      newLines->InsertCellPoint(ptiter-2);
      newLines->InsertCellPoint(0);

      //Orient polygon
      newpd->SetPoints(newPoints);
      newpd->SetLines(newLines);
      vtkNew(vtkPolyData, boundary);
      vtkNew(vtkPolygon, boundaryPoly);
      this->Orient(newpd, transform, boundary, boundaryPoly);

      //Triangulate with delaunay2D
#if VTKSV_DELAUNAY_TYPE == OLD
      vtkNew( vtkDelaunay2D_60 , del2D);
#elif VTKSV_DELAUNAY_TYPE == TRIANGLE
      vtknew( vtktriangledelaunay2d , del2d);
#else
      vtkNew( vtkDelaunay2D , del2D);
#endif
      del2D->SetInputData(newpd);
      del2D->SetSourceData(boundary);
      del2D->SetTolerance(0.0);
      del2D->SetAlpha(0.0);
      del2D->SetOffset(0);
      del2D->SetProjectionPlaneMode(VTK_SET_TRANSFORM_PLANE);
      del2D->SetTransform(transform);
      del2D->BoundingTriangulationOff();
      del2D->Update();
      polys = del2D->GetOutput()->GetPolys();
      vtkNew(vtkTriangleFilter, triangulator);
      //If the number of cells output is not two minus the number of
      //points, the triangulation failed with 0 offset! Try again with
      //a higher offset. This typically resolves triangulation issues
      if (polys->GetNumberOfCells() != newpd->GetNumberOfPoints() - 2)
        {
        int numoffsets = 1;
        while ((polys->GetNumberOfCells() != newpd->GetNumberOfPoints()-2)
            && numoffsets < 20)
          {
#if VTKSV_DELAUNAY_TYPE == OLD
          vtkNew( vtkDelaunay2D_60 , del2Doffset);
#elif VTKSV_DELAUNAY_TYPE == TRIANGLE
          vtkNew( vtkTriangleDelaunay2D , del2Doffset);
#else
          vtkNew( vtkDelaunay2D , del2Doffset);
#endif
          del2Doffset->SetInputData(newpd);
          del2Doffset->SetSourceData(boundary);
          del2Doffset->SetTolerance(0.0);
          del2Doffset->SetAlpha(0.0);
          del2Doffset->SetOffset(numoffsets);
          del2Doffset->SetProjectionPlaneMode(VTK_SET_TRANSFORM_PLANE);
          del2Doffset->SetTransform(transform);
          del2Doffset->BoundingTriangulationOff();
          del2Doffset->Update();

          polys->DeepCopy(del2Doffset->GetOutput()->GetPolys());
          numoffsets++;
          }
        if (polys->GetNumberOfCells() != newpd->GetNumberOfPoints() - 2)
          {
          //If the offsets all failed, try last attempt with ear splitting
          triangulator->SetInputData(boundary);
          triangulator->Update();
          polys = triangulator->GetOutput()->GetPolys();

          splitCells->Delete();
          splitCells = nullptr;
          delete [] pointMapper;
          delete [] interPtBool;
          return splitCells;
          }
        }
      else
        {
        polys = del2D->GetOutput()->GetPolys();
        }

      // Renumber the point IDs.
      vtkIdType npts;
      const vtkIdType *ptIds;
      interLines->BuildLinks();
      for (polys->InitTraversal(); polys->GetNextCell(npts, ptIds);)
        {
        if (pointMapper[ptIds[0]] >= points->GetNumberOfPoints() ||
            pointMapper[ptIds[1]] >= points->GetNumberOfPoints() ||
            pointMapper[ptIds[2]] >= points->GetNumberOfPoints())
          {
          vtkGenericWarningMacro(<< "Invalid point ID!!!");
          }

        splitCells->InsertNextCell(npts);
        int interPtCount = 0;
        int interPts[3];
        for (int i = 0; i < npts; i++)
          {
          vtkIdType remappedPtId;
          if (pointMapper[ptIds[i]] < 3) // Point from the cell
            {
            remappedPtId = cellPts[ pointMapper[ptIds[i]] ];
            //If original cell point is also on intersecting lines
            if (CellPointOnInterLine[pointMapper[ptIds[i]]])
              {
              interPts[interPtCount++] =
                reverseLineIdMap[pointMapper[ptIds[i]] ];
              }
            }
          else  //If point is from intersection lines
            {
            remappedPtId = reverseIdMap[ pointMapper[ptIds[i]] ];
            interPts[interPtCount++] =
              reverseLineIdMap[ pointMapper[ptIds[i]] ];
            }
          splitCells->InsertCellPoint(remappedPtId);
          }
        if (interPtCount >= 2) //If there are more than two, inter line
          {
          //Add the information to new cell mapping on intersection lines
          this->AddToNewCellMap(inputIndex, interPtCount, interPts,
              interLines, numCurrCells);
          }
        numCurrCells++;
        }
      delete [] pointMapper;
      }
    }
  else  //Not (intersection lines and new points)
    {
#endif
    //Possible to have only additional point and not lines
    //Triangulate with delaunay2D
#if VTKSV_DELAUNAY_TYPE == OLD
    vtkNew( vtkDelaunay2D_60 , del2D);
#elif VTKSV_DELAUNAY_TYPE == TRIANGLE
    vtkNew( vtkDelaunay2D , del2D);
#else
    vtkNew( vtkDelaunay2D , del2D);
#endif
    del2D->SetInputData(fullpd);
    del2D->SetSourceData(fullpd);
    del2D->SetTolerance(0.0);
    del2D->SetAlpha(0.0);
    del2D->SetOffset(0);
    del2D->SetProjectionPlaneMode(VTK_SET_TRANSFORM_PLANE);
    del2D->SetTransform(transform);
    del2D->BoundingTriangulationOff();
    del2D->Update();
    vtkCellArray *polys = del2D->GetOutput()->GetPolys();

    // Renumber the point IDs.
    vtkIdType npts;
    const vtkIdType *ptIds;
    for (polys->InitTraversal(); polys->GetNextCell(npts, ptIds);)
      {
      if (ptIds[0] >= points->GetNumberOfPoints() ||
          ptIds[1] >= points->GetNumberOfPoints() ||
          ptIds[2] >= points->GetNumberOfPoints())
        {
        vtkGenericWarningMacro(<< "Invalid point ID!!!");
        }

      splitCells->InsertNextCell(npts);
      int interPtCount = 0;
      int interPts[3];
      for (int i = 0; i < npts; i++)
        {
        vtkIdType remappedPtId;
        if (ptIds[i] < 3) // Point from the cell
          {
          remappedPtId = cellPts[ ptIds[i] ];
          if (CellPointOnInterLine[ptIds[i]])
            {
            interPts[interPtCount++] = reverseLineIdMap[ptIds[i] ];
            }
          }
        else
          {
          remappedPtId = reverseIdMap[ ptIds[i] ];
          interPts[interPtCount++] = reverseLineIdMap[ptIds[i] ];
          }
        splitCells->InsertCellPoint(remappedPtId);
        }
      if (interPtCount >= 2)
        {
        this->AddToNewCellMap(inputIndex, interPtCount, interPts,
            interLines, numCurrCells);
        }
      numCurrCells++;
      }
#ifdef VTKSV_PREDELAUNAY_SPLIT
    }
#endif

  delete [] interPtBool;
  return splitCells;
}

// ----------------------
// Impl::AddToPointEdgeMap
// ----------------------
int vtkSVLoopIntersectionPolyDataFilter::Impl
::AddToPointEdgeMap(int index, vtkIdType ptId, double x[3], vtkPolyData *mesh,
                    vtkIdType cellId, vtkIdType edgeId, vtkIdType lineId,
                    vtkIdType triPtIds[3])
{
  int value = -1;
  vtkIdType edgePtId0 = triPtIds[edgeId];
  vtkIdType edgePtId1 = triPtIds[(edgeId+1) % 3];
  double pt0[3], pt1[3];

  mesh->GetPoint(edgePtId0, pt0);
  mesh->GetPoint(edgePtId1, pt1);

  // Check to see if this point-cell combo is already in the list
  PointEdgeMapIteratorType iterLower =
    this->PointEdgeMap[index]->lower_bound(ptId);
  PointEdgeMapIteratorType iterUpper =
    this->PointEdgeMap[index]->upper_bound(ptId);

  while (iterLower != iterUpper)
    {
    if (iterLower->second.CellId == cellId)
      {
      return iterLower->second.EdgeId;
      }
    ++iterLower;
    }

  double t, dist, closestPt[3];
  dist = vtkLine::DistanceToLine(x, pt0, pt1, t, closestPt);
  if (fabs(dist) < pow(this->Tolerance, 3) && t >= 0.0 && t <= 1.0)
    {
    CellEdgeLineType cellEdgeLine;
    cellEdgeLine.CellId = cellId;
    cellEdgeLine.EdgeId = edgeId;
    cellEdgeLine.LineId = lineId;
    this->PointEdgeMap[index]->insert(std::make_pair(ptId, cellEdgeLine));
    value = edgeId;
    }
  return value;
}

// ----------------------
// Impl::AddToNewCellMap
// ----------------------
/// \brief Add new cells to the mapping data array attached to the intersection lines
void vtkSVLoopIntersectionPolyDataFilter::Impl::AddToNewCellMap(
    int inputIndex, int interPtCount, int interPts[3],
    vtkPolyData *interLines, int numCurrCells)
{
  vtkIdList **cellIds;
  cellIds = new vtkIdList*[interPtCount];
  for (int i = 0; i < interPtCount; i++)
    {
    cellIds[i] = vtkIdList::New();
    vtkNew(vtkIdList, temp);
    interLines->GetPointCells(interPts[i], cellIds[i]);
    if (i > 0)
      {
      temp->DeepCopy(cellIds[i-1]);
      temp->IntersectWith(cellIds[i]);
      }
    if (temp->GetNumberOfIds() > 0)
      {
      //For each id
      for (int j = 0; j < temp->GetNumberOfIds(); j++)
        {
        //If it hasn't already been set
        if (this->NewCellIds[inputIndex]->GetComponent(temp->GetId(j), 0) == -1)
          {
          //Add to new cell mapping data array on intersection lines
          this->NewCellIds[inputIndex]->InsertComponent(temp->GetId(j),
              0, numCurrCells);
          }
        else
          {
          //Add to new cell mapping data array on intersection lines
          this->NewCellIds[inputIndex]->InsertComponent(temp->GetId(j),
              1, numCurrCells);
          }
        }
      }
    }
  //If number of intersection points is more than two, intersection line
  if (interPtCount > 2)
    {
    cellIds[0]->IntersectWith(cellIds[interPtCount-1]);
    if (cellIds[0]->GetNumberOfIds() > 0)
      {
      for (int j = 0;j < cellIds[0]->GetNumberOfIds(); j++)
        {
        if (this->NewCellIds[inputIndex]->
            GetComponent(cellIds[0]->GetId(j), 0) == -1)
          {
          //Add to new cell mapping data array on intersection lines
          this->NewCellIds[inputIndex]->InsertComponent(cellIds[0]->GetId(j),
              0, numCurrCells);
          }
        else
          {
          //Add to new cell mapping data array on intersection lines
          this->NewCellIds[inputIndex]->InsertComponent(cellIds[0]->GetId(j),
              1, numCurrCells);
          }
        }
      }
    }
  for (int i = 0; i < interPtCount; i++)
    {
    cellIds[i]->Delete();
    }
  delete [] cellIds;
}

// ----------------------
// Impl::GetLoops
// ----------------------
int vtkSVLoopIntersectionPolyDataFilter::Impl
::GetLoops(vtkPolyData *pd, std::vector<simPolygon> *loops)
{
  vtkNew(vtkIdList, pointCells);
  vtkNew(vtkIdList, cellPoints);
  simPoint nextPt;
  vtkIdType nextCell;
  bool *ptBool;
  ptBool = new bool[pd->GetNumberOfPoints()];
  bool *lineBool;
  lineBool = new bool[pd->GetNumberOfCells()];

  int numPoints = pd->GetNumberOfPoints();
  int numCells = pd->GetNumberOfCells();

  for (vtkIdType ptId = 0; ptId < numPoints; ptId++)
    {
    ptBool[ptId] = false;
    }
    vtkDebugWithObjectMacro(this->ParentFilter, <<"Number Of Cells: "<<numCells);
  for (vtkIdType lineId = 0; lineId < numCells; lineId++)
    {
    lineBool[lineId] = false;
    }

  //For each point in triangle and additional lines
  for (vtkIdType ptId = 0; ptId < numPoints; ptId++)
    {
    //if the point hasn't already been touch and put in a loop
    if (ptBool[ptId] == false)
      {
      nextPt.id = ptId;
      pd->GetPoint(nextPt.id, nextPt.pt);
      simPolygon interloop;
      interloop.points.push_back(nextPt);

      ptBool[nextPt.id] = true;
      pd->GetPointCells(nextPt.id, pointCells);
      nextCell = pointCells->GetId(0);
      lineBool[nextCell] = true;

      //Get one loop for untouched point
      if (this->GetSingleLoop(pd, &interloop, nextCell, ptBool, lineBool) != SV_OK)
        {
        delete [] ptBool;
        delete [] lineBool;
        return SV_ERROR;
        }
      //Add new loop
      loops->push_back(interloop);
      }
    }
  //Check now for untouched lines, possible to still have
  for (vtkIdType lineId = 0; lineId <pd->GetNumberOfCells(); lineId++)
    {
    if (lineBool[lineId] == false)
      {
      vtkDebugWithObjectMacro(this->ParentFilter, <<"LINE FALSE: Find extra loop/s");
      pd->GetCellPoints(lineId, cellPoints);
      nextPt.id = cellPoints->GetId(0);
      pd->GetPoint(nextPt.id, nextPt.pt);
      simPolygon interloop;
      interloop.points.push_back(nextPt);

      lineBool[lineId] = true;
      nextCell = lineId;

      //Get single loop if the line is still untouched
      if (this->GetSingleLoop(pd, &interloop, nextCell, ptBool, lineBool) != SV_OK)
        {
        delete [] ptBool;
        delete [] lineBool;
        return SV_ERROR;
        }
      //Add new loop to loops
      loops->push_back(interloop);
      }
    }

  delete [] ptBool;
  delete [] lineBool;

  return SV_OK;
}

// ----------------------
// Impl::GetSingleLoop
// ----------------------
int vtkSVLoopIntersectionPolyDataFilter::Impl
::GetSingleLoop(vtkPolyData *pd, simPolygon *loop, vtkIdType nextCell,
    bool *interPtBool, bool *lineBool)
{
  int intertype = 0;
  vtkNew(vtkIdList, pointCells);
  vtkNew(vtkIdList, cellPoints);

  //Set up next and next cell values
  vtkIdType nextPt = loop->points.front().id;
  vtkIdType startPt = nextPt;
  interPtBool[nextPt] = true;
  pd->GetCellPoints(nextCell, cellPoints);

  simPoint newpoint;
  vtkIdType prevPt = nextPt;
  //Find next point by following line and choosing point that is not already
  //being used
  if (cellPoints->GetId(0) == nextPt)
    {
    newpoint.id = cellPoints->GetId(1);
    nextPt = cellPoints->GetId(1);
    }
  else
    {
    newpoint.id = cellPoints->GetId(0);
    nextPt = cellPoints->GetId(0);
    }
  pd->GetPoint(newpoint.id, newpoint.pt);
  loop->points.push_back(newpoint);
  interPtBool[nextPt] = true;

  //Loop until we get back to the point we started at, completing the loop!
  while (nextPt != startPt)
    {
    pd->GetPointCells(nextPt, pointCells);
    //There are multiple lines attached to this point; must figure out
    //the correct way to go
    if (pointCells->GetNumberOfIds() > 2)
      {
      //This is the first intersection. Find line of minimum angle and
      //set the orientation of the loop (i.e. CW or CCW)
      if (intertype == 0)
        {
        this->SetLoopOrientation(pd, loop, &nextCell, nextPt, prevPt, pointCells);
        intertype = 1;
        }
      //This is not the first intersection. Follow line that continues along
      //the set loop orientation
      else
        {
        if (this->FollowLoopOrientation(pd, loop, &nextCell, nextPt, prevPt,
              pointCells) != SV_OK)
          {
          return SV_ERROR;
          }
        }
      }
    //There is one line attached to point. This means the intersection has
    //an open intersection loop (i.e. the surfaces are open and one does not
    //completeley intersect the other.
    //Make an artificial triangle loop in this case
    else if (pointCells->GetNumberOfIds() < 2)
      {
      vtkNew(vtkPolyData, currentpd);
      vtkNew(vtkCellArray, currentcells);
      currentcells = pd->GetLines();
      currentcells->InsertNextCell(2);
      currentcells->InsertCellPoint(nextPt);
      currentcells->InsertCellPoint(startPt);
      nextCell = currentcells->GetNumberOfCells()-1;
      currentpd->SetLines(currentcells);
      currentpd->SetPoints(pd->GetPoints());
      pd->DeepCopy(currentpd);
      pd->BuildLinks();
      }
    //Normal number of lines, simply follow around triangle loop
    else
      {
      if (pointCells->GetId(0) == nextCell)
        {
        nextCell = pointCells->GetId(1);
        }
      else
        {
        nextCell = pointCells->GetId(0);
        }
      }
    lineBool[nextCell] = true;

    prevPt = nextPt;
    pd->GetCellPoints(nextCell, cellPoints);
    simPoint internewpoint;
    if (cellPoints->GetId(0) == nextPt)
      {
      internewpoint.id = cellPoints->GetId(1);
      nextPt = cellPoints->GetId(1);
      }
    else
      {
      internewpoint.id = cellPoints->GetId(0);
      nextPt = cellPoints->GetId(0);
      }
    pd->GetPoint(internewpoint.id, internewpoint.pt);
    loop->points.push_back(internewpoint);
    interPtBool[nextPt] = true;
    }
  //Cell is boring; i.e. it only has boundary points. set the orientation
  if (intertype == 0)
    {
    nextPt = 0;
    pd->GetPointCells(nextPt, pointCells);
    nextCell = pointCells->GetId(0);
    pd->GetCellPoints(pointCells->GetId(1), cellPoints);
    if (cellPoints->GetId(0) == nextPt)
      {
      prevPt = cellPoints->GetId(1);
      }
    else
      {
      prevPt = cellPoints->GetId(0);
      }

    loop->orientation = this->GetLoopOrientation(pd, nextCell, prevPt, nextPt);
    }
  return SV_OK;
}

// ----------------------
// Impl::FollowLoopOrientation
// ----------------------
int vtkSVLoopIntersectionPolyDataFilter::Impl
::FollowLoopOrientation(vtkPolyData *pd, simPolygon *loop, vtkIdType *nextCell,
    vtkIdType nextPt, vtkIdType prevPt, vtkIdList *pointCells)
{
  //Follow the orientation of this loop
  int foundcell = 0;
  double newcell = 0;
  double minangle = VTK_DOUBLE_MAX;
  for (vtkIdType i = 0; i < pointCells->GetNumberOfIds(); i++)
    {
    vtkIdType cellId = pointCells->GetId(i);
    if (*nextCell != cellId)
      {
      //Get orientation for newly selected line
      int neworient = this->GetLoopOrientation(pd, cellId, prevPt, nextPt);

      //If the orientation of the newly selected line is correct, check
      //the angle of this it will make with the previous line
      if (neworient == loop->orientation)
        {
        foundcell = 1;
        double l0pt0[3], l0pt1[3], l1pt0[3], l1pt1[3];
        pd->GetPoint(prevPt, l0pt0);
        pd->GetPoint(nextPt, l0pt1);
        vtkNew(vtkIdList, specialCellPoints);
        pd->GetCellPoints(cellId, specialCellPoints);
        if (specialCellPoints->GetId(0) == nextPt)
          {
          pd->GetPoint(specialCellPoints->GetId(1), l1pt0);
          pd->GetPoint(specialCellPoints->GetId(0), l1pt1);
          }
        else
          {
          pd->GetPoint(specialCellPoints->GetId(0), l1pt0);
          pd->GetPoint(specialCellPoints->GetId(1), l1pt1);
          }
        double edge1[3], edge2[3];
        for (int j = 0; j < 2; j++)
          {
          edge1[j] = l0pt1[j]-l0pt0[j];
          edge2[j] = l1pt1[j]-l1pt0[j];
          }
        edge1[2] = 0.;
        edge2[2] = 0.;
        vtkMath::Normalize(edge1);
        vtkMath::Normalize(edge2);
        double angle =
          vtkMath::DegreesFromRadians(acos(vtkMath::Dot(edge1, edge2)));
        if (angle < minangle)
          {
          minangle = angle;
          newcell = cellId;
          }
        }
      }
    }
  if (foundcell == 0)
    {
    vtkWarningWithObjectMacro(this->ParentFilter, << "No cell with correct orientation found");
    return SV_ERROR;
    }

  //Set the next line to follow equal to the line that follows the
  //orientation of the loop and has the minimum angle. Angle check is
  //necessary because it is possible to have more than one line that follow
  //the loop orientation
  *nextCell = newcell;
  return SV_OK;
}

// ----------------------
// Impl::SetLoopOrientation
// ----------------------
void vtkSVLoopIntersectionPolyDataFilter::Impl
::SetLoopOrientation(vtkPolyData *pd, simPolygon *loop, vtkIdType *nextCell,
    vtkIdType nextPt, vtkIdType prevPt, vtkIdList *pointCells)
{
  //Set the orientation of this loop!
  double mincell = 0;
  double minangle = VTK_DOUBLE_MAX;
  for (vtkIdType i = 0; i < pointCells->GetNumberOfIds(); i++)
    {
    vtkIdType cellId = pointCells->GetId(i);
    //If the next line is not equal to the current line, check the angle
    //it makes with the previous line
    if (*nextCell != cellId)
      {
      double l0pt0[3], l0pt1[3], l1pt0[3], l1pt1[3];
      pd->GetPoint(prevPt, l0pt0);
      pd->GetPoint(nextPt, l0pt1);
      vtkNew(vtkIdList, specialCellPoints);
      pd->GetCellPoints(cellId, specialCellPoints);
      if (specialCellPoints->GetId(0) == nextPt)
        {
        pd->GetPoint(specialCellPoints->GetId(1), l1pt0);
        pd->GetPoint(specialCellPoints->GetId(0), l1pt1);
        }
      else
        {
        pd->GetPoint(specialCellPoints->GetId(0), l1pt0);
        pd->GetPoint(specialCellPoints->GetId(1), l1pt1);
        }
      double edge1[3], edge2[3];
      for (int j = 0; j < 2; j++)
        {
        edge1[j] = l0pt1[j] - l0pt0[j];
        edge2[j] = l1pt1[j] - l1pt0[j];
        }
      edge1[2] = 0.;
      edge2[2] = 0.;
      vtkMath::Normalize(edge1);
      vtkMath::Normalize(edge2);
      double angle =
        vtkMath::DegreesFromRadians(acos(vtkMath::Dot(edge1, edge2)));

      if (angle < minangle)
        {
        minangle = angle;
        mincell = cellId;
        }
      }
    }
  //Set the next line as the line that makes the minimum angle with the
  //previous cell and set the orientation of the loop
  *nextCell = mincell;
  loop->orientation = this->GetLoopOrientation(pd, *nextCell, prevPt, nextPt);
}

// ----------------------
// Impl::GetLoopOrientation
// ----------------------
int vtkSVLoopIntersectionPolyDataFilter::Impl::GetLoopOrientation(
    vtkPolyData *pd, vtkIdType cell, vtkIdType ptId1, vtkIdType ptId2)
{
  //Calculate the actual orientation of this loop, by calculating the signed
  //area of the triangle made by the three points
  vtkNew(vtkIdList, cellPoints);
  pd->GetCellPoints(cell, cellPoints);

  vtkIdType ptId3;
  if (cellPoints->GetId(0) == ptId2)
    {
    ptId3 = cellPoints->GetId(1);
    }
  else
    {
    ptId3 = cellPoints->GetId(0);
    }

  double pt1[3], pt2[3], pt3[3];
  pd->GetPoint(ptId1, pt1);
  pd->GetPoint(ptId2, pt2);
  pd->GetPoint(ptId3, pt3);

  double area = 0;
  area = area + (pt1[0]*pt2[1])-(pt2[0]*pt1[1]);
  area = area + (pt2[0]*pt3[1])-(pt3[0]*pt2[1]);
  area = area + (pt3[0]*pt1[1])-(pt1[0]*pt3[1]);

  int orientation = 1;

  if (fabs(area) < 1e-10)
    {
    //The area is very small for these three based upon the transformed pd
    //from the cells original three points. Get a new transform from these
    //interior three points to make sure the area is correct
    vtkDebugWithObjectMacro(this->ParentFilter, <<"Very Small Area Triangle");
    vtkDebugWithObjectMacro(this->ParentFilter, <<"Double check area with more accurate transform");
    vtkNew(vtkPoints, testPoints);
    vtkNew(vtkPolyData, testPD);
    vtkNew(vtkCellArray, testCells);
    testPoints->InsertNextPoint(this->SplittingPD->GetPoint(ptId1));
    testPoints->InsertNextPoint(this->SplittingPD->GetPoint(ptId2));
    testPoints->InsertNextPoint(this->SplittingPD->GetPoint(ptId3));
    for (int i = 0; i < 3; i++)
      {
      testCells->InsertNextCell(2);
      testCells->InsertCellPoint(i);
      testCells->InsertCellPoint((i+1)%3);
      }
    testPD->SetPoints(testPoints);
    testPD->SetLines(testCells);
    testPD->BuildLinks();

    vtkNew(vtkTransform, newTransform);
    int sign = this->GetTransform(newTransform, testPoints);
    if (sign != this->TransformSign)
      {
      testPoints->SetPoint(0, this->SplittingPD->GetPoint(ptId2));
      testPoints->SetPoint(1, this->SplittingPD->GetPoint(ptId1));
      this->GetTransform(newTransform, testPoints);
      testPoints->SetPoint(0, this->SplittingPD->GetPoint(ptId1));
      testPoints->SetPoint(1, this->SplittingPD->GetPoint(ptId2));
      }

    vtkNew(vtkTransformPolyDataFilter, newTransformer);
    newTransformer->SetInputData(testPD);
    newTransformer->SetTransform(newTransform);
    newTransformer->Update();

    newTransformer->GetOutput()->GetPoint(0, pt1);
    newTransformer->GetOutput()->GetPoint(1, pt2);
    newTransformer->GetOutput()->GetPoint(2, pt3);

    vtkDebugWithObjectMacro(this->ParentFilter, <<"Area was: "<<area);
    area = 0;
    area = area + (pt1[0]*pt2[1])-(pt2[0]*pt1[1]);
    area = area + (pt2[0]*pt3[1])-(pt3[0]*pt2[1]);
    area = area + (pt3[0]*pt1[1])-(pt1[0]*pt3[1]);
    vtkDebugWithObjectMacro(this->ParentFilter, <<"Corrected area is: "<<area);
    }
  if (area < 0)
    {
    orientation = -1;
    }

  return orientation;
}

// ----------------------
// Impl::Orient
// ----------------------
void vtkSVLoopIntersectionPolyDataFilter::Impl
::Orient(vtkPolyData *pd, vtkTransform *transform, vtkPolyData *boundary,
                vtkPolygon *boundarypoly)
{
  //Orient this loop in a counter clockwise direction in preperation for
  //cell splitting. For delaunay2d, the polygon should be in CCW order, but
  //also for ear clipping method, it is nice to have also in CCW order.
  vtkNew(vtkTransformPolyDataFilter, transformer);
  vtkNew(vtkPolyData, transformedpd);

  transformer->SetInputData(pd);
  transformer->SetTransform(transform);
  transformer->Update();
  transformedpd = transformer->GetOutput();

  double area = 0;
  double tedgept1[3];
  double tedgept2[3];
  vtkIdType nextPt;
  for (nextPt = 0; nextPt < pd->GetNumberOfPoints() - 1; nextPt++)
    {
    transformedpd->GetPoint(nextPt, tedgept1);
    transformedpd->GetPoint(nextPt+1, tedgept2);
    area = area + (tedgept1[0]*tedgept2[1])-(tedgept2[0]*tedgept1[1]);
    }
  transformedpd->GetPoint(nextPt, tedgept1);
  transformedpd->GetPoint(0, tedgept2);
  area = area + (tedgept1[0]*tedgept2[1])-(tedgept2[0]*tedgept1[1]);

  if (area < 0)
    {
    for (nextPt = pd->GetNumberOfPoints() - 1; nextPt > -1; nextPt--)
      {
      boundarypoly->GetPointIds()->InsertNextId(nextPt);
      }
    }
  else
    {
    for (nextPt = 0; nextPt < pd->GetNumberOfPoints(); nextPt++)
      {
      boundarypoly->GetPointIds()->InsertNextId(nextPt);
      }
    }
  vtkNew(vtkCellArray, cellarray);
  cellarray->InsertNextCell(boundarypoly);
  boundary->SetPoints(pd->GetPoints());
  boundary->SetPolys(cellarray);
}

// ----------------------
// Impl::CheckLine
// ----------------------
/// \brief Check to make sure the line is unique
int vtkSVLoopIntersectionPolyDataFilter::Impl::CheckLine(
    vtkPolyData *pd, vtkIdType ptId1, vtkIdType ptId2)
{
  vtkNew(vtkIdList, pointCells1);
  vtkNew(vtkIdList, pointCells2);

  pd->GetPointCells(ptId1, pointCells1);
  pd->GetPointCells(ptId2, pointCells2);

  pointCells1->IntersectWith(pointCells2);

  int unique = 1;
  if (pointCells1->GetNumberOfIds() > 0)
    {
    unique = 0;
    }

  return unique;
}

// ----------------------
// Impl::GetTransform
// ----------------------
int vtkSVLoopIntersectionPolyDataFilter::Impl::GetTransform(
    vtkTransform *transform, vtkPoints *points)
{
  double zaxis[3] = {0, 0, 1};
  double rotationAxis[3], normal[3], center[3], rotationAngle;

  double pt0[3], pt1[3], pt2[3];
  points->GetPoint(0, pt0);
  points->GetPoint(1, pt1);
  points->GetPoint(2, pt2);
  vtkTriangle::ComputeNormal(pt0, pt1, pt2, normal);

  double dotZAxis = vtkMath::Dot(normal, zaxis);
  if (fabs(1.0 - dotZAxis) < 1e-6)
    {
    // Aligned with z-axis
    rotationAxis[0] = 1.0;
    rotationAxis[1] = 0.0;
    rotationAxis[2] = 0.0;
    rotationAngle = 0.0;
    }
  else if (fabs(1.0 + dotZAxis) < 1e-6)
    {
    // Co-linear with z-axis, but reversed sense.
    // Aligned with z-axis
    rotationAxis[0] = 1.0;
    rotationAxis[1] = 0.0;
    rotationAxis[2] = 0.0;
    rotationAngle = 180.0;
    }
  else
    {
    // The general case
    vtkMath::Cross(normal, zaxis, rotationAxis);
    vtkMath::Normalize(rotationAxis);
    rotationAngle =
      vtkMath::DegreesFromRadians(acos(vtkMath::Dot(zaxis, normal)));
    }

  transform->PreMultiply();
  transform->Identity();

  transform->RotateWXYZ(rotationAngle,
                        rotationAxis[0],
                        rotationAxis[1],
                        rotationAxis[2]);

  vtkTriangle::TriangleCenter(pt0, pt1, pt2, center);
  transform->Translate(-center[0], -center[1], -center[2]);

  int zaxisdotsign = 1;
  if (dotZAxis < 0)
    {
    zaxisdotsign = -1;
    }

  return zaxisdotsign;
}

//----------------------------------------------------------------------------
vtkStandardNewMacro(vtkSVLoopIntersectionPolyDataFilter);

//----------------------------------------------------------------------------
vtkSVLoopIntersectionPolyDataFilter::vtkSVLoopIntersectionPolyDataFilter()
  : SplitFirstOutput(1), SplitSecondOutput(1)
{
  this->SetNumberOfInputPorts(2);
  this->SetNumberOfOutputPorts(3);

  this->NumberOfIntersectionPoints = 0;
  this->NumberOfIntersectionLines = 0;

  this->CheckMesh = 1;
  this->CheckInput = 0;
  this->Status = 1;
  this->ComputeIntersectionPointArray = 0;
  this->Tolerance = 1e-6;
}

//----------------------------------------------------------------------------
vtkSVLoopIntersectionPolyDataFilter::~vtkSVLoopIntersectionPolyDataFilter()
{
}

//----------------------------------------------------------------------------
void vtkSVLoopIntersectionPolyDataFilter::PrintSelf(ostream &os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "NumberOfIntersectionPoints: " <<
          this->NumberOfIntersectionPoints << "\n";
  os << indent << "NumberOfIntersectionLines: " <<
          this->NumberOfIntersectionLines << "\n";

  os << indent << "SplitFirstOutput: " << this->SplitFirstOutput << "\n";
  os << indent << "SplitSecondOutput: " << this->SplitSecondOutput << "\n";
  os << indent << "CheckMesh: " << this->CheckMesh << "\n";
  os << indent << "Status: " << this->CheckMesh << "\n";
  os << indent << "ComputeIntersectionPointArray: " <<
          this->ComputeIntersectionPointArray << "\n";
  os << indent << "Tolerance: " <<
          this->Tolerance << "\n";
}

//----------------------------------------------------------------------------
int vtkSVLoopIntersectionPolyDataFilter::TriangleTriangleIntersection(
                                        double p1[3], double q1[3],
                                        double r1[3], double p2[3],
                                        double q2[3], double r2[3],
                                        int &coplanar, double pt1[3],
                                        double pt2[3], double surfaceid[2],
                                        double tolerance)
{
  double n1[3], n2[3];

  // Compute supporting plane normals.
  vtkTriangle::ComputeNormal(p1, q1, r1, n1);
  vtkTriangle::ComputeNormal(p2, q2, r2, n2);
  double s1 = -vtkMath::Dot(n1, p1);
  double s2 = -vtkMath::Dot(n2, p2);

  // Compute signed distances of points p1, q1, r1 from supporting
  // plane of second triangle.
  double dist1[3];
  dist1[0] = vtkMath::Dot(n2, p1) + s2;
  dist1[1] = vtkMath::Dot(n2, q1) + s2;
  dist1[2] = vtkMath::Dot(n2, r1) + s2;

  // If signs of all points are the same, all the points lie on the
  // same side of the supporting plane, and we can exit early.
  if ((dist1[0]*dist1[1] > tolerance) && (dist1[0]*dist1[2] > tolerance))
    {
    //vtkDebugMacro(<<"Same side supporting plane 1!");
    return SV_ERROR;
    }
  // Do the same for p2, q2, r2 and supporting plane of first
  // triangle.
  double dist2[3];
  dist2[0] = vtkMath::Dot(n1, p2) + s1;
  dist2[1] = vtkMath::Dot(n1, q2) + s1;
  dist2[2] = vtkMath::Dot(n1, r2) + s1;

  // If signs of all points are the same, all the points lie on the
  // same side of the supporting plane, and we can exit early.
  if ((dist2[0]*dist2[1] > tolerance) && (dist2[0]*dist2[2] > tolerance))
    {
    //vtkDebugMacro(<<"Same side supporting plane 2!");
    return SV_ERROR;
    }
  // Check for coplanarity of the supporting planes.
  if (fabs(n1[0] - n2[0]) < 1e-9 &&
       fabs(n1[1] - n2[1]) < 1e-9 &&
       fabs(n1[2] - n2[2]) < 1e-9 &&
       fabs(s1 - s2) < 1e-9)
    {
    coplanar = 1;
    //vtkDebugMacro(<<"Coplanar!");
    return SV_ERROR;
    }

  coplanar = 0;

  // There are more efficient ways to find the intersection line (if
  // it exists), but this is clear enough.
  double *pts1[3] = {p1, q1, r1}, *pts2[3] = {p2, q2, r2};

  // Find line of intersection (L = p + t*v) between two planes.
  double n1n2 = vtkMath::Dot(n1, n2);
  double a = (s1 - s2*n1n2) / (n1n2*n1n2 - 1.0);
  double b = (s2 - s1*n1n2) / (n1n2*n1n2 - 1.0);
  double p[3], v[3];
  p[0] = a*n1[0] + b*n2[0];
  p[1] = a*n1[1] + b*n2[1];
  p[2] = a*n1[2] + b*n2[2];
  vtkMath::Cross(n1, n2, v);
  vtkMath::Normalize(v);

  int index1 = 0, index2 = 0;
  double t1[3], t2[3];
  int ts1=50, ts2=50;
  for (int i = 0; i < 3; i++)
    {
    double t, x[3];
    int id1 = i, id2 = (i+1) % 3;

    // Find t coordinate on line of intersection between two planes.
    double val1 = vtkSVLoopIntersectionPolyDataFilter::Impl::IntersectPlaneWithLine(
        pts1[id1], pts1[id2], n2, p2, t, x);
    if (val1 == 1 ||
        (t > (0-tolerance) && t < (1+tolerance)))
      {
         if (t < 1+tolerance && t > 1-tolerance)
           {
           ts1 = index1;
           }

         t1[index1++] = vtkMath::Dot(x, v) - vtkMath::Dot(p, v);
      }

    double val2 = vtkSVLoopIntersectionPolyDataFilter::Impl::IntersectPlaneWithLine(
        pts2[id1], pts2[id2], n1, p1, t, x);
    if (val2 == 1 ||
        (t > (0-tolerance) && t < (1+tolerance)))
      {
        if (t < 1+tolerance && t > 1-tolerance)
          {
          ts2 = index2;
          }

        t2[index2++] = vtkMath::Dot(x, v) - vtkMath::Dot(p, v);
      }
    }

  //If the value of the index is greater than 2, the intersecting point
  //actually is intersected by all three edges. In this case, set the two
  //edges to the two edges where the intersecting point is not the end point
  if (index1 > 2)
    {
    index1--;
    std::swap(t1[ts1], t1[2]);
    }
  if (index2 > 2)
    {
    index2--;
    std::swap(t2[ts2], t2[2]);
    }
  // Check if only one edge or all edges intersect the supporting
  // planes intersection.
  if (index1 != 2 || index2 != 2)
    {
    //vtkDebugMacro(<<"Only one edge intersecting!");
    return SV_ERROR;
    }

  // Check for NaNs
  if (vtkMath::IsNan(t1[0]) || vtkMath::IsNan(t1[1]) ||
      vtkMath::IsNan(t2[0]) || vtkMath::IsNan(t2[1]))
    {
    //vtkWarningMacro(<<"NaNs!");
    return SV_ERROR;
    }

  if (t1[0] > t1[1])
    {
    std::swap(t1[0], t1[1]);
    }
  if (t2[0] > t2[1])
    {
    std::swap(t2[0], t2[1]);
    }
  // Handle the different interval configuration cases.
  double tt1, tt2;
  if (t1[1] < t2[0] || t2[1] < t1[0])
    {
    //vtkDebugMacro(<<"No Overlap!");
    return SV_ERROR; // No overlap
    }
  else if (t1[0] < t2[0])
    {
    if (t1[1] < t2[1])
      {
      //First point on surface 2, second point on surface 1
      surfaceid[0] = 2;
      surfaceid[1] = 1;
      tt1 = t2[0];
      tt2 = t1[1];
      }
    else
      {
      //Both points belong to lines on surface 2
      surfaceid[0] = 2;
      surfaceid[1] = 2;
      tt1 = t2[0];
      tt2 = t2[1];
      }
    }
  else // t1[0] >= t2[0]
    {
    if (t1[1] < t2[1])
      {
      //Both points belong to lines on surface 1
      surfaceid[0] = 1;
      surfaceid[1] = 1;
      tt1 = t1[0];
      tt2 = t1[1];
      }
    else
      {
      //First point on surface 1, second point on surface 2
      surfaceid[0] = 1;
      surfaceid[1] = 2;
      tt1 = t1[0];
      tt2 = t2[1];
      }
    }

  // Create actual intersection points.
  pt1[0] = p[0] + tt1*v[0];
  pt1[1] = p[1] + tt1*v[1];
  pt1[2] = p[2] + tt1*v[2];

  pt2[0] = p[0] + tt2*v[0];
  pt2[1] = p[1] + tt2*v[1];
  pt2[2] = p[2] + tt2*v[2];

  return SV_OK;
}

void vtkSVLoopIntersectionPolyDataFilter::CleanAndCheckSurface(vtkPolyData *pd,
    double stats[2], double tolerance)
{
  int badEdges = 0;
  int freeEdges = 0;
  vtkNew(vtkCleanPolyData, cleaner);
  vtkNew(vtkIntArray, bad);
  vtkNew(vtkIntArray, freeedge);
  vtkNew(vtkIdList, edgeneighbors);

  //Clean the input surface
  cleaner->SetInputData(pd);
  cleaner->ToleranceIsAbsoluteOn();
  cleaner->SetAbsoluteTolerance(tolerance);
  cleaner->Update();
  pd->DeepCopy(cleaner->GetOutput());
  pd->BuildLinks();

  //Loop through the surface and find edges with cells that have either more
  //than one neighbor or no neighbors. No neighbors can be okay,as this can
  //indicate a free edge. However, for a polydata surface, multiple neighbors
  //indicates a bad cell with possible intersecting facets!
  for (int i = 0; i < pd->GetNumberOfCells(); i++)
    {
    const vtkIdType *pts;
    vtkIdType npts = 0;
    pd->GetCellPoints(i, npts, pts);
    int badcell = 0;
    int freeedgecell = 0;
    for (int j = 0; j < npts; j++)
      {
      vtkIdType p0 = pts[j];
      vtkIdType p1 = pts[(j+1)%npts];

      pd->GetCellEdgeNeighbors(i, p0, p1, edgeneighbors);
      if (edgeneighbors->GetNumberOfIds() > 1)
        {
        badEdges++;
        badcell++;
        }
      else if (edgeneighbors->GetNumberOfIds() < 1)
        {
        freeEdges++;
        freeedgecell++;
        }

      }
    bad->InsertValue(i, badcell);
    freeedge->InsertValue(i, freeedgecell);
    }

  bad->SetName("BadTriangle");
  pd->GetCellData()->AddArray(bad);

  freeedge->SetName("FreeEdge");
  pd->GetCellData()->AddArray(freeedge);

  stats[0] = freeEdges;
  stats[1] = badEdges;
}

void vtkSVLoopIntersectionPolyDataFilter::CleanAndCheckInput(vtkPolyData *pd,
    double tolerance)
{
  vtkNew(vtkCleanPolyData, cleaner);
  vtkNew(vtkTriangleFilter, triangulator);
  vtkNew(vtkPolyDataNormals, normaler);

  //vtkDebugMacro(<<"Cleaning");
  cleaner->SetInputData(pd);
  cleaner->ToleranceIsAbsoluteOn();
  cleaner->SetAbsoluteTolerance(tolerance);
  cleaner->Update();
  //vtkDebugMacro(<<"Triangulating");
  triangulator->SetInputData(cleaner->GetOutput());
  triangulator->Update();
  //vtkDebugMacro(<<"Getting Normals");
  normaler->SetInputData(triangulator->GetOutput());
  normaler->AutoOrientNormalsOn();
  normaler->SplittingOff();
  normaler->ComputeCellNormalsOn();
  normaler->Update();

  vtkIdType npts;
  const vtkIdType *cellPts;
  double pt0[3], pt1[3], pt2[3];
  normaler->GetOutput()->GetPolys()->GetCell(0, npts, cellPts);
  normaler->GetOutput()->GetPoints()->GetPoint(cellPts[0], pt0);
  normaler->GetOutput()->GetPoints()->GetPoint(cellPts[1], pt1);
  normaler->GetOutput()->GetPoints()->GetPoint(cellPts[2], pt2);

  double v1[3], v2[3], cellNorm[3];
  for (int i = 0; i < 3; i++)
    {
    v1[i] = pt1[i] - pt0[i];
    v2[i] = pt2[i] - pt1[i];
    }
  vtkMath::Cross(v1, v2, cellNorm);

  double arrayNormal[3];
  //vtkDebugMacro(<<"Getting Normal Array");
  normaler->GetOutput()->GetCellData()->GetNormals("Normals")->GetTuple(0,
      arrayNormal);
}

//----------------------------------------------------------------------------
int vtkSVLoopIntersectionPolyDataFilter::RequestData(
                                        vtkInformation* vtkNotUsed(request),
                                        vtkInformationVector** inputVector,
                                        vtkInformationVector*  outputVector)
{
  vtkInformation* inInfo0 = inputVector[0]->GetInformationObject(0);
  vtkInformation* inInfo1 = inputVector[1]->GetInformationObject(0);
  vtkInformation* outIntersectionInfo =
    outputVector->GetInformationObject(0);
  vtkInformation* outPolyDataInfo0 =
    outputVector->GetInformationObject(1);
  vtkInformation* outPolyDataInfo1 =
    outputVector->GetInformationObject(2);

  vtkPolyData *input0 = vtkPolyData::SafeDownCast(
    inInfo0->Get(vtkDataObject::DATA_OBJECT()));

  vtkPolyData *input1 = vtkPolyData::SafeDownCast(
    inInfo1->Get(vtkDataObject::DATA_OBJECT()));

  if (this->CheckInput)
    {
    vtkDebugMacro(<<"Checking Input 0");
    this->CleanAndCheckInput(input0, this->Tolerance);
    vtkDebugMacro(<<"Checking Input 1");
    this->CleanAndCheckInput(input1, this->Tolerance);
    }

  vtkPolyData *outputIntersection = vtkPolyData::SafeDownCast(
    outIntersectionInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkNew( vtkPoints, outputIntersectionPoints);
  outputIntersection->SetPoints(outputIntersectionPoints);

  vtkPolyData *outputPolyData0 = vtkPolyData::SafeDownCast(
    outPolyDataInfo0->Get(vtkDataObject::DATA_OBJECT()));

  vtkPolyData *outputPolyData1 = vtkPolyData::SafeDownCast(
    outPolyDataInfo1->Get(vtkDataObject::DATA_OBJECT()));

  // Set up new poly data for the inputs to build cells and links.
  vtkNew(vtkPolyData , mesh0);
  mesh0->DeepCopy(input0);

  vtkNew(vtkPolyData , mesh1);
  mesh1->DeepCopy(input1);

  // Find the triangle-triangle intersections between mesh0 and mesh1
  vtkNew(vtkOBBTree , obbTree0);
  obbTree0->SetDataSet(mesh0);
  obbTree0->SetNumberOfCellsPerNode(10);
  obbTree0->SetMaxLevel(1000000);
  obbTree0->SetTolerance(this->Tolerance);
  obbTree0->AutomaticOn();
  obbTree0->BuildLocator();

  vtkNew(vtkOBBTree , obbTree1);
  obbTree1->SetDataSet(mesh1);
  obbTree1->SetNumberOfCellsPerNode(10);
  obbTree1->SetMaxLevel(1000000);
  obbTree1->SetTolerance(this->Tolerance);
  obbTree1->AutomaticOn();
  obbTree1->BuildLocator();

  // Set up the structure for determining exact triangle-triangle
  // intersections.
  vtkSVLoopIntersectionPolyDataFilter::Impl *impl =
    new vtkSVLoopIntersectionPolyDataFilter::Impl();
  impl->ParentFilter = this;
  impl->Mesh[0]  = mesh0;
  impl->Mesh[1]  = mesh1;
  impl->OBBTree1 = obbTree1;
  impl->Tolerance = this->Tolerance;

  vtkNew(vtkCellArray , lines);
  outputIntersection->SetLines(lines);
  impl->IntersectionLines = lines;

  // Add cell data arrays that map the intersection line to the cells
  // it splits.
  impl->CellIds[0] = vtkIdTypeArray::New();
  impl->CellIds[0]->SetName("Input0CellID");
  outputIntersection->GetCellData()->AddArray(impl->CellIds[0]);
  impl->CellIds[0]->Delete();
  impl->CellIds[1] = vtkIdTypeArray::New();
  impl->CellIds[1]->SetName("Input1CellID");
  outputIntersection->GetCellData()->AddArray(impl->CellIds[1]);
  impl->CellIds[1]->Delete();

  impl->PointCellIds[0] = vtkIdTypeArray::New();
  impl->PointCellIds[0]->SetName("PointCellsIDs");
  impl->PointCellIds[1] = vtkIdTypeArray::New();
  impl->PointCellIds[1]->SetName("PointCellsIDs");

  impl->SurfaceId = vtkIdTypeArray::New();
  impl->SurfaceId->SetName("SurfaceID");
  outputIntersection->GetPointData()->AddArray(impl->SurfaceId);

  impl->NewCellIds[0] = vtkIdTypeArray::New();
  impl->NewCellIds[0]->SetNumberOfComponents(2);
  impl->NewCellIds[1] = vtkIdTypeArray::New();
  impl->NewCellIds[1]->SetNumberOfComponents(2);

  double bounds0[6], bounds1[6];
  mesh0->GetBounds(bounds0);
  mesh1->GetBounds(bounds1);
  for (int i = 0; i < 3; i++)
    {
    int minIdx = 2*i;
    int maxIdx = 2*i+1;
    if (bounds1[minIdx] < bounds0[minIdx])
      {
      bounds0[minIdx] = bounds1[minIdx];
      }
    if (bounds1[maxIdx] > bounds0[maxIdx])
      {
      bounds0[maxIdx] = bounds1[maxIdx];
      }
    }

  //Set up the point merger for insertion of points into the intersection
  //lines. Tolerance is set to 1e-6
  vtkNew(vtkPointLocator , pointMerger);
  pointMerger->SetTolerance(sqrt((double) 2.0)*this->Tolerance);
  pointMerger->InitPointInsertion(outputIntersection->GetPoints(), bounds0);
  impl->PointMerger = pointMerger;

  // This performs the triangle intersection search
  obbTree0->IntersectWithOBBTree
    (obbTree1, 0, vtkSVLoopIntersectionPolyDataFilter::
     Impl::FindTriangleIntersections, impl);

  int rawLines = outputIntersection->GetNumberOfLines();

  for (int i = 0; i < 2; i++)
    {
    for (vtkIdType interCellId = 0; interCellId < rawLines; interCellId++)
      {
      impl->NewCellIds[i]->InsertTuple2(interCellId, -1, -1);
      }
    }

  vtkDebugMacro(<<"LINEPTSBEFORE "<<outputIntersection->GetNumberOfPoints());
  //The point merger doesn't doesn't detect 100 percent of the points already
  //inserted into the points object. This sometimes causes multiple lines
  //or points. To account for this, this simple clean retains what we need.
  vtkNew(vtkPolyData, tmpLines);
  tmpLines->DeepCopy(outputIntersection);
  tmpLines->BuildLinks();

  vtkNew(vtkCleanPolyData, lineCleaner);
  lineCleaner->SetInputData(outputIntersection);
  lineCleaner->ToleranceIsAbsoluteOn();
  lineCleaner->SetAbsoluteTolerance(this->Tolerance);
  lineCleaner->Update();
  outputIntersection->DeepCopy(lineCleaner->GetOutput());
  vtkNew(vtkPointLocator , linePtMapper);
  linePtMapper->SetDataSet(outputIntersection);
  linePtMapper->BuildLocator();
  double newpt[3];
  vtkIdType mapPtId=0;
  for (vtkIdType ptId = 0; ptId < tmpLines->GetNumberOfPoints(); ptId++)
    {
    tmpLines->GetPoint(ptId, newpt);
    mapPtId = linePtMapper->FindClosestPoint(newpt);
    impl->PointMapper->insert(std::make_pair(mapPtId, ptId));
    }
  vtkDebugMacro(<<"LINEPTSAFTER "<<outputIntersection->GetNumberOfPoints());
  this->NumberOfIntersectionPoints = outputIntersection->GetNumberOfPoints();
  this->NumberOfIntersectionLines = outputIntersection->GetNumberOfLines();
  if (this->NumberOfIntersectionPoints == 0 ||
      this->NumberOfIntersectionLines == 0)
    {
    vtkGenericWarningMacro(<< "No Intersection between objects ");
    impl->NewCellIds[0]->Delete();
    impl->NewCellIds[1]->Delete();
    impl->PointCellIds[0]->Delete();
    impl->PointCellIds[1]->Delete();
    impl->SurfaceId->Delete();

    delete impl;
    return SV_OK;
    }

  impl->BoundaryPoints[0] = vtkIntArray::New();
  impl->BoundaryPoints[1] = vtkIntArray::New();
  // Split the first output if so desired, needed if performing boolean op
  if (this->SplitFirstOutput)
    {
    mesh0->BuildLinks();
    if (impl->SplitMesh(0, outputPolyData0, outputIntersection) != SV_OK)
      {
      this->Status = 0;
      this->NumberOfIntersectionPoints = 0;
      this->NumberOfIntersectionLines = 0;
      impl->NewCellIds[0]->Delete();
      impl->NewCellIds[1]->Delete();
      impl->BoundaryPoints[0]->Delete();
      impl->BoundaryPoints[1]->Delete();
      impl->PointCellIds[0]->Delete();
      impl->PointCellIds[1]->Delete();
      impl->SurfaceId->Delete();

      delete impl;
      return SV_ERROR;
      }

    if (this->ComputeIntersectionPointArray)
      {
      impl->BoundaryPoints[0]->SetName("BoundaryPoints");
      outputPolyData0->GetPointData()->AddArray(impl->BoundaryPoints[0]);
      outputPolyData0->GetPointData()->SetActiveScalars("BoundaryPoints");
      }
    if (this->CheckMesh)
      {
      double dummy[2];
      CleanAndCheckSurface(outputPolyData0, dummy, this->Tolerance);
      }

    outputPolyData0->BuildLinks();
    }
  else
    {
    outputPolyData0->ShallowCopy(mesh0);
    }

  // Split the second output if desired
  if (this->SplitSecondOutput)
    {
    mesh1->BuildLinks();
    if (impl->SplitMesh(1, outputPolyData1, outputIntersection) != SV_OK)
      {
      this->Status = 0;
      this->NumberOfIntersectionPoints = 0;
      this->NumberOfIntersectionLines = 0;
      impl->NewCellIds[0]->Delete();
      impl->NewCellIds[1]->Delete();
      impl->BoundaryPoints[0]->Delete();
      impl->BoundaryPoints[1]->Delete();
      impl->PointCellIds[0]->Delete();
      impl->PointCellIds[1]->Delete();
      impl->SurfaceId->Delete();

      delete impl;
      return SV_ERROR;
      }

    if (this->ComputeIntersectionPointArray)
      {
      impl->BoundaryPoints[1]->SetName("BoundaryPoints");
      outputPolyData1->GetPointData()->AddArray(impl->BoundaryPoints[1]);
      outputPolyData1->GetPointData()->SetActiveScalars("BoundaryPoints");
      }
    if (this->CheckMesh)
      {
      double dummy[2];
      CleanAndCheckSurface(outputPolyData1, dummy, this->Tolerance);
      }

    outputPolyData1->BuildLinks();
    }
  else
    {
    outputPolyData1->ShallowCopy(mesh1);
    }

  impl->NewCellIds[0]->SetName("NewCell0ID");
  outputIntersection->GetCellData()->AddArray(impl->NewCellIds[0]);
  impl->NewCellIds[0]->Delete();
  impl->NewCellIds[1]->SetName("NewCell1ID");
  outputIntersection->GetCellData()->AddArray(impl->NewCellIds[1]);
  impl->NewCellIds[1]->Delete();

  impl->BoundaryPoints[0]->Delete();
  impl->BoundaryPoints[1]->Delete();
  impl->PointCellIds[0]->Delete();
  impl->PointCellIds[1]->Delete();

  impl->SurfaceId->Delete();

  delete impl;

  return SV_OK;
}

//----------------------------------------------------------------------------
int vtkSVLoopIntersectionPolyDataFilter::FillInputPortInformation(int port,
                                                        vtkInformation *info)
{
  if (!this->Superclass::FillInputPortInformation(port, info))
    {
    return SV_ERROR;
    }
  if (port == 0)
    {
    info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkPolyData");
    }
  else if (port == 1)
    {
    info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkPolyData");
    info->Set(vtkAlgorithm::INPUT_IS_OPTIONAL(), 0);
    }
  return SV_OK;
}

//----------------------------------------------------------------------------
//

// Given a line defined by the two points p1,p2; and a plane defined by the
// normal n and point p0, compute an intersection. The parametric
// coordinate along the line is returned in t, and the coordinates of
// intersection are returned in x. A zero is returned if the plane and line
// do not intersect between (0<=t<=1). If the plane and line are parallel,
// zero is returned and t is set to VTK_LARGE_DOUBLE.
int vtkSVLoopIntersectionPolyDataFilter::Impl::IntersectPlaneWithLine(double p1[3], double p2[3], double n[3],
                                                                      double p0[3], double& t, double x[3])
{
  double num, den, p21[3];
  double fabsden, fabstolerance;

  // Compute line vector
  //
  p21[0] = p2[0] - p1[0];
  p21[1] = p2[1] - p1[1];
  p21[2] = p2[2] - p1[2];

  // Compute denominator.  If ~0, line and plane are parallel.
  //
  num = vtkMath::Dot(n,p0) - ( n[0]*p1[0] + n[1]*p1[1] + n[2]*p1[2] ) ;
  den = n[0]*p21[0] + n[1]*p21[1] + n[2]*p21[2];
  //
  // If denominator with respect to numerator is "zero", then the line and
  // plane are considered parallel.
  //

  // trying to avoid an expensive call to fabs()
  if (den < 0.0)
    {
    fabsden = -den;
    }
  else
    {
    fabsden = den;
    }
  if (num < 0.0)
    {
    fabstolerance = -num*1.0e-6;
    }
  else
    {
    fabstolerance = num*1.0e-6;
    }
  if ( fabsden <= fabstolerance )
    {
    t = VTK_DOUBLE_MAX;
    return 0;
    }

  // valid intersection
  t = num / den;

  x[0] = p1[0] + t*p21[0];
  x[1] = p1[1] + t*p21[1];
  x[2] = p1[2] + t*p21[2];

  if ( t >= 0.0 && t <= 1.0 )
    {
    return 1;
    }
  else
    {
    return 0;
    }
}
