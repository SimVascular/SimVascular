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
 * \class vtkSVPolyDataSliceAndDiceFilter
 *
 * \brief This is a filter to decompose a polydata surface into a series
 * of patches that represent a polycube structure using the object centerlines
 * \details The centerlines are first processed and a graph simplification
 * of the basic structure of the polydata is constructed. The polydata is
 * segmented at each bifurcation and a new patch is created to become the linking
 * cube at branching locations of a polycube structure. Finally, the polycube
 * is constructed based on the final decomposition and the necessary information
 * attached to the polycube in order to parameterize each individual patch
 *
 * \author Adam Updegrove
 * \author updega2@gmail.com
 * \author UC Berkeley
 * \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVPolyDataSliceAndDiceFilter_h
#define vtkSVPolyDataSliceAndDiceFilter_h

#include "vtkPolyDataAlgorithm.h"
#include "vtkSVParameterizationModule.h" // For exports

#include "vtkEdgeTable.h"
#include "vtkFloatArray.h"
#include "vtkPolyData.h"
#include "vtkSVGeneralizedPolycube.h"

#include "svGraph.h"

#include <map>
#include <list>

class VTKSVPARAMETERIZATION_EXPORT vtkSVPolyDataSliceAndDiceFilter : public vtkPolyDataAlgorithm
{
public:
  static vtkSVPolyDataSliceAndDiceFilter* New();
  void PrintSelf(ostream& os, vtkIndent indent);

  //@{
  /// \brief Set/Get macros for the slice length along the vessel length. Governs
  /// how often the polydata is sliced for the surgery lines
  vtkSetMacro(SliceLength, double);
  vtkGetMacro(SliceLength, double);
  //@}

  //@{
  /// \brief Set/Get macros to a indicate whether the polycube will be constructed
  vtkSetMacro(ConstructPolycube, int);
  vtkGetMacro(ConstructPolycube, int);
  //@}

  //@{
  /// \brief Set/Get macros for all the array names either used in the filter
  /// or assigned to a surface during the course of the operation
  vtkGetStringMacro(SliceIdsArrayName);
  vtkSetStringMacro(SliceIdsArrayName);
  vtkGetStringMacro(GroupIdsArrayName);
  vtkSetStringMacro(GroupIdsArrayName);
  vtkGetStringMacro(SegmentIdsArrayName);
  vtkSetStringMacro(SegmentIdsArrayName);
  vtkGetStringMacro(SphereRadiusArrayName);
  vtkSetStringMacro(SphereRadiusArrayName);
  vtkGetStringMacro(BoundaryPointsArrayName);
  vtkSetStringMacro(BoundaryPointsArrayName);
  vtkGetStringMacro(InternalIdsArrayName);
  vtkSetStringMacro(InternalIdsArrayName);
  vtkGetStringMacro(DijkstraArrayName);
  vtkSetStringMacro(DijkstraArrayName);
  //@}

  //@{
  /// \brief Set/Get macro for the centerlines. CenterlinesPd must be provided!
  vtkSetObjectMacro(CenterlinesPd, vtkPolyData);
  vtkGetObjectMacro(CenterlinesPd, vtkPolyData);
  //@}

  //@{
  /// \brief Set/Get macros for the final polycube structure
  vtkSetObjectMacro(Polycube, vtkSVGeneralizedPolycube);
  vtkGetObjectMacro(Polycube, vtkSVGeneralizedPolycube);
  //@}

  /// \brief Get macro for the surgery lines along length of object
  vtkGetObjectMacro(SurgeryLinesPd, vtkPolyData);

  /// \brief Get macro for the graph
  vtkGetObjectMacro(GraphPd, vtkPolyData);

  /**
   * \brief Constructs cubes to populate a polycube structure from node of svGraph
   * \details This function is constructed to be used with svGraph::Recurse. The
   * void pointers are made available to pass any extra information through the function.
   * \param gCell Node of graph to generate cubes for a polycube structure.
   * \return SV_OK if function completes without error
   */
  static int GraphToPolycube(svGCell *gCell, void *arg0,
                             void *arg1, void *arg2);
  /**
   * \brief Gets the index on the polycube based on the parent node and diverging
   * node directions.
   * \param parent direction of parent (Numbers correspond to DIRECTIONS enum)
   * \param divchild direction of parent (Numbers correspond to DIRECTIONS enum)
   * \param index index of cube to get (0-7)
   * \return SV_OK if function completes without error
   */
  static int LookupIndex(const int parent, const int divchild, const int index);


  /// \brief directions of nodes in graph simplification
  enum DIRECTIONS
  {
    RIGHT = 0,
    LEFT,
    FRONT,
    BACK,
    UP,
    DOWN
  };

  const static int DT[6][4]; ///< \brief Direction Table
  const static int RT[9][8]; ///< \brief Index Rotation Table

protected:
  vtkSVPolyDataSliceAndDiceFilter();
  ~vtkSVPolyDataSliceAndDiceFilter();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector);

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.
  int FindGroupBoundaries(); ///< \brief Find and label nodes that form boundary between groups.
  int BuildPolycube(); ///< \brief If building polycube, construct it based off graph.
  int GetCriticalPoints(); ///< \brief Get points that separate three or more regions.
  int SliceBifurcations(); ///< \brief Process and cut each bifurcation.
  int SliceBranches(); ///< \brief Process and cut each branch.

  /** \brief Checks to see if the number of points in a centerline has been
   * surpassed and need to set back in order to process the final bit correctly.
   * \param ptId The current point id; will be modified if number of points has been surpassed.
   * \param numPts Number of total points in the centerline.
   * \return If number of points is surpassed, 1 should be returned; otherwise, 0. */
  void CheckLength(int &ptId, const int numPts, int &done);

  /** \brief Increments the pt id if slicing down and decrements the pt id if slicing up.
   * \param ptId The current point id; will be increased or decreased. */
  void UpdatePtId(int &ptId);

  /** \brief Function to double check that slice is bueno.
   * \param pd The polydata to double check.
   * \return SV_OK if the polydata passes inspection. */
  int CheckSlice(vtkPolyData *pd);

  /** \brief Small function to ensure that we have the correcting starting
    * point according to what we have defined to be the "front".
    * \param pd The full polydata
    * \param frontDir The vector to test
    * \param frontId The current front id, will be switched if incorrect direction
    * \param backId The current back id, will be switched if incorrect direction
    * \return SV_OK */
  int GetCorrectFrontPoint(vtkPolyData *pd,
                           double frontDir[3],
                           int &frontId,
                           int &backId);

  /** \brief Thresholds out a region for each of the ids provided, and then
   * also returns whatever is leftover after all of that thresholding.
   * \param startPd The pd to threshold.
   * \param id0, The first id.
   * \param pd0, Empty polydata to fill with region 0
   * \param id1, The second id.
   * \param pd1, Empty polydata to fill with region 1
   * \param id2, The third id.
   * \param pd2, Empty polydata to fill with region 2
   * \param leftovers, Whatever is left of startPd
   * \return SV_OK */
  int GetFourPolyDataRegions(vtkPolyData *startPd,
                             const int id0, vtkPolyData *pd0,
                             const int id1, vtkPolyData *pd1,
                             const int id2, vtkPolyData *pd2,
                             vtkPolyData *leftovers);

  /** \brief Function to get the critical surgery points, or the surgery points
   * at a bifurcation. */
  int CriticalSurgeryPoints(vtkPolyData *pd,
                           const int frontId,
                           const int backId,
                           const int groupId,
                           const int checkId,
                           double startPt[3],
                           double secondPt[3],
                           vtkIdList *fixedGoToPoints,
                           vtkIdList *fixedSurgeryPoints,
                           double startDir[3]);

  /// \brief Function to the starting surgery points
  int GetFirstSurgeryPoints(vtkPolyData *pd, int pointId,
                            vtkIdList *surgeryPoints,
                            double xvec[3], double zvec[3]);
  /// \brief Function to get four points around length of ring
  int GetSurgeryPoints(vtkPolyData *pd,
                       vtkPolyData *parentPd,
                       vtkDataArray *pointIds,
                       const double clStartPt[3],
                       const double clSecondPt[3],
                       const int front,
                       const int back,
                       const int checkId,
                       std::string arrayName,
                       vtkIdList *surgeryPoints,
                       double startDir[3]);
  /// \brief Function to get two points from front to back
  int GetHalfSurgeryPoints(vtkPolyData *pd,
                           vtkDataArray *pointIds,
                           const int cellId,
                           const int front,
                           const int back,
                           vtkIdList *surgeryPoints);

  /// \brief Function to the next surgery points when processing
  int GetNextSurgeryPoints(vtkPolyData *pd,
                           double centerPt[3],
                           vtkIdList *surgeryPoints,
                           double xvec[3], double zvec[3],
                           double radius,
                           vtkIdList *surgeryLineIds);

  /// \brief Function to the final surgery points
  int GetEndSurgeryPoints(vtkPolyData *pd, svGCell *gCell,
                          double centerPt[3],
                          vtkIdList *surgeryPoints,
                          int endSurgeryIds[8],
                          double xvec[3], double zvec[3],
                          double radius,
                          vtkIdList *surgeryLineIds,
                          int cellIndices[8],
                          int &secondRun);

  /** \brief Function to determine branch slicing strategy based on number
    * of critical points.
    * \param branchPd Polydata for the specific branch.
    * \param branchCenterline Corresponding centerline for the group.
    * \param gCell Corresponding graph node.
    * \return SV_OK if function completes without error. */
  int DetermineSliceStrategy(vtkPolyData *branchPd,
                             vtkPolyData *branchCenterline,
                             svGCell *gCell,
                             vtkIdList *surgeryPoints);

  /** \brief Insert a critical point and its associated groups into the
   *  CriticalPointMap.
   *  \param pointId Id of point to add to map
   *  \param groupIds List of group ids that touch the point
   *  \return SV_OK */
  int InsertCriticalPoints(const int pointId, vtkIdList *groupIds);

  /** \brief Threshold out a specific group of the full polydata.
   *  \param branchId Group id to threshold.
   *  \param branchPd Empty polydata to fill with branch pd.
   *  \param branchCenterlinesPd Empty polydata for corresponding group's centerlines.
   *  \return SV_OK  */
  int GetBranch(const int branchId, vtkPolyData *branchPd,
                vtkPolyData *branchCenterlinesPd);

  /** \brief Function to slice individual branch.
   *  \param branchPd Polydata for the specific branch.
   *  \param branchCenterline Corresponding centerline for the group
   *  \param gCell Corresponding graph node.
   *  \param sliceIds If branch segmented into multiple regions, these ids fill
   *  \param secondRun If the surgery line does not end up at the correct node
   *  of a segment internal to two bifurcations, the graph needs to be rotated
   *  and we need to run again. In this case, we indicate this with this parameter.
   *  the different regions.
   *  \return SV_OK if function completes without error. */
  int SliceBranch(vtkPolyData *branchPd, vtkPolyData *branchCenterline,
                    svGCell *gCell,
                    vtkDataArray *sliceIds,
                    int secondRun);

  /** \brief Function to slice a single bifurcation
   *  \param pd The full polydata.
   *  \param gCell The corresponding cell where bifurcation is in between cell
   *  and its children. */
  int SliceBifurcation(vtkPolyData *pd, svGCell *gCell);

  /** \brief A helpful function that first updates the direction of the
   *  gCell's diverging child, and then proceeds to call the UpdateCellDirection function
   *  in svGraph. This gives us a correct polycube structure!
   *  \param gCell The gCell who's diverging child direction needs to be fixed!
   *  \actualId The id of the point that corresponds to the bottom of where we
   *  started in gCell
   *  \param cellIndices The cellIndices corresponding to current gCell and
   *  diverging child direction
   *  \return SV_OK if function completes without error  */
  int FixGraphDirections(svGCell *gCell, const int actualId,
                         int cellIndices[8]);

  /** \brief Convenience function to get z axis from centerline start and end point
   *  \param startPt Start point of line
   *  \param endPt End point of line
   *  \param zvec Empty triple to hold the result z vector
   *  \return SV_OK */
  int GetSectionZAxis(const double startPt[3], const double endPt[3],
                      double zvec[3]);

  /** \brief Convenience function to get x axis from centerline start and end point
   *  \param startPt Start point of line
   *  \param endPt End point of line
   *  \param surfacePt Point on the surface to use as reference
   *  \param xvec Empty triple to hold the result x vector
   *  \return SV_OK */
  int GetSectionXAxis(const double startPt[3], const double endPt[3],
                      const double surfacePt[3], double xvec[3]);

  /** \brief Gets point that is on boundary closest to point projected in
   *  direction xvec from centerPt
   *  \param pd Polydata to find point on boundary
   *  \param centerPt Point on centerline near boundary
   *  \param xvec Direction to project in
   *  \param radius Radius of vessel at current location
   *  \param boundary Empty polydata to be filled with the boundary close to
   *  centerPt
   *  \param returnStartId Id of point that is on the boundary edge of the given
   *  polydata and closest to the projected point
   *  \return SV_OK */
  int GetClose3DPoint(vtkPolyData *pd, double centerPt[3],
                      double xvec[3],
                      double radius,
                      vtkPolyData *boundary,
                      int &returnStartId);

  /** \brief Adds the ids in the list to the full Surgery Lines
   *  \param surgeryLineIds List of point ids to add to the Surgery Lines
   *  \return SV_OK */
  int AddSurgeryPoints(vtkIdList *surgeryLineIds);


private:
  vtkSVPolyDataSliceAndDiceFilter(const vtkSVPolyDataSliceAndDiceFilter&);  // Not implemented.
  void operator=(const vtkSVPolyDataSliceAndDiceFilter&);  // Not implemented.

  char *BoundaryPointsArrayName; // Array for boundary points (internal)
  char *DijkstraArrayName; // Array for dijkstra filter (internal)
  char *GroupIdsArrayName; // Array for groupids; must be defined on centerlines
  char *InternalIdsArrayName; // Array to keep track of node numbers wehn thresholding and slicing (internal)
  char *SegmentIdsArrayName; // Array for new patch ids
  char *SliceIdsArrayName; // Unnecessary array name TODO: Remove
  char *SphereRadiusArrayName;  // Array for maximum inscribed sphere radius; must be defined on centerlines

  vtkPolyData    *InitialPd; // Input polydata, kept for reference
  vtkPolyData    *WorkPd; // Polydata used during filter processing
  vtkPolyData    *GraphPd; // Polydata populated with the graph simplification
  vtkPolyData    *CenterlinesPd; // Polydata containing centerlines; must be provided!
  vtkPolyData    *SurgeryLinesPd; // Polydata containing the lines slicing the vessels along their length
  svGraph        *CenterlineGraph; // A graph simplification
  vtkSVGeneralizedPolycube *Polycube; // The polycube structure

  std::multimap<int , int> CriticalPointMap; // Stl map keeping track of what points map to what groups and vice versa

  int ConstructPolycube;
  int SliceDirection;
  int TotalSliceId;
  int IT[6][6][8];

  double FirstBranchVec[3];
  double SliceLength;

};

#endif
