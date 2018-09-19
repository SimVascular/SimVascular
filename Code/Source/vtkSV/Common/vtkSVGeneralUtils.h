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
 *  \class vtkSVGeneralUtils
 *  \brief This is a class of purely static functions and no member data.
 *  It is essentially a compilation of useful and simple functions to be called
 *  anywhere in code base.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVGeneralUtils_h
#define vtkSVGeneralUtils_h

#include "vtkObject.h"
#include "vtkSVCommonModule.h" // For export

#include "vtkDataSet.h"
#include "vtkEdgeTable.h"
#include "vtkFloatArray.h"
#include "vtkImplicitFunction.h"
#include "vtkMatrix4x4.h"
#include "vtkObjectFactory.h"
#include "vtkPlane.h"
#include "vtkPolyData.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGlobals.h"

#include <string>
#include <sstream>
#include <iostream>
#include <map>
#include <list>

class VTKSVCOMMON_EXPORT vtkSVGeneralUtils : public vtkObject
{
public:
  vtkTypeMacro(vtkSVGeneralUtils,vtkObject);

  // Source functions
 /** \brief Function to make a polydata of a plane.
  *  \param pt0 origin of plane.
  *  \param pt1 point to extend line from origin to create first axis.
  *  \param pt2 point to extend line from origin to create second axis.
  *  \param res0 resolution to set on first axis.
  *  \param res1 resolution to set on second axis.
  *  \param triangulate indicate whether plane should be triangulated.
  *  \param pd empty polydata to return plane in. */
  static int MakePlane(double pt0[3], double pt1[3], double pt2[3],
                       int res0, int res1, int triangulate, vtkPolyData *pd);

 /** \brief Function to make a polydata of a cylinder.
  *  \param r Radius of cylinder.
  *  \param length Total length of cylinder.
  *  \param resolution resolution of cylinder in the circumferential direction..
  *  \param center center of the cylinder.
  *  \param axis axis of the cylinder.
  *  \param triangulate indicate whether cylinder should be triangulated.
  *  \param pd empty polydata to return cylinder in. */
  static int MakeCylinder(double r, double length, double resolution,
                          double ctr[3], double axis[3], int triangulate,
                          vtkPolyData *pd);

 /** \brief Function to make a polydata of a cube.
  *  \param dims dimensions of cube.
  *  \param center center of the cube.
  *  \param triangulate indicate whether cube should be triangulated.
  *  \param pd empty polydata to return cube in. */
  static int MakeCube(double dims[3], double ctr[3], int triangulate,
                      vtkPolyData *pd);

  //Checking functions

  /** \brief Function to check is array with name exists in cell or point data
   *  \param ds this is the object to check if the array exists
   *  \param datatype this is point or cell. point=0,cell=1
   *  \param arrayname this is the name of the array to check
   *  \reutrn this returns 1 if the array exists and zero if it doesn't
   *  or the function does not return properly. */
  static int CheckArrayExists(vtkDataSet *ds, int datatype, std::string arrayname);

  /** \brief
   *  \param polydata to check. */
  static int CheckSurface(vtkPolyData *pd);

  /** \brief
   *  \param polydata to check.
   *  \param numNonTriangleElements returns the number of non-triangle elements.
   *  \param numNonManifoldEdges returns the number of non-manifold elements.
   *  \param numOpenEdges returns the number of open edges.
   *  \param surfaceGenus returns the surface genus. */

  static int CheckSurface(vtkPolyData *pd, int &numNonTriangleCells, int &numNonManifoldEdges, int &numOpenEdges, int &surfaceGenus);

  //General operations
  /** \brief Gets region closest to given point.
   *  \param pd Input pd which is copied in place with result.
   *  \param pt The closest point connected region.
   *  \return SV_OK */
  static int GetClosestPointConnectedRegion(vtkPolyData *pd,
                                            double pt[3]);

  /** \brief Gets region closest to given point.
   *  \param inPd Input pd.
   *  \param pt The closest point connected region.
   *  \param outPd Result pd.
   *  \return SV_OK */
  static int GetClosestPointConnectedRegion(vtkPolyData *inPd,
                                            double pt[3],
                                            vtkPolyData *outPd);
  /** \brief Sets ids on pd using vtkIdFilter.
   *  \param pd Inpud pd which is copied in place
   *  \param arrayName Name of array to give to ids
   *  \return SV_OK */
  static int GiveIds(vtkPolyData *pd,
                     std::string arrayName);

  /** \brief Sets ids on pd using vtkIdFilter.
   *  \param inPd Input pd.
   *  \param arrayName Name of array to give to ids
   *  \param outPd Result pd.
   *  \return SV_OK */
  static int GiveIds(vtkPolyData *inPd,
                     std::string arrayName,
                     vtkPolyData *outPd);

  /** \brief Function to iterate a point in a VTK_LINE or VTK_POLY_LINE
   *  \param pd Input pd containing line cells.
   *  \param pointId current point id; will be updated in function.
   *  \param prevCellId previous cell id; will be updated in function.
   *  \return SV_OK */
  static int IteratePoint(vtkPolyData *pd, int &pointId, int &prevCellId);

  /** \brief Awesome function that is wrapper around vtkThreshold
   *  \param pd Input pd to threshold, updated in place.
   *  \param minVal minumum value.
   *  \param maxVal maximum value.
   *  \param dataType 0 for point data, 1 for cell data.
   *  \param arrayName Name of array to be used to threshold polydata.
   *  \return SV_OK */
  static int ThresholdPd(vtkPolyData *pd, int minVal, int maxVal, int dataType,
                         std::string arrayName);

  /** \brief Awesome function that is wrapper around vtkThreshold
   *  \param pd Input pd to threshold.
   *  \param minVal minumum value.
   *  \param maxVal maximum value.
   *  \param dataType 0 for point data, 1 for cell data.
   *  \param arrayName Name of array to be used to threshold polydata.
   *  \param returnPd The resultant thresholded pd
   *  \return SV_OK */
  static int ThresholdPd(vtkPolyData *pd, int minVal, int maxVal, int dataType,
                         std::string arrayName, vtkPolyData *returnPd);

  /** \brief Awesome function that is wrapper around vtkThreshold
   *  \param ug Input ug to threshold, updated in place.
   *  \param minVal minumum value.
   *  \param maxVal maximum value.
   *  \param dataType 0 for point data, 1 for cell data.
   *  \param arrayName Name of array to be used to threshold ug.
   *  \return SV_OK */
  static int ThresholdUg(vtkUnstructuredGrid *ug, int minVal, int maxVal, int dataType,
                         std::string arrayName);

  /** \brief Awesome function that is wrapper around vtkThreshold
   *  \param ug Input ug to threshold.
   *  \param minVal minumum value.
   *  \param maxVal maximum value.
   *  \param dataType 0 for point data, 1 for cell data.
   *  \param arrayName Name of array to be used to threshold ug.
   *  \param returnPd The resultant thresholded ug
   *  \return SV_OK */
  static int ThresholdUg(vtkUnstructuredGrid *ug, int minVal, int maxVal, int dataType,
                         std::string arrayName, vtkUnstructuredGrid *returnUg);

  /** \brief Get a polydata of the edges of the input polydata. Each edge is
   *  a VTK_LINE cell..
   *  \return SV_OK.  */
  static int GetEdgePolyData(vtkPolyData *pd, vtkPolyData *edgePd);

  /** \brief Get centroid of points */
  static int GetCentroidOfPoints(vtkPoints *points, double centroid[3]);

  /** \brief For an array of integers on pd, will return a list of values on cells
   *  attached to point.
   *  \param pd The full polydata.
   *  \param arrayName Name of array to get cell data of.
   *  \param pointId The point Id to get array values of.
   *  \param valList list of values on cells attached to point.
   *  \return SV_OK */
  static int GetPointCellsValues(vtkPointSet *ps, std::string arrayName,
                                 const int pointId, vtkIdList *valList);

  /** \brief For an array of integers on pd, will return a list of values on neighboring cells
   *  \param pd The full polydata.
   *  \param arrayName Name of array to get cell data of.
   *  \param cellId The cell Id to get neighbors values of.
   *  \param valList list of values on cells attached to point.
   *  \return SV_OK */
  static int GetNeighborsCellsValues(vtkPolyData *pd, std::string arrayName,
                                    const int cellId, vtkIdList *valList);

  /** \brief Perform a cut of the polydata, but with crinkle clip. Uses vtkExtractGeometry
   *  \param inPd The pd to cut.
   *  \param cutFunction implicit function defining cut plane/box/etc.
   *  \param extractBoundaryCells If 1, cells that are slice through are extracted.
   *  \param extractInside If 1, flips the function and extracts otherside of pd.
   *  \param outPd Result of the cut operation.
   *  \return SV_OK */
  static int ExtractionCut(vtkPolyData *inPd, vtkImplicitFunction *cutFunction,
                           const int extractBoundaryCells,
                           const int extractInside,
                           vtkPolyData *outPd);

  /** \brief Perform a cut of the polydata.
   *  \param inPd The pd to cut.
   *  \param cutFunction implicit function defining cut plane/box/etc.
   *  \param extractInside If 1, flips the function and extracts otherside of pd.
   *  \param clippedOutPd if generateClippedOutput is 1 and pd is provided, the
   *  surface that was clipped out is returned with this parameter.
   *  \param outPd Result of the cut operation.
   *  \return SV_OK */
  static int ClipCut(vtkPolyData *inPd, vtkImplicitFunction *cutFunction,
                     const int generateClippedOutput,
                     const int extractInside,
                     vtkPolyData *outPd,
                     vtkPolyData *clippedOutPd);

  /** \brief For a polydata with cells of VTK_LINE of VTK_POLY_LINE, will calculate
   *  the total distance of that line.
   *  \param pd The polydata.
   *  \return the unsigned distance. */
  static double GetPointsLength(vtkPolyData *pd);

  //@{
  /** \brief Given a pointset and an array, we populate the array with a value,
   *  but only if the current value has a certain value.
   *  \param pointset The given dataset.
   *  \param sliceIds The data array that we want to replace values in (cell data).
   *  \param sliceId The value to be added to array.
   *  \param replaceVal We only replace when the current value is equal to this!
   *  \param arrName If this is part of a larger dataset, this is used to link
   *  back to full dataset. If not, just run dataset through vtkIdFilter, and
   *  provide ids array name.
   *  \return SV_OK if function completes withouth error. */
  static int ReplaceDataOnCells(vtkPointSet *pointset, vtkDataArray *sliceIds,
                                const int sliceId, const int replaceVal,
                                const std::string &arrName);
  static int ReplaceDataOnCells(vtkPointSet *pointset,
                                const int replaceVal, const int currVal,
                                const std::string &arrName);
  //@}

  /** \brief Given two points, return the cutPlane perpendicular to their vector
   *  \param endPt End point of vector.
   *  \param startPt Start point of vector.
   *  \param cutPlane The plane defined by vector.
   *  \return SV_OK. */
  static int GetCutPlane(double endPt[3], double startPt[3],
                         vtkPlane *cutPlane);

  /** \brief Computes the mass center of points use vtkCenterOfMass.
   *  \return SV_OK.  */
  static int ComputeMassCenter(vtkPolyData *pd, double massCenter[3]);

  /** \brief Computes the scalar values for the barycentric coordinates of a
   *  point in a triangle given its three vertices.
   *  \param f The point where the barycentric coordinates are going to be
   *  calculated.
   *  \param a0 The returned scalar coordinate of vertex 0.
   *  \param a1 The returned scalar coordinate of vertex 1.
   *  \param a2 The returned scalar coordinate of vertex 2.
   *  \return SV_OK.  */
  static int GetBarycentricCoordinates(double f[3], double pt0[3], double pt1[3],
                                       double pt2[3], double &a0, double &a1, double &a2);

  /** \brief Get the nieghboring points, or the points that share an edge
   *  with the point of interest.
   *  \param p0 The point to get neighbors of.
   *  \param pd The polydata to use to get neighbors.
   *  \param pointNeighbors The list of returned neighboring point ids.
   *  \return SV_OK.  */
  static int GetPointNeighbors(vtkIdType p0, vtkPolyData *pd, vtkIdList *pointNeighbors);

  /** \brief Get cotangent angle of a directional edge.
   *  \param pt0 The first point of the edge.
   *  \param pt1 The second point of the edge.
   *  \param pt2 The point making a triangle with the first two points, and
   *  where the angle is to be computed.
   *  \param angle The angle of the directional edge.
   *  \return SV_OK.  */
  static int GetEdgeCotangentAngle(double pt0[3], double pt1[3], double pt2[3], double &angle);

  /** \brief Function to create an edge table from an arbitrary polydata
   *  \param pd The polydata to create an edge table for.
   *  \param edgeTable an empty edge table populate with edges.
   *  \param edgeWeights An empty array to fill with harmonic edge weights.
   *  \param edgeNeighbors An empy array to fill with the edge cell neighbors.
   *  \param isBoundary An empty array to act as a boolean indicating if edge is on a boundary.
   *  \return SV_OK.  */
  static int CreateEdgeTable(vtkPolyData *pd, vtkEdgeTable *edgeTable,
                             vtkFloatArray *edgeWeights,
                             vtkIntArray *edgeNeighbors,
                             vtkIntArray *isBoundary);

  /** \brief Function to compute the harmonic edge weight.
   *  \param pd The polydata on which the edge lies.
   *  \param cellId The cellId containing the edge.
   *  \param neighborCellId The neighboring cell containing the edge (The cell
   *  and its neighbor should have one common edge).
   *  \param p0 First point id of edge.
   *  \param p1 Second point id of edge.
   *  \param The returned weight of the edge.
   *  \return SV_OK.  */
  static int ComputeHarmonicEdgeWeight(vtkPolyData *pd, vtkIdType cellId,
                                       vtkIdType neighborCellId,
                                       vtkIdType p0, vtkIdType p1, double &weight);

  /** \brief Convert a data field to its own polydata.
   *  \param inPd polydata with array of fieldName, as long as it has three components.
   *  \param fieldName Name of data array on surface.
   *  \param outPd Empty polydata to be filled with dara array polydata.
   *  return SV_OK if function completes without error */
  static int ConvertFieldToPolyData(vtkPolyData *inPd, std::string fieldName, vtkPolyData *outPd);

  /** \brief Computes normals of polydata and adds normal data array to surface.
   *  return SV_OK if function completes without error */
  static int ComputeNormals(vtkPolyData *pd);

  /** \brief Compute laplacian of points of a mesh using harmonic edge weights.
   *  \return SV_OK */
  static int ComputeMeshLaplacian(vtkPolyData *pd, vtkEdgeTable *edgeTable,
                                  vtkFloatArray *edgeWeights, vtkIntArray *edgeNeighbors,
                                  vtkFloatArray *laplacian, int map);

  /** \brief Compute laplacian of mesh data array using harmonic edge weights.
   *  \return SV_OK */
  static int ComputeDataArrayLaplacian(vtkFloatArray *data, vtkPolyData *pd,
                                       vtkEdgeTable *edgeTable,
                                       vtkFloatArray *edgeWeights, vtkIntArray *edgeNeighbors,
                                       vtkFloatArray *laplacian, int map);

  /** \brief Compute laplacian at specific point of mesh.
   *  \return SV_OK */
  static int ComputePointLaplacian(vtkIdType p0, vtkPolyData *pd,
                            vtkEdgeTable *edgeTable, vtkFloatArray *edgeWeights,
                            vtkIntArray *edgeNeighbors, double laplacian[], int map);

  /** \brief Compute laplacian at specific data point of mesh.
   *  \return SV_OK */
  static int ComputeDataLaplacian(vtkIdType p0, vtkFloatArray *data, vtkPolyData *pd,
                                  vtkEdgeTable *edgeTable, vtkFloatArray *edgeWeights,
                                  vtkIntArray *edgeNeighbors,
                                  double laplacian[], int map);

  /** \brief Function to iterate around a polydata of VTK_LINE's and get an
   *  ordered set of points.
   *  \param pd The polydata of lines.
   *  \param startPt The id of the point to start at.
   *  \param nextCell The id of the next cell to go to.
   *  \param loop Empty polydata to hold ordered points.
   *  \param boundaryIds If not NULL, this should contain a list of some of
   *  the points on the boundary. The algorithm will check and see if they
   *  come in the correct order, and if they don't, it will return SV_ERROR.
   *  \return SV_OK. If boundaryIds provided, SV_OK if boundaryIds are in correct order. */
  static int RunLoopFind(vtkPolyData *pd, vtkIdType startPt, vtkIdType nextCell,
                         vtkPolyData *loop, vtkIdList *boundaryIds);


  /** \brief Function to separate multiple boundary loops. */
  static int SeparateLoops(vtkPolyData *pd, vtkPolyData **loops, int numBoundaries, const double xvec[3], const double zvec[3], const int boundaryStart[2]);

  //@{
  /** \brief Get rotation matrix to align two vectors.
   *  \param rotMatrix populated with resultant matrix.
   *  return SV_OK */
  static int GetRotationMatrix(double vec0[3], double vec1[3], vtkMatrix4x4 *rotMatrix);
  static int GetRotationMatrix(double vec0[3], double vec1[3], double rotMatrix[16]);
  //@}

  //@{
  /** \brief Transforms pd or ug with the given rotation matrix.
   *  return SV_OK */
  static int ApplyRotationMatrix(vtkPolyData *pd, vtkMatrix4x4 *rotMatrix);
  static int ApplyRotationMatrix(vtkPolyData *pd, double rotMatrix[16]);
  static int ApplyRotationMatrix(vtkUnstructuredGrid *ug, vtkMatrix4x4 *rotMatrix);
  static int ApplyRotationMatrix(vtkUnstructuredGrid *ug, double rotMatrix[16]);
  //@}

  /** \brief Transforms the three points of a triangle in 3d to 2d. pt0 is
   *   relocated to 0.0, 0.0 and pt1 is relocated to lie on the x axis.
   *  return SV_OK */
  static int TransformTriangleToXYPlane(double pt0[3], double pt1[3], double pt2[3],
                                        double outPt0[3], double outPt1[3], double outPt[2]);

  static int ComputeParametricDerivatives(double pt0[3], double pt1[3], double pt2[3],
                                                    double pPt0[3], double pPt1[3], double pPt2[3],
                                                    double dXdXi, double dXdEta,
                                                    double dYdXi, double dYdEta,
                                                    double dZdXi, double dZdEta);

  static int ComputeJacobianDerivatives(double pt0[3], double pt1[3], double pt2[3],
                                                  double pPt0[3], double pPt1[3], double pPt2[3],
                                                  double dXdXi, double dXdEta,
                                                  double dYdXi, double dYdEta,
                                                  double dZdXi, double dZdEta);

  static int GetParametricPoints(double pt0[3], double pt1[3], double pt2[3],
                                 double pPt0[3], double pPt1[3], double pPt2[3]);

  /** \brief Get all angles of a polydata surface.
   *  \param cellAngles data array containing the angle values of all on mesh.
   *  It has three components as there are three angles per cell.
   *  return SV_OK */
  static int GetPolyDataAngles(vtkPolyData *pd, vtkFloatArray *cellAngles);

  /** \brief Function to get regions of scalar labels definied on a surface.
   * Typically used with clustering algorithms, the regions allow quick
   * access to the cells in the cluster and the points on the edges of the cluster.
   */
  static int GetRegions(vtkPolyData *pd, std::string arrayName,
                        std::vector<Region> &allRegions);
  static int GetSpecificRegions(vtkPolyData *pd, std::string arrayName,
                                             std::vector<Region> &allRegions,
                                             vtkIdList *targetRegions);

  static int GetCCWPoint(vtkPolyData *pd, const int pointId, const int cellId);

  static int GetCWPoint(vtkPolyData *pd, const int pointId, const int cellId);

  static int CheckBoundaryEdge(vtkPolyData *pd, std::string arrayName, const int cellId, const int pointId0, const int pointId1);

  static int CheckCellValuesEdge(vtkPolyData *pd, std::string arrayName, const int cellId, const int pointId0, const int pointId1);

  static void SplineKnots(std::vector<int> &u, int n, int t);

  static void SplineCurve(const std::vector<XYZ> &inp, int n, const std::vector<int> &knots, int t, std::vector<XYZ> &outp, int res);

  static void SplinePoint(const std::vector<int> &u, int n, int t, double v, const std::vector<XYZ> &control, XYZ &output);

  static double SplineBlend(int k, int t, const std::vector<int> &u, double v);

  static int GetCellRingNeighbors(vtkPolyData *pd,
                                  vtkIdList *cellIds,
                                  int ringNumber,
                                  int totNumberOfRings,
                                  std::vector<std::vector<int> > &neighbors);

  int GetCellDirectNeighbors(vtkPolyData *pd,
                             std::vector<std::vector<int> > &neighbors,
                             std::vector<int> &numNeighbors);

  /** \brief Naive implementation to get most reoccuring number in list. Okay
   *  because list size is small. */
  static void GetMostOccuringVal(vtkIdList *idList, int &output, int &max_count);

  static int SmoothBoundaries(vtkPolyData *pd, std::string arrayName);
  static int SmoothSpecificBoundaries(vtkPolyData *pd, std::string arrayName, vtkIdList *targetRegions);

  static int GetPointEdgeCells(vtkPolyData *pd, std::string arrayName,
                               const int cellId, const int pointId,
                               vtkIdList *sameCells);

  static int CurveFitBoundaries(vtkPolyData *pd, std::string arrayName,
                                std::vector<Region> allRegions);

  /** \brief Correct cells on the boundary by updating val if they have
   *  multiple neighboring cells of the same value */
  static int CorrectCellBoundaries(vtkPolyData *pd, std::string cellArrayName);
  /** \brief Correct cells on the boundary by updating val if they have
   *  multiple neighboring cells of the same value */
  static int CorrectSpecificCellBoundaries(vtkPolyData *pd, std::string cellArrayName,
                                    vtkIdList *targetRegions);

  /** \brief From three vectors, compute transformation from global to local */
  static int ComputeRotationMatrix(const double from_x[3], const double from_y[3],
                                   const double from_z[3], const double to_x[3],
                                   const double to_y[3], const double to_z[3],
                                   double rotMatrix[9]);

  static int FindPointMatchingValues(vtkPointSet *ps, std::string arrayName, vtkIdList *matchingVals, int &returnPtId);
  static int FindPointsMatchingValues(vtkPointSet *ps, std::string arrayName, vtkIdList *matchingVals, vtkIdList *returnPtIds);


protected:
  vtkSVGeneralUtils();
  ~vtkSVGeneralUtils();

private:
  vtkSVGeneralUtils(const vtkSVGeneralUtils&);  // Not implemented.
  void operator=(const vtkSVGeneralUtils&);  // Not implemented.
};

#endif
