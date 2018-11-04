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
 *  \class vtkSVFindGeodesicPath
 *  \brief This class uses vtkDijkstraGraphGeodesicPath to get path betwen points
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVFindGeodesicPath_h
#define vtkSVFindGeodesicPath_h

#include "vtkSVMiscModule.h" // For export

#include "vtkIdList.h"
#include "vtkPolyData.h"
#include "vtkPolyDataAlgorithm.h"

class VTKSVMISC_EXPORT vtkSVFindGeodesicPath : public vtkPolyDataAlgorithm
{
public:
  static vtkSVFindGeodesicPath* New();
  vtkTypeMacro(vtkSVFindGeodesicPath,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Get/Set macros for point that will be close to the target boundary
  vtkGetVector3Macro(ClosePt, double);
  vtkSetVector3Macro(ClosePt, double);
  //@}

  //@{
  /// \brief Get/Set macros for boundary on which the closest point lies
  vtkGetObjectMacro(BoundaryPd, vtkPolyData);
  vtkSetObjectMacro(BoundaryPd, vtkPolyData);
  //@}

  //@{
  /// \brief Get/Set macros for Ids that create the path from the start to the end point
  /// (if using end pt)
  vtkGetObjectMacro(PathIds, vtkIdList);
  vtkSetObjectMacro(PathIds, vtkIdList);
  //@}

  //@{
  /// \brief Point ids for start and end of geodesic path. Start point must
  /// be provided
  vtkGetMacro(StartPtId, int);
  vtkSetMacro(StartPtId, int);
  vtkGetMacro(EndPtId, int);
  vtkSetMacro(EndPtId, int);
  //@}

  //@{
  /// \brief Get/Set macro to designate whether to repel points that are close
  //  to boundaries or open edges
  vtkGetMacro(RepelCloseBoundaryPoints, int);
  vtkSetMacro(RepelCloseBoundaryPoints, int);
  //@}

  //@{
  /// \brief Set/Get macro to indicate whether an array should be created on
  /// the surface with PathBooleanArrayName.
  vtkGetMacro(AddPathBooleanArray, int);
  vtkSetMacro(AddPathBooleanArray, int);
  //@}

  //@{
  /// \brief Get/Set macros for the names of arrays used by filter
  vtkGetStringMacro(InternalIdsArrayName);
  vtkSetStringMacro(InternalIdsArrayName);
  vtkGetStringMacro(DijkstraArrayName);
  vtkSetStringMacro(DijkstraArrayName);
  vtkGetStringMacro(PathBooleanArrayName);
  vtkSetStringMacro(PathBooleanArrayName);
  //@}

protected:
  vtkSVFindGeodesicPath();
  ~vtkSVFindGeodesicPath();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  /** \brief Sets up and runs vtkDijkstra filter, and if repelPoints are
   *  provided, these are set and used. */
  int RunDijkstra(vtkPoints *repelPoints);

  /** \brief Finds closes boundary point to the given ClosePt to use as the
   *  EndPtId if the EndPtId is not provided. */
  int FindClosestBoundaryPoint();

  /** \brief If RepelCloseBoundaryPoints is turned on, this function is used
   *  to get the close boundary points to both StartPtId and EndPtId. */
  int GetCloseBoundaryPoints(const int startPtId, const int endPtId,
                             vtkPoints *repelPoints);

  /** \brief Gets the boundary points that are direct neighbors to given
   *  pointId. */
  int GetNeighborBoundaryPoints(const int ptId,vtkPolyData *pd,
                                vtkPoints *repelPoints);

  char *InternalIdsArrayName;
  char *DijkstraArrayName;
  char *PathBooleanArrayName;

  vtkPolyData *WorkPd;
  vtkPolyData *BoundaryPd;
  vtkIdList   *PathIds;
  vtkIntArray *PathBoolean;

  int StartPtId;
  int EndPtId;
  int AddPathBooleanArray;
  int RemoveInternalIds;
  int RepelCloseBoundaryPoints;

  double ClosePt[3];

private:
  vtkSVFindGeodesicPath(const vtkSVFindGeodesicPath&);  // Not implemented.
  void operator=(const vtkSVFindGeodesicPath&);  // Not implemented.

};

#endif
