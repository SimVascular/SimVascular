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
 *  \class vtkSVGroupsClipper
 *  \brief Using a polydata centerlines, separate the polydata into regions
 *  based on the centerlines
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVGroupsClipper_h
#define vtkSVGroupsClipper_h

#include "vtkPolyDataAlgorithm.h"
#include "vtkPolyData.h"
#include "vtkIdList.h"
#include "vtkSVFiltersModule.h" // For export

class VTKSVFILTERS_EXPORT vtkSVGroupsClipper : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkSVGroupsClipper,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);

  static vtkSVGroupsClipper *New();

  //@{
  /// \brief Get/Set macro for the object's centerlines
  vtkSetObjectMacro(Centerlines,vtkPolyData);
  vtkGetObjectMacro(Centerlines,vtkPolyData);
  //@}

  //@{
  /// \brief Get/Set macro for centerline group ids to use in the filter
  vtkSetObjectMacro(CenterlineGroupIds,vtkIdList);
  vtkGetObjectMacro(CenterlineGroupIds,vtkIdList);
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
  //@}

  //@{
  /// \brief Get/Set/Boolean macro to indicate whether to clip all centerline
  /// group ids
  vtkSetMacro(ClipAllCenterlineGroupIds,int);
  vtkGetMacro(ClipAllCenterlineGroupIds,int);
  vtkBooleanMacro(ClipAllCenterlineGroupIds,int);
  //@}

  //@{
  /// \brief Get/Set the cutoff radius factor for clipping of the surface
  //  distance functions
  vtkSetMacro(CutoffRadiusFactor,double);
  vtkGetMacro(CutoffRadiusFactor,double);
  //@}

  //@{
  /// \brief Get/Set the clip value for clipping of the surface distance functions.
  vtkSetMacro(ClipValue,double);
  vtkGetMacro(ClipValue,double);
  //@}

  //@{
  /// \brief Get/Set the radius information
  vtkSetMacro(UseRadiusInformation,int);
  vtkGetMacro(UseRadiusInformation,int);
  vtkBooleanMacro(UseRadiusInformation,int);
  //@}

protected:
  vtkSVGroupsClipper();
  ~vtkSVGroupsClipper();

  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *);

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  /** \brief After all the polydata has been clipped, this is used to find
   *  locations where the groups separate.
   *  \param separateIds empty list to be filled with all ids of points that
   *  are touching cells of three different regions. */
  int FindGroupSeparatingPoints(vtkPolyData *pd,
                                vtkIdList *separateIds);

  /** \brief Goes through, finds all sets of three points that indicate a group
   *  separation region and punches a hole through the surface.
   *  \param seperatIds from FindGroupSeparatingPoints.
   *  \param newPoints list of new points that need to be added to surface.*/
  int PunchHoles(vtkPolyData *pd, vtkIdList *separateIds, vtkPoints *newPoints);


  /** \brief Goes through and gets the loop of points around the hole that need to be
   *  used to connect with new points.
   *  \param newPoints from PunchHoles. */
  int FillHoles(vtkPolyData *pd, vtkPoints *newPoints);

  /** \brief Adds new point to the surface and attach an edge to all points surrounding
   *  the hole to give a watertight surface.
   *  \param boundary the boundary of the hole.
   *  \param center the new point to be added. */
  int FillRegionGroups(vtkPolyData *pd, vtkPolyData *boundary, double center[3]);

  char *CenterlineGroupIdsArrayName;
  char *CenterlineRadiusArrayName;
  char *GroupIdsArrayName;
  char *BlankingArrayName;

  vtkPolyData *WorkPd;
  vtkPolyData *Centerlines;
  vtkIdList *CenterlineGroupIds;

  int ClipAllCenterlineGroupIds;
  int UseRadiusInformation;

  double CutoffRadiusFactor;
  double ClipValue;

private:
  vtkSVGroupsClipper(const vtkSVGroupsClipper&);  // Not implemented.
  void operator=(const vtkSVGroupsClipper&);  // Not implemented.
};

#endif
