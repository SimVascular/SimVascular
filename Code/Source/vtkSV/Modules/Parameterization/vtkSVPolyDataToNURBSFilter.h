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
 *  \class vtkSVPolyDataToNURBSFilter
 *
 *  \brief This filter takes a polydata, calls another filter to decompose
 *  that geometry and obtain its polycube structure. Then using the polycube
 *  structure, it maps each portion of the polycube to a planar domain to
 *  form a full parameterization. Global interplation techniques are then
 *  used to form the full nurbs surface.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVPolyDataToNURBSFilter_h
#define vtkSVPolyDataToNURBSFilter_h

#include "vtkPolyDataAlgorithm.h"
#include "vtkSVParameterizationModule.h" // For exports

#include "vtkAppendPolyData.h"
#include "vtkEdgeTable.h"
#include "vtkFloatArray.h"
#include "vtkSVGeneralizedPolycube.h"
#include "vtkPolyData.h"

class VTKSVPARAMETERIZATION_EXPORT vtkSVPolyDataToNURBSFilter : public vtkPolyDataAlgorithm
{
public:
  static vtkSVPolyDataToNURBSFilter* New();
  void PrintSelf(ostream& os, vtkIndent indent);

  //@{
  // \brief Flag to indicate whether texture coordinates should be added to input
  vtkGetMacro(AddTextureCoordinates, double);
  vtkSetMacro(AddTextureCoordinates, double);
  //@}

  //@{
  // \brief Array names for the segment and group ids. Must exist on input
  // pd if also providing centerlines. Otherwise, these are the names
  // that will be used to segment the polydata with the created centerlines
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
  vtkGetStringMacro(BooleanPathArrayName);
  vtkSetStringMacro(BooleanPathArrayName);
  //@}

  //@{
  // \brief Macro to Get/Set objects used by filter
  vtkGetObjectMacro(CenterlinesPd, vtkPolyData);
  vtkSetObjectMacro(CenterlinesPd, vtkPolyData);
  vtkGetObjectMacro(BranchBaseDomainPd, vtkPolyData);
  vtkSetObjectMacro(BranchBaseDomainPd, vtkPolyData);
  vtkGetObjectMacro(BifurcationBaseDomainPd, vtkPolyData);
  vtkSetObjectMacro(BifurcationBaseDomainPd, vtkPolyData);
  vtkGetObjectMacro(TexturedPd, vtkPolyData);
  vtkSetObjectMacro(TexturedPd, vtkPolyData);
  vtkGetObjectMacro(LoftedPd, vtkPolyData);
  vtkSetObjectMacro(LoftedPd, vtkPolyData);
  //@}

  /** \brief Convenience function to write groups file read by simvascular. TODO: Move to IO. */
  static int WriteToGroupsFile(vtkPolyData *pd, std::string fileName);

  /** \brief Function to get spacing of parameterization. */
  static int GetSpacingOfTCoords(vtkPolyData *pd, double &xSpacing, double &ySpacing);

  /** \brief Gets a new point order based on node location rather than node number
   *  ordering. */
  static int GetNewPointOrder(vtkPolyData *pd, double xSpacing, double ySpacing,
                              vtkIntArray *newPointOrder);

protected:
  vtkSVPolyDataToNURBSFilter();
  ~vtkSVPolyDataToNURBSFilter();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector);

  int RunFilter(); // Perform operation
  int MakeBaseDomains(); // Perform operation
  int PerformMappings(); // Perform all mappings
  int SliceAndDice();  // Send to slice and dice filter

  /** \brief Gets one individual segment given a segment id. Also returns the
   *  associated surgery lines. */
  int GetSegment(const int segmentId, vtkPolyData *segmentPd, vtkPolyData *surgeryLinePd);

  /** \brief Gets a single slice of the polydata given the slice id and the segment polydata. */
  int GetSlice(const int sliceId, vtkPolyData *segmentPd, vtkPolyData *slicePd);

  /** \brief Performs the mapping of one branch.
   *  \param branchId branch to map.
   *  \param appender appender to attach the parameterization mapped to the input polydata.
   *  \param inputAppender appender to attach the form given to mapping code with textures.
   *  \param loftAppender appender to attach the lofted nurbs representation. */
  int MapBranch(const int branchId, vtkAppendPolyData *appender,
                vtkAppendPolyData *inputAppender,
                vtkAppendPolyData *loftAppender);

  /** \brief Performs the mapping of one bifurcation.
   *  \param branchId branch to map.
   *  \param appender appender to attach the parameterization mapped to the input polydata.
   *  \param inputAppender appender to attach the form given to mapping code with textures.
   *  \param loftAppender appender to attach the lofted nurbs representation. */
  int MapBifurcation(const int bifurcationId, vtkAppendPolyData *appender,
                     vtkAppendPolyData *inputAppender,
                     vtkAppendPolyData *loftAppender);

  /** \brief Maps a single slice to the given base domain..
   *  \param slicePd The slice polydata.
   *  \param surgeryLinePd Teh surgery lines for the mapping.
   *  \param sliceBaseDomain Empty polydata to hold the paramterized slice.
   *  \param firstCorners Set of 4 nodes that are top corner constraints.
   *  \param secondCorners Set of 4 nodes that are bottom corner constraints. */
  int MapSliceToBaseDomain(vtkPolyData *slicePd, vtkPolyData *surgeryLinePd, vtkPolyData *sliceBaseDomain,
                     vtkIntArray *firstCorners, vtkIntArray *secondCorners,
                     double xvec[3], double zvec[3]);

  /** \brief Maps a single open slice to the given base domain.
   *  \param slicePd The slice polydata.
   *  \param sliceBaseDomain Empty polydata to hold the paramterized slice.
   *  \param firstCorners Set of 4 nodes that are top corner constraints.
   *  \param secondCorners Set of 4 nodes that are bottom corner constraints. */
  int MapOpenSliceToBaseDomain(vtkPolyData *slicePd, vtkPolyData *sliceBaseDomain,
                       vtkIntArray *firstCorners, vtkIntArray *secondCorners,
                       double xvec[3], double zvec[3]);

  /** \brief send the final paramterized portion to a mapping filter that
   *  maps the base domain to the original surface. See vtkSVMapInterpolator.*/
  int InterpolateMapOntoTarget(vtkPolyData *sourceS2Pd,
                               vtkPolyData *targetPd,
                               vtkPolyData *targetS2Pd,
                               vtkPolyData *mappedPd);

  /** \brief Use the parameterization to add texture coordinates to the
   *  original surface. */
  int UseMapToAddTextureCoordinates(vtkPolyData *pd,
                                    vtkPolyData *mappedPd,
                                    const double xSize,
                                    const double ySize);

  /** \brief Use the vtkSVNURBS code to loft a polydata surface for portion
   *  pd. */
  int LoftNURBSSurface(vtkPolyData *pd, vtkPolyData *loftedPd);

private:
  vtkSVPolyDataToNURBSFilter(const vtkSVPolyDataToNURBSFilter&);  // Not implemented.
  void operator=(const vtkSVPolyDataToNURBSFilter&);  // Not implemented.

  char *SliceIdsArrayName;
  char *GroupIdsArrayName;
  char *SegmentIdsArrayName;
  char *SphereRadiusArrayName;
  char *InternalIdsArrayName;
  char *BoundaryPointsArrayName;
  char *DijkstraArrayName;
  char *BooleanPathArrayName;

  vtkPolyData *InputPd;
  vtkPolyData *BranchBaseDomainPd;
  vtkPolyData *BifurcationBaseDomainPd;
  vtkPolyData *ParameterizedPd;
  vtkPolyData *TexturedPd;
  vtkPolyData *LoftedPd;
  vtkPolyData *CenterlinesPd;
  vtkPolyData *SurgeryLinesPd;

  vtkSVGeneralizedPolycube *Polycube;

  int AddTextureCoordinates;
  int BaseDomainXResolution;
  int BaseDomainYResolution;


};

#endif
