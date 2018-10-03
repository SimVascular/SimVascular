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
 *  \class vtkSVParameterizeSurfaceOnPolycube
 *  \brief Using a polydata centerlines, separate the polydata into regions
 *  based on the centerlines
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVParameterizeSurfaceOnPolycube_h
#define vtkSVParameterizeSurfaceOnPolycube_h

#include "vtkSVParameterizationModule.h" // For exports

#include "vtkIdList.h"
#include "vtkMatrix4x4.h"
#include "vtkPolyDataAlgorithm.h"
#include "vtkPolyData.h"
#include "vtkStructuredGrid.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGlobals.h"

class VTKSVPARAMETERIZATION_EXPORT vtkSVParameterizeSurfaceOnPolycube : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkSVParameterizeSurfaceOnPolycube,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  static vtkSVParameterizeSurfaceOnPolycube *New();

  //@{
  /// \brief Get/Set macro for surface polycube
  vtkSetObjectMacro(PolycubePd,vtkPolyData);
  vtkGetObjectMacro(PolycubePd,vtkPolyData);
  //@}

  //@{
  /// \brief Get/Set macro for surface polycube
  vtkGetObjectMacro(PolycubeOnSurfacePd,vtkPolyData);
  //@}

  //@{
  /// \brief Get/Set temporary
  vtkGetObjectMacro(NURBSSurfaceRepresentationPd,vtkPolyData);
  //@}

  //@{
  /// \brief Get/Set macro for surface polycube
  vtkSetObjectMacro(PolycubeUg,vtkUnstructuredGrid);
  vtkGetObjectMacro(PolycubeUg,vtkUnstructuredGrid);
  //@}

  //@{
  /// \brief Get/Set macro for array name used by the filter. Must
  //  be present on the surface and polycube.
  vtkSetStringMacro(GroupIdsArrayName);
  vtkGetStringMacro(GroupIdsArrayName);
  //@}

  //@{
  /// \brief Get/Set macro for array name used by the filter. Must
  // be presetn on the surface and polycube.
  vtkSetStringMacro(PatchIdsArrayName);
  vtkGetStringMacro(PatchIdsArrayName);
  //@}

  //@{
  /// \brief Get/Set macro for array name used for the structured grid points
  // ids on the polycube unstructured grid
  vtkSetStringMacro(GridIdsArrayName);
  vtkGetStringMacro(GridIdsArrayName);
  //@}

  //@{
  /// \brief Get/Set the radius information
  vtkSetMacro(EnforcePolycubeConnectivity, int);
  vtkGetMacro(EnforcePolycubeConnectivity, int);
  vtkBooleanMacro(EnforcePolycubeConnectivity, int);
  //@}

protected:
  vtkSVParameterizeSurfaceOnPolycube();
  ~vtkSVParameterizeSurfaceOnPolycube();

  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *) override;

  int RotateGroupToGlobalAxis(vtkPolyData *pd,
                              const int thresholdId,
                              std::string arrayName,
                              vtkPolyData *rotPd,
                              vtkMatrix4x4 *rotMatrix0,
                              vtkMatrix4x4 *rotMatrix1);
  int InterpolateMapOntoTarget(vtkPolyData *sourceBasePd,
                               vtkPolyData *targetPd,
                               vtkPolyData *targetBasePd,
                               vtkPolyData *mappedPd,
                               std::string dataMatchingArrayName);

  int GetInteriorPointMaps(vtkPolyData *pdWithAllInterior,
                           vtkPolyData *pdWithCleanInterior,
                           vtkPolyData *pdWithoutInterior,
                           std::vector<int> &ptMap,
                           std::vector<std::vector<int> > &invPtMap);
  int RemoveInteriorCells(vtkPolyData *quadMesh);

  vtkPolyData *WorkPd;
  vtkPolyData *PolycubePd;
  vtkPolyData *SurfaceOnPolycubePd;
  vtkPolyData *PolycubeOnSurfacePd;
  vtkPolyData *NURBSSurfaceRepresentationPd;

  vtkUnstructuredGrid *PolycubeUg;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  int FormNURBSSurface();

  char *GroupIdsArrayName;
  char *PatchIdsArrayName;
  char *GridIdsArrayName;

  int EnforcePolycubeConnectivity;

private:
  vtkSVParameterizeSurfaceOnPolycube(const vtkSVParameterizeSurfaceOnPolycube&);  // Not implemented.
  void operator=(const vtkSVParameterizeSurfaceOnPolycube&);  // Not implemented.
};

#endif
