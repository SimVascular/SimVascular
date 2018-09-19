/*=========================================================================

  Program:   Visualization Toolkit
  Module:    vtkSVPolyDataSurfaceInspector.h

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef vtkSVPolyDataSurfaceInspector_h
#define vtkSVPolyDataSurfaceInspector_h

#include "vtkPolyDataAlgorithm.h"
#include "vtkSVMiscModule.h" // For export

class VTKSVMISC_EXPORT vtkSVPolyDataSurfaceInspector : public vtkPolyDataAlgorithm
{
public:
  static vtkSVPolyDataSurfaceInspector *New();
  vtkTypeMacro(vtkSVPolyDataSurfaceInspector,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Get and set whether to calculate the number of connected regions
  vtkGetMacro(CheckNumberOfConnectedRegions, int);
  vtkSetMacro(CheckNumberOfConnectedRegions, int);
  vtkBooleanMacro(CheckNumberOfConnectedRegions, int);
  //@}

  //@{
  /// \brief Get and set whether to check for the number of holes
  vtkGetMacro(CheckNumberOfHoles, int);
  vtkSetMacro(CheckNumberOfHoles, int);
  vtkBooleanMacro(CheckNumberOfHoles, int);
  //@}

  //@{
  /// \brief Get macros for surface stats
  vtkGetMacro(NumberOfElements, int);
  vtkGetMacro(NumberOfPoints, int);
  vtkGetMacro(NumberOfEdges, int);
  vtkGetMacro(NumberOfOpenEdges, int);
  vtkGetMacro(NumberOfNonTriangularElements, int);
  vtkGetMacro(NumberOfNonManifoldEdges, int);
  vtkGetMacro(SurfaceGenus, int);
  vtkGetMacro(NumberOfConnectedRegions, int);
  vtkGetMacro(NumberOfHoles, int);
  //@}


protected:

  vtkSVPolyDataSurfaceInspector();
  ~vtkSVPolyDataSurfaceInspector() {;}

  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *) override;

private:

  int NumberOfElements;
  int NumberOfPoints;
  int NumberOfEdges;
  int NumberOfOpenEdges;
  int NumberOfNonTriangularElements;
  int NumberOfNonManifoldEdges;
  int SurfaceGenus;
  int NumberOfConnectedRegions;
  int NumberOfHoles;

  int CheckNumberOfConnectedRegions;
  int CheckNumberOfHoles;

  vtkSVPolyDataSurfaceInspector(const vtkSVPolyDataSurfaceInspector&);  // Not implemented.
  void operator=(const vtkSVPolyDataSurfaceInspector&);  // Not implemented.
};

#endif
