/*=========================================================================

Program:   VMTK
Module:    $RCSfile: vtkvmtkPolyDataCenterlineSections.h,v $
Language:  C++
Date:      $Date: 2006/10/17 15:16:16 $
Version:   $Revision: 1.1 $

  Copyright (c) Luca Antiga, David Steinman. All rights reserved.
  See LICENSE file for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm 
  for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// .NAME vtkvmtkPolyDataCenterlineSections - Indicate centerline branches and bifurcations and calculate cross-sectional area.
// .SECTION Description
// ...

#define SV_OK                 1
#define SV_ERROR              0

#ifndef __vtkvmtkPolyDataCenterlineSections_h
#define __vtkvmtkPolyDataCenterlineSections_h

#include "vtkPolyDataAlgorithm.h"
//#include "vtkvmtkComputationalGeometryWin32Header.h"
#include "vtkvmtkWin32Header.h"
#include "vtkPolyData.h"

class VTK_VMTK_COMPUTATIONAL_GEOMETRY_EXPORT vtkvmtkPolyDataCenterlineSections : public vtkPolyDataAlgorithm
{
  public: 
  vtkTypeMacro(vtkvmtkPolyDataCenterlineSections,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) VTK_OVERRIDE; 

  static vtkvmtkPolyDataCenterlineSections* New();

  vtkSetObjectMacro(Centerlines,vtkPolyData);
  vtkGetObjectMacro(Centerlines,vtkPolyData);

  vtkGetObjectMacro(Surface,vtkPolyData);

  vtkSetStringMacro(CenterlineSectionAreaArrayName);
  vtkGetStringMacro(CenterlineSectionAreaArrayName);

  vtkSetStringMacro(CenterlineSectionMinSizeArrayName);
  vtkGetStringMacro(CenterlineSectionMinSizeArrayName);

  vtkSetStringMacro(CenterlineSectionMaxSizeArrayName);
  vtkGetStringMacro(CenterlineSectionMaxSizeArrayName);

  vtkSetStringMacro(CenterlineSectionShapeArrayName);
  vtkGetStringMacro(CenterlineSectionShapeArrayName);

  vtkSetStringMacro(CenterlineSectionClosedArrayName);
  vtkGetStringMacro(CenterlineSectionClosedArrayName);

  vtkSetStringMacro(CenterlineSectionBifurcationArrayName);
  vtkGetStringMacro(CenterlineSectionBifurcationArrayName);

  vtkSetStringMacro(CenterlineSectionNormalArrayName);
  vtkGetStringMacro(CenterlineSectionNormalArrayName);

  vtkSetStringMacro(RadiusArrayName);
  vtkGetStringMacro(RadiusArrayName);

  vtkSetStringMacro(GlobalNodeIdArrayName);
  vtkGetStringMacro(GlobalNodeIdArrayName);

  vtkSetStringMacro(BifurcationIdArrayNameTmp);
  vtkGetStringMacro(BifurcationIdArrayNameTmp);

  vtkSetStringMacro(BifurcationIdArrayName);
  vtkGetStringMacro(BifurcationIdArrayName);

  vtkSetStringMacro(BranchIdArrayNameTmp);
  vtkGetStringMacro(BranchIdArrayNameTmp);

  vtkSetStringMacro(BranchIdArrayName);
  vtkGetStringMacro(BranchIdArrayName);

  vtkSetStringMacro(PathArrayName);
  vtkGetStringMacro(PathArrayName);

  protected:
  vtkvmtkPolyDataCenterlineSections();
  ~vtkvmtkPolyDataCenterlineSections();  

  virtual int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *) VTK_OVERRIDE;

  // based on original vmtk function
  int ComputeCenterlineSections(vtkPolyData* output);

  // new functions to detect branches/bifurcations
  int CleanBifurcation();
  int ConnectivityCenterline(vtkPolyData* geo, char* nameThis, char* nameOther);
  int GroupCenterline();
  int SplitCenterline(vtkPolyData* branches, vtkPolyData* bifurcations);
  int GenerateCleanCenterline();
  int BranchSurface(char* nameBranch, char* nameBifurcation);
  int CalculateTangent();
  int RefineCapPoints();

  bool IsOnePiece(vtkPolyData* inp);

  vtkPolyData* Centerlines;
  vtkPolyData* Surface;

  char* RadiusArrayName;
  char* GlobalNodeIdArrayName;
  char* BifurcationIdArrayNameTmp;
  char* BifurcationIdArrayName;
  char* BranchIdArrayNameTmp;
  char* BranchIdArrayName;
  char* PathArrayName;
  char* CenterlineSectionAreaArrayName;
  char* CenterlineSectionMinSizeArrayName;
  char* CenterlineSectionMaxSizeArrayName;
  char* CenterlineSectionShapeArrayName;
  char* CenterlineSectionClosedArrayName;
  char* CenterlineSectionBifurcationArrayName;
  char* CenterlineSectionNormalArrayName;

  private:
  vtkvmtkPolyDataCenterlineSections(const vtkvmtkPolyDataCenterlineSections&);  // Not implemented.
  void operator=(const vtkvmtkPolyDataCenterlineSections&);  // Not implemented.
};

#endif
