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
// .NAME vtkvmtkPolyDataCenterlineSections - Extract a set of vessel sections from a surface along a centerline.
// .SECTION Description
//  The set of vessel sections contain the profile as well as the the following information about the section:
//  - Centerline Section Area
//  - Centerline Section Min Size
//  - Centerline Section Max Size
//  - Centerline Section Shape
//  - Centerline Section Closed

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

  protected:
  vtkvmtkPolyDataCenterlineSections();
  ~vtkvmtkPolyDataCenterlineSections();  

  virtual int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *) VTK_OVERRIDE;

  void ComputeCenterlineSections(vtkPolyData* output);

  void CleanBifurcation();
  void ConnectivityCenterline(vtkPolyData* geo, char* nameThis, char* nameOther);
  void GroupCenterline();
  void SplitCenterline(vtkPolyData* branches, vtkPolyData* bifurcations);
  void GenerateCleanCenterline();
  void BranchSurface(char* nameBranch, char* nameBifurcation);
  void CalculateTangent();
  void RefineCapPoints();

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
