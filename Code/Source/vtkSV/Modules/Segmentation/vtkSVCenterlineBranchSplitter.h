/*=========================================================================

Program:   VMTK
Module:    $RCSfile: vtkSVCenterlineBranchSplitter.h,v $
Language:  C++
Date:      $Date: 2006/04/06 16:46:43 $
Version:   $Revision: 1.5 $

  Copyright (c) Luca Antiga, David Steinman. All rights reserved.
  See LICENCE file for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm
  for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
  // .NAME vtkSVCenterlineBranchSplitter - ...
  // .SECTION Description
  // ...

#ifndef vtkSVCenterlineBranchSplitter_h
#define vtkSVCenterlineBranchSplitter_h

#include "vtkSVSegmentationModule.h" // For exports

#include "vtkvmtkCenterlineSplittingAndGroupingFilter.h"

class VTKSVSEGMENTATION_EXPORT vtkSVCenterlineBranchSplitter : public vtkvmtkCenterlineSplittingAndGroupingFilter
{
  public:
  vtkTypeMacro(vtkSVCenterlineBranchSplitter,vtkvmtkCenterlineSplittingAndGroupingFilter);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  static vtkSVCenterlineBranchSplitter *New();

  //@{
  /// \brief Get/Set to use absolute distance
  vtkSetMacro(UseAbsoluteMergeDistance, int);
  vtkGetMacro(UseAbsoluteMergeDistance, int);
  vtkBooleanMacro(UseAbsoluteMergeDistance, int);
  //@}

  //@{
  /// \brief Get/Set to use absolute distance
  vtkSetMacro(RadiusMergeRatio, double);
  vtkGetMacro(RadiusMergeRatio, double);
  //@}

  //@{
  /// \brief Get/Set to use absolute distance
  vtkSetMacro(MergeDistance, double);
  vtkGetMacro(MergeDistance, double);
  //@}

  protected:
  vtkSVCenterlineBranchSplitter();
  ~vtkSVCenterlineBranchSplitter();

  virtual void ComputeCenterlineSplitting(vtkPolyData* input, vtkIdType cellId) override;
  virtual void GroupTracts(vtkPolyData* input, vtkPolyData* centerlineTracts) override;
  virtual void SplitCenterline(vtkPolyData* input, vtkIdType cellId, int numberOfSplittingPoints, const vtkIdType* subIds, const double* pcoords, const int* tractBlanking, vtkPolyData* splitCenterline) override;

  int UseAbsoluteMergeDistance;
  double RadiusMergeRatio;
  double MergeDistance;

  private:
  vtkSVCenterlineBranchSplitter(const vtkSVCenterlineBranchSplitter&);  // Not implemented.
  void operator=(const vtkSVCenterlineBranchSplitter&);  // Not implemented.
};

#endif
