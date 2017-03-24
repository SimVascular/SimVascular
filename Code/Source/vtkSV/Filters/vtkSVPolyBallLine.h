/*=========================================================================

Program:   VMTK
Module:    $RCSfile: vtkSVPolyBallLine.h,v $
Language:  C++
Date:      $Date: 2006/04/06 16:46:43 $
Version:   $Revision: 1.4 $

  Copyright (c) Luca Antiga, David Steinman. All rights reserved.
  See LICENCE file for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm
  for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/**
 *  \class vtkSVPolyBallLine
 */

#ifndef vtkSVPolyBallLine_h
#define vtkSVPolyBallLine_h

#include "vtkImplicitFunction.h"
#include "vtkSVFiltersModule.h" // For export
#include "vtkPolyData.h"
#include "vtkIdList.h"

class VTKSVFILTERS_EXPORT vtkSVPolyBallLine : public vtkImplicitFunction
{
public:

  static vtkSVPolyBallLine *New();
  vtkTypeMacro(vtkSVPolyBallLine,vtkImplicitFunction);
  void PrintSelf(ostream& os, vtkIndent indent);

  //@{
  /// \brief Evaluate polyball.
  double EvaluateFunction(double x[3]);
  double EvaluateFunction(double x, double y, double z)
  {return this->vtkImplicitFunction::EvaluateFunction(x, y, z); } ;
  //@}

  //@{
  /// \brief Evaluate polyball gradient.
  void EvaluateGradient(double x[3], double n[3]);
  //@}

  //@{
  /// \brief Set / get input poly data.
  vtkSetObjectMacro(Input,vtkPolyData);
  vtkGetObjectMacro(Input,vtkPolyData);
  //@}

  //@{
  /// \brief Set / get input cell ids used for the function.
  vtkSetObjectMacro(InputCellIds,vtkIdList);
  vtkGetObjectMacro(InputCellIds,vtkIdList);
  //@}

  //@{
  /// \brief Set / get a single input cell id used for the function.
  vtkSetMacro(InputCellId,vtkIdType);
  vtkGetMacro(InputCellId,vtkIdType);
  //@}

  //@{
  /// \brief Set / get poly ball radius array name.
  vtkSetStringMacro(PolyBallRadiusArrayName);
  vtkGetStringMacro(PolyBallRadiusArrayName);
  //@}

  //@{
  /// \brief Get the id of the last nearest poly ball center.
  vtkGetMacro(LastPolyBallCellId,vtkIdType);
  vtkGetMacro(LastPolyBallCellSubId,vtkIdType);
  vtkGetMacro(LastPolyBallCellPCoord,double);
  vtkGetVectorMacro(LastPolyBallCenter,double,3);
  vtkGetMacro(LastPolyBallCenterRadius,double);
  //@}

  //@{
  /// \brief Use radius information
  vtkSetMacro(UseRadiusInformation,int);
  vtkGetMacro(UseRadiusInformation,int);
  vtkBooleanMacro(UseRadiusInformation,int);
  //@}

  static double ComplexDot(double x[4], double y[4]);

protected:
  vtkSVPolyBallLine();
  ~vtkSVPolyBallLine();

  char* PolyBallRadiusArrayName;

  vtkPolyData* Input;
  vtkIdList* InputCellIds;
  vtkIdType InputCellId;

  vtkIdType LastPolyBallCellId;
  vtkIdType LastPolyBallCellSubId;

  int UseRadiusInformation;

  double LastPolyBallCellPCoord;
  double LastPolyBallCenter[3];
  double LastPolyBallCenterRadius;

private:
  vtkSVPolyBallLine(const vtkSVPolyBallLine&);  // Not implemented.
  void operator=(const vtkSVPolyBallLine&);  // Not implemented.
};

#endif


