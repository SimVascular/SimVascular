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
 *  \class vtkSVPolyBallLine
 */

#ifndef vtkSVPolyBallLine_h
#define vtkSVPolyBallLine_h

#include "vtkSVSegmentationModule.h" // For export

#include "vtkCellLocator.h"
#include "vtkIdList.h"
#include "vtkImplicitFunction.h"
#include "vtkPolyData.h"
#include "vtkPointLocator.h"

#include "vtkSVGlobals.h"

class VTKSVSEGMENTATION_EXPORT vtkSVPolyBallLine : public vtkImplicitFunction
{
public:

  static vtkSVPolyBallLine *New();
  vtkTypeMacro(vtkSVPolyBallLine,vtkImplicitFunction);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Evaluate polyball.
  double EvaluateFunction(double x[3]) override;
  double EvaluateFunction(double x, double y, double z)
  {return this->vtkImplicitFunction::EvaluateFunction(x, y, z); } ;
  //@}

  //@{
  /// \brief Evaluate polyball gradient.
  void EvaluateGradient(double x[3], double n[3]) override;
  //@}
  //
  //@{
  /// \brief Set / get input poly data.
  vtkSetVector3Macro(LastLocalCoordX, double);
  vtkGetVector3Macro(LastLocalCoordX, double);
  vtkSetVector3Macro(LastLocalCoordY, double);
  vtkGetVector3Macro(LastLocalCoordY, double);
  vtkSetVector3Macro(LastLocalCoordZ, double);
  vtkGetVector3Macro(LastLocalCoordZ, double);
  //@}

  //@{
  /// \brief Set / get input poly data.
  vtkSetVector3Macro(PointNormal, double);
  vtkGetVector3Macro(PointNormal, double);
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
  /// \brief Set / get poly ball radius array name.
  vtkSetStringMacro(LocalCoordinatesArrayName);
  vtkGetStringMacro(LocalCoordinatesArrayName);
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

  //@{
  /// \brief Use given normal for directional help
  vtkSetMacro(UsePointNormal,int);
  vtkGetMacro(UsePointNormal,int);
  vtkBooleanMacro(UsePointNormal,int);
  //@}

  //@{
  /// \brief
  vtkSetMacro(PointNormalThreshold,double);
  vtkGetMacro(PointNormalThreshold,double);
  //@}

  //@{
  /// \brief
  vtkSetMacro(CellSearchRadius,double);
  vtkGetMacro(CellSearchRadius,double);
  //@}

  //@{
  vtkSetMacro(FastEvaluate,int);
  vtkGetMacro(FastEvaluate,int);
  vtkBooleanMacro(FastEvaluate,int);
  //@}

  //@{
  /// \brief Use radius information
  vtkSetMacro(UseLocalCoordinates,int);
  vtkGetMacro(UseLocalCoordinates,int);
  vtkBooleanMacro(UseLocalCoordinates,int);
  //@}

  static double ComplexDot(double x[4], double y[4]);

  void BuildLocator();
  void PreprocessInputForFastEvaluate();

protected:
  vtkSVPolyBallLine();
  ~vtkSVPolyBallLine();

  char* PolyBallRadiusArrayName;
  char* LocalCoordinatesArrayName;

  vtkPolyData* Input;
  vtkIdList* InputCellIds;
  vtkIdType InputCellId;

  vtkPointLocator *PointLocator;
  vtkCellLocator  *CellLocator;

  vtkIdType LastPolyBallCellId;
  vtkIdType LastPolyBallCellSubId;

  int UseRadiusInformation;
  int UsePointNormal;
  int UseLocalCoordinates;
  int FastEvaluate;

  double PointNormalThreshold;
  double LastPolyBallCellPCoord;
  double LastPolyBallCenter[3];
  double LastPolyBallCenterRadius;
  double LastLocalCoordX[3];
  double LastLocalCoordY[3];
  double LastLocalCoordZ[3];
  double PointNormal[3];
  double CellSearchRadius;

  std::vector<std::vector<int> > BifurcationPointCellsVector;
  std::vector<std::vector<int> > CellPointsVector;
  std::vector<XYZ> PointsVector;
  std::vector<double> RadiusVector;

private:
  vtkSVPolyBallLine(const vtkSVPolyBallLine&);  // Not implemented.
  void operator=(const vtkSVPolyBallLine&);  // Not implemented.
};

#endif


