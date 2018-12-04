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

/** \file vtkSVUpdeSmoothing.h
 *  \brief Smoothes a surface using a constrained method solving the
 *  minimization between the original surface and a laplacian smoothed
 *  surface
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 *  \note Most functions in class call functions in cv_polydatasolid_utils.
 */

#ifndef vtkSVUpdeSmoothing_h
#define vtkSVUpdeSmoothing_h

#include "vtkPolyDataAlgorithm.h"
#include "vtkSVGeometryModule.h" // for export
#include "vtkCellLocator.h"
#include <set>

class VTKSVGEOMETRY_EXPORT vtkSVUpdeSmoothing : public vtkPolyDataAlgorithm
{
public:
  static vtkSVUpdeSmoothing* New();
  vtkTypeMacro(vtkSVUpdeSmoothing, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  vtkGetObjectMacro(SourcePd,vtkPolyData);
  vtkSetObjectMacro(SourcePd,vtkPolyData);
  //@}

  //@{
  vtkGetMacro(NumberOfOuterSmoothOperations,int);
  vtkSetMacro(NumberOfOuterSmoothOperations,int);
  //@}

  //@{
  vtkGetMacro(NumberOfInnerSmoothOperations,int);
  vtkSetMacro(NumberOfInnerSmoothOperations,int);
  //@}

  //@{
  vtkGetMacro(UseInputAsSource,int);
  vtkSetMacro(UseInputAsSource,int);
  //@}

  //@{
  // Description:
  // Set/get the name for the point array attached to the input surface
  vtkSetStringMacro(SmoothPointArrayName);
  vtkGetStringMacro(SmoothPointArrayName);
  //@}

protected:
  vtkSVUpdeSmoothing();
  ~vtkSVUpdeSmoothing();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;


  int RunFilter(vtkPolyData *original, vtkPolyData *output);

  int UntangleSurface(vtkDoubleArray *shapeImproveFunction,
                      vtkDoubleArray *shapeImproveDirection);
  int SmoothSurface(vtkDoubleArray *shapeImproveFunction,
                    vtkDoubleArray *shapeImproveDirection);

  int PointCellStatus(double currentPt[3], int sourceCell, int &pointCellStatus);
  int EdgeStatusWithDir(double currentPt[3], int sourceCell, double moveDir[3], int &edgeStatus) ;
  int ComputeOptimizationPoint(int pointId, double pt0[3], double pt1[3], double pt2[3],
                               double oppositePt[3], int sourceCell, double newPt[3]);
  int ComputeOptimizationDirection(double pt0[3], double pt1[3], double currentPt[3], double oppositePt[3], int sourceCell, double newDir[3]);
  int MovePointToEdge(double currentPt[3], int sourceCell, double moveDir[3], double newPt[3], int &edgeStatus);
  int MovePointFromEdgeToEdge(double currentPt[3], int ptId0, int ptId1, int ptId2, double moveDir[3], double newPt[3], int &edgeStatus);
  int MovePointFromPointToEdge(double currentPt[3], int ptId0, int ptId1, int ptId2, double moveDir[3], double newPt[3], int &edgeStatus);
  int MovePointDistance(double currentPt[3], double moveDir[3], double length, double newPt[3]);
  int ComputeUntanglingDerivatives(double pt0[3], double pt1[3], double pt2[3], double oppositePt[3], double theta, double newDir[3]);
  int GetJacobianDerivatives(double pt0[3], double pt1[3], double pt2[3],
                           double pPt0[3], double pPt1[3], double pPt2[3],
                           double &dJdX, double &dJdY, double &dJdZ);
  int ComputeShapeImprovementDerivatives(double pt0[3], double pt1[3], double pt2[3], double oppositePt[3],
                                       double dK[3]);

  int ComputeUntanglingFunction(double pt0[3], double pt1[3], double pt2[3], double compareNormal[3], double theta, double &f);

  int ComputeShapeImprovementFunction(double pt0[3], double pt1[3], double pt2[3], double oppositePt[3], double &f);
  int ComputeVertexCondition(int ptId, double &vertexCondition, double optDirection[3]);
  int CheckVertexInverted(int ptId, double compareNormal[3], double &vertexCondition, double optDirection[3]);

  double Determinant(double mat[4]);

  int GetJacobians(double pPt0[3], double pPt1[3], double pPt2[3],
                   double J0[4], double J1[4], double J2[4]);

  int UseInputAsSource;
  int NumberOfOuterSmoothOperations;
  int NumberOfInnerSmoothOperations;
  double Alpha;
  double Beta;

  vtkPolyData *WorkPd;
  vtkPolyData *SourcePd;
  vtkIntArray *SmoothPointArray;
  vtkCellLocator *CellLocator;

  vtkDataArray *SourceCellNormals;
  vtkDataArray *SourcePointNormals;
  vtkDataArray *OriginalCellNormals;
  vtkDataArray *OriginalPointNormals;

  char *SmoothPointArrayName;

  std::vector<std::vector<int> > CellsOnSource;
  std::vector<std::vector<int> > PointCells;
  std::vector<std::vector<int> > CellPoints;
  std::vector<int> FixedPoints;

private:
  vtkSVUpdeSmoothing(const vtkSVUpdeSmoothing&);  // Not implemented.
  void operator=(const vtkSVUpdeSmoothing&);  // Not implemented.
};

#endif


