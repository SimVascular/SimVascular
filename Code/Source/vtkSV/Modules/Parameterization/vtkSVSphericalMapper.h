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
 *  \class vtkSVSphericalMapper
 *  \brief This is a vtk filter to map a triangulated surface to a sphere.
 *  \details This filter uses the heat flow method to map a triangulated
 *  surface to a sphere. The first step is to compute the Tutte Energy, and
 *  the second step is to perform the conformal map. For more details, see
 *  Gu et al., Genus Zero Surface Conformal Mapping and Its
 *  Application to Brain Surface Mapping, 2004.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVSphericalMapper_h
#define vtkSVSphericalMapper_h

#include "vtkSVParameterizationModule.h" // For exports

#include "vtkPolyDataAlgorithm.h"

#include "vtkEdgeTable.h"
#include "vtkFloatArray.h"
#include "vtkPolyData.h"

class VTKSVPARAMETERIZATION_EXPORT vtkSVSphericalMapper : public vtkPolyDataAlgorithm
{
public:
  static vtkSVSphericalMapper* New();
  //vtkTypeRevisionMacro(vtkSVSphericalMapper, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  // Description:
  // Print statements used for debugging
  vtkGetMacro(Verbose, int);
  vtkSetMacro(Verbose, int);

  // Description:
  // Get and set macros for the time step to be taken during the computation
  // of the Tutte Energy and the Harmonic Energy
  vtkGetMacro(InitialTimeStep, double);
  vtkSetMacro(InitialTimeStep, double);

  // Description:
  // Get and set macros for the Energy criterion during the computation of
  // the Tutte Energy and the Harmonic Energy
  vtkGetMacro(TutteEnergyCriterion, double);
  vtkSetMacro(TutteEnergyCriterion, double);
  vtkGetMacro(HarmonicEnergyCriterion, double);
  vtkSetMacro(HarmonicEnergyCriterion, double);

  // Description:
  // Print statements used for debugging
  vtkGetMacro(MaxNumIterations, int);
  vtkSetMacro(MaxNumIterations, int);

  // Description:
  // Print statements used for debugging
  vtkGetMacro(NumSaveIterations, int);
  vtkSetMacro(NumSaveIterations, int);

  // CG Update method
  vtkGetMacro(CGUpdateMethod, int);
  vtkSetMacro(CGUpdateMethod, int);

  // CG Update method
  vtkGetMacro(BoundaryType, int);
  vtkSetMacro(BoundaryType, int);

  // CG Update method
  vtkGetMacro(BoundaryConstraintType, int);
  vtkSetMacro(BoundaryConstraintType, int);

  // North and South Boundaries
  vtkSetVector2Macro(BoundaryStart, int);
  vtkSetVector2Macro(CubeStart, int);
  vtkSetObjectMacro(FirstLoopPts, vtkIntArray);
  vtkSetObjectMacro(SecondLoopPts, vtkIntArray);
  vtkSetObjectMacro(FirstLoopHelper, vtkIntArray);
  vtkSetObjectMacro(SecondLoopHelper, vtkIntArray);

  // Axis of the object to use on orientation with sphee map
  vtkSetVector3Macro(ObjectXAxis, double);
  vtkSetVector3Macro(ObjectZAxis, double);

  // Description:
  // Place to save files if Verbose == 3
  vtkSetStringMacro(IterOutputFilename);

  //MAP
  enum MAP
  {
    TUTTE = 0,
    HARMONIC
  };

  //CG_UPDATE_TYPE
  enum CG_UPDATE_TYPE
  {
    CG_NONE=0, //No update
    CG_FLETCHER_REEVES,
    CG_POLAK_RIBIERE,
    CG_HESTENESS_STIEFEL,
    CG_DAI_YUAN,
  };

  //BOUNDARY_TYPE
  enum BOUNDARY_TYPE
  {
    CLOSED = 0,
    NORTH  = 1,
    SOUTH  = 2,
    FRONT  = 8,
    BACK   = 16,
    LEFT   = 32,
    RIGHT  = 64,
  };

  //Helper functions
  static int ComputeEnergy(vtkPolyData *pd, vtkEdgeTable *edgeTable,
                           vtkFloatArray *edgeWeights, double &energy, int map);
  static int ComputeStringEnergy(double e0[], double e1[], double weight,
                          double stringEnergy[]);
  static int CalculateCircleLength(vtkPolyData *lines, double &length);
  static int CalculateSquareEdgeLengths(vtkPolyData *lines, vtkIntArray *markerPts, double lengths[]);
  static int PDCheckArrayName(vtkPolyData *pd, int datatype, std::string arrayname);
  static int CubeBoundaryToSphere(double inCoords[], double outCoords[]);
  static int RotateByAngle(const double pt[3], const double angle, double returnPt[3]);

protected:
  vtkSVSphericalMapper();
  ~vtkSVSphericalMapper();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  // Main functions in filter
  int PerformMapping();
  int FirstStep(int map);
  int SphericalTutteMapping();
  int SphericalConformalMapper();
  int InitiateCGArrays();
  int SetBoundaries();
  int FindBoundaries();
  int SetLoopOnUnitCircle(vtkPolyData *lines, double length, double radius);
  int SetCubeBoundary(vtkPolyData *lines, vtkIntArray *markerPts, vtkIntArray *markerDirs, double cubeStart[], double lengths[]);
  int SetCircleBoundary(vtkPolyData *lines, vtkIntArray *markerPts, vtkIntArray *markerDirs, double cubeStart[], double lengths[], double radius);
  int DetermineBoundaryPlan(int &numLoops, int bBool[]);
  int GetCubeStartPoint(int id, double startCoords[]);

  // Helper functions
  // Vector functions in vtk!
  int WolfeLineSearch(int map);
  int ComputeMobiusTransformation();
  int ComputeResidual(double &residual);
  int UpdateMap(vtkFloatArray *laplacian, int map, int cg_update);//Sets current descent direction without cg
  int StepForward(int map, int cg_update); // https://en.wikipedia.org/wiki/Nonlinear_conjugate_gradient_method
  int FRUpdateMap(int map); //Fletcher-Reeves
  int PRUpdateMap(int map); //Polak-Ribier
  int HSUpdateMap(int map); //Hesteness-Stiefel
  int DYUpdateMap(int map); //Dai-Yuan
  int CGUpdateMap(int map, double beta[]);

  // Point and edge wise functions using discrete laplace-beltrami

private:
  vtkSVSphericalMapper(const vtkSVSphericalMapper&);  // Not implemented.
  void operator=(const vtkSVSphericalMapper&);  // Not implemented.

  int    Verbose;
  double InitialTimeStep;
  double TimeStep;
  double TutteEnergyCriterion;
  double HarmonicEnergyCriterion;
  int    MaxNumIterations;
  int    NumBoundaries;
  int    CGUpdateMethod;
  double MassCenter[3];
  double ObjectXAxis[3];
  double ObjectZAxis[3];

  vtkPolyData   *InitialPd;
  vtkEdgeTable  *EdgeTable;
  vtkFloatArray *EdgeWeights;
  vtkFloatArray *PrevDescent;
  vtkFloatArray *CurrDescent;
  vtkFloatArray *ConjugateDir;
  vtkIntArray   *EdgeNeighbors;
  vtkIntArray   *IsBoundary;
  vtkPolyData   *HarmonicMap[2];
  vtkPolyData   *Boundaries;

  int         BoundaryType;
  int         BoundaryConstraintType;
  int         BoundaryStart[2];
  int         CubeStart[2];
  vtkIntArray *FirstLoopPts;
  vtkIntArray *SecondLoopPts;
  vtkIntArray *FirstLoopHelper;
  vtkIntArray *SecondLoopHelper;

  char *IterOutputFilename;
  int  NumSaveIterations;
  int  SaveIter;
};

#endif
