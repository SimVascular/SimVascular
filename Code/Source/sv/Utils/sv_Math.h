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

#ifndef __CVMATH_H
#define __CVMATH_H

/**********************************************************
 * Someday I may want to make this method inherent from a *
 * vtkMath object.                                        *
 **********************************************************/

#include "SimVascular.h"
#include "svUtilsExports.h" // For exports
#include <array>
#include <vector>

class SV_EXPORT_UTILS cvMath {

public:
    cvMath();
    ~cvMath();

  double **createArray(int a, int b);
  void deleteArray(double **ptr, int a, int b);

  int linearInterpolate(double **orgPts, int numOrgPts, double t0,
                              double dt, int numOutPts, double ***outPts);
  int linearInterpolateCurve(double **orgPts, int numOrgPts, int closed,
                             int numOutPts, double ***rtnOutPts);

  void FFT(double Qdata[],int nn,int isign);
  int FFT(double **pts, int numPts, int numInterpPts, int numDesiredTerms, double ***terms);
  int inverseFFT(double **terms, int numTerms, double t0, double dt, double omega,
                 int numRtnPts, double ***rtnPts);

  int compute_v_womersley(double **terms, int numTerms, double viscosity, double density,
                     double omega, double radmax, double rad, double time, double *v);

  int curveLength(double **pts, int numPts, int closed, double *length);


  int smoothCurve(double **orgPts, int numOrgPts, int closed, int keepNumModes,
                          int numOutPts, double ***rtnOutPts);

  int fitLeastSquares(int numberOfSamples,double **xt,int xOrder,double **yt,
                      int yOrder,double **mt);
  int GetInsertintIndexByDistance( std::vector<std::array<double,3> > points, std::array<double,3> point, 
                            bool insertOnlyIfDifferent = true, bool useDistanceSum = true);
  
  int GetInsertintIndexByDistanceSum( std::vector<std::array<double,3> > points, std::array<double,3> point, 
                            bool insertOnlyIfDifferent = true);

  int GetInsertintIndexByProjectedDistance( std::vector<std::array<double,3> > points, std::array<double,3> point, 
                            bool insertOnlyIfDifferent = true);
                            
  std::array<double,3> GetPerpendicularNormalVector(std::array<double,3> vec);
  
  std::vector<std::array<double, 3> > CreateSmoothedCurve(std::vector<std::array<double, 3> > points, bool closed, int numModes, int sampleRate, int outputNumPts);

  private:
  int complex_mult (double z1[], double z2[], double zout[]);
  double complex_mag (double z1[]);
  int complex_div (double z1[], double z2[], double zout[]);
  double factrl(int n);
  int ber_bei (int order, double x, double z[]);
  double compute_velocity (int k, double omega, double time, double radmax, double alpha_k, double Y, double Qk[]);

};

#endif

