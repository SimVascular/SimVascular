/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef __CVMATH_H
#define __CVMATH_H

/**********************************************************
 * Someday I may want to make this method inherent from a *
 * vtkMath object.                                        *
 **********************************************************/
 
class cvMath {

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
  
  private:
  int complex_mult (double z1[], double z2[], double zout[]);
  double complex_mag (double z1[]);
  int complex_div (double z1[], double z2[], double zout[]);
  double factrl(int n);
  int ber_bei (int order, double x, double z[]);
  double compute_velocity (int k, double omega, double time, double radmax, double alpha_k, double Y, double Qk[]);
  
};

#endif

