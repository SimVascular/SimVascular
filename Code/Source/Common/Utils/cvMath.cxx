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

#include "SimVascular.h"

#include "stdio.h"
#include "stdlib.h"
#include "math.h"
#include <math.h>

#include "cvMath.h"

#include "cvVTK.h"

cvMath::cvMath() {
}

cvMath::~cvMath() {
}

// dynamically allocate a 2-dimensional array
double **cvMath::createArray(int a, int b) {
    double ** rtn = new double*[a+1];
    if (rtn == NULL) {
        printf("ERROR: Memory allocation error.\n");
        return NULL;
    }
    for (int i = 0; i < a+1; i++) {
        rtn[i] = new double[b+1];
        if (rtn[i] == NULL) {
            printf("ERROR:  Memory allocation error.\n");
            return NULL;
        }
        for (int j = 0; j < b + 1; j++) {
            rtn[i][j] = 0.0;
        }
    }
    return rtn;
}

// dynamically deallocate a 2-dimensional array
void cvMath::deleteArray(double **ptr, int a, int b) {
    for (int i = 0; i < a+1; i++) {
        delete ptr[i];
    }
    delete ptr;
}

int cvMath::linearInterpolate(double **orgPts, int numOrgPts, double t0, 
                            double dt, int numOutPts, double ***rtnOutPts) {

    // This method takes an original set of points and returns a
    // newly allocated set of interpolated points (where the requested
    // number of points is numOutPts).  Linear interpolation is used, and
    // the values outside of the range of orgPts are fixed to the values
    // at t_0 and t_n.

    int i,j;

    if (numOrgPts <= 0 || numOutPts <= 0) {
        return CV_ERROR;
    }

    double **outPts = createArray(numOutPts,2);
    if (*outPts == NULL) {
        return CV_ERROR;
    }

    double t;

    for (i=0; i < numOutPts; i++) {

        t = t0 + dt*i;

        outPts[i][0] = t;

        // if time is outside of data range, fix to values at beginning
        // and end of interval
        if (t <= orgPts[0][0]) {
          outPts[i][1] = orgPts[0][1];
          continue;
        } else if (t >= orgPts[numOrgPts-1][0]) {
          outPts[i][1] = orgPts[numOrgPts-1][1];
          continue;
         }

        // interpolate
        for (j = 1; j < numOrgPts; j++) {
          if (t < orgPts[j][0]) {
              double m = (orgPts[j][1]-orgPts[j-1][1])/(orgPts[j][0]-orgPts[j-1][0]);
              outPts[i][1] = m*(t - orgPts[j-1][0]) + orgPts[j-1][1];
              break;
          }   
      }

      if (j == numOrgPts) {
          fprintf(stdout,"Error interpolating point %i.\n",i);
          deleteArray(outPts,numOutPts,2);
          return CV_ERROR;
      }
    }

    // debug
    //for (i = 0; i < numOutPts; i++) {
    //    fprintf(stdout,"%i: %8.3lf %8.3lf\n",i,outPts[i][0],outPts[i][1]);
    //}

    *rtnOutPts = outPts;

    return CV_OK;

}

#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

void cvMath::FFT(double data[],int nn,int isign) {

	int n,mmax,m,j,istep,i;
	double wtemp,wr,wpr,wpi,wi,theta;
	double tempr,tempi;

	n=nn << 1;
	j=1;
	for (i=1;i<n;i+=2) {
		if (j > i) {
			SWAP(data[j-1],data[i-1]);
			SWAP(data[j+1-1],data[i+1-1]);
		}
		m=n >> 1;
		while (m >= 2 && j > m) {
			j -= m;
			m >>= 1;
		}
		j += m;
	}
	mmax=2;
	while (n > mmax) {
		istep=2*mmax;
		theta=6.28318530717959/(isign*mmax);
		wtemp=sin(0.5*theta);
		wpr = -2.0*wtemp*wtemp;
		wpi=sin(theta);
		wr=1.0;
		wi=0.0;
		for (m=1;m<mmax;m+=2) {
			for (i=m;i<=n;i+=istep) {
				j=i+mmax;
				tempr=wr*data[j-1]-wi*data[j+1-1];
				tempi=wr*data[j+1-1]+wi*data[j-1];
				data[j-1]=data[i-1]-tempr;
				data[j+1-1]=data[i+1-1]-tempi;
				data[i-1] += tempr;
				data[i+1-1] += tempi;
			}
			wr=(wtemp=wr)*wpr-wi*wpi+wr;
			wi=wi*wpr+wtemp*wpi+wi;
		}
		mmax=istep;
	}
}

#undef SWAP

int cvMath::FFT(double **pts, int numPts, int numInterpPts, int numDesiredTerms, double ***rtnterms) {

    int i;

    if (numInterpPts <= 0 || numDesiredTerms <= 0 || numPts <= 0) {
        return CV_ERROR;
    }

    double **terms = createArray(numDesiredTerms,2);

    if (*terms == NULL) {
        return CV_ERROR;
    }
   
    // here we calculate dt so that our time series will go from
    // 0 to T - dt.

    double t0 = pts[0][0];
    double dt = (pts[numPts-1][0]-t0)/numInterpPts;
    double **outPts = NULL;

    if (linearInterpolate(pts, numPts, t0, dt, numInterpPts, &outPts) == CV_ERROR) {
        return CV_ERROR;
    }

    // create a real-imaginary array to do fft
    double *data = new double [2*numInterpPts];
    for (i = 0; i < numInterpPts; i++) {
        data[2*i] = outPts[i][1];
        data[2*i+1] = 0.0;
    }
    deleteArray(outPts,numInterpPts,2);

    FFT(data,numInterpPts,1);

    terms[0][0] = data[0]/numInterpPts;
    terms[0][1] = data[1]/numInterpPts;

    for (i=1;i<numDesiredTerms;i++) {
      terms[i][0]=2.0*data[2*i]/numInterpPts;
      terms[i][1]=2.0*data[2*i+1]/numInterpPts;
    }

    delete data;

    *rtnterms = terms;

    return CV_OK;

}

int cvMath::inverseFFT(double **terms, int numTerms, double t0, double dt, double omega, 
                         int numRtnPts, double ***rtnPts) {

  int i,j;
  double omega_t;

  double **pts = createArray(numRtnPts,2);
  if (pts == NULL) {
      return CV_ERROR;
  }

  for (i=0;i<numRtnPts;i++) {
    pts[i][0] = t0+i*dt;
    omega_t = omega*i*dt;
    pts[i][1] = terms[0][0];
    for (j=1;j<numTerms;j++) {
      pts[i][1] += terms[j][0]*cos(j*omega_t) + terms[j][1]*sin(j*omega_t);
    }
  }

  *rtnPts = pts;

  return CV_OK;

}


int cvMath::compute_v_womersley(double **terms, int numTerms, double viscosity, double density,
                             double omega, double radmax, double rad, double time, double *vel)
{
  int k;    
  double Pi = 3.1415926535;
  double Y;
  double v = 0;

  Y = rad/radmax;
  v = 2.0*terms[0][0]*(1.0 - pow(Y,2.0))/(Pi*pow(radmax,2.0));

  for (k=1;k<numTerms;k++) {
      // The womersley code from Charley requires we
      // specify the complex conjugant of the the fourier
      // terms obtained from the FFT code (from Numerical Recipes)
      // negate sign here 
      terms[k][1]=-terms[k][1];
    double alpha_k = radmax*pow(omega*k*density/viscosity,0.5);
    v += compute_velocity(k,omega,time,radmax,alpha_k,Y,terms[k]);
  }

  *vel = v;

  return CV_OK;

}

double cvMath::compute_velocity (int k, double omega, double time, double radmax, 
                                   double alpha_k, double Y, double Qk[]) {

  double z1[2],z2[2],z3[2],z4[2],z5[2],z6[2],z7[2],z8[2],z9[2],z10[2],v_kwt;
  double Pi = 3.1415926535;

  
  ber_bei(0, Y*alpha_k, z1);
  ber_bei(0, alpha_k, z2);
  complex_div(z1,z2,z3);
  z3[0] = 1 - z3[0];
  z3[1] = -z3[1];
  ber_bei(1, alpha_k, z4);
  ber_bei(0, alpha_k, z5);
  z6[0] = -(alpha_k*sqrt(2.0))/2.0;
  z6[1] = (alpha_k*sqrt(2.0))/2.0;
  complex_mult(z5,z6,z7);
  complex_div(z4,z7,z8);
  z8[0] = 1 - 2.0*z8[0];
  z8[1] = -2.0*z8[1];
  complex_div(z3,z8,z9);
  complex_mult(Qk,z9,z10);
  // note Charley's original code contains an error here, and
  // the sign should be negative instead of positive as it
  // was in his code
  v_kwt = (z10[0]*cos(k*omega*time) - z10[1]*sin(k*omega*time))/(Pi*radmax*radmax);
 
 return v_kwt;

}


int cvMath::ber_bei (int order, double x, double z[]) {

  int k,kmax=30;
  double toler=1.0e-4,z_old[2];
  double Pi = 3.1415926535898;
  double x_term, arg,ca,sa,mu,mod,phase,sqrt2;

  z[0] = 0.0;
  z[1] = 0.0;

 if (fabs(x) < 15.0){
  for (k=0;k<=kmax;k++) {
   z_old[0] = z[0];
   z_old[1] = z[1];
   if (fabs(x) < toler && k==0) {
    x_term = 1.0;
   } else {
    x_term = pow((x*x/4.0),k);
   }
   arg = (3.0*order/4.0 + k/2.0)*Pi;
   ca = cos(arg);
   sa = sin(arg);
   z[0] = z[0] + (ca/factrl(k))*(x_term/factrl(order+k));
   z[1] = z[1] + (sa/factrl(k))*(x_term/factrl(order+k));
  }

   if (fabs((z_old[0]-z[0])/z_old[0])>toler || fabs((z_old[1]-z[1])/z_old[1])>toler) {

    printf("  ***************************** \n");
    printf("    Error in ber_bei function \n");
    printf("     value did not converge !!! \n");
    printf("  ***************************** \n");

   }
   if (fabs(x) < toler && order==0) {
 /* do nothing */
   } else {
    z[0] = z[0]*pow((x/2.0),order);
    z[1] = z[1]*pow((x/2.0),order);
   }
 } else {

 sqrt2 = sqrt(2.0);
 mu = 4*order*order;
 mod = 1.0 - (mu-1.0)/(8.0*sqrt2*x) + (mu-1)*(mu-1)/(256.0*x*x)
             -(mu-1.0)*(mu*mu + 14.0*mu - 399.0)/(6144.0*sqrt2*x*x*x);
 mod = mod*(exp(x/sqrt2)/(pow(2.0*Pi*x,0.5)));
 phase = x/sqrt2 + (order/2.0 - 0.125)*Pi + (mu-1.0)/(8.0*sqrt2*x) + (mu-1)/(16.0*x*x)
             -(mu-1.0)*(mu-25.0)/(384.0*sqrt2*x*x*x);
 z[0] = mod*cos(phase);
 z[1] = mod*sin(phase);
 }




 return 0;
}


double cvMath::factrl(int n)
{
	static int ntop=4;
	static double a[33]={1.0,1.0,2.0,6.0,24.0};
	int j;

	if (n < 0) printf("Negative factorial in routine FACTRL");
	if (n > 32) printf("input too large for factorial computation in routine FACTRL");
	while (ntop<n) {
		j=ntop++;
		a[ntop]=a[j]*ntop;
	}
	return a[n];
}

/* These functions performs operations on complex numbers_*/

int cvMath::complex_mult (double z1[], double z2[], double zout[])

{
 zout[0] = z1[0]*z2[0] - z1[1]*z2[1];
 zout[1] = z1[1]*z2[0] + z1[0]*z2[1];

 return 0;
}


double cvMath::complex_mag (double z1[])

{
 double zmagsqd,zmag;
 zmagsqd = z1[0]*z1[0] + z1[1]*z1[1];
 zmag = sqrt(zmagsqd);

 return zmag;
}


int cvMath::complex_div (double z1[], double z2[], double zout[])

{
 double zmagsqd;

 zmagsqd = pow(complex_mag(z2),2.0);
 zout[0] = (z1[0]*z2[0] + z1[1]*z2[1])/zmagsqd;
 zout[1] = (z1[1]*z2[0] - z1[0]*z2[1])/zmagsqd;

 return 0;
}
       

int cvMath::curveLength(double **pts, int numPts, int closed, double *length) {

    // This method takes an original set of points and returns the length
    // of the line 2-D line.  

    // If you specify closed == 1, the curve is assumed to be closed
    // and the distance between the last point and the first is included
    // in the value returned.

    if (numPts <= 1) {
        *length = 0;
        return CV_ERROR;
    }

    int numSegments;
    if (closed == 0) {
        numSegments=numPts-1;
    } else {
        numSegments=numPts;
    }

    double result = 0;
    for (int i=0; i < numSegments;i++) {
        int j=i+1;
        if (j == numPts) {
            j = 0;
        }
        result += sqrt( (pts[j][0]-pts[i][0])*(pts[j][0]-pts[i][0]) + 
                        (pts[j][1]-pts[i][1])*(pts[j][1]-pts[i][1]) +
                        (pts[j][2]-pts[i][2])*(pts[j][2]-pts[i][2]) );
    }

    *length = result;
    return CV_OK;

}


int cvMath::linearInterpolateCurve(double **orgPts, int numOrgPts, int closed, 
                                     int numOutPts, double ***rtnOutPts) {

    // This method takes an original set of points and returns a
    // newly allocated set of interpolated points (where the requested
    // number of points is numOutPts).  Linear interpolation between the points
    // of the 3D curve is used.

    if (numOrgPts <= 1 || numOutPts <= 2) {
        return CV_ERROR;
    }

    // find the length of the curve
    double length = 0;
    curveLength(orgPts,numOrgPts,closed,&length);
 
    // now do linear interpolation of each coordinate
    double **xin = createArray(numOrgPts+1,2);
    double **yin = createArray(numOrgPts+1,2);
    double **zin = createArray(numOrgPts+1,2);   
 
    int i;
    double t = 0;
    for (i=0;i < numOrgPts;i++) {
        xin[i][0]=t;xin[i][1]=orgPts[i][0];
        yin[i][0]=t;yin[i][1]=orgPts[i][1];
        zin[i][0]=t;zin[i][1]=orgPts[i][2];
        int j = i+1;
        if (j == numOrgPts) {
            j = 0;
        }
        t += sqrt( (orgPts[j][0]-orgPts[i][0])*(orgPts[j][0]-orgPts[i][0]) +
                   (orgPts[j][1]-orgPts[i][1])*(orgPts[j][1]-orgPts[i][1]) +
                   (orgPts[j][2]-orgPts[i][2])*(orgPts[j][2]-orgPts[i][2]) ); 
    }

    int numPts = numOrgPts;
    double dt = length / (numOutPts-1);
    if (closed != 0) {
        xin[numOrgPts][0]=length;xin[numOrgPts][1]=orgPts[0][0];
        yin[numOrgPts][0]=length;yin[numOrgPts][1]=orgPts[0][1];
        zin[numOrgPts][0]=length;zin[numOrgPts][1]=orgPts[0][2];
        numPts++;
        dt = length / numOutPts;
    }

    // now do linear interpolation on each coordinate
    double **xout = createArray(numOutPts,2);
    double **yout = createArray(numOutPts,2);
    double **zout = createArray(numOutPts,2);

    if (linearInterpolate(xin, numPts, 0, dt, numOutPts, &xout) == CV_ERROR) {
        deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
        deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2);
        deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);
        return CV_ERROR;
    }
    if (linearInterpolate(yin, numPts, 0, dt, numOutPts, &yout) == CV_ERROR) {
        deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
        deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2);
        deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);
        return CV_ERROR;
    }
    if (linearInterpolate(zin, numPts, 0, dt, numOutPts, &zout) == CV_ERROR) {
        deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
        deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2);
        deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);
        return CV_ERROR;
    }

    // put it all back together
    double **outPts = createArray(numOutPts,3);
    if (*outPts == NULL) {
        deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
        deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2); 
        deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);
        return CV_ERROR;
    }
    for (i = 0; i < numOutPts;i++) {
        outPts[i][0]=xout[i][1];
        outPts[i][1]=yout[i][1];
        outPts[i][2]=zout[i][1];
    }

    // debug
    //for (i = 0; i < numOutPts; i++) {
    //    fprintf(stdout,"%i: %8.3lf %8.3lf %8.3lf\n",i,outPts[i][0],outPts[i][1],outPts[i][2]);
    //}

    // clean up
    deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
    deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2); 
    deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);

    *rtnOutPts = outPts;

    return CV_OK;

}

int cvMath::fitLeastSquares(int numberOfSamples,double **xt,int xOrder,double **yt,
                    int yOrder,double **mt) {

    // this wrapper around the vtk method to do a least squares fit
    vtkMath *vmathobj = vtkMath::New();
    if (vmathobj->SolveLeastSquares(numberOfSamples,xt,xOrder,yt,yOrder,mt) == 0) {
      return CV_ERROR;
    }
    vmathobj->Delete(); 
    return CV_OK;
 
}  



int cvMath::smoothCurve(double **orgPts, int numOrgPts, int closed, int keepNumModes, 
                                     int numOutPts, double ***rtnOutPts) {

    // This method takes an original set of points and returns a
    // newly allocated set of interpolated points (where the requested
    // number of points is numOutPts).  A FFT is performed on the points
    // and only the requested number of modes are maintained.

    if (numOrgPts <= 1 || numOutPts <= 2) {
        return CV_ERROR;
    }

    if (keepNumModes < 1) {
        return CV_ERROR;
    }

    // find the length of the curve
    double length = 0;
    curveLength(orgPts,numOrgPts,closed,&length);
 
    // now do linear interpolation of each coordinate
    double **xin = createArray(numOrgPts+1,2);
    double **yin = createArray(numOrgPts+1,2);
    double **zin = createArray(numOrgPts+1,2);   
 
    int i;
    double t = 0;
    for (i=0;i < numOrgPts;i++) {
        xin[i][0]=t;xin[i][1]=orgPts[i][0];
        yin[i][0]=t;yin[i][1]=orgPts[i][1];
        zin[i][0]=t;zin[i][1]=orgPts[i][2];
        int j = i+1;
        if (j == numOrgPts) {
            j = 0;
        }
        t += sqrt( (orgPts[j][0]-orgPts[i][0])*(orgPts[j][0]-orgPts[i][0]) +
                   (orgPts[j][1]-orgPts[i][1])*(orgPts[j][1]-orgPts[i][1]) +
                   (orgPts[j][2]-orgPts[i][2])*(orgPts[j][2]-orgPts[i][2]) ); 
    }

    int numPts = numOrgPts;
    double dt = length / (numOutPts-1);
    if (closed != 0) {
        xin[numOrgPts][0]=length;xin[numOrgPts][1]=orgPts[0][0];
        yin[numOrgPts][0]=length;yin[numOrgPts][1]=orgPts[0][1];
        zin[numOrgPts][0]=length;zin[numOrgPts][1]=orgPts[0][2];
        numPts++;
        dt = length / numOutPts;
    }

    // now do a FFT on each coordinate
    double **xmodes;
    double **ymodes;
    double **zmodes;
  
    // need to unhardcore this
    int numInterpPts = 2048;

    if (FFT(xin, numPts, numInterpPts, keepNumModes, &xmodes) == CV_ERROR) {
        deleteArray(xin,numOrgPts+1,2);
        deleteArray(yin,numOrgPts+1,2);
        deleteArray(zin,numOrgPts+1,2);
        return CV_ERROR;
    }
    deleteArray(xin,numOrgPts+1,2);
    if (FFT(yin, numPts, numInterpPts, keepNumModes, &ymodes) == CV_ERROR) {
        deleteArray(xmodes,keepNumModes,2);
        deleteArray(yin,numOrgPts+1,2);
        deleteArray(zin,numOrgPts+1,2);
        return CV_ERROR;
    }
    deleteArray(yin,numOrgPts+1,2);
    if (FFT(zin, numPts, numInterpPts, keepNumModes, &zmodes) == CV_ERROR) {
        deleteArray(xmodes,keepNumModes,2);
        deleteArray(ymodes,keepNumModes,2);
        deleteArray(zin,numOrgPts+1,2);
        return CV_ERROR;
    }
    deleteArray(zin,numOrgPts+1,2);

    double **xout;
    double **yout;
    double **zout;

    double t0 = 0;
    double Pi = 3.1415926535;
    double omega = 2.0*Pi/length;

    if (inverseFFT(xmodes, keepNumModes, t0, dt, omega, 
                   numOutPts, &xout) == CV_ERROR) {
        deleteArray(xmodes,keepNumModes,2);
        deleteArray(ymodes,keepNumModes,2);
        deleteArray(zmodes,keepNumModes,2);
        return CV_ERROR;
    }
    if (inverseFFT(ymodes, keepNumModes, t0, dt, omega, 
                   numOutPts, &yout) == CV_ERROR) {
        deleteArray(xmodes,keepNumModes,2);deleteArray(xout,numOutPts,2);
        deleteArray(ymodes,keepNumModes,2);
        deleteArray(zmodes,keepNumModes,2);
        return CV_ERROR;
    }
    if (inverseFFT(zmodes, keepNumModes, t0, dt, omega, 
                   numOutPts, &zout) == CV_ERROR) {
        deleteArray(xmodes,keepNumModes,2);deleteArray(xout,numOutPts,2);
        deleteArray(ymodes,keepNumModes,2);deleteArray(yout,numOutPts,2);
        deleteArray(zmodes,keepNumModes,2);
        return CV_ERROR;
    }

    // put it all back together
    double **outPts = createArray(numOutPts,3);
    if (*outPts == NULL) {
        deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
        deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2); 
        deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);
        return CV_ERROR;
    }
    for (i = 0; i < numOutPts;i++) {
        outPts[i][0]=xout[i][1];
        outPts[i][1]=yout[i][1];
        outPts[i][2]=zout[i][1];
    }

    // debug
    //for (i = 0; i < numOutPts; i++) {
    //    fprintf(stdout,"%i: %8.3lf %8.3lf %8.3lf\n",i,outPts[i][0],outPts[i][1],outPts[i][2]);
    //}

    // clean up
    deleteArray(xout,numOutPts,2);
    deleteArray(yout,numOutPts,2); 
    deleteArray(zout,numOutPts,2);

    *rtnOutPts = outPts;

    return CV_OK;

}
