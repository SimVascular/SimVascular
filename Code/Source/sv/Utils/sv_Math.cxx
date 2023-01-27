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

#include "SimVascular.h"

#include "stdio.h"
#include "stdlib.h"
#include "math.h"
#include <math.h>
#include <array>
#include <cmath>

#include "sv_VTK.h"

#include "sv_Math.h"

cvMath::cvMath() {
}

cvMath::~cvMath() {
}

// dynamically allocate a 2-dimensional array
double **cvMath::createArray(int a, int b) {
    double ** rtn = new double*[a+1];
    if (rtn == nullptr) {
        printf("ERROR: Memory allocation error.\n");
        return nullptr;
    }
    for (int i = 0; i < a+1; i++) {
        rtn[i] = new double[b+1];
        if (rtn[i] == nullptr) {
            printf("ERROR:  Memory allocation error.\n");
            return nullptr;
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
        return SV_ERROR;
    }

    double **outPts = createArray(numOutPts,2);
    if (*outPts == nullptr) {
        return SV_ERROR;
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
          return SV_ERROR;
      }
    }

    // debug
    //for (i = 0; i < numOutPts; i++) {
    //    fprintf(stdout,"%i: %8.3lf %8.3lf\n",i,outPts[i][0],outPts[i][1]);
    //}

    *rtnOutPts = outPts;

    return SV_OK;

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

    if (numInterpPts <= 0 || numDesiredTerms <= 0 || numPts <= 0 || numInterpPts<numDesiredTerms) {
        return SV_ERROR;
    }

    double **terms = createArray(numDesiredTerms,2);

    if (*terms == nullptr) {
        return SV_ERROR;
    }

    // here we calculate dt so that our time series will go from
    // 0 to T - dt.

    double t0 = pts[0][0];
    double dt = (pts[numPts-1][0]-t0)/numInterpPts;
    double **outPts = nullptr;

    if (linearInterpolate(pts, numPts, t0, dt, numInterpPts, &outPts) == SV_ERROR) {
        return SV_ERROR;
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

    return SV_OK;

}

int cvMath::inverseFFT(double **terms, int numTerms, double t0, double dt, double omega,
                         int numRtnPts, double ***rtnPts) {

  int i,j;
  double omega_t;

  double **pts = createArray(numRtnPts,2);
  if (pts == nullptr) {
      return SV_ERROR;
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

  return SV_OK;

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

  return SV_OK;

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

int cvMath::complex_mult(double z1[], double z2[], double zout[])

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

    //avoid multiplication go to infinity, first scale to a smaler number
    double zz1[2];
    double zz2[2];
    zz1[0]=z1[0];
    zz1[1]=z1[1];
    zz2[0]=z2[0];
    zz2[1]=z2[1];

    double a=fabs(zz2[0]);
    double b=fabs(zz2[1]);
    if(a>10000 || b>10000)
    {
        double c=a>b?a:b;
        zz1[0]/=c;
        zz1[1]/=c;
        zz2[0]/=c;
        zz2[1]/=c;
    }

    zmagsqd = pow(complex_mag(zz2),2.0);
    zout[0] = (zz1[0]*zz2[0] + zz1[1]*zz2[1])/zmagsqd;
    zout[1] = (zz1[1]*zz2[0] - zz1[0]*zz2[1])/zmagsqd;

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
        return SV_ERROR;
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
    return SV_OK;

}


int cvMath::linearInterpolateCurve(double **orgPts, int numOrgPts, int closed,
                                     int numOutPts, double ***rtnOutPts) {

    // This method takes an original set of points and returns a
    // newly allocated set of interpolated points (where the requested
    // number of points is numOutPts).  Linear interpolation between the points
    // of the 3D curve is used.

    if (numOrgPts <= 1 || numOutPts <= 2) {
        return SV_ERROR;
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

    if (linearInterpolate(xin, numPts, 0, dt, numOutPts, &xout) == SV_ERROR) {
        deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
        deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2);
        deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);
        return SV_ERROR;
    }
    if (linearInterpolate(yin, numPts, 0, dt, numOutPts, &yout) == SV_ERROR) {
        deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
        deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2);
        deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);
        return SV_ERROR;
    }
    if (linearInterpolate(zin, numPts, 0, dt, numOutPts, &zout) == SV_ERROR) {
        deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
        deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2);
        deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);
        return SV_ERROR;
    }

    // put it all back together
    double **outPts = createArray(numOutPts,3);
    if (*outPts == nullptr) {
        deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
        deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2);
        deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);
        return SV_ERROR;
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

    return SV_OK;

}

int cvMath::fitLeastSquares(int numberOfSamples,double **xt,int xOrder,double **yt,
                    int yOrder,double **mt) {

    // this wrapper around the vtk method to do a least squares fit
    vtkMath *vmathobj = vtkMath::New();
    if (vmathobj->SolveLeastSquares(numberOfSamples,xt,xOrder,yt,yOrder,mt) == 0) {
      return SV_ERROR;
    }
    vmathobj->Delete();
    return SV_OK;

}



int cvMath::smoothCurve(double **orgPts, int numOrgPts, int closed, int keepNumModes,
                                     int numOutPts, double ***rtnOutPts) {

    // This method takes an original set of points and returns a
    // newly allocated set of interpolated points (where the requested
    // number of points is numOutPts).  A FFT is performed on the points
    // and only the requested number of modes are maintained.

    if (numOrgPts <= 1 || numOutPts <= 2) {
        return SV_ERROR;
    }

    if (keepNumModes < 1) {
        return SV_ERROR;
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

    if (FFT(xin, numPts, numInterpPts, keepNumModes, &xmodes) == SV_ERROR) {
        deleteArray(xin,numOrgPts+1,2);
        deleteArray(yin,numOrgPts+1,2);
        deleteArray(zin,numOrgPts+1,2);
        return SV_ERROR;
    }
    deleteArray(xin,numOrgPts+1,2);
    if (FFT(yin, numPts, numInterpPts, keepNumModes, &ymodes) == SV_ERROR) {
        deleteArray(xmodes,keepNumModes,2);
        deleteArray(yin,numOrgPts+1,2);
        deleteArray(zin,numOrgPts+1,2);
        return SV_ERROR;
    }
    deleteArray(yin,numOrgPts+1,2);
    if (FFT(zin, numPts, numInterpPts, keepNumModes, &zmodes) == SV_ERROR) {
        deleteArray(xmodes,keepNumModes,2);
        deleteArray(ymodes,keepNumModes,2);
        deleteArray(zin,numOrgPts+1,2);
        return SV_ERROR;
    }
    deleteArray(zin,numOrgPts+1,2);

    double **xout;
    double **yout;
    double **zout;

    double t0 = 0;
    double Pi = 3.1415926535;
    double omega = 2.0*Pi/length;

    if (inverseFFT(xmodes, keepNumModes, t0, dt, omega,
                   numOutPts, &xout) == SV_ERROR) {
        deleteArray(xmodes,keepNumModes,2);
        deleteArray(ymodes,keepNumModes,2);
        deleteArray(zmodes,keepNumModes,2);
        return SV_ERROR;
    }
    if (inverseFFT(ymodes, keepNumModes, t0, dt, omega,
                   numOutPts, &yout) == SV_ERROR) {
        deleteArray(xmodes,keepNumModes,2);deleteArray(xout,numOutPts,2);
        deleteArray(ymodes,keepNumModes,2);
        deleteArray(zmodes,keepNumModes,2);
        return SV_ERROR;
    }
    if (inverseFFT(zmodes, keepNumModes, t0, dt, omega,
                   numOutPts, &zout) == SV_ERROR) {
        deleteArray(xmodes,keepNumModes,2);deleteArray(xout,numOutPts,2);
        deleteArray(ymodes,keepNumModes,2);deleteArray(yout,numOutPts,2);
        deleteArray(zmodes,keepNumModes,2);
        return SV_ERROR;
    }

    // put it all back together
    double **outPts = createArray(numOutPts,3);
    if (*outPts == nullptr) {
        deleteArray(xin,numOrgPts+1,2); deleteArray(xout,numOutPts,2);
        deleteArray(yin,numOrgPts+1,2); deleteArray(yout,numOutPts,2);
        deleteArray(zin,numOrgPts+1,2); deleteArray(zout,numOutPts,2);
        return SV_ERROR;
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

    return SV_OK;

}

// ------------------------------
// GetInsertintIndexByDistance
// ------------------------------
int cvMath::GetInsertintIndexByDistance( std::vector<std::array<double,3> > points, std::array<double,3> point, bool insertOnlyIfDifferent, bool useDistanceSum)
{
    if(useDistanceSum)
        return GetInsertintIndexByDistanceSum(points, point,insertOnlyIfDifferent);
    else
        return GetInsertintIndexByProjectedDistance(points, point,insertOnlyIfDifferent);
}

int cvMath::GetInsertintIndexByDistanceSum( std::vector<std::array<double,3> > points, std::array<double,3> point, bool insertOnlyIfDifferent)
{
    int idx=-2;

    if(points.size()<2){
        idx=points.size();
    }else{
        bool firstTime=true;
        int markingIndex;
        int insertingIndex=-2;
        double minDist;

        for(int i=0;i<points.size()-1;i++)
        {
            std::array<double,3> startPoint=points[i];
            std::array<double,3> endPoint=points[i+1];

            double dist1 = 0.;
            double dist2 = 0.;
            for (int i=0;i<3;i++)
            {
                dist1+=pow(startPoint[i]-point[i],2);
                dist2+=pow(endPoint[i]-point[i],2);
            }
            dist1 = sqrt(dist1);
            dist2 = sqrt(dist2);

            if(dist1==0)//check if a same point already exist
            {
                if(insertOnlyIfDifferent)
                    return -2;
                else
                    return i;
            }

            if(dist2==0)//check if a same point already exist
            {
                if(insertOnlyIfDifferent)
                    return -2;
                else
                    return i+1;
            }

            double distSum=dist1+dist2;
            if(firstTime){
                markingIndex=i;
                minDist=distSum;
                firstTime=false;
            }
            else if(distSum<minDist)
            {
                markingIndex=i;
                minDist=distSum;
            }

        }

        std::array<double,3> startPoint=points[markingIndex];
        std::array<double,3> endPoint=points[markingIndex+1];

        std::array<double,3> n1;
        for (int i = 0;i<3;i++)
            n1[i] = endPoint[i] - startPoint[i];
            
        //normalize and dot
        double l1=0.;
        double l2=0.;
        double lth = sqrt(pow(n1[0],2)+pow(n1[1],2)+pow(n1[2],2));
        for (int i = 0;i<3;i++)
        {
            n1[i] /= lth;
            l1 += n1[i]*(point[i]-startPoint[i]);
            l2 += -1*n1[i]*(point[i]-endPoint[i]);
        }

        if(l1<=0.0)
        {
            insertingIndex=markingIndex;
        }else if(l2<=0.0){
            insertingIndex=markingIndex+2;
        }else{
            insertingIndex=markingIndex+1;
        }

        if(insertingIndex!=-2)
        {
            idx=insertingIndex;
        }
    }

    return idx;
}

int cvMath::GetInsertintIndexByProjectedDistance( std::vector<std::array<double,3> > points, std::array<double,3> point, bool insertOnlyIfDifferent)
{
    int idx=-2;

    if(points.size()<2){
        idx=points.size();
    }else{
        bool firstTime=true;
        int insertingIndex=-2;
        double minDist;

        for(int i=0;i<points.size()-1;i++)
        {
            std::array<double,3> startPoint=points[i];
            std::array<double,3> endPoint=points[i+1];

            std::array<double,3> n1;
            
            for (int i = 0;i<3;i++)
                n1[i] = endPoint[i] - startPoint[i];
            
            //normalize and dot
            double l1=0.;
            double l2=0.;
            double lth =sqrt(pow(n1[0],2)+pow(n1[1],2)+pow(n1[2],2));
            for (int i = 0;i<3;i++)
            {
                n1[i] /= lth;
                l1 += n1[i]*(point[i]-startPoint[i]);
                l2 += -1*n1[i]*(point[i]-endPoint[i]);
            }

            std::array<double,3> crossPoint;
            for (int i = 0;i<3;i++)
                crossPoint[i] = startPoint[i] - n1[i]*l1;
            
            double dist1 = 0., dist2 = 0., dist3 = 0.;
            for (int i=0;i<3;i++)
            {
                dist1+=pow(startPoint[i]-point[i],2);
                dist2+=pow(endPoint[i]-point[i],2);
                dist3+=pow(crossPoint[i]-point[i],2);
            }
            dist1 = sqrt(dist1);
            dist2 = sqrt(dist2);
            dist3 = sqrt(dist3);


            if(dist1==0)//check if a same point already exist
            {
                if(insertOnlyIfDifferent)
                    return -2;
                else
                    return i;
            }

            if(dist2==0)//check if a same point already exist
            {
                if(insertOnlyIfDifferent)
                    return -2;
                else
                    return i+1;
            }

            if(l1>=0.0&&l2>=0.0)
            {
                if(firstTime || dist3<minDist)
                {
                    insertingIndex=i+1;
                    minDist=dist3;
                }
            }
            else if(l1<0)
            {
                if(firstTime || dist1<minDist)
                {
                    insertingIndex=i;
                    minDist=dist1;
                }
            }
            else
            {
                if(firstTime || dist2<minDist)
                {
                    insertingIndex=i+2;
                    minDist=dist2;
                }
            }

            firstTime=false;
        }

        idx=insertingIndex;
    }

    return idx;
}

std::array<double,3> cvMath::GetPerpendicularNormalVector(std::array<double,3> vec)
{
    std::array<double,3> pvec;

    pvec.fill(0.);

    if(vec[0]==0&&vec[1]==0&&vec[2]==0)
    {
        //        pvec[2]=1;
        return pvec;
    }

    int replaceIdx;

    double dotProduct=0;

    if(std::abs(vec[2])>0.0001)
    {
        pvec[1]=1;
        replaceIdx=2;
        dotProduct=vec[0]*pvec[0]+vec[1]*pvec[1];
    }
    else if(std::abs(vec[1])>0.0001)
    {
        pvec[0]=1;
        replaceIdx=1;
        dotProduct=vec[0]*pvec[0]+vec[2]*pvec[2];
    }
    else
    {
        pvec[2]=1;
        replaceIdx=0;
        dotProduct=vec[1]*pvec[1]+vec[2]*pvec[2];
    }

    pvec[replaceIdx]=-dotProduct/vec[replaceIdx];

    double lth  = sqrt(pow(pvec[0],2)+pow(pvec[1],2)+pow(pvec[2],2));
    for (int i = 0;i<3;i++)
        pvec[i] /= lth;

    return pvec;
}

std::vector<std::array<double, 3> > cvMath::CreateSmoothedCurve(std::vector<std::array<double, 3> > points, bool closed, int numModes, int sampleRate, int outputNumPts)
{
    std::vector<std::array<double, 3>  > outputPoints;
    int numPts=points.size();

    std::vector<std::array<double, 3>  > actualPoints;

    if(sampleRate>0)
    {
        for(int i=0;i<numPts;i+=sampleRate){
            actualPoints.push_back(points[i]);
        }
        if(sampleRate>1){
            actualPoints.push_back(points[numPts-1]);
        }
        outputNumPts=actualPoints.size();
    }
    else if(outputNumPts>1)
    {
        actualPoints=points;
    }
    else
    {
        return outputPoints;
    }

    if(!closed)
    {
        for(int i=outputNumPts-1;i>=0;i--){
            actualPoints.push_back(actualPoints[i]);
        }
    }

    int actualNumPts=actualPoints.size();

    cvMath *cMath = new cvMath();

    double **pts = cMath->createArray(actualNumPts,3);
    for(int i=0;i<actualNumPts;i++)
    {
        pts[i][0] = actualPoints[i][0];
        pts[i][1] = actualPoints[i][1];
        pts[i][2] = actualPoints[i][2];
    }
    double **outPts = nullptr;
    int isClosed=closed?1:0;
    int rslt;
    if(closed)
    {
        rslt=cMath->smoothCurve(pts, actualNumPts, 1, numModes, outputNumPts, &outPts);
    }
    else
    {
        rslt=cMath->smoothCurve(pts, actualNumPts, 0, numModes, 2*outputNumPts, &outPts);
    }
    delete cMath;
    if (rslt == SV_ERROR) {
        return outputPoints;
    }

    for(int i=0;i<outputNumPts;i++){
        std::array<double, 3>   point;
        point[0]=outPts[i][0];
        point[1]=outPts[i][1];
        point[2]=outPts[i][2];

        outputPoints.push_back(point);
    }

    return outputPoints;
}
