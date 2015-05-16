/* Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
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

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "cv_image.h"
#include "cv_misc_utils.h"


static double gMachineEpsilon;


// =============
//   ReadImage
// =============
// This function reads a stack of image files (flat, headerless
// binaries) and puts the intensity data into an array.  Upon success,
// a pointer to a newly allocated Image_T is returned.  Deallocation
// is the responsibility of the caller.  If the function fails, a NULL
// pointer is returned.  Note that it seems that both raw MR data in
// the Signa format and slice planes from David Paik's path planner
// store images as arrays of short's (size = 2), so that's why
// Image_T::data is a short *.

Image_T *ReadImage( char *filebase, int fileNumRange[], char *imgTypeFlag,
		    int imgDims[], double pixelDims[] )
{
  FILE *fp;
  Image_T *image;
  char filename[1000];
  int filecount, filenum;
  int i, dataLen, fileLen, num;
  short *tmpDataShort = NULL;
  double *tmpDataDouble = NULL;
  int rowIx, colIx, planeIx, planeOffset;
  int convertShort;

  // Uh, yeah, I know it's ugly to put this here, but what else are we
  // going to do?  Also, seems the real source of errors (in the
  // assertions on rx,ry,rz in LinearInterpolate) is from the
  // coordinate values exhibiting some numerical "noise" at some small
  // decimal places.  This noise in turn is leading to mis-calculated
  // quotients.  By clamping to a precision of 100 times the machine's
  // epsilon, I'm hoping to eliminate this noise.
  gMachineEpsilon = 100.0 * FindMachineEpsilon();

  if ( !strcmp( imgTypeFlag, "-short" ) ) {
    convertShort = 1;
  } else if ( !strcmp( imgTypeFlag, "-double" ) ) {
    convertShort = 0;
  } else {
    fprintf(stderr, "ERR: Invalid image data type flag.\n");
    return NULL;
  }

  if ( fileNumRange[1] < fileNumRange[0] ) {
    fprintf(stderr, "ERR: Invalid file num range.\n");
    return NULL;
  }

  image = new Image_T;
  image->gradValid = 0;
  if ( fileNumRange[0] == fileNumRange[1] ) {
    image->dim = 2;
  } else {
    image->dim = 3;
  }

  image->imgDims[0] = imgDims[0];
  image->imgDims[1] = imgDims[1];
  if ( image->dim == 3 ) {
    image->imgDims[2] = imgDims[2];
  } else {
    image->imgDims[2] = 1;
  }

  image->pixelDims[0] = pixelDims[0];
  image->pixelDims[1] = pixelDims[1];
  if ( image->dim == 3 ) {
    image->pixelDims[2] = pixelDims[2];
  } else {
    image->pixelDims[2] = 1.0;
  }

  dataLen = imgDims[0] * imgDims[1] * imgDims[2];
  fileLen = imgDims[0] * imgDims[1];

  image->pixels = new Pixel_T [dataLen];
  if (convertShort) {
    tmpDataShort = new short [dataLen];
  } else {
    tmpDataDouble = new double [dataLen];
  }

  for ( filecount = fileNumRange[0];
	filecount <= fileNumRange[1];
	filecount++ ) {

    filenum = filecount - fileNumRange[0];

    sprintf( filename, "%s.%03d", filebase, filecount );
    fp = fopen( filename, "r" );
    if (fp == NULL) {
      fprintf(stderr, "ERR: Couldn't open image file %s.\n", filename);
      delete image->pixels;
      delete image;
      return NULL;
    }

    if (convertShort) {
      num = fread( (tmpDataShort + (filenum * fileLen)), sizeof(short),
		   fileLen, fp );
    } else {
      num = fread( (tmpDataDouble + (filenum * fileLen)), sizeof(double),
		   fileLen, fp );
    }

    fclose(fp);
    if (num != fileLen) {
      fprintf(stderr, "ERR: Image file size mismatch [%s].\n", filename);
      delete image->pixels;
      delete image;
      return NULL;
    }
  }

  for (i = 0; i < dataLen; i++) {
    planeIx = i / (imgDims[0] * imgDims[1]);
    planeOffset = i % (imgDims[0] * imgDims[1]);
    rowIx = planeOffset / imgDims[0];
    colIx = planeOffset % imgDims[0];
    if (convertShort) {
      image->pixels[i].intensity = (double)tmpDataShort[i];
    } else {
      image->pixels[i].intensity = tmpDataDouble[i];
    }
    image->pixels[i].plane = planeIx;
    image->pixels[i].row = rowIx;
    image->pixels[i].col = colIx;
  }

  if (convertShort) delete tmpDataShort;
  else delete tmpDataDouble;

  strcpy( image->filebase, filebase );
  image->fileNumRange[0] = fileNumRange[0];
  image->fileNumRange[1] = fileNumRange[1];

  image->closed = 0;

  image->lowerleft[0] = 0.0;
  image->lowerleft[1] = 0.0;
  image->lowerleft[2] = 0.0;

  return image;
}


// ================
//   Image_Delete
// ================
// A good reason why this should have been OO from the beginning!

void Image_Delete( Image_T *img )
{
  if ( img != NULL ) {
    if ( img->pixels != NULL ) {
      delete [] img->pixels;
    }
    delete img;
  }
  return;
}


// =====================
//   ComputePointSlope
// =====================
// Computes the first spatial derivative at a given point (curr) in
// the dimension indicated by dimFlag (0 = x, 1 = y, 2 = z).  The
// derivative is computed using linear interpolation, and the points
// prev, curr and next are presumed to be from a single-valued
// function.  That is, it is assumed that the slopes of the previous
// and next edges are both finite (i.e. not infinite).
//
// Note some funny business with mixing of types (i.e. of short, int
// and double).  This arises from the fact that the images are stored
// as short's, but working in physical space (i.e. mm) requires
// double's.  And just to be safe, I assign operations btw short's to
// int's to avoid potential overflow problems.

double ComputePointSlope( Pixel_T *prev, Pixel_T *curr, Pixel_T *next, 
                          Image_T *image, int dimFlag )
{
  double rngPrev, rngCurr, rngNext;
  double drng;
  double ddom, domCoord;
  double lenPrev, lenNext;
  double interpFactor, domInterp, rngInterp;
  double slopeDdom, slopeDrng, slope;

  rngPrev = prev->intensity;
  rngCurr = curr->intensity;
  rngNext = next->intensity;

  switch (dimFlag) {
  case 0:
    // Differentiate w.r.t. x.
    ddom = image->pixelDims[0];
    domCoord = (curr->row * ddom) + (ddom / 2.0);
    break;
  case 1:
    // Differentiate w.r.t. y.
    ddom = image->pixelDims[1];
    domCoord = (curr->col * ddom) + (ddom / 2.0);
    break;
  case 2:
    // Differentiate w.r.t. z.
    ddom = image->pixelDims[2];
    domCoord = (curr->plane * ddom) + (ddom / 2.0);
    break;
  default:
    fprintf(stderr, "ERR: Invalid dimension flag in ComputePointSlope.\n");
    exit(0);
  }

  drng = rngCurr - rngPrev;
  lenPrev = sqrt( sqr(ddom) + sqr(drng) );
  drng = rngCurr - rngNext;
  lenNext = sqrt( sqr(ddom) + sqr(drng) );

  // Interpolate along next edge.
  if (lenPrev < lenNext) {
    interpFactor = lenPrev / lenNext;
    domInterp = domCoord + interpFactor * ddom;
    rngInterp = (double)rngCurr - interpFactor * (rngCurr - rngNext);
    slopeDdom = ddom + (domInterp - domCoord);
    slopeDrng = rngInterp - (double)rngPrev;
  }

  // Interpolate along previous edge.
  else {
    interpFactor = lenNext / lenPrev;
    domInterp = domCoord - interpFactor * ddom;
    rngInterp = (double)rngCurr - interpFactor * (rngCurr - rngPrev);
    slopeDdom = (domCoord - domInterp) + ddom;
    slopeDrng = (double)rngNext - rngInterp;
  }

  slope = slopeDrng / slopeDdom;

  return slope;
}


// ====================
//   ComputeImageGrad
// ====================
// It's not exactly clear whether image data is in row-major or
// column-major order.  All indications point to column-major
// (i.e. elements in a particular *row* are contiguous).  As a result,
// I'm going to assume for now that I should traverse all columns for
// each row, and then traverse rows.  If this assumption is wrong
// (i.e. if the file was in fact written s.t. all elements in a
// *column* are contiguous), then what is computed as the x component
// of grad I will actually be the y component, and vice versa.
//
// Note that this implementation does not compute first derivatives at
// pixels along the outside border of the image.  This is because the
// linear interpolation scheme employed in ComputePointSlope requires
// one neighbor on each side of the pixel at which the derivative is
// being computed.  We could do any of various things to get around
// this (e.g. pad an extra layer of pixels around the image whose
// intensities are copied from the edge of the actual image), but
// being able to evaluate the gradient at the very edge of the image
// is probably not important, as the propagating front should probably
// not get too close to the image boundary.

void ComputeImageGrad( Image_T *image )
{
  int xdim, ydim, zdim;
  int i, j, k, pixIx, prevIx, nextIx;
  Pixel_T *prev, *curr, *next;
  int tri;

  if ( image->gradValid ) {
    return;
  }

  xdim = image->imgDims[0];   // xdim == # cols
  ydim = image->imgDims[1];   // ydim == # rows
  zdim = image->imgDims[2];   // zdim == # planes

  if ( image->dim == 3 ) {
    tri = 1;
  } else {
    tri = 0;
  }

  for (k = 0; k < zdim; k++) {
    for (j = 0; j < ydim; j++) {
      for (i = 0; i < xdim; i++) {
	pixIx = (k * xdim * ydim) + (j * xdim) + i;

	// Boundary pixels:
	if ( ( i == 0 ) || ( i == (xdim-1) ) ||
	     ( j == 0 ) || ( j == (ydim-1) ) ||
	     ( (tri) && ( k == 0 ) ) || ( (tri) && ( k == (zdim-1) ) ) ) {

	  curr = &( image->pixels[pixIx] );

	  // x:
	  prevIx = ( i == 0 ) ? 0 : pixIx-1;
	  nextIx = ( i == (xdim-1) ) ? (xdim-1) : pixIx+1;
	  prev = &( image->pixels[prevIx] );
	  next = &( image->pixels[nextIx] );
	  image->pixels[pixIx].gradX = ComputePointSlope( prev, curr, next,
							  image, 0 );

	  // y:
	  prevIx = ( j == 0 ) ? 0 : pixIx-xdim;
	  nextIx = ( j == (ydim-1) ) ? (ydim-1) : pixIx+xdim;
	  prev = &( image->pixels[prevIx] );
	  next = &( image->pixels[nextIx] );
	  image->pixels[pixIx].gradY = ComputePointSlope( prev, curr, next,
							  image, 1 );

	  // z:
	  if ( tri ) {
	    prevIx = ( k == 0 ) ? 0 : pixIx - (xdim*ydim);
	    nextIx = ( k == (zdim-1) ) ? (zdim-1) : pixIx + (xdim*ydim);
	    prev = &( image->pixels[prevIx] );
	    next = &( image->pixels[nextIx] );
	    image->pixels[pixIx].gradZ = ComputePointSlope( prev, curr, next,
							    image, 2 );
	  } else {
	    image->pixels[pixIx].gradZ = 0.0;
	  }

	  // Before 2/16/00:
	  //	  image->pixels[pixIx].gradX = 0.0;
	  //	  image->pixels[pixIx].gradY = 0.0;
	  //	  image->pixels[pixIx].gradZ = 0.0;

	  continue;
	}

	// Differentiation w.r.t. x:
	// (adjacent elements in x are also physically adjacent)
	prev = &( image->pixels[pixIx-1] );
	curr = &( image->pixels[pixIx] );
	next = &( image->pixels[pixIx+1] );
	image->pixels[pixIx].gradX = ComputePointSlope( prev, curr, next, 
							image, 0 );

	// Differentiation w.r.t. y:
	prev = &( image->pixels[pixIx-xdim] );
	curr = &( image->pixels[pixIx] );
	next = &( image->pixels[pixIx+xdim] );
	image->pixels[pixIx].gradY = ComputePointSlope( prev, curr, next, 
							image, 1 );

	// Differentiation w.r.t. z:
	if ( tri ) {
	  prev = &( image->pixels[pixIx - (xdim*ydim)] );
	  curr = &( image->pixels[pixIx] );
	  next = &( image->pixels[pixIx + (xdim*ydim)] );
	  image->pixels[pixIx].gradZ = ComputePointSlope( prev, curr, next, 
							  image, 2 );
	} else {
	  image->pixels[pixIx].gradZ = 0.0;
	}
      }
    }
  }

  image->gradValid = 1;
  return;
}


// ----------
// GetMaxGrad
// ----------

double GetMaxGrad( Image_T *image )
{
  double rng[2];
  Img_GetMagGradRange( image, rng );
  return rng[1];
}


// -------------------
// Img_GetMagGradRange
// -------------------

void Img_GetMagGradRange( Image_T *image, double rng[] )
{
  int numPix;
  int i;
  double gx, gy, gz;
  double mag, currMin, currMax;

  ComputeImageGrad( image );
  numPix = (image->imgDims[0]) * (image->imgDims[1]) * (image->imgDims[2]);
  for ( i = 0; i < numPix; i++ ) {
    gx = image->pixels[i].gradX;
    gy = image->pixels[i].gradY;
    gz = image->pixels[i].gradZ;
    mag = Magnitude( gx, gy, gz );
    if ( i == 0 ) {
      currMin = currMax = mag;
    } else {
      currMax = maximum( mag, currMax );
      currMin = minimum( mag, currMin );
    }
  }
  rng[0] = currMin;
  rng[1] = currMax;
}


// ---------------------
// Img_GetXYMagGradRange
// ---------------------

void Img_GetXYMagGradRange( Image_T *image, double rng[] )
{
  int numPix;
  int i;
  double gx, gy;
  double mag, currMin, currMax;

  ComputeImageGrad( image );
  numPix = (image->imgDims[0]) * (image->imgDims[1]) * (image->imgDims[2]);
  for ( i = 0; i < numPix; i++ ) {
    gx = image->pixels[i].gradX;
    gy = image->pixels[i].gradY;
    mag = Magnitude( gx, gy, 0.0 );
    if ( i == 0 ) {
      currMin = currMax = mag;
    } else {
      currMax = maximum( mag, currMax );
      currMin = minimum( mag, currMin );
    }
  }
  rng[0] = currMin;
  rng[1] = currMax;
}


// --------------------
// Img_GetZMagGradRange
// --------------------

void Img_GetZMagGradRange( Image_T *image, double rng[] )
{
  int numPix;
  int i;
  double gz;
  double mag, currMin, currMax;

  ComputeImageGrad( image );
  numPix = (image->imgDims[0]) * (image->imgDims[1]) * (image->imgDims[2]);
  for ( i = 0; i < numPix; i++ ) {
    gz = image->pixels[i].gradZ;
    mag = fabs(gz);
    if ( i == 0 ) {
      currMin = currMax = mag;
    } else {
      currMax = maximum( mag, currMax );
      currMin = minimum( mag, currMin );
    }
  }
  rng[0] = currMin;
  rng[1] = currMax;
}


// ---------------------
// Img_GetIntensityRange
// ---------------------

void Img_GetIntensityRange( Image_T *image, double rng[] )
{
  int numPix;
  int i;
  double datum;
  double currMin, currMax;

  numPix = (image->imgDims[0]) * (image->imgDims[1]) * (image->imgDims[2]);
  for ( i = 0; i < numPix; i++ ) {
    datum = image->pixels[i].intensity;
    if ( i == 0 ) {
      currMin = currMax = datum;
    } else {
      currMax = maximum( datum, currMax );
      currMin = minimum( datum, currMin );
    }
  }
  rng[0] = currMin;
  rng[1] = currMax;
}


// --------------
// SetImageClosed
// --------------

void SetImageClosed( Image_T *image, int flag )
{
  if ( flag ) {
    image->closed = 1;
  } else {
    image->closed = 0;
  }
}


// --------------
// GetImageClosed
// --------------

int GetImageClosed( Image_T *image )
{
  return image->closed;
}


// ------------
// SetLowerLeft
// ------------

void SetLowerLeft( Image_T *image, double pos[] )
{
  image->lowerleft[0] = pos[0];
  image->lowerleft[1] = pos[1];
  if ( image->dim == 3 ) {
    image->lowerleft[2] = pos[2];
  }
}


// ------------
// GetLowerLeft
// ------------

void GetLowerLeft( Image_T *image, double pos[] )
{
  pos[0] = image->lowerleft[0];
  pos[1] = image->lowerleft[1];
  pos[2] = image->lowerleft[2];
}


// --------
// InBorder
// --------

int InBorder( Image_T *image, double pos[], int borderWd )
{
  double border[3];
  double side[3];
  double ppos[3];

  if ( borderWd <= 0 ) {
    return 0;
  }

  ppos[0] = pos[0] - image->lowerleft[0];
  ppos[1] = pos[1] - image->lowerleft[1];
  ppos[2] = pos[2] - image->lowerleft[2];

  border[0] = image->pixelDims[0] * borderWd;
  border[1] = image->pixelDims[1] * borderWd;
  border[2] = image->pixelDims[2] * borderWd;

  side[0] = image->pixelDims[0] * image->imgDims[0];
  side[1] = image->pixelDims[1] * image->imgDims[1];
  side[2] = image->pixelDims[2] * image->imgDims[2];

  if ( ( ppos[0] <= border[0] ) || ( ppos[0] >= ( side[0] - border[0] ) ) ||
       ( ppos[1] <= border[1] ) || ( ppos[1] >= ( side[1] - border[1] ) ) ||
       ( ppos[2] <= border[2] ) || ( ppos[2] >= ( side[2] - border[2] ) ) ) {
    return 1;
  }
  return 0;
}


// --------------------
// CloseImageGradBounds
// --------------------
// Post-process an image's gradient fields by over-writing the
// computed values in a boundary region (of an arbitrary width) with
// the maximum gradient value.  This is being done so that fronts
// propagating in an image can be "sealed off" at the image edges.

// The DIRECTIONALITY of the applied maximum gradient is an unresolved
// issue.

void CloseImageGradBounds( Image_T *image )
{
  int xdim, ydim, zdim;
  int i, j, k, pixIx;
  double maxG;
  double rng[2];
  int tri;
  int bdReg = 3;

  if ( image->closed ) {
    return;
  }

  ComputeImageGrad( image );
  Img_GetMagGradRange( image, rng );
  maxG = rng[1];

  xdim = image->imgDims[0];   // xdim == # cols
  ydim = image->imgDims[1];   // ydim == # rows
  zdim = image->imgDims[2];   // zdim == # planes

  if ( image->dim == 3 ) {
    tri = 1;
  } else {
    tri = 0;
  }

  for (k = 0; k < zdim; k++) {
    for (j = 0; j < ydim; j++) {
      for (i = 0; i < xdim; i++) {
	pixIx = (k * xdim * ydim) + (j * ydim) + i;

	if ( ( i < bdReg ) || ( i >= (xdim-bdReg) ) ||
	     ( j < bdReg ) || ( j >= (ydim-bdReg) ) ||
	     ( (tri) && ( k < bdReg ) ) ||
	     ( (tri) && ( k >= (zdim-bdReg) ) ) ) {
	  image->pixels[pixIx].gradX = maxG;
	  image->pixels[pixIx].gradY = maxG;
	  image->pixels[pixIx].gradZ = maxG;
	  continue;
	}
      }
    }
  }

  image->closed = 1;

  return;
}


// -----------
// CreateImage
// -----------
// Build an Image_T object using a given data array.  The data may
// be of either type double or short, as specified with imgTypeFlag,
// but the is currently no error checking for inconsistency.  The data
// object may actually be double's, while imgTypeFlag may be
// incorrectly specified as -short, which would obviously cause things
// to be all hosed up.
//
// NOTE that the input data array is COPIED, so the caller should NOT
// surrender memory mgmt of that array upon calling this function.

Image_T *CreateImage( void *data, int numData, char *imgTypeFlag,
		      int imgDims[], double pixelDims[] )
{
  Image_T *image;
  int i, len;
  short *tmpDataShort;
  float *tmpDataFloat;
  double *tmpDataDouble;
  int rowIx, colIx, planeIx, planeOffset;
  int dataCode;

   // See notes at the other call to FindMachineEpsilon.
  gMachineEpsilon = 100.0 * FindMachineEpsilon();

  if ( !strcmp( imgTypeFlag, "-short" ) ) {
    dataCode = 0;
  } else if ( !strcmp( imgTypeFlag, "-double" ) ) {
    dataCode = 1;
  } else if ( !strcmp( imgTypeFlag, "-float" ) ) {
    dataCode = 2;
  } else {
    fprintf(stderr, "ERR: Invalid image data type flag.\n");
    exit(0);
  }

  image = new Image_T;
  image->gradValid = 0;
  for (i = 0; i < 3; i++) {
    image->imgDims[i] = imgDims[i];
    image->pixelDims[i] = pixelDims[i];
  }
  if ( image->imgDims[2] == 1 ) {
    image->dim = 2;
  } else {
    image->dim = 3;
  }
  len = imgDims[0] * imgDims[1] * imgDims[2];
  if (len != numData) {
    fprintf(stderr, "ERR: Data size mismatch.\n");
    return NULL;
  }
  image->pixels = new Pixel_T [len];

  switch (dataCode) {
  case 0:
    tmpDataShort = (short *)data;
    break;
  case 1:
    tmpDataDouble = (double *)data;
    break;
  case 2:
    tmpDataFloat = (float *)data;
    break;
  }

  for (i = 0; i < len; i++) {
    planeIx = i / (imgDims[0] * imgDims[1]);
    planeOffset = i % (imgDims[0] * imgDims[1]);
    rowIx = planeOffset / imgDims[0];
    colIx = planeOffset % imgDims[0];

    switch (dataCode) {
    case 0:
      image->pixels[i].intensity = (double)tmpDataShort[i];
      break;
    case 1:
      image->pixels[i].intensity = tmpDataDouble[i];
      break;
    case 2:
      image->pixels[i].intensity = (double)tmpDataFloat[i];
      break;
    }

    image->pixels[i].plane = planeIx;
    image->pixels[i].row = rowIx;
    image->pixels[i].col = colIx;
  }

  image->closed = 0;

  image->lowerleft[0] = 0.0;
  image->lowerleft[1] = 0.0;
  image->lowerleft[2] = 0.0;

  return image;
}


// ------------
// LinearInterp
// ------------
// Bi-/tri- linear interpolation of specified image quantity.

int LinearInterp( Image_T *image, ImageData_T code, double pos[],
		  double *value )
{
  int pixelCol, pixelRow, pixelPlane, pixelIx;
  int inBorder, octant;
  double xBdWidth, yBdWidth, zBdWidth;
  double pixelCx, pixelCy, pixelCz;
  int ix1, ix2, ix3, ix4, ix5, ix6, ix7, ix8;
  double I1, I2, I3, I4, I5, I6, I7, I8;
  int rowNum1, colNum1, planeNum1, planeOffset;
  double cx1, cy1, cz1;
  double rx, ry, rz;
  double q14, q58, result;
  int tri;
  double x, y, z;
  double ppos[3];

  x = pos[0] - image->lowerleft[0];
  y = pos[1] - image->lowerleft[1];
  if ( image->dim == 3 ) {
    z = pos[2] - image->lowerleft[2];
    tri = 1;
  } else {
    z = 0.0;
    tri = 0;
  }

  ppos[0] = x;
  ppos[1] = y;
  ppos[2] = z;

  if ( !InRange( image, ppos ) ) {
    return 0;
  }

  xBdWidth = image->pixelDims[0] / 2.0;
  yBdWidth = image->pixelDims[1] / 2.0;
  zBdWidth = image->pixelDims[2] / 2.0;
  if ( ( x <= xBdWidth ) ||
       ( x >= ( ( image->imgDims[0] * image->pixelDims[0] ) - xBdWidth ) ) ||
       ( y <= yBdWidth ) ||
       ( y >= ( ( image->imgDims[1] * image->pixelDims[1] ) - yBdWidth ) ) ||
       ( (tri) && ( z <= zBdWidth ) ) ||
       ( (tri) && ( z >= ( ( image->imgDims[2] * image->pixelDims[2] ) -
			   zBdWidth ) ) ) ) {
    inBorder = 1;
  } else {
    inBorder = 0;
  }
  
  pixelCol = (int)floor( x / image->pixelDims[0] );
  pixelRow = (int)floor( y / image->pixelDims[1] );
  if (tri) {
    pixelPlane = (int)floor( z / image->pixelDims[2] );
  } else {
    pixelPlane = 0;
  }

  pixelCol = ( pixelCol >= image->imgDims[0] ) ?
    ( image->imgDims[0] - 1 ) : pixelCol;
  pixelRow = ( pixelRow >= image->imgDims[1] ) ?
    ( image->imgDims[1] - 1 ) : pixelRow;
  pixelPlane = ( pixelPlane >= image->imgDims[2] ) ?
    ( image->imgDims[2] - 1 ) : pixelPlane;

  pixelIx = \
    (image->imgDims[0] * image->imgDims[1] * pixelPlane) \
    + (image->imgDims[0] * pixelRow) \
    + pixelCol;

  if (inBorder) {
    switch (code) {
    case IMG_INTENSITY:
      *value = (image->pixels[pixelIx].intensity);
      return 1;
    case IMG_GRADIX:
      *value = (image->pixels[pixelIx].gradX);
      return 1;
    case IMG_GRADIY:
      *value = (image->pixels[pixelIx].gradY);
      return 1;
    case IMG_GRADIZ:
      *value = (image->pixels[pixelIx].gradZ);
      return 1;
    default:
      fprintf(stderr, "ERR: ImageData_T not handled correctly.\n");
      return 0;
    }
  }

  // Find centroid of containing voxel:
  pixelCx = pixelCol * image->pixelDims[0] + xBdWidth;
  pixelCy = pixelRow * image->pixelDims[1] + yBdWidth;
  pixelCz = pixelPlane * image->pixelDims[2] + zBdWidth;

  if ( x > pixelCx ) {
    if ( y > pixelCy ) {
      if ( ( !tri ) || ( z > pixelCz ) ) {
	octant = 1;
      } else {
	octant = 5;
      }
    } else {
      if ( ( !tri ) || ( z > pixelCz ) ) {
	octant = 4;
      } else {
	octant = 8;
      }
    }
  } else {
    if ( y > pixelCy ) {
      if ( ( !tri ) || ( z > pixelCz ) ) {
	octant = 2;
      } else {
	octant = 6;
      }
    } else {
      if ( ( !tri ) || ( z > pixelCz ) ) {
	octant = 3;
      } else {
	octant = 7;
      }
    }
  }

  if ( !tri ) {
    assert( octant != 5 );
    assert( octant != 6 );
    assert( octant != 7 );
    assert( octant != 8 );
  }

  switch (octant) {
  case 1:
    // ix1 to ix4 lie in the same plane (i.e. z const.):
    ix1 = pixelIx;
    ix2 = ix1 + 1;
    ix3 = ix2 + image->imgDims[0];
    ix4 = ix3 - 1;

    // ix5 to ix8 lie in the next plane (i.e. z' = z + 1):
    ix5 = pixelIx + (image->imgDims[0] * image->imgDims[1]);
    ix6 = ix5 + 1;
    ix7 = ix6 + image->imgDims[0];
    ix8 = ix7 - 1;
    break;

  case 2:
    ix2 = pixelIx;
    ix1 = ix2 - 1;
    ix3 = ix2 + image->imgDims[0];
    ix4 = ix3 - 1;

    ix6 = pixelIx + (image->imgDims[0] * image->imgDims[1]);
    ix5 = ix6 - 1;
    ix7 = ix6 + image->imgDims[0];
    ix8 = ix7 - 1;
    break;

  case 3:
    ix3 = pixelIx;
    ix2 = ix3 - image->imgDims[0];
    ix4 = ix3 - 1;
    ix1 = ix2 - 1;

    ix7 = pixelIx + (image->imgDims[0] * image->imgDims[1]);
    ix6 = ix7 - image->imgDims[0];
    ix8 = ix7 - 1;
    ix5 = ix6 - 1;
    break;

  case 4:
    ix4 = pixelIx;
    ix3 = ix4 + 1;
    ix2 = ix3 - image->imgDims[0];
    ix1 = ix2 - 1;

    ix8 = pixelIx + (image->imgDims[0] * image->imgDims[1]);
    ix7 = ix8 + 1;
    ix6 = ix7 - image->imgDims[0];
    ix5 = ix6 - 1;
    break;

  case 5:
    // same as case 1 except pixelIx lies in the upper plane:
    ix5 = pixelIx;
    ix6 = ix5 + 1;
    ix7 = ix6 + image->imgDims[0];
    ix8 = ix7 - 1;

    ix1 = pixelIx - (image->imgDims[0] * image->imgDims[1]);
    ix2 = ix1 + 1;
    ix3 = ix2 + image->imgDims[0];
    ix4 = ix3 - 1;
    break;

  case 6:
    ix6 = pixelIx;
    ix5 = ix6 - 1;
    ix7 = ix6 + image->imgDims[0];
    ix8 = ix7 - 1;

    ix2 = pixelIx - (image->imgDims[0] * image->imgDims[1]);
    ix1 = ix2 - 1;
    ix3 = ix2 + image->imgDims[0];
    ix4 = ix3 - 1;
    break;

  case 7:
    ix7 = pixelIx;
    ix6 = ix7 - image->imgDims[0];
    ix8 = ix7 - 1;
    ix5 = ix6 - 1;

    ix3 = pixelIx - (image->imgDims[0] * image->imgDims[1]);
    ix2 = ix3 - image->imgDims[0];
    ix4 = ix3 - 1;
    ix1 = ix2 - 1;
    break;

  case 8:
    ix8 = pixelIx;
    ix7 = ix8 + 1;
    ix6 = ix7 - image->imgDims[0];
    ix5 = ix6 - 1;

    ix4 = pixelIx - (image->imgDims[0] * image->imgDims[1]);
    ix3 = ix4 + 1;
    ix2 = ix3 - image->imgDims[0];
    ix1 = ix2 - 1;
    break;

  default:
    return 0;
    break;
  }

  // Check the sanity of the resulting indices (yes, I'm paranoid):

  assert( (ix1 / (image->imgDims[0] * image->imgDims[1])) >= 0 );
  assert( (ix1 / (image->imgDims[0] * image->imgDims[1])) <
	  image->imgDims[2] );

  if ( tri ) {
    assert( (ix5 / (image->imgDims[0] * image->imgDims[1])) >= 0 );
    assert( (ix5 / (image->imgDims[0] * image->imgDims[1])) <
	    image->imgDims[2] );
  }

  switch (code) {
  case IMG_INTENSITY:
    I1 = image->pixels[ix1].intensity;
    I2 = image->pixels[ix2].intensity;
    I3 = image->pixels[ix3].intensity;
    I4 = image->pixels[ix4].intensity;
    if ( tri ) {
      I5 = image->pixels[ix5].intensity;
      I6 = image->pixels[ix6].intensity;
      I7 = image->pixels[ix7].intensity;
      I8 = image->pixels[ix8].intensity;
    }
    break;
  case IMG_GRADIX:
    I1 = image->pixels[ix1].gradX;
    I2 = image->pixels[ix2].gradX;
    I3 = image->pixels[ix3].gradX;
    I4 = image->pixels[ix4].gradX;
    if ( tri ) {
      I5 = image->pixels[ix5].gradX;
      I6 = image->pixels[ix6].gradX;
      I7 = image->pixels[ix7].gradX;
      I8 = image->pixels[ix8].gradX;
    }
    break;
  case IMG_GRADIY:
    I1 = image->pixels[ix1].gradY;
    I2 = image->pixels[ix2].gradY;
    I3 = image->pixels[ix3].gradY;
    I4 = image->pixels[ix4].gradY;
    if ( tri ) {
      I5 = image->pixels[ix5].gradY;
      I6 = image->pixels[ix6].gradY;
      I7 = image->pixels[ix7].gradY;
      I8 = image->pixels[ix8].gradY;
    }
    break;
  case IMG_GRADIZ:
    I1 = image->pixels[ix1].gradZ;
    I2 = image->pixels[ix2].gradZ;
    I3 = image->pixels[ix3].gradZ;
    I4 = image->pixels[ix4].gradZ;
    if ( tri ) {
      I5 = image->pixels[ix5].gradZ;
      I6 = image->pixels[ix6].gradZ;
      I7 = image->pixels[ix7].gradZ;
      I8 = image->pixels[ix8].gradZ;
    }
    break;
  default:
    fprintf(stderr, "ERR: ImageData_T not handled correctly.\n");
    return 0;
    break;
  }

  // To calculate rx and ry, we want to determine the x and y coords
  // of the centroid of the pixel whose index we have assigned to be
  // ix1.

  planeNum1 = ix1 / (image->imgDims[0] * image->imgDims[1]);
  planeOffset = ix1 % (image->imgDims[0] * image->imgDims[1]);
  rowNum1 = planeOffset / image->imgDims[0];
  colNum1 = planeOffset % image->imgDims[0];
  
  cx1 = colNum1 * image->pixelDims[0] + xBdWidth;
  cy1 = rowNum1 * image->pixelDims[1] + yBdWidth;
  if ( tri ) {
    cz1 = planeNum1 * image->pixelDims[2] + zBdWidth;
  }

  rx = ( x - cx1 ) / image->pixelDims[0];
  if ( fabs(rx-1.0) <= gMachineEpsilon ) {
    rx = 1.0;
  }
  ry = ( y - cy1 ) / image->pixelDims[1];
  if ( fabs(ry-1.0) <= gMachineEpsilon ) {
    ry = 1.0;
  }
  if ( tri ) {
    rz = ( z - cz1 ) / image->pixelDims[2];
    if ( fabs(rz-1.0) <= gMachineEpsilon ) {
      rz = 1.0;
    }
  }

  assert( rx >= 0.0 );
  assert( rx <= 1.0 );
  assert( ry >= 0.0 );
  assert( ry <= 1.0 );
  if ( tri ) {
    assert( rz >= 0.0 );
    assert( rz <= 1.0 );
  }

  q14 = ( 1.0 - rx - ry + (rx*ry) ) * I1;
  q14 += rx * ( 1.0 - ry ) * I2;
  q14 += rx * ry * I3;
  q14 += ry * ( 1.0 - rx ) * I4;

  if ( tri ) {
    q58 = ( 1.0 - rx - ry + (rx*ry) ) * I5;
    q58 += rx * ( 1.0 - ry ) * I6;
    q58 += rx * ry * I7;
    q58 += ry * ( 1.0 - rx ) * I8;
  }

  if ( !tri ) {
    result = q14;
  } else {
    result = rz * ( q58 - q14 ) + q14;
  }

  *value = result;
  return 1;
}


// ------------
// GetIntensity
// ------------

int GetIntensity( Image_T *image, double pos[], double *result )
{
  return LinearInterp( image, IMG_INTENSITY, pos, result );
}


// ---------
// GetGradIx
// ---------

int GetGradIx( Image_T *image, double pos[], double *result )
{
  if ( ! image->gradValid ) {
    ComputeImageGrad( image );
  }
  return LinearInterp( image, IMG_GRADIX, pos, result );
}


// ---------
// GetGradIy
// ---------

int GetGradIy( Image_T *image, double pos[], double *result )
{
  if ( ! image->gradValid ) {
    ComputeImageGrad( image );
  }
  return LinearInterp( image, IMG_GRADIY, pos, result );
}


// ---------
// GetGradIz
// ---------

int GetGradIz( Image_T *image, double pos[], double *result )
{
  if ( ! image->gradValid ) {
    ComputeImageGrad( image );
  }
  return LinearInterp( image, IMG_GRADIZ, pos, result );
}


// -----------
// WriteZSlice
// -----------

void WriteZSlice( Image_T *image, char *filename, int num,
		  char *imgTypeFlag, ImageData_T field )
{
  FILE *fp;
  int len, i;
  short sDatum;
  double dDatum;
  int convertShort;

  if ( !strcmp( imgTypeFlag, "-short" ) ) {
    convertShort = 1;
  } else if ( !strcmp( imgTypeFlag, "-double" ) ) {
    convertShort = 0;
  } else {
    fprintf(stderr, "ERR: Invalid image data type flag.\n");
    return;
  }

  fp = fopen( filename, "w" );

  len = image->imgDims[0] * image->imgDims[1] * image->imgDims[2];
  for ( i = 0; i < len; i++ ) {
    if ( image->pixels[i].plane == num ) {
      switch (field) {
      case IMG_INTENSITY:
	if (convertShort) {
	  sDatum = (short)(image->pixels[i].intensity);
	  fwrite( &sDatum, sizeof(short), 1, fp );
	} else {
	  dDatum = image->pixels[i].intensity;
	  fwrite( &dDatum, sizeof(double), 1, fp );
	}
	break;
      case IMG_GRADIX:
	if (convertShort) {
	  sDatum = (short)(image->pixels[i].gradX);
	  fwrite( &sDatum, sizeof(short), 1, fp );
	} else {
	  dDatum = image->pixels[i].gradX;
	  fwrite( &dDatum, sizeof(double), 1, fp );
	}
	break;
      case IMG_GRADIY:
	if (convertShort) {
	  sDatum = (short)(image->pixels[i].gradY);
	  fwrite( &sDatum, sizeof(short), 1, fp );
	} else {
	  dDatum = image->pixels[i].gradY;
	  fwrite( &dDatum, sizeof(double), 1, fp );
	}
	break;
      case IMG_GRADIZ:
	if (convertShort) {
	  sDatum = (short)(image->pixels[i].gradZ);
	  fwrite( &sDatum, sizeof(short), 1, fp );
	} else {
	  dDatum = image->pixels[i].gradZ;
	  fwrite( &dDatum, sizeof(double), 1, fp );
	}
	break;
      }
    }
  }

  fclose(fp);
  return;
}


// -------
// InRange
// -------
// For INTERNAL USE only!

int InRange( Image_T *image, double pos[] )
{
  double x, y, z;
  double maxx;
  double maxy;
  double maxz;

  x = pos[0];
  maxx = image->pixelDims[0] * image->imgDims[0];
  if ( (x < 0.0) || (x > maxx) ) {
    return 0;
  }

  y = pos[1];
  maxy = image->pixelDims[1] * image->imgDims[1];
  if ( (y < 0.0) || (y > maxy) ) {
    return 0;
  }

  if ( image->dim == 3 ) {
    z = pos[2];
    maxz = image->pixelDims[2] * image->imgDims[2];
    if ( (z < 0.0) || (z > maxz) ) {
      return 0;
    }
  }

  return 1;
}


// ------------------
// Img_GetMemoryUsage
// ------------------

int Img_GetMemoryUsage( Image_T *image )
{
  int numPix;
  int sz;

  if ( image == NULL ) {
    return 0;
  }

  numPix = image->imgDims[0] * image->imgDims[1] * image->imgDims[2];
  sz = sizeof( Image_T );
  sz += numPix * sizeof( Pixel_T );

  return sz;
}
