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

#ifndef __CVIMAGE_H
#define __CVIMAGE_H

#include "SimVascular.h"

/* KCW [12/9/98]
 * ---
 * Yes, this should be an object, and there are a lot of things
 * contained here which demonstrate reasons for objects, but this has
 * evolved from a C-style implementation and will probably remain this
 * way until we build more image functionality. */

typedef struct {
  double intensity;
  int row;
  int col;
  int plane;
  double gradX;
  double gradY;
  double gradZ;
} Pixel_T;

typedef struct {
  Pixel_T *pixels;
  int imgDims[3];        // in pixels
  double pixelDims[3];   // in physical units
  char filebase[1000];
  int fileNumRange[2];
  int dim;
  int gradValid;
  int closed;
  double lowerleft[3];
} Image_T;

typedef enum {
  IMG_INTENSITY, IMG_GRADIX, IMG_GRADIY, IMG_GRADIZ
} ImageData_T;


Image_T *ReadImage( char *filebase, int fileNumRange[], char *imgTypeFlag, 
		    int imgDims[], double pixelDims[] );

Image_T *CreateImage( void *data, int numData, char *imgTypeFlag,
		      int imgDims[], double pixelDims[] );

void Image_Delete( Image_T *img );

double ComputePointSlope( Pixel_T *prev, Pixel_T *curr, Pixel_T *next, 
                          Image_T *image, int dimFlag );

void ComputeImageGrad( Image_T *image );

double GetMaxGrad( Image_T *image );

void Img_GetMagGradRange( Image_T *image, double rng[] );

void Img_GetXYMagGradRange( Image_T *image, double rng[] );

void Img_GetZMagGradRange( Image_T *image, double rng[] );

void Img_GetIntensityRange( Image_T *image, double rng[] );

void SetImageClosed( Image_T *image, int flag );

int GetImageClosed( Image_T *image );

void SetLowerLeft( Image_T *image, double pos[] );

void GetLowerLeft( Image_T *image, double pos[] );

int InBorder( Image_T *image, double pos[], int borderWd );

void CloseImageGradBounds( Image_T *image );

int LinearInterp( Image_T *image, ImageData_T code, double pos[],
		  double *value );

int GetIntensity( Image_T *image, double pos[], double *result );

int GetGradIx( Image_T *image, double pos[], double *result );

int GetGradIy( Image_T *image, double pos[], double *result );

int GetGradIz( Image_T *image, double pos[], double *result );

void WriteZSlice( Image_T *image, char *filename, int num,
		  char *imgTypeFlag, ImageData_T field );

int InRange( Image_T *image, double pos[] );

int Img_GetMemoryUsage( Image_T *image );


#endif // __IMAGE_H
