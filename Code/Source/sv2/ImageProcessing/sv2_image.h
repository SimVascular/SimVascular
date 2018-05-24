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

#ifndef __CVIMAGE_H
#define __CVIMAGE_H

#include "SimVascular.h"
#include "svImageExports.h" // For exports

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


SV_EXPORT_IMAGE Image_T *ReadImage( char *filebase, int fileNumRange[], char *imgTypeFlag,
		    int imgDims[], double pixelDims[] );

SV_EXPORT_IMAGE Image_T *CreateImage( void *data, int numData, char *imgTypeFlag,
		      int imgDims[], double pixelDims[] );

SV_EXPORT_IMAGE void Image_Delete( Image_T *img );

SV_EXPORT_IMAGE double ComputePointSlope( Pixel_T *prev, Pixel_T *curr, Pixel_T *next,
                          Image_T *image, int dimFlag );

SV_EXPORT_IMAGE void ComputeImageGrad( Image_T *image );

SV_EXPORT_IMAGE double GetMaxGrad( Image_T *image );

SV_EXPORT_IMAGE void Img_GetMagGradRange( Image_T *image, double rng[] );

SV_EXPORT_IMAGE void Img_GetXYMagGradRange( Image_T *image, double rng[] );

SV_EXPORT_IMAGE void Img_GetZMagGradRange( Image_T *image, double rng[] );

SV_EXPORT_IMAGE void Img_GetIntensityRange( Image_T *image, double rng[] );

SV_EXPORT_IMAGE void SetImageClosed( Image_T *image, int flag );

SV_EXPORT_IMAGE int GetImageClosed( Image_T *image );

SV_EXPORT_IMAGE void SetLowerLeft( Image_T *image, double pos[] );

SV_EXPORT_IMAGE void GetLowerLeft( Image_T *image, double pos[] );

SV_EXPORT_IMAGE int InBorder( Image_T *image, double pos[], int borderWd );

SV_EXPORT_IMAGE void CloseImageGradBounds( Image_T *image );

SV_EXPORT_IMAGE int LinearInterp( Image_T *image, ImageData_T code, double pos[],
		  double *value );

SV_EXPORT_IMAGE int GetIntensity( Image_T *image, double pos[], double *result );

SV_EXPORT_IMAGE int GetGradIx( Image_T *image, double pos[], double *result );

SV_EXPORT_IMAGE int GetGradIy( Image_T *image, double pos[], double *result );

SV_EXPORT_IMAGE int GetGradIz( Image_T *image, double pos[], double *result );

SV_EXPORT_IMAGE void WriteZSlice( Image_T *image, char *filename, int num,
		  char *imgTypeFlag, ImageData_T field );

SV_EXPORT_IMAGE int InRange( Image_T *image, double pos[] );

SV_EXPORT_IMAGE int Img_GetMemoryUsage( Image_T *image );


#endif // __IMAGE_H
