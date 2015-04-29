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
#include "cvVTK.h"
#include "cvMath.h"
#include "cvRepository.h"
#include "cvRepositoryData.h"
#include "cvPolyData.h"
#include "cv_sys_geom.h"
#include "cv_calc_correction_eqn.h"

int img_calcCorrectionEqn(int numRegions,vtkPolyData **listPd,
                          int numImages,vtkStructuredPoints **listImg,
                          int order, double results[]) {

   int i;

   // check the range on the order of the requested polynomial
   if (order < 0 || order > 2) {
       fprintf(stderr,"ERROR:  Order of $order not permitted.\n");
       return CV_ERROR;
   }
   if (numRegions <= 0 || numImages <= 0) {
       fprintf(stderr,"ERROR: invalid value of numRegions or numImages.\n");
       return CV_ERROR;
   }


   // assuming all of the image slices are in the same exact
   // location, lets get the extent and spacing info from the first
   vtkFloatingPointType spacing[3];
   vtkFloatingPointType vtk_origin[3];
   int dimensions[3];
   int extent[6];

   listImg[0]->GetSpacing(spacing);
   listImg[0]->GetExtent(extent);
   listImg[0]->GetOrigin(vtk_origin);
   listImg[0]->GetDimensions(dimensions);

   // debug
   fprintf(stdout,"spacing: %f %f %f\n",spacing[0],spacing[1],spacing[2]);
   fprintf(stdout,"extent: %i %i %i %i %i %i\n",extent[0],extent[1],extent[2],
           extent[3],extent[4],extent[5]);
   fprintf(stdout,"vtk_origin: %f %f %f\n",vtk_origin[0],vtk_origin[1],
           vtk_origin[2]);
   fprintf(stdout,"dimensions: %i %i %i\n",dimensions[0],dimensions[1],
           dimensions[2]);


   // to speed things up, we first find the bounding box of each
   // region and convert these into voxel indices.  Only then will
   // we do the point classification to determine the voxels inside
   // of the region.

   int *imin = new int[numRegions];
   int *imax = new int[numRegions];
   int *jmin = new int[numRegions];
   int *jmax = new int[numRegions];

   vtkFloatingPointType bbox[6];

   for (i = 0; i < numRegions; i++) {
       // find bbox
      listPd[i]->GetPoints()->ComputeBounds();
      listPd[i]->GetPoints()->GetBounds(bbox);
      imin[i] = int((bbox[0] - vtk_origin[0])/spacing[0]);
      imax[i] = int((bbox[1] - vtk_origin[0])/spacing[0]);
      jmin[i] = int((bbox[2] - vtk_origin[1])/spacing[1]);
      jmax[i] = int((bbox[3] - vtk_origin[1])/spacing[1]);
      if (imin[i] < extent[0] || imax[i] > extent[1] ||
	  jmin[i] < extent[2] || jmax[i] > extent[3]) {
          fprintf(stderr,"ERROR: region %i bounding box not completely inside of domain.\n",i);
          delete [] imin;delete [] imax;delete [] jmin;delete [] jmax;
          return CV_ERROR;
      }
   }

   // now do point a point classification on the polygon
   // to find voxels to be used in calculating equation
   int maxPixNum = dimensions[0]*dimensions[1];

   short int *voxs = new short int [maxPixNum];
   for (i = 0; i < maxPixNum; i++) {
       voxs[i] = 0;
   }

   double pt[2];
   int n,j;
   cvPolyData *pd;
   int numIn = 0;

   for (n = 0; n < numRegions; n++) {
      pd = new cvPolyData(listPd[n]);
      // dummy call to initialize polygon in ptInPoly
      pt[0]=0;pt[1]=0;
      int result = 0;
      sys_geom_PtInPoly(pd,pt,0,&result);
      for (i = imin[n];i <= imax[n]; i++) {
	for (j = jmin[n];j <= jmax[n];j++) {
            pt[0] = i*spacing[0]+vtk_origin[0];
            pt[1] = j*spacing[1]+vtk_origin[1];
            if (sys_geom_PtInPoly(pd,pt,1,&result) == CV_ERROR) {
                delete [] voxs;
                delete [] imin;delete [] imax;delete [] jmin;delete [] jmax;
                return CV_ERROR;
            }
            if (result == 1) {
              int pixel = i + j * dimensions[0];
              // only count a pixel once, it may have been in a 
              // previous region
              if (voxs[pixel] == 0) {
                voxs[pixel] = 1;
                numIn++;
              }
	    }
	}  // j
      }  // i
      delete pd;
   }  // n

   // done with the bounding boxes
   delete [] imin;delete [] imax;delete [] jmin;delete [] jmax;

   // debugging
   fprintf(stdout,"found %i pixels inside of static regions.\n",numIn);

   // depending on the requested order set up the matrices
   // for the least squares fit

   cvMath *mathobj = new cvMath();

   int xOrder;
   int yOrder = 1;

   if (order == 0) {
       xOrder = 1;
   } else if (order == 1) {
       xOrder = 3;
   } else if (order == 2) {
       xOrder = 6;
   } else {
       // should never get here
       delete [] voxs;
       delete mathobj;
       return CV_ERROR;
   }

   double **xt = mathobj->createArray(numIn*numImages,xOrder);
   double **yt = mathobj->createArray(numIn*numImages,yOrder);
   double **mt = mathobj->createArray(xOrder,yOrder);

   int ni;
   int p;
   vtkFloatingPointType value;
   int s = 0;

   // does a zeroth order fit just equate to averaging the values?

   if (order == 0) {
     for (p = 0; p < maxPixNum; p++) {
       if (voxs[p] == 1) {
         // figure out i,j indices for pixel
         i =  p%dimensions[0];
         j =  p/dimensions[1];
         pt[0] = i*spacing[0]+vtk_origin[0];
         pt[1] = j*spacing[1]+vtk_origin[1];
         for (ni = 0; ni < numImages; ni++) {
            value = listImg[ni]->GetPointData()->GetScalars()->GetTuple1(p);
            yt[s][0] = value;
            xt[s][0] = 1;
            s++;
         }
       } 
     }
   } else if (order == 1) {
     for (p = 0; p < maxPixNum; p++) {
       if (voxs[p] == 1) {
         // figure out i,j indices for pixel
         i =  p%dimensions[0];
         j =  p/dimensions[1];
         pt[0] = i*spacing[0]+vtk_origin[0];
         pt[1] = j*spacing[1]+vtk_origin[1];
         for (ni = 0; ni < numImages; ni++) {
            value = listImg[ni]->GetPointData()->GetScalars()->GetTuple1(p);
            yt[s][0] = value;
            xt[s][0] = 1;xt[s][1] = pt[0];xt[s][2] = pt[1];
            s++;
         } 
       }
     }
   } else if (order == 2) {
     for (p = 0; p < maxPixNum; p++) {
       if (voxs[p] == 1) {
         // figure out i,j indices for pixel
         i =  p%dimensions[0];
         j =  p/dimensions[1];
         pt[0] = i*spacing[0]+vtk_origin[0];
         pt[1] = j*spacing[1]+vtk_origin[1];
         for (ni = 0; ni < numImages; ni++) {
            value = listImg[ni]->GetPointData()->GetScalars()->GetTuple1(p);
            yt[s][0] = value;
            xt[s][0] = 1;xt[s][1] = pt[0];xt[s][2] = pt[1];
            xt[s][3] = pt[0]*pt[0];
            xt[s][4] = pt[1]*pt[1];
            xt[s][5] = pt[0]*pt[1];
            s++;  
         } 
       }
     }
   }

   if (s != numIn*numImages) {
       // should never happen!
       mathobj->deleteArray(xt,numIn*numImages,xOrder);
       mathobj->deleteArray(yt,numIn*numImages,yOrder);
       mathobj->deleteArray(mt,xOrder,yOrder);
       delete mathobj;
       delete [] voxs;
       return CV_ERROR;
   }

   if ( (mathobj->fitLeastSquares(s,xt,xOrder,yt,yOrder,mt)) == CV_ERROR) {
      mathobj->deleteArray(xt,numIn*numImages,xOrder);
      mathobj->deleteArray(yt,numIn*numImages,yOrder);
      mathobj->deleteArray(mt,xOrder,yOrder);
      delete mathobj;
      delete [] voxs;
      return CV_ERROR;
   }

   // set results 
   results[0]=0;results[1]=0;results[2]=0;
   results[3]=0;results[4]=0;results[5]=0;
   for (i = 0; i < xOrder; i++) {
       results[i] = mt[i][0];
   }

   // clean up
   mathobj->deleteArray(xt,numIn*numImages,xOrder);
   mathobj->deleteArray(yt,numIn*numImages,yOrder);
   mathobj->deleteArray(mt,xOrder,yOrder);
   delete mathobj;
   delete [] voxs;

   return CV_OK;

}

int img_calcCorrectionEqnAuto(int numRegions,vtkPolyData **listPd,
                          int numImages,vtkStructuredPoints **listImg,
                          int order, double sdev_limit_factor,double results[],
                          vtkStructuredPoints **maskImg) {

   int i;

   // check the range on the order of the requested polynomial
   if (order < 0 || order > 2) {
       fprintf(stderr,"ERROR:  Order of $order not permitted.\n");
       return CV_ERROR;
   }
   if (numImages <= 0) {
       fprintf(stderr,"ERROR: invalid value of numRegions or numImages.\n");
       return CV_ERROR;
   }


   // assuming all of the image slices are in the same exact
   // location, lets get the extent and spacing info from the first
   vtkFloatingPointType spacing[3];
   vtkFloatingPointType vtk_origin[3];
   int dimensions[3];
   int extent[6];

   listImg[0]->GetSpacing(spacing);
   listImg[0]->GetExtent(extent);
   listImg[0]->GetOrigin(vtk_origin);
   listImg[0]->GetDimensions(dimensions);

   // debug
   fprintf(stdout,"spacing: %f %f %f\n",spacing[0],spacing[1],spacing[2]);
   fprintf(stdout,"extent: %i %i %i %i %i %i\n",extent[0],extent[1],extent[2],
           extent[3],extent[4],extent[5]);
   fprintf(stdout,"vtk_origin: %f %f %f\n",vtk_origin[0],vtk_origin[1],
           vtk_origin[2]);
   fprintf(stdout,"dimensions: %i %i %i\n",dimensions[0],dimensions[1],
           dimensions[2]);

   int maxPixNum = dimensions[0]*dimensions[1];
   short int *voxs = new short int [maxPixNum];

   //
   // if numRegions == 0, we use the entire slice.
   //

   double pt[2];
   int n,j;
   int numIn = 0;
 
   if (numRegions != 0) {

     // to speed things up, we first find the bounding box of each
     // region and convert these into voxel indices.  Only then will
     // we do the point classification to determine the voxels inside
     // of the region.

     int *imin = new int[numRegions];
     int *imax = new int[numRegions];
     int *jmin = new int[numRegions];
     int *jmax = new int[numRegions];

     vtkFloatingPointType bbox[6];

     for (i = 0; i < numRegions; i++) {
        // find bbox
        listPd[i]->GetPoints()->ComputeBounds();
        listPd[i]->GetPoints()->GetBounds(bbox);
        imin[i] = int((bbox[0] - vtk_origin[0])/spacing[0]);
        imax[i] = int((bbox[1] - vtk_origin[0])/spacing[0]);
        jmin[i] = int((bbox[2] - vtk_origin[1])/spacing[1]);
        jmax[i] = int((bbox[3] - vtk_origin[1])/spacing[1]);
        if (imin[i] < extent[0] || imax[i] > extent[1] ||
  	    jmin[i] < extent[2] || jmax[i] > extent[3]) {
            fprintf(stderr,"ERROR: region %i bounding box not completely inside of domain.\n",i);
            delete [] imin;delete [] imax;delete [] jmin;delete [] jmax;
            delete voxs;
            return CV_ERROR;
        }
     }

     // now do point a point classification on the polygon
     // to find voxels to be used in calculating equation

     for (i = 0; i < maxPixNum; i++) {
       voxs[i] = 0;
     }

     cvPolyData *pd;

     for (n = 0; n < numRegions; n++) {
       pd = new cvPolyData(listPd[n]);
       // dummy call to initialize polygon in ptInPoly
       pt[0]=0;pt[1]=0;
       int result = 0;
       sys_geom_PtInPoly(pd,pt,0,&result);
       for (i = imin[n];i <= imax[n]; i++) {
 	 for (j = jmin[n];j <= jmax[n];j++) {
            pt[0] = i*spacing[0]+vtk_origin[0];
            pt[1] = j*spacing[1]+vtk_origin[1];
            if (sys_geom_PtInPoly(pd,pt,1,&result) == CV_ERROR) {
                delete [] voxs;
                delete [] imin;delete [] imax;delete [] jmin;delete [] jmax;
                return CV_ERROR;
            }
            if (result == 1) {
              int pixel = i + j * dimensions[0];
              // only count a pixel once, it may have been in a 
              // previous region
              if (voxs[pixel] == 0) {
                voxs[pixel] = 1;
                numIn++;
              }
	    }
	  }  // j
        }  // i
        delete pd;
     }  // n

     // done with the bounding boxes
     delete [] imin;delete [] imax;delete [] jmin;delete [] jmax;

   } else {

       // all pixels in image are included in test region
     for (i = 0; i < maxPixNum; i++) {
       voxs[i] = 1;
     }
     numIn = maxPixNum;

   }

   // debugging
   fprintf(stdout,"found %i pixels inside of test regions.\n",numIn);

   //
   // detect regions of interest
   //

   int ni;
   int p;
   vtkFloatingPointType value;
   int s = 0;

   double *mean = new double [maxPixNum];
   double *sdev = new double [maxPixNum];
   short int *mask = new short int [maxPixNum];

   for (i = 0; i < maxPixNum; i++) {
       mean[i] = 0.0;
       sdev[i] = 0.0;
       mask[i] = 0;
   }

   // calculate each pixel mean

   for (p = 0; p < maxPixNum; p++) {
     if (voxs[p] == 1) {
       // figure out i,j indices for pixel
       i =  p%dimensions[0];
       j =  p/dimensions[1];
       //pt[0] = i*spacing[0]+vtk_origin[0];
       //pt[1] = j*spacing[1]+vtk_origin[1];
       for (ni = 0; ni < numImages; ni++) {
         value = listImg[ni]->GetPointData()->GetScalars()->GetTuple1(p);
         mean[p] += value;
         s++;
       }
       mean[p] = mean[p] / numImages; 
     }
   }

   if (s != numIn*numImages) {
       // should never happen!
       delete [] voxs;
       delete [] mean;
       delete [] sdev;
       delete [] mask;
       return CV_ERROR;
   }

   s = 0;
   int numMaskPix = 0;
   double minSD =  9999999.99;
   double maxSD = -9999999.99;
   double avgSD = 0.0;

   // calculate the standard deviation of each pixel and compare to a
   // user specified cutoff
   for (p = 0; p < maxPixNum; p++) {
     if (voxs[p] == 1) {
       // figure out i,j indices for pixel
       i =  p%dimensions[0];
       j =  p/dimensions[1];
       //pt[0] = i*spacing[0]+vtk_origin[0];
       //pt[1] = j*spacing[1]+vtk_origin[1];
       sdev[p] = 0.0;
       for (ni = 0; ni < numImages; ni++) {
         value = listImg[ni]->GetPointData()->GetScalars()->GetTuple1(p);
         sdev[p] += (mean[p] - value)*(mean[p] - value);
         s++;
       }
       sdev[p] = sqrt (sdev[p] / numImages);
       avgSD += sdev[p];
       if (sdev[p] > maxSD) maxSD = sdev[p];
       if (sdev[p] < minSD) minSD = sdev[p];
     }
   }
   avgSD = avgSD/numIn;

   // create a histogram

   int no_int = 40;
   double* limits = new double [no_int + 1];
   int *dN = new int [no_int + 1];
   //limits[0] = minSD;
   limits[0] = 0.0;
   for (i = 0; i <= no_int; i++) {
       //limits[i] = minSD + (maxSD-minSD)/no_int*i;
       limits[i] = avgSD/no_int*i;
       dN[i] = 0;
   }
   limits[no_int] = maxSD;

   int overfl = 0;
   int underfl = 0;

   // calculate the standard deviation of each pixel and compare to a
   // user specified cutoff
   for (p = 0; p < maxPixNum; p++) {
     if (voxs[p] == 1) {
	for(j=0; j<=no_int; j++){
			if (limits[j]>sdev[p]) break;
		}
		if (j==0) {
			underfl++;
		} else if (sdev[i]>limits[no_int]){
			overfl++;
		} else {
			dN[j-1]++;
		}
     }
   }

   if (underfl){
     fprintf(stderr, "warning: %d values were smaller ", underfl);
     fprintf(stderr, "than the lower limit.\n");
   }
   if (overfl){
     fprintf(stderr, "warning: %d values were larger ", overfl);
     fprintf(stderr, "than the upper limit.\n");
   }
   fprintf(stdout,"\n HISTOGRAM \n");
   int numItems = 0;
   for(j=0; j<no_int; j++){
       numItems += dN[j];
   }

   int curTot = 0;
   int foundIt = 0;
   double stdThr = 0.0;

   for (j = 0; j < no_int; j++) {
     curTot += dN[j];
     fprintf(stdout,"%f %f %i %f\n", limits[j],limits[j+1], dN[j],1.0*curTot/numItems);
     if (foundIt == 0 && (curTot > sdev_limit_factor*numItems)) {
         stdThr = limits[j+1];
         foundIt = 1;
     }
   }
   fprintf(stdout,"pixels with a std dev below %f are static.\n",stdThr);

   for (p = 0; p < maxPixNum; p++) {
     if (voxs[p] == 1) {
       if (sdev[p] < stdThr) {
           mask[p] = 1;
           numMaskPix++;
       }
     }
   }

   delete [] voxs;
   delete [] mean;
   delete [] sdev;

   // debugging
   fprintf(stdout,"found %i pixels in mask.\n",numMaskPix);

   // depending on the requested order set up the matrices
   // for the least squares fit

   cvMath *mathobj = new cvMath();

   int xOrder;
   int yOrder = 1;

   if (order == 0) {
       xOrder = 1;
   } else if (order == 1) {
       xOrder = 3;
   } else if (order == 2) {
       xOrder = 6;
   } else {
       // should never get here
       delete [] mask;
       delete mathobj;
       return CV_ERROR;
   }

   double **xt = mathobj->createArray(numMaskPix*numImages,xOrder);
   double **yt = mathobj->createArray(numMaskPix*numImages,yOrder);
   double **mt = mathobj->createArray(xOrder,yOrder);

   s = 0;

   // does a zeroth order fit just equate to averaging the values?

   if (order == 0) {
     for (p = 0; p < maxPixNum; p++) {
       if (mask[p] == 1) {
         // figure out i,j indices for pixel
         i =  p%dimensions[0];
         j =  p/dimensions[1];
         pt[0] = i*spacing[0]+vtk_origin[0];
         pt[1] = j*spacing[1]+vtk_origin[1];
         for (ni = 0; ni < numImages; ni++) {
            value = listImg[ni]->GetPointData()->GetScalars()->GetTuple1(p);
            yt[s][0] = value;
            xt[s][0] = 1;
            s++;
         }
       } 
     }
   } else if (order == 1) {
     for (p = 0; p < maxPixNum; p++) {
       if (mask[p] == 1) {
         // figure out i,j indices for pixel
         i =  p%dimensions[0];
         j =  p/dimensions[1];
         pt[0] = i*spacing[0]+vtk_origin[0];
         pt[1] = j*spacing[1]+vtk_origin[1];
         for (ni = 0; ni < numImages; ni++) {
            value = listImg[ni]->GetPointData()->GetScalars()->GetTuple1(p);
            yt[s][0] = value;
            xt[s][0] = 1;xt[s][1] = pt[0];xt[s][2] = pt[1];
            s++;
         } 
       }
     }
   } else if (order == 2) {
     for (p = 0; p < maxPixNum; p++) {
       if (mask[p] == 1) {
         // figure out i,j indices for pixel
         i =  p%dimensions[0];
         j =  p/dimensions[1];
         pt[0] = i*spacing[0]+vtk_origin[0];
         pt[1] = j*spacing[1]+vtk_origin[1];
         for (ni = 0; ni < numImages; ni++) {
            value = listImg[ni]->GetPointData()->GetScalars()->GetTuple1(p);
            yt[s][0] = value;
            xt[s][0] = 1;xt[s][1] = pt[0];xt[s][2] = pt[1];
            xt[s][3] = pt[0]*pt[0];
            xt[s][4] = pt[1]*pt[1];
            xt[s][5] = pt[0]*pt[1];
            s++;  
         } 
       }
     }
   }

   if (s != numMaskPix*numImages) {
       // should never happen!
       fprintf(stdout,"s (%i) not equal to numMaskPix*numImages (%i %i)\n",s,numMaskPix,numImages);
       mathobj->deleteArray(xt,numMaskPix*numImages,xOrder);
       mathobj->deleteArray(yt,numMaskPix*numImages,yOrder);
       mathobj->deleteArray(mt,xOrder,yOrder);
       delete mathobj;
       delete [] mask;
       return CV_ERROR;
   }

   if ( (mathobj->fitLeastSquares(s,xt,xOrder,yt,yOrder,mt)) == CV_ERROR) {
      mathobj->deleteArray(xt,numMaskPix*numImages,xOrder);
      mathobj->deleteArray(yt,numMaskPix*numImages,yOrder);
      mathobj->deleteArray(mt,xOrder,yOrder);
      delete mathobj;
      delete [] mask;
      return CV_ERROR;
   }

   // set results 
   results[0]=0;results[1]=0;results[2]=0;
   results[3]=0;results[4]=0;results[5]=0;
   for (i = 0; i < xOrder; i++) {
       results[i] = mt[i][0];
   }

   // clean up
   mathobj->deleteArray(xt,numMaskPix*numImages,xOrder);
   mathobj->deleteArray(yt,numMaskPix*numImages,yOrder);
   mathobj->deleteArray(mt,xOrder,yOrder);
   delete mathobj;

   
   // return the mask for vis
   vtkShortArray* maskvtkPts = vtkShortArray::New();
   maskvtkPts->SetNumberOfComponents(1);
   maskvtkPts->Allocate(maxPixNum,100);
   for (i = 0; i < maxPixNum; i++) {
       maskvtkPts->InsertNextTuple1(mask[i]);
   }

   vtkStructuredPoints *rtnImg = vtkStructuredPoints::New();
   rtnImg->CopyStructure(listImg[0]);
   // not in vtk 6.0.0 rtnImg->SetScalarTypeToShort();
   rtnImg->GetPointData()->SetScalars(maskvtkPts);

   maskvtkPts->Delete();

   *maskImg = rtnImg;

   delete [] mask;

   return CV_OK;

}

