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

#include "cvDistanceMap.h"

#include <stdio.h>
#include <math.h>

#include "vtkXMLDataSetWriter.h"

cvDistanceMap::cvDistanceMap() {
    map_ = NULL;
    path_ = NULL;
    mask_ = NULL;
    useCityBlock_ = 1;

    // create index table for 26-connectivity neighborhood
    int num = 0,i,j,k;
    for (i = -1; i < 2; i++) {
      for (j = -1; j < 2; j++) {
        for (k = -1; k < 2; k++) {
          if (i == 0 && j == 0 && k == 0) {
          } else {
            b_[0][num] = i; b_[1][num] = j;b_[2][num] = k;
            num++;
          }
        }
      }
    }
    if (num != 26) {
      fprintf(stderr,"ERROR:  need 26 neighbors and found %i\n",num); 
      exit(-1);
    }
}

cvDistanceMap::~cvDistanceMap() {
    if (map_ != NULL) {
        map_->Delete();
    }
    if (path_ != NULL) {
        path_->Delete();
    }
}

int cvDistanceMap::createDistanceMap (vtkStructuredPoints *vtksp,
                                        vtkFloatingPointType thrval, 
                                        int start[3]) {

    // Code to create a PathPlan binary segmentation from a VTK
    // structured points set.

    if (map_ != NULL) {
        map_ -> Delete();
        map_ = NULL;
    }

    start_[0] = start[0];
    start_[1] = start[1];
    start_[2] = start[2];

    int s;

    DISTANCEMAPVTKTYPE *mapScalars = DISTANCEMAPVTKTYPE::New();
    mapScalars->SetNumberOfComponents(1);
    mapScalars->Allocate(1000,1000);
    mapScalars->Initialize();

    vtkStructuredPoints *mapsp;
    mapsp = vtkStructuredPoints::New();
//    mapsp = vtkStructuredPoints::New();
    mapsp->CopyStructure(vtksp);
    // not-in-vtk-6.0    mapsp->CopyInformation(vtksp);
    mapsp->GetPointData()->SetScalars(mapScalars);
    vtkFloatingPointType origin[3];
    vtkFloatingPointType spacing[3];
    vtksp->GetOrigin(origin);
    mapsp->SetOrigin(origin);
    vtksp->GetSpacing(spacing);
    mapsp->SetSpacing(spacing);

    int n;
    distanceMapType Dq;

    vtkFloatingPointType pixDimsF[3];
    vtkFloatingPointType originF[3];

    vtksp->GetDimensions( imgDims_ );
    vtksp->GetSpacing( pixDimsF );
    vtksp->GetOrigin( originF );

    fprintf(stdout,"dims: %i %i %i\n", imgDims_[0],imgDims_[1],imgDims_[2]);

    vtkDataArray *vScalars = vtksp->GetPointData()->GetScalars();
    
    int buckindex, bloop;

    // count the number of non-zero
    int nonZeroPixels = 0;
    int totalNumPixels = imgDims_[0]*imgDims_[1]*imgDims_[2];
 
    for (s = 0; s < totalNumPixels; s++) {
      if ((int)(vScalars->GetTuple1(s)) >= thrval) {
        nonZeroPixels++;
      }
    }

    fprintf(stdout,"pixels in threshold: %i\n",nonZeroPixels);

    mapScalars->SetNumberOfTuples(totalNumPixels);

    // add all interior pixels into the first bucket
    for (s = 0; s < totalNumPixels;s++) {
      if ((int)(vScalars->GetTuple1(s)) >= thrval) {
          //bucket[0]->InsertNextId(s);
         // hopefully big number!
         mapScalars->SetTuple1(s,MAX_DISTANCE_VAL);
      } else {
          mapScalars->SetTuple1(s,-1);
      }
    }

    // wastefully allocation
    int bucketlookup[5];
    for (int i = 0; i < 5; i++) {
      bucketlookup[i] = i;
    }

    vtkIdList *bucket[4];
    bucket[0] = vtkIdList::New();bucket[0]->Allocate(100,100);
    bucket[1] = vtkIdList::New();bucket[1]->Allocate(100,100);
    bucket[2] = vtkIdList::New();bucket[2]->Allocate(100,100);

    bucketlookup[0] = 0;
    bucketlookup[1] = 1;
    bucketlookup[2] = 2;

    // loop until all the buckets are empty
    int loopAllBuckets = 0;
    int p = vtksp->ComputePointId(start);
    int q = 0;

    // id point is a zero distance from itself
    mapScalars->SetTuple1(p,0);

//    bucket[0]->InsertNextId(p);
//    bucket[0]->DeleteId(p);

    while ((loopAllBuckets < nonZeroPixels) && (p != -1)) {

        if (useCityBlock_ == 0) {
          get26ConnectivityNeighbors(p);
        } else {
          getCityBlockNeighbors(p);
        }

        // loop over neighbors_s and calc distance
        for (n = 0; n < numNeighbors_; n++) {
            q = neighbors_[n]; 

            if (q < 0) continue;
            // check distance value
            //fprintf(stdout,"mapScalars [%i] : %i\n",q,(int)mapScalars->GetTuple1(q)); 
            if ((int)(mapScalars->GetTuple1(q)) >= 0) {
                distanceMapType pval = (distanceMapType)(mapScalars->GetTuple1(p));
                distanceMapType qval = (distanceMapType)(mapScalars->GetTuple1(q)); 
                Dq = pval + 1;
                //fprintf(stdout,"%i %i %i\n",p,q,Dq);
                if (Dq < qval) {
                    if (qval != MAX_DISTANCE_VAL) {
                      buckindex = -1;
                      for (bloop = 0; bloop < 3; bloop++) {
                        if (qval == bucketlookup[bloop]) {
                          buckindex = bloop;
                          break;
                          }
                      }
                      if (buckindex < 0) {
                        fprintf(stdout,"invalid bucket (Dq = %i, qval = %i)\n",Dq,qval);
                        exit(-1);
                      }
                      bucket[buckindex] -> DeleteId(q);
                    }
                    mapScalars->SetTuple1(q,Dq);
                    buckindex = -1;
                    for (bloop = 0; bloop < 3; bloop++) {
                      if (Dq == bucketlookup[bloop]) {
                          buckindex = bloop;
                          break;
                      }
                    }
                    if (buckindex < 0) {
                      fprintf(stdout,"invalid bucket (Dq = %i, q = %i)\n",Dq,q);
                      exit(-1);
                    }
                    bucket[buckindex]->InsertNextId(q);
                }
            } 
        }

      p = -1;

      while (loopAllBuckets < nonZeroPixels) {
        buckindex = -1;
        for (bloop = 0; bloop < 3; bloop++) {
          if (loopAllBuckets == bucketlookup[bloop]) {
             buckindex = bloop;
             break;
          }
        }
        if (buckindex < 0) {
          fprintf(stdout,"invalid bucket (loopAllBuckets %i)\n",loopAllBuckets);
                        exit(-1);
        }
        if (bucket[buckindex]->GetNumberOfIds() == 0) {
            //buckets[buckindex+3] = buckets[buckindex];
          bucketlookup[buckindex] = loopAllBuckets+3;
            //buckets[buckindex] = NULL;
          loopAllBuckets++;
        } else {
            //if (buckets[loopAllBuckets] == NULL) {
            //  fprintf(stdout,"invalid bucket (loopAllBuckets %i)\n",loopAllBuckets);
            //  exit(-1);
            //}
            //if (buckets[loopAllBuckets]->GetNumberOfIds() == 0) {
            //    fprintf(stdout,"error, zero entries in bucket %i\n",loopAllBuckets);
            //    exit(-1);
            //}
            p = bucket[buckindex]->GetId(0);
            bucket[buckindex]->DeleteId(p);
            break;
        }
      }

    }

//    vtkXMLDataSetWriter *foo = vtkXMLDataSetWriter::New();
//    foo->SetInputDataObject(mapsp);
//    foo->SetFileName("debug.vti");
//    foo->Write();

    map_ = mapsp;

    //delete [] buckets;
    bucket[0]->Delete();
    bucket[1]->Delete();
    bucket[2]->Delete();

    return CV_OK;

}

vtkStructuredPoints* cvDistanceMap::getDistanceMap() {
    return map_;
}

vtkPolyData* cvDistanceMap::getPathOld(int stop[3]) {

    if (map_ == NULL) {
        return NULL;
    }

    path_ = NULL;

    stop_[0] = stop[0];
    stop_[1] = stop[1];
    stop_[2] = stop[2];

    map_->GetDimensions( imgDims_ );

    vtkDataArray *mapScalars = map_->GetPointData()->GetScalars();

    vtkPoints *mypts   =  vtkPoints::New();
    mypts->Allocate(100,100);
    vtkCellArray  *mylines =  vtkCellArray::New();
    mylines->Allocate(100,100);
    mylines->InitTraversal();
    vtkPolyData *mypd  =  vtkPolyData::New();
  
    mypd->SetPoints(mypts);
    mypd->SetLines(mylines);

    int p = map_->ComputePointId(stop);
    int q = 0;

    double x[3];

    map_ -> GetPoint(p,x);
    mypts->InsertNextPoint(x);
/*
    // make sure starting point has zero value
    int pval =  (int)(mapScalars->GetTuple1(p));
    if (pval != 0) {
        mypts->Delete();
        mylines->Delete();
        mypd->Delete();
        return NULL;
    }
*/

    int minq = MAX_DISTANCE_VAL;
    
    vtkIdList *line = vtkIdList::New();
    line->Allocate(3,3);

    while (minq > 0) {

        // for p find the city block neighbors_s

        if (useCityBlock_ == 0) {
          get26ConnectivityNeighbors(p);
        } else {
          getCityBlockNeighbors(p);
        }

        // loop over neighbors and calc distance
        p = -1;
        for (int n = 0; n < numNeighbors_; n++) {
            q = neighbors_[n];
            if (q < 0) continue;
            int qval = (int)(mapScalars->GetTuple1(q)); 
            if (qval >= 0 && qval <= minq) {
                minq = qval;
                p = q;
            }
        }

        if (p < 0) {
          fprintf(stdout,"ERROR:  could not find less than equal path to next pt!\n");
          mypts->Delete();
          mylines->Delete();
          mypd->Delete();
          line->Delete();
          return NULL;
        }

        map_ -> GetPoint(p,x);
        mypts->InsertNextPoint(x);
        int numpts = mypts->GetNumberOfPoints();
        if ( numpts > 1) {
            line->Initialize();
            line->InsertNextId(numpts-2);
            line->InsertNextId(numpts-1);
            mypd->InsertNextCell(VTK_LINE,line);
        }
    }

    path_ = mypd;

    return path_;

}


vtkPolyData* cvDistanceMap::getPath(int stop[3], int minqstop) {

    int i,n;
    
    if (map_ == NULL) {
        return NULL;
    }

    path_ = NULL;

    stop_[0] = stop[0];
    stop_[1] = stop[1];
    stop_[2] = stop[2];

    map_->GetDimensions( imgDims_ );

    vtkDataArray *mapScalars = map_->GetPointData()->GetScalars();

    int p = map_->ComputePointId(stop);
    int q = 0;

    distanceMapType minq = MAX_DISTANCE_VAL;

    vtkIdList *curpath = vtkIdList::New();
    curpath->Allocate(100,100);

    while (minq > minqstop) {

        if (useCityBlock_ == 0) {
          get26ConnectivityNeighbors(p);
        } else {
          getCityBlockNeighbors(p);
        }

        // loop over neighbors and calc distance
        p = -1;
        for (n = 0; n < numNeighbors_; n++) {
            q = neighbors_[n];
            if (q < 0) continue;
            distanceMapType qval = (distanceMapType)(mapScalars->GetTuple1(q)); 
            if (qval >= 0 && qval <= minq) {
                minq = qval;
                p = q;
            }
        }

        if (p < 0) {
          fprintf(stdout,"ERROR:  could not find less than equal path to next pt!\n");
          curpath->Delete();
          return NULL;
        }
  
        curpath->InsertNextId(p);

    }

    // create return vtk polydata

    vtkPoints *mypts   =  vtkPoints::New();
    mypts->Allocate(100,100);
    vtkCellArray  *mylines =  vtkCellArray::New();
    mylines->Allocate(100,100);
    mylines->InitTraversal();
    vtkPolyData *mypd  =  vtkPolyData::New();

    vtkIdList *line = vtkIdList::New();
    line->Allocate(3,3);
  
    mypd->SetPoints(mypts);
    mypd->SetLines(mylines);

    double x[3];

    for (i = curpath->GetNumberOfIds() - 1; i >= 0; i--) {
        p = curpath->GetId(i);
        map_ -> GetPoint(p,x);
        mypts->InsertNextPoint(x);
        int numpts = mypts->GetNumberOfPoints();
        if ( numpts > 1) {
            line->Initialize();
            line->InsertNextId(numpts-2);
            line->InsertNextId(numpts-1);
            mypd->InsertNextCell(VTK_LINE,line);
        }
    }

    path_ = mypd;

    curpath->Delete();

    return path_;

}


vtkPolyData* cvDistanceMap::getPathByThinning(int stop[3],  int minqstop, int maxIterNum) {

  int i,n;

  if (map_ == NULL) {
    return NULL;
  }

  path_ = NULL;

  stop_[0] = stop[0];
  stop_[1] = stop[1];
  stop_[2] = stop[2];

  map_->GetDimensions( imgDims_ );

  vtkDataArray *mapScalars = map_->GetPointData()->GetScalars();

  int p = map_->ComputePointId(stop);
  int q = 0;
  int maskval;

  distanceMapType minq;
  vtkDataArray *maskScalars = NULL;

  // only create the initial mask if we are going to use thinning
  if (maxIterNum > 1) {
      createInitMask();
      maskScalars = mask_->GetPointData()->GetScalars();
  }

  int curIterNum = 0;
  vtkIdList *prevpath = NULL;
  vtkIdList *curpath = NULL;

  while (curIterNum < maxIterNum) {

    curpath = vtkIdList::New();
    curpath->Allocate(100,100);

    int pathIncludesNewPts = 0;
    minq = MAX_DISTANCE_VAL;
    p = map_->ComputePointId(stop);
    curpath->InsertNextId(p);

    while (minq > minqstop) {

        if (useCityBlock_ == 0) {
          get26ConnectivityNeighbors(p);
        } else {
          getCityBlockNeighbors(p);
        }

        // loop over neighbors and calc distance
        p = -1;
        maskval = 1;

        for (n = 0; n < numNeighbors_; n++) {
            q = neighbors_[n];
            if (q < 0) continue;
            distanceMapType qval = (distanceMapType)(mapScalars->GetTuple1(q));
            if (curIterNum > 0) maskval = (int)(maskScalars->GetTuple1(q));
            if (qval >= 0 && qval < minq && maskval > 0) {
                minq = qval;
                p = q;
                pathIncludesNewPts++;
            }
        }

        if (p < 0) {
          if (curIterNum == 0) {
            fprintf(stdout,"ERROR:  could not find less than equal path to next pt!\n");
            curpath->Delete();
            return NULL;
          }
          // now need to use points from previous path to
          // keep going
          // check and see if an existing point is a neighbor
          // otherwise, we have a problem
          for (n = 0; n < numNeighbors_; n++) {
            q = neighbors_[n];
            if (q < 0) continue;
            distanceMapType qval = (distanceMapType)(mapScalars->GetTuple1(q));
            if (prevpath->IsId(q) >= 0) {   
              if (qval >= 0 && qval < minq) {
                minq = qval;
                p = q;
              }
            }
          }
          if (p < 0) {         
            fprintf(stdout,"ERROR:  could not find less than equal path to next pt!\n");
            curpath->Delete();
            prevpath->Delete();
            return NULL;
          }     
        }
        fprintf(stdout,"adding (%i) \n",p);
        curpath->InsertNextId(p);
    }
    fprintf(stdout,"length of curpath (%i)\n",curpath->GetNumberOfIds());

    curIterNum++;
    if (curIterNum == maxIterNum) break;
    fprintf(stdout,"number of new points in path (%i)\n",pathIncludesNewPts);
    if (pathIncludesNewPts == 0) break;

    // do thinning
    int numPixelsRemoved = 0;
    thinMask(&numPixelsRemoved);
    fprintf(stderr,"pass [%i]: num of pixels removed = %i\n",curIterNum,numPixelsRemoved);
 
    if (numPixelsRemoved == 0) break;

    if (prevpath != NULL) prevpath->Delete();
    prevpath = curpath;
 
  }

  // create return vtk polydata

  vtkPoints *mypts   =  vtkPoints::New();
  mypts->Allocate(100,100);
  vtkCellArray  *mylines =  vtkCellArray::New();
  mylines->Allocate(100,100);
  mylines->InitTraversal();
  vtkPolyData *mypd  =  vtkPolyData::New();
  vtkIdList *line = vtkIdList::New();
  line->Allocate(3,3);

  mypd->SetPoints(mypts);
  mypd->SetLines(mylines);

  double x[3];

  for (i = curpath->GetNumberOfIds() - 1; i >= 0; i--) {
    p = curpath->GetId(i);
    map_ -> GetPoint(p,x);
    mypts->InsertNextPoint(x);
    int numpts = mypts->GetNumberOfPoints();
    if ( numpts > 1) {
      line->Initialize();
      line->InsertNextId(numpts-2);
      line->InsertNextId(numpts-1);
      mypd->InsertNextCell(VTK_LINE,line);
    }
  }

  path_ = mypd;
  curpath->Delete();
  if (maxIterNum > 1) prevpath->Delete();
 
  return path_;

}


void cvDistanceMap::setDistanceMap(vtkStructuredPoints *sp) {
    map_ = sp;
} 

void cvDistanceMap::setUseCityBlockDistance() {
    useCityBlock_ = 1;
} 

void cvDistanceMap::setUse26ConnectivityDistance() {
    useCityBlock_ = 0;
} 


int cvDistanceMap::getCityBlockNeighbors(int p) {

  int i,j,k;

  // convert id to ijk
  k = p / (imgDims_[0]*imgDims_[1]);
  j = (p - k * imgDims_[0]*imgDims_[1]) / imgDims_[0];
  i = p - k * imgDims_[0]*imgDims_[1] - j*imgDims_[0];
  //fprintf(stdout,"ijk [%i]: %i %i %i %i %i\n",p,i,j,k,(int)mapScalars->GetTuple1(p),(int)vScalars->GetTuple1(p));

  if (i - 1 < 0) {
    neighbors_[0] = -1;
  } else {
    neighbors_[0] =  (i-1)+imgDims_[0]*j+k*imgDims_[0]*imgDims_[1];
  }

  if (i + 1 >= imgDims_[0]) {
    neighbors_[1] = -1;
  } else {
    neighbors_[1] =  (i+1)+imgDims_[0]*j+k*imgDims_[0]*imgDims_[1];
  }

  if (j - 1 < 0) {
    neighbors_[2] = -1;
  } else {
    neighbors_[2] =  i+imgDims_[0]*(j-1)+k*imgDims_[0]*imgDims_[1];
  }

  if (j + 1 >= imgDims_[1]) {
    neighbors_[3] = -1;
  } else {
    neighbors_[3] =  i+imgDims_[0]*(j+1)+k*imgDims_[0]*imgDims_[1];
  }

  if (k - 1 < 0) {
    neighbors_[4] = -1;
  } else {
    neighbors_[4] =  i+imgDims_[0]*j+(k-1)*imgDims_[0]*imgDims_[1];
  }

  if (k + 1 >= imgDims_[2]) {
    neighbors_[5] = -1;
  } else {
    neighbors_[5] =  i+imgDims_[0]*j+(k+1)*imgDims_[0]*imgDims_[1];
  }

  numNeighbors_ = 6;

  return CV_OK;

}

int cvDistanceMap::get26ConnectivityNeighbors(int p) {

  int i,j,k;

  // convert id to ijk
  k = p / (imgDims_[0]*imgDims_[1]);
  j = (p - k * imgDims_[0]*imgDims_[1]) / imgDims_[0];
  i = p - k * imgDims_[0]*imgDims_[1] - j*imgDims_[0];
  //fprintf(stdout,"ijk [%i]: %i %i %i %i %i\n",p,i,j,k,(int)mapScalars->GetTuple1(p),(int)vScalars->GetTuple1(p));

  int n,ti,tj,tk;

  for (n = 0; n < 27; n++) {
      ti = i + b_[0][n];
      tj = j + b_[1][n];
      tk = k + b_[2][n];
      if (ti < 0 || tj <  0 || tk < 0 ||
          ti >= imgDims_[0] || tj >= imgDims_[1] || tk >= imgDims_[2]  ) {
          neighbors_[n] = -1;
      } else {
          neighbors_[n] = ti+imgDims_[0]*tj+tk*imgDims_[0]*imgDims_[1];
      }
  }

  numNeighbors_ = 26;

  return CV_OK;

}


int cvDistanceMap::createInitMask() {


    if (mask_ != NULL) {
        mask_ -> Delete();
        mask_ = NULL;
    }
   
    int s;

    vtkShortArray *maskScalars = vtkShortArray::New();
    maskScalars->SetNumberOfComponents(1);
    maskScalars->Allocate(1000,1000);
    maskScalars->Initialize();

    mask_ = vtkStructuredPoints::New();
    mask_->CopyStructure(map_);
    // note in vtk-6.0.0  mask_->CopyInformation(map_);
    mask_->GetPointData()->SetScalars(maskScalars);
    vtkFloatingPointType origin[3];
    vtkFloatingPointType spacing[3];
    map_->GetOrigin(origin);
    mask_->SetOrigin(origin);
    map_->GetSpacing(spacing);
    mask_->SetSpacing(spacing);

    mask_->GetDimensions( imgDims_ );

    vtkDataArray *mapScalars = map_->GetPointData()->GetScalars();

    // initial map contains all connected values
    int maskNum = 0;
    int numScalars = mapScalars->GetNumberOfTuples();
    for (s = 0; s < numScalars; s++) {
      distanceMapType d = (distanceMapType)(mapScalars->GetTuple1(s));
      if (d >= 0 && d != MAX_DISTANCE_VAL) {
          maskScalars->InsertNextTuple1(1);
          maskNum++;
      } else {
        maskScalars->InsertNextTuple1(0);
      } 
    }


    vtkXMLDataSetWriter *foo = vtkXMLDataSetWriter::New();
    foo->SetInputData(mask_);
    foo->SetFileName("debug.vti");
    foo->Write();

    fprintf(stdout,"number of unmasked pixels (%i)\n",maskNum);

    return CV_OK;
    
}

int cvDistanceMap::thinMask(int *numPixelsRemoved) {

    *numPixelsRemoved = 0;

    vtkDataArray *maskScalars = mask_->GetPointData()->GetScalars();

    // scan each direction, build list of 
    // indexes to remove, then parallel thin
    int i,j,k,p,s,n;

    vtkIdList *removeIds = vtkIdList::New();
    removeIds->Allocate(100,100);

    int numPixels = 0;
    int inside = 0;
    int sprev = 0;
    for (i = 0; i < imgDims_[0]; i++) {
      for (j = 0; j < imgDims_[1]; j++) {
        for (k = 0; k < imgDims_[2]; k++) {
          s =  i+imgDims_[0]*j+k*imgDims_[0]*imgDims_[1];
          p =  maskScalars->GetTuple1(s);
          if (inside == 0 && p > 0) {
            removeIds->InsertNextId(s);
            inside = 1;
          } else if (inside == 1 && p == 0) {
            inside = 0;
            removeIds->InsertNextId(sprev);
          }
          sprev = s; 
        }
        if (inside == 1) {
            removeIds->InsertNextId(s);
            inside = 0;
        }
      }
    }

    numPixels = removeIds->GetNumberOfIds();

    fprintf(stdout,"num pixels removed (%i)\n",numPixels);

    *numPixelsRemoved = numPixels;

    // mask all surface pixels
    for (n = 0; n < numPixels; n++) {
        maskScalars->SetTuple1(removeIds->GetId(n),0);
    }  

    vtkXMLDataSetWriter *foo = vtkXMLDataSetWriter::New();
    foo->SetInputData(mask_);
    foo->SetFileName("debug-thinned.vti");
    foo->Write();

    removeIds->Delete();

    return CV_OK;

}
