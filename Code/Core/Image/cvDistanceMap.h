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

#ifndef __CVDISTANCEMAP_H
#define __CVDISTANCEMAP_H

#include "SimVascular.h"
#include "cvVTK.h"

// use ints for distance map
typedef int distanceMapType;
#define DISTANCEMAPVTKTYPE vtkIntArray
#define MAX_DISTANCE_VAL 999999999
// should be more like   2147483647 
// use shorts
//typedef short distanceMapType;
//#define DISTANCEMAPVTKTYPE vtkShortArray
//#define MAX_DISTANCE_VAL 32000

class cvDistanceMap {

  public:

    cvDistanceMap();
    ~cvDistanceMap();
    int createDistanceMap(vtkStructuredPoints *vtksp,
                          vtkFloatingPointType thrval,
                          int start[3]);
    vtkStructuredPoints* getDistanceMap();
    void setDistanceMap(vtkStructuredPoints *sp); 

    vtkPolyData* getPath(int stop[3], int minqstop);
    vtkPolyData* getPathByThinning(int stop[3], int minqstop, int maxIterNum);
    vtkPolyData* getPathOld(int stop[3]);
    
    void setUseCityBlockDistance();
    void setUse26ConnectivityDistance();
    
  private:

    int getCityBlockNeighbors(int p);
    int get26ConnectivityNeighbors(int p);
    int createInitMask();
    int thinMask(int *numPixelsRemoved);
        
    vtkStructuredPoints *map_;
    vtkStructuredPoints *mask_;
    vtkPolyData *path_;
    int start_[3];
    int stop_[3];

    int neighbors_[26];
    int numNeighbors_;
    // 26 connectivity index lookup table
    int b_[3][26]; 
    int useCityBlock_;
        
    int imgDims_[3];

};

#endif
