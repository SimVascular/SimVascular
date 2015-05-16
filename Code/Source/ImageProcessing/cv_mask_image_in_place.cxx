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

#include "cv_mask_image_in_place.h"

#include <stdio.h>
#include <math.h>

int gdscMaskImageInPlace(vtkStructuredPoints *imgsp,
                          vtkStructuredPoints *masksp,
                         double replaceVal,int notval) {


    int i;
    int numPtsImg = imgsp->GetNumberOfPoints();
    int numPtsMask = masksp->GetNumberOfPoints();
    

    vtkDataArray *mscalars = masksp->GetPointData()->GetScalars();
    vtkDataArray *iscalars = imgsp->GetPointData()->GetScalars();

    if (numPtsImg != numPtsMask) {
        fprintf(stderr,"ERROR:  number of points in image and mask must match!\n");
        return CV_ERROR;
    }

    int numBlanked = 0;

    if (notval) {
      for (i = 0; i < numPtsImg; i++) {
        if (mscalars->GetTuple1(i)) {
         iscalars->SetTuple1(i,replaceVal);
         numBlanked++;
        }
      }
    } else {
      for (i = 0; i < numPtsImg; i++) {
        if (!(mscalars->GetTuple1(i))) {
          iscalars->SetTuple1(i,replaceVal);
          numBlanked++;
        } 
      }
    }

    fprintf(stdout,"  %i pixels changed out of %i\n",numBlanked,numPtsImg);

    return CV_OK;
      
}
