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

#ifndef __CVREAD_HEADER_H
#define __CVREAD_HEADER_H

#include "SimVascular.h"
#include "svImageExports.h" // For exports

SV_EXPORT_IMAGE int mrRead_Header (char *filename, float *vdims_x, float *vdims_y,
                        int *dim_x, int *dim_y, int *file_hdr_size,
                        float ul[], float ur[], float br[],
                        int *rtnvenc, float *rtnvencscale,
                        int *rtnvas_collapse, float *rtnuser2,
		        float *rtnuser5, float *rtnuser6, float *rtnuser7,
                        float *rtnuser8, float *rtnuser9,
                        float *rtnuser12, float *rtnuser13, float *rtnuser14,
                        char *patid, char *patname, char *psdname,
                        int *magWeightFlag, int *examNumber,
                        float nrm_RAS[],int *acquisitionTime,
                        int *heart_rate,int *im_no, int *im_seno);

#endif
