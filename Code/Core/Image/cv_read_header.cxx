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

#include "stdlib.h"
#include "stdio.h"

#include "cv_img_offsets.h"

#include "cvVTK.h"
#include "vtkByteSwap.h"

#include "cv_misc_utils.h"

int mrRead_unsigned_short_int(FILE *in,int offset,unsigned short int *value) {

    // set file position
    if (fseek(in, offset, SEEK_SET)) { 
      fprintf(stderr,"ERROR: Could not set seek position to %i.\n",offset);
      return CV_ERROR;
    }

    // read bytes from file    
    if (fread(value, sizeof(unsigned short int), 1, in) != 1) {
        fprintf(stderr,"ERROR: Could not read unsigned short int!\n");
        return CV_ERROR;
    }

    // correct for platform dependence
    vtkByteSwap::Swap2BE((short*)value);

    return CV_OK;

}

int mrRead_short_int(FILE *in,int offset,short int *value) {

    // set file position
    if (fseek(in, offset, SEEK_SET)) { 
      fprintf(stderr,"ERROR: Could not set seek position to %i.\n",offset);
      return CV_ERROR;
    }

    // read bytes from file    
    if (fread(value, sizeof(short int), 1, in) != 1) {
        fprintf(stderr,"ERROR: Could not read unsigned short int!\n");
        return CV_ERROR;
    }

    // correct for platform dependence
    vtkByteSwap::Swap2BE(value);

    return CV_OK;

}

int mrRead_int(FILE *in,int offset,int *value) {

    // set file position
    if (fseek(in, offset, SEEK_SET)) { 
      fprintf(stderr,"ERROR: Could not set seek position to %i.\n",offset);
      return CV_ERROR;
    }

    // read bytes from file    
    if (fread(value, sizeof(int), 1, in) != 1) {
        fprintf(stderr,"ERROR: Could not read unsigned short int!\n");
        return CV_ERROR;
    }

    // correct for platform dependence
    vtkByteSwap::Swap4BE((char*)value);

    return CV_OK;

}
int mrRead_float(FILE *in,int offset,float *value) {

    // set file position
    if (fseek(in, offset, SEEK_SET)) { 
      fprintf(stderr,"ERROR: Could not set seek position to %i.\n",offset);
      return CV_ERROR;
    }

    // read bytes from file    
    if (fread(value, sizeof(float), 1, in) != 1) {
        fprintf(stderr,"ERROR: Could not read float!\n");
        return CV_ERROR;
    }

    // correct for platform dependence
    vtkByteSwap::Swap4BE((char*)value);

    return CV_OK;
}

int mrRead_char(FILE *in,int offset,int numBytes, char *value) {

    // set file position
    if (fseek(in, offset, SEEK_SET)) { 
      fprintf(stderr,"ERROR: Could not set seek position to %i.\n",offset);
      return CV_ERROR;
    }

    // read bytes from file    
    if (fread(value, sizeof(char), numBytes, in) != numBytes) {
        fprintf(stderr,"ERROR: Could not read float!\n");
        return CV_ERROR;
    }

    // no platform dependence for characters

    return CV_OK;
}

int mrRead_Header (char *filename, float *vdims_x, float *vdims_y,
                        int *dim_x, int *dim_y, int *file_hdr_size,
                        float ul[], float ur[], float br[],
                        int *rtnvenc, float *rtnvencscale,
		        int *rtnvas_collapse, float *rtnuser2, 
		        float *rtnuser5, float *rtnuser6, 
		        float *rtnuser7,float *rtnuser8,float *rtnuser9,
		        float *rtnuser12,float *rtnuser13,float *rtnuser14,
                        char *patid, char *patname, char *psdname,
                        int *magWeightFlag, int *examNumber,
                        float nrm_RAS[],int *acquisitionTime,
                        int *heart_rate,
                        int *rtnim_no, int *rtnim_seno) {

    FILE *in;

    in = fopen(filename,"rb");
    if ( in  == NULL) {
        fprintf(stderr,"ERROR:  Could not open file %s!\n",filename);
        return CV_ERROR;
    }

    // check magic number
    int img_magic  = 0;
    mrRead_int(in, OFFSET_MAGIC_NUM , &img_magic);
    if (img_magic != 1229801286) {
        fprintf(stdout,"ERROR:  This file does not appear to be in Genesis format!\n");
        fclose(in);
        return CV_ERROR;
    }

    // get offsets

    int hdr_length = 0;
    int SUoffset   = 0;
    int EXoffset   = 0;
    int SEoffset   = 0;
    int MRoffset   = 0;

    mrRead_int(in, OFFSET_HEADER_LEN , &hdr_length);
    mrRead_int(in, OFFSET_SUITE, &SUoffset);
    mrRead_int(in, OFFSET_EXAM, &EXoffset);
    mrRead_int(in, OFFSET_SERIES, &SEoffset);
    mrRead_int(in, OFFSET_IMAGE, &MRoffset);
   
    fprintf(stdout,"\n\nHeader offsets\n\n");
    fprintf(stdout,"SUoffset      : %05x\n",SUoffset);
    fprintf(stdout,"EXoffset      : %05x\n",EXoffset);
    fprintf(stdout,"SEoffset      : %05x\n",SEoffset);
    fprintf(stdout,"MRoffset      : %05x\n",MRoffset);
    fprintf(stdout,"Header length : %05x\n",hdr_length);

    // check to make sure MR was used
    char ex_typ[3];
    mrRead_char(in , EXoffset + EX_TYP, 3, ex_typ);
    if (ex_typ[0] != 'M' || ex_typ[1] != 'R') {
      fprintf(stdout,"Exam type (%s) apparently not MR!\n",ex_typ);
      fclose(in);
      return CV_ERROR;
    }

    // read in a couple of params
    unsigned short int im_exno = 0;
    float              pixelSize_x = 0, pixelSize_y = 0;
    short int          imatrix_X, imatrix_Y = 0;
    int                img_width = 0, img_height = 0;
    float              tlhc_R = 0, tlhc_A = 0, tlhc_S = 0;
    float              trhc_R = 0, trhc_A = 0, trhc_S = 0;
    float              brhc_R = 0, brhc_A = 0, brhc_S = 0;
    float              norm_R = 0, norm_A = 0, norm_S = 0; 
    float              slthick = 0;
    short int          venc = 0;
    float              vencscale = 0;
    short int          vas_collapse = 0;
    short int          im_seno = 0;
    short int          im_no = 0;
    int                im_actual_dt = 0;
    float              user2 = 0;
    float              user5 = 0;
    float              user6 = 0;
    float              user7 = 0;
    float              user8 = 0;
    float              user9 = 0;
    float              user12 = 0;
    float              user13 = 0;
    float              user14 = 0;
    short int          vasflags = 0;
    short int          hrtrate = 0;
    float              MR_dim_X = 0;
    float              MR_dim_Y = 0;

    mrRead_char(in, EXoffset + EX_PATID, 13, patid);
    mrRead_char(in, EXoffset + EX_PATNAME, 25, patname);  
    mrRead_unsigned_short_int(in , MRoffset + EX_NO, &im_exno);
    mrRead_float(in , MRoffset + IM_PIXSIZE_X , &pixelSize_x);
    mrRead_float(in , MRoffset + IM_PIXSIZE_Y , &pixelSize_y);
    mrRead_float(in , MRoffset + IM_SLTHICK , &slthick);

    mrRead_float(in , MRoffset + IM_TLHC_R , &tlhc_R);
    mrRead_float(in , MRoffset + IM_TLHC_A , &tlhc_A);
    mrRead_float(in , MRoffset + IM_TLHC_S , &tlhc_S);

    mrRead_float(in , MRoffset + IM_TRHC_R , &trhc_R);
    mrRead_float(in , MRoffset + IM_TRHC_A , &trhc_A);
    mrRead_float(in , MRoffset + IM_TRHC_S , &trhc_S);

    mrRead_float(in , MRoffset + IM_BRHC_R , &brhc_R);
    mrRead_float(in , MRoffset + IM_BRHC_A , &brhc_A);
    mrRead_float(in , MRoffset + IM_BRHC_S , &brhc_S);

    mrRead_float(in , MRoffset + IM_NORM_R , &norm_R);
    mrRead_float(in , MRoffset + IM_NORM_A , &norm_A);
    mrRead_float(in , MRoffset + IM_NORM_S , &norm_S);
    mrRead_short_int(in , MRoffset + IM_IMATRIX_X , &imatrix_X);
    mrRead_short_int(in , MRoffset + IM_IMATRIX_Y , &imatrix_Y);
    mrRead_int(in , OFFSET_IMAGE_WIDTH, &img_width);
    mrRead_int(in , OFFSET_IMAGE_HEIGHT, &img_height);
    mrRead_short_int(in, MRoffset + IM_VAS_COLLAPSE, &vas_collapse);
    mrRead_float(in , MRoffset + IM_VENCSCALE , &vencscale);
    mrRead_short_int(in, MRoffset +  IM_VENC , &venc);
    mrRead_short_int(in, MRoffset + IM_SENO , &im_seno);
    mrRead_short_int(in, MRoffset + IM_NO, &im_no);
    mrRead_int(in, MRoffset + IM_ACTUAL_DT, &im_actual_dt);
    mrRead_float(in, MRoffset + IM_USER2 , &user2);
    mrRead_float(in, MRoffset + IM_USER5 , &user5);
    mrRead_float(in, MRoffset + IM_USER6 , &user6);
    mrRead_float(in, MRoffset + IM_USER7 , &user7);
    mrRead_float(in, MRoffset + IM_USER8 , &user8);
    mrRead_float(in, MRoffset + IM_USER9 , &user9);
    mrRead_float(in, MRoffset + IM_USER12 , &user12);
    mrRead_float(in, MRoffset + IM_USER13 , &user13);
    mrRead_float(in, MRoffset + IM_USER14 , &user14);
    mrRead_short_int(in , MRoffset + IM_VASFLAGS , &vasflags);
    mrRead_char(in, MRoffset + IM_PSDNAME, 33, psdname);
    mrRead_short_int(in, MRoffset + IM_HRTRATE, &hrtrate);
    mrRead_float(in, MRoffset + IM_DIM_X , &MR_dim_X);
    mrRead_float(in, MRoffset + IM_DIM_Y , &MR_dim_Y);

    fprintf(stdout,"\n\nSelected info\n\n");
    //fprintf(stdout,"patid              : %s\n",patid);
    //fprintf(stdout,"patname            : %s\n",patname);
    fprintf(stdout,"psdname            : %s\n",psdname);
    //fprintf(stdout,"exam  number       : %hi\n",im_exno);
    fprintf(stdout,"im_seno            : %i\n",im_seno);
    fprintf(stdout,"im_no              : %i\n",im_no);
    //fprintf(stdout,"actual acq. time   : %i\n",im_actual_dt);
    fprintf(stdout,"pixel size         : %f by %f\n",pixelSize_x,pixelSize_y);
    fprintf(stdout,"slice thickness    : %f\n", slthick);
    fprintf(stdout,"imatrix            : %i by %i\n",imatrix_X,imatrix_Y); 
    fprintf(stdout,"img_width          : %i\n",img_width);
    fprintf(stdout,"img_height         : %i\n",img_height);
    fprintf(stdout,"top left corner    : %f %f %f \n",tlhc_R, tlhc_A, tlhc_S);
    fprintf(stdout,"top right corner   : %f %f %f \n",trhc_R, trhc_A, trhc_S);
    fprintf(stdout,"bottom right corner: %f %f %f \n",brhc_R, brhc_A, brhc_S);
    fprintf(stdout,"Normal to RAS plane: %f %f %f \n",norm_R, norm_A, norm_S);
    fprintf(stdout,"Header Length      : %i\n",hdr_length);
    fprintf(stdout,"venc               : %i\n",venc);
    fprintf(stdout,"vencscale          : %f\n",vencscale);
    fprintf(stdout,"vas_collapse       : %i\n",vas_collapse);
    fprintf(stdout,"user2              : %f\n",user2);
    fprintf(stdout,"user5              : %f\n",user5);
    fprintf(stdout,"user6              : %f\n",user6);
    fprintf(stdout,"user7              : %f\n",user7);
    fprintf(stdout,"user8              : %f\n",user8);
    fprintf(stdout,"user9              : %f\n",user9);
    fprintf(stdout,"user12             : %f\n",user12);
    fprintf(stdout,"user13             : %f\n",user13);
    fprintf(stdout,"user14             : %f\n",user14);
    fprintf(stdout,"mag. weight. flag  : %hi\n",vasflags);
    fprintf(stdout,"heart rate (bpm)   : %hi\n",hrtrate);
    fprintf(stdout,"MR_dim_X           : %f\n",MR_dim_X);
    fprintf(stdout,"MR_dim_Y           : %f\n",MR_dim_Y);
    fprintf(stdout,"\n\n");

    fclose(in);

    *vdims_x = pixelSize_x;
    *vdims_y = pixelSize_y;
    *dim_x = img_width;
    *dim_y = img_height;
    *file_hdr_size = hdr_length;
    *rtnvenc = venc;
    *rtnvencscale = vencscale;
    *rtnvas_collapse = vas_collapse;
    *rtnuser2 = user2;
    *rtnuser5 = user5;
    *rtnuser6 = user6;
    *rtnuser7 = user7;
    *rtnuser8 = user8;
    *rtnuser9 = user9;
    *rtnuser12 = user12;
    *rtnuser13 = user13;
    *rtnuser14 = user14;
    *heart_rate = hrtrate;
    *rtnim_no = im_no;
    *rtnim_seno = im_seno;

    ul[0] = tlhc_R; ul[1] = tlhc_A; ul[2] = tlhc_S;
    ur[0] = trhc_R; ur[1] = trhc_A; ur[2] = trhc_S;
    br[0] = brhc_R; br[1] = brhc_A; br[2] = brhc_S;

    nrm_RAS[0] = norm_R; nrm_RAS[1] = norm_A; nrm_RAS[2] = norm_S;
    *magWeightFlag = vasflags;
    *examNumber = im_exno;
    *acquisitionTime = im_actual_dt;

    return CV_OK;
}
