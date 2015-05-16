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
#include <string.h>
#include "cvRepository.h"
#include "cvRepositoryData.h"
#include "cvStrPts.h"
#include "cvPolyData.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cv_read_header.h"
#include "cv_decode.h"
#include "cv_calc_correction_eqn.h"
#include "cv_img_threshold.h"
#include "cvDistanceMap.h"
#include "cv_mask_image_in_place.h"
#include "cv_image_init.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

#include "cv_globals.h"

// Prototypes:

int Image_ReadHeaderCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );
int Image_DecodeCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );
int Image_CalcCorrectionEqnCmd( ClientData clientData, Tcl_Interp *interp,
                                int argc, CONST84 char *argv[] );
int Image_CalcCorrectionEqnAutoCmd( ClientData clientData, Tcl_Interp *interp,
                                int argc, CONST84 char *argv[] );
int Image_ThresholdCmd( ClientData clientData, Tcl_Interp *interp,
                                int argc, CONST84 char *argv[] );
int Image_ComputeStructuredCoordCmd( ClientData clientData, Tcl_Interp *interp,
                                int argc, CONST84 char *argv[] );
int Image_CreateDistanceMapCmd( ClientData clientData, Tcl_Interp *interp,
                                int argc, CONST84 char *argv[] );
int Image_FindPathCmd( ClientData clientData, Tcl_Interp *interp,
                                int argc, CONST84 char *argv[] );
int Image_MaskInPlaceCmd( ClientData clientData, Tcl_Interp *interp,
                                int argc, CONST84 char *argv[] );

// ----------
// Image_Init
// ----------

int Image_Init( Tcl_Interp *interp )
{
  Tcl_CreateCommand( interp, "img_readHeader_5X", Image_ReadHeaderCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL ); 
  Tcl_CreateCommand( interp, "img_decode", Image_DecodeCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "img_calcCorrectionEqn", Image_CalcCorrectionEqnCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "img_calcCorrectionEqnAuto", Image_CalcCorrectionEqnAutoCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "img_threshold", Image_ThresholdCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "img_computeStructuredCoord", Image_ComputeStructuredCoordCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "img_createDistanceMap", Image_CreateDistanceMapCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "img_findPath", Image_FindPathCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "img_mask", Image_MaskInPlaceCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  return TCL_OK;
}


// --------------------
// Image_ReadHeaderCmd
// --------------------

int Image_ReadHeaderCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;

  char *filename;
  int readProtected = 0;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-file", STRING_Type, &filename, NULL, REQUIRED, 0, { 0 } },
    { "-protected", INT_Type, &readProtected, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );

  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command
  float vdims_x, vdims_y;
  int dim_x, dim_y;
  int file_hdr_size;
  float ul[3],ur[3],br[3];
  int venc;
  float vencscale;
  int vas_collapse;
  float user2;
  float user5;
  float user6;
  float user7;
  float user8;
  float user9;
  float user12;
  float user13;
  float user14;
  // these string sizes need to match those contained in the image
  // header
  char patid[13];
  char patname[25];
  char psdname[33];
  int magWeightFlag;
  int examNumber;
  int acquisitionTime;
  float nrm_RAS[3];
  int heart_rate;
  int im_no;
  int im_seno;

  int status = mrRead_Header (filename, &vdims_x, &vdims_y,
                              &dim_x, &dim_y, &file_hdr_size,
                              ul, ur, br,&venc,&vencscale,
                              &vas_collapse,&user2, &user5, &user6,
			      &user7, &user8 ,&user9,
                              &user12, &user13 ,&user14, 
                              patid, patname, psdname,
                              &magWeightFlag, &examNumber, nrm_RAS, 
                              &acquisitionTime,&heart_rate,&im_no,&im_seno);

  if ( status == CV_ERROR ) {
    Tcl_AppendResult( interp, "Problem reading header for ", filename, (char *)NULL );
    return TCL_ERROR;
  }

  char tmpStr[1024];
  tmpStr[0]='\0';
  sprintf(tmpStr,"extent {%i %i}",dim_x,dim_y);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"voxel_dims {%.8f %.8f}",vdims_x,vdims_y);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"file_hdr_size %i",file_hdr_size);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"top_left_corner {%.8f %.8f %.8f}",ul[0],ul[1],ul[2]);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"top_right_corner {%.8f %.8f %.8f}",ur[0],ur[1],ur[2]);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"bottom_right_corner {%.8f %.8f %.8f}",br[0],br[1],br[2]);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"venc %i",venc);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"vencscale %.8f",vencscale);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"vas_collapse %i",vas_collapse);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"user2 %f",user2);
  Tcl_AppendElement(interp, tmpStr);  
  tmpStr[0]='\0';
  sprintf(tmpStr,"user5 %f",user5);
  Tcl_AppendElement(interp, tmpStr);  
  tmpStr[0]='\0';
  sprintf(tmpStr,"user6 %f",user6);
  Tcl_AppendElement(interp, tmpStr);  
  tmpStr[0]='\0';
  sprintf(tmpStr,"user7 %f",user7);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"user8 %f",user8);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"user9 %f",user9);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"user12 %f",user12);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"user13 %f",user13);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"user14 %f",user14);
  Tcl_AppendElement(interp, tmpStr);
  if (readProtected != 0) {
    tmpStr[0]='\0';
    sprintf(tmpStr,"patient_id {%s}",patid);
    Tcl_AppendElement(interp, tmpStr);
    tmpStr[0]='\0';
    sprintf(tmpStr,"patient_name {%s}",patname);
    Tcl_AppendElement(interp, tmpStr);
  }
  tmpStr[0]='\0';
  sprintf(tmpStr,"psdname {%s}",psdname);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"mag_weight_flag %i",magWeightFlag);
  Tcl_AppendElement(interp, tmpStr);
  if (readProtected != 0) {
    tmpStr[0]='\0';
    sprintf(tmpStr,"exam_number %i",examNumber);
    Tcl_AppendElement(interp, tmpStr);
  }
  tmpStr[0]='\0';
  sprintf(tmpStr,"normal_to_plane {%.8f %.8f %.8f}",nrm_RAS[0],nrm_RAS[1],nrm_RAS[2]);
  Tcl_AppendElement(interp, tmpStr);
  if (readProtected != 0) {
    tmpStr[0]='\0';
    sprintf(tmpStr,"acquisition_time %i",acquisitionTime);
    Tcl_AppendElement(interp, tmpStr);
  }
  tmpStr[0]='\0';
  sprintf(tmpStr,"heart_rate_bpm %i",heart_rate);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"im_no %i",im_no);
  Tcl_AppendElement(interp, tmpStr);
  tmpStr[0]='\0';
  sprintf(tmpStr,"im_seno %i",im_seno);
  Tcl_AppendElement(interp, tmpStr);

  return TCL_OK;
}


// ---------------
// Image_DecodeCmd
// ---------------

int Image_DecodeCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;

  char *magname;
  magname = NULL;
  char *phasename;
  char *result;
  double venc,vencscale;
  int mag_weight_flag = 0;

  int table_sz = 5;
  ARG_Entry arg_table[] = {
    { "-magImage", STRING_Type, &magname, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-phaseImage", STRING_Type, &phasename, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
    { "-venc", DOUBLE_Type, &venc, NULL, REQUIRED, 0, { 0 } },
    { "-vencscale", DOUBLE_Type, &vencscale, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );

  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *vtkspMag, *vtkspPhase;

  if (magname != NULL) {
    mag_weight_flag = 1;
    // Look up given image object:
    img = gRepository->GetObject( magname );
    if ( img == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", magname, (char *)NULL );
      return TCL_ERROR;
    }

    // Make sure image is of type STRUCTURED_PTS_T:
    type = img->GetType();
    if ( type != STRUCTURED_PTS_T ) {
      Tcl_AppendResult( interp, "error: object ", magname,
	  	      "not of type StructuredPts", (char *)NULL );
      return TCL_ERROR;
    }
    // Retrive geometric information:
    vtkspMag = ((cvStrPts*)img)->GetVtkStructuredPoints();
  }

  // Look up given image object:
  img = gRepository->GetObject( phasename );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", phasename, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", phasename,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }

  // Retrive geometric information:
  vtkspPhase = ((cvStrPts*)img)->GetVtkStructuredPoints();

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( result ) ) {
    Tcl_AppendResult( interp, "object ", result, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  vtkStructuredPoints *obj;
  int status;
  if (mag_weight_flag == 0) {
    status = mr_decode (vtkspPhase,venc,vencscale,&obj);
  } else {
    status = mr_decode_masked (vtkspMag,vtkspPhase,venc,vencscale,&obj);
  }

  if ( status == CV_ERROR ) {
    Tcl_AppendResult( interp, "Problem decoding ", magname, " and ",phasename,(char *)NULL );
    return TCL_ERROR;
  }

  cvStrPts *sp = new cvStrPts( obj );
  sp->SetName( result );
  if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", result,
		      " in repository", (char *)NULL );
    delete sp;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, sp->GetName(), TCL_VOLATILE );
  return TCL_OK;

}


// --------------------------
// Image_CalcCorrectionEqnCmd
// --------------------------

int Image_CalcCorrectionEqnCmd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  char *usage;
  int i;
  ARG_List regionsArg;
  ARG_List imagesArg;
  int order = 0;
  RepositoryDataT type;

  int table_sz = 3;
  ARG_Entry arg_table[] = {
    { "-regions", LIST_Type, &regionsArg, NULL, REQUIRED, 0, { 0 } },
    { "-images", LIST_Type, &imagesArg, NULL, REQUIRED, 0, { 0 } },
    { "-order", INT_Type, &order, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );

  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command

  // check for valid order
  if (order < 0 || order > 3) {
        Tcl_AppendResult( interp, "order must be 0,1, or 2", (char *)NULL );
        ARG_FreeListArgvs( table_sz, arg_table );
        return TCL_ERROR;
  }

  int numRegions = regionsArg.argc;
  int numImages = imagesArg.argc;

  if (numRegions == 0) {
       Tcl_AppendResult( interp, "empty list of regions", (char *)NULL );
        ARG_FreeListArgvs( table_sz, arg_table );
        return TCL_ERROR;
  }

  if (numImages == 0) {
       Tcl_AppendResult( interp, "empty list of images", (char *)NULL );
        ARG_FreeListArgvs( table_sz, arg_table );
        return TCL_ERROR;
  }


  // find the corresponding repository objects to each name
  cvRepositoryData *pd,*img;
  vtkPolyData **listPd = new vtkPolyData* [numRegions];
  vtkStructuredPoints **listImg = new vtkStructuredPoints* [numImages];

  for (i = 0; i < numRegions; i++) {
    if (regionsArg.argv[i] != NULL) {
      pd = gRepository->GetObject( regionsArg.argv[i] );
      if ( pd == NULL ) {
        Tcl_AppendResult( interp, "couldn't find object ", regionsArg.argv[i], (char *)NULL );
        delete [] listPd;
        delete [] listImg;
        ARG_FreeListArgvs( table_sz, arg_table );
        return TCL_ERROR;
      }

      // Make sure region is of type POLYDATA_T:
      type = pd->GetType();
      if ( type != POLY_DATA_T ) {
        Tcl_AppendResult( interp, "error: object ", regionsArg.argv[i],
	  	      "not of type cvPolyData", (char *)NULL );
        delete [] listPd;
        delete [] listImg;
        ARG_FreeListArgvs( table_sz, arg_table );
        return TCL_ERROR;
      }
      listPd[i] = ((cvPolyData*)pd)->GetVtkPolyData();
    } else {
      Tcl_AppendResult( interp, "NULL region pointer encountered ", (char *)NULL );
      delete [] listPd;
      delete [] listImg;
      ARG_FreeListArgvs( table_sz, arg_table );
      return TCL_ERROR;     
    }
  }

  // find the corresponding repository objects to each name
  for (i = 0; i < numImages; i++) {
    if (imagesArg.argv[i] != NULL) {
      img = gRepository->GetObject( imagesArg.argv[i] );
      if ( img == NULL ) {
        Tcl_AppendResult( interp, "couldn't find object ", imagesArg.argv[i], (char *)NULL );
        delete [] listPd;
        delete [] listImg;
        ARG_FreeListArgvs ( table_sz, arg_table );
        return TCL_ERROR;
      }

      // Make sure image is of type POLYDATA_T:
      type = img->GetType();
      if ( type != STRUCTURED_PTS_T ) {
        Tcl_AppendResult( interp, "error: object ", imagesArg.argv[i],
	  	      "not of type StructuredPts", (char *)NULL );
        delete [] listPd;
        delete [] listImg;
        ARG_FreeListArgvs ( table_sz, arg_table );
        return TCL_ERROR;
      }
      listImg[i] = ((cvStrPts*)img)->GetVtkStructuredPoints();

    } else {
      Tcl_AppendResult( interp, "NULL image pointer encountered ", (char *)NULL );
      delete [] listPd;
      delete [] listImg;
      ARG_FreeListArgvs ( table_sz, arg_table );
      return TCL_ERROR;     
    }
  }

  // debug info
  for (i = 0; i < numRegions; i++) {
      fprintf(stdout,"region %i: %s %i\n",i,regionsArg.argv[i],listPd[i]);
  }
  for (i = 0; i < numImages; i++) {
      fprintf(stdout,"images %i: %s %i\n",i,imagesArg.argv[i],listImg[i]);
  }

  // classify points and calculate correction equation
  double results[6];
  int status = img_calcCorrectionEqn(numRegions,listPd,numImages,listImg,order,results);

  // clean up
  delete [] listPd;
  delete [] listImg;
  ARG_FreeListArgvs( table_sz, arg_table );

  if ( status == CV_ERROR ) {
    Tcl_AppendResult( interp, "error finding correction equation ", (char *)NULL );
    return TCL_ERROR;
  } 

  // return a string with the correction equation
  char r[2048];
  r[0] = '\0';
  if (order == 0) {
      sprintf(r,"%le",results[0]);
  } else if (order == 1) {
      sprintf(r,"%le %s %le %s %le %s",results[0],
         " + ", results[1], "*$x + ", results[2], "*$y");
  } else if (order == 2) {
      sprintf(r,"%le %s %le %s %le %s %le %s %le %s %le %s",
             results[0]," + ",results[1],"*$x + ",results[2],
             "*$y + ",results[3],"*$x*$x + ",results[4],
             "*$y*$y + ",results[5],"*$x*$y");
  }
  fprintf(stdout,r);
  fprintf(stdout,"\n");

  Tcl_AppendResult( interp, r, (char *)NULL );
  return TCL_OK;

}  


// ------------------------------
// Image_CalcCorrectionEqnAutoCmd
// ------------------------------

int Image_CalcCorrectionEqnAutoCmd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  char *usage;
  int i;
  ARG_List regionsArg;
  ARG_List imagesArg;
  int order = 0;
  double factor = 0;
  RepositoryDataT type;
  char *objName;

  int table_sz = 5;
  ARG_Entry arg_table[] = {
    { "-regions", LIST_Type, &regionsArg, NULL, REQUIRED, 0, { 0 } },
    { "-images", LIST_Type, &imagesArg, NULL, REQUIRED, 0, { 0 } },
    { "-order", INT_Type, &order, NULL, REQUIRED, 0, { 0 } },
    { "-factor", DOUBLE_Type, &factor, NULL, REQUIRED, 0, { 0 } }, 
    { "-mask", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );

  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command

  // check for valid order
  if (order < 0 || order > 3) {
        Tcl_AppendResult( interp, "order must be 0,1, or 2", (char *)NULL );
        ARG_FreeListArgvs( table_sz, arg_table );
        return TCL_ERROR;
  }

  int numImages = imagesArg.argc;
  if (numImages == 0) {
       Tcl_AppendResult( interp, "empty list of images", (char *)NULL );
        ARG_FreeListArgvs( table_sz, arg_table );
        return TCL_ERROR;
  }

  int numRegions = 0;
  numRegions = regionsArg.argc;
  fprintf(stdout,"numRegions from UI: %i\n",numRegions);
  fflush(stdout);

  //  if (numRegions == 0) {
  //       Tcl_AppendResult( interp, "empty list of regions", (char *)NULL );
  //        ARG_FreeListArgvs( table_sz, arg_table );
  //        return TCL_ERROR;
  //  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "object ", objName, " already exists",
		      (char *)NULL );
    ARG_FreeListArgvs ( table_sz, arg_table );
    return TCL_ERROR;
  }


  // find the corresponding repository objects to each name
  cvRepositoryData *pd,*img;

  vtkStructuredPoints **listImg = new vtkStructuredPoints* [numImages];
 
  // find the corresponding repository objects to each name
  for (i = 0; i < numImages; i++) {
    if (imagesArg.argv[i] != NULL) {
      img = gRepository->GetObject( imagesArg.argv[i] );
      if ( img == NULL ) {
        Tcl_AppendResult( interp, "couldn't find object ", imagesArg.argv[i], (char *)NULL );
        delete [] listImg;
        ARG_FreeListArgvs ( table_sz, arg_table );
        return TCL_ERROR;
      }

      // Make sure image is of type POLYDATA_T:
      type = img->GetType();
      if ( type != STRUCTURED_PTS_T ) {
        Tcl_AppendResult( interp, "error: object ", imagesArg.argv[i],
	  	      "not of type StructuredPts", (char *)NULL );
        delete [] listImg;
        ARG_FreeListArgvs ( table_sz, arg_table );
        return TCL_ERROR;
      }
      listImg[i] = ((cvStrPts*)img)->GetVtkStructuredPoints();

    } else {
      Tcl_AppendResult( interp, "NULL image pointer encountered ", (char *)NULL );
      delete [] listImg;
      ARG_FreeListArgvs ( table_sz, arg_table );
      return TCL_ERROR;     
    }
  }

  vtkPolyData **listPd = NULL;
  if (numRegions > 0) {
  listPd = new vtkPolyData* [numRegions];
  for (i = 0; i < numRegions; i++) {
    if (regionsArg.argv[i] != NULL) {
      pd = gRepository->GetObject( regionsArg.argv[i] );
      if ( pd == NULL ) {
        Tcl_AppendResult( interp, "couldn't find object ", regionsArg.argv[i], (char *)NULL );
        delete [] listPd;
        delete [] listImg;
        ARG_FreeListArgvs( table_sz, arg_table );
        return TCL_ERROR;
      }

      // Make sure region is of type POLYDATA_T:
      type = pd->GetType();
      if ( type != POLY_DATA_T ) {
        Tcl_AppendResult( interp, "error: object ", regionsArg.argv[i],
	  	      "not of type cvPolyData", (char *)NULL );
        delete [] listPd;
        delete [] listImg;
        ARG_FreeListArgvs( table_sz, arg_table );
        return TCL_ERROR;
      }
      listPd[i] = ((cvPolyData*)pd)->GetVtkPolyData();
    } else {
      Tcl_AppendResult( interp, "NULL region pointer encountered ", (char *)NULL );
      delete [] listPd;
      delete [] listImg;
      ARG_FreeListArgvs( table_sz, arg_table );
      return TCL_ERROR;     
    }
  }
  }

  // debug info
  for (i = 0; i < numRegions; i++) {
      fprintf(stdout,"region %i: %s %i\n",i,regionsArg.argv[i],listPd[i]);
  }
  for (i = 0; i < numImages; i++) {
      fprintf(stdout,"images %i: %s %i\n",i,imagesArg.argv[i],listImg[i]);
  }

  // classify points and calculate correction equation
  double results[6];
  vtkStructuredPoints* maskImg = NULL;
  int status = img_calcCorrectionEqnAuto(numRegions,listPd,numImages,listImg,order,factor,results,&maskImg);

  // clean up
  if (numRegions != 0) delete [] listPd;
  delete [] listImg;
  ARG_FreeListArgvs( table_sz, arg_table );

  if ( status == CV_ERROR ) {
    Tcl_AppendResult( interp, "error finding correction equation ", (char *)NULL );
    return TCL_ERROR;
  } 

  // return a string with the correction equation
  char r[2048];
  r[0] = '\0';
  if (order == 0) {
      sprintf(r,"%le",results[0]);
  } else if (order == 1) {
      sprintf(r,"%le %s %le %s %le %s",results[0],
         " + ", results[1], "*$x + ", results[2], "*$y");
  } else if (order == 2) {
      sprintf(r,"%le %s %le %s %le %s %le %s %le %s %le %s",
             results[0]," + ",results[1],"*$x + ",results[2],
             "*$y + ",results[3],"*$x*$x + ",results[4],
             "*$y*$y + ",results[5],"*$x*$y");
  }
  fprintf(stdout,r);
  fprintf(stdout,"\n");

  Tcl_AppendResult( interp, r, (char *)NULL );

  cvStrPts *sp = new cvStrPts(maskImg);

  // Register the image
  if ( !( gRepository->Register( objName, sp ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete sp;
    return TCL_ERROR;
  }

  return TCL_OK;

}  


// ------------------
// Image_ThresholdCmd
// ------------------

int Image_ThresholdCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;

  char *imagename;
  imagename = NULL;
  char *result;
  result = NULL;

  double thrMin,thrMax;
  int max_num_pts;

  int table_sz = 5;
  ARG_Entry arg_table[] = {
    { "-image", STRING_Type, &imagename, NULL, REQUIRED, 0, { 0 } },
    { "-result", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
    { "-min_value", DOUBLE_Type, &thrMin, NULL, REQUIRED, 0, { 0 } },
    { "-max_value", DOUBLE_Type, &thrMax, NULL, REQUIRED, 0, { 0 } },
    { "-max_num_pts", INT_Type, &max_num_pts, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );

  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *vtksp;

  if (imagename != NULL) {
    // Look up given image object:
    img = gRepository->GetObject( imagename );
    if ( img == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", imagename, (char *)NULL );
      return TCL_ERROR;
    }

    // Make sure image is of type STRUCTURED_PTS_T:
    type = img->GetType();
    if ( type != STRUCTURED_PTS_T ) {
      Tcl_AppendResult( interp, "error: object ", imagename,
	  	      "not of type StructuredPts", (char *)NULL );
      return TCL_ERROR;
    }
    // Retrive geometric information:
    vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( result ) ) {
    Tcl_AppendResult( interp, "object ", result, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  cvPolyData *obj = NULL;
  int status = img_threshold(vtksp, thrMin, thrMax, max_num_pts, &obj);

  if ( status == CV_ERROR || obj == NULL) {
    Tcl_AppendResult( interp, "Problem thresholding ", imagename,(char *)NULL );
    return TCL_ERROR;
  }

  obj->SetName( result );
  if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", result,
		      " in repository", (char *)NULL );
    delete obj;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, obj->GetName(), TCL_VOLATILE );
  return TCL_OK;

}


// -------------------------------
// Image_ComputeStructuredCoordCmd
// -------------------------------

int Image_ComputeStructuredCoordCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;

  char *imagename;
  ARG_List ptList;

  imagename = NULL;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-image", STRING_Type, &imagename, NULL, REQUIRED, 0, { 0 } },
    { "-pt", LIST_Type, &ptList, NULL, REQUIRED, 0, { 0 } }
  };

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );

  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  double pt[3];
  int npt;
  if ( ARG_ParseTclListStatic( interp, ptList, DOUBLE_Type, pt, 3, &npt )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }
  ARG_FreeListArgvs( table_sz, arg_table );

  // Do work of command

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *vtksp;

  if (imagename != NULL) {
    // Look up given image object:
    img = gRepository->GetObject( imagename );
    if ( img == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", imagename, (char *)NULL );
      return TCL_ERROR;
    }

    // Make sure image is of type STRUCTURED_PTS_T:
    type = img->GetType();
    if ( type != STRUCTURED_PTS_T ) {
      Tcl_AppendResult( interp, "error: object ", imagename,
	  	      "not of type StructuredPts", (char *)NULL );
      return TCL_ERROR;
    }
    // Retrive geometric information:
    vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
  }

  vtkFloatingPointType x[3];
  int ijk[3];
  vtkFloatingPointType pcoords[3];

  char rtnstr[2048];
  rtnstr[0]='\0';

  x[0]=pt[0];x[1]=pt[1];x[2]=pt[2];

  if ( (vtksp->ComputeStructuredCoordinates(x, ijk, pcoords)) == 0) {
      Tcl_SetResult (interp,rtnstr,TCL_VOLATILE);
      return TCL_OK;
  }

  // return ijk
  rtnstr[0]='\0';
  sprintf(rtnstr,"%i %i %i",ijk[0],ijk[1],ijk[2]);
  Tcl_AppendElement(interp, rtnstr);

  // return pcoords
  rtnstr[0]='\0';
  sprintf(rtnstr,"%.6e %.6e %.6e",pcoords[0],pcoords[1],pcoords[2]);
  Tcl_AppendElement(interp, rtnstr);

  // return intensity for convenience
  vtkFloatingPointType intensity = 0.0;
  intensity = vtksp->GetPointData()->GetScalars()->GetTuple1(vtksp->ComputePointId(ijk));
  rtnstr[0]='\0';
  sprintf(rtnstr,"%f",intensity); 
  Tcl_AppendElement(interp, rtnstr);

  return TCL_OK;

}


// --------------------------
// Image_CreateDistanceMapCmd
// --------------------------

int Image_CreateDistanceMapCmd( ClientData clientData, Tcl_Interp *interp,
			    int argc, CONST84 char *argv[] )
{
  char *usage;

  char *srcName;
  char *dstName;

  ARG_List startList;
  double thr;
  int useCityBlock = 1;

  int table_sz = 5;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-start", LIST_Type, &startList, NULL, REQUIRED, 0, { 0 } },
    { "-thr", DOUBLE_Type, &thr, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-city_block", BOOL_Type, &useCityBlock, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );

  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *sp;

  // Look up given image object:
  img = gRepository->GetObject( srcName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", srcName,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }

  // Retrive geometric information:
  sp = ((cvStrPts*)img)->GetVtkStructuredPoints();

  int nstart;
  int start[3];
 
  // Parse coordinate lists:
  if ( ARG_ParseTclListStatic( interp, startList, INT_Type, start, 3, &nstart )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }

  ARG_FreeListArgvs( table_sz, arg_table );

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  vtkStructuredPoints *mapsp = NULL;
  vtkFloatingPointType thrval = thr;

  cvDistanceMap* distmap = new cvDistanceMap();
  if (useCityBlock == FALSE) {
      distmap->setUse26ConnectivityDistance();
  }
  int status = distmap->createDistanceMap(sp,thrval,start);

  if ( status == CV_ERROR ) {
    Tcl_AppendResult( interp, "Problem creating distance map for ", srcName,(char *)NULL );
    return TCL_ERROR;
  }

  cvStrPts *repossp = new cvStrPts( distmap->getDistanceMap() );
  delete distmap;

  repossp->SetName( dstName );
  if ( !( gRepository->Register( repossp->GetName(), repossp ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete repossp;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, repossp->GetName(), TCL_VOLATILE );
  return TCL_OK;

}


// -----------------
// Image_FindPathCmd
// -----------------

int Image_FindPathCmd( ClientData clientData, Tcl_Interp *interp,
			    int argc, CONST84 char *argv[] )
{
  char *usage;

  char *srcName;
  char *dstName;

  ARG_List stopList;
  int useCityBlock = 1;
  int maxIter = -1;
  int minqstop = 0;

  int table_sz = 6;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-stop", LIST_Type, &stopList, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
    { "-city_block", BOOL_Type, &useCityBlock, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-thin_passes", INT_Type, &maxIter, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-min_dist", INT_Type, &minqstop, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );

  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *sp;

  // Look up given image object:
  img = gRepository->GetObject( srcName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", srcName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", srcName,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }

  // Retrive geometric information:
  sp = ((cvStrPts*)img)->GetVtkStructuredPoints();

  int nstart;
  int stop[3];
 
  // Parse coordinate lists:
  if ( ARG_ParseTclListStatic( interp, stopList, INT_Type, stop, 3, &nstart )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }

  ARG_FreeListArgvs( table_sz, arg_table );

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "object ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  vtkStructuredPoints *mapsp = NULL;

  cvDistanceMap* distmap = new cvDistanceMap();
  distmap->setDistanceMap(sp);
  if (useCityBlock == FALSE) {
      distmap->setUse26ConnectivityDistance();
  }
  vtkPolyData *pd;
  if (maxIter < 0) {
    pd = distmap->getPath(stop,minqstop);
  } else {
    pd = distmap->getPathByThinning(stop,minqstop,maxIter);
  }

  if ( pd == NULL ) {
    Tcl_AppendResult( interp, "Problem finding path for ", srcName,(char *)NULL );
    return TCL_ERROR;
  }

  cvPolyData *dst = new cvPolyData (pd);

  dst->SetName( dstName );
  if ( !( gRepository->Register( dst->GetName(), dst ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
    delete dst;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, dst->GetName(), TCL_VOLATILE );
  return TCL_OK;

}


// --------------------
// Image_MaskInPlaceCmd
// --------------------

int Image_MaskInPlaceCmd( ClientData clientData, Tcl_Interp *interp,
			    int argc, CONST84 char *argv[] )
{
  char *usage;

  char *objName;
  char *maskName;
  
  double replaceVal = 0;
  int notval = 0;

  int table_sz = 4;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-mask", STRING_Type, &maskName, NULL, REQUIRED, 0, { 0 } },
    { "-value", DOUBLE_Type, &replaceVal, NULL, GDSC_OPTIONAL, 0, { 0 } },
    { "-not", BOOL_Type, &notval, NULL, GDSC_OPTIONAL, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );

  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command

  RepositoryDataT type;
  cvRepositoryData *img, *mask;
  vtkStructuredPoints *imgsp, *masksp;

  // Look up given image object:
  img = gRepository->GetObject( objName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", objName,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }


  // Look up given mask object:
  mask = gRepository->GetObject( maskName );
  if ( mask == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", maskName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure mask is of type STRUCTURED_PTS_T:
  type = mask->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", maskName,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }


  // Retrive geometric information:
  imgsp = ((cvStrPts*)img)->GetVtkStructuredPoints();
  masksp = ((cvStrPts*)mask)->GetVtkStructuredPoints();

  int status = gdscMaskImageInPlace(imgsp,masksp,replaceVal,notval);

  if ( status == CV_ERROR ) {
    Tcl_AppendResult( interp, "Problem masking in place for ", objName,(char *)NULL );
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, img->GetName(), TCL_VOLATILE );
  return TCL_OK;

}

