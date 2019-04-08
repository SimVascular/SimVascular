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

#include "SimVascular.h"

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_RepositoryData.h"
#include "sv_StrPts.h"
#include "sv_PolyData.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv2_read_header.h"
#include "sv2_decode.h"
#include "sv2_calc_correction_eqn.h"
#include "sv2_img_threshold.h"
#include "sv2_DistanceMap.h"
#include "sv2_mask_image_in_place.h"
#include "sv2_image_init_py.h"

#ifdef SV_USE_PYTHON
#include "Python.h"
#include "vtkPythonUtil.h"
#include "vtkSmartPointer.h"
#endif


// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

#include "sv2_globals.h"

PyObject *ImgErr;
PyObject *Image_ReadHeaderCmd(PyObject *self, PyObject *args);
PyObject *Image_DecodeCmd(PyObject *self, PyObject *args);
PyObject *Image_CalcCorrectionEqnCmd(PyObject *self, PyObject *args);
PyObject *Image_CalcCorrectionEqnAutoCmd(PyObject *self, PyObject *args);
PyObject *Image_ComputeStructuredCoordCmd(PyObject *self, PyObject *args);
PyObject *Image_ThresholdCmd(PyObject* self, PyObject* args);
PyObject *Image_CreateDistanceMapCmd(PyObject *self, PyObject *args);
PyObject *Image_FindPathCmd(PyObject *self, PyObject *args);
PyObject *Image_MaskInPlaceCmd(PyObject *self, PyObject *args);
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC
initpyImage(void);
#elif PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC
PyInit_pyImage(void);
#endif

// ----------
// Image_Init
// ----------
int Image_pyInit()
{

#if PYTHON_MAJOR_VERSION == 2
  initpyImage();
#elif PYTHON_MAJOR_VERSION == 3
  PyInit_pyImage();
#endif


  return SV_OK;

}


// --------------------
// Image_ReadHeaderCmd
// --------------------

PyObject *Image_ReadHeaderCmd(PyObject *self, PyObject *args)
{
 // char *usage;

  char *filename;
  int readProtected = 0;
  if (!PyArg_ParseTuple(args,"s|i", &filename,&readProtected))
  {
    PyErr_SetString(ImgErr, "Could not import 1 char and 1 int, filename, readProtected ");
    
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
  char tmpStr[1024];
  if ( status == SV_ERROR ) {
    sprintf(tmpStr,"Problem reading header for ", filename );
    PyErr_SetString( ImgErr, tmpStr);
    
  }
  PyObject *pylist=PyList_New(0);
  tmpStr[0]='\0';
  sprintf(tmpStr,"extent {%i %i}",dim_x,dim_y);
  PyObject* pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"voxel_dims {%.8f %.8f}",vdims_x,vdims_y);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"file_hdr_size %i",file_hdr_size);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"top_left_corner {%.8f %.8f %.8f}",ul[0],ul[1],ul[2]);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"top_right_corner {%.8f %.8f %.8f}",ur[0],ur[1],ur[2]);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"bottom_right_corner {%.8f %.8f %.8f}",br[0],br[1],br[2]);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"venc %i",venc);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"vencscale %.8f",vencscale);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"vas_collapse %i",vas_collapse);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"user2 %f",user2);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"user5 %f",user5);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"user6 %f",user6);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"user7 %f",user7);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"user8 %f",user8);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"user9 %f",user9);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"user12 %f",user12);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"user13 %f",user13);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"user14 %f",user14);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  if (readProtected != 0) {
    tmpStr[0]='\0';
    sprintf(tmpStr,"patient_id {%s}",patid);
    pyStr= PyString_FromString(tmpStr);
    PyList_Append( pylist,  pyStr );
    tmpStr[0]='\0';
    sprintf(tmpStr,"patient_name {%s}",patname);
    pyStr= PyString_FromString(tmpStr);
    PyList_Append( pylist,  pyStr );
  }
  tmpStr[0]='\0';
  sprintf(tmpStr,"psdname {%s}",psdname);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"mag_weight_flag %i",magWeightFlag);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  if (readProtected != 0) {
    tmpStr[0]='\0';
    sprintf(tmpStr,"exam_number %i",examNumber);
    pyStr= PyString_FromString(tmpStr);
    PyList_Append( pylist,  pyStr );
  }
  tmpStr[0]='\0';
  sprintf(tmpStr,"normal_to_plane {%.8f %.8f %.8f}",nrm_RAS[0],nrm_RAS[1],nrm_RAS[2]);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  if (readProtected != 0) {
    tmpStr[0]='\0';
    sprintf(tmpStr,"acquisition_time %i",acquisitionTime);
    pyStr= PyString_FromString(tmpStr);
    PyList_Append( pylist,  pyStr );
  }
  tmpStr[0]='\0';
  sprintf(tmpStr,"heart_rate_bpm %i",heart_rate);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"im_no %i",im_no);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );
  tmpStr[0]='\0';
  sprintf(tmpStr,"im_seno %i",im_seno);
  pyStr= PyString_FromString(tmpStr);
  PyList_Append( pylist,  pyStr );

  return pylist;
}


// ---------------
// Image_DecodeCmd
// ---------------

PyObject* Image_DecodeCmd(PyObject *self, PyObject *args)
{
  char *usage;

  char *magname;
  magname = NULL;
  char *phasename;
  char *result;
  double venc,vencscale;
  int mag_weight_flag = 0;

  if (!PyArg_ParseTuple(args,"ssdd|s",&phasename,&result,&venc,&vencscale,&magname))
  {
    PyErr_SetString(ImgErr, "Could not import 2 char and 2 doubles or one optional char: phasename,result,venc,vencscale,magname");
    
  }

  // Do work of command
  char tmpStr[1024];
  tmpStr[0]='\0';

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *vtkspMag, *vtkspPhase;

  if (magname != NULL) {
    mag_weight_flag = 1;
    // Look up given image object:
    img = gRepository->GetObject( magname );
    if ( img == NULL ) {
      sprintf(tmpStr,"couldn't find object ", magname );
      PyErr_SetString( ImgErr, tmpStr );
      tmpStr[0]='\0';
      
    }

    // Make sure image is of type STRUCTURED_PTS_T:
    type = img->GetType();
    if ( type != STRUCTURED_PTS_T ) {
      sprintf(tmpStr,"error: object ", magname,
	  	      "not of type StructuredPts");
      PyErr_SetString( ImgErr, tmpStr );
      tmpStr[0]='\0';
      
    }
    // Retrive geometric information:
    vtkspMag = ((cvStrPts*)img)->GetVtkStructuredPoints();
  }

  // Look up given image object:
  img = gRepository->GetObject( phasename );
  if ( img == NULL ) {
    sprintf(tmpStr,"couldn't find object ", phasename );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    sprintf(tmpStr,"error: object ", phasename,
    "not of type StructuredPts" );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  // Retrive geometric information:
  vtkspPhase = ((cvStrPts*)img)->GetVtkStructuredPoints();

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( result ) ) {
    sprintf(tmpStr,"object ", result, " already exists",
		      (char *)NULL);
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';

    
  }

  vtkStructuredPoints *obj;
  int status;
  if (mag_weight_flag == 0) {
    status = mr_decode (vtkspPhase,venc,vencscale,&obj);
  } else {
    status = mr_decode_masked (vtkspMag,vtkspPhase,venc,vencscale,&obj);
  }

  if ( status == SV_ERROR ) {
    sprintf(tmpStr,"Problem decoding ", magname, " and ",phasename,(char *)NULL ,
    (char *)NULL);
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  cvStrPts *sp = new cvStrPts( obj );
  sp->SetName( result );
  if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
    sprintf(tmpStr,"error registering obj ", result,
    " in repository");
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    delete sp;
    
  }

  return Py_BuildValue("s",sp->GetName());

}


// --------------------------
// Image_CalcCorrectionEqnCmd
// --------------------------

PyObject*  Image_CalcCorrectionEqnCmd(PyObject *self, PyObject *args)
{
  char *usage;
  int i;
  int order = 0;
  RepositoryDataT type;

  PyObject *regionsArg;
  PyObject *imagesArg;
  if (!PyArg_ParseTuple(args,"OOi",&regionsArg,&imagesArg,&order))
  {
    PyErr_SetString(ImgErr,"Could not import 2 tuples, and 1 int: regionsArg,imagesArg,order");
    
  }

  // Do work of command
  char tmpStr[1024];
  tmpStr[0]='\0';
  // check for valid order
  if (order < 0 || order > 3) {
        sprintf(tmpStr,"order must be 0,1, or 2");
        PyErr_SetString( ImgErr, tmpStr );
        tmpStr[0]='\0';
        
  }

  int numRegions = PyList_Size(regionsArg);
  int numImages = PyList_Size(imagesArg);

  if (numRegions == 0) {
    sprintf(tmpStr,"empty list of regions" );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  if (numImages == 0) {
    sprintf(tmpStr,"empty list of images" );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }


  // find the corresponding repository objects to each name
  cvRepositoryData *pd,*img;
  vtkPolyData **listPd = new vtkPolyData* [numRegions];
  vtkStructuredPoints **listImg = new vtkStructuredPoints* [numImages];
  for (i = 0; i < numRegions; i++)
  {
    if (PyString_AsString(PyList_GetItem(regionsArg,i))  != NULL)
    {
      pd = gRepository->GetObject( PyString_AsString(PyList_GetItem(regionsArg,i)) );
      if ( pd == NULL )
      {
        sprintf(tmpStr,"couldn't find object ", PyString_AsString(PyList_GetItem(regionsArg,i))  );
        PyErr_SetString( ImgErr, tmpStr );
        tmpStr[0]='\0';
        delete [] listPd;
        delete [] listImg;
        
      }

      // Make sure region is of type POLYDATA_T:
      type = pd->GetType();
      if ( type != POLY_DATA_T )
      {
        sprintf(tmpStr,"error: object ", PyString_AsString(PyList_GetItem(regionsArg,i)) ,
        "not of type cvPolyData" );
        PyErr_SetString( ImgErr, tmpStr );
        tmpStr[0]='\0';
        delete [] listPd;
        delete [] listImg;
        
      }
      listPd[i] = ((cvPolyData*)pd)->GetVtkPolyData();
    }
    else
    {
      sprintf(tmpStr,"NULL region pointer encountered ");
      PyErr_SetString( ImgErr, tmpStr );
      tmpStr[0]='\0';
      delete [] listPd;
      delete [] listImg;
      
    }
  }

  // find the corresponding repository objects to each name
  for (i = 0; i < numImages; i++)
  {
    if (PyString_AsString(PyList_GetItem(imagesArg,i))  != NULL)
    {
      img = gRepository->GetObject( PyString_AsString(PyList_GetItem(imagesArg,i)) );
      if ( img == NULL )
      {
        sprintf(tmpStr,"couldn't find object ", PyString_AsString(PyList_GetItem(imagesArg,i)));
        PyErr_SetString( ImgErr, tmpStr );
        tmpStr[0]='\0';
        delete [] listPd;
        delete [] listImg;
        
      }

      // Make sure image is of type POLYDATA_T:
      type = img->GetType();
      if ( type != STRUCTURED_PTS_T )
      {
        sprintf(tmpStr,"error: object ", PyString_AsString(PyList_GetItem(imagesArg,i)),
        "not of type StructuredPts");
        PyErr_SetString( ImgErr, tmpStr );
        tmpStr[0]='\0';
        delete [] listPd;
        delete [] listImg;
        
      }
      listImg[i] = ((cvStrPts*)img)->GetVtkStructuredPoints();

    }
    else
    {
      sprintf(tmpStr,"NULL image pointer encountered ");
      PyErr_SetString( ImgErr, tmpStr );
      tmpStr[0]='\0';
      delete [] listPd;
      delete [] listImg;
      
    }
  }

  // debug info
  for (i = 0; i < numRegions; i++)
  {
      fprintf(stdout,"region %i: %s %p\n",i,PyString_AsString(PyList_GetItem(regionsArg,i)),listPd[i]);
  }
  for (i = 0; i < numImages; i++)
  {
      fprintf(stdout,"images %i: %s %p\n",i,PyString_AsString(PyList_GetItem(imagesArg,i)),listImg[i]);
  }

  // classify points and calculate correction equation
  double results[6];
  int status = img_calcCorrectionEqn(numRegions,listPd,numImages,listImg,order,results);

  // clean up
  delete [] listPd;
  delete [] listImg;

  if ( status == SV_ERROR )
  {
    sprintf(tmpStr,"error finding correction equation ");
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  // return a string with the correction equation
  char r[2048];
  r[0] = '\0';
  if (order == 0)
  {
      sprintf(r,"%le",results[0]);
  }
  else if (order == 1)
  {
      sprintf(r,"%le %s %le %s %le %s",results[0],
         " + ", results[1], "*$x + ", results[2], "*$y");
  }
  else if (order == 2)
  {
      sprintf(r,"%le %s %le %s %le %s %le %s %le %s %le %s",
             results[0]," + ",results[1],"*$x + ",results[2],
             "*$y + ",results[3],"*$x*$x + ",results[4],
             "*$y*$y + ",results[5],"*$x*$y");
  }
  fprintf(stdout,r);
  fprintf(stdout,"\n");

  return Py_BuildValue("s",r);

}


// ------------------------------
// Image_CalcCorrectionEqnAutoCmd
// ------------------------------

PyObject *Image_CalcCorrectionEqnAutoCmd(PyObject *self, PyObject *args )
{
  int i;
  PyObject *regionsArg;
  PyObject *imagesArg;
  int order = 0;
  double factor = 0;
  RepositoryDataT type;
  char *objName;


  if (!PyArg_ParseTuple(args,"OOids",&regionsArg,&imagesArg,
            &order,&factor,&objName))
  {
    PyErr_SetString(ImgErr,"Could not import 2 tuples, 1 int, 1 double and 1 char: regionsArg,imagesArg,order,factor,objname");
  }

  // Do work of command
  char tmpStr[1024];
  tmpStr[0]='\0';
  // check for valid order
  if (order < 0 || order > 3)
  {
    sprintf(tmpStr,"order must be 0,1, or 2");
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
        
  }

  int numImages = PyList_Size(imagesArg);
  if (numImages == 0)
  {
    sprintf(tmpStr,"empty list of images" );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  int numRegions =PyList_Size(regionsArg);
  fprintf(stdout,"numRegions from UI: %i\n",numRegions);
  fflush(stdout);

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) )
  {
    sprintf(tmpStr,"object ", objName, " already exists");
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }


  // find the corresponding repository objects to each name
  cvRepositoryData *pd,*img;

  vtkStructuredPoints **listImg = new vtkStructuredPoints* [numImages];

  // find the corresponding repository objects to each name
  for (i = 0; i < numImages; i++)
  {
    if (PyString_AsString(PyList_GetItem(imagesArg,i))!= NULL)
    {
      img = gRepository->GetObject( PyString_AsString(PyList_GetItem(imagesArg,i)));
      if ( img == NULL )
      {
        sprintf(tmpStr,"couldn't find object ", PyString_AsString(PyList_GetItem(imagesArg,i)));
        PyErr_SetString( ImgErr, tmpStr );
        tmpStr[0]='\0';
        delete [] listImg;
        
      }

      // Make sure image is of type POLYDATA_T:
      type = img->GetType();
      if ( type != STRUCTURED_PTS_T )
      {
        sprintf(tmpStr,"error: object ", PyString_AsString(PyList_GetItem(imagesArg,i)),
        "not of type StructuredPts");
        PyErr_SetString( ImgErr, tmpStr );
        tmpStr[0]='\0';
        delete [] listImg;
        
      }
      listImg[i] = ((cvStrPts*)img)->GetVtkStructuredPoints();

    }
    else
    {
      sprintf(tmpStr, "NULL image pointer encountered ");
      PyErr_SetString( ImgErr, tmpStr );
      tmpStr[0]='\0';
      delete [] listImg;
      
    }
  }

  vtkPolyData **listPd = NULL;
  if (numRegions > 0)
  {
    listPd = new vtkPolyData* [numRegions];
    for (i = 0; i < numRegions; i++)
    {
      if (PyString_AsString(PyList_GetItem(regionsArg,i))!= NULL)
      {
        pd = gRepository->GetObject( PyString_AsString(PyList_GetItem(regionsArg,i)));
        if ( pd == NULL )
        {
          sprintf(tmpStr, "couldn't find object ", PyString_AsString(PyList_GetItem(regionsArg,i)));
          PyErr_SetString( ImgErr, tmpStr );
          tmpStr[0]='\0';
          delete [] listPd;
          delete [] listImg;
          
        }

      // Make sure region is of type POLYDATA_T:
        type = pd->GetType();
        if ( type != POLY_DATA_T )
        {
          sprintf(tmpStr, "error: object ", PyString_AsString(PyList_GetItem(regionsArg,i)),"not of type cvPolyData");
          PyErr_SetString( ImgErr, tmpStr );
          tmpStr[0]='\0';
          delete [] listPd;
          delete [] listImg;
          
        }
        listPd[i] = ((cvPolyData*)pd)->GetVtkPolyData();
      }
      else
      {
      sprintf(tmpStr, "NULL region pointer encountered " );
      PyErr_SetString( ImgErr, tmpStr );
      tmpStr[0]='\0';
      delete [] listPd;
      delete [] listImg;
      
      }
    }
  }

  // debug info
  for (i = 0; i < numRegions; i++)
  {
      fprintf(stdout,"region %i: %s %p\n",i,PyString_AsString(PyList_GetItem(regionsArg,i)),listPd[i]);
  }
  for (i = 0; i < numImages; i++)
  {
      fprintf(stdout,"images %i: %s %p\n",i,PyString_AsString(PyList_GetItem(imagesArg,i)),listImg[i]);
  }

  // classify points and calculate correction equation
  double results[6];
  vtkStructuredPoints* maskImg = NULL;
  int status = img_calcCorrectionEqnAuto(numRegions,listPd,numImages,listImg,order,factor,results,&maskImg);

  // clean up
  if (numRegions != 0)
   delete [] listPd;
  delete [] listImg;

  if ( status == SV_ERROR )
  {
    sprintf(tmpStr, "error finding correction equation ");
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  // return a string with the correction equation
  char r[2048];
  r[0] = '\0';
  if (order == 0)
  {
      sprintf(r,"%le",results[0]);
  }
  else if (order == 1)
  {
      sprintf(r,"%le %s %le %s %le %s",results[0],
         " + ", results[1], "*$x + ", results[2], "*$y");
  }
  else if (order == 2)
  {
      sprintf(r,"%le %s %le %s %le %s %le %s %le %s %le %s",
             results[0]," + ",results[1],"*$x + ",results[2],
             "*$y + ",results[3],"*$x*$x + ",results[4],
             "*$y*$y + ",results[5],"*$x*$y");
  }
  fprintf(stdout,r);
  fprintf(stdout,"\n");

  sprintf(tmpStr, r);
  tmpStr[0]='\0';

  cvStrPts *sp = new cvStrPts(maskImg);

  // Register the image
  if ( !( gRepository->Register( objName, sp ) ) )
  {
    sprintf(tmpStr, "error registering obj ", objName,
    " in repository");
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    delete sp;
    
  }

  return Py_BuildValue("s",r);

}


// ------------------
// Image_ThresholdCmd
// ------------------
PyObject* Image_ThresholdCmd(PyObject* self, PyObject* args)
{

  char *imagename;
  imagename = NULL;
  char *result;
  result = NULL;

  double thrMin,thrMax;
  int max_num_pts;


  if (!PyArg_ParseTuple(args,"ssddi", &imagename,&result,&thrMin,&thrMax,&max_num_pts))
  {
    PyErr_SetString(ImgErr, "Could not import 2 chars, 2 doubles and 1 int, imagename, result, thrMin, thrMax, max_num_pts");
    
  }

  // Do work of command
  char tmpStr[1024];
  tmpStr[0]='\0';

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *vtksp;

  if (imagename != NULL)
  {
    // Look up given image object:
    img = gRepository->GetObject( imagename );
    if ( img == NULL )
    {
      sprintf(tmpStr, "couldn't find object %s", imagename);
      PyErr_SetString( ImgErr, tmpStr );
      tmpStr[0]='\0';
      
    }

    // Make sure image is of type STRUCTURED_PTS_T:
    type = img->GetType();
    if ( type != STRUCTURED_PTS_T )
    {
      sprintf(tmpStr, "error: object %s not of type StructuredPts", imagename);
      PyErr_SetString( ImgErr, tmpStr );
      tmpStr[0]='\0';
      
    }
    // Retrive geometric information:
    vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( result ) )
  {
    sprintf(tmpStr, "object %s already exists",result);
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  cvPolyData *obj = NULL;
  int status = img_threshold(vtksp, thrMin, thrMax, max_num_pts, &obj);

  if ( status == SV_ERROR || obj == NULL)
  {
    sprintf(tmpStr, "Problem thresholding %s", imagename);
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  obj->SetName( result );
  if ( !( gRepository->Register( obj->GetName(), obj ) ) )
  {
   //if (!(gRepository->Register(result,polydataObj))){
    sprintf(tmpStr, "error registering obj %s in repository", result);
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    delete obj;
    
  }

  vtkSmartPointer<vtkPolyData> polydataObj =
  vtkSmartPointer<vtkPolyData>::New();
  polydataObj = obj->GetVtkPolyData();
  //instead of exporting the object name, output the vtkPolydata object
  PyObject* pyVtkObj=vtkPythonUtil::GetObjectFromPointer(polydataObj);
  return pyVtkObj;

}


// -------------------------------
// Image_ComputeStructuredCoordCmd
// -------------------------------

PyObject* Image_ComputeStructuredCoordCmd(PyObject *self, PyObject *args )
{

  char *imagename;
  PyObject* ptList;

  imagename = NULL;

  if (!PyArg_ParseTuple(args,"sO",&imagename, &ptList))
  {
    PyErr_SetString(ImgErr,"Could not import one char and one tuple,imagename,ptList");
    
  }

  double pt[3];

  for (int i = 0; i < 3; i++)
  {
    pt[i]=PyFloat_AsDouble(PyList_GetItem(ptList,i));
    if (PyErr_Occurred())
    {
      PyErr_SetString( ImgErr, "Error parsing ptlist!" );
      
    }
  }
  char tmpStr[1024];
  tmpStr[0]='\0';
  // Do work of command

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *vtksp;

  if (imagename != NULL)
  {
    // Look up given image object:
    img = gRepository->GetObject( imagename );
    if ( img == NULL )
    {
      sprintf(tmpStr, "couldn't find object ", imagename);
      PyErr_SetString( ImgErr, tmpStr );
      tmpStr[0]='\0';
      
    }

    // Make sure image is of type STRUCTURED_PTS_T:
    type = img->GetType();
    if ( type != STRUCTURED_PTS_T )
    {
      sprintf(tmpStr,  "error: object ", imagename,
      "not of type StructuredPts" );
      PyErr_SetString( ImgErr, tmpStr );
      tmpStr[0]='\0';
      
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

  if ( (vtksp->ComputeStructuredCoordinates(x, ijk, pcoords)) == 0)
  {
      return Py_BuildValue("s",rtnstr);
  }
  PyObject *pylist=PyList_New(3);
  // return ijk
  rtnstr[0]='\0';
  sprintf(rtnstr,"%i %i %i",ijk[0],ijk[1],ijk[2]);
  PyObject* pyStr = PyString_FromString(rtnstr);
  PyList_SetItem( pylist, 0, pyStr );

  // return pcoords
  rtnstr[0]='\0';
  sprintf(rtnstr,"%.6e %.6e %.6e",pcoords[0],pcoords[1],pcoords[2]);
  pyStr = PyString_FromString(rtnstr);
  PyList_SetItem( pylist, 1, pyStr );

  // return intensity for convenience
  vtkFloatingPointType intensity = 0.0;
  intensity = vtksp->GetPointData()->GetScalars()->GetTuple1(vtksp->ComputePointId(ijk));
  rtnstr[0]='\0';
  sprintf(rtnstr,"%f",intensity);
  pyStr = PyString_FromString(rtnstr);
  PyList_SetItem( pylist, 2, pyStr );

  return Py_BuildValue("s",pylist);

}


// --------------------------
// Image_CreateDistanceMapCmd
// --------------------------

PyObject *Image_CreateDistanceMapCmd(PyObject *self, PyObject *args)
{
  //char *usage;

  char *srcName;
  PyObject* startList;

  double thr;
  char *dstName;

  int useCityBlock = 1;


  if (!PyArg_ParseTuple(args,"sOds|i",&srcName,&startList,
          &thr,&dstName,&useCityBlock))
  {
    PyErr_SetString(ImgErr,"Could not import 1 char, 1 tuple, 1 double, 1 char and 1 int(bool): srcName,startList,thr, dstName,useCityBlock");
    
  }

  // Do work of command
  char tmpStr[1024];
  tmpStr[0]='\0';

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *sp;

  // Look up given image object:
  img = gRepository->GetObject( srcName );
  if ( img == NULL )
  {
    sprintf(tmpStr,  "couldn't find object ", srcName  );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T )
  {
    sprintf(tmpStr,  "error: object ", srcName,
    "not of type StructuredPts");
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  // Retrive geometric information:
  sp = ((cvStrPts*)img)->GetVtkStructuredPoints();

  int nstart;
  int start[3];

  // Parse coordinate lists:
  for (int i = 0; i < 3; i++)
  {
    start[i]=PyInt_AsLong(PyList_GetItem(startList,i));
    if (PyErr_Occurred()||PyList_Size(startList)!=3)
    {
      PyErr_SetString( ImgErr, "Error parsing coordinate lists!" );
      
    }
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) )
  {
    sprintf(tmpStr,  "object ", dstName, " already exists",
    (char *)NULL);
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  vtkStructuredPoints *mapsp = NULL;
  vtkFloatingPointType thrval = thr;

  cvDistanceMap* distmap = new cvDistanceMap();
  if (useCityBlock== 0)
  {
      distmap->setUse26ConnectivityDistance();
  }
  int status = distmap->createDistanceMap(sp,thrval,start);

  if ( status == SV_ERROR )
  {
    sprintf(tmpStr, "Problem creating distance map for ", srcName);
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  cvStrPts *repossp = new cvStrPts( distmap->getDistanceMap() );
  delete distmap;

  repossp->SetName( dstName );
  if ( !( gRepository->Register( repossp->GetName(), repossp ) ) )
  {
    sprintf(tmpStr, "error registering obj ", dstName,
    " in repository"  );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    delete repossp;
    
  }

  return Py_BuildValue("s", repossp->GetName());

}


// -----------------
// Image_FindPathCmd
// -----------------

PyObject *Image_FindPathCmd( PyObject *self, PyObject *args )
{
 // char *usage;

  char *srcName;
  char *dstName;

  PyObject* stopList;
  int useCityBlock = 1;
  int maxIter = -1;
  int minqstop = 0;


  if (!PyArg_ParseTuple(args,"sOs|iii",&srcName,&stopList,
          &dstName,&useCityBlock,&maxIter,&minqstop))
  {
    PyErr_SetString(ImgErr,"Could not import 2 chars, 1 tuple or 3 optional int: srcName,stopList,dstName, useCityBlock,maIter,minqstop");
    
  }
  // Do work of command
  char tmpStr[1024];
  tmpStr[0]='\0';

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *sp;

  // Look up given image object:
  img = gRepository->GetObject( srcName );
  if ( img == NULL )
  {
    sprintf(tmpStr, "couldn't find object ", srcName  );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T )
  {
    sprintf(tmpStr, "error: object ", srcName,
    "not of type StructuredPts"  );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  // Retrive geometric information:
  sp = ((cvStrPts*)img)->GetVtkStructuredPoints();

  int nstart;
  int stop[3];

  // Parse coordinate lists:
  for (int i = 0; i < 3; i++)
  {
    stop[i]=PyInt_AsLong(PyList_GetItem(stopList,i));
    if (PyErr_Occurred()||PyList_Size(stopList)!=3)
    {
      PyErr_SetString( ImgErr, "Error parsing coordinate lists!" );
      
    }
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    sprintf(tmpStr, "object ", dstName, " already exists");
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  vtkStructuredPoints *mapsp = NULL;

  cvDistanceMap* distmap = new cvDistanceMap();
  distmap->setDistanceMap(sp);
  if (useCityBlock ==0) {
      distmap->setUse26ConnectivityDistance();
  }
  vtkPolyData *pd;
  if (maxIter < 0) {
    pd = distmap->getPath(stop,minqstop);
  } else {
    pd = distmap->getPathByThinning(stop,minqstop,maxIter);
  }

  if ( pd == NULL ) {
    sprintf(tmpStr, "Problem finding path for ", srcName);
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  cvPolyData *dst = new cvPolyData (pd);

  dst->SetName( dstName );
  if ( !( gRepository->Register( dst->GetName(), dst ) ) )
  {
    sprintf(tmpStr, "error registering obj ", dstName,
    " in repository" );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    delete dst;
    
  }

  //instead of exporting the object name, output the vtkPolydata object
  PyObject* pyVtkObj=vtkPythonUtil::GetObjectFromPointer(pd);
  return pyVtkObj;

}


// --------------------
// Image_MaskInPlaceCmd
// --------------------

PyObject* Image_MaskInPlaceCmd( PyObject *self, PyObject *args)
{

  char *objName;
  char *maskName;

  double replaceVal = 0;
  int notval = 0;

  if (!PyArg_ParseTuple(args,"ss|di",&objName,&maskName,&replaceVal,&notval))
  {
    PyErr_SetString(ImgErr,"Could not import 2 chars or 1 optional double, 1 optional int: objName,maskName,replaceVal, notval");
    
  }

  // Do work of command
  char tmpStr[1024];
  tmpStr[0]='\0';

  RepositoryDataT type;
  cvRepositoryData *img, *mask;
  vtkStructuredPoints *imgsp, *masksp;

  // Look up given image object:
  img = gRepository->GetObject( objName );
  if ( img == NULL )
  {
    sprintf(tmpStr, "couldn't find object ", objName  );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T )
  {
    sprintf(tmpStr, "error: object ", objName,
    "not of type StructuredPts"  );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }


  // Look up given mask object:
  mask = gRepository->GetObject( maskName );
  if ( mask == NULL )
  {
    sprintf(tmpStr, "couldn't find object ", maskName   );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  // Make sure mask is of type STRUCTURED_PTS_T:
  type = mask->GetType();
  if ( type != STRUCTURED_PTS_T )
  {
    sprintf(tmpStr, "error: object ", maskName,
    "not of type StructuredPts" );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }


  // Retrive geometric information:
  imgsp = ((cvStrPts*)img)->GetVtkStructuredPoints();
  masksp = ((cvStrPts*)mask)->GetVtkStructuredPoints();

  bool notvalBool = (notval!=0);
  int status = MaskImageInPlace(imgsp,masksp,replaceVal,notvalBool);

  if ( status == SV_ERROR )
  {
    sprintf(tmpStr, "Problem masking in place for ", objName );
    PyErr_SetString( ImgErr, tmpStr );
    tmpStr[0]='\0';
    
  }

  return Py_BuildValue("s",img->GetName());

}


//All functions listed and initiated as pyImage_methods declared here
// --------------------
// pyImage_methods
// --------------------
PyMethodDef pyImage_methods[] = {
  {"ReadHeader_5X", Image_ReadHeaderCmd, METH_VARARGS,NULL},
  {"Decode", Image_DecodeCmd, METH_VARARGS,NULL},
  {"CalcCorrectionEqn", Image_CalcCorrectionEqnCmd, METH_VARARGS,NULL},
  {"CalcCorrectionEqnAuto", Image_CalcCorrectionEqnAutoCmd, METH_VARARGS,NULL},
  {"SetImageThreshold", Image_ThresholdCmd, METH_VARARGS,NULL},
  {"ComputeStructuredCoord", Image_ComputeStructuredCoordCmd, METH_VARARGS,NULL},
  {"CreateDistanceMap", Image_CreateDistanceMapCmd, METH_VARARGS,NULL},
  {"FindPath", Image_FindPathCmd, METH_VARARGS,NULL},
  {"Mask", Image_MaskInPlaceCmd, METH_VARARGS,NULL},
  {NULL, NULL,0,NULL},
  };

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyImagemodule = {
   PyModuleDef_HEAD_INIT,
   "pyImage",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyImage_methods
};
#endif

// --------------------
// initpyImage
// --------------------
#if PYTHON_MAJOR_VERSION == 2

PyMODINIT_FUNC
initpyImage(void)
{
  PyObject *pyIm;

  pyIm = Py_InitModule("pyImage",pyImage_methods);

  ImgErr = PyErr_NewException("pyImage.error",NULL,NULL);
  Py_INCREF(ImgErr);
  PyModule_AddObject(pyIm,"error",ImgErr);
  return;

}
#endif
#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC
PyInit_pyImage(void)
{
  PyObject *pyIm;

  pyIm = PyModule_Create(&pyImagemodule);
  ImgErr = PyErr_NewException("pyImage.error",NULL,NULL);
  Py_INCREF(ImgErr);
  PyModule_AddObject(pyIm,"error",ImgErr);

  return pyIm;
}
#endif


