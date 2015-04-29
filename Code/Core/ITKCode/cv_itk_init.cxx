/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "SimVascular.h" 

#include "vtkStructuredPoints.h"
#include "vtkSystemIncludes.h"
#include <vtkImageReslice.h>     // To expand pad voxel space by background value EDIT
#include <vtkImageConstantPad.h>
#include "a2itk.h"

#include "cv_misc_utils.h"
#include "cv_arg.h"
#include "cvRepository.h"
#include "cvStrPts.h"

#include "cv_itk_init.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

// Prototypes:
// -----------

// gdscITK commands
// ----------------

int gdscITK_ReadRegionOfInterestCmd( ClientData clientData, Tcl_Interp *interp,
				     int argc, CONST84 char *argv[] );
int gdscITK_MinMaxCurvSmoothCmd(ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );
int gdscITK_SigMapCmd(ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] );
int gdscITK_FastMarchCmd(ClientData clientData, Tcl_Interp *interp,
			 int argc, CONST84 char *argv[] );
int gdscITK_TLSF_Cmd(ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

int gdscITK_createSignedDistanceImageWithFMMCmd( ClientData clientData, Tcl_Interp *interp,                    int argc, CONST84 char *argv[] );
int gdscITK_expNegativeImageCmd( ClientData clientData, Tcl_Interp *interp,
                    int argc, CONST84 char *argv[] );
int gdscITK_shapeDetectionCmd( ClientData clientData, Tcl_Interp *interp,
                    int argc, CONST84 char *argv[] );

// gdscITK methods
// ---------------

// ------------
// gdscITK_Init
// ------------
// Bind action-oriented commands to Tcl interp.

extern "C" CV_DLL_EXPORT int Simvascularitk_Init( Tcl_Interp *interp )
{
  Tcl_CreateCommand( interp, "itk_readRegionOfInterest", gdscITK_ReadRegionOfInterestCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "itk_minMaxCurvSmooth", gdscITK_MinMaxCurvSmoothCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL );
  Tcl_CreateCommand( interp, "itk_sigMap", gdscITK_SigMapCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);
  Tcl_CreateCommand( interp, "itk_fastMarch", gdscITK_FastMarchCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);
  Tcl_CreateCommand( interp, "itk_TLSF", gdscITK_TLSF_Cmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);

  Tcl_CreateCommand( interp, "itk_createDistImage", gdscITK_createSignedDistanceImageWithFMMCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);
  Tcl_CreateCommand( interp, "itk_expNegativeImage", gdscITK_expNegativeImageCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);
  Tcl_CreateCommand( interp, "itk_shapeDetection", gdscITK_shapeDetectionCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);
  return TCL_OK;
}

template<typename PixelType>
static int TLSFTypeHelper(vtkStructuredPoints *sp, vtkStructuredPoints *spLset, Tcl_Interp *interp, 
						  double upper, double lower, double prop, double curv, int numIter, char *levelsetName)
{
	int testType = sp->GetScalarType();
	int status;
	switch(testType){
  case VTK_SHORT:
	  status = TLSFHelper<PixelType, short>(sp, spLset, interp, upper, lower, prop, curv, numIter, levelsetName);break;
  case VTK_INT:
	  status = TLSFHelper<PixelType, int>(sp, spLset, interp, upper, lower, prop, curv, numIter, levelsetName);break;
  case VTK_DOUBLE:
	  status = TLSFHelper<PixelType, double>(sp, spLset, interp, upper, lower, prop, curv, numIter, levelsetName);break; 
  case VTK_FLOAT:
	  status = TLSFHelper<PixelType, float>(sp, spLset, interp, upper, lower, prop, curv, numIter, levelsetName);break;
  default:
	  std::cerr<<"Could not interpret type of vtkStructuredPoints data."<<std::endl; status = TCL_ERROR;
	}
	return status;
}

template<typename PixelType1, typename PixelType2>
static int TLSFHelper(vtkStructuredPoints *sp, vtkStructuredPoints *spLset, Tcl_Interp *interp, 
		      double upper, double lower, double prop, double curv, int numIter, char *levelsetName)
{

  a2itk<3, PixelType1, PixelType2, float> *itker = new a2itk<3, PixelType1, PixelType2, float>();

	vtkStructuredPoints *lsetImg;

  try{
	itker->setInput1(spLset);
	itker->setInput2(sp);
	itker->itkThresholdLevelSet( upper, lower, prop, curv, numIter );
	lsetImg = itker->getOutput();  
  }catch(itk::ExceptionObject &e){
	  delete itker;
	  Tcl_AppendResult( interp, "ITK exception: ", e.what(), (char*) NULL);
	  return TCL_ERROR;
  }

  cvStrPts *gdscSP = new cvStrPts( lsetImg );
  
  char* newName = (char *) malloc(strlen(levelsetName) + 6);
  strcpy(newName, levelsetName);
  strcat(newName, "_lset");

  gRepository->UnRegister( newName );
  gdscSP->SetName( newName );
  
  if ( !( gRepository->Register( gdscSP->GetName(), gdscSP ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", newName,
			" in repository", (char *)NULL );
    delete gdscSP;
	delete itker;
    free(newName);
    return TCL_ERROR;
  }


  Tcl_SetResult( interp, gdscSP->GetName(), TCL_VOLATILE );
  delete itker;
  free(newName);
  return TCL_OK;  

}

/**
 * gdscITK_TLSF_Cmd
 */

int gdscITK_TLSF_Cmd(ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] ){
  char *usage;
  
  char *imageName;
  char *levelsetName;

  double upper,lower,prop,curv;
  int numIter;

  int table_sz=7;
  ARG_Entry arg_table[]={
    {"-image", STRING_Type, &imageName, NULL, REQUIRED, 0, { 0 } },
    {"-levelset", STRING_Type, &levelsetName, NULL, REQUIRED, 0, { 0 } },
    {"-upperThreshold", DOUBLE_Type, &upper, NULL, REQUIRED, 0, { 0 } },
    {"-lowerThreshold", DOUBLE_Type, &lower, NULL, REQUIRED, 0, { 0 } },
    {"-propagationScaling", DOUBLE_Type, &prop, NULL, REQUIRED, 0, { 0 } },
    {"-curvatureScaling", DOUBLE_Type, &curv, NULL, REQUIRED, 0, { 0 } },
    {"-numberOfIterations", INT_Type, &numIter, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr(1, argv, table_sz, arg_table);

  if( argc == 1) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE);
    return TCL_OK;
  }

  if( ARG_ParseTclStr( interp, argc, argv, 1, 
		       table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  // Do work of command

  RepositoryDataT type;
  cvRepositoryData *img;
  cvRepositoryData *imgLset;
  vtkStructuredPoints *sp;
  vtkStructuredPoints *spLset;

  // Look up given image object:
  img = gRepository->GetObject( imageName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", imageName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", imageName,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }

  // Look up given image object:
  imgLset = gRepository->GetObject( levelsetName );
  if ( imgLset == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", levelsetName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = imgLset->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", levelsetName,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }

  // Retrive geometric information:
  sp = ((cvStrPts*)img)->GetVtkStructuredPoints();
  spLset = ((cvStrPts*)imgLset)->GetVtkStructuredPoints();

  ARG_FreeListArgvs( table_sz, arg_table );

  int testType = spLset->GetScalarType();

  int status;

  switch(testType){
  case VTK_SHORT:
    //// NEED TO FIX!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    //    status = TLSFTypeHelper<short>(sp, spLset, interp, upper, lower, prop, curv, numIter, levelsetName);break;
  case VTK_INT:
    // NEED TO FIX!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // status = TLSFTypeHelper<int>(sp, spLset, interp, upper, lower, prop, curv, numIter, levelsetName);break;
  case VTK_DOUBLE:
    // NEED TO FIX!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // status = TLSFTypeHelper<double>(sp, spLset, interp, upper, lower, prop, curv, numIter, levelsetName);break; 
  case VTK_FLOAT:
    // NEED TO FIX!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    // status = TLSFTypeHelper<float>(sp, spLset, interp, upper, lower, prop, curv, numIter, levelsetName);break;
  default:
    std::cerr<<"Could not interpret type of vtkStructuredPoints data."<<std::endl; status = TCL_ERROR;
  }

  return status;
}


template<typename PixelType>
static int FMMHelper(vtkStructuredPoints *sp, Tcl_Interp *interp, char* levelSetName, double start, 
		     double stop, int nlistpts, ARG_List *indpts, int npts, int table_sz, ARG_Entry arg_table[])
{
  a2itk<3, PixelType> *itker = new a2itk<3, PixelType>();
  double pt[3];
  int numSeeds = 0;
  double seeds[itker->SEEDS_ARRAY_MAX][3];

  for (int i = 0; i < nlistpts; i++) {
    if ( ARG_ParseTclListStatic( interp, indpts[i], DOUBLE_Type, pt, 3, &npts )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete [] indpts;
      return TCL_ERROR;
    }
    if ( npts != 3 ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete [] indpts;
      return TCL_ERROR;
    }
    if(i >= itker->SEEDS_ARRAY_MAX ){
      Tcl_SetResult( interp, "too many seeds", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete [] indpts;
      return TCL_ERROR;
    }
    seeds[i][0] = pt[0];
    seeds[i][1] = pt[1]; 
    seeds[i][2] = pt[2];   
    numSeeds++;
  }

  delete [] indpts;

  ARG_FreeListArgvs( table_sz, arg_table );
  vtkStructuredPoints *fmmImg;
  try{
	itker->setInput1(sp);
	itker->itkFmmFilter( seeds, numSeeds, start, stop );
	fmmImg = itker->getOutput(); 
  }catch(itk::ExceptionObject &e){
	delete itker;
	Tcl_AppendResult( interp, "ITK exception: ", e.what(), (char*) NULL);
    return TCL_ERROR;
  }

  cvStrPts *gdscSP = new cvStrPts( fmmImg );
  
  char* newName = (char *) malloc(strlen(levelSetName) + 11);
  strcpy(newName, levelSetName);
  strcat(newName, "_fmm");

  gRepository->UnRegister( newName );
  gdscSP->SetName( newName );
  if ( !( gRepository->Register( gdscSP->GetName(), gdscSP ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", newName,
			" in repository", (char *)NULL );
    delete gdscSP;

    free(newName);
    return TCL_ERROR;
  }


  Tcl_SetResult( interp, gdscSP->GetName(), TCL_VOLATILE );
  delete itker;
  free(newName);
  return TCL_OK;
}

/**
 * gdscITK_FastMarchCmd
 */

int gdscITK_FastMarchCmd(ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] ){
  char *usage;
  
  char *imageName;
  char *levelSetName;
  double start;
  double stop;

  ARG_List seedsList;
  int table_sz=5;
  ARG_Entry arg_table[]={
    {"-image", STRING_Type, &imageName, NULL, REQUIRED, 0, { 0 } },
    {"-output", STRING_Type, &levelSetName, NULL, REQUIRED, 0, { 0 } },
    { "-seedPts", LIST_Type, &seedsList, NULL, REQUIRED, 0, { 0 } },
    {"-start", DOUBLE_Type, &start, NULL, REQUIRED, 0, { 0 } },
    {"-stop", DOUBLE_Type, &stop, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr(1, argv, table_sz, arg_table);

  if( argc == 1) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE);
    return TCL_OK;
  }

  if( ARG_ParseTclStr( interp, argc, argv, 1, 
		       table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE);
    return TCL_ERROR;
  }

  // Do work of command
  
  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *sp;

  // Look up given image object:
  img = gRepository->GetObject( imageName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", imageName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", imageName,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }

  // Retrive geometric information:
  sp = ((cvStrPts*)img)->GetVtkStructuredPoints();

  // create list of seed points

  int nlistpts = seedsList.argc;
  int npts = 0;

  ARG_List *indpts = new ARG_List [nlistpts];

  if ( ARG_ParseTclListStatic( interp, seedsList, LIST_Type, indpts, nlistpts, &npts )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    delete indpts;
    return TCL_ERROR;
  }

  int testType = sp->GetScalarType();

  int status;

  switch(testType){
  case VTK_SHORT:
    status = FMMHelper<short>(sp, interp, levelSetName, start, stop, nlistpts, 
			      indpts, npts, table_sz, arg_table);break;
  case VTK_FLOAT:
    status = FMMHelper<float>(sp, interp, levelSetName, start, stop, nlistpts, 
			      indpts, npts, table_sz, arg_table);break;
  case VTK_INT:
    status = FMMHelper<int>(sp, interp, levelSetName, start, stop, nlistpts, 
			      indpts, npts, table_sz, arg_table);break;
  case VTK_DOUBLE:
    status = FMMHelper<double>(sp, interp, levelSetName, start, stop, nlistpts, 
			      indpts, npts, table_sz, arg_table);break;
  default:
    std::cerr<<"Could not interpret type of vtkStructuredPoints data."<<std::endl; status = TCL_ERROR;
  }

  return status;

}

template<typename PixelType>
static int SigMapHelper(vtkStructuredPoints *sp, Tcl_Interp *interp, char* imageName, double min, 
			double max, double alpha, double beta)
{
  a2itk<3, PixelType, PixelType, float> *itker = new a2itk<3, PixelType, PixelType, float>();
  vtkStructuredPoints *sigmappedImg;
  
  try{
	itker->setInput1(sp);
	itker->itkSigMapFilter(min, max, alpha, beta);
	sigmappedImg = itker->getOutput();
  }catch(itk::ExceptionObject &e){
    delete itker;
	Tcl_AppendResult( interp, "ITK exception: ", e.what(), (char*) NULL);
    return TCL_ERROR;
  }
   

  cvStrPts *gdscSP = new cvStrPts( sigmappedImg );
  
  char* newName = (char *) malloc(strlen(imageName) + 11);
  strcpy(newName, imageName);
  strcat(newName, "_sigmapped");

  gRepository->UnRegister( newName );
  gdscSP->SetName( newName );
  if ( !( gRepository->Register( gdscSP->GetName(), gdscSP ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", newName,
			" in repository", (char *)NULL );
    delete gdscSP;

    free(newName);
    return TCL_ERROR;
  }


  Tcl_SetResult( interp, gdscSP->GetName(), TCL_VOLATILE );
  delete itker;
  
  free(newName);
  return TCL_OK;
}

/**
 * gdscITK_SigMapCmd
 
 */

int gdscITK_SigMapCmd(ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] ){
  char *usage;
  
  char *imageName;
  char *inFile;
  char *outFile;
  double min,max,alpha,beta;

  int table_sz=5;
  ARG_Entry arg_table[]={
    {"-image", STRING_Type, &imageName, NULL, REQUIRED, 0, { 0 } },
    {"-min", DOUBLE_Type, &min, NULL, REQUIRED, 0, { 0 } },
    {"-max", DOUBLE_Type, &max, NULL, REQUIRED, 0, { 0 } },
    {"-alpha", DOUBLE_Type, &alpha, NULL, REQUIRED, 0, { 0 } },
    {"-beta", DOUBLE_Type, &beta, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr(1, argv, table_sz, arg_table);

  if( argc == 1) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE);
    return TCL_OK;
  }

  if( ARG_ParseTclStr( interp, argc, argv, 1, 
		       table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE);
 
    return TCL_ERROR;
  }

  // Do work of command
  
  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *sp;

  // Look up given image object:
  img = gRepository->GetObject( imageName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", imageName, (char *)NULL );
    
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", imageName,
		      "not of type StructuredPts", (char *)NULL );
    
    return TCL_ERROR;
  }

  // Retrive geometric information:
  sp = ((cvStrPts*)img)->GetVtkStructuredPoints();

  ARG_FreeListArgvs( table_sz, arg_table );

  int testType = sp->GetScalarType();

  int status;

  switch(testType){
  case VTK_SHORT:
    status = SigMapHelper<short>(sp, interp, imageName, min, max, alpha, beta);break;
  case VTK_FLOAT:
    status = SigMapHelper<float>(sp, interp, imageName, min, max, alpha, beta);break;
  case VTK_INT:
    status = SigMapHelper<int>(sp, interp, imageName, min, max, alpha, beta);break;
  case VTK_DOUBLE:
    status = SigMapHelper<double>(sp, interp, imageName, min, max, alpha, beta);break;
  default:
    std::cerr<<"Could not interpret type of vtkStructuredPoints data."<<std::endl; status = TCL_ERROR;
  }

  return status;
}


template<typename PixelType>
static int SmoothHelper(vtkStructuredPoints *sp, Tcl_Interp *interp,int numIter, int radius, 
			double timeStep, char* imageName)
{
  a2itk<3, PixelType> *itker = new a2itk<3, PixelType>();
  vtkStructuredPoints *smoothedImg;
  try{
	itker->setInput1(sp);
	itker->itkMinMaxCurvFlowFilter((unsigned int)numIter, (long)radius, timeStep);
	smoothedImg = itker->getOutput();
  }catch(itk::ExceptionObject &e){
    delete itker;	
	Tcl_AppendResult( interp, "ITK exception: ", e.what(), (char*) NULL);
    return TCL_ERROR;
  }
  
  cvStrPts *gdscSP = new cvStrPts( smoothedImg );

  char* newName = (char *) malloc(strlen(imageName) + 10);
  strcpy(newName, imageName);
  strcat(newName, "_smoothed");

  gRepository->UnRegister( newName );
  gdscSP->SetName( newName );
  if ( !( gRepository->Register( gdscSP->GetName(), gdscSP ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", newName,
			" in repository", (char *)NULL );
    delete gdscSP;

    free(newName);
    return TCL_ERROR;
  }


  Tcl_SetResult( interp, gdscSP->GetName(), TCL_VOLATILE );
  delete itker;
  free(newName);
  return TCL_OK;
}

/**
 * gdscITK_MinMaxCurvSmoothCmd
 **/

int gdscITK_MinMaxCurvSmoothCmd(ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] ){
  char *usage;
  
  char *imageName;
  int numIter;
  int radius;
  double timeStep;

  int table_sz=4;
  ARG_Entry arg_table[]={
    {"-image", STRING_Type, &imageName, NULL, REQUIRED, 0, { 0 } },
    {"-numIterations", INT_Type, &numIter, NULL, REQUIRED, 0, { 0 } },
    {"-stencilRadius", INT_Type, &radius, NULL, REQUIRED, 0, { 0 } },
    {"-timeStep", DOUBLE_Type, &timeStep, NULL, REQUIRED, 0, { 0 } },
  };

  usage = ARG_GenSyntaxStr(1, argv, table_sz, arg_table);

  if( argc == 1) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE);
    return TCL_OK;
  }

  if( ARG_ParseTclStr( interp, argc, argv, 1, 
		       table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE);

    return TCL_ERROR;
  }

  ARG_FreeListArgvs( table_sz, arg_table );

  // Do work of command
  
  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *sp;

  // Look up given image object:
  img = gRepository->GetObject( imageName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", imageName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", imageName,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }

  // Retrive geometric information:
  sp = ((cvStrPts*)img)->GetVtkStructuredPoints();

  int testType = sp->GetScalarType();

  int status;

  /* short and int don't seem to be supported */
  switch(testType){
  case VTK_SHORT:
    status = SmoothHelper<short>(sp, interp, numIter, radius, timeStep, imageName);break;
  case VTK_FLOAT:
    status = SmoothHelper<float>(sp, interp, numIter, radius, timeStep, imageName);break;
  case VTK_DOUBLE:
    status = SmoothHelper<double>(sp, interp, numIter, radius, timeStep, imageName);break;
  default:
    std::cerr<<"Could not interpret type of vtkStructuredPoints data."<<std::endl; status = TCL_ERROR;
  }

  return status;
}

// Pads @param imageToPad with @param fillValue.  Padding is @param padding thick.
static void PadVtkStructuredPoints( vtkStructuredPoints* imageToPad, int padding, float fillValue ) {
	int* dimensions = imageToPad->GetDimensions();
  int xDim = dimensions[0];
  int yDim = dimensions[1];
  int zDim = dimensions[2];

	int testType = imageToPad->GetScalarType();

	vtkDataArray *scalars = imageToPad->GetPointData()->GetScalars();
	vtkDataArray *newScalars;

  switch(testType){
  case VTK_SHORT:
		newScalars = vtkShortArray::New();
		break;
  case VTK_FLOAT:
    newScalars = vtkFloatArray::New();
		break;
  case VTK_INT:
    newScalars = vtkIntArray::New();
		break;
  case VTK_DOUBLE:
		newScalars = vtkDoubleArray::New();
		break;
  default:
    std::cerr<<"Could not interpret type of vtkStructuredPoints data."<<std::endl;
		return;
  }

	

	int xDimNew = xDim + 2 * padding;
	int yDimNew = yDim + 2 * padding;
	int zDimNew = zDim + 2 * padding;

	newScalars->SetNumberOfTuples(xDimNew * yDimNew * zDimNew);

	struct MapToOneDArray {
		MapToOneDArray(int dimx, int dimy): dimx(dimx), dimy(dimy){};

		int operator()(int x, int y, int z){
			return x+y*dimx+z*dimx*dimy;
		}

		int dimx;
		int dimy;
	};

	struct InPaddedRegionT {
		InPaddedRegionT(int dimx, int dimy, int dimz, int padding) 
			: dimx(dimx), dimy(dimy), dimz(dimz), padding(padding){};

		bool operator()(int x, int y, int z){
			return (x < padding || x >= dimx + padding) ||
			       (y < padding || y >= dimy + padding) ||
						 (z < padding || z >= dimz + padding);
		}

		int dimx;
		int dimy;
		int dimz;
		int padding;
	};

	MapToOneDArray newMapper(xDimNew, yDimNew);
	MapToOneDArray oldMapper(xDim, yDim);

	InPaddedRegionT inPaddedRegion(xDim, yDim, zDim, padding);
	
	for( int x = 0; x < xDimNew; x++) {
		for( int y = 0; y < yDimNew; y++) {
			for( int z = 0; z < zDimNew; z++) {
				if( inPaddedRegion( x, y, z) )
					newScalars->SetTuple1( newMapper(x, y, z) , fillValue);
				else {
					float newScalar = scalars->GetTuple1( oldMapper(x - padding, y - padding, z - padding) );
					newScalars->SetTuple1( newMapper(x, y, z) , newScalar);
				}
			}
		}
	}

	imageToPad->SetDimensions( xDimNew, yDimNew, zDimNew );
	imageToPad->GetPointData()->SetScalars(newScalars);
	imageToPad->Update();
}

template<typename PixelType>
static int ReadRegionHelper(vtkStructuredPoints* sp, Tcl_Interp *interp, int start[3], 
			    int dims[3], double spacing, char* imageName)
{
  a2itk<3, PixelType> *itker = new a2itk<3, PixelType>();
  vtkStructuredPoints *resampledImg;
  vtkStructuredPoints *tmp = NULL;

	try{
		itker->setInput1(sp);
		itker->itkRegionOfInterestFilter(start, dims);
		tmp = itker->getOutput();
		itker->setInput1( tmp );
		itker->itkResampleFilter(spacing, dims);
		tmp->Delete();
		resampledImg = itker->getOutput();
	}catch(itk::ExceptionObject &e){
		delete itker;
		if( tmp != NULL)
			tmp->Delete();
		Tcl_AppendResult( interp, "ITK exception: ", e.what(), (char*) NULL);
		return TCL_ERROR;
	}

	//add 5 voxel zero-padding to resampledImg to create paddedResampledImg

	const int padding = 5;
	const int fillValue = 0;
	PadVtkStructuredPoints( resampledImg, padding, fillValue );

	cvStrPts *gdscSP = new cvStrPts( resampledImg );

  char* newName = (char *) malloc(strlen(imageName) + 11);
  strcpy(newName, imageName);
  strcat(newName, "_resampled");

  gRepository->UnRegister( newName );
  gdscSP->SetName( newName );
  if ( !( gRepository->Register( gdscSP->GetName(), gdscSP ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", newName,
			" in repository", (char *)NULL );
    delete gdscSP;

    free(newName);
    return TCL_ERROR;
  }


  Tcl_SetResult( interp, gdscSP->GetName(), TCL_VOLATILE );
  delete itker;
  resampledImg->Delete();
  free(newName);
  return TCL_OK;
}

// -------------------------------
// gdscITK_ReadRegionOfInterestCmd
// -------------------------------

int gdscITK_ReadRegionOfInterestCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;

  char *imageName;
  ARG_List startList,dimsList;
  double spacing;

  int table_sz = 4;
  ARG_Entry arg_table[] = {
    { "-image", STRING_Type, &imageName, NULL, REQUIRED, 0, { 0 } },
    { "-start", LIST_Type, &startList, NULL, REQUIRED, 0, { 0 } },
    { "-dims", LIST_Type, &dimsList, NULL, REQUIRED, 0, { 0 } },
    { "-spacing", DOUBLE_Type, &spacing, NULL, REQUIRED, 0, { 0 } },
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
  
  int nstart,ndims;
  int start[3],dims[3];
  
  // Parse coordinate lists:
  if ( ARG_ParseTclListStatic( interp, startList, INT_Type, start, 3, &nstart )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, dimsList, INT_Type, dims, 3, &ndims )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }

  ARG_FreeListArgvs( table_sz, arg_table );

  // Do work of command

  RepositoryDataT type;
  cvRepositoryData *img;
  vtkStructuredPoints *sp;

  // Look up given image object:
  img = gRepository->GetObject( imageName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", imageName, (char *)NULL );

    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", imageName,
		      "not of type StructuredPts", (char *)NULL );

    return TCL_ERROR;
  }

  // Retrive geometric information:
  sp = ((cvStrPts*)img)->GetVtkStructuredPoints();

  int testType = sp->GetScalarType();

  int status;

  switch(testType){
  case VTK_SHORT:
    status = ReadRegionHelper<short>(sp, interp, start, dims, spacing, imageName);break;
  case VTK_FLOAT:
    status = ReadRegionHelper<float>(sp, interp, start, dims, spacing, imageName);break;
  case VTK_INT:
    status = ReadRegionHelper<int>(sp, interp, start, dims, spacing, imageName);break;
  case VTK_DOUBLE:
    status = ReadRegionHelper<double>(sp, interp, start, dims, spacing, imageName);break;
  default:
    std::cerr<<"Could not interpret type of vtkStructuredPoints data."<<std::endl; status = TCL_ERROR;
  }

  return status;
}



// ---------------------------------------------
// gdscITK_createSignedDistanceImageWithFMMCmd
// ---------------------------------------------

int gdscITK_createSignedDistanceImageWithFMMCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;

  RepositoryDataT type;
  int status;
  cvStrPts *sp;
  char *objName;
  ARG_List seedsList,originList,spacingList,dimsList,startList;
  double time;

  int table_sz = 7;
  ARG_Entry arg_table[] = {
    { "-seedPts", LIST_Type, &seedsList, NULL, REQUIRED, 0, { 0 } },
    { "-time", DOUBLE_Type, &time, NULL, REQUIRED, 0, { 0 } },
    { "-origin", LIST_Type, &originList, NULL, REQUIRED, 0, { 0 } },
    { "-spacing", LIST_Type, &spacingList, NULL, REQUIRED, 0, { 0 } },
    { "-dims", LIST_Type, &dimsList, NULL, REQUIRED, 0, { 0 } },
    { "-start", LIST_Type, &startList, NULL, REQUIRED, 0, { 0 } },
    { "-return", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
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

  int norigin,nspacing, ndims, nstart;
  double origin[3],spacing[3];
  int dims[3],start[3];
 
  // Parse lists

  if ( ARG_ParseTclListStatic( interp, originList, DOUBLE_Type, origin, 3, &norigin )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, spacingList, DOUBLE_Type, spacing, 3, &nspacing )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, dimsList, INT_Type, dims, 3, &ndims )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }
  if ( ARG_ParseTclListStatic( interp, startList, INT_Type, start, 3, &nstart )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }

  // create list of seed points

  int nlistpts = seedsList.argc;
  int npts = 0;

  ARG_List *indpts = new ARG_List [nlistpts];

  if ( ARG_ParseTclListStatic( interp, seedsList, LIST_Type, indpts, nlistpts, &npts )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    delete indpts;
    return TCL_ERROR;
  }

  int i;
  double *seeds = new double[3*npts];
  double pt[3];
  int numSeeds = 0;

  for (i = 0; i < nlistpts; i++) {
    if ( ARG_ParseTclListStatic( interp, indpts[i], DOUBLE_Type, pt, 3, &npts )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete [] indpts;
      delete [] seeds;
      return TCL_ERROR;
    }
    if ( npts != 3 ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete [] indpts;
      delete [] seeds;
      return TCL_ERROR;
    }
    seeds[3*i+0] = pt[0];
    seeds[3*i+1] = pt[1]; 
    seeds[3*i+2] = pt[2];   
    numSeeds++;
  }

  delete [] indpts;

  for (i = 0; i < nlistpts; i++) {
    fprintf(stdout,"Point %i:  %lf %lf\n",i,seeds[3*i+0],seeds[3*i+1]);
  }

  ARG_FreeListArgvs( table_sz, arg_table );

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "obj ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  vtkStructuredPoints *img = NULL;

  if (dims[2] > 1) {

    a2itk<3>* itker = new a2itk<3>;
    status = itker->createSignedDistanceImageWithFMM (numSeeds, seeds,
                                                   origin,spacing,dims,start,time);

    delete [] seeds;

    if (status == CV_ERROR) {
      Tcl_SetResult( interp, "error creating 2D image", TCL_VOLATILE );
       delete itker;
       return TCL_ERROR;
    }

    img = itker->getOutput();

    sp = new cvStrPts( img );
    sp->SetName( objName );
    if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
      delete sp;
      return TCL_ERROR;
    }

    delete itker;

  } else {

    a2itk<2>* itker = new a2itk<2>;
    status = itker->createSignedDistanceImageWithFMM (numSeeds, seeds,
                                                   origin,spacing,dims,start,time);

    delete [] seeds;

    if (status == CV_ERROR) {
      Tcl_SetResult( interp, "error creating 2D image", TCL_VOLATILE );
       delete itker;
       return TCL_ERROR;
    }

    img = itker->getOutput();

    sp = new cvStrPts( img );
    sp->SetName( objName );
    if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
      delete sp;
      return TCL_ERROR;
    }

    delete itker;

  }
  

  Tcl_SetResult( interp, sp->GetName(), TCL_VOLATILE );
  return TCL_OK;

}




// -----------------------------
// gdscITK_expNegativeImageCmd
// -----------------------------

int gdscITK_expNegativeImageCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;

  int status;
  char *srcName, *dstName;
  double factor;

  int table_sz = 3;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-factor", DOUBLE_Type, &factor, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
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
  cvStrPts *sp;

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
  vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "obj ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  vtkStructuredPoints *vout = NULL;

  if ((vtksp->GetDimensions())[2] > 1) {

    a2itk<3>* itker = new a2itk<3>;
    itker->setInput1(vtksp);
    status = itker->expNegativeImageFilter(factor);

    if (status == CV_ERROR) {
      Tcl_SetResult( interp, "error creating 2D image", TCL_VOLATILE );
      delete itker;
      return TCL_ERROR;
    }

    vout = itker->getOutput();

    sp = new cvStrPts( vout );
    sp->SetName( dstName );
    if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
      delete sp;
      return TCL_ERROR;
    }

    delete itker;

  } else {

    a2itk<2>* itker = new a2itk<2>;
    itker->setInput1(vtksp);
    status = itker->expNegativeImageFilter(factor);

    if (status == CV_ERROR) {
      Tcl_SetResult( interp, "error creating 2D image", TCL_VOLATILE );
      delete itker;
      return TCL_ERROR;
    }

    vout = itker->getOutput();

    sp = new cvStrPts( vout );
    sp->SetName( dstName );
    if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
      delete sp;
      return TCL_ERROR;
    }

    delete itker;

  }


  Tcl_SetResult( interp, sp->GetName(), TCL_VOLATILE );
  return TCL_OK;

}


// -----------------------------------
// gdscITK_shapeDetectionCmd
// -----------------------------------

int gdscITK_shapeDetectionCmd( ClientData clientData, Tcl_Interp *interp,
		  int argc, CONST84 char *argv[] )
{
  char *usage;

  int status;
  char *srcName, *featureName, *dstName;
  double propScale,curvScale,maxRMSerror;
  int maxNumIters;

  int table_sz = 7;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &srcName, NULL, REQUIRED, 0, { 0 } },
    { "-featureImg", STRING_Type, &featureName, NULL, REQUIRED, 0, { 0 } },
    { "-propScale", DOUBLE_Type, &propScale, NULL, REQUIRED, 0, { 0 } },
    { "-curvScale", DOUBLE_Type, &curvScale, NULL, REQUIRED, 0, { 0 } },
    { "-maxRMS", DOUBLE_Type, &maxRMSerror, NULL, REQUIRED, 0, { 0 } },
    { "-maxIters", INT_Type, &maxNumIters, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &dstName, NULL, REQUIRED, 0, { 0 } },
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

  vtkStructuredPoints *vPhiImg;
  vtkStructuredPoints *vFeatureImg;
  cvStrPts *sp;

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
  vPhiImg = ((cvStrPts*)img)->GetVtkStructuredPoints();

  // Look up given image object:
  img = gRepository->GetObject( featureName );
  if ( img == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", featureName, (char *)NULL );
    return TCL_ERROR;
  }

  // Make sure image is of type STRUCTURED_PTS_T:
  type = img->GetType();
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "error: object ", featureName,
		      "not of type StructuredPts", (char *)NULL );
    return TCL_ERROR;
  }

  // Retrive geometric information:
  vFeatureImg = ((cvStrPts*)img)->GetVtkStructuredPoints();

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( dstName ) ) {
    Tcl_AppendResult( interp, "obj ", dstName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  vtkStructuredPoints *vout = NULL;

  if ((vFeatureImg->GetDimensions())[2] > 1) {

    a2itk<3>* itker = new a2itk<3>;
    itker->setInput1(vPhiImg);
    itker->setInput2(vFeatureImg);

    status = itker->shapeDetectionLevelSet (propScale, curvScale,
                                maxRMSerror, maxNumIters);

    if (status == CV_ERROR) {
      Tcl_SetResult( interp, "error creating 2D image", TCL_VOLATILE );
      return TCL_ERROR;
    }

    vout = itker->getOutput();

    sp = new cvStrPts( vout );
    sp->SetName( dstName );
    if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
      delete sp;
      return TCL_ERROR;
    }

    delete itker;

  } else {

    a2itk<2>* itker = new a2itk<2>;
    itker->setInput1(vPhiImg);
    itker->setInput2(vFeatureImg);

    status = itker->shapeDetectionLevelSet (propScale, curvScale,
                                maxRMSerror, maxNumIters);

    if (status == CV_ERROR) {
      Tcl_SetResult( interp, "error creating 2D image", TCL_VOLATILE );
      return TCL_ERROR;
    }

    vout = itker->getOutput();

    sp = new cvStrPts( vout );
    sp->SetName( dstName );
    if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
      Tcl_AppendResult( interp, "error registering obj ", dstName,
		      " in repository", (char *)NULL );
      delete sp;
      return TCL_ERROR;
    }

    delete itker;

  }

  Tcl_SetResult( interp, sp->GetName(), TCL_VOLATILE );
  return TCL_OK;

}
