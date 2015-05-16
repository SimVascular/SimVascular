/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
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
 *
 *=========================================================================*/
/*
 * cv_itkls2d_init.cxx
 *
 *  Created on: Feb 12, 2014
 *      Author: Jameson Merkow
 */

#include "SimVascular.h"
#include "cvTCLMacros.h"

#include "cv_LsetCore_init.h"
#include "cvITKLevelSet.h"
#include "cvITKLevelSetBase.h"
#include "cvLevelSetVelocity.h"
#include "cvRepository.h"
#include "cvSolidModel.h"
#include "cv_misc_utils.h"
#include "cv_arg.h"

#include "Util/cvITKUtils.h"

#include "cv_ITKLset_init.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

// Helper fns
// ----------

// NewName: Checks if the name is already used
static int NewName( CONST84 char *name );
static void PrintMethods();

// Prototypes:
// -----------
typedef itk::Image<short,2> ImageType;
// cvITKLevelSets New and delete
static int itkls2d_NewCmd(CXX_TCL_STDARGS);
void Deleteitkls2d( ClientData clientData );
// cvITKLevelSets object methods
static int itkls2d_ObjectCmd( CXX_TCL_STDARGS );
static int itkls2d_SetInputsMtd( CXX_TCL_STDARGS );
static int itkls2d_PhaseOneLevelSetMtd( CXX_TCL_STDARGS );
static int itkls2d_PhaseTwoLevelSetMtd( CXX_TCL_STDARGS );
static int itkls2d_GACLevelSetMtd( CXX_TCL_STDARGS );
CVTCLMtdDeclareMacro(itkls2d,GetFront);
CVTCLMtdDeclareMacro(itkls2d,GetFrontImage);
CVTCLMtdDeclareMacro(itkls2d,GetVelocityImage);
CVTCLMtdDeclareMacro(itkls2d,WriteFront);
CVTCLMtdDeclareMacro(itkls2d,SetMaxIterations);
CVTCLMtdDeclareMacro(itkls2d,SetMaxRMSError);
CVTCLMtdDeclareMacro(itkls2d,SetDebug);
CVTCLMtdDeclareMacro(itkls2d,SetPropagationScaling);
CVTCLMtdDeclareMacro(itkls2d,SetAdvectionScaling);
CVTCLMtdDeclareMacro(itkls2d,SetCurvatureScaling);

CVTCLMtdDeclareMacro(itkls2d,SetUseNormalVectorCurvature);
CVTCLMtdDeclareMacro(itkls2d,SetUseMeanCurvature);
CVTCLMtdDeclareMacro(itkls2d,SetUseMinimalCurvature);


// cvITKLevelSet Properties
CVTCLObjMemberGetObjPropertyMacro(itkls2d,cvITKLevelSet,Front,cvPolyData);
CVTCLObjMemberGetObjPropertyMacro(itkls2d,cvITKLevelSet,VelocityImage,cvStrPts);
CVTCLObjMemberGetObjPropertyMacro(itkls2d,cvITKLevelSet,FrontImage,cvStrPts);

CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,MaxIterations,int,INT_Type);
CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,MaxRMSError,double,DOUBLE_Type);
CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,PropagationScaling,double,DOUBLE_Type);
CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,CurvatureScaling,double,DOUBLE_Type);
CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,AdvectionScaling,double,DOUBLE_Type);
CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,Debug,bool,BOOL_Type);
CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,UseInputImageAsFeature,bool,BOOL_Type);
CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,UseInputImageDistance,bool,BOOL_Type);
// CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,UseNormalVectorCurvature,bool,BOOL_Type);
// CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,UseMeanCurvature,bool,BOOL_Type);
// CVTCLObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,UseMinimalCurvature,bool,BOOL_Type);



//Helper functions
// NewName: Checks if the name is already used
static int NewName( CONST84 char *name )
{
	Tcl_HashEntry *entryPtr;
	entryPtr = Tcl_FindHashEntry( &gLsetCoreTable, name );
	if ( entryPtr != NULL ) {
		return 0;
	}
	return 1;
}


// -------------
// itkls2d_Init
// -------------
int itkls2d_Init( Tcl_Interp *interp ){

	//Usage: CVTCLFunctionInit(Prefix,FunctionName,TclName)
	CVTCLFunctionInit(itkls2d,NewCmd,ITKLevelSet2D);
	CVTCLFunctionInit(itkls2d,NewCmd,itkls2d);

	return TCL_OK;

}

// Creates the rest of the commands
int itkls2d_ObjectCmd( CXX_TCL_STDARGS )
{
	////std::cout << "Method: " << argv[1] << std::endl;
	if ( argc == 1 ) {
		PrintMethods();
		return TCL_OK;
	}

	if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
		Tcl_SetResult( interp, "ITKLevelSet2D", TCL_STATIC );
		return TCL_OK;
	}

	CVTCLObjMethodInit(itkls2d,SetInputsMtd,SetInputs)
	CVTCLObjMethodInit(itkls2d,PhaseOneLevelSetMtd,PhaseOneLevelSet)
	CVTCLObjMethodInit(itkls2d,PhaseTwoLevelSetMtd,PhaseTwoLevelSet)
	CVTCLObjMethodInit(itkls2d,GACLevelSetMtd,GACLevelSet)
	CVTCLObjMethodInit(itkls2d,GetFrontMtd,GetFront)
	CVTCLObjMethodInit(itkls2d,GetVelocityImageMtd,GetVelocityImage)
	CVTCLObjMethodInit(itkls2d,GetFrontImageMtd,GetFrontImage)
	CVTCLObjMethodInit(itkls2d,WriteFrontMtd,WriteFront);
	CVTCLObjMethodInit(itkls2d,SetMaxRMSErrorMtd,SetMaxRMSError)
	CVTCLObjMethodInit(itkls2d,SetMaxIterationsMtd,SetMaxIterations)
	CVTCLObjMethodInit(itkls2d,SetPropagationScalingMtd,SetPropagationScaling)
	CVTCLObjMethodInit(itkls2d,SetCurvatureScalingMtd,SetCurvatureScaling)
	CVTCLObjMethodInit(itkls2d,SetAdvectionScalingMtd,SetAdvectionScaling)
	CVTCLObjMethodInit(itkls2d,SetMaxIterationsMtd,SetMaxIterations)
	CVTCLObjMethodInit(itkls2d,SetDebugMtd,SetDebug)
	CVTCLObjMethodInit(itkls2d,SetUseInputImageAsFeatureMtd,SetUseInputImageAsFeature)
	CVTCLObjMethodInit(itkls2d,SetUseInputImageDistanceMtd,SetUseInputImageDistance)

	//CVTCLObjMethodInit(itkls2d,SetUseNormalVectorCurvatureMtd,SetUseNormalVectorCurvature)
	//CVTCLObjMethodInit(itkls2d,SetUseMeanCurvatureMtd,SetUseMeanCurvature)
	//CVTCLObjMethodInit(itkls2d,SetUseMinimalCurvatureMtd,SetUseMinimalCurvature)

	Tcl_AppendResult( interp, "\"", argv[1],
			"\" not a recognized ITKLevelSet method", (char *)NULL );
	return TCL_ERROR;
}

static void PrintMethods()
{
	printf("Im not sure yet...\n");
}

// -------------
// itkls2d_NewCmd
// -------------

// itkls2d <objName> Or ITKLevelSet <objName>
static int itkls2d_NewCmd( CXX_TCL_STDARGS )
{

	CONST84 char *lsName;
	cvITKLevelSet *ls;
	Tcl_HashEntry *entryPtr;
	int newEntry = 0;


	// Check syntax:
	if (argc != 2) {
		Tcl_AppendResult( interp, "usage: ", argv[0], " <objName>",
				(char *)NULL );
		return TCL_ERROR;
	}

	// Make sure this is a new object name:
	lsName = argv[1];
	if ( !NewName( lsName ) ) {
		Tcl_AppendResult( interp, "ITKLevelSetCore object \"", lsName,
				"\" already exists", (char *)NULL );
		return TCL_ERROR;
	}

	// Allocate new cvLevelSet object:
	ls = new cvITKLevelSet;
	if ( ls == NULL ) {
		Tcl_AppendResult( interp, "error allocating object \"", lsName,
				"\"", (char *)NULL );
		return TCL_ERROR;
	}

	strcpy( ls->tclName_, lsName );
	entryPtr = Tcl_CreateHashEntry( &gLsetCoreTable, lsName, &newEntry );
	if ( !newEntry ) {
		Tcl_SetResult( interp, "error updating cvLevelSet hash table",
				TCL_STATIC );
		delete ls;
		return TCL_ERROR;
	}
	Tcl_SetHashValue( entryPtr, (ClientData)ls );
	char lsNameStr[2048];
	lsNameStr[0]='\0';
	sprintf(lsNameStr,"%s",lsName);
	Tcl_SetResult( interp, lsNameStr, TCL_VOLATILE );
	Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), itkls2d_ObjectCmd,
			(ClientData)ls, Deleteitkls2d );
	return TCL_OK;
}

// --------------
// DeleteLsetCore
// --------------
// Deletion callback invoked when the Tcl object is deleted.  Delete
// Tcl hash table entry as well as the cvITKLevelSet object itself.

void Deleteitkls2d( ClientData clientData )
{

	cvITKLevelSet *ls = (cvITKLevelSet *)clientData;
	Tcl_HashEntry *entryPtr;

	entryPtr = Tcl_FindHashEntry( &gLsetCoreTable, ls->tclName_ );
	if ( entryPtr == NULL ) {
		printf("Error looking up LsetCore object %s for deletion.\n",
				ls->tclName_);
	} else {
		Tcl_DeleteHashEntry( entryPtr );
	}
	delete ls;
}



int itkls2d_SetInputsMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSet *ls = (cvITKLevelSet *)clientData;
	char *usage;
	char *inputImageName;
	char *seedPdName;


	int table_size = 2;

	ARG_Entry arg_table[] = {
			{ "-image", STRING_Type, &inputImageName, NULL, REQUIRED, 0, { 0 } },
			{ "-seed", STRING_Type, &seedPdName, NULL, REQUIRED, 0, { 0 } },
	};

	usage = ARG_GenSyntaxStr(2, argv, table_size, arg_table );

	CVTCLTestArgsMacro(2)
	CVTCLParseTclStrMacro(2)



	RepositoryDataT typeImg1;
	char *typeImg1Str;
	typeImg1 = gRepository->GetType( inputImageName );
	typeImg1Str = RepositoryDataT_EnumToStr( typeImg1 );
	cvRepositoryData *inputImage;
	if (inputImageName != NULL) {
		// Look up given image object:
		inputImage = gRepository->GetObject( inputImageName );
		if ( inputImage == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", inputImageName, (char *)NULL );
			return TCL_ERROR;
		}
		printf("Found Object\n");
		// Make sure image is of type STRUCTURED_PTS_T:
		typeImg1 = inputImage->GetType();
		if ( typeImg1 != STRUCTURED_PTS_T ) {
			Tcl_AppendResult( interp, "error: object ", inputImageName,
					"not of type StructuredPts", (char *)NULL );
			return TCL_ERROR;
		}
	}


	RepositoryDataT typeImg2;
	char *typeImg2Str;
	typeImg2 = gRepository->GetType( seedPdName );
	typeImg2Str = RepositoryDataT_EnumToStr( typeImg2 );
	cvRepositoryData *seedPolyData;

	if (seedPdName != NULL) {
		// Look up given image object:
		seedPolyData = gRepository->GetObject( seedPdName );
		if ( seedPolyData == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", seedPdName, (char *)NULL );
			return TCL_ERROR;
		}
		printf("Found Object\n");
		// Make sure image is of type STRUCTURED_PTS_T:
		typeImg2 = seedPolyData->GetType();
		if ( typeImg2 != POLY_DATA_T) {
			Tcl_AppendResult( interp, "error: object ", seedPdName,
					"not of type PolyData", (char *)NULL );
			return TCL_ERROR;
		}
	}

	ls->SetInputImage((cvStrPts*)inputImage);
	ls->SetSeed((cvPolyData*)seedPolyData);


	return TCL_OK;
}

/*
 *
 *
 */

static int itkls2d_PhaseOneLevelSetMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSet *ls = (cvITKLevelSet *)clientData;
	char *usage;
	double kc, expFactorRising,expFactorFalling, advScale;

	double sigmaFeat = -1, sigmaAdv = -1;



	int table_size = 5;
	ARG_Entry arg_table[] = {
			{ "-Kc", DOUBLE_Type, &kc, NULL, REQUIRED, 0, { 0 } },
			{ "-expRising", DOUBLE_Type, &expFactorRising, NULL, REQUIRED, 0, { 0 } },
			{ "-expFalling", DOUBLE_Type, &expFactorFalling, NULL, REQUIRED, 0, { 0 } },
			{ "-sigmaFeat", DOUBLE_Type, &sigmaFeat, NULL, GDSC_OPTIONAL, 0, { 0 } },
			{ "-sigmaAdv", DOUBLE_Type, &sigmaAdv, NULL, GDSC_OPTIONAL, 0, { 0 } },
	};

	usage = ARG_GenSyntaxStr(2, argv, table_size, arg_table );

	CVTCLParseTclStrMacro(2)
	CVTCLTestArgsMacro(2)
	std::cout << "sigmaFeat " << sigmaFeat << std::endl;

	if(sigmaFeat >= 0)
		ls->SetSigmaFeature(sigmaFeat);
	if(sigmaAdv >= 0)
		ls->SetSigmaAdvection(sigmaAdv);

	ls->ComputePhaseOneLevelSet(kc, expFactorRising,expFactorFalling);

	return TCL_OK;
}

static int itkls2d_PhaseTwoLevelSetMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSet *ls = (cvITKLevelSet *)clientData;
	char *usage;
	double klow, kupp;
	double sigmaFeat = -1, sigmaAdv = -1;

	int table_size = 4;
	ARG_Entry arg_table[] = {
			{ "-Klow", DOUBLE_Type, &klow, NULL, REQUIRED, 0, { 0 } },
			{ "-Kupp", DOUBLE_Type, &kupp, NULL, REQUIRED, 0, { 0 } },
			{ "-sigmaFeat", DOUBLE_Type, &sigmaFeat, NULL, GDSC_OPTIONAL, 0, { 0 } },
			{ "-sigmaAdv", DOUBLE_Type, &sigmaAdv, NULL, GDSC_OPTIONAL, 0, { 0 } },
	};

	usage = ARG_GenSyntaxStr(2, argv, table_size, arg_table );


	CVTCLParseTclStrMacro(2)
	CVTCLTestArgsMacro(2)
	//std::cout << "Entering LS" << std::endl;

	if(sigmaFeat >= 0)
		ls->SetSigmaFeature(sigmaFeat);
	if(sigmaAdv >= 0)
		ls->SetSigmaAdvection(sigmaAdv);


	ls->ComputePhaseTwoLevelSet(kupp,klow);

	return TCL_OK;
}
static int itkls2d_GACLevelSetMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSet *ls = (cvITKLevelSet *)clientData;
	char *usage;
	double sigma, expFactor;

	int table_size = 2;
	ARG_Entry arg_table[] = {
			{ "-expFactor", DOUBLE_Type, &expFactor, NULL, REQUIRED, 0, { 0 } },
			{ "-sigmaSpeed", DOUBLE_Type, &sigma, NULL, GDSC_OPTIONAL, 0, { 0 } },
	};

	usage = ARG_GenSyntaxStr(2, argv, table_size, arg_table );

	CVTCLParseTclStrMacro(2)
	CVTCLTestArgsMacro(2)

	ls->ComputeGACLevelSet(expFactor);


	return TCL_OK;
}



static int itkls2d_WriteFrontMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSet *ls = (cvITKLevelSet *)clientData;
	ls->WriteFrontImages();
	return TCL_OK;
}




