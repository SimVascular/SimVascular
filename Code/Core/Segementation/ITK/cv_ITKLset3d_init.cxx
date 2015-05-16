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
 * cv_itkls3d_init.cxx
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
typedef itk::Image<short,3> ImageType;
// cvITKLevelSets New and delete
static int itkls3d_NewCmd(CXX_TCL_STDARGS);
void Deleteitkls3d( ClientData clientData );
// cvITKLevelSets object methods
static int itkls3d_ObjectCmd( CXX_TCL_STDARGS );
static int itkls3d_SetInputsMtd( CXX_TCL_STDARGS );
static int itkls3d_PhaseOneLevelSetMtd( CXX_TCL_STDARGS );
static int itkls3d_PhaseTwoLevelSetMtd( CXX_TCL_STDARGS );
static int itkls3d_GACLevelSetMtd( CXX_TCL_STDARGS );
static int itkls3d_LaplacianLevelSetMtd( CXX_TCL_STDARGS );
static int itkls3d_CopyFrontToSeedMtd( CXX_TCL_STDARGS );
CVTCLMtdDeclareMacro(itkls3d,GetFront);
CVTCLMtdDeclareMacro(itkls3d,GetFrontImage);
CVTCLMtdDeclareMacro(itkls3d,GetVelocityImage);
CVTCLMtdDeclareMacro(itkls3d,WriteFront);
CVTCLMtdDeclareMacro(itkls3d,SetMaxIterations);
CVTCLMtdDeclareMacro(itkls3d,SetMaxRMSError);
CVTCLMtdDeclareMacro(itkls3d,SetDebug);
CVTCLMtdDeclareMacro(itkls3d,SetPropagationScaling);
CVTCLMtdDeclareMacro(itkls3d,SetLaplacianScaling);
CVTCLMtdDeclareMacro(itkls3d,SetAdvectionScaling);
CVTCLMtdDeclareMacro(itkls3d,SetCurvatureScaling);

CVTCLMtdDeclareMacro(itkls3d,SetUseNormalVectorCurvature);
CVTCLMtdDeclareMacro(itkls3d,SetUseMeanCurvature);
CVTCLMtdDeclareMacro(itkls3d,SetUseMinimalCurvature);
CVTCLMtdDeclareMacro(itkls3d,SetBinarySeed);


// cvITKLevelSet Properties
CVTCLObjMemberGetObjPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,Front,cvPolyData);
CVTCLObjMemberGetObjPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,VelocityImage,cvStrPts);
CVTCLObjMemberGetObjPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,FrontImage,cvStrPts);

CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,MaxIterations,int,INT_Type);
CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,MaxRMSError,double,DOUBLE_Type);
CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,PropagationScaling,double,DOUBLE_Type);
CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,CurvatureScaling,double,DOUBLE_Type);
CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,AdvectionScaling,double,DOUBLE_Type);
CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,LaplacianScaling,double,DOUBLE_Type);
CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,Debug,bool,BOOL_Type);
CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,UseNormalVectorCurvature,bool,BOOL_Type);
CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,UseMeanCurvature,bool,BOOL_Type);
CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,UseMinimalCurvature,bool,BOOL_Type);
CVTCLObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,BinarySeed,bool,BOOL_Type);



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
// itkls3d_Init
// -------------
int itkls3d_Init( Tcl_Interp *interp ){

	//Usage: CVTCLFunctionInit(Prefix,FunctionName,TclName)
	CVTCLFunctionInit(itkls3d,NewCmd,ITKLevelSet3D);
	CVTCLFunctionInit(itkls3d,NewCmd,itkls3d);

	return TCL_OK;

}


// Creates the rest of the commands
int itkls3d_ObjectCmd( CXX_TCL_STDARGS )
{
	////std::cout << "Method: " << argv[1] << std::endl;
	if ( argc == 1 ) {
		PrintMethods();
		return TCL_OK;
	}

	if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
		Tcl_SetResult( interp, "ITKLevelSet3D", TCL_STATIC );
		return TCL_OK;
	}

	CVTCLObjMethodInit(itkls3d,SetInputsMtd,SetInputs)
	CVTCLObjMethodInit(itkls3d,PhaseOneLevelSetMtd,PhaseOneLevelSet)
	CVTCLObjMethodInit(itkls3d,PhaseTwoLevelSetMtd,PhaseTwoLevelSet)
	CVTCLObjMethodInit(itkls3d,GACLevelSetMtd,GACLevelSet)
	CVTCLObjMethodInit(itkls3d,LaplacianLevelSetMtd,LaplacianLevelSet)
	CVTCLObjMethodInit(itkls3d,GetFrontMtd,GetFront)
	CVTCLObjMethodInit(itkls3d,GetVelocityImageMtd,GetVelocityImage)
	CVTCLObjMethodInit(itkls3d,GetFrontImageMtd,GetFrontImage)
	CVTCLObjMethodInit(itkls3d,WriteFrontMtd,WriteFront);
	CVTCLObjMethodInit(itkls3d,SetMaxRMSErrorMtd,SetMaxRMSError)
	CVTCLObjMethodInit(itkls3d,SetMaxIterationsMtd,SetMaxIterations)
	CVTCLObjMethodInit(itkls3d,SetPropagationScalingMtd,SetPropagationScaling)
	CVTCLObjMethodInit(itkls3d,SetLaplacianScalingMtd,SetLaplacianScaling)
	CVTCLObjMethodInit(itkls3d,SetCurvatureScalingMtd,SetCurvatureScaling)
	CVTCLObjMethodInit(itkls3d,SetAdvectionScalingMtd,SetAdvectionScaling)
	CVTCLObjMethodInit(itkls3d,SetMaxIterationsMtd,SetMaxIterations)
	CVTCLObjMethodInit(itkls3d,SetDebugMtd,SetDebug)
	CVTCLObjMethodInit(itkls3d,CopyFrontToSeedMtd,CopyFrontToSeed)

	CVTCLObjMethodInit(itkls3d,SetUseNormalVectorCurvatureMtd,SetUseNormalVectorCurvature)
	CVTCLObjMethodInit(itkls3d,SetUseMeanCurvatureMtd,SetUseMeanCurvature)
	CVTCLObjMethodInit(itkls3d,SetUseMinimalCurvatureMtd,SetUseMinimalCurvature)
	CVTCLObjMethodInit(itkls3d,SetBinarySeedMtd,SetBinarySeed)

	Tcl_AppendResult( interp, "\"", argv[1],
			"\" not a recognized ITKLevelSet method", (char *)NULL );
	return TCL_ERROR;
}

static void PrintMethods()
{
	printf("Im not sure yet...\n");
}

// -------------
// itkls3d_NewCmd
// -------------

// itkls3d <objName> Or ITKLevelSet <objName>
static int itkls3d_NewCmd( CXX_TCL_STDARGS )
{

	CONST84 char *lsName;
	cvITKLevelSetBase<ImageType> *ls;
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
	ls = new cvITKLevelSetBase<ImageType>;
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
	Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), itkls3d_ObjectCmd,
			(ClientData)ls, Deleteitkls3d );
	return TCL_OK;
}

// --------------
// DeleteLsetCore
// --------------
// Deletion callback invoked when the Tcl object is deleted.  Delete
// Tcl hash table entry as well as the cvITKLevelSet object itself.

void Deleteitkls3d( ClientData clientData )
{

	cvITKLevelSetBase<ImageType> *ls = (cvITKLevelSetBase<ImageType> *)clientData;
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



int itkls3d_SetInputsMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls = (cvITKLevelSetBase<ImageType> *)clientData;
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
		//printf("Found Object\n");
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

static int itkls3d_PhaseOneLevelSetMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls = (cvITKLevelSetBase<ImageType> *)clientData;
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

static int itkls3d_PhaseTwoLevelSetMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls = (cvITKLevelSetBase<ImageType> *)clientData;
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
static int itkls3d_GACLevelSetMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls = (cvITKLevelSetBase<ImageType> *)clientData;
	char *usage;
	double sigma, expFactor,kappa,iso;

	int table_size = 4;
	ARG_Entry arg_table[] = {
			{ "-expFactor", DOUBLE_Type, &expFactor, NULL, REQUIRED, 0, { 0 } },
			{ "-sigmaSpeed", DOUBLE_Type, &sigma, NULL, GDSC_OPTIONAL, 0, { 0 } },
			{ "-kappa", DOUBLE_Type, &kappa, NULL, GDSC_OPTIONAL, 0, { 0 } },
			{ "-iso", DOUBLE_Type, &iso, NULL, GDSC_OPTIONAL, 0, { 0 } },
	};

	usage = ARG_GenSyntaxStr(2, argv, table_size, arg_table );

	CVTCLParseTclStrMacro(2);
	CVTCLTestArgsMacro(2);

	if(sigma >= 0)
		ls->SetSigmaFeature(sigma);
	ls->ComputeGACLevelSet(expFactor,kappa,iso);


	return TCL_OK;
}
static int itkls3d_LaplacianLevelSetMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls = (cvITKLevelSetBase<ImageType> *)clientData;
	char *usage;
	double sigma, expFactor,kappa,iso;
	std::cout << "Laplacian" << std::endl;
	int table_size = 4;
	ARG_Entry arg_table[] = {
			{ "-expFactor", DOUBLE_Type, &expFactor, NULL, REQUIRED, 0, { 0 } },
			{ "-sigmaSpeed", DOUBLE_Type, &sigma, NULL, GDSC_OPTIONAL, 0, { 0 } },
			{ "-kappa", DOUBLE_Type, &kappa, NULL, GDSC_OPTIONAL, 0, { 0 } },
			{ "-iso", DOUBLE_Type, &iso, NULL, GDSC_OPTIONAL, 0, { 0 } },
	};

	usage = ARG_GenSyntaxStr(2, argv, table_size, arg_table );

	CVTCLParseTclStrMacro(2);
	CVTCLTestArgsMacro(2);

	if(sigma >= 0)
		ls->SetSigmaFeature(sigma);
	ls->ComputeLaplacianLevelSet(expFactor,kappa,iso);


	return TCL_OK;
}


static int itkls3d_WriteFrontMtd( CXX_TCL_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls = (cvITKLevelSetBase<ImageType> *)clientData;
	ls->WriteFrontImages();
	return TCL_OK;
}

static int itkls3d_CopyFrontToSeedMtd(CXX_TCL_STDARGS)
{

	cvITKLevelSetBase<ImageType> *ls = (cvITKLevelSetBase<ImageType> *)clientData;\
	ls->CopyFrontToSeed();
	return TCL_OK;
}




