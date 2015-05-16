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
 * cvTCLMacros.h
 *
 *  Created on: Apr 8, 2014
 *      Author: jameson
 */

#ifndef CVTCLMACROS_H_
#define CVTCLMACROS_H_

#define CVTCLFunctionInit(Prefix,FunctionName,TclName)  						\
		Tcl_CreateCommand( interp, #TclName, Prefix##_##FunctionName ,				\
			(ClientData)NULL, (Tcl_CmdDeleteProc *)NULL ); 						\

#define CVTCLObjMethodInit(Prefix,FunctionName,TclName) 									\
		if ( Tcl_StringMatch( argv[1], #TclName ) ) {						 	\
			if ( Prefix##_##FunctionName ( clientData, interp, argc, argv ) != TCL_OK ) { 	\
				return TCL_ERROR; 												\
			} 																	\
			return TCL_OK; 														\
		} 																		\

#define CVTCLRepositoryExistsMacro(obj) 										\
		if ( gRepository->Exists( obj ) ) { 									\
			Tcl_AppendResult( interp, "object ", obj, " already exists", 		\
					(char *)NULL ); 											\
					return TCL_ERROR;											\
		}																		\

#define CVTCLRepositoryRegisterMacro(objName,obj) 								\
		if ( !(gRepository->Register( objName, obj )) ) { 						\
			Tcl_SetResult( interp, "error registering obj in repository",		\
					TCL_STATIC );												\
					delete obj; 												\
					return TCL_ERROR;											\
		} 																		\

#define CVTCLParseTclStrMacro(numArgs)   										\
		if ( ARG_ParseTclStr( interp, argc, argv, numArgs,						\
				table_size, arg_table ) != TCL_OK ) { 							\
			Tcl_SetResult( interp, usage, TCL_VOLATILE ); 						\
			return TCL_ERROR; 													\
		} 																		\

#define CVTCLTestArgsMacro(numArgs) 											\
		if ( argc == numArgs ) { 												\
			Tcl_SetResult( interp, usage, TCL_VOLATILE ); 						\
			return TCL_OK; 														\
		} 																		\

#define CXX_TCL_STDARGS ClientData clientData, Tcl_Interp *interp, int argc, CONST84 char *argv[]

#define CVTCLMtdDeclareMacro(objname,name) 										\
		static int objname##_##name##Mtd( CXX_TCL_STDARGS )						\

#define CVTCLObjMemberSetPropertyMacro(tclname,objname,property,type,cv_type) 	\
		CVTCLMtdDeclareMacro(tclname,Set##property) 							\
		{																		\
			objname *ls = (objname *)clientData; 								\
			char *usage;														\
			type input; 														\
			int table_size = 1;													\
			ARG_Entry arg_table[] = {											\
					{ "-input", cv_type, &input, NULL, REQUIRED, 0, { 0 } },	\
			};																	\
			usage = ARG_GenSyntaxStr(2, argv, table_size, arg_table );			\
			CVTCLParseTclStrMacro(2)											\
			CVTCLTestArgsMacro(2)												\
			ls->Set##property(input); 											\
			return TCL_OK;														\
		}																		\


#define CVTCLObjMemberGetObjPropertyMacro(tclname,objname,property,type) 		\
		CVTCLMtdDeclareMacro(tclname,Get##property) 							\
		{																		\
			objname *ls = (objname *)clientData; 								\
			char *usage;														\
			type* cvObj;														\
			char * objName;														\
			int table_size = 1; 												\
			ARG_Entry arg_table[] = {											\
					{ "-out", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } }, \
			};																	\
			usage = ARG_GenSyntaxStr( 2, argv, table_size, arg_table );			\
			CVTCLTestArgsMacro(2)												\
			CVTCLParseTclStrMacro(2)											\
			cvObj = ls->Get##property (); 										\
			if ( cvObj == NULL ) { 												\
				Tcl_SetResult( interp, "error, obj is null", TCL_STATIC ); 		\
				return TCL_ERROR;												\
			}																	\
			CVTCLRepositoryRegisterMacro(objName,cvObj);						\
			Tcl_SetResult( interp, cvObj->GetName(), TCL_VOLATILE );			\
			return TCL_OK;														\
		}																		\




#endif /* CVITKMACROS_H_ */


