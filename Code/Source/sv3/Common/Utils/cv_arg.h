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

#ifndef __CVARG_H
#define __CVARG_H

#include "SimVascular.h"
#include "svUtilsExports.h" // For exports
#include "tcl.h"

#define REQUIRED 1
#define SV_REQUIRED 1
#define SV_OPTIONAL 0
#define MAX_EXCLUSIONS 10


typedef enum ARG_Type_enum {
  BOOL_Type,
  INT_Type,
  DOUBLE_Type,
  STRING_Type,
  LIST_Type
} ARG_Type;


typedef struct ARG_Entry_s {
  char *name;
  ARG_Type type;
  void *addr;
  CONST84 char *str_repr;
  int required;
  int valid;
  char *exclusions[MAX_EXCLUSIONS];
} ARG_Entry;


typedef struct ARG_List_s {
  int argc;
  CONST84 char **argv;
} ARG_List;

SV_EXPORT_UTILS char* ARG_GenSyntaxStr( int argc, CONST84 char *argv[],
			int table_size, ARG_Entry arg_table[] );


SV_EXPORT_UTILS int ARG_ParseTclStr( Tcl_Interp *interp,
		     int argc, CONST84 char *argv[], int startIx,
		     int table_size, ARG_Entry arg_table[] );


SV_EXPORT_UTILS int ARG_ParseTclListStatic( Tcl_Interp *interp, ARG_List list, ARG_Type t,
			    void *arr, int arrsz, int *numasn );


SV_EXPORT_UTILS int ARG_ParseTclListDynamic( Tcl_Interp *interp, ARG_List list, ARG_Type t,
			     void **arr, int *numasn );


SV_EXPORT_UTILS int ARG_ParseTclObj( Tcl_Interp *interp,
		     int objc, Tcl_Obj *CONST objv[],
		     int table_size, ARG_Entry arg_table[] );


SV_EXPORT_UTILS void ARG_FreeListArgvs( int table_size, ARG_Entry arg_table[] );


SV_EXPORT_UTILS void ARG_Show( int table_size, ARG_Entry arg_table[] );

SV_EXPORT_UTILS void tcl_printstr( Tcl_Interp *interp, char *str );


#endif /* __CVARG_H */
