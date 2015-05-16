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

#ifndef __CVARG_H
#define __CVARG_H

#include "SimVascular.h"
#include "tcl.h"

#define REQUIRED 1
#define GDSC_REQUIRED 1
#define GDSC_OPTIONAL 0
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


char *ARG_GenSyntaxStr( int argc, CONST84 char *argv[],
			int table_size, ARG_Entry arg_table[] );


int ARG_ParseTclStr( Tcl_Interp *interp,
		     int argc, CONST84 char *argv[], int startIx,
		     int table_size, ARG_Entry arg_table[] );


int ARG_ParseTclListStatic( Tcl_Interp *interp, ARG_List list, ARG_Type t,
			    void *arr, int arrsz, int *numasn );


int ARG_ParseTclListDynamic( Tcl_Interp *interp, ARG_List list, ARG_Type t,
			     void **arr, int *numasn );


int ARG_ParseTclObj( Tcl_Interp *interp,
		     int objc, Tcl_Obj *CONST objv[],
		     int table_size, ARG_Entry arg_table[] );


void ARG_FreeListArgvs( int table_size, ARG_Entry arg_table[] );


void ARG_Show( int table_size, ARG_Entry arg_table[] );

void tcl_printstr( Tcl_Interp *interp, char *str );


#endif /* __CVARG_H */
