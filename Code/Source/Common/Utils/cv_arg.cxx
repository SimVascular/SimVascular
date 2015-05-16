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

#include "cv_arg.h"
#include <string.h>
#include <stdio.h>
#include <stdarg.h>


#define MAX_ARGVAL_PAIRS 100


static int ARG_FindArgvIx( char *name, int argc, CONST84 char *argv[] );


/* --------------- */
/* ARG_ParseTclStr */
/* --------------- */

/* For string args, this function is going to simply copy pointers
 * from argv into the arg address given in arg_table.  Also note that
 * if the pointer to use for storing an arg's value is NULL, this is
 * taken to be a normal indication that this arg should simply be
 * skipped (i.e. not parsed).  However, this is useful only for
 * clarity, since any entries that are left out of the table in the
 * first place obviously won't get parsed.
 *
 * Note that the caller should ultimately make a call to Tcl_Free for
 * any argv's in any ARG_List struct's that are passed to
 * ARG_ParseTclStr:
 *   Tcl_Free((char *) argv);
 */

int ARG_ParseTclStr( Tcl_Interp *interp,
		     int argc, CONST84 char *argv[], int startIx,
		     int table_size, ARG_Entry arg_table[] )
{
  int i;
  char *name;
  ARG_Type type;
  int ix, error;
  void *addr;
  CONST84 char **str;
  int required;
  ARG_List *list;
  int numPairs;
  int parsed[MAX_ARGVAL_PAIRS];

  if ( startIx >= argc ) {
    return TCL_ERROR;
  }
  if ( ( (argc - startIx) % 2 ) != 0 ) {
    printf( "arg/val mismatch\n" );
    return TCL_ERROR;
  }
  numPairs = ( argc - startIx ) / 2;
  if ( numPairs > MAX_ARGVAL_PAIRS ) {
    printf( "ARG_ParseTclStr: max. # arg/val pairs is currently set to %d\n",
	   MAX_ARGVAL_PAIRS );
    return TCL_ERROR;
  }
  for (i = 0; i < numPairs; i++) {
    parsed[i] = 0;
  }

  for (i = 0; i < table_size; i++) {
    arg_table[i].valid = 0;
  }

  for (i = 0; i < table_size; i++) {
    name = arg_table[i].name;
    type = arg_table[i].type;
    addr = arg_table[i].addr;
    str = &(arg_table[i].str_repr);

    if (addr == NULL) {
      return TCL_ERROR;
    }

    ix = ARG_FindArgvIx( name, argc, argv );
    if ( (ix < 0) || (ix >= (argc-1)) ) {
      if ( arg_table[i].required == REQUIRED ) {
	printf( "required arg \"%s\" not found\n", name );
	return TCL_ERROR;
      } else {
	continue;
      }
    }

    switch (type) {
    case BOOL_Type:
      error = Tcl_GetBoolean( interp, argv[ix+1], (int*)addr );
      break;
    case INT_Type:
      error = Tcl_GetInt( interp, argv[ix+1], (int*)addr );
      break;
    case DOUBLE_Type:
      error = Tcl_GetDouble( interp, argv[ix+1], (double*)addr );
      break;
    case STRING_Type:
      *((CONST84 char**)addr) = argv[ix+1];
      error = TCL_OK;
      break;
    case LIST_Type:
      list = (ARG_List*)addr;
      /* Tcl_SplitList allocates memory for list->argv */
      error = Tcl_SplitList( interp, argv[ix+1], &(list->argc),
			     &(list->argv) );
      break;
    default:
      error = TCL_ERROR;
      break;
    }
    if (error != TCL_OK) {
      printf( "conversion error on arg \"%s\"\n", name );
      return error;
    }
    (*str) = argv[ix+1];
    arg_table[i].valid = 1;
    parsed[(ix-startIx)/2] = 1;
  }

  // This check is redundant (I think), but the semantics are a bit
  // subtle, so I'm leaving this in for the moment.  Note that there
  // are two phases to the parsing in the above loop:
  //   1. attempt to locate the arg
  //   2. attempt to parse its value

  for (i = 0; i < table_size; i++) {
    required = arg_table[i].required;
    if (required == REQUIRED) {
      if ( arg_table[i].valid != 1 ) {
	printf( "required arg \"%s\" not found\n", arg_table[i].name );
	return TCL_ERROR;
      }
    }
  }

  for (i = 0; i < numPairs; i++) {
    if ( ! ( parsed[i] ) ) {
      ix = (i * 2) + startIx;
      printf( "unrecognized arg [%s]\n", argv[ix] );
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


/* -------------- */
/* ARG_FindArgvIx */
/* -------------- */

/* Simple search for named arg.  Note that a more efficient
 * implementation might use a better search, since doing things this
 * way involves O(n^2) strcmp's.  However, for small command lines
 * which aren't called too frequently, this should be fine.
 */

static int ARG_FindArgvIx( char *name, int argc, CONST84 char *argv[] )
{
  int i;

  for (i = 0; i < argc; i++) {
    if ( strcmp(name, argv[i]) == 0 ) {
      return i;
    }
  }
  return -1;
}


/* ---------------------- */
/* ARG_ParseTclListStatic */
/* ---------------------- */

/* Parse an array of strings (as generated by the call to
 * Tcl_SplitList in ARG_ParseTclStr).  The caller passes in a
 * pre-allocated array of the given type (cast to void*) and of the size
 * indicated by num.  On return, num contains the number of successful
 * assignments made to arr.  If conversion of any particular list element
 * causes an error, then num will be less than the size of the input
 * list, and TCL_ERROR is returned.
 *
 * ARG_String is not handled, since ARG_List's already consist of an
 * array of strings.
 *
 * The returned num is the number of list items successfully parsed
 * and put into arr.  If there were any parsing errors encountered
 * while traversing the list, then an error status is returned, though
 * any elements returned in arr will still be usable.
 */

int ARG_ParseTclListStatic( Tcl_Interp *interp, ARG_List list, ARG_Type t,
			    void *arr, int arrsz, int *numasn )
{
  int i;
  int error;
  int listSz;
  int *boolArr;
  int *intArr;
  double *dblArr;
  ARG_List *listArr;

  if ( t == STRING_Type ) {
    return TCL_ERROR;
  }
  
  if ( arr == NULL ) {
    return TCL_ERROR;
  }

  listSz = list.argc;
  if ( arrsz < listSz ) {
    return TCL_ERROR;
  }

  (*numasn) = 0;
  for (i = 0; i < listSz; i++) {
    switch (t) {
    case BOOL_Type:
      boolArr = (int *)arr;
      error = Tcl_GetBoolean( interp, list.argv[i], &(boolArr[i]) );
      break;
    case INT_Type:
      intArr = (int *)arr;
      error = Tcl_GetInt( interp, list.argv[i], &(intArr[i]) );
      break;
    case DOUBLE_Type:
      dblArr = (double *)arr;
      error = Tcl_GetDouble( interp, list.argv[i], &(dblArr[i]) );
      break;
    case LIST_Type:
      listArr = (ARG_List *)arr;
      error = Tcl_SplitList( interp, list.argv[i], &(listArr[i].argc),
			     &(listArr[i].argv) );
      break;
    }
    if ( error == TCL_OK ) {
      (*numasn)++;
    }
  }

  if ( (*numasn) != listSz ) {
    return TCL_ERROR;
  } else {
    return TCL_OK;
  }
}


/* ----------------------- */
/* ARG_ParseTclListDynamic */
/* ----------------------- */

/* Parse a list of strings (as generated by the call to Tcl_SplitList
 * in ARG_ParseTclStr).  Caller requests that an array be dynamically
 * allocated by this function (using Tcl_Alloc), which must subsequently
 * be free'd with Tcl_Free.
 *
 * ARG_String is not handled, since ARG_List's already consist of an
 * array of strings.
 *
 * The returned num is the number of list items successfully parsed
 * and put into arr.  If there were any parsing errors encountered
 * while traversing the list, then an error status is returned, though
 * any elements returned in (*arr) will still be usable and memory
 * pointed to by (*arr) will still need deallocting via Tcl_Free.
 */

int ARG_ParseTclListDynamic( Tcl_Interp *interp, ARG_List list, ARG_Type t,
			     void **arr, int *numasn )
{
  int i;
  int error;
  int listSz;
  int *boolArr;
  int *intArr;
  double *dblArr;
  ARG_List *listArr;

  listSz = list.argc;

  switch (t) {
  case BOOL_Type:
    (*arr) = (void *) Tcl_Alloc( sizeof(int) * listSz );
    break;
  case INT_Type:
    (*arr) = (void *) Tcl_Alloc( sizeof(int) * listSz );
    break;
  case DOUBLE_Type:
    (*arr) = (void *) Tcl_Alloc( sizeof(double) * listSz );
    break;
  case STRING_Type:
    return TCL_ERROR;
    break;
  case LIST_Type:
    (*arr) = (void *) Tcl_Alloc( sizeof(ARG_List) * listSz );
    break;
  default:
    return TCL_ERROR;
    break;
  }

  (*numasn) = 0;
  for (i = 0; i < listSz; i++) {
    switch (t) {
    case BOOL_Type:
      boolArr = (int *)(*arr);
      error = Tcl_GetBoolean( interp, list.argv[i], &(boolArr[i]) );
      break;
    case INT_Type:
      intArr = (int *)(*arr);
      error = Tcl_GetInt( interp, list.argv[i], &(intArr[i]) );
      break;
    case DOUBLE_Type:
      dblArr = (double *)(*arr);
      error = Tcl_GetDouble( interp, list.argv[i], &(dblArr[i]) );
      break;
    case LIST_Type:
      listArr = (ARG_List *)(*arr);
      error = Tcl_SplitList( interp, list.argv[i], &(listArr[i].argc),
			     &(listArr[i].argv) );
      break;
    }
    if ( error == TCL_OK ) {
      (*numasn)++;
    }
  }

  if ( (*numasn) != listSz ) {
    return TCL_ERROR;
  } else {
    return TCL_OK;
  }
}


/* ---------------- */
/* ARG_GenSyntaxStr */
/* ---------------- */

/* Call with two sets of strings.  The first are command arg's which
 * do not use arg/val pair semantics.  These are just appended to the
 * result string.  Then add syntax generated from the given arg
 * table.  Caller need not worry about result clean-up.  But caller
 * should also use result for read-only access.
 */

char *ARG_GenSyntaxStr( int argc, CONST84 char *argv[],
			int table_size, ARG_Entry arg_table[] )
{
  static Tcl_DString ds;
  int i;
  char *name;
  ARG_Type type;
  int required;

  // Initialize Tcl string, free'ing any previously allocated memory:
  Tcl_DStringFree( &ds );

  // Start with any non arg/val pairs from the given argc, argv:
  for (i = 0; i < argc; i++) {
    Tcl_DStringAppend( &ds, argv[i], -1 );
    if ( i < (argc-1) ) {
      Tcl_DStringAppend( &ds, " ", -1 );
    }
  }

  // Then add syntax of the given arg table:
  for (i = 0; i < table_size; i++) {
    name = arg_table[i].name;
    type = arg_table[i].type;
    required = arg_table[i].required;
    Tcl_DStringAppend( &ds, " ", -1 );
    Tcl_DStringAppend( &ds, name, -1 );
    switch (type) {
    case BOOL_Type:
      Tcl_DStringAppend( &ds, " <bool> ", -1 );
      break;
    case INT_Type:
      Tcl_DStringAppend( &ds, " <int> ", -1 );
      break;
    case DOUBLE_Type:
      Tcl_DStringAppend( &ds, " <double> ", -1 );
      break;
    case STRING_Type:
      Tcl_DStringAppend( &ds, " <str> ", -1 );
      break;
    case LIST_Type:
      Tcl_DStringAppend( &ds, " <list> ", -1 );
      break;
    default:
      Tcl_DStringAppend( &ds, " <UNRECOGNIZED> ", -1 );
      break;
    }
    switch (required) {
    case REQUIRED:
      Tcl_DStringAppend( &ds, "[R]", -1 );
      break;
    case GDSC_OPTIONAL:
    default:
      Tcl_DStringAppend( &ds, "[O]", -1 );
      break;
    }
  }

  return Tcl_DStringValue( &ds );
}


/* ----------------- */
/* ARG_FreeListArgvs */
/* ----------------- */

/* See Ousterhout p.317: the argv array of strings generated by
 * Tcl_SplitList is allocated such that pointers and strings are
 * allocated in a single block of memory, so only a single call to
 * Tcl_Free is needed.
 */

void ARG_FreeListArgvs( int table_size, ARG_Entry arg_table[] )
{
  int i;
  ARG_List *list;

  for (i = 0; i < table_size; i++) {
    if ( arg_table[i].type == LIST_Type ) {
      if ( arg_table[i].valid ) {
	list = (ARG_List*)arg_table[i].addr;
	Tcl_Free( (char *) list->argv );
      }
    }
  }
  return;
}


/* -------- */
/* ARG_Show */
/* -------- */

void ARG_Show( int table_size, ARG_Entry arg_table[] )
{
  int i, j;
  char *name;
  ARG_Type type;
  ARG_List *list;
  void *addr;

  for (i = 0; i < table_size; i++) {
    name = arg_table[i].name;
    type = arg_table[i].type;
    addr = arg_table[i].addr;
    printf( "%-20s ", name );
    switch (type) {
    case BOOL_Type:
      printf( "%d\n", *((int*)addr) );
      break;
    case INT_Type:
      printf( "%d\n", *((int*)addr) );
      break;
    case DOUBLE_Type:
      printf( "%f\n", *((double*)addr) );
      break;
    case STRING_Type:
      printf( "%s\n", *((char**)addr) );
      break;
    case LIST_Type:
      list = (ARG_List*)addr;
      for (j = 0; j < list->argc; j++) {
	printf( "%s ", list->argv[j] );
      }
      printf("\n");
      break;
    default:
      printf( "unrecognized type\n" );
      break;
    }
  }

  return;
}


/* ------------ */
/* tcl_printstr */
/* ------------ */

void tcl_printstr( Tcl_Interp *interp, char *str )
{
  Tcl_VarEval( interp, "puts [list ", str, "]", (char *) NULL );
  return;
}
