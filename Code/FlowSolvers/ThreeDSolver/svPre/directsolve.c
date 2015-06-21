/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson.
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
 *
 *=========================================================================*/

#include "cvFlowsolverOptions.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "simvascular_sparse.h"

// general utility
int debugprint(FILE* ,char* , ...);
extern FILE *stddbg;
extern int verbose_;

typedef struct {
      int row;
      int col;
      double value;
} Kentry;

// defined in displacements.cc
void deleteKentries(Kentry *Kentries);

#define SPARSE_OFFSET 1

int StanfordSolveSparseMatrix(Kentry* Kentries, double b[],
                     int numUniqueEntries, int size, double soln[]) {
    
   int i;
   int row;
   int col;
   double value;

   spMatrix A;
   spError err, Error;
   spREAL AbsThreshold,RelThreshold;
   spREAL *pElement;
   
#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH   
   FILE *fp;
   fp = NULL;
   fp = fopen("nonzero-inside-solver-call","w");
   fprintf(fp,"title goes here (%i nonzero)\n",numUniqueEntries);
   fprintf(fp,"%i        real\n",size);
   for (i = 0; i < numUniqueEntries; i++) {
       /* we add 1 to row and col numbers for sparse */
      fprintf(fp,"%i %i %lf\n",Kentries[i].row+SPARSE_OFFSET,Kentries[i].col+SPARSE_OFFSET,
            Kentries[i].value);
   }
   fprintf(fp,"0 0 0.0\n"); 
   for (i = 0; i < size; i++) {
      fprintf(fp,"%lf\n",b[i+SPARSE_OFFSET]);
   }
   fclose(fp);
#endif
   
   /* create the matrix */
   debugprint(stddbg,"  allocate A matrix.\n");
   fflush(stdout); 
   A = spCreate(size, 0, &err);
   if( err >= spFATAL || A == NULL) {
      fprintf(stderr,"error allocating matrix.\n");   
      exit(-1);
   }
    
   for (i = 0; i < numUniqueEntries; i++) {
       if (!(i % (int)(numUniqueEntries/50.0))) {
        debugprint(stddbg,"inserting into A: %i of %i\n",i,numUniqueEntries); 
       }
     row   = Kentries[i].row+SPARSE_OFFSET;
     col   = Kentries[i].col+SPARSE_OFFSET;
     value = Kentries[i].value;
     pElement = spGetElement(A,row,col);
     if (pElement == NULL) {
       fprintf(stderr, "error: insufficient memory available.\n");
       exit(-1);
     }
     *pElement = value;
   }

   debugprint(stddbg,"  free memory for Kentries\n");  
   deleteKentries(Kentries);
 
   spSetReal( A );
#if MODIFIED_NODAL
   spMNA_Preorder( A );
#endif

   RelThreshold = 0;
   AbsThreshold = 0;
   debugprint(stddbg,"  order and factor matrix.\n"); 
   Error = spOrderAndFactor( A, b, RelThreshold, AbsThreshold,
                             1 );
   if ( Error >= spFATAL ) {
      fprintf(stdout,"Fatal error (%i)\n",Error);
     exit(-1);
   }
  
   /* spPrint( A,1,1,1); */

   for (i = 0; i <= size; i++) {
       soln[0] = 0;
   }

   debugprint(stddbg,"  call spSolve.\n");
   
   spSolve( A, b, soln);

   debugprint(stddbg,"  destroy A.\n");
   
   spDestroy(A);
   return 0;
}

#ifdef IGNORE_THIS_TEST_CODE_FOR_SPARSE
int main(int argc, char* argv[]) {

    int i;  
    char title[1024];
    char line[1024]; 
    int size;
    int row;
    int col;
    double value;
    spMatrix A;
    spREAL x[4096];
    spREAL b[4096];
    spError err, Error;
    spREAL AbsThreshold,RelThreshold;
    spREAL *pElement;
    FILE *fp = NULL;

    if (argc != 2) {
        fprintf(stderr,"usage: sptest <matrix_filename>\n");
        exit(-1); 
    }

    fprintf(stdout,"Reading file [%s]\n",argv[1]);

    if ((fp = fopen(argv[1],"r")) == NULL) {
        fprintf(stderr,"Error opening file [%s]\n",argv[1]);
        exit(-1);
    }
    
    title[0]='\0'; 
    fgets(title,1024,fp); 

    fgets(line,1024,fp);
    if (sscanf(line,"%i real",&size) !=  1) {
        fprintf(stderr,"Error reading size.\n");
        exit(-1);
    }

    // create the matrix

    A = spCreate(size, 0, &err);
    if( err >= spFATAL || A == NULL) {
      fprintf(stderr,"error allocating matrix.\n");   
      exit(-1);
    }

    while (0 == 0) {
        line[0]='\0';
        fgets(line,1024,fp);
        if (sscanf(line,"%i %i %lf",&row,&col,&value) !=  3) {
          fprintf(stderr,"Error reading matrix.\n");
          exit(-1);
        }
        if (row == 0 && col == 0) break;

        //spElement *pElement;
        pElement = spGetElement(A,row,col);
        if (pElement == NULL)
        {   fprintf(stderr, "error: insufficient memory available.\n");
            exit(-1);
        }  
        *pElement = value;
        pElement = spGetElement(A,row,col);
    }

    b[0] = 0;  
    for (i = 1; i <= size; i++) {
        line[0]='\0';
        fgets(line,1024,fp);
        if (sscanf(line,"%lf",&value) !=  1) {
          fprintf(stderr,"Error reading RHS.\n");
          exit(-1);
        }
        b[i] = value;
    }
    
    spSetReal( A );

    /*spPrint( A,1,1,1);*/

#if MODIFIED_NODAL
    spMNA_Preorder( A );
#endif
    RelThreshold = 0;
    AbsThreshold = 0;
  Error = spOrderAndFactor( A, b, RelThreshold, AbsThreshold,
                            1 );
  if ( Error >= spFATAL ) {
      fprintf(stdout,"Fatal error (%i)\n",Error);
    exit(-1);
  }
  
  /*spPrint( A,1,1,1);*/

  for (i = 0; i <= size; i++) {
      x[0] = 0;
  }
  
  spSolve( A, b, x);

  /* Print the Solution. */
  for (i = 1; i <= size; i++) {
      fprintf(stdout,"diplacement[%i] = %lg\n",i,x[i]);
  }
  
  spDestroy(A);
  return 0;
    
}
#endif






