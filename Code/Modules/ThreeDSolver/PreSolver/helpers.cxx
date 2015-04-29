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

#include "cmd.h"
#include "cvSolverIO.h"

#ifdef USE_ZLIB
#include "simvascular_zlib.h"
#else
#include <stdlib.h>
#define gzopen fopen
#define gzprintf fprintf
#define gzFile FILE*
#define gzclose fclose
#define gzgets fgets
#define gzeof feof
#endif
#include <time.h>

// =========
//   Cross
// =========

inline
void Cross( double ax, double ay, double az, 
            double bx, double by, double bz, 
            double *prodx, double *prody, double *prodz )
{
  (*prodx) = ay * bz - az * by;
  (*prody) = -( ax * bz - az * bx );
  (*prodz) = ax * by - ay * bx;
  return;
}


// =======
//   Dot
// =======

inline
double Dot( double ax, double ay, double az, 
            double bx, double by, double bz )
{
  double product;

  product = ax * bx + ay * by + az * bz;
  return product;
}

//
// some helpful global variables
//
extern char* oformat;

extern int numNodes_;
extern int numElements_;
extern int numMeshEdges_;
extern int numMeshFaces_;
extern int numSolnVars_;
extern int numBoundaryFaces_;
extern int** boundaryElements_;
extern double* nodes_;
extern int* elements_;
extern int* boundaryElementsIds_;
extern int* xadj_;
extern int xadjSize_;
extern int* adjncy_;
extern int adjncySize_;
extern int* iBC_;
extern int* iBCB_;
extern double* BCB_;
extern int   DisplacementNumElements_;
extern int*  DisplacementConn_[3];
extern int   DisplacementNumNodes_;
extern int*  DisplacementNodeMap_;

int writeGEOMBCDAT(char* filename);

gzFile fp_ = NULL;
char buffer_[MAXCMDLINELENGTH];

int NWopenFile(char* filename) {

    if (fp_ != NULL) {
      fprintf(stderr,"ERROR: Apparently a file is already open! (opening %s)\n",filename);
      return CV_ERROR;
    }

    fp_ = NULL;
    fp_ = gzopen (filename, "rb");
    if (fp_ == NULL) {
      fprintf(stderr,"ERROR: could not open file (%s)\n",filename);
      return CV_ERROR;
    }

    return CV_OK;
}


int NWcloseFile() {
    if (fp_ == NULL) {
        fprintf(stderr,"Closing a file already closed!\n");
        return CV_OK;
    }
    gzclose(fp_);
    fp_ = NULL;
    return CV_OK;
}


int NWgetNextLine(int *eof) {        
    for (int i = 0; i < MAXCMDLINELENGTH;i++) {
      buffer_[i]='\0';
    }
    #ifdef USE_ZLIB
      gzgets(fp_,buffer_,MAXCMDLINELENGTH);
    #else
      fgets(buffer_,MAXCMDLINELENGTH,fp_);
    #endif

    *eof = gzeof(fp_);

    return CV_OK;
}


int NWgetNextNonBlankLine(int *eof) {
    NWgetNextLine(eof);
    for (int i=0;i < MAXCMDLINELENGTH;i++) {
     if (buffer_[i] == '\0') break;
     if (buffer_[i] == '\n') break;
     if (buffer_[i] != ' ') return CV_OK;
    }
    return CV_ERROR;
}


int parseCmdStr(char *cmd, char *mystr) {
    // parse command string
    int n = 0;
    int end = 0;
    char ignored[MAXSTRINGLENGTH];
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);
    mystr[0]='\0';
    cmd_token_get (&n, cmd, mystr, &end);
    return CV_OK;
}


int parseNum(char *cmd, int *num) {

    // parse command string
    char mystr[MAXSTRINGLENGTH];
    parseCmdStr(cmd,mystr);
 
    // do work
    *num = 0;
    if (sscanf(mystr,"%i",num) != 1) {
        fprintf(stderr,"error parsing num!\n");
        return CV_ERROR;
    }

    return CV_OK;
 
}


int parseNum2(char *cmd, int *num) {

    // parse command string
    int n = 0;
    int end = 0;
    char ignored[MAXSTRINGLENGTH];
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);
    char infile[MAXPATHLEN];
    infile[0]='\0';
    cmd_token_get (&n, cmd, infile, &end);
    char surfidstr[MAXSTRINGLENGTH];
    surfidstr[0]='\0';
    cmd_token_get (&n, cmd, surfidstr, &end);

    // do work

    *num = 0;

    int surfID = 0;
    if (sscanf(surfidstr,"%i",&surfID) != 1) {
        fprintf(stderr,"error parsing num!\n");
        return CV_ERROR;
    }

    *num = surfID;

    return CV_OK;
 
}


int parseDouble(char *cmd, double *num) {

    // parse command string
    char mystr[MAXSTRINGLENGTH];
    parseCmdStr(cmd,mystr);
 
    // do work
    *num = 0.0;
    if (sscanf(mystr,"%lf",num) != 1) {
        fprintf(stderr,"error parsing double!\n");
        return CV_ERROR;
    }

    return CV_OK;
 
}


int parseDouble2(char *cmd, double *num) {

    // parse command string

    int n = 0;
    int end = 0;
    char ignored[MAXSTRINGLENGTH];
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);

    char mystr[MAXSTRINGLENGTH];
    mystr[0]='\0';
    cmd_token_get (&n, cmd, mystr, &end);

 
    // do work
    *num = 0.0;
    if (sscanf(mystr,"%lf",num) != 1) {
        fprintf(stderr,"error parsing double!\n");
        return CV_ERROR;
    }

    return CV_OK;
 
}


int parseDouble3(char *cmd, double *v1, double *v2, double *v3) {
    
    // parse command string
    int n = 0;
    int end = 0;
    char ignored[MAXSTRINGLENGTH];
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);
    char str1[MAXPATHLEN];
    str1[0]='\0';
    cmd_token_get (&n, cmd, str1, &end);
    char str2[MAXSTRINGLENGTH];
    str2[0]='\0';
    cmd_token_get (&n, cmd, str2, &end);
    char str3[MAXSTRINGLENGTH];
    str3[0]='\0';
    cmd_token_get (&n, cmd, str3, &end);

    // do work
    *v1 = 0.0;
    if (sscanf(str1,"%lf",v1) != 1) {
        fprintf(stderr,"error parsing double!\n");
        return CV_ERROR;
    }
    *v2 = 0.0;
    if (sscanf(str2,"%lf",v2) != 1) {
        fprintf(stderr,"error parsing double!\n");
        return CV_ERROR;
    }
    *v3 = 0.0;
    if (sscanf(str3,"%lf",v3) != 1) {
        fprintf(stderr,"error parsing double!\n");
        return CV_ERROR;
    }

    return CV_OK;
 
}


int parseFile(char *cmd) {

    // parse command string
    char infile[MAXPATHLEN];
    parseCmdStr(cmd,infile);

    // do work
    return NWopenFile(infile);
 
}

int setNodesWithCode(char *cmd, int val) {

    // enter
    debugprint(stddbg,"Entering setNodesWithCode.\n");

    int i;

    // do work
    if (numNodes_ == 0) {
      fprintf(stderr,"ERROR:  Must specify number of nodes before you read them in!\n");
      return CV_ERROR;
    }

    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    if (iBC_ == NULL) {
     iBC_ = new int [numNodes_];
     for (i  = 0; i < numNodes_;i++) {
         iBC_[i] = 0;
     }
    }

    int eof = 0;
    int nodeId = 0;

    while (NWgetNextNonBlankLine(&eof) == CV_OK) {
        if (sscanf(buffer_,"%i",&nodeId) != 1) {
            fprintf(stderr,"WARNING:  line not of correct format (%s)\n",buffer_);
        } else {      
          // this should be a bit set instead of an int!!
          iBC_[nodeId - 1] = val;
        }
    }
    if (eof == 0) return CV_ERROR;
    NWcloseFile();

    // cleanup
    debugprint(stddbg,"Exiting setNodesWithCode.\n");
    return CV_OK;

}


int check_node_order(int n0, int n1, int n2, int n3, int elementId, 
                     int *k0,int *k1, int *k2, int *k3) {

    int i,j0,j1,j2,j3;

    if (n3 >= 0) {

        if (cvsolver_node_order_ == 0) {
          // Not necessary if using conn
          // flip the middle to vertexes because we used a value of 1
          // for node ordering in meshsim and scorec used the value of 0
          j0 = n0;
          j1 = n2;
          j2 = n1;
          j3 = n3;
        } else {
          j0 = n0;
          j1 = n1;
          j2 = n2;
          j3 = n3;    
        }

    } else {

        j0 = n0;
        j1 = n1;
        j2 = n2;
        j3 = -1;

        for (i = 0; i < 4; i++) {
            if (elements_[i*numElements_+(elementId-1)] != j0 && 
                elements_[i*numElements_+(elementId-1)] != j1 &&
                elements_[i*numElements_+(elementId-1)] != j2) {
                j3 = elements_[i*numElements_+(elementId-1)];
                break;
            }  
        }
        if (j3 < 0) {
            //gzclose(fp);
            fprintf(stderr,"ERROR:  could not find nodes in element (%i %i %i %i)\n",
                    elementId,n0,n1,n2);
            return CV_ERROR;
        }

    }

    double a[3];
    double b[3];
    double c[3];
    double norm0,norm1,norm2;

    a[0] = nodes_[0*numNodes_+j1 - 1] - nodes_[0*numNodes_+j0 - 1];
    a[1] = nodes_[1*numNodes_+j1 - 1] - nodes_[1*numNodes_+j0 - 1];
    a[2] = nodes_[2*numNodes_+j1 - 1] - nodes_[2*numNodes_+j0 - 1];
    b[0] = nodes_[0*numNodes_+j2 - 1] - nodes_[0*numNodes_+j0 - 1];
    b[1] = nodes_[1*numNodes_+j2 - 1] - nodes_[1*numNodes_+j0 - 1];
    b[2] = nodes_[2*numNodes_+j2 - 1] - nodes_[2*numNodes_+j0 - 1];
    c[0] = nodes_[0*numNodes_+j3 - 1] - nodes_[0*numNodes_+j0 - 1];
    c[1] = nodes_[1*numNodes_+j3 - 1] - nodes_[1*numNodes_+j0 - 1];
    c[2] = nodes_[2*numNodes_+j3 - 1] - nodes_[2*numNodes_+j0 - 1];
 
    Cross(a[0],a[1],a[2],b[0],b[1],b[2],&norm0,&norm1,&norm2);
    double mydot = Dot(norm0,norm1,norm2,c[0],c[1],c[2]);

    if (mydot > 0) {
      int tmpj = j0;
      j0 = j2;
      j2 = j1;
      j1 = tmpj;
      debugprint(stdout,"elementId %i : %i %i %i %i   (flipped) %lf\n",elementId,j0,j1,j2,j3,mydot);
     } else {
      debugprint(stdout,"elementId %i : %i %i %i %i  %lf\n",elementId,j0,j1,j2,j3,mydot);
     }

    *k0 = j0; *k1 = j1; *k2 = j2; *k3 = j3;

    return CV_OK;
}


int setBoundaryFacesWithCode(char *cmd,int setSurfID, int surfID, 
                                       int setCode, int code, double value) {

    // enter
    debugprint(stddbg,"Entering setBoundaryFacesWithCode.\n");

    int i;

    // parse command string
    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    if (iBCB_ == NULL) {
        iBCB_ = new int [2*numBoundaryFaces_];
        BCB_ = new double [ numBoundaryFaces_*6];
        for (i = 0; i < 2*numBoundaryFaces_; i++) {
            iBCB_[i] = 0;
        }
        for (i = 0; i < numBoundaryFaces_;i++) {
            BCB_[i]=0.0;
        }
    }

    int n0,n1,n2,n3;

    int elementId,matId;

    int eof = 0;

    while (NWgetNextNonBlankLine(&eof) == CV_OK) {
 
        if (sscanf(buffer_,"%i %i %i %i %i",&elementId,&matId,&n0,&n1,&n2) != 5) {
            fprintf(stderr,"WARNING:  line not of correct format (%s)\n",buffer_);
            NWcloseFile();
            return CV_ERROR;
        }

        int j0 = n0;
        int j1 = n1;
        int j2 = n2;
        int j3 = -1;

        for (i = 0; i < 4; i++) {
            if (elements_[i*numElements_+(elementId-1)] != j0 && 
                elements_[i*numElements_+(elementId-1)] != j1 &&
                elements_[i*numElements_+(elementId-1)] != j2) {
                j3 = elements_[i*numElements_+(elementId-1)];
                break;
            }  
        }
        if (j3 < 0) {
            NWcloseFile();
            fprintf(stderr,"ERROR:  could not find nodes in element (%i %i %i %i)\n",
                    elementId,n0,n1,n2);
            return CV_ERROR;
        }

        double a[3];
        double b[3];
        double c[3];
        double norm0,norm1,norm2;

        a[0] = nodes_[0*numNodes_+j1 - 1] - nodes_[0*numNodes_+j0 - 1];
        a[1] = nodes_[1*numNodes_+j1 - 1] - nodes_[1*numNodes_+j0 - 1];
        a[2] = nodes_[2*numNodes_+j1 - 1] - nodes_[2*numNodes_+j0 - 1];
        b[0] = nodes_[0*numNodes_+j2 - 1] - nodes_[0*numNodes_+j0 - 1];
        b[1] = nodes_[1*numNodes_+j2 - 1] - nodes_[1*numNodes_+j0 - 1];
        b[2] = nodes_[2*numNodes_+j2 - 1] - nodes_[2*numNodes_+j0 - 1];
        c[0] = nodes_[0*numNodes_+j3 - 1] - nodes_[0*numNodes_+j0 - 1];
        c[1] = nodes_[1*numNodes_+j3 - 1] - nodes_[1*numNodes_+j0 - 1];
        c[2] = nodes_[2*numNodes_+j3 - 1] - nodes_[2*numNodes_+j0 - 1];

        Cross(a[0],a[1],a[2],b[0],b[1],b[2],&norm0,&norm1,&norm2);
        double mydot = Dot(norm0,norm1,norm2,c[0],c[1],c[2]);
  
        if (mydot > 0) {
            int tmpj = j0;
            j0 = j2;
            j2 = j1;
            j1 = tmpj;
            fprintf(stdout,"elementId %i : %i %i %i %i   (flipped0) %lf\n",elementId,j0,j1,j2,j3,mydot);
        } else {
            //fprintf(stdout,"elementId %i : %i %i %i %i  %lf\n",elementId,j0,j1,j2,j3,mydot);
        }

        // find matching element already read in
        int foundIt = 0;
        for (i = 0; i < numBoundaryFaces_;i++) {

          if  (boundaryElementsIds_[i] == (elementId -1)) {
              if (boundaryElements_[0][i] == j0 &&
                  boundaryElements_[1][i] == j1 &&
                  boundaryElements_[2][i] == j2 &&
                  boundaryElements_[3][i] == j3) {
                  if (setCode) {
                    iBCB_[i] = code;
                    BCB_[1*numBoundaryFaces_+i] = value;
                    foundIt = 1;
                  }
                  if (setSurfID) {
                    iBCB_[numBoundaryFaces_+i] = surfID;
                    foundIt = 1;
                  }
                  break;
              }
          }

        }

        if (foundIt == 0) {
              fprintf(stderr,"ERROR: could not find pressure face in boundary faces!\n");
              return CV_ERROR;
        }
    }

    NWcloseFile();

    // cleanup
    debugprint(stddbg,"Exiting setBoundaryFacesWithCode.\n");
    return CV_OK;
}


void siftDownEdges(int **edges, int root, int bottom, int array_size)
{
  int done, maxChild;
  int temp0,temp1;

  done = 0;
  while ((root*2 <= bottom) && (!done))
  {
    if (root*2 == bottom)
      maxChild = root * 2;
    else if ((edges[0][root * 2] > edges[0][root * 2 + 1]) || \
             ((edges[0][root * 2] == edges[0][root *2 + 1]) && (edges[1][root*2] > edges[1][root*2+1]))  )
      maxChild = root * 2;
    else
      maxChild = root * 2 + 1;

    // hack since this seems to constantly end 
    if (root < 0 || root >= array_size) break;
    if (maxChild < 0 || maxChild >= array_size) {
      fprintf(stdout,"warning: illegal maxChild (%i)\n",maxChild);
      break;
    }

    if ( (edges[0][root] < edges[0][maxChild]) || \
         ((edges[0][root] == edges[0][maxChild]) && (edges[1][root] < edges[1][maxChild]))
       )

    {
      temp0 = edges[0][root];
      temp1 = edges[1][root];
      edges[0][root] = edges[0][maxChild];
      edges[1][root] = edges[1][maxChild];
      edges[0][maxChild] = temp0;
      edges[1][maxChild] = temp1;
      root = maxChild;
    }
    else
      done = 1;
  }
}


int fixFreeEdgeNodes(char *cmd) {

    // enter
    debugprint(stddbg,"Entering fixFreeEdgeNodes.\n");

    int i,j;

    // parse command string
    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    if (iBC_ == NULL) {
     iBC_ = new int [numNodes_];
     for (i  = 0; i < numNodes_;i++) {
         iBC_[i] = 0;
     }
    }

    // count lines in file
    int eof = 0;
    int numLinesInFile = 0;
    while (NWgetNextNonBlankLine(&eof) == CV_OK) {
        numLinesInFile++;
    }
    NWcloseFile();

    debugprint(stddbg,"Number of lines in file: %i\n",numLinesInFile);

    if (numLinesInFile == 0) {
        return CV_ERROR;
    }

    int* edges[2];
    edges[0] = new int[numLinesInFile*3];
    edges[1] = new int[numLinesInFile*3];

    if (parseFile(cmd) == CV_ERROR) {
        delete [] edges[0];
        delete [] edges[1];
        return CV_ERROR;
    }

    
    int n0,n1,n2,n3;
    int elementId,matId;
    eof = 0;
    int numEdges = 0;

    while (NWgetNextNonBlankLine(&eof) == CV_OK) {
 
        if (sscanf(buffer_,"%i %i %i %i %i",&elementId,&matId,&n0,&n1,&n2) != 5) {
            fprintf(stderr,"WARNING:  line not of correct format (%s)\n",buffer_);
            NWcloseFile();
            delete [] edges[0];
            delete [] edges[1];
            return CV_ERROR;
        }

        // stuff edges into array

        // edge 1
        if (n0 < n1) {
            edges[0][numEdges] = n0;
            edges[1][numEdges] = n1;
            numEdges++;
        } else {
            edges[0][numEdges] = n1;
            edges[1][numEdges] = n0;
            numEdges++;
        }
        // edge 2
        if (n1 < n2) {
            edges[0][numEdges] = n1;
            edges[1][numEdges] = n2;
            numEdges++;
        } else {
            edges[0][numEdges] = n2;
            edges[1][numEdges] = n1;
            numEdges++;
        }
        // edge 3
        if (n0 < n2) {
            edges[0][numEdges] = n0;
            edges[1][numEdges] = n2;
            numEdges++;
        } else {
            edges[0][numEdges] = n2;
            edges[1][numEdges] = n0;
            numEdges++;
        }
    }

    NWcloseFile();
 
    // sort edges so we can find free edges

    // debugging output
    //for (i = 0; i < numEdges; i++) {
    //    debugprint(stddbg,"org order [%i] %i %i\n",i,edges[0][i],edges[1][i]);
    //}

   int index0, index1;


    // heap sort 

    int temp0,temp1;
    int array_size = numEdges;

    for (i = (array_size / 2)-1; i >= 0; i--)
      siftDownEdges(edges, i, array_size, array_size);

    for (i = array_size-1; i >= 1; i--)
    {
      temp0 = edges[0][0];
      temp1 = edges[1][0];
      edges[0][0] = edges[0][i];
      edges[1][0] = edges[1][i];
      edges[0][i] = temp0;
      edges[1][i] = temp1;
      siftDownEdges(edges, 0, i-1, array_size);
    }


    
    // insertion sort algorithm
/*
    for (i=1; i < numEdges; i++) {
       index0 = edges[0][i];
       index1 = edges[1][i];
       j = i;
       while ((j > 0) && (edges[0][j-1] > index0)) {
         edges[0][j] = edges[0][j-1];
         edges[1][j] = edges[1][j-1];
         j = j - 1;
       }
       edges[0][j] = index0;
       edges[1][j] = index1;
    }
*/    

    // debugging output
    //for (i = 0; i < numEdges; i++) {
    //    debugprint(stddbg,"after heap [%i] %i %i\n",i,edges[0][i],edges[1][i]);
    //}

    // second pass
    // NOTE:  if you use the heap double heap sort above, this pass
    // should be unnecessary 

    for (i=1; i < numEdges; i++) {
       index0 = edges[0][i];
       index1 = edges[1][i];
       j = i;
       while ((j > 0) && ((edges[0][j-1] >= index0) && (edges[1][j-1] > index1))) {
         edges[0][j] = edges[0][j-1];
         edges[1][j] = edges[1][j-1];
         j = j - 1;
       }
       edges[0][j] = index0;
       edges[1][j] = index1;
    }

    // debugging output
    //for (i = 0; i < numEdges; i++) {
    //    debugprint(stddbg,"after insert [%i] %i %i\n",i,edges[0][i],edges[1][i]);
    //}

    // fix free nodes
    for (i = 0; i < numEdges-1; i++) {
      // if edge occurs twice, not a free edge
      if ((edges[0][i] == edges[0][i+1]) && (edges[1][i] == edges[1][i+1])) {
          i++;
      } else {
          debugprint(stddbg,"  Fixing Node: %i\n",edges[0][i]);
          debugprint(stddbg,"  Fixing Node: %i\n",edges[1][i]);
          // no slip code
          // this should be a bit set instead of an int!!
          iBC_[edges[0][i] - 1] = 56;
          iBC_[edges[1][i] - 1] = 56;
      }     
    } 

    // cleanup
    delete [] edges[0];
    delete [] edges[1];

    debugprint(stddbg,"Exiting fixFreeEdgeNodes.\n");
    return CV_OK;
}


void siftDownKentriesCalcMesh(int **ids, int root, int bottom, int array_size)
{
  int done, maxChild;
  int temp0,temp1,temp2;

  done = 0;
  while ((root*2 <= bottom) && (!done))
  {
    if (root*2 == bottom)
      maxChild = root * 2;
    else if (ids[0][root * 2] > ids[0][root * 2 + 1])
      maxChild = root * 2;
    else
      maxChild = root * 2 + 1;

    // hack since this seems to constantly end 
    if (root < 0 || root >= array_size) break;
    if (maxChild < 0 || maxChild >= array_size) {
      fprintf(stdout,"warning: illegal maxChild (%i)\n",maxChild);
      fprintf(stdout,"root= %d bottom=%d array_size=%d\n",root,bottom,array_size);
      break;
    }

    if (ids[0][root] < ids[0][maxChild])
    {
      temp0 = ids[0][root];
      temp1 = ids[1][root];
      temp2 = ids[2][root];
      ids[0][root] = ids[0][maxChild];
      ids[1][root] = ids[1][maxChild];
      ids[2][root] = ids[2][maxChild];
      ids[0][maxChild] = temp0;
      ids[1][maxChild] = temp1;
      ids[2][maxChild] = temp2;
      root = maxChild;
    }
    else
      done = 1;
  }
}


int createMeshForDispCalc(char *cmd) {

    // enter
    debugprint(stddbg,"Entering createMeshForDispCalc.\n");

    int i,j;

    // parse command string
    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    // count lines in file
    int eof = 0;
    int numLinesInFile = 0;
    while (NWgetNextNonBlankLine(&eof) == CV_OK) {
        numLinesInFile++;
    }
    NWcloseFile();

    debugprint(stddbg,"Number of lines in file: %i\n",numLinesInFile);

    if (numLinesInFile == 0) {
        return CV_ERROR;
    }
    
    int* ids[3];
    ids[0] = new int[numLinesInFile*3];
    ids[1] = new int[numLinesInFile*3];
    ids[2] = new int[numLinesInFile*3];
    if (parseFile(cmd) == CV_ERROR) {
        delete [] ids[0];
        delete [] ids[1];
        delete [] ids[2];
        return CV_ERROR;
    }

    
    int n0,n1,n2,n3;
    int elementId,matId;
    eof = 0;
    int numIds = 0;
    int numElements = 0;
    while (NWgetNextNonBlankLine(&eof) == CV_OK) {
 
        if (sscanf(buffer_,"%i %i %i %i %i",&elementId,&matId,&n0,&n1,&n2) != 5) {
            fprintf(stderr,"WARNING:  line not of correct format (%s)\n",buffer_);
            NWcloseFile();
            delete [] ids[0];
            delete [] ids[1];
            delete [] ids[2];
            return CV_ERROR;
        }

        ids[0][numIds] = n0;
        ids[1][numIds] = numElements;
        ids[2][numIds] = 0;
        numIds++;
        ids[0][numIds] = n1;
        ids[1][numIds] = numElements;
        ids[2][numIds] = 1;
        numIds++;
        ids[0][numIds] = n2;
        ids[1][numIds] = numElements;
        ids[2][numIds] = 2;
        numIds++;

        numElements++;
    }

    NWcloseFile();
 
    // sort node ids so we can find duplicates

    // debugging output
    //for (i = 0; i < numIds; i++) {
    //    debugprint(stddbg,"original:  %i %i %i %i\n",i,ids[0][i],ids[1][i],ids[2][i]);
    //}

    // heap sort

    int array_size = numIds;
    int temp0,temp1,temp2;

    for (i = (array_size / 2)-1; i >= 0; i--)
      siftDownKentriesCalcMesh(ids, i, array_size, array_size);

    for (i = array_size-1; i >= 1; i--) {
          temp0 = ids[0][0];
          temp1 = ids[1][0];
          temp2 = ids[2][0];
          ids[0][0] = ids[0][i];
          ids[1][0] = ids[1][i];
          ids[2][0] = ids[2][i];
          ids[0][i] = temp0;
          ids[1][i] = temp1;
          ids[2][i] = temp2;
          siftDownKentriesCalcMesh(ids, 0, i-1, array_size);
    }

    /*
    // insertion sort algorithm
    int index0, index1, index2;

    for (i=1; i < numIds; i++) {
       index0 = ids[0][i];
       index1 = ids[1][i];
       index2 = ids[2][i];
       j = i;
       while ((j > 0) && (ids[0][j-1] > index0)) {
         ids[0][j] = ids[0][j-1];
         ids[1][j] = ids[1][j-1];
         ids[2][j] = ids[2][j-1];
         j = j - 1;
       }
       ids[0][j] = index0;
       ids[1][j] = index1;
       ids[2][j] = index2;
    }
    */

    // debugging output
    //for (i = 0; i < numIds; i++) {
    //    debugprint(stddbg,"sorted:  %i %i %i %i\n",i,ids[0][i],ids[1][i],ids[2][i]);
    //}

    // count the number of unique nodes
    int numUniqueNodes = 1;
    int index0 = ids[0][0];
    for (i = 0; i < numIds; i++) {
        if (ids[0][i] != index0) {
            numUniqueNodes++;
            index0 = ids[0][i];
        }  
    }
    debugprint(stddbg,"  Number of Unique Nodes Found: %i\n",numUniqueNodes);

    // create renumbered connectivity for initial disp. calc.
    int* conn[3];
    conn[0] = new int[numElements];
    conn[1] = new int[numElements];
    conn[2] = new int[numElements];

    int* map = new int[numUniqueNodes];

    index0 = -1;
    j = 0;
    for (i = 0; i < numIds; i++) {
        if (ids[0][i] != index0) {
            map[j] = ids[0][i];
            //debugprint(stddbg,"  map[%i] %i\n",j,map[j]);
            index0 = ids[0][i];
            j++;
        }
        conn[ids[2][i]][ids[1][i]] = j - 1;
    }
    debugprint(stddbg,"  Number of Unique Nodes Found: %i\n",j);

    delete [] ids[0];
    delete [] ids[1];
    delete [] ids[2];

    // set global variables
    DisplacementNumElements_ = numElements;
    DisplacementConn_[0]     = conn[0];
    DisplacementConn_[1]     = conn[1];
    DisplacementConn_[2]     = conn[2];
    DisplacementNumNodes_    = numUniqueNodes;
    DisplacementNodeMap_     = map;

    debugprint(stddbg,"Exiting createMeshForDispCalc.\n");
    return CV_OK;
}

