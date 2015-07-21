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

#include <vector>
#include <algorithm>

using namespace std;

#include "vtkSmartPointer.h"
#include "vtkPoints.h"
#include "vtkUnstructuredGrid.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkIdList.h"
#include "vtkCellType.h"
#include "vtkPointData.h"
#include "vtkGeometryFilter.h"
#include "vtkCleanPolyData.h"
#include "vtkCellData.h"
#include "vtkCellArray.h"
#include "vtkExtractEdges.h"

#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkUnstructuredGridWriter.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkPolyDataWriter.h"
#include "vtkXMLUnstructuredGridReader.h"
#include "vtkUnstructuredGridReader.h"
#include "vtkXMLPolyDataReader.h"
#include "vtkPolyDataReader.h"
#include "vtkPolyDataNormals.h"
#include "vtkTransform.h"
#include "vtkTransformPolyDataFilter.h"
#include "vtkTriangleFilter.h"
#include "vtkTriangle.h"
#include "vtkFeatureEdges.h"
#include "vtkLinearExtrusionFilter.h"
#include "vtkPointLocator.h"
#include "vtkCellLocator.h"
#include "vtkGenericCell.h"

double PI=3.14159265358979323846;

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

int parseCmdStr2(char *cmd, char *mystr) {
    // parse command string
    int n = 0;
    int end = 0;
    char ignored[MAXSTRINGLENGTH];
    ignored[0]='\0';
    cmd_token_get (&n, cmd, ignored, &end);
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

double magnitude(double* vec){
    return sqrt(vec[0]*vec[0]+vec[1]*vec[1]+vec[2]*vec[2]);
}

void normalize(double* vec){
    double mag=magnitude(vec);
    if(mag>0.0){
        vec[0]/=mag;
        vec[1]/=mag;
        vec[2]/=mag;
    }
}

int geom_flatten(double* normal, int flag, vtkPolyData* inpd, double* rotated_norm, vtkPolyData** outpd) {

    double theta =acos(normal[2]);
    double rotvector[3];
    rotvector[0]=-normal[1];
    rotvector[1]=normal[0];
    rotvector[2]=0.0;

    double magrot=sqrt(rotvector[0]*rotvector[0]+rotvector[1]*rotvector[1]+rotvector[2]*rotvector[2]);

    vtkPolyData* tmpPD=vtkPolyData::New();
    tmpPD->Allocate(10,10);
    vtkPoints* tmpPt=vtkPoints::New();
    tmpPt->Allocate(10,10);
    double pt1[3];
    inpd->GetPoint(0,pt1);
    tmpPt->InsertNextPoint(pt1);
    vtkDoubleArray* tmpV=vtkDoubleArray::New();
    tmpV->SetNumberOfComponents(3);
    tmpV->Allocate(10,10);

    if(flag==1){
        tmpV->InsertNextTuple3(normal[0],normal[1],normal[2]);
    } else {
        tmpV->InsertNextTuple3(rotated_norm[0],rotated_norm[1],rotated_norm[2]);
    }
    tmpPD->SetPoints(tmpPt);
    tmpPD->GetPointData()->SetVectors(tmpV);

    if (magrot > 0.00001) {
        rotvector[0]/=magrot;
        rotvector[1]/=magrot;
        rotvector[2]/=magrot;

        double rtheta=(2.0*PI-theta)*180.0/PI;
        if(flag==0) {
            rtheta=-rtheta;
       }

       vtkTransform* myTrans=vtkTransform::New();
       myTrans->PostMultiply();
       myTrans->RotateWXYZ(rtheta,rotvector[0],rotvector[1],rotvector[2]);

       vtkTransformPolyDataFilter* myFilter=vtkTransformPolyDataFilter::New();
       myFilter->SetTransform(myTrans);

       myFilter->SetInputDataObject(tmpPD);
       myFilter->Update();
       double* new_norm=myFilter->GetOutput()->GetPointData()->GetVectors()->GetTuple3(0);
       normalize(new_norm);
       rotated_norm[0]=new_norm[0];
       rotated_norm[1]=new_norm[1];
       rotated_norm[2]=new_norm[2];

       myFilter->SetInputDataObject(inpd);
       myFilter->SetTransform(myTrans);
       myFilter->Update();

       (*outpd)=vtkPolyData::New();
       (*outpd)->DeepCopy(myFilter->GetOutput());

       myTrans->Delete();
       myFilter->Delete();
    } else {
        rotated_norm[0]=normal[0];
        rotated_norm[1]=normal[1];
        rotated_norm[2]=normal[2];
        (*outpd)=vtkPolyData::New();
        (*outpd)->DeepCopy(inpd);
    }

//    fprintf(stdout,"pptt %f %f %f\n",(*outpd)->GetPoint(400)[0],(*outpd)->GetPoint(400)[1],(*outpd)->GetPoint(400)[2]);

    tmpPD->Delete();
    tmpPt->Delete();
    tmpV->Delete();

    return CV_OK;
}

int geom_bbox( vtkPolyData* pd, double bbox[]){

    int numPts;
    double pt[3];
    int i;

    numPts=pd->GetNumberOfPoints();

    for ( i = 0; i < numPts; i++ ) {
        pd->GetPoints()->GetPoint(i, pt);
        if ( i == 0 ) {
            bbox[0] = bbox[1] = pt[0];
            bbox[2] = bbox[3] = pt[1];
            bbox[4] = bbox[5] = pt[2];
        }

        bbox[0] = std::min( bbox[0], pt[0]);
        bbox[1] = std::max( bbox[1], pt[0]);

        bbox[2] = std::min( bbox[2], pt[1]);
        bbox[3] = std::max( bbox[3], pt[1]);

        bbox[4] = std::min( bbox[4], pt[2]);
        bbox[5] = std::max( bbox[5], pt[2]);
    }

    return CV_OK;
}

int geom_translate( vtkPolyData* src, double translate[], vtkPolyData** dst )
{
    int i,numPts;
    double pt[3];

    vtkPolyData* result=vtkPolyData::New();
    result->DeepCopy(src);

    vtkPoints *pts = result->GetPoints();
    numPts = pts->GetNumberOfPoints();
    for ( i = 0; i < numPts; i++ ) {
        pts->GetPoint( i, pt );
        pt[0] += translate[0];
        pt[1] += translate[1];
        pt[2] += translate[2];
        pts->SetPoint( i, pt );
    }

    *dst = result;

    return CV_OK;
}

int geom_surf_area( vtkPolyData *pd, double *area ){

    double surf_area=0.0;

    vtkTriangleFilter* tri = vtkTriangleFilter::New();
    tri->SetInputData(pd);
    tri->Update();
    vtkPolyData* tri_pd = tri->GetOutput();

    for(int i = 0; i < tri_pd->GetNumberOfCells(); i++)
    {
        vtkCell* cell = tri_pd->GetCell(i);

        double p0[3];
        double p1[3];
        double p2[3];
        cell->GetPoints()->GetPoint(0, p0);
        cell->GetPoints()->GetPoint(1, p1);
        cell->GetPoints()->GetPoint(2, p2);

        surf_area+=vtkTriangle::TriangleArea(p0, p1, p2);
    }

    *area=surf_area;
    tri->Delete();

    return CV_OK;
}

double** createArray(int a, int b) {
    double ** rtn = new double*[a+1];
    if (rtn == NULL) {
        fprintf(stderr,"ERROR: Memory allocation error.\n");
        return NULL;
    }
    for (int i = 0; i < a+1; i++) {
        rtn[i] = new double[b+1];
        if (rtn[i] == NULL) {
            fprintf(stderr,"ERROR:  Memory allocation error.\n");
            return NULL;
        }
        for (int j = 0; j < b + 1; j++) {
            rtn[i][j] = 0.0;
        }
    }
    return rtn;
}

// dynamically deallocate a 2-dimensional array
void deleteArray(double **ptr, int a, int b) {
    for (int i = 0; i < a+1; i++) {
        delete ptr[i];
    }
    delete ptr;
}

int linearInterpolate(double **orgPts, int numOrgPts, double t0,
                            double dt, int numOutPts, double ***rtnOutPts) {

    // This method takes an original set of points and returns a
    // newly allocated set of interpolated points (where the requested
    // number of points is numOutPts).  Linear interpolation is used, and
    // the values outside of the range of orgPts are fixed to the values
    // at t_0 and t_n.

    int i,j;

    if (numOrgPts <= 0 || numOutPts <= 0) {
        return CV_ERROR;
    }

    double **outPts = createArray(numOutPts,2);
    if (*outPts == NULL) {
        return CV_ERROR;
    }

    double t;

    for (i=0; i < numOutPts; i++) {

        t = t0 + dt*i;

        outPts[i][0] = t;

        // if time is outside of data range, fix to values at beginning
        // and end of interval
        if (t <= orgPts[0][0]) {
          outPts[i][1] = orgPts[0][1];
          continue;
        } else if (t >= orgPts[numOrgPts-1][0]) {
          outPts[i][1] = orgPts[numOrgPts-1][1];
          continue;
         }

        // interpolate
        for (j = 1; j < numOrgPts; j++) {
          if (t < orgPts[j][0]) {
              double m = (orgPts[j][1]-orgPts[j-1][1])/(orgPts[j][0]-orgPts[j-1][0]);
              outPts[i][1] = m*(t - orgPts[j-1][0]) + orgPts[j-1][1];
              break;
          }
      }

      if (j == numOrgPts) {
          fprintf(stderr,"Error interpolating point %i.\n",i);
          deleteArray(outPts,numOutPts,2);
          return CV_ERROR;
      }
    }

    // debug
    //for (i = 0; i < numOutPts; i++) {
    //    fprintf(stdout,"%i: %8.3lf %8.3lf\n",i,outPts[i][0],outPts[i][1]);
    //}

    *rtnOutPts = outPts;

    return CV_OK;

}

#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

void mathFFT(double data[],int nn,int isign) {

    int n,mmax,m,j,istep,i;
    double wtemp,wr,wpr,wpi,wi,theta;
    double tempr,tempi;

    n=nn << 1;
    j=1;
    for (i=1;i<n;i+=2) {
        if (j > i) {
            SWAP(data[j-1],data[i-1]);
            SWAP(data[j+1-1],data[i+1-1]);
        }
        m=n >> 1;
        while (m >= 2 && j > m) {
            j -= m;
            m >>= 1;
        }
        j += m;
    }
    mmax=2;
    while (n > mmax) {
        istep=2*mmax;
        theta=6.28318530717959/(isign*mmax);
        wtemp=sin(0.5*theta);
        wpr = -2.0*wtemp*wtemp;
        wpi=sin(theta);
        wr=1.0;
        wi=0.0;
        for (m=1;m<mmax;m+=2) {
            for (i=m;i<=n;i+=istep) {
                j=i+mmax;
                tempr=wr*data[j-1]-wi*data[j+1-1];
                tempi=wr*data[j+1-1]+wi*data[j-1];
                data[j-1]=data[i-1]-tempr;
                data[j+1-1]=data[i+1-1]-tempi;
                data[i-1] += tempr;
                data[i+1-1] += tempi;
            }
            wr=(wtemp=wr)*wpr-wi*wpi+wr;
            wi=wi*wpr+wtemp*wpi+wi;
        }
        mmax=istep;
    }
}

#undef SWAP

int mathFFT(double **pts, int numPts, int numInterpPts, int numDesiredTerms, double ***rtnterms) {

    int i;

    if (numInterpPts <= 0 || numDesiredTerms <= 0 || numPts <= 0) {
        return CV_ERROR;
    }

    double **terms = createArray(numDesiredTerms,2);

    if (*terms == NULL) {
        return CV_ERROR;
    }

    // here we calculate dt so that our time series will go from
    // 0 to T - dt.

    double t0 = pts[0][0];
    double dt = (pts[numPts-1][0]-t0)/numInterpPts;
    double **outPts = NULL;

    if (linearInterpolate(pts, numPts, t0, dt, numInterpPts, &outPts) == CV_ERROR) {
        return CV_ERROR;
    }

    // create a real-imaginary array to do fft
    double *data = new double [2*numInterpPts];
    for (i = 0; i < numInterpPts; i++) {
        data[2*i] = outPts[i][1];
        data[2*i+1] = 0.0;
    }
    deleteArray(outPts,numInterpPts,2);

    mathFFT(data,numInterpPts,1);

    terms[0][0] = data[0]/numInterpPts;
    terms[0][1] = data[1]/numInterpPts;

    for (i=1;i<numDesiredTerms;i++) {
      terms[i][0]=2.0*data[2*i]/numInterpPts;
      terms[i][1]=2.0*data[2*i+1]/numInterpPts;
    }

    delete data;

    *rtnterms = terms;

    return CV_OK;

}

int inverseFFT(double **terms, int numTerms, double t0, double dt, double omega,
                         int numRtnPts, double ***rtnPts) {

  int i,j;
  double omega_t;

  double **pts = createArray(numRtnPts,2);
  if (pts == NULL) {
      return CV_ERROR;
  }

  for (i=0;i<numRtnPts;i++) {
    pts[i][0] = t0+i*dt;
    omega_t = omega*i*dt;
    pts[i][1] = terms[0][0];
    for (j=1;j<numTerms;j++) {
      pts[i][1] += terms[j][0]*cos(j*omega_t) + terms[j][1]*sin(j*omega_t);
    }
  }

  *rtnPts = pts;

  return CV_OK;

}

double distance(double p1[], double p2[]){
    double dx=p2[0]-p1[0];
    double dy=p2[1]-p1[1];
    double dz=p2[2]-p1[2];
    return sqrt(dx*dx+dy*dy+dz*dz);
}

double FindMachineEpsilon()
{
  double num = 1.0;
  double test = 1.0;

  while ( num + test > num ) {
    test /= 10.0;
  }
  return (test * 10.0);
}

int surface_center(vtkPolyData* pd, double ctr[]){

    double tol = 1e10 * FindMachineEpsilon();
    vtkCleanPolyData *merge = vtkCleanPolyData::New();
    merge->SetTolerance( tol );
    //  merge->ConvertLinesToPointsOn();  // new method as of vtk 3.2.0
    merge->SetInputDataObject(pd);
    merge->Update();

    vtkPolyData* mpd=merge->GetOutput();

    ctr[0] = ctr[1] = ctr[2] = 0.0;

    double pt[3];
    int numPts=mpd->GetNumberOfPoints();
    for (int i = 0; i < numPts; i++ ) {
        mpd->GetPoint(i,pt);
        ctr[0] += pt[0];
        ctr[1] += pt[1];
        ctr[2] += pt[2];
    }
    ctr[0] /= numPts;
    ctr[1] /= numPts;
    ctr[2] /= numPts;

    merge->Delete();

    return CV_OK;
}

int geom_intersect_with_line( vtkPolyData* pd, double p0[], double p1[], double intersect[])
{
    // return value
    intersect[0] = 0.0;
    intersect[1] = 0.0;
    intersect[2] = 0.0;

    double a0[3];
    double a1[3];
    double tol = 0.001;
    double t = 0.0;
    double x[3];
    double pcoords[3];
    int subId = 0;
    vtkIdType cellId = 0;

    for (int i=0; i < 3; i++) {
        a0[i]=p0[i];
        a1[i]=p1[i];
    }

    vtkCellLocator *locator = vtkCellLocator::New();
    vtkGenericCell *cell = vtkGenericCell::New();

    locator->SetDataSet(pd);
    locator->BuildLocator();

    //fprintf(stdout,"a0: %f %f %f\n",a0[0],a0[1],a0[2]);
    //fprintf(stdout,"a1: %f %f %f\n",a1[0],a1[1],a1[2]);
    //fprintf(stdout,"tol: %f\n",tol);

    x[0]=0;x[1]=0;x[2]=0;
    //if (locator->IntersectWithLine(a0, a1, tol, t, x, pcoords, subId) == 0) {
    if (locator->IntersectWithLine(a0,a1,tol,t,x,pcoords,subId,cellId,cell) == 0) {
        fprintf(stderr,"ERROR:  Line does not intersect vtkPolyData!\n");
        locator->Delete();
        cell->Delete();
        return CV_ERROR;
    }

    locator->Delete();
    cell->Delete();

    intersect[0]=x[0];intersect[1]=x[1];intersect[2]=x[2];

    return CV_OK;
}

int geom_create_ratio_map(vtkPolyData* inlet_mesh_face, double radmax, vtkPolyData** result) {

    int i,j;

//    fprintf(stdout,"Find free edges of inlet mesh face.\n");

    //extract the free edges from mesh
    vtkFeatureEdges* myFE=vtkFeatureEdges::New();
    myFE->SetInputDataObject(inlet_mesh_face);
    myFE->FeatureEdgesOff();
    myFE->NonManifoldEdgesOff();
    myFE->BoundaryEdgesOn();
    myFE->ColoringOff();
    myFE->Update();

    vtkPolyData* meshFreeEdges=vtkPolyData::New();
    meshFreeEdges->DeepCopy(myFE->GetOutput());

    //extrude mesh free edges
    vtkLinearExtrusionFilter* myLinFilt=vtkLinearExtrusionFilter::New();
    myLinFilt->SetExtrusionTypeToVectorExtrusion();
    myLinFilt->SetInputDataObject(meshFreeEdges);
    myLinFilt->SetVector(0,0,1);
    myLinFilt->SetScaleFactor(2);
    myLinFilt->Update();
    vtkPolyData* extrudedMeshWall=vtkPolyData::New();
    extrudedMeshWall->DeepCopy(myLinFilt->GetOutput());

//    fprintf(stdout,"Find interior nodes on mesh face (i.e. non-boundary nodes).\n");
    double p1[3],p2[3];
    vtkPoints* nodes=vtkPoints::New();
    for(i=0;i<inlet_mesh_face->GetNumberOfPoints();i++){
        int  onEdge=0;
        inlet_mesh_face->GetPoint(i,p1);
        for(j=0;j<meshFreeEdges->GetNumberOfPoints();j++){
            meshFreeEdges->GetPoint(j,p2);
            if(distance(p1,p2)<0.001){
                onEdge=1;
                break;
            }
        }
        if(onEdge==0){
            nodes->InsertNextPoint(p1[0],p1[1],p1[2]);
        }
    }
//    fprintf(stdout,"Found %d interior nodes.\n",nodes->GetNumberOfPoints());

    double ctrMesh[3];
    surface_center(meshFreeEdges,ctrMesh);
//    fprintf(stdout,"Center of mesh face: %f %f %f\n",ctrMesh[0],ctrMesh[1],ctrMesh[2]);


    //create a new set of scalars
    vtkDoubleArray* vScalars=vtkDoubleArray::New();
    vScalars->SetNumberOfComponents(1);
    vScalars->Allocate(100,100);

    //create point locator
    vtkPointLocator* pointLocator=vtkPointLocator::New();
    pointLocator->SetDataSet(inlet_mesh_face);
    pointLocator->AutomaticOn();
    pointLocator->SetTolerance(0.001);
    pointLocator->BuildLocator();

    for(i=0;i<inlet_mesh_face->GetNumberOfPoints();i++){
        vScalars->InsertNextTuple1(-radmax);
    }

    double node[3];

    for(i=0;i<nodes->GetNumberOfPoints();i++){
        nodes->GetPoint(i,node);

        double r_m_pt[3];
        r_m_pt[0]=node[0]-ctrMesh[0];
        r_m_pt[1]=node[1]-ctrMesh[1];
        r_m_pt[2]=0.0;
        double r_m=magnitude(r_m_pt);

        double angle=atan2(r_m_pt[1],r_m_pt[0]);//in radians

        double circle[2];
        circle[0]=150.0*cos(angle);
        circle[1]=150.0*sin(angle);

        double outsidePtMesh[3],ctrMesh_b[3];
        outsidePtMesh[0]=ctrMesh[0]+circle[0];
        outsidePtMesh[1]=ctrMesh[1]+circle[1];
        outsidePtMesh[2]=1.0;
        ctrMesh_b[0]=ctrMesh[0];
        ctrMesh_b[1]=ctrMesh[1];
        ctrMesh_b[2]=1.0;

        double bdryPtMesh[3];
        if(geom_intersect_with_line(extrudedMeshWall,ctrMesh_b,outsidePtMesh,bdryPtMesh)==CV_ERROR){
            //arbitrarily add a degree and try again and then give up
            circle[0]=10.0*cos(angle+PI/180);
            circle[1]=10.0*sin(angle+PI/180);
            outsidePtMesh[0]=ctrMesh[0]+circle[0];
            outsidePtMesh[1]=ctrMesh[1]+circle[1];
            outsidePtMesh[2]=1.0;
            geom_intersect_with_line(extrudedMeshWall,ctrMesh_b,outsidePtMesh,bdryPtMesh);
        }
        bdryPtMesh[2]=0.0;

        double R_m_pt[3];
        R_m_pt[0]=bdryPtMesh[0]-ctrMesh[0];
        R_m_pt[1]=bdryPtMesh[1]-ctrMesh[1];
        R_m_pt[2]=0.0;
        double R_m=magnitude(R_m_pt);
        if (r_m>R_m) {
            fprintf(stderr,"ERROR:  inside radius (%f) exceeds outside radius( %f).\n",r_m,R_m);
            fprintf(stderr,"ERROR1:  %d\n",i);
            return CV_ERROR;
        }

        double R_pc=radmax;
        double r_pc=r_m*R_pc/R_m;
        if(r_pc>R_pc){
            fprintf(stderr,"ERROR:  inside radius (%f) exceeds outside radius( %f).\n",r_pc,R_pc);
            fprintf(stderr,"ERROR2:  %d\n",i);
            return CV_ERROR;
        }

        //update velocities for mesh
        vtkIdType ptId=pointLocator->FindClosestPoint(node);
        vScalars->SetTuple1(ptId,r_pc);
    }

    //create the output object
    vtkPolyData* outputObj=vtkPolyData::New();
    outputObj->SetPoints(inlet_mesh_face->GetPoints());
    outputObj->CopyStructure(inlet_mesh_face);
    outputObj->GetPointData()->SetScalars(vScalars);

    (*result)=vtkPolyData::New();
    (*result)->DeepCopy(outputObj);

    myFE->Delete();
    meshFreeEdges->Delete();
    myLinFilt->Delete();
    extrudedMeshWall->Delete();
    nodes->Delete();
    vScalars->Delete();
    pointLocator->Delete();
    outputObj->Delete();

    return CV_OK;
}

double complex_mag (double z1[])

{
    double zmagsqd,zmag;
    zmagsqd = z1[0]*z1[0] + z1[1]*z1[1];
    zmag = sqrt(zmagsqd);

    return zmag;
}

int complex_mult (double z1[], double z2[], double zout[])

{
    zout[0] = z1[0]*z2[0] - z1[1]*z2[1];
    zout[1] = z1[1]*z2[0] + z1[0]*z2[1];

    return 0;
}

int complex_div (double z1[], double z2[], double zout[])

{
    double zmagsqd;

    zmagsqd = pow(complex_mag(z2),2.0);
    zout[0] = (z1[0]*z2[0] + z1[1]*z2[1])/zmagsqd;
    zout[1] = (z1[1]*z2[0] - z1[0]*z2[1])/zmagsqd;

    return 0;
}

double factrl(int n)
{
    static int ntop=4;
    static double a[33]={1.0,1.0,2.0,6.0,24.0};
    int j;

    if (n < 0) fprintf(stdout,"Negative factorial in routine FACTRL.\n");
    if (n > 32) fprintf(stdout,"input too large for factorial computation in routine FACTRL.\n");
    while (ntop<n) {
        j=ntop++;
        a[ntop]=a[j]*ntop;
    }
    return a[n];
}

int ber_bei (int order, double x, double z[]) {

    int k,kmax=30;
    double toler=1.0e-4,z_old[2];
    double x_term, arg,ca,sa,mu,mod,phase,sqrt2;

    z[0] = 0.0;
    z[1] = 0.0;

    if (fabs(x) < 15.0){
        for (k=0;k<=kmax;k++) {
            z_old[0] = z[0];
            z_old[1] = z[1];
            if (fabs(x) < toler && k==0) {
                x_term = 1.0;
            } else {
                x_term = pow((x*x/4.0),k);
            }
            arg = (3.0*order/4.0 + k/2.0)*PI;
            ca = cos(arg);
            sa = sin(arg);
            z[0] = z[0] + (ca/factrl(k))*(x_term/factrl(order+k));
            z[1] = z[1] + (sa/factrl(k))*(x_term/factrl(order+k));
        }

        if (fabs((z_old[0]-z[0])/z_old[0])>toler || fabs((z_old[1]-z[1])/z_old[1])>toler) {

            fprintf(stdout,"Error in ber_bei function: value did not converge !!! \n");
            //return CV_ERROR;

        }
        if (fabs(x) < toler && order==0) {
            /* do nothing */
        } else {
            z[0] = z[0]*pow((x/2.0),order);
            z[1] = z[1]*pow((x/2.0),order);
        }
    } else {

        sqrt2 = sqrt(2.0);
        mu = 4*order*order;
        mod = 1.0 - (mu-1.0)/(8.0*sqrt2*x) + (mu-1)*(mu-1)/(256.0*x*x)
                             -(mu-1.0)*(mu*mu + 14.0*mu - 399.0)/(6144.0*sqrt2*x*x*x);
        mod = mod*(exp(x/sqrt2)/(pow(2.0*PI*x,0.5)));
        phase = x/sqrt2 + (order/2.0 - 0.125)*PI + (mu-1.0)/(8.0*sqrt2*x) + (mu-1)/(16.0*x*x)
                             -(mu-1.0)*(mu-25.0)/(384.0*sqrt2*x*x*x);
        z[0] = mod*cos(phase);
        z[1] = mod*sin(phase);
    }

    return 0;
}

double compute_velocity (int k, double omega, double time, double radmax,
        double alpha_k, double Y, double Qk[]) {

    double z1[2],z2[2],z3[2],z4[2],z5[2],z6[2],z7[2],z8[2],z9[2],z10[2],v_kwt;

    ber_bei(0, Y*alpha_k, z1);
    ber_bei(0, alpha_k, z2);
    complex_div(z1,z2,z3);
    z3[0] = 1 - z3[0];
    z3[1] = -z3[1];
    ber_bei(1, alpha_k, z4);
    ber_bei(0, alpha_k, z5);
    z6[0] = -(alpha_k*sqrt(2.0))/2.0;
    z6[1] = (alpha_k*sqrt(2.0))/2.0;
    complex_mult(z5,z6,z7);
    complex_div(z4,z7,z8);
    z8[0] = 1 - 2.0*z8[0];
    z8[1] = -2.0*z8[1];
    complex_div(z3,z8,z9);
    complex_mult(Qk,z9,z10);

    v_kwt = (z10[0]*cos(k*omega*time) - z10[1]*sin(k*omega*time))/(PI*radmax*radmax);

    return v_kwt;

}

int compute_v_womersley(double **terms, int numTerms, double viscosity, double density,
        double omega, double radmax, double rad, double time, double *vel)
{
    int k;
    double Y;
    double v = 0;

    Y = rad/radmax;
    v = 2.0*terms[0][0]*(1.0 - pow(Y,2.0))/(PI*pow(radmax,2.0));

    for (k=1;k<numTerms;k++) {
        terms[k][1]=-terms[k][1];
        double alpha_k = radmax*pow(omega*k*density/viscosity,0.5);
        v += compute_velocity(k,omega,time,radmax,alpha_k,Y,terms[k]);
        terms[k][1]=-terms[k][1];//recover its original values
    }

    *vel = v;

    return CV_OK;

}

int VtkUtils_GetPoints( vtkPolyData *pd, double **pts, int *numPts )
{
    int i;
    double tmp[3];

    *numPts = pd->GetNumberOfPoints();
    if (pd->GetNumberOfPoints() == 0) {
        return CV_ERROR;
    }

    *pts = new double [(*numPts) * 3];

    for (i = 0; i < (*numPts); i++) {
        pd->GetPoint( i, tmp );
        (*pts)[3*i] = tmp[0];
        (*pts)[3*i+1] = tmp[1];
        (*pts)[3*i+2] = tmp[2];
    }

    return CV_OK;
}

int VtkUtils_GetAllPolys( vtkPolyData *pd, int *numPgns, vtkIdType **pgns )
{
    vtkCellArray *pdPgns;
    int size, i;
    vtkIdType npts, *pts;
    int pos = 0;

    (*numPgns) = pd->GetNumberOfPolys();
    pdPgns = pd->GetPolys();
    size = pdPgns->GetNumberOfConnectivityEntries();
    (*pgns) = new vtkIdType [size];

    pdPgns->InitTraversal();
    while ( pdPgns->GetNextCell( npts, pts ) ) {
        (*pgns)[pos] = npts;
        for (i = 0; i < npts; i++) {
            (*pgns)[pos+i+1] = pts[i];
        }
        pos += (npts+1);
    }

    if ( pos > size ) {
        fprintf(stderr,"ERROR [VtkUtils_GetAllPolys]: unexpected vtkCellArray result\n");
        delete [] (*pgns);
        return CV_ERROR;
    }

    return CV_OK;
}

int gdscCalcU(double xx[3][5], double *d, double r, double s, double *u) {

    double uval;
    int i;
    double rp,sp,rm,sm;
    double h[5];

    // routine to calcuate the value of u at (r,s)

    rp = 1.0 + r;
    sp = 1.0 + s;
    rm = 1.0 - r;
    sm = 1.0 - s;

    // interpolation functions

    h[1] = 0.25 * rp * sp;
    h[2] = 0.25 * rm * sp;
    h[3] = 0.25 * rm * sm;
    h[4] = 0.25 * rp * sm;

    uval = 0.0;
    for (i = 1;i <= 4; i++) {
        uval = uval + h[i]*d[i];
    }

    *u = uval;

    return CV_OK;

}

int gdscCalcJacDet(double xx[3][5], double r, double s, double *determinant) {

    /**************************************************************
     *  The code below was adapted from Finite Element Procedures *
     *  by K. Bathe, 1996, pages 482-3.                           *
     **************************************************************/

    *determinant = 0.0;

    int i,j,k;
    double rp,sp,rm,sm;
    double h[5],p[3][5],xj[3][3];
    double dum,det;

    rp = 1.0 + r;
    sp = 1.0 + s;
    rm = 1.0 - r;
    sm = 1.0 - s;

    // interpolation functions

    h[1] = 0.25 * rp * sp;
    h[2] = 0.25 * rm * sp;
    h[3] = 0.25 * rm * sm;
    h[4] = 0.25 * rp * sm;

    //
    // natural coordinate derviatives of the interpolation functions
    //

    // with respect to r

    p[1][1] = 0.25 * sp;
    p[1][2] = -p[1][1];
    p[1][3] = -0.25 * sm;
    p[1][4] = -p[1][3];

    // with respect to s

    p[2][1] = 0.25 * rp;
    p[2][2] = 0.25 * rm;
    p[2][3] = -p[2][2];
    p[2][4] = -p[2][1];

    // evaluate the jacobian at point (r,s)

    for (i = 1; i <= 2; i++) {
        for (j = 1; j <= 2; j++) {
            dum = 0;
            for (k = 1; k <= 4; k++) {
                dum = dum + p[i][k]*xx[j][k];
            }
            xj[i][j]=dum;
        }
    }

    // compute the determinant of the jacobian at point (r,s)
    det = xj[1][1]*xj[2][2] - xj[2][1]*xj[1][2];
    if (det < 0.0) {
        //fprintf(stderr,"ERROR: Jacobian determinant negative! (%lf)\n"  ,det);
        return CV_ERROR;
    }

    *determinant = det;
    return CV_OK;

}

int gdscIntegrateSurfElem(vtkFloatingPointType crd[4][3], vtkFloatingPointType *uvalues, double *q) {

  int i,j;
  double qflow = 0.0;
  double xx[3][5],d[5];
  double u,det;

  // 2-point gaussian integration point locations
  // note that for 2 pt gauss integration the weights are 1.0
  double a[3];
  a[0] = 0.0;
  a[1] = 0.577350269189626;
  a[2] = -a[1];

  *q = qflow;

  for (i = 1; i <= 4; i++) {
      for (j = 1; j <= 2; j++) {
          xx[j][i]=crd[i-1][j-1];
      }
  }

  // note that all of the code for using shape functions assumes
  // the indexes vary from 1 to 4, while the input to this
  // routine goes from 0 to 3.

  for (i = 1; i <= 4; i++) {
      d[i] = uvalues[i-1];
  }

  // calculate a sample determinant, and rearrange points
  // if det. is negative.
  if (gdscCalcJacDet(xx,a[1],a[1],&det) == CV_ERROR) {
      double swapd,swapxx[3];
      // swap point 1 -> 4
      swapxx[1] = xx[1][1] ; swapxx[2] = xx[2][1];
      xx[1][1] = xx[1][4] ; xx[2][1] = xx[2][4];
      xx [1][4] = swapxx[1] ; xx[2][4] = swapxx[2];
      // swap point 2 -> 3
      swapxx[1] = xx[1][2] ; swapxx[2] = xx[2][2];
      xx[1][2] = xx[1][3] ; xx[2][2] = xx[2][3];
      xx [1][3] = swapxx[1] ; xx[2][3] = swapxx[2];
      // swap d1 -> d4
      swapd = d[1];
      d[1] = d[4];
      d[4] = swapd;
      // swap d2 -> d3
      swapd = d[2];
      d[2] = d[3];
      d[3] = swapd;
      //fprintf(stderr,"Swapped points.\n");
  }

  qflow = 0.0;
  for (i = 1; i <= 2; i++) {
    for (j = 1; j <= 2; j++) {
      if (gdscCalcU(xx,d,a[i],a[j],&u) == CV_ERROR) {
        fprintf(stderr,"ERROR: Problem calculating u.\n");
        return CV_ERROR;
      }
      if (gdscCalcJacDet(xx,a[i],a[j],&det) == CV_ERROR) {
        fprintf(stderr,"ERROR: Jacobian determinant negative! (%lf)\n",det);
        return CV_ERROR;
      }
      if (det < 1E-6) {
          fprintf(stderr,"Warning: ignoring small element with det of (%e).\n",det);
          qflow = 0.0;
          *q = qflow;
          return CV_OK;
      } else {
        qflow = qflow + u*det;
      }
    }
  }

  *q = qflow;
  return CV_OK;

}

int geom_integrate_surface(vtkPolyData* pd, int tensorType, double *nrm, double *q )
{

  int i,j;
  vtkDataArray *scalars = NULL;
  vtkDataArray *vectors = NULL;
  int numPts, numPolys;
  double *pts;
  vtkIdType *polys;
  double qflow;
  double qtotal = 0.0;
  *q = qtotal;
  double crd[4][3];

  if (tensorType < 0 || tensorType > 1) {
      fprintf(stderr,"ERROR:  Invalid tensorType (%i).\n",tensorType);
      return CV_ERROR;
  }
  if (tensorType == 0) {
    scalars = pd->GetPointData()->GetScalars();
    if (scalars == NULL) {
        fprintf(stderr,"ERROR: No scalars!\n");
        return CV_ERROR;
    }
  } else {
    vectors = pd->GetPointData()->GetVectors();
    if (vectors == NULL) {
        fprintf(stderr,"ERROR: No vectors!\n");
        return CV_ERROR;
    }
  }

  if ( VtkUtils_GetPoints( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERROR: VtkUtils_GetPoints failed\n");
    return CV_ERROR;
  }
  if ( VtkUtils_GetAllPolys( pd, &numPolys, &polys ) != CV_OK ) {
    printf("ERRPR: VtkUtils_GetAllPolys failed\n");
    return CV_ERROR;
  }

  int polyIndex = 0;
  int conn[4];

  for (i = 0; i < numPolys; i++) {
      int nElemNodes = polys[polyIndex++];
      if (nElemNodes < 3 || nElemNodes > 4) {
          fprintf(stderr,"ERROR:  Invalid number of nodes in element (%i).\n",nElemNodes);
          return CV_ERROR;
      }

      conn[0]=polys[polyIndex++];
      conn[1]=polys[polyIndex++];
      conn[2]=polys[polyIndex++];
      if (nElemNodes == 3) {
          conn[3]=conn[2];
      } else {
          conn[3]=polys[polyIndex++];
      }

      for (j = 0; j < 4; j++) {
          crd[j][0]=pts[conn[j]*3+0];
          crd[j][1]=pts[conn[j]*3+1];
          crd[j][2]=pts[conn[j]*3+2];
      }

      vtkFloatingPointType uvalues[5];
      // if tensorType = 0, scalar is assumed to be through plane component
      if (tensorType == 0) {
          for (j = 0; j < 4; j++) {
              uvalues[j] = scalars->GetTuple1(conn[j]);
          }
      } else {
          // tensorType = 1, we need to dot the velocity vector with the surface normal
          for (j = 0; j < 4; j++) {
              vtkFloatingPointType v[3];
              vectors->GetTuple(conn[j],v);
              uvalues[j] = nrm[0]*v[0]+nrm[1]*v[1]+nrm[2]*v[2];
          }
      }

      qflow = 0.0;
      if (gdscIntegrateSurfElem(crd, uvalues, &qflow) == CV_ERROR) {
          fprintf(stderr,"ERROR:  Problem calculating surface integral.\n");
          *q = 0.0;
          return CV_ERROR;
      }
      qtotal = qtotal + qflow;
  }

  *q = qtotal;

  return CV_OK;
}

int geom_map_womersley_map(double** terms, int nterm,double time, double mu, double omega, double rho, double radmax,
        double flow_rate, vtkPolyData* inlet_mesh_face, vtkPolyData* radiusMap, double outwardUnitNormal[], int shape,
        int preserve, vtkPolyData** result){

    //vtkDoubleArray* r_pc_map=radiusMap->GetPointData()->GetScalars();
    vtkDoubleArray* r_pc_map=static_cast<vtkDoubleArray*>(radiusMap->GetPointData()->GetScalars());

    //create a new set of scalars and vectors
    vtkDoubleArray* vScalars=vtkDoubleArray::New();
    vScalars->SetNumberOfComponents(1);
    vScalars->Allocate(100,100);
    vtkDoubleArray* vVectors=vtkDoubleArray::New();
    vVectors->SetNumberOfComponents(3);
    vVectors->Allocate(100,100);
    int numNodes=inlet_mesh_face->GetNumberOfPoints();

    vScalars->SetNumberOfTuples(numNodes);
    vVectors->SetNumberOfTuples(numNodes);

    for(int i=0;i<numNodes;i++){
        double R_pc=radmax;
        double r_pc=r_pc_map->GetTuple1(i);

        if(r_pc<0){
            vScalars->SetTuple1(i,0);
            vVectors->SetTuple3(i,0,0,0);
            continue;
        }

        r_pc=fabs(r_pc);
        if(r_pc>R_pc){
            vScalars->SetTuple1(i,0);
            vVectors->SetTuple3(i,0,0,0);
        }else{
            double vel=0.0;
            switch(shape){
            case WOMERSLEY:
                if(compute_v_womersley(terms,nterm, mu, rho,
                                     omega, radmax, r_pc, time, &vel)==CV_ERROR){
                    fprintf(stderr,"ERROR: in womersley velocity calculation.\n");
                    return CV_ERROR;
                }
                break;
            case PARABOLIC:
                vel=1.0-r_pc*r_pc/(radmax*radmax);
                break;
            case PLUG:
                vel=1.0;
                break;
            default:
                fprintf(stderr,"ERROR: invalid BCT shape.\n");
                return CV_ERROR;
            }
            vScalars->SetTuple1(i,vel);
            vVectors->SetTuple3(i,outwardUnitNormal[0]*vel,outwardUnitNormal[1]*vel,outwardUnitNormal[2]*vel);
        }
    }

    vtkPolyData* outputObj=vtkPolyData::New();
    outputObj->SetPoints(inlet_mesh_face->GetPoints());
    outputObj->CopyStructure(inlet_mesh_face);
    outputObj->GetPointData()->SetScalars(vScalars);
    outputObj->GetPointData()->SetVectors(vVectors);

//    (*result)=vtkPolyData::New();
//    (*result)->DeepCopy(outputObj);

    double orgFlow=flow_rate;
    vtkPolyData* tmpresult=vtkPolyData::New();
    tmpresult->DeepCopy(outputObj);
    double newFlow=0.0;

    geom_integrate_surface(tmpresult,1,outwardUnitNormal,&newFlow);
    double scaleFactor=1.0;
    if(preserve==1){
        scaleFactor=orgFlow/newFlow;
    }
//    fprintf(stdout,"orgFlow: %f  newFlow: %f  ratio: %f\n",orgFlow,newFlow,scaleFactor);

    vtkDoubleArray* vectors=static_cast<vtkDoubleArray*>(tmpresult->GetPointData()->GetVectors());
    double* v;
    for(int i=0;i<vectors->GetNumberOfTuples();i++){
        v=vectors->GetTuple3(i);
        vectors->SetTuple3(i,scaleFactor*v[0],scaleFactor*v[1],scaleFactor*v[2]);
    }

    (*result)=vtkPolyData::New();
    (*result)->DeepCopy(tmpresult);

    double scaledFlow=0.0;
    geom_integrate_surface(tmpresult,1,outwardUnitNormal,&scaledFlow);
//    fprintf(stdout,"scaled flow rate:  %f\n",scaledFlow);

    vScalars->Delete();
    vVectors->Delete();
    outputObj->Delete();
    tmpresult->Delete();

    return CV_OK;
}

int create_bct(BCTData& bct,char *faceFile, char *flowFile,double rho, double mu, int shape,
        double period, int pointNum,int modeNum, int preserve, int flip){

    int i,j;
    pointNum=pointNum-1;

    // read times and flow rates and process
    //======================================================================
    FILE* fp = NULL;
    fp=fopen(flowFile,"r");
    if(fp==NULL){
        fprintf(stderr, "ERROR: could not open file (%s).", flowFile);
        return CV_ERROR;
    }

    vector<double> tvec,flvec;
    char line[MAXCMDLINELENGTH];
    line[0]='\0';
    double t,flowrate;
    while (fgets (line, MAXCMDLINELENGTH, fp) != (char *)0) {

        if (line[0] == '#' || line[0] == '\n'|| line[0] == '\r' || line[0] == ' ') {
            continue;
        }

        if(sscanf(line, "%lf %lf", &t,&flowrate)!=2){
            fclose (fp);
            fprintf(stderr,"ERROR:  Incorrect format of line in flow rate file (%s).\n", flowFile);
            return CV_ERROR;
        }

        tvec.push_back(t);
        flvec.push_back(flowrate);

        line[0]='\0';
    }
    fclose (fp);

    int numPts=tvec.size();
//    fprintf(stdout,"Found %i points in %s\n",numPts,flowFile);

    double **pts = createArray(numPts,2);

    for(i=0;i<numPts;i++){
        pts[i][0] = tvec[i];
        pts[i][1] = flvec[i];
    }

    double **terms = NULL;
    if (mathFFT(pts, numPts, 256, modeNum, &terms) == CV_ERROR) {
        fprintf(stderr,"ERROR: math FFT in (%s).\n",flowFile);
        return CV_ERROR;
    }

    double omega=2*PI/period;

    double **plotme = NULL;
    if (inverseFFT(terms, modeNum, 0,period/pointNum, omega, pointNum, &plotme) == CV_ERROR){
        fprintf(stderr,"ERROR: math inverseFFT in (%s).\n",flowFile);
        return CV_ERROR;
    }

    //read vtp file and process
    //=======================================================================
    vtkXMLPolyDataReader* reader=vtkXMLPolyDataReader::New();
    reader->SetFileName(faceFile);
    reader->Update();

    vtkPolyData* pd=NULL;
    pd=reader->GetOutput();
    if(pd==NULL){
        fprintf(stderr,"ERROR: problem parsing file (%s).\n",faceFile);
        return CV_ERROR;
    }
    int nodeNum=pd->GetNumberOfPoints();

    //calculate normals for nodes
    vtkCleanPolyData* cleaner=vtkCleanPolyData::New();
    cleaner->SetTolerance(0.0001);
    cleaner->SetInputDataObject(pd);
    cleaner->Update();

    vtkPolyData* pdMerged=cleaner->GetOutput();
    vtkPolyDataNormals* pdNormals=vtkPolyDataNormals::New();
    pdNormals->SetInputData(pdMerged);
    pdNormals->AutoOrientNormalsOn();
    pdNormals->ComputePointNormalsOn();
    //pdNormals->FlipNormalsOn();
    pdNormals->SplittingOn();
    pdNormals->ComputeCellNormalsOn();
    pdNormals-> ConsistencyOn();
    pdNormals->NonManifoldTraversalOff();
    pdNormals->Update();

    //calculate average normal
    vtkDoubleArray* normals=static_cast<vtkDoubleArray*>(pdNormals->GetOutput()->GetPointData()->GetNormals());

    int numNodes=normals->GetNumberOfTuples();

    double nrmave[3];
    nrmave[0]=0.0;
    nrmave[1]=0.0;
    nrmave[2]=0.0;
    double* nrm;

    for(i=0;i<numNodes;i++){
        nrm=normals->GetTuple3(i);
        nrmave[0]=nrmave[0]+nrm[0];
        nrmave[1]=nrmave[1]+nrm[1];
        nrmave[2]=nrmave[2]+nrm[2];
    }

    nrmave[0]=nrmave[0]/numNodes;
    nrmave[1]=nrmave[1]/numNodes;
    nrmave[2]=nrmave[2]/numNodes;

    //set the average normal as outward
    //pd->GetCellData()->SetActiveScalars("GlobalElementID");
    int elementId=pd->GetCellData()->GetScalars("GlobalElementID")->GetTuple1(0);
    int n0,n1,n2,n3;
    for (i = 0; i < numBoundaryFaces_; i++) {
        if (boundaryElementsIds_[i] == (elementId - 1)) {
            n0=boundaryElements_[0][i];
            n1=boundaryElements_[1][i];
            n2=boundaryElements_[2][i];
            n3=boundaryElements_[3][i];
        }
    }

    double dotProduct = 0.0;
    for(i=0;i<3;i++){
        dotProduct = dotProduct + nrmave[i]*(nodes_[i*numNodes_+n3-1]-nodes_[i*numNodes_+n0-1]);
    }

    if(dotProduct>0.0){
        nrmave[0] = -nrmave[0];
        nrmave[1] = -nrmave[1];
        nrmave[2] = -nrmave[2];
    }

    //flip normal
    if(flip==1){
        nrmave[0] = -nrmave[0];
        nrmave[1] = -nrmave[1];
        nrmave[2] = -nrmave[2];
    }

//    fprintf(stdout,"outward nrm: %.15f %.15f %.15f\n",nrmave[0],nrmave[1],nrmave[2]);

    double* rotated_norm=new double[3]();
    vtkPolyData* flat_pd=NULL;

    if(geom_flatten(nrmave,1,pd,rotated_norm,&flat_pd)==CV_ERROR){
        return CV_ERROR;
    }

//    fprintf(stdout,"rotated nrm: %.15e %.15e %.15e\n",rotated_norm[0],rotated_norm[1],rotated_norm[2]);

    double outwardUnitNormal[3];
    if(rotated_norm[2]>0){
        outwardUnitNormal[0]=0.0;
        outwardUnitNormal[1]=0.0;
        outwardUnitNormal[2]=1.0;
    } else {
        outwardUnitNormal[0]=0.0;
        outwardUnitNormal[1]=0.0;
        outwardUnitNormal[2]=-1.0;
    }

    double bbox[6];
    geom_bbox(flat_pd,bbox);

    double translate_vec[]={0,0,-bbox[4]};
    vtkPolyData* flat_pd0=NULL;
    geom_translate(flat_pd,translate_vec,&flat_pd0);

    double area;
    geom_surf_area(flat_pd0,&area);
//    fprintf(stdout,"area: %f\n",area);

    double radmax=sqrt(area/PI);
//    fprintf(stdout,"effective radius: %f\n",radmax);

    vtkPolyData* radiusMap=NULL;
    geom_create_ratio_map(flat_pd0,radmax,&radiusMap);

    translate_vec[0]=0;
    translate_vec[1]=0;
    translate_vec[2]=bbox[4];

    bct.pointNum=pointNum;
    bct.pd=vtkPolyData::New();
    bct.pd->DeepCopy(pd);
    bct.t=new double[pointNum+1];
    bct.mapped_data=new vtkDoubleArray*[pointNum];

    for(j=0;j<pointNum;j++){
        double time=plotme[j][0];
        double flow_rate=plotme[j][1];

        vtkPolyData* mapped_flat_0=NULL;
        if(geom_map_womersley_map(terms,modeNum,time,mu,omega,rho,radmax,flow_rate,flat_pd0,
                radiusMap,outwardUnitNormal,shape,preserve,&mapped_flat_0)==CV_ERROR){
            return CV_ERROR;
        }

        vtkPolyData* mapped_flat=NULL;
        geom_translate(mapped_flat_0,translate_vec,&mapped_flat);

        vtkPolyData* mapped=NULL;
        double back_to_norm[3];
        back_to_norm[0]=rotated_norm[0];
        back_to_norm[1]=rotated_norm[1];
        back_to_norm[2]=rotated_norm[2];

        if(geom_flatten(nrmave,0,mapped_flat,back_to_norm,&mapped)==CV_ERROR){
            return CV_ERROR;
        }

    //check normal
//    fprintf(stdout,"noriginal nrm : %f %f %f\n",nrmave[0],nrmave[1],nrmave[2]);
//    fprintf(stdout,"rotated  nrm :  %f %f %f\n",rotated_norm[0],rotated_norm[1],rotated_norm[2]);
//    fprintf(stdout,"back to nrm  :  %f %f %f\n",back_to_norm[0],back_to_norm[1],back_to_norm[2]);
//    fprintf(stdout,"outward unit : %f %f %f\n",outwardUnitNormal[0],outwardUnitNormal[1],outwardUnitNormal[2]);


        vtkDoubleArray* mapped_data=vtkDoubleArray::New();
        mapped_data->DeepCopy(mapped->GetPointData()->GetVectors());
        bct.t[j]=time;
        bct.mapped_data[j]=mapped_data;

        //mapped_flat_0->Delete();
        //mapped_flat->Delete();
        //mapped->Delete();
    }

    bct.t[pointNum]=period;

    reader->Delete();
    cleaner->Delete();
    pdNormals->Delete();

    return CV_OK;
}
