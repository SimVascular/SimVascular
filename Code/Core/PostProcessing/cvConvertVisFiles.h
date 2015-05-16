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

#ifndef __CVCONVERTVIS_H
#define __CVCONVERTVIS_H

//#include "sys/param.h"
#define MAXPATHLEN 1024

#define MAXVISLINELENGTH 4096

#include "SimVascular.h"

#include "cvRepositoryData.h"
#include "cvUnstructuredGrid.h"
#include "cvPolyData.h"

#ifdef USE_ZLIB
#include "simvascular_zlib.h"
#else
#include <stdlib.h>
#define gzopen fopen
#define gzprintf fprintf
#define gzFile FILE*
#define gzclose fclose
#define Z_NULL NULL
#define gzeof feof
//gzgets requires different args than fgets
//#define gzgets fgets
#endif

#define NEXTLINE_EOF -1
#define NEXTLINE_OK 0
#define NEXTLINE_DATASTR 1

// -------------------
// cvConvertVisFiles
// -------------------

class cvConvertVisFiles {

  public:

    cvConvertVisFiles();
    ~cvConvertVisFiles();

    // mesh
    int ReadVisMesh(char *infilename);
    cvUnstructuredGrid* GetGridObj();
    void SetGrid(cvUnstructuredGrid *obj) {grid_=(vtkUnstructuredGrid*)(obj->GetVtkPtr());}
    void SetTractionNodes(int numnodes, int* nodes);
    
    // results
    int ReadVisRes(char *infilename);
    cvPolyData* GetPressureObj();
    cvPolyData* GetVelocityObj();
    cvPolyData* GetResObj();
    cvPolyData* GetStressObj();
    cvPolyData* GetTransportObj();
    cvPolyData* GetTractionObj();
    cvPolyData* GetDisplacementObj();
    cvPolyData* GetWSSObj();

  protected:

    int openInputFile(char* filename, gzFile* fp);
    int closeInputFile(gzFile fp);

    int readVelocityFromFile();
    int readPressureFromFile();
    int readTransportFromFile();
    int readStressFromFile();
    int readTractionFromFile();
    int readDisplacementFromFile();
    int readWSSFromFile();
    
    int findStringInFile(char *findme, gzFile fp);
    int readNextLineFromFile(gzFile fp);

  private:
    
    gzFile meshfp_;
    gzFile resfp_;
    char meshfilename_[MAXPATHLEN];
    char resfilename_[MAXPATHLEN];

    int  numTractionNodes_;
    int* tractionNodes_;
    
    int meshLoaded_;
    int resLoaded_;
    int meshExported_;

    int haveVelocityResults_;
    int havePressureResults_;
    int haveTransportResults_;
    int haveStressResults_;
    int haveTractionResults_;
    int haveDisplacementResults_;
    int haveWSSResults_;

    char currentLine_[MAXVISLINELENGTH];
    
    vtkPoints* meshpts_;
    vtkUnstructuredGrid* grid_;
    vtkFloatingPointArrayType* pressure_;
    vtkFloatingPointArrayType* velocity_;
    vtkFloatingPointArrayType* transport_;
    vtkFloatingPointArrayType* stress_;
    vtkFloatingPointArrayType* traction_;
    vtkFloatingPointArrayType* displacement_;
    vtkFloatingPointArrayType* wss_;
    
};

#endif
