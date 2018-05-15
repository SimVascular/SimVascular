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

#ifndef __CVCONVERTVIS_H
#define __CVCONVERTVIS_H

//#include "sys/param.h"
#define MAXPATHLEN 1024

#define MAXVISLINELENGTH 4096

#include "SimVascular.h"
#include "svPostExports.h" // For exports

#include "sv_RepositoryData.h"
#include "sv_UnstructuredGrid.h"
#include "sv_PolyData.h"

#ifdef SV_USE_ZLIB
  #ifdef SV_USE_SYSTEM_ZLIB
    #include <zlib.h>
  #else
    #include "simvascular_zlib.h"
  #endif
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

class SV_EXPORT_POST cvConvertVisFiles {

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
