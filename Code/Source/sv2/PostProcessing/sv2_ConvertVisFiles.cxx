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

#include "SimVascular.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "sv_VTK.h"
#include "sv_PolyData.h"
#include "sv_vtk_utils.h"

#include "sv2_ConvertVisFiles.h"

// -------------------
// cvConvertVisFiles
// -------------------

cvConvertVisFiles::cvConvertVisFiles() {
    meshfp_= NULL;
    resfp_ = NULL;
    meshfilename_[0] = '\0';
    resfilename_[0] = '\0';

    numTractionNodes_ = 0;
    tractionNodes_ = NULL;

    meshLoaded_ = 0;
    resLoaded_ = 0;
    meshExported_ = 0;
    pressure_ = NULL;
    transport_ = NULL;
    stress_ = NULL;
    velocity_ = NULL;
    traction_ = NULL;
    displacement_ = NULL;
    wss_ = NULL;
    haveVelocityResults_ = 0;
    havePressureResults_ = 0;
    haveTransportResults_ = 0;
    haveStressResults_ = 0;
    haveTractionResults_ = 0;
    haveDisplacementResults_ = 0;
    haveWSSResults_ = 0;
    currentLine_[0]  = '\0';
    meshpts_ = NULL;
    grid_ = NULL;
}


// --------------------
// ~cvConvertVisFiles
// --------------------

cvConvertVisFiles::~cvConvertVisFiles() {

    //fprintf(stdout,"inside of destructor: meshExported_ %i meshLoaded %i resLoaded_ %i\n",meshExported_,meshLoaded_,resLoaded_);
    // here we rely on the reference counting
    // if the user has requested a vtk object
    // using a Get... method of this class.

    // delete the mesh if the user didn't ask for it
    if ((meshExported_ == 0) && (meshLoaded_ == 1)) {
        //fprintf(stdout,"debug: deleting mesh\n");
      if (meshpts_ != NULL) {
         meshpts_->Delete();
      }
      if (grid_ != NULL) {
         grid_->Delete();
      }
    }
    // delete the results objs
    if (resLoaded_ == 1) {
        //fprintf(stdout,"debug: deleting results\n");
      if ((havePressureResults_ == 1) && (pressure_ != NULL)) {
            pressure_->Delete();
      }
      if ((haveVelocityResults_ == 1) && (velocity_ != NULL)) {
            velocity_->Delete();
      }
      if ((haveTransportResults_ == 1) && (transport_ != NULL)) {
            transport_->Delete();
      }
      if ((haveStressResults_ == 1) && (stress_ != NULL)) {
            stress_->Delete();
      }
      if ((haveTractionResults_ == 1) && (traction_ != NULL)) {
            traction_->Delete();
      }
      if ((haveDisplacementResults_ == 1) && (displacement_ != NULL)) {
            displacement_->Delete();
      }
      if ((haveWSSResults_ == 1) && (wss_ != NULL)) {
            wss_->Delete();
      }
    }

    if (numTractionNodes_ != 0) {
        delete [] tractionNodes_;
    }
}


int cvConvertVisFiles::openInputFile(char* filename, gzFile* fp) {
    // open the output file
    *fp = NULL;
    *fp = gzopen (filename, "rb");
    if (*fp == Z_NULL) {
      fprintf(stderr,"Error: Could not open input file %s.\n",filename);
      return SV_ERROR;
    }
    return SV_OK;
}

int cvConvertVisFiles::closeInputFile(gzFile fp) {
  gzclose(fp);
  return SV_OK;
}


int cvConvertVisFiles::ReadVisMesh(char *infilename) {

    // open the mesh file
    if (openInputFile(infilename, &meshfp_) == SV_ERROR) {
        return SV_ERROR;
    }

    if (meshfp_ == NULL) {
        return SV_ERROR;
    }
    //
    // read the nodal coordinates
    //

    // skip until we find the string
    if (findStringInFile("number of nodal coordinates", meshfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find number of nodal coordinates.\n");
        closeInputFile(meshfp_);
        return SV_ERROR;
    }

    int numNodes = 0;
    if (sscanf(currentLine_,"    number of nodal coordinates %i\n",&numNodes) != 1) {
      fprintf(stderr,"ERROR: bad format of number of nodal coordinates (%s).\n",currentLine_);
      closeInputFile(meshfp_);
      return SV_ERROR;
    }

    fprintf(stdout,"Reading %i nodes.\n",numNodes);

    // skip until we find the string
    if (findStringInFile("nodal coordinates",meshfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find nodal coordinates.");
        closeInputFile(meshfp_);
        meshpts_->Delete();
        return SV_ERROR;
    }

    vtkPoints* meshpts_ = vtkPoints::New();
    meshpts_->Allocate(numNodes+2,10000);
    meshpts_->SetNumberOfPoints(numNodes);
    meshpts_->Initialize();

    double dpt[3];
    dpt[0] = 0;dpt[1] = 0;dpt[2] = 0;
    vtkFloatingPointType  fpt[3];
    int nodeid = 0;

    while (0 == 0) {
      int flag = readNextLineFromFile(meshfp_);
      if (flag == NEXTLINE_EOF) {
          closeInputFile(meshfp_);
          meshpts_->Delete();
          return SV_ERROR;
      }
      if (strstr(currentLine_,"end node coordinates") != NULL) {
          break;
      }
      if (strstr(currentLine_,"end nodal coordinates") != NULL) {
          break;
      }
      if (sscanf(currentLine_,"%i %lf %lf %lf",&nodeid,&dpt[0],&dpt[1],&dpt[2]) != 4) {
          fprintf(stderr,"ERROR: invalid line (%s).\n",currentLine_);
          closeInputFile(meshfp_);
          meshpts_->Delete();
          return SV_ERROR;
      }

      // convert to vtkFloatingPointTypes for vtk
      fpt[0] = dpt[0]; fpt[1] = dpt[1]; fpt[2] = dpt[2];

      // in the tcl scripts, we keep track of a mapping between node ids
      // and their location in the vtk pts list.  Here we just assume
      // the pts are numbered from 1 to numNodes, and return an error
      // otherwise.
      //set map($node) $numNodes

      if (nodeid < 1 || nodeid > numNodes) {
          fprintf(stderr,"ERROR:  node id (%i) out of allowable range [1,%i].",nodeid,numNodes);
          closeInputFile(meshfp_);
          meshpts_->Delete();
          return SV_ERROR;
      }
      //fprintf(stdout,"insert pt: %i value: %f %f %f\n",nodeid-1,fpt[0],fpt[1],fpt[2]);
      meshpts_->InsertPoint(nodeid-1,fpt);

    }

    fprintf(stdout,"Done reading %i nodes.\n",meshpts_->GetNumberOfPoints());


    //
    //  read element connectivity
    //

    // skip until we find the string
    if (findStringInFile("nodes per element",meshfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find nodes per element.\n");
        closeInputFile(meshfp_);
        meshpts_->Delete();
        return SV_ERROR;
    }
    int nodesPerElement = 0;
    if (sscanf(currentLine_,"      nodes per element %i\n",&nodesPerElement) != 1) {
      fprintf(stderr,"ERROR: could not find number of nodes per element!\n");
      closeInputFile(meshfp_);
      meshpts_->Delete();
      return SV_ERROR;
    }

    // skip until we find the string
    if (findStringInFile("number of elements",meshfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find number of elements.\n");
        closeInputFile(meshfp_);
        meshpts_->Delete();
        return SV_ERROR;
    }

    int numElements = 0;
    if (sscanf(currentLine_,"      number of elements %i\n",&numElements) != 1) {
      fprintf(stderr,"ERROR: could not find number of elements!\n");
      closeInputFile(meshfp_);
      meshpts_->Delete();
      return SV_ERROR;
    }

    // skip until we find the string "connectivity"
    if (findStringInFile("connectivity",meshfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find connectivity.\n");
        closeInputFile(meshfp_);
        meshpts_->Delete();
        return SV_ERROR;
    }

    fprintf(stdout,"Reading %i elements.\n",numElements);

    // create the unstructured grid object
    grid_ = vtkUnstructuredGrid::New();
    grid_->Allocate(numElements+2,1000);
    grid_->SetPoints(meshpts_);

    vtkIdList* ptids = vtkIdList::New();
    ptids->Allocate(10,10);
    ptids->Initialize();
    ptids->SetNumberOfIds(10);

    int conn[10];
    conn[0] = 0;conn[1] = 0;conn[2] = 0;conn[3] = 0;
    conn[4] = 0;conn[5] = 0;conn[6] = 0;conn[7] = 0;
    int elementid = 0;
    int currnum = 0;

    while (0 == 0) {

      int flag = readNextLineFromFile(meshfp_);
      if (flag == NEXTLINE_EOF) {
          closeInputFile(meshfp_);
          meshpts_->Delete();
          ptids->Delete();
          grid_->Delete();
          return SV_ERROR;
      }
      if (strstr(currentLine_,"end connectivity") != NULL) {
          break;
      }

      if (nodesPerElement == 4) {
        if (sscanf(currentLine_,"%i %i %i %i %i",&elementid,
                &conn[0],&conn[1],&conn[2],&conn[3]) != 5) {
          fprintf(stderr,"ERROR: invalid line (%s).\n",currentLine_);
          closeInputFile(meshfp_);
          meshpts_->Delete();
          ptids->Delete();
          grid_->Delete();
          return SV_ERROR;
        }
        ptids->SetNumberOfIds(4);
        ptids->SetId(0,conn[0]-1);ptids->SetId(1,conn[1]-1);
        ptids->SetId(2,conn[2]-1);ptids->SetId(3,conn[3]-1);
      } else if (nodesPerElement == 8) {
        if (sscanf(currentLine_,"%i %i %i %i %i %i %i %i %i",&elementid,
                &conn[0],&conn[1],&conn[2],&conn[3],
                &conn[4],&conn[5],&conn[6],&conn[7]) != 9) {
          fprintf(stderr,"ERROR: invalid line (%s).\n",currentLine_);
          closeInputFile(meshfp_);
          meshpts_->Delete();
          ptids->Delete();
          grid_->Delete();
          return SV_ERROR;
        }
        ptids->SetNumberOfIds(8);
        ptids->SetId(0,conn[0]-1);ptids->SetId(1,conn[1]-1);
        ptids->SetId(2,conn[2]-1);ptids->SetId(3,conn[3]-1);
        ptids->SetId(4,conn[4]-1);ptids->SetId(5,conn[5]-1);
        ptids->SetId(6,conn[6]-1);ptids->SetId(7,conn[7]-1);
      } else {
        fprintf(stderr,"ERROR: invalid nodes per element (%i).\n",nodesPerElement);
        closeInputFile(meshfp_);
        meshpts_->Delete();
        ptids->Delete();
        grid_->Delete();
        return SV_ERROR;
      }

      if (elementid < 1 || elementid > numElements) {
          fprintf(stderr,"ERROR:  element id (%i) out of allowable range [1,%i].\n",nodeid,numElements);
          closeInputFile(meshfp_);
          meshpts_->Delete();
          ptids->Delete();
          grid_->Delete();
          return SV_ERROR;
      }

      if ((elementid-1) != currnum) {
          fprintf(stderr,"\n\nWARNING: Elements are being renumbered!!!\n\n");
      }
      if (nodesPerElement == 4) {
        grid_->InsertNextCell(VTK_TETRA,ptids);
      } else if (nodesPerElement == 8) {
        grid_->InsertNextCell(VTK_HEXAHEDRON,ptids);
      } else {
        closeInputFile(meshfp_);
        meshpts_->Delete();
        ptids->Delete();
        grid_->Delete();
        return SV_ERROR;
      }

      currnum++;

    }

    ptids->Delete();

    fprintf(stdout,"Done reading %i elements.\n",numElements);

    if (closeInputFile(meshfp_) == SV_ERROR) {
        meshpts_->Delete();
        grid_->Delete();
        return SV_ERROR;
    }

    meshLoaded_ = 1;

    fprintf(stdout,"debug: number of grid points: %i\n",grid_->GetNumberOfPoints());
    fprintf(stdout,"debug: number of tets: %i\n",grid_->GetNumberOfCells());

    return SV_OK;

}

int cvConvertVisFiles::readNextLineFromFile(gzFile fp) {

#ifdef SV_USE_ZLIB
    if (gzgets(fp,currentLine_,MAXVISLINELENGTH) ==Z_NULL) {
#else
    if (fgets(currentLine_,MAXVISLINELENGTH,fp) == NULL) {
#endif
        //fprintf(stderr,"ERROR:  readNextLine failed.\n");
        return SV_ERROR;
    }

    return SV_OK;

}

int cvConvertVisFiles::findStringInFile(char *findme, gzFile fp) {

    while (readNextLineFromFile(fp) == SV_OK) {
      if (strstr(currentLine_,findme) != NULL) {
          return SV_OK;
      }
    }
    return SV_ERROR;

}



int cvConvertVisFiles::ReadVisRes(char *infilename) {

    haveVelocityResults_ = 0;
    havePressureResults_ = 0;
    haveTransportResults_ = 0;
    haveStressResults_ = 0;
    haveTractionResults_ = 0;
    haveDisplacementResults_ = 0;

    // check and make sure the mesh has been loaded
    if (grid_ == NULL) {
        fprintf(stderr,"ERROR: no mesh loaded!\n");
        return SV_ERROR;
    }

    // open the results file
    if (openInputFile(infilename, &resfp_) == SV_ERROR) {
        return SV_ERROR;
    }

    if (resfp_ == NULL) {
        return SV_ERROR;
    }

    // loop over the file looking for all analysis results

    while (0 == 0) {

      if (findStringInFile("analysis results", resfp_) == SV_ERROR) {
        if (gzeof(resfp_)) {
            closeInputFile(resfp_);
            if ((haveVelocityResults_ + havePressureResults_ +
                 haveTransportResults_ + haveStressResults_) == 0) {
                fprintf(stderr,"ERROR: no results found!\n");
                return SV_ERROR;
            }
            // eof file reached, no error.
            resLoaded_ = 1;
            return SV_OK;
        }
        fprintf(stderr,"ERROR:  of unknown origin.\n");
        closeInputFile(resfp_);
        return SV_ERROR;
      }

      if (strstr(currentLine_,"end analysis results") != NULL) {
        continue;
      }

      if (strstr(currentLine_,"pressure") != NULL) {
        readPressureFromFile();
      } else if (strstr(currentLine_,"velocity") != NULL) {
        readVelocityFromFile();
      } else if (strstr(currentLine_,"transport") != NULL) {
        readTransportFromFile();
      } else if (strstr(currentLine_,"species_") != NULL) {
        fprintf(stderr,"WARNING: ignoring results (%s).\n",currentLine_);
      } else if (strstr(currentLine_,"species") != NULL) {
        readTransportFromFile();
      } else if (strstr(currentLine_,"wall") != NULL) {
        readWSSFromFile();
      } else if (strstr(currentLine_,"stress") != NULL) {
        readStressFromFile();
      } else if (strstr(currentLine_,"traction") != NULL) {
        readTractionFromFile();
      } else if (strstr(currentLine_,"displacement") != NULL) {
        readDisplacementFromFile();
      } else {
        fprintf(stderr,"WARNING: ignoring results (%s).\n",currentLine_);
      }

    }

    return SV_ERROR;

}

int cvConvertVisFiles::readPressureFromFile() {

    int numNodes = grid_->GetNumberOfPoints();

    fprintf(stdout,"Reading %i nodal pressures.\n",numNodes);

    // skip until we find the string
    if (findStringInFile("number of data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find number of pressure data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }
    if (findStringInFile("data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find pressure data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }

    vtkFloatingPointArrayType* scalars = vtkFloatingPointArrayType::New();
    scalars->SetNumberOfComponents(1);
    scalars->Allocate(numNodes,10000);
    scalars->Initialize();

    double d = 0;
    vtkFloatingPointType f = 0;
    int nodeid = 1;

    while (0 == 0) {
      int flag = readNextLineFromFile(resfp_);
      if (flag == NEXTLINE_EOF) {
          closeInputFile(resfp_);
          scalars->Delete();
          return SV_ERROR;
      }
      if (strstr(currentLine_,"end data") != NULL) {
          break;
      }
      if (sscanf(currentLine_,"%lf",&d) != 1) {
          fprintf(stderr,"ERROR: invalid line (%s).\n",currentLine_);
          closeInputFile(resfp_);
          scalars->Delete();
          return SV_ERROR;
      }

      // convert to vtkFloatingPointTypes for vtk
      f = d;

      if (nodeid < 1 || nodeid > numNodes) {
          fprintf(stderr,"ERROR:  node id (%i) out of allowable range [1,%i].",nodeid,numNodes);
          closeInputFile(resfp_);
          scalars->Delete();
          return SV_ERROR;
      }

      scalars->InsertNextTuple1(f);
      nodeid++;

    }

    if (nodeid != (numNodes+1)) {
        fprintf(stderr,"ERROR:  not enough pressure data (%i != %i)\n",nodeid,numNodes);
        scalars->Delete();
        return SV_ERROR;
    }

    fprintf(stdout,"Done reading %i nodal pressures.\n",numNodes);

    havePressureResults_=1;
    pressure_ = scalars;
    return SV_OK;

}

int cvConvertVisFiles::readVelocityFromFile() {

    int numNodes = grid_->GetNumberOfPoints();

    fprintf(stdout,"Reading %i nodal velocities.\n",numNodes);

    // skip until we find the string
    if (findStringInFile("number of data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find number of velocity data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }
    if (findStringInFile("data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find velocity data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }

    vtkFloatingPointArrayType* vectors = vtkFloatingPointArrayType::New();
    vectors->SetNumberOfComponents(3);
    vectors->Allocate(numNodes,10000);
    vectors->Initialize();

    double d[3];
    vtkFloatingPointType f[3];
    d[0] = 0; d[1] = 0; d[2] = 0;
    f[0] = 0; f[1] = 0; f[2] = 0;
    int nodeid = 1;

    while (0 == 0) {
      int flag = readNextLineFromFile(resfp_);
      if (flag == NEXTLINE_EOF) {
          closeInputFile(resfp_);
          vectors->Delete();
          return SV_ERROR;
      }
      if (strstr(currentLine_,"end data") != NULL) {
          break;
      }
      if (sscanf(currentLine_,"%lf %lf %lf",&d[0],&d[1],&d[2]) != 3) {
          fprintf(stderr,"ERROR: invalid line (%s).\n",currentLine_);
          closeInputFile(resfp_);
          vectors->Delete();
          return SV_ERROR;
      }

      // convert to vtkFloatingPointTypes for vtk
      f[0] = d[0];f[1] = d[1];f[2] = d[2];

      if (nodeid < 1 || nodeid > numNodes) {
          fprintf(stderr,"ERROR:  node id (%i) out of allowable range [1,%i].",nodeid,numNodes);
          closeInputFile(resfp_);
          vectors->Delete();
          return SV_ERROR;
      }

      vectors->InsertNextTuple3(f[0],f[1],f[2]);
      nodeid++;

    }

    if (nodeid != (numNodes+1)) {
        fprintf(stderr,"ERROR:  not enough velocity data (%i != %i)\n",nodeid,numNodes);
        vectors->Delete();
        return SV_ERROR;
    }

    fprintf(stdout,"Done reading %i nodal velocities.\n",numNodes);

    haveVelocityResults_=1;
    velocity_ = vectors;
    return SV_OK;

}


int cvConvertVisFiles::readTractionFromFile() {

    gzFile nodesFile = NULL;
    int realnodeid = 0;

    int numNodes = grid_->GetNumberOfPoints();

    if (numTractionNodes_ == 0) {
      fprintf(stdout,"Reading %i nodal traction vectors.\n",numNodes);
    } else {
      fprintf(stdout,"Reading %i nodal traction vectors.\n",numTractionNodes_);
    }

    // skip until we find the string
    if (findStringInFile("number of data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find number of velocity data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }
    if (findStringInFile("data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find velocity data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }

    vtkFloatingPointArrayType* traction = vtkFloatingPointArrayType::New();
    traction->SetNumberOfComponents(3);
    traction->Allocate(numNodes,10000);
    traction->SetNumberOfTuples(numNodes);

    double d[3];
    vtkFloatingPointType f[3];
    d[0] = 0; d[1] = 0; d[2] = 0;
    f[0] = 0; f[1] = 0; f[2] = 0;
    int nodeid = 1;

    if (numTractionNodes_ > 0) {
		traction->FillComponent(0,0.0);
		traction->FillComponent(1,0.0);
		traction->FillComponent(2,0.0);
    }

    while (0 == 0) {
      int flag = readNextLineFromFile(resfp_);
      if (flag == NEXTLINE_EOF) {
          closeInputFile(resfp_);
          traction->Delete();
          return SV_ERROR;
      }
      if (strstr(currentLine_,"end data") != NULL) {
          break;
      }
      if (sscanf(currentLine_,"%lf %lf %lf",&d[0],&d[1],&d[2]) != 3) {
          fprintf(stderr,"ERROR: invalid line (%s).\n",currentLine_);
          closeInputFile(resfp_);
          traction->Delete();
          return SV_ERROR;
      }

      // convert to vtkFloatingPointTypes for vtk
      f[0] = d[0];f[1] = d[1];f[2] = d[2];

      int realnodeid = nodeid;

      if (numTractionNodes_) {
          realnodeid = tractionNodes_[nodeid-1];
          if (nodeid > numTractionNodes_) {
            fprintf(stderr,"ERROR:  node id (%i) out of traction nodes range (%i).",realnodeid,numTractionNodes_);
            closeInputFile(resfp_);
            traction->Delete();
            return SV_ERROR;
          }
      }

      if (realnodeid < 1 || realnodeid > numNodes) {
          fprintf(stderr,"ERROR:  node id (%i) out of allowable range [1,%i].",realnodeid,numNodes);
          closeInputFile(resfp_);
          traction->Delete();
          return SV_ERROR;
      }

      //fprintf(stdout,"set node %i\n",realnodeid);
      // remember that in vtk data structures node 1 is actually in slot 0
      traction->SetTuple3((vtkIdType)(realnodeid-1),f[0],f[1],f[2]);

      nodeid++;

    }

    if (nodeid != (numNodes+1) && (numTractionNodes_ == 0)) {
        fprintf(stderr,"ERROR:  not enough traction data (%i != %i)\n",nodeid,numNodes);
        traction->Delete();
        return SV_ERROR;
    } else if (nodeid != (numTractionNodes_ + 1) && (numTractionNodes_ > 0)) {
        fprintf(stderr,"ERROR:  not enough traction data (%i != %i)\n",nodeid,numTractionNodes_);
        traction->Delete();
        return SV_ERROR;
    }

    fprintf(stdout,"Done reading %i nodal traction vectors.\n",(nodeid-1));

    haveTractionResults_=1;
    traction_ = traction;
    return SV_OK;

}


int cvConvertVisFiles::readDisplacementFromFile() {

    gzFile nodesFile = NULL;
    int realnodeid = 0;

    int i;

    int numNodes = grid_->GetNumberOfPoints();

    if (numTractionNodes_ == 0) {
      fprintf(stdout,"Reading %i nodal displacement vectors.\n",numNodes);
    } else {
      fprintf(stdout,"Reading %i nodal displacement vectors.\n",numTractionNodes_);
    }

    // skip until we find the string
    if (findStringInFile("number of data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find number of velocity data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }
    if (findStringInFile("data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find velocity data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }

    vtkFloatingPointArrayType* displacement = vtkFloatingPointArrayType::New();
    displacement->SetNumberOfComponents(3);
    displacement->Allocate(numNodes,10000);
    displacement->SetNumberOfTuples(numNodes);

    double d[3];
    vtkFloatingPointType f[3];
    d[0] = 0; d[1] = 0; d[2] = 0;
    f[0] = 0; f[1] = 0; f[2] = 0;
    int nodeid = 1;

    int* keepme = NULL;
    if (numTractionNodes_ > 0) {
      displacement->FillComponent(0,0.0);
      displacement->FillComponent(1,0.0);
      displacement->FillComponent(2,0.0);
      keepme = new int[numNodes];
      for (i = 0; i < numNodes; i++) {
          keepme[i] = 0;
      }
      for (i = 0; i < numTractionNodes_; i++) {
          keepme[tractionNodes_[i] - 1] = 1;
      }

    }

    while (0 == 0) {
      int flag = readNextLineFromFile(resfp_);
      if (flag == NEXTLINE_EOF) {
          closeInputFile(resfp_);
          displacement->Delete();
          if (keepme != NULL) delete [] keepme;
          return SV_ERROR;
      }
      if (strstr(currentLine_,"end data") != NULL) {
          break;
      }
      if (sscanf(currentLine_,"%lf %lf %lf",&d[0],&d[1],&d[2]) != 3) {
          fprintf(stderr,"ERROR: invalid line (%s).\n",currentLine_);
          closeInputFile(resfp_);
          displacement->Delete();
          if (keepme != NULL) delete [] keepme;
          return SV_ERROR;
      }

      // convert to vtkFloatingPointTypes for vtk
      f[0] = d[0];f[1] = d[1];f[2] = d[2];

      int realnodeid = nodeid;

      if (realnodeid < 1 || realnodeid > numNodes) {
          fprintf(stderr,"ERROR:  node id (%i) out of allowable range [1,%i].",realnodeid,numNodes);
          closeInputFile(resfp_);
          displacement->Delete();
          if (keepme != NULL) delete [] keepme;
          return SV_ERROR;
      }

      //fprintf(stdout,"set node %i\n",realnodeid);
      // remember that in vtk data structures node 1 is actually in slot 0
      if (keepme == NULL) {
        displacement->SetTuple3((vtkIdType)(realnodeid-1),f[0],f[1],f[2]);
      } else {
          if (keepme[realnodeid-1] == 0) {
            displacement->SetTuple3((vtkIdType)(realnodeid-1),0,0,0);
          } else {
            displacement->SetTuple3((vtkIdType)(realnodeid-1),f[0],f[1],f[2]);
          }
      }
      nodeid++;

    }

    if (nodeid != (numNodes+1)) {
        fprintf(stderr,"ERROR:  not enough displacement data (%i != %i)\n",nodeid,numNodes);
        displacement->Delete();
        if (keepme != NULL) delete [] keepme;
        return SV_ERROR;
    }

    fprintf(stdout,"Done reading %i nodal displacement vectors.\n",(nodeid-1));

    if (keepme != NULL) delete [] keepme;

    haveDisplacementResults_=1;
    displacement_ = displacement;
    return SV_OK;

}


int cvConvertVisFiles::readWSSFromFile() {

    gzFile nodesFile = NULL;
    int realnodeid = 0;

    int i;

    int numNodes = grid_->GetNumberOfPoints();

    if (numTractionNodes_ == 0) {
      fprintf(stdout,"Reading %i nodal wss vectors.\n",numNodes);
    } else {
      fprintf(stdout,"Reading %i nodal wss vectors.\n",numTractionNodes_);
    }

    // skip until we find the string
    if (findStringInFile("number of data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find number of velocity data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }
    if (findStringInFile("data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find velocity data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }

    vtkFloatingPointArrayType* wss = vtkFloatingPointArrayType::New();
    wss->SetNumberOfComponents(3);
    wss->Allocate(numNodes,10000);
    wss->SetNumberOfTuples(numNodes);

    double d[3];
    vtkFloatingPointType f[3];
    d[0] = 0; d[1] = 0; d[2] = 0;
    f[0] = 0; f[1] = 0; f[2] = 0;
    int nodeid = 1;

    int* keepme = NULL;
    if (numTractionNodes_ > 0) {
      wss->FillComponent(0,0.0);
      wss->FillComponent(1,0.0);
      wss->FillComponent(2,0.0);
      keepme = new int[numNodes];
      for (i = 0; i < numNodes; i++) {
          keepme[i] = 0;
      }
      for (i = 0; i < numTractionNodes_; i++) {
          keepme[tractionNodes_[i] - 1] = 1;
      }

    }

    while (0 == 0) {
      int flag = readNextLineFromFile(resfp_);
      if (flag == NEXTLINE_EOF) {
          closeInputFile(resfp_);
          wss->Delete();
          if (keepme != NULL) delete [] keepme;
          return SV_ERROR;
      }
      if (strstr(currentLine_,"end data") != NULL) {
          break;
      }
      if (sscanf(currentLine_,"%lf %lf %lf",&d[0],&d[1],&d[2]) != 3) {
          fprintf(stderr,"ERROR: invalid line (%s).\n",currentLine_);
          closeInputFile(resfp_);
          wss->Delete();
          if (keepme != NULL) delete [] keepme;
          return SV_ERROR;
      }

      // convert to vtkFloatingPointTypes for vtk
      f[0] = d[0];f[1] = d[1];f[2] = d[2];

      int realnodeid = nodeid;

      if (realnodeid < 1 || realnodeid > numNodes) {
          fprintf(stderr,"ERROR:  node id (%i) out of allowable range [1,%i].",realnodeid,numNodes);
          closeInputFile(resfp_);
          wss->Delete();
          if (keepme != NULL) delete [] keepme;
          return SV_ERROR;
      }

      //fprintf(stdout,"set node %i\n",realnodeid);
      // remember that in vtk data structures node 1 is actually in slot 0
      if (keepme == NULL) {
        wss->SetTuple3((vtkIdType)(realnodeid-1),f[0],f[1],f[2]);
      } else {
          if (keepme[realnodeid-1] == 0) {
            wss->SetTuple3((vtkIdType)(realnodeid-1),0,0,0);
          } else {
            wss->SetTuple3((vtkIdType)(realnodeid-1),f[0],f[1],f[2]);
          }
      }
      nodeid++;

    }

    if (nodeid != (numNodes+1)) {
        fprintf(stderr,"ERROR:  not enough wss data (%i != %i)\n",nodeid,numNodes);
        wss->Delete();
        if (keepme != NULL) delete [] keepme;
        return SV_ERROR;
    }

    fprintf(stdout,"Done reading %i nodal wss vectors.\n",(nodeid-1));

    if (keepme != NULL) delete [] keepme;

    haveWSSResults_=1;
    wss_ = wss;
    return SV_OK;

}


int cvConvertVisFiles::readTransportFromFile() {

    int numNodes = grid_->GetNumberOfPoints();

    fprintf(stdout,"Reading %i nodal transport.\n",numNodes);

    // skip until we find the string
    if (findStringInFile("number of data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find number of transport data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }
    if (findStringInFile("data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find transport data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }

    vtkFloatingPointArrayType* scalars = vtkFloatingPointArrayType::New();
    scalars->SetNumberOfComponents(1);
    scalars->Allocate(numNodes,10000);
    scalars->Initialize();

    double d = 0;
    vtkFloatingPointType f = 0;
    int nodeid = 1;

    while (0 == 0) {
      int flag = readNextLineFromFile(resfp_);
      if (flag == NEXTLINE_EOF) {
          closeInputFile(resfp_);
          scalars->Delete();
          return SV_ERROR;
      }
      if (strstr(currentLine_,"end data") != NULL) {
          break;
      }
      if (sscanf(currentLine_,"%lf",&d) != 1) {
          fprintf(stderr,"ERROR: invalid line (%s).\n",currentLine_);
          closeInputFile(resfp_);
          scalars->Delete();
          return SV_ERROR;
      }

      // convert to vtkFloatingPointTypes for vtk
      f = d;

      if (nodeid < 1 || nodeid > numNodes) {
          fprintf(stderr,"ERROR:  node id (%i) out of allowable range [1,%i].",nodeid,numNodes);
          closeInputFile(resfp_);
          scalars->Delete();
          return SV_ERROR;
      }

      scalars->InsertNextTuple1(f);
      nodeid++;

    }

    if (nodeid != (numNodes+1)) {
        fprintf(stderr,"ERROR:  not enough transport data (%i != %i)\n",nodeid,numNodes);
        scalars->Delete();
        return SV_OK;
        return SV_ERROR;
    }

    fprintf(stdout,"Done reading %i nodal transport values.\n",numNodes);

    haveTransportResults_=1;
    transport_ = scalars;
    return SV_OK;

}


int cvConvertVisFiles::readStressFromFile() {

    int numNodes = grid_->GetNumberOfPoints();

    fprintf(stdout,"Reading %i nodal tensors.\n",numNodes);

    // skip until we find the string
    if (findStringInFile("number of data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find number of tensors data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }
    if (findStringInFile("data",resfp_) == SV_ERROR) {
        fprintf(stderr,"ERROR:  Could not find tensor data.");
        closeInputFile(resfp_);
        return SV_ERROR;
    }

    vtkFloatingPointArrayType* tensors = vtkFloatingPointArrayType::New();
    tensors->SetNumberOfComponents(9);
    tensors->Allocate(numNodes,10000);
    tensors->Initialize();

    double d[6];
    vtkFloatingPointType f[6];
    d[0] = 0; d[1] = 0; d[2] = 0;
    d[3] = 0; d[4] = 0; d[5] = 0;
    f[0] = 0; f[1] = 0; f[2] = 0;
    f[3] = 0; f[4] = 0; f[5] = 0;
    int nodeid = 1;

    while (0 == 0) {
      int flag = readNextLineFromFile(resfp_);
      if (flag == NEXTLINE_EOF) {
          closeInputFile(resfp_);
          tensors->Delete();
          return SV_ERROR;
      }
      if (strstr(currentLine_,"end data") != NULL) {
          break;
      }
      if (sscanf(currentLine_,"%lf %lf %lf %lf %lf %lf",
                 &d[0],&d[1],&d[2],&d[3],&d[4],&d[5]) != 6) {
          fprintf(stderr,"ERROR: invalid line (%s).\n",currentLine_);
          closeInputFile(resfp_);
          tensors->Delete();
          return SV_ERROR;
      }

      // convert to vtkFloatingPointTypes for vtk
      f[0] = d[0];f[1] = d[1];f[2] = d[2];
      f[3] = d[3];f[4] = d[4];f[5] = d[5];

      if (nodeid < 1 || nodeid > numNodes) {
          fprintf(stderr,"ERROR:  node id (%i) out of allowable range [1,%i].",nodeid,numNodes);
          closeInputFile(resfp_);
          tensors->Delete();
          return SV_ERROR;
      }

      // assume the following format of the stress results
      // components
      // "xx"   0
      // "yy"   1
      // "zz"   2
      // "xy"   3
      // "yz"   4
      // "xz"   5
      //
      //  | xx xy xz |       | 0 3 5 |
      //  | yx yy yz |  -->  | 3 1 4 |
      //  | xz yz zz |       | 5 4 2 |
      //
      // end components

      tensors->InsertNextTuple9(f[0],f[3],f[5],f[3],f[1],f[4],f[5],f[4],f[2]);
      nodeid++;

    }

    if (nodeid != (numNodes+1)) {
        fprintf(stderr,"ERROR:  not enough tensor data (%i != %i)\n",nodeid,numNodes);
        tensors->Delete();
        return SV_ERROR;
    }

    fprintf(stdout,"Done reading %i nodal tensors.\n",numNodes);

    haveStressResults_=1;
    stress_ = tensors;
    return SV_OK;

}


cvUnstructuredGrid* cvConvertVisFiles::GetGridObj() {
    meshExported_ = 1;
    cvUnstructuredGrid* reposobj = new cvUnstructuredGrid(grid_);
    return reposobj;
}

cvPolyData* cvConvertVisFiles::GetPressureObj() {
    vtkPolyData* pd = vtkPolyData::New();
    pd->SetPoints(grid_->GetPoints());
    pd->GetPointData()->SetScalars(pressure_);
    cvPolyData* reposobj = new cvPolyData(pd);
    pd->Delete();
    return reposobj;
}

cvPolyData* cvConvertVisFiles::GetVelocityObj() {
    vtkPolyData* pd = vtkPolyData::New();
    pd->SetPoints(grid_->GetPoints());
    pd->GetPointData()->SetVectors(velocity_);
    cvPolyData* reposobj = new cvPolyData(pd);
    pd->Delete();
    return reposobj;
}

cvPolyData* cvConvertVisFiles::GetResObj() {
    vtkPolyData* pd = vtkPolyData::New();
    pd->SetPoints(grid_->GetPoints());
    pd->GetPointData()->SetScalars(pressure_);
    pd->GetPointData()->SetVectors(velocity_);
    cvPolyData* reposobj = new cvPolyData(pd);
    pd->Delete();
    return reposobj;
}

cvPolyData* cvConvertVisFiles::GetTransportObj() {
    vtkPolyData* pd = vtkPolyData::New();
    pd->SetPoints(grid_->GetPoints());
    pd->GetPointData()->SetScalars(transport_);
    cvPolyData* reposobj = new cvPolyData(pd);
    pd->Delete();
    return reposobj;
}

cvPolyData* cvConvertVisFiles::GetStressObj() {
    vtkPolyData* pd = vtkPolyData::New();
    pd->SetPoints(grid_->GetPoints());
    pd->GetPointData()->SetTensors(stress_);
    cvPolyData* reposobj = new cvPolyData(pd);
    pd->Delete();
    return reposobj;
}

cvPolyData* cvConvertVisFiles::GetTractionObj() {
    vtkPolyData* pd = vtkPolyData::New();
    pd->SetPoints(grid_->GetPoints());
    pd->GetPointData()->SetVectors(traction_);
    cvPolyData* reposobj = new cvPolyData(pd);
    pd->Delete();
    return reposobj;
}

cvPolyData* cvConvertVisFiles::GetDisplacementObj() {
    vtkPolyData* pd = vtkPolyData::New();
    pd->SetPoints(grid_->GetPoints());
    pd->GetPointData()->SetVectors(displacement_);
    cvPolyData* reposobj = new cvPolyData(pd);
    pd->Delete();
    return reposobj;
}

cvPolyData* cvConvertVisFiles::GetWSSObj() {
    vtkPolyData* pd = vtkPolyData::New();
    pd->SetPoints(grid_->GetPoints());
    pd->GetPointData()->SetVectors(wss_);
    cvPolyData* reposobj = new cvPolyData(pd);
    pd->Delete();
    return reposobj;
}

void cvConvertVisFiles::SetTractionNodes(int numnodes, int* tractionNodes) {
    numTractionNodes_ = numnodes;
    tractionNodes_ = tractionNodes;
}
