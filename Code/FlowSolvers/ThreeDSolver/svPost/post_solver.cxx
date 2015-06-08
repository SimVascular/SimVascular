/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2013 Open Source Medical
 * Software Corporation
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
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <memory.h>
#include <float.h>

#include <iostream>
#include <string>
#include <vector>

#ifndef WIN32
#include <unistd.h>
#include <strings.h>
#endif
#ifdef WIN32
#include <string.h>
#endif

#include "cvFlowsolverOptions.h"

#include "SimVascular.h"
#include "cvSolverIO.h"

#include "vtkPoints.h"
#include "vtkUnstructuredGrid.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkIdList.h"
#include "vtkCellType.h"
#include "vtkXMLUnstructuredGridReader.h"
#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkUnstructuredGridWriter.h"
#include "vtkPointData.h"
#include "vtkGeometryFilter.h"
#include "vtkCleanPolyData.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkPolyDataWriter.h"
#include "vtkCellData.h"
#include "vtkXMLPolyDataReader.h"
#include "vtkCellArray.h"

#ifdef USE_ZLIB
#include "simvascular_zlib.h"
#else
#include <stdlib.h>
#define gzopen fopen
#define gzprintf fprintf
#define gzFile FILE*
#define gzclose fclose
#endif

#ifdef WIN32

void  bzero(void* ptr, size_t sz) {
    int i;
    char *cptr;
    cptr = (char*) ptr;
    for (i=0; i < sz; i++) {
        cptr[i]=0;
    }
    return;
}

#endif

using namespace std;

/*
   nv is the number of degrees of freedom per node
   numel is the total number of elements
   ien_ is the TRUE global connectivity
       (index local element node number and element
        number to TRUE global node number)
   numnp is the total number of nodes
   x is the coordinates (3*numnp)
   q is the solution (nv*numnp)
   nen is the number of nodes in an element
 */

class PostSolver {

public:

    PostSolver();
    ~PostSolver();

    int SetInputDirectory(const char* indir);
    int QueryNumberOfSolutionVariables(int stepNumber);
    int QueryNumberOfProcessors();
    int QueryFluidProperties();
    int QueryWallProperties();
    int ReadConnectivity();

    int GetNumberOfNodalCoordinates() {return nshgtot_;};
    int GetNumberOfElements() {return neltot_;};
    int GetNumberOfSolutionVariables() {return numvar_;};
    int GetNumberOfProcessorsUsed() {return numprocs_;};

    double* GetCoordinatePtr() {return xglobal_;};
    int* GetConnectivityPtr() {return ien_;};
    double* GetFluidProperties() {return matprops_;};
    double* GetWallProperties() {return wpglobal_;};

    int ExportFlowSolverFileFormat(int lstep, int newstepnumber, bool RequestedNewSn,
            bool RequestedSolution, bool RequestedTimeDeriv, bool RequestedDisplacements, bool RequestedYbar,
            double* qglobal,double* aglobal,double* dglobal, double* yglobal,int numy, const char* outdir);

    int ExportYBar(int stepnumber, int numy, double* yglobal, const char* outdir);


    int ParseRestartFile( int stepNumber, const char* field , int *numval, double **myglobal);

    int ExistRestartFile (int stepNumber);

    int SetInputFileType(const char* iotype);

    // CTEST: Compare Residual Files
    int CompareResidualFiles(const char* newResFile,const char* refResFile, double threshold);
    // CTEST: Compare VTU Files
    int CompareVTUFiles(const char* newResFile,const char* refResFile, double threshold);


private:

    int ione_,itwo_,ithree_,iseven_;

    double *xlocal_;
    double *xglobal_;
    int *ien_;
    int **ncorpd2d_;
    int **ecorpd2d_;
    double *wplocal_;
    double *wpglobal_;
    double *matprops_;

    char iotype_[80];
    char indir_[255];

    int numvar_;
    int numprocs_;
    int nshgtot_;
    int neltot_;
    int maxnshg_;
    int nsd_;

};

PostSolver::PostSolver() {

    ione_   = 1;
    itwo_   = 2;
    ithree_ = 3;
    iseven_ = 7;

    xlocal_  = NULL;
    xglobal_ = NULL;
    ien_     = NULL;
    wplocal_ = NULL;
    wpglobal_= NULL;
    matprops_=NULL;

    iotype_[0]='\0';
    sprintf(iotype_,"%s","binary");

    // default directory is current directory
    indir_[0]='.';
    indir_[1]='/';
    indir_[2]='\0';

    numvar_    = 0;
    numprocs_  = 0;
    nshgtot_   = 0;
    neltot_    = 0;
    maxnshg_   = 0;
}

PostSolver::~PostSolver() {

    if (xlocal_ != NULL) {
        delete [] xlocal_;
    }
    if (ien_ != NULL) {
        delete [] ien_;
    }

    if (ncorpd2d_ != NULL) {
        for(int i=0; i< numprocs_; i++) {
            delete [] ncorpd2d_[i];
        }
        delete [] ncorpd2d_;
    }
    if (ecorpd2d_ != NULL) {
        for(int i=0; i< numprocs_; i++) {
            delete [] ecorpd2d_[i];
        }
        delete [] ecorpd2d_;
    }

    if (wplocal_ != NULL) {
        delete [] wplocal_;
    }

}

int PostSolver::SetInputFileType(const char* iotype) {
    iotype_[0]='\0';
    sprintf(iotype_,"%s",iotype);
    return CV_OK;
}

int PostSolver::SetInputDirectory(const char* indir) {

    indir_[0]='\0';
    sprintf(indir_,"%s",indir);
    return CV_OK;

}

int PostSolver::QueryNumberOfSolutionVariables(int stepNumber) {

    char filename[255];
    filename[0]='\0';

    int irstin;
    int intfromfile[50];

    /* scanning restart.<stepnum>.1 for numvar_ */
    sprintf(filename,"%srestart.%d.1",indir_,stepNumber);
    cout << "Opening " << filename << " to scan for number of variables..."<<endl;
    if (openfile_(filename, "read", &irstin   ) !=  CVSOLVER_IO_OK) {
        return CV_ERROR;
    }
    readheader_(&irstin,"solution",(void*)intfromfile,&ithree_,"double",iotype_);
    numvar_=intfromfile[1];  /* pushing the second int into numvar_ */
    closefile_( &irstin, "read" );

    return CV_OK;

}


int PostSolver::QueryNumberOfProcessors() {

    int iarray[10];

    char filename[255];
    filename[0]='\0';

    int igeom = 0;

    sprintf(filename,"%sgeombc.dat.%d",indir_,1); /* geometry and bc database */
    cout << "Opening " << filename << " to scan for number of processors and global modes..."<<endl;
    if (openfile_(filename, "read", &igeom) !=  CVSOLVER_IO_OK) {
        return CV_ERROR;
    }
    readheader_(&igeom,"number of processors",(void*)iarray,&ione_,"integer",iotype_);
    numprocs_ = iarray[0];
    readheader_(&igeom,"number of global modes",(void*)iarray,&ione_,"integer",iotype_);
    nshgtot_ = iarray[0];
    closefile_(&igeom, "read");

    return CV_OK;

}

int PostSolver::QueryFluidProperties(){
    int iarray[10];

    char filename[255];
    filename[0]='\0';

    int igeom = 0;

    sprintf(filename,"%sgeombc.dat.%d",indir_,1); /* geometry and bc database */
    cout << "Opening " << filename << " to scan for material properties..."<<endl;
    if (openfile_(filename, "read", &igeom) !=  CVSOLVER_IO_OK) {
        return CV_ERROR;
    }
    iarray[0]=0;
    readheader_(&igeom,"material properties",(void*)iarray,&ione_,"double",iotype_);
    int numprops= iarray[0];
    if(numprops<=0){
        cout << "Warning: No material properties found in geombc"<<endl;
        return CV_ERROR;
    }else if(numprops==1){
        cout << "Warning: No enough material properties found in geombc"<<endl;
        return CV_ERROR;
    }else{
        matprops_=new double[numprops]();
        readdatablock_(&igeom,"material properties",(void*)matprops_,&numprops,"double", iotype_);
    }

    return CV_OK;
}

int PostSolver::ReadConnectivity() {

    int i,j,k,l;
    int igeom = 0;
    int numel, nen, nshgl;
    int np;

    int nshlmin,nelsofar,nelblk,nentp,nenl,nshl;
    int ixsiz, nsd, itmpentsiz, iqsiz;
    int gnum, opnum, nendx, junique;
    int neltp, ipordl;

    int *tmpent = NULL;

    int iarray[10];
    int intfromfile[50];

    char filename[255];
    filename[0]='\0';

    neltot_ = 0;
    maxnshg_ = 0;

    /* scanning geom.dat.<procnum> to add up neltot_ and to determine maxnshg_ */
    for(i=0; i< numprocs_; i++){
        filename[0]='\0';
        sprintf(filename,"%sgeombc.dat.%d",indir_,i+1); /* geometry and bc database */
        if (openfile_(filename, "read", &igeom) !=  CVSOLVER_IO_OK) {
            return CV_ERROR;
        }
        readheader_(&igeom,"number of interior elements",(void*)iarray,&ione_,"integer",iotype_);
        numel=iarray[0];
        readheader_(&igeom,"maximum number of element nodes",(void*)iarray,&ione_,"integer",iotype_);
        nen=iarray[0];
        readheader_(&igeom,"number of modes",(void*)iarray,&ione_,"integer",iotype_);
        nshgl=iarray[0];
        if(nshgl>maxnshg_) maxnshg_=nshgl;
        closefile_(&igeom, "read" );
        neltot_+=numel;
    }

    // now that we have our size of array information we can allocate
    // our  local and global geometry arrays (here local means on a
    // given processor and global means the total array assembled
    // across all processors)

    xglobal_ = new double [ 3*nshgtot_ ];
    xlocal_ = new double [ 3*maxnshg_ ];

    ncorpd2d_ = new int* [numprocs_];
    ecorpd2d_ = new int* [numprocs_];
    bool realGlobalElementID=true;

    nendx = nen;
    if(nen>4) nendx=8;  /* DX thinks of things as ALL tets or hexes ONLY */
    ien_ = new int [ nendx*neltot_ ];
    nshlmin=nendx;
    nelsofar=0;

    // Next we loop over the processors and read each processors
    // geometry database.  Using the ncorpd2d_ array, we reconstruct the
    // global geometry structures (coordinates and connectivity)

    for(i=0; i< numprocs_; i++){

        /* open geom file  and read header*/
        sprintf(filename,"%sgeombc.dat.%d",indir_,i+1);
        cout << "Reducing : " << filename << endl;
        if (openfile_( filename, "read", &igeom ) !=  CVSOLVER_IO_OK) {
            return CV_ERROR;
        }
        readheader_(&igeom,"number of nodes",(void*)iarray,&ione_,"integer", iotype_);
        np=iarray[0];
        readheader_(&igeom,"number of modes",(void*)iarray,&ione_,"integer", iotype_);
        nshgl=iarray[0];
        readheader_(&igeom,"number of interior tpblocks",(void*)iarray,&ione_,"integer",iotype_);
        nelblk=iarray[0];
        readheader_(&igeom,"number of interior elements",(void*)iarray,&ione_,"integer",iotype_);
        numel=iarray[0];

        /* read coordinates and fill into global array */
        readheader_(&igeom,"co-ordinates",(void*)intfromfile,&itwo_,"double",iotype_);
        np=intfromfile[0];
        nsd=intfromfile[1];
        // Read the number of dimensions
        nsd_=nsd;
        ixsiz=np*nsd;
        readdatablock_(&igeom,"co-ordinates",(void*)xlocal_,&ixsiz, "double",iotype_);
        /* get the map from partion numbering to global numbering */
        if ( numprocs_ > 1 ) {
            readheader_(&igeom,"mode number map from partition to global",(void*)intfromfile,&ione_,"integer",iotype_);
            np=intfromfile[0];
            ncorpd2d_[i] = new int[np];
            readdatablock_(&igeom,"mode number map from partition to global",(void*)ncorpd2d_[i],&np,"integer",iotype_);

            intfromfile[0]=0;
            readheader_(&igeom,"element number map from partition to global",(void*)intfromfile,&ione_,"integer",iotype_);
            numel=intfromfile[0];
            if(numel>0){
                ecorpd2d_[i] = new int[numel];
                readdatablock_(&igeom,"element number map from partition to global",(void*)ecorpd2d_[i],&numel,"integer",iotype_);
            }else{
                realGlobalElementID=false;
            }

        } else {
            ncorpd2d_[i] = new int [nshgl];
            for(j=0; j< nshgl ; j++)
                ncorpd2d_[i][j]=j+1;
            ecorpd2d_[i] = new int [numel];
            for(j=0; j< numel ; j++)
                ecorpd2d_[i][j]=j+1;
        }

        /* map it to global numbering */
        for(k=0; k< 3; k++)
            for(j=0; j< nshgl ; j++)
                xglobal_[k*nshgtot_+ncorpd2d_[i][j]-1] = xlocal_[k*nshgl+j];

        /*read connectivity data */
        for(k=0; k< nelblk; k++){//nelblk is always 1
            /* keyphrase identifying interior connectivity element block */
            readheader_(&igeom,"connectivity interior",(void*)intfromfile,&iseven_,"integer",iotype_);
            neltp  =intfromfile[0];
            nenl   =intfromfile[1];
            ipordl =intfromfile[2];
            nshl   =intfromfile[3];

            if(nshl < nshlmin) nshlmin=nshl;
            /* allocate the array to the right size */
            tmpent = new int [ nshl*neltp ];
            itmpentsiz=neltp*nshl;
            /* now read the array */
            readdatablock_(&igeom,"connectivity interior",(void*)tmpent,&itmpentsiz,"integer", iotype_);

            if(realGlobalElementID){
                for(l=0; l< neltp; l++){
                    for(j=0; j<nshl ; j++){
                        gnum=j*neltot_+ecorpd2d_[i][l]-1;
                        opnum=tmpent[j*neltp+l];
                        ien_[gnum]=ncorpd2d_[i][opnum-1];
                    }
                }
            }else{
                /* Now we need to bring ien_ to the global numbering */
                for(l=0; l< neltp; l++){
                    for(j=0; j<nshl ; j++){
                        gnum=nelsofar+j*neltot_+l;
                        opnum=tmpent[j*neltp+l];
                        ien_[gnum]=ncorpd2d_[i][opnum-1];
                    }

                    /* pad to largest nendx since dx has to treat pyr and wedg as
                       degenerate hex */
                    opnum=ien_[gnum];  /* hijack opnum to keep last real node */
                    for(j=nshl; j<nendx ; j++){
                        gnum=nelsofar+j*neltot_+l;
                        ien_[gnum]=opnum;
                    }
                }
                nelsofar+=neltp;

            }

            delete [] tmpent;
        }
        closefile_( &igeom, "read" );
    }

    return CV_OK;

}

int PostSolver::QueryWallProperties(){
    int i,j,k;
    int iarray[10];
    int igeom = 0;

    char filename[255];
    filename[0]='\0';

    wpglobal_ = new double [ 2*nshgtot_ ];
    wplocal_ = new double [ 2*maxnshg_ ];

    for(i=0; i< numprocs_; i++){
        sprintf(filename,"%sgeombc.dat.%d",indir_,i+1);
        cout << "Reducing : wall properties from " << filename << endl;
        if (openfile_( filename, "read", &igeom ) !=  CVSOLVER_IO_OK) {
            delete [] wpglobal_;
            delete [] wplocal_;
            wpglobal_=NULL;
            wplocal_=NULL;
            return CV_ERROR;
        }

        /* read variable wall properties  */
        iarray[0]=0;
        readheader_(&igeom,"varwallprop",(void*)iarray,&itwo_,"double",iotype_);
        if(iarray[0]>0){
            int numNode=iarray[0];
            //int numComp=iarray[1];
            int isize=2*numNode;

            readdatablock_(&igeom,"varwallprop",(void*)wplocal_,&isize, "double",iotype_);

            for(k=0; k< 2; k++){
                for(j=0; j< numNode ; j++){
                    wpglobal_[k*nshgtot_+ncorpd2d_[i][j]-1] = wplocal_[k*numNode+j];
                }
            }
            closefile_( &igeom, "read" );
        }else{
            cout << "No wall properties found from " << filename<<". Will search from restart files." << endl;
            delete [] wpglobal_;
            delete [] wplocal_;
            wpglobal_=NULL;
            wplocal_=NULL;
            closefile_( &igeom, "read" );
            break;
        }

    }

    return CV_OK;
}

vtkUnstructuredGrid* createGrid(int nshgtot,double* xglobal,int neltot, int* ien){
    int i;
    vtkPoints* pts = NULL;
    vtkUnstructuredGrid* grid = NULL;

    grid = vtkUnstructuredGrid::New();
    grid->Allocate(neltot,1000);

    // nodes

    cout << "Create vtkPoints with " << nshgtot << " nodes." << endl;

    pts = vtkPoints::New();
    pts->Allocate(nshgtot,1000);
    pts->SetNumberOfPoints(nshgtot);

    vtkIntArray* gid = vtkIntArray::New();
    gid->SetNumberOfComponents(1);
    gid->Allocate(nshgtot,1000);
    gid->SetNumberOfTuples(nshgtot);
    gid->SetName("GlobalNodeID");

    for( i=0; i< nshgtot; i++ ) {
        pts->SetPoint(i,xglobal[0*nshgtot+i],xglobal[1*nshgtot+i],xglobal[2*nshgtot+i]);
        // global ids start at 1
        gid->SetTuple1(i,i+1);
    }

    grid->SetPoints(pts);
    grid->GetPointData()->AddArray(gid);

    pts->Delete();
    gid->Delete();

    // elements

    cout << "Create tets for " << neltot << " elements." << endl;

    vtkIdList* ptids = vtkIdList::New();
    ptids->Allocate(10,10);
    ptids->Initialize();
    ptids->SetNumberOfIds(4);

    vtkIntArray* eid = vtkIntArray::New();
    eid->SetNumberOfComponents(1);
    eid->Allocate(neltot,1000);
    eid->SetNumberOfTuples(neltot);
    eid->SetName("GlobalElementID");


    for(i=0; i< neltot; i++){
        //for(j=0; j < nendx; j++) gzprintf(frest,"%d  ",ien[j*neltot+i]);
        ptids->SetId(0,ien[0*neltot+i]-1);
        ptids->SetId(1,ien[1*neltot+i]-1);
        ptids->SetId(2,ien[2*neltot+i]-1);
        ptids->SetId(3,ien[3*neltot+i]-1);
        grid->InsertNextCell(VTK_TETRA,ptids);
        eid->SetTuple1(i,i+1);
    }

    ptids->Delete();

    grid->GetCellData()->SetScalars(eid);
    grid->GetCellData()->SetActiveScalars("GlobalElementID");
    grid->GetCellData()->CopyAllOn();

    eid->Delete();

    return grid;
}

vtkDoubleArray* cloneVtkDoubleArray(int nshgtot, int numComponent, vtkDoubleArray* source){

    vtkDoubleArray *target = vtkDoubleArray::New();
    target->SetNumberOfComponents(numComponent);
    target->Allocate(nshgtot,10000);
    target->SetNumberOfTuples(nshgtot);

    for (int i=0; i< nshgtot; i++) {
        //target->SetTuple4(i,aglobal[0*nshgtot+i],aglobal[1*nshgtot+i],aglobal[2*nshgtot+i],aglobal[3*nshgtot+i]);
        target->SetTuple(i,i,source);
    }

    return target;
}

int SetWallFlag(int nshgtot,const char* wallfn, int **wallflag){

    // read file
    vtkPolyData* pd = NULL;
    vtkXMLPolyDataReader* reader = vtkXMLPolyDataReader::New();
    reader->SetFileName(wallfn);
    reader->Update();
    pd = reader->GetOutput();
    if (pd == NULL) {
        cout << "ERROR in reading wall file: "<<wallfn<<"!" << endl;
        return CV_ERROR;
    }
    vtkIntArray* gids = NULL;
    gids =static_cast<vtkIntArray*>(reader->GetOutput()->GetPointData()->GetArray("GlobalNodeID"));
    if (gids == NULL) {
        cout << "ERROR: problem finding GlobalNodeID in wall file: "<<wallfn<<"!" << endl;
        return CV_ERROR;
    }

    int* tempwallflag=new int[nshgtot]();

    for (int i = 0; i < gids->GetNumberOfTuples(); i++) {
        int nodeId = gids->GetTuple1(i);
        tempwallflag[nodeId - 1] = 1;
    }

    // cleanup
    reader->Delete();

    *wallflag =  tempwallflag;

    return CV_OK;

}

int WallFiltering(double* vglobal, int nshgtot, int numComp,int* wallflag){
    for(int k=0; k< numComp; k++){
        for(int j=0; j< nshgtot ; j++){
            if(wallflag[j]==0){
                vglobal[k*nshgtot+j] = 0.0;

            }
        }
    }

    return CV_OK;

}

int GetWallElements(int neltot,int* ien, const char* wallfn, int* wallElementNum, int** wallElements){

    vtkPolyData* pd = NULL;
    vtkXMLPolyDataReader* reader = vtkXMLPolyDataReader::New();
    reader->SetFileName(wallfn);
    reader->Update();
    pd = reader->GetOutput();

    pd->GetPointData()->SetActiveScalars("GlobalNodeID");
    pd->GetCellData()->SetActiveScalars("GlobalElementID");

    vtkIdList* ptids = vtkIdList::New();
    ptids->Allocate(10, 10);
    ptids->Initialize();

    vtkCellArray *cells = pd->GetPolys();
    cells->InitTraversal();

    vtkIntArray* gids = NULL;
    gids = static_cast<vtkIntArray*>(pd->GetPointData()->GetArray("GlobalNodeID"));
    if (gids == NULL) {
        cout << "ERROR: problem finding GlobalNodeID in " << wallfn << endl;
        return CV_ERROR;
    }

    int n0, n1, n2, n3;
    int elementId, cellNum;
    cellNum=cells->GetNumberOfCells();
    int* we=new int[cellNum*4]();

    for (int icell = 0; icell < cellNum; icell++) {

        ptids->Reset();
        cells->GetCell(4 * icell, ptids);
        if (ptids->GetNumberOfIds() != 3) {
            cout << "ERROR:  invalid number of ids in cell ("<<ptids->GetNumberOfIds()<<")!" << endl;
            return CV_ERROR;
        }
        elementId = pd->GetCellData()->GetScalars()->GetTuple1(icell);
        n0 = gids->GetTuple1(ptids->GetId(0));
        n1 = gids->GetTuple1(ptids->GetId(1));
        n2 = gids->GetTuple1(ptids->GetId(2));
        n3 = -1;

        for (int i = 0; i < 4; i++) {
            if (ien[i * neltot + (elementId - 1)] != n0
                    && ien[i * neltot + (elementId - 1)] != n1
                    && ien[i * neltot + (elementId - 1)] != n2){
                n3 = ien[i * neltot + (elementId - 1)];
                break;
            }
        }

        if (n3 < 0) {
            cout << "ERROR:  could not find nodes in element ("<<elementId<<" "<<n0<<" "<<n1<<" "<<n2<<") in "<< wallfn << endl;
            return CV_ERROR;
        }

        we[0*cellNum+icell]=n0;
        we[1*cellNum+icell]=n1;
        we[2*cellNum+icell]=n2;
        we[3*cellNum+icell]=n3;

    }

    *wallElementNum=cellNum;
    *wallElements=we;

    ptids->Delete();
    reader->Delete();

    return CV_OK;
}

int CalculateWallStress(int nshgtot, int wallElementNum, int* wallElements, bool applyWD, double* xglobal,double* dglobal,double* qglobal,double mu,
        double** tglobal, double** wglobal){
    int nsd=3;
    double* sA=new double[nshgtot]();
    double* sTF=new double[nshgtot*nsd]();
    double* sSF=new double[nshgtot*nsd]();
    double* wallTraction=new double[nshgtot*nsd]();
    double* wallShearStress=new double[nshgtot*nsd]();

    double     beArea, eJac,normM, temp, coef;
    double     beNorm[3], eNaNbx[4][3];
    double     V[3][2], normV[3];
    double     SJ, Xa, Xb, Xc, Ya, Yb, Yc, Za, Zb, Zc;
    double     Tdn[3], ndTdn, taue[3];
    int nid[4];

    int n1,n2,i,j;

    double* x=new double[nshgtot*nsd]();
    if(dglobal==NULL || !applyWD){
        for (int n1=0; n1< nshgtot; n1++) {
            for(int i=0;i<3;i++){
                x[i*nshgtot+n1]=xglobal[i*nshgtot+n1];
            }
        }
    }else{
        for (int n1=0; n1< nshgtot; n1++) {
            for(int i=0;i<3;i++){
                x[i*nshgtot+n1]=xglobal[i*nshgtot+n1]+dglobal[i*nshgtot+n1];
            }
        }
    }


    //	double** x = new double*[3];
    //	x[0] = new double[nshg];
    //	x[1] = new double[nshg];
    //	x[2] = new double[nshg];

    coef=24.0;

    for(int n1=0;n1<wallElementNum;n1++){
        nid[0]=wallElements[0*wallElementNum+n1]-1;//used as index in C array
        nid[1]=wallElements[1*wallElementNum+n1]-1;
        nid[2]=wallElements[2*wallElementNum+n1]-1;
        nid[3]=wallElements[3*wallElementNum+n1]-1;

        //for beNorm
        for(j=0;j<nsd-1;j++){
            for(i=0;i<nsd;i++){
                V[i][j] = x[i*nshgtot+nid[j+1]]-x[i*nshgtot+nid[0]];
            }
        }

        normV[0] = V[1][0]*V[2][1] - V[2][0]*V[1][1];
        normV[1] = V[2][0]*V[0][1] - V[0][0]*V[2][1];
        normV[2] = V[0][0]*V[1][1] - V[1][0]*V[0][1];

        normM = 0.0;
        for(i=0;i<nsd;i++){
            normM = normM + normV[i]*normV[i];
        }
        normM = sqrt(normM);
        beNorm[0] = normV[0]/normM;
        beNorm[1] = normV[1]/normM;
        beNorm[2] = normV[2]/normM;

        temp = 0.0;
        for(i=0;i<nsd;i++){
            temp = temp + beNorm[i]*(x[i*nshgtot+nid[3]]-x[i*nshgtot+nid[0]]);
        }
        if(temp>0.0){
            beNorm[0] = -beNorm[0];
            beNorm[1] = -beNorm[1];
            beNorm[2] = -beNorm[2];
        }

        //for beArea
        Xa = x[0*nshgtot+nid[0]] - x[0*nshgtot+nid[2]];
        Xb = x[0*nshgtot+nid[1]] - x[0*nshgtot+nid[2]];

        Ya = x[1*nshgtot+nid[0]] - x[1*nshgtot+nid[2]];
        Yb = x[1*nshgtot+nid[1]] - x[1*nshgtot+nid[2]];

        Za = x[2*nshgtot+nid[0]] - x[2*nshgtot+nid[2]];
        Zb = x[2*nshgtot+nid[1]] - x[2*nshgtot+nid[2]];

        beArea = sqrt((Xa*Yb-Xb*Ya)*(Xa*Yb-Xb*Ya)+(Ya*Zb-Yb*Za)*(Ya*Zb-Yb*Za)+(Za*Xb-Zb*Xa)*(Za*Xb-Zb*Xa));

        Xa = x[0*nshgtot+nid[0]] - x[0*nshgtot+nid[3]];
        Xb = x[0*nshgtot+nid[1]] - x[0*nshgtot+nid[3]];
        Xc = x[0*nshgtot+nid[2]] - x[0*nshgtot+nid[3]];

        Ya = x[1*nshgtot+nid[0]] - x[1*nshgtot+nid[3]];
        Yb = x[1*nshgtot+nid[1]] - x[1*nshgtot+nid[3]];
        Yc = x[1*nshgtot+nid[2]] - x[1*nshgtot+nid[3]];

        Za = x[2*nshgtot+nid[0]] - x[2*nshgtot+nid[3]];
        Zb = x[2*nshgtot+nid[1]] - x[2*nshgtot+nid[3]];
        Zc = x[2*nshgtot+nid[2]] - x[2*nshgtot+nid[3]];

        eJac = Xa*Yb*Zc+Xb*Yc*Za+Xc*Ya*Zb-(Xc*Yb*Za + Xb*Ya*Zc + Xa*Yc*Zb);

        SJ = fabs(eJac)/eJac/24.0;
        eJac = fabs(eJac);

        eNaNbx[0][0] = SJ*(Yb*Zc - Yc*Zb);
        eNaNbx[1][0] = SJ*(Yc*Za - Ya*Zc);
        eNaNbx[2][0] = SJ*(Ya*Zb - Yb*Za);

        eNaNbx[0][1] = SJ*(Xc*Zb - Xb*Zc);
        eNaNbx[1][1] = SJ*(Xa*Zc - Xc*Za);
        eNaNbx[2][1] = SJ*(Xb*Za - Xa*Zb);

        eNaNbx[0][2] = SJ*(Xb*Yc - Xc*Yb);
        eNaNbx[1][2] = SJ*(Xc*Ya - Xa*Yc);
        eNaNbx[2][2] = SJ*(Xa*Yb - Xb*Ya);

        for(i=0;i<nsd;i++){
            eNaNbx[3][i] = 0.0;
            for(n2=0;n2<3;n2++){
                eNaNbx[3][i] = eNaNbx[3][i] - eNaNbx[n2][i];
            }
        }

        double ux[3][3] = {0.0};
        for(n2=0;n2<4;n2++){
            for(i=0;i<nsd;i++){
                for(j=0;j<nsd;j++){
                    ux[i][j] = ux[i][j]+ coef*qglobal[(i+1)*nshgtot+nid[n2]]*eNaNbx[n2][j]/eJac;

                }
            }
        }

        for(i=0;i<nsd;i++){
            Tdn[i] = 0.0;
            for(j=0;j<nsd;j++){
                Tdn[i] = Tdn[i] + mu*(ux[i][j] + ux[j][i])*beNorm[j];
            }
        }

        ndTdn = 0.0;
        for(i=0;i<nsd;i++){
            ndTdn = ndTdn + Tdn[i]*beNorm[i];
        }

        for(i=0;i<nsd;i++){
            taue[i] = Tdn[i] - ndTdn*beNorm[i];
        }

        for(n2=0;n2<3;n2++){
            sA[nid[n2]] = sA[nid[n2]] + beArea;
            for(i=0;i<nsd;i++){
                sTF[i*nshgtot+nid[n2]] = sTF[i*nshgtot+nid[n2]] - beArea*Tdn[i];
                sSF[i*nshgtot+nid[n2]] = sSF[i*nshgtot+nid[n2]] - beArea*taue[i];
            }
        }
    }

    for(int n=0;n<nshgtot;n++){
        if (sA[n]>0.0){
            for(i=0;i<nsd;i++){
                wallTraction[i*nshgtot+n] = sTF[i*nshgtot+n]/sA[n];
                wallShearStress[i*nshgtot+n]  = sSF[i*nshgtot+n] /sA[n];
            }
        }
    }

    *tglobal=wallTraction;
    *wglobal=wallShearStress;

    delete [] sA;
    delete [] sTF;
    delete [] sSF;
    delete [] x;

    return CV_OK;
}


// =======================================
// Export using old flowsolver file format
// =======================================

int PostSolver::ExportFlowSolverFileFormat(int lstep, int newstepnumber, bool RequestedNewSn,
        bool RequestedSolution, bool RequestedTimeDeriv, bool RequestedDisplacements, bool RequestedYbar,
        double* qglobal,double* aglobal,double* dglobal, double* yglobal,int numy, const char* outdir){

    // Write the total restart into restart.lstep.0  NOTE 0 is not
    // used in our partitioned system which goes from 1..nproc
    int i=-1;
    int irstin = 0;
    int iarray[10];
    int magic_number = 362436;
    int* mptr = &magic_number;
    char rfile[255];

    // allow user to reset the step number
    if (RequestedNewSn) {
        lstep = newstepnumber;
    }

    sprintf(rfile,"%srestart.%d.%d",outdir,lstep,i+1);
    if (openfile_(rfile, "write" , &irstin) !=  CVSOLVER_IO_OK) {
        return CV_ERROR;
    }

    writestring_( &irstin,"# SimVascular Input File Version 2.0\n");
    writestring_( &irstin, "# Byte Order Magic Number : 362436 \n");

    bzero( (void*)rfile, 255 );
    sprintf(rfile,"# Output generated by phPost version 2.25:  \n");
    writestring_( &irstin, rfile );

    int one = 1;
    int size = 1;
    int nitems = 1;
    iarray[ 0 ] = 1;
    writeheader_( &irstin, "byteorder magic number ",(void*)iarray, &nitems, &size, "integer", iotype_ );

    writedatablock_( &irstin, "byteorder magic number ",(void*)mptr, &nitems, "integer", iotype_ );

    bzero( (void*)rfile, 255 );
    sprintf(rfile,"number of modes : < 0 > %d\n", nshgtot_);
    writestring_( &irstin, rfile );

    bzero( (void*)rfile, 255 );
    sprintf(rfile,"number of variables : < 0 > %d\n", numvar_);
    writestring_( &irstin, rfile );

    if(RequestedSolution) {
        size =  numvar_*nshgtot_;
        nitems = 3;
        iarray[ 0 ] = nshgtot_;
        iarray[ 1 ] = numvar_;
        iarray[ 2 ] = lstep;

        writeheader_( &irstin, "solution ",
                ( void* )iarray, &nitems, &size,"double", iotype_ );

        nitems = numvar_*nshgtot_;
        writedatablock_( &irstin, "solution ",
                ( void* )(qglobal), &nitems, "double", iotype_ );
    }

    // finished writing the solution in restart

    // if the acceleration is requested, before closing the file write the acceleration
    if(RequestedTimeDeriv){
        size =  numvar_*nshgtot_;
        nitems = 3;
        iarray[ 0 ] = nshgtot_;
        iarray[ 1 ] = numvar_;
        iarray[ 2 ] = lstep;

        writeheader_( &irstin, "time derivative of solution ",
                ( void* )iarray, &nitems, &size,"double", iotype_ );

        nitems = numvar_*nshgtot_;
        writedatablock_( &irstin, "time derivative of solution ",
                ( void* )(aglobal), &nitems, "double", iotype_ );
    }

    // if the displacement is requested, write it to the file
    if(RequestedDisplacements) {
        nitems = 3;
        iarray[ 0 ] = nshgtot_;
        iarray[ 1 ] = nsd_;
        iarray[ 2 ] = lstep;
        size = nsd_*nshgtot_;
        writeheader_( &irstin, "displacement ",
                ( void* )iarray, &nitems, &size,"double", iotype_ );
        nitems = size;
        writedatablock_( &irstin, "displacement ",
                ( void* )(dglobal), &nitems, "double", iotype_ );
    }

    if(RequestedYbar) {
        nitems = 3;
        iarray[ 0 ] = nshgtot_;
        iarray[ 1 ] = numy;
        iarray[ 2 ] = lstep;
        size = numy*nshgtot_;
        writeheader_( &irstin, "ybar ",
                ( void* )iarray, &nitems, &size,"double", iotype_ );
        nitems = size;
        writedatablock_( &irstin, "ybar ",
                ( void* )(yglobal), &nitems, "double", iotype_ );
    }

    closefile_( &irstin, "write" );

    return CV_OK;

}

// =================
// Export YBar Field
// =================
int PostSolver::ExportYBar(int stepnumber, int numy, double* yglobal, const char* outdir){

    // Declare
    int irstin = 0;
    char rfile[255];
    int iarray[10];
    int magic_number = 362436;
    int* mptr = &magic_number;

    sprintf(rfile,"%s%s.%d.0",outdir,"ybar",stepnumber);
    if (openfile_(rfile, "write" , &irstin) !=  CVSOLVER_IO_OK) {
        return CV_ERROR;
    }

    writestring_( &irstin,"# SimVascular Input File Version 2.0\n");
    writestring_( &irstin, "# Byte Order Magic Number : 362436 \n");

    bzero( (void*)rfile, 255 );
    sprintf(rfile,"# Output generated by phPost version 2.7:  \n");
    writestring_( &irstin, rfile );

    int size = 1;
    int nitems = 1;
    iarray[0] = 1;
    writeheader_( &irstin, "byteorder magic number ",
            (void*)iarray, &nitems, &size, "integer", iotype_ );

    writedatablock_( &irstin, "byteorder magic number ",
            (void*)mptr, &nitems, "integer", iotype_ );

    bzero( (void*)rfile, 255 );
    sprintf(rfile,"number of modes : < 0 > %d\n", nshgtot_);
    writestring_( &irstin, rfile );

    bzero( (void*)rfile, 255 );
    sprintf(rfile,"number of variables : < 0 > %d\n", numy);
    writestring_( &irstin, rfile );

    size =  numy*nshgtot_;
    nitems = 3;
    iarray[0] = nshgtot_;
    iarray[1] = numy;
    iarray[2] = stepnumber;

    writeheader_( &irstin, "ybar",
            (void*)iarray, &nitems, &size,"double", iotype_ );

    nitems = numy*nshgtot_;
    writedatablock_( &irstin, "ybar",
            (void*)(yglobal), &nitems, "double", iotype_ );

    closefile_( &irstin, "write" );

    return CV_OK;

}

int PostSolver::ExistRestartFile( int stepNumber) {
    int i;
    char filename[255];
    filename[0]='\0';
    FILE* inputfile;

    for(i=0; i<numprocs_; i++){
        sprintf(filename,"%srestart.%d.%d",indir_,stepNumber, i+1);
        inputfile=fopen(filename,"r");
        if(inputfile==NULL){
            return CV_ERROR;
        }else{
            fclose(inputfile);
        }
    }

    return CV_OK;
}


int PostSolver::ParseRestartFile( int stepNumber, const char* field , int *numval, double **myglobal) {

    // We loop over the processors and read each processors
    // solution database.  Using the ncorpd2d_ array, we reconstruct
    // the global solution data

    int i,j,k;
    int nshgl, lstep, iqsiz;
    int numvar;

    int iarray[10];
    int intfromfile[50];

    char filename[255];
    filename[0]='\0';
    int irstin = 0;

    *myglobal = NULL;

    double *rtnglobal = new double [ numvar_*nshgtot_ ];
    double *qlocal   =  new double [ numvar_*maxnshg_ ];

    for (int nate = 0; nate < numvar_*maxnshg_; nate++) {
        qlocal[nate] = 0.0;
    }

    for(i=0; i<numprocs_; i++){

        // read in solution for current processor
        sprintf(filename,"%srestart.%d.%d",indir_,stepNumber, i+1);
        cout << "Reducing (" << field << ") results : " << filename << endl;
        if (openfile_(filename, "read", &irstin   ) !=  CVSOLVER_IO_OK) {
            return CV_ERROR;
        }

        intfromfile[0] = -1;
        readheader_(&irstin,field,(void*)intfromfile,&ithree_,"double",iotype_);

        nshgl=intfromfile[0];
        if ( nshgl == -1 ) {
            closefile_( &irstin, "read" );
            cout << "NOTE: No (" << field << ") in " << filename << endl;
            return CV_ERROR;
        }

        numvar = intfromfile[1];
        lstep=intfromfile[2];
        iqsiz=nshgl*numvar;

        readdatablock_(&irstin,field,(void*)qlocal, &iqsiz, "double", iotype_);
        closefile_( &irstin, "read" );

        cout << "Done reading (" << field << ") results : " << filename << endl;

        /* map solution to global */
        for(k=0; k< numvar; k++){
            for(j=0; j< nshgl ; j++){
                rtnglobal[k*nshgtot_+ncorpd2d_[i][j]-1] = qlocal[k*nshgl+j];
            }
        }

    }

    *numval = numvar;
    *myglobal =  rtnglobal;

    delete [] qlocal;

    return CV_OK;

}


int calcMeanWallShearStressAndPressure(int start, int stop, int incr, bool sim_units_cm, vtkPolyData *pd) {

    int i = 0;
    int j = 0;
    double shear[3];
    int count = 0;

    // count number of arrays to process
    int numArrays = 0;
    for (i = start; i <= stop; i += incr) {
        numArrays++;
    }

    fprintf(stdout,"numArrays: %i\n",numArrays);

    int numPts = pd->GetNumberOfPoints();
    fprintf(stdout,"numPts: %i\n",numPts);

    char pname[80];
    char vname[80];
    char tname[80];
    char dname[80];
    char wname[80];

    // create a list of the pressure vectors
    vtkDoubleArray **p   = new vtkDoubleArray*[numArrays];
    count = 0;
    for (i = start; i <= stop; i += incr) {
        pname[0] = '\0';
        sprintf(pname,"%s_%05i","pressure",i);
        p[count] = static_cast<vtkDoubleArray*>(pd->GetPointData()->GetArray(pname));
        count++;
    }

    // create a list of the shear vectors

    bool HaveTractions = true;

    vtkDoubleArray **traction = new vtkDoubleArray*[numArrays];

    count = 0;
    for (i = start; i <= stop; i += incr) {
        tname[0] = '\0';
        sprintf(tname,"%s_%05i","traction",i);
        traction[count] = static_cast<vtkDoubleArray*>(pd->GetPointData()->GetArray(tname));
        if (traction[count] == NULL) {
            delete [] traction;
            HaveTractions = false;
            break;
        }
        count++;
    }

    // create a list of the shear vectors

    bool HaveWSS = true;

    vtkDoubleArray **wss = new vtkDoubleArray*[numArrays];

    count = 0;
    for (i = start; i <= stop; i += incr) {
        wname[0] = '\0';
        sprintf(wname,"%s_%05i","WSS",i);
        wss[count] = static_cast<vtkDoubleArray*>(pd->GetPointData()->GetArray(wname));
        if (wss[count] == NULL) {
            delete [] wss;
            HaveWSS = false;
            break;
        }
        count++;
    }

    // create return vector

    vtkDoubleArray *meanpressure = vtkDoubleArray::New();
    meanpressure->SetNumberOfComponents(1);
    meanpressure->Allocate(numPts,10000);
    meanpressure->SetNumberOfTuples(numPts);
    meanpressure->SetName("pressure_avg");

    vtkDoubleArray *pressure_mmHg = vtkDoubleArray::New();
    pressure_mmHg->SetNumberOfComponents(1);
    pressure_mmHg->Allocate(numPts,10000);
    pressure_mmHg->SetNumberOfTuples(numPts);
    pressure_mmHg->SetName("pressure_avg_mmHg");

    // calc mean pressure

    for (i = 0; i < numPts; i++) {
        double pressure = 0.0;
        for (j = 0; j < numArrays; j++) {
            pressure += p[j]->GetTuple1(i);
        }
        pressure = pressure / numArrays;
        meanpressure->SetTuple1(i,pressure);
        double pressure_in_mmHg;
        if (sim_units_cm) {
            pressure_in_mmHg = pressure * 76 / 101325;
        } else {
            pressure_in_mmHg = pressure * 760 / 101325;
        }
        pressure_mmHg->SetTuple1(i,pressure_in_mmHg);
    }

    pd->GetPointData()->AddArray(meanpressure);
    pd->GetPointData()->AddArray(pressure_mmHg);
    meanpressure->Delete();
    pressure_mmHg->Delete();

    delete [] p;

    // if we have wall shear stress

    if (HaveTractions == true) {

        vtkDoubleArray *shearmean = vtkDoubleArray::New();
        shearmean->SetNumberOfComponents(1);
        shearmean->Allocate(numPts,10000);
        shearmean->SetNumberOfTuples(numPts);
        shearmean->SetName("TAWSS");

        vtkDoubleArray *shearpulse = vtkDoubleArray::New();
        shearpulse->SetNumberOfComponents(1);
        shearpulse->Allocate(numPts,10000);
        shearpulse->SetNumberOfTuples(numPts);
        shearpulse->SetName("shear_pulse");

        vtkDoubleArray *osi = vtkDoubleArray::New();
        osi->SetNumberOfComponents(1);
        osi->Allocate(numPts,10000);
        osi->SetNumberOfTuples(numPts);
        osi->SetName("OSI");

        for (i = 0; i < numPts; i++) {
            double v0 = 0.0;
            double v1 = 0.0;
            double v2 = 0.0;
            double pulsemag = 0.0;
            for (j = 0; j < numArrays; j++) {
                traction[j]->GetTuple(i,shear);
                v0 += shear[0];v1 += shear[1];v2 += shear[2];
                pulsemag += sqrt(shear[0]*shear[0]+shear[1]*shear[1]+shear[2]*shear[2]);
            }
            v0 = v0/numArrays; v1 = v1/numArrays; v2 = v2/numArrays;
            double mag = sqrt(v0*v0+v1*v1+v2*v2);
            shearmean->SetTuple1(i,mag);
            pulsemag = pulsemag / numArrays;
            shearpulse->SetTuple1(i,pulsemag);
            double osival = 0;
            if (pulsemag > 0.00001) {
                osival = 1.0/2.0*(1-mag/pulsemag);
            }
            osi->SetTuple1(i,osival);
        }

        fprintf(stdout,"found traction and adding mean, pulse, osi.\n");

        pd->GetPointData()->AddArray(shearmean);
        pd->GetPointData()->AddArray(shearpulse);
        pd->GetPointData()->AddArray(osi);
        shearmean->Delete();
        shearpulse->Delete();
        osi->Delete();

        delete [] traction;

    }

    if (HaveWSS == true) {

        vtkDoubleArray *shearmean_wss = vtkDoubleArray::New();
        shearmean_wss->SetNumberOfComponents(1);
        shearmean_wss->Allocate(numPts,10000);
        shearmean_wss->SetNumberOfTuples(numPts);
        shearmean_wss->SetName("TAWSS_wss");

        vtkDoubleArray *shearpulse_wss = vtkDoubleArray::New();
        shearpulse_wss->SetNumberOfComponents(1);
        shearpulse_wss->Allocate(numPts,10000);
        shearpulse_wss->SetNumberOfTuples(numPts);
        shearpulse_wss->SetName("shear_pulse_wss");

        vtkDoubleArray *osi_wss = vtkDoubleArray::New();
        osi_wss->SetNumberOfComponents(1);
        osi_wss->Allocate(numPts,10000);
        osi_wss->SetNumberOfTuples(numPts);
        osi_wss->SetName("OSI_wss");

        for (i = 0; i < numPts; i++) {
            double v0 = 0.0;
            double v1 = 0.0;
            double v2 = 0.0;
            double pulsemag = 0.0;
            for (j = 0; j < numArrays; j++) {
                wss[j]->GetTuple(i,shear);
                v0 += shear[0];v1 += shear[1];v2 += shear[2];
                pulsemag += sqrt(shear[0]*shear[0]+shear[1]*shear[1]+shear[2]*shear[2]);
            }
            v0 = v0/numArrays; v1 = v1/numArrays; v2 = v2/numArrays;
            double mag = sqrt(v0*v0+v1*v1+v2*v2);
            shearmean_wss->SetTuple1(i,mag);
            pulsemag = pulsemag / numArrays;
            shearpulse_wss->SetTuple1(i,pulsemag);
            double osival = 0;
            if (pulsemag > 0.00001) {
                osival = 1.0/2.0*(1-mag/pulsemag);
            }
            osi_wss->SetTuple1(i,osival);
        }

        fprintf(stdout,"found wss and adding mean, pulse, osi.\n");

        pd->GetPointData()->AddArray(shearmean_wss);
        pd->GetPointData()->AddArray(shearpulse_wss);
        pd->GetPointData()->AddArray(osi_wss);
        shearmean_wss->Delete();
        shearpulse_wss->Delete();
        osi_wss->Delete();

        delete [] wss;
    }
    return CV_OK;
}

// ==============================
// Read Residual Column from File
// ==============================
int readResidualFromHistorFile(const char* newResFile,std::vector<double> &newRes){
    // Init Vector
    char * pch;
    std::string line;
    // Create File
    ifstream myfile(newResFile);
    if (myfile.is_open()){
        while (getline(myfile,line)){
            pch = strtok ((char *)line.c_str()," ");
            pch = strtok (NULL, " ");
            pch = strtok (NULL, " ");
            newRes.push_back(atof(pch));
        }
        myfile.close();
    }else{
        cout << "Unable to open file";
        return 1;
    }
    // Write to a File
    return 0;
}

// ====================================
// CTEST: Compare Solver Residual Files
// ====================================
int PostSolver::CompareResidualFiles(const char* newResFile,const char* refResFile, double threshold){
    // Allocate
    std::vector<double> newRes;
    std::vector<double> refRes;

    // Write to a File
    FILE* outFile;
    outFile = fopen("residualDiff.out","w");

    // Header
    printf("\n");
    printf("=============================\n");
    printf("RESIDUAL FILE COMPARE UTILITY\n");
    printf("=============================\n");

    // Read Files
    printf("Reading Simulation Residual File: %s\n",newResFile);
    readResidualFromHistorFile(newResFile,newRes);
    printf("Reading Reference Residual File: %s\n",refResFile);
    readResidualFromHistorFile(refResFile,refRes);

    // Get Minimum Size
    int resSize = 0;
    if(newRes.size()>refRes.size()){
        resSize = refRes.size();
    }else{
        resSize = newRes.size();
    }

    // Compure the Difference Norm
    double resDiff = 0.0;
    printf("Computing Residual...");
    for(int loopA=0;loopA<resSize;loopA++){
        resDiff = resDiff + (refRes[loopA] - newRes[loopA])*(refRes[loopA] - newRes[loopA]);
    }
    resDiff = sqrt(resDiff/(double)resSize);
    printf("Done.\n");

    // PRINT REPORT ON FILE
    fprintf(outFile,"%s\n",(resDiff < threshold) ? "True" : "False");
    fprintf(outFile,"Residual %e, Threshold %e\n",resDiff,threshold);
    fprintf(outFile,"\n");
    fprintf(outFile,"Reading Simulation Residual File: %s\n",newResFile);
    fprintf(outFile,"Reading Reference Residual File: %s\n",refResFile);
    fprintf(outFile,"\n");
    fprintf(outFile,"%20s %20s\n","Sim Res","Ref Res");
    for(int loopA=0;loopA<resSize;loopA++){
        fprintf(outFile,"%20e %20e\n",newRes[loopA],refRes[loopA]);
    }
    fprintf(outFile,"\n");
    fclose(outFile);

    // Result File Written
    printf("Completed. Result written to residualDiff.out\n");

    // Return
    return 0;
}

// =====================================================================
// Get Name of Point Data Sets (should be all that needs to be compared)
// =====================================================================
void GetVTUPointData(vtkUnstructuredGrid* model,std::vector<std::string> &pointDataNames){
    for(int loopA=0;loopA<model->GetPointData()->GetNumberOfArrays();loopA++){
        pointDataNames.push_back(model->GetPointData()->GetArray(loopA)->GetName());
    }
}

// ====================================
// CTEST: Compare Solver Residual Files
// ====================================
int PostSolver::CompareVTUFiles(const char* newResFile,const char* refResFile, double threshold){

    // Header
    printf("\n");
    printf("========================\n");
    printf("VTU FILE COMPARE UTILITY\n");
    printf("========================\n");

    // Read two files
    // Read First Model
    printf("Reading Simulation File: %s\n",newResFile);
    vtkXMLUnstructuredGridReader* readerModel1 = NULL;
    readerModel1 = vtkXMLUnstructuredGridReader::New();
    readerModel1->SetFileName(newResFile);
    readerModel1->Update();
    vtkUnstructuredGrid* model1 = readerModel1->GetOutput();

    // Read Second Model
    printf("Reading Reference File: %s\n",refResFile);
    vtkXMLUnstructuredGridReader* readerModel2 = NULL;
    readerModel2 = vtkXMLUnstructuredGridReader::New();
    readerModel2->SetFileName(refResFile);
    readerModel2->Update();
    vtkUnstructuredGrid* model2 = readerModel2->GetOutput();

    // If number of cells or points is not the same then They are not compatible
    vtkIdType totPoint1 = model1->GetNumberOfPoints();
    vtkIdType totPoint2 = model2->GetNumberOfPoints();
    vtkIdType totCells1 = model1->GetNumberOfCells();
    vtkIdType totCells2 = model2->GetNumberOfCells();

    // If Not compatible Write A file to report
    int totPoints = 0.0;
    if((totPoint1 != totPoint2)||(totCells1 != totCells2)){
        FILE* outFile;
        outFile = fopen("vtuDiff.out","w");
        fprintf(outFile,"NOT COMPATIBLE\n");
        fclose(outFile);
        printf("Terminated. Files are not compatible, so exiting.\n");
        return 1;
    }else{
        totPoints = totPoint1;
    }

    // Get all PointData Array
    std::vector<std::string> pointDataNames1;
    std::vector<std::string> pointDataNames2;
    GetVTUPointData(model1,pointDataNames1);
    GetVTUPointData(model2,pointDataNames2);

    // Match PointDatasets
    printf("Matching Point Datasets...");
    std::vector<std::string> resultQtyName;
    std::vector<double> resultQtyRes;
    vtkDoubleArray* FirstArray = NULL;
    vtkDoubleArray* SecondArray = NULL;
    double diffNorm = 0.0;
    double firstValue = 0.0;
    double secondValue = 0.0;
    vtkIdType numComponents = 0;
    for(int loopA=0;loopA<pointDataNames1.size();loopA++){
        for(int loopB=0;loopB<pointDataNames2.size();loopB++){
            if (pointDataNames1[loopA].compare(pointDataNames2[loopB]) == 0){
                // These two Results are Matching
                // Get The two Vectors
                FirstArray = (vtkDoubleArray *)model1->GetPointData()->GetArray(loopA);
                SecondArray = (vtkDoubleArray *)model2->GetPointData()->GetArray(loopB);
                // Get Number Of Components
                numComponents = FirstArray->GetNumberOfComponents();
                // Eval the Difference Norm
                diffNorm = 0.0;
                for(int loopC=0;loopC<totPoints;loopC++){
                    for(int loopD=0;loopD<numComponents;loopD++){
                        firstValue = FirstArray->GetComponent(loopD,loopC);
                        secondValue = SecondArray->GetComponent(loopD,loopC);
                        diffNorm = diffNorm + (firstValue-secondValue)*(firstValue-secondValue);
                    }
                }
                diffNorm = sqrt(diffNorm/(double)(totPoints*numComponents));
                // Add To Result Lists
                resultQtyName.push_back(pointDataNames1[loopA]);
                resultQtyRes.push_back(diffNorm);
            }
        }
    }
    printf("Done.\n");

    // Compute the overall Difference Norm
    double overallNorm = 0.0;
    for(int loopA=0;loopA<resultQtyRes.size();loopA++){
        overallNorm = overallNorm + resultQtyRes[loopA]*resultQtyRes[loopA];
    }
    overallNorm = sqrt(overallNorm/(double)resultQtyRes.size());

    // Write Final File With All DiffNorms
    FILE* outFile;
    outFile = fopen("vtuDiff.out","w");
    // Print the norm
    fprintf(outFile,"%s\n",overallNorm < threshold ? "True" : "False");
    fprintf(outFile,"Overall Norm %e, Threshold %e\n",overallNorm, threshold);
    fprintf(outFile,"\n");
    // Print The list of norms for all result quantities
    fprintf(outFile,"%20s %20s\n","Point Result Name","Difference Norm");
    fprintf(outFile,"%20s %20s\n","=================","===============");
    for(int loopA=0;loopA<resultQtyName.size();loopA++){
        fprintf(outFile,"%20s %20e\n",resultQtyName[loopA].c_str(),resultQtyRes[loopA]);
    }
    fclose(outFile);
    printf("Completed. Result written to vtuDiff.out\n");
    return 0;
}

// ============
// MAIN PROGRAM
// ============
int main(int argc, char* argv[])
{

    int i,j;

    int stepnumber = 0;
    int newstepnumber = 0;
    int sn_start = 0;
    int sn_stop  = 0;
    int sn_incr  = 0;

    /* variables used in processing the commandline */
    int iarg, arglength;
    string tmpstr;
    bool StepNumberAvailable = false;

    char visfn[255];
    char visfnmesh[255];
    char vtufn[255];
    char vtpfn[255];
    /* Files to Compare Residuals and VTU */
    char resFile1[255];
    char resFile2[255];
    char vtufile1[255];
    char vtufile2[255];
    float threshold;

    char wallfn[255];

    visfn[0]='\0';
    visfnmesh[0]='\0';
    vtufn[0]='\0';
    vtpfn[0]='\0';
    resFile1[0]='\0';
    resFile2[0]='\0';
    vtufile1[0]='\0';
    vtufile2[0]='\0';
    wallfn[0]='\0';
    double rho=-1.0;
    double mu=-1.0;

    char realfn[255];
    realfn[0]='\0';

    // default directory is current directory
    char indir[255];
    indir[0]='.';
    indir[1]='/';
    indir[2]='\0';

    // default directory is current directory
    char outdir[255];
    outdir[0]='.';
    outdir[1]='/';
    outdir[2]='\0';

    char gzext[4];
    double currTuple[5];
    gzext[0]='\0';
#ifdef USE_ZLIB
    gzext[0]='.';
    gzext[1]='g';
    gzext[2]='z';
    gzext[3]='\0';
#endif

    /* BEGIN PROCESSING COMMAND-LINE ARGUMENTS */
    /* Assume the command line is okay */
    bool BogusCmdLine = false;
    /* Assume no options specified at command line */
    bool RequestedHelp = false;
    bool RequestedStepNumber = false;
    bool RequestedVIS = false;
    bool RequestedVISmesh = false;
    bool RequestedSolution= false;
    bool RequestedInPlaneTraction = false;
    bool RequestedDisplacements  = false;
    bool RequestedWallprops  = false;
    bool RequestedWSS  = false;
    bool RequestedCalcWS= false;
    bool RequestedApplyWD= false;
    bool RequestedWallFilter = false;
    bool RequestedVTU = false;
    bool RequestedVTP = false;
    bool RequestedMultipleSteps = false;
    bool RequestedVTKcombo = false;
    bool RequestedAll = true;
    bool RequestedUnitsCm = true;
    bool RequestedTimeDeriv = false;
    bool RequestedYbar = false;
    bool RequestedFlowSolverFormat = false;
    bool RequestedOnlyLastStep = false;
    bool RequestedNewSn = false;
    bool RequestedASCIIFormat = false;
    bool RequestedReadResidual = false;
    bool RequestedCompareResults = false;

    /* argc is the number of strings on the command-line */
    /*  starting with the program name */
    for(iarg=1; iarg<argc; iarg++){
        arglength = strlen(argv[iarg]);
        /* replace 0..arglength-1 with argv[iarg] */
        tmpstr.replace(0,arglength,argv[iarg],0,arglength);
        if(tmpstr=="-h"){
            RequestedHelp = true;
            cout << endl;
            cout << "usage:" <<endl;
            cout << "  postsolver -sn stepnumber ..." << endl;
            cout << endl;
            cout << "COMMAND-LINE ARGUMENT SUMMARY" << endl;
            cout << "  -h                  : Display usage and command-line argument summary"<< endl;
            cout << "  -sn stepnumber      : Specify single step number to reduce"<< endl;
            cout << "  -start stepnumber   : Specify starting step number"<< endl;
            cout << "  -stop stepnumber    : Specify stopping step number"<< endl;
            cout << "  -newsn stepnumber   : override step number in file"<<endl;
            cout << "  -incr increment     : Specify increment between steps"<< endl;
            cout << "  -ph                 : Write flowsolver-format file restart.<stepnumber>.0"<<endl;
            cout << "  -laststep           : Only write last step to file restart.<stepnumber>.0"<<endl;
            cout << "  -vis file_prefix    : Write Vis-format results file" <<endl;
            cout << "  -vismesh filename   : Write Vis-format mesh file" <<endl;
            cout << "  -vtu prefix or file : Write VTK XML UnstructuredGrid" <<endl;
            cout << "  -vtp prefix or file : Write Surface Only VTK XML PolyData" <<endl;
            cout << "  -vtkcombo           : Combine all VTK in single file" <<endl;
            cout << "  -sol                : Reduce solution(pressure and velocity)"<<endl;
            cout << "  -traction           : Reduce in-plane traction"<<endl;
            cout << "  -disp               : Reduce displacements"<<endl;
            cout << "  -wallprop           : Reduce wall properties"<<endl;
            cout << "  -rho                : Fluid density"<<endl;
            cout << "  -mu                 : Fluid viscosity"<<endl;
            cout << "  -td                 : Reduce time-derivative field"<< endl;
            cout << "  -wss                : Reduce Wall Shear Stress"<<endl;
            cout << "  -wfilter filename   : output wall data only on specified wall"<<endl;
            cout << "  -calcws             : Recalculate wall stress on specified wall"<<endl;
            cout << "  -applywd            : Apply wall deformation during wall stress calculation"<<endl;
            cout << "  -ybar               : Reduce ybar field"<<endl;
            cout << "  -nonbinary          : Read/Write files in ASCII format" <<endl;
            //cout << "  -none               : do not reduce everything found"<<endl;
            cout << "  -all                : Reduce all available data"<<endl;
            cout << "  -sim_units_mm       : set sim  units to mm"<<endl;
            cout << "  -sim_units_cm       : set sim  units to cm (default)"<<endl;
            cout << "  -indir              : directory containing restart files (default .)"<<endl;
            cout << "  -outdir             : directory containing output files (default .)"<<endl;
            cout << "  -res                : Compares residual profiles on histor.dat files"<<endl;
            cout << "                        and writes a report to residualDiff.out"<<endl;
            cout << "  -compare            : Compare pressures and velocities in result VTU files"<<endl;
            cout << "                        and writes a report to vtuDiff.out"<<endl;

            cout << "END COMMAND-LINE ARGUMENT SUMMARY" << endl;
            cout << endl;
        }
        else if(tmpstr=="-sn"){
            iarg++;
            stepnumber = atoi(argv[iarg]);
            StepNumberAvailable = true;
        }
        else if(tmpstr=="-newsn"){
            RequestedNewSn = true;
            iarg++;
            newstepnumber = atoi(argv[iarg]);
        }
        else if(tmpstr=="-start"){
            iarg++;
            sn_start = atoi(argv[iarg]);
            RequestedMultipleSteps = true;
        }
        else if(tmpstr=="-stop"){
            iarg++;
            sn_stop = atoi(argv[iarg]);
            RequestedMultipleSteps = true;
        }
        else if(tmpstr=="-incr"){
            iarg++;
            sn_incr = atoi(argv[iarg]);
            RequestedMultipleSteps = true;
        }
        else if(tmpstr=="-vis"){
            RequestedVIS = true;
            iarg++;
            visfn[0]='\0';
            sprintf(visfn,"%s",argv[iarg]);
        }
        else if(tmpstr=="-vismesh"){
            RequestedVISmesh = true;
            iarg++;
            visfnmesh[0]='\0';
            sprintf(visfnmesh,"%s",argv[iarg]);
        }
        else if(tmpstr=="-indir"){
            iarg++;
            indir[0]='\0';
            sprintf(indir,"%s/",argv[iarg]);
        }
        else if(tmpstr=="-outdir"){
            iarg++;
            outdir[0]='\0';
            sprintf(outdir,"%s/",argv[iarg]);
        }
        else if(tmpstr=="-sol"){
            RequestedSolution = true;
        }
        else if(tmpstr=="-traction"){
            RequestedInPlaneTraction = true;
        }
        else if(tmpstr=="-disp"){
            RequestedDisplacements = true;
        }
        else if(tmpstr=="-wallprop"){
            RequestedWallprops = true;
        }
        else if(tmpstr=="-wfilter"){
            RequestedWallFilter = true;
            iarg++;
            wallfn[0]='\0';
            sprintf(wallfn,"%s",argv[iarg]);
        }
        else if(tmpstr=="-wss"){
            RequestedWSS = true;
        }
        else if(tmpstr=="-calcws"){
            RequestedCalcWS = true;
        }
        else if(tmpstr=="-applywd"){
            RequestedApplyWD = true;
        }
        else if(tmpstr=="-rho"){
            iarg++;
            rho = atof(argv[iarg]);
        }
        else if(tmpstr=="-mu"){
            iarg++;
            mu = atof(argv[iarg]);
        }
        else if(tmpstr=="-ybar"){
            RequestedYbar = true;
        }
        else if(tmpstr=="-td"){
            RequestedTimeDeriv = true;
        }
        else if(tmpstr=="-ph"){
            RequestedFlowSolverFormat = true;
        }
        else if(tmpstr=="-laststep"){
            RequestedOnlyLastStep = true;
        }
        else if(tmpstr=="-none"){
            RequestedAll = false;
        }
        else if(tmpstr=="-all"){
            RequestedAll = true;
        }
        else if(tmpstr=="-sim_units_mm"){
            RequestedUnitsCm = false;
        }
        else if(tmpstr=="-sim_units_cm"){
            RequestedUnitsCm = true;
        }
        else if(tmpstr=="-vtu"){
            RequestedVTU = true;
            iarg++;
            vtufn[0]='\0';
            sprintf(vtufn,"%s",argv[iarg]);
        }
        else if(tmpstr=="-vtp"){
            RequestedVTP = true;
            iarg++;
            vtpfn[0]='\0';
            sprintf(vtpfn,"%s",argv[iarg]);
        }
        else if(tmpstr=="-vtkcombo"){
            RequestedVTKcombo = true;
        }
        else if(tmpstr=="-nonbinary"){
            RequestedASCIIFormat = true;
        }
        else if(tmpstr=="-res"){
            RequestedReadResidual = true;
            // First Residual File
            iarg++;
            resFile1[0]='\0';
            sprintf(resFile1,"%s",argv[iarg]);
            // Second Residual File
            iarg++;
            resFile2[0]='\0';
            sprintf(resFile2,"%s",argv[iarg]);
            iarg++;
            sscanf(argv[iarg], "%e", &threshold);
        }
        else if(tmpstr=="-compare"){
            RequestedCompareResults = true;
            // First Residual File
            iarg++;
            vtufile1[0]='\0';
            sprintf(vtufile1,"%s",argv[iarg]);
            // Second Residual File
            iarg++;
            vtufile2[0]='\0';
            sprintf(vtufile2,"%s",argv[iarg]);
            iarg++;
            sscanf(argv[iarg], "%e", &threshold);
        }
        else {
            BogusCmdLine = true;
        }
        /* reset tmpstr for next argument */
        tmpstr.erase(0,arglength);
    }

    // CONFIRMATION MESSAGES
    if(RequestedSolution){
        cout << "Will reduce solution(pressure and velocity) as requested" << endl;
    }
    if(RequestedInPlaneTraction){
        cout << "Will reduce boundary flux field as requested" << endl;
    }
    if(RequestedDisplacements){
        cout << "Will reduce displacement field as requested" << endl;
    }
    if(RequestedWallprops){
        cout << "Will reduce wall property field as requested" << endl;
    }
    if(RequestedWSS){
        cout << "Will reduce wall shear stress field as requested" << endl;
    }
    if(RequestedTimeDeriv){
        cout << "Will reduce time-derivative field as requested" << endl;
    }
    if(RequestedYbar){
        cout << "Will reduce ybar field as requested" << endl;
    }

    if(RequestedSolution||RequestedTimeDeriv||RequestedDisplacements||RequestedWSS||RequestedInPlaneTraction||RequestedWallprops||RequestedYbar){
        RequestedAll=false;
    }

    if(RequestedAll){
        cout << "Will reduce all available data" << endl;
        RequestedSolution = true;
        RequestedTimeDeriv = true;
        RequestedDisplacements  = true;
        RequestedWSS  = true;
        RequestedInPlaneTraction = true;
        RequestedWallprops  = true;
        RequestedYbar=true;
    }

    if(RequestedHelp){
        cout << endl;
        cout << "Exiting before performing any of the above operations due to -h flag";
        cout << endl;
        return(0);
    }

    if ((StepNumberAvailable == true) && (RequestedMultipleSteps == true)) {
        cout << "Cannot specify both single step and multiple steps, so exiting." << endl;
        return(1);
    }

    if((!RequestedReadResidual)&&(!RequestedCompareResults)){
        if ((RequestedMultipleSteps == false) && (StepNumberAvailable == false)) {
            cout << "No step number or range of steps given, so exiting." << endl;
            return(1);
        }
    }

    // Exit if the user has set RequestedNewSn but not RequestedFlowSolverFormat
    if ((RequestedFlowSolverFormat == false) && (RequestedNewSn == true)) {
        cout << "The new step number option only works with the flowsolver (-ph) export format, so exiting." << endl;
        return(1);
    }

    if (!RequestedMultipleSteps) {
        sn_start = stepnumber;
        sn_stop = stepnumber;
        sn_incr = 1;
    }

    // prevent user from specifying dumb increment
    if (sn_incr == 0) {
        sn_stop = sn_start;
        sn_incr = 1;
    }

    // need to set step number since we pull numvars from
    // stepnumber
    if (RequestedMultipleSteps) {
        stepnumber = sn_start;
    }

    // do work

    double *qglobal = NULL;
    double *tglobal = NULL;
    double *dglobal = NULL;
    double *wglobal = NULL;
    double *aglobal = NULL;
    double *yglobal = NULL;

    double *wpglobal = NULL;
    int *wallflag = NULL;

    int wallElementNum;
    int *wallElements=NULL;

    PostSolver* pp = new PostSolver();

    // ==========================================
    // CTEST: READ AND COMPARE TWO RESIDUAL FILES
    // ==========================================
    if(RequestedReadResidual){
        pp->CompareResidualFiles(resFile1,resFile2,threshold);
        return 0;
    }

    // =====================================
    // CTEST: READ AND COMPARE TWO VTU FILES
    // =====================================
    if(RequestedCompareResults){
        pp->CompareVTUFiles(vtufile1,vtufile2,threshold);
        return 0;
    }

    if (RequestedASCIIFormat) {
        pp->SetInputFileType("ascii");
    }

    if (pp->SetInputDirectory(indir) == CV_ERROR) {
        return 1;
    }
    if (pp->QueryNumberOfSolutionVariables(stepnumber) == CV_ERROR) {
        return 1;
    }
    if (pp->QueryNumberOfProcessors() == CV_ERROR) {
        return 1;
    }

    if(RequestedCalcWS && mu<=0.0){
        if(pp->QueryFluidProperties()==CV_ERROR){
            return 1;
        }
        double *matprops=pp->GetFluidProperties();
        if(matprops!=NULL){
            mu=matprops[1];
            if(mu<=0.0){
                cout << "ERROR: fluid viscosity(mu)<=0! in geombc!" << endl;
                return 1;
            }
        }

    }

    if (pp->ReadConnectivity() == CV_ERROR) {
        return 1;
    }

    int numvar = pp->GetNumberOfSolutionVariables();

    // assume always linear tets
    int nendx = 4;

    int nshgtot = pp->GetNumberOfNodalCoordinates();
    int neltot = pp->GetNumberOfElements();
    int numprocs = pp->GetNumberOfProcessorsUsed();

    cout << "Number of solution variables found (" << numvar << ")" << endl;
    cout << "Number of nodes found (" << nshgtot << ")" << endl;
    cout << "Number of elements found (" << neltot << ")" << endl;
    cout << "Number of processors used (" << numprocs << ")" << endl;


    double *xglobal = pp->GetCoordinatePtr();
    int *ien = pp->GetConnectivityPtr();

    //
    //  write out mesh in Spectrum Vis format if requested
    // NOTE: this code assumes there is only 1 material region.
    // NOTE: assumes only linear tets!
    //

    if (RequestedVISmesh) {

        realfn[0]='\0';
        sprintf(realfn,"%s%s%s",outdir,visfnmesh,gzext);

        gzFile fmesh = NULL;
        fmesh = gzopen(realfn,"wb");

        // output the nodes
        char s[80];
        gzprintf(fmesh, "problem  \"%s\"  \n", "simvascular mesh");
        gzprintf(fmesh, "  time information \n");
        gzprintf(fmesh, "    number of time steps %d \n", 1);
        gzprintf(fmesh, "    time steps \n");
        gzprintf(fmesh, "    %d  %g \n", 1, 1.0);
        gzprintf(fmesh, "    end time steps \n");
        gzprintf(fmesh, "  end time information \n\n");
        s[0]='\0';
        sprintf (s, "region_%d", 1);
        gzprintf(fmesh, "  region  \"%s\" \n", s);
        gzprintf(fmesh, "    nature \"%s\" \n", "solid");

        gzprintf(fmesh, "    number of nodal coordinates %d \n", nshgtot);
        gzprintf(fmesh, "    nodal coordinates \n");

        for(i=0; i< nshgtot; i++){
            gzprintf(fmesh,"%i ",i+1);
            for(j=0; j < 3; j++) gzprintf(fmesh,"%22.7le  ",xglobal[j*nshgtot+i]);
            gzprintf(fmesh,"\n");
        }

        gzprintf(fmesh, "    end node coordinates \n");

        // output the elements

        gzprintf(fmesh, "    element set \"%s\"  \n", "eset_1");
        gzprintf(fmesh, "      material id 0  \n");
        gzprintf(fmesh, "      nodes per element %d \n", 4);
        gzprintf(fmesh, "      topology \"%s\" \n", "tet");
        gzprintf(fmesh, "      number of elements %d \n", neltot);
        gzprintf(fmesh, "      connectivity \n" );

        for(i=0; i< neltot; i++){
            gzprintf(fmesh,"%i ", i+1);
            for(j=0; j < nendx; j++) gzprintf(fmesh,"%d  ",ien[j*neltot+i]);
            gzprintf(fmesh,"\n");
        }


        gzprintf(fmesh, "      end connectivity \n");
        gzprintf(fmesh, "    end element set \n");
        gzprintf(fmesh, "  end region \n\n");
        gzprintf(fmesh, "end problem  \n");
        gzclose(fmesh);

    }

//    vtkPoints* pts = NULL;
    vtkUnstructuredGrid* grid = NULL;
    vtkUnstructuredGrid* gridForAverage = NULL;

    if (RequestedVTU || RequestedVTP) {

        grid=createGrid(nshgtot,xglobal,neltot,ien);

    }

    if(RequestedWallFilter){

        if ( (SetWallFlag(nshgtot,wallfn,&wallflag)) == CV_ERROR ) {
            return 1;
        }

        if(RequestedCalcWS){
            if ( (GetWallElements(neltot,ien,wallfn,&wallElementNum,&wallElements)) == CV_ERROR ) {
                return 1;
            }
        }

    }

    if(RequestedWallprops){
        if(pp->QueryWallProperties()==CV_ERROR){//must call after ReadConnectivity()
            return 1;
        }
        wpglobal = pp->GetWallProperties();
    }

    // ===============================
    // NOW LOOP OVER THE RESTART FILES
    // ===============================

    for ( stepnumber = sn_start; stepnumber <= sn_stop; stepnumber += sn_incr) {

        int numq = 0;
        int numt = 0;
        int numd = 0;
        int numw = 0;
        int numa = 0;
        int numy = 0;

        if (RequestedAll) {
            RequestedSolution = true;
            RequestedInPlaneTraction = true;
            RequestedDisplacements  = true;
            //RequestedWallprops  = true;
            RequestedWSS  = true;
            RequestedTimeDeriv = true;
            RequestedYbar=true;
        }

        if(RequestedSolution){
            if ( (pp->ParseRestartFile( stepnumber , "solution" , &numq, &qglobal)) == CV_ERROR ) {
                if (RequestedAll) {
                    RequestedSolution=false;
                }else{
                    cout << "ERROR reading solution in step " << stepnumber << "!" << endl;
                    return 1;
                }
            }
        }

        if (RequestedTimeDeriv) {
            if ( (pp->ParseRestartFile( stepnumber , "time derivative of solution" , &numa, &aglobal)) == CV_ERROR ) {
                if (RequestedAll) {
                    RequestedTimeDeriv = false;
                } else {
                    cout << "ERROR reading time derivative of solution in step " << stepnumber << "!" << endl;
                    return 1;
                }
            }
        }

        if (RequestedDisplacements) {
            if ( (pp->ParseRestartFile( stepnumber , "displacement" , &numd, &dglobal)) == CV_ERROR ) {
                if (RequestedAll) {
                    RequestedDisplacements = false;
                } else {
                    cout << "ERROR reading displacement in step " << stepnumber << "!" << endl;
                    return 1;
                }
            }else{
                if(RequestedWallFilter){
                    WallFiltering(dglobal,nshgtot,3,wallflag); //set displacement zero if not on the wall
                }
            }
        }

        if (RequestedWallprops) {
            if(wpglobal==NULL){
                if ( pp->ExistRestartFile(0) ==CV_ERROR || (pp->ParseRestartFile( 0 , "varwallprop" , &numd, &wpglobal)) == CV_ERROR  ) {
                    if (RequestedAll) {
                        RequestedWallprops = false;
                    } else {
                        cout << "ERROR reading wall properties from restart.0.x!" << endl;
                        return 1;
                    }
                }
            }
        }

        if(RequestedWallFilter){
            if(RequestedCalcWS){//recalculate wall traction and shear stress
                if(dglobal==NULL){
                    cout << "Warning: no displacement found for wall stress calculation!" << endl;
                }
                if(qglobal==NULL){
                    cout << "ERROR: no solution found for wall stress calculation!" << endl;
                    return 1;
                }
                if ( (CalculateWallStress(nshgtot,wallElementNum,wallElements,RequestedApplyWD,xglobal,dglobal,qglobal,mu,&tglobal,&wglobal)) == CV_ERROR ) {
                    return 1;
                }
            }
        }

        if (RequestedInPlaneTraction) {
            if(!RequestedCalcWS){
                if ( (pp->ParseRestartFile( stepnumber , "boundary flux" , &numt, &tglobal)) == CV_ERROR ) {
                    if (RequestedAll) {
                        RequestedInPlaneTraction = false;
                    } else {
                        cout << "ERROR reading boundary flux in step " << stepnumber << "!" << endl;
                        return 1;
                    }
                }
            }
        }

        if (RequestedWSS) {
            if(!RequestedCalcWS){
                if ( (pp->ParseRestartFile( stepnumber , "wall shear stresses" , &numw, &wglobal)) == CV_ERROR ) {
                    if (RequestedAll) {
                        RequestedWSS = false;
                    } else {
                        cout << "ERROR reading wall shear stresses in step " << stepnumber << "!" << endl;
                        return 1;
                    }
                }
            }
        }

        if (RequestedYbar) {
            if ( (pp->ParseRestartFile( stepnumber , "ybar" , &numy, &yglobal)) == CV_ERROR ) {
                if (RequestedAll) {
                    RequestedYbar = false;
                } else {
                    cout << "ERROR reading ybar in step " << stepnumber << "!" << endl;
                    return 1;
                }
            }
        }

        // ===========================
        // Export in Flowsolver format
        // ===========================
        if (RequestedFlowSolverFormat){

            if(!RequestedOnlyLastStep || (RequestedOnlyLastStep&&stepnumber==sn_stop)) {

                cout << "Exporting Flowsolver restart file...";

                pp->ExportFlowSolverFileFormat(stepnumber,newstepnumber,RequestedNewSn,
                        RequestedSolution,RequestedTimeDeriv,RequestedDisplacements,RequestedYbar,
                        qglobal,aglobal,dglobal,yglobal,numy,outdir);

                cout << "Done." << endl;
            }
        }

        // =============================
        // Write out Spectrum Vis format
        // =============================
        if(RequestedVIS) {

            int numNodes = nshgtot;

            realfn[0]='\0';
            sprintf(realfn,"%s%s_res%05i.vis%s",outdir,visfn,stepnumber,gzext);

            gzFile frest = NULL;
            frest = gzopen(realfn,"wb");

            // write out initial header
            gzprintf(frest,"  region \"fluid_region\"\n");
            gzprintf(frest,"    time step 1\n");
            gzprintf(frest,"    time 1.0\n");
            gzprintf(frest,"\n");

            // pressure
            gzprintf(frest,"    analysis results \"pressure\"\n");
            gzprintf(frest,"      number of data %i\n",numNodes);
            gzprintf(frest, "      type \"nodal\"\n");
            gzprintf(frest, "      order \"scalar\"\n");
            gzprintf(frest, "      length 1\n");
            gzprintf(frest, "      data\n");
            for(i=0; i< nshgtot; i++){
                gzprintf(frest,"%25.15le \n",qglobal[i]);
            }
            gzprintf(frest, "      end data\n");
            gzprintf(frest, "    end analysis results\n");
            gzprintf(frest, "\n");

            // velocity
            gzprintf(frest, "    analysis results \"velocity\"\n");
            gzprintf(frest, "      number of data %i\n",numNodes);
            gzprintf(frest, "      type \"nodal\"\n");
            gzprintf(frest, "      order \"vector\"\n");
            gzprintf(frest, "      number of components 3\n");
            gzprintf(frest, "      components\n");
            gzprintf(frest, "      \"x\"\n");
            gzprintf(frest, "      \"y\"\n");
            gzprintf(frest, "      \"z\"\n");
            gzprintf(frest, "      end components\n");
            gzprintf(frest, "      length 3\n");
            gzprintf(frest, "      data\n");
            for (i=0; i< nshgtot; i++) {
                for (j=1; j < 4; j++) {
                    gzprintf(frest,"%25.15le ",qglobal[j*nshgtot+i]);
                }
                gzprintf(frest,"\n");
            }
            gzprintf(frest, "      end data\n");
            gzprintf(frest, "    end analysis results\n");
            gzprintf(frest, "\n");

            if (numvar > 5) {
                // transport
                gzprintf(frest,"    analysis results \"transport\"\n");
                gzprintf(frest,"      number of data %i\n",numNodes);
                gzprintf(frest, "      type \"nodal\"\n");
                gzprintf(frest, "      order \"scalar\"\n");
                gzprintf(frest, "      length 1\n");
                gzprintf(frest, "      data\n");
                for(i=0; i< nshgtot; i++){
                    gzprintf(frest,"%25.15le \n",qglobal[5*nshgtot+i]);
                }
                gzprintf(frest, "      end data\n");
                gzprintf(frest, "    end analysis results\n");
                gzprintf(frest, "\n");
            }

            // traction
            if(RequestedInPlaneTraction) {
                gzprintf(frest, "    analysis results \"traction\"\n");
                gzprintf(frest, "      number of data %i\n",numNodes);
                gzprintf(frest, "      type \"nodal\"\n");
                gzprintf(frest, "      order \"vector\"\n");
                gzprintf(frest, "      number of components 3\n");
                gzprintf(frest, "      components\n");
                gzprintf(frest, "      \"x\"\n");
                gzprintf(frest, "      \"y\"\n");
                gzprintf(frest, "      \"z\"\n");
                gzprintf(frest, "      end components\n");
                gzprintf(frest, "      length 3\n");
                gzprintf(frest, "      data\n");
                for (i=0; i< nshgtot; i++) {
                    for (j=1; j < 4; j++) {
                        gzprintf(frest,"%25.15le ",tglobal[j*nshgtot+i]);
                    }
                    gzprintf(frest,"\n");
                }
                gzprintf(frest, "      end data\n");
                gzprintf(frest, "    end analysis results\n");
                gzprintf(frest, "\n");
            }

            // displacements
            if(RequestedDisplacements) {
                gzprintf(frest, "    analysis results \"displacement\"\n");
                gzprintf(frest, "      number of data %i\n",numNodes);
                gzprintf(frest, "      type \"nodal\"\n");
                gzprintf(frest, "      order \"vector\"\n");
                gzprintf(frest, "      number of components 3\n");
                gzprintf(frest, "      components\n");
                gzprintf(frest, "      \"x\"\n");
                gzprintf(frest, "      \"y\"\n");
                gzprintf(frest, "      \"z\"\n");
                gzprintf(frest, "      end components\n");
                gzprintf(frest, "      length 3\n");
                gzprintf(frest, "      data\n");
                for (i=0; i< nshgtot; i++) {
                    for (j=0; j < 3; j++) {
                        gzprintf(frest,"%25.15le ",dglobal[j*nshgtot+i]);
                    }
                    gzprintf(frest,"\n");
                }
                gzprintf(frest, "      end data\n");
                gzprintf(frest, "    end analysis results\n");
                gzprintf(frest, "\n");
            }

            // wall properties
            if(RequestedWallprops) {
                gzprintf(frest, "    analysis results \"wall propertie\"\n");
                gzprintf(frest, "      number of data %i\n",numNodes);
                gzprintf(frest, "      type \"nodal\"\n");
                gzprintf(frest, "      order \"vector\"\n");
                gzprintf(frest, "      number of components 2\n");
                gzprintf(frest, "      components\n");
                gzprintf(frest, "      \"thickness\"\n");
                gzprintf(frest, "      \"Evw\"\n");
                gzprintf(frest, "      end components\n");
                gzprintf(frest, "      length 2\n");
                gzprintf(frest, "      data\n");
                for (i=0; i< nshgtot; i++) {
                    for (j=0; j < 2; j++) {
                        gzprintf(frest,"%25.15le ",wpglobal[j*nshgtot+i]);
                    }
                    gzprintf(frest,"\n");
                }
                gzprintf(frest, "      end data\n");
                gzprintf(frest, "    end analysis results\n");
                gzprintf(frest, "\n");
            }

            // Wall Shear Stress
            if(RequestedWSS) {
                gzprintf(frest, "    analysis results \"wall shear stress\"\n");
                gzprintf(frest, "      number of data %i\n",numNodes);
                gzprintf(frest, "      type \"nodal\"\n");
                gzprintf(frest, "      order \"vector\"\n");
                gzprintf(frest, "      number of components 3\n");
                gzprintf(frest, "      components\n");
                gzprintf(frest, "      \"x\"\n");
                gzprintf(frest, "      \"y\"\n");
                gzprintf(frest, "      \"z\"\n");
                gzprintf(frest, "      end components\n");
                gzprintf(frest, "      length 3\n");
                gzprintf(frest, "      data\n");
                for (i=0; i< nshgtot; i++) {
                    for (j=0; j < 3; j++) {
                        gzprintf(frest,"%25.15le ",wglobal[j*nshgtot+i]);
                    }
                    gzprintf(frest,"\n");
                }
                gzprintf(frest, "      end data\n");
                gzprintf(frest, "    end analysis results\n");
                gzprintf(frest, "\n");
            }

            // CAREFUL - DES - MAKE SURE THAT THIS IS FINE !!!
            // Time Derivative of Solution
            if(RequestedTimeDeriv) {
                gzprintf(frest, "    analysis results \"time derivative of solution\"\n");
                gzprintf(frest, "      number of data %i\n",numNodes);
                gzprintf(frest, "      type \"nodal\"\n");
                gzprintf(frest, "      order \"vector\"\n");
                gzprintf(frest, "      number of components 4\n");
                gzprintf(frest, "      components\n");
                gzprintf(frest, "      \"p\"\n");
                gzprintf(frest, "      \"x\"\n");
                gzprintf(frest, "      \"y\"\n");
                gzprintf(frest, "      \"z\"\n");
                gzprintf(frest, "      end components\n");
                gzprintf(frest, "      length 4\n");
                gzprintf(frest, "      data\n");
                for (i=0; i< nshgtot; i++) {
                    for (j=0; j < 4; j++) {
                        gzprintf(frest,"%25.15le ",aglobal[j*nshgtot+i]);
                    }
                    gzprintf(frest,"\n");
                }
                gzprintf(frest, "      end data\n");
                gzprintf(frest, "    end analysis results\n");
                gzprintf(frest, "\n");
            }

            gzclose(frest);

        }

        // ================================
        // Generate yBar file for each step
        // ================================
//        if(RequestedYbar) {
//            pp->ExportYBar(stepnumber,numy,yglobal,outdir);
//        }

        // =========================
        // Generate VTU or VTP Files
        // =========================
        if ((RequestedVTU == true) || (RequestedVTP == true)) {

            char pname[80];
            char vname[80];
            char tname[80];
            char dname[80];
            char wpname[80];
            char wname[80];
            char aname[80];
            char yname[80];
            pname[0] = '\0';
            vname[0] = '\0';
            tname[0] = '\0';
            dname[0] = '\0';
            wpname[0] = '\0';
            wname[0] = '\0';
            aname[0] = '\0';
            yname[0] = '\0';

            if (!RequestedVTKcombo) {
                sprintf(pname,"%s","pressure");
                sprintf(vname,"%s","velocity");
                sprintf(tname,"%s","traction");
                sprintf(dname,"%s","displacement");
                sprintf(wpname,"%s","wallproperty");
                sprintf(wname,"%s","WSS");
                sprintf(aname,"%s","timeDeriv");
                sprintf(yname,"%s","ybar");
            } else {
                sprintf(pname,"%s_%05i","pressure",stepnumber);
                sprintf(vname,"%s_%05i","velocity",stepnumber);
                sprintf(tname,"%s_%05i","traction",stepnumber);
                sprintf(dname,"%s_%05i","displacement",stepnumber);
                sprintf(wpname,"%s_%05i","wallproperty",0);
                sprintf(wname,"%s_%05i","WSS",stepnumber);
                sprintf(aname,"%s_%05i","timeDeriv",stepnumber);
                sprintf(yname,"%s_%05i","ybar",stepnumber);
            }

            // ========
            // PRESSURE
            // ========
            if(RequestedSolution){
                vtkDoubleArray *pressure = vtkDoubleArray::New();
                pressure->SetNumberOfComponents(1);
                pressure->Allocate(nshgtot,10000);
                pressure->SetNumberOfTuples(nshgtot);
                pressure->SetName(pname);
                for(i=0; i< nshgtot; i++){
                    pressure->SetTuple1(i,qglobal[i]);
                }

                grid->GetPointData()->AddArray(pressure);
                grid->GetPointData()->SetActiveScalars(pname);

                pressure->Delete();
            }
            // ========
            // VELOCITY
            // ========
            if(RequestedSolution){
                vtkDoubleArray *velocity = vtkDoubleArray::New();
                velocity->SetNumberOfComponents(3);
                velocity->Allocate(nshgtot,10000);
                velocity->SetNumberOfTuples(nshgtot);
                velocity->SetName(vname);
                for (i=0; i< nshgtot; i++) {
                    velocity->SetTuple3(i,qglobal[1*nshgtot+i],qglobal[2*nshgtot+i],qglobal[3*nshgtot+i]);
                }

                grid->GetPointData()->AddArray(velocity);
                grid->GetPointData()->SetActiveVectors(vname);

                velocity->Delete();
            }

            // ignore transport if it exists

            // ========
            // TRACTION
            // ========

            if(RequestedInPlaneTraction) {

                vtkDoubleArray *traction = vtkDoubleArray::New();
                traction->SetNumberOfComponents(3);
                traction->Allocate(nshgtot,10000);
                traction->SetNumberOfTuples(nshgtot);
                traction->SetName(tname);
                for (i=0; i< nshgtot; i++) {
                    traction->SetTuple3(i,tglobal[0*nshgtot+i],tglobal[1*nshgtot+i],tglobal[2*nshgtot+i]);
                }

                grid->GetPointData()->AddArray(traction);

                traction->Delete();

            }

            // =============
            // DISPLACEMENTS
            // =============

            if (RequestedDisplacements) {
                if(RequestedWallFilter){
                    WallFiltering(dglobal,nshgtot,3,wallflag); //set displacement zero if not on the wall
                }
                vtkDoubleArray *displacement = vtkDoubleArray::New();
                displacement->SetNumberOfComponents(3);
                displacement->Allocate(nshgtot,10000);
                displacement->SetNumberOfTuples(nshgtot);
                displacement->SetName(dname);
                for (i=0; i< nshgtot; i++) {
                    displacement->SetTuple3(i,dglobal[0*nshgtot+i],dglobal[1*nshgtot+i],dglobal[2*nshgtot+i]);
                }

                grid->GetPointData()->AddArray(displacement);

                displacement->Delete();

            }

            // =============
            // WALLPROPERTIES
            // =============

            if (RequestedWallprops) {

                vtkDoubleArray *wallproperty = vtkDoubleArray::New();
                wallproperty->SetNumberOfComponents(2);
                wallproperty->Allocate(nshgtot,10000);
                wallproperty->SetNumberOfTuples(nshgtot);
                wallproperty->SetName(wpname);
                for (i=0; i< nshgtot; i++) {
                    wallproperty->SetTuple2(i,wpglobal[0*nshgtot+i],wpglobal[1*nshgtot+i]);
                }

                grid->GetPointData()->AddArray(wallproperty);

                wallproperty->Delete();

            }
            // ===
            // WSS
            // ===

            if (RequestedWSS) {

                vtkDoubleArray *wss = vtkDoubleArray::New();
                wss->SetNumberOfComponents(3);
                wss->Allocate(nshgtot,10000);
                wss->SetNumberOfTuples(nshgtot);
                wss->SetName(wname);
                for (i=0; i< nshgtot; i++) {
                    wss->SetTuple3(i,wglobal[0*nshgtot+i],wglobal[1*nshgtot+i],wglobal[2*nshgtot+i]);
                }

                grid->GetPointData()->AddArray(wss);

                wss->Delete();

            }

            // ===========================
            // TIME DERIVATIVE OF SOLUTION
            // ===========================

            if (RequestedTimeDeriv) {

                vtkDoubleArray *acc = vtkDoubleArray::New();
                acc->SetNumberOfComponents(4);
                acc->Allocate(nshgtot,10000);
                acc->SetNumberOfTuples(nshgtot);
                acc->SetName(aname);
                for (i=0; i< nshgtot; i++) {
                    acc->SetTuple4(i,aglobal[0*nshgtot+i],aglobal[1*nshgtot+i],aglobal[2*nshgtot+i],aglobal[3*nshgtot+i]);
                }

                grid->GetPointData()->AddArray(acc);

                acc->Delete();

            }

            // ====
            // YBAR
            // ====

            if(RequestedYbar) {
                vtkDoubleArray *ybar = vtkDoubleArray::New();
                ybar->SetNumberOfComponents(numy);
                ybar->Allocate(nshgtot,10000);
                ybar->SetNumberOfTuples(nshgtot);
                ybar->SetName(yname);
                for (i=0; i< nshgtot; i++) {
                    for (int loopB=0; loopB< numy; loopB++) {
                        currTuple[loopB] = yglobal[loopB*nshgtot+i];
                    }
                    // Set Tuple
                    ybar->SetTuple(i,currTuple);
                }
                grid->GetPointData()->AddArray(ybar);
                ybar->Delete();
            }


            if (((RequestedVTP == true) && (RequestedVTKcombo == false)) ||
                    ((RequestedVTP == true) && (RequestedVTKcombo == true) && (stepnumber == sn_stop))) {

                realfn[0]='\0';
                if (!RequestedVTKcombo) {
                    sprintf(realfn,"%s%s_%05i.vtp",outdir,vtpfn,stepnumber);
                    //sprintf(realfn,"%s%s_%05i.vtk",outdir,vtpfn,stepnumber);
                } else {
                    sprintf(realfn,"%s%s",outdir,vtpfn);
                }

                vtkGeometryFilter* surfFilt = vtkGeometryFilter::New();
                surfFilt->MergingOff();
                surfFilt->SetInputDataObject(grid);
                surfFilt->Update();
                vtkCleanPolyData* cleaner = vtkCleanPolyData::New();
                cleaner->PointMergingOff();
                cleaner->PieceInvariantOff();
                cleaner->SetInputDataObject(surfFilt->GetOutput());
                cleaner->Update();
                if (RequestedVTKcombo) {
                    calcMeanWallShearStressAndPressure(sn_start, sn_stop, sn_incr, RequestedUnitsCm,cleaner->GetOutput());
                }
                vtkXMLPolyDataWriter *polywriter = vtkXMLPolyDataWriter::New();
                polywriter->SetCompressorTypeToZLib();
                polywriter->EncodeAppendedDataOff();
                //vtkPolyDataWriter *polywriter = vtkPolyDataWriter::New();
                polywriter->SetInputDataObject(cleaner->GetOutput());
                polywriter->SetFileName(realfn);
                polywriter->Write();
                polywriter->Delete();
                cleaner->Delete();
                surfFilt->Delete();

            }

            //write mean values into average_result.vtp when RequestedVTKcombo == false
            if ((RequestedVTP == true) && (RequestedVTKcombo == false) ){

                if(stepnumber == sn_start){
                    gridForAverage=createGrid(nshgtot,xglobal,neltot,ien);
                }

                sprintf(pname,"%s_%05i","pressure",stepnumber);
                sprintf(tname,"%s_%05i","traction",stepnumber);
                sprintf(wname,"%s_%05i","WSS",stepnumber);

                vtkDoubleArray* dataArray = NULL;
                if(RequestedSolution){
                    dataArray=cloneVtkDoubleArray(nshgtot,1,static_cast<vtkDoubleArray*>(grid->GetPointData()->GetArray("pressure")));
                    dataArray->SetName(pname);
                    gridForAverage->GetPointData()->AddArray(dataArray);
                }
                if(RequestedInPlaneTraction){
                    dataArray=cloneVtkDoubleArray(nshgtot,3,static_cast<vtkDoubleArray*>(grid->GetPointData()->GetArray("traction")));
                    dataArray->SetName(tname);
                    gridForAverage->GetPointData()->AddArray(dataArray);
                }
                if(RequestedWSS){
                    dataArray=cloneVtkDoubleArray(nshgtot,3,static_cast<vtkDoubleArray*>(grid->GetPointData()->GetArray("WSS")));
                    dataArray->SetName(wname);
                    gridForAverage->GetPointData()->AddArray(dataArray);
                }

                if(stepnumber == sn_stop){
                    realfn[0]='\0';
                    sprintf(realfn,"%s%s",outdir,"average_result.vtp");
                    vtkGeometryFilter* surfFilt = vtkGeometryFilter::New();
                    surfFilt->MergingOff();
                    surfFilt->SetInputDataObject(gridForAverage);
                    surfFilt->Update();
                    vtkCleanPolyData* cleaner = vtkCleanPolyData::New();
                    cleaner->PointMergingOff();
                    cleaner->PieceInvariantOff();
                    cleaner->SetInputDataObject(surfFilt->GetOutput());
                    cleaner->Update();

                    calcMeanWallShearStressAndPressure(sn_start, sn_stop, sn_incr, RequestedUnitsCm,cleaner->GetOutput());

                    for ( int sn = sn_start; sn <= sn_stop; sn += sn_incr) {
                        sprintf(pname,"%s_%05i","pressure",sn);
                        sprintf(tname,"%s_%05i","traction",sn);
                        sprintf(wname,"%s_%05i","WSS",sn);
                        cleaner->GetOutput()->GetPointData()->RemoveArray(pname);
                        cleaner->GetOutput()->GetPointData()->RemoveArray(tname);
                        cleaner->GetOutput()->GetPointData()->RemoveArray(wname);
                    }

                    vtkXMLPolyDataWriter *polywriter = vtkXMLPolyDataWriter::New();
                    polywriter->SetCompressorTypeToZLib();
                    polywriter->EncodeAppendedDataOff();
                    polywriter->SetInputDataObject(cleaner->GetOutput());
                    polywriter->SetFileName(realfn);
                    polywriter->Write();
                    polywriter->Delete();
                    cleaner->Delete();
                    surfFilt->Delete();
                }

            }

            if (((RequestedVTU == true) && (RequestedVTKcombo == false)) ||
                    ((RequestedVTU == true) && (RequestedVTKcombo == true) && (stepnumber == sn_stop))) {

                realfn[0]='\0';
                if (!RequestedVTKcombo) {
                    sprintf(realfn,"%s%s_%05i.vtu",outdir,vtufn,stepnumber);
                    //sprintf(realfn,"%s_%05i.vtk",outdir,vtufn,stepnumber);
                } else {
                    sprintf(realfn,"%s%s",outdir,vtufn);
                }

                vtkXMLUnstructuredGridWriter *ugwriter = vtkXMLUnstructuredGridWriter::New();
                ugwriter->SetCompressorTypeToZLib();
                ugwriter->EncodeAppendedDataOff();
                //vtkUnstructuredGridWriter *ugwriter = vtkUnstructuredGridWriter::New();
                ugwriter->SetInputDataObject(grid);
                ugwriter->SetFileName(realfn);
                ugwriter->Write();
                ugwriter->Delete();

            }
        }

        // need to remove arrays here for non-combo case!!
        if (!RequestedVTKcombo && (RequestedVTU || RequestedVTP)) {
            grid->GetPointData()->RemoveArray("pressure");
            grid->GetPointData()->RemoveArray("velocity");
            grid->GetPointData()->RemoveArray("traction");
            grid->GetPointData()->RemoveArray("displacement");
            grid->GetPointData()->RemoveArray("wallproperty");
            grid->GetPointData()->RemoveArray("WSS");
            grid->GetPointData()->RemoveArray("timeDeriv");
        }

        if (qglobal != NULL) delete [] qglobal;
        if (tglobal != NULL) delete [] tglobal;
        if (dglobal != NULL) delete [] dglobal;
        //if (wpglobal != NULL) delete [] wpglobal;//no change with time. so keep it.
        if (wglobal != NULL) delete [] wglobal;
        if (aglobal != NULL) delete [] aglobal;
        if (yglobal != NULL) delete [] yglobal;
        qglobal = NULL;
        tglobal = NULL;
        dglobal = NULL;
        //wpglobal = NULL;//no change with time. so keep it.
        wglobal = NULL;
        aglobal = NULL;
        yglobal = NULL;

    }

    if (grid != NULL) grid->Delete();
    if (gridForAverage != NULL) gridForAverage->Delete();

    delete pp;

    if (qglobal != NULL) delete [] qglobal;
    if (tglobal != NULL) delete [] tglobal;
    if (dglobal != NULL) delete [] dglobal;
    if (wpglobal != NULL) delete [] wpglobal;
    if (wglobal != NULL) delete [] wglobal;
    if (aglobal != NULL) delete [] aglobal;
    if (yglobal != NULL) delete [] yglobal;
    if (wallflag !=NULL) delete [] wallflag;
    if (wallElements !=NULL) delete [] wallElements;

    return 0;

}
