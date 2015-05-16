/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
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

#include "cvFlowsolverOptions.h"

#include <stdlib.h>
#include <math.h>

#include "cmd.h"
#include "cvSolverIO.h"
#include <time.h>

#include "gmresfortran.h"

struct Kentry {
    int row;
    int col;
    double value;
};



//extern double* DisplacementSolution_;
extern double* EvwSolution_;

extern char buffer_[MAXCMDLINELENGTH];
extern int* iBC_;
extern double* gBC_;
extern int numNodes_;
extern int numElements_;
extern double* nodes_;
extern int* elements_;

int parseFile(char *cmd);
int NWcloseFile();
int NWgetNextNonBlankLine(int *eof);


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


// ========
//   Norm
// ========

inline
double Norm( double ax, double ay, double az )
{
    double product;

    product = sqrt(ax * ax + ay * ay + az * az);
    return product;
}


// ==========
//   SubVec
// ==========

inline
void SubVec( double ax, double ay, double az,
        double bx, double by, double bz,
        double *prodx, double *prody, double *prodz )
{
    (*prodx) = ax - bx;
    (*prody) = ay - by;
    (*prodz) = az - bz;
    return;
}


// ==========
//   Mat3Vec
// ==========

inline
void Mat3Vec( double mat[3][3],double v[3],double *prodx, double *prody, double *prodz )
{

    (*prodx) = mat[0][0]*v[0] + mat[0][1]*v[1] + mat[0][2]*v[2];
    (*prody) = mat[1][0]*v[0] + mat[1][1]*v[1] + mat[1][2]*v[2];
    (*prodz) = mat[2][0]*v[0] + mat[2][1]*v[1] + mat[2][2]*v[2];
    return;
}





double stiffmatrixlocal(double x[4][3],double kab[4][4]) ;



double massmatrixlocal(double x[4][3],double mab[4][4]) {

    int i,j;
    double xr,xs,xt,yr,ys,yt,zr,zs,zt,cof[3];
    double rx,sx,tx,ry,sy,ty,rz,sz,tz,detj;

    xr=x[0][0]-x[3][0];
    xs=x[1][0]-x[3][0];
    xt=x[2][0]-x[3][0];

    yr=x[0][1]-x[3][1];
    ys=x[1][1]-x[3][1];
    yt=x[2][1]-x[3][1];

    zr=x[0][2]-x[3][2];
    zs=x[1][2]-x[3][2];
    zt=x[2][2]-x[3][2];

    cof[0]=ys*zt-yt*zs;
    cof[1]=yt*zr-yr*zt;
    cof[2]=yr*zs-ys*zr;




    detj=xr*cof[0]+xs*cof[1]+xt*cof[2];



    if(fabs(detj)>0.00000001){

    mab[0][0]=1.0/6.0*(1.0/4.0*1.0/4.0)/detj;
    } else {
    mab[0][0]=0.0;

    }

    mab[0][1]=mab[0][0];
    mab[0][2]=mab[0][0];
    mab[0][3]=mab[0][0];

    mab[1][0]=mab[0][0];
    mab[1][1]=mab[0][0];
    mab[1][2]=mab[0][0];
    mab[1][3]=mab[0][0];

    mab[2][0]=mab[0][0];
    mab[2][1]=mab[0][0];
    mab[2][2]=mab[0][0];
    mab[2][3]=mab[0][0];
    mab[3][0]=mab[0][0];
    mab[3][1]=mab[0][0];
    mab[3][2]=mab[0][0];
    mab[3][3]=mab[0][0];



    return 0;


}


void add2ST(Kentry Kentries[], int &NNZ, double newval, int addrow, int addcol);



void matvec_triad (Kentry Kentries[],int &NNZ, double x[], double y[],int isym ){

    //sparse matrix  y=A*x sparse matrix, triple format
    int i;


    for(i = 1; i<NNZ; i++) {
        y[Kentries[i].row]= y[Kentries[i].row] + Kentries[i].value * x[Kentries[i].col];

    }

    if (isym==1) {
        for(i = 1; i<NNZ;i++) {
            if (Kentries[i].row !=Kentries[i].col)
                y[Kentries[i].col]= y[Kentries[i].col] + Kentries[i].value * x[Kentries[i].row];
        }
    }


}


int CallFortranGMRES(Kentry* Kentries, int NNZ, double *b,int nunknown,double *u) ;


int calcEvwDistribution() {

    int nsdim = 3;
    int nnode = 3;
    int nudof = 1;



    int i,j,k;
    int ig, jg;
    int il, jl;
    int ielem;
    int kk;
    int nunknown,NNZ,NNZ2,numtimesteps;




    int* ID=new int[numNodes_];
    int* LM[4];
    double x[4][3],kab[4][4],mab[4][4],kab2[4][4];
    double tstart,tend,dt;



    //Dirichlet nodal BCs

    //double* g    = new double[numNodes_];



    LM[0]= new int[numElements_];
    LM[1]= new int[numElements_];
    LM[2]= new int[numElements_];
    LM[3]= new int[numElements_];

    nunknown=0;
    dt=0.01;
    numtimesteps=0;

    for (i=0; i<numNodes_;i++) {

        if (gBC_[i] <= 0.0 ) {

            ID[i]=nunknown;
            nunknown++;


        } else {

            ID[i]=-1;
        }
    }



    for (i=0;i<numElements_;i++) {
        for (j=0;j<4;j++) {
            LM[j][i]=ID[elements_[numElements_*j+i]-1];
        }
    }



    // Assembly of the element contributions
    int Ksize=10*nunknown;
    Kentry* Kentries = new Kentry[Ksize]();
    Kentry* Kentries2 = new Kentry[Ksize]();
    double* Fglobal = new double[nunknown]();
    double* Fglobaltemp = new double[nunknown]();
    double* soln    = new double[nunknown]();
    double* soln_old    = new double[nunknown]();


    for (i=0; i<numNodes_;i++) {
        if (ID[i]!=-1) {
            soln[ID[i]]=EvwSolution_[i];
            soln_old[ID[i]]=EvwSolution_[i];
        }
    }

    NNZ=0;
    NNZ2=0;

    tstart=clock();

    for (ielem =0; ielem <numElements_  ; ielem++) {

        //    printf("assemble element %d \n",ielem);
        for (i = 0; i < 4; i++) {

            x[i][0] = nodes_[0*numNodes_+elements_[numElements_*i+ielem]-1];
            x[i][1] = nodes_[1*numNodes_+elements_[numElements_*i+ielem]-1];
            x[i][2] = nodes_[2*numNodes_+elements_[numElements_*i+ielem]-1];

        }

        stiffmatrixlocal(x,kab);
        massmatrixlocal(x,mab);
        //
        for (i=0;i<4;i++){
            for(j=0;j<4;j++) {
                kab[i][j]=mab[i][j]+dt/2.0*kab[i][j];
                kab2[i][j]=mab[i][j]-dt/2.0*kab[i][j];
            }
        }

        for (j=0;j<4;j++){
            for (k=0;k<4;k++) {
                //assemble the global matrix
                //
                if(LM[j][ielem] !=-1 && LM[k][ielem] != -1 && LM[j][ielem] <= LM[k][ielem] ) {

                    if(fabs(kab[j][k])>0.0) {

                        if(NNZ>=Ksize-2) {
                            printf("ERROR: NNZ>=Ksize-2. Increase Ksize. Abort! \n");
                            return 0;

                        } else {
                            add2ST(Kentries,NNZ, kab[j][k], LM[j][ielem] , LM[k][ielem]);
                        }
                    }

                    if(fabs(kab2[j][k])>0.0000001) {

                        if(NNZ2>=Ksize-2) {
                            printf("ERROR: NNZ>=Ksize-2. Increase Ksize. Abort! \n");
                            return 0;

                        } else {
                            add2ST(Kentries2,NNZ2, kab2[j][k], LM[j][ielem] , LM[k][ielem]);
                        }
                    }


                }

                //assemble the RHS vector
                if(LM[j][ielem] != -1 && LM[k][ielem] ==-1 ) {

                    Fglobal[LM[j][ielem]]=Fglobal[LM[j][ielem]]-gBC_[elements_[numElements_*k+ielem]-1]*(kab[j][k]-kab2[j][k]);



                }


            }

        }
    }


    for (k=0;k<numtimesteps;k++ ) {
        printf("Timestep= %i \n ",k);
        for (i=0;i<nunknown;i++){
            soln_old[i]=soln[i];
        }
        Fglobaltemp[nunknown]=0.0;
        matvec_triad(Kentries2,NNZ2, soln_old, Fglobaltemp,1 );
        for (i=0;i<nunknown;i++){
            Fglobal[i]=Fglobal[i]+Fglobaltemp[i];
        }

        tend=clock();

        printf("NNZ=%d CPU time used=%lf \n ",NNZ, (tend-tstart)/CLOCKS_PER_SEC);
        //       printf("KAB global %d %d = %lf \n ",20,22,Kentries[20].value);




        CallFortranGMRES(Kentries, NNZ, Fglobal,nunknown,soln);


    }

    for (i=0;i<numNodes_;i++) {
        if(ID[i]!=-1){
            EvwSolution_[i]=soln[ID[i]];

        } else {
            EvwSolution_[i]=gBC_[i];

        }

    }

    delete [] Kentries;
    delete [] Kentries2;
    delete [] Fglobal;
    delete [] Fglobaltemp;
    delete [] soln;
    delete [] soln_old;
    delete [] LM[0];
    delete [] LM[1];
    delete [] LM[2];
    delete [] LM[3];
    delete [] ID;

    // reset gBC_ to initial state
    for (i=0;i<numNodes_;i++) {
        gBC_[i]=-1.0;

    }



    return 0;


}
