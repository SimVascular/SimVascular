#include "cvFlowsolverOptions.h"

#include <stdlib.h>
#include <math.h>
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
extern double* ThicknessSolution_;
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
int parseCmdStr(char *cmd, char *mystr);

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





double stiffmatrixlocal(double x[4][3],double kab[4][4]) {

    int i,j;
    double xr,xs,xt,yr,ys,yt,zr,zs,zt,cof[3][3];
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

    cof[0][0]=ys*zt-yt*zs;
    cof[0][1]=yt*zr-yr*zt;
    cof[0][2]=yr*zs-ys*zr;

    cof[1][0]=zs*xt-zt*xs;
    cof[1][1]=zt*xr-zr*xt;
    cof[1][2]=zr*xs-zs*xr;

    cof[2][0]=xs*yt-xt*ys;
    cof[2][1]=xt*yr-xr*yt;
    cof[2][2]=xr*ys-xs*yr;



    detj=xr*cof[0][0]+xs*cof[0][1]+xt*cof[0][2];

    detj=detj*6.0;

    if(fabs(detj)>0.00000001){

    kab[0][0]=(cof[0][0]*cof[0][0]+cof[1][0]*cof[1][0]+cof[2][0]*cof[2][0])/detj;
    kab[0][1]=(cof[0][0]*cof[0][1]+cof[1][0]*cof[1][1]+cof[2][0]*cof[2][1])/detj;
    kab[0][2]=(cof[0][0]*cof[0][2]+cof[1][0]*cof[1][2]+cof[2][0]*cof[2][2])/detj;
    kab[0][3]=(cof[0][0]*(-cof[0][0]-cof[0][1]-cof[0][2])+cof[1][0]*(-cof[1][0]-cof[1][1]-cof[1][2])
            +cof[2][0]*(-cof[2][0]-cof[2][1]-cof[2][2]))/detj;

    kab[1][0]=kab[0][1];
    kab[1][1]=(cof[0][1]*cof[0][1]+cof[1][1]*cof[1][1]+cof[2][1]*cof[2][1])/detj;
    kab[1][2]=(cof[0][1]*cof[0][2]+cof[1][1]*cof[1][2]+cof[2][1]*cof[2][2])/detj;
    kab[1][3]=(cof[0][1]*(-cof[0][0]-cof[0][1]-cof[0][2])+cof[1][1]*(-cof[1][0]-cof[1][1]-cof[1][2])
            +cof[2][1]*(-cof[2][0]-cof[2][1]-cof[2][2]))/detj;

    kab[2][0]=kab[0][2];
    kab[2][1]=kab[1][2];
    kab[2][2]=(cof[0][2]*cof[0][2]+cof[1][2]*cof[1][2]+cof[2][2]*cof[2][2])/detj;
    kab[2][3]=(cof[0][2]*(-cof[0][0]-cof[0][1]-cof[0][2])+cof[1][2]*(-cof[1][0]-cof[1][1]-cof[1][2])
            +cof[2][2]*(-cof[2][0]-cof[2][1]-cof[2][2]))/detj;
    kab[3][0]=kab[0][3];
    kab[3][1]=kab[1][3];
    kab[3][2]=kab[2][3];
    kab[3][3]=(pow(-cof[0][0]-cof[0][1]-cof[0][2],2)+pow(-cof[1][0]-cof[1][1]-cof[1][2],2)
            +pow(-cof[2][0]-cof[2][1]-cof[2][2],2))/detj;
        } else {

         for (i=0;i<4;i++){
           for (j=0;j<4;j++){
           kab[i][j]=0.0;
            }
         }
        }



    return 0;


}


void add2ST(Kentry Kentries[], int &NNZ, double newval, int addrow, int addcol){



    Kentries[NNZ].row=addrow;
    Kentries[NNZ].col=addcol;
    Kentries[NNZ].value=newval;
    NNZ++;




}



int CallFortranGMRES(Kentry* Kentries, int NNZ, double *b,int nunknown,double *u) {


    int i;

    //  printf("malloc M IA JA \n");
    double *M = (double*)malloc(NNZ*sizeof(double));
    int  *IA = (int*)malloc(NNZ*sizeof(int));
    int  *JA = (int*)malloc(NNZ*sizeof(int));

    for (i = 0; i < NNZ; i++) {
        // used in Fortran, index+1
        IA[i]   = Kentries[i].row+1;
        JA[i]   = Kentries[i].col+1;
        M[i]    = Kentries[i].value;


    }

    // std::cout <<"M[2]"<<M[2]<<std::endl;
    // delete [] Kentries;

    gmresfortran(&nunknown,&NNZ,IA,JA, M, b,u);
    //   std::cout <<"u[2]"<<u[2]<<std::endl;
    //   std::cout <<"u[1]"<<u[1]<<std::endl;

    free(M);
    free(IA);
    free(JA);

    return 0;

}

void siftDownKentries(Kentry Kentries[], int root, int bottom, int array_size);

int calcThicknessEvwDistribution(int Laplacetype) {

    int nsdim = 3;
    int nnode = 3;
    int nudof = 1;



    int i,j,k;
    int ig, jg;
    int il, jl;
    int ielem;
    int kk;
    int nunknown,NNZ;



    // printf("new ID \n");
    int* ID=new int[numNodes_];
    int* LM[4];
    double x[4][3],kab[4][4];
    double tstart,tend;



    //Dirichlet nodal BCs

    //double* g    = new double[numNodes_];


    //   printf("new LM \n");
    LM[0]= new int[numElements_];
    LM[1]= new int[numElements_];
    LM[2]= new int[numElements_];
    LM[3]= new int[numElements_];

    nunknown=0;

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

    //   printf("new Kentries, Fglobal  and soln \n");
    int Ksize=100*nunknown;
    Kentry* Kentries = new Kentry[Ksize]();
    double* Fglobal = new double[nunknown]();


    double* soln    = new double[nunknown]();

    NNZ=0;



    tstart=clock();


    for (ielem =0; ielem <numElements_  ; ielem++) {

        if (ielem==numElements_/5 ||ielem==numElements_/5*2 || ielem==numElements_/5*3 ||ielem==numElements_/5*4) {
            printf("%d out of %d elements assembled \n",ielem,numElements_);

        }
        for (i = 0; i < 4; i++) {

            x[i][0] = nodes_[0*numNodes_+elements_[numElements_*i+ielem]-1];
            x[i][1] = nodes_[1*numNodes_+elements_[numElements_*i+ielem]-1];
            x[i][2] = nodes_[2*numNodes_+elements_[numElements_*i+ielem]-1];

        }


           stiffmatrixlocal(x,kab);


        //
        for (j=0;j<4;j++){
            for (k=0;k<4;k++) {
                //assemble the global matrix
                //
                if(LM[j][ielem] !=-1 && LM[k][ielem] != -1 && LM[j][ielem] <= LM[k][ielem] ) {

                    if(fabs(kab[j][k])>0.00000001) {

                        if(NNZ>=Ksize-2) {
                            printf("ERROR: NNZ>=Ksize-2. Increase Ksize. Abort! \n");

                        } else {


                            add2ST(Kentries,NNZ, kab[j][k], LM[j][ielem] , LM[k][ielem]);

                        }
                    }


                }

                //assemble the RHS vector
                if(LM[j][ielem] != -1 && LM[k][ielem] ==-1 ) {


                    Fglobal[LM[j][ielem]]=Fglobal[LM[j][ielem]]-gBC_[elements_[numElements_*k+ielem]-1]*(kab[j][k]);

                }


            }

        }
    }



    printf("heap sort \n");


    //sort global stiff matrix and sum

    int row, col;
    double value;

    int array_size = NNZ;

    // heap sort by row
    for (i = (array_size / 2)-1; i >= 0; i--)
        // printf("call siftdown root=%d array_size=%d\n",i,array_size);
        siftDownKentries(Kentries, i, array_size, array_size);

    for (i = array_size-1; i >= 1; i--)
    {
        row   = Kentries[0].row;
        col   = Kentries[0].col;
        value = Kentries[0].value;
        Kentries[0].row   = Kentries[i].row;
        Kentries[0].col   = Kentries[i].col;
        Kentries[0].value = Kentries[i].value;
        Kentries[i].row   = row;
        Kentries[i].col   = col;
        Kentries[i].value = value;
        siftDownKentries(Kentries, 0, i-1, array_size);
    }



    //sum duplicate entries
    printf("sum duplicate entries \n");

    array_size = NNZ;

    for (i=0;i<NNZ;i++){
        if (Kentries[i].col!=-1) {
            for (j=i+1;j<NNZ;j++){
                if(Kentries[i].row==Kentries[j].row ){
                    if(Kentries[i].col==Kentries[j].col){
                        Kentries[i].value=Kentries[i].value+Kentries[j].value;
                        //tage duplicate entry
                        Kentries[j].col=-1;
                        array_size=array_size-1;
                    }
                } else {
                    break;
                }
            }
        }

    }

    //create global matrix without duplicate entries
    Ksize=50*nunknown;
    Kentry* Kentriesnew = new Kentry[Ksize];
    j=0;
    for (i=0;i<NNZ;i++){
        if(Kentries[i].col!=-1){
            Kentriesnew[j].col=Kentries[i].col;
            Kentriesnew[j].row=Kentries[i].row;
            Kentriesnew[j].value=Kentries[i].value;
            j=j+1;
        }


    }

    delete [] Kentries;







    //   printf("newNNZ=%d j=%d \n ",array_size,j);

    tend=clock();
    printf("NNZ=%d CPU time used=%lf \n ",array_size, (tend-tstart)/CLOCKS_PER_SEC);
    //  printf("row=%d col=%d val=%lf \n",Kentriesnew[i].row,Kentriesnew[i].col,Kentriesnew[i].value);
    //  printf("row=%d Fglobal=%lf \n",i,Fglobal[i]);

    CallFortranGMRES(Kentriesnew, array_size, Fglobal,nunknown,soln);


    delete [] Kentriesnew;



    //   printf("writing Solution\n ");

    if (Laplacetype==0) {
        for (i=0;i<numNodes_;i++) {
            if(ID[i]!=-1){
                ThicknessSolution_[i]=soln[ID[i]];
            } else {
                ThicknessSolution_[i]=gBC_[i];
            }
       /*     if (ThicknessSolution_[i]<0.0){
                printf("Warning: Negative Thickness=%lf \n",ThicknessSolution_[i]);
                printf("coordinates: %lf,%lf,%lf \n", nodes_[0*numNodes_+i-1],nodes_[1*numNodes_+i-1],nodes_[2*numNodes_+i-1]);
            }*/
        }
    }

    //   parseCmdStr("Evw_BC",keyword);
    if (Laplacetype==1) {
        for (i=0;i<numNodes_;i++) {
            if(ID[i]!=-1){
                EvwSolution_[i]=soln[ID[i]];
            } else {
                EvwSolution_[i]=gBC_[i];

            }

       /*     if (EvwSolution_[i]<0.0){
                printf("Warning: Negative Evw=%lf ID= %d \n",EvwSolution_[i],ID[i]);
                printf("coordinates: %lf,%lf,%lf \n", nodes_[0*numNodes_+i-1],nodes_[1*numNodes_+i-1],nodes_[2*numNodes_+i-1]);

            } */
        }



    }

    //  printf("delete Fglobal\n ");
    delete [] Fglobal;
    // printf("delete soln \n ");
    delete [] soln;
    //printf("delete LM \n ");
    delete [] LM[0];
    delete [] LM[1];
    delete [] LM[2];
    delete [] LM[3];
    // printf("delete ID \n ");
    delete [] ID;
    // reset gBC_ to initial state
    for (i=0;i<numNodes_;i++) {
        gBC_[i]=-1.0;

    }

    return 0;


}
