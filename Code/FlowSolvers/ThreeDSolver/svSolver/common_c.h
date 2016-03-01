/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Rensselaer Polytechnic Institute, Charles A. Taylor,
 * Kenneth E. Jansen.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:

 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * Neither the name of the Stanford University or Rensselaer Polytechnic
 * Institute nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior
 * written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 *=========================================================================*/

// Routine contains the structures for reading the user input through
// input_)fform.cpp. The default values for all these variables are defined in
// input.config.
//
// Input variables that have been previously declared in common.h have to be
// re-declared here, in a consistant structure.

#ifdef SV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE

#define workfc WORKFC
#define fronts FRONTS
#define newdim NEWDIM
#define timer4 TIMER4
#define extrat EXTRAT
#define aerfrc AERFRC
#define conpar CONPAR
#define shpdat SHPDAT
#define elmpar ELMPAR
#define genpar GENPAR
#define inpdat INPDAT
#define intdat INTDAT
#define mio MIO
#define mioname MIONAME
#define itrpnt ITRPNT
#define matdat MATDAT
#define mmatpar MMATPAR
#define outpar OUTPAR
#define point POINT
#define precis PRECIS
#define propar PROPAR
#define resdat RESDAT
#define solpar SOLPAR
#define timdat TIMDAT
#define timpar TIMPAR
#define incomp INCOMP
#define mtimer1 MTIMER1
#define mtimer2 MTIMER2
#define title TITLE
#define sclrs SCLRS
#define nomodule NOMODULE
#define sequence SEQUENCE

#endif

#ifdef SV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE

#define workfc workfc_
#define fronts fronts_
#define newdim newdim_
#define timer4 timer4_
#define extrat extrat_
#define aerfrc aerfrc_
#define conpar conpar_
#define shpdat shpdat_
#define elmpar elmpar_
#define genpar genpar_
#define inpdat inpdat_
#define intdat intdat_
#define mio mio_
#define mioname mioname_
#define itrpnt itrpnt_
#define matdat matdat_
#define mmatpar mmatpar_
#define outpar outpar_
#define point point_
#define precis precis_
#define propar propar_
#define resdat resdat_
#define solpar solpar_
#define timdat timdat_
#define timpar timpar_
#define incomp incomp_
#define mtimer1 mtimer1_
#define mtimer2 mtimer2_
#define title title_
#define sclrs sclrs_
#define nomodule nomodule_
#define sequence sequence_

#endif

#define MAXBLK   5000
#define MAXSURF  199
#define MAXTS   100
#define MAXTOP   5
#define MAXQPT   125
#define MAXSH    125
#define NSD      3
#define machin   'RS/6000'
#define machfl   4
//#define zero   0.0000000000000000000000000000000d0
#define pt125   0.1250000000000000000000000000000d0
#define pt25   0.2500000000000000000000000000000d0
#define pt33   0.3333333333333333333333333333333d0
#define pt39   0.3968502629920498686879264098181d0
#define pt5   0.5000000000000000000000000000000d0
#define pt57   0.5773502691896257645091487805020d0
#define pt66   0.6666666666666666666666666666667d0
#define pt75   0.7500000000000000000000000000000d0
#define one   1.0000000000000000000000000000000d0
#define sqt2   1.4142135623730950488016887242097d0
#define onept5   1.5000000000000000000000000000000d0
#define two   2.0000000000000000000000000000000d0
#define three   3.0000000000000000000000000000000d0
#define four   4.0000000000000000000000000000000d0
#define five   5.0000000000000000000000000000000d0
#define pi   3.1415926535897932384626433832795d0
extern "C" {
  extern struct {
    int master;
    int numpe;
    int myrank;
  } workfc ;

  extern struct {
    int maxfront;
    int nlwork;
  } fronts ;

  extern struct {
    int numper;
    int nshgt;
    int nshg0;
  } newdim ;

  extern struct {
    double birth;
    double death;
    double comtim;
  } timer4 ;

  extern struct {
    double ttim[100];
  } extrat ;

  extern struct {
    double scdiff[5];
    double tdecay;
    int nsclr, isclr,nsolt, nosource;
    bool consrv_sclr_conv_vel;
  } sclrs;

  extern struct {
    double flxID[MAXSURF+1][10] ;
    double Force[3];
    double HFlux;
    double flxIDsclr[MAXSURF][4];
    int nsrfCM;
    int nsrflist[MAXSURF+1];
    int isrfIM;
  } aerfrc ;

  extern struct {
    int numnp;
    int numel;
    int numelb;
    int numpbc;
    int nen;
    int nfaces;
    int numflx;
    int ndof;
    int iALE;
    int navier;
    int necho;
    int ichem;
    int iRK;
    int nedof;
    int nshg;
    int nnz;
    int istop;
    int nflow;
    int nnz_tot;
    int idtn;
  } conpar ;

  extern struct {
    int nshape;
    int nshapeb;
    int maxshb;
    int nshl;
    int nshlb;
    int nfath;
    int ntopsh;
    int nsonmax;
  } shpdat ;

  extern struct {
    int lelCat;
    int lcsyst;
    int iorder;
    int nenb;
    int nelblk;
    int nelblb;
    int ndofl;
    int nsymdl;
    int nenl;
    int nfacel;
    int nenbl;
    int intind;
    int mattyp;
  } elmpar ;

  extern struct {
    double E3nsd;
    int I3nsd;
    int nsymdf;
    int ndofBC;
    int ndiBCB;
    int ndBCB;
    int Jactyp;
    int jump;
    int ires;
    int iprec;
    int iprev;
    int ibound;
    int idiff;
    int lhs;
    int itau;
    int ipord;
    int ipred;
    int lstres;
    int iepstm;
    double dtsfct;
    double taucfct;
    int ibksiz;
    int iabc;
    int isurf;
    int idflx;
    double Bo;
    int EntropyPressure;
  } genpar ;

  extern struct {
    double epstol[8];  /* 1+ max number of scalars  (beginning of the
                          end of time sequences) */
    double Delt[MAXTS];
    double CFLfl[MAXTS];
    double CFLsl[MAXTS];
    int nstep[MAXTS];
    int niter[MAXTS];
    int impl[MAXTS];
    double rhoinf[MAXTS];
    int LHSupd[6];
    int loctim[MAXTS];
    double deltol[2][MAXTS];
    int svLSFlag;
    int BCTFlag;
    int BCTMatchingFlag;
    int svLSType;
    int BCTFileNumber;
    int tractionMethod;
    int solverTask;
  } inpdat ;

  extern struct {
    int iin;
    int igeom;
    int ipar;
    int ibndc;
    int imat;
    int iecho;
    int iout;
    int ichmou;
    int irstin;
    int irstou;
    int ihist;
    int iflux;
    int ierror;
    int itable;
    int iforce;
    int igraph;
    int itime;
  } mio ;

  extern struct {
    double fin;
    double fgeom;
    double fpar;
    double fbndc;
    double fmat;
    double fecho;
    double frstin;
    double frstou;
    double fhist;
    double ferror;
    double ftable;
    double fforce;
    double fgraph;
    double ftime;
  } mioname ;

  extern struct {
    int mHBrg;
    int meBrg;
    int myBrg;
    int mRcos;
    int mRsin;
  } itrpnt ;

  extern struct {
    double datmat[MAXTS][7][3];
    int matflg[MAXTS][6];
    int nummat;
    int mexist;
  } matdat ;

  extern struct {
    double pr, Planck, Stephan, Nh, Rh, Rgas;
    double gamma, gamma1, s0;
    //, const, xN2, xO2;
    //double yN2,    yO2,    Msh[5], cpsh[5],s0sh[5],h0sh[5];
    //double Rs[5],  cps[5], cvs[5], h0s[5], Trot[5],sigs[5];
    //double Tvib[5],g0s[5], dofs[5],ithm;
  } mmatpar ;

  extern struct {
    double ro;
    double vel;
    double temper;
    double press;
    double entrop;
    int ntout;
    int ioform;
    int iowflux;
    int iofieldv;
    char iotype[80];
    int ioybar;
    int ioyerror;
    /*  int iostats; */
/*      int ipresref; */
  } outpar ;

  extern struct {
    int mbeg;
    int mend;
    int mprec;
  } point ;

  extern struct {
    double epsM;
    int iabres;
  } precis ;

  extern struct {
    int npro;
  } propar ;

  extern struct {
    double resfrt;
  } resdat ;

  extern struct {
    int ivart;
    int iDC;
    int iPcond;
    int Kspace;
    int nGMRES;
    int iconvflow;
    int iconvsclr;
    int idcsclr[2];
  } solpar ;

  extern struct {
    double time;
    double CFLfld;
    double CFLsld;
    double Dtgl;
    double Dtmax;
    double alpha;
    double etol;
    int lstep;
    int ifunc;
    int itseq;
    int istep;
    int iter;
    int nitr;
    double almi;
    double alfi;
    double gami;
    double flmpl;
    double flmpr;
    double dtol[2];
    int iCFLworst;
  } timdat ;

  extern struct {
    int LCtime;
    int ntseq;
  } timpar ;

  extern struct {
    int numeqns[100];
    int minIters;
    int maxIters;
    int iprjFlag;
    int nPrjs;
    int ipresPrjFlag;
    int nPresPrjs;
    double prestol;
    double statsflow[6];
    double statssclr[6];
    int iverbose;
    int maxNSIters;
    int maxMomentumIters;
    int maxContinuityIters;
  } incomp ;

  extern struct {
    double ccode[13];
  } mtimer1 ;

  extern struct {
    double flops;
    double gbytes;
    double sbytes;
    int iclock;
    int icd;
    int icode;
    int icode2;
    int icode3;
  } mtimer2 ;

  extern struct {
    double title;
    int ititle;
  } title ;

  extern struct {
    int intg[MAXTS][2];
  }intdat;

  extern struct {
    double bcttimescale;
    double ValueListResist[MAXSURF+1];
    double rhovw;
    double thicknessvw;
    double evw;
    double rnuvw;
    double rshearconstantvw;
    double betai;
    double rescontrol;
    double ResCriteria;
    double backFlowStabCoef;
    int icardio;
    int itvn;
    int ipvsq;
    int numResistSrfs;
    int nsrflistResist[MAXSURF+1];
    // CLOSED LOOP
    int numNeumannSrfs;
    int nsrflistNeumann[MAXSURF+1];
    int iGenInitialization;
    int iGenFromFile;
    int numDirichletSrfs;
    int nsrflistDirichlet[MAXSURF+1];
    //==================================
    int numImpSrfs;
    int nsrflistImp[MAXSURF+1];
    int impfile;
    int numRCRSrfs;
    int nsrflistRCR[MAXSURF+1];
    int ircrfile;
    // CORONARY
    int numCORSrfs;
    int nsrflistCOR[MAXSURF+1];
    int icorfile;
    // =============================
    int numVisFluxSrfs;
    int nsrflistVisFlux[MAXSURF+1];
    int numCalcSrfs;
    int nsrflistCalc[MAXSURF+1];
    // CLOSED LOOP
    int numNormalSrfs;
    int nsrflistNormal[MAXSURF+1];
    //============================
    int Lagrange;
    int numLagrangeSrfs;
    int nsrflistLagrange[MAXSURF+1];
    int iLagfile;
    int MinNumIter;
    int ideformwall;
    int applyWallDeformation;
    // VARWALL
    int ivarwallprop;
    // =======
    int iwallmassfactor;
    int iwallstiffactor;
    int nProps;
 } nomodule;

  extern struct {
    int seqsize;
    int stepseq[100];
  } sequence;

}
